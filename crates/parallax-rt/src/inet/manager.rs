use crate::inet::partition::Partition;
use crate::inet::types::{PartitionIdx, WorkerId};
use crate::inet::worker::Worker;
use crate::error::RuntimeError;
use crate::inet::{CompiledDefs, FunctionId, load_function_into_state, rewrite_port, NodeIndexMaps, readback, ReadbackError, Term};

use std::cell::UnsafeCell;
use std::collections::BTreeSet;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;
use std::thread::JoinHandle;
use crossbeam_queue::SegQueue;
use log;
use parallax_net::Wire;
use parking_lot::RwLock;
use parking_lot::Mutex;
use parallax_net::Port;
use parallax_hir::hir::HirType;


/// 0: Storage for all partitions (wrapped for unsafe sharing, can only be accessed by the owner).
/// 1: Storage for partition ownership requests.
pub type AllPartitions = Vec<(UnsafeCell<Partition>, Mutex<BTreeSet<PartitionIdx>>)>;

/// Manages the overall interaction net runtime execution.
/// Handles partition ownership, termination detection, and worker coordination.
#[derive(Debug)]
pub struct RuntimeManager {
    /// Storage for all partitions (wrapped for unsafe sharing).
    /// Access is governed by the ownership protocol.
    /// NOTE: Initially only contains partition 0.
    pub all_partitions: Arc<RwLock<AllPartitions>>,
    
    /// Directory of partition mailboxes, one per worker.
    /// Workers receive ownership transfers through their mailbox.
    pub worker_mailboxes: Arc<Vec<SegQueue<PartitionIdx>>>,

    /// Queue of empty partitions so allocations can be reused. If empty, get a write lock on the all_partitions and push a new partition.
    pub empty_partitions: Arc<SegQueue<PartitionIdx>>,
    
    /// Queue of idle worker IDs (excluding worker 0 initially). 
    pub idle_workers: Arc<SegQueue<WorkerId>>,
    
    /// Worker thread handles
    pub worker_handles: Vec<JoinHandle<()>>,
    
    /// Signal to workers to shut down.
    pub shutdown_flag: Arc<AtomicBool>,
    
    /// Number of worker threads.
    pub num_workers: usize,
    
    /// Shared definitions of compiled functions.
    pub compiled_defs: Arc<CompiledDefs>,

    /// The HIR type of the entry point's return value.
    /// Cloned when the manager starts running.
    pub entry_type: Option<HirType>,
}

impl RuntimeManager {
    /// Creates a new RuntimeManager.
    pub fn new(
        num_workers: usize,
        compiled_defs: Arc<CompiledDefs>,
    ) -> Self {
        let default_partition_capacity = 1024; // Or some other sensible default

        // Initialize idle stack (workers 1 to num_workers-1)
        let idle_workers = SegQueue::new();
        for worker_id in 1..num_workers {
            idle_workers.push(worker_id as WorkerId);
        }
        let idle_workers = Arc::new(idle_workers);

        Self {
            all_partitions: Arc::new(RwLock::new(vec![(UnsafeCell::new(Partition::new(0, default_partition_capacity)), Mutex::new(BTreeSet::new()))])),
            worker_mailboxes: Arc::new((0..num_workers).map(|_| SegQueue::new()).collect()),
            empty_partitions: Arc::new(SegQueue::new()),
            idle_workers,
            worker_handles: Vec::new(),
            shutdown_flag: Arc::new(AtomicBool::new(false)),
            num_workers,
            compiled_defs,
            entry_type: None, // Initialize entry_type as None
        }
    }

    /// Loads the entry point, creates worker 0, and runs its loop directly.
    pub fn run(&mut self, entry_point_id: FunctionId, entry_type: &HirType) -> Result<(), RuntimeError> {
        log::info!("RuntimeManager starting execution (single-threaded)...");

        // Store the entry type
        self.entry_type = Some(entry_type.clone());

        let partition_id: PartitionIdx = 0;
        let mut global_root_port = Port::NULL; // Variable to store the final root port

        // --- 1. Load Entry Point Function into Partition 0 --- 
        {
            let entry_point_net = self.compiled_defs.get(entry_point_id).expect("Entry point ID out of bounds for compiled_defs Vec");
            let mut partitions_guard = self.all_partitions.write();
            let entry_partition_state = partitions_guard.get_mut(0).expect("Partition 0 must exist");
            let (entry_partition_cell, _) = entry_partition_state;
            let entry_partition = unsafe { &mut *entry_partition_cell.get() };

            log::debug!("Loading entry point function ID {} into partition {}", entry_point_id, partition_id);
            let node_maps = load_function_into_state(&entry_point_net, entry_partition);
            log::debug!("Loaded {} constructors, {} duplicators, etc.", 
                       entry_partition.constructors.len(), entry_partition.duplicators.len());

            let mut initial_active_pair_count = 0;
            for local_wire in &entry_point_net.initial_active_pairs {
                let port_a = rewrite_port(local_wire.0, &node_maps, partition_id);
                let port_b = rewrite_port(local_wire.1, &node_maps, partition_id);
                if port_a != Port::NULL && port_b != Port::NULL {
                    let global_wire = Wire(port_a, port_b);
                    entry_partition.add_active_pair(global_wire);
                    initial_active_pair_count += 1;
                } else {
                    log::warn!("Skipping initial wire {:?} due to NULL port after rewrite.", local_wire);
                }
            }
            log::debug!("Pushed {} initial active pairs to partition {}", initial_active_pair_count, partition_id);

            // Determine and store the global root port
            global_root_port = rewrite_port(entry_point_net.root, &node_maps, partition_id);
            log::debug!("Global root port after rewrite: {:?}", global_root_port);
            if global_root_port == Port::NULL {
                log::warn!("Entry point resulted in a NULL global root port!");
                // Potentially return an error here if a NULL root is invalid
            }
            
        } // Release write lock on all_partitions

        // --- 5. Create and Run Worker 0 --- 
        log::info!("Creating and running worker 0...");
        let mut worker_0 = Worker {
            id: 0,
            all_partitions: Arc::clone(&self.all_partitions),
            owned_partitions: BTreeSet::from([partition_id]),
            requested_partitions: BTreeSet::new(),
            worker_mailboxes: Arc::clone(&self.worker_mailboxes),
            empty_partitions: Arc::clone(&self.empty_partitions),
            idle_workers: Arc::clone(&self.idle_workers),
            shutdown_flag: Arc::clone(&self.shutdown_flag),
            num_workers: self.num_workers,
            compiled_defs: Arc::clone(&self.compiled_defs),
        };

        worker_0.run_loop();
        log::info!("Worker 0 finished reduction loop.");

        // --- Result Extraction --- 
        log::info!("Attempting to read back result from root port: {:?}", global_root_port);
        if global_root_port != Port::NULL {
            if let Some(ref result_type) = self.entry_type {
                match readback(global_root_port, &self.all_partitions) { // Pass the stored type
                    Ok(term) => {
                        log::info!("Readback successful!");
                        // Use println! for potentially large/complex term output
                        println!("Final Term: {:#?}", term); // TODO: Use pretty printer here
                    }
                    Err(e) => {
                        log::error!("Readback failed: {}", e);
                        // Optionally return an error from the run function
                        // return Err(RuntimeError::ReadbackFailed(e));
                    }
                }
            } else {
                log::error!("Cannot read back result: Entry type was not stored.");
                // Potentially return an error
            }
        } else {
            log::warn!("Cannot read back result: Global root port was NULL.");
        }

        log::info!("RuntimeManager finished execution (single-threaded).");
        Ok(())
    }
}