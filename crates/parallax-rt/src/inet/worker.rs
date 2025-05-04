// parallax-rt/src/inet/worker.rs

use std::{cell::UnsafeCell, collections::BTreeSet, sync::{atomic::{AtomicBool, Ordering}, Arc}};
use crossbeam_queue::SegQueue; // Using SegQueue for the global redex queue and mailbox
use log;
use rsgc::heap::thread::Thread as GcThread;
use parallax_net::{Wire, Port}; // Renamed Redex to Wire
use parking_lot::RwLock;
use super::{manager::AllPartitions, partition::Partition};
use super::CompiledDefs; // Import compiled definitions
use super::types::{PartitionIdx, WorkerId};
use super::reduce; // Import the reduce function
use crate::error::RuntimeError; // Import RuntimeError

/// Represents a worker thread in the runtime system.
/// 
/// Workers pull redexes from local partition queues and process them
/// based on an ownership model, where each partition is owned by exactly
/// one worker at any time.
#[derive(Debug)]
pub struct Worker {
    /// Unique ID for this worker
    pub id: WorkerId,
    
    /// Storage for all partitions (wrapped for unsafe sharing).
    /// Access is governed by the ownership protocol.
    /// NOTE: Initially only contains partition 0.
    pub all_partitions: Arc<RwLock<AllPartitions>>,

    /// Set of partition IDs this worker currently owns
    pub owned_partitions: BTreeSet<PartitionIdx>,
    
    /// Set of partition IDs this worker has requested but not yet received
    pub requested_partitions: BTreeSet<PartitionIdx>,
    
    /// Directory of partition mailboxes, one per worker.
    /// Workers receive ownership transfers through their mailbox.
    pub worker_mailboxes: Arc<Vec<SegQueue<PartitionIdx>>>,

    /// Queue of empty partitions so allocations can be reused. If empty, get a write lock on the all_partitions and push a new partition.
    pub empty_partitions: Arc<SegQueue<PartitionIdx>>,
    
    /// Queue of idle worker IDs (excluding worker 0 initially). 
    pub idle_workers: Arc<SegQueue<WorkerId>>,

    /// Signal to workers to shut down.
    pub shutdown_flag: Arc<AtomicBool>,
    
    /// Number of worker threads.
    pub num_workers: usize,
    
    /// Shared definitions of compiled functions.
    pub compiled_defs: Arc<CompiledDefs>,
}

impl Worker {
    /// The main execution loop for the worker thread.
    pub fn run_loop(&mut self) {
        log::debug!("Worker {} entering run loop.", self.id);

        let _gc_thread = GcThread::current(); // Keep GC thread

        while !self.shutdown_flag.load(Ordering::Relaxed) {
            // === Safepoint ===
            // _gc_thread.safepoint(); // TODO: Re-enable GC safepoint later
            // === End Safepoint ===

            let processed_redex = self.process_partitions();

            // Basic yielding/mailbox check mechanism if no work was done
            if !processed_redex {
                 // TODO: Implement more sophisticated idling/work-stealing later
                 self.check_mailbox(); // Check mailbox before potential yielding/sleeping
                 // Maybe add a short sleep or yield if still no work?
                 // std::thread::yield_now(); 
            }
        }
        log::info!("Worker {} exiting run loop.", self.id);
    }

    /// Checks the worker's mailbox for incoming partition ownership transfers.
    fn check_mailbox(&mut self) {
        let mailbox = &self.worker_mailboxes[self.id];
        while let Some(partition_idx) = mailbox.pop() {
             log::debug!("Worker {}: Received ownership of partition {} from mailbox.", self.id, partition_idx);
             self.owned_partitions.insert(partition_idx);
             // TODO: If this worker was idle, wake it up or notify manager?
        }
    }

    /// Processes all active pairs from the active queue in each owned partition.
    /// After processing, swaps the active and next queues for each partition.
    /// Returns true if any active pair was processed, false otherwise.
    fn process_partitions(&mut self) -> bool {
        let lock = self.all_partitions.read();

        let mut work_done = false;

        // Process each owned partition
        for partition_idx in self.owned_partitions.iter() {
            let partition_state = lock.get(*partition_idx as usize).expect("Partition index must be valid");
            let (partition_cell, _) = partition_state;
            let partition = unsafe { &mut *partition_cell.get() };
            
            
            // Process all collected active pairs
            for active_pair in partition.active_queue_mut().drain(..) {
                // SAFETY: We are the only owner of the partition at this time
                unsafe { self.reduce(active_pair, &lock, &self.compiled_defs) }
                work_done = true;
            }
            
            partition.swap_queues();
        }

        // We must drop the lock before checking the mailbox to avoid deadlock
        drop(lock);
        
        // Check mailbox after attempting work
        self.check_mailbox();

        work_done
    }
}