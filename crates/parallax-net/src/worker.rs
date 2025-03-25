use std::sync::{atomic::Ordering, Arc};
use crossbeam_queue::SegQueue;
use std::cell::UnsafeCell;
use fxhash::{FxBuildHasher, FxHashSet};

use crate::Runtime;

/// The state of a worker that can only be accessed by the owning thread
/// 
/// This struct contains all the mutable state of a worker.
/// Access to this state is controlled by the worker thread itself.
pub struct WorkerState {
    /// The IDs of partitions currently owned by this worker
    pub owned_partition_ids: FxHashSet<usize>,
}

/// A worker is a thread that processes partitions and their associated redexes.
/// 
/// Each worker:
/// - Has a unique ID
/// - Maintains a set of partitions it owns
/// - Processes redexes from its owned partitions
/// - Can request and transfer partition ownership
/// 
/// # Thread Safety
/// 
/// A worker is designed to be owned by a single thread.
/// The owner has exclusive access to the worker's state, while other threads
/// can only access the concurrent queues (incoming_partition_ids).
/// 
/// # Performance
/// 
/// The worker is optimized for:
/// - Efficient partition ownership tracking
/// - Fast partition ownership transfers
/// - Lock-free communication with other workers
pub struct Worker {
    /// The worker's unique ID
    pub id: usize,

    /// The runtime that this worker belongs to
    runtime: Arc<Runtime>,
    
    /// The state that can only be accessed by the owning thread
    /// 
    /// # Safety
    /// - This field is wrapped in UnsafeCell to allow interior mutability
    /// - Only the worker thread that owns this Worker instance may access this state
    /// - All methods that access this state are marked unsafe and require ownership verification
    /// - No concurrent access to this field should ever occur
    state: UnsafeCell<WorkerState>,
    
    /// Mailbox for incoming partition IDs from other workers
    /// 
    /// This queue is used for partition ownership transfers.
    /// When a worker transfers a partition to this worker, it adds the partition ID
    /// to this queue. The worker then processes this queue to update its owned partitions.
    incoming_partition_ids: SegQueue<usize>,
}

// We need to explicitly mark Worker as Send and Sync since it contains UnsafeCell
// This is safe because we manage access to the UnsafeCell carefully
unsafe impl Send for Worker {}
unsafe impl Sync for Worker {}

impl Worker {
    /// Creates a new worker with the given ID and runtime
    /// 
    /// # Arguments
    /// * `id` - The unique ID for this worker
    /// * `runtime` - The runtime this worker belongs to
    pub fn new(id: usize, runtime: Arc<Runtime>) -> Self {
        Worker {
            id,
            runtime,
            state: UnsafeCell::new(WorkerState {
                owned_partition_ids: FxHashSet::with_capacity_and_hasher(128, FxBuildHasher::default()),
            }),
            incoming_partition_ids: SegQueue::new(),
        }
    }
    
    /// Add a partition to this worker's ownership
    /// 
    /// # Safety
    /// The caller must own this worker
    pub unsafe fn own_partition(&self, partition_id: usize) {
        let state = &mut *self.state.get();
        state.owned_partition_ids.insert(partition_id);
    }
    
    /// Request a partition from another worker
    /// 
    /// This is safe to call from any thread since it uses concurrent queues.
    /// The request will be added to the target partition's ownership request queue.
    pub fn request_partition_from(&self, partition_id: usize) {
        // Add ownership request to partition
        if let Some(partition) = self.runtime.get_partition(partition_id) {
            partition.push_ownership_request(self.id);
        }
    }
    
    /// Transfer a partition to another worker
    /// 
    /// # Safety
    /// The caller must own this worker
    pub unsafe fn transfer_partition_to(&self, partition_id: usize, other: &Worker) {
        let state = &mut *self.state.get();
        if state.owned_partition_ids.remove(&partition_id) {
            other.incoming_partition_ids.push(partition_id);
        } else {
            panic!("Worker {} does not own partition {}", self.id, partition_id);
        }
    }
    
    /// Process incoming partitions
    /// 
    /// This method processes any partition ownership transfers that have been
    /// requested by other workers.
    /// 
    /// # Safety
    /// The caller must own this worker
    pub unsafe fn process_incoming_partitions(&self) {
        while let Some(partition_id) = self.incoming_partition_ids.pop() {
            let state = &mut *self.state.get();
            state.owned_partition_ids.insert(partition_id);
        }
    }

    /// The main worker loop
    /// 
    /// This method:
    /// 1. Processes redexes from owned partitions
    /// 2. Handles partition ownership transfers
    /// 3. Coordinates with other workers
    /// 4. Manages shutdown
    pub fn run(self: Arc<Self>) {
        'main: loop {
            if self.runtime.running_workers.fetch_sub(1, Ordering::SeqCst) == 0 {
                break 'main;
            }
            
            'idle: loop {
            }
            
            self.runtime.running_workers.fetch_add(1, Ordering::SeqCst);
        }
    }

    /// Get the worker's ID
    pub fn get_id(&self) -> usize {
        self.id
    }
}