use std::collections::VecDeque;
use crossbeam_queue::SegQueue;
use std::cell::UnsafeCell;
use slab::Slab;

use crate::node::{Constructor, Duplicator, Ref, Number, Switch, Async, Redex};
use crate::Port;

/// Use slab for efficient node storage with O(1) allocation/deallocation
/// and good cache locality
pub type NodeStorage<T> = Slab<T>;

/// Request to transfer a partition to another worker
/// 
/// This is used for work stealing and load balancing.
/// When a worker becomes idle, it can request ownership of partitions
/// from other workers to find more work.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct OwnershipRequest {
    /// The worker requesting the partition
    pub requester_id: usize,
}

/// The state of a partition that can only be accessed by the owning thread
/// 
/// This struct contains all the mutable state of a partition.
/// Access to this state is controlled by the partition's owner.
pub struct PartitionState {
    // Node storage
    pub constructors: NodeStorage<Constructor>,
    pub duplicators: NodeStorage<Duplicator>,
    pub refs: NodeStorage<Ref>,
    pub numbers: NodeStorage<Number>,
    pub switches: NodeStorage<Switch>,
    pub asyncs: NodeStorage<Async>,
    
    // Redex queue for this partition (non-concurrent since it's only accessed by the owner)
    pub redexes: VecDeque<Redex>,
}

/// A partition is a collection of nodes belonging to a particular thread.
/// It provides storage for different node types and manages redexes and erasure.
/// 
/// # Thread Safety
/// 
/// A partition is designed to be owned by a single worker thread at a time.
/// The owner has exclusive access to the partition's state, while other threads
/// can only access the concurrent queues (erase_queue and ownership_requests).
/// 
/// # Memory Management
/// 
/// The partition uses:
/// - `Slab` for efficient node storage
/// - `SegQueue` for concurrent queues
/// - `VecDeque` for the redex queue (owned by the owner thread)
pub struct Partition {
    // The state that can only be accessed by the owning thread
    state: UnsafeCell<PartitionState>,
    
    // Queue for nodes to erase (concurrent so other threads can add to it)
    pub erase_queue: SegQueue<Port>,

    // Queue for ownership requests (concurrent so other threads can add to it)
    pub ownership_requests: SegQueue<OwnershipRequest>,
}

// We need to explicitly mark Partition as Send and Sync since it contains UnsafeCell
// This is safe because we manage access to the UnsafeCell carefully
unsafe impl Send for Partition {}
unsafe impl Sync for Partition {}

impl Default for Partition {
    fn default() -> Self {
        Self::new()
    }
}

impl Partition {
    /// Creates a new empty partition
    /// 
    /// Uses a default capacity of 1024 nodes for each storage type.
    pub fn new() -> Self {
        Self::with_capacity(1024)
    }

    /// Creates a new partition with a given capacity
    /// 
    /// # Arguments
    /// * `capacity` - The initial capacity for each node storage type
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            state: UnsafeCell::new(PartitionState {
                constructors: NodeStorage::with_capacity(capacity),
                duplicators: NodeStorage::with_capacity(capacity),
                refs: NodeStorage::with_capacity(capacity),
                numbers: NodeStorage::with_capacity(capacity),
                switches: NodeStorage::with_capacity(capacity),
                asyncs: NodeStorage::with_capacity(capacity),
                redexes: VecDeque::with_capacity(capacity),
            }),
            erase_queue: SegQueue::new(),
            ownership_requests: SegQueue::new(),
        }
    }
    
    /// Adds a redex to this partition's queue
    /// 
    /// # Safety
    /// The caller must own this partition
    pub unsafe fn push_redex(&self, redex: Redex) {
        let state = &mut *self.state.get();
        state.redexes.push_back(redex);
    }
    
    /// Gets the next redex from this partition's queue
    /// 
    /// # Safety
    /// The caller must own this partition
    pub unsafe fn pop_redex(&self) -> Option<Redex> {
        let state = &mut *self.state.get();
        state.redexes.pop_front()
    }
    
    /// Returns the number of pending redexes
    /// 
    /// # Safety
    /// The caller must own this partition
    pub unsafe fn redex_count(&self) -> usize {
        let state = &*self.state.get();
        state.redexes.len()
    }

    /// Adds an ownership request to this partition's queue
    /// 
    /// This is safe to call from any thread since it uses a concurrent queue
    pub fn push_ownership_request(&self, requester_id: usize) {
        let request = OwnershipRequest { requester_id };
        self.ownership_requests.push(request);
    }

    /// Gets the next ownership request from this partition's queue
    /// 
    /// # Safety
    /// The caller must own this partition
    pub unsafe fn pop_ownership_request(&self) -> Option<OwnershipRequest> {
        self.ownership_requests.pop()
    }

    /// Gets mutable access to the partition state
    /// 
    /// # Safety
    /// The caller must own this partition
    #[inline(always)]
    pub unsafe fn state_mut(&self) -> &mut PartitionState {
        &mut *self.state.get()
    }

    /// Gets immutable access to the partition state
    /// 
    /// # Safety
    /// The caller must own this partition
    #[inline(always)]
    pub unsafe fn state(&self) -> &PartitionState {
        &*self.state.get()
    }
}