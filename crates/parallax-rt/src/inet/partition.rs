// parallax-rt/src/inet/partition.rs

// Import base types from parallax_net
use parallax_net::node::{Constructor, Duplicator, Static, Number, Switch, Async, Eraser, Pointer};
use parallax_net::Redex;
use slab::Slab;
// Use VecDeque for local redex queue
use std::collections::VecDeque;
// Import PartitionId from types
use crate::inet::types::PartitionIdx;

// Use slab for efficient node storage with O(1) allocation/deallocation
// and good cache locality
pub type NodeStorage<T> = Slab<T>;

/// The state of an interaction net partition that can only be accessed by the owning thread.
///
/// Contains the interaction net nodes and a double-queue system for redexes,
/// where one queue is processed while the other collects new redexes.
#[derive(Debug)]
pub struct Partition {
    /// Unique identifier for this partition.
    pub id: PartitionIdx,
    // Node storage using Slab for efficiency
    pub constructors: NodeStorage<Constructor>,
    pub duplicators: NodeStorage<Duplicator>,
    pub statics: NodeStorage<Static>,
    pub numbers: NodeStorage<Number>,
    pub switches: NodeStorage<Switch>,
    pub asyncs: NodeStorage<Async>,
    // Re-added Eraser storage as per spec
    pub erasers: NodeStorage<Eraser>,
    // Add Pointer storage
    pub pointers: NodeStorage<Pointer>,

    /// Double queue system for redexes
    /// Queue 0 and 1 alternate between being the active queue (for processing)
    /// and the next queue (for collecting new redexes)
    pub redex_queues: [VecDeque<Redex>; 2],
    
    /// Tracks which queue is currently being processed (0 or 1)
    pub active_queue_index: usize,
}

impl Partition {
    /// Creates a new, empty Partition with a default capacity.
    pub fn new(id: PartitionIdx, capacity: usize) -> Self {
        Partition {
            id,
            constructors: NodeStorage::with_capacity(capacity),
            duplicators: NodeStorage::with_capacity(capacity),
            statics: NodeStorage::with_capacity(capacity),
            numbers: NodeStorage::with_capacity(capacity),
            switches: NodeStorage::with_capacity(capacity),
            asyncs: NodeStorage::with_capacity(capacity),
            erasers: NodeStorage::with_capacity(capacity), // Initialize erasers slab
            pointers: NodeStorage::with_capacity(capacity), // Initialize pointers slab
            redex_queues: [
                VecDeque::with_capacity(capacity),
                VecDeque::with_capacity(capacity),
            ],
            active_queue_index: 0,
        }
    }

    /// Gets a reference to the active queue (the one being processed)
    #[inline]
    pub fn active_queue(&self) -> &VecDeque<Redex> {
        &self.redex_queues[self.active_queue_index]
    }

    /// Gets a mutable reference to the active queue (the one being processed)
    #[inline]
    pub fn active_queue_mut(&mut self) -> &mut VecDeque<Redex> {
        &mut self.redex_queues[self.active_queue_index]
    }

    /// Gets a mutable reference to the next queue (where new redexes are added)
    #[inline]
    pub fn next_queue_mut(&mut self) -> &mut VecDeque<Redex> {
        &mut self.redex_queues[1 - self.active_queue_index]
    }

    /// Swaps the active and next queues
    #[inline]
    pub fn swap_queues(&mut self) {
        self.active_queue_index = 1 - self.active_queue_index;
    }

    /// Adds a redex to the next queue
    #[inline]
    pub fn add_redex(&mut self, redex: Redex) {
        self.next_queue_mut().push_back(redex);
    }

    // --- Allocation Methods ---
    // These methods simply insert the node into the corresponding slab
    // and return the new index (key) assigned by the slab.

    #[inline]
    pub fn alloc_constructor(&mut self, node: Constructor) -> usize {
        self.constructors.insert(node)
    }

    #[inline]
    pub fn alloc_duplicator(&mut self, node: Duplicator) -> usize {
        self.duplicators.insert(node)
    }

    #[inline]
    pub fn alloc_static(&mut self, node: Static) -> usize {
        self.statics.insert(node)
    }

    #[inline]
    pub fn alloc_number(&mut self, node: Number) -> usize {
        self.numbers.insert(node)
    }

    #[inline]
    pub fn alloc_switch(&mut self, node: Switch) -> usize {
        self.switches.insert(node)
    }

    #[inline]
    pub fn alloc_async(&mut self, node: Async) -> usize {
        self.asyncs.insert(node)
    }

    // Added alloc_eraser
    #[inline]
    pub fn alloc_eraser(&mut self, node: Eraser) -> usize {
        self.erasers.insert(node)
    }

    // Added alloc_pointer
    #[inline]
    pub fn alloc_pointer(&mut self, node: Pointer) -> usize {
        self.pointers.insert(node)
    }
} 