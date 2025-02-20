//! NUMA-aware work stealing scheduler for parallel graph reduction.
//!
//! This module implements a sophisticated work stealing scheduler optimized for NUMA architectures.
//! The scheduler employs multiple strategies to maximize performance:
//!
//! # Core Features
//! 
//! - **NUMA-Aware Stealing**: Prioritizes work stealing within the same NUMA node to minimize
//!   cross-node memory traffic. Uses hardware topology information to make informed decisions.
//!
//! - **Adaptive Work Distribution**: 
//!   - Dynamic batch sizing based on NUMA distance
//!   - Priority-based scheduling with work item priorities
//!   - Load balancing across NUMA domains
//!
//! - **Lock-Free Implementation**:
//!   - Uses Crossbeam's lock-free deque for work queues
//!   - Atomic operations for statistics and thresholds
//!   - Minimizes contention in high-concurrency scenarios
//!
//! # Architecture
//!
//! The scheduler uses a three-level hierarchical stealing strategy:
//!
//! 1. **Local NUMA Node**: First attempts to steal from threads in the same NUMA node
//!    to minimize memory access latency.
//!
//! 2. **Global Queue**: Falls back to the global injector queue which serves as a
//!    load balancing mechanism across all threads.
//!
//! 3. **Remote NUMA Nodes**: As a last resort, attempts to steal from other NUMA nodes,
//!    with stealing batch sizes adjusted based on NUMA distance.
//!
//! # Performance Considerations
//!
//! - **Batch Stealing**: Implements intelligent batch stealing with sizes that adapt to:
//!   - NUMA topology
//!   - Current system load
//!
//! - **Load Balancing**: 
//!   - Monitors queue sizes and work distribution

use crate::NodeIndex;
use crossbeam::deque::{Injector, Steal, Worker, Stealer};
use hwlocality::{
    object::types::ObjectType,
    Topology,
};
use std::sync::Arc;
use crossbeam::utils::Backoff;
use std::collections::HashMap;
use crate::numa;
use std::sync::atomic::{AtomicUsize, Ordering};
use dashmap::DashMap;
use crate::utils::{EnhancedBackoff, BatchSizeParams, calculate_batch_size};
use crate::cache::{CACHE_LINE_SIZE, CacheAligned, CachePressureMonitor};

/// Work item for the scheduler representing a potential node reduction.
/// Includes priority information to enable priority-based scheduling.
#[derive(Debug, Clone)]
pub struct WorkItem {
    /// Left node in the potential reduction
    pub left: NodeIndex,
    /// Right node in the potential reduction
    pub right: NodeIndex,
    /// Priority level for scheduling (higher values = higher priority)
    /// Used to ensure critical path operations are processed first
    pub priority: u32,
}

// Make WorkItem Send + Sync for thread safety
unsafe impl Send for WorkItem {}
unsafe impl Sync for WorkItem {}

/// Threshold for considering a queue busy enough to avoid stealing from
const QUEUE_BUSY_THRESHOLD: usize = 32;
/// Base number of steal attempts before backing off
const BASE_STEAL_ATTEMPTS: u32 = 3;
/// Maximum steal batch size
const MAX_STEAL_BATCH: usize = 8;

/// Represents a potential victim for work stealing operations.
/// Encapsulates all relevant metrics used to make stealing decisions.
#[derive(Debug)]
struct StealCandidate {
    /// Index of the victim thread
    thread_idx: usize,
    /// NUMA distance to the victim (lower is better)
    numa_distance: u64,
    /// Current approximate size of victim's queue
    queue_size: usize,
    /// Whether this candidate is in the same NUMA node
    /// Used for quick filtering of local vs remote candidates
    same_numa: bool,
}

/// Manages adaptive load thresholds for work stealing decisions.
/// Adjusts thresholds based on stealing success rates.
#[derive(Debug)]
struct LoadThresholdTracker {
    /// Current threshold for considering a queue busy enough to steal from
    threshold: AtomicUsize,
    /// Moving average of work stealing success rate (scaled by 1000)
    success_rate: AtomicUsize,
    /// Number of attempts in current adjustment window
    attempts: AtomicUsize,
    /// Size of the window for threshold adjustments
    window_size: usize,
}

impl LoadThresholdTracker {
    fn new(initial_threshold: usize, window_size: usize) -> Self {
        Self {
            threshold: AtomicUsize::new(initial_threshold),
            success_rate: AtomicUsize::new(1000), // Start at 100% (scaled by 1000)
            attempts: AtomicUsize::new(0),
            window_size,
        }
    }

    fn record_attempt(&self, success: bool) {
        let attempts = self.attempts.fetch_add(1, Ordering::Relaxed);
        
        // Update success rate with exponential moving average
        let old_rate = self.success_rate.load(Ordering::Relaxed);
        let new_rate = if success {
            (old_rate * 9 + 1000) / 10
        } else {
            (old_rate * 9) / 10
        };
        self.success_rate.store(new_rate, Ordering::Relaxed);

        // Adjust threshold if window is complete
        if attempts >= self.window_size {
            self.adjust_threshold();
            self.attempts.store(0, Ordering::Relaxed);
        }
    }

    fn adjust_threshold(&self) {
        let success_rate = self.success_rate.load(Ordering::Relaxed) as f64 / 1000.0;
        let current = self.threshold.load(Ordering::Relaxed);

        let new_threshold = if success_rate > 0.8 {
            // High success rate - be more aggressive
            (current * 3) / 4
        } else if success_rate < 0.4 {
            // Low success rate - be more conservative
            (current * 5) / 4
        } else {
            current
        };

        self.threshold.store(new_threshold.max(8).min(128), Ordering::Relaxed);
    }

    fn get_threshold(&self) -> usize {
        self.threshold.load(Ordering::Relaxed)
    }
}

/// Thread-local work queue with NUMA-aware work stealing capabilities.
/// Implements sophisticated stealing strategies based on system topology.
pub struct WorkQueue {
    /// Local work queue for this thread
    local: Worker<WorkItem>,
    /// References to other threads' queues for stealing
    stealers: Arc<Vec<Stealer<WorkItem>>>,
    /// Global work queue for load balancing
    global: Arc<Injector<WorkItem>>,
    /// NUMA node this queue belongs to
    numa_node: usize,
    /// CPU core this queue is bound to
    core_id: usize,
    /// Mapping of core IDs to NUMA nodes
    numa_map: Arc<HashMap<usize, usize>>,
    /// Load threshold tracker
    threshold_tracker: LoadThresholdTracker,
}

// Implement Send + Sync for thread safety
unsafe impl Send for WorkQueue {}
unsafe impl Sync for WorkQueue {}

impl WorkQueue {
    /// Creates a new work queue for a specific thread.
    ///
    /// # Arguments
    /// * `core_id` - CPU core this queue is bound to
    /// * `stealers` - References to other threads' queues
    /// * `global` - Global work injector queue
    /// * `numa_map` - Mapping of core IDs to NUMA nodes
    pub fn new(
        core_id: usize,
        stealers: Arc<Vec<Stealer<WorkItem>>>,
        global: Arc<Injector<WorkItem>>,
        numa_map: Arc<HashMap<usize, usize>>,
    ) -> Self {
        let numa_node = *numa_map.get(&core_id).unwrap_or(&0);

        Self {
            local: Worker::new_fifo(),
            stealers,
            global,
            numa_node,
            core_id,
            numa_map,
            threshold_tracker: LoadThresholdTracker::new(QUEUE_BUSY_THRESHOLD, 100),
        }
    }

    /// Pushes a work item to this thread's local queue.
    pub fn push(&self, item: WorkItem) {
        self.local.push(item);
    }

    /// Tries to pop a work item from this thread's local queue.
    pub fn pop(&self) -> Option<WorkItem> {
        self.local.pop()
    }

    /// Gets the current size of the local queue
    pub fn len(&self) -> usize {
        self.local.len()
    }

    /// Calculates the NUMA distance to another core
    fn numa_distance_to(&self, other_core: usize) -> u64 {
        let other_node = *self.numa_map.get(&other_core).unwrap_or(&0);
        numa::calculate_distance(self.numa_node, other_node)
    }

    /// Gets potential victims for stealing, sorted by priority
    fn get_steal_candidates(&self) -> Vec<StealCandidate> {
        let mut candidates: Vec<StealCandidate> = self.stealers
            .iter()
            .enumerate()
            .filter(|(i, _)| *i != self.core_id)
            .map(|(i, stealer)| {
                let numa_distance = self.numa_distance_to(i);
                let same_numa = self.numa_map.get(&i) == Some(&self.numa_node);
                let queue_size = stealer.len();
                
                StealCandidate {
                    thread_idx: i,
                    numa_distance,
                    queue_size,
                    same_numa,
                }
            })
            .collect();

        // Enhanced sorting that heavily weights affinity for same-NUMA nodes
        candidates.sort_by(|a, b| {
            if a.same_numa && b.same_numa {
                // For same NUMA nodes, prioritize affinity more heavily
                b.queue_size.cmp(&a.queue_size)
            } else if !a.same_numa && !b.same_numa {
                // For remote nodes, balance affinity with NUMA distance
                let a_score = (a.numa_distance as f64 * 0.6) as i64;
                let b_score = (b.numa_distance as f64 * 0.6) as i64;
                b_score.cmp(&a_score)
                    .then_with(|| b.queue_size.cmp(&a.queue_size))
            } else {
                // Prefer same NUMA node unless remote node has very high affinity
                if !a.same_numa && a.numa_distance > self.numa_distance_to(a.thread_idx) * 2 {
                    std::cmp::Ordering::Less
                } else if !b.same_numa && b.numa_distance > self.numa_distance_to(b.thread_idx) * 2 {
                    std::cmp::Ordering::Greater
                } else {
                    b.same_numa.cmp(&a.same_numa)
                }
            }
        });

        candidates
    }

    /// Attempts to steal a batch of work items from another queue.
    /// Returns a vector of stolen items, which may be empty if stealing failed.
    fn try_steal_batch(&self, stealer: &Stealer<WorkItem>, batch_size: usize) -> Vec<WorkItem> {
        let mut stolen = Vec::with_capacity(batch_size);
        let mut backoff = EnhancedBackoff::new();

        while stolen.len() < batch_size {
            match stealer.steal() {
                Steal::Success(item) => {
                    stolen.push(item);
                }
                Steal::Empty => break,
                Steal::Retry => {
                    if !backoff.snooze() {
                        break;
                    }
                }
            }
        }

        stolen
    }

    /// Calculates the optimal batch size for stealing based on various factors.
    fn calculate_batch_size(&self, candidate: &StealCandidate) -> usize {
        let params = BatchSizeParams {
            current_size: MAX_STEAL_BATCH,
            success_rate: if candidate.same_numa { 1.0 } else { 0.7 },
            numa_distance: Some(candidate.numa_distance),
            memory_pressure: Some(candidate.queue_size as f64 / QUEUE_BUSY_THRESHOLD as f64),
            min_size: 1,
            max_size: MAX_STEAL_BATCH,
        };

        calculate_batch_size(params)
    }

    /// Attempts to steal work from other queues, prioritizing local NUMA node.
    pub fn steal_work(&self) -> Option<WorkItem> {
        let mut backoff = EnhancedBackoff::new();
        let threshold = self.threshold_tracker.get_threshold();

        // Get potential victims sorted by priority
        let mut candidates = self.get_steal_candidates();
        candidates.sort_by_key(|c| (
            !c.same_numa,           // Prefer same NUMA node
            c.numa_distance,        // Then minimize NUMA distance
            -(c.queue_size as i64)  // Then prefer larger queues
        ));

        // Try stealing from each candidate
        for candidate in &candidates {
            if candidate.queue_size < threshold {
                continue;
            }

            let stealer = &self.stealers[candidate.thread_idx];
            let batch_size = self.calculate_batch_size(candidate);
            let stolen = self.try_steal_batch(stealer, batch_size);

            if !stolen.is_empty() {
                self.threshold_tracker.record_attempt(true);
                
                // Sort stolen items by priority
                let mut stolen = stolen;
                stolen.sort_by_key(|item| std::cmp::Reverse(item.priority));

                // Keep highest priority item to return
                let return_item = stolen.remove(0);
                
                // Push others to local queue, maintaining priority order
                for item in stolen {
                    self.local.push(item);
                }
                
                return Some(return_item);
            }
        }

        self.threshold_tracker.record_attempt(false);
        None
    }
}

/// NUMA-aware work stealing scheduler managing work distribution across threads.
/// Coordinates work queues and implements global load balancing strategies.
pub struct Scheduler {
    /// Per-thread work queues
    queues: Vec<Arc<WorkQueue>>,
    /// Global work queue for load balancing
    global: Arc<Injector<WorkItem>>,
}

// Implement Send + Sync for thread safety
unsafe impl Send for Scheduler {}
unsafe impl Sync for Scheduler {}

impl Scheduler {
    /// Creates a new scheduler with the specified number of threads.
    /// Initializes the NUMA topology and creates work queues for each thread.
    ///
    /// # Arguments
    /// * `num_threads` - Number of worker threads to create queues for
    pub fn new(num_threads: usize) -> Self {
        let global = Arc::new(Injector::new());
        let numa_map = Arc::new(numa::create_core_to_node_mapping(num_threads));
        
        // Create temporary workers to get stealers
        let mut workers = Vec::with_capacity(num_threads);
        let mut stealers = Vec::with_capacity(num_threads);
        for _ in 0..num_threads {
            let worker = Worker::new_fifo();
            stealers.push(worker.stealer());
            workers.push(worker);
        }
        let stealers = Arc::new(stealers);

        // Create queues with final stealers list
        let queues = (0..num_threads)
            .map(|i| {
                Arc::new(WorkQueue::new(
                    i,
                    stealers.clone(),
                    global.clone(),
                    numa_map.clone(),
                ))
            })
            .collect();

        Self {
            queues,
            global,
        }
    }

    /// Gets the work queue for a specific thread.
    ///
    /// # Arguments
    /// * `thread_id` - ID of the thread to get the queue for
    pub fn get_queue(&self, thread_id: usize) -> Arc<WorkQueue> {
        self.queues[thread_id % self.queues.len()].clone()
    }

    /// Submits a work item to the global queue for load balancing.
    ///
    /// # Arguments
    /// * `item` - Work item to submit
    pub fn submit(&self, item: WorkItem) {
        self.global.push(item);
    }

    /// Returns the number of NUMA nodes in the system.
    pub fn numa_nodes(&self) -> usize {
        numa::get_num_nodes()
    }

    /// Rebalances work across NUMA nodes if load is imbalanced
    pub fn rebalance(&self) {
        let mut node_loads: HashMap<usize, usize> = HashMap::new();
        
        // Calculate load per NUMA node
        for queue in &self.queues {
            let load = queue.len();
            *node_loads.entry(queue.numa_node).or_default() += load;
        }

        // Calculate average load
        let total_load: usize = node_loads.values().sum();
        let avg_load = total_load / node_loads.len();

        // Find overloaded and underloaded nodes
        let overloaded: Vec<_> = node_loads.iter()
            .filter(|(_, &load)| load > avg_load * 2)
            .collect();
        let underloaded: Vec<_> = node_loads.iter()
            .filter(|(_, &load)| load < avg_load / 2)
            .collect();

        // Rebalance by moving work from overloaded to underloaded nodes
        for (&over_node, _) in overloaded {
            for (&under_node, _) in &underloaded {
                // Find queues in these nodes
                let over_queues: Vec<_> = self.queues.iter()
                    .filter(|q| q.numa_node == over_node)
                    .collect();
                let under_queues: Vec<_> = self.queues.iter()
                    .filter(|q| q.numa_node == under_node)
                    .collect();

                // Move some work to balance
                for over_queue in over_queues {
                    for under_queue in &under_queues {
                        while over_queue.len() > under_queue.len() * 2 {
                            if let Some(item) = over_queue.pop() {
                                under_queue.push(item);
                            }
                        }
                    }
                }
            }
        }
    }
}