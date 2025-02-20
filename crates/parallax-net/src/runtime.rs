//! Parallel graph reduction runtime with NUMA-aware work stealing.
//!
//! This module implements a parallel runtime for graph reduction that leverages:
//! - NUMA-aware work stealing for optimal performance on multi-socket systems
//! - Lock-free work queues using Crossbeam's deque implementation
//! - SIMD-accelerated batch processing of reductions
//! - Automatic thread pinning to CPU cores
//! - Priority-based work scheduling
//! - Efficient handling of asynchronous computations
//!
//! The runtime uses a combination of thread-local work queues and a global injector
//! queue to distribute work across threads. Work stealing is prioritized within
//! NUMA nodes to minimize cross-node memory access.

use crossbeam::utils::Backoff;
use parking_lot::Mutex;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread;
use crate::{NodeIndex, NodeStorage, NodeAllocator, Reducer, ReductionResult};
use crate::work_stealing::{Scheduler, WorkItem as StealWorkItem};
use crate::async_node::{AsyncEvent, AsyncQueue};
use hwlocality::Topology;
use crate::numa;

/// Size of memory chunks allocated for node storage, optimized for L1 cache
const CHUNK_SIZE: usize = 64 * 1024; // 64KB chunks for L1 cache optimization

/// Represents a potential reduction between two nodes in the graph.
/// Used to schedule work in the runtime's queues.
#[derive(Debug, Clone)]
pub struct WorkItem {
    /// Left node in the potential reduction
    pub left: NodeIndex,
    /// Right node in the potential reduction
    pub right: NodeIndex,
}

/// Configuration options for the runtime system
#[derive(Debug, Clone)]
pub struct Config {
    /// Number of worker threads to spawn
    pub num_threads: usize,
    /// Size of memory chunks for node allocation
    pub chunk_size: usize,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            num_threads: std::thread::available_parallelism()
                .map(|n| n.get())
                .unwrap_or(1),
            chunk_size: CHUNK_SIZE,
        }
    }
}

/// Main runtime for parallel graph reduction.
/// Manages worker threads, work distribution, and NUMA-aware scheduling.
pub struct Runtime {
    /// Runtime configuration
    config: Config,
    /// Flag indicating if runtime is running
    is_running: Arc<AtomicBool>,
    /// Shared node storage
    node_storage: Arc<NodeStorage>,
    /// NUMA-aware node allocator
    node_allocator: Arc<NodeAllocator>,
    /// Work stealing scheduler
    scheduler: Arc<Scheduler>,
    /// System topology information
    topology: Arc<Topology>,
    /// Queue for async completions
    async_queue: Arc<Mutex<AsyncQueue>>,
}

impl Runtime {
    /// Creates a new runtime instance with the given configuration
    pub fn new(config: Config) -> Self {
        let node_storage = Arc::new(NodeStorage::new(config.chunk_size * config.num_threads));
        let node_allocator = Arc::new(NodeAllocator::new(config.num_threads));
        let scheduler = Arc::new(Scheduler::new(config.num_threads));
        let async_queue = Arc::new(Mutex::new(AsyncQueue::new(4096)));

        Self {
            config,
            is_running: Arc::new(AtomicBool::new(false)),
            node_storage,
            node_allocator,
            scheduler,
            topology: Arc::new(Topology::new().expect("Failed to initialize topology")),
            async_queue,
        }
    }

    /// Submits a work item to the runtime for processing
    pub fn submit(&self, item: WorkItem) {
        self.scheduler.submit(StealWorkItem {
            left: item.left,
            right: item.right,
            priority: 0,
        });
    }

    /// Starts the runtime and begins processing work items
    pub fn run(&self) {
        self.is_running.store(true, Ordering::Release);
        let mut handles = vec![];

        // Start worker threads
        for thread_id in 0..self.config.num_threads {
            let storage = Arc::clone(&self.node_storage);
            let allocator = Arc::clone(&self.node_allocator);
            let scheduler = Arc::clone(&self.scheduler);
            let is_running = Arc::clone(&self.is_running);

            handles.push(thread::spawn(move || {
                // Pin thread to core
                numa::bind_thread_to_core(thread_id);

                // Create reducer for this thread
                let reducer = Reducer::new(
                    storage,
                    allocator.clone(),
                    thread_id,
                );

                // Get work queue for this thread
                let queue = scheduler.get_queue(thread_id);
                let mut backoff = Backoff::new();
                let mut idle_count = 0;
                let mut batch = Vec::with_capacity(32); // Pre-allocate batch buffer

                while is_running.load(Ordering::Acquire) {
                    // Try to collect a batch of work items
                    while batch.len() < 32 {
                        if let Some(item) = queue.pop().or_else(|| queue.steal_work()) {
                            batch.push((item.left, item.right));
                        } else {
                            break;
                        }
                    }

                    // Process batch if we have any items
                    if !batch.is_empty() {
                        let results = reducer.reduce_batch(&batch);
                        
                        // Process results and schedule new work
                        for (result, (left, right)) in results.into_iter().zip(batch.drain(..)) {
                            match result {
                                ReductionResult::Complete(result) => {
                                    if result.0 != 0 {
                                        queue.push(StealWorkItem {
                                            left: result,
                                            right: NodeIndex(0),
                                            priority: 1, // Increment priority for results
                                        });
                                    }
                                }
                                ReductionResult::NeedsAsync(node) => {
                                    queue.push(StealWorkItem {
                                        left: node,
                                        right: right,
                                        priority: 0,
                                    });
                                }
                                ReductionResult::NoReduction => {}
                            }
                        }
                        
                        // Reset backoff and idle count on successful batch
                        backoff.reset();
                        idle_count = 0;
                    } else {
                        // No work available
                        idle_count += 1;

                        // Back off with increasing delays
                        if backoff.is_completed() {
                            thread::yield_now();
                            backoff = Backoff::new();
                        } else {
                            backoff.snooze();
                        }
                    }
                }
            }));
        }

        // Wait for all threads to complete
        for handle in handles {
            handle.join().unwrap();
        }
    }

    pub fn stop(&mut self) {
        self.is_running.store(false, Ordering::Release);
    }

    /// Processes a completed async computation
    fn handle_async_completion(&self, event: AsyncEvent) {
        let AsyncEvent { node, value } = event;
        
        // Get the async node
        if let Some(async_node) = self.node_storage.get(node.0) {
            // Set the result value and mark as ready
            async_node.set_value(value as u32);  // Convert u64 to u32
            async_node.set_ready(true);
            
            // Get the waiting node from the async node's ports
            let ports = async_node.get_ports();
            if ports[1] != 0 {  // If there's a waiting node
                let waiting_node = NodeIndex(ports[1] as usize);
                
                // Submit for reduction - this will trigger handle_async
                // which will create the appropriate interaction net
                self.submit(WorkItem {
                    left: node,
                    right: waiting_node,
                });
            }
        }
    }

    /// Handles a batch of async completions
    fn process_async_completions(&self) {
        let mut queue = self.async_queue.lock();
        while let Some(event) = queue.pop() {
            self.handle_async_completion(event);
        }
    }
}

impl Drop for Runtime {
    fn drop(&mut self) {
        self.stop();
    }
}