use std::sync::{Arc, RwLock, Mutex, Condvar};
use std::sync::atomic::{AtomicUsize, AtomicBool, Ordering};
use std::time::Duration;
use crossbeam_deque::{Worker, Stealer, Steal};
use rand::{thread_rng, Rng};
use crate::runtime::types::{AgentType, Redex, ReductionRule};
use crate::runtime::storage::ThreadLocalStorage;
use crate::runtime::runtime::ParallelRuntime;
use crate::runtime::reduction::ReductionEngine;
use super::Port;

// Constants for memory management
const THREAD_NODE_ALLOC_SIZE: usize = 0xFFF;
const THREAD_VAR_ALLOC_SIZE: usize = 0xFFF;

// Add Dijkstra-Scholten termination detection
#[derive(Debug)]
pub struct DijkstraScholten {
    // Per-thread state
    sent_messages: AtomicUsize,
    received_messages: AtomicUsize,
    is_passive: AtomicBool,
    
    // Parent in spanning tree (None for root)
    parent: Option<usize>,
    
    // Children in spanning tree
    children: Mutex<Vec<usize>>,
    
    // Signal for termination detection
    termination_signal: Arc<(Mutex<bool>, Condvar)>,
}

impl DijkstraScholten {
    pub fn new(parent: Option<usize>) -> Self {
        Self {
            sent_messages: AtomicUsize::new(0),
            received_messages: AtomicUsize::new(0),
            is_passive: AtomicBool::new(true),
            parent,
            children: Mutex::new(Vec::new()),
            termination_signal: Arc::new((Mutex::new(false), Condvar::new())),
        }
    }

    pub fn add_child(&self, child: usize) {
        self.children.lock().unwrap().push(child);
    }

    pub fn send_message(&self) {
        self.sent_messages.fetch_add(1, Ordering::SeqCst);
        self.is_passive.store(false, Ordering::SeqCst);
    }

    pub fn receive_message(&self) {
        self.received_messages.fetch_add(1, Ordering::SeqCst);
    }

    pub fn try_signal_parent(&self) -> bool {
        if self.parent.is_none() {
            // Root node - check global termination
            if self.is_terminated() {
                let (lock, cvar) = &*self.termination_signal;
                let mut terminated = lock.lock().unwrap();
                *terminated = true;
                cvar.notify_all();
                return true;
            }
        } else if self.is_passive.load(Ordering::SeqCst) 
            && self.sent_messages.load(Ordering::SeqCst) == self.received_messages.load(Ordering::SeqCst) {
            // Non-root node - signal parent if balanced and passive
            return true;
        }
        false
    }

    pub fn is_terminated(&self) -> bool {
        self.is_passive.load(Ordering::SeqCst) 
            && self.sent_messages.load(Ordering::SeqCst) == self.received_messages.load(Ordering::SeqCst)
            && self.children.lock().unwrap().iter().all(|&child| {
                // Check if child is terminated
                // This would need access to child's state
                true // Placeholder
            })
    }
}

// Thread-local state for parallel graph reduction
pub struct ThreadState {
    // Thread identification
    pub tid: u32,
    pub tids: u32,

    // Performance counters
    pub tick: u32,
    pub interactions: u32,

    // Thread-local storage
    pub storage: Arc<RwLock<ThreadLocalStorage>>,

    // Memory allocation tracking
    pub next_node_idx: usize,
    pub next_var_idx: usize,
    pub node_locations: Vec<usize>,
    pub var_locations: Vec<usize>,

    // Work queues
    pub local_queue: Worker<Redex>,
    pub stealers: Vec<Stealer<Redex>>,
    pub high_priority: Vec<Redex>,

    // Migration tracking
    pub remote_refs: Vec<(Port, u32)>, // Tracks ports that reference remote thread data

    // Termination detection
    pub termination_detector: DijkstraScholten,
}

impl ThreadState {
    pub fn new(tid: u32, tids: u32, stealers: Vec<Stealer<Redex>>, parent: Option<usize>, runtime: &ParallelRuntime) -> Self {
        // Store our storage in the runtime
        if let Some(storage) = runtime.get_thread_storage(tid as usize) {
            Self {
                tid,
                tids,
                tick: 0,
                interactions: 0,
                storage,  // Already an Arc<RwLock<_>>
                next_node_idx: 0,
                next_var_idx: 0,
                node_locations: Vec::with_capacity(THREAD_NODE_ALLOC_SIZE),
                var_locations: Vec::with_capacity(THREAD_VAR_ALLOC_SIZE),
                local_queue: Worker::new_fifo(),
                stealers,
                high_priority: Vec::new(),
                remote_refs: Vec::new(),
                termination_detector: DijkstraScholten::new(parent),
            }
        } else {
            panic!("Failed to initialize thread storage for tid {}", tid);
        }
    }

    pub fn access_remote_storage<T, F>(&self, tid: u32, runtime: &ParallelRuntime, f: F) -> Option<T>
    where
        F: FnOnce(&ThreadLocalStorage) -> Option<T>,
    {
        runtime.get_thread_storage(tid as usize)
            .and_then(|storage| {
                storage.read().ok().and_then(|guard| f(&*guard))
            })
    }

    pub fn access_remote_storage_mut<T, F>(&self, tid: u32, runtime: &ParallelRuntime, f: F) -> Option<T>
    where
        F: FnOnce(&mut ThreadLocalStorage) -> Option<T>,
    {
        runtime.get_thread_storage(tid as usize)
            .and_then(|storage| {
                storage.write().ok().and_then(|mut guard| f(&mut *guard))
            })
    }

    // Create a new port, ensuring thread-local indexing
    pub fn new_port(&self, agent_type: AgentType, local_idx: u64, port_idx: u8) -> Port {
        let mut port = Port(0);
        port.set_destination_agent_type(agent_type as u8);
        port.set_thread_id(self.tid as u8);
        port.set_local_index(local_idx);
        port.set_destination_port(port_idx);
        port
    }

    pub fn try_steal(&mut self) -> Option<Redex> {
        use std::thread;
        
        // First check high priority local work
        if let Some(redex) = self.high_priority.pop() {
            self.termination_detector.receive_message();
            return Some(redex);
        }

        let mut rng = thread_rng();
        let num_stealers = self.stealers.len();
        let mut tried = vec![false; num_stealers];
        let mut attempts = 0;
        const MAX_ATTEMPTS: usize = 3;
        const BACKOFF_BASE_MS: u64 = 1;

        while attempts < MAX_ATTEMPTS {
            // Randomly select an untried victim
            let untried: Vec<usize> = (0..num_stealers)
                .filter(|&i| !tried[i])
                .collect();

            if untried.is_empty() {
                // Reset tried array for next attempt
                tried.fill(false);
                attempts += 1;
                
                if attempts < MAX_ATTEMPTS {
                    // Exponential backoff between attempts
                    let backoff = Duration::from_millis(BACKOFF_BASE_MS << attempts);
                    thread::sleep(backoff);
                }
                continue;
            }

            let victim_idx = untried[rng.gen_range(0..untried.len())];
            tried[victim_idx] = true;

            match self.stealers[victim_idx].steal() {
                Steal::Success(redex) => {
                    self.termination_detector.receive_message();
                    return Some(redex);
                },
                Steal::Empty => continue,
                Steal::Retry => {
                    // On retry, we'll try the same victim again
                    tried[victim_idx] = false;
                }
            }
        }

        None
    }

    pub fn with_storage<F, R>(&self, f: F) -> R 
    where
        F: for<'a> FnOnce(&'a ThreadLocalStorage) -> R,
        R: 'static,
    {
        let storage = self.storage.read().unwrap();
        f(&*storage)
    }

    pub fn with_storage_mut<F, R>(&mut self, f: F) -> R 
    where
        F: for<'a> FnOnce(&'a mut ThreadLocalStorage) -> R,
        R: 'static,
    {
        let mut storage = self.storage.write().unwrap();
        f(&mut *storage)
    }

    // Main reduction loop
    pub fn reduce(&mut self, runtime: &ParallelRuntime) {
        // Signal thread is ready
        runtime.thread_manager.signal_thread_ready(self.tid as usize);
        runtime.thread_manager.synchronize();

        // Main reduction loop
        while !runtime.thread_manager.is_shutdown() {
            // Try to get work from our queues
            let redex = self.high_priority.pop()
                .or_else(|| self.local_queue.pop())
                .or_else(|| self.try_steal());

            match redex {
                Some(redex) => {
                    self.process_redex(redex, runtime);
                    self.tick += 1;
                }
                None => {
                    // No work found, try to terminate
                    if self.try_terminate(runtime) {
                        break;
                    }
                    // Back off a bit before trying again
                    std::thread::sleep(Duration::from_millis(1));
                }
            }
        }

        // Signal that we've terminated
        runtime.thread_manager.signal_terminated(self.tid as usize);
    }

    // Determine which reduction rule applies
    fn get_reduction_rule(&self, redex: &Redex, runtime: &ParallelRuntime) -> ReductionRule {
        use AgentType::*;
        
        let left_type = if redex.left.thread_id() as u32 == self.tid {
            self.with_storage(|s| s.get_agent(redex.left))
        } else {
            self.access_remote_storage(redex.left.thread_id() as u32, runtime, |s| s.get_agent(redex.left))
        };

        let right_type = if redex.right.thread_id() as u32 == self.tid {
            self.with_storage(|s| s.get_agent(redex.right))
        } else {
            self.access_remote_storage(redex.right.thread_id() as u32, runtime, |s| s.get_agent(redex.right))
        };

        match (left_type, right_type) {
            (Some(Reference), _) => ReductionRule::Call,
            (Some(Eraser), _) | (_, Some(Eraser)) => ReductionRule::Erase,
            (Some(Constructor), Some(Constructor)) => ReductionRule::Annihilate,
            (Some(Constructor), Some(Duplicator)) |
            (Some(Duplicator), Some(Constructor)) => ReductionRule::Commute,
            (Some(Number), Some(Number)) => ReductionRule::Compute,
            (Some(Number), Some(Switch)) |
            (Some(Switch), Some(Number)) => ReductionRule::Switch,
            _ => ReductionRule::Erase,
        }
    }

    // Process a single redex
    fn process_redex(&mut self, redex: Redex, runtime: &ParallelRuntime) {
        let left_tid = redex.left.thread_id() as u32;
        let right_tid = redex.right.thread_id() as u32;

        // If either port is remote, we need special handling
        if left_tid != self.tid || right_tid != self.tid {
            // Try to acquire both storages if needed
            let can_process = match (left_tid != self.tid, right_tid != self.tid) {
                (true, true) => {
                    // Need both remote storages
                    let left_storage = runtime.get_thread_storage(left_tid as usize);
                    let right_storage = runtime.get_thread_storage(right_tid as usize);
                    left_storage.is_some() && right_storage.is_some()
                }
                (true, false) => runtime.get_thread_storage(left_tid as usize).is_some(),
                (false, true) => runtime.get_thread_storage(right_tid as usize).is_some(),
                (false, false) => true,
            };

            if !can_process {
                // If we can't get the needed storages, push back to global queue
                self.termination_detector.send_message();
                runtime.global_queue.push(redex);
                return;
            }
        }

        // Get the reduction rule for this redex
        let rule = self.get_reduction_rule(&redex, runtime);
        
        // Apply the appropriate reduction rule
        let success = match rule {
            ReductionRule::Call => {
                self.reduce_call(redex.left, runtime) && self.reduce_call(redex.right, runtime)
            }
            ReductionRule::Erase => {
                self.reduce_erase(redex.left, runtime) && self.reduce_erase(redex.right, runtime)
            }
            ReductionRule::Compute => {
                self.reduce_compute(redex.left, runtime) && self.reduce_compute(redex.right, runtime)
            }
            ReductionRule::Switch => {
                self.reduce_switch(redex.left, runtime) && self.reduce_switch(redex.right, runtime)
            }
            ReductionRule::Annihilate => {
                self.reduce_annihilate(redex.left, runtime) && self.reduce_annihilate(redex.right, runtime)
            }
            ReductionRule::Commute => {
                self.reduce_commute(redex.left, runtime) && self.reduce_commute(redex.right, runtime)
            }
        };
        
        // If reduction failed, push back to global queue
        if !success {
            self.termination_detector.send_message();
            runtime.global_queue.push(redex);
            return;
        }
        
        // Update interaction counter
        self.interactions += 1;
    }

    // Try to terminate if all threads are idle
    fn try_terminate(&self, runtime: &ParallelRuntime) -> bool {
        // Increment idle counter
        runtime.idle_threads.fetch_add(1, Ordering::SeqCst);
        
        // Check if all threads are idle
        let is_last = runtime.idle_threads.load(Ordering::SeqCst) == self.tids;
        
        if is_last {
            // Last thread to go idle - signal termination
            true
        } else {
            // Not all threads idle - decrement and continue
            runtime.idle_threads.fetch_sub(1, Ordering::SeqCst);
            false
        }
    }
}

impl ReductionEngine for ThreadState {
    fn reduce_call(&mut self, redex: Port, runtime: &ParallelRuntime) -> bool {
        // Implementation here
        true
    }

    fn reduce_erase(&mut self, redex: Port, runtime: &ParallelRuntime) -> bool {
        // Implementation here
        true
    }

    fn reduce_compute(&mut self, redex: Port, runtime: &ParallelRuntime) -> bool {
        // Implementation here
        true
    }

    fn reduce_switch(&mut self, redex: Port, runtime: &ParallelRuntime) -> bool {
        // Implementation here
        true
    }

    fn reduce_annihilate(&mut self, redex: Port, runtime: &ParallelRuntime) -> bool {
        // Implementation here
        true
    }

    fn reduce_commute(&mut self, redex: Port, runtime: &ParallelRuntime) -> bool {
        // Implementation here
        true
    }
} 