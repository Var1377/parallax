use std::sync::{Arc, RwLock, Mutex, Condvar, Barrier};
use std::thread::JoinHandle;
use std::sync::atomic::{AtomicU32, AtomicU64};
use crossbeam_deque::Injector;
use crate::runtime::types::*;
use crate::runtime::storage::ThreadLocalStorage;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ThreadLifecycle {
    Created,
    Initializing,
    Running,
    ShuttingDown,
    Terminated,
    Failed(u32), // Error code
}

pub struct ThreadManager {
    lifecycle: Arc<(Mutex<Vec<ThreadLifecycle>>, Condvar)>,
    handles: Mutex<Vec<JoinHandle<()>>>,
    barrier: Arc<Barrier>,
    shutdown: Arc<(Mutex<bool>, Condvar)>,
}

impl ThreadManager {
    pub fn new(num_threads: usize) -> Self {
        Self {
            lifecycle: Arc::new((
                Mutex::new(vec![ThreadLifecycle::Created; num_threads]),
                Condvar::new()
            )),
            handles: Mutex::new(Vec::with_capacity(num_threads)),
            barrier: Arc::new(Barrier::new(num_threads + 1)), // +1 for coordinator
            shutdown: Arc::new((Mutex::new(false), Condvar::new())),
        }
    }

    pub fn register_thread(&self, handle: JoinHandle<()>, thread_id: usize) {
        let (lock, cvar) = &*self.lifecycle;
        let mut states = lock.lock().unwrap();
        states[thread_id] = ThreadLifecycle::Initializing;
        cvar.notify_all();
        
        self.handles.lock().unwrap().push(handle);
    }

    pub fn signal_thread_ready(&self, thread_id: usize) {
        let (lock, cvar) = &*self.lifecycle;
        let mut states = lock.lock().unwrap();
        states[thread_id] = ThreadLifecycle::Running;
        cvar.notify_all();
    }

    pub fn wait_for_threads_ready(&self) {
        let (lock, cvar) = &*self.lifecycle;
        let mut states = lock.lock().unwrap();
        while states.iter().any(|&s| s == ThreadLifecycle::Initializing) {
            states = cvar.wait(states).unwrap();
        }
    }

    pub fn signal_shutdown(&self) {
        let (lock, cvar) = &*self.shutdown;
        let mut shutdown = lock.lock().unwrap();
        *shutdown = true;
        cvar.notify_all();
    }

    pub fn signal_terminated(&self, thread_id: usize) {
        let (lock, cvar) = &*self.lifecycle;
        let mut states = lock.lock().unwrap();
        states[thread_id] = ThreadLifecycle::Terminated;
        cvar.notify_all();
    }

    pub fn signal_failed(&self, thread_id: usize, error_code: u32) {
        let (lock, cvar) = &*self.lifecycle;
        let mut states = lock.lock().unwrap();
        states[thread_id] = ThreadLifecycle::Failed(error_code);
        cvar.notify_all();
    }

    pub fn wait_for_all_terminated(&self) {
        let (lock, cvar) = &*self.lifecycle;
        let mut states = lock.lock().unwrap();
        while states.iter().any(|&s| match s {
            ThreadLifecycle::Terminated | ThreadLifecycle::Failed(_) => false,
            _ => true
        }) {
            states = cvar.wait(states).unwrap();
        }
    }

    pub fn join_threads(&self) -> Vec<Result<(), u32>> {
        let mut results = Vec::new();
        let mut handles = self.handles.lock().unwrap();
        
        while let Some(handle) = handles.pop() {
            results.push(handle.join().map_err(|_| 1));
        }
        
        results
    }

    pub fn synchronize(&self) {
        self.barrier.wait();
    }

    pub fn worker_count(&self) -> usize {
        self.barrier.wait();
        let handles = self.handles.lock().unwrap();
        handles.len()
    }

    pub fn is_shutdown(&self) -> bool {
        let (lock, _) = &*self.shutdown;
        *lock.lock().unwrap()
    }
}

pub struct ParallelRuntime {
    pub global_queue: Injector<Redex>,
    pub total_interactions: AtomicU64,
    pub idle_threads: AtomicU32,
    pub thread_manager: ThreadManager,
    pub thread_storages: Vec<Arc<RwLock<ThreadLocalStorage>>>,
}

impl ParallelRuntime {
    pub fn new(num_threads: usize) -> Self {
        let mut thread_storages = Vec::with_capacity(num_threads);
        for _ in 0..num_threads {
            thread_storages.push(Arc::new(RwLock::new(ThreadLocalStorage::new())));
        }
        
        Self {
            global_queue: Injector::new(),
            total_interactions: AtomicU64::new(0),
            idle_threads: AtomicU32::new(0),
            thread_manager: ThreadManager::new(num_threads),
            thread_storages,
        }
    }

    pub fn get_thread_storage(&self, tid: usize) -> Option<Arc<RwLock<ThreadLocalStorage>>> {
        self.thread_storages.get(tid).cloned()
    }

    pub fn start_threads<F>(&self, thread_factory: F) -> Vec<Result<(), u32>>
    where
        F: Fn(usize) -> JoinHandle<()>,
    {
        // Wait for barrier and get thread count
        self.thread_manager.barrier.wait();
        let worker_count = self.thread_manager.worker_count();
        
        for thread_id in 0..worker_count {
            let handle = thread_factory(thread_id);
            self.thread_manager.register_thread(handle, thread_id);
        }

        self.thread_manager.wait_for_threads_ready();
        self.thread_manager.join_threads()
    }

    pub fn shutdown(&self) {
        let (lock, _) = &*self.thread_manager.lifecycle;
        let states = lock.lock().unwrap();
        for thread_id in 0..states.len() {
            self.thread_manager.signal_shutdown();
        }
        self.thread_manager.wait_for_all_terminated();
    }
} 