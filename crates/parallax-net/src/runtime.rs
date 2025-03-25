use std::sync::atomic::{AtomicUsize, AtomicU16, Ordering};
use std::sync::Arc;
use parking_lot::RwLock;
use std::thread::{self, JoinHandle};
use std::cell::UnsafeCell;
use crate::partition::Partition;
use crate::worker::Worker;

/// The central runtime that manages workers, packets, and work distribution.
/// 
/// The Runtime is responsible for:
/// - Managing a pool of worker threads
/// - Coordinating work distribution across workers
/// - Providing access to partitions
/// - Ensuring thread safety and performance
/// 
/// # Thread Safety
/// 
/// The Runtime uses several mechanisms to ensure thread safety:
/// - `RwLock` for partition access
/// - `UnsafeCell` for worker state (with careful access management)
/// - Atomic operations for coordination
/// 
/// # Performance
/// 
/// The Runtime is optimized for:
/// - Efficient work distribution
/// - Cache-friendly memory access patterns
/// - Lock-free algorithms where possible
pub struct Runtime {
    /// The global partition storage
    /// 
    /// This is protected by a RwLock to allow concurrent reads but exclusive writes.
    /// Workers can read partitions they don't own, but only the owner can modify a partition.
    pub partitions: RwLock<Vec<Arc<Partition>>>,
    
    /// All active workers in the system
    /// 
    /// # Safety
    /// - Only mutate this field when you know no other thread is accessing it
    /// - Workers are only added during initialization before threads are spawned
    /// - After threads are spawned, this is only read, never modified
    workers: UnsafeCell<Vec<Arc<Worker>>>,

    /// The threads that run the workers
    /// 
    /// # Safety
    /// - Only access this field when you know no other thread is accessing it
    /// - Thread handles are only added during initialization and removed during shutdown
    /// - No concurrent access to this field should happen
    worker_threads: UnsafeCell<Vec<JoinHandle<()>>>,

    /// The number of idle workers
    /// 
    /// This is used for work distribution and shutdown coordination.
    /// Workers decrement this when they become idle and increment it when they find work.
    pub running_workers: AtomicUsize,
}

// We need to explicitly mark Runtime as Send and Sync since it contains UnsafeCell
// This is safe because we manage access to the UnsafeCells carefully
unsafe impl Send for Runtime {}
unsafe impl Sync for Runtime {}

impl Runtime {
    /// Creates a new Runtime instance
    /// 
    /// The number of workers is determined by the number of available CPU cores.
    /// This ensures optimal utilization of the system's resources.
    pub fn new() -> Arc<Self> {
        let num_cpus = num_cpus::get();
        Arc::new(Runtime {
            partitions: RwLock::new(Vec::new()),
            workers: UnsafeCell::new(Vec::new()),
            worker_threads: UnsafeCell::new(Vec::new()),
            running_workers: AtomicUsize::new(num_cpus),
        })
    }

    /// Starts the runtime and spawns worker threads
    /// 
    /// This is a convenience method that creates a new runtime and starts it.
    /// The runtime will continue running until explicitly shut down.
    pub fn run() {
        let runtime = Self::new();
        Self::start(runtime);
    }
    
    /// Get a worker by ID
    /// 
    /// # Safety
    /// This is safe to call after initialization because workers are only read, not modified
    pub fn get_worker(&self, id: usize) -> Option<Arc<Worker>> {
        // Safety: We only read from the workers vector, which is safe after initialization
        unsafe {
            let workers = &*self.workers.get();
            workers.get(id).cloned()
        }
    }
    
    /// Get a partition by ID
    /// 
    /// # Safety
    /// - Only call this method if you own the partition or are in the process of transferring ownership
    /// - The caller must verify they own the partition before modifying it
    pub fn get_partition(&self, id: usize) -> Option<Arc<Partition>> {
        let partitions = self.partitions.read();
        partitions.get(id).cloned()
    }
    
    /// Insert a new partition into the runtime
    /// 
    /// # Returns
    /// The ID of the newly inserted partition
    pub fn insert_partition(&self, partition: Arc<Partition>) -> usize {
        let mut partitions = self.partitions.write();
        let id = partitions.len();
        partitions.push(partition);
        id
    }
    
    /// Starts the runtime by spawning worker threads
    /// 
    /// This method:
    /// 1. Creates a worker for each CPU core
    /// 2. Spawns a thread for each worker
    /// 3. Initializes the work distribution system
    /// 
    /// # Safety
    /// This method must only be called once during initialization
    pub fn start(runtime: Arc<Self>) {
        let num_cpus = num_cpus::get();
        
        // Create workers and spawn threads for them
        // Safety: This is running during initialization, before any threads access the workers/worker_threads
        unsafe {
            let workers = &mut *runtime.workers.get();
            let worker_threads = &mut *runtime.worker_threads.get();
            
            for i in 0..num_cpus {
                let worker = Arc::new(Worker::new(i, runtime.clone()));
                workers.push(worker.clone());
            }

            for worker in workers.iter() {
                let handle = thread::spawn(move || {
                    worker.clone().run();
                });
                worker_threads.push(handle);
            }
        }
        
        // Instead of joining, we'll just let the runtime continue
        // The actual program should keep hold of the runtime to prevent threads from being dropped
        println!("Runtime started with {} workers", num_cpus);
    }
    
    /// Shuts down the runtime and waits for all workers to finish
    /// 
    /// This method:
    /// 1. Signals all workers to stop
    /// 2. Waits for all worker threads to finish
    /// 3. Cleans up resources
    /// 
    /// # Safety
    /// Only one thread should call end(), so we have exclusive access
    /// to the worker_threads field during shutdown.
    pub fn end(&self) {
        // Safety: Only one thread should call end(), so we have exclusive access
        // to the worker_threads field during shutdown.
        unsafe {
            let worker_threads = &mut *self.worker_threads.get();
            while let Some(handle) = worker_threads.pop() {
                handle.join().unwrap();
            }
        }
        println!("Runtime shutdown complete");
    }
}