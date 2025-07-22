# Thread Pool Configuration in Rayon

## Introduction

Rayon provides flexible thread pool configuration through the `ThreadPoolBuilder`. You can customize the global thread pool or create custom thread pools for specific workloads.

## Global Thread Pool

By default, Rayon creates a global thread pool that's used by all parallel operations. This pool is initialized automatically on first use, but you can configure it explicitly.

### Default Configuration

```rust
use rayon::prelude::*;

// Uses the global thread pool with default settings
let sum: i32 = (0..1000).into_par_iter().sum();
```

The default configuration:
- **Number of threads**: Equal to the number of logical CPU cores
- **Thread names**: "rayon-core-{index}"
- **Stack size**: System default
- **Scheduling**: Depth-first (LIFO for better cache locality)

### Environment Variables

You can configure the global pool using environment variables:

```bash
# Set number of threads
export RAYON_NUM_THREADS=4

# Run your program
cargo run
```

Supported environment variables:
- `RAYON_NUM_THREADS`: Number of worker threads (overrides CPU detection)
- `RAYON_RS_NUM_CPUS`: Deprecated, use `RAYON_NUM_THREADS` instead

## Configuring the Global Thread Pool

### Basic Configuration

```rust
use rayon::ThreadPoolBuilder;

fn main() -> Result<(), rayon::ThreadPoolBuildError> {
    // Configure global pool before any parallel operations
    ThreadPoolBuilder::new()
        .num_threads(4)
        .thread_name(|index| format!("my-worker-{}", index))
        .build_global()?;
    
    // Now all parallel operations use the configured pool
    let sum: i32 = (0..1000).into_par_iter().sum();
    
    Ok(())
}
```

### Advanced Global Configuration

```rust
use rayon::ThreadPoolBuilder;
use std::panic;

fn main() -> Result<(), rayon::ThreadPoolBuildError> {
    ThreadPoolBuilder::new()
        .num_threads(8)
        .thread_name(|index| format!("compute-{}", index))
        .stack_size(8 * 1024 * 1024) // 8MB stack
        .panic_handler(|panic_info| {
            eprintln!("Worker thread panicked: {:?}", panic_info);
            // Custom panic handling logic
        })
        .start_handler(|index| {
            println!("Worker thread {} starting", index);
            // Thread initialization code
        })
        .exit_handler(|index| {
            println!("Worker thread {} exiting", index);
            // Thread cleanup code
        })
        .build_global()?;
    
    Ok(())
}
```

## Custom Thread Pools

For more control, you can create custom thread pools for specific workloads.

### Creating Custom Pools

```rust
use rayon::{ThreadPool, ThreadPoolBuilder};

fn main() -> Result<(), rayon::ThreadPoolBuildError> {
    // Create a custom thread pool
    let pool = ThreadPoolBuilder::new()
        .num_threads(4)
        .thread_name(|index| format!("custom-{}", index))
        .build()?;
    
    // Execute work in the custom pool
    let result = pool.install(|| {
        (0..1000).into_par_iter().sum::<i32>()
    });
    
    println!("Result: {}", result);
    Ok(())
}
```

### Multiple Thread Pools

```rust
use rayon::{ThreadPool, ThreadPoolBuilder};

fn main() -> Result<(), rayon::ThreadPoolBuildError> {
    // CPU-intensive pool
    let cpu_pool = ThreadPoolBuilder::new()
        .num_threads(num_cpus::get())
        .thread_name(|index| format!("cpu-{}", index))
        .build()?;
    
    // I/O pool (fewer threads to avoid overwhelming I/O)
    let io_pool = ThreadPoolBuilder::new()
        .num_threads(4)
        .thread_name(|index| format!("io-{}", index))
        .build()?;
    
    // Use different pools for different work
    let cpu_result = cpu_pool.install(|| {
        (0..1_000_000).into_par_iter()
            .map(|x| expensive_computation(x))
            .sum::<i32>()
    });
    
    let io_result = io_pool.install(|| {
        vec!["file1.txt", "file2.txt", "file3.txt"]
            .into_par_iter()
            .map(|filename| std::fs::read_to_string(filename).unwrap_or_default())
            .collect::<Vec<String>>()
    });
    
    Ok(())
}

fn expensive_computation(x: i32) -> i32 {
    // Simulate CPU-intensive work
    (0..1000).fold(x, |acc, i| acc.wrapping_mul(i + 1))
}
```

## Thread Pool Configuration Options

### Number of Threads

```rust
use rayon::ThreadPoolBuilder;

// Explicit thread count
let pool = ThreadPoolBuilder::new()
    .num_threads(8)
    .build()?;

// Use all available cores
let pool = ThreadPoolBuilder::new()
    .num_threads(num_cpus::get())
    .build()?;

// Use half the available cores
let pool = ThreadPoolBuilder::new()
    .num_threads(num_cpus::get() / 2)
    .build()?;

// Dynamic based on workload
let heavy_computation_threads = if cfg!(debug_assertions) { 2 } else { num_cpus::get() };
let pool = ThreadPoolBuilder::new()
    .num_threads(heavy_computation_threads)
    .build()?;
```

### Thread Naming

```rust
use rayon::ThreadPoolBuilder;

// Simple naming
let pool = ThreadPoolBuilder::new()
    .thread_name(|index| format!("worker-{}", index))
    .build()?;

// Descriptive naming
let pool = ThreadPoolBuilder::new()
    .thread_name(|index| format!("image-processor-{:02}", index))
    .build()?;

// Include process ID for debugging
let pool = ThreadPoolBuilder::new()
    .thread_name(|index| format!("rayon-{}-{}", std::process::id(), index))
    .build()?;
```

### Stack Size Configuration

```rust
use rayon::ThreadPoolBuilder;

// Large stack for recursive algorithms
let pool = ThreadPoolBuilder::new()
    .stack_size(16 * 1024 * 1024) // 16MB
    .build()?;

// Small stack for memory-constrained environments
let pool = ThreadPoolBuilder::new()
    .stack_size(512 * 1024) // 512KB
    .build()?;

// Platform-specific stack sizes
let stack_size = if cfg!(target_os = "windows") {
    8 * 1024 * 1024  // 8MB on Windows
} else {
    2 * 1024 * 1024  // 2MB on Unix
};

let pool = ThreadPoolBuilder::new()
    .stack_size(stack_size)
    .build()?;
```

### Scheduling Strategy

```rust
use rayon::ThreadPoolBuilder;

// Depth-first (default) - better cache locality
let depth_first_pool = ThreadPoolBuilder::new()
    .build()?;

// Breadth-first - better for many small tasks
let breadth_first_pool = ThreadPoolBuilder::new()
    .breadth_first()
    .build()?;
```

## Thread Lifecycle Handlers

### Start and Exit Handlers

```rust
use rayon::ThreadPoolBuilder;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

fn main() -> Result<(), rayon::ThreadPoolBuildError> {
    let thread_counter = Arc::new(AtomicUsize::new(0));
    let counter_clone = Arc::clone(&thread_counter);
    
    let pool = ThreadPoolBuilder::new()
        .num_threads(4)
        .start_handler(move |index| {
            counter_clone.fetch_add(1, Ordering::Relaxed);
            println!("Thread {} started", index);
            
            // Thread-local initialization
            thread_local_init();
        })
        .exit_handler(|index| {
            println!("Thread {} exiting", index);
            
            // Thread-local cleanup
            thread_local_cleanup();
        })
        .build()?;
    
    pool.install(|| {
        // Your parallel work here
        (0..1000).into_par_iter().sum::<i32>()
    });
    
    println!("Active threads: {}", thread_counter.load(Ordering::Relaxed));
    Ok(())
}

fn thread_local_init() {
    // Initialize thread-local resources
    // e.g., database connections, caches, etc.
}

fn thread_local_cleanup() {
    // Clean up thread-local resources
}
```

### Panic Handling

```rust
use rayon::ThreadPoolBuilder;
use std::panic;

fn main() -> Result<(), rayon::ThreadPoolBuildError> {
    let pool = ThreadPoolBuilder::new()
        .panic_handler(|panic_info| {
            // Log the panic
            eprintln!("Worker thread panic: {:?}", panic_info);
            
            // Send to monitoring system
            send_panic_to_monitoring(panic_info);
            
            // Don't abort the process, just log
        })
        .build()?;
    
    // This won't crash the entire program
    pool.install(|| {
        (0..10).into_par_iter().for_each(|i| {
            if i == 5 {
                panic!("Intentional panic for testing");
            }
            println!("Processing {}", i);
        });
    });
    
    Ok(())
}

fn send_panic_to_monitoring(panic_info: Box<dyn std::any::Any + Send>) {
    // Implementation for sending panic info to monitoring system
}
```

## Custom Thread Spawning

### Basic Custom Spawning

```rust
use rayon::ThreadPoolBuilder;
use std::io;

fn main() -> Result<(), rayon::ThreadPoolBuildError> {
    let pool = ThreadPoolBuilder::new()
        .spawn_handler(|thread| {
            // Custom thread spawning logic
            std::thread::spawn(move || {
                // Custom initialization
                setup_thread_local_state();
                
                // Run the rayon worker
                thread.run();
            });
            Ok(())
        })
        .build()?;
    
    Ok(())
}

fn setup_thread_local_state() {
    // Custom per-thread initialization
}
```

### Scoped Thread Pools

```rust
use rayon::ThreadPoolBuilder;

fn main() -> Result<(), rayon::ThreadPoolBuildError> {
    let shared_data = vec![1, 2, 3, 4, 5];
    
    // Create a scoped thread pool that can access local data
    ThreadPoolBuilder::new()
        .build_scoped(
            |thread| {
                // This closure runs in each worker thread
                // It has access to `shared_data` through the closure
                println!("Worker has access to shared_data: {:?}", shared_data);
                thread.run();
            },
            |pool| {
                // This closure runs with the pool available
                pool.install(|| {
                    (0..10).into_par_iter().for_each(|i| {
                        println!("Processing {} with shared data", i);
                    });
                });
            }
        )?;
    
    Ok(())
}
```

### Integration with External Thread Management

```rust
use rayon::ThreadPoolBuilder;
use std::sync::mpsc;
use std::thread;

fn main() -> Result<(), rayon::ThreadPoolBuildError> {
    let (tx, rx) = mpsc::channel();
    
    let pool = ThreadPoolBuilder::new()
        .spawn_handler(move |thread_builder| {
            let tx = tx.clone();
            
            // Use external thread management
            thread::Builder::new()
                .name(format!("external-{}", thread_builder.index()))
                .spawn(move || {
                    // Notify external system of thread start
                    tx.send(format!("Thread {} started", thread_builder.index())).ok();
                    
                    // Run rayon worker
                    thread_builder.run();
                })?;
            
            Ok(())
        })
        .build()?;
    
    // Monitor thread creation
    thread::spawn(move || {
        while let Ok(msg) = rx.recv() {
            println!("External monitor: {}", msg);
        }
    });
    
    Ok(())
}
```

## Using Current Thread

### Include Current Thread in Pool

```rust
use rayon::ThreadPoolBuilder;

fn main() -> Result<(), rayon::ThreadPoolBuildError> {
    // Create pool that includes the current thread
    let pool = ThreadPoolBuilder::new()
        .num_threads(4)
        .use_current_thread()
        .build()?;
    
    // Current thread becomes worker thread 0
    pool.install(|| {
        println!("Current thread index: {:?}", rayon::current_thread_index());
        
        (0..1000).into_par_iter().for_each(|i| {
            if i % 100 == 0 {
                println!("Processing {} on thread {:?}", i, rayon::current_thread_index());
            }
        });
    });
    
    Ok(())
}
```

### Yielding in Current Thread

```rust
use rayon::{ThreadPoolBuilder, yield_now, yield_local};

fn main() -> Result<(), rayon::ThreadPoolBuildError> {
    let pool = ThreadPoolBuilder::new()
        .use_current_thread()
        .build()?;
    
    pool.install(|| {
        // Spawn some background work
        for i in 0..10 {
            rayon::spawn(move || {
                println!("Background task {}", i);
            });
        }
        
        // Yield to allow spawned tasks to run
        yield_now();
        
        // Do some work, yielding periodically
        for i in 0..1000 {
            if i % 100 == 0 {
                yield_local(); // Yield to local tasks only
            }
            // Do work...
        }
    });
    
    Ok(())
}
```

## Thread Pool Best Practices

### 1. Pool Sizing

```rust
use rayon::ThreadPoolBuilder;

fn create_optimal_pool() -> Result<rayon::ThreadPool, rayon::ThreadPoolBuildError> {
    let num_threads = match std::env::var("WORKER_THREADS") {
        Ok(val) => val.parse().unwrap_or_else(|_| num_cpus::get()),
        Err(_) => {
            // Default sizing based on workload
            let cpu_count = num_cpus::get();
            if cfg!(debug_assertions) {
                // Fewer threads in debug mode
                std::cmp::min(4, cpu_count)
            } else {
                // Use all cores in release mode
                cpu_count
            }
        }
    };
    
    ThreadPoolBuilder::new()
        .num_threads(num_threads)
        .build()
}
```

### 2. Resource Management

```rust
use rayon::ThreadPoolBuilder;
use std::sync::Arc;

struct PoolResources {
    database_pool: Arc<DatabasePool>,
    cache: Arc<Cache>,
}

fn create_resource_aware_pool() -> Result<rayon::ThreadPool, rayon::ThreadPoolBuildError> {
    let resources = Arc::new(PoolResources {
        database_pool: Arc::new(create_database_pool()),
        cache: Arc::new(create_cache()),
    });
    
    ThreadPoolBuilder::new()
        .start_handler({
            let resources = Arc::clone(&resources);
            move |_index| {
                // Initialize thread-local resources
                THREAD_LOCAL_DB.with(|db| {
                    *db.borrow_mut() = Some(resources.database_pool.get_connection());
                });
            }
        })
        .exit_handler(|_index| {
            // Clean up thread-local resources
            THREAD_LOCAL_DB.with(|db| {
                if let Some(conn) = db.borrow_mut().take() {
                    conn.close();
                }
            });
        })
        .build()
}

// Mock types for example
struct DatabasePool;
struct Cache;
impl DatabasePool {
    fn get_connection(&self) -> DatabaseConnection { DatabaseConnection }
}
struct DatabaseConnection;
impl DatabaseConnection {
    fn close(self) {}
}
fn create_database_pool() -> DatabasePool { DatabasePool }
fn create_cache() -> Cache { Cache }

thread_local! {
    static THREAD_LOCAL_DB: std::cell::RefCell<Option<DatabaseConnection>> = 
        std::cell::RefCell::new(None);
}
```

### 3. Error Handling

```rust
use rayon::ThreadPoolBuilder;

fn create_robust_pool() -> Result<rayon::ThreadPool, rayon::ThreadPoolBuildError> {
    ThreadPoolBuilder::new()
        .panic_handler(|panic_info| {
            // Log panic with context
            log::error!("Worker thread panic: {:?}", panic_info);
            
            // Increment panic counter for monitoring
            PANIC_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            
            // Don't abort - let the pool continue with remaining threads
        })
        .spawn_handler(|thread| {
            // Retry thread creation on failure
            for attempt in 0..3 {
                match std::thread::Builder::new()
                    .name(format!("worker-{}", thread.index()))
                    .spawn(|| thread.run())
                {
                    Ok(_) => return Ok(()),
                    Err(e) if attempt < 2 => {
                        log::warn!("Thread spawn attempt {} failed: {}", attempt + 1, e);
                        std::thread::sleep(std::time::Duration::from_millis(100));
                    }
                    Err(e) => return Err(e),
                }
            }
            unreachable!()
        })
        .build()
}

static PANIC_COUNTER: std::sync::atomic::AtomicUsize = 
    std::sync::atomic::AtomicUsize::new(0);
```

## Performance Considerations

### Thread Pool Overhead

```rust
use rayon::ThreadPoolBuilder;
use std::time::Instant;

fn benchmark_pool_overhead() {
    let pool = ThreadPoolBuilder::new()
        .num_threads(4)
        .build()
        .unwrap();
    
    // Measure pool installation overhead
    let start = Instant::now();
    for _ in 0..1000 {
        pool.install(|| {
            // Minimal work
            1 + 1
        });
    }
    let overhead = start.elapsed();
    println!("Pool installation overhead: {:?}", overhead);
    
    // Compare with direct execution
    let start = Instant::now();
    for _ in 0..1000 {
        1 + 1;
    }
    let direct = start.elapsed();
    println!("Direct execution: {:?}", direct);
    
    println!("Overhead ratio: {:.2}x", 
             overhead.as_nanos() as f64 / direct.as_nanos() as f64);
}
```

### Memory Usage

```rust
use rayon::ThreadPoolBuilder;

fn memory_efficient_pool() -> Result<rayon::ThreadPool, rayon::ThreadPoolBuildError> {
    ThreadPoolBuilder::new()
        .num_threads(4)
        .stack_size(1024 * 1024) // 1MB stack (smaller than default)
        .start_handler(|_| {
            // Minimize thread-local allocations
            // Pre-allocate commonly used buffers
        })
        .build()
}
```

## Integration Patterns

### With Async Runtime

```rust
use rayon::ThreadPoolBuilder;
use tokio::task;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create dedicated CPU pool separate from async runtime
    let cpu_pool = ThreadPoolBuilder::new()
        .num_threads(num_cpus::get())
        .thread_name(|index| format!("cpu-{}", index))
        .build()?;
    
    // Offload CPU work to dedicated pool
    let result = task::spawn_blocking(move || {
        cpu_pool.install(|| {
            (0..1_000_000).into_par_iter()
                .map(|x| expensive_computation(x))
                .sum::<i32>()
        })
    }).await?;
    
    println!("CPU computation result: {}", result);
    Ok(())
}

fn expensive_computation(x: i32) -> i32 {
    (0..1000).fold(x, |acc, i| acc.wrapping_mul(i + 1))
}
```

### With Web Servers

```rust
use rayon::ThreadPoolBuilder;

fn setup_web_server_pools() -> Result<(rayon::ThreadPool, rayon::ThreadPool), rayon::ThreadPoolBuildError> {
    // Request processing pool
    let request_pool = ThreadPoolBuilder::new()
        .num_threads(num_cpus::get())
        .thread_name(|index| format!("request-{}", index))
        .build()?;
    
    // Background task pool
    let background_pool = ThreadPoolBuilder::new()
        .num_threads(2)
        .thread_name(|index| format!("background-{}", index))
        .build()?;
    
    Ok((request_pool, background_pool))
}
```

## Debugging Thread Pools

### Thread Pool Inspection

```rust
use rayon::ThreadPoolBuilder;

fn inspect_pool() -> Result<(), rayon::ThreadPoolBuildError> {
    let pool = ThreadPoolBuilder::new()
        .num_threads(4)
        .build()?;
    
    pool.install(|| {
        println!("Current thread index: {:?}", rayon::current_thread_index());
        println!("Total threads: {}", rayon::current_num_threads());
        
        // Check if we're in a rayon thread
        if rayon::current_thread_index().is_some() {
            println!("Running in Rayon thread pool");
        } else {
            println!("Not in Rayon thread pool");
        }
    });
    
    Ok(())
}
```

### Performance Monitoring

```rust
use rayon::ThreadPoolBuilder;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Instant;

fn monitored_pool() -> Result<rayon::ThreadPool, rayon::ThreadPoolBuildError> {
    let task_counter = Arc::new(AtomicUsize::new(0));
    let start_time = Arc::new(Instant::now());
    
    let counter_clone = Arc::clone(&task_counter);
    let time_clone = Arc::clone(&start_time);
    
    ThreadPoolBuilder::new()
        .start_handler(move |index| {
            println!("Thread {} started at {:?}", index, time_clone.elapsed());
        })
        .exit_handler(move |index| {
            let tasks = counter_clone.load(Ordering::Relaxed);
            println!("Thread {} exiting after {} tasks", index, tasks);
        })
        .build()
}
```

## Next Steps

Understanding thread pool configuration enables you to:
- Optimize resource usage for your specific workload
- Integrate Rayon with other systems effectively
- Debug performance issues
- Handle errors gracefully

Continue with:
- [Parallel Algorithms](04-algorithms.md) - Common patterns and techniques
- [Performance Tuning](05-performance.md) - Optimization strategies
- [Debugging](06-debugging.md) - Troubleshooting parallel execution