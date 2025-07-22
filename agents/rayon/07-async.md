# Integrating Rayon with Async Code

## Introduction

Combining Rayon's data parallelism with async programming requires careful consideration. This guide covers best practices for using Rayon in async contexts.

## Key Principles

### 1. Separate CPU and I/O Work

```rust
use rayon::prelude::*;
use tokio::task;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // I/O-bound work stays async
    let data = fetch_data_async().await?;
    
    // CPU-bound work goes to Rayon
    let processed = task::spawn_blocking(move || {
        data.into_par_iter()
            .map(|item| expensive_cpu_work(item))
            .collect::<Vec<_>>()
    }).await?;
    
    // Back to async for I/O
    save_results_async(processed).await?;
    
    Ok(())
}

async fn fetch_data_async() -> Result<Vec<i32>, Box<dyn std::error::Error>> {
    // Simulate async I/O
    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
    Ok((0..10000).collect())
}

async fn save_results_async(data: Vec<i32>) -> Result<(), Box<dyn std::error::Error>> {
    // Simulate async I/O
    tokio::time::sleep(tokio::time::Duration::from_millis(50)).await;
    println!("Saved {} results", data.len());
    Ok(())
}

fn expensive_cpu_work(x: i32) -> i32 {
    (0..1000).fold(x, |acc, i| acc.wrapping_mul(i + 1))
}
```

### 2. Use spawn_blocking for CPU Work

```rust
use rayon::prelude::*;
use tokio::task;

async fn parallel_processing_in_async() -> Result<Vec<String>, Box<dyn std::error::Error>> {
    let data: Vec<i32> = (0..100000).collect();
    
    // Move CPU-intensive work to blocking thread pool
    let results = task::spawn_blocking(move || {
        data.into_par_iter()
            .filter(|&x| x % 2 == 0)
            .map(|x| format!("processed_{}", x * x))
            .collect::<Vec<_>>()
    }).await?;
    
    Ok(results)
}

// Multiple parallel tasks
async fn multiple_parallel_tasks() -> Result<(), Box<dyn std::error::Error>> {
    let data1: Vec<i32> = (0..50000).collect();
    let data2: Vec<i32> = (50000..100000).collect();
    
    // Run multiple CPU tasks concurrently
    let (results1, results2) = tokio::try_join!(
        task::spawn_blocking(move || {
            data1.into_par_iter().map(|x| x * 2).collect::<Vec<_>>()
        }),
        task::spawn_blocking(move || {
            data2.into_par_iter().map(|x| x * 3).collect::<Vec<_>>()
        })
    )?;
    
    println!("Processed {} and {} items", results1.len(), results2.len());
    Ok(())
}
```

## Async-Aware Patterns

### 1. Streaming with Async

```rust
use rayon::prelude::*;
use tokio::sync::mpsc;
use tokio_stream::{wrappers::ReceiverStream, StreamExt};

async fn async_streaming_processing() -> Result<(), Box<dyn std::error::Error>> {
    let (tx, rx) = mpsc::channel(100);
    
    // Producer task
    let producer = tokio::spawn(async move {
        for batch in (0..100000).collect::<Vec<_>>().chunks(1000) {
            let batch = batch.to_vec();
            
            // Process batch in parallel
            let processed = tokio::task::spawn_blocking(move || {
                batch.into_par_iter()
                    .map(|x| x * x)
                    .collect::<Vec<_>>()
            }).await.unwrap();
            
            if tx.send(processed).await.is_err() {
                break;
            }
        }
    });
    
    // Consumer stream
    let mut stream = ReceiverStream::new(rx);
    let mut total_processed = 0;
    
    while let Some(batch) = stream.next().await {
        total_processed += batch.len();
        
        // Simulate async processing of results
        tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
        
        if total_processed % 10000 == 0 {
            println!("Processed {} items so far", total_processed);
        }
    }
    
    producer.await?;
    println!("Total processed: {}", total_processed);
    
    Ok(())
}
```

### 2. Async Iterator Bridge

```rust
use rayon::prelude::*;
use tokio::task;
use futures::stream::{self, StreamExt};

// Bridge between async streams and parallel processing
async fn async_parallel_bridge<T, R>(
    items: Vec<T>,
    async_processor: impl Fn(Vec<R>) -> futures::future::BoxFuture<'static, Result<(), Box<dyn std::error::Error + Send + Sync>>> + Send + Sync + 'static,
    parallel_processor: impl Fn(T) -> R + Send + Sync + 'static,
) -> Result<(), Box<dyn std::error::Error>>
where
    T: Send + 'static,
    R: Send + 'static,
{
    const BATCH_SIZE: usize = 1000;
    
    let batches: Vec<Vec<T>> = items.chunks(BATCH_SIZE)
        .map(|chunk| chunk.to_vec())
        .collect();
    
    // Process batches as a stream
    let stream = stream::iter(batches)
        .map(move |batch| {
            let processor = &parallel_processor;
            task::spawn_blocking(move || {
                batch.into_par_iter()
                    .map(processor)
                    .collect::<Vec<_>>()
            })
        })
        .buffer_unordered(4); // Process up to 4 batches concurrently
    
    tokio::pin!(stream);
    
    while let Some(batch_result) = stream.next().await {
        let processed_batch = batch_result?;
        async_processor(processed_batch).await?;
    }
    
    Ok(())
}

// Usage example
async fn example_async_parallel_bridge() -> Result<(), Box<dyn std::error::Error>> {
    let data: Vec<i32> = (0..50000).collect();
    
    async_parallel_bridge(
        data,
        |batch| Box::pin(async move {
            // Async processing of results
            println!("Saving batch of {} items", batch.len());
            tokio::time::sleep(tokio::time::Duration::from_millis(50)).await;
            Ok(())
        }),
        |x| x * x, // Parallel processing
    ).await?;
    
    Ok(())
}
```

## Thread Pool Integration

### 1. Dedicated Thread Pools

```rust
use rayon::{ThreadPool, ThreadPoolBuilder};
use tokio::task;

struct AsyncRayonService {
    cpu_pool: ThreadPool,
}

impl AsyncRayonService {
    fn new() -> Result<Self, rayon::ThreadPoolBuildError> {
        let cpu_pool = ThreadPoolBuilder::new()
            .num_threads(num_cpus::get())
            .thread_name(|i| format!("cpu-worker-{}", i))
            .build()?;
        
        Ok(Self { cpu_pool })
    }
    
    async fn process_data(&self, data: Vec<i32>) -> Result<Vec<i32>, Box<dyn std::error::Error>> {
        let pool = &self.cpu_pool;
        
        let result = task::spawn_blocking(move || {
            pool.install(|| {
                data.into_par_iter()
                    .filter(|&x| x % 2 == 0)
                    .map(|x| x * x)
                    .collect::<Vec<_>>()
            })
        }).await?;
        
        Ok(result)
    }
    
    async fn parallel_reduce(&self, data: Vec<f64>) -> Result<f64, Box<dyn std::error::Error>> {
        let pool = &self.cpu_pool;
        
        let result = task::spawn_blocking(move || {
            pool.install(|| {
                data.into_par_iter()
                    .map(|x| x.powi(2))
                    .sum::<f64>()
            })
        }).await?;
        
        Ok(result)
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let service = AsyncRayonService::new()?;
    
    let data: Vec<i32> = (0..100000).collect();
    let processed = service.process_data(data).await?;
    
    println!("Processed {} items", processed.len());
    
    Ok(())
}
```

### 2. Resource Management

```rust
use rayon::{ThreadPool, ThreadPoolBuilder};
use tokio::sync::Semaphore;
use std::sync::Arc;

struct ManagedAsyncRayon {
    cpu_pool: ThreadPool,
    semaphore: Arc<Semaphore>,
}

impl ManagedAsyncRayon {
    fn new(max_concurrent_tasks: usize) -> Result<Self, rayon::ThreadPoolBuildError> {
        let cpu_pool = ThreadPoolBuilder::new()
            .num_threads(num_cpus::get())
            .build()?;
        
        let semaphore = Arc::new(Semaphore::new(max_concurrent_tasks));
        
        Ok(Self { cpu_pool, semaphore })
    }
    
    async fn process_with_backpressure<T, R>(
        &self,
        data: Vec<T>,
        processor: impl Fn(T) -> R + Send + Sync + 'static,
    ) -> Result<Vec<R>, Box<dyn std::error::Error>>
    where
        T: Send + 'static,
        R: Send + 'static,
    {
        // Acquire permit to limit concurrent CPU tasks
        let _permit = self.semaphore.acquire().await?;
        
        let pool = &self.cpu_pool;
        let result = tokio::task::spawn_blocking(move || {
            pool.install(|| {
                data.into_par_iter()
                    .map(processor)
                    .collect::<Vec<_>>()
            })
        }).await?;
        
        Ok(result)
    }
}

async fn example_managed_processing() -> Result<(), Box<dyn std::error::Error>> {
    let manager = ManagedAsyncRayon::new(2)?; // Max 2 concurrent CPU tasks
    
    // Launch multiple tasks - only 2 will run concurrently
    let tasks = (0..5).map(|i| {
        let data: Vec<i32> = (i * 10000..(i + 1) * 10000).collect();
        manager.process_with_backpressure(data, |x| x * x)
    });
    
    let results = futures::future::try_join_all(tasks).await?;
    
    for (i, result) in results.iter().enumerate() {
        println!("Task {}: processed {} items", i, result.len());
    }
    
    Ok(())
}
```

## Error Handling in Async Context

### 1. Graceful Error Propagation

```rust
use rayon::prelude::*;
use tokio::task;

#[derive(Debug)]
enum ProcessingError {
    ComputationError(String),
    AsyncError(String),
}

impl std::fmt::Display for ProcessingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProcessingError::ComputationError(msg) => write!(f, "Computation error: {}", msg),
            ProcessingError::AsyncError(msg) => write!(f, "Async error: {}", msg),
        }
    }
}

impl std::error::Error for ProcessingError {}

async fn robust_async_parallel_processing(
    data: Vec<i32>
) -> Result<Vec<String>, ProcessingError> {
    // Parallel processing with error handling
    let results = task::spawn_blocking(move || {
        let results: Result<Vec<String>, String> = data
            .into_par_iter()
            .map(|x| {
                if x < 0 {
                    Err(format!("Negative number not allowed: {}", x))
                } else {
                    Ok(format!("processed_{}", x))
                }
            })
            .collect(); // This will stop on first error
        
        results
    }).await
    .map_err(|e| ProcessingError::AsyncError(format!("Task join error: {}", e)))?
    .map_err(ProcessingError::ComputationError)?;
    
    Ok(results)
}

// Collect all errors instead of failing fast
async fn collect_all_errors_async(
    data: Vec<i32>
) -> Result<(Vec<String>, Vec<String>), ProcessingError> {
    let results = task::spawn_blocking(move || {
        let results: Vec<Result<String, String>> = data
            .into_par_iter()
            .map(|x| {
                if x < 0 {
                    Err(format!("Negative number: {}", x))
                } else {
                    Ok(format!("processed_{}", x))
                }
            })
            .collect();
        
        let (successes, errors): (Vec<_>, Vec<_>) = results
            .into_iter()
            .partition(Result::is_ok);
        
        let successes: Vec<String> = successes.into_iter().map(Result::unwrap).collect();
        let errors: Vec<String> = errors.into_iter().map(Result::unwrap_err).collect();
        
        (successes, errors)
    }).await
    .map_err(|e| ProcessingError::AsyncError(format!("Task join error: {}", e)))?;
    
    Ok(results)
}
```

### 2. Timeout Handling

```rust
use rayon::prelude::*;
use tokio::{task, time};

async fn parallel_processing_with_timeout(
    data: Vec<i32>,
    timeout_duration: std::time::Duration,
) -> Result<Vec<i32>, Box<dyn std::error::Error>> {
    let processing_task = task::spawn_blocking(move || {
        data.into_par_iter()
            .map(|x| {
                // Simulate potentially slow work
                std::thread::sleep(std::time::Duration::from_millis(1));
                x * x
            })
            .collect::<Vec<_>>()
    });
    
    match time::timeout(timeout_duration, processing_task).await {
        Ok(Ok(results)) => Ok(results),
        Ok(Err(join_error)) => Err(format!("Task failed: {}", join_error).into()),
        Err(_) => Err("Processing timed out".into()),
    }
}

async fn example_timeout_handling() -> Result<(), Box<dyn std::error::Error>> {
    let data: Vec<i32> = (0..10000).collect();
    
    match parallel_processing_with_timeout(data, std::time::Duration::from_secs(5)).await {
        Ok(results) => println!("Processed {} items", results.len()),
        Err(e) => eprintln!("Processing failed: {}", e),
    }
    
    Ok(())
}
```

## Performance Considerations

### 1. Avoiding Thread Pool Exhaustion

```rust
use rayon::prelude::*;
use tokio::{task, sync::Semaphore};
use std::sync::Arc;

async fn prevent_thread_pool_exhaustion() -> Result<(), Box<dyn std::error::Error>> {
    // Limit concurrent blocking tasks to prevent exhausting the thread pool
    let semaphore = Arc::new(Semaphore::new(4));
    
    let tasks = (0..20).map(|i| {
        let semaphore = Arc::clone(&semaphore);
        async move {
            let _permit = semaphore.acquire().await?;
            
            let data: Vec<i32> = (i * 1000..(i + 1) * 1000).collect();
            
            task::spawn_blocking(move || {
                data.into_par_iter()
                    .map(|x| expensive_computation(x))
                    .sum::<i32>()
            }).await
        }
    });
    
    let results = futures::future::try_join_all(tasks).await?;
    
    println!("Completed {} tasks", results.len());
    Ok(())
}

fn expensive_computation(x: i32) -> i32 {
    (0..1000).fold(x, |acc, i| acc.wrapping_mul(i + 1))
}
```

### 2. Optimal Batch Sizing

```rust
use rayon::prelude::*;
use tokio::task;

async fn optimal_batching(data: Vec<i32>) -> Result<Vec<i32>, Box<dyn std::error::Error>> {
    // Calculate optimal batch size based on data size and available parallelism
    let total_items = data.len();
    let num_threads = rayon::current_num_threads();
    let target_batches = num_threads * 4; // 4x oversubscription
    let batch_size = std::cmp::max(1000, total_items / target_batches);
    
    let batches: Vec<Vec<i32>> = data.chunks(batch_size)
        .map(|chunk| chunk.to_vec())
        .collect();
    
    println!("Processing {} items in {} batches of ~{} items each", 
             total_items, batches.len(), batch_size);
    
    let mut results = Vec::new();
    
    for batch in batches {
        let batch_result = task::spawn_blocking(move || {
            batch.into_par_iter()
                .map(|x| x * x)
                .collect::<Vec<_>>()
        }).await?;
        
        results.extend(batch_result);
    }
    
    Ok(results)
}
```

## Best Practices

### 1. Clear Separation of Concerns

```rust
use rayon::prelude::*;
use tokio::task;

// Good: Clear separation between async I/O and parallel CPU work
struct DataProcessor {
    // CPU-intensive operations
    pub fn process_parallel(data: Vec<i32>) -> Vec<i32> {
        data.into_par_iter()
            .filter(|&x| x % 2 == 0)
            .map(|x| x * x)
            .collect()
    }
    
    pub fn aggregate_parallel(data: Vec<f64>) -> f64 {
        data.into_par_iter().sum()
    }
}

struct AsyncService;

impl AsyncService {
    // I/O-bound operations
    async fn fetch_data(&self) -> Result<Vec<i32>, Box<dyn std::error::Error>> {
        // Simulate async I/O
        tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
        Ok((0..100000).collect())
    }
    
    async fn save_results(&self, data: Vec<i32>) -> Result<(), Box<dyn std::error::Error>> {
        // Simulate async I/O
        tokio::time::sleep(tokio::time::Duration::from_millis(50)).await;
        println!("Saved {} results", data.len());
        Ok(())
    }
    
    // Orchestration
    async fn process_workflow(&self) -> Result<(), Box<dyn std::error::Error>> {
        // Async I/O
        let data = self.fetch_data().await?;
        
        // CPU work in blocking task
        let processed = task::spawn_blocking(move || {
            DataProcessor::process_parallel(data)
        }).await?;
        
        // Back to async I/O
        self.save_results(processed).await?;
        
        Ok(())
    }
}
```

### 2. Resource Management

```rust
use rayon::{ThreadPool, ThreadPoolBuilder};
use tokio::sync::RwLock;
use std::sync::Arc;

#[derive(Clone)]
struct SharedResourceManager {
    cpu_pool: Arc<ThreadPool>,
    active_tasks: Arc<RwLock<usize>>,
    max_tasks: usize,
}

impl SharedResourceManager {
    fn new(max_tasks: usize) -> Result<Self, rayon::ThreadPoolBuildError> {
        let cpu_pool = Arc::new(
            ThreadPoolBuilder::new()
                .num_threads(num_cpus::get())
                .build()?
        );
        
        Ok(Self {
            cpu_pool,
            active_tasks: Arc::new(RwLock::new(0)),
            max_tasks,
        })
    }
    
    async fn execute_parallel<T, R, F>(
        &self,
        data: T,
        processor: F,
    ) -> Result<R, Box<dyn std::error::Error>>
    where
        T: Send + 'static,
        R: Send + 'static,
        F: FnOnce(T) -> R + Send + 'static,
    {
        // Check if we can accept more tasks
        {
            let active = self.active_tasks.read().await;
            if *active >= self.max_tasks {
                return Err("Too many active tasks".into());
            }
        }
        
        // Increment active task count
        {
            let mut active = self.active_tasks.write().await;
            *active += 1;
        }
        
        let pool = Arc::clone(&self.cpu_pool);
        let active_tasks = Arc::clone(&self.active_tasks);
        
        let result = tokio::task::spawn_blocking(move || {
            let result = pool.install(|| processor(data));
            
            // Decrement active task count
            tokio::runtime::Handle::current().block_on(async {
                let mut active = active_tasks.write().await;
                *active -= 1;
            });
            
            result
        }).await?;
        
        Ok(result)
    }
}
```

## Common Pitfalls and Solutions

### 1. Blocking the Async Runtime

```rust
// BAD: Blocking the async runtime
async fn bad_example() {
    let data: Vec<i32> = (0..100000).collect();
    
    // This blocks the async runtime!
    let result: i32 = data.into_par_iter().sum();
    
    println!("Result: {}", result);
}

// GOOD: Use spawn_blocking
async fn good_example() {
    let data: Vec<i32> = (0..100000).collect();
    
    let result = tokio::task::spawn_blocking(move || {
        data.into_par_iter().sum::<i32>()
    }).await.unwrap();
    
    println!("Result: {}", result);
}
```

### 2. Mixing Async and Sync Incorrectly

```rust
// BAD: Trying to use async inside parallel iterator
async fn bad_async_in_parallel() {
    let data: Vec<i32> = (0..100).collect();
    
    // This won't compile - async closures not supported
    // let results: Vec<String> = data.into_par_iter()
    //     .map(|x| async { format!("item_{}", x) })
    //     .collect();
}

// GOOD: Separate parallel and async processing
async fn good_async_parallel_separation() {
    let data: Vec<i32> = (0..100).collect();
    
    // Parallel processing first
    let processed = tokio::task::spawn_blocking(move || {
        data.into_par_iter()
            .map(|x| x * x)
            .collect::<Vec<_>>()
    }).await.unwrap();
    
    // Then async processing
    let mut results = Vec::new();
    for item in processed {
        let formatted = format!("item_{}", item);
        results.push(formatted);
    }
}
```

## Summary

- Use `spawn_blocking` to move CPU work off the async runtime
- Keep I/O async and CPU work parallel
- Manage resource usage with semaphores and limits
- Handle errors gracefully in both domains
- Avoid blocking the async runtime with parallel operations
- Use dedicated thread pools for better resource control

## Next Steps

- [Custom Iterators](08-custom-iterators.md) - Build your own parallel iterators
- [API Reference](09-api-reference.md) - Complete API documentation