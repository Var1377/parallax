# Performance Tuning with Rayon

## Introduction

This guide covers strategies for optimizing parallel code performance with Rayon. Understanding these techniques will help you achieve maximum speedup and efficiency.

## Performance Fundamentals

### Understanding Parallel Overhead

Every parallel operation has overhead costs:

```rust
use rayon::prelude::*;
use std::time::Instant;

fn measure_overhead() {
    let data: Vec<i32> = (0..1000).collect();
    
    // Sequential baseline
    let start = Instant::now();
    let seq_sum: i32 = data.iter().sum();
    let seq_time = start.elapsed();
    
    // Parallel version
    let start = Instant::now();
    let par_sum: i32 = data.par_iter().sum();
    let par_time = start.elapsed();
    
    println!("Sequential: {:?}", seq_time);
    println!("Parallel: {:?}", par_time);
    println!("Overhead: {:.2}x", par_time.as_nanos() as f64 / seq_time.as_nanos() as f64);
    
    assert_eq!(seq_sum, par_sum);
}
```

### Amdahl's Law in Practice

```rust
use rayon::prelude::*;

fn amdahls_law_demo() {
    let data: Vec<f64> = (0..1_000_000).map(|x| x as f64).collect();
    
    // 90% parallel, 10% sequential
    let start = std::time::Instant::now();
    
    // Sequential part (10%)
    let setup_cost = data.len() as f64 * 0.1;
    
    // Parallel part (90%)
    let parallel_result: f64 = data
        .par_iter()
        .map(|&x| expensive_computation(x))
        .sum();
    
    let total_time = start.elapsed();
    
    // Theoretical maximum speedup with infinite cores: 1 / (0.1) = 10x
    println!("Result: {}, Time: {:?}", parallel_result + setup_cost, total_time);
}

fn expensive_computation(x: f64) -> f64 {
    (0..100).fold(x, |acc, i| acc + (i as f64).sin())
}
```

## Granularity Optimization

### Finding the Right Chunk Size

```rust
use rayon::prelude::*;
use std::time::Instant;

fn benchmark_chunk_sizes(data: &[i32]) {
    let chunk_sizes = [1, 10, 100, 1000, 10000];
    
    for &chunk_size in &chunk_sizes {
        let start = Instant::now();
        
        let result: i32 = data
            .par_chunks(chunk_size)
            .map(|chunk| chunk.iter().map(|&x| expensive_work(x)).sum::<i32>())
            .sum();
        
        let elapsed = start.elapsed();
        println!("Chunk size {}: {:?} (result: {})", chunk_size, elapsed, result);
    }
}

fn expensive_work(x: i32) -> i32 {
    // Simulate work that takes ~1μs
    (0..100).fold(x, |acc, i| acc.wrapping_mul(i + 1))
}

// Adaptive chunk sizing
fn adaptive_parallel_map<T, R, F>(data: Vec<T>, f: F) -> Vec<R>
where
    T: Send,
    R: Send,
    F: Fn(T) -> R + Sync,
{
    let num_threads = rayon::current_num_threads();
    let optimal_chunks = num_threads * 4; // 4x oversubscription
    let chunk_size = std::cmp::max(1, data.len() / optimal_chunks);
    
    data.par_chunks(chunk_size)
        .flat_map(|chunk| chunk.iter().map(&f).collect::<Vec<_>>())
        .collect()
}
```

### Work Granularity Guidelines

```rust
use rayon::prelude::*;

// TOO FINE: Each task is too small
fn too_fine_grained(data: &[i32]) -> i32 {
    data.par_iter()
        .map(|&x| x + 1) // Trivial operation
        .sum()
}

// BETTER: Batch small operations
fn better_granularity(data: &[i32]) -> i32 {
    data.par_chunks(1000)
        .map(|chunk| chunk.iter().map(|&x| x + 1).sum::<i32>())
        .sum()
}

// OPTIMAL: Right-sized work units
fn optimal_granularity(data: &[i32]) -> i32 {
    let work_per_item = estimate_work_complexity();
    let target_work_per_thread = 10_000; // microseconds
    let chunk_size = target_work_per_thread / work_per_item;
    
    data.par_chunks(chunk_size.max(1))
        .map(|chunk| {
            chunk.iter().map(|&x| expensive_computation_int(x)).sum::<i32>()
        })
        .sum()
}

fn estimate_work_complexity() -> usize {
    // Return estimated microseconds per item
    10 // 10μs per item
}

fn expensive_computation_int(x: i32) -> i32 {
    (0..1000).fold(x, |acc, i| acc.wrapping_add(i))
}
```

## Memory Optimization

### Cache-Friendly Access Patterns

```rust
use rayon::prelude::*;

// BAD: Poor cache locality
fn poor_cache_usage(matrix: &[Vec<i32>]) -> Vec<i32> {
    (0..matrix[0].len())
        .into_par_iter()
        .map(|col| {
            // Accessing column-wise (poor cache locality)
            matrix.iter().map(|row| row[col]).sum()
        })
        .collect()
}

// GOOD: Better cache locality
fn good_cache_usage(matrix: &[Vec<i32>]) -> Vec<i32> {
    let num_cols = matrix[0].len();
    let mut col_sums = vec![0; num_cols];
    
    // Process row-wise in parallel (good cache locality)
    let partial_sums: Vec<Vec<i32>> = matrix
        .par_chunks(100) // Process multiple rows per thread
        .map(|chunk| {
            let mut local_sums = vec![0; num_cols];
            for row in chunk {
                for (j, &val) in row.iter().enumerate() {
                    local_sums[j] += val;
                }
            }
            local_sums
        })
        .collect();
    
    // Combine partial results
    for partial in partial_sums {
        for (i, sum) in partial.into_iter().enumerate() {
            col_sums[i] += sum;
        }
    }
    
    col_sums
}

// Memory-efficient streaming
fn memory_efficient_processing(data: &[f64]) -> f64 {
    // Avoid intermediate collections
    data.par_iter()
        .filter(|&&x| x > 0.0)
        .map(|&x| x.sqrt())
        .filter(|&x| x < 100.0)
        .sum() // Direct reduction
}
```

### Reducing Allocations

```rust
use rayon::prelude::*;

// BAD: Many allocations
fn many_allocations(data: &[String]) -> Vec<String> {
    data.par_iter()
        .map(|s| format!("processed: {}", s)) // New allocation per item
        .collect()
}

// BETTER: Reuse buffers
fn reuse_buffers(data: &[String]) -> Vec<String> {
    data.par_chunks(1000)
        .flat_map(|chunk| {
            let mut buffer = String::with_capacity(1000);
            chunk.iter().map(|s| {
                buffer.clear();
                buffer.push_str("processed: ");
                buffer.push_str(s);
                buffer.clone() // Still allocates, but reuses capacity
            }).collect::<Vec<_>>()
        })
        .collect()
}

// BEST: Pre-allocated output
fn preallocated_output(data: &[String]) -> Vec<String> {
    let mut result = Vec::with_capacity(data.len());
    result.resize(data.len(), String::new());
    
    result.par_iter_mut().zip(data).for_each(|(output, input)| {
        output.clear();
        output.push_str("processed: ");
        output.push_str(input);
    });
    
    result
}
```

## Load Balancing

### Handling Uneven Workloads

```rust
use rayon::prelude::*;
use std::time::Duration;

#[derive(Debug)]
struct Task {
    id: usize,
    complexity: usize,
}

impl Task {
    fn process(&self) -> String {
        // Simulate variable work
        std::thread::sleep(Duration::from_millis(self.complexity as u64));
        format!("Task {} completed", self.id)
    }
}

// BAD: Static partitioning with uneven work
fn static_partitioning(tasks: Vec<Task>) -> Vec<String> {
    tasks.into_par_iter()
        .map(|task| task.process())
        .collect()
}

// GOOD: Sort by complexity (largest first)
fn complexity_sorted(mut tasks: Vec<Task>) -> Vec<String> {
    tasks.par_sort_by_key(|task| std::cmp::Reverse(task.complexity));
    
    tasks.into_par_iter()
        .map(|task| task.process())
        .collect()
}

// BETTER: Dynamic work stealing with smaller chunks
fn dynamic_work_stealing(tasks: Vec<Task>) -> Vec<String> {
    // Smaller chunks enable better work stealing
    tasks.into_par_iter()
        .with_min_len(1) // Force fine-grained work distribution
        .map(|task| task.process())
        .collect()
}

// BEST: Adaptive load balancing
fn adaptive_load_balancing(tasks: Vec<Task>) -> Vec<String> {
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;
    
    let completed = Arc::new(AtomicUsize::new(0));
    let total = tasks.len();
    
    let results: Vec<String> = tasks
        .into_par_iter()
        .map(|task| {
            let result = task.process();
            let done = completed.fetch_add(1, Ordering::Relaxed) + 1;
            
            if done % 100 == 0 {
                println!("Progress: {}/{}", done, total);
            }
            
            result
        })
        .collect();
    
    results
}
```

### Custom Work Distribution

```rust
use rayon::prelude::*;

// Custom work distribution for specific patterns
fn custom_work_distribution<T, R>(
    data: Vec<T>,
    work_estimator: impl Fn(&T) -> usize + Sync,
    processor: impl Fn(T) -> R + Sync,
) -> Vec<R>
where
    T: Send,
    R: Send,
{
    // Group items by estimated work complexity
    let mut work_groups: Vec<Vec<T>> = vec![Vec::new(); 4];
    
    for item in data {
        let complexity = work_estimator(&item);
        let group = match complexity {
            0..=10 => 0,      // Light work
            11..=100 => 1,    // Medium work
            101..=1000 => 2,  // Heavy work
            _ => 3,           // Very heavy work
        };
        work_groups[group].push(item);
    }
    
    // Process each group with appropriate parallelism
    let mut results = Vec::new();
    
    // Light work: larger chunks
    results.extend(
        work_groups[0]
            .into_par_iter()
            .with_min_len(1000)
            .map(&processor)
    );
    
    // Medium work: medium chunks
    results.extend(
        work_groups[1]
            .into_par_iter()
            .with_min_len(100)
            .map(&processor)
    );
    
    // Heavy work: small chunks
    results.extend(
        work_groups[2]
            .into_par_iter()
            .with_min_len(10)
            .map(&processor)
    );
    
    // Very heavy work: individual items
    results.extend(
        work_groups[3]
            .into_par_iter()
            .with_min_len(1)
            .map(&processor)
    );
    
    results
}
```

## Thread Pool Optimization

### Right-Sizing Thread Pools

```rust
use rayon::{ThreadPool, ThreadPoolBuilder};

fn create_optimized_pools() -> Result<(ThreadPool, ThreadPool), rayon::ThreadPoolBuildError> {
    let cpu_cores = num_cpus::get();
    
    // CPU-intensive pool: use all cores
    let cpu_pool = ThreadPoolBuilder::new()
        .num_threads(cpu_cores)
        .thread_name(|i| format!("cpu-{}", i))
        .build()?;
    
    // I/O-bound pool: fewer threads to avoid context switching
    let io_pool = ThreadPoolBuilder::new()
        .num_threads(std::cmp::min(4, cpu_cores))
        .thread_name(|i| format!("io-{}", i))
        .build()?;
    
    Ok((cpu_pool, io_pool))
}

// Workload-specific optimization
fn workload_specific_optimization() -> Result<(), rayon::ThreadPoolBuildError> {
    let (cpu_pool, io_pool) = create_optimized_pools()?;
    
    // CPU-intensive work
    let cpu_result = cpu_pool.install(|| {
        (0..1_000_000)
            .into_par_iter()
            .map(|x| expensive_cpu_work(x))
            .sum::<i64>()
    });
    
    // I/O-bound work
    let io_results = io_pool.install(|| {
        vec!["file1.txt", "file2.txt", "file3.txt"]
            .into_par_iter()
            .map(|filename| {
                // Simulate I/O
                std::thread::sleep(std::time::Duration::from_millis(100));
                format!("processed {}", filename)
            })
            .collect::<Vec<_>>()
    });
    
    println!("CPU result: {}", cpu_result);
    println!("I/O results: {:?}", io_results);
    
    Ok(())
}

fn expensive_cpu_work(x: i32) -> i64 {
    (0..1000).fold(x as i64, |acc, i| acc.wrapping_mul(i + 1))
}
```

### Thread Affinity and NUMA Awareness

```rust
use rayon::ThreadPoolBuilder;

fn numa_aware_pool() -> Result<rayon::ThreadPool, rayon::ThreadPoolBuildError> {
    ThreadPoolBuilder::new()
        .num_threads(num_cpus::get())
        .start_handler(|thread_index| {
            // Set thread affinity (platform-specific)
            #[cfg(target_os = "linux")]
            {
                // Example: bind threads to specific CPU cores
                // This is pseudocode - actual implementation would use libc
                // set_thread_affinity(thread_index);
            }
            
            println!("Thread {} started on core {}", thread_index, thread_index);
        })
        .build()
}

// Memory allocation optimization
fn memory_optimized_processing(data: &[f64]) -> Vec<f64> {
    // Pre-allocate result vector
    let mut result = Vec::with_capacity(data.len());
    unsafe { result.set_len(data.len()); }
    
    // Process in parallel, writing directly to pre-allocated memory
    result.par_iter_mut().zip(data).for_each(|(output, &input)| {
        *output = expensive_computation(input);
    });
    
    result
}

fn expensive_computation(x: f64) -> f64 {
    (0..100).fold(x, |acc, i| acc + (i as f64).sin())
}
```

## Profiling and Measurement

### Built-in Performance Monitoring

```rust
use rayon::prelude::*;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Instant;

struct PerformanceMonitor {
    tasks_completed: AtomicUsize,
    total_time: std::sync::Mutex<std::time::Duration>,
    start_time: Instant,
}

impl PerformanceMonitor {
    fn new() -> Self {
        Self {
            tasks_completed: AtomicUsize::new(0),
            total_time: std::sync::Mutex::new(std::time::Duration::ZERO),
            start_time: Instant::now(),
        }
    }
    
    fn record_task(&self, duration: std::time::Duration) {
        self.tasks_completed.fetch_add(1, Ordering::Relaxed);
        let mut total = self.total_time.lock().unwrap();
        *total += duration;
    }
    
    fn report(&self) {
        let completed = self.tasks_completed.load(Ordering::Relaxed);
        let total_time = *self.total_time.lock().unwrap();
        let wall_time = self.start_time.elapsed();
        
        println!("Tasks completed: {}", completed);
        println!("Total CPU time: {:?}", total_time);
        println!("Wall clock time: {:?}", wall_time);
        println!("Parallelism efficiency: {:.2}%", 
                 total_time.as_secs_f64() / wall_time.as_secs_f64() * 100.0);
    }
}

fn monitored_processing(data: Vec<i32>) -> Vec<String> {
    let monitor = Arc::new(PerformanceMonitor::new());
    
    let results: Vec<String> = data
        .into_par_iter()
        .map(|x| {
            let start = Instant::now();
            let result = format!("processed: {}", expensive_work(x));
            let duration = start.elapsed();
            
            monitor.record_task(duration);
            result
        })
        .collect();
    
    monitor.report();
    results
}
```

### Custom Benchmarking

```rust
use rayon::prelude::*;
use std::time::Instant;

fn benchmark_parallel_vs_sequential<T, R>(
    data: Vec<T>,
    sequential_fn: impl Fn(&[T]) -> R,
    parallel_fn: impl Fn(Vec<T>) -> R,
) where
    T: Clone,
    R: std::fmt::Debug + PartialEq,
{
    // Warm up
    let _ = parallel_fn(data.clone());
    
    // Sequential benchmark
    let start = Instant::now();
    let seq_result = sequential_fn(&data);
    let seq_time = start.elapsed();
    
    // Parallel benchmark
    let start = Instant::now();
    let par_result = parallel_fn(data.clone());
    let par_time = start.elapsed();
    
    // Verify results match
    assert_eq!(seq_result, par_result);
    
    let speedup = seq_time.as_secs_f64() / par_time.as_secs_f64();
    let efficiency = speedup / rayon::current_num_threads() as f64;
    
    println!("Sequential time: {:?}", seq_time);
    println!("Parallel time: {:?}", par_time);
    println!("Speedup: {:.2}x", speedup);
    println!("Efficiency: {:.2}%", efficiency * 100.0);
}

// Usage example
fn benchmark_example() {
    let data: Vec<i32> = (0..100_000).collect();
    
    benchmark_parallel_vs_sequential(
        data,
        |data| data.iter().map(|&x| expensive_work(x)).sum::<i32>(),
        |data| data.into_par_iter().map(|x| expensive_work(x)).sum::<i32>(),
    );
}
```

## Common Performance Pitfalls

### 1. Over-Parallelization

```rust
use rayon::prelude::*;

// BAD: Parallelizing trivial work
fn over_parallelized(data: &[i32]) -> i32 {
    data.par_iter().map(|&x| x + 1).sum() // Too simple for parallelism
}

// GOOD: Keep simple operations sequential
fn appropriately_sequential(data: &[i32]) -> i32 {
    data.iter().map(|&x| x + 1).sum()
}

// GOOD: Parallelize only when work is substantial
fn appropriately_parallel(data: &[i32]) -> i32 {
    if data.len() < 1000 {
        // Sequential for small datasets
        data.iter().map(|&x| expensive_work(x)).sum()
    } else {
        // Parallel for large datasets
        data.par_iter().map(|&x| expensive_work(x)).sum()
    }
}
```

### 2. False Sharing

```rust
use rayon::prelude::*;
use std::sync::atomic::{AtomicUsize, Ordering};

// BAD: False sharing between threads
fn false_sharing_bad(data: &[i32]) -> Vec<usize> {
    let counters = vec![AtomicUsize::new(0); 8];
    
    data.par_iter().for_each(|&x| {
        let index = (x as usize) % 8;
        counters[index].fetch_add(1, Ordering::Relaxed); // False sharing!
    });
    
    counters.into_iter().map(|c| c.load(Ordering::Relaxed)).collect()
}

// GOOD: Thread-local accumulation
fn false_sharing_good(data: &[i32]) -> Vec<usize> {
    let partial_results: Vec<Vec<usize>> = data
        .par_chunks(1000)
        .map(|chunk| {
            let mut local_counters = vec![0; 8];
            for &x in chunk {
                let index = (x as usize) % 8;
                local_counters[index] += 1;
            }
            local_counters
        })
        .collect();
    
    // Combine results
    let mut final_counters = vec![0; 8];
    for partial in partial_results {
        for (i, count) in partial.into_iter().enumerate() {
            final_counters[i] += count;
        }
    }
    
    final_counters
}
```

### 3. Excessive Synchronization

```rust
use rayon::prelude::*;
use std::sync::Mutex;

// BAD: Excessive locking
fn excessive_locking(data: &[i32]) -> Vec<String> {
    let results = Mutex::new(Vec::new());
    
    data.par_iter().for_each(|&x| {
        let processed = format!("item: {}", x);
        results.lock().unwrap().push(processed); // Serializes everything!
    });
    
    results.into_inner().unwrap()
}

// GOOD: Collect without locking
fn no_locking(data: &[i32]) -> Vec<String> {
    data.par_iter()
        .map(|&x| format!("item: {}", x))
        .collect() // Built-in parallel collection
}

// GOOD: Batch updates to reduce contention
fn batched_updates(data: &[i32]) -> Vec<String> {
    let partial_results: Vec<Vec<String>> = data
        .par_chunks(1000)
        .map(|chunk| {
            chunk.iter().map(|&x| format!("item: {}", x)).collect()
        })
        .collect();
    
    // Flatten results
    partial_results.into_iter().flatten().collect()
}
```

## Platform-Specific Optimizations

### CPU Architecture Considerations

```rust
use rayon::prelude::*;

// Optimize for different CPU architectures
fn architecture_aware_processing(data: &[f64]) -> Vec<f64> {
    let cpu_features = detect_cpu_features();
    
    if cpu_features.has_avx2 {
        // Use vectorized operations for AVX2
        vectorized_processing(data)
    } else if cpu_features.has_sse {
        // Use SSE optimizations
        sse_processing(data)
    } else {
        // Fallback to scalar operations
        scalar_processing(data)
    }
}

struct CpuFeatures {
    has_avx2: bool,
    has_sse: bool,
}

fn detect_cpu_features() -> CpuFeatures {
    // Simplified feature detection
    CpuFeatures {
        has_avx2: cfg!(target_feature = "avx2"),
        has_sse: cfg!(target_feature = "sse"),
    }
}

fn vectorized_processing(data: &[f64]) -> Vec<f64> {
    data.par_chunks(8) // Process 8 elements at once (AVX2 width)
        .flat_map(|chunk| {
            chunk.iter().map(|&x| x.sqrt()).collect::<Vec<_>>()
        })
        .collect()
}

fn sse_processing(data: &[f64]) -> Vec<f64> {
    data.par_chunks(4) // Process 4 elements at once (SSE width)
        .flat_map(|chunk| {
            chunk.iter().map(|&x| x.sqrt()).collect::<Vec<_>>()
        })
        .collect()
}

fn scalar_processing(data: &[f64]) -> Vec<f64> {
    data.par_iter().map(|&x| x.sqrt()).collect()
}
```

### Memory Hierarchy Optimization

```rust
use rayon::prelude::*;

// Cache-aware processing
fn cache_aware_matrix_multiply(a: &[Vec<f64>], b: &[Vec<f64>]) -> Vec<Vec<f64>> {
    let n = a.len();
    let m = b[0].len();
    let k = b.len();
    
    // Block size optimized for L1 cache (typically 32KB)
    let block_size = 64;
    
    let mut result = vec![vec![0.0; m]; n];
    
    // Parallel over blocks
    (0..n).into_par_iter().step_by(block_size).for_each(|i_block| {
        for j_block in (0..m).step_by(block_size) {
            for k_block in (0..k).step_by(block_size) {
                // Process block sequentially for cache efficiency
                let i_end = std::cmp::min(i_block + block_size, n);
                let j_end = std::cmp::min(j_block + block_size, m);
                let k_end = std::cmp::min(k_block + block_size, k);
                
                for i in i_block..i_end {
                    for j in j_block..j_end {
                        let mut sum = 0.0;
                        for k_idx in k_block..k_end {
                            sum += a[i][k_idx] * b[k_idx][j];
                        }
                        result[i][j] += sum;
                    }
                }
            }
        }
    });
    
    result
}
```

## Performance Testing Framework

```rust
use rayon::prelude::*;
use std::time::{Duration, Instant};

pub struct BenchmarkSuite {
    name: String,
    warmup_iterations: usize,
    measurement_iterations: usize,
}

impl BenchmarkSuite {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            warmup_iterations: 3,
            measurement_iterations: 10,
        }
    }
    
    pub fn benchmark<F, R>(&self, name: &str, mut f: F) -> BenchmarkResult
    where
        F: FnMut() -> R,
    {
        // Warmup
        for _ in 0..self.warmup_iterations {
            let _ = f();
        }
        
        // Measurement
        let mut times = Vec::with_capacity(self.measurement_iterations);
        for _ in 0..self.measurement_iterations {
            let start = Instant::now();
            let _ = f();
            times.push(start.elapsed());
        }
        
        BenchmarkResult::new(name, times)
    }
    
    pub fn compare<F1, F2, R>(&self, name1: &str, mut f1: F1, name2: &str, mut f2: F2)
    where
        F1: FnMut() -> R,
        F2: FnMut() -> R,
        R: PartialEq + std::fmt::Debug,
    {
        let result1 = self.benchmark(name1, &mut f1);
        let result2 = self.benchmark(name2, &mut f2);
        
        println!("\n{} Comparison:", self.name);
        println!("{}: {}", name1, result1);
        println!("{}: {}", name2, result2);
        
        let speedup = result1.mean().as_secs_f64() / result2.mean().as_secs_f64();
        if speedup > 1.0 {
            println!("{} is {:.2}x faster than {}", name2, speedup, name1);
        } else {
            println!("{} is {:.2}x faster than {}", name1, 1.0 / speedup, name2);
        }
    }
}

pub struct BenchmarkResult {
    name: String,
    times: Vec<Duration>,
}

impl BenchmarkResult {
    fn new(name: &str, times: Vec<Duration>) -> Self {
        Self {
            name: name.to_string(),
            times,
        }
    }
    
    pub fn mean(&self) -> Duration {
        let total: Duration = self.times.iter().sum();
        total / self.times.len() as u32
    }
    
    pub fn min(&self) -> Duration {
        *self.times.iter().min().unwrap()
    }
    
    pub fn max(&self) -> Duration {
        *self.times.iter().max().unwrap()
    }
}

impl std::fmt::Display for BenchmarkResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "mean: {:?}, min: {:?}, max: {:?}", 
               self.mean(), self.min(), self.max())
    }
}

// Usage example
fn performance_testing_example() {
    let suite = BenchmarkSuite::new("Parallel vs Sequential");
    let data: Vec<i32> = (0..100_000).collect();
    
    suite.compare(
        "Sequential",
        || data.iter().map(|&x| expensive_work(x)).sum::<i32>(),
        "Parallel",
        || data.par_iter().map(|&x| expensive_work(x)).sum::<i32>(),
    );
}
```

## Best Practices Summary

1. **Measure First**: Always profile before optimizing
2. **Right-size Work**: Ensure tasks are substantial enough to benefit from parallelism
3. **Optimize Memory Access**: Consider cache locality and memory allocation patterns
4. **Balance Load**: Handle uneven workloads with appropriate strategies
5. **Minimize Synchronization**: Avoid excessive locking and false sharing
6. **Choose Appropriate Granularity**: Balance overhead vs parallelism
7. **Consider Platform**: Optimize for target CPU architecture and memory hierarchy
8. **Test Thoroughly**: Use comprehensive benchmarking to validate improvements

## Next Steps

With performance optimization knowledge:
- [Debugging](06-debugging.md) - Troubleshoot performance issues
- [Async Integration](07-async.md) - Combine with async programming
- [Custom Iterators](08-custom-iterators.md) - Build optimized parallel iterators