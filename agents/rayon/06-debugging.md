# Debugging Parallel Code with Rayon

## Introduction

Debugging parallel code presents unique challenges. This guide covers techniques and tools for troubleshooting Rayon-based applications.

## Common Parallel Bugs

### 1. Race Conditions

```rust
use rayon::prelude::*;
use std::sync::atomic::{AtomicUsize, Ordering};

// BAD: Race condition
fn race_condition_example() {
    let mut counter = 0;
    (0..1000).into_par_iter().for_each(|_| {
        counter += 1; // Race condition!
    });
    println!("Counter: {}", counter); // Unpredictable result
}

// GOOD: Atomic operations
fn atomic_solution() {
    let counter = AtomicUsize::new(0);
    (0..1000).into_par_iter().for_each(|_| {
        counter.fetch_add(1, Ordering::Relaxed);
    });
    println!("Counter: {}", counter.load(Ordering::Relaxed));
}

// BETTER: Use reduction
fn reduction_solution() {
    let count: usize = (0..1000).into_par_iter().map(|_| 1).sum();
    println!("Counter: {}", count);
}
```

### 2. Deadlocks

```rust
use rayon::prelude::*;
use std::sync::Mutex;

// BAD: Potential deadlock
fn deadlock_example() {
    let mutex1 = Mutex::new(0);
    let mutex2 = Mutex::new(0);
    
    (0..10).into_par_iter().for_each(|i| {
        if i % 2 == 0 {
            let _guard1 = mutex1.lock().unwrap();
            let _guard2 = mutex2.lock().unwrap(); // Lock order: 1, 2
        } else {
            let _guard2 = mutex2.lock().unwrap();
            let _guard1 = mutex1.lock().unwrap(); // Lock order: 2, 1 - DEADLOCK!
        }
    });
}

// GOOD: Consistent lock ordering
fn deadlock_solution() {
    let mutex1 = Mutex::new(0);
    let mutex2 = Mutex::new(0);
    
    (0..10).into_par_iter().for_each(|i| {
        // Always acquire locks in the same order
        let _guard1 = mutex1.lock().unwrap();
        let _guard2 = mutex2.lock().unwrap();
        
        // Do work based on i
        if i % 2 == 0 {
            // Even work
        } else {
            // Odd work
        }
    });
}
```

## Debugging Techniques

### 1. Thread-Safe Logging

```rust
use rayon::prelude::*;
use std::sync::Mutex;

// Thread-safe debug logging
lazy_static::lazy_static! {
    static ref DEBUG_LOG: Mutex<Vec<String>> = Mutex::new(Vec::new());
}

fn debug_log(message: String) {
    DEBUG_LOG.lock().unwrap().push(format!(
        "[Thread {:?}] {}",
        rayon::current_thread_index(),
        message
    ));
}

fn debug_parallel_execution() {
    (0..10).into_par_iter().for_each(|i| {
        debug_log(format!("Processing item {}", i));
        
        // Simulate work
        std::thread::sleep(std::time::Duration::from_millis(100));
        
        debug_log(format!("Completed item {}", i));
    });
    
    // Print debug log
    let log = DEBUG_LOG.lock().unwrap();
    for entry in log.iter() {
        println!("{}", entry);
    }
}
```

### 2. Deterministic Testing

```rust
use rayon::prelude::*;
use std::collections::HashMap;

// Make parallel execution deterministic for testing
fn deterministic_parallel_map<T, R, F>(data: Vec<T>, f: F) -> Vec<R>
where
    T: Send,
    R: Send,
    F: Fn(T) -> R + Sync,
{
    let mut indexed_data: Vec<(usize, T)> = data.into_iter().enumerate().collect();
    
    let mut results: Vec<(usize, R)> = indexed_data
        .into_par_iter()
        .map(|(index, item)| (index, f(item)))
        .collect();
    
    // Sort by original index to maintain order
    results.sort_by_key(|(index, _)| *index);
    
    results.into_iter().map(|(_, result)| result).collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_deterministic_execution() {
        let data = vec![1, 2, 3, 4, 5];
        
        // Run multiple times - should always produce same result
        for _ in 0..10 {
            let result = deterministic_parallel_map(data.clone(), |x| x * 2);
            assert_eq!(result, vec![2, 4, 6, 8, 10]);
        }
    }
}
```

### 3. Performance Debugging

```rust
use rayon::prelude::*;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Instant;

struct ThreadMonitor {
    thread_work_counts: Vec<AtomicUsize>,
    start_time: Instant,
}

impl ThreadMonitor {
    fn new() -> Self {
        let num_threads = rayon::current_num_threads();
        Self {
            thread_work_counts: (0..num_threads)
                .map(|_| AtomicUsize::new(0))
                .collect(),
            start_time: Instant::now(),
        }
    }
    
    fn record_work(&self) {
        if let Some(thread_index) = rayon::current_thread_index() {
            self.thread_work_counts[thread_index].fetch_add(1, Ordering::Relaxed);
        }
    }
    
    fn report(&self) {
        println!("Thread work distribution after {:?}:", self.start_time.elapsed());
        for (i, counter) in self.thread_work_counts.iter().enumerate() {
            let count = counter.load(Ordering::Relaxed);
            println!("  Thread {}: {} tasks", i, count);
        }
        
        // Calculate load balance
        let counts: Vec<usize> = self.thread_work_counts
            .iter()
            .map(|c| c.load(Ordering::Relaxed))
            .collect();
        
        let min_work = *counts.iter().min().unwrap();
        let max_work = *counts.iter().max().unwrap();
        let balance = if max_work > 0 {
            min_work as f64 / max_work as f64
        } else {
            1.0
        };
        
        println!("Load balance: {:.2}% (1.0 = perfect)", balance * 100.0);
    }
}

fn debug_load_balancing() {
    let monitor = Arc::new(ThreadMonitor::new());
    
    // Simulate uneven workload
    let tasks: Vec<usize> = (0..1000).map(|i| if i % 100 == 0 { 1000 } else { 1 }).collect();
    
    tasks.into_par_iter().for_each(|work_amount| {
        monitor.record_work();
        
        // Simulate variable work
        for _ in 0..work_amount {
            std::hint::black_box(42 * 42);
        }
    });
    
    monitor.report();
}
```

## Testing Strategies

### 1. Property-Based Testing

```rust
use rayon::prelude::*;

// Property: parallel and sequential should produce same results
fn test_parallel_sequential_equivalence<T, R>(
    data: Vec<T>,
    sequential_fn: impl Fn(&[T]) -> R,
    parallel_fn: impl Fn(Vec<T>) -> R,
) where
    T: Clone,
    R: PartialEq + std::fmt::Debug,
{
    let seq_result = sequential_fn(&data);
    let par_result = parallel_fn(data);
    assert_eq!(seq_result, par_result, "Parallel and sequential results differ");
}

// Property: operations should be associative for parallel reduction
fn test_associativity<T>(data: Vec<T>, op: impl Fn(T, T) -> T + Sync)
where
    T: Clone + PartialEq + std::fmt::Debug + Send,
{
    if data.len() < 3 { return; }
    
    // Test different groupings
    let a = data[0].clone();
    let b = data[1].clone();
    let c = data[2].clone();
    
    let left_assoc = op(op(a.clone(), b.clone()), c.clone());
    let right_assoc = op(a, op(b, c));
    
    assert_eq!(left_assoc, right_assoc, "Operation is not associative");
}

#[cfg(test)]
mod property_tests {
    use super::*;
    
    #[test]
    fn test_parallel_sum() {
        let data: Vec<i32> = (0..1000).collect();
        
        test_parallel_sequential_equivalence(
            data,
            |data| data.iter().sum::<i32>(),
            |data| data.into_par_iter().sum::<i32>(),
        );
    }
    
    #[test]
    fn test_addition_associativity() {
        let data = vec![1, 2, 3, 4, 5];
        test_associativity(data, |a, b| a + b);
    }
}
```

### 2. Stress Testing

```rust
use rayon::prelude::*;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

fn stress_test_parallel_execution() {
    let running = Arc::new(AtomicBool::new(true));
    let error_count = Arc::new(AtomicUsize::new(0));
    
    // Run stress test for 10 seconds
    let start = Instant::now();
    while start.elapsed() < Duration::from_secs(10) {
        let data: Vec<i32> = (0..10000).collect();
        
        // Test parallel execution
        let result = std::panic::catch_unwind(|| {
            data.into_par_iter()
                .map(|x| x * x)
                .filter(|&x| x % 2 == 0)
                .sum::<i32>()
        });
        
        match result {
            Ok(_) => {
                // Success - continue
            }
            Err(_) => {
                error_count.fetch_add(1, Ordering::Relaxed);
                eprintln!("Panic detected in parallel execution");
            }
        }
    }
    
    let errors = error_count.load(Ordering::Relaxed);
    if errors > 0 {
        panic!("Stress test failed with {} errors", errors);
    } else {
        println!("Stress test passed - no errors detected");
    }
}
```

## Debugging Tools Integration

### 1. Custom Panic Handler

```rust
use rayon::ThreadPoolBuilder;
use std::sync::Mutex;

lazy_static::lazy_static! {
    static ref PANIC_LOG: Mutex<Vec<String>> = Mutex::new(Vec::new());
}

fn setup_debug_thread_pool() -> Result<rayon::ThreadPool, rayon::ThreadPoolBuildError> {
    ThreadPoolBuilder::new()
        .panic_handler(|panic_info| {
            let panic_msg = format!(
                "Thread panic: {:?} at {:?}",
                panic_info,
                std::thread::current().id()
            );
            
            PANIC_LOG.lock().unwrap().push(panic_msg.clone());
            eprintln!("{}", panic_msg);
            
            // Don't abort - let other threads continue
        })
        .build()
}

fn get_panic_log() -> Vec<String> {
    PANIC_LOG.lock().unwrap().clone()
}
```

### 2. Memory Debugging

```rust
use rayon::prelude::*;

// Track memory allocations in parallel code
struct AllocationTracker {
    allocations: std::sync::atomic::AtomicUsize,
    deallocations: std::sync::atomic::AtomicUsize,
}

impl AllocationTracker {
    fn new() -> Self {
        Self {
            allocations: std::sync::atomic::AtomicUsize::new(0),
            deallocations: std::sync::atomic::AtomicUsize::new(0),
        }
    }
    
    fn track_allocation(&self) {
        self.allocations.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    }
    
    fn track_deallocation(&self) {
        self.deallocations.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    }
    
    fn report(&self) {
        let allocs = self.allocations.load(std::sync::atomic::Ordering::Relaxed);
        let deallocs = self.deallocations.load(std::sync::atomic::Ordering::Relaxed);
        println!("Allocations: {}, Deallocations: {}, Leaked: {}", 
                 allocs, deallocs, allocs.saturating_sub(deallocs));
    }
}

// Custom allocator wrapper for debugging
fn debug_memory_usage() {
    let tracker = std::sync::Arc::new(AllocationTracker::new());
    
    (0..1000).into_par_iter().for_each(|i| {
        let tracker = tracker.clone();
        
        // Simulate allocation
        tracker.track_allocation();
        let data = vec![i; 100];
        
        // Use data
        let _sum: i32 = data.iter().sum();
        
        // Simulate deallocation
        tracker.track_deallocation();
        drop(data);
    });
    
    tracker.report();
}
```

## Performance Debugging

### 1. Bottleneck Detection

```rust
use rayon::prelude::*;
use std::time::Instant;

fn detect_bottlenecks() {
    let data: Vec<i32> = (0..100000).collect();
    
    // Time each stage of the pipeline
    let start = Instant::now();
    
    let filtered: Vec<i32> = {
        let stage_start = Instant::now();
        let result = data.par_iter().filter(|&&x| x % 2 == 0).cloned().collect();
        println!("Filter stage: {:?}", stage_start.elapsed());
        result
    };
    
    let mapped: Vec<i32> = {
        let stage_start = Instant::now();
        let result = filtered.par_iter().map(|&x| expensive_computation(x)).collect();
        println!("Map stage: {:?}", stage_start.elapsed());
        result
    };
    
    let reduced: i32 = {
        let stage_start = Instant::now();
        let result = mapped.par_iter().sum();
        println!("Reduce stage: {:?}", stage_start.elapsed());
        result
    };
    
    println!("Total time: {:?}", start.elapsed());
    println!("Result: {}", reduced);
}

fn expensive_computation(x: i32) -> i32 {
    (0..1000).fold(x, |acc, i| acc.wrapping_mul(i + 1))
}
```

### 2. Thread Utilization Monitoring

```rust
use rayon::prelude::*;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

struct ThreadUtilizationMonitor {
    thread_active_time: Vec<AtomicU64>,
    start_time: Instant,
}

impl ThreadUtilizationMonitor {
    fn new() -> Self {
        let num_threads = rayon::current_num_threads();
        Self {
            thread_active_time: (0..num_threads)
                .map(|_| AtomicU64::new(0))
                .collect(),
            start_time: Instant::now(),
        }
    }
    
    fn start_work(&self) -> Option<WorkTimer> {
        rayon::current_thread_index().map(|index| {
            WorkTimer {
                thread_index: index,
                start_time: Instant::now(),
                monitor: self,
            }
        })
    }
    
    fn record_work_time(&self, thread_index: usize, duration: Duration) {
        self.thread_active_time[thread_index]
            .fetch_add(duration.as_nanos() as u64, Ordering::Relaxed);
    }
    
    fn report(&self) {
        let total_elapsed = self.start_time.elapsed();
        println!("Thread utilization over {:?}:", total_elapsed);
        
        for (i, active_time) in self.thread_active_time.iter().enumerate() {
            let active_nanos = active_time.load(Ordering::Relaxed);
            let active_duration = Duration::from_nanos(active_nanos);
            let utilization = active_duration.as_secs_f64() / total_elapsed.as_secs_f64();
            
            println!("  Thread {}: {:.1}% utilization", i, utilization * 100.0);
        }
    }
}

struct WorkTimer<'a> {
    thread_index: usize,
    start_time: Instant,
    monitor: &'a ThreadUtilizationMonitor,
}

impl<'a> Drop for WorkTimer<'a> {
    fn drop(&mut self) {
        let duration = self.start_time.elapsed();
        self.monitor.record_work_time(self.thread_index, duration);
    }
}

fn monitor_thread_utilization() {
    let monitor = Arc::new(ThreadUtilizationMonitor::new());
    
    (0..1000).into_par_iter().for_each(|i| {
        let _timer = monitor.start_work();
        
        // Simulate variable work
        let work_amount = if i % 10 == 0 { 10000 } else { 1000 };
        for _ in 0..work_amount {
            std::hint::black_box(42 * 42);
        }
    });
    
    monitor.report();
}
```

## Best Practices for Debugging

1. **Use Thread-Safe Logging**: Avoid println! in parallel code
2. **Test Deterministically**: Make tests reproducible
3. **Monitor Performance**: Track thread utilization and load balance
4. **Handle Panics Gracefully**: Use custom panic handlers
5. **Validate Assumptions**: Test associativity and commutativity
6. **Profile Memory Usage**: Watch for leaks and excessive allocation
7. **Use Stress Testing**: Run long-duration tests to catch rare bugs
8. **Isolate Problems**: Test components separately before combining

## Next Steps

- [Async Integration](07-async.md) - Combine Rayon with async programming
- [Custom Iterators](08-custom-iterators.md) - Build your own parallel iterators
- [API Reference](09-api-reference.md) - Complete API documentation