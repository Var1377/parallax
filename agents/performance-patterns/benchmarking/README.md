# Benchmarking Memory Usage and Performance Measurement

## Table of Contents

1. [Introduction](#introduction)
2. [Benchmarking Fundamentals](#benchmarking-fundamentals)
3. [Memory Usage Measurement](#memory-usage-measurement)
4. [Performance Profiling Tools](#performance-profiling-tools)
5. [Criterion.rs for Microbenchmarks](#criterionrs-for-microbenchmarks)
6. [Memory Allocation Tracking](#memory-allocation-tracking)
7. [Cache Performance Analysis](#cache-performance-analysis)
8. [Continuous Performance Monitoring](#continuous-performance-monitoring)
9. [Statistical Analysis](#statistical-analysis)
10. [Best Practices](#best-practices)

## Introduction

Accurate performance measurement is crucial for optimizing memory usage and overall application performance. This guide covers comprehensive techniques for benchmarking memory-intensive operations, measuring allocation patterns, and analyzing cache behavior in Rust applications.

### Why Measure Performance?

```rust
use std::time::Instant;

// Without measurement, optimizations are just guesses
fn demonstrate_measurement_importance() {
    let data = vec![1, 2, 3, 4, 5];
    
    // Approach 1: Looks efficient
    let start = Instant::now();
    let sum1: i32 = data.iter().sum();
    let time1 = start.elapsed();
    
    // Approach 2: Looks less efficient
    let start = Instant::now();
    let mut sum2 = 0;
    for &x in &data {
        sum2 += x;
    }
    let time2 = start.elapsed();
    
    println!("Iterator sum: {:?}", time1);
    println!("Manual loop: {:?}", time2);
    
    // Results might surprise you!
    // Always measure your specific use case
}
```

## Benchmarking Fundamentals

### Basic Timing Measurements

```rust
use std::time::{Duration, Instant};

struct BenchmarkResult {
    min: Duration,
    max: Duration,
    mean: Duration,
    median: Duration,
    std_dev: Duration,
    iterations: usize,
}

fn benchmark_function<F, R>(mut f: F, iterations: usize) -> BenchmarkResult 
where 
    F: FnMut() -> R,
{
    let mut times = Vec::with_capacity(iterations);
    
    // Warmup
    for _ in 0..10 {
        let _ = f();
    }
    
    // Actual measurements
    for _ in 0..iterations {
        let start = Instant::now();
        let _ = f();
        let elapsed = start.elapsed();
        times.push(elapsed);
    }
    
    times.sort();
    
    let min = times[0];
    let max = times[times.len() - 1];
    let median = times[times.len() / 2];
    let mean = Duration::from_nanos(
        times.iter().map(|d| d.as_nanos()).sum::<u128>() / times.len() as u128
    );
    
    // Calculate standard deviation
    let mean_nanos = mean.as_nanos() as f64;
    let variance = times.iter()
        .map(|d| {
            let diff = d.as_nanos() as f64 - mean_nanos;
            diff * diff
        })
        .sum::<f64>() / times.len() as f64;
    let std_dev = Duration::from_nanos(variance.sqrt() as u64);
    
    BenchmarkResult {
        min,
        max,
        mean,
        median,
        std_dev,
        iterations,
    }
}

// Usage example
fn benchmark_data_structures() {
    use std::collections::{HashMap, BTreeMap};
    use indexmap::IndexMap;
    
    let data: Vec<(i32, String)> = (0..1000)
        .map(|i| (i, format!("value_{}", i)))
        .collect();
    
    // Benchmark HashMap insertion
    let hashmap_result = benchmark_function(|| {
        let mut map = HashMap::new();
        for (k, v) in &data {
            map.insert(*k, v.clone());
        }
        map
    }, 100);
    
    // Benchmark IndexMap insertion
    let indexmap_result = benchmark_function(|| {
        let mut map = IndexMap::new();
        for (k, v) in &data {
            map.insert(*k, v.clone());
        }
        map
    }, 100);
    
    println!("HashMap insertion: {:?} ± {:?}", 
        hashmap_result.mean, hashmap_result.std_dev);
    println!("IndexMap insertion: {:?} ± {:?}", 
        indexmap_result.mean, indexmap_result.std_dev);
}
```

### High-Resolution Timing

```rust
use std::time::Instant;

// For very fast operations, use high-resolution timing
fn precise_benchmark<F, R>(f: F, min_duration: std::time::Duration) -> (Duration, u64)
where 
    F: Fn() -> R,
{
    let mut iterations = 1u64;
    let mut total_time = Duration::ZERO;
    
    // Find appropriate iteration count
    loop {
        let start = Instant::now();
        for _ in 0..iterations {
            std::hint::black_box(f());
        }
        total_time = start.elapsed();
        
        if total_time >= min_duration {
            break;
        }
        
        iterations *= 2;
    }
    
    (total_time / iterations as u32, iterations)
}

// Example: Benchmarking very fast operations
fn benchmark_fast_operations() {
    let data = vec![1, 2, 3, 4, 5];
    
    // Benchmark array access
    let (time_per_access, iterations) = precise_benchmark(|| {
        std::hint::black_box(data[2])
    }, Duration::from_millis(100));
    
    println!("Array access: {:?} per operation ({} iterations)", 
        time_per_access, iterations);
    
    // Benchmark simple arithmetic
    let (time_per_op, iterations) = precise_benchmark(|| {
        std::hint::black_box(42 + 37)
    }, Duration::from_millis(100));
    
    println!("Addition: {:?} per operation ({} iterations)", 
        time_per_op, iterations);
}
```

## Memory Usage Measurement

### Heap Allocation Tracking

```rust
use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering};

// Custom allocator that tracks allocations
struct TrackingAllocator {
    allocated: AtomicUsize,
    deallocated: AtomicUsize,
    allocation_count: AtomicUsize,
    deallocation_count: AtomicUsize,
}

impl TrackingAllocator {
    const fn new() -> Self {
        Self {
            allocated: AtomicUsize::new(0),
            deallocated: AtomicUsize::new(0),
            allocation_count: AtomicUsize::new(0),
            deallocation_count: AtomicUsize::new(0),
        }
    }
    
    fn reset(&self) {
        self.allocated.store(0, Ordering::SeqCst);
        self.deallocated.store(0, Ordering::SeqCst);
        self.allocation_count.store(0, Ordering::SeqCst);
        self.deallocation_count.store(0, Ordering::SeqCst);
    }
    
    fn stats(&self) -> AllocationStats {
        AllocationStats {
            bytes_allocated: self.allocated.load(Ordering::SeqCst),
            bytes_deallocated: self.deallocated.load(Ordering::SeqCst),
            allocations: self.allocation_count.load(Ordering::SeqCst),
            deallocations: self.deallocation_count.load(Ordering::SeqCst),
        }
    }
}

unsafe impl GlobalAlloc for TrackingAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let ptr = System.alloc(layout);
        if !ptr.is_null() {
            self.allocated.fetch_add(layout.size(), Ordering::SeqCst);
            self.allocation_count.fetch_add(1, Ordering::SeqCst);
        }
        ptr
    }
    
    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        System.dealloc(ptr, layout);
        self.deallocated.fetch_add(layout.size(), Ordering::SeqCst);
        self.deallocation_count.fetch_add(1, Ordering::SeqCst);
    }
}

#[derive(Debug, Clone, Copy)]
struct AllocationStats {
    bytes_allocated: usize,
    bytes_deallocated: usize,
    allocations: usize,
    deallocations: usize,
}

impl AllocationStats {
    fn net_bytes(&self) -> isize {
        self.bytes_allocated as isize - self.bytes_deallocated as isize
    }
    
    fn net_allocations(&self) -> isize {
        self.allocations as isize - self.deallocations as isize
    }
}

#[global_allocator]
static ALLOCATOR: TrackingAllocator = TrackingAllocator::new();

// Benchmark with allocation tracking
fn benchmark_with_allocation_tracking<F, R>(f: F) -> (R, AllocationStats)
where 
    F: FnOnce() -> R,
{
    ALLOCATOR.reset();
    let result = f();
    let stats = ALLOCATOR.stats();
    (result, stats)
}

// Example usage
fn test_allocation_patterns() {
    // Test Vec allocation pattern
    let (vec, stats) = benchmark_with_allocation_tracking(|| {
        let mut v = Vec::new();
        for i in 0..1000 {
            v.push(i);
        }
        v
    });
    
    println!("Vec creation stats: {:?}", stats);
    println!("Final capacity: {}", vec.capacity());
    
    // Test HashMap allocation pattern
    let (map, stats) = benchmark_with_allocation_tracking(|| {
        let mut m = std::collections::HashMap::new();
        for i in 0..1000 {
            m.insert(i, format!("value_{}", i));
        }
        m
    });
    
    println!("HashMap creation stats: {:?}", stats);
    println!("Final capacity: {}", map.capacity());
}
```

### Memory Usage Profiling

```rust
use std::process::Command;
use std::time::{Duration, Instant};

// Memory usage monitoring
struct MemoryMonitor {
    pid: u32,
}

impl MemoryMonitor {
    fn new() -> Self {
        Self {
            pid: std::process::id(),
        }
    }
    
    fn current_memory_usage(&self) -> Option<MemoryUsage> {
        // Read from /proc/self/status on Linux
        let status = std::fs::read_to_string("/proc/self/status").ok()?;
        
        let mut vm_rss = None;
        let mut vm_size = None;
        
        for line in status.lines() {
            if line.starts_with("VmRSS:") {
                vm_rss = line.split_whitespace()
                    .nth(1)?
                    .parse::<usize>()
                    .ok()
                    .map(|kb| kb * 1024); // Convert KB to bytes
            } else if line.starts_with("VmSize:") {
                vm_size = line.split_whitespace()
                    .nth(1)?
                    .parse::<usize>()
                    .ok()
                    .map(|kb| kb * 1024); // Convert KB to bytes
            }
        }
        
        Some(MemoryUsage {
            rss: vm_rss?,
            virtual_size: vm_size?,
        })
    }
    
    fn monitor_during<F, R>(&self, f: F) -> (R, MemoryProfile)
    where 
        F: FnOnce() -> R,
    {
        let initial = self.current_memory_usage().unwrap_or_default();
        let start_time = Instant::now();
        
        let result = f();
        
        let end_time = Instant::now();
        let final_usage = self.current_memory_usage().unwrap_or_default();
        
        let profile = MemoryProfile {
            initial_rss: initial.rss,
            final_rss: final_usage.rss,
            peak_rss: final_usage.rss, // Simplified - would need continuous monitoring
            duration: end_time - start_time,
        };
        
        (result, profile)
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct MemoryUsage {
    rss: usize,        // Resident Set Size (physical memory)
    virtual_size: usize, // Virtual memory size
}

#[derive(Debug)]
struct MemoryProfile {
    initial_rss: usize,
    final_rss: usize,
    peak_rss: usize,
    duration: Duration,
}

impl MemoryProfile {
    fn memory_delta(&self) -> isize {
        self.final_rss as isize - self.initial_rss as isize
    }
    
    fn memory_rate(&self) -> f64 {
        self.memory_delta() as f64 / self.duration.as_secs_f64()
    }
}

// Example usage
fn profile_memory_intensive_operation() {
    let monitor = MemoryMonitor::new();
    
    let (result, profile) = monitor.monitor_during(|| {
        // Simulate memory-intensive operation
        let mut data = Vec::new();
        for i in 0..1_000_000 {
            data.push(format!("Item {}", i));
        }
        data.len()
    });
    
    println!("Operation result: {}", result);
    println!("Memory profile: {:?}", profile);
    println!("Memory delta: {} bytes", profile.memory_delta());
    println!("Memory rate: {:.2} bytes/sec", profile.memory_rate());
}
```

## Performance Profiling Tools

### Integration with External Profilers

```rust
// Perf integration for Linux
fn run_with_perf_profiling<F, R>(f: F, output_file: &str) -> R
where 
    F: FnOnce() -> R,
{
    // Start perf recording
    let mut perf_child = std::process::Command::new("perf")
        .args(&["record", "-g", "-o", output_file, "--", "sleep", "0"])
        .spawn()
        .expect("Failed to start perf");
    
    // Run the function
    let result = f();
    
    // Stop perf recording
    perf_child.kill().ok();
    perf_child.wait().ok();
    
    result
}

// Valgrind integration
fn run_with_valgrind_massif<F, R>(f: F, output_file: &str) -> R
where 
    F: FnOnce() -> R,
{
    // This would typically be run as a separate process
    // For demonstration, we'll just run the function
    println!("Run with: valgrind --tool=massif --massif-out-file={} your_program", output_file);
    f()
}

// CPU profiling with sampling
struct SamplingProfiler {
    samples: Vec<StackSample>,
    sampling_interval: Duration,
}

#[derive(Debug)]
struct StackSample {
    timestamp: Instant,
    // In a real implementation, this would contain stack trace information
    function_name: String,
}

impl SamplingProfiler {
    fn new(sampling_interval: Duration) -> Self {
        Self {
            samples: Vec::new(),
            sampling_interval,
        }
    }
    
    fn profile<F, R>(&mut self, f: F) -> R
    where 
        F: FnOnce() -> R,
    {
        let start = Instant::now();
        
        // In a real implementation, this would use signal handlers
        // or a separate thread to collect samples
        let result = f();
        
        // Simulate collecting samples
        let mut sample_time = start;
        while sample_time < Instant::now() {
            self.samples.push(StackSample {
                timestamp: sample_time,
                function_name: "sample_function".to_string(),
            });
            sample_time += self.sampling_interval;
        }
        
        result
    }
    
    fn report(&self) {
        println!("Collected {} samples", self.samples.len());
        // In a real implementation, this would analyze the stack traces
        // and produce a flame graph or call tree
    }
}
```

### Custom Performance Counters

```rust
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

// Performance counter system
#[derive(Debug, Clone)]
pub struct PerformanceCounters {
    counters: Arc<Mutex<HashMap<String, CounterData>>>,
}

#[derive(Debug, Clone)]
struct CounterData {
    count: u64,
    total_time: Duration,
    min_time: Duration,
    max_time: Duration,
}

impl PerformanceCounters {
    pub fn new() -> Self {
        Self {
            counters: Arc::new(Mutex::new(HashMap::new())),
        }
    }
    
    pub fn time_operation<F, R>(&self, name: &str, f: F) -> R
    where 
        F: FnOnce() -> R,
    {
        let start = Instant::now();
        let result = f();
        let elapsed = start.elapsed();
        
        self.record_timing(name, elapsed);
        result
    }
    
    fn record_timing(&self, name: &str, duration: Duration) {
        let mut counters = self.counters.lock().unwrap();
        let entry = counters.entry(name.to_string()).or_insert(CounterData {
            count: 0,
            total_time: Duration::ZERO,
            min_time: Duration::MAX,
            max_time: Duration::ZERO,
        });
        
        entry.count += 1;
        entry.total_time += duration;
        entry.min_time = entry.min_time.min(duration);
        entry.max_time = entry.max_time.max(duration);
    }
    
    pub fn report(&self) {
        let counters = self.counters.lock().unwrap();
        
        println!("Performance Report:");
        println!("{:<30} {:>10} {:>15} {:>15} {:>15} {:>15}", 
            "Operation", "Count", "Total", "Average", "Min", "Max");
        println!("{:-<100}", "");
        
        for (name, data) in counters.iter() {
            let avg = data.total_time / data.count as u32;
            println!("{:<30} {:>10} {:>15?} {:>15?} {:>15?} {:>15?}",
                name, data.count, data.total_time, avg, data.min_time, data.max_time);
        }
    }
}

// Usage example
fn demonstrate_performance_counters() {
    let counters = PerformanceCounters::new();
    
    // Simulate various operations
    for i in 0..100 {
        counters.time_operation("parse_token", || {
            // Simulate parsing work
            std::thread::sleep(Duration::from_micros(10 + i % 5));
        });
        
        if i % 10 == 0 {
            counters.time_operation("type_check", || {
                // Simulate type checking work
                std::thread::sleep(Duration::from_micros(50));
            });
        }
    }
    
    counters.report();
}
```

## Criterion.rs for Microbenchmarks

### Basic Criterion Setup

```toml
# Cargo.toml
[dev-dependencies]
criterion = { version = "0.5", features = ["html_reports"] }

[[bench]]
name = "memory_benchmarks"
harness = false
```

```rust
// benches/memory_benchmarks.rs
use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use std::collections::{HashMap, BTreeMap};
use indexmap::IndexMap;
use smallvec::{SmallVec, smallvec};
use bumpalo::Bump;

fn bench_data_structures(c: &mut Criterion) {
    let mut group = c.benchmark_group("data_structures");
    
    for size in [10, 100, 1000, 10000].iter() {
        // Benchmark Vec vs SmallVec
        group.bench_with_input(BenchmarkId::new("vec_creation", size), size, |b, &size| {
            b.iter(|| {
                let mut v = Vec::new();
                for i in 0..size {
                    v.push(black_box(i));
                }
                v
            });
        });
        
        group.bench_with_input(BenchmarkId::new("smallvec_creation", size), size, |b, &size| {
            b.iter(|| {
                let mut v = SmallVec::<[i32; 8]>::new();
                for i in 0..size {
                    v.push(black_box(i));
                }
                v
            });
        });
        
        // Benchmark HashMap vs IndexMap
        group.bench_with_input(BenchmarkId::new("hashmap_insertion", size), size, |b, &size| {
            b.iter(|| {
                let mut map = HashMap::new();
                for i in 0..size {
                    map.insert(black_box(i), black_box(i * 2));
                }
                map
            });
        });
        
        group.bench_with_input(BenchmarkId::new("indexmap_insertion", size), size, |b, &size| {
            b.iter(|| {
                let mut map = IndexMap::new();
                for i in 0..size {
                    map.insert(black_box(i), black_box(i * 2));
                }
                map
            });
        });
    }
    
    group.finish();
}

fn bench_arena_allocation(c: &mut Criterion) {
    let mut group = c.benchmark_group("arena_allocation");
    
    group.bench_function("heap_allocation", |b| {
        b.iter(|| {
            let mut vec = Vec::new();
            for i in 0..1000 {
                vec.push(Box::new(black_box(i)));
            }
            vec
        });
    });
    
    group.bench_function("arena_allocation", |b| {
        b.iter(|| {
            let bump = Bump::new();
            let mut vec = Vec::new();
            for i in 0..1000 {
                vec.push(bump.alloc(black_box(i)));
            }
            vec
        });
    });
    
    group.finish();
}

fn bench_memory_access_patterns(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_access");
    
    // Sequential vs random access
    let data = (0..10000).collect::<Vec<i32>>();
    let indices: Vec<usize> = (0..1000).map(|_| fastrand::usize(0..10000)).collect();
    
    group.bench_function("sequential_access", |b| {
        b.iter(|| {
            let mut sum = 0;
            for i in 0..1000 {
                sum += black_box(data[i]);
            }
            sum
        });
    });
    
    group.bench_function("random_access", |b| {
        b.iter(|| {
            let mut sum = 0;
            for &i in &indices {
                sum += black_box(data[i]);
            }
            sum
        });
    });
    
    group.finish();
}

criterion_group!(benches, bench_data_structures, bench_arena_allocation, bench_memory_access_patterns);
criterion_main!(benches);
```

### Advanced Criterion Features

```rust
use criterion::{Criterion, BatchSize, Throughput, measurement::WallTime};

fn advanced_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("advanced");
    
    // Benchmark with setup and teardown
    group.bench_function("with_setup", |b| {
        b.iter_batched(
            || {
                // Setup: create test data
                (0..1000).map(|i| format!("item_{}", i)).collect::<Vec<_>>()
            },
            |data| {
                // Benchmark: process the data
                data.iter().map(|s| s.len()).sum::<usize>()
            },
            BatchSize::SmallInput
        );
    });
    
    // Benchmark with throughput measurement
    let data_size = 1024 * 1024; // 1MB
    let data = vec![0u8; data_size];
    
    group.throughput(Throughput::Bytes(data_size as u64));
    group.bench_function("memory_copy", |b| {
        b.iter(|| {
            let mut dest = vec![0u8; data_size];
            dest.copy_from_slice(&data);
            black_box(dest);
        });
    });
    
    // Custom measurement
    group.bench_function("custom_measurement", |b| {
        b.iter_custom(|iters| {
            let start = std::time::Instant::now();
            for _ in 0..iters {
                // Your operation here
                black_box(expensive_operation());
            }
            start.elapsed()
        });
    });
    
    group.finish();
}

fn expensive_operation() -> i32 {
    (0..1000).sum()
}
```

## Memory Allocation Tracking

### Detailed Allocation Analysis

```rust
use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::Mutex;
use std::collections::HashMap;

// Detailed allocation tracker
struct DetailedTracker {
    allocations: Mutex<HashMap<*mut u8, AllocationInfo>>,
    stats: Mutex<DetailedStats>,
}

#[derive(Debug, Clone)]
struct AllocationInfo {
    size: usize,
    align: usize,
    timestamp: std::time::Instant,
    backtrace: String, // In real implementation, would be actual backtrace
}

#[derive(Debug, Default)]
struct DetailedStats {
    total_allocated: usize,
    total_deallocated: usize,
    current_allocated: usize,
    peak_allocated: usize,
    allocation_count: usize,
    deallocation_count: usize,
    size_histogram: HashMap<usize, usize>, // size bucket -> count
}

impl DetailedTracker {
    const fn new() -> Self {
        Self {
            allocations: Mutex::new(HashMap::new()),
            stats: Mutex::new(DetailedStats {
                total_allocated: 0,
                total_deallocated: 0,
                current_allocated: 0,
                peak_allocated: 0,
                allocation_count: 0,
                deallocation_count: 0,
                size_histogram: HashMap::new(),
            }),
        }
    }
    
    fn record_allocation(&self, ptr: *mut u8, layout: Layout) {
        let info = AllocationInfo {
            size: layout.size(),
            align: layout.align(),
            timestamp: std::time::Instant::now(),
            backtrace: "placeholder".to_string(), // Would capture real backtrace
        };
        
        {
            let mut allocations = self.allocations.lock().unwrap();
            allocations.insert(ptr, info);
        }
        
        {
            let mut stats = self.stats.lock().unwrap();
            stats.total_allocated += layout.size();
            stats.current_allocated += layout.size();
            stats.peak_allocated = stats.peak_allocated.max(stats.current_allocated);
            stats.allocation_count += 1;
            
            // Update size histogram
            let bucket = size_to_bucket(layout.size());
            *stats.size_histogram.entry(bucket).or_insert(0) += 1;
        }
    }
    
    fn record_deallocation(&self, ptr: *mut u8) {
        let info = {
            let mut allocations = self.allocations.lock().unwrap();
            allocations.remove(&ptr)
        };
        
        if let Some(info) = info {
            let mut stats = self.stats.lock().unwrap();
            stats.total_deallocated += info.size;
            stats.current_allocated -= info.size;
            stats.deallocation_count += 1;
        }
    }
    
    fn report(&self) {
        let stats = self.stats.lock().unwrap();
        let allocations = self.allocations.lock().unwrap();
        
        println!("Allocation Report:");
        println!("  Total allocated: {} bytes", stats.total_allocated);
        println!("  Total deallocated: {} bytes", stats.total_deallocated);
        println!("  Currently allocated: {} bytes", stats.current_allocated);
        println!("  Peak allocated: {} bytes", stats.peak_allocated);
        println!("  Allocation count: {}", stats.allocation_count);
        println!("  Deallocation count: {}", stats.deallocation_count);
        println!("  Outstanding allocations: {}", allocations.len());
        
        println!("\nSize histogram:");
        let mut buckets: Vec<_> = stats.size_histogram.iter().collect();
        buckets.sort_by_key(|(size, _)| *size);
        for (bucket, count) in buckets {
            println!("  {} bytes: {} allocations", bucket, count);
        }
    }
}

fn size_to_bucket(size: usize) -> usize {
    // Round up to next power of 2 for bucketing
    if size == 0 {
        0
    } else {
        1 << (64 - (size - 1).leading_zeros())
    }
}

unsafe impl GlobalAlloc for DetailedTracker {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let ptr = System.alloc(layout);
        if !ptr.is_null() {
            self.record_allocation(ptr, layout);
        }
        ptr
    }
    
    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        self.record_deallocation(ptr);
        System.dealloc(ptr, layout);
    }
}

// Usage with scoped tracking
struct AllocationScope {
    tracker: &'static DetailedTracker,
    initial_stats: DetailedStats,
}

impl AllocationScope {
    fn new(tracker: &'static DetailedTracker) -> Self {
        let initial_stats = tracker.stats.lock().unwrap().clone();
        Self {
            tracker,
            initial_stats,
        }
    }
    
    fn report_delta(&self) {
        let current_stats = self.tracker.stats.lock().unwrap();
        
        println!("Allocation delta:");
        println!("  Allocated: {} bytes", 
            current_stats.total_allocated - self.initial_stats.total_allocated);
        println!("  Deallocated: {} bytes", 
            current_stats.total_deallocated - self.initial_stats.total_deallocated);
        println!("  Net change: {} bytes", 
            (current_stats.current_allocated as isize) - (self.initial_stats.current_allocated as isize));
    }
}
```

## Cache Performance Analysis

### Cache Miss Simulation

```rust
use std::time::Instant;

// Simulate different cache access patterns
fn analyze_cache_behavior() {
    const SIZE: usize = 16 * 1024 * 1024; // 16MB array
    const CACHE_LINE_SIZE: usize = 64;
    const L1_CACHE_SIZE: usize = 32 * 1024; // 32KB
    const L2_CACHE_SIZE: usize = 256 * 1024; // 256KB
    const L3_CACHE_SIZE: usize = 8 * 1024 * 1024; // 8MB
    
    let mut data = vec![0u8; SIZE];
    
    // Test 1: Sequential access (L1 cache friendly)
    let start = Instant::now();
    for i in 0..L1_CACHE_SIZE {
        data[i] = (i % 256) as u8;
    }
    let l1_time = start.elapsed();
    
    // Test 2: Sequential access (L2 cache friendly)
    let start = Instant::now();
    for i in 0..L2_CACHE_SIZE {
        data[i] = (i % 256) as u8;
    }
    let l2_time = start.elapsed();
    
    // Test 3: Sequential access (L3 cache friendly)
    let start = Instant::now();
    for i in 0..L3_CACHE_SIZE {
        data[i] = (i % 256) as u8;
    }
    let l3_time = start.elapsed();
    
    // Test 4: Sequential access (exceeds L3 cache)
    let start = Instant::now();
    for i in 0..SIZE {
        data[i] = (i % 256) as u8;
    }
    let ram_time = start.elapsed();
    
    // Test 5: Strided access (cache line aligned)
    let start = Instant::now();
    for i in (0..SIZE).step_by(CACHE_LINE_SIZE) {
        data[i] = (i % 256) as u8;
    }
    let strided_time = start.elapsed();
    
    // Test 6: Random access (cache unfriendly)
    let indices: Vec<usize> = (0..10000).map(|_| fastrand::usize(0..SIZE)).collect();
    let start = Instant::now();
    for &i in &indices {
        data[i] = (i % 256) as u8;
    }
    let random_time = start.elapsed();
    
    println!("Cache behavior analysis:");
    println!("L1 cache access: {:?}", l1_time);
    println!("L2 cache access: {:?}", l2_time);
    println!("L3 cache access: {:?}", l3_time);
    println!("RAM access: {:?}", ram_time);
    println!("Strided access: {:?}", strided_time);
    println!("Random access: {:?}", random_time);
    
    // Calculate relative performance
    let l1_ns_per_access = l1_time.as_nanos() / L1_CACHE_SIZE as u128;
    let l2_ns_per_access = l2_time.as_nanos() / L2_CACHE_SIZE as u128;
    let l3_ns_per_access = l3_time.as_nanos() / L3_CACHE_SIZE as u128;
    let ram_ns_per_access = ram_time.as_nanos() / SIZE as u128;
    let random_ns_per_access = random_time.as_nanos() / indices.len() as u128;
    
    println!("\nNanoseconds per access:");
    println!("L1: {} ns", l1_ns_per_access);
    println!("L2: {} ns", l2_ns_per_access);
    println!("L3: {} ns", l3_ns_per_access);
    println!("RAM: {} ns", ram_ns_per_access);
    println!("Random: {} ns", random_ns_per_access);
}
```

### Cache-Friendly vs Cache-Unfriendly Patterns

```rust
use std::time::Instant;

// Compare Array of Structures vs Structure of Arrays
#[derive(Clone, Copy)]
struct Particle {
    x: f32,
    y: f32,
    z: f32,
    vx: f32,
    vy: f32,
    vz: f32,
}

struct ParticlesSoA {
    x: Vec<f32>,
    y: Vec<f32>,
    z: Vec<f32>,
    vx: Vec<f32>,
    vy: Vec<f32>,
    vz: Vec<f32>,
}

fn benchmark_data_layout() {
    const COUNT: usize = 1_000_000;
    
    // Array of Structures
    let mut particles_aos = vec![Particle {
        x: 0.0, y: 0.0, z: 0.0,
        vx: 1.0, vy: 1.0, vz: 1.0,
    }; COUNT];
    
    // Structure of Arrays
    let mut particles_soa = ParticlesSoA {
        x: vec![0.0; COUNT],
        y: vec![0.0; COUNT],
        z: vec![0.0; COUNT],
        vx: vec![1.0; COUNT],
        vy: vec![1.0; COUNT],
        vz: vec![1.0; COUNT],
    };
    
    let dt = 0.016; // 60 FPS
    
    // Benchmark AoS: Update positions
    let start = Instant::now();
    for particle in &mut particles_aos {
        particle.x += particle.vx * dt;
        particle.y += particle.vy * dt;
        particle.z += particle.vz * dt;
    }
    let aos_time = start.elapsed();
    
    // Benchmark SoA: Update positions
    let start = Instant::now();
    for i in 0..COUNT {
        particles_soa.x[i] += particles_soa.vx[i] * dt;
        particles_soa.y[i] += particles_soa.vy[i] * dt;
        particles_soa.z[i] += particles_soa.vz[i] * dt;
    }
    let soa_time = start.elapsed();
    
    // Benchmark SoA: Update only X positions (better cache utilization)
    let start = Instant::now();
    for i in 0..COUNT {
        particles_soa.x[i] += particles_soa.vx[i] * dt;
    }
    let soa_x_only_time = start.elapsed();
    
    println!("Data layout benchmark:");
    println!("AoS (all positions): {:?}", aos_time);
    println!("SoA (all positions): {:?}", soa_time);
    println!("SoA (X only): {:?}", soa_x_only_time);
    
    println!("SoA speedup (all): {:.2}x", 
        aos_time.as_nanos() as f64 / soa_time.as_nanos() as f64);
    println!("SoA speedup (X only): {:.2}x", 
        aos_time.as_nanos() as f64 / soa_x_only_time.as_nanos() as f64);
}
```

## Continuous Performance Monitoring

### Performance Regression Detection

```rust
use std::collections::HashMap;
use std::time::Duration;
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
struct BenchmarkResult {
    name: String,
    duration: Duration,
    timestamp: std::time::SystemTime,
    commit_hash: String,
    metadata: HashMap<String, String>,
}

#[derive(Serialize, Deserialize, Debug)]
struct PerformanceHistory {
    results: Vec<BenchmarkResult>,
}

impl PerformanceHistory {
    fn new() -> Self {
        Self {
            results: Vec::new(),
        }
    }
    
    fn add_result(&mut self, result: BenchmarkResult) {
        self.results.push(result);
        
        // Keep only last 100 results
        if self.results.len() > 100 {
            self.results.remove(0);
        }
    }
    
    fn detect_regression(&self, benchmark_name: &str, threshold: f64) -> Option<RegressionInfo> {
        let recent_results: Vec<_> = self.results
            .iter()
            .filter(|r| r.name == benchmark_name)
            .collect();
        
        if recent_results.len() < 10 {
            return None; // Not enough data
        }
        
        // Compare last result with median of previous 10
        let last_result = recent_results.last()?;
        let previous_results = &recent_results[recent_results.len() - 11..recent_results.len() - 1];
        
        let mut previous_times: Vec<_> = previous_results
            .iter()
            .map(|r| r.duration.as_nanos())
            .collect();
        previous_times.sort();
        
        let median_time = previous_times[previous_times.len() / 2];
        let current_time = last_result.duration.as_nanos();
        
        let regression_ratio = current_time as f64 / median_time as f64;
        
        if regression_ratio > 1.0 + threshold {
            Some(RegressionInfo {
                benchmark_name: benchmark_name.to_string(),
                regression_ratio,
                current_time: last_result.duration,
                baseline_time: Duration::from_nanos(median_time as u64),
                commit_hash: last_result.commit_hash.clone(),
            })
        } else {
            None
        }
    }
    
    fn save_to_file(&self, path: &str) -> Result<(), Box<dyn std::error::Error>> {
        let json = serde_json::to_string_pretty(self)?;
        std::fs::write(path, json)?;
        Ok(())
    }
    
    fn load_from_file(path: &str) -> Result<Self, Box<dyn std::error::Error>> {
        let json = std::fs::read_to_string(path)?;
        let history = serde_json::from_str(&json)?;
        Ok(history)
    }
}

#[derive(Debug)]
struct RegressionInfo {
    benchmark_name: String,
    regression_ratio: f64,
    current_time: Duration,
    baseline_time: Duration,
    commit_hash: String,
}

// CI integration example
fn run_performance_ci() {
    let mut history = PerformanceHistory::load_from_file("performance_history.json")
        .unwrap_or_else(|_| PerformanceHistory::new());
    
    // Run benchmarks
    let benchmarks = [
        ("vec_creation", || benchmark_vec_creation()),
        ("hashmap_insertion", || benchmark_hashmap_insertion()),
        ("arena_allocation", || benchmark_arena_allocation()),
    ];
    
    let commit_hash = std::env::var("GIT_COMMIT").unwrap_or_else(|_| "unknown".to_string());
    
    for (name, benchmark_fn) in &benchmarks {
        let duration = time_function(benchmark_fn);
        
        let result = BenchmarkResult {
            name: name.to_string(),
            duration,
            timestamp: std::time::SystemTime::now(),
            commit_hash: commit_hash.clone(),
            metadata: HashMap::new(),
        };
        
        history.add_result(result);
        
        // Check for regression
        if let Some(regression) = history.detect_regression(name, 0.1) { // 10% threshold
            eprintln!("Performance regression detected in {}: {:.2}x slower", 
                regression.benchmark_name, regression.regression_ratio);
            eprintln!("Current: {:?}, Baseline: {:?}", 
                regression.current_time, regression.baseline_time);
            
            // In CI, this would fail the build
            std::process::exit(1);
        }
    }
    
    history.save_to_file("performance_history.json").unwrap();
}

fn time_function<F>(f: F) -> Duration
where 
    F: FnOnce(),
{
    let start = std::time::Instant::now();
    f();
    start.elapsed()
}

fn benchmark_vec_creation() {
    let mut v = Vec::new();
    for i in 0..10000 {
        v.push(i);
    }
}

fn benchmark_hashmap_insertion() {
    let mut map = std::collections::HashMap::new();
    for i in 0..10000 {
        map.insert(i, i * 2);
    }
}

fn benchmark_arena_allocation() {
    let bump = bumpalo::Bump::new();
    for i in 0..10000 {
        bump.alloc(i);
    }
}
```

## Statistical Analysis

### Statistical Significance Testing

```rust
// Statistical analysis for benchmark results
struct StatisticalAnalysis {
    samples: Vec<f64>,
}

impl StatisticalAnalysis {
    fn new(samples: Vec<f64>) -> Self {
        Self { samples }
    }
    
    fn mean(&self) -> f64 {
        self.samples.iter().sum::<f64>() / self.samples.len() as f64
    }
    
    fn variance(&self) -> f64 {
        let mean = self.mean();
        let sum_sq_diff: f64 = self.samples
            .iter()
            .map(|x| (x - mean).powi(2))
            .sum();
        sum_sq_diff / (self.samples.len() - 1) as f64
    }
    
    fn std_dev(&self) -> f64 {
        self.variance().sqrt()
    }
    
    fn confidence_interval(&self, confidence: f64) -> (f64, f64) {
        let mean = self.mean();
        let std_err = self.std_dev() / (self.samples.len() as f64).sqrt();
        
        // Simplified - would use t-distribution for small samples
        let z_score = match confidence {
            0.95 => 1.96,
            0.99 => 2.576,
            _ => 1.96,
        };
        
        let margin = z_score * std_err;
        (mean - margin, mean + margin)
    }
    
    fn is_significantly_different(&self, other: &StatisticalAnalysis, alpha: f64) -> bool {
        // Simplified t-test
        let mean1 = self.mean();
        let mean2 = other.mean();
        let var1 = self.variance();
        let var2 = other.variance();
        let n1 = self.samples.len() as f64;
        let n2 = other.samples.len() as f64;
        
        let pooled_std_err = ((var1 / n1) + (var2 / n2)).sqrt();
        let t_stat = (mean1 - mean2).abs() / pooled_std_err;
        
        // Simplified critical value (would use proper t-distribution)
        let critical_value = match alpha {
            0.05 => 1.96,
            0.01 => 2.576,
            _ => 1.96,
        };
        
        t_stat > critical_value
    }
}

// Example usage
fn statistical_benchmark_comparison() {
    // Collect multiple samples for each approach
    let mut vec_times = Vec::new();
    let mut smallvec_times = Vec::new();
    
    for _ in 0..50 {
        // Benchmark Vec
        let start = std::time::Instant::now();
        let mut v = Vec::new();
        for i in 0..100 {
            v.push(i);
        }
        vec_times.push(start.elapsed().as_nanos() as f64);
        
        // Benchmark SmallVec
        let start = std::time::Instant::now();
        let mut v = smallvec::SmallVec::<[i32; 8]>::new();
        for i in 0..100 {
            v.push(i);
        }
        smallvec_times.push(start.elapsed().as_nanos() as f64);
    }
    
    let vec_analysis = StatisticalAnalysis::new(vec_times);
    let smallvec_analysis = StatisticalAnalysis::new(smallvec_times);
    
    println!("Vec: mean = {:.2} ns, std_dev = {:.2} ns", 
        vec_analysis.mean(), vec_analysis.std_dev());
    println!("SmallVec: mean = {:.2} ns, std_dev = {:.2} ns", 
        smallvec_analysis.mean(), smallvec_analysis.std_dev());
    
    let (vec_low, vec_high) = vec_analysis.confidence_interval(0.95);
    let (smallvec_low, smallvec_high) = smallvec_analysis.confidence_interval(0.95);
    
    println!("Vec 95% CI: [{:.2}, {:.2}]", vec_low, vec_high);
    println!("SmallVec 95% CI: [{:.2}, {:.2}]", smallvec_low, smallvec_high);
    
    if vec_analysis.is_significantly_different(&smallvec_analysis, 0.05) {
        println!("Difference is statistically significant (p < 0.05)");
    } else {
        println!("Difference is not statistically significant");
    }
}
```

## Best Practices

### 1. Establish Baselines

```rust
// Always establish performance baselines before optimizing
fn establish_baseline() {
    println!("Establishing performance baseline...");
    
    // Measure current performance
    let baseline_results = run_comprehensive_benchmarks();
    
    // Save baseline for future comparison
    save_baseline(&baseline_results);
    
    println!("Baseline established. Use this for future comparisons.");
}

fn run_comprehensive_benchmarks() -> BenchmarkSuite {
    // Implementation would run all relevant benchmarks
    BenchmarkSuite::new()
}

struct BenchmarkSuite {
    // Benchmark results
}

impl BenchmarkSuite {
    fn new() -> Self {
        Self {}
    }
}

fn save_baseline(results: &BenchmarkSuite) {
    // Save to file for future reference
}
```

### 2. Use Representative Data

```rust
// Use realistic data sizes and patterns
fn benchmark_with_realistic_data() {
    // ❌ Don't benchmark with tiny datasets
    // let small_data = vec![1, 2, 3];
    
    // ✅ Use representative sizes
    let realistic_sizes = [100, 1_000, 10_000, 100_000];
    
    for &size in &realistic_sizes {
        let data: Vec<i32> = (0..size).collect();
        
        // Benchmark with this size
        let duration = time_function(|| {
            process_data(&data);
        });
        
        println!("Size {}: {:?}", size, duration);
    }
}

fn process_data(data: &[i32]) {
    // Your processing logic
    let _sum: i32 = data.iter().sum();
}
```

### 3. Control for External Factors

```rust
// Minimize external interference
fn controlled_benchmark() {
    // Warm up the system
    for _ in 0..10 {
        expensive_operation();
    }
    
    // Disable frequency scaling if possible
    // Set process priority
    // Ensure consistent system state
    
    // Run actual benchmark
    let results = benchmark_function(expensive_operation, 100);
    
    println!("Controlled benchmark results: {:?}", results);
}

fn expensive_operation() {
    // Your operation here
}
```

### 4. Measure What Matters

```rust
// Focus on metrics that matter for your use case
struct CompilerMetrics {
    parse_time: Duration,
    type_check_time: Duration,
    codegen_time: Duration,
    memory_peak: usize,
    allocations: usize,
}

fn measure_compiler_performance() -> CompilerMetrics {
    let start_memory = get_memory_usage();
    let allocation_tracker = AllocationTracker::new();
    
    let parse_start = Instant::now();
    let ast = parse_source_code();
    let parse_time = parse_start.elapsed();
    
    let typecheck_start = Instant::now();
    let typed_ast = type_check(ast);
    let type_check_time = typecheck_start.elapsed();
    
    let codegen_start = Instant::now();
    let code = generate_code(typed_ast);
    let codegen_time = codegen_start.elapsed();
    
    let end_memory = get_memory_usage();
    
    CompilerMetrics {
        parse_time,
        type_check_time,
        codegen_time,
        memory_peak: end_memory,
        allocations: allocation_tracker.count(),
    }
}

// Placeholder implementations
fn get_memory_usage() -> usize { 0 }
fn parse_source_code() -> () { }
fn type_check(_ast: ()) -> () { }
fn generate_code(_typed_ast: ()) -> () { }

struct AllocationTracker;
impl AllocationTracker {
    fn new() -> Self { Self }
    fn count(&self) -> usize { 0 }
}
```

---

This comprehensive guide covers the essential aspects of benchmarking memory usage and performance measurement in Rust applications. These techniques are crucial for optimizing performance-critical applications like compilers, where understanding allocation patterns, cache behavior, and performance characteristics is essential for achieving optimal performance.