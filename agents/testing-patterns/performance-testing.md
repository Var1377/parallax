# Performance Testing Patterns

Performance testing ensures your code meets speed, memory, and scalability requirements. This guide covers benchmarking, performance regression detection, and optimization strategies using Rust's testing ecosystem.

## Table of Contents

- [Performance Testing Strategy](#performance-testing-strategy)
- [Benchmarking with Criterion](#benchmarking-with-criterion)
- [Memory Usage Testing](#memory-usage-testing)
- [Scalability Testing](#scalability-testing)
- [Performance Regression Detection](#performance-regression-detection)
- [Profiling Integration](#profiling-integration)
- [Load Testing](#load-testing)
- [Performance CI/CD Integration](#performance-cicd-integration)

## Performance Testing Strategy

### Performance Test Pyramid

```rust
// Performance test organization
benches/
├── micro_benchmarks.rs      // Individual function performance
├── integration_benchmarks.rs // Component interaction performance
├── system_benchmarks.rs     // End-to-end system performance
└── stress_tests.rs          // Resource limits and breaking points

tests/
├── performance/
│   ├── regression_tests.rs  // Performance regression detection
│   ├── memory_tests.rs      // Memory usage validation
│   └── scalability_tests.rs // Scalability validation
```

### Performance Requirements Definition

```rust
// tests/performance/requirements.rs
pub struct PerformanceRequirements {
    pub max_execution_time: std::time::Duration,
    pub max_memory_usage: usize,
    pub min_throughput: f64,
    pub max_latency_p99: std::time::Duration,
}

impl PerformanceRequirements {
    pub fn for_parser() -> Self {
        Self {
            max_execution_time: std::time::Duration::from_millis(100),
            max_memory_usage: 50 * 1024 * 1024, // 50MB
            min_throughput: 1000.0, // operations per second
            max_latency_p99: std::time::Duration::from_millis(10),
        }
    }
    
    pub fn for_compiler() -> Self {
        Self {
            max_execution_time: std::time::Duration::from_secs(5),
            max_memory_usage: 500 * 1024 * 1024, // 500MB
            min_throughput: 100.0,
            max_latency_p99: std::time::Duration::from_millis(100),
        }
    }
}

#[test]
fn test_parser_performance_requirements() {
    let requirements = PerformanceRequirements::for_parser();
    let test_input = load_test_program("medium_complexity.px");
    
    let start = std::time::Instant::now();
    let result = parse(&test_input);
    let duration = start.elapsed();
    
    assert!(result.is_ok(), "Parser should succeed");
    assert!(
        duration <= requirements.max_execution_time,
        "Parser took {:?}, expected <= {:?}",
        duration,
        requirements.max_execution_time
    );
}
```

## Benchmarking with Criterion

### Basic Benchmarking Setup

```toml
# Cargo.toml
[dev-dependencies]
criterion = { version = "0.5", features = ["html_reports"] }

[[bench]]
name = "parser_benchmarks"
harness = false

[[bench]]
name = "compiler_benchmarks"
harness = false
```

### Comprehensive Benchmark Suite

```rust
// benches/parser_benchmarks.rs
use criterion::{
    black_box, criterion_group, criterion_main, 
    Criterion, BenchmarkId, Throughput, PlotConfiguration, AxisScale
};
use my_compiler::*;

fn benchmark_parser_sizes(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser_input_sizes");
    
    // Configure plotting
    group.plot_config(PlotConfiguration::default().summary_scale(AxisScale::Logarithmic));
    
    for size in [100, 500, 1000, 5000, 10000].iter() {
        let input = generate_test_program(*size);
        
        // Set throughput for better analysis
        group.throughput(Throughput::Bytes(input.len() as u64));
        
        group.bench_with_input(
            BenchmarkId::new("parse", size),
            &input,
            |b, input| {
                b.iter(|| parse(black_box(input)))
            },
        );
    }
    
    group.finish();
}

fn benchmark_parser_complexity(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser_complexity");
    
    let test_cases = vec![
        ("simple_function", include_str!("../tests/fixtures/simple.px")),
        ("nested_expressions", include_str!("../tests/fixtures/nested.px")),
        ("complex_control_flow", include_str!("../tests/fixtures/complex.px")),
        ("deeply_nested", include_str!("../tests/fixtures/deep.px")),
    ];
    
    for (name, input) in test_cases {
        group.bench_function(name, |b| {
            b.iter(|| parse(black_box(input)))
        });
    }
    
    group.finish();
}

fn benchmark_parser_memory_patterns(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser_memory");
    
    // Test different memory allocation patterns
    group.bench_function("many_small_allocations", |b| {
        b.iter(|| {
            let input = generate_many_small_functions(1000);
            parse(black_box(&input))
        })
    });
    
    group.bench_function("few_large_allocations", |b| {
        b.iter(|| {
            let input = generate_large_function(10000);
            parse(black_box(&input))
        })
    });
    
    group.finish();
}

fn benchmark_parser_regression(c: &mut Criterion) {
    // Regression benchmarks for specific performance issues
    let regression_cases = vec![
        ("issue_123_slow_parsing", load_regression_case("issue_123.px")),
        ("issue_456_memory_leak", load_regression_case("issue_456.px")),
    ];
    
    let mut group = c.benchmark_group("parser_regressions");
    
    for (name, input) in regression_cases {
        group.bench_function(name, |b| {
            b.iter(|| parse(black_box(&input)))
        });
    }
    
    group.finish();
}

criterion_group!(
    benches,
    benchmark_parser_sizes,
    benchmark_parser_complexity,
    benchmark_parser_memory_patterns,
    benchmark_parser_regression
);
criterion_main!(benches);
```

### Advanced Benchmark Patterns

```rust
// benches/advanced_benchmarks.rs
use criterion::*;
use std::hint::black_box;

fn benchmark_with_setup_and_teardown(c: &mut Criterion) {
    let mut group = c.benchmark_group("with_setup");
    
    group.bench_function("expensive_setup", |b| {
        b.iter_batched(
            || {
                // Setup phase (not measured)
                create_expensive_test_data()
            },
            |test_data| {
                // Measured phase
                process_data(black_box(test_data))
            },
            BatchSize::SmallInput,
        )
    });
    
    group.finish();
}

fn benchmark_with_custom_measurement(c: &mut Criterion) {
    let mut group = c.benchmark_group("custom_measurement");
    
    group.bench_function("memory_allocations", |b| {
        b.iter_custom(|iters| {
            let start_memory = get_memory_usage();
            let start_time = std::time::Instant::now();
            
            for _ in 0..iters {
                let result = allocate_and_process(black_box(1000));
                black_box(result);
            }
            
            let end_time = std::time::Instant::now();
            let end_memory = get_memory_usage();
            
            println!("Memory used: {} bytes", end_memory - start_memory);
            end_time - start_time
        })
    });
    
    group.finish();
}

fn benchmark_async_operations(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    
    c.bench_function("async_operation", |b| {
        b.to_async(&rt).iter(|| async {
            let result = async_process_data(black_box("test input")).await;
            black_box(result)
        })
    });
}

fn benchmark_with_statistical_analysis(c: &mut Criterion) {
    let mut group = c.benchmark_group("statistical_analysis");
    
    // Configure for more precise measurements
    group.sample_size(1000);
    group.measurement_time(std::time::Duration::from_secs(10));
    group.warm_up_time(std::time::Duration::from_secs(3));
    
    group.bench_function("precise_measurement", |b| {
        b.iter(|| {
            // Operation with high variance
            variable_performance_operation(black_box(42))
        })
    });
    
    group.finish();
}
```

## Memory Usage Testing

### Memory Tracking Infrastructure

```rust
// tests/performance/memory.rs
use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering};

struct TrackingAllocator;

static ALLOCATED: AtomicUsize = AtomicUsize::new(0);
static DEALLOCATED: AtomicUsize = AtomicUsize::new(0);

unsafe impl GlobalAlloc for TrackingAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let ptr = System.alloc(layout);
        if !ptr.is_null() {
            ALLOCATED.fetch_add(layout.size(), Ordering::Relaxed);
        }
        ptr
    }
    
    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        System.dealloc(ptr, layout);
        DEALLOCATED.fetch_add(layout.size(), Ordering::Relaxed);
    }
}

#[global_allocator]
static GLOBAL: TrackingAllocator = TrackingAllocator;

pub fn reset_memory_tracking() {
    ALLOCATED.store(0, Ordering::Relaxed);
    DEALLOCATED.store(0, Ordering::Relaxed);
}

pub fn get_memory_stats() -> (usize, usize, usize) {
    let allocated = ALLOCATED.load(Ordering::Relaxed);
    let deallocated = DEALLOCATED.load(Ordering::Relaxed);
    let current = allocated - deallocated;
    (allocated, deallocated, current)
}

#[test]
fn test_memory_usage() {
    reset_memory_tracking();
    
    let test_data = create_test_data(1000);
    let (allocated_before, _, current_before) = get_memory_stats();
    
    let result = process_data(&test_data);
    let (allocated_after, _, current_after) = get_memory_stats();
    
    let memory_used = allocated_after - allocated_before;
    let memory_retained = current_after - current_before;
    
    println!("Memory used: {} bytes", memory_used);
    println!("Memory retained: {} bytes", memory_retained);
    
    // Assert memory usage is within bounds
    assert!(memory_used < 10 * 1024 * 1024, "Used too much memory: {} bytes", memory_used);
    assert!(memory_retained < 1024 * 1024, "Retained too much memory: {} bytes", memory_retained);
    
    assert!(result.is_ok());
}
```

### Memory Leak Detection

```rust
// tests/performance/leak_detection.rs
use std::sync::Arc;
use std::collections::HashMap;

#[test]
fn test_memory_leak_detection() {
    let initial_memory = get_system_memory_usage();
    
    // Run operation multiple times to amplify leaks
    for i in 0..1000 {
        let data = create_data_structure(i);
        process_and_drop(data);
        
        // Check memory every 100 iterations
        if i % 100 == 0 {
            let current_memory = get_system_memory_usage();
            let memory_growth = current_memory - initial_memory;
            
            // Allow some growth but detect significant leaks
            assert!(
                memory_growth < 50 * 1024 * 1024, // 50MB
                "Potential memory leak detected at iteration {}: {} bytes growth",
                i, memory_growth
            );
        }
    }
    
    // Force garbage collection if applicable
    std::thread::sleep(std::time::Duration::from_millis(100));
    
    let final_memory = get_system_memory_usage();
    let total_growth = final_memory - initial_memory;
    
    assert!(
        total_growth < 10 * 1024 * 1024, // 10MB
        "Memory leak detected: {} bytes not freed",
        total_growth
    );
}

#[test]
fn test_reference_cycle_detection() {
    use std::rc::{Rc, Weak};
    use std::cell::RefCell;
    
    #[derive(Debug)]
    struct Node {
        value: i32,
        children: RefCell<Vec<Rc<Node>>>,
        parent: RefCell<Weak<Node>>,
    }
    
    impl Node {
        fn new(value: i32) -> Rc<Self> {
            Rc::new(Node {
                value,
                children: RefCell::new(Vec::new()),
                parent: RefCell::new(Weak::new()),
            })
        }
    }
    
    let initial_memory = get_system_memory_usage();
    
    // Create potential reference cycles
    for _ in 0..1000 {
        let parent = Node::new(1);
        let child = Node::new(2);
        
        // This could create a cycle if not handled properly
        parent.children.borrow_mut().push(child.clone());
        *child.parent.borrow_mut() = Rc::downgrade(&parent);
        
        // Explicitly break cycles
        parent.children.borrow_mut().clear();
    }
    
    let final_memory = get_system_memory_usage();
    let memory_growth = final_memory - initial_memory;
    
    assert!(
        memory_growth < 5 * 1024 * 1024, // 5MB
        "Reference cycle leak detected: {} bytes",
        memory_growth
    );
}

#[cfg(target_os = "linux")]
fn get_system_memory_usage() -> usize {
    let status = std::fs::read_to_string("/proc/self/status").unwrap();
    for line in status.lines() {
        if line.starts_with("VmRSS:") {
            let parts: Vec<&str> = line.split_whitespace().collect();
            return parts[1].parse::<usize>().unwrap() * 1024; // Convert KB to bytes
        }
    }
    0
}

#[cfg(not(target_os = "linux"))]
fn get_system_memory_usage() -> usize {
    // Platform-specific implementation or use a crate like `sysinfo`
    0
}
```

## Scalability Testing

### Load Scalability Tests

```rust
// tests/performance/scalability.rs
use rayon::prelude::*;
use std::sync::Arc;
use std::time::Instant;

#[test]
fn test_cpu_scalability() {
    let test_data = generate_large_dataset(10000);
    let data = Arc::new(test_data);
    
    // Test with different thread counts
    for thread_count in [1, 2, 4, 8, 16] {
        let pool = rayon::ThreadPoolBuilder::new()
            .num_threads(thread_count)
            .build()
            .unwrap();
        
        let start = Instant::now();
        
        pool.install(|| {
            data.par_iter()
                .map(|item| expensive_computation(item))
                .collect::<Vec<_>>()
        });
        
        let duration = start.elapsed();
        
        println!("Threads: {}, Time: {:?}", thread_count, duration);
        
        // Verify reasonable scaling
        if thread_count > 1 {
            // Should see some improvement with more threads
            // (This is a simplified check - real scaling analysis is more complex)
            assert!(
                duration < std::time::Duration::from_secs(60),
                "Processing took too long with {} threads: {:?}",
                thread_count, duration
            );
        }
    }
}

#[test]
fn test_memory_scalability() {
    // Test how performance degrades with increasing memory usage
    for data_size in [1000, 10000, 100000, 1000000] {
        let start_memory = get_system_memory_usage();
        let start_time = Instant::now();
        
        let data = generate_test_data(data_size);
        let result = process_large_dataset(&data);
        
        let end_time = Instant::now();
        let end_memory = get_system_memory_usage();
        
        let duration = end_time - start_time;
        let memory_used = end_memory - start_memory;
        
        println!(
            "Data size: {}, Time: {:?}, Memory: {} MB",
            data_size,
            duration,
            memory_used / (1024 * 1024)
        );
        
        // Check for reasonable scaling
        let expected_max_time = std::time::Duration::from_millis(data_size as u64 / 100);
        assert!(
            duration < expected_max_time,
            "Processing {} items took too long: {:?}",
            data_size, duration
        );
        
        assert!(result.is_ok());
    }
}

#[test]
fn test_concurrent_access_scalability() {
    use std::sync::{Arc, Mutex};
    use std::thread;
    
    let shared_data = Arc::new(Mutex::new(HashMap::new()));
    let mut handles = vec![];
    
    // Test with increasing concurrency
    for thread_count in [1, 2, 4, 8, 16] {
        let start = Instant::now();
        
        for i in 0..thread_count {
            let data = shared_data.clone();
            let handle = thread::spawn(move || {
                for j in 0..1000 {
                    let key = format!("thread_{}_item_{}", i, j);
                    let mut map = data.lock().unwrap();
                    map.insert(key, j);
                }
            });
            handles.push(handle);
        }
        
        for handle in handles.drain(..) {
            handle.join().unwrap();
        }
        
        let duration = start.elapsed();
        
        println!("Concurrent threads: {}, Time: {:?}", thread_count, duration);
        
        // Verify the data is correct
        let final_data = shared_data.lock().unwrap();
        assert_eq!(final_data.len(), thread_count * 1000);
        
        // Clear for next iteration
        shared_data.lock().unwrap().clear();
    }
}
```

### Algorithmic Complexity Testing

```rust
// tests/performance/complexity.rs
use std::time::Instant;

#[test]
fn test_algorithmic_complexity() {
    let mut measurements = Vec::new();
    
    // Test with increasing input sizes
    for size in [100, 200, 400, 800, 1600, 3200] {
        let input = generate_sorted_data(size);
        
        let start = Instant::now();
        let result = binary_search_algorithm(&input, size / 2);
        let duration = start.elapsed();
        
        measurements.push((size, duration));
        
        assert!(result.is_some(), "Should find element in sorted data");
        
        println!("Size: {}, Time: {:?}", size, duration);
    }
    
    // Verify O(log n) complexity
    for i in 1..measurements.len() {
        let (size1, time1) = measurements[i - 1];
        let (size2, time2) = measurements[i];
        
        let size_ratio = size2 as f64 / size1 as f64;
        let time_ratio = time2.as_nanos() as f64 / time1.as_nanos() as f64;
        
        // For O(log n), time ratio should be less than size ratio
        assert!(
            time_ratio < size_ratio,
            "Algorithm appears to be worse than O(log n): size ratio {:.2}, time ratio {:.2}",
            size_ratio, time_ratio
        );
    }
}

#[test]
fn test_sorting_complexity() {
    let mut measurements = Vec::new();
    
    for size in [1000, 2000, 4000, 8000] {
        let mut data = generate_random_data(size);
        
        let start = Instant::now();
        data.sort();
        let duration = start.elapsed();
        
        measurements.push((size, duration));
        
        // Verify it's actually sorted
        assert!(data.windows(2).all(|w| w[0] <= w[1]));
        
        println!("Sort size: {}, Time: {:?}", size, duration);
    }
    
    // Verify O(n log n) complexity
    for i in 1..measurements.len() {
        let (size1, time1) = measurements[i - 1];
        let (size2, time2) = measurements[i];
        
        let size_ratio = size2 as f64 / size1 as f64;
        let time_ratio = time2.as_nanos() as f64 / time1.as_nanos() as f64;
        let expected_ratio = size_ratio * size_ratio.log2();
        
        // Allow some variance in measurements
        assert!(
            time_ratio < expected_ratio * 2.0,
            "Sorting appears to be worse than O(n log n): expected ratio {:.2}, actual ratio {:.2}",
            expected_ratio, time_ratio
        );
    }
}
```

## Performance Regression Detection

### Baseline Performance Storage

```rust
// tests/performance/baseline.rs
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Serialize, Deserialize)]
pub struct PerformanceBaseline {
    pub benchmarks: HashMap<String, BenchmarkBaseline>,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub git_commit: String,
    pub rust_version: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct BenchmarkBaseline {
    pub mean_time: f64,
    pub std_dev: f64,
    pub min_time: f64,
    pub max_time: f64,
    pub memory_usage: Option<usize>,
}

impl PerformanceBaseline {
    pub fn load_or_create(path: &str) -> Self {
        if let Ok(content) = std::fs::read_to_string(path) {
            serde_json::from_str(&content).unwrap_or_else(|_| Self::new())
        } else {
            Self::new()
        }
    }
    
    pub fn new() -> Self {
        Self {
            benchmarks: HashMap::new(),
            timestamp: chrono::Utc::now(),
            git_commit: get_git_commit(),
            rust_version: get_rust_version(),
        }
    }
    
    pub fn update_benchmark(&mut self, name: &str, baseline: BenchmarkBaseline) {
        self.benchmarks.insert(name.to_string(), baseline);
        self.timestamp = chrono::Utc::now();
    }
    
    pub fn save(&self, path: &str) -> Result<(), Box<dyn std::error::Error>> {
        let content = serde_json::to_string_pretty(self)?;
        std::fs::write(path, content)?;
        Ok(())
    }
    
    pub fn check_regression(&self, name: &str, current_time: f64, threshold: f64) -> bool {
        if let Some(baseline) = self.benchmarks.get(name) {
            let regression_threshold = baseline.mean_time * (1.0 + threshold);
            current_time > regression_threshold
        } else {
            false // No baseline to compare against
        }
    }
}

fn get_git_commit() -> String {
    std::process::Command::new("git")
        .args(&["rev-parse", "HEAD"])
        .output()
        .ok()
        .and_then(|output| String::from_utf8(output.stdout).ok())
        .map(|s| s.trim().to_string())
        .unwrap_or_else(|| "unknown".to_string())
}

fn get_rust_version() -> String {
    std::process::Command::new("rustc")
        .args(&["--version"])
        .output()
        .ok()
        .and_then(|output| String::from_utf8(output.stdout).ok())
        .map(|s| s.trim().to_string())
        .unwrap_or_else(|| "unknown".to_string())
}

#[test]
fn test_performance_regression() {
    let baseline_path = "performance_baseline.json";
    let mut baseline = PerformanceBaseline::load_or_create(baseline_path);
    
    // Run benchmark
    let start = Instant::now();
    let result = expensive_operation();
    let duration = start.elapsed();
    
    assert!(result.is_ok());
    
    let current_time = duration.as_secs_f64();
    let benchmark_name = "expensive_operation";
    
    // Check for regression (allow 10% slowdown)
    if baseline.check_regression(benchmark_name, current_time, 0.10) {
        panic!(
            "Performance regression detected in {}: current {:.3}s, baseline {:.3}s",
            benchmark_name,
            current_time,
            baseline.benchmarks[benchmark_name].mean_time
        );
    }
    
    // Update baseline if this is a new benchmark or improvement
    if !baseline.benchmarks.contains_key(benchmark_name) || 
       current_time < baseline.benchmarks[benchmark_name].mean_time {
        baseline.update_benchmark(benchmark_name, BenchmarkBaseline {
            mean_time: current_time,
            std_dev: 0.0, // Would need multiple runs to calculate
            min_time: current_time,
            max_time: current_time,
            memory_usage: None,
        });
        
        baseline.save(baseline_path).unwrap();
    }
}
```

### Continuous Performance Monitoring

```rust
// tests/performance/monitoring.rs
use std::time::Instant;
use std::collections::VecDeque;

pub struct PerformanceMonitor {
    measurements: VecDeque<f64>,
    max_samples: usize,
}

impl PerformanceMonitor {
    pub fn new(max_samples: usize) -> Self {
        Self {
            measurements: VecDeque::new(),
            max_samples,
        }
    }
    
    pub fn record_measurement(&mut self, duration: f64) {
        self.measurements.push_back(duration);
        if self.measurements.len() > self.max_samples {
            self.measurements.pop_front();
        }
    }
    
    pub fn mean(&self) -> f64 {
        if self.measurements.is_empty() {
            0.0
        } else {
            self.measurements.iter().sum::<f64>() / self.measurements.len() as f64
        }
    }
    
    pub fn std_dev(&self) -> f64 {
        if self.measurements.len() < 2 {
            0.0
        } else {
            let mean = self.mean();
            let variance = self.measurements.iter()
                .map(|x| (x - mean).powi(2))
                .sum::<f64>() / (self.measurements.len() - 1) as f64;
            variance.sqrt()
        }
    }
    
    pub fn detect_anomaly(&self, current: f64, threshold_std_devs: f64) -> bool {
        if self.measurements.len() < 10 {
            false // Need enough samples
        } else {
            let mean = self.mean();
            let std_dev = self.std_dev();
            (current - mean).abs() > threshold_std_devs * std_dev
        }
    }
}

#[test]
fn test_performance_monitoring() {
    let mut monitor = PerformanceMonitor::new(100);
    
    // Collect baseline measurements
    for _ in 0..50 {
        let start = Instant::now();
        let _result = normal_operation();
        let duration = start.elapsed().as_secs_f64();
        monitor.record_measurement(duration);
    }
    
    println!("Baseline mean: {:.6}s, std dev: {:.6}s", 
             monitor.mean(), monitor.std_dev());
    
    // Test current performance
    let start = Instant::now();
    let _result = normal_operation();
    let current_duration = start.elapsed().as_secs_f64();
    
    // Detect anomalies (3 standard deviations)
    if monitor.detect_anomaly(current_duration, 3.0) {
        panic!(
            "Performance anomaly detected: current {:.6}s, mean {:.6}s, std dev {:.6}s",
            current_duration, monitor.mean(), monitor.std_dev()
        );
    }
    
    monitor.record_measurement(current_duration);
}
```

## Profiling Integration

### CPU Profiling

```rust
// tests/performance/profiling.rs
#[cfg(feature = "profiling")]
use pprof;

#[test]
#[cfg(feature = "profiling")]
fn test_with_cpu_profiling() {
    let guard = pprof::ProfilerGuard::new(100).unwrap();
    
    // Run the code to profile
    for _ in 0..1000 {
        expensive_computation();
    }
    
    // Generate profile report
    if let Ok(report) = guard.report().build() {
        let file = std::fs::File::create("profile.svg").unwrap();
        report.flamegraph(file).unwrap();
        println!("CPU profile saved to profile.svg");
    }
}

#[test]
fn test_with_manual_profiling() {
    use std::collections::HashMap;
    use std::time::Instant;
    
    let mut profile_data = HashMap::new();
    
    // Profile parsing phase
    let start = Instant::now();
    let ast = parse_phase();
    profile_data.insert("parse", start.elapsed());
    
    // Profile type checking phase
    let start = Instant::now();
    let typed_ast = typecheck_phase(ast);
    profile_data.insert("typecheck", start.elapsed());
    
    // Profile code generation phase
    let start = Instant::now();
    let code = codegen_phase(typed_ast);
    profile_data.insert("codegen", start.elapsed());
    
    // Analyze profile data
    let total_time: std::time::Duration = profile_data.values().sum();
    
    for (phase, duration) in &profile_data {
        let percentage = duration.as_secs_f64() / total_time.as_secs_f64() * 100.0;
        println!("{}: {:?} ({:.1}%)", phase, duration, percentage);
        
        // Assert no phase takes too long
        assert!(
            percentage < 80.0,
            "Phase '{}' takes too much time: {:.1}%",
            phase, percentage
        );
    }
    
    assert!(code.is_ok());
}
```

### Memory Profiling

```rust
// tests/performance/memory_profiling.rs
#[cfg(feature = "jemalloc")]
use jemallocator;

#[cfg(feature = "jemalloc")]
#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

#[test]
#[cfg(feature = "jemalloc")]
fn test_with_memory_profiling() {
    use jemalloc_ctl::{stats, epoch};
    
    // Advance epoch to get fresh stats
    epoch::advance().unwrap();
    let allocated_start = stats::allocated::read().unwrap();
    
    // Run memory-intensive operation
    let data = create_large_data_structure();
    let result = process_data(&data);
    
    // Get memory stats
    epoch::advance().unwrap();
    let allocated_end = stats::allocated::read().unwrap();
    let memory_used = allocated_end - allocated_start;
    
    println!("Memory allocated: {} bytes", memory_used);
    
    // Assert memory usage is reasonable
    assert!(
        memory_used < 100 * 1024 * 1024, // 100MB
        "Used too much memory: {} bytes",
        memory_used
    );
    
    assert!(result.is_ok());
}

#[test]
fn test_memory_allocation_patterns() {
    let mut allocation_sizes = Vec::new();
    
    // Hook into allocation tracking
    reset_memory_tracking();
    
    for size in [100, 1000, 10000, 100000] {
        let before = get_memory_stats();
        
        let data = allocate_data_of_size(size);
        process_data(&data);
        
        let after = get_memory_stats();
        let allocated = after.0 - before.0;
        
        allocation_sizes.push((size, allocated));
        
        println!("Input size: {}, Memory allocated: {}", size, allocated);
    }
    
    // Analyze allocation patterns
    for i in 1..allocation_sizes.len() {
        let (size1, alloc1) = allocation_sizes[i - 1];
        let (size2, alloc2) = allocation_sizes[i];
        
        let size_ratio = size2 as f64 / size1 as f64;
        let alloc_ratio = alloc2 as f64 / alloc1 as f64;
        
        // Memory allocation should scale reasonably with input size
        assert!(
            alloc_ratio < size_ratio * 2.0,
            "Memory allocation scaling poorly: size ratio {:.2}, allocation ratio {:.2}",
            size_ratio, alloc_ratio
        );
    }
}
```

## Load Testing

### Concurrent Load Testing

```rust
// tests/performance/load_testing.rs
use std::sync::{Arc, Barrier};
use std::thread;
use std::time::{Duration, Instant};

#[test]
fn test_concurrent_load() {
    let num_threads = 10;
    let operations_per_thread = 1000;
    let barrier = Arc::new(Barrier::new(num_threads));
    let mut handles = vec![];
    
    let start_time = Arc::new(std::sync::Mutex::new(None));
    
    for thread_id in 0..num_threads {
        let barrier = barrier.clone();
        let start_time = start_time.clone();
        
        let handle = thread::spawn(move || {
            // Wait for all threads to be ready
            barrier.wait();
            
            // Record start time (only once)
            {
                let mut start = start_time.lock().unwrap();
                if start.is_none() {
                    *start = Some(Instant::now());
                }
            }
            
            let mut thread_results = Vec::new();
            
            // Perform operations
            for op_id in 0..operations_per_thread {
                let op_start = Instant::now();
                let result = perform_operation(thread_id, op_id);
                let op_duration = op_start.elapsed();
                
                thread_results.push((result.is_ok(), op_duration));
            }
            
            thread_results
        });
        
        handles.push(handle);
    }
    
    // Collect results from all threads
    let mut all_results = Vec::new();
    for handle in handles {
        let thread_results = handle.join().unwrap();
        all_results.extend(thread_results);
    }
    
    let total_start = start_time.lock().unwrap().unwrap();
    let total_duration = total_start.elapsed();
    
    // Analyze results
    let successful_ops = all_results.iter().filter(|(success, _)| *success).count();
    let total_ops = all_results.len();
    let success_rate = successful_ops as f64 / total_ops as f64;
    
    let total_throughput = total_ops as f64 / total_duration.as_secs_f64();
    
    let operation_times: Vec<Duration> = all_results.iter().map(|(_, duration)| *duration).collect();
    let avg_latency = operation_times.iter().sum::<Duration>() / operation_times.len() as u32;
    
    // Sort for percentile calculations
    let mut sorted_times = operation_times;
    sorted_times.sort();
    let p95_latency = sorted_times[(sorted_times.len() as f64 * 0.95) as usize];
    let p99_latency = sorted_times[(sorted_times.len() as f64 * 0.99) as usize];
    
    println!("Load test results:");
    println!("  Total operations: {}", total_ops);
    println!("  Success rate: {:.2}%", success_rate * 100.0);
    println!("  Throughput: {:.2} ops/sec", total_throughput);
    println!("  Average latency: {:?}", avg_latency);
    println!("  95th percentile latency: {:?}", p95_latency);
    println!("  99th percentile latency: {:?}", p99_latency);
    
    // Assert performance requirements
    assert!(success_rate > 0.99, "Success rate too low: {:.2}%", success_rate * 100.0);
    assert!(total_throughput > 100.0, "Throughput too low: {:.2} ops/sec", total_throughput);
    assert!(p99_latency < Duration::from_millis(100), "P99 latency too high: {:?}", p99_latency);
}

#[test]
fn test_sustained_load() {
    let duration = Duration::from_secs(30);
    let target_rps = 100; // requests per second
    let interval = Duration::from_millis(1000 / target_rps);
    
    let start_time = Instant::now();
    let mut operation_count = 0;
    let mut error_count = 0;
    let mut latencies = Vec::new();
    
    while start_time.elapsed() < duration {
        let op_start = Instant::now();
        
        match perform_operation(0, operation_count) {
            Ok(_) => {},
            Err(_) => error_count += 1,
        }
        
        let op_latency = op_start.elapsed();
        latencies.push(op_latency);
        operation_count += 1;
        
        // Maintain target rate
        let elapsed = op_start.elapsed();
        if elapsed < interval {
            thread::sleep(interval - elapsed);
        }
    }
    
    let total_duration = start_time.elapsed();
    let actual_rps = operation_count as f64 / total_duration.as_secs_f64();
    let error_rate = error_count as f64 / operation_count as f64;
    
    println!("Sustained load test results:");
    println!("  Duration: {:?}", total_duration);
    println!("  Operations: {}", operation_count);
    println!("  Actual RPS: {:.2}", actual_rps);
    println!("  Error rate: {:.2}%", error_rate * 100.0);
    
    // Assert sustained performance
    assert!(actual_rps > target_rps as f64 * 0.9, "RPS too low: {:.2}", actual_rps);
    assert!(error_rate < 0.01, "Error rate too high: {:.2}%", error_rate * 100.0);
}
```

## Performance CI/CD Integration

### GitHub Actions Performance Pipeline

```yaml
# .github/workflows/performance.yml
name: Performance Testing

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]
  schedule:
    - cron: '0 2 * * *'  # Nightly performance tests

jobs:
  benchmark:
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - uses: Swatinem/rust-cache@v2
      
      - name: Run benchmarks
        run: |
          cargo bench --bench '*' -- --output-format json | tee benchmark-results.json
          
      - name: Store benchmark results
        uses: benchmark-action/github-action-benchmark@v1
        with:
          tool: 'cargo'
          output-file-path: benchmark-results.json
          github-token: ${{ secrets.GITHUB_TOKEN }}
          auto-push: true
          alert-threshold: '150%'
          comment-on-alert: true
          fail-on-alert: true
          
  performance-tests:
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - uses: Swatinem/rust-cache@v2
      
      - name: Run performance regression tests
        run: cargo test --test performance_regression
        env:
          RUST_LOG: info
          
      - name: Upload performance baseline
        if: github.ref == 'refs/heads/main'
        uses: actions/upload-artifact@v3
        with:
          name: performance-baseline
          path: performance_baseline.json
          
  memory-tests:
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - uses: Swatinem/rust-cache@v2
      
      - name: Install memory profiling tools
        run: |
          sudo apt-get update
          sudo apt-get install -y valgrind
          
      - name: Run memory tests
        run: |
          cargo test --test memory_tests
          
      - name: Run with Valgrind
        run: |
          cargo build --tests
          valgrind --tool=memcheck --leak-check=full --error-exitcode=1 \
            target/debug/deps/memory_tests-*
```

---

Performance testing ensures your code meets speed, memory, and scalability requirements. By implementing comprehensive benchmarking, memory tracking, and regression detection, you can maintain high performance standards throughout your development lifecycle.