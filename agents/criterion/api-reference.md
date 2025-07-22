# Criterion.rs 0.6 API Reference

This comprehensive API reference covers all major types, traits, and functions in Criterion.rs 0.6 with practical examples.

## Core Types

### `Criterion<M>`

The main entry point for benchmarking. Generic over measurement type `M`.

```rust
use criterion::{Criterion, WallTime};

// Default criterion with wall-time measurement
let mut criterion = Criterion::default();

// Custom measurement type
let mut criterion = Criterion::<WallTime>::default();
```

#### Methods

##### `bench_function<F>`

Benchmark a single function.

```rust
use criterion::{Criterion, black_box};

fn bench_example(c: &mut Criterion) {
    c.bench_function("fibonacci", |b| {
        b.iter(|| black_box(fibonacci(black_box(20))))
    });
}

fn fibonacci(n: u64) -> u64 {
    match n {
        0 | 1 => 1,
        n => fibonacci(n - 1) + fibonacci(n - 2),
    }
}
```

##### `benchmark_group`

Create a benchmark group for related tests.

```rust
use criterion::{Criterion, BenchmarkId};

fn bench_group_example(c: &mut Criterion) {
    let mut group = c.benchmark_group("sorting");
    
    for size in [100, 1000, 10000].iter() {
        group.bench_with_input(
            BenchmarkId::new("bubble_sort", size),
            size,
            |b, &size| {
                b.iter_with_setup(
                    || (0..size).rev().collect::<Vec<_>>(),
                    |mut data| {
                        bubble_sort(&mut data);
                        data
                    }
                )
            }
        );
    }
    
    group.finish();
}
```

#### Configuration Methods

##### `sample_size`

Set the number of samples to collect.

```rust
use criterion::Criterion;

fn configured_benchmark(c: &mut Criterion) {
    let mut criterion = c.sample_size(200);  // 200 samples instead of default 100
    
    criterion.bench_function("high_precision", |b| {
        b.iter(|| expensive_computation())
    });
}
```

##### `measurement_time`

Set the target measurement time.

```rust
use criterion::Criterion;
use std::time::Duration;

fn timed_benchmark(c: &mut Criterion) {
    let mut criterion = c.measurement_time(Duration::from_secs(10));
    
    criterion.bench_function("long_measurement", |b| {
        b.iter(|| slow_computation())
    });
}
```

##### `warm_up_time`

Set the warm-up time before measurement.

```rust
use criterion::Criterion;
use std::time::Duration;

fn warmed_benchmark(c: &mut Criterion) {
    let mut criterion = c.warm_up_time(Duration::from_secs(2));
    
    criterion.bench_function("warmed_up", |b| {
        b.iter(|| computation_with_warmup())
    });
}
```

##### `confidence_level`

Set the confidence level for statistical analysis.

```rust
use criterion::Criterion;

fn confident_benchmark(c: &mut Criterion) {
    let mut criterion = c.confidence_level(0.99);  // 99% confidence intervals
    
    criterion.bench_function("high_confidence", |b| {
        b.iter(|| precise_computation())
    });
}
```

##### `significance_level`

Set the significance level for change detection.

```rust
use criterion::Criterion;

fn significant_benchmark(c: &mut Criterion) {
    let mut criterion = c.significance_level(0.01);  // 1% significance level
    
    criterion.bench_function("strict_significance", |b| {
        b.iter(|| critical_computation())
    });
}
```

##### `noise_threshold`

Set the noise threshold for outlier detection.

```rust
use criterion::Criterion;

fn noise_tolerant_benchmark(c: &mut Criterion) {
    let mut criterion = c.noise_threshold(0.05);  // 5% noise threshold
    
    criterion.bench_function("noisy_environment", |b| {
        b.iter(|| variable_computation())
    });
}
```

### `BenchmarkGroup<M>`

A group of related benchmarks with shared configuration.

```rust
use criterion::{Criterion, BenchmarkId, Throughput};

fn group_example(c: &mut Criterion) {
    let mut group = c.benchmark_group("data_processing");
    
    // Configure the entire group
    group.sample_size(50);
    group.measurement_time(Duration::from_secs(3));
    
    for size in [1000, 10000, 100000].iter() {
        group.throughput(Throughput::Elements(*size as u64));
        
        group.bench_with_input(
            BenchmarkId::new("process_data", size),
            size,
            |b, &size| {
                let data = generate_data(size);
                b.iter(|| process_data(&data))
            }
        );
    }
    
    group.finish();
}
```

#### Methods

##### `bench_function`

Add a simple function benchmark to the group.

```rust
fn group_bench_function(c: &mut Criterion) {
    let mut group = c.benchmark_group("algorithms");
    
    group.bench_function("algorithm_a", |b| {
        b.iter(|| algorithm_a())
    });
    
    group.bench_function("algorithm_b", |b| {
        b.iter(|| algorithm_b())
    });
    
    group.finish();
}
```

##### `bench_with_input`

Add a parameterized benchmark to the group.

```rust
use criterion::{BenchmarkId, Criterion};

fn parameterized_group(c: &mut Criterion) {
    let mut group = c.benchmark_group("matrix_operations");
    
    let sizes = [10, 50, 100, 200];
    
    for &size in &sizes {
        group.bench_with_input(
            BenchmarkId::new("matrix_multiply", size),
            &size,
            |b, &size| {
                let matrix_a = create_matrix(size);
                let matrix_b = create_matrix(size);
                b.iter(|| matrix_multiply(&matrix_a, &matrix_b))
            }
        );
    }
    
    group.finish();
}
```

##### `throughput`

Set throughput measurement for the group.

```rust
use criterion::{Criterion, Throughput};

fn throughput_group(c: &mut Criterion) {
    let mut group = c.benchmark_group("data_throughput");
    
    let data_size = 1024 * 1024;  // 1MB
    group.throughput(Throughput::Bytes(data_size));
    
    group.bench_function("compress_data", |b| {
        let data = vec![0u8; data_size as usize];
        b.iter(|| compress(&data))
    });
    
    group.finish();
}
```

##### `sampling_mode`

Set the sampling mode for the group.

```rust
use criterion::{Criterion, SamplingMode};

fn sampling_group(c: &mut Criterion) {
    let mut group = c.benchmark_group("sampling_modes");
    
    // Auto sampling (default)
    group.sampling_mode(SamplingMode::Auto);
    group.bench_function("auto_sampling", |b| {
        b.iter(|| fast_operation())
    });
    
    // Flat sampling
    group.sampling_mode(SamplingMode::Flat);
    group.bench_function("flat_sampling", |b| {
        b.iter(|| consistent_operation())
    });
    
    group.finish();
}
```

### `Bencher`

The benchmark runner that measures execution time.

#### Methods

##### `iter<F>`

Basic iteration measurement.

```rust
fn basic_iter_example(c: &mut Criterion) {
    c.bench_function("basic_iter", |b| {
        b.iter(|| {
            // Code to benchmark
            expensive_function()
        })
    });
}
```

##### `iter_with_setup<S, R, F>`

Iteration with setup that's not measured.

```rust
fn setup_example(c: &mut Criterion) {
    c.bench_function("with_setup", |b| {
        b.iter_with_setup(
            || {
                // Setup code (not measured)
                create_test_data()
            },
            |data| {
                // Measured code
                process_data(data)
            }
        )
    });
}
```

##### `iter_with_large_drop<F>`

Iteration where the return value is expensive to drop.

```rust
fn large_drop_example(c: &mut Criterion) {
    c.bench_function("large_drop", |b| {
        b.iter_with_large_drop(|| {
            // Return value drop time is not measured
            create_large_structure()
        })
    });
}
```

##### `iter_batched<S, R, F>`

Batch iteration with different batch sizes.

```rust
use criterion::BatchSize;

fn batched_example(c: &mut Criterion) {
    c.bench_function("batched", |b| {
        b.iter_batched(
            || create_input(),          // Setup for each batch
            |input| process(input),     // Process the batch
            BatchSize::SmallInput       // Batch size hint
        )
    });
}
```

##### `iter_batched_ref<S, R, F>`

Batch iteration with references.

```rust
use criterion::BatchSize;

fn batched_ref_example(c: &mut Criterion) {
    c.bench_function("batched_ref", |b| {
        b.iter_batched_ref(
            || create_large_input(),    // Setup (expensive)
            |input| process_ref(input), // Process by reference
            BatchSize::LargeInput       // Large batch size
        )
    });
}
```

##### `to_async<R>`

Convert to async benchmarking.

```rust
use criterion::Criterion;
use tokio::runtime::Runtime;

fn async_example(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    
    c.bench_function("async_operation", |b| {
        b.to_async(&rt).iter(|| async {
            async_computation().await
        })
    });
}

async fn async_computation() -> u64 {
    tokio::time::sleep(tokio::time::Duration::from_nanos(100)).await;
    42
}
```

## Measurement Types

### `WallTime`

Default wall-clock time measurement.

```rust
use criterion::{Criterion, WallTime};

fn wall_time_example() {
    let mut criterion = Criterion::<WallTime>::default();
    
    criterion.bench_function("wall_time", |b| {
        b.iter(|| std::thread::sleep(std::time::Duration::from_nanos(100)))
    });
}
```

### Custom Measurements

Implement the `Measurement` trait for custom metrics.

```rust
use criterion::measurement::{Measurement, ValueFormatter};
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug)]
struct CpuCycles;

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
struct CycleCount(u64);

impl Measurement for CpuCycles {
    type Intermediate = u64;
    type Value = CycleCount;

    fn start(&self) -> Self::Intermediate {
        unsafe { std::arch::x86_64::_rdtsc() }
    }

    fn end(&self, start: Self::Intermediate) -> Self::Value {
        let end = unsafe { std::arch::x86_64::_rdtsc() };
        CycleCount(end - start)
    }

    fn add(&self, v1: &Self::Value, v2: &Self::Value) -> Self::Value {
        CycleCount(v1.0 + v2.0)
    }

    fn zero(&self) -> Self::Value {
        CycleCount(0)
    }

    fn to_f64(&self, value: &Self::Value) -> f64 {
        value.0 as f64
    }

    fn formatter(&self) -> &dyn ValueFormatter {
        &CycleFormatter
    }
}

struct CycleFormatter;

impl ValueFormatter for CycleFormatter {
    fn format_value(&self, value: f64) -> String {
        format!("{:.0} cycles", value)
    }

    fn format_throughput(&self, throughput: &criterion::Throughput, value: f64) -> String {
        match throughput {
            criterion::Throughput::Bytes(bytes) => {
                format!("{:.0} cycles/byte", value / *bytes as f64)
            }
            criterion::Throughput::Elements(elements) => {
                format!("{:.0} cycles/element", value / *elements as f64)
            }
        }
    }

    fn scale_values(&self, _typical_value: f64, _values: &mut [f64]) -> &'static str {
        "cycles"
    }

    fn scale_throughputs(
        &self,
        _typical_value: f64,
        throughput: &criterion::Throughput,
        _values: &mut [f64],
    ) -> &'static str {
        match throughput {
            criterion::Throughput::Bytes(_) => "cycles/byte",
            criterion::Throughput::Elements(_) => "cycles/element",
        }
    }

    fn scale_for_machines(&self, _values: &mut [f64]) -> &'static str {
        "cycles"
    }
}

fn custom_measurement_example() {
    let mut criterion = Criterion::default().with_measurement(CpuCycles);
    
    criterion.bench_function("cpu_cycles", |b| {
        b.iter(|| {
            let mut sum = 0u64;
            for i in 0..1000 {
                sum = sum.wrapping_add(i);
            }
            std::hint::black_box(sum)
        })
    });
}
```

## Profiler Integration

### `Profiler` Trait

Implement custom profilers.

```rust
use criterion::profiler::Profiler;
use std::path::Path;

struct CustomProfiler;

impl Profiler for CustomProfiler {
    fn start_profiling(&mut self, benchmark_id: &str, benchmark_dir: &Path) {
        println!("Starting profiling for {}", benchmark_id);
        // Start your profiler here
    }

    fn stop_profiling(&mut self, benchmark_id: &str, benchmark_dir: &Path) {
        println!("Stopping profiling for {}", benchmark_id);
        // Stop your profiler and save results
    }
}

fn profiler_example(c: &mut Criterion) {
    let mut criterion = c.with_profiler(CustomProfiler);
    
    criterion.bench_function("profiled", |b| {
        b.iter(|| expensive_computation())
    });
}
```

## Utility Types

### `BenchmarkId`

Identifier for parameterized benchmarks.

```rust
use criterion::{BenchmarkId, Criterion};

fn benchmark_id_example(c: &mut Criterion) {
    let mut group = c.benchmark_group("algorithms");
    
    let algorithms = ["quicksort", "mergesort", "heapsort"];
    let sizes = [100, 1000, 10000];
    
    for algorithm in &algorithms {
        for &size in &sizes {
            group.bench_with_input(
                BenchmarkId::new(*algorithm, size),
                &(algorithm, size),
                |b, &(algorithm, size)| {
                    b.iter_with_setup(
                        || generate_data(size),
                        |mut data| match algorithm {
                            "quicksort" => data.sort(),
                            "mergesort" => merge_sort(&mut data),
                            "heapsort" => heap_sort(&mut data),
                            _ => unreachable!(),
                        }
                    )
                }
            );
        }
    }
    
    group.finish();
}
```

### `Throughput`

Specify throughput measurements.

```rust
use criterion::{Criterion, Throughput};

fn throughput_example(c: &mut Criterion) {
    let mut group = c.benchmark_group("data_processing");
    
    let data_size = 1024 * 1024;  // 1MB
    
    // Bytes throughput
    group.throughput(Throughput::Bytes(data_size));
    group.bench_function("process_bytes", |b| {
        let data = vec![0u8; data_size as usize];
        b.iter(|| process_bytes(&data))
    });
    
    // Elements throughput
    let element_count = 10000;
    group.throughput(Throughput::Elements(element_count));
    group.bench_function("process_elements", |b| {
        let elements = vec![0i32; element_count as usize];
        b.iter(|| process_elements(&elements))
    });
    
    group.finish();
}
```

### `SamplingMode`

Control sampling behavior.

```rust
use criterion::{Criterion, SamplingMode};

fn sampling_mode_example(c: &mut Criterion) {
    let mut group = c.benchmark_group("sampling");
    
    // Auto sampling (adapts to function execution time)
    group.sampling_mode(SamplingMode::Auto);
    group.bench_function("auto", |b| {
        b.iter(|| variable_time_function())
    });
    
    // Flat sampling (fixed iterations per sample)
    group.sampling_mode(SamplingMode::Flat);
    group.bench_function("flat", |b| {
        b.iter(|| consistent_time_function())
    });
    
    group.finish();
}
```

### `BatchSize`

Hint for batch iteration sizing.

```rust
use criterion::{BatchSize, Criterion};

fn batch_size_example(c: &mut Criterion) {
    c.bench_function("small_input", |b| {
        b.iter_batched(
            || create_small_input(),
            |input| process(input),
            BatchSize::SmallInput
        )
    });
    
    c.bench_function("large_input", |b| {
        b.iter_batched(
            || create_large_input(),
            |input| process(input),
            BatchSize::LargeInput
        )
    });
    
    c.bench_function("per_iteration", |b| {
        b.iter_batched(
            || create_unique_input(),
            |input| process(input),
            BatchSize::PerIteration
        )
    });
}
```

## Configuration Types

### `PlotConfiguration`

Configure plot generation.

```rust
use criterion::{Criterion, PlotConfiguration, AxisScale};

fn plot_config_example(c: &mut Criterion) {
    let plot_config = PlotConfiguration::default()
        .summary_scale(AxisScale::Logarithmic);
    
    let mut criterion = c.plot_config(plot_config);
    
    criterion.bench_function("plotted", |b| {
        b.iter(|| computation())
    });
}
```

## Macros

### `criterion_group!`

Define a group of benchmark functions.

```rust
use criterion::{criterion_group, criterion_main, Criterion};

fn bench_fibonacci(c: &mut Criterion) {
    c.bench_function("fib 10", |b| b.iter(|| fibonacci(10)));
    c.bench_function("fib 20", |b| b.iter(|| fibonacci(20)));
}

fn bench_factorial(c: &mut Criterion) {
    c.bench_function("fact 10", |b| b.iter(|| factorial(10)));
    c.bench_function("fact 20", |b| b.iter(|| factorial(20)));
}

criterion_group!(benches, bench_fibonacci, bench_factorial);
criterion_main!(benches);
```

### `criterion_main!`

Generate the main function for benchmarks.

```rust
use criterion::{criterion_group, criterion_main, Criterion};

fn bench_group_1(c: &mut Criterion) {
    // Benchmarks...
}

fn bench_group_2(c: &mut Criterion) {
    // More benchmarks...
}

criterion_group!(group1, bench_group_1);
criterion_group!(group2, bench_group_2);
criterion_main!(group1, group2);
```

## Utility Functions

### `black_box`

Prevent compiler optimizations.

```rust
use std::hint::black_box;

fn black_box_example(c: &mut Criterion) {
    c.bench_function("with_black_box", |b| {
        b.iter(|| {
            let input = black_box(42);
            let result = expensive_computation(input);
            black_box(result)
        })
    });
}
```

Note: In Criterion 0.6, prefer `std::hint::black_box` over `criterion::black_box` (which is deprecated).

## Complete Example

Here's a comprehensive example demonstrating many API features:

```rust
use criterion::{
    criterion_group, criterion_main, BenchmarkId, Criterion, 
    Throughput, SamplingMode, BatchSize
};
use std::hint::black_box;
use std::time::Duration;

// Basic benchmark
fn fibonacci_benchmark(c: &mut Criterion) {
    c.bench_function("fibonacci_20", |b| {
        b.iter(|| black_box(fibonacci(black_box(20))))
    });
}

// Parameterized benchmark with throughput
fn sorting_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("sorting_algorithms");
    
    // Configure the group
    group
        .sample_size(50)
        .measurement_time(Duration::from_secs(3))
        .sampling_mode(SamplingMode::Auto);
    
    for size in [100, 1000, 10000].iter() {
        group.throughput(Throughput::Elements(*size as u64));
        
        // Bubble sort
        group.bench_with_input(
            BenchmarkId::new("bubble_sort", size),
            size,
            |b, &size| {
                b.iter_batched(
                    || (0..size).rev().collect::<Vec<_>>(),
                    |mut data| {
                        bubble_sort(&mut data);
                        data
                    },
                    BatchSize::SmallInput
                )
            }
        );
        
        // Quick sort (std::sort)
        group.bench_with_input(
            BenchmarkId::new("quick_sort", size),
            size,
            |b, &size| {
                b.iter_batched(
                    || (0..size).rev().collect::<Vec<_>>(),
                    |mut data| {
                        data.sort();
                        data
                    },
                    BatchSize::SmallInput
                )
            }
        );
    }
    
    group.finish();
}

// Async benchmark
fn async_benchmark(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    
    c.bench_function("async_computation", |b| {
        b.to_async(&rt).iter(|| async {
            tokio::time::sleep(Duration::from_nanos(100)).await;
            black_box(42)
        })
    });
}

// Custom configuration
fn configured_benchmark(c: &mut Criterion) {
    let mut criterion = c
        .sample_size(200)
        .measurement_time(Duration::from_secs(5))
        .warm_up_time(Duration::from_secs(1))
        .confidence_level(0.99)
        .significance_level(0.01)
        .noise_threshold(0.02);
    
    criterion.bench_function("high_precision", |b| {
        b.iter(|| black_box(expensive_computation()))
    });
}

// Helper functions
fn fibonacci(n: u64) -> u64 {
    match n {
        0 | 1 => 1,
        n => fibonacci(n - 1) + fibonacci(n - 2),
    }
}

fn bubble_sort(arr: &mut [i32]) {
    let len = arr.len();
    for i in 0..len {
        for j in 0..len - 1 - i {
            if arr[j] > arr[j + 1] {
                arr.swap(j, j + 1);
            }
        }
    }
}

fn expensive_computation() -> u64 {
    (0..10000).map(|x| x * x).sum()
}

criterion_group!(
    benches,
    fibonacci_benchmark,
    sorting_benchmark,
    async_benchmark,
    configured_benchmark
);
criterion_main!(benches);
```

## Error Handling

### Common Errors and Solutions

```rust
use criterion::{Criterion, BenchmarkId};

fn error_handling_examples(c: &mut Criterion) {
    // Error: Benchmark function panics
    c.bench_function("might_panic", |b| {
        b.iter(|| {
            match risky_operation() {
                Ok(result) => result,
                Err(_) => {
                    // Don't panic in benchmarks - it invalidates measurements
                    eprintln!("Warning: Operation failed");
                    0  // Return default value
                }
            }
        })
    });
    
    // Error: Forgetting to call group.finish()
    let mut group = c.benchmark_group("example");
    group.bench_function("test", |b| b.iter(|| computation()));
    group.finish();  // Don't forget this!
    
    // Error: Using the same BenchmarkId twice
    let mut group = c.benchmark_group("unique_ids");
    group.bench_function("test_1", |b| b.iter(|| computation()));
    // group.bench_function("test_1", |b| b.iter(|| other_computation())); // Error!
    group.bench_function("test_2", |b| b.iter(|| other_computation())); // OK
    group.finish();
}

fn risky_operation() -> Result<u64, &'static str> {
    Ok(42)
}

fn computation() -> u64 { 42 }
fn other_computation() -> u64 { 24 }
```

## Best Practices

### 1. Use `black_box` to prevent optimizations

```rust
use std::hint::black_box;

// Good
c.bench_function("optimized", |b| {
    b.iter(|| {
        let input = black_box(42);
        let result = computation(input);
        black_box(result)
    })
});

// Bad - compiler might optimize away the computation
c.bench_function("not_optimized", |b| {
    b.iter(|| {
        computation(42)  // Result is discarded, might be optimized away
    })
});
```

### 2. Use appropriate iteration methods

```rust
// For simple functions
c.bench_function("simple", |b| {
    b.iter(|| simple_function())
});

// When setup is expensive
c.bench_function("with_setup", |b| {
    b.iter_with_setup(
        || expensive_setup(),
        |data| process(data)
    )
});

// When return value is expensive to drop
c.bench_function("large_return", |b| {
    b.iter_with_large_drop(|| create_large_object())
});
```

### 3. Configure appropriately for your use case

```rust
// For quick development feedback
let mut criterion = c
    .sample_size(10)
    .measurement_time(Duration::from_secs(1));

// For production performance monitoring
let mut criterion = c
    .sample_size(200)
    .measurement_time(Duration::from_secs(10))
    .confidence_level(0.99);
```

This API reference covers the essential types and methods in Criterion.rs 0.6. For the most up-to-date information, always refer to the [official documentation](https://docs.rs/criterion/).