# Getting Started with Criterion.rs 0.6

This guide will walk you through setting up and writing your first benchmarks with Criterion.rs.

## Installation and Setup

### Adding Criterion to Your Project

Add Criterion as a development dependency in your `Cargo.toml`:

```toml
[dev-dependencies]
criterion = { version = "0.6", features = ["html_reports"] }

[[bench]]
name = "my_benchmark"
harness = false
```

### Project Structure

Organize your benchmarks in the `benches/` directory:

```
my_project/
├── Cargo.toml
├── src/
│   └── lib.rs
└── benches/
    ├── my_benchmark.rs
    ├── algorithm_comparison.rs
    └── performance_regression.rs
```

## Your First Benchmark

### Basic Function Benchmark

Create `benches/basic_benchmark.rs`:

```rust
use criterion::{criterion_group, criterion_main, Criterion};

fn fibonacci(n: u64) -> u64 {
    match n {
        0 => 1,
        1 => 1,
        n => fibonacci(n-1) + fibonacci(n-2),
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("fib 20", |b| b.iter(|| fibonacci(20)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
```

### Running Benchmarks

```bash
# Run all benchmarks
cargo bench

# Run specific benchmark
cargo bench --bench basic_benchmark

# Run with verbose output
cargo bench -- --verbose

# Generate HTML reports
cargo bench --features html_reports
```

## Essential Patterns

### Using `black_box` to Prevent Optimization

```rust
use criterion::{criterion_group, criterion_main, Criterion};
use std::hint::black_box;

fn expensive_computation(input: &[i32]) -> i32 {
    input.iter().sum()
}

fn benchmark_computation(c: &mut Criterion) {
    let data = vec![1; 1000];
    
    c.bench_function("sum_computation", |b| {
        b.iter(|| {
            // Prevent compiler from optimizing away the computation
            black_box(expensive_computation(black_box(&data)))
        })
    });
}

criterion_group!(benches, benchmark_computation);
criterion_main!(benches);
```

### Parameterized Benchmarks

```rust
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};

fn sort_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("sorting");
    
    for size in [10, 100, 1000, 10000].iter() {
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
        
        group.bench_with_input(
            BenchmarkId::new("quick_sort", size),
            size,
            |b, &size| {
                b.iter_with_setup(
                    || (0..size).rev().collect::<Vec<_>>(),
                    |mut data| {
                        data.sort();
                        data
                    }
                )
            }
        );
    }
    
    group.finish();
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

criterion_group!(benches, sort_benchmark);
criterion_main!(benches);
```

### Throughput Measurements

```rust
use criterion::{criterion_group, criterion_main, Criterion, Throughput};

fn throughput_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("throughput");
    
    for size in [1024, 4096, 16384, 65536].iter() {
        group.throughput(Throughput::Bytes(*size as u64));
        group.bench_with_input(
            BenchmarkId::new("process_bytes", size),
            size,
            |b, &size| {
                let data = vec![0u8; size];
                b.iter(|| process_data(&data))
            }
        );
    }
    
    group.finish();
}

fn process_data(data: &[u8]) -> u64 {
    data.iter().map(|&x| x as u64).sum()
}

criterion_group!(benches, throughput_benchmark);
criterion_main!(benches);
```

## Async Benchmarks

### Basic Async Benchmark

```rust
use criterion::{criterion_group, criterion_main, Criterion};
use tokio::runtime::Runtime;

async fn async_computation(n: u64) -> u64 {
    // Simulate async work
    tokio::time::sleep(tokio::time::Duration::from_nanos(n * 100)).await;
    n * 2
}

fn async_benchmark(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    
    c.bench_function("async_computation", |b| {
        b.to_async(&rt).iter(|| async_computation(100))
    });
}

criterion_group!(benches, async_benchmark);
criterion_main!(benches);
```

### Async with Custom Runtime

```rust
use criterion::{criterion_group, criterion_main, Criterion};

fn async_with_handle_benchmark(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let handle = rt.handle().clone();
    
    c.bench_function("async_with_handle", |b| {
        b.to_async(&handle).iter(|| async {
            tokio::task::yield_now().await;
            42
        })
    });
}

criterion_group!(benches, async_with_handle_benchmark);
criterion_main!(benches);
```

## Configuration Options

### Global Configuration

```rust
use criterion::{criterion_group, criterion_main, Criterion};
use std::time::Duration;

fn configured_benchmark(c: &mut Criterion) {
    let mut criterion = c
        .warm_up_time(Duration::from_millis(500))
        .measurement_time(Duration::from_secs(2))
        .sample_size(100)
        .confidence_level(0.95)
        .significance_level(0.05)
        .noise_threshold(0.02);
    
    criterion.bench_function("configured_test", |b| {
        b.iter(|| {
            // Your benchmark code here
            std::hint::black_box(42)
        })
    });
}

criterion_group!(benches, configured_benchmark);
criterion_main!(benches);
```

### Group-Level Configuration

```rust
use criterion::{criterion_group, criterion_main, Criterion, SamplingMode};
use std::time::Duration;

fn group_configuration(c: &mut Criterion) {
    let mut group = c.benchmark_group("configured_group");
    
    group
        .sample_size(50)
        .measurement_time(Duration::from_secs(1))
        .sampling_mode(SamplingMode::Flat);
    
    group.bench_function("test_1", |b| b.iter(|| expensive_operation()));
    group.bench_function("test_2", |b| b.iter(|| another_operation()));
    
    group.finish();
}

fn expensive_operation() -> u64 {
    (0..1000).sum()
}

fn another_operation() -> u64 {
    (0..500).product()
}

criterion_group!(benches, group_configuration);
criterion_main!(benches);
```

## Benchmark Iteration Patterns

### `iter()` - Basic Iteration

```rust
// Measures the time to execute the closure once per iteration
c.bench_function("basic_iter", |b| {
    b.iter(|| expensive_function())
});
```

### `iter_with_setup()` - Setup and Teardown

```rust
// Setup is not measured, only the main closure
c.bench_function("with_setup", |b| {
    b.iter_with_setup(
        || create_test_data(),  // Setup (not measured)
        |data| process_data(data)  // Measured operation
    )
});
```

### `iter_with_large_drop()` - Expensive Cleanup

```rust
// Use when the return value is expensive to drop
c.bench_function("large_drop", |b| {
    b.iter_with_large_drop(|| {
        create_large_structure()  // Drop time not measured
    })
});
```

### `iter_batched()` - Batch Processing

```rust
use criterion::BatchSize;

c.bench_function("batched", |b| {
    b.iter_batched(
        || create_input(),  // Setup for each batch
        |input| process(input),  // Process the batch
        BatchSize::SmallInput  // Batch size hint
    )
});
```

## Common Patterns and Best Practices

### 1. Avoiding Measurement Overhead

```rust
// Good: Minimal overhead
c.bench_function("efficient", |b| {
    let data = prepare_data();  // Setup outside
    b.iter(|| {
        std::hint::black_box(process(&data))
    })
});

// Avoid: Setup inside measurement
c.bench_function("inefficient", |b| {
    b.iter(|| {
        let data = prepare_data();  // This gets measured!
        process(&data)
    })
});
```

### 2. Preventing Dead Code Elimination

```rust
use std::hint::black_box;

// Ensure the compiler doesn't optimize away your code
c.bench_function("prevent_optimization", |b| {
    b.iter(|| {
        let result = expensive_computation(black_box(42));
        black_box(result)
    })
});
```

### 3. Memory Allocation Benchmarks

```rust
c.bench_function("allocation_test", |b| {
    b.iter(|| {
        let vec: Vec<i32> = (0..1000).collect();
        black_box(vec)
    })
});
```

### 4. Error Handling in Benchmarks

```rust
c.bench_function("with_error_handling", |b| {
    b.iter(|| {
        match risky_operation() {
            Ok(result) => black_box(result),
            Err(_) => panic!("Benchmark failed"),
        }
    })
});
```

## Output Interpretation

### Terminal Output

```
fib 20                  time:   [26.029 us 26.251 us 26.505 us]
                        change: [-0.8749% +0.8134% +2.4928%] (p = 0.31 > 0.05)
                        No change in performance detected.
Found 2 outliers among 100 measurements (2.00%)
  1 (1.00%) high mild
  1 (1.00%) high severe
```

**Interpretation:**
- **Time range**: 95% confidence interval for the mean execution time
- **Change**: Performance change compared to previous baseline
- **p-value**: Statistical significance (p > 0.05 means no significant change)
- **Outliers**: Measurements that deviate significantly from the norm

### HTML Reports

When using the `html_reports` feature, Criterion generates detailed HTML reports in `target/criterion/`:

- Interactive plots showing performance distributions
- Statistical analysis with confidence intervals
- Comparison charts for multiple benchmarks
- Detailed measurement data

## Next Steps

- [Statistical Analysis](./statistical-analysis.md) - Understanding Criterion's statistical methods
- [Benchmark Organization](./benchmark-organization.md) - Structuring complex benchmark suites
- [Custom Measurements](./custom-measurements.md) - Implementing custom measurement types
- [API Reference](./api-reference.md) - Complete API documentation