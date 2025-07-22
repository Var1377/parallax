# Benchmark Organization in Criterion.rs 0.6

This guide covers best practices for organizing, structuring, and managing complex benchmark suites with Criterion.rs.

## Project Structure

### Recommended Directory Layout

```
my_project/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   ├── algorithms/
│   │   ├── mod.rs
│   │   ├── sorting.rs
│   │   └── searching.rs
│   └── data_structures/
│       ├── mod.rs
│       ├── vector.rs
│       └── hashmap.rs
└── benches/
    ├── algorithms/
    │   ├── mod.rs
    │   ├── sorting_benchmarks.rs
    │   └── searching_benchmarks.rs
    ├── data_structures/
    │   ├── mod.rs
    │   ├── vector_benchmarks.rs
    │   └── hashmap_benchmarks.rs
    ├── integration/
    │   ├── mod.rs
    │   └── end_to_end_benchmarks.rs
    └── regression/
        ├── mod.rs
        └── performance_regression.rs
```

### Cargo.toml Configuration

```toml
[package]
name = "my_project"
version = "0.1.0"
edition = "2021"

[dependencies]
# Your regular dependencies

[dev-dependencies]
criterion = { version = "0.6", features = ["html_reports"] }
rand = "0.8"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"

# Separate benchmark binaries for different categories
[[bench]]
name = "algorithms"
harness = false

[[bench]]
name = "data_structures"
harness = false

[[bench]]
name = "integration"
harness = false

[[bench]]
name = "regression"
harness = false
```

## Benchmark Groups and Organization

### Basic Group Organization

```rust
// benches/algorithms.rs
use criterion::{criterion_group, criterion_main, Criterion};

mod sorting;
mod searching;

fn sorting_benchmarks(c: &mut Criterion) {
    sorting::bubble_sort_benchmarks(c);
    sorting::quick_sort_benchmarks(c);
    sorting::merge_sort_benchmarks(c);
}

fn searching_benchmarks(c: &mut Criterion) {
    searching::linear_search_benchmarks(c);
    searching::binary_search_benchmarks(c);
    searching::hash_lookup_benchmarks(c);
}

criterion_group!(
    algorithm_benches,
    sorting_benchmarks,
    searching_benchmarks
);
criterion_main!(algorithm_benches);
```

### Hierarchical Group Structure

```rust
// benches/algorithms/sorting_benchmarks.rs
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use my_project::algorithms::sorting::*;

pub fn bubble_sort_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("sorting/bubble_sort");
    
    for size in [10, 100, 1000].iter() {
        group.throughput(Throughput::Elements(*size as u64));
        group.bench_with_input(
            BenchmarkId::new("random_data", size),
            size,
            |b, &size| {
                b.iter_with_setup(
                    || generate_random_data(size),
                    |mut data| bubble_sort(&mut data)
                )
            }
        );
        
        group.bench_with_input(
            BenchmarkId::new("sorted_data", size),
            size,
            |b, &size| {
                b.iter_with_setup(
                    || generate_sorted_data(size),
                    |mut data| bubble_sort(&mut data)
                )
            }
        );
        
        group.bench_with_input(
            BenchmarkId::new("reverse_sorted", size),
            size,
            |b, &size| {
                b.iter_with_setup(
                    || generate_reverse_sorted_data(size),
                    |mut data| bubble_sort(&mut data)
                )
            }
        );
    }
    
    group.finish();
}

fn generate_random_data(size: usize) -> Vec<i32> {
    use rand::Rng;
    let mut rng = rand::thread_rng();
    (0..size).map(|_| rng.gen_range(0..1000)).collect()
}

fn generate_sorted_data(size: usize) -> Vec<i32> {
    (0..size as i32).collect()
}

fn generate_reverse_sorted_data(size: usize) -> Vec<i32> {
    (0..size as i32).rev().collect()
}
```

## Parameterized Benchmarks

### Multi-Dimensional Parameters

```rust
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};

fn matrix_multiplication_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("matrix_multiplication");
    
    let sizes = [16, 32, 64, 128, 256];
    let algorithms = ["naive", "blocked", "strassen"];
    
    for &size in &sizes {
        for &algorithm in &algorithms {
            group.bench_with_input(
                BenchmarkId::from_parameter(format!("{}x{}/{}", size, size, algorithm)),
                &(size, algorithm),
                |b, &(size, algorithm)| {
                    b.iter_with_setup(
                        || (generate_matrix(size), generate_matrix(size)),
                        |(a, b)| match algorithm {
                            "naive" => naive_multiply(&a, &b),
                            "blocked" => blocked_multiply(&a, &b),
                            "strassen" => strassen_multiply(&a, &b),
                            _ => unreachable!(),
                        }
                    )
                }
            );
        }
    }
    
    group.finish();
}

fn generate_matrix(size: usize) -> Vec<Vec<f64>> {
    use rand::Rng;
    let mut rng = rand::thread_rng();
    (0..size)
        .map(|_| (0..size).map(|_| rng.gen()).collect())
        .collect()
}

criterion_group!(benches, matrix_multiplication_benchmarks);
criterion_main!(benches);
```

### Complex Parameter Structures

```rust
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
struct BenchmarkConfig {
    data_size: usize,
    algorithm: String,
    optimization_level: u8,
    use_simd: bool,
}

impl std::fmt::Display for BenchmarkConfig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "size_{}_algo_{}_opt_{}_simd_{}",
            self.data_size,
            self.algorithm,
            self.optimization_level,
            self.use_simd
        )
    }
}

fn complex_parameter_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("complex_parameters");
    
    let configs = vec![
        BenchmarkConfig {
            data_size: 1000,
            algorithm: "quicksort".to_string(),
            optimization_level: 2,
            use_simd: false,
        },
        BenchmarkConfig {
            data_size: 1000,
            algorithm: "quicksort".to_string(),
            optimization_level: 3,
            use_simd: true,
        },
        BenchmarkConfig {
            data_size: 10000,
            algorithm: "mergesort".to_string(),
            optimization_level: 2,
            use_simd: false,
        },
        // Add more configurations...
    ];
    
    for config in configs {
        group.bench_with_input(
            BenchmarkId::new("complex_sort", config.to_string()),
            &config,
            |b, config| {
                b.iter_with_setup(
                    || generate_test_data(config.data_size),
                    |mut data| sort_with_config(&mut data, config)
                )
            }
        );
    }
    
    group.finish();
}

fn generate_test_data(size: usize) -> Vec<i32> {
    use rand::Rng;
    let mut rng = rand::thread_rng();
    (0..size).map(|_| rng.gen()).collect()
}

fn sort_with_config(data: &mut [i32], config: &BenchmarkConfig) {
    match config.algorithm.as_str() {
        "quicksort" => {
            if config.use_simd {
                simd_quicksort(data, config.optimization_level);
            } else {
                standard_quicksort(data);
            }
        }
        "mergesort" => {
            if config.use_simd {
                simd_mergesort(data, config.optimization_level);
            } else {
                standard_mergesort(data);
            }
        }
        _ => data.sort(),
    }
}

criterion_group!(benches, complex_parameter_benchmarks);
criterion_main!(benches);
```

## Shared Test Data and Utilities

### Common Benchmark Utilities

```rust
// benches/common/mod.rs
use rand::{Rng, SeedableRng};
use rand_chacha::ChaCha8Rng;

pub struct BenchmarkData {
    pub small_dataset: Vec<i32>,
    pub medium_dataset: Vec<i32>,
    pub large_dataset: Vec<i32>,
    pub sorted_dataset: Vec<i32>,
    pub reverse_sorted_dataset: Vec<i32>,
}

impl BenchmarkData {
    pub fn new() -> Self {
        let mut rng = ChaCha8Rng::seed_from_u64(42); // Fixed seed for reproducibility
        
        Self {
            small_dataset: (0..100).map(|_| rng.gen()).collect(),
            medium_dataset: (0..10_000).map(|_| rng.gen()).collect(),
            large_dataset: (0..1_000_000).map(|_| rng.gen()).collect(),
            sorted_dataset: (0..10_000).collect(),
            reverse_sorted_dataset: (0..10_000).rev().collect(),
        }
    }
}

pub fn setup_benchmark_environment() {
    // Set thread priority, disable frequency scaling, etc.
    #[cfg(unix)]
    {
        unsafe {
            libc::nice(-20); // Increase process priority
        }
    }
}

pub fn create_test_matrix(rows: usize, cols: usize) -> Vec<Vec<f64>> {
    let mut rng = ChaCha8Rng::seed_from_u64(42);
    (0..rows)
        .map(|_| (0..cols).map(|_| rng.gen()).collect())
        .collect()
}

pub fn create_test_graph(nodes: usize, edge_probability: f64) -> Vec<Vec<bool>> {
    let mut rng = ChaCha8Rng::seed_from_u64(42);
    let mut graph = vec![vec![false; nodes]; nodes];
    
    for i in 0..nodes {
        for j in i + 1..nodes {
            if rng.gen::<f64>() < edge_probability {
                graph[i][j] = true;
                graph[j][i] = true;
            }
        }
    }
    
    graph
}
```

### Using Shared Utilities

```rust
// benches/algorithms/graph_benchmarks.rs
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};

mod common;
use common::*;

fn graph_algorithm_benchmarks(c: &mut Criterion) {
    setup_benchmark_environment();
    
    let mut group = c.benchmark_group("graph_algorithms");
    
    let node_counts = [10, 50, 100, 500];
    let edge_probabilities = [0.1, 0.3, 0.5, 0.8];
    
    for &nodes in &node_counts {
        for &prob in &edge_probabilities {
            let graph = create_test_graph(nodes, prob);
            
            group.bench_with_input(
                BenchmarkId::from_parameter(format!("dfs_{}nodes_{}prob", nodes, prob)),
                &graph,
                |b, graph| {
                    b.iter(|| depth_first_search(graph, 0))
                }
            );
            
            group.bench_with_input(
                BenchmarkId::from_parameter(format!("bfs_{}nodes_{}prob", nodes, prob)),
                &graph,
                |b, graph| {
                    b.iter(|| breadth_first_search(graph, 0))
                }
            );
        }
    }
    
    group.finish();
}

criterion_group!(benches, graph_algorithm_benchmarks);
criterion_main!(benches);
```

## Configuration Management

### Environment-Specific Configuration

```rust
use criterion::{criterion_group, criterion_main, Criterion, SamplingMode};
use std::time::Duration;

fn get_benchmark_config() -> Criterion {
    let mut criterion = Criterion::default();
    
    // Check if running in CI environment
    if std::env::var("CI").is_ok() {
        criterion = criterion
            .sample_size(50)                                    // Fewer samples in CI
            .measurement_time(Duration::from_secs(2))           // Shorter measurement time
            .warm_up_time(Duration::from_millis(500))           // Shorter warm-up
            .noise_threshold(0.05);                             // More lenient noise threshold
    } else {
        criterion = criterion
            .sample_size(100)                                   // More samples locally
            .measurement_time(Duration::from_secs(5))           // Longer measurement time
            .warm_up_time(Duration::from_secs(1))               // Standard warm-up
            .noise_threshold(0.02);                             // Stricter noise threshold
    }
    
    // Check for performance mode
    if std::env::var("BENCH_MODE").as_deref() == Ok("performance") {
        criterion = criterion
            .sample_size(200)
            .measurement_time(Duration::from_secs(10))
            .confidence_level(0.99)
            .significance_level(0.01);
    }
    
    criterion
}

fn configured_benchmarks(c: &mut Criterion) {
    let criterion = get_benchmark_config();
    
    criterion.bench_function("example_benchmark", |b| {
        b.iter(|| expensive_computation())
    });
}

criterion_group!(benches, configured_benchmarks);
criterion_main!(benches);
```

### Feature-Based Configuration

```rust
use criterion::{criterion_group, criterion_main, Criterion};

fn feature_specific_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("feature_tests");
    
    #[cfg(feature = "simd")]
    {
        group.bench_function("simd_implementation", |b| {
            b.iter(|| simd_algorithm())
        });
    }
    
    #[cfg(not(feature = "simd"))]
    {
        group.bench_function("scalar_implementation", |b| {
            b.iter(|| scalar_algorithm())
        });
    }
    
    #[cfg(feature = "parallel")]
    {
        group.bench_function("parallel_implementation", |b| {
            b.iter(|| parallel_algorithm())
        });
    }
    
    group.finish();
}

criterion_group!(benches, feature_specific_benchmarks);
criterion_main!(benches);
```

## Benchmark Suites and Categories

### Categorized Benchmark Execution

```rust
// benches/main.rs
use criterion::{criterion_group, criterion_main};

mod algorithms;
mod data_structures;
mod integration;
mod regression;

// Quick benchmarks for development
criterion_group!(
    quick,
    algorithms::quick_sort_basic,
    data_structures::vector_basic
);

// Comprehensive benchmarks for CI
criterion_group!(
    comprehensive,
    algorithms::all_sorting_algorithms,
    algorithms::all_searching_algorithms,
    data_structures::all_data_structures,
    integration::end_to_end_tests
);

// Performance regression tests
criterion_group!(
    regression,
    regression::performance_regression_tests
);

// Memory-intensive benchmarks
criterion_group!(
    memory,
    algorithms::large_dataset_sorting,
    data_structures::memory_usage_tests
);

// Choose which groups to run based on environment
#[cfg(feature = "quick")]
criterion_main!(quick);

#[cfg(feature = "comprehensive")]
criterion_main!(comprehensive);

#[cfg(feature = "regression")]
criterion_main!(regression);

#[cfg(feature = "memory")]
criterion_main!(memory);

#[cfg(not(any(feature = "quick", feature = "comprehensive", feature = "regression", feature = "memory")))]
criterion_main!(quick); // Default to quick benchmarks
```

### Running Specific Benchmark Categories

```bash
# Run quick benchmarks for development
cargo bench --features quick

# Run comprehensive benchmarks for CI
cargo bench --features comprehensive

# Run regression tests
cargo bench --features regression

# Run memory benchmarks
cargo bench --features memory

# Run specific benchmark file
cargo bench --bench algorithms

# Run specific benchmark function
cargo bench "sorting/quick_sort"

# Run benchmarks matching a pattern
cargo bench "bubble_sort"
```

## Best Practices

### 1. Consistent Naming Conventions

```rust
// Use hierarchical naming for related benchmarks
"category/subcategory/specific_test"
"sorting/comparison_based/quicksort"
"sorting/non_comparison/radix_sort"
"data_structures/linear/vector/push"
"data_structures/linear/vector/pop"
```

### 2. Reproducible Test Data

```rust
use rand::{Rng, SeedableRng};
use rand_chacha::ChaCha8Rng;

// Always use seeded RNG for reproducible results
fn generate_test_data(size: usize) -> Vec<i32> {
    let mut rng = ChaCha8Rng::seed_from_u64(42);
    (0..size).map(|_| rng.gen()).collect()
}
```

### 3. Proper Resource Management

```rust
fn resource_intensive_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("resource_intensive");
    
    // Configure for resource-intensive benchmarks
    group
        .sample_size(20)                                        // Fewer samples
        .measurement_time(Duration::from_secs(1))               // Shorter measurement
        .warm_up_time(Duration::from_millis(100));              // Minimal warm-up
    
    group.bench_function("large_allocation", |b| {
        b.iter_with_large_drop(|| {
            // Large allocation that's expensive to drop
            vec![0u8; 100_000_000]
        })
    });
    
    group.finish();
}
```

### 4. Documentation and Comments

```rust
/// Benchmarks for sorting algorithms with different input characteristics.
/// 
/// Tests include:
/// - Random data: Typical use case
/// - Sorted data: Best case for some algorithms
/// - Reverse sorted: Worst case for some algorithms
/// - Nearly sorted: Common real-world scenario
/// 
/// Sizes tested: 10, 100, 1000, 10000 elements
/// Algorithms: bubble_sort, quick_sort, merge_sort, heap_sort
fn comprehensive_sorting_benchmarks(c: &mut Criterion) {
    // Implementation...
}
```

## Next Steps

- [Performance Regression Detection](./regression-detection.md) - Tracking performance over time
- [Custom Measurements](./custom-measurements.md) - Implementing custom metrics
- [HTML Reports](./html-reports.md) - Visualizing benchmark results