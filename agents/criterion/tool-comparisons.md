# Benchmarking Tool Comparisons

This guide compares Criterion.rs with other popular benchmarking tools across different languages and ecosystems, helping you choose the right tool for your needs.

## Rust Benchmarking Tools

### Criterion.rs vs. Built-in `cargo bench`

| Feature | Criterion.rs | `cargo bench` |
|---------|-------------|---------------|
| **Statistical Analysis** | ✅ Bootstrap resampling, confidence intervals | ❌ Basic timing only |
| **Regression Detection** | ✅ Automatic baseline comparison | ❌ Manual comparison |
| **HTML Reports** | ✅ Interactive plots and analysis | ❌ Terminal output only |
| **Outlier Detection** | ✅ Tukey's method | ❌ No outlier handling |
| **Custom Measurements** | ✅ CPU cycles, memory, custom metrics | ❌ Time only |
| **Async Support** | ✅ Native async/await | ❌ No async support |
| **Setup Overhead** | Medium (external dependency) | Low (built-in) |
| **Learning Curve** | Medium | Low |

**Example Comparison:**

```rust
// cargo bench (built-in)
#![feature(test)]
extern crate test;

#[bench]
fn bench_fibonacci(b: &mut test::Bencher) {
    b.iter(|| fibonacci(20));
}

// Criterion.rs
use criterion::{criterion_group, criterion_main, Criterion};

fn criterion_fibonacci(c: &mut Criterion) {
    c.bench_function("fib 20", |b| b.iter(|| fibonacci(20)));
}

criterion_group!(benches, criterion_fibonacci);
criterion_main!(benches);
```

**When to use Criterion.rs:**
- Need statistical rigor and confidence intervals
- Want regression detection and baseline comparison
- Require detailed HTML reports
- Working with async code
- Need custom measurement types

**When to use `cargo bench`:**
- Simple, quick performance checks
- Minimal dependencies preferred
- Basic timing measurements sufficient
- Working on nightly Rust (required for `cargo bench`)

### Criterion.rs vs. Divan

| Feature | Criterion.rs | Divan |
|---------|-------------|-------|
| **Statistical Analysis** | ✅ Comprehensive bootstrap analysis | ✅ Basic statistics |
| **Performance** | Good | ✅ Faster compilation and execution |
| **Memory Usage** | Higher | ✅ Lower memory footprint |
| **HTML Reports** | ✅ Rich interactive reports | ❌ Terminal only |
| **Async Support** | ✅ Full async support | ✅ Async support |
| **Custom Measurements** | ✅ Extensive customization | ✅ Basic custom measurements |
| **Maturity** | ✅ Mature, stable | Newer project |

**Example Comparison:**

```rust
// Divan
use divan::Bencher;

#[divan::bench]
fn fibonacci(bencher: Bencher) {
    bencher.bench(|| fibonacci_impl(20));
}

// Criterion.rs
use criterion::{criterion_group, criterion_main, Criterion};

fn fibonacci_criterion(c: &mut Criterion) {
    c.bench_function("fibonacci", |b| b.iter(|| fibonacci_impl(20)));
}
```

**When to choose Divan:**
- Faster benchmark compilation and execution
- Lower memory usage requirements
- Simpler API preferred
- Terminal-only output sufficient

**When to choose Criterion.rs:**
- Need comprehensive statistical analysis
- Want detailed HTML reports
- Require regression detection
- Need mature, battle-tested tool

### Criterion.rs vs. Iai

| Feature | Criterion.rs | Iai |
|---------|-------------|-----|
| **Measurement Type** | Wall-clock time (default) | ✅ CPU instructions (deterministic) |
| **Reproducibility** | Good (with proper setup) | ✅ Excellent (instruction counting) |
| **Statistical Analysis** | ✅ Comprehensive | ❌ Basic |
| **Platform Support** | ✅ Cross-platform | ❌ Linux only (Valgrind dependency) |
| **Setup Complexity** | Low | High (requires Valgrind) |
| **Real-world Performance** | ✅ Measures actual execution time | ❌ Instructions may not correlate with time |

**Example Comparison:**

```rust
// Iai
use iai::black_box;

fn fibonacci_iai() {
    black_box(fibonacci(black_box(20)));
}

iai::main!(fibonacci_iai);

// Criterion.rs
use criterion::{criterion_group, criterion_main, Criterion, black_box};

fn fibonacci_criterion(c: &mut Criterion) {
    c.bench_function("fibonacci", |b| {
        b.iter(|| black_box(fibonacci(black_box(20))))
    });
}
```

**When to choose Iai:**
- Need deterministic, reproducible results
- CPU instruction counting preferred
- Working exclusively on Linux
- Micro-optimizations and instruction-level analysis

**When to choose Criterion.rs:**
- Need cross-platform support
- Want to measure real-world performance
- Require comprehensive statistical analysis
- Need wall-clock time measurements

## Cross-Language Comparisons

### Criterion.rs vs. Google Benchmark (C++)

| Feature | Criterion.rs | Google Benchmark |
|---------|-------------|------------------|
| **Language** | Rust | C++ |
| **Statistical Analysis** | ✅ Bootstrap resampling | ✅ Basic statistics |
| **Memory Safety** | ✅ Rust's memory safety | ❌ Manual memory management |
| **Ease of Use** | ✅ High-level API | Medium (C++ complexity) |
| **Performance** | High | ✅ Very high (native C++) |
| **Custom Measurements** | ✅ Extensive | ✅ Extensive |
| **Integration** | Cargo ecosystem | CMake/Bazel |

**Example Comparison:**

```cpp
// Google Benchmark (C++)
#include <benchmark/benchmark.h>

static void BM_Fibonacci(benchmark::State& state) {
    for (auto _ : state) {
        benchmark::DoNotOptimize(fibonacci(20));
    }
}
BENCHMARK(BM_Fibonacci);
BENCHMARK_MAIN();
```

```rust
// Criterion.rs
use criterion::{criterion_group, criterion_main, Criterion, black_box};

fn fibonacci_benchmark(c: &mut Criterion) {
    c.bench_function("fibonacci", |b| {
        b.iter(|| black_box(fibonacci(20)))
    });
}

criterion_group!(benches, fibonacci_benchmark);
criterion_main!(benches);
```

### Criterion.rs vs. pytest-benchmark (Python)

| Feature | Criterion.rs | pytest-benchmark |
|---------|-------------|------------------|
| **Language** | Rust | Python |
| **Performance** | ✅ Native speed | Slower (Python overhead) |
| **Statistical Analysis** | ✅ Comprehensive | ✅ Good statistics |
| **Ease of Use** | Medium | ✅ Very easy (pytest integration) |
| **Ecosystem** | Rust/Cargo | ✅ Python/pip (larger ecosystem) |
| **Memory Usage** | Low | Higher (Python runtime) |

**Example Comparison:**

```python
# pytest-benchmark (Python)
def test_fibonacci(benchmark):
    result = benchmark(fibonacci, 20)
    assert result > 0
```

```rust
// Criterion.rs
use criterion::{criterion_group, criterion_main, Criterion};

fn fibonacci_benchmark(c: &mut Criterion) {
    c.bench_function("fibonacci", |b| b.iter(|| fibonacci(20)));
}

criterion_group!(benches, fibonacci_benchmark);
criterion_main!(benches);
```

### Criterion.rs vs. JMH (Java)

| Feature | Criterion.rs | JMH |
|---------|-------------|-----|
| **Language** | Rust | Java |
| **JVM Warmup** | N/A | ✅ Handles JVM warmup |
| **Statistical Analysis** | ✅ Bootstrap resampling | ✅ Comprehensive statistics |
| **Memory Management** | Manual/RAII | ✅ Automatic GC |
| **Performance** | ✅ Native performance | JVM overhead |
| **Microbenchmark Safety** | Good | ✅ Excellent (JVM optimizations) |

**Example Comparison:**

```java
// JMH (Java)
@Benchmark
public int fibonacci() {
    return fibonacci(20);
}
```

```rust
// Criterion.rs
use criterion::{criterion_group, criterion_main, Criterion};

fn fibonacci_benchmark(c: &mut Criterion) {
    c.bench_function("fibonacci", |b| b.iter(|| fibonacci(20)));
}

criterion_group!(benches, fibonacci_benchmark);
criterion_main!(benches);
```

## Feature Comparison Matrix

| Feature | Criterion.rs | cargo bench | Divan | Iai | Google Benchmark | pytest-benchmark | JMH |
|---------|-------------|-------------|-------|-----|------------------|------------------|-----|
| **Statistical Rigor** | ✅✅✅ | ❌ | ✅ | ❌ | ✅ | ✅✅ | ✅✅✅ |
| **HTML Reports** | ✅✅✅ | ❌ | ❌ | ❌ | ❌ | ✅ | ✅ |
| **Regression Detection** | ✅✅✅ | ❌ | ✅ | ❌ | ✅ | ✅ | ✅ |
| **Custom Measurements** | ✅✅✅ | ❌ | ✅ | ❌ | ✅✅ | ✅ | ✅✅ |
| **Async Support** | ✅✅✅ | ❌ | ✅✅ | ❌ | ❌ | ✅ | ❌ |
| **Cross-platform** | ✅✅✅ | ✅✅✅ | ✅✅✅ | ❌ | ✅✅✅ | ✅✅✅ | ✅✅✅ |
| **Setup Complexity** | ✅✅ | ✅✅✅ | ✅✅✅ | ❌ | ✅ | ✅✅✅ | ✅ |
| **Performance** | ✅✅✅ | ✅✅✅ | ✅✅✅ | ✅✅ | ✅✅✅ | ❌ | ✅✅ |
| **Memory Safety** | ✅✅✅ | ✅✅✅ | ✅✅✅ | ✅✅✅ | ❌ | ✅ | ✅✅ |
| **Ecosystem Integration** | ✅✅ | ✅✅✅ | ✅✅ | ✅ | ✅ | ✅✅✅ | ✅✅ |

Legend: ✅✅✅ Excellent, ✅✅ Good, ✅ Basic, ❌ Not supported

## Use Case Recommendations

### Choose Criterion.rs when:

1. **Rust Projects with Statistical Requirements**
   ```rust
   // Need confidence intervals and regression detection
   c.bench_function("critical_algorithm", |b| {
       b.iter(|| performance_critical_code())
   });
   ```

2. **Comprehensive Performance Analysis**
   ```rust
   // Multiple measurement types and detailed reports
   let mut criterion = Criterion::default()
       .with_measurement(CpuCycles)
       .with_profiler(PerfProfiler::new());
   ```

3. **Async Code Benchmarking**
   ```rust
   // Native async support
   c.bench_function("async_operation", |b| {
       b.to_async(&rt).iter(|| async_function())
   });
   ```

4. **Performance Regression Monitoring**
   ```rust
   // Automatic baseline comparison
   cargo bench  // Establishes baseline
   // Make changes...
   cargo bench  // Detects regressions
   ```

### Choose Alternatives when:

1. **Simple Timing Needs** → `cargo bench`
   ```rust
   #[bench]
   fn simple_timing(b: &mut test::Bencher) {
       b.iter(|| simple_function());
   }
   ```

2. **Faster Compilation** → Divan
   ```rust
   #[divan::bench]
   fn fast_compile(bencher: Bencher) {
       bencher.bench(|| function());
   }
   ```

3. **Deterministic Results** → Iai
   ```rust
   // Instruction counting for reproducibility
   fn deterministic() {
       iai::black_box(algorithm());
   }
   ```

4. **C++ Projects** → Google Benchmark
   ```cpp
   static void BM_Function(benchmark::State& state) {
       for (auto _ : state) {
           benchmark::DoNotOptimize(function());
       }
   }
   ```

5. **Python Projects** → pytest-benchmark
   ```python
   def test_function(benchmark):
       result = benchmark(function)
   ```

6. **JVM Projects** → JMH
   ```java
   @Benchmark
   public void function() {
       // JVM-optimized benchmarking
   }
   ```

## Migration Guides

### From `cargo bench` to Criterion.rs

```rust
// Before (cargo bench)
#![feature(test)]
extern crate test;

#[bench]
fn bench_function(b: &mut test::Bencher) {
    b.iter(|| expensive_function());
}

// After (Criterion.rs)
use criterion::{criterion_group, criterion_main, Criterion};

fn criterion_function(c: &mut Criterion) {
    c.bench_function("function", |b| {
        b.iter(|| expensive_function())
    });
}

criterion_group!(benches, criterion_function);
criterion_main!(benches);
```

**Migration Steps:**
1. Add Criterion to `Cargo.toml`
2. Remove `#![feature(test)]` and `extern crate test`
3. Convert `#[bench]` functions to Criterion format
4. Add `criterion_group!` and `criterion_main!` macros

### From Google Benchmark to Criterion.rs

```cpp
// Before (Google Benchmark)
#include <benchmark/benchmark.h>

static void BM_Algorithm(benchmark::State& state) {
    for (auto _ : state) {
        benchmark::DoNotOptimize(algorithm());
    }
}
BENCHMARK(BM_Algorithm);
```

```rust
// After (Criterion.rs)
use criterion::{criterion_group, criterion_main, Criterion, black_box};

fn algorithm_benchmark(c: &mut Criterion) {
    c.bench_function("algorithm", |b| {
        b.iter(|| black_box(algorithm()))
    });
}

criterion_group!(benches, algorithm_benchmark);
criterion_main!(benches);
```

**Key Differences:**
- `benchmark::DoNotOptimize()` → `black_box()`
- `benchmark::State` loop → `b.iter()` closure
- `BENCHMARK()` macro → `criterion_group!()` and `criterion_main!()`

## Performance Comparison

### Benchmark Overhead Comparison

| Tool | Setup Overhead | Per-iteration Overhead | Memory Usage |
|------|----------------|------------------------|--------------|
| Criterion.rs | ~50ms | ~10ns | ~10MB |
| cargo bench | ~5ms | ~5ns | ~1MB |
| Divan | ~20ms | ~5ns | ~5MB |
| Iai | ~100ms | ~50ns | ~20MB |
| Google Benchmark | ~30ms | ~5ns | ~5MB |

### Accuracy Comparison

| Tool | Timing Precision | Statistical Rigor | Reproducibility |
|------|------------------|-------------------|-----------------|
| Criterion.rs | ±1ns | High (bootstrap) | Good |
| cargo bench | ±10ns | Low | Fair |
| Divan | ±1ns | Medium | Good |
| Iai | ±1 instruction | N/A | Excellent |
| Google Benchmark | ±1ns | Medium | Good |

## Conclusion

**Criterion.rs excels when you need:**
- Statistical rigor and confidence intervals
- Comprehensive HTML reports
- Regression detection and baseline comparison
- Async benchmarking support
- Custom measurement types
- Cross-platform compatibility

**Consider alternatives when:**
- You need minimal dependencies (`cargo bench`)
- Faster compilation is critical (Divan)
- Deterministic instruction counting is required (Iai)
- Working in other languages (Google Benchmark, JMH, pytest-benchmark)

For most Rust projects requiring serious performance analysis, Criterion.rs provides the best balance of features, statistical rigor, and ease of use.

## Next Steps

- [API Reference](./api-reference.md) - Complete Criterion.rs API documentation
- [Getting Started](./getting-started.md) - Begin using Criterion.rs
- [Statistical Analysis](./statistical-analysis.md) - Understanding Criterion's statistical methods