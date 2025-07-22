# Criterion.rs 0.6 - Comprehensive Documentation

Criterion.rs is a statistics-driven benchmarking library for Rust that provides robust, reliable performance measurements with statistical analysis and regression detection.

## Table of Contents

1. [Getting Started](./getting-started.md) - Basic benchmarking setup and examples
2. [Statistical Analysis](./statistical-analysis.md) - Understanding Criterion's statistical features
3. [Benchmark Organization](./benchmark-organization.md) - Structuring and organizing benchmarks
4. [Performance Regression Detection](./regression-detection.md) - Detecting and analyzing performance changes
5. [Custom Measurements](./custom-measurements.md) - Implementing custom measurement types
6. [Profiling Integration](./profiling-integration.md) - Integrating with profilers and analysis tools
7. [HTML Report Generation](./html-reports.md) - Generating and customizing HTML reports
8. [Tool Comparisons](./tool-comparisons.md) - Comparing Criterion with other benchmarking tools
9. [API Reference](./api-reference.md) - Complete API documentation with examples

## What's New in Version 0.6

### Key Changes
- **MSRV bumped to Rust 1.80** for modern language features
- **Enhanced Tokio support** with `Handle` integration for async benchmarks
- **Deprecated `criterion::black_box()`** in favor of `std::hint::black_box()`
- **Improved Windows compatibility** for Gnuplot detection
- **Unpinned clap dependency** for better ecosystem compatibility

### Breaking Changes
- Minimum supported Rust version is now 1.80
- `criterion::black_box()` is deprecated (use `std::hint::black_box()`)
- Some internal APIs have been refined for better performance

### New Features
- Better async runtime integration
- Enhanced statistical analysis capabilities
- Improved HTML report generation
- More flexible custom measurement support

## Quick Start

Add Criterion to your `Cargo.toml`:

```toml
[dev-dependencies]
criterion = { version = "0.6", features = ["html_reports"] }

[[bench]]
name = "my_benchmark"
harness = false
```

Create a basic benchmark:

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

Run with: `cargo bench`

## Core Features

### Statistical Rigor
- Bootstrap resampling for robust statistics
- Confidence intervals and significance testing
- Outlier detection using Tukey's method
- Regression analysis with R² calculation

### Flexible Measurement
- Wall-clock time (default)
- CPU cycles and instructions
- Memory allocations
- Custom measurement traits

### Rich Reporting
- Interactive HTML reports with SVG plots
- Terminal output with statistical summaries
- CSV data export for external analysis
- Baseline comparison and regression detection

### Async Support
- Native async/await benchmark support
- Multiple runtime backends (Tokio, async-std)
- Configurable runtime integration

## Architecture Overview

Criterion uses a three-tier API structure:

1. **Criterion** - Global configuration and benchmark runner
2. **BenchmarkGroup** - Groups related benchmarks with shared configuration
3. **Bencher** - Individual benchmark execution and measurement

This hierarchical design allows for flexible configuration inheritance while maintaining clean separation of concerns.

## Getting Help

- [Official Documentation](https://docs.rs/criterion/)
- [GitHub Repository](https://github.com/bheisler/criterion.rs)
- [User Guide](https://bheisler.github.io/criterion.rs/book/)
- [Examples](https://github.com/bheisler/criterion.rs/tree/master/benches)

## License

Criterion.rs is dual-licensed under Apache 2.0 and MIT licenses.