# Statistical Analysis in Criterion.rs 0.6

Criterion.rs provides robust statistical analysis capabilities that go far beyond simple timing measurements. This guide explains the statistical methods used and how to interpret and configure them.

## Core Statistical Concepts

### Bootstrap Resampling

Criterion uses bootstrap resampling to estimate the sampling distribution of performance measurements:

```rust
use criterion::{criterion_group, criterion_main, Criterion};

fn statistical_benchmark(c: &mut Criterion) {
    c.bench_function("bootstrap_example", |b| {
        b.iter(|| {
            // Criterion will run this many times and use bootstrap
            // resampling to estimate confidence intervals
            expensive_computation()
        })
    });
}

fn expensive_computation() -> u64 {
    (0..1000).map(|x| x * x).sum()
}

criterion_group!(benches, statistical_benchmark);
criterion_main!(benches);
```

**How Bootstrap Works:**
1. Collect sample measurements (default: 100 samples)
2. Resample with replacement to create bootstrap samples
3. Calculate statistics on each bootstrap sample
4. Use the distribution of bootstrap statistics to estimate confidence intervals

### Confidence Intervals

Criterion reports 95% confidence intervals by default:

```
time:   [26.029 us 26.251 us 26.505 us]
```

This means we're 95% confident the true mean execution time lies between 26.029 and 26.505 microseconds.

#### Configuring Confidence Level

```rust
use criterion::{criterion_group, criterion_main, Criterion};

fn confidence_config(c: &mut Criterion) {
    let mut criterion = c.confidence_level(0.99);  // 99% confidence intervals
    
    criterion.bench_function("high_confidence", |b| {
        b.iter(|| expensive_computation())
    });
}

criterion_group!(benches, confidence_config);
criterion_main!(benches);
```

### Significance Testing

Criterion uses statistical hypothesis testing to determine if performance changes are significant:

```rust
use criterion::{criterion_group, criterion_main, Criterion};

fn significance_testing(c: &mut Criterion) {
    let mut criterion = c.significance_level(0.01);  // 1% significance level (99% confidence)
    
    criterion.bench_function("significance_test", |b| {
        b.iter(|| expensive_computation())
    });
}

criterion_group!(benches, significance_testing);
criterion_main!(benches);
```

**Interpretation:**
- **p < 0.05**: Statistically significant change detected
- **p ≥ 0.05**: No significant change detected

## Outlier Detection

Criterion automatically detects and reports outliers using Tukey's method:

### Outlier Classification

```
Found 3 outliers among 100 measurements (3.00%)
  1 (1.00%) low mild
  1 (1.00%) high mild  
  1 (1.00%) high severe
```

**Outlier Types:**
- **Low/High Mild**: 1.5 × IQR beyond Q1/Q3
- **Low/High Severe**: 3.0 × IQR beyond Q1/Q3

### Configuring Outlier Handling

```rust
use criterion::{criterion_group, criterion_main, Criterion};

fn outlier_config(c: &mut Criterion) {
    let mut criterion = c.noise_threshold(0.05);  // 5% noise threshold
    
    criterion.bench_function("outlier_handling", |b| {
        b.iter(|| {
            // Occasionally slow operation that might produce outliers
            if rand::random::<f64>() < 0.01 {
                std::thread::sleep(std::time::Duration::from_micros(100));
            }
            expensive_computation()
        })
    });
}

criterion_group!(benches, outlier_config);
criterion_main!(benches);
```

### Understanding Outlier Impact

```rust
use criterion::{criterion_group, criterion_main, Criterion, BenchmarkId};

fn outlier_analysis(c: &mut Criterion) {
    let mut group = c.benchmark_group("outlier_comparison");
    
    // Stable performance
    group.bench_function("stable", |b| {
        b.iter(|| {
            std::hint::black_box((0..1000).sum::<i32>())
        })
    });
    
    // Variable performance with outliers
    group.bench_function("variable", |b| {
        b.iter(|| {
            if rand::random::<f64>() < 0.1 {
                // 10% chance of slow execution
                std::thread::sleep(std::time::Duration::from_micros(50));
            }
            std::hint::black_box((0..1000).sum::<i32>())
        })
    });
    
    group.finish();
}

criterion_group!(benches, outlier_analysis);
criterion_main!(benches);
```

## Regression Analysis

Criterion performs linear regression to detect performance trends:

### Understanding Regression Output

```
slope  [2.6251 us 2.6479 us 2.6708 us] per iteration
R²     [0.9891 0.9903 0.9915]
```

**Interpretation:**
- **Slope**: Time per iteration (for parameterized benchmarks)
- **R²**: Coefficient of determination (how well the linear model fits)
  - R² > 0.9: Excellent linear relationship
  - R² > 0.7: Good linear relationship
  - R² < 0.5: Poor linear relationship

### Parameterized Regression Analysis

```rust
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};

fn regression_analysis(c: &mut Criterion) {
    let mut group = c.benchmark_group("algorithm_complexity");
    
    for size in [10, 50, 100, 500, 1000, 5000].iter() {
        group.throughput(Throughput::Elements(*size as u64));
        group.bench_with_input(
            BenchmarkId::new("linear_search", size),
            size,
            |b, &size| {
                let data: Vec<i32> = (0..size).collect();
                let target = size / 2;
                b.iter(|| {
                    linear_search(&data, target)
                })
            }
        );
        
        group.bench_with_input(
            BenchmarkId::new("binary_search", size),
            size,
            |b, &size| {
                let data: Vec<i32> = (0..size).collect();
                let target = size / 2;
                b.iter(|| {
                    data.binary_search(&target)
                })
            }
        );
    }
    
    group.finish();
}

fn linear_search(data: &[i32], target: i32) -> Option<usize> {
    data.iter().position(|&x| x == target)
}

criterion_group!(benches, regression_analysis);
criterion_main!(benches);
```

## Advanced Statistical Configuration

### Sample Size and Measurement Time

```rust
use criterion::{criterion_group, criterion_main, Criterion};
use std::time::Duration;

fn statistical_config(c: &mut Criterion) {
    let mut criterion = c
        .sample_size(200)                                    // More samples for better statistics
        .measurement_time(Duration::from_secs(5))           // Longer measurement time
        .warm_up_time(Duration::from_secs(1))               // Warm-up period
        .confidence_level(0.99)                             // 99% confidence intervals
        .significance_level(0.01)                           // 1% significance level
        .noise_threshold(0.02);                             // 2% noise threshold
    
    criterion.bench_function("high_precision", |b| {
        b.iter(|| expensive_computation())
    });
}

criterion_group!(benches, statistical_config);
criterion_main!(benches);
```

### Sampling Modes

```rust
use criterion::{criterion_group, criterion_main, Criterion, SamplingMode};

fn sampling_modes(c: &mut Criterion) {
    let mut group = c.benchmark_group("sampling_comparison");
    
    // Auto sampling (default) - adapts based on function execution time
    group.sampling_mode(SamplingMode::Auto);
    group.bench_function("auto_sampling", |b| {
        b.iter(|| fast_operation())
    });
    
    // Flat sampling - fixed number of iterations per sample
    group.sampling_mode(SamplingMode::Flat);
    group.bench_function("flat_sampling", |b| {
        b.iter(|| fast_operation())
    });
    
    group.finish();
}

fn fast_operation() -> u64 {
    42 * 42
}

criterion_group!(benches, sampling_modes);
criterion_main!(benches);
```

## Statistical Interpretation Guide

### Reading Criterion Output

```
algorithm_name          time:   [2.1234 ms 2.1456 ms 2.1678 ms]
                        change: [-2.3456% -1.2345% +0.1234%] (p = 0.23 > 0.05)
                        No change in performance detected.
Found 1 outliers among 100 measurements (1.00%)
  1 (1.00%) high mild
slope  [2.1234 us 2.1456 us 2.1678 us] per iteration
R²     [0.9876 0.9888 0.9900]
```

**Line-by-Line Breakdown:**

1. **Time**: [lower_bound mean upper_bound] - 95% confidence interval for execution time
2. **Change**: Performance change compared to baseline with confidence interval
3. **p-value**: Statistical significance test result
4. **Outliers**: Number and classification of outlier measurements
5. **Slope**: Linear regression slope (for parameterized benchmarks)
6. **R²**: Goodness of fit for linear regression

### Performance Change Interpretation

| Change % | Interpretation |
|----------|----------------|
| < -10% | Significant improvement |
| -10% to -5% | Moderate improvement |
| -5% to +5% | No meaningful change |
| +5% to +10% | Moderate regression |
| > +10% | Significant regression |

### Statistical Significance

| p-value | Interpretation |
|---------|----------------|
| p < 0.01 | Highly significant |
| p < 0.05 | Significant |
| p < 0.10 | Marginally significant |
| p ≥ 0.10 | Not significant |

## Custom Statistical Analysis

### Exporting Raw Data

```rust
use criterion::{criterion_group, criterion_main, Criterion};

fn data_export(c: &mut Criterion) {
    // Criterion automatically exports data to target/criterion/
    // You can process this data with external tools
    
    c.bench_function("data_export_example", |b| {
        b.iter(|| expensive_computation())
    });
}

criterion_group!(benches, data_export);
criterion_main!(benches);
```

**Exported Files:**
- `raw.csv`: Raw measurement data
- `estimates.json`: Statistical estimates
- `sample.json`: Sample measurements
- `tukey.json`: Outlier analysis results

### Processing Exported Data

```python
# Example Python script to analyze Criterion data
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# Load raw measurement data
data = pd.read_csv('target/criterion/benchmark_name/base/raw.csv')

# Calculate additional statistics
print(f"Mean: {data['sample_measured_value'].mean():.2f} ns")
print(f"Median: {data['sample_measured_value'].median():.2f} ns")
print(f"Std Dev: {data['sample_measured_value'].std():.2f} ns")
print(f"CV: {data['sample_measured_value'].std() / data['sample_measured_value'].mean() * 100:.2f}%")

# Create custom visualizations
plt.figure(figsize=(10, 6))
plt.hist(data['sample_measured_value'], bins=50, alpha=0.7)
plt.xlabel('Execution Time (ns)')
plt.ylabel('Frequency')
plt.title('Distribution of Execution Times')
plt.show()
```

## Best Practices for Statistical Analysis

### 1. Choose Appropriate Sample Sizes

```rust
// For stable, fast operations
c.sample_size(100);  // Default, usually sufficient

// For variable or slow operations
c.sample_size(200);  // More samples for better statistics

// For very noisy operations
c.sample_size(500);  // Even more samples
```

### 2. Set Realistic Measurement Times

```rust
use std::time::Duration;

// For fast operations (< 1μs)
c.measurement_time(Duration::from_secs(10));

// For medium operations (1μs - 1ms)
c.measurement_time(Duration::from_secs(5));  // Default

// For slow operations (> 1ms)
c.measurement_time(Duration::from_secs(2));
```

### 3. Handle Outliers Appropriately

```rust
// For stable systems
c.noise_threshold(0.01);  // 1% - strict outlier detection

// For variable systems
c.noise_threshold(0.05);  // 5% - more lenient

// For very noisy systems
c.noise_threshold(0.10);  // 10% - very lenient
```

### 4. Interpret Results Carefully

- Always consider confidence intervals, not just point estimates
- Look for consistent trends across multiple runs
- Consider practical significance, not just statistical significance
- Account for system variability and external factors

## Next Steps

- [Performance Regression Detection](./regression-detection.md) - Detecting performance changes over time
- [Benchmark Organization](./benchmark-organization.md) - Structuring benchmark suites
- [HTML Reports](./html-reports.md) - Visualizing statistical results