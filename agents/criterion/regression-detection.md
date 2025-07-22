# Performance Regression Detection in Criterion.rs 0.6

Criterion.rs provides sophisticated performance regression detection capabilities that help you identify when code changes impact performance. This guide covers how to set up, configure, and interpret regression detection.

## Understanding Baselines

### What are Baselines?

Baselines are reference measurements that Criterion uses to compare current performance against previous runs. They enable:

- Detection of performance improvements and regressions
- Statistical significance testing of changes
- Historical performance tracking
- Automated performance monitoring in CI/CD

### Baseline Storage

Criterion stores baselines in the `target/criterion/` directory:

```
target/criterion/
├── benchmark_name/
│   ├── base/                    # Current baseline
│   │   ├── estimates.json       # Statistical estimates
│   │   ├── raw.csv             # Raw measurement data
│   │   ├── sample.json         # Sample measurements
│   │   └── tukey.json          # Outlier analysis
│   ├── change/                  # Comparison results
│   │   └── estimates.json       # Change estimates
│   └── report/                  # HTML reports
│       └── index.html
```

## Basic Regression Detection

### Setting Up Baseline Comparison

```rust
use criterion::{criterion_group, criterion_main, Criterion};

fn regression_benchmark(c: &mut Criterion) {
    c.bench_function("algorithm_performance", |b| {
        b.iter(|| {
            // Your algorithm implementation
            expensive_algorithm()
        })
    });
}

fn expensive_algorithm() -> u64 {
    (0..10000).map(|x| x * x).sum()
}

criterion_group!(benches, regression_benchmark);
criterion_main!(benches);
```

### Running Baseline Comparison

```bash
# First run - establishes baseline
cargo bench

# Make code changes...

# Second run - compares against baseline
cargo bench

# Output will show performance change:
# algorithm_performance   time:   [1.234 ms 1.245 ms 1.256 ms]
#                         change: [-2.34% -1.23% +0.12%] (p = 0.23 > 0.05)
#                         No change in performance detected.
```

## Advanced Regression Detection

### Custom Baseline Management

```rust
use criterion::{criterion_group, criterion_main, Criterion};

fn baseline_management_example(c: &mut Criterion) {
    // Save current measurements as a named baseline
    let mut criterion = c.retain_baseline("v1.0.0".to_string());
    
    criterion.bench_function("versioned_algorithm", |b| {
        b.iter(|| algorithm_v1())
    });
}

// Later, compare against the named baseline
fn compare_against_version(c: &mut Criterion) {
    let mut criterion = c.load_baseline("v1.0.0".to_string());
    
    criterion.bench_function("versioned_algorithm", |b| {
        b.iter(|| algorithm_v2())  // New implementation
    });
}

criterion_group!(benches, baseline_management_example);
criterion_main!(benches);
```

### Automated Regression Testing

```rust
use criterion::{criterion_group, criterion_main, Criterion};
use std::time::Duration;

fn regression_test_suite(c: &mut Criterion) {
    let mut criterion = c
        .significance_level(0.01)                               // Stricter significance testing
        .confidence_level(0.99)                                 // Higher confidence
        .measurement_time(Duration::from_secs(10))              // Longer measurement for precision
        .sample_size(200);                                      // More samples for accuracy
    
    // Critical performance paths
    criterion.bench_function("critical_path_1", |b| {
        b.iter(|| critical_algorithm_1())
    });
    
    criterion.bench_function("critical_path_2", |b| {
        b.iter(|| critical_algorithm_2())
    });
    
    criterion.bench_function("critical_path_3", |b| {
        b.iter(|| critical_algorithm_3())
    });
}

criterion_group!(benches, regression_test_suite);
criterion_main!(benches);
```

## CI/CD Integration

### GitHub Actions Example

```yaml
# .github/workflows/performance.yml
name: Performance Regression Tests

on:
  pull_request:
    branches: [ main ]
  push:
    branches: [ main ]

jobs:
  performance:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v3
      with:
        fetch-depth: 0  # Need full history for baseline comparison
    
    - name: Install Rust
      uses: actions-rs/toolchain@v1
      with:
        toolchain: stable
        profile: minimal
        override: true
    
    - name: Cache Criterion Baselines
      uses: actions/cache@v3
      with:
        path: target/criterion
        key: criterion-${{ runner.os }}-${{ github.ref }}
        restore-keys: |
          criterion-${{ runner.os }}-refs/heads/main
    
    - name: Run Performance Tests
      run: |
        cargo bench --bench regression -- --output-format json > benchmark_results.json
    
    - name: Check for Regressions
      run: |
        python scripts/check_regressions.py benchmark_results.json
    
    - name: Upload Results
      uses: actions/upload-artifact@v3
      with:
        name: benchmark-results
        path: |
          benchmark_results.json
          target/criterion/*/report/
```

### Regression Detection Script

```python
#!/usr/bin/env python3
# scripts/check_regressions.py

import json
import sys
from typing import Dict, List, Tuple

def parse_criterion_output(filename: str) -> Dict:
    """Parse Criterion JSON output."""
    with open(filename, 'r') as f:
        return json.load(f)

def check_for_regressions(results: Dict, threshold: float = 0.05) -> List[Tuple[str, float, float]]:
    """
    Check for performance regressions above threshold.
    
    Returns list of (benchmark_name, change_percent, p_value) for regressions.
    """
    regressions = []
    
    for benchmark_name, data in results.items():
        if 'change' in data:
            change = data['change']
            p_value = change.get('p_value', 1.0)
            
            # Check if change is statistically significant and above threshold
            if p_value < 0.05:  # Statistically significant
                change_percent = change['mean']['point_estimate']
                if change_percent > threshold:  # Performance regression
                    regressions.append((benchmark_name, change_percent, p_value))
    
    return regressions

def main():
    if len(sys.argv) != 2:
        print("Usage: check_regressions.py <criterion_output.json>")
        sys.exit(1)
    
    results = parse_criterion_output(sys.argv[1])
    regressions = check_for_regressions(results, threshold=0.05)  # 5% threshold
    
    if regressions:
        print("❌ Performance regressions detected:")
        for name, change, p_value in regressions:
            print(f"  {name}: +{change:.2%} (p={p_value:.4f})")
        sys.exit(1)
    else:
        print("✅ No significant performance regressions detected")
        sys.exit(0)

if __name__ == "__main__":
    main()
```

## Interpreting Regression Results

### Understanding Change Output

```
algorithm_performance   time:   [1.234 ms 1.245 ms 1.256 ms]
                        change: [-2.34% -1.23% +0.12%] (p = 0.23 > 0.05)
                        Performance has improved.
```

**Components:**
- **Time**: Current performance with confidence interval
- **Change**: Performance change with confidence interval
- **p-value**: Statistical significance (p < 0.05 indicates significant change)
- **Interpretation**: Criterion's assessment of the change

### Change Significance Levels

| p-value | Interpretation |
|---------|----------------|
| p < 0.01 | Highly significant change |
| p < 0.05 | Significant change |
| p < 0.10 | Marginally significant |
| p ≥ 0.10 | No significant change |

### Performance Change Categories

| Change % | Category | Action |
|----------|----------|---------|
| < -10% | Major improvement | Document and celebrate |
| -10% to -5% | Moderate improvement | Note in changelog |
| -5% to +5% | No meaningful change | Continue monitoring |
| +5% to +10% | Moderate regression | Investigate cause |
| > +10% | Major regression | Block deployment |

## Advanced Regression Analysis

### Multi-Baseline Comparison

```rust
use criterion::{criterion_group, criterion_main, Criterion};

fn multi_baseline_comparison(c: &mut Criterion) {
    // Compare against multiple baselines
    let baselines = ["v1.0.0", "v1.1.0", "v1.2.0"];
    
    for baseline in &baselines {
        let mut criterion = c.load_baseline(baseline.to_string());
        
        criterion.bench_function(&format!("vs_{}", baseline), |b| {
            b.iter(|| current_algorithm())
        });
    }
}

criterion_group!(benches, multi_baseline_comparison);
criterion_main!(benches);
```

### Regression Trend Analysis

```rust
use criterion::{criterion_group, criterion_main, Criterion, BenchmarkId};
use std::collections::HashMap;

fn trend_analysis(c: &mut Criterion) {
    let mut group = c.benchmark_group("performance_trends");
    
    // Track performance across different code paths
    let test_cases = [
        ("small_input", generate_small_data()),
        ("medium_input", generate_medium_data()),
        ("large_input", generate_large_data()),
    ];
    
    for (name, data) in test_cases {
        group.bench_with_input(
            BenchmarkId::new("algorithm_trend", name),
            &data,
            |b, data| {
                b.iter(|| process_data(data))
            }
        );
    }
    
    group.finish();
}

criterion_group!(benches, trend_analysis);
criterion_main!(benches);
```

### Custom Regression Thresholds

```rust
use criterion::{criterion_group, criterion_main, Criterion};

fn custom_thresholds(c: &mut Criterion) {
    // Different thresholds for different types of benchmarks
    
    // Critical path - very strict threshold
    let mut critical = c
        .significance_level(0.001)                              // 0.1% significance
        .noise_threshold(0.01);                                 // 1% noise threshold
    
    critical.bench_function("critical_algorithm", |b| {
        b.iter(|| critical_path_algorithm())
    });
    
    // Non-critical path - more lenient
    let mut non_critical = c
        .significance_level(0.05)                               // 5% significance
        .noise_threshold(0.05);                                 // 5% noise threshold
    
    non_critical.bench_function("non_critical_algorithm", |b| {
        b.iter(|| non_critical_algorithm())
    });
}

criterion_group!(benches, custom_thresholds);
criterion_main!(benches);
```

## Regression Detection Strategies

### 1. Continuous Monitoring

```rust
// Set up benchmarks that run on every commit
fn continuous_monitoring(c: &mut Criterion) {
    let mut criterion = c
        .sample_size(50)                                        // Faster execution
        .measurement_time(Duration::from_secs(2))               // Shorter measurement
        .significance_level(0.05);                              // Standard significance
    
    // Monitor key performance indicators
    criterion.bench_function("kpi_response_time", |b| {
        b.iter(|| simulate_request_processing())
    });
    
    criterion.bench_function("kpi_throughput", |b| {
        b.iter(|| simulate_batch_processing())
    });
}
```

### 2. Release Validation

```rust
// Comprehensive benchmarks for release validation
fn release_validation(c: &mut Criterion) {
    let mut criterion = c
        .sample_size(200)                                       // High precision
        .measurement_time(Duration::from_secs(10))              // Long measurement
        .confidence_level(0.99)                                 // High confidence
        .significance_level(0.01);                              // Strict significance
    
    // Comprehensive test suite
    criterion.bench_function("full_system_test", |b| {
        b.iter(|| full_system_simulation())
    });
}
```

### 3. A/B Testing

```rust
fn ab_testing_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("ab_testing");
    
    // Test different implementations
    group.bench_function("implementation_a", |b| {
        b.iter(|| algorithm_implementation_a())
    });
    
    group.bench_function("implementation_b", |b| {
        b.iter(|| algorithm_implementation_b())
    });
    
    group.finish();
}
```

## Best Practices

### 1. Stable Test Environment

```bash
# Set CPU governor to performance mode
echo performance | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor

# Disable CPU frequency scaling
sudo cpupower frequency-set --governor performance

# Set process priority
nice -n -20 cargo bench
```

### 2. Consistent Test Data

```rust
use rand::{Rng, SeedableRng};
use rand_chacha::ChaCha8Rng;

fn generate_consistent_test_data() -> Vec<i32> {
    // Always use the same seed for reproducible results
    let mut rng = ChaCha8Rng::seed_from_u64(42);
    (0..10000).map(|_| rng.gen()).collect()
}
```

### 3. Baseline Management

```bash
# Save baseline before major changes
cargo bench --save-baseline before_optimization

# Make changes...

# Compare against saved baseline
cargo bench --load-baseline before_optimization

# Clean up old baselines
rm -rf target/criterion/*/old_baseline_name
```

### 4. Automated Alerts

```python
# Example Slack notification script
import requests
import json

def send_regression_alert(benchmark_name: str, change_percent: float):
    webhook_url = "YOUR_SLACK_WEBHOOK_URL"
    
    message = {
        "text": f"🚨 Performance Regression Detected",
        "attachments": [
            {
                "color": "danger",
                "fields": [
                    {
                        "title": "Benchmark",
                        "value": benchmark_name,
                        "short": True
                    },
                    {
                        "title": "Performance Change",
                        "value": f"+{change_percent:.2%}",
                        "short": True
                    }
                ]
            }
        ]
    }
    
    requests.post(webhook_url, json=message)
```

## Troubleshooting Regression Detection

### Common Issues

1. **Noisy Measurements**
   ```rust
   // Increase sample size and measurement time
   c.sample_size(500)
    .measurement_time(Duration::from_secs(20));
   ```

2. **Inconsistent Environment**
   ```bash
   # Use consistent CPU settings
   sudo cpupower frequency-set --governor performance
   ```

3. **False Positives**
   ```rust
   // Adjust noise threshold
   c.noise_threshold(0.05);  // 5% threshold
   ```

4. **Missing Baselines**
   ```bash
   # Ensure baseline directory exists
   mkdir -p target/criterion
   ```

## Next Steps

- [Custom Measurements](./custom-measurements.md) - Implementing custom metrics for regression detection
- [HTML Reports](./html-reports.md) - Visualizing regression analysis results
- [Profiling Integration](./profiling-integration.md) - Deep performance analysis