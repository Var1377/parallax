# HTML Report Generation in Criterion.rs 0.6

Criterion.rs generates comprehensive HTML reports with interactive visualizations, statistical analysis, and performance comparisons. This guide covers how to generate, customize, and interpret these reports.

## Enabling HTML Reports

### Basic Setup

Add the `html_reports` feature to your `Cargo.toml`:

```toml
[dev-dependencies]
criterion = { version = "0.6", features = ["html_reports"] }
```

### Generating Reports

```rust
use criterion::{criterion_group, criterion_main, Criterion};

fn html_report_benchmark(c: &mut Criterion) {
    c.bench_function("example_function", |b| {
        b.iter(|| {
            // Your benchmark code
            expensive_computation()
        })
    });
}

fn expensive_computation() -> u64 {
    (0..10000).map(|x| x * x).sum()
}

criterion_group!(benches, html_report_benchmark);
criterion_main!(benches);
```

Run benchmarks to generate HTML reports:

```bash
cargo bench
```

Reports are generated in `target/criterion/`:

```
target/criterion/
├── example_function/
│   ├── base/
│   │   ├── estimates.json
│   │   ├── raw.csv
│   │   └── sample.json
│   └── report/
│       ├── index.html          # Main report page
│       ├── pdf.svg             # Probability density function
│       ├── regression.svg      # Regression analysis
│       ├── MAD.svg            # Mean absolute deviation
│       ├── mean.svg           # Mean estimation
│       ├── median.svg         # Median estimation
│       ├── SD.svg             # Standard deviation
│       └── slope.svg          # Slope analysis (for parameterized benchmarks)
└── report/
    └── index.html              # Summary report for all benchmarks
```

## Report Structure and Content

### Main Report Page

The main HTML report includes:

1. **Executive Summary**
   - Performance statistics with confidence intervals
   - Change detection compared to baseline
   - Statistical significance testing results

2. **Interactive Plots**
   - Probability density function (PDF)
   - Regression analysis for parameterized benchmarks
   - Statistical distribution visualizations

3. **Raw Data**
   - Sample measurements
   - Outlier analysis
   - Bootstrap resampling results

### Individual Benchmark Reports

Each benchmark gets its own detailed report with:

```html
<!-- Example report structure -->
<!DOCTYPE html>
<html>
<head>
    <title>Criterion.rs Benchmark Report</title>
    <style>/* Criterion's CSS styles */</style>
</head>
<body>
    <h1>Benchmark: example_function</h1>
    
    <!-- Performance Summary -->
    <div class="summary">
        <h2>Summary</h2>
        <p>Time: [1.234 ms 1.245 ms 1.256 ms]</p>
        <p>Change: [-2.34% -1.23% +0.12%] (p = 0.23 > 0.05)</p>
    </div>
    
    <!-- Interactive SVG Plots -->
    <div class="plots">
        <h2>Probability Density Function</h2>
        <object data="pdf.svg" type="image/svg+xml"></object>
        
        <h2>Regression Analysis</h2>
        <object data="regression.svg" type="image/svg+xml"></object>
    </div>
    
    <!-- Additional Analysis -->
    <div class="analysis">
        <h2>Additional Statistics</h2>
        <!-- Mean, median, standard deviation plots -->
    </div>
</body>
</html>
```

## Customizing HTML Reports

### Custom Report Configuration

```rust
use criterion::{criterion_group, criterion_main, Criterion, PlotConfiguration, AxisScale};

fn custom_report_config(c: &mut Criterion) {
    let plot_config = PlotConfiguration::default()
        .summary_scale(AxisScale::Logarithmic)
        .sample_size(200);
    
    let mut criterion = c.plot_config(plot_config);
    
    criterion.bench_function("custom_report", |b| {
        b.iter(|| expensive_computation())
    });
}

criterion_group!(benches, custom_report_config);
criterion_main!(benches);
```

### Custom CSS Styling

Create a custom CSS file and reference it in your reports:

```css
/* custom-criterion.css */
body {
    font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
    background-color: #f5f5f5;
    margin: 0;
    padding: 20px;
}

.summary {
    background-color: white;
    border-radius: 8px;
    padding: 20px;
    margin-bottom: 20px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}

.performance-improved {
    color: #28a745;
    font-weight: bold;
}

.performance-regressed {
    color: #dc3545;
    font-weight: bold;
}

.plots {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(400px, 1fr));
    gap: 20px;
}

.plot-container {
    background-color: white;
    border-radius: 8px;
    padding: 15px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}

.plot-title {
    font-size: 1.2em;
    font-weight: bold;
    margin-bottom: 10px;
    color: #333;
}
```

### Custom Report Templates

```rust
use criterion::{criterion_group, criterion_main, Criterion};
use std::path::Path;

// Custom report generation function
fn generate_custom_report(benchmark_name: &str, data: &BenchmarkData) {
    let template = r#"
<!DOCTYPE html>
<html>
<head>
    <title>Custom Benchmark Report: {benchmark_name}</title>
    <link rel="stylesheet" href="custom-criterion.css">
    <script src="https://d3js.org/d3.v7.min.js"></script>
</head>
<body>
    <div class="header">
        <h1>Performance Report: {benchmark_name}</h1>
        <div class="timestamp">Generated: {timestamp}</div>
    </div>
    
    <div class="summary-card">
        <h2>Performance Summary</h2>
        <div class="metric">
            <span class="label">Mean Execution Time:</span>
            <span class="value">{mean_time}</span>
        </div>
        <div class="metric">
            <span class="label">Standard Deviation:</span>
            <span class="value">{std_dev}</span>
        </div>
        <div class="metric">
            <span class="label">Confidence Interval:</span>
            <span class="value">[{ci_lower}, {ci_upper}]</span>
        </div>
    </div>
    
    <div class="visualizations">
        <div class="plot-container">
            <h3>Performance Distribution</h3>
            <div id="distribution-plot"></div>
        </div>
        
        <div class="plot-container">
            <h3>Time Series</h3>
            <div id="timeseries-plot"></div>
        </div>
    </div>
    
    <script>
        // Custom D3.js visualizations
        const data = {data_json};
        
        // Create distribution plot
        createDistributionPlot(data.samples);
        
        // Create time series plot
        createTimeSeriesPlot(data.timeseries);
    </script>
</body>
</html>
"#;

    // Fill template with actual data
    let html = template
        .replace("{benchmark_name}", benchmark_name)
        .replace("{timestamp}", &chrono::Utc::now().to_rfc3339())
        .replace("{mean_time}", &format!("{:.2} ms", data.mean))
        .replace("{std_dev}", &format!("{:.2} ms", data.std_dev))
        .replace("{ci_lower}", &format!("{:.2} ms", data.ci_lower))
        .replace("{ci_upper}", &format!("{:.2} ms", data.ci_upper))
        .replace("{data_json}", &serde_json::to_string(&data).unwrap());
    
    // Write custom report
    let output_path = Path::new("target/criterion/custom_reports")
        .join(format!("{}.html", benchmark_name));
    std::fs::create_dir_all(output_path.parent().unwrap()).unwrap();
    std::fs::write(output_path, html).unwrap();
}

fn custom_template_benchmark(c: &mut Criterion) {
    c.bench_function("custom_template", |b| {
        b.iter(|| expensive_computation())
    });
    
    // Generate custom report after benchmarking
    // This would typically be done in a post-processing step
}

criterion_group!(benches, custom_template_benchmark);
criterion_main!(benches);
```

## Interactive Visualizations

### Understanding SVG Plots

Criterion generates interactive SVG plots with the following features:

#### 1. Probability Density Function (PDF)
```svg
<!-- Example PDF plot structure -->
<svg viewBox="0 0 800 600" xmlns="http://www.w3.org/2000/svg">
    <!-- Axes -->
    <g class="axes">
        <line x1="50" y1="550" x2="750" y2="550" stroke="black"/>
        <line x1="50" y1="50" x2="50" y2="550" stroke="black"/>
    </g>
    
    <!-- Distribution curve -->
    <path d="M50,400 Q200,300 400,350 Q600,400 750,450" 
          fill="none" stroke="blue" stroke-width="2"/>
    
    <!-- Confidence interval shading -->
    <rect x="150" y="300" width="500" height="200" 
          fill="lightblue" opacity="0.3"/>
    
    <!-- Interactive tooltips -->
    <circle cx="400" cy="350" r="5" fill="red">
        <title>Mean: 1.245 ms</title>
    </circle>
</svg>
```

#### 2. Regression Analysis
For parameterized benchmarks, Criterion shows linear regression:

```svg
<!-- Regression plot showing performance vs input size -->
<svg viewBox="0 0 800 600" xmlns="http://www.w3.org/2000/svg">
    <!-- Data points -->
    <circle cx="100" cy="500" r="3" fill="blue">
        <title>Size: 10, Time: 0.1 ms</title>
    </circle>
    <circle cx="200" cy="450" r="3" fill="blue">
        <title>Size: 100, Time: 0.5 ms</title>
    </circle>
    <circle cx="300" cy="400" r="3" fill="blue">
        <title>Size: 1000, Time: 1.2 ms</title>
    </circle>
    
    <!-- Regression line -->
    <line x1="50" y1="520" x2="750" y2="200" 
          stroke="red" stroke-width="2"/>
    
    <!-- R² value -->
    <text x="600" y="100" font-family="Arial" font-size="14">
        R² = 0.9876
    </text>
</svg>
```

### Adding Custom JavaScript

```html
<!-- Enhanced interactivity -->
<script>
document.addEventListener('DOMContentLoaded', function() {
    // Add zoom functionality to plots
    const plots = document.querySelectorAll('object[type="image/svg+xml"]');
    
    plots.forEach(plot => {
        plot.addEventListener('load', function() {
            const svgDoc = plot.contentDocument;
            const svg = svgDoc.querySelector('svg');
            
            // Add zoom and pan functionality
            let scale = 1;
            let translateX = 0;
            let translateY = 0;
            
            svg.addEventListener('wheel', function(e) {
                e.preventDefault();
                const delta = e.deltaY > 0 ? 0.9 : 1.1;
                scale *= delta;
                
                svg.style.transform = `scale(${scale}) translate(${translateX}px, ${translateY}px)`;
            });
            
            // Add data export functionality
            const exportButton = document.createElement('button');
            exportButton.textContent = 'Export Data';
            exportButton.onclick = function() {
                exportPlotData(plot);
            };
            plot.parentNode.insertBefore(exportButton, plot.nextSibling);
        });
    });
});

function exportPlotData(plotElement) {
    // Extract data from SVG and export as CSV
    const svgDoc = plotElement.contentDocument;
    const dataPoints = svgDoc.querySelectorAll('circle[title]');
    
    let csv = 'x,y,tooltip\n';
    dataPoints.forEach(point => {
        const x = point.getAttribute('cx');
        const y = point.getAttribute('cy');
        const tooltip = point.querySelector('title').textContent;
        csv += `${x},${y},"${tooltip}"\n`;
    });
    
    // Download CSV
    const blob = new Blob([csv], { type: 'text/csv' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = 'benchmark_data.csv';
    a.click();
    URL.revokeObjectURL(url);
}
</script>
```

## Comparative Reports

### Baseline Comparison Reports

```rust
use criterion::{criterion_group, criterion_main, Criterion, BenchmarkId};

fn comparative_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("algorithm_comparison");
    
    // Test different algorithms
    let algorithms = ["bubble_sort", "quick_sort", "merge_sort", "heap_sort"];
    let sizes = [100, 1000, 10000];
    
    for &algorithm in &algorithms {
        for &size in &sizes {
            group.bench_with_input(
                BenchmarkId::new(algorithm, size),
                &size,
                |b, &size| {
                    b.iter_with_setup(
                        || generate_random_data(size),
                        |mut data| match algorithm {
                            "bubble_sort" => bubble_sort(&mut data),
                            "quick_sort" => data.sort(),
                            "merge_sort" => merge_sort(&mut data),
                            "heap_sort" => heap_sort(&mut data),
                            _ => unreachable!(),
                        }
                    )
                }
            );
        }
    }
    
    group.finish();
}

criterion_group!(benches, comparative_benchmark);
criterion_main!(benches);
```

This generates a comparative report showing:
- Performance comparison across algorithms
- Scaling behavior with input size
- Statistical significance of differences
- Interactive plots for easy comparison

### Multi-Version Comparison

```bash
# Compare performance across versions
git checkout v1.0.0
cargo bench --save-baseline v1.0.0

git checkout v1.1.0
cargo bench --save-baseline v1.1.0

git checkout v1.2.0
cargo bench --load-baseline v1.0.0
cargo bench --load-baseline v1.1.0
```

## Report Automation and CI Integration

### Automated Report Generation

```yaml
# .github/workflows/benchmark-reports.yml
name: Generate Benchmark Reports

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  benchmark:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v3
    
    - name: Install Rust
      uses: actions-rs/toolchain@v1
      with:
        toolchain: stable
        profile: minimal
        override: true
    
    - name: Run Benchmarks
      run: cargo bench
    
    - name: Generate Report Archive
      run: |
        cd target/criterion
        tar -czf ../../benchmark-reports.tar.gz .
    
    - name: Upload Reports
      uses: actions/upload-artifact@v3
      with:
        name: benchmark-reports
        path: benchmark-reports.tar.gz
    
    - name: Deploy to GitHub Pages
      if: github.ref == 'refs/heads/main'
      uses: peaceiris/actions-gh-pages@v3
      with:
        github_token: ${{ secrets.GITHUB_TOKEN }}
        publish_dir: target/criterion/report
        destination_dir: benchmarks
```

### Report Processing Script

```python
#!/usr/bin/env python3
# scripts/process_reports.py

import json
import os
import shutil
from pathlib import Path
import argparse

def process_criterion_reports(criterion_dir: Path, output_dir: Path):
    """Process Criterion reports and create summary."""
    
    # Create output directory
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Copy all HTML reports
    report_dirs = list(criterion_dir.glob("*/report"))
    
    summary_data = {
        "benchmarks": [],
        "generated_at": datetime.now().isoformat(),
        "total_benchmarks": len(report_dirs)
    }
    
    for report_dir in report_dirs:
        benchmark_name = report_dir.parent.name
        
        # Copy report files
        dest_dir = output_dir / benchmark_name
        shutil.copytree(report_dir, dest_dir, dirs_exist_ok=True)
        
        # Extract summary data
        estimates_file = report_dir.parent / "base" / "estimates.json"
        if estimates_file.exists():
            with open(estimates_file) as f:
                estimates = json.load(f)
                
            summary_data["benchmarks"].append({
                "name": benchmark_name,
                "mean": estimates["mean"]["point_estimate"],
                "std_dev": estimates["std_dev"]["point_estimate"],
                "median": estimates["median"]["point_estimate"],
                "confidence_interval": {
                    "lower": estimates["mean"]["confidence_interval"]["lower_bound"],
                    "upper": estimates["mean"]["confidence_interval"]["upper_bound"]
                }
            })
    
    # Generate summary report
    generate_summary_html(summary_data, output_dir / "index.html")
    
    # Save summary JSON
    with open(output_dir / "summary.json", "w") as f:
        json.dump(summary_data, f, indent=2)

def generate_summary_html(data: dict, output_file: Path):
    """Generate HTML summary report."""
    
    html_template = """
<!DOCTYPE html>
<html>
<head>
    <title>Benchmark Summary Report</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; }
        .benchmark-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px; }
        .benchmark-card { border: 1px solid #ddd; border-radius: 8px; padding: 20px; }
        .benchmark-name { font-size: 1.2em; font-weight: bold; margin-bottom: 10px; }
        .metric { margin: 5px 0; }
        .metric-label { font-weight: bold; }
    </style>
</head>
<body>
    <h1>Benchmark Summary Report</h1>
    <p>Generated: {generated_at}</p>
    <p>Total Benchmarks: {total_benchmarks}</p>
    
    <div class="benchmark-grid">
        {benchmark_cards}
    </div>
</body>
</html>
"""
    
    benchmark_cards = ""
    for benchmark in data["benchmarks"]:
        card = f"""
        <div class="benchmark-card">
            <div class="benchmark-name">
                <a href="{benchmark['name']}/index.html">{benchmark['name']}</a>
            </div>
            <div class="metric">
                <span class="metric-label">Mean:</span> {benchmark['mean']:.2f} ns
            </div>
            <div class="metric">
                <span class="metric-label">Std Dev:</span> {benchmark['std_dev']:.2f} ns
            </div>
            <div class="metric">
                <span class="metric-label">Median:</span> {benchmark['median']:.2f} ns
            </div>
            <div class="metric">
                <span class="metric-label">95% CI:</span> 
                [{benchmark['confidence_interval']['lower']:.2f}, {benchmark['confidence_interval']['upper']:.2f}] ns
            </div>
        </div>
        """
        benchmark_cards += card
    
    html = html_template.format(
        generated_at=data["generated_at"],
        total_benchmarks=data["total_benchmarks"],
        benchmark_cards=benchmark_cards
    )
    
    with open(output_file, "w") as f:
        f.write(html)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Process Criterion benchmark reports")
    parser.add_argument("criterion_dir", type=Path, help="Path to target/criterion directory")
    parser.add_argument("output_dir", type=Path, help="Output directory for processed reports")
    
    args = parser.parse_args()
    process_criterion_reports(args.criterion_dir, args.output_dir)
```

## Best Practices for HTML Reports

### 1. Organize Reports Hierarchically

```
target/criterion/
├── report/
│   └── index.html              # Main summary
├── algorithms/
│   ├── sorting/
│   │   ├── bubble_sort/
│   │   ├── quick_sort/
│   │   └── merge_sort/
│   └── searching/
│       ├── linear_search/
│       └── binary_search/
└── data_structures/
    ├── vector/
    └── hashmap/
```

### 2. Include Contextual Information

```rust
fn documented_benchmark(c: &mut Criterion) {
    // Add benchmark metadata
    c.bench_function("well_documented", |b| {
        // Document what this benchmark measures
        b.iter(|| {
            // Algorithm: Quick Sort
            // Input: Random integers, size 1000
            // Measures: Wall-clock time
            // Expected: O(n log n) average case
            quick_sort_implementation()
        })
    });
}
```

### 3. Use Consistent Naming

```rust
// Good: Hierarchical, descriptive names
"data_structures/vector/push"
"data_structures/vector/pop"
"algorithms/sorting/quicksort/random_data"
"algorithms/sorting/quicksort/sorted_data"

// Avoid: Unclear or inconsistent names
"test1"
"benchmark_function"
"my_test"
```

### 4. Validate Report Generation

```bash
#!/bin/bash
# scripts/validate_reports.sh

# Run benchmarks
cargo bench

# Check that reports were generated
REPORT_DIR="target/criterion"
if [ ! -d "$REPORT_DIR" ]; then
    echo "Error: No reports generated"
    exit 1
fi

# Check for main index file
if [ ! -f "$REPORT_DIR/report/index.html" ]; then
    echo "Error: Main report index missing"
    exit 1
fi

# Validate HTML files
find "$REPORT_DIR" -name "*.html" -exec htmlhint {} \;

echo "All reports validated successfully"
```

## Next Steps

- [Tool Comparisons](./tool-comparisons.md) - Comparing Criterion with other benchmarking tools
- [API Reference](./api-reference.md) - Complete API documentation
- [Custom Measurements](./custom-measurements.md) - Creating custom visualizations for custom metrics