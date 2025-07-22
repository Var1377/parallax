# Benchmarking and Profiling

This chapter covers comprehensive performance measurement techniques for Logos lexers, including benchmarking, profiling, and performance analysis.

## Setting Up Benchmarks

### Criterion.rs Integration

Criterion is the standard benchmarking library for Rust:

```toml
[dev-dependencies]
criterion = { version = "0.5", features = ["html_reports"] }

[[bench]]
name = "lexer_bench"
harness = false
```

### Basic Benchmark Structure

```rust
// benches/lexer_bench.rs
use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use logos::Logos;
use your_crate::Token;

// Sample data for benchmarking
static SAMPLE_CODE: &str = include_str!("../test_data/sample.rs");
static SAMPLE_JSON: &str = include_str!("../test_data/sample.json");
static SAMPLE_TEXT: &str = include_str!("../test_data/sample.txt");

fn bench_tokenization(c: &mut Criterion) {
    let mut group = c.benchmark_group("tokenization");
    
    // Set throughput for MB/s measurements
    group.throughput(Throughput::Bytes(SAMPLE_CODE.len() as u64));
    
    group.bench_function("rust_code", |b| {
        b.iter(|| {
            let mut lexer = Token::lexer(black_box(SAMPLE_CODE));
            while let Some(token) = lexer.next() {
                black_box(token);
            }
        })
    });
    
    group.throughput(Throughput::Bytes(SAMPLE_JSON.len() as u64));
    group.bench_function("json_data", |b| {
        b.iter(|| {
            let mut lexer = Token::lexer(black_box(SAMPLE_JSON));
            while let Some(token) = lexer.next() {
                black_box(token);
            }
        })
    });
    
    group.finish();
}

fn bench_token_counting(c: &mut Criterion) {
    let mut group = c.benchmark_group("token_counting");
    
    group.throughput(Throughput::Bytes(SAMPLE_CODE.len() as u64));
    group.bench_function("count_tokens", |b| {
        b.iter(|| {
            Token::lexer(black_box(SAMPLE_CODE))
                .filter_map(|t| t.ok())
                .count()
        })
    });
    
    group.finish();
}

criterion_group!(benches, bench_tokenization, bench_token_counting);
criterion_main!(benches);
```

### Advanced Benchmarking

```rust
use criterion::{BenchmarkId, Criterion, ParameterizedBenchmark};
use std::time::Duration;

fn bench_input_sizes(c: &mut Criterion) {
    let mut group = c.benchmark_group("input_sizes");
    
    // Test different input sizes
    let sizes = vec![100, 1000, 10000, 100000];
    
    for size in sizes {
        let input = "hello world ".repeat(size / 12); // Approximate size
        
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("tokenize", size),
            &input,
            |b, input| {
                b.iter(|| {
                    let mut lexer = Token::lexer(black_box(input));
                    while let Some(token) = lexer.next() {
                        black_box(token);
                    }
                })
            },
        );
    }
    
    group.finish();
}

fn bench_token_types(c: &mut Criterion) {
    let mut group = c.benchmark_group("token_types");
    
    // Test different token type distributions
    let test_cases = vec![
        ("identifiers", "hello world foo bar baz qux"),
        ("numbers", "123 456 789 101112 131415"),
        ("mixed", "let x = 42; if (x > 0) { return true; }"),
        ("strings", r#""hello" "world" "foo bar" "baz qux""#),
        ("operators", "+ - * / == != < > <= >= && ||"),
    ];
    
    for (name, input) in test_cases {
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_function(name, |b| {
            b.iter(|| {
                let mut lexer = Token::lexer(black_box(input));
                while let Some(token) = lexer.next() {
                    black_box(token);
                }
            })
        });
    }
    
    group.finish();
}

fn bench_error_handling(c: &mut Criterion) {
    let mut group = c.benchmark_group("error_handling");
    
    let valid_input = "let x = 42;";
    let invalid_input = "let x = @#$;";
    
    group.bench_function("valid_tokens", |b| {
        b.iter(|| {
            let mut lexer = Token::lexer(black_box(valid_input));
            while let Some(token) = lexer.next() {
                black_box(token);
            }
        })
    });
    
    group.bench_function("invalid_tokens", |b| {
        b.iter(|| {
            let mut lexer = Token::lexer(black_box(invalid_input));
            while let Some(token) = lexer.next() {
                black_box(token);
            }
        })
    });
    
    group.finish();
}
```

## Profiling Techniques

### CPU Profiling with perf

```bash
# Build with debug info for profiling
cargo build --release --bin your_lexer

# Profile with perf
perf record --call-graph=dwarf ./target/release/your_lexer large_input.txt

# Analyze results
perf report

# Generate annotated source
perf annotate

# Top functions by CPU usage
perf top
```

### Flamegraph Generation

```bash
# Install flamegraph
cargo install flamegraph

# Generate flamegraph for your lexer
cargo flamegraph --bin your_lexer -- large_input.txt

# Generate flamegraph for benchmarks
cargo flamegraph --bench lexer_bench
```

### Memory Profiling

```bash
# Using heaptrack
heaptrack ./target/release/your_lexer input.txt
heaptrack_gui heaptrack.your_lexer.*.gz

# Using valgrind
valgrind --tool=massif ./target/release/your_lexer input.txt
ms_print massif.out.*

# Using dhat
valgrind --tool=dhat ./target/release/your_lexer input.txt
```

## Performance Analysis

### Benchmark Results Interpretation

```rust
// Example benchmark output analysis
/*
tokenization/rust_code  time:   [1.2345 ms 1.2567 ms 1.2789 ms]
                        thrpt:  [1.1234 GiB/s 1.1456 GiB/s 1.1678 GiB/s]

tokenization/json_data  time:   [987.65 µs 1.0123 ms 1.0456 ms]
                        thrpt:  [1.4567 GiB/s 1.5123 GiB/s 1.5678 GiB/s]

Analysis:
- JSON lexing is faster than Rust code lexing
- Throughput is measured in GiB/s (higher is better)
- Time measurements show confidence intervals
*/

fn analyze_benchmark_results() {
    println!("Benchmark Analysis:");
    println!("- Look for throughput (MB/s or GiB/s) - higher is better");
    println!("- Compare different input types");
    println!("- Check for performance regressions");
    println!("- Identify bottlenecks in specific token types");
}
```

### Performance Regression Testing

```rust
// benches/regression_test.rs
use criterion::{criterion_group, criterion_main, Criterion};

fn regression_test(c: &mut Criterion) {
    let baseline_input = include_str!("../test_data/baseline.txt");
    
    c.bench_function("baseline_performance", |b| {
        b.iter(|| {
            let mut lexer = Token::lexer(baseline_input);
            while let Some(token) = lexer.next() {
                criterion::black_box(token);
            }
        })
    });
}

// Run with: cargo bench --bench regression_test
// Compare results over time to detect regressions
criterion_group!(regression, regression_test);
criterion_main!(regression);
```

## Comparative Benchmarking

### Logos vs Other Lexers

```rust
// benches/comparison.rs
use criterion::{criterion_group, criterion_main, Criterion, BenchmarkId};

// Logos implementation
#[derive(logos::Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum LogosToken {
    #[regex("[a-zA-Z][a-zA-Z0-9]*")]
    Identifier,
    
    #[regex("[0-9]+")]
    Number,
    
    #[token("+")]
    Plus,
    
    #[token("-")]
    Minus,
}

// Regex-based implementation for comparison
use regex::Regex;

struct RegexLexer {
    identifier: Regex,
    number: Regex,
    plus: Regex,
    minus: Regex,
    whitespace: Regex,
}

impl RegexLexer {
    fn new() -> Self {
        Self {
            identifier: Regex::new(r"[a-zA-Z][a-zA-Z0-9]*").unwrap(),
            number: Regex::new(r"[0-9]+").unwrap(),
            plus: Regex::new(r"\+").unwrap(),
            minus: Regex::new(r"-").unwrap(),
            whitespace: Regex::new(r"[ \t\n\f]+").unwrap(),
        }
    }
    
    fn tokenize(&self, input: &str) -> Vec<String> {
        let mut tokens = Vec::new();
        let mut pos = 0;
        
        while pos < input.len() {
            let remaining = &input[pos..];
            
            if let Some(m) = self.whitespace.find(remaining) {
                if m.start() == 0 {
                    pos += m.end();
                    continue;
                }
            }
            
            if let Some(m) = self.identifier.find(remaining) {
                if m.start() == 0 {
                    tokens.push(m.as_str().to_string());
                    pos += m.end();
                    continue;
                }
            }
            
            if let Some(m) = self.number.find(remaining) {
                if m.start() == 0 {
                    tokens.push(m.as_str().to_string());
                    pos += m.end();
                    continue;
                }
            }
            
            if let Some(m) = self.plus.find(remaining) {
                if m.start() == 0 {
                    tokens.push(m.as_str().to_string());
                    pos += m.end();
                    continue;
                }
            }
            
            if let Some(m) = self.minus.find(remaining) {
                if m.start() == 0 {
                    tokens.push(m.as_str().to_string());
                    pos += m.end();
                    continue;
                }
            }
            
            // Skip unknown character
            pos += 1;
        }
        
        tokens
    }
}

fn comparison_benchmark(c: &mut Criterion) {
    let input = "let x = 42 + 7 - 3";
    let regex_lexer = RegexLexer::new();
    
    let mut group = c.benchmark_group("lexer_comparison");
    
    group.bench_function("logos", |b| {
        b.iter(|| {
            let mut lexer = LogosToken::lexer(criterion::black_box(input));
            while let Some(token) = lexer.next() {
                criterion::black_box(token);
            }
        })
    });
    
    group.bench_function("regex", |b| {
        b.iter(|| {
            let tokens = regex_lexer.tokenize(criterion::black_box(input));
            criterion::black_box(tokens);
        })
    });
    
    group.finish();
}

criterion_group!(comparison, comparison_benchmark);
criterion_main!(comparison);
```

## Real-World Performance Testing

### Large File Processing

```rust
use std::fs;
use std::path::Path;

fn bench_large_files(c: &mut Criterion) {
    let mut group = c.benchmark_group("large_files");
    
    // Test with actual large files
    let test_files = vec![
        "test_data/large_source.rs",
        "test_data/large_json.json",
        "test_data/large_text.txt",
    ];
    
    for file_path in test_files {
        if Path::new(file_path).exists() {
            let content = fs::read_to_string(file_path).unwrap();
            let file_name = Path::new(file_path).file_stem().unwrap().to_str().unwrap();
            
            group.throughput(Throughput::Bytes(content.len() as u64));
            group.bench_function(file_name, |b| {
                b.iter(|| {
                    let mut lexer = Token::lexer(criterion::black_box(&content));
                    let mut count = 0;
                    while let Some(_) = lexer.next() {
                        count += 1;
                    }
                    criterion::black_box(count);
                })
            });
        }
    }
    
    group.finish();
}

fn bench_streaming(c: &mut Criterion) {
    let mut group = c.benchmark_group("streaming");
    
    // Simulate streaming by processing chunks
    let large_input = "hello world ".repeat(10000);
    let chunk_size = 1024;
    
    group.bench_function("chunked_processing", |b| {
        b.iter(|| {
            let mut total_tokens = 0;
            
            for chunk in large_input.as_bytes().chunks(chunk_size) {
                if let Ok(chunk_str) = std::str::from_utf8(chunk) {
                    let mut lexer = Token::lexer(chunk_str);
                    while let Some(_) = lexer.next() {
                        total_tokens += 1;
                    }
                }
            }
            
            criterion::black_box(total_tokens);
        })
    });
    
    group.finish();
}
```

### Memory Usage Analysis

```rust
use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering};

// Custom allocator to track memory usage
struct TrackingAllocator;

static ALLOCATED: AtomicUsize = AtomicUsize::new(0);

unsafe impl GlobalAlloc for TrackingAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let ret = System.alloc(layout);
        if !ret.is_null() {
            ALLOCATED.fetch_add(layout.size(), Ordering::SeqCst);
        }
        ret
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        System.dealloc(ptr, layout);
        ALLOCATED.fetch_sub(layout.size(), Ordering::SeqCst);
    }
}

#[global_allocator]
static GLOBAL: TrackingAllocator = TrackingAllocator;

fn bench_memory_usage(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_usage");
    
    group.bench_function("memory_tracking", |b| {
        b.iter_custom(|iters| {
            let start_memory = ALLOCATED.load(Ordering::SeqCst);
            let start_time = std::time::Instant::now();
            
            for _ in 0..iters {
                let mut lexer = Token::lexer("let x = 42;");
                while let Some(token) = lexer.next() {
                    criterion::black_box(token);
                }
            }
            
            let end_time = std::time::Instant::now();
            let end_memory = ALLOCATED.load(Ordering::SeqCst);
            
            println!("Memory used: {} bytes", end_memory.saturating_sub(start_memory));
            
            end_time - start_time
        });
    });
    
    group.finish();
}
```

## Performance Optimization Strategies

### Identifying Bottlenecks

```rust
// Use criterion's profiling features
fn detailed_profiling(c: &mut Criterion) {
    let mut group = c.benchmark_group("profiling");
    
    // Enable profiling
    group.measurement_time(std::time::Duration::from_secs(10));
    group.sample_size(100);
    
    group.bench_function("detailed_tokenization", |b| {
        b.iter_custom(|iters| {
            let start = std::time::Instant::now();
            
            for _ in 0..iters {
                let mut lexer = Token::lexer("complex input with many tokens");
                
                // Measure different phases
                let mut token_count = 0;
                let mut error_count = 0;
                
                while let Some(result) = lexer.next() {
                    match result {
                        Ok(_) => token_count += 1,
                        Err(_) => error_count += 1,
                    }
                }
                
                criterion::black_box((token_count, error_count));
            }
            
            start.elapsed()
        });
    });
    
    group.finish();
}
```

### A/B Testing Optimizations

```rust
// Test different optimization approaches
fn optimization_comparison(c: &mut Criterion) {
    let mut group = c.benchmark_group("optimizations");
    
    let input = "let x = 42; let y = x + 1;";
    
    // Original implementation
    group.bench_function("original", |b| {
        b.iter(|| {
            let mut lexer = OriginalToken::lexer(criterion::black_box(input));
            while let Some(token) = lexer.next() {
                criterion::black_box(token);
            }
        })
    });
    
    // Optimized implementation
    group.bench_function("optimized", |b| {
        b.iter(|| {
            let mut lexer = OptimizedToken::lexer(criterion::black_box(input));
            while let Some(token) = lexer.next() {
                criterion::black_box(token);
            }
        })
    });
    
    group.finish();
}
```

## Continuous Performance Monitoring

### CI/CD Integration

```yaml
# .github/workflows/benchmark.yml
name: Benchmark

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  benchmark:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v2
    
    - name: Install Rust
      uses: actions-rs/toolchain@v1
      with:
        toolchain: stable
        
    - name: Run benchmarks
      run: cargo bench --bench lexer_bench
      
    - name: Store benchmark results
      uses: benchmark-action/github-action-benchmark@v1
      with:
        tool: 'cargo'
        output-file-path: target/criterion/reports/index.html
        github-token: ${{ secrets.GITHUB_TOKEN }}
        auto-push: true
```

### Performance Regression Detection

```rust
// scripts/check_performance.rs
use std::process::Command;
use std::fs;

fn main() {
    // Run benchmarks
    let output = Command::new("cargo")
        .args(&["bench", "--bench", "lexer_bench"])
        .output()
        .expect("Failed to run benchmarks");
    
    // Parse results and compare with baseline
    let results = String::from_utf8_lossy(&output.stdout);
    
    // Simple regression detection
    if results.contains("change: +") {
        let changes: Vec<&str> = results
            .lines()
            .filter(|line| line.contains("change: +"))
            .collect();
        
        for change in changes {
            if let Some(percent) = extract_percentage(change) {
                if percent > 5.0 {
                    eprintln!("Performance regression detected: {}", change);
                    std::process::exit(1);
                }
            }
        }
    }
    
    println!("Performance check passed");
}

fn extract_percentage(line: &str) -> Option<f64> {
    // Extract percentage from benchmark output
    // Implementation depends on criterion output format
    None
}
```

## Best Practices

1. **Establish Baselines**: Create baseline benchmarks before optimization
2. **Use Realistic Data**: Test with real-world input data
3. **Measure Multiple Metrics**: Track throughput, latency, and memory usage
4. **Profile Regularly**: Use profiling tools to identify bottlenecks
5. **Test Edge Cases**: Benchmark error conditions and edge cases
6. **Automate Testing**: Integrate performance tests into CI/CD
7. **Document Results**: Keep records of performance improvements
8. **Compare Alternatives**: Benchmark against other lexer implementations

## Tools and Resources

### Profiling Tools
- **perf**: Linux performance profiler
- **Instruments**: macOS profiling tool
- **flamegraph**: Flame graph generation
- **heaptrack**: Memory profiler
- **valgrind**: Memory debugging and profiling

### Benchmarking Libraries
- **criterion**: Statistical benchmarking
- **iai**: Instruction-level benchmarking
- **divan**: Alternative benchmarking framework

### Analysis Tools
- **cargo-profdata**: LLVM profiling data
- **cargo-asm**: Assembly output analysis
- **cargo-expand**: Macro expansion analysis

## Next Steps

- [API Reference](./09-api-reference.md) - Complete API documentation
- [Performance Optimization](./03-performance-optimization.md) - Optimization techniques
- [Error Handling](./04-error-handling.md) - Error handling performance