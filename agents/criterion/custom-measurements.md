# Custom Measurements and Profiling Integration in Criterion.rs 0.6

Criterion.rs supports custom measurement types beyond wall-clock time, enabling you to measure CPU cycles, memory allocations, cache misses, and other performance metrics. This guide covers implementing custom measurements and integrating with profiling tools.

## Understanding Measurement Types

### Built-in Measurements

Criterion provides several built-in measurement types:

1. **WallTime** (default) - Wall-clock time measurement
2. **CpuTime** - CPU time measurement
3. **Custom measurements** - User-defined metrics

### Measurement Trait

All measurements implement the `Measurement` trait:

```rust
pub trait Measurement {
    type Intermediate: Copy + Clone + Debug;
    type Value: Serialize + for<'de> Deserialize<'de> + Debug + Clone;

    fn start(&self) -> Self::Intermediate;
    fn end(&self, i: Self::Intermediate) -> Self::Value;
    fn add(&self, v1: &Self::Value, v2: &Self::Value) -> Self::Value;
    fn zero(&self) -> Self::Value;
    fn to_f64(&self, value: &Self::Value) -> f64;
    fn formatter(&self) -> &dyn ValueFormatter;
}
```

## CPU Cycle Measurements

### Basic CPU Cycle Counter

```rust
use criterion::{criterion_group, criterion_main, Criterion};
use std::arch::x86_64::_rdtsc;

#[derive(Clone, Copy, Debug)]
struct CpuCycles;

#[derive(Clone, Copy, Debug, serde::Serialize, serde::Deserialize)]
struct CycleCount(u64);

impl criterion::measurement::Measurement for CpuCycles {
    type Intermediate = u64;
    type Value = CycleCount;

    fn start(&self) -> Self::Intermediate {
        unsafe { _rdtsc() }
    }

    fn end(&self, start: Self::Intermediate) -> Self::Value {
        let end = unsafe { _rdtsc() };
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

    fn formatter(&self) -> &dyn criterion::measurement::ValueFormatter {
        &CycleFormatter
    }
}

struct CycleFormatter;

impl criterion::measurement::ValueFormatter for CycleFormatter {
    fn format_value(&self, value: f64) -> String {
        if value < 1_000.0 {
            format!("{:.0} cycles", value)
        } else if value < 1_000_000.0 {
            format!("{:.1} K cycles", value / 1_000.0)
        } else if value < 1_000_000_000.0 {
            format!("{:.1} M cycles", value / 1_000_000.0)
        } else {
            format!("{:.1} G cycles", value / 1_000_000_000.0)
        }
    }

    fn format_throughput(&self, throughput: &criterion::Throughput, value: f64) -> String {
        match throughput {
            criterion::Throughput::Bytes(bytes) => {
                format!("{} cycles/byte", self.format_value(value / *bytes as f64))
            }
            criterion::Throughput::Elements(elements) => {
                format!("{} cycles/element", self.format_value(value / *elements as f64))
            }
        }
    }

    fn scale_values(&self, typical_value: f64, values: &mut [f64]) -> &'static str {
        if typical_value < 1_000.0 {
            "cycles"
        } else if typical_value < 1_000_000.0 {
            for value in values {
                *value /= 1_000.0;
            }
            "K cycles"
        } else if typical_value < 1_000_000_000.0 {
            for value in values {
                *value /= 1_000_000.0;
            }
            "M cycles"
        } else {
            for value in values {
                *value /= 1_000_000_000.0;
            }
            "G cycles"
        }
    }

    fn scale_throughputs(
        &self,
        typical_value: f64,
        throughput: &criterion::Throughput,
        values: &mut [f64],
    ) -> &'static str {
        match throughput {
            criterion::Throughput::Bytes(_) => {
                self.scale_values(typical_value, values);
                "cycles/byte"
            }
            criterion::Throughput::Elements(_) => {
                self.scale_values(typical_value, values);
                "cycles/element"
            }
        }
    }

    fn scale_for_machines(&self, values: &mut [f64]) -> &'static str {
        for value in values {
            *value /= 1_000_000.0;
        }
        "M cycles"
    }
}

fn cpu_cycle_benchmark(c: &mut Criterion<CpuCycles>) {
    c.bench_function("cpu_cycles_example", |b| {
        b.iter(|| {
            // CPU-intensive computation
            let mut sum = 0u64;
            for i in 0..1000 {
                sum = sum.wrapping_add(i * i);
            }
            std::hint::black_box(sum)
        })
    });
}

fn main() {
    let mut criterion = Criterion::default().with_measurement(CpuCycles);
    cpu_cycle_benchmark(&mut criterion);
}
```

### Using CPU Cycles with Groups

```rust
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};

fn cpu_cycle_comparison(c: &mut Criterion<CpuCycles>) {
    let mut group = c.benchmark_group("algorithm_cycles");
    
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

fn main() {
    let mut criterion = Criterion::default().with_measurement(CpuCycles);
    cpu_cycle_comparison(&mut criterion);
}
```

## Memory Allocation Measurements

### Tracking Memory Allocations

```rust
use criterion::{criterion_group, criterion_main, Criterion};
use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering};

// Global allocation tracker
static ALLOCATED: AtomicUsize = AtomicUsize::new(0);
static DEALLOCATED: AtomicUsize = AtomicUsize::new(0);

struct TrackingAllocator;

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

#[derive(Clone, Copy, Debug)]
struct MemoryMeasurement;

#[derive(Clone, Copy, Debug, serde::Serialize, serde::Deserialize)]
struct MemoryUsage {
    allocated: usize,
    deallocated: usize,
}

impl criterion::measurement::Measurement for MemoryMeasurement {
    type Intermediate = (usize, usize);
    type Value = MemoryUsage;

    fn start(&self) -> Self::Intermediate {
        (
            ALLOCATED.load(Ordering::Relaxed),
            DEALLOCATED.load(Ordering::Relaxed),
        )
    }

    fn end(&self, (start_alloc, start_dealloc): Self::Intermediate) -> Self::Value {
        let end_alloc = ALLOCATED.load(Ordering::Relaxed);
        let end_dealloc = DEALLOCATED.load(Ordering::Relaxed);
        
        MemoryUsage {
            allocated: end_alloc - start_alloc,
            deallocated: end_dealloc - start_dealloc,
        }
    }

    fn add(&self, v1: &Self::Value, v2: &Self::Value) -> Self::Value {
        MemoryUsage {
            allocated: v1.allocated + v2.allocated,
            deallocated: v1.deallocated + v2.deallocated,
        }
    }

    fn zero(&self) -> Self::Value {
        MemoryUsage {
            allocated: 0,
            deallocated: 0,
        }
    }

    fn to_f64(&self, value: &Self::Value) -> f64 {
        (value.allocated as f64) - (value.deallocated as f64)
    }

    fn formatter(&self) -> &dyn criterion::measurement::ValueFormatter {
        &MemoryFormatter
    }
}

struct MemoryFormatter;

impl criterion::measurement::ValueFormatter for MemoryFormatter {
    fn format_value(&self, value: f64) -> String {
        if value < 1024.0 {
            format!("{:.0} B", value)
        } else if value < 1024.0 * 1024.0 {
            format!("{:.1} KB", value / 1024.0)
        } else if value < 1024.0 * 1024.0 * 1024.0 {
            format!("{:.1} MB", value / (1024.0 * 1024.0))
        } else {
            format!("{:.1} GB", value / (1024.0 * 1024.0 * 1024.0))
        }
    }

    fn format_throughput(&self, throughput: &criterion::Throughput, value: f64) -> String {
        match throughput {
            criterion::Throughput::Bytes(bytes) => {
                format!("{}/byte", self.format_value(value / *bytes as f64))
            }
            criterion::Throughput::Elements(elements) => {
                format!("{}/element", self.format_value(value / *elements as f64))
            }
        }
    }

    fn scale_values(&self, typical_value: f64, values: &mut [f64]) -> &'static str {
        if typical_value < 1024.0 {
            "B"
        } else if typical_value < 1024.0 * 1024.0 {
            for value in values {
                *value /= 1024.0;
            }
            "KB"
        } else if typical_value < 1024.0 * 1024.0 * 1024.0 {
            for value in values {
                *value /= 1024.0 * 1024.0;
            }
            "MB"
        } else {
            for value in values {
                *value /= 1024.0 * 1024.0 * 1024.0;
            }
            "GB"
        }
    }

    fn scale_throughputs(
        &self,
        typical_value: f64,
        throughput: &criterion::Throughput,
        values: &mut [f64],
    ) -> &'static str {
        match throughput {
            criterion::Throughput::Bytes(_) => {
                let unit = self.scale_values(typical_value, values);
                match unit {
                    "B" => "B/byte",
                    "KB" => "KB/byte",
                    "MB" => "MB/byte",
                    "GB" => "GB/byte",
                    _ => unreachable!(),
                }
            }
            criterion::Throughput::Elements(_) => {
                let unit = self.scale_values(typical_value, values);
                match unit {
                    "B" => "B/element",
                    "KB" => "KB/element",
                    "MB" => "MB/element",
                    "GB" => "GB/element",
                    _ => unreachable!(),
                }
            }
        }
    }

    fn scale_for_machines(&self, values: &mut [f64]) -> &'static str {
        for value in values {
            *value /= 1024.0;
        }
        "KB"
    }
}

fn memory_benchmark(c: &mut Criterion<MemoryMeasurement>) {
    c.bench_function("vector_allocation", |b| {
        b.iter(|| {
            let mut vec = Vec::new();
            for i in 0..1000 {
                vec.push(i);
            }
            std::hint::black_box(vec)
        })
    });
}

fn main() {
    let mut criterion = Criterion::default().with_measurement(MemoryMeasurement);
    memory_benchmark(&mut criterion);
}
```

## Hardware Performance Counter Integration

### Using `perf_event` for Hardware Counters

```toml
# Add to Cargo.toml
[dev-dependencies]
criterion = "0.6"
perf-event = "0.4"
```

```rust
use criterion::{criterion_group, criterion_main, Criterion};
use perf_event::{Builder, Group};
use std::collections::HashMap;

#[derive(Clone, Debug)]
struct PerfCounters {
    counter_types: Vec<perf_event::events::Hardware>,
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
struct PerfMeasurement {
    counters: HashMap<String, u64>,
}

impl PerfCounters {
    fn new() -> Self {
        Self {
            counter_types: vec![
                perf_event::events::Hardware::CPU_CYCLES,
                perf_event::events::Hardware::INSTRUCTIONS,
                perf_event::events::Hardware::CACHE_REFERENCES,
                perf_event::events::Hardware::CACHE_MISSES,
                perf_event::events::Hardware::BRANCH_INSTRUCTIONS,
                perf_event::events::Hardware::BRANCH_MISSES,
            ],
        }
    }
}

impl criterion::measurement::Measurement for PerfCounters {
    type Intermediate = Group;
    type Value = PerfMeasurement;

    fn start(&self) -> Self::Intermediate {
        let mut group = Group::new().unwrap();
        
        for &counter_type in &self.counter_types {
            let counter = Builder::new()
                .group(&mut group)
                .kind(counter_type)
                .build()
                .unwrap();
            group.add_counter(counter);
        }
        
        group.enable().unwrap();
        group
    }

    fn end(&self, mut group: Self::Intermediate) -> Self::Value {
        group.disable().unwrap();
        let counts = group.read().unwrap();
        
        let mut counters = HashMap::new();
        for (i, &counter_type) in self.counter_types.iter().enumerate() {
            let name = format!("{:?}", counter_type).to_lowercase();
            counters.insert(name, counts[i]);
        }
        
        PerfMeasurement { counters }
    }

    fn add(&self, v1: &Self::Value, v2: &Self::Value) -> Self::Value {
        let mut counters = HashMap::new();
        
        for (key, value1) in &v1.counters {
            let value2 = v2.counters.get(key).unwrap_or(&0);
            counters.insert(key.clone(), value1 + value2);
        }
        
        PerfMeasurement { counters }
    }

    fn zero(&self) -> Self::Value {
        PerfMeasurement {
            counters: HashMap::new(),
        }
    }

    fn to_f64(&self, value: &Self::Value) -> f64 {
        // Use CPU cycles as the primary metric
        value.counters.get("cpu_cycles").unwrap_or(&0).clone() as f64
    }

    fn formatter(&self) -> &dyn criterion::measurement::ValueFormatter {
        &PerfFormatter
    }
}

struct PerfFormatter;

impl criterion::measurement::ValueFormatter for PerfFormatter {
    fn format_value(&self, value: f64) -> String {
        if value < 1_000.0 {
            format!("{:.0} cycles", value)
        } else if value < 1_000_000.0 {
            format!("{:.1} K cycles", value / 1_000.0)
        } else if value < 1_000_000_000.0 {
            format!("{:.1} M cycles", value / 1_000_000.0)
        } else {
            format!("{:.1} G cycles", value / 1_000_000_000.0)
        }
    }

    // ... implement other required methods similar to previous examples
}

fn perf_counter_benchmark(c: &mut Criterion<PerfCounters>) {
    c.bench_function("matrix_multiply", |b| {
        let size = 100;
        let matrix_a = vec![vec![1.0; size]; size];
        let matrix_b = vec![vec![2.0; size]; size];
        
        b.iter(|| {
            matrix_multiply(&matrix_a, &matrix_b)
        })
    });
}

fn matrix_multiply(a: &[Vec<f64>], b: &[Vec<f64>]) -> Vec<Vec<f64>> {
    let size = a.len();
    let mut result = vec![vec![0.0; size]; size];
    
    for i in 0..size {
        for j in 0..size {
            for k in 0..size {
                result[i][j] += a[i][k] * b[k][j];
            }
        }
    }
    
    result
}

fn main() {
    let mut criterion = Criterion::default().with_measurement(PerfCounters::new());
    perf_counter_benchmark(&mut criterion);
}
```

## Profiler Integration

### Intel VTune Integration

```rust
use criterion::{criterion_group, criterion_main, Criterion, Profiler};
use std::process::Command;
use std::path::Path;

struct VTuneProfiler {
    output_dir: String,
}

impl VTuneProfiler {
    fn new(output_dir: &str) -> Self {
        Self {
            output_dir: output_dir.to_string(),
        }
    }
}

impl Profiler for VTuneProfiler {
    fn start_profiling(&mut self, benchmark_id: &str, benchmark_dir: &Path) {
        let output_path = benchmark_dir.join(&self.output_dir);
        std::fs::create_dir_all(&output_path).unwrap();
        
        Command::new("vtune")
            .args(&[
                "-collect", "hotspots",
                "-result-dir", output_path.to_str().unwrap(),
                "-target-pid", &std::process::id().to_string(),
            ])
            .spawn()
            .expect("Failed to start VTune profiler");
    }

    fn stop_profiling(&mut self, benchmark_id: &str, benchmark_dir: &Path) {
        Command::new("vtune")
            .args(&["-command", "stop"])
            .output()
            .expect("Failed to stop VTune profiler");
    }
}

fn vtune_benchmark(c: &mut Criterion) {
    let mut criterion = c.with_profiler(VTuneProfiler::new("vtune_results"));
    
    criterion.bench_function("cpu_intensive", |b| {
        b.iter(|| {
            // CPU-intensive computation for profiling
            let mut result = 0u64;
            for i in 0..1_000_000 {
                result = result.wrapping_add(i * i);
            }
            std::hint::black_box(result)
        })
    });
}

criterion_group!(benches, vtune_benchmark);
criterion_main!(benches);
```

### Perf Integration

```rust
use criterion::{criterion_group, criterion_main, Criterion, Profiler};
use std::process::{Command, Stdio};
use std::path::Path;

struct PerfProfiler {
    child: Option<std::process::Child>,
}

impl PerfProfiler {
    fn new() -> Self {
        Self { child: None }
    }
}

impl Profiler for PerfProfiler {
    fn start_profiling(&mut self, benchmark_id: &str, benchmark_dir: &Path) {
        let output_file = benchmark_dir.join(format!("{}.perf.data", benchmark_id));
        
        let child = Command::new("perf")
            .args(&[
                "record",
                "-g",                                           // Call graph
                "-F", "997",                                    // Sampling frequency
                "-o", output_file.to_str().unwrap(),
                "-p", &std::process::id().to_string(),
            ])
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("Failed to start perf");
        
        self.child = Some(child);
    }

    fn stop_profiling(&mut self, benchmark_id: &str, benchmark_dir: &Path) {
        if let Some(mut child) = self.child.take() {
            child.kill().expect("Failed to kill perf process");
            child.wait().expect("Failed to wait for perf process");
        }
    }
}

fn perf_benchmark(c: &mut Criterion) {
    let mut criterion = c.with_profiler(PerfProfiler::new());
    
    criterion.bench_function("algorithm_profile", |b| {
        b.iter(|| {
            expensive_algorithm()
        })
    });
}

fn expensive_algorithm() -> u64 {
    let mut result = 0u64;
    for i in 0..100_000 {
        result = result.wrapping_add(fibonacci(i % 30));
    }
    result
}

fn fibonacci(n: u64) -> u64 {
    match n {
        0 | 1 => 1,
        n => fibonacci(n - 1) + fibonacci(n - 2),
    }
}

criterion_group!(benches, perf_benchmark);
criterion_main!(benches);
```

### Custom Profiler Integration

```rust
use criterion::{criterion_group, criterion_main, Criterion, Profiler};
use std::path::Path;
use std::time::Instant;

struct CustomProfiler {
    start_time: Option<Instant>,
    samples: Vec<f64>,
}

impl CustomProfiler {
    fn new() -> Self {
        Self {
            start_time: None,
            samples: Vec::new(),
        }
    }
}

impl Profiler for CustomProfiler {
    fn start_profiling(&mut self, _benchmark_id: &str, _benchmark_dir: &Path) {
        self.start_time = Some(Instant::now());
        self.samples.clear();
        
        // Start custom profiling logic
        println!("Starting custom profiling...");
    }

    fn stop_profiling(&mut self, benchmark_id: &str, benchmark_dir: &Path) {
        if let Some(start) = self.start_time.take() {
            let duration = start.elapsed();
            
            // Save profiling results
            let output_file = benchmark_dir.join(format!("{}_profile.json", benchmark_id));
            let profile_data = serde_json::json!({
                "benchmark_id": benchmark_id,
                "duration_ms": duration.as_millis(),
                "samples": self.samples,
            });
            
            std::fs::write(output_file, profile_data.to_string()).unwrap();
            println!("Custom profiling completed for {}", benchmark_id);
        }
    }
}

fn custom_profiler_benchmark(c: &mut Criterion) {
    let mut criterion = c.with_profiler(CustomProfiler::new());
    
    criterion.bench_function("custom_profiled", |b| {
        b.iter(|| {
            // Your benchmark code here
            std::hint::black_box(expensive_computation())
        })
    });
}

criterion_group!(benches, custom_profiler_benchmark);
criterion_main!(benches);
```

## Multi-Metric Measurements

### Combining Multiple Metrics

```rust
use criterion::{criterion_group, criterion_main, Criterion};
use std::collections::HashMap;

#[derive(Clone, Debug)]
struct MultiMetric;

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
struct MultiMeasurement {
    wall_time_ns: u64,
    cpu_cycles: u64,
    instructions: u64,
    cache_misses: u64,
}

impl criterion::measurement::Measurement for MultiMetric {
    type Intermediate = (std::time::Instant, u64, u64, u64);
    type Value = MultiMeasurement;

    fn start(&self) -> Self::Intermediate {
        let wall_start = std::time::Instant::now();
        let cpu_start = unsafe { std::arch::x86_64::_rdtsc() };
        
        // In a real implementation, you'd start hardware counters here
        (wall_start, cpu_start, 0, 0)
    }

    fn end(&self, (wall_start, cpu_start, _, _): Self::Intermediate) -> Self::Value {
        let wall_end = std::time::Instant::now();
        let cpu_end = unsafe { std::arch::x86_64::_rdtsc() };
        
        MultiMeasurement {
            wall_time_ns: wall_end.duration_since(wall_start).as_nanos() as u64,
            cpu_cycles: cpu_end - cpu_start,
            instructions: 0,  // Would be filled by hardware counters
            cache_misses: 0,  // Would be filled by hardware counters
        }
    }

    fn add(&self, v1: &Self::Value, v2: &Self::Value) -> Self::Value {
        MultiMeasurement {
            wall_time_ns: v1.wall_time_ns + v2.wall_time_ns,
            cpu_cycles: v1.cpu_cycles + v2.cpu_cycles,
            instructions: v1.instructions + v2.instructions,
            cache_misses: v1.cache_misses + v2.cache_misses,
        }
    }

    fn zero(&self) -> Self::Value {
        MultiMeasurement {
            wall_time_ns: 0,
            cpu_cycles: 0,
            instructions: 0,
            cache_misses: 0,
        }
    }

    fn to_f64(&self, value: &Self::Value) -> f64 {
        value.wall_time_ns as f64
    }

    fn formatter(&self) -> &dyn criterion::measurement::ValueFormatter {
        &MultiFormatter
    }
}

struct MultiFormatter;

impl criterion::measurement::ValueFormatter for MultiFormatter {
    fn format_value(&self, value: f64) -> String {
        format!("{:.2} ns", value)
    }

    // ... implement other required methods
}

fn multi_metric_benchmark(c: &mut Criterion<MultiMetric>) {
    c.bench_function("multi_metric_test", |b| {
        b.iter(|| {
            // Algorithm that we want to measure with multiple metrics
            let mut data = vec![0; 10000];
            for i in 0..data.len() {
                data[i] = i * i;
            }
            std::hint::black_box(data)
        })
    });
}

fn main() {
    let mut criterion = Criterion::default().with_measurement(MultiMetric);
    multi_metric_benchmark(&mut criterion);
}
```

## Best Practices for Custom Measurements

### 1. Minimize Measurement Overhead

```rust
// Good: Minimal overhead in measurement
impl Measurement for FastMeasurement {
    fn start(&self) -> Self::Intermediate {
        unsafe { std::arch::x86_64::_rdtsc() }  // Fast, single instruction
    }

    fn end(&self, start: Self::Intermediate) -> Self::Value {
        let end = unsafe { std::arch::x86_64::_rdtsc() };
        MeasurementValue(end - start)
    }
}

// Avoid: Heavy computation in measurement
impl Measurement for SlowMeasurement {
    fn start(&self) -> Self::Intermediate {
        // Don't do this - expensive system calls
        let mut file = std::fs::File::open("/proc/stat").unwrap();
        // ... complex parsing
    }
}
```

### 2. Handle Measurement Errors Gracefully

```rust
impl Measurement for RobustMeasurement {
    fn start(&self) -> Self::Intermediate {
        match self.try_start() {
            Ok(intermediate) => intermediate,
            Err(_) => {
                eprintln!("Warning: Measurement failed, falling back to wall time");
                std::time::Instant::now()
            }
        }
    }
}
```

### 3. Provide Clear Documentation

```rust
/// Measures CPU cycles using the RDTSC instruction.
/// 
/// # Platform Support
/// - x86_64: Full support using RDTSC
/// - ARM: Not supported
/// - Other: Falls back to wall time
/// 
/// # Accuracy
/// - Resolution: ~1 cycle
/// - Overhead: ~10-20 cycles per measurement
/// - Affected by: CPU frequency scaling, out-of-order execution
/// 
/// # Usage
/// ```rust
/// let mut criterion = Criterion::default().with_measurement(CpuCycles);
/// ```
struct CpuCycles;
```

### 4. Validate Measurements

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_measurement_consistency() {
        let measurement = CpuCycles;
        
        // Test that measurement is consistent
        let start = measurement.start();
        std::thread::sleep(std::time::Duration::from_millis(1));
        let value = measurement.end(start);
        
        assert!(measurement.to_f64(&value) > 0.0);
    }

    #[test]
    fn test_measurement_additivity() {
        let measurement = CpuCycles;
        let v1 = CycleCount(100);
        let v2 = CycleCount(200);
        let sum = measurement.add(&v1, &v2);
        
        assert_eq!(measurement.to_f64(&sum), 300.0);
    }
}
```

## Next Steps

- [HTML Reports](./html-reports.md) - Visualizing custom measurement results
- [Profiling Integration](./profiling-integration.md) - Advanced profiling techniques
- [API Reference](./api-reference.md) - Complete measurement API documentation