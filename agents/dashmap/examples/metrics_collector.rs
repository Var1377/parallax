// Real-time Metrics Collector Example
//
// This example demonstrates using DashMap for high-performance metrics collection
// in a multi-threaded environment, similar to what you'd find in monitoring systems
// like Prometheus or StatsD.

use dashmap::DashMap;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, AtomicI64, Ordering};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};
use std::thread;
use std::collections::HashMap;

#[derive(Debug, Clone)]
enum MetricValue {
    Counter(Arc<AtomicU64>),
    Gauge(Arc<AtomicI64>),
    Histogram(Arc<Histogram>),
    Timer(Arc<Timer>),
}

#[derive(Debug)]
struct Histogram {
    buckets: DashMap<u64, AtomicU64>, // bucket_upper_bound -> count
    sum: AtomicU64,
    count: AtomicU64,
}

impl Histogram {
    fn new(buckets: Vec<u64>) -> Self {
        let bucket_map = DashMap::new();
        for bucket in buckets {
            bucket_map.insert(bucket, AtomicU64::new(0));
        }
        
        Self {
            buckets: bucket_map,
            sum: AtomicU64::new(0),
            count: AtomicU64::new(0),
        }
    }
    
    fn observe(&self, value: u64) {
        self.sum.fetch_add(value, Ordering::Relaxed);
        self.count.fetch_add(1, Ordering::Relaxed);
        
        // Find appropriate bucket
        for bucket in self.buckets.iter() {
            if value <= *bucket.key() {
                bucket.value().fetch_add(1, Ordering::Relaxed);
            }
        }
    }
    
    fn snapshot(&self) -> HistogramSnapshot {
        let mut buckets = HashMap::new();
        for entry in self.buckets.iter() {
            buckets.insert(*entry.key(), entry.value().load(Ordering::Relaxed));
        }
        
        HistogramSnapshot {
            buckets,
            sum: self.sum.load(Ordering::Relaxed),
            count: self.count.load(Ordering::Relaxed),
        }
    }
}

#[derive(Debug)]
struct HistogramSnapshot {
    buckets: HashMap<u64, u64>,
    sum: u64,
    count: u64,
}

impl HistogramSnapshot {
    fn average(&self) -> f64 {
        if self.count > 0 {
            self.sum as f64 / self.count as f64
        } else {
            0.0
        }
    }
    
    fn percentile(&self, p: f64) -> Option<u64> {
        if self.count == 0 {
            return None;
        }
        
        let target_count = (self.count as f64 * p / 100.0) as u64;
        let mut cumulative = 0;
        
        let mut sorted_buckets: Vec<_> = self.buckets.iter().collect();
        sorted_buckets.sort_by_key(|(bucket, _)| *bucket);
        
        for (bucket, count) in sorted_buckets {
            cumulative += count;
            if cumulative >= target_count {
                return Some(*bucket);
            }
        }
        
        None
    }
}

#[derive(Debug)]
struct Timer {
    histogram: Histogram,
}

impl Timer {
    fn new() -> Self {
        // Default buckets for timing (in milliseconds)
        let buckets = vec![1, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000];
        Self {
            histogram: Histogram::new(buckets),
        }
    }
    
    fn time<F, R>(&self, f: F) -> R
    where
        F: FnOnce() -> R,
    {
        let start = Instant::now();
        let result = f();
        let duration = start.elapsed();
        self.histogram.observe(duration.as_millis() as u64);
        result
    }
    
    fn observe_duration(&self, duration: Duration) {
        self.histogram.observe(duration.as_millis() as u64);
    }
    
    fn snapshot(&self) -> HistogramSnapshot {
        self.histogram.snapshot()
    }
}

#[derive(Debug, Clone)]
struct MetricLabels {
    labels: Arc<DashMap<String, String>>,
}

impl MetricLabels {
    fn new() -> Self {
        Self {
            labels: Arc::new(DashMap::new()),
        }
    }
    
    fn with_label(self, key: String, value: String) -> Self {
        self.labels.insert(key, value);
        self
    }
    
    fn to_string(&self) -> String {
        let mut parts: Vec<String> = self.labels.iter()
            .map(|entry| format!("{}={}", entry.key(), entry.value()))
            .collect();
        parts.sort();
        parts.join(",")
    }
}

struct MetricsCollector {
    metrics: Arc<DashMap<String, MetricValue>>,
    start_time: Instant,
}

impl MetricsCollector {
    fn new() -> Self {
        Self {
            metrics: Arc::new(DashMap::new()),
            start_time: Instant::now(),
        }
    }
    
    fn counter(&self, name: &str) -> Counter {
        let key = name.to_string();
        let metric = self.metrics.entry(key.clone())
            .or_insert_with(|| MetricValue::Counter(Arc::new(AtomicU64::new(0))));
        
        if let MetricValue::Counter(counter) = metric.value() {
            Counter {
                name: key,
                value: counter.clone(),
            }
        } else {
            panic!("Metric {} is not a counter", name);
        }
    }
    
    fn gauge(&self, name: &str) -> Gauge {
        let key = name.to_string();
        let metric = self.metrics.entry(key.clone())
            .or_insert_with(|| MetricValue::Gauge(Arc::new(AtomicI64::new(0))));
        
        if let MetricValue::Gauge(gauge) = metric.value() {
            Gauge {
                name: key,
                value: gauge.clone(),
            }
        } else {
            panic!("Metric {} is not a gauge", name);
        }
    }
    
    fn histogram(&self, name: &str, buckets: Vec<u64>) -> HistogramMetric {
        let key = name.to_string();
        let metric = self.metrics.entry(key.clone())
            .or_insert_with(|| MetricValue::Histogram(Arc::new(Histogram::new(buckets))));
        
        if let MetricValue::Histogram(histogram) = metric.value() {
            HistogramMetric {
                name: key,
                histogram: histogram.clone(),
            }
        } else {
            panic!("Metric {} is not a histogram", name);
        }
    }
    
    fn timer(&self, name: &str) -> TimerMetric {
        let key = name.to_string();
        let metric = self.metrics.entry(key.clone())
            .or_insert_with(|| MetricValue::Timer(Arc::new(Timer::new())));
        
        if let MetricValue::Timer(timer) = metric.value() {
            TimerMetric {
                name: key,
                timer: timer.clone(),
            }
        } else {
            panic!("Metric {} is not a timer", name);
        }
    }
    
    fn snapshot(&self) -> MetricsSnapshot {
        let mut counters = HashMap::new();
        let mut gauges = HashMap::new();
        let mut histograms = HashMap::new();
        let mut timers = HashMap::new();
        
        for entry in self.metrics.iter() {
            match entry.value() {
                MetricValue::Counter(counter) => {
                    counters.insert(entry.key().clone(), counter.load(Ordering::Relaxed));
                }
                MetricValue::Gauge(gauge) => {
                    gauges.insert(entry.key().clone(), gauge.load(Ordering::Relaxed));
                }
                MetricValue::Histogram(histogram) => {
                    histograms.insert(entry.key().clone(), histogram.snapshot());
                }
                MetricValue::Timer(timer) => {
                    timers.insert(entry.key().clone(), timer.snapshot());
                }
            }
        }
        
        MetricsSnapshot {
            counters,
            gauges,
            histograms,
            timers,
            uptime: self.start_time.elapsed(),
            timestamp: SystemTime::now().duration_since(UNIX_EPOCH).unwrap(),
        }
    }
    
    fn reset(&self) {
        self.metrics.clear();
    }
}

#[derive(Debug)]
struct MetricsSnapshot {
    counters: HashMap<String, u64>,
    gauges: HashMap<String, i64>,
    histograms: HashMap<String, HistogramSnapshot>,
    timers: HashMap<String, HistogramSnapshot>,
    uptime: Duration,
    timestamp: Duration,
}

impl MetricsSnapshot {
    fn to_prometheus_format(&self) -> String {
        let mut output = String::new();
        
        // Counters
        for (name, value) in &self.counters {
            output.push_str(&format!("# TYPE {} counter\n", name));
            output.push_str(&format!("{} {}\n", name, value));
        }
        
        // Gauges
        for (name, value) in &self.gauges {
            output.push_str(&format!("# TYPE {} gauge\n", name));
            output.push_str(&format!("{} {}\n", name, value));
        }
        
        // Histograms
        for (name, histogram) in &self.histograms {
            output.push_str(&format!("# TYPE {} histogram\n", name));
            
            let mut sorted_buckets: Vec<_> = histogram.buckets.iter().collect();
            sorted_buckets.sort_by_key(|(bucket, _)| *bucket);
            
            for (bucket, count) in sorted_buckets {
                output.push_str(&format!("{}_bucket{{le=\"{}\"}} {}\n", name, bucket, count));
            }
            
            output.push_str(&format!("{}_sum {}\n", name, histogram.sum));
            output.push_str(&format!("{}_count {}\n", name, histogram.count));
        }
        
        // System metrics
        output.push_str(&format!("# TYPE process_uptime_seconds gauge\n"));
        output.push_str(&format!("process_uptime_seconds {}\n", self.uptime.as_secs_f64()));
        
        output
    }
}

struct Counter {
    name: String,
    value: Arc<AtomicU64>,
}

impl Counter {
    fn inc(&self) {
        self.value.fetch_add(1, Ordering::Relaxed);
    }
    
    fn add(&self, value: u64) {
        self.value.fetch_add(value, Ordering::Relaxed);
    }
    
    fn get(&self) -> u64 {
        self.value.load(Ordering::Relaxed)
    }
}

struct Gauge {
    name: String,
    value: Arc<AtomicI64>,
}

impl Gauge {
    fn set(&self, value: i64) {
        self.value.store(value, Ordering::Relaxed);
    }
    
    fn inc(&self) {
        self.value.fetch_add(1, Ordering::Relaxed);
    }
    
    fn dec(&self) {
        self.value.fetch_sub(1, Ordering::Relaxed);
    }
    
    fn add(&self, value: i64) {
        self.value.fetch_add(value, Ordering::Relaxed);
    }
    
    fn get(&self) -> i64 {
        self.value.load(Ordering::Relaxed)
    }
}

struct HistogramMetric {
    name: String,
    histogram: Arc<Histogram>,
}

impl HistogramMetric {
    fn observe(&self, value: u64) {
        self.histogram.observe(value);
    }
}

struct TimerMetric {
    name: String,
    timer: Arc<Timer>,
}

impl TimerMetric {
    fn time<F, R>(&self, f: F) -> R
    where
        F: FnOnce() -> R,
    {
        self.timer.time(f)
    }
    
    fn observe_duration(&self, duration: Duration) {
        self.timer.observe_duration(duration);
    }
}

// Simulate a web service with metrics
fn simulate_web_service(metrics: Arc<MetricsCollector>, thread_id: usize) {
    let request_counter = metrics.counter("http_requests_total");
    let response_time = metrics.timer("http_request_duration_ms");
    let active_connections = metrics.gauge("active_connections");
    let request_size = metrics.histogram("http_request_size_bytes", vec![100, 1000, 10000, 100000]);
    
    for i in 0..100 {
        // Simulate incoming connection
        active_connections.inc();
        
        // Simulate request processing
        let processing_time = response_time.time(|| {
            // Simulate variable processing time
            let delay = Duration::from_millis(10 + (i % 50));
            thread::sleep(delay);
            
            // Simulate request size
            let size = 100 + (i * 137) % 50000; // Pseudo-random size
            request_size.observe(size as u64);
            
            format!("Response from thread {} request {}", thread_id, i)
        });
        
        // Record request
        request_counter.inc();
        
        // Simulate connection closing
        active_connections.dec();
        
        println!("Thread {}: Processed request {} in {:?}", 
                thread_id, i, processing_time);
    }
}

fn main() {
    println!("DashMap Metrics Collector Example");
    println!("=================================");
    
    let metrics = Arc::new(MetricsCollector::new());
    
    // Create some initial metrics
    let startup_counter = metrics.counter("application_starts_total");
    startup_counter.inc();
    
    let memory_gauge = metrics.gauge("memory_usage_bytes");
    memory_gauge.set(1024 * 1024 * 64); // 64MB
    
    // Simulate multiple web service threads
    let mut handles = Vec::new();
    
    for thread_id in 0..4 {
        let metrics = metrics.clone();
        let handle = thread::spawn(move || {
            simulate_web_service(metrics, thread_id);
        });
        handles.push(handle);
    }
    
    // Monitor metrics in background
    let monitor_metrics = metrics.clone();
    let monitor_handle = thread::spawn(move || {
        for i in 0..20 {
            thread::sleep(Duration::from_millis(500));
            
            let snapshot = monitor_metrics.snapshot();
            println!("\n--- Metrics Snapshot {} ---", i + 1);
            
            if let Some(requests) = snapshot.counters.get("http_requests_total") {
                println!("Total HTTP requests: {}", requests);
            }
            
            if let Some(connections) = snapshot.gauges.get("active_connections") {
                println!("Active connections: {}", connections);
            }
            
            if let Some(response_times) = snapshot.timers.get("http_request_duration_ms") {
                println!("Response time stats:");
                println!("  Average: {:.2}ms", response_times.average());
                println!("  P50: {:?}ms", response_times.percentile(50.0));
                println!("  P95: {:?}ms", response_times.percentile(95.0));
                println!("  P99: {:?}ms", response_times.percentile(99.0));
            }
            
            if let Some(request_sizes) = snapshot.histograms.get("http_request_size_bytes") {
                println!("Request size stats:");
                println!("  Average: {:.2} bytes", request_sizes.average());
                println!("  Total requests: {}", request_sizes.count);
            }
        }
    });
    
    // Wait for all service threads to complete
    for handle in handles {
        handle.join().unwrap();
    }
    
    // Stop monitoring
    monitor_handle.join().unwrap();
    
    // Final metrics report
    println!("\n=== Final Metrics Report ===");
    let final_snapshot = metrics.snapshot();
    
    println!("Uptime: {:.2} seconds", final_snapshot.uptime.as_secs_f64());
    println!("Metrics collected: {}", metrics.metrics.len());
    
    // Print Prometheus format
    println!("\n=== Prometheus Format ===");
    println!("{}", final_snapshot.to_prometheus_format());
    
    // Demonstrate metric aggregation
    println!("\n=== Metric Aggregation Example ===");
    
    let aggregator = MetricsCollector::new();
    
    // Simulate collecting metrics from multiple sources
    let sources = ["web-1", "web-2", "web-3", "api-1", "api-2"];
    
    for source in &sources {
        let requests = aggregator.counter(&format!("requests_by_service_{}", source));
        let errors = aggregator.counter(&format!("errors_by_service_{}", source));
        
        // Simulate different load patterns
        match *source {
            s if s.starts_with("web") => {
                requests.add(1000 + (s.len() as u64 * 100));
                errors.add(5 + s.len() as u64);
            }
            s if s.starts_with("api") => {
                requests.add(500 + (s.len() as u64 * 50));
                errors.add(2 + s.len() as u64);
            }
            _ => {}
        }
    }
    
    let aggregated_snapshot = aggregator.snapshot();
    println!("Service metrics:");
    for (name, value) in &aggregated_snapshot.counters {
        println!("  {}: {}", name, value);
    }
    
    // Calculate error rates
    println!("\nError rates:");
    for source in &sources {
        let requests_key = format!("requests_by_service_{}", source);
        let errors_key = format!("errors_by_service_{}", source);
        
        if let (Some(requests), Some(errors)) = (
            aggregated_snapshot.counters.get(&requests_key),
            aggregated_snapshot.counters.get(&errors_key)
        ) {
            let error_rate = (*errors as f64 / *requests as f64) * 100.0;
            println!("  {}: {:.2}%", source, error_rate);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_counter_operations() {
        let metrics = MetricsCollector::new();
        let counter = metrics.counter("test_counter");
        
        assert_eq!(counter.get(), 0);
        
        counter.inc();
        assert_eq!(counter.get(), 1);
        
        counter.add(5);
        assert_eq!(counter.get(), 6);
    }
    
    #[test]
    fn test_gauge_operations() {
        let metrics = MetricsCollector::new();
        let gauge = metrics.gauge("test_gauge");
        
        assert_eq!(gauge.get(), 0);
        
        gauge.set(42);
        assert_eq!(gauge.get(), 42);
        
        gauge.inc();
        assert_eq!(gauge.get(), 43);
        
        gauge.dec();
        assert_eq!(gauge.get(), 42);
        
        gauge.add(-10);
        assert_eq!(gauge.get(), 32);
    }
    
    #[test]
    fn test_histogram_operations() {
        let metrics = MetricsCollector::new();
        let histogram = metrics.histogram("test_histogram", vec![10, 50, 100]);
        
        histogram.observe(5);
        histogram.observe(25);
        histogram.observe(75);
        histogram.observe(150);
        
        let snapshot = metrics.snapshot();
        let hist_snapshot = snapshot.histograms.get("test_histogram").unwrap();
        
        assert_eq!(hist_snapshot.count, 4);
        assert_eq!(hist_snapshot.sum, 255);
        assert_eq!(hist_snapshot.average(), 63.75);
    }
    
    #[test]
    fn test_concurrent_metrics() {
        let metrics = Arc::new(MetricsCollector::new());
        let mut handles = Vec::new();
        
        for i in 0..10 {
            let metrics = metrics.clone();
            let handle = thread::spawn(move || {
                let counter = metrics.counter("concurrent_counter");
                for _ in 0..100 {
                    counter.inc();
                }
            });
            handles.push(handle);
        }
        
        for handle in handles {
            handle.join().unwrap();
        }
        
        let snapshot = metrics.snapshot();
        assert_eq!(snapshot.counters.get("concurrent_counter"), Some(&1000));
    }
}