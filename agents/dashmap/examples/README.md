# DashMap Examples

This directory contains practical examples demonstrating various DashMap usage patterns and real-world applications.

## Examples Overview

### Basic Usage
- [**basic_operations.rs**](./basic_operations.rs) - Fundamental operations (insert, get, remove)
- [**entry_api.rs**](./entry_api.rs) - Advanced entry API patterns
- [**iteration.rs**](./iteration.rs) - Different iteration approaches

### Concurrent Patterns
- [**web_server_cache.rs**](./web_server_cache.rs) - HTTP response caching
- [**session_store.rs**](./session_store.rs) - User session management
- [**metrics_collector.rs**](./metrics_collector.rs) - Real-time metrics aggregation
- [**producer_consumer.rs**](./producer_consumer.rs) - Producer-consumer pattern

### Performance Optimization
- [**shard_tuning.rs**](./shard_tuning.rs) - Optimal shard configuration
- [**memory_management.rs**](./memory_management.rs) - Memory usage optimization
- [**parallel_processing.rs**](./parallel_processing.rs) - Rayon integration

### Advanced Use Cases
- [**lru_cache.rs**](./lru_cache.rs) - LRU cache implementation
- [**rate_limiter.rs**](./rate_limiter.rs) - Token bucket rate limiter
- [**distributed_counter.rs**](./distributed_counter.rs) - Distributed counting
- [**configuration_manager.rs**](./configuration_manager.rs) - Dynamic configuration

### Testing and Benchmarking
- [**stress_test.rs**](./stress_test.rs) - Concurrent stress testing
- [**benchmark.rs**](./benchmark.rs) - Performance benchmarking
- [**memory_leak_test.rs**](./memory_leak_test.rs) - Memory safety verification

## Running Examples

Each example is a standalone Rust file that can be run with:

```bash
# Basic example
cargo run --example basic_operations

# With features
cargo run --example parallel_processing --features rayon

# With optimizations
cargo run --example benchmark --release
```

## Example Categories

### 🚀 Getting Started
Perfect for newcomers to DashMap:
- `basic_operations.rs` - Start here
- `entry_api.rs` - Learn advanced patterns
- `iteration.rs` - Understand iteration

### 🌐 Web Applications
Real-world web server patterns:
- `web_server_cache.rs` - Response caching
- `session_store.rs` - Session management
- `rate_limiter.rs` - Request rate limiting

### 📊 Data Processing
High-performance data handling:
- `metrics_collector.rs` - Real-time analytics
- `parallel_processing.rs` - Multi-core processing
- `distributed_counter.rs` - Distributed systems

### ⚡ Performance
Optimization techniques:
- `shard_tuning.rs` - Configuration tuning
- `memory_management.rs` - Memory optimization
- `benchmark.rs` - Performance measurement

### 🔧 Advanced
Complex patterns and edge cases:
- `lru_cache.rs` - Cache eviction policies
- `configuration_manager.rs` - Dynamic config
- `stress_test.rs` - Robustness testing

## Common Patterns

### Thread-Safe Counters
```rust
use dashmap::DashMap;
use std::sync::Arc;

let counters = Arc::new(DashMap::new());

// Atomic increment
counters.entry("page_views".to_string())
    .and_modify(|v| *v += 1)
    .or_insert(1);
```

### Caching with TTL
```rust
use dashmap::DashMap;
use std::time::{Duration, Instant};

struct CacheEntry<T> {
    value: T,
    expires_at: Instant,
}

let cache = DashMap::new();
let ttl = Duration::from_secs(300); // 5 minutes

cache.insert("key", CacheEntry {
    value: "cached_data",
    expires_at: Instant::now() + ttl,
});
```

### Producer-Consumer Queue
```rust
use dashmap::DashMap;
use std::sync::Arc;

let queue = Arc::new(DashMap::new());

// Producer
queue.insert(task_id, task_data);

// Consumer
if let Some((id, data)) = queue.iter().next() {
    let id = *id.key();
    queue.remove(&id);
    process_task(data.value());
}
```

## Best Practices Demonstrated

1. **Proper Error Handling** - All examples show robust error handling
2. **Memory Management** - Efficient memory usage patterns
3. **Concurrency Safety** - Thread-safe access patterns
4. **Performance Optimization** - Tuning for different workloads
5. **Testing Strategies** - Comprehensive testing approaches

## Contributing Examples

When adding new examples:

1. **Clear Documentation** - Explain the use case and approach
2. **Complete Code** - Include all necessary imports and setup
3. **Error Handling** - Show proper error handling patterns
4. **Comments** - Explain non-obvious code sections
5. **Performance Notes** - Include performance considerations

## Dependencies

Most examples use only `dashmap` and standard library. Some examples require additional dependencies:

- `rayon` - For parallel processing examples
- `tokio` - For async examples
- `serde` - For serialization examples
- `criterion` - For benchmarking examples

Check individual example files for specific requirements.