# DashMap 6.0 Documentation

Comprehensive documentation for DashMap - a blazingly fast concurrent HashMap for Rust.

## Table of Contents

1. [Getting Started](./getting-started.md) - Quick introduction and basic usage
2. [Architecture](./architecture.md) - Shard-based design and internal structure
3. [Performance](./performance.md) - Benchmarks and performance characteristics
4. [API Reference](./api-reference.md) - Complete API documentation with examples
5. [Iteration and Entry API](./iteration-entry.md) - Advanced iteration and entry patterns
6. [Comparison with RwLock<HashMap>](./comparison.md) - When to use DashMap vs alternatives
7. [Memory Ordering](./memory-ordering.md) - Safety guarantees and memory model
8. [Best Practices](./best-practices.md) - Patterns for optimal concurrent access
9. [Examples](./examples/) - Practical examples and use cases

## Quick Start

```rust
use dashmap::DashMap;
use std::sync::Arc;

// Create a new DashMap
let map = Arc::new(DashMap::new());

// Insert values from multiple threads
let map_clone = map.clone();
std::thread::spawn(move || {
    map_clone.insert("key1", "value1");
});

// Read values concurrently
if let Some(value) = map.get("key1") {
    println!("Found: {}", *value);
}
```

## Key Features

- **Lock-free reads**: Multiple readers can access different shards simultaneously
- **Shard-based design**: Reduces contention through intelligent partitioning
- **Drop-in replacement**: Similar API to `std::collections::HashMap`
- **Memory efficient**: Optimized memory layout with cache-friendly design
- **Thread-safe**: All operations are safe for concurrent access

## Version Information

This documentation covers DashMap version 6.0.x, which includes:
- Improved shard allocation strategy
- Enhanced memory ordering guarantees
- Better performance under high contention
- Refined API with additional safety features

## License

DashMap is licensed under the MIT License.