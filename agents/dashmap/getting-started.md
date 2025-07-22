# Getting Started with DashMap

DashMap is a concurrent HashMap implementation that provides thread-safe access without requiring external synchronization. This guide will help you get started with the basics.

## Installation

Add DashMap to your `Cargo.toml`:

```toml
[dependencies]
dashmap = "6.0"
```

### Optional Features

```toml
[dependencies]
dashmap = { version = "6.0", features = ["serde", "rayon"] }
```

Available features:
- `serde` - Serialization support
- `rayon` - Parallel iteration support
- `raw-api` - Access to internal shard API (unstable)
- `inline` - More aggressive inlining for performance

## Basic Usage

### Creating a DashMap

```rust
use dashmap::DashMap;

// Create with default settings
let map = DashMap::new();

// Create with initial capacity
let map = DashMap::with_capacity(1000);

// Create with custom shard count (must be power of 2)
let map = DashMap::with_shard_amount(32);
```

### Inserting and Retrieving Values

```rust
use dashmap::DashMap;

let map = DashMap::new();

// Insert a key-value pair
map.insert("key1", "value1");

// Get a value (returns Option<Ref<K, V>>)
if let Some(value) = map.get("key1") {
    println!("Value: {}", *value);
}

// Get a mutable reference
if let Some(mut value) = map.get_mut("key1") {
    *value = "new_value";
}
```

### Thread Safety

DashMap can be safely shared between threads:

```rust
use dashmap::DashMap;
use std::sync::Arc;
use std::thread;

let map = Arc::new(DashMap::new());

// Spawn multiple threads
let handles: Vec<_> = (0..4).map(|i| {
    let map = map.clone();
    thread::spawn(move || {
        // Each thread can safely insert
        map.insert(format!("key{}", i), i * 10);
        
        // And read from the map
        for j in 0..4 {
            if let Some(value) = map.get(&format!("key{}", j)) {
                println!("Thread {} found key{}: {}", i, j, *value);
            }
        }
    })
}).collect();

// Wait for all threads
for handle in handles {
    handle.join().unwrap();
}

// Check final state
println!("Final map size: {}", map.len());
```

### Entry API

The entry API provides atomic operations for complex logic:

```rust
use dashmap::DashMap;

let map = DashMap::new();

// Insert if not present, or modify if present
map.entry("counter".to_string())
    .and_modify(|v| *v += 1)
    .or_insert(1);

// Always insert a value
let mut entry = map.entry("key".to_string()).or_insert(42);
*entry += 1;
```

### Iteration

DashMap supports safe iteration even during concurrent modifications:

```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("a", 1);
map.insert("b", 2);
map.insert("c", 3);

// Iterate over key-value pairs
for entry in &map {
    println!("{}: {}", entry.key(), entry.value());
}

// Mutable iteration
for mut entry in map.iter_mut() {
    *entry.value_mut() *= 2;
}
```

## Common Patterns

### Caching

```rust
use dashmap::DashMap;
use std::sync::Arc;

struct Cache {
    data: Arc<DashMap<String, String>>,
}

impl Cache {
    fn new() -> Self {
        Self {
            data: Arc::new(DashMap::new()),
        }
    }
    
    fn get_or_compute(&self, key: &str, compute: impl FnOnce() -> String) -> String {
        self.data.entry(key.to_string())
            .or_insert_with(compute)
            .clone()
    }
}
```

### Counters

```rust
use dashmap::DashMap;
use std::sync::Arc;

let counters = Arc::new(DashMap::new());

// Increment counter atomically
fn increment_counter(counters: &DashMap<String, u64>, key: &str) {
    counters.entry(key.to_string())
        .and_modify(|v| *v += 1)
        .or_insert(1);
}
```

### Producer-Consumer

```rust
use dashmap::DashMap;
use std::sync::Arc;
use std::thread;
use std::time::Duration;

let shared_data = Arc::new(DashMap::new());

// Producer thread
let producer_data = shared_data.clone();
let producer = thread::spawn(move || {
    for i in 0..100 {
        producer_data.insert(i, format!("value_{}", i));
        thread::sleep(Duration::from_millis(10));
    }
});

// Consumer thread
let consumer_data = shared_data.clone();
let consumer = thread::spawn(move || {
    loop {
        let current_size = consumer_data.len();
        if current_size > 0 {
            println!("Current items: {}", current_size);
        }
        
        if current_size >= 100 {
            break;
        }
        thread::sleep(Duration::from_millis(50));
    }
});

producer.join().unwrap();
consumer.join().unwrap();
```

## Error Handling

DashMap operations are generally infallible, but some patterns to be aware of:

```rust
use dashmap::DashMap;

let map = DashMap::new();

// get() returns Option
match map.get("key") {
    Some(value) => println!("Found: {}", *value),
    None => println!("Key not found"),
}

// remove() returns Option<(K, V)>
match map.remove("key") {
    Some((key, value)) => println!("Removed {}: {}", key, value),
    None => println!("Key was not present"),
}

// try_get() can return TryResult::Locked if shard is busy
use dashmap::try_result::TryResult;

match map.try_get("key") {
    TryResult::Present(value) => println!("Found: {}", *value),
    TryResult::Absent => println!("Key not found"),
    TryResult::Locked => println!("Shard is locked, try again"),
}
```

## Performance Tips

1. **Choose appropriate shard count**: Default is usually good, but for very high contention, consider more shards
2. **Use `try_get` for non-blocking access**: When you can't afford to wait for locks
3. **Batch operations**: Group related operations together when possible
4. **Consider read-only views**: Use `into_read_only()` for read-heavy workloads

## Next Steps

- Learn about the [shard-based architecture](./architecture.md)
- Explore [performance characteristics](./performance.md)
- Check out the [complete API reference](./api-reference.md)
- See [best practices](./best-practices.md) for optimal usage