# Iteration and Entry API

DashMap provides sophisticated iteration and entry APIs that enable safe concurrent access patterns while maintaining performance and ergonomics.

## Iteration Overview

DashMap supports multiple iteration patterns, each optimized for different use cases:

1. **Immutable iteration** - Read-only access to key-value pairs
2. **Mutable iteration** - Modify values during iteration
3. **Owning iteration** - Consume the map and iterate over owned values
4. **Parallel iteration** - Leverage multiple cores with Rayon (optional)

## Immutable Iteration

### Basic Iteration

```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("key1", 10);
map.insert("key2", 20);
map.insert("key3", 30);

// Iterate over key-value pairs
for entry in &map {
    println!("{}: {}", entry.key(), entry.value());
}

// Alternative syntax
for entry in map.iter() {
    println!("{}: {}", entry.key(), entry.value());
}
```

### Reference Types

The iterator returns `RefMulti<K, V>` which provides:

```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("hello", "world");

for entry in &map {
    let key: &str = entry.key();           // Immutable reference to key
    let value: &str = entry.value();       // Immutable reference to value
    let pair: (&str, &str) = entry.pair(); // Both as tuple
    
    println!("Key: {}, Value: {}", key, value);
}
```

### Concurrent Safety

Iteration is safe even during concurrent modifications:

```rust
use dashmap::DashMap;
use std::sync::Arc;
use std::thread;

let map = Arc::new(DashMap::new());

// Populate initial data
for i in 0..1000 {
    map.insert(i, i * 2);
}

let map_clone = map.clone();

// Spawn a thread that modifies the map
let modifier = thread::spawn(move || {
    for i in 1000..2000 {
        map_clone.insert(i, i * 2);
    }
});

// Iterate while modifications happen
// This is safe and will see a consistent snapshot
for entry in &*map {
    println!("{}: {}", entry.key(), entry.value());
}

modifier.join().unwrap();
```

## Mutable Iteration

### Modifying Values

```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("a", 1);
map.insert("b", 2);
map.insert("c", 3);

// Modify all values during iteration
for mut entry in map.iter_mut() {
    *entry.value_mut() *= 2;
}

// Verify changes
for entry in &map {
    println!("{}: {}", entry.key(), entry.value());
}
// Output: a: 2, b: 4, c: 6
```

### Conditional Modifications

```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("apple", 5);
map.insert("banana", 3);
map.insert("cherry", 8);

// Double values greater than 5
for mut entry in map.iter_mut() {
    if *entry.value() > 5 {
        *entry.value_mut() *= 2;
    }
}
```

### Mutable Reference Types

```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("key", 42);

for mut entry in map.iter_mut() {
    let key: &str = entry.key();              // Immutable key reference
    let value: &mut i32 = entry.value_mut();  // Mutable value reference
    let pair: (&str, &mut i32) = entry.pair_mut(); // Mixed tuple
    
    *value += 1;
}
```

## Owning Iteration

### Consuming the Map

```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("key1".to_string(), 10);
map.insert("key2".to_string(), 20);

// Consume the map and get owned values
for (key, value) in map {
    println!("Owned: {} -> {}", key, value);
    // key and value are owned String and i32
}

// map is no longer accessible here
```

### Converting to Standard Collections

```rust
use dashmap::DashMap;
use std::collections::HashMap;

let dash_map = DashMap::new();
dash_map.insert("a", 1);
dash_map.insert("b", 2);

// Convert to HashMap
let hash_map: HashMap<&str, i32> = dash_map.into_iter().collect();

// Convert to Vec
let dash_map2 = DashMap::new();
dash_map2.insert("x", 10);
dash_map2.insert("y", 20);

let vec: Vec<(&str, i32)> = dash_map2.into_iter().collect();
```

## Parallel Iteration with Rayon

### Setup

Add Rayon support to your `Cargo.toml`:

```toml
[dependencies]
dashmap = { version = "6.0", features = ["rayon"] }
rayon = "1.7"
```

### Parallel Processing

```rust
use dashmap::DashMap;
use rayon::prelude::*;

let map = DashMap::new();

// Populate with data
for i in 0..1_000_000 {
    map.insert(i, i * i);
}

// Parallel iteration for read-only operations
let sum: i64 = map.par_iter()
    .map(|entry| *entry.value() as i64)
    .sum();

println!("Sum of all values: {}", sum);
```

### Parallel Mutable Operations

```rust
use dashmap::DashMap;
use rayon::prelude::*;

let map = DashMap::new();

for i in 0..100_000 {
    map.insert(i, i);
}

// Parallel mutable iteration
map.par_iter_mut()
    .for_each(|mut entry| {
        *entry.value_mut() *= 2;
    });
```

### Parallel Filtering and Collection

```rust
use dashmap::DashMap;
use rayon::prelude::*;

let map = DashMap::new();

for i in 0..10_000 {
    map.insert(format!("key_{}", i), i);
}

// Parallel filter and collect
let even_values: Vec<i32> = map.par_iter()
    .filter(|entry| *entry.value() % 2 == 0)
    .map(|entry| *entry.value())
    .collect();

println!("Found {} even values", even_values.len());
```

## Entry API

The Entry API provides atomic operations for complex insertion and modification logic.

### Basic Entry Operations

```rust
use dashmap::DashMap;

let map = DashMap::new();

// Insert if not present
map.entry("key".to_string()).or_insert(42);

// Insert with default
map.entry("counter".to_string()).or_default(); // Requires Default trait

// Insert with function
map.entry("computed".to_string()).or_insert_with(|| {
    // Expensive computation only runs if key doesn't exist
    expensive_computation()
});

fn expensive_computation() -> i32 {
    // Simulate expensive work
    std::thread::sleep(std::time::Duration::from_millis(100));
    999
}
```

### Conditional Modifications

```rust
use dashmap::DashMap;

let map = DashMap::new();

// Increment counter, or initialize to 1
map.entry("visits".to_string())
    .and_modify(|v| *v += 1)
    .or_insert(1);

// Complex conditional logic
map.entry("score".to_string())
    .and_modify(|score| {
        if *score < 100 {
            *score += 10;
        }
    })
    .or_insert(10);
```

### Entry Types

#### Occupied Entry

```rust
use dashmap::{DashMap, mapref::entry::Entry};

let map = DashMap::new();
map.insert("existing".to_string(), 42);

match map.entry("existing".to_string()) {
    Entry::Occupied(mut entry) => {
        println!("Key exists with value: {}", entry.get());
        
        // Modify the value
        entry.insert(100);
        
        // Or remove it
        let old_value = entry.remove();
        println!("Removed value: {}", old_value);
    }
    Entry::Vacant(_) => {
        println!("Key doesn't exist");
    }
}
```

#### Vacant Entry

```rust
use dashmap::{DashMap, mapref::entry::Entry};

let map = DashMap::new();

match map.entry("new_key".to_string()) {
    Entry::Vacant(entry) => {
        println!("Inserting new key: {}", entry.key());
        entry.insert(42);
    }
    Entry::Occupied(_) => {
        println!("Key already exists");
    }
}
```

### Advanced Entry Patterns

#### Atomic Increment

```rust
use dashmap::DashMap;

fn atomic_increment(map: &DashMap<String, u64>, key: &str) -> u64 {
    let mut entry = map.entry(key.to_string()).or_insert(0);
    *entry += 1;
    *entry
}

let counters = DashMap::new();
let count = atomic_increment(&counters, "page_views");
println!("Page views: {}", count);
```

#### Conditional Insert

```rust
use dashmap::DashMap;

fn insert_if_better(map: &DashMap<String, i32>, key: String, value: i32) -> bool {
    match map.entry(key) {
        dashmap::mapref::entry::Entry::Vacant(entry) => {
            entry.insert(value);
            true
        }
        dashmap::mapref::entry::Entry::Occupied(mut entry) => {
            if value > *entry.get() {
                entry.insert(value);
                true
            } else {
                false
            }
        }
    }
}

let scores = DashMap::new();
insert_if_better(&scores, "player1".to_string(), 100);
insert_if_better(&scores, "player1".to_string(), 80);  // Won't update
insert_if_better(&scores, "player1".to_string(), 120); // Will update
```

#### Lazy Initialization

```rust
use dashmap::DashMap;
use std::sync::Arc;

struct ExpensiveResource {
    data: Vec<u8>,
}

impl ExpensiveResource {
    fn new() -> Self {
        // Simulate expensive initialization
        std::thread::sleep(std::time::Duration::from_millis(100));
        Self {
            data: vec![0; 1024],
        }
    }
}

fn get_or_create_resource(
    cache: &DashMap<String, Arc<ExpensiveResource>>,
    key: &str
) -> Arc<ExpensiveResource> {
    cache.entry(key.to_string())
        .or_insert_with(|| Arc::new(ExpensiveResource::new()))
        .clone()
}

let resource_cache = DashMap::new();
let resource = get_or_create_resource(&resource_cache, "config");
```

## Iteration Performance Considerations

### Shard-Aware Iteration

DashMap iteration works by acquiring read locks on each shard sequentially:

```rust
// Conceptual implementation
impl<K, V> DashMap<K, V> {
    fn iter(&self) -> impl Iterator<Item = RefMulti<K, V>> {
        self.shards.iter().flat_map(|shard| {
            let guard = shard.read(); // Acquire read lock
            // Iterate over this shard's entries
            shard_iterator(guard)
        })
    }
}
```

### Performance Tips

#### 1. Minimize Iteration Duration

```rust
use dashmap::DashMap;

let map = DashMap::new();

// Good: Quick iteration
let sum: i32 = map.iter().map(|entry| *entry.value()).sum();

// Bad: Long-running iteration blocks writers
for entry in &map {
    expensive_operation(*entry.value()); // Blocks other operations
}

// Better: Collect first, then process
let values: Vec<i32> = map.iter().map(|entry| *entry.value()).collect();
for value in values {
    expensive_operation(value);
}
```

#### 2. Use Parallel Iteration for CPU-Intensive Work

```rust
use dashmap::DashMap;
use rayon::prelude::*;

let map = DashMap::new();

// Sequential: Uses one core
let result1: Vec<i32> = map.iter()
    .map(|entry| expensive_computation(*entry.value()))
    .collect();

// Parallel: Uses all cores
let result2: Vec<i32> = map.par_iter()
    .map(|entry| expensive_computation(*entry.value()))
    .collect();

fn expensive_computation(x: i32) -> i32 {
    // CPU-intensive work
    (0..1000).fold(x, |acc, i| acc + i)
}
```

#### 3. Batch Modifications

```rust
use dashmap::DashMap;

let map = DashMap::new();

// Bad: Many individual entry operations
for i in 0..1000 {
    map.entry(format!("key_{}", i))
        .and_modify(|v| *v += 1)
        .or_insert(1);
}

// Better: Batch collect then modify
let updates: Vec<(String, i32)> = map.iter()
    .map(|entry| (entry.key().clone(), *entry.value() + 1))
    .collect();

for (key, value) in updates {
    map.insert(key, value);
}
```

## Error Handling and Edge Cases

### Handling Lock Contention

```rust
use dashmap::{DashMap, try_result::TryResult};

let map = DashMap::new();

// Non-blocking access during iteration
match map.try_get("key") {
    TryResult::Present(value) => {
        println!("Value: {}", *value);
    }
    TryResult::Absent => {
        println!("Key not found");
    }
    TryResult::Locked => {
        println!("Shard is locked, try again later");
        // Handle contention gracefully
    }
}
```

### Safe Iteration During Modifications

```rust
use dashmap::DashMap;
use std::sync::Arc;
use std::thread;

let map = Arc::new(DashMap::new());

// Initial data
for i in 0..100 {
    map.insert(i, i);
}

let map_clone = map.clone();

// Modifier thread
let modifier = thread::spawn(move || {
    for i in 100..200 {
        map_clone.insert(i, i);
        // Remove some old entries
        if i % 10 == 0 {
            map_clone.remove(&(i - 100));
        }
    }
});

// Iterator sees consistent snapshot per shard
// New insertions may or may not be visible
// Removed entries remain visible if iteration started before removal
for entry in &*map {
    println!("{}: {}", entry.key(), entry.value());
}

modifier.join().unwrap();
```

## Best Practices

### 1. Choose the Right Iteration Type

```rust
use dashmap::DashMap;

let map = DashMap::new();

// For read-only access: use immutable iteration
for entry in &map {
    process_readonly(entry.key(), entry.value());
}

// For value modifications: use mutable iteration
for mut entry in map.iter_mut() {
    if should_modify(entry.key()) {
        *entry.value_mut() = new_value();
    }
}

// For consuming the map: use owning iteration
for (key, value) in map {
    transfer_ownership(key, value);
}

fn process_readonly(_key: &i32, _value: &i32) {}
fn should_modify(_key: &i32) -> bool { true }
fn new_value() -> i32 { 42 }
fn transfer_ownership(_key: i32, _value: i32) {}
```

### 2. Leverage Entry API for Atomic Operations

```rust
use dashmap::DashMap;

let map = DashMap::new();

// Atomic increment
fn increment_counter(map: &DashMap<String, u64>, key: &str) {
    map.entry(key.to_string())
        .and_modify(|v| *v += 1)
        .or_insert(1);
}

// Atomic max update
fn update_max(map: &DashMap<String, i32>, key: &str, value: i32) {
    map.entry(key.to_string())
        .and_modify(|v| *v = (*v).max(value))
        .or_insert(value);
}
```

### 3. Use Parallel Iteration Appropriately

```rust
use dashmap::DashMap;
use rayon::prelude::*;

let map = DashMap::new();

// Good: CPU-intensive work benefits from parallelism
let results: Vec<_> = map.par_iter()
    .map(|entry| cpu_intensive_work(*entry.value()))
    .collect();

// Bad: I/O-bound work doesn't benefit much
let _: Vec<_> = map.par_iter()
    .map(|entry| {
        // Network I/O - parallelism overhead not worth it
        fetch_from_network(entry.key())
    })
    .collect();

fn cpu_intensive_work(x: i32) -> i32 { x * x }
fn fetch_from_network(_key: &i32) -> String { "data".to_string() }
```

The iteration and entry APIs in DashMap provide powerful tools for safe, efficient concurrent programming while maintaining the familiar patterns from standard Rust collections.