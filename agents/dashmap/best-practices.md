# Best Practices for DashMap

This guide provides comprehensive best practices for using DashMap effectively in concurrent applications, covering performance optimization, safety patterns, and common pitfalls.

## Performance Best Practices

### 1. Optimal Shard Configuration

#### Choose the Right Shard Count

```rust
use dashmap::DashMap;

// Default: Good for most cases
let map = DashMap::new(); // 4 × CPU cores, power of 2

// High contention workloads: More shards
let cores = std::thread::available_parallelism().unwrap().get();
let high_contention_map = DashMap::with_shard_amount(cores * 8);

// Low contention workloads: Fewer shards (less memory overhead)
let low_contention_map = DashMap::with_shard_amount(cores * 2);

// Memory-constrained environments: Minimal shards
let minimal_map = DashMap::with_shard_amount(8);
```

#### Shard Count Guidelines

| Scenario | Recommended Shards | Reasoning |
|----------|-------------------|-----------|
| Web server (high read) | CPU cores × 4-8 | Maximize read parallelism |
| Database cache | CPU cores × 2-4 | Balance memory and performance |
| Real-time analytics | CPU cores × 6-8 | Handle burst writes |
| Embedded systems | 8-16 | Minimize memory overhead |
| Single-threaded + occasional sharing | 4-8 | Minimal overhead |

### 2. Key Distribution Optimization

#### Ensure Good Hash Distribution

```rust
use dashmap::DashMap;
use std::hash::{Hash, Hasher};

// Bad: Sequential keys may cluster in same shards
let map = DashMap::new();
for i in 0..1000 {
    map.insert(i, i); // May cause hot shards
}

// Better: String keys with good distribution
for i in 0..1000 {
    map.insert(format!("user_{}", i), i);
}

// Best: Custom key type with good hash distribution
#[derive(Hash, Eq, PartialEq, Clone)]
struct OptimalKey {
    id: u64,
    category: String,
    timestamp: u64,
}

let map = DashMap::new();
for i in 0..1000 {
    let key = OptimalKey {
        id: i,
        category: format!("cat_{}", i % 10),
        timestamp: i * 1000,
    };
    map.insert(key, i);
}
```

#### Test Hash Distribution

```rust
use dashmap::DashMap;
use std::collections::HashMap;

fn analyze_distribution<K: Hash + Eq + Clone>(
    map: &DashMap<K, i32>,
    keys: &[K]
) -> HashMap<usize, usize> {
    let mut shard_counts = HashMap::new();
    
    for key in keys {
        let shard = map.determine_shard(map.hash_usize(key));
        *shard_counts.entry(shard).or_insert(0) += 1;
    }
    
    shard_counts
}

// Usage
let map = DashMap::with_shard_amount(16);
let keys: Vec<String> = (0..1000).map(|i| format!("key_{}", i)).collect();

let distribution = analyze_distribution(&map, &keys);
println!("Shard distribution: {:?}", distribution);

// Good distribution: roughly equal counts across shards
// Bad distribution: some shards have much more than others
```

### 3. Memory Management

#### Pre-allocate Capacity

```rust
use dashmap::DashMap;

// Bad: Frequent reallocations during growth
let map = DashMap::new();
for i in 0..1_000_000 {
    map.insert(i, i); // Causes multiple reallocations
}

// Good: Pre-allocate expected capacity
let map = DashMap::with_capacity(1_000_000);
for i in 0..1_000_000 {
    map.insert(i, i); // No reallocations needed
}
```

#### Monitor Memory Usage

```rust
use dashmap::DashMap;

fn monitor_memory_usage<K, V>(map: &DashMap<K, V>) {
    let len = map.len();
    let capacity = map.capacity();
    let load_factor = len as f64 / capacity as f64;
    
    println!("Entries: {}, Capacity: {}, Load factor: {:.2}", 
             len, capacity, load_factor);
    
    // Optimal load factor: 0.7-0.8
    if load_factor > 0.9 {
        println!("Warning: High load factor, consider pre-allocation");
    }
}

let map = DashMap::new();
// ... populate map ...
monitor_memory_usage(&map);
```

#### Shrink When Appropriate

```rust
use dashmap::DashMap;

let map = DashMap::with_capacity(1_000_000);

// Populate with data
for i in 0..1_000_000 {
    map.insert(i, i);
}

// Remove most entries
for i in 0..900_000 {
    map.remove(&i);
}

// Shrink to reclaim memory
map.shrink_to_fit();

println!("After shrinking - Len: {}, Capacity: {}", map.len(), map.capacity());
```

### 4. Hasher Selection

#### Choose Appropriate Hasher

```rust
use dashmap::DashMap;
use std::collections::hash_map::RandomState;

// Default (SipHash): Cryptographically secure, slower
let secure_map = DashMap::with_hasher(RandomState::new());

// AHash: Fast, good distribution, not cryptographically secure
use ahash::RandomState as AHashState;
let fast_map = DashMap::with_hasher(AHashState::new());

// FxHash: Very fast, poor security, good for trusted inputs
use rustc_hash::FxHasher;
use std::hash::BuildHasherDefault;
let fx_map = DashMap::with_hasher(BuildHasherDefault::<FxHasher>::default());
```

#### Hasher Performance Comparison

```rust
use dashmap::DashMap;
use std::time::Instant;

fn benchmark_hasher<S: std::hash::BuildHasher + Clone>(
    hasher: S,
    name: &str
) -> std::time::Duration {
    let map = DashMap::with_hasher(hasher);
    
    let start = Instant::now();
    for i in 0..100_000 {
        map.insert(format!("key_{}", i), i);
    }
    for i in 0..100_000 {
        map.get(&format!("key_{}", i));
    }
    start.elapsed()
}

// Compare different hashers
let sip_time = benchmark_hasher(
    std::collections::hash_map::RandomState::new(), 
    "SipHash"
);
let ahash_time = benchmark_hasher(
    ahash::RandomState::new(), 
    "AHash"
);

println!("SipHash: {:?}, AHash: {:?}", sip_time, ahash_time);
```

## Concurrency Best Practices

### 1. Minimize Lock Scope

#### Avoid Long-Running Operations Under Locks

```rust
use dashmap::DashMap;

let map = DashMap::new();

// Bad: Long operation while holding reference
for entry in &map {
    expensive_computation(*entry.value()); // Blocks other operations
}

// Good: Extract data first, then process
let data: Vec<i32> = map.iter().map(|entry| *entry.value()).collect();
for value in data {
    expensive_computation(value); // No locks held
}

fn expensive_computation(x: i32) {
    std::thread::sleep(std::time::Duration::from_millis(1));
}
```

#### Use Non-Blocking Operations When Possible

```rust
use dashmap::{DashMap, try_result::TryResult};

let map = DashMap::new();

// Blocking operation
let value = map.get("key"); // May wait for lock

// Non-blocking alternative
match map.try_get("key") {
    TryResult::Present(value) => {
        println!("Found: {}", *value);
    }
    TryResult::Absent => {
        println!("Key not found");
    }
    TryResult::Locked => {
        println!("Shard busy, try again later");
        // Handle gracefully - retry, use cached value, etc.
    }
}
```

### 2. Batch Operations

#### Group Related Operations

```rust
use dashmap::DashMap;

let map = DashMap::new();

// Bad: Many individual operations
for i in 0..1000 {
    map.entry(format!("counter_{}", i % 10))
        .and_modify(|v| *v += 1)
        .or_insert(1);
}

// Better: Batch by expected shard
use std::collections::HashMap;

let mut batches: HashMap<usize, Vec<(String, i32)>> = HashMap::new();
for i in 0..1000 {
    let key = format!("counter_{}", i % 10);
    let shard = map.determine_shard(map.hash_usize(&key));
    batches.entry(shard).or_default().push((key, 1));
}

// Process each batch
for (shard, operations) in batches {
    for (key, value) in operations {
        map.entry(key)
            .and_modify(|v| *v += value)
            .or_insert(value);
    }
}
```

#### Atomic Multi-Step Operations

```rust
use dashmap::DashMap;

let map = DashMap::new();

// Atomic transfer between accounts
fn transfer_funds(
    accounts: &DashMap<String, i64>,
    from: &str,
    to: &str,
    amount: i64
) -> Result<(), String> {
    // Use entry API for atomicity within single key
    match accounts.entry(from.to_string()) {
        dashmap::mapref::entry::Entry::Occupied(mut from_entry) => {
            if *from_entry.get() >= amount {
                *from_entry.get_mut() -= amount;
                
                // Second operation
                accounts.entry(to.to_string())
                    .and_modify(|balance| *balance += amount)
                    .or_insert(amount);
                
                Ok(())
            } else {
                Err("Insufficient funds".to_string())
            }
        }
        dashmap::mapref::entry::Entry::Vacant(_) => {
            Err("Account not found".to_string())
        }
    }
}

// Note: This is not atomic across both accounts
// For true atomicity across multiple keys, use external coordination
```

### 3. Read-Heavy Optimizations

#### Use Read-Only Views

```rust
use dashmap::DashMap;

let map = DashMap::new();

// Populate data
for i in 0..10_000 {
    map.insert(i, i * 2);
}

// Convert to read-only for better read performance
let read_only = map.into_read_only();

// Read-only operations are more efficient
for i in 0..10_000 {
    if let Some(value) = read_only.get(&i) {
        println!("Value: {}", value);
    }
}
```

#### Parallel Iteration for Read-Heavy Workloads

```rust
use dashmap::DashMap;
use rayon::prelude::*;

let map = DashMap::new();

// Populate with data
for i in 0..1_000_000 {
    map.insert(i, i * i);
}

// Parallel read processing
let sum: i64 = map.par_iter()
    .map(|entry| *entry.value() as i64)
    .sum();

println!("Sum: {}", sum);
```

## Safety Best Practices

### 1. Avoid Deadlocks

#### Never Hold Multiple References Across Operations

```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("key1", 1);
map.insert("key2", 2);

// Bad: Potential deadlock
let _ref1 = map.get("key1");
let _ref2 = map.get("key2"); // May deadlock if different thread holds these in reverse order

// Good: Release references quickly
{
    let ref1 = map.get("key1").unwrap();
    println!("Value 1: {}", *ref1);
} // ref1 released

{
    let ref2 = map.get("key2").unwrap();
    println!("Value 2: {}", *ref2);
} // ref2 released
```

#### Consistent Lock Ordering

```rust
use dashmap::DashMap;

let map = DashMap::new();

// When you must hold multiple references, use consistent ordering
fn safe_multi_access(map: &DashMap<String, i32>, key1: &str, key2: &str) {
    // Always access keys in lexicographic order to prevent deadlocks
    let (first_key, second_key) = if key1 < key2 {
        (key1, key2)
    } else {
        (key2, key1)
    };
    
    let first_ref = map.get(first_key);
    let second_ref = map.get(second_key);
    
    // Process both references
    if let (Some(first), Some(second)) = (first_ref, second_ref) {
        println!("Values: {}, {}", *first, *second);
    }
}
```

### 2. Handle Contention Gracefully

#### Implement Retry Logic

```rust
use dashmap::{DashMap, try_result::TryResult};
use std::time::Duration;

fn resilient_get<K, V>(
    map: &DashMap<K, V>,
    key: &K,
    max_retries: usize
) -> Option<V>
where
    K: std::hash::Hash + Eq,
    V: Clone,
{
    for attempt in 0..max_retries {
        match map.try_get(key) {
            TryResult::Present(value) => return Some(value.clone()),
            TryResult::Absent => return None,
            TryResult::Locked => {
                if attempt < max_retries - 1 {
                    // Exponential backoff
                    let delay = Duration::from_micros(10 * (1 << attempt));
                    std::thread::sleep(delay);
                }
            }
        }
    }
    
    // Fallback to blocking operation
    map.get(key).map(|v| v.clone())
}
```

#### Circuit Breaker Pattern

```rust
use dashmap::DashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::{Duration, Instant};

struct CircuitBreaker {
    failure_count: AtomicUsize,
    last_failure: std::sync::Mutex<Option<Instant>>,
    threshold: usize,
    timeout: Duration,
}

impl CircuitBreaker {
    fn new(threshold: usize, timeout: Duration) -> Self {
        Self {
            failure_count: AtomicUsize::new(0),
            last_failure: std::sync::Mutex::new(None),
            threshold,
            timeout,
        }
    }
    
    fn call<T>(&self, operation: impl FnOnce() -> Result<T, ()>) -> Result<T, ()> {
        // Check if circuit is open
        if self.failure_count.load(Ordering::Relaxed) >= self.threshold {
            let last_failure = self.last_failure.lock().unwrap();
            if let Some(last) = *last_failure {
                if last.elapsed() < self.timeout {
                    return Err(()); // Circuit open
                }
            }
        }
        
        // Try operation
        match operation() {
            Ok(result) => {
                self.failure_count.store(0, Ordering::Relaxed);
                Ok(result)
            }
            Err(e) => {
                self.failure_count.fetch_add(1, Ordering::Relaxed);
                *self.last_failure.lock().unwrap() = Some(Instant::now());
                Err(e)
            }
        }
    }
}

// Usage with DashMap
fn protected_map_access(
    map: &DashMap<String, String>,
    key: &str,
    circuit_breaker: &CircuitBreaker
) -> Result<Option<String>, ()> {
    circuit_breaker.call(|| {
        match map.try_get(key) {
            dashmap::try_result::TryResult::Present(value) => Ok(Some(value.clone())),
            dashmap::try_result::TryResult::Absent => Ok(None),
            dashmap::try_result::TryResult::Locked => Err(()),
        }
    })
}
```

### 3. Memory Safety Patterns

#### Safe Reference Management

```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("key", vec![1, 2, 3]);

// Safe: Extract data instead of holding references
let data = map.get("key").map(|v| v.clone());
if let Some(vec) = data {
    for item in vec {
        println!("Item: {}", item);
    }
}

// Safe: Use scoped access
map.view("key", |_key, value| {
    println!("Length: {}", value.len());
    // Reference automatically released at end of closure
});
```

#### Avoid Reference Leaks

```rust
use dashmap::DashMap;
use std::sync::Arc;

// Bad: Storing references can lead to issues
struct BadContainer {
    // Don't store DashMap references
    // reference: dashmap::mapref::one::Ref<'static, String, i32>,
}

// Good: Store the map itself or cloned values
struct GoodContainer {
    map: Arc<DashMap<String, i32>>,
    cached_value: Option<i32>,
}

impl GoodContainer {
    fn get_value(&self, key: &str) -> Option<i32> {
        self.map.get(key).map(|v| *v)
    }
    
    fn cache_value(&mut self, key: &str) {
        self.cached_value = self.get_value(key);
    }
}
```

## Error Handling Best Practices

### 1. Graceful Degradation

#### Handle Lock Contention

```rust
use dashmap::{DashMap, try_result::TryResult};

struct ResilientCache {
    primary: DashMap<String, String>,
    fallback: std::sync::RwLock<std::collections::HashMap<String, String>>,
}

impl ResilientCache {
    fn get(&self, key: &str) -> Option<String> {
        // Try primary cache first
        match self.primary.try_get(key) {
            TryResult::Present(value) => Some(value.clone()),
            TryResult::Absent => None,
            TryResult::Locked => {
                // Fallback to secondary cache
                self.fallback.read().ok()?.get(key).cloned()
            }
        }
    }
    
    fn insert(&self, key: String, value: String) {
        // Always try primary first
        self.primary.insert(key.clone(), value.clone());
        
        // Update fallback if possible
        if let Ok(mut fallback) = self.fallback.try_write() {
            fallback.insert(key, value);
        }
    }
}
```

### 2. Monitoring and Observability

#### Performance Metrics

```rust
use dashmap::DashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Instant;

struct InstrumentedMap<K, V> {
    map: DashMap<K, V>,
    read_count: AtomicU64,
    write_count: AtomicU64,
    contention_count: AtomicU64,
}

impl<K, V> InstrumentedMap<K, V>
where
    K: std::hash::Hash + Eq + Clone,
    V: Clone,
{
    fn new() -> Self {
        Self {
            map: DashMap::new(),
            read_count: AtomicU64::new(0),
            write_count: AtomicU64::new(0),
            contention_count: AtomicU64::new(0),
        }
    }
    
    fn get(&self, key: &K) -> Option<V> {
        self.read_count.fetch_add(1, Ordering::Relaxed);
        
        match self.map.try_get(key) {
            dashmap::try_result::TryResult::Present(value) => Some(value.clone()),
            dashmap::try_result::TryResult::Absent => None,
            dashmap::try_result::TryResult::Locked => {
                self.contention_count.fetch_add(1, Ordering::Relaxed);
                self.map.get(key).map(|v| v.clone())
            }
        }
    }
    
    fn insert(&self, key: K, value: V) {
        self.write_count.fetch_add(1, Ordering::Relaxed);
        self.map.insert(key, value);
    }
    
    fn stats(&self) -> (u64, u64, u64) {
        (
            self.read_count.load(Ordering::Relaxed),
            self.write_count.load(Ordering::Relaxed),
            self.contention_count.load(Ordering::Relaxed),
        )
    }
}
```

## Common Anti-Patterns to Avoid

### 1. Inefficient Patterns

#### Don't Use DashMap as a Mutex Replacement

```rust
use dashmap::DashMap;

// Bad: Using DashMap to protect single values
let protected_value = DashMap::new();
protected_value.insert("value", 42);

// To modify:
protected_value.entry("value".to_string())
    .and_modify(|v| *v += 1);

// Good: Use appropriate synchronization primitives
use std::sync::atomic::{AtomicI32, Ordering};
let atomic_value = AtomicI32::new(42);
atomic_value.fetch_add(1, Ordering::Relaxed);
```

#### Don't Hold References During Blocking Operations

```rust
use dashmap::DashMap;

let map = DashMap::new();

// Bad: Holding reference during I/O
let value_ref = map.get("key").unwrap();
let result = std::fs::read_to_string("file.txt"); // Blocks other operations
println!("Value: {}, File: {:?}", *value_ref, result);

// Good: Extract value first
let value = map.get("key").map(|v| *v).unwrap();
let result = std::fs::read_to_string("file.txt");
println!("Value: {}, File: {:?}", value, result);
```

### 2. Memory Leaks

#### Don't Create Circular References

```rust
use dashmap::DashMap;
use std::sync::Arc;

// Bad: Potential circular reference
struct Node {
    id: u32,
    children: DashMap<u32, Arc<Node>>,
    parent: Option<Arc<Node>>, // Circular reference risk
}

// Good: Use weak references or IDs
use std::sync::Weak;

struct SafeNode {
    id: u32,
    children: DashMap<u32, Arc<SafeNode>>,
    parent: Option<Weak<SafeNode>>, // Weak reference breaks cycles
}
```

### 3. Performance Anti-Patterns

#### Don't Use String Keys Unnecessarily

```rust
use dashmap::DashMap;

// Bad: String keys when integers would work
let map: DashMap<String, i32> = DashMap::new();
for i in 0..1000 {
    map.insert(i.to_string(), i); // Unnecessary allocation
}

// Good: Use appropriate key types
let map: DashMap<u32, i32> = DashMap::new();
for i in 0..1000 {
    map.insert(i, i); // No allocation
}
```

## Testing Best Practices

### 1. Concurrent Testing

```rust
use dashmap::DashMap;
use std::sync::Arc;
use std::thread;

#[test]
fn test_concurrent_access() {
    let map = Arc::new(DashMap::new());
    let num_threads = 8;
    let operations_per_thread = 1000;
    
    let handles: Vec<_> = (0..num_threads).map(|thread_id| {
        let map = map.clone();
        thread::spawn(move || {
            for i in 0..operations_per_thread {
                let key = thread_id * operations_per_thread + i;
                
                // Insert
                map.insert(key, thread_id);
                
                // Read
                assert_eq!(map.get(&key).map(|v| *v), Some(thread_id));
                
                // Update
                map.entry(key).and_modify(|v| *v += 1);
                
                // Remove
                if i % 10 == 0 {
                    map.remove(&key);
                }
            }
        })
    }).collect();
    
    for handle in handles {
        handle.join().unwrap();
    }
    
    // Verify final state
    println!("Final map size: {}", map.len());
}
```

### 2. Property-Based Testing

```rust
use dashmap::DashMap;
use std::collections::HashSet;

#[test]
fn test_insertion_consistency() {
    let map = DashMap::new();
    let mut expected_keys = HashSet::new();
    
    // Insert known keys
    for i in 0..1000 {
        map.insert(i, i * 2);
        expected_keys.insert(i);
    }
    
    // Verify all keys are present
    let mut actual_keys = HashSet::new();
    for entry in &map {
        actual_keys.insert(*entry.key());
        assert_eq!(*entry.value(), *entry.key() * 2);
    }
    
    assert_eq!(expected_keys, actual_keys);
}
```

## Summary

Following these best practices will help you:

1. **Maximize Performance**: Through proper shard configuration, key distribution, and memory management
2. **Ensure Safety**: By avoiding deadlocks, handling contention gracefully, and managing references properly
3. **Maintain Reliability**: Through proper error handling, monitoring, and testing
4. **Avoid Common Pitfalls**: By understanding anti-patterns and their solutions

DashMap is a powerful tool for concurrent programming, but like any concurrent data structure, it requires careful consideration of access patterns, performance characteristics, and safety requirements to use effectively.