# Ordered Maps with IndexMap

## Table of Contents

1. [Introduction](#introduction)
2. [Core Concepts](#core-concepts)
3. [Basic Usage](#basic-usage)
4. [Performance Characteristics](#performance-characteristics)
5. [Memory Layout](#memory-layout)
6. [Advanced Features](#advanced-features)
7. [Optimization Strategies](#optimization-strategies)
8. [Integration Patterns](#integration-patterns)
9. [Benchmarking](#benchmarking)
10. [Best Practices](#best-practices)

## Introduction

`IndexMap` is a hash table where the iteration order of key-value pairs is independent of hash values and matches insertion order. It combines the fast lookup of `HashMap` with the predictable iteration order of `BTreeMap`, making it ideal for applications where both performance and deterministic behavior are important.

### Key Benefits

- **Insertion order preservation**: Iteration order matches insertion order
- **Fast lookups**: O(1) average case like HashMap
- **Stable iteration**: Deterministic iteration order across runs
- **Memory efficiency**: Compact storage with good cache locality
- **Rich API**: Additional operations like indexing and sorting

### When to Use IndexMap

✅ **Ideal for:**
- Configuration maps where order matters
- Symbol tables in compilers
- JSON-like data structures
- APIs requiring stable output
- Caches with predictable eviction order

❌ **Consider alternatives when:**
- Order doesn't matter (use HashMap)
- Need range queries (use BTreeMap)
- Memory is extremely constrained
- Only need insertion order (consider Vec of pairs)

## Core Concepts

### Insertion Order Preservation

```rust
use indexmap::IndexMap;

let mut map = IndexMap::new();
map.insert("third", 3);
map.insert("first", 1);
map.insert("second", 2);

// Iteration follows insertion order, not key order
for (key, value) in &map {
    println!("{}: {}", key, value);
}
// Output: third: 3, first: 1, second: 2
```

### Index-Based Access

```rust
use indexmap::IndexMap;

let mut map = IndexMap::new();
map.insert("a", 1);
map.insert("b", 2);
map.insert("c", 3);

// Access by index (insertion order)
assert_eq!(map.get_index(0), Some((&"a", &1)));
assert_eq!(map.get_index(1), Some((&"b", &2)));
assert_eq!(map.get_index(2), Some((&"c", &3)));

// Get index of key
assert_eq!(map.get_index_of("b"), Some(1));
```

### Dual Storage Strategy

IndexMap uses two main data structures:
1. **Hash table**: For fast key lookups (maps key → index)
2. **Dense array**: For ordered storage of key-value pairs

```rust
use indexmap::IndexMap;

// Internal structure (conceptual):
// hash_table: HashMap<KeyHash, usize>  // key hash → index
// entries: Vec<Bucket<K, V>>           // ordered key-value pairs
```

## Basic Usage

### Creation and Initialization

```rust
use indexmap::{IndexMap, indexmap};

// Empty map
let mut map = IndexMap::new();

// Using macro (like hashmap!)
let map2 = indexmap! {
    "key1" => "value1",
    "key2" => "value2",
    "key3" => "value3",
};

// With capacity
let map3 = IndexMap::with_capacity(100);

// With custom hasher
use std::collections::hash_map::RandomState;
let map4: IndexMap<String, i32, RandomState> = IndexMap::default();
```

### Basic Operations

```rust
use indexmap::IndexMap;

let mut map = IndexMap::new();

// Insert
map.insert("key1", 100);
map.insert("key2", 200);

// Get
let value = map.get("key1");
assert_eq!(value, Some(&100));

// Contains
assert!(map.contains_key("key1"));

// Remove
let removed = map.remove("key1");
assert_eq!(removed, Some(100));

// Length
assert_eq!(map.len(), 1);
```

### Iteration

```rust
use indexmap::IndexMap;

let map = indexmap! {
    "a" => 1,
    "b" => 2,
    "c" => 3,
};

// Iterate in insertion order
for (key, value) in &map {
    println!("{}: {}", key, value);
}

// Iterate keys only
for key in map.keys() {
    println!("Key: {}", key);
}

// Iterate values only
for value in map.values() {
    println!("Value: {}", value);
}

// Enumerate with index
for (index, (key, value)) in map.iter().enumerate() {
    println!("{}: {} => {}", index, key, value);
}
```

## Performance Characteristics

### Time Complexity

| Operation | Average Case | Worst Case |
|-----------|--------------|------------|
| Insert | O(1) | O(n) |
| Get | O(1) | O(n) |
| Remove | O(1) | O(n) |
| Index access | O(1) | O(1) |
| Iteration | O(n) | O(n) |

### Lookup Performance

```rust
use indexmap::IndexMap;
use std::collections::HashMap;
use std::time::Instant;

fn benchmark_lookups() {
    let size = 100_000;
    
    // Create IndexMap
    let mut index_map = IndexMap::new();
    for i in 0..size {
        index_map.insert(i, i * 2);
    }
    
    // Create HashMap for comparison
    let mut hash_map = HashMap::new();
    for i in 0..size {
        hash_map.insert(i, i * 2);
    }
    
    // Benchmark lookups
    let keys: Vec<i32> = (0..1000).map(|_| fastrand::i32(0..size)).collect();
    
    let start = Instant::now();
    for &key in &keys {
        let _ = index_map.get(&key);
    }
    let index_time = start.elapsed();
    
    let start = Instant::now();
    for &key in &keys {
        let _ = hash_map.get(&key);
    }
    let hash_time = start.elapsed();
    
    println!("IndexMap lookups: {:?}", index_time);
    println!("HashMap lookups: {:?}", hash_time);
    println!("Overhead: {:.2}%", 
        (index_time.as_nanos() as f64 / hash_time.as_nanos() as f64 - 1.0) * 100.0);
}
```

### Iteration Performance

```rust
use indexmap::IndexMap;
use std::collections::HashMap;
use std::time::Instant;

fn benchmark_iteration() {
    let size = 100_000;
    
    let mut index_map = IndexMap::new();
    let mut hash_map = HashMap::new();
    
    for i in 0..size {
        index_map.insert(i, i * 2);
        hash_map.insert(i, i * 2);
    }
    
    // Benchmark iteration
    let start = Instant::now();
    let index_sum: i32 = index_map.values().sum();
    let index_time = start.elapsed();
    
    let start = Instant::now();
    let hash_sum: i32 = hash_map.values().sum();
    let hash_time = start.elapsed();
    
    println!("IndexMap iteration: {:?}", index_time);
    println!("HashMap iteration: {:?}", hash_time);
    println!("IndexMap is {:.2}x faster", 
        hash_time.as_nanos() as f64 / index_time.as_nanos() as f64);
}
```

### Memory Usage

```rust
use indexmap::IndexMap;
use std::collections::HashMap;
use std::mem;

fn analyze_memory_usage() {
    let mut index_map = IndexMap::new();
    let mut hash_map = HashMap::new();
    
    // Add same data to both
    for i in 0..1000 {
        index_map.insert(i, i.to_string());
        hash_map.insert(i, i.to_string());
    }
    
    println!("IndexMap size: {} bytes", mem::size_of_val(&index_map));
    println!("HashMap size: {} bytes", mem::size_of_val(&hash_map));
    
    // IndexMap typically uses less memory due to:
    // 1. Dense storage of entries
    // 2. No per-entry overhead for ordering
    // 3. Better cache locality
}
```

## Memory Layout

### Internal Structure

```rust
use indexmap::IndexMap;

// IndexMap internal layout (simplified):
// 
// struct IndexMap<K, V, S> {
//     core: IndexMapCore<K, V>,  // Main data structure
//     hash_builder: S,           // Hash function builder
// }
//
// struct IndexMapCore<K, V> {
//     indices: RawTable<usize>,  // Hash table: key hash → entry index
//     entries: Vec<Bucket<K, V>>, // Dense array of key-value pairs
// }
//
// struct Bucket<K, V> {
//     hash: HashValue,  // Cached hash value
//     key: K,          // Key
//     value: V,        // Value
// }
```

### Memory Efficiency

```rust
use indexmap::IndexMap;
use std::mem;

fn demonstrate_memory_layout() {
    type Map = IndexMap<i32, String>;
    
    println!("IndexMap overhead: {} bytes", mem::size_of::<Map>());
    
    let mut map = IndexMap::new();
    
    // Each entry stores:
    // - Hash value (for fast rehashing)
    // - Key
    // - Value
    // No additional pointers or metadata per entry
    
    map.insert(1, "one".to_string());
    map.insert(2, "two".to_string());
    
    // Dense storage means good cache locality
    // Iteration touches consecutive memory
}
```

### Cache Locality

```rust
use indexmap::IndexMap;
use std::time::Instant;

fn demonstrate_cache_locality() {
    let size = 10_000;
    
    // IndexMap: entries stored densely
    let mut index_map = IndexMap::new();
    for i in 0..size {
        index_map.insert(i, i * 2);
    }
    
    // Measure cache-friendly iteration
    let start = Instant::now();
    let mut sum = 0;
    for _ in 0..100 {
        sum += index_map.values().sum::<i32>();
    }
    let time = start.elapsed();
    
    println!("Cache-friendly iteration: {:?}", time);
    println!("Sum: {}", sum); // Prevent optimization
}
```

## Advanced Features

### Index-Based Operations

```rust
use indexmap::IndexMap;

let mut map = indexmap! {
    "first" => 1,
    "second" => 2,
    "third" => 3,
};

// Get by index
let (key, value) = map.get_index(1).unwrap();
assert_eq!(key, &"second");
assert_eq!(value, &2);

// Get mutable by index
let (key, value) = map.get_index_mut(1).unwrap();
*value = 20;

// Remove by index
let (key, value) = map.remove_index(0).unwrap();
assert_eq!(key, "first");
assert_eq!(value, 1);

// Swap indices
map.swap_indices(0, 1);
```

### Sorting Operations

```rust
use indexmap::IndexMap;

let mut map = indexmap! {
    "zebra" => 26,
    "apple" => 1,
    "banana" => 2,
};

// Sort by keys
map.sort_keys();
// Now: apple, banana, zebra

// Sort by values
map.sort_by(|_k1, v1, _k2, v2| v1.cmp(v2));

// Sort with custom comparator
map.sort_by(|k1, _v1, k2, _v2| k1.len().cmp(&k2.len()));

// Reverse order
map.reverse();
```

### Entry API

```rust
use indexmap::{IndexMap, map::Entry};

let mut map = IndexMap::new();

// Entry API for complex insertion logic
match map.entry("key") {
    Entry::Occupied(mut entry) => {
        *entry.get_mut() += 1;
    }
    Entry::Vacant(entry) => {
        entry.insert(1);
    }
}

// Or more concisely
*map.entry("key").or_insert(0) += 1;

// With index information
let entry = map.entry("new_key");
let index = entry.index();
entry.or_insert(42);
```

### Slicing and Ranges

```rust
use indexmap::IndexMap;

let map = indexmap! {
    "a" => 1, "b" => 2, "c" => 3, "d" => 4, "e" => 5
};

// Get slice of entries
let slice = map.get_range(1..4).unwrap();
for (key, value) in slice {
    println!("{}: {}", key, value);
}
// Output: b: 2, c: 3, d: 4

// Mutable slice
let mut map = map;
let slice_mut = map.get_range_mut(1..3).unwrap();
for (_key, value) in slice_mut {
    *value *= 10;
}
```

### Equivalent Keys

```rust
use indexmap::IndexMap;
use indexmap::Equivalent;

// Custom equivalent implementation
#[derive(Hash, PartialEq, Eq)]
struct CaseInsensitive(String);

impl Equivalent<CaseInsensitive> for str {
    fn equivalent(&self, key: &CaseInsensitive) -> bool {
        self.to_lowercase() == key.0.to_lowercase()
    }
}

let mut map = IndexMap::new();
map.insert(CaseInsensitive("Hello".to_string()), 1);

// Can lookup with &str using Equivalent
assert_eq!(map.get("hello"), Some(&1));
assert_eq!(map.get("HELLO"), Some(&1));
```

## Optimization Strategies

### Choosing the Right Hasher

```rust
use indexmap::IndexMap;
use fnv::FnvBuildHasher;
use std::collections::hash_map::RandomState;

// Default hasher (cryptographically secure)
type SecureMap<K, V> = IndexMap<K, V, RandomState>;

// Fast hasher for non-adversarial inputs
type FastMap<K, V> = IndexMap<K, V, FnvBuildHasher>;

fn benchmark_hashers() {
    let data: Vec<(i32, String)> = (0..10000)
        .map(|i| (i, format!("value_{}", i)))
        .collect();
    
    // Benchmark with default hasher
    let start = std::time::Instant::now();
    let mut secure_map = SecureMap::new();
    for (k, v) in &data {
        secure_map.insert(*k, v.clone());
    }
    let secure_time = start.elapsed();
    
    // Benchmark with FNV hasher
    let start = std::time::Instant::now();
    let mut fast_map = FastMap::default();
    for (k, v) in &data {
        fast_map.insert(*k, v.clone());
    }
    let fast_time = start.elapsed();
    
    println!("Secure hasher: {:?}", secure_time);
    println!("Fast hasher: {:?}", fast_time);
    println!("Speedup: {:.2}x", 
        secure_time.as_nanos() as f64 / fast_time.as_nanos() as f64);
}
```

### Capacity Management

```rust
use indexmap::IndexMap;

// Pre-allocate for known sizes
let mut map = IndexMap::with_capacity(1000);

// Avoid repeated reallocations
for i in 0..1000 {
    map.insert(i, i.to_string());
}

// Reserve additional capacity
map.reserve(500);

// Shrink to fit actual size
map.shrink_to_fit();

// Check capacity
println!("Capacity: {}, Length: {}", map.capacity(), map.len());
```

### Bulk Operations

```rust
use indexmap::IndexMap;

let mut map = IndexMap::new();

// Extend from iterator
let data = (0..1000).map(|i| (i, i.to_string()));
map.extend(data);

// Retain elements
map.retain(|&k, _v| k % 2 == 0);

// Drain range
let drained: IndexMap<_, _> = map.drain(100..200).collect();

// Clear while preserving capacity
map.clear();
```

### Memory-Efficient Patterns

```rust
use indexmap::IndexMap;

// Use references when possible to avoid cloning
fn process_map(map: &IndexMap<String, Vec<i32>>) {
    for (key, values) in map {
        // Process without cloning
        let sum: i32 = values.iter().sum();
        println!("{}: {}", key, sum);
    }
}

// Use entry API to avoid double lookups
fn increment_counter(map: &mut IndexMap<String, i32>, key: &str) {
    *map.entry(key.to_string()).or_insert(0) += 1;
}

// Batch operations for efficiency
fn batch_insert(map: &mut IndexMap<i32, String>, items: Vec<(i32, String)>) {
    map.reserve(items.len());
    map.extend(items);
}
```

## Integration Patterns

### Compiler Symbol Tables

```rust
use indexmap::IndexMap;

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub type_info: TypeInfo,
    pub scope_level: usize,
    pub location: SourceLocation,
}

pub struct SymbolTable {
    // Preserve declaration order for error messages
    symbols: IndexMap<String, Symbol>,
    scope_stack: Vec<usize>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: IndexMap::new(),
            scope_stack: vec![0],
        }
    }
    
    pub fn declare(&mut self, name: String, symbol: Symbol) -> Result<(), String> {
        if self.symbols.contains_key(&name) {
            return Err(format!("Symbol '{}' already declared", name));
        }
        
        self.symbols.insert(name, symbol);
        Ok(())
    }
    
    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }
    
    // Iterate in declaration order for diagnostics
    pub fn declared_symbols(&self) -> impl Iterator<Item = (&String, &Symbol)> {
        self.symbols.iter()
    }
}
```

### Configuration Management

```rust
use indexmap::IndexMap;
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct Config {
    // Preserve order for config file output
    #[serde(with = "indexmap::serde_seq")]
    pub settings: IndexMap<String, ConfigValue>,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(untagged)]
pub enum ConfigValue {
    String(String),
    Number(i64),
    Boolean(bool),
    Array(Vec<ConfigValue>),
    Object(IndexMap<String, ConfigValue>),
}

impl Config {
    pub fn load_from_file(path: &str) -> Result<Self, Box<dyn std::error::Error>> {
        let content = std::fs::read_to_string(path)?;
        let config: Config = toml::from_str(&content)?;
        Ok(config)
    }
    
    pub fn save_to_file(&self, path: &str) -> Result<(), Box<dyn std::error::Error>> {
        let content = toml::to_string_pretty(self)?;
        std::fs::write(path, content)?;
        Ok(())
    }
    
    // Maintain insertion order when merging configs
    pub fn merge(&mut self, other: Config) {
        for (key, value) in other.settings {
            self.settings.insert(key, value);
        }
    }
}
```

### AST Node Attributes

```rust
use indexmap::IndexMap;

#[derive(Debug)]
pub struct AstNode {
    pub kind: NodeKind,
    pub children: Vec<AstNode>,
    // Preserve attribute order for code generation
    pub attributes: IndexMap<String, AttributeValue>,
    pub span: SourceSpan,
}

impl AstNode {
    pub fn new(kind: NodeKind) -> Self {
        Self {
            kind,
            children: Vec::new(),
            attributes: IndexMap::new(),
            span: SourceSpan::default(),
        }
    }
    
    pub fn with_attribute(mut self, name: String, value: AttributeValue) -> Self {
        self.attributes.insert(name, value);
        self
    }
    
    // Generate code with attributes in declaration order
    pub fn generate_code(&self, output: &mut String) {
        for (name, value) in &self.attributes {
            writeln!(output, "@{} = {}", name, value).unwrap();
        }
        
        // Generate node content...
    }
}
```

### Caching with LRU Behavior

```rust
use indexmap::IndexMap;

pub struct LruCache<K, V> {
    map: IndexMap<K, V>,
    capacity: usize,
}

impl<K, V> LruCache<K, V> 
where 
    K: std::hash::Hash + Eq + Clone,
{
    pub fn new(capacity: usize) -> Self {
        Self {
            map: IndexMap::with_capacity(capacity),
            capacity,
        }
    }
    
    pub fn get(&mut self, key: &K) -> Option<&V> {
        if let Some(value) = self.map.remove(key) {
            // Move to end (most recently used)
            self.map.insert(key.clone(), value);
            self.map.get(key)
        } else {
            None
        }
    }
    
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        if let Some(old_value) = self.map.remove(&key) {
            // Update existing key
            self.map.insert(key, value);
            Some(old_value)
        } else {
            // New key
            if self.map.len() >= self.capacity {
                // Remove least recently used (first item)
                self.map.remove_index(0);
            }
            self.map.insert(key, value);
            None
        }
    }
}
```

## Benchmarking

### Comprehensive Performance Suite

```rust
use indexmap::IndexMap;
use std::collections::{HashMap, BTreeMap};
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn bench_insertion(c: &mut Criterion) {
    let data: Vec<(i32, String)> = (0..1000)
        .map(|i| (i, format!("value_{}", i)))
        .collect();
    
    c.bench_function("indexmap_insert", |b| {
        b.iter(|| {
            let mut map = IndexMap::new();
            for (k, v) in &data {
                map.insert(black_box(*k), black_box(v.clone()));
            }
            map
        })
    });
    
    c.bench_function("hashmap_insert", |b| {
        b.iter(|| {
            let mut map = HashMap::new();
            for (k, v) in &data {
                map.insert(black_box(*k), black_box(v.clone()));
            }
            map
        })
    });
    
    c.bench_function("btreemap_insert", |b| {
        b.iter(|| {
            let mut map = BTreeMap::new();
            for (k, v) in &data {
                map.insert(black_box(*k), black_box(v.clone()));
            }
            map
        })
    });
}

fn bench_lookup(c: &mut Criterion) {
    let mut index_map = IndexMap::new();
    let mut hash_map = HashMap::new();
    let mut btree_map = BTreeMap::new();
    
    for i in 0..1000 {
        let value = format!("value_{}", i);
        index_map.insert(i, value.clone());
        hash_map.insert(i, value.clone());
        btree_map.insert(i, value);
    }
    
    let keys: Vec<i32> = (0..100).map(|_| fastrand::i32(0..1000)).collect();
    
    c.bench_function("indexmap_lookup", |b| {
        b.iter(|| {
            for &key in &keys {
                black_box(index_map.get(&key));
            }
        })
    });
    
    c.bench_function("hashmap_lookup", |b| {
        b.iter(|| {
            for &key in &keys {
                black_box(hash_map.get(&key));
            }
        })
    });
    
    c.bench_function("btreemap_lookup", |b| {
        b.iter(|| {
            for &key in &keys {
                black_box(btree_map.get(&key));
            }
        })
    });
}

fn bench_iteration(c: &mut Criterion) {
    let mut index_map = IndexMap::new();
    let mut hash_map = HashMap::new();
    let mut btree_map = BTreeMap::new();
    
    for i in 0..1000 {
        index_map.insert(i, i * 2);
        hash_map.insert(i, i * 2);
        btree_map.insert(i, i * 2);
    }
    
    c.bench_function("indexmap_iter", |b| {
        b.iter(|| {
            let sum: i32 = index_map.values().sum();
            black_box(sum)
        })
    });
    
    c.bench_function("hashmap_iter", |b| {
        b.iter(|| {
            let sum: i32 = hash_map.values().sum();
            black_box(sum)
        })
    });
    
    c.bench_function("btreemap_iter", |b| {
        b.iter(|| {
            let sum: i32 = btree_map.values().sum();
            black_box(sum)
        })
    });
}

criterion_group!(benches, bench_insertion, bench_lookup, bench_iteration);
criterion_main!(benches);
```

### Memory Usage Analysis

```rust
use indexmap::IndexMap;
use std::collections::{HashMap, BTreeMap};

fn analyze_memory_overhead() {
    let sizes = [10, 100, 1000, 10000];
    
    for &size in &sizes {
        let mut index_map = IndexMap::new();
        let mut hash_map = HashMap::new();
        let mut btree_map = BTreeMap::new();
        
        for i in 0..size {
            let value = format!("value_{}", i);
            index_map.insert(i, value.clone());
            hash_map.insert(i, value.clone());
            btree_map.insert(i, value);
        }
        
        println!("Size: {}", size);
        println!("  IndexMap capacity: {}", index_map.capacity());
        println!("  HashMap capacity: {}", hash_map.capacity());
        println!("  BTreeMap len: {}", btree_map.len());
        
        // Estimate memory usage
        let index_overhead = index_map.capacity() * std::mem::size_of::<(i32, String)>();
        let hash_overhead = hash_map.capacity() * std::mem::size_of::<(i32, String)>();
        
        println!("  IndexMap estimated overhead: {} bytes", index_overhead);
        println!("  HashMap estimated overhead: {} bytes", hash_overhead);
        println!();
    }
}
```

## Best Practices

### 1. Choose IndexMap When Order Matters

```rust
use indexmap::IndexMap;
use std::collections::HashMap;

// ✅ Use IndexMap for stable iteration order
fn generate_config_file(settings: &IndexMap<String, String>) -> String {
    let mut output = String::new();
    for (key, value) in settings {
        output.push_str(&format!("{}={}\n", key, value));
    }
    output // Always same order
}

// ❌ HashMap gives unpredictable order
fn unstable_config(settings: &HashMap<String, String>) -> String {
    let mut output = String::new();
    for (key, value) in settings {
        output.push_str(&format!("{}={}\n", key, value));
    }
    output // Order varies between runs
}
```

### 2. Leverage Index-Based Operations

```rust
use indexmap::IndexMap;

// ✅ Use index operations for ordered access
fn process_in_order(map: &mut IndexMap<String, i32>) {
    // Process first item specially
    if let Some((key, value)) = map.get_index_mut(0) {
        *value *= 2;
        println!("First item: {} = {}", key, value);
    }
    
    // Process remaining items
    for i in 1..map.len() {
        if let Some((key, value)) = map.get_index_mut(i) {
            *value += 1;
        }
    }
}

// ✅ Use sorting when needed
fn sort_by_priority(map: &mut IndexMap<String, Task>) {
    map.sort_by(|_k1, v1, _k2, v2| v1.priority.cmp(&v2.priority));
}
```

### 3. Optimize for Your Use Case

```rust
use indexmap::IndexMap;
use fnv::FnvBuildHasher;

// ✅ Use fast hasher for trusted input
type FastIndexMap<K, V> = IndexMap<K, V, FnvBuildHasher>;

// ✅ Pre-allocate when size is known
fn build_symbol_table(symbols: &[Symbol]) -> IndexMap<String, Symbol> {
    let mut table = IndexMap::with_capacity(symbols.len());
    for symbol in symbols {
        table.insert(symbol.name.clone(), symbol.clone());
    }
    table
}

// ✅ Use entry API to avoid double lookups
fn update_counter(counters: &mut IndexMap<String, i32>, key: &str) {
    *counters.entry(key.to_string()).or_insert(0) += 1;
}
```

### 4. Handle Serialization Properly

```rust
use indexmap::IndexMap;
use serde::{Serialize, Deserialize};

// ✅ Preserve order in serialization
#[derive(Serialize, Deserialize)]
struct Config {
    #[serde(with = "indexmap::serde_seq")]
    settings: IndexMap<String, String>,
}

// ✅ Or use default serialization (as object)
#[derive(Serialize, Deserialize)]
struct Metadata {
    properties: IndexMap<String, serde_json::Value>,
}

fn test_serialization() {
    let mut config = Config {
        settings: IndexMap::new(),
    };
    
    config.settings.insert("debug".to_string(), "true".to_string());
    config.settings.insert("optimize".to_string(), "false".to_string());
    
    // JSON will preserve insertion order
    let json = serde_json::to_string_pretty(&config).unwrap();
    println!("{}", json);
}
```

### 5. Memory Management

```rust
use indexmap::IndexMap;

// ✅ Shrink when appropriate
fn cleanup_cache(cache: &mut IndexMap<String, CachedData>) {
    // Remove old entries
    cache.retain(|_k, v| !v.is_expired());
    
    // Shrink to fit if significantly smaller
    if cache.len() < cache.capacity() / 2 {
        cache.shrink_to_fit();
    }
}

// ✅ Use drain for bulk operations
fn process_batch(map: &mut IndexMap<String, Task>) -> Vec<TaskResult> {
    let batch_size = 100;
    let tasks: IndexMap<_, _> = map.drain(..batch_size.min(map.len())).collect();
    
    tasks.into_iter()
        .map(|(key, task)| process_task(key, task))
        .collect()
}
```

---

This comprehensive guide covers the essential aspects of using IndexMap for ordered hash table operations in Rust applications. IndexMap is particularly valuable in compiler implementations like Parallax, where maintaining insertion order for symbol tables, configuration maps, and other data structures is crucial for deterministic behavior and debugging.