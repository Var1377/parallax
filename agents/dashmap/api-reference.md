# DashMap API Reference

Complete API reference for DashMap 6.0 with examples and usage patterns.

## Table of Contents

1. [Core Types](#core-types)
2. [Construction](#construction)
3. [Basic Operations](#basic-operations)
4. [Entry API](#entry-api)
5. [Iteration](#iteration)
6. [Utility Methods](#utility-methods)
7. [Advanced Features](#advanced-features)
8. [Type Aliases](#type-aliases)

## Core Types

### DashMap<K, V, S>

The main concurrent hash map type.

```rust
pub struct DashMap<K, V, S = RandomState> {
    // Internal fields are private
}
```

**Type Parameters:**
- `K`: Key type (must implement `Eq + Hash`)
- `V`: Value type
- `S`: Hash builder type (defaults to `RandomState`)

**Trait Implementations:**
- `Clone` (when `K: Clone, V: Clone, S: Clone`)
- `Debug` (when `K: Debug, V: Debug`)
- `Default` (when `S: Default + BuildHasher + Clone`)
- `IntoIterator`
- `Extend<(K, V)>`
- `FromIterator<(K, V)>`

## Construction

### new()

Creates a new empty DashMap with default configuration.

```rust
pub fn new() -> DashMap<K, V, RandomState>
where
    K: Eq + Hash,
```

**Example:**
```rust
use dashmap::DashMap;

let map: DashMap<String, i32> = DashMap::new();
```

### with_capacity(capacity)

Creates a new DashMap with specified initial capacity.

```rust
pub fn with_capacity(capacity: usize) -> DashMap<K, V, RandomState>
where
    K: Eq + Hash,
```

**Parameters:**
- `capacity`: Initial capacity hint

**Example:**
```rust
use dashmap::DashMap;

let map: DashMap<String, i32> = DashMap::with_capacity(1000);
```

### with_shard_amount(shard_amount)

Creates a new DashMap with specified number of shards.

```rust
pub fn with_shard_amount(shard_amount: usize) -> DashMap<K, V, RandomState>
where
    K: Eq + Hash,
```

**Parameters:**
- `shard_amount`: Number of shards (must be power of 2, > 1)

**Panics:** If `shard_amount` is not a power of 2 or ≤ 1

**Example:**
```rust
use dashmap::DashMap;

let map: DashMap<String, i32> = DashMap::with_shard_amount(32);
```

### with_hasher(hasher)

Creates a new DashMap with custom hasher.

```rust
pub fn with_hasher<S>(hasher: S) -> DashMap<K, V, S>
where
    K: Eq + Hash,
    S: BuildHasher + Clone,
```

**Parameters:**
- `hasher`: Custom hash builder

**Example:**
```rust
use dashmap::DashMap;
use ahash::RandomState;

let map = DashMap::with_hasher(RandomState::new());
```

### with_capacity_and_hasher(capacity, hasher)

Creates a new DashMap with capacity and custom hasher.

```rust
pub fn with_capacity_and_hasher<S>(
    capacity: usize, 
    hasher: S
) -> DashMap<K, V, S>
where
    K: Eq + Hash,
    S: BuildHasher + Clone,
```

**Example:**
```rust
use dashmap::DashMap;
use ahash::RandomState;

let map = DashMap::with_capacity_and_hasher(1000, RandomState::new());
```

### with_capacity_and_shard_amount(capacity, shard_amount)

Creates a new DashMap with capacity and shard count.

```rust
pub fn with_capacity_and_shard_amount(
    capacity: usize,
    shard_amount: usize
) -> DashMap<K, V, RandomState>
where
    K: Eq + Hash,
```

**Example:**
```rust
use dashmap::DashMap;

let map = DashMap::with_capacity_and_shard_amount(10000, 64);
```

### with_capacity_and_hasher_and_shard_amount(capacity, hasher, shard_amount)

Creates a new DashMap with all custom parameters.

```rust
pub fn with_capacity_and_hasher_and_shard_amount<S>(
    capacity: usize,
    hasher: S,
    shard_amount: usize
) -> DashMap<K, V, S>
where
    K: Eq + Hash,
    S: BuildHasher + Clone,
```

**Example:**
```rust
use dashmap::DashMap;
use ahash::RandomState;

let map = DashMap::with_capacity_and_hasher_and_shard_amount(
    10000, 
    RandomState::new(), 
    64
);
```

## Basic Operations

### insert(key, value)

Inserts a key-value pair, returning the previous value if present.

```rust
pub fn insert(&self, key: K, value: V) -> Option<V>
```

**Parameters:**
- `key`: Key to insert
- `value`: Value to associate with key

**Returns:** Previous value if key existed, `None` otherwise

**Locking:** May deadlock if called while holding references into the map

**Example:**
```rust
use dashmap::DashMap;

let map = DashMap::new();

// Insert new key
assert_eq!(map.insert("key1", 42), None);

// Update existing key
assert_eq!(map.insert("key1", 100), Some(42));
```

### get(key)

Gets an immutable reference to a value.

```rust
pub fn get<Q>(&self, key: &Q) -> Option<Ref<K, V>>
where
    K: Borrow<Q>,
    Q: Hash + Eq + ?Sized,
```

**Parameters:**
- `key`: Key to look up

**Returns:** `Some(Ref<K, V>)` if found, `None` otherwise

**Locking:** May deadlock if called while holding mutable references

**Example:**
```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("key", 42);

if let Some(value) = map.get("key") {
    println!("Value: {}", *value);
}
```

### get_mut(key)

Gets a mutable reference to a value.

```rust
pub fn get_mut<Q>(&self, key: &Q) -> Option<RefMut<K, V>>
where
    K: Borrow<Q>,
    Q: Hash + Eq + ?Sized,
```

**Parameters:**
- `key`: Key to look up

**Returns:** `Some(RefMut<K, V>)` if found, `None` otherwise

**Locking:** May deadlock if called while holding any references

**Example:**
```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("counter", 0);

if let Some(mut counter) = map.get_mut("counter") {
    *counter += 1;
}

assert_eq!(*map.get("counter").unwrap(), 1);
```

### try_get(key)

Non-blocking version of `get()`.

```rust
pub fn try_get<Q>(&self, key: &Q) -> TryResult<Ref<K, V>>
where
    K: Borrow<Q>,
    Q: Hash + Eq + ?Sized,
```

**Returns:** 
- `TryResult::Present(Ref<K, V>)` if found
- `TryResult::Absent` if not found
- `TryResult::Locked` if shard is locked

**Example:**
```rust
use dashmap::{DashMap, try_result::TryResult};

let map = DashMap::new();
map.insert("key", 42);

match map.try_get("key") {
    TryResult::Present(value) => println!("Value: {}", *value),
    TryResult::Absent => println!("Key not found"),
    TryResult::Locked => println!("Shard is busy"),
}
```

### try_get_mut(key)

Non-blocking version of `get_mut()`.

```rust
pub fn try_get_mut<Q>(&self, key: &Q) -> TryResult<RefMut<K, V>>
where
    K: Borrow<Q>,
    Q: Hash + Eq + ?Sized,
```

**Example:**
```rust
use dashmap::{DashMap, try_result::TryResult};

let map = DashMap::new();
map.insert("counter", 0);

match map.try_get_mut("counter") {
    TryResult::Present(mut counter) => *counter += 1,
    TryResult::Absent => println!("Counter not found"),
    TryResult::Locked => println!("Counter is busy"),
}
```

### remove(key)

Removes a key-value pair.

```rust
pub fn remove<Q>(&self, key: &Q) -> Option<(K, V)>
where
    K: Borrow<Q>,
    Q: Hash + Eq + ?Sized,
```

**Parameters:**
- `key`: Key to remove

**Returns:** `Some((K, V))` if key existed, `None` otherwise

**Locking:** May deadlock if called while holding references

**Example:**
```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("key", 42);

let removed = map.remove("key");
assert_eq!(removed, Some(("key", 42)));

assert!(map.get("key").is_none());
```

### remove_if(key, predicate)

Conditionally removes a key-value pair.

```rust
pub fn remove_if<Q>(&self, key: &Q, f: impl FnOnce(&K, &V) -> bool) -> Option<(K, V)>
where
    K: Borrow<Q>,
    Q: Hash + Eq + ?Sized,
```

**Parameters:**
- `key`: Key to potentially remove
- `f`: Predicate function

**Returns:** `Some((K, V))` if key existed and predicate returned `true`

**Example:**
```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("score", 85);

// Remove only if score is below 90
let removed = map.remove_if("score", |_key, &value| value < 90);
assert_eq!(removed, Some(("score", 85)));
```

### remove_if_mut(key, predicate)

Conditionally removes with mutable access to value.

```rust
pub fn remove_if_mut<Q>(&self, key: &Q, f: impl FnOnce(&K, &mut V) -> bool) -> Option<(K, V)>
where
    K: Borrow<Q>,
    Q: Hash + Eq + ?Sized,
```

**Example:**
```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("counter", 5);

// Decrement and remove if reaches zero
let removed = map.remove_if_mut("counter", |_key, value| {
    *value -= 1;
    *value == 0
});

// Counter was decremented to 4, not removed
assert!(removed.is_none());
assert_eq!(*map.get("counter").unwrap(), 4);
```

### contains_key(key)

Checks if a key exists in the map.

```rust
pub fn contains_key<Q>(&self, key: &Q) -> bool
where
    K: Borrow<Q>,
    Q: Hash + Eq + ?Sized,
```

**Example:**
```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("key", 42);

assert!(map.contains_key("key"));
assert!(!map.contains_key("missing"));
```

## Entry API

### entry(key)

Gets an entry for advanced manipulation.

```rust
pub fn entry(&self, key: K) -> Entry<K, V>
```

**Returns:** `Entry<K, V>` enum (either `Occupied` or `Vacant`)

**Locking:** May deadlock if called while holding references

**Example:**
```rust
use dashmap::DashMap;

let map = DashMap::new();

// Insert or update
map.entry("counter".to_string())
    .and_modify(|v| *v += 1)
    .or_insert(1);
```

### try_entry(key)

Non-blocking version of `entry()`.

```rust
pub fn try_entry(&self, key: K) -> Option<Entry<K, V>>
```

**Returns:** `Some(Entry<K, V>)` if successful, `None` if shard is locked

**Example:**
```rust
use dashmap::DashMap;

let map = DashMap::new();

if let Some(entry) = map.try_entry("key".to_string()) {
    entry.or_insert(42);
} else {
    println!("Shard is busy, try again later");
}
```

### Entry Methods

#### Entry::key()

Gets the key of the entry.

```rust
pub fn key(&self) -> &K
```

#### Entry::into_key()

Consumes the entry and returns the key.

```rust
pub fn into_key(self) -> K
```

#### Entry::or_insert(value)

Inserts value if entry is vacant, returns mutable reference.

```rust
pub fn or_insert(self, value: V) -> RefMut<K, V>
```

#### Entry::or_insert_with(f)

Inserts result of function if entry is vacant.

```rust
pub fn or_insert_with(self, f: impl FnOnce() -> V) -> RefMut<K, V>
```

#### Entry::or_default()

Inserts default value if entry is vacant.

```rust
pub fn or_default(self) -> RefMut<K, V>
where
    V: Default,
```

#### Entry::and_modify(f)

Modifies entry if occupied.

```rust
pub fn and_modify(self, f: impl FnOnce(&mut V)) -> Self
```

**Example:**
```rust
use dashmap::DashMap;

let map = DashMap::new();

// Complex entry manipulation
let final_value = map.entry("score".to_string())
    .and_modify(|score| {
        *score = (*score + 10).min(100); // Cap at 100
    })
    .or_insert(10);

println!("Final score: {}", *final_value);
```

### OccupiedEntry Methods

#### get()

Gets immutable reference to the value.

```rust
pub fn get(&self) -> &V
```

#### get_mut()

Gets mutable reference to the value.

```rust
pub fn get_mut(&mut self) -> &mut V
```

#### insert(value)

Replaces the value and returns the old value.

```rust
pub fn insert(&mut self, value: V) -> V
```

#### remove()

Removes the entry and returns the value.

```rust
pub fn remove(self) -> V
```

#### remove_entry()

Removes the entry and returns the key-value pair.

```rust
pub fn remove_entry(self) -> (K, V)
```

### VacantEntry Methods

#### insert(value)

Inserts the value and returns a mutable reference.

```rust
pub fn insert(self, value: V) -> RefMut<K, V>
```

#### insert_entry(value)

Inserts the value and returns an OccupiedEntry.

```rust
pub fn insert_entry(self, value: V) -> OccupiedEntry<K, V>
where
    K: Clone,
```

## Iteration

### iter()

Creates an immutable iterator.

```rust
pub fn iter(&self) -> Iter<K, V, S, DashMap<K, V, S>>
```

**Returns:** Iterator yielding `RefMulti<K, V>`

**Locking:** May deadlock if called while holding mutable references

**Example:**
```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("a", 1);
map.insert("b", 2);

for entry in map.iter() {
    println!("{}: {}", entry.key(), entry.value());
}
```

### iter_mut()

Creates a mutable iterator.

```rust
pub fn iter_mut(&self) -> IterMut<K, V, S, DashMap<K, V, S>>
```

**Returns:** Iterator yielding `RefMutMulti<K, V>`

**Locking:** May deadlock if called while holding any references

**Example:**
```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("a", 1);
map.insert("b", 2);

// Double all values
for mut entry in map.iter_mut() {
    *entry.value_mut() *= 2;
}
```

### into_iter()

Consumes the map and creates an owning iterator.

```rust
impl<K, V, S> IntoIterator for DashMap<K, V, S>
```

**Returns:** Iterator yielding `(K, V)`

**Example:**
```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("a", 1);
map.insert("b", 2);

for (key, value) in map {
    println!("Owned: {} -> {}", key, value);
}
```

### Parallel Iteration (Rayon)

Requires the `rayon` feature.

#### par_iter()

Creates a parallel immutable iterator.

```rust
pub fn par_iter(&self) -> rayon::iter::ParIter<RefMulti<K, V>>
```

#### par_iter_mut()

Creates a parallel mutable iterator.

```rust
pub fn par_iter_mut(&self) -> rayon::iter::ParIterMut<RefMutMulti<K, V>>
```

**Example:**
```rust
use dashmap::DashMap;
use rayon::prelude::*;

let map = DashMap::new();
for i in 0..1000 {
    map.insert(i, i * i);
}

// Parallel sum
let sum: i32 = map.par_iter()
    .map(|entry| *entry.value())
    .sum();

println!("Sum: {}", sum);
```

## Utility Methods

### len()

Returns the number of key-value pairs.

```rust
pub fn len(&self) -> usize
```

**Locking:** May deadlock if called while holding mutable references

**Example:**
```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("a", 1);
map.insert("b", 2);

assert_eq!(map.len(), 2);
```

### is_empty()

Checks if the map is empty.

```rust
pub fn is_empty(&self) -> bool
```

**Example:**
```rust
use dashmap::DashMap;

let map: DashMap<String, i32> = DashMap::new();
assert!(map.is_empty());

map.insert("key".to_string(), 42);
assert!(!map.is_empty());
```

### capacity()

Returns the total capacity across all shards.

```rust
pub fn capacity(&self) -> usize
```

**Example:**
```rust
use dashmap::DashMap;

let map = DashMap::with_capacity(1000);
println!("Capacity: {}", map.capacity());
```

### clear()

Removes all key-value pairs.

```rust
pub fn clear(&self)
```

**Locking:** May deadlock if called while holding references

**Example:**
```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("a", 1);
map.insert("b", 2);

map.clear();
assert!(map.is_empty());
```

### shrink_to_fit()

Shrinks capacity to fit current size.

```rust
pub fn shrink_to_fit(&self)
```

**Locking:** May deadlock if called while holding references

**Example:**
```rust
use dashmap::DashMap;

let map = DashMap::with_capacity(1000);
map.insert("key", 42);

map.shrink_to_fit();
println!("New capacity: {}", map.capacity());
```

### retain(predicate)

Retains only elements matching the predicate.

```rust
pub fn retain(&self, f: impl FnMut(&K, &mut V) -> bool)
```

**Parameters:**
- `f`: Predicate function returning `true` for elements to keep

**Locking:** May deadlock if called while holding references

**Example:**
```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("a", 1);
map.insert("b", 2);
map.insert("c", 3);

// Keep only even values
map.retain(|_key, value| *value % 2 == 0);

assert_eq!(map.len(), 1);
assert!(map.contains_key("b"));
```

### alter(key, f)

Modifies a value in-place.

```rust
pub fn alter<Q>(&self, key: &Q, f: impl FnOnce(&K, V) -> V)
where
    K: Borrow<Q>,
    Q: Hash + Eq + ?Sized,
```

**Parameters:**
- `key`: Key to modify
- `f`: Function to transform the value

**Panics:** If the closure panics

**Example:**
```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("score", 85);

// Double the score
map.alter("score", |_key, value| value * 2);

assert_eq!(*map.get("score").unwrap(), 170);
```

### alter_all(f)

Modifies all values in-place.

```rust
pub fn alter_all(&self, f: impl FnMut(&K, V) -> V)
```

**Example:**
```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("a", 1);
map.insert("b", 2);

// Double all values
map.alter_all(|_key, value| value * 2);

assert_eq!(*map.get("a").unwrap(), 2);
assert_eq!(*map.get("b").unwrap(), 4);
```

### view(key, f)

Applies a function to a value without extracting it.

```rust
pub fn view<Q, R>(&self, key: &Q, f: impl FnOnce(&K, &V) -> R) -> Option<R>
where
    K: Borrow<Q>,
    Q: Hash + Eq + ?Sized,
```

**Parameters:**
- `key`: Key to view
- `f`: Function to apply to the key-value pair

**Returns:** `Some(R)` if key exists, `None` otherwise

**Example:**
```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("data", vec![1, 2, 3, 4, 5]);

let length = map.view("data", |_key, value| value.len());
assert_eq!(length, Some(5));
```

### hasher()

Returns a reference to the hash builder.

```rust
pub fn hasher(&self) -> &S
```

**Example:**
```rust
use dashmap::DashMap;
use std::collections::hash_map::RandomState;

let map = DashMap::with_hasher(RandomState::new());
let hasher: &RandomState = map.hasher();
```

### hash_usize(item)

Hashes an item using the map's hasher.

```rust
pub fn hash_usize<T: Hash>(&self, item: &T) -> usize
```

**Example:**
```rust
use dashmap::DashMap;

let map: DashMap<String, i32> = DashMap::new();
let hash = map.hash_usize(&"test_key".to_string());
println!("Hash: {}", hash);
```

## Advanced Features

### Raw API (requires `raw-api` feature)

#### shards()

Returns a reference to the internal shards.

```rust
pub fn shards(&self) -> &[CachePadded<RwLock<HashMap<K, V>>>]
```

#### determine_map(key)

Finds which shard a key belongs to.

```rust
pub fn determine_map<Q>(&self, key: &Q) -> usize
where
    K: Borrow<Q>,
    Q: Hash + Eq + ?Sized,
```

#### determine_shard(hash)

Finds which shard a hash belongs to.

```rust
pub fn determine_shard(&self, hash: usize) -> usize
```

**Example:**
```rust
#[cfg(feature = "raw-api")]
{
    use dashmap::DashMap;

    let map = DashMap::new();
    let shard_index = map.determine_map("key");
    println!("Key 'key' is in shard {}", shard_index);
    
    let shards = map.shards();
    println!("Total shards: {}", shards.len());
}
```

### Read-Only View

#### into_read_only()

Converts the map into a read-only view.

```rust
pub fn into_read_only(self) -> ReadOnlyView<K, V, S>
```

**Example:**
```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("key", 42);

let read_only = map.into_read_only();
let value = read_only.get("key").unwrap();
println!("Value: {}", value);
```

### Memory Management

#### try_reserve(additional)

Tries to reserve capacity for additional elements.

```rust
pub fn try_reserve(&mut self, additional: usize) -> Result<(), TryReserveError>
```

**Example:**
```rust
use dashmap::DashMap;

let mut map = DashMap::new();
match map.try_reserve(1000) {
    Ok(()) => println!("Reserved capacity"),
    Err(_) => println!("Failed to reserve capacity"),
}
```

## Type Aliases

### Reference Types

```rust
// Immutable reference to a key-value pair
pub type Ref<'a, K, V> = mapref::one::Ref<'a, K, V>;

// Mutable reference to a key-value pair  
pub type RefMut<'a, K, V> = mapref::one::RefMut<'a, K, V>;

// Multi-reference for iteration
pub type RefMulti<'a, K, V> = mapref::multiple::RefMulti<'a, K, V>;
```

### Entry Types

```rust
// Entry enum for advanced manipulation
pub enum Entry<'a, K, V> {
    Occupied(OccupiedEntry<'a, K, V>),
    Vacant(VacantEntry<'a, K, V>),
}

// Occupied entry
pub struct OccupiedEntry<'a, K, V> { /* private fields */ }

// Vacant entry
pub struct VacantEntry<'a, K, V> { /* private fields */ }
```

### Try Result

```rust
// Result type for non-blocking operations
pub enum TryResult<T> {
    Present(T),
    Absent,
    Locked,
}
```

## Operator Overloads

DashMap provides convenient operator overloads:

```rust
use dashmap::DashMap;

let map = DashMap::new();

// Insert using << operator
&map << ("key", 42);

// Get using >> operator  
let value = &map >> "key"; // Returns Ref<K, V>

// Get mutable using | operator
let mut_value = &map | "key"; // Returns RefMut<K, V>

// Remove using - operator
let removed = &map - "key"; // Returns Option<(K, V)>

// Contains using & operator
let exists = &map & "key"; // Returns bool
```

## Error Types

### TryReserveError

```rust
#[non_exhaustive]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TryReserveError {}
```

Returned when memory allocation fails during `try_reserve()`.

## Feature Flags

### serde

Enables serialization support:

```rust
use dashmap::DashMap;
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize)]
struct Data {
    map: DashMap<String, i32>,
}
```

### rayon

Enables parallel iteration:

```rust
use dashmap::DashMap;
use rayon::prelude::*;

let map = DashMap::new();
let sum: i32 = map.par_iter().map(|entry| *entry.value()).sum();
```

### raw-api

Enables access to internal shards:

```rust
let map = DashMap::new();
let shards = map.shards();
let shard_count = shards.len();
```

### inline

Enables more aggressive inlining for better performance at the cost of compilation time.

This comprehensive API reference covers all public methods and types in DashMap 6.0. Each method includes its signature, parameters, return values, and practical examples to help you use DashMap effectively in your concurrent applications.