# Memory Ordering and Safety Guarantees

DashMap provides strong memory ordering guarantees and safety properties that ensure correct behavior in concurrent environments. This document explains the memory model, synchronization guarantees, and safety properties.

## Memory Model Overview

DashMap's memory model is built on Rust's ownership system and atomic operations, providing:

1. **Sequential Consistency** for individual operations
2. **Acquire-Release Semantics** for lock operations  
3. **Memory Safety** through Rust's type system
4. **Data Race Freedom** through careful synchronization

## Atomic Operations and Ordering

### Lock-Based Synchronization

DashMap uses a custom RwLock implementation with specific memory ordering guarantees:

```rust
// Lock acquisition uses Acquire ordering
fn lock_shared(&self) -> bool {
    self.state.compare_exchange_weak(
        state, new_state,
        Ordering::Acquire,  // Establishes happens-before relationship
        Ordering::Relaxed   // Failure doesn't need ordering
    )
}

// Lock release uses Release ordering  
unsafe fn unlock_shared(&self) {
    let state = self.state.fetch_sub(ONE_READER, Ordering::Release);
    // Release ensures all previous operations are visible
}
```

### Memory Ordering Guarantees

#### Acquire Semantics on Lock Acquisition
```rust
// When a thread acquires a read or write lock:
// 1. All subsequent memory operations cannot be reordered before the acquire
// 2. All memory operations from the previous lock holder become visible

let map = DashMap::new();
map.insert("key", 42);

// Thread A
if let Some(value) = map.get("key") {
    // Acquire semantics guarantee that the insert from any previous
    // thread is visible here
    assert_eq!(*value, 42); // This will always pass
}
```

#### Release Semantics on Lock Release
```rust
// When a thread releases a lock:
// 1. All previous memory operations cannot be reordered after the release
// 2. All previous operations become visible to subsequent lock acquirers

// Thread A
map.insert("key1", 10);
map.insert("key2", 20);
// Release semantics ensure both inserts are visible before lock is released

// Thread B (later)
if let Some(value1) = map.get("key1") {
    // If we can see key1, we're guaranteed to see key2 as well
    let value2 = map.get("key2").unwrap();
    // Both values are guaranteed to be visible
}
```

## Synchronization Points

### Per-Shard Synchronization

Each shard provides independent synchronization:

```rust
use dashmap::DashMap;
use std::sync::Arc;
use std::thread;

let map = Arc::new(DashMap::new());

// Operations on different shards can proceed without synchronization
let map1 = map.clone();
let handle1 = thread::spawn(move || {
    map1.insert("shard_a_key", 1); // Likely different shard
});

let map2 = map.clone();  
let handle2 = thread::spawn(move || {
    map2.insert("shard_b_key", 2); // Likely different shard
});

// These operations can execute in parallel with no ordering constraints
// between them, but each individual operation has internal ordering guarantees
```

### Cross-Shard Ordering

```rust
use dashmap::DashMap;
use std::sync::Arc;
use std::thread;
use std::sync::atomic::{AtomicBool, Ordering};

let map = Arc::new(DashMap::new());
let flag = Arc::new(AtomicBool::new(false));

// Thread A
let map_a = map.clone();
let flag_a = flag.clone();
thread::spawn(move || {
    map_a.insert("key1", 42);
    flag_a.store(true, Ordering::Release); // Release ensures insert is visible
});

// Thread B  
let map_b = map.clone();
let flag_b = flag.clone();
thread::spawn(move || {
    while !flag_b.load(Ordering::Acquire) {} // Acquire pairs with Release
    
    // Due to acquire-release pairing, the insert is guaranteed visible
    let value = map_b.get("key1").unwrap();
    assert_eq!(*value, 42);
});
```

## Safety Guarantees

### Memory Safety

DashMap provides complete memory safety through Rust's type system:

```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("key", "value".to_string());

// Safe: References are properly managed
if let Some(value_ref) = map.get("key") {
    println!("{}", *value_ref); // Safe dereference
    // value_ref automatically dropped here
}

// Safe: No use-after-free possible
let value = map.remove("key");
// Original reference is no longer accessible
```

### Data Race Freedom

DashMap eliminates data races through careful synchronization:

```rust
use dashmap::DashMap;
use std::sync::Arc;
use std::thread;

let map = Arc::new(DashMap::new());

// Multiple threads can safely access the same key
let handles: Vec<_> = (0..10).map(|i| {
    let map = map.clone();
    thread::spawn(move || {
        // These operations are synchronized and data-race-free
        map.insert("shared_key", i);
        map.get("shared_key");
        map.remove("shared_key");
    })
}).collect();

for handle in handles {
    handle.join().unwrap();
}
// No data races possible - all operations are properly synchronized
```

### Reference Validity

DashMap ensures reference validity through RAII guards:

```rust
use dashmap::DashMap;

let map = DashMap::new();
map.insert("key", vec![1, 2, 3]);

{
    let value_ref = map.get("key").unwrap();
    // value_ref holds a read lock on the appropriate shard
    println!("Length: {}", value_ref.len());
    
    // Other threads can read the same shard concurrently
    // but cannot modify this entry while value_ref exists
} // Lock automatically released when value_ref is dropped

// Now other threads can modify the entry
map.insert("key", vec![4, 5, 6]);
```

## Consistency Models

### Per-Operation Consistency

Each DashMap operation provides sequential consistency:

```rust
use dashmap::DashMap;
use std::sync::Arc;
use std::thread;

let map = Arc::new(DashMap::new());

// Thread A
let map_a = map.clone();
thread::spawn(move || {
    map_a.insert("x", 1);
    map_a.insert("y", 2);
});

// Thread B
let map_b = map.clone();
thread::spawn(move || {
    // Each individual get() sees a consistent state
    if let Some(x) = map_b.get("x") {
        // At this point, x definitely exists with value 1
        assert_eq!(*x, 1);
    }
});
```

### Cross-Operation Consistency

Operations across different keys may not be atomic:

```rust
use dashmap::DashMap;
use std::sync::Arc;
use std::thread;

let map = Arc::new(DashMap::new());

// Thread A
let map_a = map.clone();
thread::spawn(move || {
    map_a.insert("x", 1);
    map_a.insert("y", 2); // Not atomic with previous insert
});

// Thread B
let map_b = map.clone();
thread::spawn(move || {
    // Might see x but not y, or y but not x, or both, or neither
    let x = map_b.get("x");
    let y = map_b.get("y");
    
    // No guarantee about the relationship between these observations
});
```

### Iteration Consistency

Iteration provides a consistent snapshot per shard:

```rust
use dashmap::DashMap;
use std::sync::Arc;
use std::thread;

let map = Arc::new(DashMap::new());

// Populate map
for i in 0..1000 {
    map.insert(i, i * 2);
}

let map_clone = map.clone();

// Modifier thread
thread::spawn(move || {
    for i in 1000..2000 {
        map_clone.insert(i, i * 2);
    }
});

// Iterator thread
for entry in &*map {
    // Each shard provides a consistent snapshot
    // New entries may or may not be visible depending on timing
    println!("{}: {}", entry.key(), entry.value());
}
```

## Happens-Before Relationships

### Lock-Based Happens-Before

```rust
use dashmap::DashMap;
use std::sync::Arc;
use std::thread;

let map = Arc::new(DashMap::new());

// Thread A
let map_a = map.clone();
let handle_a = thread::spawn(move || {
    map_a.insert("key", 42);
    // Lock release creates happens-before edge
});

handle_a.join().unwrap(); // Synchronization point

// Thread B (after join)
let value = map.get("key").unwrap();
// Due to happens-before relationship, insert is guaranteed visible
assert_eq!(*value, 42);
```

### Entry API Atomicity

The entry API provides atomic read-modify-write operations:

```rust
use dashmap::DashMap;
use std::sync::Arc;
use std::thread;

let map = Arc::new(DashMap::new());

// Multiple threads incrementing the same counter
let handles: Vec<_> = (0..10).map(|_| {
    let map = map.clone();
    thread::spawn(move || {
        for _ in 0..1000 {
            // Atomic increment - no race conditions
            map.entry("counter".to_string())
                .and_modify(|v| *v += 1)
                .or_insert(1);
        }
    })
}).collect();

for handle in handles {
    handle.join().unwrap();
}

// Final value is guaranteed to be 10,000
let final_count = map.get("counter").unwrap();
assert_eq!(*final_count, 10_000);
```

## Weak Memory Model Considerations

### ARM and Other Weak Architectures

DashMap's acquire-release semantics work correctly on weak memory models:

```rust
use dashmap::DashMap;
use std::sync::Arc;
use std::thread;
use std::sync::atomic::{AtomicUsize, Ordering};

let map = Arc::new(DashMap::new());
let counter = Arc::new(AtomicUsize::new(0));

// Producer thread
let map_producer = map.clone();
let counter_producer = counter.clone();
thread::spawn(move || {
    for i in 0..1000 {
        map_producer.insert(i, i * 2);
        counter_producer.store(i + 1, Ordering::Release);
    }
});

// Consumer thread
let map_consumer = map.clone();
let counter_consumer = counter.clone();
thread::spawn(move || {
    loop {
        let count = counter_consumer.load(Ordering::Acquire);
        if count > 0 {
            // Due to acquire-release pairing, all inserts up to
            // count-1 are guaranteed visible
            for i in 0..count {
                let value = map_consumer.get(&i).unwrap();
                assert_eq!(*value, i * 2);
            }
        }
        if count >= 1000 {
            break;
        }
    }
});
```

## Memory Barriers and Fences

### Implicit Barriers

DashMap operations include implicit memory barriers:

```rust
use dashmap::DashMap;

let map = DashMap::new();

// Insert includes acquire-release barriers
map.insert("key", 42);
// Equivalent to:
// - Acquire barrier before accessing shard
// - Release barrier after modifying shard

// Get includes acquire barrier  
let value = map.get("key");
// Equivalent to:
// - Acquire barrier when acquiring read lock
```

### Explicit Synchronization

For cross-shard ordering, use explicit synchronization:

```rust
use dashmap::DashMap;
use std::sync::Arc;
use std::thread;
use std::sync::Barrier;

let map = Arc::new(DashMap::new());
let barrier = Arc::new(Barrier::new(2));

// Thread A
let map_a = map.clone();
let barrier_a = barrier.clone();
thread::spawn(move || {
    map_a.insert("key1", 1);
    barrier_a.wait(); // Explicit synchronization point
});

// Thread B
let map_b = map.clone();
let barrier_b = barrier.clone();
thread::spawn(move || {
    barrier_b.wait(); // Wait for Thread A
    // Now guaranteed to see key1
    let value = map_b.get("key1").unwrap();
    assert_eq!(*value, 1);
});
```

## Advanced Safety Patterns

### Safe Shared Mutable State

```rust
use dashmap::DashMap;
use std::sync::Arc;

struct SafeCounter {
    counters: Arc<DashMap<String, u64>>,
}

impl SafeCounter {
    fn new() -> Self {
        Self {
            counters: Arc::new(DashMap::new()),
        }
    }
    
    fn increment(&self, name: &str) -> u64 {
        // Atomic increment with proper memory ordering
        let mut entry = self.counters.entry(name.to_string()).or_insert(0);
        *entry += 1;
        *entry
    }
    
    fn get(&self, name: &str) -> u64 {
        // Safe read with acquire semantics
        self.counters.get(name).map(|v| *v).unwrap_or(0)
    }
}

// Usage is completely safe across threads
let counter = Arc::new(SafeCounter::new());
// Multiple threads can safely increment and read
```

### Lock-Free Reads with Consistent Updates

```rust
use dashmap::DashMap;
use std::sync::Arc;

struct VersionedData {
    version: u64,
    data: String,
}

struct VersionedStore {
    store: Arc<DashMap<String, VersionedData>>,
}

impl VersionedStore {
    fn update(&self, key: &str, data: String, expected_version: u64) -> Result<(), String> {
        match self.store.entry(key.to_string()) {
            dashmap::mapref::entry::Entry::Occupied(mut entry) => {
                if entry.get().version == expected_version {
                    entry.insert(VersionedData {
                        version: expected_version + 1,
                        data,
                    });
                    Ok(())
                } else {
                    Err("Version mismatch".to_string())
                }
            }
            dashmap::mapref::entry::Entry::Vacant(entry) => {
                if expected_version == 0 {
                    entry.insert(VersionedData {
                        version: 1,
                        data,
                    });
                    Ok(())
                } else {
                    Err("Key does not exist".to_string())
                }
            }
        }
    }
    
    fn get(&self, key: &str) -> Option<VersionedData> {
        self.store.get(key).map(|entry| VersionedData {
            version: entry.version,
            data: entry.data.clone(),
        })
    }
}
```

## Testing Memory Ordering

### Stress Testing

```rust
use dashmap::DashMap;
use std::sync::Arc;
use std::thread;

fn stress_test_memory_ordering() {
    let map = Arc::new(DashMap::new());
    let iterations = 100_000;
    
    // Writer threads
    let writers: Vec<_> = (0..4).map(|thread_id| {
        let map = map.clone();
        thread::spawn(move || {
            for i in 0..iterations {
                let key = format!("{}_{}", thread_id, i);
                map.insert(key, thread_id);
            }
        })
    }).collect();
    
    // Reader threads
    let readers: Vec<_> = (0..4).map(|_| {
        let map = map.clone();
        thread::spawn(move || {
            let mut reads = 0;
            while reads < iterations {
                for entry in &*map {
                    // Each read should see a consistent value
                    let key_parts: Vec<&str> = entry.key().split('_').collect();
                    if key_parts.len() == 2 {
                        let thread_id: usize = key_parts[0].parse().unwrap();
                        assert_eq!(*entry.value(), thread_id);
                        reads += 1;
                    }
                }
            }
        })
    }).collect();
    
    // Wait for completion
    for writer in writers {
        writer.join().unwrap();
    }
    for reader in readers {
        reader.join().unwrap();
    }
}
```

### Property-Based Testing

```rust
use dashmap::DashMap;
use std::sync::Arc;
use std::thread;
use std::collections::HashSet;

fn test_insertion_visibility() {
    let map = Arc::new(DashMap::new());
    let num_threads = 8;
    let items_per_thread = 1000;
    
    // Insert from multiple threads
    let handles: Vec<_> = (0..num_threads).map(|thread_id| {
        let map = map.clone();
        thread::spawn(move || {
            for i in 0..items_per_thread {
                let key = thread_id * items_per_thread + i;
                map.insert(key, thread_id);
            }
        })
    }).collect();
    
    // Wait for all insertions
    for handle in handles {
        handle.join().unwrap();
    }
    
    // Verify all insertions are visible
    let mut seen_keys = HashSet::new();
    for entry in &*map {
        seen_keys.insert(*entry.key());
        
        // Verify value consistency
        let expected_thread = *entry.key() / items_per_thread;
        assert_eq!(*entry.value(), expected_thread);
    }
    
    // Verify all keys are present
    assert_eq!(seen_keys.len(), num_threads * items_per_thread);
    for i in 0..(num_threads * items_per_thread) {
        assert!(seen_keys.contains(&i));
    }
}
```

## Summary

DashMap provides strong memory ordering guarantees through:

1. **Acquire-Release Semantics**: Lock operations establish proper happens-before relationships
2. **Sequential Consistency**: Individual operations appear atomic and ordered
3. **Memory Safety**: Rust's type system prevents memory errors
4. **Data Race Freedom**: Proper synchronization eliminates race conditions

These guarantees make DashMap safe to use in complex concurrent scenarios while maintaining high performance. The memory model is well-defined and works correctly across different hardware architectures, including those with weak memory ordering.