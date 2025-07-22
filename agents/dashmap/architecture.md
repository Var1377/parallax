# DashMap Architecture: Shard-Based Design

DashMap achieves high-performance concurrent access through a sophisticated shard-based architecture that minimizes lock contention while maintaining safety guarantees.

## Core Architecture

### Shard-Based Partitioning

DashMap divides the hash table into multiple independent shards, each protected by its own RwLock:

```rust
pub struct DashMap<K, V, S = RandomState> {
    shift: usize,                                    // Bit shift for shard selection
    shards: Box<[CachePadded<RwLock<HashMap<K, V>>>]>, // Array of sharded hash tables
    hasher: S,                                       // Hash function builder
}
```

### Key Components

1. **Shards**: Independent hash tables, each with its own lock
2. **Cache Padding**: Prevents false sharing between shards
3. **Custom RwLock**: Optimized reader-writer lock implementation
4. **Hash Distribution**: Ensures even distribution across shards

## Shard Selection Algorithm

### Hash-Based Partitioning

```rust
fn determine_shard(&self, hash: usize) -> usize {
    // Leave the high 7 bits for the HashBrown SIMD tag
    (hash << 7) >> self.shift
}
```

The shard selection process:

1. **Hash the key** using the configured hasher
2. **Apply bit manipulation** to extract shard index
3. **Preserve SIMD bits** for HashBrown optimization
4. **Ensure power-of-2 distribution** for efficient modulo operation

### Shard Count Determination

```rust
fn default_shard_amount() -> usize {
    (std::thread::available_parallelism().map_or(1, usize::from) * 4)
        .next_power_of_two()
}
```

Default shard count calculation:
- **Base**: 4 × number of CPU cores
- **Constraint**: Must be power of 2
- **Rationale**: Balance between parallelism and memory overhead

## Memory Layout

### Cache-Friendly Design

```rust
use crossbeam_utils::CachePadded;

// Each shard is cache-padded to prevent false sharing
shards: Box<[CachePadded<RwLock<HashMap<K, V>>>]>
```

Benefits of cache padding:
- **Prevents false sharing** between adjacent shards
- **Improves cache locality** for shard-local operations
- **Reduces memory bus contention** in multi-core systems

### SharedValue Wrapper

```rust
#[repr(transparent)]
pub struct SharedValue<T> {
    value: UnsafeCell<T>,
}
```

The `SharedValue` wrapper:
- **Enables safe concurrent access** to values
- **Maintains memory safety** during concurrent reads/writes
- **Zero-cost abstraction** with `#[repr(transparent)]`

## Locking Strategy

### Custom RwLock Implementation

DashMap uses a custom RwLock optimized for its access patterns:

```rust
pub struct RawRwLock {
    state: AtomicUsize,
}

const READERS_PARKED: usize = 0b0001;
const WRITERS_PARKED: usize = 0b0010;
const ONE_READER: usize = 0b0100;
const ONE_WRITER: usize = !(READERS_PARKED | WRITERS_PARKED);
```

### State Machine

The lock state encodes:
- **Reader count** in high bits
- **Writer presence** in specific bits
- **Parking state** for blocked threads

### Lock Acquisition Patterns

#### Fast Path (Uncontended)
```rust
fn try_lock_shared_fast(&self) -> bool {
    let state = self.state.load(Ordering::Relaxed);
    if let Some(new_state) = state.checked_add(ONE_READER) {
        if new_state & ONE_WRITER != ONE_WRITER {
            return self.state.compare_exchange_weak(
                state, new_state, 
                Ordering::Acquire, Ordering::Relaxed
            ).is_ok();
        }
    }
    false
}
```

#### Slow Path (Contended)
- **Spin waiting** with exponential backoff
- **Parking/unparking** using `parking_lot_core`
- **Fair scheduling** to prevent starvation

## Concurrency Model

### Reader-Writer Semantics

- **Multiple readers** can access the same shard simultaneously
- **Single writer** has exclusive access to a shard
- **Cross-shard operations** can proceed in parallel

### Lock Granularity

```
┌─────────────────────────────────────────────────────────┐
│                    DashMap                              │
├─────────────┬─────────────┬─────────────┬─────────────┤
│   Shard 0   │   Shard 1   │   Shard 2   │   Shard N   │
│ RwLock<HM>  │ RwLock<HM>  │ RwLock<HM>  │ RwLock<HM>  │
├─────────────┼─────────────┼─────────────┼─────────────┤
│ Key₀ → Val₀ │ Key₁ → Val₁ │ Key₂ → Val₂ │ Key₃ → Val₃ │
│ Key₄ → Val₄ │ Key₅ → Val₅ │ Key₆ → Val₆ │ Key₇ → Val₇ │
└─────────────┴─────────────┴─────────────┴─────────────┘
```

Benefits:
- **Fine-grained locking** reduces contention
- **Independent shards** allow parallel access
- **Scalable performance** with thread count

## Hash Distribution

### Quality Requirements

Good hash distribution is critical for performance:

```rust
// Example: Poor distribution leads to hot shards
let map = DashMap::new();
for i in 0..1000 {
    map.insert(i * 8, i); // All keys might hash to same shard!
}

// Better: Use diverse key patterns
let map = DashMap::new();
for i in 0..1000 {
    map.insert(format!("key_{}", i), i); // Better distribution
}
```

### Hash Function Impact

Different hashers affect distribution:

```rust
use std::collections::hash_map::RandomState;
use ahash::AHasher;

// Default hasher (SipHash-based)
let map1 = DashMap::with_hasher(RandomState::new());

// Fast hasher (AHash)
let map2 = DashMap::with_hasher(ahash::RandomState::new());
```

## Memory Ordering Guarantees

### Acquire-Release Semantics

```rust
// Lock acquisition uses Acquire ordering
self.state.compare_exchange_weak(
    state, new_state,
    Ordering::Acquire,  // Success ordering
    Ordering::Relaxed   // Failure ordering
)

// Lock release uses Release ordering
self.state.compare_exchange(
    ONE_WRITER, 0,
    Ordering::Release,  // Success ordering
    Ordering::Relaxed   // Failure ordering
)
```

### Synchronization Points

1. **Lock acquisition** establishes happens-before relationship
2. **Lock release** ensures visibility of modifications
3. **Cross-shard operations** maintain consistency

## Performance Characteristics

### Scalability Properties

- **Read operations**: Scale linearly with core count (different shards)
- **Write operations**: Limited by shard contention
- **Mixed workloads**: Benefit from reader-writer parallelism

### Contention Patterns

```rust
// Low contention: Operations on different shards
thread::spawn(|| map.insert("key1", value1)); // Shard A
thread::spawn(|| map.insert("key2", value2)); // Shard B

// High contention: Operations on same shard
thread::spawn(|| map.insert("key1", value1)); // Shard A
thread::spawn(|| map.insert("key5", value2)); // Shard A (if hash collision)
```

### Memory Overhead

Per-shard overhead:
- **RwLock state**: ~8 bytes
- **HashMap metadata**: ~24 bytes
- **Cache padding**: ~64 bytes per shard
- **Total**: ~96 bytes + data per shard

## Comparison with Alternatives

### vs. Single RwLock<HashMap>

| Aspect | DashMap | RwLock<HashMap> |
|--------|---------|-----------------|
| Read Scalability | Excellent | Limited |
| Write Scalability | Good | Poor |
| Memory Overhead | Higher | Lower |
| API Complexity | Simple | Simple |

### vs. Lock-Free Maps

| Aspect | DashMap | Lock-Free |
|--------|---------|-----------|
| Implementation Complexity | Moderate | High |
| Memory Reclamation | Automatic | Manual/Epoch |
| Worst-case Latency | Bounded | Unbounded |
| Memory Usage | Predictable | Variable |

## Design Trade-offs

### Advantages

1. **Predictable performance** under contention
2. **Memory safety** without garbage collection
3. **Simple API** similar to standard HashMap
4. **Good cache locality** within shards

### Limitations

1. **Memory overhead** from multiple hash tables
2. **Hash quality dependency** for load balancing
3. **Limited atomic operations** across keys
4. **Iteration complexity** across multiple shards

## Implementation Details

### Shard Initialization

```rust
pub fn with_capacity_and_hasher_and_shard_amount(
    mut capacity: usize,
    hasher: S,
    shard_amount: usize,
) -> Self {
    assert!(shard_amount > 1);
    assert!(shard_amount.is_power_of_two());

    let shift = util::ptr_size_bits() - ncb(shard_amount);
    
    if capacity != 0 {
        capacity = (capacity + (shard_amount - 1)) & !(shard_amount - 1);
    }
    
    let cps = capacity / shard_amount;
    
    let shards = (0..shard_amount)
        .map(|_| CachePadded::new(RwLock::new(HashMap::with_capacity(cps))))
        .collect();
        
    Self { shift, shards, hasher }
}
```

### Key Insights

1. **Power-of-2 requirement** enables efficient bit masking
2. **Capacity distribution** ensures balanced shard sizes
3. **Shift calculation** optimizes shard selection
4. **Cache padding** prevents false sharing

This architecture enables DashMap to achieve excellent concurrent performance while maintaining the familiar HashMap API and strong safety guarantees.