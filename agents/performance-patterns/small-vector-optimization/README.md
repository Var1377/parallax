# Small Vector Optimization with SmallVec

## Table of Contents

1. [Introduction](#introduction)
2. [Core Concepts](#core-concepts)
3. [Basic Usage](#basic-usage)
4. [Advanced Patterns](#advanced-patterns)
5. [Performance Analysis](#performance-analysis)
6. [Memory Layout](#memory-layout)
7. [Optimization Strategies](#optimization-strategies)
8. [Integration Patterns](#integration-patterns)
9. [Benchmarking](#benchmarking)
10. [Best Practices](#best-practices)

## Introduction

Small Vector Optimization (SVO) is a technique where small collections are stored inline within the collection object itself, avoiding heap allocation for common small cases. The `smallvec` crate provides a `SmallVec<T, N>` type that stores up to `N` elements inline and falls back to heap allocation for larger collections.

### Key Benefits

- **Zero heap allocations** for small collections (≤ N elements)
- **Improved cache locality** for small data sets
- **Reduced allocation overhead** and fragmentation
- **Better performance** for common small cases
- **Drop-in replacement** for `Vec<T>`

### Performance Impact

```rust
use smallvec::{SmallVec, smallvec};

// Traditional Vec: always heap-allocated
let vec: Vec<i32> = vec![1, 2, 3];  // Heap allocation

// SmallVec: inline for small sizes
let small: SmallVec<[i32; 4]> = smallvec![1, 2, 3];  // Stack allocation
let large: SmallVec<[i32; 4]> = (0..10).collect();   // Heap allocation
```

## Core Concepts

### Inline vs Heap Storage

SmallVec uses a tagged union to distinguish between inline and heap storage:

```rust
use smallvec::SmallVec;

// Type alias for clarity
type SmallVec4<T> = SmallVec<[T; 4]>;

let mut vec = SmallVec4::new();

// These are stored inline (no heap allocation)
vec.push(1);
vec.push(2);
vec.push(3);
vec.push(4);

assert!(!vec.spilled()); // Still inline

// This triggers heap allocation
vec.push(5);
assert!(vec.spilled()); // Now on heap
```

### Memory Layout

```rust
use smallvec::SmallVec;
use std::mem;

type SmallVec8<T> = SmallVec<[T; 8]>;

// The SmallVec struct contains:
// - Tagged length (with heap/inline bit)
// - Union of inline array or heap pointer + capacity
println!("SmallVec size: {}", mem::size_of::<SmallVec8<i32>>());
println!("Vec size: {}", mem::size_of::<Vec<i32>>());

// SmallVec is larger than Vec due to inline storage
// but saves heap allocations for small cases
```

### Capacity Management

```rust
use smallvec::SmallVec;

type SmallVec4<T> = SmallVec<[T; 4]>;

let mut vec = SmallVec4::new();

// Inline capacity
assert_eq!(vec.inline_size(), 4);
assert_eq!(vec.len(), 0);
assert_eq!(vec.capacity(), 4); // Inline capacity

// After spilling to heap
vec.extend(0..10);
assert!(vec.capacity() >= 10); // Heap capacity
```

## Basic Usage

### Creation and Initialization

```rust
use smallvec::{SmallVec, smallvec};

// Empty SmallVec
let mut vec: SmallVec<[i32; 4]> = SmallVec::new();

// Using macro (like vec!)
let vec2 = smallvec![1, 2, 3, 4];

// From iterator
let vec3: SmallVec<[i32; 8]> = (0..5).collect();

// From slice
let vec4 = SmallVec::<[i32; 4]>::from_slice(&[1, 2, 3]);

// With capacity
let vec5 = SmallVec::<[i32; 4]>::with_capacity(10);
```

### Basic Operations

```rust
use smallvec::{SmallVec, smallvec};

let mut vec = smallvec![1, 2, 3];

// Push/pop (same as Vec)
vec.push(4);
let last = vec.pop();

// Indexing
vec[0] = 10;
let first = vec[0];

// Iteration
for item in &vec {
    println!("{}", item);
}

// Length and capacity
println!("Length: {}, Capacity: {}", vec.len(), vec.capacity());
```

### Conversion Operations

```rust
use smallvec::{SmallVec, smallvec};

let small = smallvec![1, 2, 3, 4];

// Convert to Vec
let vec: Vec<i32> = small.into_vec();

// Convert from Vec
let vec = vec![1, 2, 3, 4, 5];
let small: SmallVec<[i32; 4]> = SmallVec::from_vec(vec);

// Into boxed slice
let small = smallvec![1, 2, 3];
let boxed: Box<[i32]> = small.into_boxed_slice();
```

## Advanced Patterns

### Choosing Optimal Inline Size

```rust
use smallvec::SmallVec;

// Analyze your data to choose N
// Common patterns:

// Small identifier lists
type IdentifierList = SmallVec<[String; 2]>;  // Most have 1-2 items

// Function parameters
type ParamList<T> = SmallVec<[T; 4]>;  // Most functions have ≤4 params

// Small buffers
type SmallBuffer = SmallVec<[u8; 64]>;  // Cache line sized

// Error contexts
type ErrorStack = SmallVec<[String; 3]>;  // Usually shallow
```

### Zero-Sized Type Optimization

```rust
use smallvec::SmallVec;

// ZSTs are handled specially
let mut vec: SmallVec<[(); 1000]> = SmallVec::new();

// No heap allocation even for "large" counts
for _ in 0..10000 {
    vec.push(());
}

assert!(!vec.spilled()); // Still inline!
```

### Drain and Retain Operations

```rust
use smallvec::{SmallVec, smallvec};

let mut vec = smallvec![1, 2, 3, 4, 5, 6];

// Drain range
let drained: Vec<_> = vec.drain(1..4).collect();
assert_eq!(drained, [2, 3, 4]);
assert_eq!(vec, [1, 5, 6]);

// Retain elements
vec.retain(|&x| x % 2 == 1);
assert_eq!(vec, [1, 5]);
```

### Specialization Features

```rust
// Enable specialization feature for better performance
// Cargo.toml: smallvec = { version = "2.0", features = ["specialization"] }

use smallvec::SmallVec;

// With specialization, Copy types get optimized implementations
let slice = &[1, 2, 3, 4, 5];
let vec: SmallVec<[i32; 4]> = SmallVec::from_slice(slice);
// More efficient with specialization enabled
```

### Extract If (Unstable)

```rust
// Enable extract_if feature
// Cargo.toml: smallvec = { version = "2.0", features = ["extract_if"] }

use smallvec::{SmallVec, smallvec};

let mut vec = smallvec![1, 2, 3, 4, 5, 6];

// Extract elements matching predicate
let extracted: SmallVec<[i32; 4]> = vec.extract_if(|&x| x % 2 == 0).collect();
assert_eq!(extracted, [2, 4, 6]);
assert_eq!(vec, [1, 3, 5]);
```

## Performance Analysis

### Allocation Patterns

```rust
use smallvec::{SmallVec, smallvec};
use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering};

// Track allocations
static ALLOCATION_COUNT: AtomicUsize = AtomicUsize::new(0);

struct TrackingAllocator;

unsafe impl GlobalAlloc for TrackingAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        ALLOCATION_COUNT.fetch_add(1, Ordering::SeqCst);
        System.alloc(layout)
    }
    
    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        System.dealloc(ptr, layout)
    }
}

#[global_allocator]
static ALLOCATOR: TrackingAllocator = TrackingAllocator;

fn test_allocations() {
    let start_count = ALLOCATION_COUNT.load(Ordering::SeqCst);
    
    // SmallVec with inline storage
    let small = smallvec![1, 2, 3];  // No allocation
    
    // Regular Vec
    let vec = vec![1, 2, 3];  // 1 allocation
    
    let end_count = ALLOCATION_COUNT.load(Ordering::SeqCst);
    println!("Allocations: {}", end_count - start_count);
}
```

### Cache Performance

```rust
use smallvec::{SmallVec, smallvec};
use std::time::Instant;

fn benchmark_cache_performance() {
    const COUNT: usize = 10000;
    
    // SmallVec with inline storage
    let small_vecs: Vec<SmallVec<[i32; 4]>> = (0..COUNT)
        .map(|i| smallvec![i as i32, (i + 1) as i32, (i + 2) as i32])
        .collect();
    
    // Regular Vec (heap allocated)
    let vecs: Vec<Vec<i32>> = (0..COUNT)
        .map(|i| vec![i as i32, (i + 1) as i32, (i + 2) as i32])
        .collect();
    
    // Benchmark iteration (SmallVec should be faster due to cache locality)
    let start = Instant::now();
    let small_sum: i32 = small_vecs.iter()
        .flat_map(|v| v.iter())
        .sum();
    let small_time = start.elapsed();
    
    let start = Instant::now();
    let vec_sum: i32 = vecs.iter()
        .flat_map(|v| v.iter())
        .sum();
    let vec_time = start.elapsed();
    
    println!("SmallVec time: {:?}", small_time);
    println!("Vec time: {:?}", vec_time);
    println!("Speedup: {:.2}x", vec_time.as_nanos() as f64 / small_time.as_nanos() as f64);
}
```

### Memory Usage Comparison

```rust
use smallvec::{SmallVec, smallvec};
use std::mem;

fn analyze_memory_usage() {
    type SmallVec4<T> = SmallVec<[T; 4]>;
    
    // Empty containers
    println!("Empty Vec<i32>: {} bytes", mem::size_of::<Vec<i32>>());
    println!("Empty SmallVec4<i32>: {} bytes", mem::size_of::<SmallVec4<i32>>());
    
    // With 3 elements
    let vec = vec![1, 2, 3];
    let small = smallvec![1, 2, 3];
    
    println!("Vec with 3 elements:");
    println!("  Stack: {} bytes", mem::size_of_val(&vec));
    println!("  Heap: {} bytes", vec.capacity() * mem::size_of::<i32>());
    println!("  Total: {} bytes", mem::size_of_val(&vec) + vec.capacity() * mem::size_of::<i32>());
    
    println!("SmallVec with 3 elements:");
    println!("  Stack: {} bytes", mem::size_of_val(&small));
    println!("  Heap: {} bytes", if small.spilled() { small.capacity() * mem::size_of::<i32>() } else { 0 });
    println!("  Total: {} bytes", mem::size_of_val(&small) + if small.spilled() { small.capacity() * mem::size_of::<i32>() } else { 0 });
}
```

## Memory Layout

### Internal Structure

```rust
use smallvec::SmallVec;
use std::mem;

// SmallVec<[T; N]> internal layout:
// 
// struct SmallVec<T, const N: usize> {
//     len: TaggedLen,           // Length with heap/inline bit
//     raw: RawSmallVec<T, N>,   // Union of inline array or heap pointer
//     _marker: PhantomData<T>,  // Zero-sized type marker
// }
//
// union RawSmallVec<T, const N: usize> {
//     inline: ManuallyDrop<MaybeUninit<[T; N]>>,  // Inline storage
//     heap: (NonNull<T>, usize),                   // Heap pointer + capacity
// }

type SmallVec8<T> = SmallVec<[T; 8]>;

println!("SmallVec8<u8> size: {}", mem::size_of::<SmallVec8<u8>>());
println!("SmallVec8<u32> size: {}", mem::size_of::<SmallVec8<u32>>());
println!("SmallVec8<u64> size: {}", mem::size_of::<SmallVec8<u64>>());

// Size = max(inline_array_size, heap_pointer_size) + metadata
```

### Tagged Length Encoding

```rust
use smallvec::SmallVec;

// For non-ZST types, length is encoded as:
// - Most significant bits: actual length
// - Least significant bit: heap flag (1 = heap, 0 = inline)

type SmallVec4<T> = SmallVec<[T; 4]>;

let mut vec = SmallVec4::new();
vec.extend(0..3);
assert!(!vec.spilled());  // Inline storage

vec.push(3);
assert!(!vec.spilled());  // Still inline

vec.push(4);
assert!(vec.spilled());   // Now on heap
```

### Alignment Considerations

```rust
use smallvec::SmallVec;
use std::mem;

// Alignment affects memory layout
type SmallVecU8 = SmallVec<[u8; 16]>;    // 1-byte aligned
type SmallVecU64 = SmallVec<[u64; 2]>;   // 8-byte aligned

println!("SmallVecU8 align: {}", mem::align_of::<SmallVecU8>());
println!("SmallVecU64 align: {}", mem::align_of::<SmallVecU64>());

// Higher alignment may increase total size due to padding
```

## Optimization Strategies

### Choosing Inline Size

```rust
use smallvec::SmallVec;

// Strategy 1: Profile your application
fn profile_sizes() {
    let mut size_histogram = std::collections::HashMap::new();
    
    // Collect size data from your application
    for vec_size in collect_actual_sizes() {
        *size_histogram.entry(vec_size).or_insert(0) += 1;
    }
    
    // Choose N to cover 80-90% of cases
    let total: usize = size_histogram.values().sum();
    let mut cumulative = 0;
    
    for (size, count) in size_histogram.iter() {
        cumulative += count;
        let percentage = (cumulative as f64 / total as f64) * 100.0;
        println!("Size {}: {:.1}% cumulative", size, percentage);
        
        if percentage >= 90.0 {
            println!("Recommended inline size: {}", size);
            break;
        }
    }
}

// Strategy 2: Common patterns
type TokenList = SmallVec<[Token; 8]>;      // Most expressions have ≤8 tokens
type ArgList<T> = SmallVec<[T; 4]>;         // Most functions have ≤4 args
type ErrorChain = SmallVec<[Error; 3]>;     // Error chains usually shallow
type PathComponents = SmallVec<[String; 6]>; // File paths usually ≤6 components
```

### Memory vs Performance Tradeoffs

```rust
use smallvec::SmallVec;
use std::mem;

// Large inline size: more memory, fewer allocations
type LargeSmallVec<T> = SmallVec<[T; 32]>;

// Small inline size: less memory, more allocations
type TinySmallVec<T> = SmallVec<[T; 2]>;

fn analyze_tradeoffs() {
    println!("LargeSmallVec<i32>: {} bytes", mem::size_of::<LargeSmallVec<i32>>());
    println!("TinySmallVec<i32>: {} bytes", mem::size_of::<TinySmallVec<i32>>());
    println!("Vec<i32>: {} bytes", mem::size_of::<Vec<i32>>());
    
    // Choose based on:
    // - Typical collection sizes in your application
    // - Memory constraints
    // - Allocation frequency
    // - Cache line size (64 bytes on most systems)
}
```

### Specialized Operations

```rust
use smallvec::{SmallVec, smallvec};

// Optimize for specific use cases
impl<T> SmallVec<[T; 4]> {
    // Custom methods for your domain
    fn push_if_space(&mut self, item: T) -> Result<(), T> {
        if self.len() < 4 && !self.spilled() {
            self.push(item);
            Ok(())
        } else {
            Err(item)
        }
    }
    
    fn try_inline_extend<I>(&mut self, iter: I) -> bool 
    where 
        I: IntoIterator<Item = T>,
        I::IntoIter: ExactSizeIterator,
    {
        let iter = iter.into_iter();
        if self.len() + iter.len() <= 4 && !self.spilled() {
            self.extend(iter);
            true
        } else {
            false
        }
    }
}
```

## Integration Patterns

### Compiler Data Structures

```rust
use smallvec::{SmallVec, smallvec};

// AST nodes with small child lists
#[derive(Debug)]
pub struct AstNode {
    pub kind: NodeKind,
    pub children: SmallVec<[AstNode; 4]>,  // Most nodes have ≤4 children
    pub span: Span,
}

// Symbol table with small scope chains
pub struct Scope {
    pub symbols: std::collections::HashMap<String, Symbol>,
    pub parent_scopes: SmallVec<[ScopeId; 3]>,  // Usually ≤3 levels deep
}

// Type parameters (most types have few parameters)
pub struct GenericType {
    pub base: TypeId,
    pub parameters: SmallVec<[Type; 2]>,  // Most generics have 1-2 params
}
```

### Error Handling

```rust
use smallvec::{SmallVec, smallvec};

// Error context chains
#[derive(Debug)]
pub struct ErrorContext {
    pub message: String,
    pub location: SourceLocation,
    pub causes: SmallVec<[Box<ErrorContext>; 2]>,  // Usually shallow
}

impl ErrorContext {
    pub fn new(message: String, location: SourceLocation) -> Self {
        Self {
            message,
            location,
            causes: SmallVec::new(),
        }
    }
    
    pub fn with_cause(mut self, cause: ErrorContext) -> Self {
        self.causes.push(Box::new(cause));
        self
    }
}

// Diagnostic collections
pub struct DiagnosticBatch {
    pub errors: SmallVec<[Diagnostic; 8]>,    // Most files have few errors
    pub warnings: SmallVec<[Diagnostic; 16]>, // More warnings than errors
}
```

### Configuration and Options

```rust
use smallvec::{SmallVec, smallvec};

// Compiler flags and options
pub struct CompilerOptions {
    pub optimization_passes: SmallVec<[OptimizationPass; 6]>,
    pub include_paths: SmallVec<[PathBuf; 4]>,
    pub feature_flags: SmallVec<[String; 8]>,
}

// Plugin systems
pub struct PluginManager {
    pub active_plugins: SmallVec<[Box<dyn Plugin>; 4]>,
    pub plugin_order: SmallVec<[PluginId; 4]>,
}
```

### Parallel Processing

```rust
use smallvec::{SmallVec, smallvec};
use rayon::prelude::*;

// Work-stealing with small task queues
pub struct WorkQueue<T> {
    pub tasks: SmallVec<[T; 16]>,  // Small local queue
}

impl<T: Send> WorkQueue<T> {
    pub fn process_parallel<F, R>(&mut self, f: F) -> SmallVec<[R; 16]>
    where
        F: Fn(T) -> R + Sync + Send,
        R: Send,
    {
        self.tasks
            .par_iter()
            .map(|task| f(*task))
            .collect()
    }
}

// Parallel compilation units
pub fn compile_modules(modules: &[Module]) -> SmallVec<[CompiledModule; 8]> {
    modules
        .par_iter()
        .map(|module| compile_module(module))
        .collect()
}
```

## Benchmarking

### Allocation Benchmarks

```rust
use smallvec::{SmallVec, smallvec};
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn bench_small_collections(c: &mut Criterion) {
    c.bench_function("vec_small", |b| {
        b.iter(|| {
            let mut vec = Vec::new();
            for i in 0..4 {
                vec.push(black_box(i));
            }
            vec
        })
    });
    
    c.bench_function("smallvec_small", |b| {
        b.iter(|| {
            let mut vec = SmallVec::<[i32; 4]>::new();
            for i in 0..4 {
                vec.push(black_box(i));
            }
            vec
        })
    });
    
    c.bench_function("vec_large", |b| {
        b.iter(|| {
            let mut vec = Vec::new();
            for i in 0..100 {
                vec.push(black_box(i));
            }
            vec
        })
    });
    
    c.bench_function("smallvec_large", |b| {
        b.iter(|| {
            let mut vec = SmallVec::<[i32; 4]>::new();
            for i in 0..100 {
                vec.push(black_box(i));
            }
            vec
        })
    });
}

criterion_group!(benches, bench_small_collections);
criterion_main!(benches);
```

### Memory Access Patterns

```rust
use smallvec::{SmallVec, smallvec};
use std::time::Instant;

fn benchmark_access_patterns() {
    const OUTER_COUNT: usize = 10000;
    const INNER_COUNT: usize = 3;
    
    // Create many small collections
    let small_vecs: Vec<SmallVec<[i32; 4]>> = (0..OUTER_COUNT)
        .map(|i| {
            let mut vec = SmallVec::new();
            for j in 0..INNER_COUNT {
                vec.push((i * INNER_COUNT + j) as i32);
            }
            vec
        })
        .collect();
    
    let vecs: Vec<Vec<i32>> = (0..OUTER_COUNT)
        .map(|i| {
            let mut vec = Vec::new();
            for j in 0..INNER_COUNT {
                vec.push((i * INNER_COUNT + j) as i32);
            }
            vec
        })
        .collect();
    
    // Benchmark random access
    let indices: Vec<usize> = (0..1000).map(|_| fastrand::usize(..OUTER_COUNT)).collect();
    
    let start = Instant::now();
    let small_sum: i32 = indices.iter()
        .map(|&i| small_vecs[i].iter().sum::<i32>())
        .sum();
    let small_time = start.elapsed();
    
    let start = Instant::now();
    let vec_sum: i32 = indices.iter()
        .map(|&i| vecs[i].iter().sum::<i32>())
        .sum();
    let vec_time = start.elapsed();
    
    println!("SmallVec random access: {:?}", small_time);
    println!("Vec random access: {:?}", vec_time);
    println!("Speedup: {:.2}x", vec_time.as_nanos() as f64 / small_time.as_nanos() as f64);
}
```

### Size Distribution Analysis

```rust
use smallvec::SmallVec;
use std::collections::HashMap;

fn analyze_size_distribution() {
    // Simulate real-world size distribution
    let sizes = generate_realistic_sizes();
    
    let mut histogram = HashMap::new();
    for size in &sizes {
        *histogram.entry(*size).or_insert(0) += 1;
    }
    
    // Test different inline sizes
    for inline_size in [2, 4, 8, 16, 32] {
        let inline_count = sizes.iter()
            .filter(|&&size| size <= inline_size)
            .count();
        
        let inline_percentage = (inline_count as f64 / sizes.len() as f64) * 100.0;
        
        println!("Inline size {}: {:.1}% would be inline", inline_size, inline_percentage);
    }
}

fn generate_realistic_sizes() -> Vec<usize> {
    // Simulate realistic size distribution
    // Most collections are small, with exponential decay
    let mut sizes = Vec::new();
    
    for _ in 0..10000 {
        let size = if fastrand::f64() < 0.6 {
            fastrand::usize(1..=4)  // 60% are 1-4 elements
        } else if fastrand::f64() < 0.8 {
            fastrand::usize(5..=8)  // 20% are 5-8 elements
        } else if fastrand::f64() < 0.95 {
            fastrand::usize(9..=16) // 15% are 9-16 elements
        } else {
            fastrand::usize(17..=100) // 5% are larger
        };
        sizes.push(size);
    }
    
    sizes
}
```

## Best Practices

### 1. Choose Inline Size Wisely

```rust
use smallvec::SmallVec;

// ✅ Profile your application first
// Measure actual collection sizes in your use case

// ✅ Consider memory vs allocation tradeoffs
type ModerateSmallVec<T> = SmallVec<[T; 8]>;  // Good balance

// ❌ Don't guess or use arbitrary numbers
// type RandomSmallVec<T> = SmallVec<[T; 17]>; // Why 17?

// ✅ Use domain-specific knowledge
type FunctionArgs<T> = SmallVec<[T; 4]>;      // Most functions have ≤4 args
type ErrorChain = SmallVec<[Error; 3]>;       // Error chains usually shallow
```

### 2. Consider Total Memory Usage

```rust
use smallvec::SmallVec;
use std::mem;

// ✅ Be aware of size impact
fn check_sizes() {
    println!("SmallVec<[i32; 4]>: {} bytes", mem::size_of::<SmallVec<[i32; 4]>>());
    println!("SmallVec<[i32; 32]>: {} bytes", mem::size_of::<SmallVec<[i32; 32]>>());
    println!("Vec<i32>: {} bytes", mem::size_of::<Vec<i32>>());
}

// ❌ Don't use huge inline sizes without justification
// type HugeSmallVec<T> = SmallVec<[T; 1000]>; // 4KB+ per instance!

// ✅ Consider cache line size (typically 64 bytes)
type CacheLineSmallVec<T> = SmallVec<[T; 8]>; // ~64 bytes for i64
```

### 3. Handle Spilling Gracefully

```rust
use smallvec::{SmallVec, smallvec};

// ✅ Check spilling status when performance matters
fn process_efficiently(mut vec: SmallVec<[i32; 4]>) {
    if vec.spilled() {
        // Already on heap, normal Vec operations are fine
        vec.reserve(100);
    } else {
        // Still inline, be careful about growth
        if vec.len() + new_items.len() > 4 {
            // Pre-allocate to avoid multiple reallocations
            vec.reserve(vec.len() + new_items.len());
        }
    }
}

// ✅ Use appropriate methods for your use case
fn add_items(vec: &mut SmallVec<[i32; 4]>, items: &[i32]) {
    if items.len() <= 4 && vec.is_empty() {
        // Likely to stay inline
        vec.extend_from_slice(items);
    } else {
        // Likely to spill, reserve space
        vec.reserve(items.len());
        vec.extend_from_slice(items);
    }
}
```

### 4. Use Type Aliases

```rust
use smallvec::SmallVec;

// ✅ Create meaningful type aliases
pub type TokenStream = SmallVec<[Token; 8]>;
pub type ArgumentList<T> = SmallVec<[T; 4]>;
pub type ErrorStack = SmallVec<[Error; 3]>;

// ✅ Document the reasoning
/// Small vector optimized for function argument lists.
/// Inline size of 4 covers 90% of functions in typical codebases.
pub type FunctionArgs<T> = SmallVec<[T; 4]>;

// ✅ Use consistently throughout your codebase
pub struct Function {
    pub name: String,
    pub args: FunctionArgs<Parameter>,
    pub body: TokenStream,
}
```

### 5. Integration with Serde

```rust
use smallvec::{SmallVec, smallvec};
use serde::{Serialize, Deserialize};

// ✅ SmallVec works with serde out of the box
#[derive(Serialize, Deserialize)]
pub struct Config {
    pub flags: SmallVec<[String; 8]>,
    pub paths: SmallVec<[PathBuf; 4]>,
}

// ✅ Serialization preserves logical content, not storage details
let config = Config {
    flags: smallvec!["debug".to_string(), "optimize".to_string()],
    paths: smallvec![PathBuf::from("/usr/lib"), PathBuf::from("/usr/local/lib")],
};

let json = serde_json::to_string(&config).unwrap();
let restored: Config = serde_json::from_str(&json).unwrap();
// restored.flags will be inline if ≤8 elements
```

---

This comprehensive guide covers the essential aspects of using SmallVec for performance optimization in Rust applications. The small vector optimization is particularly valuable in compiler implementations like Parallax, where many small collections (token lists, parameter lists, etc.) are created and destroyed frequently during compilation phases.