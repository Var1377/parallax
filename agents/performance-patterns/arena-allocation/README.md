# Arena Allocation with Bumpalo

## Table of Contents

1. [Introduction](#introduction)
2. [Core Concepts](#core-concepts)
3. [Basic Usage](#basic-usage)
4. [Advanced Patterns](#advanced-patterns)
5. [Performance Characteristics](#performance-characteristics)
6. [Memory Layout](#memory-layout)
7. [Best Practices](#best-practices)
8. [Integration with Parallax](#integration-with-parallax)
9. [Benchmarking](#benchmarking)
10. [Common Pitfalls](#common-pitfalls)

## Introduction

Arena allocation (also known as bump allocation) is a memory management strategy where allocations are made by simply incrementing a pointer within a pre-allocated memory region. The `bumpalo` crate provides a fast, efficient arena allocator for Rust that can dramatically improve performance in scenarios with many short-lived allocations.

### Key Benefits

- **Extremely fast allocation**: O(1) allocation with minimal overhead
- **Excellent cache locality**: Sequential allocations are memory-adjacent
- **Bulk deallocation**: Free entire arena at once
- **No fragmentation**: Linear allocation pattern prevents memory fragmentation
- **Predictable performance**: No garbage collection pauses or complex allocation algorithms

### When to Use Arena Allocation

✅ **Ideal for:**
- Compiler phases (parsing, type checking, code generation)
- Request processing in web servers
- Game frame processing
- Temporary data structures
- Batch processing operations

❌ **Avoid when:**
- Long-lived objects with different lifetimes
- Objects that need individual deallocation
- Memory usage is unpredictable
- Objects with complex drop semantics

## Core Concepts

### The Bump Allocator

```rust
use bumpalo::Bump;

// Create a new arena
let bump = Bump::new();

// Allocate values - they live as long as the arena
let x = bump.alloc(42);
let y = bump.alloc("hello");
let z = bump.alloc(vec![1, 2, 3]);

// All allocations are freed when `bump` is dropped
```

### Memory Layout

```
Arena Memory Layout:
┌─────────────────────────────────────────────────────┐
│ [allocated data] ← ptr    [free space]    footer → │
└─────────────────────────────────────────────────────┘
                   ↑
              bump pointer
```

The bump pointer moves backward (toward lower addresses) with each allocation, ensuring proper alignment and efficient space usage.

### Chunk Management

Bumpalo automatically manages memory chunks:

```rust
// Small initial chunk
let bump = Bump::new();

// Grows automatically as needed
for i in 0..1000 {
    bump.alloc(i);
}

// Multiple chunks linked together
// Each chunk is sized optimally for the allocator
```

## Basic Usage

### Simple Allocation

```rust
use bumpalo::Bump;

let bump = Bump::new();

// Allocate primitive types
let number = bump.alloc(42i32);
let flag = bump.alloc(true);

// Allocate strings
let text = bump.alloc_str("Hello, arena!");

// Allocate slices
let slice = bump.alloc_slice_copy(&[1, 2, 3, 4, 5]);

println!("Number: {}, Text: {}", number, text);
```

### Working with Collections

```rust
use bumpalo::{Bump, collections::Vec};

let bump = Bump::new();

// Use bumpalo's Vec instead of std::vec::Vec
let mut vec = Vec::new_in(&bump);
vec.push(1);
vec.push(2);
vec.push(3);

// Convert to slice allocated in the arena
let slice = vec.into_bump_slice();
```

### Allocation Methods

```rust
use bumpalo::Bump;

let bump = Bump::new();

// Direct allocation
let x = bump.alloc(42);

// Allocation with initialization function
let y = bump.alloc_with(|| expensive_computation());

// Fallible allocation
let z = bump.try_alloc(large_object()).unwrap();

// Allocation with fallible initialization
let w = bump.alloc_try_with(|| {
    fallible_operation().map_err(|e| format!("Failed: {}", e))
}).unwrap();
```

## Advanced Patterns

### Custom Minimum Alignment

```rust
use bumpalo::Bump;

// Create arena with 8-byte minimum alignment
type AlignedBump = Bump<8>;
let bump = AlignedBump::with_min_align();

// All allocations will be 8-byte aligned
let x = bump.alloc(1u8); // Still 8-byte aligned despite being 1 byte
assert_eq!((x as *const u8 as usize) % 8, 0);
```

### Allocation Limits

```rust
use bumpalo::Bump;

let bump = Bump::new();

// Set a 1MB allocation limit
bump.set_allocation_limit(Some(1024 * 1024));

// This will succeed
let small = bump.try_alloc([0u8; 1000]);
assert!(small.is_ok());

// This will fail if it exceeds the limit
let large = bump.try_alloc([0u8; 2_000_000]);
assert!(large.is_err());
```

### Arena Reset for Reuse

```rust
use bumpalo::Bump;

let mut bump = Bump::new();

// First batch of allocations
{
    let data1 = bump.alloc(vec![1, 2, 3]);
    let data2 = bump.alloc("temporary");
    // Use data...
}

// Reset arena for reuse
bump.reset();

// Second batch - reuses the same memory
{
    let data3 = bump.alloc(vec![4, 5, 6]);
    let data4 = bump.alloc("new data");
    // Use data...
}
```

### Working with Complex Types

```rust
use bumpalo::Bump;

#[derive(Debug)]
struct Node<'a> {
    value: i32,
    children: Vec<&'a Node<'a>>,
}

impl<'a> Node<'a> {
    fn new_in(bump: &'a Bump, value: i32) -> &'a mut Self {
        bump.alloc(Node {
            value,
            children: Vec::new(),
        })
    }
    
    fn add_child(&mut self, child: &'a Node<'a>) {
        self.children.push(child);
    }
}

let bump = Bump::new();

// Create tree structure in arena
let root = Node::new_in(&bump, 1);
let child1 = Node::new_in(&bump, 2);
let child2 = Node::new_in(&bump, 3);

root.add_child(child1);
root.add_child(child2);
```

## Performance Characteristics

### Allocation Performance

```rust
use bumpalo::Bump;
use std::time::Instant;

fn benchmark_allocations() {
    let bump = Bump::new();
    let start = Instant::now();
    
    // Extremely fast - just pointer arithmetic
    for i in 0..1_000_000 {
        bump.alloc(i);
    }
    
    let duration = start.elapsed();
    println!("1M allocations in {:?}", duration);
    // Typically < 10ms on modern hardware
}
```

### Memory Overhead

```rust
use bumpalo::Bump;

let bump = Bump::new();

// Minimal per-allocation overhead
// Only alignment padding, no metadata per allocation
let data = bump.alloc([0u8; 1000]);

// Check actual memory usage
println!("Allocated bytes: {}", bump.allocated_bytes());
println!("Chunk capacity: {}", bump.chunk_capacity());
```

### Cache Performance

Arena allocation provides excellent cache locality:

```rust
use bumpalo::Bump;

let bump = Bump::new();

// Sequential allocations are memory-adjacent
let array: Vec<&i32> = (0..1000)
    .map(|i| bump.alloc(i))
    .collect();

// Iterating over these has excellent cache performance
let sum: i32 = array.iter().map(|&&x| x).sum();
```

## Memory Layout

### Chunk Structure

```rust
use bumpalo::Bump;

let bump = Bump::new();

// Each chunk has this structure:
// ┌─────────────────────────────────────────────────────┐
// │ [user data] ← ptr    [free space]    [ChunkFooter] │
// └─────────────────────────────────────────────────────┘

// Footer contains:
// - Pointer to chunk start
// - Layout information
// - Link to previous chunk
// - Current bump pointer
// - Total allocated bytes
```

### Alignment Handling

```rust
use bumpalo::Bump;
use std::mem::align_of;

let bump = Bump::new();

// Different types get proper alignment
let byte = bump.alloc(1u8);      // 1-byte aligned
let int = bump.alloc(1i32);      // 4-byte aligned  
let long = bump.alloc(1i64);     // 8-byte aligned

// Alignment is handled automatically
assert_eq!((byte as *const u8 as usize) % align_of::<u8>(), 0);
assert_eq!((int as *const i32 as usize) % align_of::<i32>(), 0);
assert_eq!((long as *const i64 as usize) % align_of::<i64>(), 0);
```

### Chunk Growth Strategy

```rust
use bumpalo::Bump;

// Initial chunk: ~512 bytes
let bump = Bump::new();

// Grows exponentially up to page size
// Then grows by page-sized increments
// Optimized for allocator efficiency

// You can pre-allocate for known workloads
let bump_large = Bump::with_capacity(1024 * 1024); // 1MB
```

## Best Practices

### 1. Scope Arena Lifetime Appropriately

```rust
use bumpalo::Bump;

// ✅ Good: Arena lifetime matches usage
fn process_request(data: &[u8]) -> String {
    let bump = Bump::new();
    
    // All temporary allocations in arena
    let parsed = parse_data_in(&bump, data);
    let processed = process_in(&bump, parsed);
    
    // Return owned result, arena is dropped
    processed.to_string()
}

// ❌ Bad: Arena outlives its usefulness
static GLOBAL_ARENA: Bump = Bump::new(); // Never freed!
```

### 2. Use Appropriate Collection Types

```rust
use bumpalo::{Bump, collections::{Vec, String}};

let bump = Bump::new();

// ✅ Use bumpalo collections for arena-allocated data
let mut arena_vec = Vec::new_in(&bump);
let mut arena_string = String::new_in(&bump);

// ❌ Don't use std collections with arena-allocated data
// let mut std_vec = std::vec::Vec::new(); // Wrong allocator!
```

### 3. Handle Drop Semantics Carefully

```rust
use bumpalo::Bump;
use std::fs::File;

let bump = Bump::new();

// ⚠️ Be careful with types that need Drop
let file = bump.alloc(File::open("data.txt").unwrap());
// File won't be closed automatically!

// ✅ Manually handle cleanup if needed
{
    let file = File::open("data.txt").unwrap();
    let data = process_file(&file);
    let result = bump.alloc(data);
    // file is dropped here
}
```

### 4. Optimize for Access Patterns

```rust
use bumpalo::Bump;

let bump = Bump::new();

// ✅ Allocate related data together for cache locality
struct ProcessingContext<'a> {
    input: &'a [u8],
    temp_buffer: &'a mut [u8],
    results: &'a mut Vec<i32>,
}

let context = bump.alloc(ProcessingContext {
    input: bump.alloc_slice_copy(input_data),
    temp_buffer: bump.alloc_slice_fill_default(1024),
    results: bump.alloc(Vec::new()),
});
```

## Integration with Parallax

### Compiler Phase Integration

```rust
use bumpalo::Bump;

// Example: Parser phase
pub struct Parser<'arena> {
    arena: &'arena Bump,
    tokens: &'arena [Token],
    current: usize,
}

impl<'arena> Parser<'arena> {
    pub fn new(arena: &'arena Bump, tokens: &'arena [Token]) -> Self {
        Self { arena, tokens, current: 0 }
    }
    
    pub fn parse_expression(&mut self) -> &'arena Expr {
        // All AST nodes allocated in arena
        match self.current_token() {
            Token::Number(n) => {
                self.advance();
                self.arena.alloc(Expr::Literal(*n))
            }
            Token::Identifier(name) => {
                self.advance();
                self.arena.alloc(Expr::Variable(name))
            }
            // ... more cases
        }
    }
}

// Usage in compiler pipeline
pub fn compile_phase(source: &str) -> CompiledResult {
    let arena = Bump::new();
    
    // Tokenize
    let tokens = tokenize_in(&arena, source);
    
    // Parse
    let mut parser = Parser::new(&arena, tokens);
    let ast = parser.parse();
    
    // Type check
    let typed_ast = type_check_in(&arena, ast);
    
    // Generate code (arena automatically freed)
    generate_code(typed_ast)
}
```

### Parallel Processing with Arena

```rust
use bumpalo::Bump;
use rayon::prelude::*;

// Each thread gets its own arena
pub fn parallel_process(items: &[InputItem]) -> Vec<OutputItem> {
    items.par_iter()
        .map(|item| {
            let arena = Bump::new();
            
            // Process item using arena for temporaries
            let temp_data = process_in(&arena, item);
            let result = finalize(temp_data);
            
            // Arena freed automatically at end of closure
            result
        })
        .collect()
}
```

### Memory Pool Pattern

```rust
use bumpalo::Bump;
use std::sync::Mutex;

pub struct ArenaPool {
    arenas: Mutex<Vec<Bump>>,
}

impl ArenaPool {
    pub fn new() -> Self {
        Self {
            arenas: Mutex::new(Vec::new()),
        }
    }
    
    pub fn get_arena(&self) -> Bump {
        let mut arenas = self.arenas.lock().unwrap();
        arenas.pop().unwrap_or_else(|| Bump::new())
    }
    
    pub fn return_arena(&self, mut arena: Bump) {
        arena.reset();
        let mut arenas = self.arenas.lock().unwrap();
        if arenas.len() < 10 { // Limit pool size
            arenas.push(arena);
        }
    }
}
```

## Benchmarking

### Allocation Speed Comparison

```rust
use bumpalo::Bump;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn bench_allocations(c: &mut Criterion) {
    c.bench_function("std_vec_allocations", |b| {
        b.iter(|| {
            let mut vec = Vec::new();
            for i in 0..1000 {
                vec.push(black_box(i));
            }
            vec
        })
    });
    
    c.bench_function("arena_allocations", |b| {
        b.iter(|| {
            let bump = Bump::new();
            let mut results = Vec::new();
            for i in 0..1000 {
                results.push(bump.alloc(black_box(i)));
            }
            results
        })
    });
}

criterion_group!(benches, bench_allocations);
criterion_main!(benches);
```

### Memory Usage Profiling

```rust
use bumpalo::Bump;

fn profile_memory_usage() {
    let bump = Bump::new();
    
    println!("Initial state:");
    println!("  Allocated: {} bytes", bump.allocated_bytes());
    println!("  Capacity: {} bytes", bump.chunk_capacity());
    
    // Allocate some data
    for i in 0..1000 {
        bump.alloc(i);
    }
    
    println!("After 1000 allocations:");
    println!("  Allocated: {} bytes", bump.allocated_bytes());
    println!("  Capacity: {} bytes", bump.chunk_capacity());
    
    // Check chunk iteration
    let chunk_count = bump.iter_allocated_chunks().count();
    println!("  Chunks: {}", chunk_count);
}
```

### Cache Performance Testing

```rust
use bumpalo::Bump;
use std::time::Instant;

fn test_cache_performance() {
    let bump = Bump::new();
    
    // Allocate 1000 integers in arena (sequential)
    let arena_data: Vec<&i32> = (0..1000)
        .map(|i| bump.alloc(i))
        .collect();
    
    // Allocate 1000 integers on heap (scattered)
    let heap_data: Vec<Box<i32>> = (0..1000)
        .map(|i| Box::new(i))
        .collect();
    
    // Time sequential access (arena should be faster)
    let start = Instant::now();
    let arena_sum: i32 = arena_data.iter().map(|&&x| x).sum();
    let arena_time = start.elapsed();
    
    let start = Instant::now();
    let heap_sum: i32 = heap_data.iter().map(|x| **x).sum();
    let heap_time = start.elapsed();
    
    println!("Arena access: {:?}", arena_time);
    println!("Heap access: {:?}", heap_time);
    println!("Speedup: {:.2}x", heap_time.as_nanos() as f64 / arena_time.as_nanos() as f64);
}
```

## Common Pitfalls

### 1. Lifetime Issues

```rust
use bumpalo::Bump;

// ❌ This won't compile - lifetime mismatch
fn bad_example() -> &'static str {
    let bump = Bump::new();
    let s = bump.alloc_str("hello");
    s // Error: s doesn't live long enough
}

// ✅ Correct approach
fn good_example<'a>(bump: &'a Bump) -> &'a str {
    bump.alloc_str("hello")
}
```

### 2. Mixing Allocators

```rust
use bumpalo::{Bump, collections::Vec as BumpVec};

let bump = Bump::new();

// ❌ Don't mix std and bumpalo collections
let mut std_vec = Vec::new();
let arena_data = bump.alloc(42);
// std_vec.push(arena_data); // Lifetime issues!

// ✅ Use consistent allocators
let mut bump_vec = BumpVec::new_in(&bump);
let arena_data = bump.alloc(42);
bump_vec.push(arena_data); // OK
```

### 3. Forgetting Drop Semantics

```rust
use bumpalo::Bump;
use std::sync::Mutex;

let bump = Bump::new();

// ⚠️ Mutex won't be properly dropped
let mutex = bump.alloc(Mutex::new(42));
// Need manual cleanup if required

// ✅ Use RAII wrapper if needed
struct ArenaGuard<'a, T> {
    value: &'a mut T,
}

impl<T> Drop for ArenaGuard<'_, T> {
    fn drop(&mut self) {
        // Custom cleanup logic
    }
}
```

### 4. Excessive Memory Usage

```rust
use bumpalo::Bump;

// ❌ Arena grows indefinitely
fn process_stream(stream: impl Iterator<Item = Data>) {
    let bump = Bump::new();
    
    for item in stream {
        let processed = process_in(&bump, item);
        // Arena never shrinks!
    }
}

// ✅ Reset arena periodically
fn process_stream_better(stream: impl Iterator<Item = Data>) {
    let mut bump = Bump::new();
    
    for (i, item) in stream.enumerate() {
        let processed = process_in(&bump, item);
        
        // Reset every 1000 items
        if i % 1000 == 999 {
            bump.reset();
        }
    }
}
```

---

This documentation provides a comprehensive guide to using bumpalo for arena allocation in performance-critical Rust applications. The patterns and examples shown here are particularly relevant for compiler implementations like Parallax, where fast allocation and deallocation of temporary data structures is crucial for performance.