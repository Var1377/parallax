# Memory Layout Optimization and Cache-Friendly Data Structures

## Table of Contents

1. [Introduction](#introduction)
2. [CPU Cache Fundamentals](#cpu-cache-fundamentals)
3. [Memory Layout Principles](#memory-layout-principles)
4. [Data Structure Optimization](#data-structure-optimization)
5. [Cache-Friendly Patterns](#cache-friendly-patterns)
6. [SIMD and Vectorization](#simd-and-vectorization)
7. [Memory Alignment](#memory-alignment)
8. [Benchmarking Cache Performance](#benchmarking-cache-performance)
9. [Compiler Optimizations](#compiler-optimizations)
10. [Best Practices](#best-practices)

## Introduction

Memory layout optimization is crucial for high-performance applications. Modern CPUs are orders of magnitude faster than memory access, making cache efficiency the primary bottleneck in many applications. This guide covers techniques for optimizing data structures and memory access patterns to maximize cache utilization.

### Performance Impact

```rust
use std::time::Instant;

// Cache-friendly: Array of Structures (AoS)
#[derive(Clone)]
struct Point3D {
    x: f32,
    y: f32,
    z: f32,
}

// Cache-unfriendly for certain operations
fn process_aos(points: &mut [Point3D]) {
    for point in points {
        point.x *= 2.0;  // Touches x, y, z even though we only need x
    }
}

// Cache-friendly: Structure of Arrays (SoA)
struct Points3D {
    x: Vec<f32>,
    y: Vec<f32>,
    z: Vec<f32>,
}

fn process_soa(points: &mut Points3D) {
    for x in &mut points.x {
        *x *= 2.0;  // Only touches x values, better cache utilization
    }
}
```

## CPU Cache Fundamentals

### Cache Hierarchy

Modern CPUs have multiple cache levels:

```rust
// Typical cache sizes (varies by CPU)
// L1 Cache: 32KB data + 32KB instruction (per core)
// L2 Cache: 256KB-1MB (per core)  
// L3 Cache: 8MB-32MB (shared)
// Main Memory: GB-TB

const L1_CACHE_SIZE: usize = 32 * 1024;      // 32KB
const L2_CACHE_SIZE: usize = 256 * 1024;     // 256KB
const L3_CACHE_SIZE: usize = 8 * 1024 * 1024; // 8MB
const CACHE_LINE_SIZE: usize = 64;            // 64 bytes
```

### Cache Line Behavior

```rust
use std::time::Instant;

// Demonstrate cache line effects
fn demonstrate_cache_lines() {
    const SIZE: usize = 1024 * 1024; // 1M elements
    let mut data = vec![0u8; SIZE];
    
    // Sequential access (cache-friendly)
    let start = Instant::now();
    for i in 0..SIZE {
        data[i] = (i % 256) as u8;
    }
    let sequential_time = start.elapsed();
    
    // Strided access (cache-unfriendly)
    let start = Instant::now();
    let stride = 4096; // Jump by 4KB each time
    for i in (0..SIZE).step_by(stride) {
        data[i] = (i % 256) as u8;
    }
    let strided_time = start.elapsed();
    
    println!("Sequential: {:?}", sequential_time);
    println!("Strided: {:?}", strided_time);
    println!("Ratio: {:.2}x", strided_time.as_nanos() as f64 / sequential_time.as_nanos() as f64);
}
```

### False Sharing

```rust
use std::sync::Arc;
use std::thread;
use std::time::Instant;

// False sharing example
#[repr(align(64))] // Align to cache line boundary
struct AlignedCounter {
    value: std::sync::atomic::AtomicU64,
}

struct UnalignedCounter {
    value: std::sync::atomic::AtomicU64,
}

fn demonstrate_false_sharing() {
    const ITERATIONS: u64 = 1_000_000;
    
    // Test with potential false sharing
    let counters = Arc::new([
        UnalignedCounter { value: std::sync::atomic::AtomicU64::new(0) },
        UnalignedCounter { value: std::sync::atomic::AtomicU64::new(0) },
    ]);
    
    let start = Instant::now();
    let handles: Vec<_> = (0..2).map(|i| {
        let counters = Arc::clone(&counters);
        thread::spawn(move || {
            for _ in 0..ITERATIONS {
                counters[i].value.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            }
        })
    }).collect();
    
    for handle in handles {
        handle.join().unwrap();
    }
    let unaligned_time = start.elapsed();
    
    // Test with cache line alignment
    let aligned_counters = Arc::new([
        AlignedCounter { value: std::sync::atomic::AtomicU64::new(0) },
        AlignedCounter { value: std::sync::atomic::AtomicU64::new(0) },
    ]);
    
    let start = Instant::now();
    let handles: Vec<_> = (0..2).map(|i| {
        let counters = Arc::clone(&aligned_counters);
        thread::spawn(move || {
            for _ in 0..ITERATIONS {
                counters[i].value.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            }
        })
    }).collect();
    
    for handle in handles {
        handle.join().unwrap();
    }
    let aligned_time = start.elapsed();
    
    println!("Unaligned: {:?}", unaligned_time);
    println!("Aligned: {:?}", aligned_time);
    println!("Speedup: {:.2}x", unaligned_time.as_nanos() as f64 / aligned_time.as_nanos() as f64);
}
```

## Memory Layout Principles

### Structure of Arrays vs Array of Structures

```rust
// Array of Structures (AoS) - traditional approach
#[derive(Clone, Copy)]
struct Particle {
    x: f32,
    y: f32,
    z: f32,
    vx: f32,
    vy: f32,
    vz: f32,
    mass: f32,
    charge: f32,
}

type ParticlesAoS = Vec<Particle>;

// Structure of Arrays (SoA) - cache-optimized
struct ParticlesSoA {
    x: Vec<f32>,
    y: Vec<f32>,
    z: Vec<f32>,
    vx: Vec<f32>,
    vy: Vec<f32>,
    vz: Vec<f32>,
    mass: Vec<f32>,
    charge: Vec<f32>,
}

impl ParticlesSoA {
    fn new(capacity: usize) -> Self {
        Self {
            x: Vec::with_capacity(capacity),
            y: Vec::with_capacity(capacity),
            z: Vec::with_capacity(capacity),
            vx: Vec::with_capacity(capacity),
            vy: Vec::with_capacity(capacity),
            vz: Vec::with_capacity(capacity),
            mass: Vec::with_capacity(capacity),
            charge: Vec::with_capacity(capacity),
        }
    }
    
    fn push(&mut self, particle: Particle) {
        self.x.push(particle.x);
        self.y.push(particle.y);
        self.z.push(particle.z);
        self.vx.push(particle.vx);
        self.vy.push(particle.vy);
        self.vz.push(particle.vz);
        self.mass.push(particle.mass);
        self.charge.push(particle.charge);
    }
    
    fn len(&self) -> usize {
        self.x.len()
    }
}

// Benchmark: Update only positions
fn update_positions_aos(particles: &mut ParticlesAoS, dt: f32) {
    for particle in particles {
        particle.x += particle.vx * dt;
        particle.y += particle.vy * dt;
        particle.z += particle.vz * dt;
    }
}

fn update_positions_soa(particles: &mut ParticlesSoA, dt: f32) {
    for i in 0..particles.len() {
        particles.x[i] += particles.vx[i] * dt;
        particles.y[i] += particles.vy[i] * dt;
        particles.z[i] += particles.vz[i] * dt;
    }
}

// SoA version can be vectorized more easily
fn update_positions_soa_vectorized(particles: &mut ParticlesSoA, dt: f32) {
    use std::simd::f32x4;
    
    let dt_vec = f32x4::splat(dt);
    let len = particles.len();
    let chunks = len / 4;
    
    for i in 0..chunks {
        let base = i * 4;
        
        // Load 4 positions and velocities at once
        let x = f32x4::from_slice(&particles.x[base..base + 4]);
        let vx = f32x4::from_slice(&particles.vx[base..base + 4]);
        
        // Vectorized update
        let new_x = x + vx * dt_vec;
        new_x.copy_to_slice(&mut particles.x[base..base + 4]);
        
        // Repeat for y and z...
    }
    
    // Handle remaining elements
    for i in chunks * 4..len {
        particles.x[i] += particles.vx[i] * dt;
        particles.y[i] += particles.vy[i] * dt;
        particles.z[i] += particles.vz[i] * dt;
    }
}
```

### Data Packing and Alignment

```rust
use std::mem;

// Poorly packed structure
#[derive(Debug)]
struct BadPacking {
    a: u8,    // 1 byte
    b: u64,   // 8 bytes (7 bytes padding before this)
    c: u8,    // 1 byte
    d: u32,   // 4 bytes (3 bytes padding before this)
}

// Well-packed structure
#[derive(Debug)]
struct GoodPacking {
    b: u64,   // 8 bytes
    d: u32,   // 4 bytes
    a: u8,    // 1 byte
    c: u8,    // 1 byte (2 bytes padding at end)
}

// Explicitly packed structure
#[repr(packed)]
#[derive(Debug)]
struct PackedStruct {
    a: u8,
    b: u64,
    c: u8,
    d: u32,
}

fn analyze_packing() {
    println!("BadPacking size: {} bytes", mem::size_of::<BadPacking>());
    println!("GoodPacking size: {} bytes", mem::size_of::<GoodPacking>());
    println!("PackedStruct size: {} bytes", mem::size_of::<PackedStruct>());
    
    println!("BadPacking align: {} bytes", mem::align_of::<BadPacking>());
    println!("GoodPacking align: {} bytes", mem::align_of::<GoodPacking>());
    println!("PackedStruct align: {} bytes", mem::align_of::<PackedStruct>());
}

// Custom alignment
#[repr(align(64))] // Align to cache line
struct CacheLineAligned {
    data: [u8; 64],
}

#[repr(align(32))] // Align to 32 bytes for SIMD
struct SimdAligned {
    data: [f32; 8],
}
```

### Bit Packing for Compact Storage

```rust
// Bit-packed structure for flags
#[derive(Clone, Copy)]
struct CompilerFlags {
    bits: u32,
}

impl CompilerFlags {
    const OPTIMIZE: u32 = 1 << 0;
    const DEBUG: u32 = 1 << 1;
    const WARNINGS: u32 = 1 << 2;
    const STRICT: u32 = 1 << 3;
    
    fn new() -> Self {
        Self { bits: 0 }
    }
    
    fn set_optimize(&mut self, value: bool) {
        if value {
            self.bits |= Self::OPTIMIZE;
        } else {
            self.bits &= !Self::OPTIMIZE;
        }
    }
    
    fn is_optimize(&self) -> bool {
        (self.bits & Self::OPTIMIZE) != 0
    }
    
    // ... similar methods for other flags
}

// Bit-packed enum for small values
#[repr(u8)]
#[derive(Clone, Copy, Debug)]
enum TokenType {
    Identifier = 0,
    Number = 1,
    String = 2,
    Operator = 3,
    Keyword = 4,
    // ... up to 255 variants
}

// Compact token representation
#[derive(Clone, Copy)]
struct CompactToken {
    // Pack type (8 bits) and length (24 bits) into u32
    type_and_length: u32,
    offset: u32,
}

impl CompactToken {
    fn new(token_type: TokenType, length: u32, offset: u32) -> Self {
        assert!(length < (1 << 24), "Length too large");
        Self {
            type_and_length: (token_type as u32) | (length << 8),
            offset,
        }
    }
    
    fn token_type(&self) -> TokenType {
        unsafe { std::mem::transmute((self.type_and_length & 0xFF) as u8) }
    }
    
    fn length(&self) -> u32 {
        self.type_and_length >> 8
    }
    
    fn offset(&self) -> u32 {
        self.offset
    }
}
```

## Data Structure Optimization

### Cache-Friendly Linked Structures

```rust
// Traditional linked list (cache-unfriendly)
struct ListNode<T> {
    data: T,
    next: Option<Box<ListNode<T>>>,
}

// Cache-friendly alternative: chunked list
struct ChunkedList<T> {
    chunks: Vec<Chunk<T>>,
    chunk_size: usize,
}

struct Chunk<T> {
    data: Vec<T>,
    next_chunk: Option<usize>,
}

impl<T> ChunkedList<T> {
    fn new(chunk_size: usize) -> Self {
        Self {
            chunks: Vec::new(),
            chunk_size,
        }
    }
    
    fn push(&mut self, item: T) {
        if self.chunks.is_empty() || 
           self.chunks.last().unwrap().data.len() >= self.chunk_size {
            // Create new chunk
            let chunk = Chunk {
                data: vec![item],
                next_chunk: None,
            };
            
            if let Some(last_chunk) = self.chunks.last_mut() {
                last_chunk.next_chunk = Some(self.chunks.len());
            }
            
            self.chunks.push(chunk);
        } else {
            // Add to existing chunk
            self.chunks.last_mut().unwrap().data.push(item);
        }
    }
    
    fn iter(&self) -> ChunkedListIter<T> {
        ChunkedListIter {
            chunks: &self.chunks,
            chunk_index: 0,
            item_index: 0,
        }
    }
}

struct ChunkedListIter<'a, T> {
    chunks: &'a [Chunk<T>],
    chunk_index: usize,
    item_index: usize,
}

impl<'a, T> Iterator for ChunkedListIter<'a, T> {
    type Item = &'a T;
    
    fn next(&mut self) -> Option<Self::Item> {
        if self.chunk_index >= self.chunks.len() {
            return None;
        }
        
        let chunk = &self.chunks[self.chunk_index];
        if self.item_index >= chunk.data.len() {
            // Move to next chunk
            self.chunk_index += 1;
            self.item_index = 0;
            return self.next();
        }
        
        let item = &chunk.data[self.item_index];
        self.item_index += 1;
        Some(item)
    }
}
```

### Memory Pool Allocation

```rust
use std::alloc::{alloc, dealloc, Layout};
use std::ptr::NonNull;

struct MemoryPool<T> {
    chunks: Vec<PoolChunk<T>>,
    chunk_size: usize,
    free_list: Option<NonNull<T>>,
}

struct PoolChunk<T> {
    memory: NonNull<T>,
    layout: Layout,
}

impl<T> MemoryPool<T> {
    fn new(chunk_size: usize) -> Self {
        Self {
            chunks: Vec::new(),
            chunk_size,
            free_list: None,
        }
    }
    
    fn allocate(&mut self) -> Option<NonNull<T>> {
        // Try free list first
        if let Some(ptr) = self.free_list {
            unsafe {
                self.free_list = *(ptr.as_ptr() as *mut Option<NonNull<T>>);
                Some(ptr)
            }
        } else {
            // Allocate new chunk if needed
            self.allocate_new_chunk()
        }
    }
    
    fn deallocate(&mut self, ptr: NonNull<T>) {
        unsafe {
            // Add to free list
            *(ptr.as_ptr() as *mut Option<NonNull<T>>) = self.free_list;
            self.free_list = Some(ptr);
        }
    }
    
    fn allocate_new_chunk(&mut self) -> Option<NonNull<T>> {
        let layout = Layout::array::<T>(self.chunk_size).ok()?;
        
        unsafe {
            let memory = alloc(layout) as *mut T;
            let memory = NonNull::new(memory)?;
            
            // Initialize free list for this chunk
            for i in 0..self.chunk_size - 1 {
                let current = memory.as_ptr().add(i);
                let next = memory.as_ptr().add(i + 1);
                *(current as *mut *mut T) = next;
            }
            
            // Last element points to current free list
            let last = memory.as_ptr().add(self.chunk_size - 1);
            *(last as *mut Option<NonNull<T>>) = self.free_list;
            
            self.chunks.push(PoolChunk { memory, layout });
            
            // Return first element, update free list to second
            self.free_list = Some(NonNull::new_unchecked(memory.as_ptr().add(1)));
            Some(memory)
        }
    }
}

impl<T> Drop for MemoryPool<T> {
    fn drop(&mut self) {
        for chunk in &self.chunks {
            unsafe {
                dealloc(chunk.memory.as_ptr() as *mut u8, chunk.layout);
            }
        }
    }
}
```

### Interned Strings for Memory Efficiency

```rust
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct InternedString {
    id: usize,
}

pub struct StringInterner {
    strings: Vec<String>,
    map: HashMap<String, usize>,
}

impl StringInterner {
    pub fn new() -> Self {
        Self {
            strings: Vec::new(),
            map: HashMap::new(),
        }
    }
    
    pub fn intern(&mut self, s: &str) -> InternedString {
        if let Some(&id) = self.map.get(s) {
            InternedString { id }
        } else {
            let id = self.strings.len();
            self.strings.push(s.to_string());
            self.map.insert(s.to_string(), id);
            InternedString { id }
        }
    }
    
    pub fn resolve(&self, interned: &InternedString) -> &str {
        &self.strings[interned.id]
    }
}

// Thread-safe version
pub struct ThreadSafeInterner {
    inner: Arc<Mutex<StringInterner>>,
}

impl ThreadSafeInterner {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(Mutex::new(StringInterner::new())),
        }
    }
    
    pub fn intern(&self, s: &str) -> InternedString {
        self.inner.lock().unwrap().intern(s)
    }
    
    pub fn resolve(&self, interned: &InternedString) -> String {
        self.inner.lock().unwrap().resolve(interned).to_string()
    }
}

// Usage in compiler
struct Symbol {
    name: InternedString,
    type_info: TypeInfo,
}

struct SymbolTable {
    interner: StringInterner,
    symbols: HashMap<InternedString, Symbol>,
}

impl SymbolTable {
    fn declare(&mut self, name: &str, type_info: TypeInfo) {
        let interned_name = self.interner.intern(name);
        let symbol = Symbol {
            name: interned_name.clone(),
            type_info,
        };
        self.symbols.insert(interned_name, symbol);
    }
}
```

## Cache-Friendly Patterns

### Data-Oriented Design

```rust
// Traditional object-oriented approach
struct GameObject {
    position: Vec3,
    velocity: Vec3,
    health: f32,
    damage: f32,
    ai_state: AIState,
    render_data: RenderData,
}

// Data-oriented approach
struct GameWorld {
    // Hot data (accessed every frame)
    positions: Vec<Vec3>,
    velocities: Vec<Vec3>,
    
    // Warm data (accessed occasionally)
    health: Vec<f32>,
    damage: Vec<f32>,
    
    // Cold data (accessed rarely)
    ai_states: Vec<AIState>,
    render_data: Vec<RenderData>,
    
    // Entity management
    entity_count: usize,
    free_entities: Vec<usize>,
}

impl GameWorld {
    fn update_movement(&mut self, dt: f32) {
        // Only touch hot data - excellent cache utilization
        for i in 0..self.entity_count {
            self.positions[i] += self.velocities[i] * dt;
        }
    }
    
    fn update_combat(&mut self) {
        // Only touch combat-related data when needed
        for i in 0..self.entity_count {
            if self.health[i] <= 0.0 {
                self.destroy_entity(i);
            }
        }
    }
    
    fn destroy_entity(&mut self, index: usize) {
        // Swap with last entity to maintain dense packing
        let last = self.entity_count - 1;
        if index != last {
            self.positions.swap(index, last);
            self.velocities.swap(index, last);
            self.health.swap(index, last);
            self.damage.swap(index, last);
            self.ai_states.swap(index, last);
            self.render_data.swap(index, last);
        }
        self.entity_count -= 1;
        self.free_entities.push(index);
    }
}
```

### Hot/Cold Data Separation

```rust
// Separate frequently accessed data from rarely accessed data
struct ASTNode {
    // Hot data (accessed during every traversal)
    kind: NodeKind,
    children_start: u32,
    children_count: u16,
    
    // Cold data stored separately
    metadata_index: u32,
}

struct ASTMetadata {
    // Cold data (accessed only for diagnostics/debugging)
    source_location: SourceLocation,
    type_annotations: Vec<TypeAnnotation>,
    documentation: Option<String>,
    attributes: HashMap<String, AttributeValue>,
}

struct AST {
    nodes: Vec<ASTNode>,           // Hot data - densely packed
    children: Vec<u32>,            // Child indices
    metadata: Vec<ASTMetadata>,    // Cold data - separate allocation
}

impl AST {
    fn traverse<F>(&self, mut visitor: F) 
    where 
        F: FnMut(&ASTNode),
    {
        // Fast traversal - only touches hot data
        for node in &self.nodes {
            visitor(node);
        }
    }
    
    fn get_metadata(&self, node_index: usize) -> &ASTMetadata {
        let node = &self.nodes[node_index];
        &self.metadata[node.metadata_index as usize]
    }
}
```

### Prefetching Strategies

```rust
use std::arch::x86_64::_mm_prefetch;

// Manual prefetching for predictable access patterns
fn process_with_prefetch(data: &mut [ComplexData]) {
    const PREFETCH_DISTANCE: usize = 8;
    
    for i in 0..data.len() {
        // Prefetch future data
        if i + PREFETCH_DISTANCE < data.len() {
            unsafe {
                _mm_prefetch(
                    &data[i + PREFETCH_DISTANCE] as *const _ as *const i8,
                    std::arch::x86_64::_MM_HINT_T0
                );
            }
        }
        
        // Process current data
        process_complex_data(&mut data[i]);
    }
}

// Software prefetching for linked structures
struct PrefetchingIterator<'a, T> {
    current: Option<&'a Node<T>>,
    prefetch_ahead: usize,
}

impl<'a, T> Iterator for PrefetchingIterator<'a, T> {
    type Item = &'a T;
    
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(node) = self.current {
            // Prefetch nodes ahead
            let mut prefetch_node = node;
            for _ in 0..self.prefetch_ahead {
                if let Some(next) = &prefetch_node.next {
                    unsafe {
                        _mm_prefetch(
                            next.as_ref() as *const _ as *const i8,
                            std::arch::x86_64::_MM_HINT_T0
                        );
                    }
                    prefetch_node = next;
                } else {
                    break;
                }
            }
            
            let data = &node.data;
            self.current = node.next.as_deref();
            Some(data)
        } else {
            None
        }
    }
}
```

## SIMD and Vectorization

### Auto-Vectorization Friendly Code

```rust
// Compiler can auto-vectorize this
fn add_arrays(a: &[f32], b: &[f32], result: &mut [f32]) {
    assert_eq!(a.len(), b.len());
    assert_eq!(a.len(), result.len());
    
    // Simple loop - compiler can vectorize
    for i in 0..a.len() {
        result[i] = a[i] + b[i];
    }
}

// Even better - use iterator methods
fn add_arrays_iter(a: &[f32], b: &[f32], result: &mut [f32]) {
    for ((a_val, b_val), result_val) in a.iter().zip(b.iter()).zip(result.iter_mut()) {
        *result_val = a_val + b_val;
    }
}

// Manual SIMD for maximum control
use std::simd::{f32x8, SimdFloat};

fn add_arrays_simd(a: &[f32], b: &[f32], result: &mut [f32]) {
    assert_eq!(a.len(), b.len());
    assert_eq!(a.len(), result.len());
    
    let len = a.len();
    let simd_len = len - (len % 8);
    
    // Process 8 elements at a time
    for i in (0..simd_len).step_by(8) {
        let a_vec = f32x8::from_slice(&a[i..i + 8]);
        let b_vec = f32x8::from_slice(&b[i..i + 8]);
        let result_vec = a_vec + b_vec;
        result_vec.copy_to_slice(&mut result[i..i + 8]);
    }
    
    // Handle remaining elements
    for i in simd_len..len {
        result[i] = a[i] + b[i];
    }
}

// Complex SIMD operations
fn dot_product_simd(a: &[f32], b: &[f32]) -> f32 {
    assert_eq!(a.len(), b.len());
    
    let len = a.len();
    let simd_len = len - (len % 8);
    let mut sum_vec = f32x8::splat(0.0);
    
    // Vectorized accumulation
    for i in (0..simd_len).step_by(8) {
        let a_vec = f32x8::from_slice(&a[i..i + 8]);
        let b_vec = f32x8::from_slice(&b[i..i + 8]);
        sum_vec += a_vec * b_vec;
    }
    
    // Horizontal sum of vector
    let mut result = sum_vec.reduce_sum();
    
    // Handle remaining elements
    for i in simd_len..len {
        result += a[i] * b[i];
    }
    
    result
}
```

### SIMD-Friendly Data Layouts

```rust
use std::simd::f32x8;

// SIMD-friendly matrix representation
#[repr(align(32))] // Align for AVX
struct Matrix4x4 {
    // Store as column-major for SIMD operations
    data: [f32; 16],
}

impl Matrix4x4 {
    fn multiply_vector_simd(&self, vec: [f32; 4]) -> [f32; 4] {
        let mut result = [0.0; 4];
        
        // Load vector into SIMD register
        let v = f32x8::from_array([vec[0], vec[1], vec[2], vec[3], 0.0, 0.0, 0.0, 0.0]);
        
        // Process two columns at once
        for col in 0..2 {
            let base = col * 8;
            let matrix_cols = f32x8::from_slice(&self.data[base..base + 8]);
            
            // Multiply and accumulate
            let products = matrix_cols * v;
            
            // Extract results (simplified - real implementation would be more complex)
            let results = products.to_array();
            result[col * 2] = results[0] + results[1] + results[2] + results[3];
            result[col * 2 + 1] = results[4] + results[5] + results[6] + results[7];
        }
        
        result
    }
}

// SIMD-friendly color processing
#[repr(C, align(16))]
struct Color {
    r: f32,
    g: f32,
    b: f32,
    a: f32,
}

fn blend_colors_simd(colors1: &[Color], colors2: &[Color], alpha: f32, result: &mut [Color]) {
    use std::simd::f32x4;
    
    let alpha_vec = f32x4::splat(alpha);
    let one_minus_alpha = f32x4::splat(1.0 - alpha);
    
    for i in 0..colors1.len() {
        // Load colors as SIMD vectors
        let c1 = f32x4::from_slice(unsafe {
            std::slice::from_raw_parts(&colors1[i].r, 4)
        });
        let c2 = f32x4::from_slice(unsafe {
            std::slice::from_raw_parts(&colors2[i].r, 4)
        });
        
        // Blend: result = c1 * (1 - alpha) + c2 * alpha
        let blended = c1 * one_minus_alpha + c2 * alpha_vec;
        
        // Store result
        blended.copy_to_slice(unsafe {
            std::slice::from_raw_parts_mut(&mut result[i].r, 4)
        });
    }
}
```

## Memory Alignment

### Custom Alignment for Performance

```rust
use std::alloc::{alloc, dealloc, Layout};
use std::ptr::NonNull;

// Aligned allocation wrapper
struct AlignedVec<T> {
    ptr: NonNull<T>,
    len: usize,
    capacity: usize,
    layout: Layout,
}

impl<T> AlignedVec<T> {
    fn new_aligned(capacity: usize, alignment: usize) -> Self {
        let layout = Layout::from_size_align(
            capacity * std::mem::size_of::<T>(),
            alignment.max(std::mem::align_of::<T>())
        ).expect("Invalid layout");
        
        let ptr = unsafe {
            let raw_ptr = alloc(layout) as *mut T;
            NonNull::new(raw_ptr).expect("Allocation failed")
        };
        
        Self {
            ptr,
            len: 0,
            capacity,
            layout,
        }
    }
    
    fn push(&mut self, value: T) {
        assert!(self.len < self.capacity, "Capacity exceeded");
        
        unsafe {
            std::ptr::write(self.ptr.as_ptr().add(self.len), value);
            self.len += 1;
        }
    }
    
    fn as_slice(&self) -> &[T] {
        unsafe {
            std::slice::from_raw_parts(self.ptr.as_ptr(), self.len)
        }
    }
    
    fn as_mut_slice(&mut self) -> &mut [T] {
        unsafe {
            std::slice::from_raw_parts_mut(self.ptr.as_ptr(), self.len)
        }
    }
}

impl<T> Drop for AlignedVec<T> {
    fn drop(&mut self) {
        // Drop all elements
        for i in 0..self.len {
            unsafe {
                std::ptr::drop_in_place(self.ptr.as_ptr().add(i));
            }
        }
        
        // Deallocate memory
        unsafe {
            dealloc(self.ptr.as_ptr() as *mut u8, self.layout);
        }
    }
}

// Usage for SIMD-aligned data
fn create_simd_buffer() -> AlignedVec<f32> {
    // Align to 32 bytes for AVX
    let mut buffer = AlignedVec::new_aligned(1024, 32);
    
    for i in 0..1024 {
        buffer.push(i as f32);
    }
    
    buffer
}
```

### Page-Aligned Allocations

```rust
use std::alloc::{alloc, dealloc, Layout};

const PAGE_SIZE: usize = 4096;

struct PageAlignedBuffer {
    ptr: *mut u8,
    size: usize,
}

impl PageAlignedBuffer {
    fn new(size: usize) -> Self {
        // Round up to page boundary
        let aligned_size = (size + PAGE_SIZE - 1) & !(PAGE_SIZE - 1);
        
        let layout = Layout::from_size_align(aligned_size, PAGE_SIZE)
            .expect("Invalid layout");
        
        let ptr = unsafe { alloc(layout) };
        assert!(!ptr.is_null(), "Allocation failed");
        
        Self {
            ptr,
            size: aligned_size,
        }
    }
    
    fn as_slice(&self) -> &[u8] {
        unsafe {
            std::slice::from_raw_parts(self.ptr, self.size)
        }
    }
    
    fn as_mut_slice(&mut self) -> &mut [u8] {
        unsafe {
            std::slice::from_raw_parts_mut(self.ptr, self.size)
        }
    }
}

impl Drop for PageAlignedBuffer {
    fn drop(&mut self) {
        unsafe {
            let layout = Layout::from_size_align(self.size, PAGE_SIZE).unwrap();
            dealloc(self.ptr, layout);
        }
    }
}
```

## Benchmarking Cache Performance

### Cache Miss Measurement

```rust
use std::time::Instant;

fn benchmark_cache_behavior() {
    const SIZE: usize = 64 * 1024 * 1024; // 64MB
    let mut data = vec![0u8; SIZE];
    
    // Sequential access (cache-friendly)
    let start = Instant::now();
    for i in 0..SIZE {
        data[i] = (i % 256) as u8;
    }
    let sequential_time = start.elapsed();
    
    // Random access (cache-unfriendly)
    let indices: Vec<usize> = (0..SIZE).map(|_| fastrand::usize(0..SIZE)).collect();
    let start = Instant::now();
    for &i in &indices {
        data[i] = (i % 256) as u8;
    }
    let random_time = start.elapsed();
    
    // Strided access (partially cache-unfriendly)
    let start = Instant::now();
    for i in (0..SIZE).step_by(64) { // Step by cache line size
        data[i] = (i % 256) as u8;
    }
    let strided_time = start.elapsed();
    
    println!("Sequential: {:?}", sequential_time);
    println!("Random: {:?}", random_time);
    println!("Strided: {:?}", strided_time);
    
    println!("Random/Sequential ratio: {:.2}", 
        random_time.as_nanos() as f64 / sequential_time.as_nanos() as f64);
    println!("Strided/Sequential ratio: {:.2}", 
        strided_time.as_nanos() as f64 / sequential_time.as_nanos() as f64);
}
```

### Memory Bandwidth Testing

```rust
use std::time::Instant;

fn benchmark_memory_bandwidth() {
    const SIZE: usize = 128 * 1024 * 1024; // 128MB
    let src = vec![1u8; SIZE];
    let mut dst = vec![0u8; SIZE];
    
    // Test different copy patterns
    
    // Simple copy
    let start = Instant::now();
    dst.copy_from_slice(&src);
    let copy_time = start.elapsed();
    
    // Manual loop
    let start = Instant::now();
    for i in 0..SIZE {
        dst[i] = src[i];
    }
    let loop_time = start.elapsed();
    
    // Chunked copy (better for large data)
    let start = Instant::now();
    for chunk in src.chunks(4096) {
        let dst_chunk = &mut dst[0..chunk.len()];
        dst_chunk.copy_from_slice(chunk);
    }
    let chunked_time = start.elapsed();
    
    let bytes_per_sec = |time: std::time::Duration| {
        (SIZE as f64) / time.as_secs_f64() / (1024.0 * 1024.0 * 1024.0)
    };
    
    println!("Copy: {:.2} GB/s", bytes_per_sec(copy_time));
    println!("Loop: {:.2} GB/s", bytes_per_sec(loop_time));
    println!("Chunked: {:.2} GB/s", bytes_per_sec(chunked_time));
}
```

### Cache Line Utilization

```rust
use std::time::Instant;

#[repr(align(64))] // Cache line aligned
struct CacheLineStruct {
    data: [u64; 8], // Exactly one cache line
}

fn benchmark_cache_line_utilization() {
    const COUNT: usize = 1024 * 1024;
    let mut data = vec![CacheLineStruct { data: [0; 8] }; COUNT];
    
    // Access all elements in each cache line (good utilization)
    let start = Instant::now();
    for item in &mut data {
        for j in 0..8 {
            item.data[j] += 1;
        }
    }
    let full_utilization = start.elapsed();
    
    // Access only first element in each cache line (poor utilization)
    let start = Instant::now();
    for item in &mut data {
        item.data[0] += 1;
    }
    let poor_utilization = start.elapsed();
    
    println!("Full cache line utilization: {:?}", full_utilization);
    println!("Poor cache line utilization: {:?}", poor_utilization);
    println!("Efficiency ratio: {:.2}", 
        poor_utilization.as_nanos() as f64 / full_utilization.as_nanos() as f64);
}
```

## Compiler Optimizations

### Profile-Guided Optimization Setup

```toml
# Cargo.toml
[profile.release]
lto = true
codegen-units = 1
panic = "abort"

[profile.pgo]
inherits = "release"
lto = true
codegen-units = 1
```

```bash
# PGO build process
# 1. Build with instrumentation
RUSTFLAGS="-Cprofile-generate=/tmp/pgo-data" cargo build --release

# 2. Run representative workload
./target/release/your_program < typical_input.txt

# 3. Build with profile data
RUSTFLAGS="-Cprofile-use=/tmp/pgo-data" cargo build --release
```

### Optimization Hints

```rust
// Branch prediction hints
#[inline(always)]
fn likely_true_branch(condition: bool) -> i32 {
    if std::intrinsics::likely(condition) {
        expensive_computation()
    } else {
        0
    }
}

#[inline(always)]
fn unlikely_error_path(result: Result<i32, Error>) -> i32 {
    match result {
        Ok(value) => value,
        Err(_) => {
            std::intrinsics::unlikely(|| {
                handle_error()
            });
            0
        }
    }
}

// Loop optimization hints
fn optimized_loop(data: &mut [f32]) {
    // Help compiler understand loop bounds
    let len = data.len();
    
    // Compiler can better optimize when it knows the loop count
    for i in 0..len {
        unsafe {
            // Bounds check elimination
            *data.get_unchecked_mut(i) *= 2.0;
        }
    }
}

// Prefetch hints for predictable access patterns
fn process_with_prefetch_hints(data: &[ComplexData]) {
    for (i, item) in data.iter().enumerate() {
        // Prefetch next items
        if i + 4 < data.len() {
            std::intrinsics::prefetch_read_data(&data[i + 4], 3);
        }
        
        process_item(item);
    }
}
```

### Target-Specific Optimizations

```rust
// CPU feature detection and dispatch
#[cfg(target_arch = "x86_64")]
fn optimized_function(data: &[f32]) -> f32 {
    if is_x86_feature_detected!("avx2") {
        unsafe { avx2_implementation(data) }
    } else if is_x86_feature_detected!("sse4.1") {
        unsafe { sse41_implementation(data) }
    } else {
        scalar_implementation(data)
    }
}

#[cfg(target_arch = "x86_64")]
#[target_feature(enable = "avx2")]
unsafe fn avx2_implementation(data: &[f32]) -> f32 {
    // AVX2-specific implementation
    use std::arch::x86_64::*;
    
    let mut sum = _mm256_setzero_ps();
    let chunks = data.len() / 8;
    
    for i in 0..chunks {
        let values = _mm256_loadu_ps(data.as_ptr().add(i * 8));
        sum = _mm256_add_ps(sum, values);
    }
    
    // Horizontal sum and handle remainder
    let mut result = [0.0f32; 8];
    _mm256_storeu_ps(result.as_mut_ptr(), sum);
    let mut total = result.iter().sum::<f32>();
    
    for i in chunks * 8..data.len() {
        total += data[i];
    }
    
    total
}

#[cfg(target_arch = "x86_64")]
#[target_feature(enable = "sse4.1")]
unsafe fn sse41_implementation(data: &[f32]) -> f32 {
    // SSE4.1-specific implementation
    scalar_implementation(data) // Simplified for example
}

fn scalar_implementation(data: &[f32]) -> f32 {
    data.iter().sum()
}
```

## Best Practices

### 1. Measure Before Optimizing

```rust
use std::time::Instant;

// Always profile your specific use case
fn benchmark_data_structures() {
    const SIZE: usize = 100_000;
    
    // Test different approaches
    let data: Vec<(i32, String)> = (0..SIZE)
        .map(|i| (i, format!("item_{}", i)))
        .collect();
    
    // Approach 1: Vec of tuples
    let start = Instant::now();
    let vec_tuples: Vec<(i32, String)> = data.clone();
    let sum1: i32 = vec_tuples.iter().map(|(id, _)| id).sum();
    let time1 = start.elapsed();
    
    // Approach 2: Separate vectors
    let start = Instant::now();
    let ids: Vec<i32> = data.iter().map(|(id, _)| *id).collect();
    let names: Vec<String> = data.iter().map(|(_, name)| name.clone()).collect();
    let sum2: i32 = ids.iter().sum();
    let time2 = start.elapsed();
    
    println!("Vec of tuples: {:?}", time1);
    println!("Separate vecs: {:?}", time2);
    println!("Speedup: {:.2}x", time1.as_nanos() as f64 / time2.as_nanos() as f64);
}
```

### 2. Design for Cache Locality

```rust
// ✅ Good: Related data together
struct ParticleSystem {
    // Hot data accessed every frame
    positions: Vec<Vec3>,
    velocities: Vec<Vec3>,
    
    // Warm data accessed occasionally  
    masses: Vec<f32>,
    charges: Vec<f32>,
    
    // Cold data accessed rarely
    debug_names: Vec<String>,
    creation_times: Vec<Instant>,
}

// ❌ Bad: Unrelated data mixed together
struct BadParticle {
    position: Vec3,      // Hot
    debug_name: String,  // Cold - wastes cache space
    velocity: Vec3,      // Hot
    creation_time: Instant, // Cold
    mass: f32,          // Warm
}
```

### 3. Use Appropriate Data Structures

```rust
use indexmap::IndexMap;
use smallvec::SmallVec;

// ✅ Choose data structures based on access patterns
struct CompilerContext {
    // Frequent lookups, order matters for diagnostics
    symbols: IndexMap<String, Symbol>,
    
    // Small collections, avoid heap allocation
    current_scope: SmallVec<[ScopeId; 4]>,
    
    // Large, frequently accessed arrays
    tokens: Vec<Token>,
    
    // Sparse data, infrequent access
    debug_info: HashMap<NodeId, DebugInfo>,
}
```

### 4. Minimize Allocations

```rust
// ✅ Reuse allocations
struct Parser {
    token_buffer: Vec<Token>,
    string_buffer: String,
}

impl Parser {
    fn parse_tokens(&mut self, input: &str) -> &[Token] {
        self.token_buffer.clear(); // Reuse allocation
        
        // Parse into existing buffer
        for token in tokenize(input) {
            self.token_buffer.push(token);
        }
        
        &self.token_buffer
    }
    
    fn parse_string(&mut self, input: &str) -> &str {
        self.string_buffer.clear(); // Reuse allocation
        
        // Build string in existing buffer
        for ch in input.chars() {
            if ch != '\\' {
                self.string_buffer.push(ch);
            }
        }
        
        &self.string_buffer
    }
}
```

### 5. Consider NUMA Topology

```rust
// For multi-threaded applications on NUMA systems
use std::thread;

fn numa_aware_processing(data: Vec<WorkItem>) {
    let num_threads = num_cpus::get();
    let chunk_size = data.len() / num_threads;
    
    let handles: Vec<_> = data
        .chunks(chunk_size)
        .enumerate()
        .map(|(cpu_id, chunk)| {
            let chunk = chunk.to_vec();
            thread::spawn(move || {
                // Bind thread to specific CPU if needed
                // (requires platform-specific code)
                
                // Process chunk with good locality
                process_chunk(chunk)
            })
        })
        .collect();
    
    for handle in handles {
        handle.join().unwrap();
    }
}
```

---

This comprehensive guide covers the essential aspects of memory layout optimization and cache-friendly programming in Rust. These techniques are particularly important for high-performance applications like compilers, where efficient memory access patterns can significantly impact compilation speed and overall performance.