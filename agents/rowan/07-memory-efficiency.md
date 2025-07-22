# Memory Efficiency Techniques

Rowan is designed from the ground up for memory efficiency, using several advanced techniques to minimize memory usage while maintaining performance. This guide covers the memory optimization strategies used in Rowan and how to leverage them effectively.

## Overview

Rowan achieves memory efficiency through:

1. **Structural sharing** - Identical subtrees share the same memory
2. **Node caching** - Common patterns are cached and reused
3. **Compact representations** - Optimized memory layouts
4. **Thin pointers** - Reduced pointer overhead
5. **Interning** - Deduplication of tokens and nodes

## Structural Sharing

### How Structural Sharing Works

Rowan's green trees are immutable, which enables safe structural sharing between different trees. When trees have common subtrees, they share the same memory for those parts.

```rust
use rowan::{GreenNodeBuilder, GreenNode};

// Example: Two functions with identical parameter lists
fn build_similar_functions() -> (GreenNode, GreenNode) {
    // First function: fn foo() {}
    let mut builder1 = GreenNodeBuilder::new();
    builder1.start_node(SyntaxKind::FUNCTION.into());
    builder1.token(SyntaxKind::FN_KW.into(), "fn");
    builder1.token(SyntaxKind::WHITESPACE.into(), " ");
    builder1.token(SyntaxKind::IDENT.into(), "foo");
    
    // Parameter list - this will be shared
    builder1.start_node(SyntaxKind::PARAM_LIST.into());
    builder1.token(SyntaxKind::L_PAREN.into(), "(");
    builder1.token(SyntaxKind::R_PAREN.into(), ")");
    builder1.finish_node();
    
    builder1.token(SyntaxKind::WHITESPACE.into(), " ");
    
    // Empty block - this will also be shared
    builder1.start_node(SyntaxKind::BLOCK.into());
    builder1.token(SyntaxKind::L_BRACE.into(), "{");
    builder1.token(SyntaxKind::R_BRACE.into(), "}");
    builder1.finish_node();
    
    builder1.finish_node();
    let func1 = builder1.finish();
    
    // Second function: fn bar() {} - will share parameter list and block
    let mut builder2 = GreenNodeBuilder::new();
    builder2.start_node(SyntaxKind::FUNCTION.into());
    builder2.token(SyntaxKind::FN_KW.into(), "fn");
    builder2.token(SyntaxKind::WHITESPACE.into(), " ");
    builder2.token(SyntaxKind::IDENT.into(), "bar"); // Only difference
    
    // Same parameter list - will be shared with func1
    builder2.start_node(SyntaxKind::PARAM_LIST.into());
    builder2.token(SyntaxKind::L_PAREN.into(), "(");
    builder2.token(SyntaxKind::R_PAREN.into(), ")");
    builder2.finish_node();
    
    builder2.token(SyntaxKind::WHITESPACE.into(), " ");
    
    // Same empty block - will be shared with func1
    builder2.start_node(SyntaxKind::BLOCK.into());
    builder2.token(SyntaxKind::L_BRACE.into(), "{");
    builder2.token(SyntaxKind::R_BRACE.into(), "}");
    builder2.finish_node();
    
    builder2.finish_node();
    let func2 = builder2.finish();
    
    (func1, func2)
}
```

### Measuring Structural Sharing

```rust
use std::collections::HashSet;

fn measure_sharing_efficiency(trees: &[GreenNode]) -> f64 {
    let mut unique_nodes = HashSet::new();
    let mut total_nodes = 0;
    
    for tree in trees {
        count_nodes(tree, &mut unique_nodes, &mut total_nodes);
    }
    
    let sharing_ratio = 1.0 - (unique_nodes.len() as f64 / total_nodes as f64);
    println!("Sharing efficiency: {:.2}%", sharing_ratio * 100.0);
    sharing_ratio
}

fn count_nodes(
    node: &GreenNode, 
    unique_nodes: &mut HashSet<*const GreenNodeData>,
    total_nodes: &mut usize
) {
    let node_ptr = node.as_ptr();
    unique_nodes.insert(node_ptr);
    *total_nodes += 1;
    
    for child in node.children() {
        if let rowan::NodeOrToken::Node(child_node) = child {
            count_nodes(&child_node, unique_nodes, total_nodes);
        }
    }
}
```

## Node Caching

### NodeCache Implementation

Rowan uses a sophisticated caching system to deduplicate nodes and tokens:

```rust
use rowan::{NodeCache, GreenNodeBuilder};

struct CachedParser {
    cache: NodeCache,
    hit_count: usize,
    miss_count: usize,
}

impl CachedParser {
    fn new() -> Self {
        Self {
            cache: NodeCache::default(),
            hit_count: 0,
            miss_count: 0,
        }
    }
    
    fn parse_with_cache(&mut self, input: &str) -> GreenNode {
        let mut builder = GreenNodeBuilder::with_cache(&mut self.cache);
        
        // Parse using the cached builder
        self.parse_into_builder(&mut builder, input);
        
        builder.finish()
    }
    
    fn cache_efficiency(&self) -> f64 {
        let total = self.hit_count + self.miss_count;
        if total == 0 {
            0.0
        } else {
            self.hit_count as f64 / total as f64
        }
    }
}
```

### Cache-Aware Parsing Strategies

```rust
impl CachedParser {
    fn parse_common_patterns(&mut self, patterns: &[&str]) -> Vec<GreenNode> {
        let mut results = Vec::new();
        
        for pattern in patterns {
            let tree = self.parse_with_cache(pattern);
            results.push(tree);
        }
        
        // Common patterns will be increasingly cached
        println!("Cache efficiency: {:.2}%", self.cache_efficiency() * 100.0);
        
        results
    }
    
    fn parse_function_signatures(&mut self) -> Vec<GreenNode> {
        // These will share common parameter patterns
        let signatures = [
            "fn foo()",
            "fn bar()",
            "fn baz()",
            "fn foo(x: i32)",
            "fn bar(x: i32)",
            "fn qux(x: i32, y: i32)",
        ];
        
        self.parse_common_patterns(&signatures)
    }
}
```

### Cache Optimization Techniques

```rust
use std::collections::HashMap;

struct SmartCache {
    node_cache: NodeCache,
    pattern_frequency: HashMap<String, usize>,
    cache_size_limit: usize,
}

impl SmartCache {
    fn should_cache_pattern(&self, pattern: &str) -> bool {
        // Only cache frequently occurring patterns
        self.pattern_frequency.get(pattern).unwrap_or(&0) > &2
    }
    
    fn evict_least_used(&mut self) {
        if self.node_cache.len() > self.cache_size_limit {
            // Implement LRU eviction
            // This is simplified - real implementation would track usage
            self.node_cache.clear();
        }
    }
    
    fn record_pattern_usage(&mut self, pattern: &str) {
        *self.pattern_frequency.entry(pattern.to_string()).or_insert(0) += 1;
    }
}
```

## Compact Memory Representations

### Thin Arc Pointers

Rowan uses "thin" Arc pointers to reduce memory overhead:

```rust
// Traditional Arc<[T]> uses fat pointers (16 bytes on 64-bit)
// ThinArc<H, T> uses thin pointers (8 bytes on 64-bit)

use rowan::arc::ThinArc;

// Example of memory layout optimization
struct CompactNode {
    // Header contains metadata
    kind: SyntaxKind,
    text_len: TextSize,
    
    // Children stored inline
    children: [GreenChild],
}

// ThinArc stores the length in the allocation itself
type OptimizedGreenNode = ThinArc<GreenNodeHead, GreenChild>;
```

### Memory Layout Analysis

```rust
use std::mem;

fn analyze_memory_layout() {
    println!("Memory sizes:");
    println!("  GreenNode: {} bytes", mem::size_of::<GreenNode>());
    println!("  GreenToken: {} bytes", mem::size_of::<GreenToken>());
    println!("  SyntaxNode: {} bytes", mem::size_of::<SyntaxNode<MyLang>>());
    println!("  SyntaxToken: {} bytes", mem::size_of::<SyntaxToken<MyLang>>());
    
    // Compare with traditional AST nodes
    println!("Traditional AST node: {} bytes", mem::size_of::<TraditionalAstNode>());
}

// Traditional AST node for comparison
struct TraditionalAstNode {
    kind: u32,
    parent: Option<Box<TraditionalAstNode>>,
    children: Vec<Box<TraditionalAstNode>>,
    text: String,
    range: (usize, usize),
}
```

## Token Interning

### Automatic Token Deduplication

```rust
use rowan::GreenToken;

fn demonstrate_token_interning() {
    let mut builder = GreenNodeBuilder::new();
    
    // These identical tokens will be interned
    builder.token(SyntaxKind::IDENT.into(), "foo");
    builder.token(SyntaxKind::IDENT.into(), "foo"); // Same as above
    builder.token(SyntaxKind::IDENT.into(), "foo"); // Same as above
    
    // Different tokens
    builder.token(SyntaxKind::IDENT.into(), "bar");
    builder.token(SyntaxKind::IDENT.into(), "baz");
    
    // The three "foo" tokens share the same memory
}
```

### Custom Interning Strategies

```rust
struct TokenInterner {
    cache: HashMap<(SyntaxKind, String), GreenToken>,
    stats: InterningStats,
}

#[derive(Default)]
struct InterningStats {
    total_tokens: usize,
    unique_tokens: usize,
    memory_saved: usize,
}

impl TokenInterner {
    fn intern_token(&mut self, kind: SyntaxKind, text: &str) -> GreenToken {
        self.stats.total_tokens += 1;
        
        let key = (kind, text.to_string());
        
        if let Some(existing) = self.cache.get(&key) {
            // Cache hit - memory saved
            self.stats.memory_saved += text.len();
            existing.clone()
        } else {
            // Cache miss - create new token
            let token = GreenToken::new(kind, text);
            self.cache.insert(key, token.clone());
            self.stats.unique_tokens += 1;
            token
        }
    }
    
    fn efficiency_report(&self) -> String {
        let dedup_ratio = 1.0 - (self.stats.unique_tokens as f64 / self.stats.total_tokens as f64);
        format!(
            "Token interning: {:.1}% deduplication, {} bytes saved",
            dedup_ratio * 100.0,
            self.stats.memory_saved
        )
    }
}
```

## Memory Usage Patterns

### Analyzing Memory Consumption

```rust
use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering};

// Custom allocator to track memory usage
struct TrackingAllocator;

static ALLOCATED: AtomicUsize = AtomicUsize::new(0);

unsafe impl GlobalAlloc for TrackingAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let ptr = System.alloc(layout);
        if !ptr.is_null() {
            ALLOCATED.fetch_add(layout.size(), Ordering::Relaxed);
        }
        ptr
    }
    
    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        System.dealloc(ptr, layout);
        ALLOCATED.fetch_sub(layout.size(), Ordering::Relaxed);
    }
}

fn measure_parse_memory_usage(input: &str) -> (GreenNode, usize) {
    let before = ALLOCATED.load(Ordering::Relaxed);
    
    let tree = parse(input);
    
    let after = ALLOCATED.load(Ordering::Relaxed);
    let memory_used = after - before;
    
    (tree, memory_used)
}
```

### Memory-Efficient Parsing Strategies

```rust
struct MemoryEfficientParser {
    cache: NodeCache,
    memory_budget: usize,
    current_usage: usize,
}

impl MemoryEfficientParser {
    fn parse_with_budget(&mut self, input: &str) -> Result<GreenNode, MemoryError> {
        let initial_usage = self.current_usage;
        
        // Use cached builder to maximize sharing
        let mut builder = GreenNodeBuilder::with_cache(&mut self.cache);
        
        // Parse in chunks if input is large
        if input.len() > 10_000 {
            self.parse_chunked(&mut builder, input)
        } else {
            self.parse_direct(&mut builder, input)
        }
    }
    
    fn parse_chunked(&mut self, builder: &mut GreenNodeBuilder, input: &str) -> Result<GreenNode, MemoryError> {
        // Break large inputs into smaller chunks to control memory usage
        let chunk_size = 1000;
        
        builder.start_node(SyntaxKind::ROOT.into());
        
        for chunk in input.as_bytes().chunks(chunk_size) {
            let chunk_str = std::str::from_utf8(chunk).unwrap();
            
            // Check memory budget before processing each chunk
            if self.current_usage > self.memory_budget {
                return Err(MemoryError::BudgetExceeded);
            }
            
            self.parse_chunk(builder, chunk_str)?;
        }
        
        builder.finish_node();
        Ok(builder.finish())
    }
}

#[derive(Debug)]
enum MemoryError {
    BudgetExceeded,
    AllocationFailed,
}
```

## Optimization Techniques

### Lazy Evaluation

```rust
struct LazyGreenNode {
    builder: Option<GreenNodeBuilder<'static>>,
    cached_result: Option<GreenNode>,
}

impl LazyGreenNode {
    fn new() -> Self {
        Self {
            builder: Some(GreenNodeBuilder::new()),
            cached_result: None,
        }
    }
    
    fn add_token(&mut self, kind: SyntaxKind, text: &str) {
        if let Some(builder) = &mut self.builder {
            builder.token(kind.into(), text);
        }
    }
    
    fn finish(&mut self) -> &GreenNode {
        if self.cached_result.is_none() {
            if let Some(builder) = self.builder.take() {
                self.cached_result = Some(builder.finish());
            }
        }
        
        self.cached_result.as_ref().unwrap()
    }
}
```

### Memory Pool Allocation

```rust
use std::alloc::{alloc, dealloc, Layout};

struct MemoryPool {
    chunks: Vec<*mut u8>,
    current_chunk: *mut u8,
    chunk_size: usize,
    offset: usize,
}

impl MemoryPool {
    fn new(chunk_size: usize) -> Self {
        Self {
            chunks: Vec::new(),
            current_chunk: std::ptr::null_mut(),
            chunk_size,
            offset: chunk_size, // Force allocation of first chunk
        }
    }
    
    fn allocate<T>(&mut self, count: usize) -> *mut T {
        let size = std::mem::size_of::<T>() * count;
        let align = std::mem::align_of::<T>();
        
        // Align offset
        self.offset = (self.offset + align - 1) & !(align - 1);
        
        if self.offset + size > self.chunk_size {
            self.allocate_new_chunk();
            self.offset = 0;
        }
        
        let ptr = unsafe { self.current_chunk.add(self.offset) as *mut T };
        self.offset += size;
        
        ptr
    }
    
    fn allocate_new_chunk(&mut self) {
        let layout = Layout::from_size_align(self.chunk_size, 8).unwrap();
        let chunk = unsafe { alloc(layout) };
        
        if chunk.is_null() {
            panic!("Failed to allocate memory chunk");
        }
        
        self.chunks.push(chunk);
        self.current_chunk = chunk;
        self.offset = 0;
    }
}

// Pool-based node builder
struct PooledGreenNodeBuilder<'pool> {
    pool: &'pool mut MemoryPool,
    nodes: Vec<PooledGreenNode>,
}

struct PooledGreenNode {
    kind: SyntaxKind,
    children: *mut GreenChild,
    child_count: usize,
}
```

## Performance Monitoring

### Memory Usage Tracking

```rust
#[derive(Debug, Default)]
struct MemoryMetrics {
    total_allocations: usize,
    peak_usage: usize,
    current_usage: usize,
    cache_hits: usize,
    cache_misses: usize,
    sharing_efficiency: f64,
}

struct MonitoredParser {
    parser: Parser,
    metrics: MemoryMetrics,
}

impl MonitoredParser {
    fn parse_with_monitoring(&mut self, input: &str) -> (GreenNode, MemoryMetrics) {
        let start_memory = get_current_memory_usage();
        
        let tree = self.parser.parse(input);
        
        let end_memory = get_current_memory_usage();
        
        self.metrics.current_usage = end_memory;
        self.metrics.peak_usage = self.metrics.peak_usage.max(end_memory);
        self.metrics.total_allocations += end_memory - start_memory;
        
        (tree, self.metrics.clone())
    }
    
    fn generate_memory_report(&self) -> String {
        format!(
            "Memory Report:\n\
             - Peak usage: {} KB\n\
             - Current usage: {} KB\n\
             - Total allocations: {} KB\n\
             - Cache hit rate: {:.1}%\n\
             - Sharing efficiency: {:.1}%",
            self.metrics.peak_usage / 1024,
            self.metrics.current_usage / 1024,
            self.metrics.total_allocations / 1024,
            self.cache_hit_rate() * 100.0,
            self.metrics.sharing_efficiency * 100.0
        )
    }
    
    fn cache_hit_rate(&self) -> f64 {
        let total = self.metrics.cache_hits + self.metrics.cache_misses;
        if total == 0 {
            0.0
        } else {
            self.metrics.cache_hits as f64 / total as f64
        }
    }
}
```

### Benchmarking Memory Efficiency

```rust
use criterion::{black_box, Criterion};

fn benchmark_memory_efficiency(c: &mut Criterion) {
    let inputs = generate_test_inputs();
    
    c.bench_function("parse_without_cache", |b| {
        b.iter(|| {
            for input in &inputs {
                let tree = parse_without_cache(black_box(input));
                black_box(tree);
            }
        })
    });
    
    c.bench_function("parse_with_cache", |b| {
        let mut cache = NodeCache::default();
        b.iter(|| {
            for input in &inputs {
                let tree = parse_with_cache(black_box(input), &mut cache);
                black_box(tree);
            }
        })
    });
}

fn generate_test_inputs() -> Vec<String> {
    // Generate inputs with common patterns to test sharing
    vec![
        "fn foo() {}".to_string(),
        "fn bar() {}".to_string(),
        "fn baz() {}".to_string(),
        // Many functions with same signature
    ]
}
```

## Best Practices

### Memory-Efficient Parsing

```rust
// Good: Use cached builder for related parsing tasks
fn parse_multiple_files(files: &[&str]) -> Vec<GreenNode> {
    let mut cache = NodeCache::default();
    let mut results = Vec::new();
    
    for file in files {
        let mut builder = GreenNodeBuilder::with_cache(&mut cache);
        let tree = parse_into_builder(&mut builder, file);
        results.push(tree);
    }
    
    results
}

// Good: Reuse common subtrees
fn build_similar_structures(variants: &[&str]) -> Vec<GreenNode> {
    let mut cache = NodeCache::default();
    let mut results = Vec::new();
    
    for variant in variants {
        let mut builder = GreenNodeBuilder::with_cache(&mut cache);
        
        // Common prefix
        builder.start_node(SyntaxKind::STRUCT.into());
        builder.token(SyntaxKind::STRUCT_KW.into(), "struct");
        builder.token(SyntaxKind::WHITESPACE.into(), " ");
        builder.token(SyntaxKind::IDENT.into(), variant);
        
        // Common suffix (will be shared)
        builder.start_node(SyntaxKind::FIELD_LIST.into());
        builder.token(SyntaxKind::L_BRACE.into(), "{");
        builder.token(SyntaxKind::R_BRACE.into(), "}");
        builder.finish_node();
        
        builder.finish_node();
        results.push(builder.finish());
    }
    
    results
}

// Avoid: Creating separate caches for related work
fn inefficient_parsing(files: &[&str]) -> Vec<GreenNode> {
    let mut results = Vec::new();
    
    for file in files {
        // Bad: New cache for each file
        let mut builder = GreenNodeBuilder::new();
        let tree = parse_into_builder(&mut builder, file);
        results.push(tree);
    }
    
    results
}
```

### Memory Monitoring

```rust
// Good: Monitor memory usage in production
struct ProductionParser {
    cache: NodeCache,
    memory_monitor: MemoryMonitor,
}

impl ProductionParser {
    fn parse_with_limits(&mut self, input: &str) -> Result<GreenNode, ParseError> {
        // Check memory before parsing
        if self.memory_monitor.current_usage() > self.memory_monitor.limit() {
            self.cache.clear(); // Free up memory
        }
        
        let mut builder = GreenNodeBuilder::with_cache(&mut self.cache);
        
        // Parse with monitoring
        let result = self.parse_monitored(&mut builder, input)?;
        
        // Log memory statistics
        self.memory_monitor.log_usage();
        
        Ok(result)
    }
}
```

Rowan's memory efficiency comes from its careful design around immutability, structural sharing, and intelligent caching. By understanding and leveraging these techniques, you can build language tools that scale to large codebases while maintaining excellent performance.