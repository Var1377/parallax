# Incremental Recomputation Strategies

This document explains how Salsa 0.23 achieves efficient incremental recomputation, based on analysis of the actual implementation.

## Core Principles

Salsa's incremental recomputation is built on several key principles:

1. **Dependency Tracking**: Automatic tracking of what each query depends on
2. **Change Detection**: Efficient detection of what has actually changed
3. **Lazy Evaluation**: Only recompute what is actually needed
4. **Memoization**: Cache results to avoid redundant computation
5. **Granular Invalidation**: Invalidate only what is affected by changes

## Revision System

### Revision Lifecycle
```rust
// From src/revision.rs
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Revision(u32);

impl Revision {
    pub fn start() -> Self { Revision(1) }
    pub fn next(self) -> Self { Revision(self.0 + 1) }
}
```

### Revision Management
```rust
// From src/runtime.rs
impl Runtime {
    pub(crate) fn new_revision(&mut self) -> Revision {
        let r_old = self.current_revision();
        let r_new = r_old.next();
        self.revisions[0] = r_new;
        r_new
    }
    
    pub(crate) fn report_tracked_write(&mut self, durability: Durability) {
        let new_revision = self.current_revision();
        // Update revisions for all durability levels <= this one
        self.revisions[1..=durability.index()].fill(new_revision);
    }
}
```

**Revision Flow:**
1. **Input Change**: Setter called on input field
2. **New Revision**: Runtime creates new revision
3. **Durability Update**: All durability levels ≤ changed input are marked
4. **Lazy Invalidation**: Dependent queries marked stale but not recomputed
5. **On-Demand Recomputation**: Queries recomputed when accessed

## Dependency Tracking

### Automatic Dependency Recording
```rust
// When a query accesses another query's result
#[salsa::tracked]
fn parse_file(db: &dyn Database, file: SourceFile) -> Ast {
    let text = file.text(db);  // ← Dependency recorded here
    parse_text(text)
}
```

The `file.text(db)` call automatically:
1. Records that `parse_file` depends on `file.text`
2. Notes the current revision of `file.text`
3. Stores this dependency information in the memo

### Dependency Storage
```rust
// From src/function/memo.rs
pub struct MemoRevisions {
    pub changed_at: Revision,
    pub durability: Durability,
    pub tracked_struct_ids: Option<ThinVec<(IngredientIndex, Id)>>,
    pub untracked_read: bool,
    pub verified_final: AtomicBool,
}
```

Dependencies are stored as:
- **Direct dependencies**: Other queries this query called
- **Tracked struct IDs**: Specific struct instances accessed
- **Durability**: Minimum durability of all dependencies
- **Changed at**: When dependencies last changed

## Change Detection Strategies

### Input-Level Change Detection
```rust
// From src/input.rs
impl<C: Configuration> IngredientImpl<C> {
    pub fn set_field<R>(
        &mut self,
        runtime: &mut Runtime,
        id: C::Struct,
        field_index: usize,
        durability: Option<Durability>,
        setter: impl FnOnce(&mut C::Fields) -> R,
    ) -> R {
        // Update field revision
        data.revisions[field_index] = runtime.current_revision();
        
        // Report write to runtime
        runtime.report_tracked_write(*field_durability);
        
        // Apply the change
        setter(&mut data.fields)
    }
}
```

**Per-Field Tracking**: Each input field has its own revision, enabling fine-grained change detection.

### Query-Level Change Detection
```rust
// From src/function/maybe_changed_after.rs
pub(super) fn maybe_changed_after<C>(
    ingredient: &IngredientImpl<C>,
    db: &C::DbView,
    input: Id,
    revision: Revision,
    cycle_heads: &mut CycleHeads,
) -> VerifyResult
where
    C: Configuration,
{
    // Check if memo exists and is newer than revision
    let Some(memo) = ingredient.get_memo_from_table_for(zalsa, input, memo_ingredient_index) else {
        return VerifyResult::changed();
    };
    
    // Check if memo is from after the revision we're checking
    if memo.revisions.changed_at > revision {
        return VerifyResult::unchanged();
    }
    
    // Recursively check dependencies
    verify_memo_dependencies(memo, revision, cycle_heads)
}
```

**Verification Process:**
1. **Memo Check**: Does a memoized result exist?
2. **Revision Check**: Is the memo newer than the change we're checking?
3. **Dependency Verification**: Are all dependencies still valid?
4. **Result**: Return whether recomputation is needed

### Tracked Struct Change Detection
```rust
// From src/tracked_struct.rs
unsafe fn update<'db>(
    &'db self,
    zalsa: &'db Zalsa,
    current_revision: Revision,
    mut id: Id,
    current_deps: &Stamp,
    fields: C::Fields<'db>,
) -> Result<Id, C::Fields<'db>> {
    // Update fields and check what changed
    let untracked_update = unsafe {
        C::update_fields(
            current_deps.changed_at,
            &mut data.revisions,
            &mut data.fields,
            fields,
        )
    };
    
    if untracked_update {
        // Untracked fields changed - new identity needed
        id = id.next_generation().expect("generation overflow");
    }
    
    // Update durability and revision
    data.durability = current_deps.durability;
    data.updated_at.swap(Some(current_revision));
}
```

**Field-Level Updates**: Only tracked fields that actually changed get new revisions.

## Durability-Based Optimization

### Durability Levels
```rust
// From src/durability.rs
impl Durability {
    pub const MIN: Durability = Durability(0);    // Changes every revision
    pub const LOW: Durability = Durability(1);    // File contents
    pub const MEDIUM: Durability = Durability(2); // Configuration
    pub const HIGH: Durability = Durability(3);   // Compiler version
}
```

### Durability-Based Skipping
```rust
// From src/runtime.rs
impl Runtime {
    pub(crate) fn last_changed_revision(&self, d: Durability) -> Revision {
        self.revisions[d.index()]
    }
}
```

**Optimization Strategy:**
- High durability inputs change rarely
- When checking if a query needs recomputation, check durability first
- If no inputs of sufficient durability have changed, skip verification

### Example Usage
```rust
#[salsa::input]
pub struct Config {
    #[durability(salsa::Durability::HIGH)]
    pub compiler_version: String,  // Almost never changes
    
    #[durability(salsa::Durability::MEDIUM)]
    pub optimization_level: u8,    // Changes occasionally
    
    #[durability(salsa::Durability::LOW)]
    pub debug_mode: bool,          // Changes frequently
}

#[salsa::tracked]
fn expensive_analysis(db: &dyn Database, config: Config, file: SourceFile) -> Analysis {
    // This function depends on all durability levels
    let version = config.compiler_version(db);  // HIGH durability
    let opt_level = config.optimization_level(db);  // MEDIUM durability
    let debug = config.debug_mode(db);  // LOW durability
    
    // Expensive computation here
}
```

**Recomputation Scenarios:**
- Change `debug_mode`: Only queries depending on LOW+ durability recompute
- Change `optimization_level`: Queries depending on MEDIUM+ durability recompute
- Change `compiler_version`: All queries recompute (HIGH durability changed)

## Memoization Strategies

### Memo Structure
```rust
// From src/function/memo.rs
pub struct Memo<'db, C: Configuration> {
    pub revisions: MemoRevisions,
    pub value: C::Output<'db>,
}

pub struct MemoRevisions {
    pub changed_at: Revision,
    pub durability: Durability,
    pub tracked_struct_ids: Option<ThinVec<(IngredientIndex, Id)>>,
    pub untracked_read: bool,
    pub verified_final: AtomicBool,
}
```

### Memo Validation
```rust
// Simplified validation logic
fn is_memo_valid(memo: &Memo, current_revision: Revision) -> bool {
    // Check if memo is from current revision
    if memo.revisions.changed_at == current_revision {
        return true;
    }
    
    // Check if any dependencies changed
    for (ingredient_index, id) in memo.revisions.tracked_struct_ids.iter() {
        if dependency_changed_since(ingredient_index, id, memo.revisions.changed_at) {
            return false;
        }
    }
    
    // Check durability-based invalidation
    let last_change = runtime.last_changed_revision(memo.revisions.durability);
    memo.revisions.changed_at >= last_change
}
```

### LRU Eviction
```rust
// From src/function/lru.rs
pub struct Lru {
    capacity: usize,
    // Internal LRU tracking
}

impl Lru {
    pub fn set_capacity(&mut self, capacity: usize) {
        self.capacity = capacity;
    }
    
    pub fn for_each_evicted(&mut self, mut f: impl FnMut(Id)) {
        // Evict least recently used memos
        while self.len() > self.capacity {
            if let Some(evicted_id) = self.pop_lru() {
                f(evicted_id);
            }
        }
    }
}
```

**Memory Management:**
- Each ingredient can have its own LRU capacity
- Automatic eviction when capacity exceeded
- Manual eviction via `db.trigger_lru_eviction()`

## Parallel Recomputation

### Work-Stealing Execution
```rust
// From src/parallel.rs (with rayon feature)
pub fn par_map<T, R>(
    items: impl IntoParallelIterator<Item = T>,
    f: impl Fn(T) -> R + Sync + Send,
) -> Vec<R>
where
    T: Send,
    R: Send,
{
    items.into_par_iter().map(f).collect()
}
```

### Thread Coordination
```rust
// From src/runtime.rs
pub(crate) fn block<'a>(
    &'a self,
    database_key: DatabaseKeyIndex,
    other_id: ThreadId,
    query_mutex_guard: SyncGuard<'a>,
) -> BlockResult<'a> {
    // Check for cycles
    if thread_id == other_id {
        return BlockResult::Cycle { same_thread: true };
    }
    
    let dg = self.dependency_graph.lock();
    if dg.depends_on(other_id, thread_id) {
        return BlockResult::Cycle { same_thread: false };
    }
    
    // Block until other thread completes
    BlockResult::Running(Running(/* ... */))
}
```

**Parallel Safety:**
- Queries can execute in parallel if they don't depend on each other
- Automatic blocking when dependencies are being computed
- Cycle detection prevents deadlocks

## Optimization Techniques

### Early Termination
```rust
#[salsa::tracked]
fn analyze_file(db: &dyn Database, file: SourceFile) -> Analysis {
    // Check if file has syntax errors first
    let errors = syntax_check(db, file);
    if !errors.is_empty() {
        return Analysis::with_errors(errors);
    }
    
    // Only do expensive analysis if syntax is valid
    expensive_semantic_analysis(db, file)
}
```

### Conditional Dependencies
```rust
#[salsa::tracked]
fn compile_with_optimization(db: &dyn Database, file: SourceFile, config: Config) -> Binary {
    let ast = parse(db, file);
    
    if config.optimization_enabled(db) {
        // Only depends on optimizer if optimization is enabled
        let optimized_ast = optimize(db, ast);
        codegen(db, optimized_ast)
    } else {
        codegen(db, ast)
    }
}
```

### Batch Processing
```rust
#[salsa::tracked]
fn process_files(db: &dyn Database, files: Vec<SourceFile>) -> Vec<Result> {
    // Process files in parallel
    salsa::par_map(files, |file| {
        process_single_file(db, file)
    })
}
```

## Performance Monitoring

### Event Tracking
```rust
use salsa::{Event, EventKind};

let db = MyDatabase::new(Some(Box::new(|event: Event| {
    match event.kind {
        EventKind::WillExecute { database_key } => {
            println!("Executing: {:?}", database_key);
        }
        EventKind::DidValidateMemoizedValue { database_key } => {
            println!("Reused memo: {:?}", database_key);
        }
        _ => {}
    }
})));
```

### Memory Usage Tracking
```rust
#[cfg(feature = "salsa_unstable")]
fn analyze_memory_usage(db: &dyn Database) {
    let struct_info = db.structs_info();
    for info in struct_info {
        println!("{}: {} instances, {} bytes", 
                 info.debug_name(), 
                 info.count(), 
                 info.size_of_fields() + info.size_of_metadata());
    }
    
    let query_info = db.queries_info();
    for (name, info) in query_info {
        println!("Query {}: {} memos, {} bytes", 
                 name, 
                 info.count(), 
                 info.size_of_fields() + info.size_of_metadata());
    }
}
```

## Best Practices

### Granularity
- **Fine-grained queries**: Better reuse, more overhead
- **Coarse-grained queries**: Less overhead, less reuse
- Profile to find the right balance

### Durability Assignment
- Use HIGH durability for rarely changing inputs (compiler version, target architecture)
- Use MEDIUM durability for occasionally changing inputs (configuration, build settings)
- Use LOW durability for frequently changing inputs (source code, user input)

### Memory Management
- Set appropriate LRU limits based on available memory
- Monitor memory usage in production
- Use `trigger_lru_eviction()` during idle periods

### Dependency Design
- Minimize dependencies where possible
- Use conditional dependencies to avoid unnecessary work
- Consider breaking large queries into smaller, more focused ones

This incremental recomputation system enables Salsa to achieve excellent performance by doing the minimum amount of work necessary while maintaining correctness and consistency.