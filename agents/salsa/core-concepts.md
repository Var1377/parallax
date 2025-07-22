# Core Concepts

This document explains the fundamental concepts in Salsa 0.23, based on analysis of the actual source code implementation.

## Architecture Overview

Salsa's architecture consists of several key components working together:

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Database      │    │    Runtime      │    │   Ingredients   │
│                 │◄──►│                 │◄──►│                 │
│ - Storage       │    │ - Revisions     │    │ - Input         │
│ - Zalsa         │    │ - Dependencies  │    │ - Function      │
│ - Views         │    │ - Cancellation  │    │ - TrackedStruct │
└─────────────────┘    └─────────────────┘    └─────────────────┘
         ▲                       ▲                       ▲
         │                       │                       │
         ▼                       ▼                       ▼
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│     Table       │    │ DependencyGraph │    │  Accumulator    │
│                 │    │                 │    │                 │
│ - Memo Storage  │    │ - Cycle Detect  │    │ - Value Collect │
│ - LRU Eviction  │    │ - Thread Safety │    │ - Query Results │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

## Database

The database is the central coordinator in Salsa. From the source code analysis:

### Database Trait
```rust
pub trait Database: Send + AsDynDatabase + Any + ZalsaDatabase {
    // Core methods for revision management
    fn trigger_lru_eviction(&mut self);
    fn synthetic_write(&mut self, durability: Durability);
    fn report_untracked_read(&self);
    fn unwind_if_revision_cancelled(&self);
}
```

### Key Responsibilities
1. **Storage Management**: Holds all memoized values and tracked data
2. **Revision Tracking**: Manages the current revision and change detection
3. **Dependency Coordination**: Tracks which queries depend on which inputs
4. **Memory Management**: Handles LRU eviction and cleanup

### Database Implementation
```rust
#[salsa::db]
#[derive(Default)]
pub struct MyDatabase {
    storage: salsa::Storage<Self>,
}

#[salsa::db]
impl salsa::Database for MyDatabase {}
```

The `#[salsa::db]` macro generates:
- Ingredient registration and lookup
- View casting for database traits
- Storage initialization and management

## Ingredients

Ingredients are the building blocks of Salsa's type system. Each ingredient type handles a specific kind of data:

### Input Ingredients
Handle mutable base data:

```rust
// From src/input.rs
pub struct IngredientImpl<C: Configuration> {
    ingredient_index: IngredientIndex,
    singleton: C::Singleton,
    memo_table_types: Arc<MemoTableTypes>,
}
```

**Key Features:**
- Store field values with individual revision tracking
- Support durability levels for different change frequencies
- Provide setter methods for mutations

### Function Ingredients
Handle derived computations:

```rust
// From src/function.rs
pub struct IngredientImpl<C: Configuration> {
    index: IngredientIndex,
    memo_ingredient_indices: MemoIngredientMap,
    lru: lru::Lru,
    view_caster: DatabaseDownCaster<C::DbView>,
    sync_table: SyncTable,
    deleted_entries: DeletedEntries<C>,
}
```

**Key Features:**
- Memoize function results
- Track dependencies automatically
- Handle cycle detection and recovery
- Support parallel execution

### Tracked Struct Ingredients
Handle structured data with tracked fields:

```rust
// From src/tracked_struct.rs
pub struct IngredientImpl<C: Configuration> {
    ingredient_index: IngredientIndex,
    phantom: PhantomData<fn() -> Value<C>>,
    free_list: SegQueue<Id>,
    memo_table_types: Arc<MemoTableTypes>,
}
```

**Key Features:**
- Efficient identity-based tracking
- Field-level change detection
- Automatic cleanup of unused structs

## Revisions and Change Detection

### Revision System
```rust
// From src/revision.rs
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Revision(u32);

impl Revision {
    pub fn start() -> Self { Revision(1) }
    pub fn next(self) -> Self { Revision(self.0 + 1) }
}
```

### Durability Levels
```rust
// From src/durability.rs
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Durability(u8);

impl Durability {
    pub const MIN: Durability = Durability(0);  // Changes every revision
    pub const LOW: Durability = Durability(1);  // File system changes
    pub const MEDIUM: Durability = Durability(2); // Configuration changes
    pub const HIGH: Durability = Durability(3); // Rare changes
}
```

**Usage Pattern:**
- Higher durability = less frequent changes
- When high durability input changes, all lower durability caches are invalidated
- Enables efficient incremental computation

### Change Propagation
1. **Input Change**: Setter method called on input
2. **Revision Increment**: New revision created
3. **Dependency Invalidation**: Dependent queries marked stale
4. **Lazy Recomputation**: Queries recomputed on next access

## Query Types

### Input Queries
```rust
#[salsa::input]
pub struct SourceFile {
    #[returns(ref)]
    pub text: String,
    pub path: PathBuf,
}
```

**Generated Methods:**
- `SourceFile::new(db, text, path)` - Constructor
- `file.text(db)` - Field accessor
- `file.set_text(&mut db).to(new_text)` - Field setter

### Tracked Functions
```rust
#[salsa::tracked]
fn parse_file(db: &dyn Database, file: SourceFile) -> Ast {
    // Implementation automatically memoized
    let text = file.text(db);
    parse_text(text)
}
```

**Automatic Features:**
- Dependency tracking on `file.text(db)` call
- Memoization of return value
- Incremental recomputation when dependencies change

### Tracked Structs
```rust
#[salsa::tracked]
pub struct ParsedFile<'db> {
    #[tracked]
    pub ast: Ast,
    
    #[tracked]
    pub errors: Vec<ParseError>,
    
    // Untracked fields don't trigger recomputation
    pub metadata: Metadata,
}
```

**Field Types:**
- `#[tracked]` fields: Changes trigger dependent query recomputation
- Untracked fields: Changes don't affect dependents (used for metadata)

### Interned Values
```rust
#[salsa::interned]
pub struct Symbol<'db> {
    #[returns(ref)]
    pub text: String,
}
```

**Benefits:**
- Deduplication: Same text gets same Symbol ID
- Efficient comparison: Compare IDs instead of strings
- Stable identity: Same text always gets same Symbol

## Dependency Tracking

### Automatic Tracking
Salsa automatically tracks dependencies when:
- Accessing input fields: `file.text(db)`
- Calling tracked functions: `parse_file(db, file)`
- Accessing tracked struct fields: `parsed.ast(db)`

### Dependency Graph
```rust
// From src/runtime/dependency_graph.rs
pub struct DependencyGraph {
    // Tracks which queries are waiting for which other queries
    // Enables cycle detection and parallel execution
}
```

### Cycle Detection
```rust
#[salsa::tracked(cycle = recover)]
fn recursive_function(db: &dyn Database, input: Input) -> Output {
    // Can call itself recursively
    // Salsa detects cycles and calls recovery function
}

fn recover_recursive_function(
    db: &dyn Database, 
    cycle: &salsa::Cycle, 
    input: Input
) -> Output {
    // Provide fallback value for cycle
    Output::default()
}
```

## Memory Management

### Memoization
```rust
// From src/function/memo.rs
pub struct Memo<'db, C: Configuration> {
    pub revisions: MemoRevisions,
    pub value: C::Output<'db>,
}
```

**Memo Lifecycle:**
1. **Creation**: When function first executes
2. **Validation**: Check if dependencies changed
3. **Reuse**: Return cached value if still valid
4. **Eviction**: Remove when memory pressure or explicit cleanup

### LRU Eviction
```rust
// From src/function/lru.rs
pub struct Lru {
    capacity: usize,
    // Tracks least recently used memos for eviction
}
```

**Eviction Strategy:**
- Configurable capacity per ingredient
- Automatic eviction when capacity exceeded
- Manual eviction via `db.trigger_lru_eviction()`

### Garbage Collection
- **Tracked Structs**: Automatically deleted when creating query no longer produces them
- **Memos**: Evicted based on LRU policy
- **Dependencies**: Cleaned up when queries are evicted

## Parallel Execution

### Work-Stealing Scheduler
```rust
// Available with rayon feature
use salsa::{par_map, join};

#[salsa::tracked]
fn process_files(db: &dyn Database, files: Vec<SourceFile>) -> Vec<Result> {
    // Parallel processing of independent files
    par_map(files, |file| process_single_file(db, file))
}
```

### Thread Safety
- **Database**: Can be shared across threads (with appropriate synchronization)
- **Snapshots**: Immutable views for parallel read access
- **Cancellation**: Automatic cancellation when inputs change during computation

### Blocking and Synchronization
```rust
// From src/runtime.rs
pub enum BlockResult<'me> {
    Running(Running<'me>),  // Another thread is computing
    Cycle { same_thread: bool },  // Cycle detected
}
```

**Coordination:**
- Queries block on dependencies being computed by other threads
- Cycle detection prevents deadlocks
- Cancellation propagates across thread boundaries

## Event System

### Event Types
```rust
// From src/event.rs
pub enum EventKind {
    WillExecute { database_key: DatabaseKeyIndex },
    DidValidateMemoizedValue { database_key: DatabaseKeyIndex },
    WillBlockOn { other_thread_id: ThreadId, database_key: DatabaseKeyIndex },
    WillDiscardStaleOutput { execute_key: DatabaseKeyIndex, output_key: DatabaseKeyIndex },
    DidDiscard { key: DatabaseKeyIndex },
}
```

### Event Handling
```rust
let db = MyDatabase::new(Some(Box::new(|event| {
    match event.kind {
        EventKind::WillExecute { database_key } => {
            println!("Executing: {:?}", database_key);
        }
        _ => {}
    }
})));
```

**Use Cases:**
- Performance profiling
- Debug logging
- Progress reporting
- Custom metrics collection

This architecture enables Salsa to provide efficient incremental computation with automatic dependency tracking, memoization, and parallel execution while maintaining memory safety and preventing common pitfalls like cycles and memory leaks.