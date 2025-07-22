# Query System Architecture

This document explains Salsa's query system based on analysis of the actual implementation in version 0.23.

## Query Types Overview

Salsa provides four main types of queries, each implemented as different "ingredients":

1. **Input Queries** - Mutable base data
2. **Tracked Functions** - Derived computations  
3. **Tracked Structs** - Structured data with field-level tracking
4. **Interned Values** - Deduplicated immutable values

## Input Queries

Input queries represent the base facts that can change over time.

### Definition
```rust
#[salsa::input]
pub struct SourceFile {
    #[returns(ref)]
    pub text: String,
    
    pub path: PathBuf,
    
    #[durability(salsa::Durability::HIGH)]
    pub encoding: Encoding,
}
```

### Generated API
The macro generates:
```rust
impl SourceFile {
    // Constructor
    pub fn new(db: &dyn Database, text: String, path: PathBuf, encoding: Encoding) -> Self;
    
    // Field accessors
    pub fn text(self, db: &dyn Database) -> &String;
    pub fn path(self, db: &dyn Database) -> PathBuf;
    pub fn encoding(self, db: &dyn Database) -> Encoding;
    
    // Field setters
    pub fn set_text(self, db: &mut dyn Database) -> Setter<String>;
    pub fn set_path(self, db: &mut dyn Database) -> Setter<PathBuf>;
    pub fn set_encoding(self, db: &mut dyn Database) -> Setter<Encoding>;
}
```

### Usage Patterns
```rust
fn main() {
    let mut db = MyDatabase::default();
    
    // Create input
    let file = SourceFile::new(&db, "let x = 42;".to_string(), "main.rs".into(), Encoding::Utf8);
    
    // Read fields (creates dependencies)
    let content = file.text(&db);
    let path = file.path(&db);
    
    // Modify fields (triggers new revision)
    file.set_text(&mut db).to("let y = 24;".to_string());
    
    // Chained setters
    file.set_text(&mut db)
        .to("new content".to_string())
        .set_path()
        .to("new_path.rs".into());
}
```

### Field Attributes

#### `#[returns(ref)]`
For fields that should return references:
```rust
#[salsa::input]
pub struct Document {
    #[returns(ref)]
    pub content: String,  // Returns &String
    
    #[returns(ref)]
    pub lines: Vec<String>,  // Returns &Vec<String>
    
    pub line_count: usize,  // Returns usize (copy)
}
```

#### `#[durability(...)]`
Set durability levels for different change frequencies:
```rust
#[salsa::input]
pub struct Config {
    #[durability(salsa::Durability::HIGH)]
    pub optimization_level: u8,  // Rarely changes
    
    #[durability(salsa::Durability::MEDIUM)]
    pub target_arch: String,  // Changes occasionally
    
    #[durability(salsa::Durability::LOW)]
    pub debug_mode: bool,  // Changes frequently
}
```

### Implementation Details

From `src/input.rs`, inputs are implemented as:
```rust
pub struct Value<C> {
    fields: C::Fields,
    revisions: C::Revisions,  // Per-field revision tracking
    durabilities: C::Durabilities,  // Per-field durability
    memos: MemoTable,  // For derived queries on this input
}
```

Key features:
- **Per-field tracking**: Each field has its own revision
- **Durability management**: Fields can have different durability levels
- **Memo storage**: Can store memoized results of queries on this input

## Tracked Functions

Tracked functions are the workhorses of Salsa - they compute derived values and are automatically memoized.

### Basic Definition
```rust
#[salsa::tracked]
fn parse_file(db: &dyn Database, file: SourceFile) -> Ast {
    let text = file.text(db);  // Creates dependency
    parse_text(text)
}
```

### Advanced Features

#### Return References
```rust
#[salsa::tracked(return_ref)]
fn file_lines(db: &dyn Database, file: SourceFile) -> Vec<String> {
    file.text(db).lines().map(|s| s.to_string()).collect()
}

// Usage - returns &Vec<String>
let lines: &Vec<String> = file_lines(db, file);
```

#### No Equality Check
```rust
#[salsa::tracked(no_eq)]
fn current_time(db: &dyn Database) -> SystemTime {
    // Always considered "changed" even if value is the same
    SystemTime::now()
}
```

#### Cycle Recovery
```rust
#[salsa::tracked(cycle = recover)]
fn recursive_size(db: &dyn Database, node: Node) -> usize {
    let mut size = 1;
    for child in node.children(db) {
        size += recursive_size(db, child);  // May create cycle
    }
    size
}

// Recovery function called when cycle detected
fn recover_recursive_size(
    db: &dyn Database,
    cycle: &salsa::Cycle,
    node: Node,
) -> usize {
    // Return reasonable default for cycles
    1
}
```

### Implementation Details

From `src/function.rs`, tracked functions use:
```rust
pub struct IngredientImpl<C: Configuration> {
    index: IngredientIndex,
    memo_ingredient_indices: MemoIngredientMap,
    lru: lru::Lru,  // For eviction
    sync_table: SyncTable,  // For parallel execution
    deleted_entries: DeletedEntries<C>,  // Cleanup
}
```

The function execution flow:
1. **Fetch**: Check if memoized value exists and is valid
2. **Execute**: Run function if needed, tracking dependencies
3. **Store**: Memoize result with dependency information
4. **Return**: Provide result to caller

## Tracked Structs

Tracked structs provide structured data with field-level change tracking.

### Definition
```rust
#[salsa::tracked]
pub struct ParsedFile<'db> {
    // Tracked fields - changes trigger recomputation
    #[tracked]
    pub ast: Ast,
    
    #[tracked]
    #[returns(ref)]
    pub errors: Vec<ParseError>,
    
    // Untracked fields - changes don't affect dependents
    pub source_map: SourceMap,
    pub metadata: FileMetadata,
}
```

### Constructor Function
```rust
#[salsa::tracked]
fn parse_file_tracked(db: &dyn Database, file: SourceFile) -> ParsedFile<'_> {
    let text = file.text(db);
    let (ast, errors, source_map) = parse_with_recovery(text);
    
    ParsedFile::new(
        db,
        ast,
        errors,
        source_map,
        FileMetadata::new(),
    )
}
```

### Field Access
```rust
fn analyze_file(db: &dyn Database, file: SourceFile) {
    let parsed = parse_file_tracked(db, file);
    
    // Accessing tracked fields creates dependencies
    let ast = parsed.ast(db);
    let errors = parsed.errors(db);
    
    // Accessing untracked fields doesn't create dependencies
    let metadata = parsed.metadata(db);
}
```

### Identity and Equality

Tracked structs have identity based on their constructor arguments:
```rust
#[salsa::tracked]
fn create_symbol(db: &dyn Database, name: String) -> Symbol<'_> {
    Symbol::new(db, name.clone(), compute_hash(&name))
}

// Same name = same identity = same Symbol instance
let sym1 = create_symbol(db, "foo".to_string());
let sym2 = create_symbol(db, "foo".to_string());
assert_eq!(sym1.id(), sym2.id());  // Same identity
```

### Implementation Details

From `src/tracked_struct.rs`:
```rust
pub struct Value<C> {
    durability: Durability,
    updated_at: OptionalAtomicRevision,  // Locking mechanism
    fields: C::Fields<'static>,
    revisions: C::Revisions,  // Per-field change tracking
    memos: MemoTable,
}
```

Key features:
- **Identity-based**: Same constructor args = same identity
- **Field-level tracking**: Each tracked field has its own revision
- **Automatic cleanup**: Unused structs are garbage collected
- **Thread safety**: Atomic operations for concurrent access

## Interned Values

Interned values provide deduplication and stable identities for immutable data.

### Definition
```rust
#[salsa::interned]
pub struct Symbol<'db> {
    #[returns(ref)]
    pub text: String,
}

#[salsa::interned]
pub struct TypeSignature<'db> {
    #[returns(ref)]
    pub params: Vec<Type>,
    
    pub return_type: Type,
}
```

### Usage
```rust
fn intern_symbols(db: &dyn Database) {
    // Same text = same Symbol instance
    let sym1 = Symbol::new(db, "hello".to_string());
    let sym2 = Symbol::new(db, "hello".to_string());
    
    assert_eq!(sym1.id(), sym2.id());  // Same identity
    assert_eq!(sym1.text(db), sym2.text(db));  // Same content
    
    // Efficient comparison by ID
    if sym1.id() == sym2.id() {
        // Much faster than string comparison
    }
}
```

### Benefits
1. **Memory efficiency**: Duplicate values stored only once
2. **Fast comparison**: Compare IDs instead of content
3. **Stable identity**: Same content always gets same ID
4. **Automatic deduplication**: No manual cache management needed

## Query Composition

### Dependency Chains
```rust
#[salsa::input]
pub struct SourceFile { /* ... */ }

#[salsa::tracked]
fn tokenize(db: &dyn Database, file: SourceFile) -> Vec<Token> {
    // Depends on file.text
}

#[salsa::tracked]
fn parse(db: &dyn Database, file: SourceFile) -> Ast {
    let tokens = tokenize(db, file);  // Depends on tokenize
    parse_tokens(tokens)
}

#[salsa::tracked]
fn type_check(db: &dyn Database, file: SourceFile) -> TypedAst {
    let ast = parse(db, file);  // Depends on parse
    check_types(ast)
}
```

Dependency graph:
```
SourceFile.text → tokenize → parse → type_check
```

When `SourceFile.text` changes:
1. `tokenize` is invalidated and re-executed
2. `parse` is invalidated and re-executed (uses new tokenize result)
3. `type_check` is invalidated and re-executed (uses new parse result)

### Cross-cutting Concerns
```rust
#[salsa::tracked]
fn analyze_file(db: &dyn Database, file: SourceFile, config: Config) -> Analysis {
    let ast = parse(db, file);  // Depends on file
    let settings = config.analysis_settings(db);  // Depends on config
    
    perform_analysis(ast, settings)
}
```

This function depends on both `file` and `config`. Changes to either will trigger recomputation.

## Advanced Query Patterns

### Conditional Dependencies
```rust
#[salsa::tracked]
fn compile_file(db: &dyn Database, file: SourceFile, config: Config) -> Binary {
    let ast = parse(db, file);
    
    if config.optimization_enabled(db) {
        let optimized = optimize(db, ast);  // Only depends on optimize if enabled
        generate_code(optimized)
    } else {
        generate_code(ast)
    }
}
```

### Accumulating Results
```rust
#[salsa::accumulator]
#[derive(Debug)]
pub struct Diagnostic {
    pub message: String,
    pub span: Span,
}

#[salsa::tracked]
fn check_file(db: &dyn Database, file: SourceFile) {
    let ast = parse(db, file);
    
    for node in ast.nodes() {
        if let Some(error) = validate_node(node) {
            Diagnostic {
                message: error.message,
                span: node.span,
            }.accumulate(db);
        }
    }
}

// Collect all diagnostics
let diagnostics = check_file::accumulated::<Diagnostic>(db, file);
```

### Query Families
```rust
#[salsa::tracked]
fn file_hash(db: &dyn Database, file: SourceFile, algorithm: HashAlgorithm) -> Hash {
    let content = file.text(db);
    compute_hash(content, algorithm)
}

// Different algorithms create different queries
let md5_hash = file_hash(db, file, HashAlgorithm::Md5);
let sha256_hash = file_hash(db, file, HashAlgorithm::Sha256);
```

## Performance Considerations

### Granularity
- **Fine-grained**: More opportunities for reuse, but more overhead
- **Coarse-grained**: Less overhead, but less reuse

```rust
// Fine-grained - better reuse
#[salsa::tracked]
fn tokenize(db: &dyn Database, file: SourceFile) -> Vec<Token> { /* ... */ }

#[salsa::tracked]
fn parse(db: &dyn Database, tokens: Vec<Token>) -> Ast { /* ... */ }

// Coarse-grained - less overhead
#[salsa::tracked]
fn parse_file(db: &dyn Database, file: SourceFile) -> Ast {
    let tokens = tokenize_inline(file.text(db));
    parse_tokens(tokens)
}
```

### Memoization Overhead
- Small, frequently-called functions may not benefit from memoization
- Large, expensive computations should always be memoized
- Use profiling to identify bottlenecks

### Memory Usage
- Tracked structs with many fields use more memory
- Interned values save memory for duplicated data
- Configure LRU limits based on available memory

This query system provides the foundation for efficient incremental computation while maintaining simplicity and type safety.