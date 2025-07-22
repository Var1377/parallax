# Migration Guide to Salsa 0.23

This guide helps you migrate from earlier versions of Salsa to version 0.23, based on analysis of the actual changes in the codebase.

## Overview of Changes

Salsa 0.23 represents a significant evolution from previous versions with several breaking changes and new features:

### Major Changes
- **New macro system**: Simplified attribute-based macros
- **Tracked structs**: Replace the old entity system
- **Improved accumulator system**: Better performance and ergonomics
- **Enhanced parallel execution**: Work-stealing scheduler
- **Better memory management**: Improved LRU and garbage collection
- **Simplified database setup**: Streamlined database trait system

## Migration from Salsa 0.16/0.17

### Database Definition Changes

#### Old (0.16/0.17)
```rust
#[salsa::query_group(ParserDatabaseStorage)]
pub trait ParserDatabase: salsa::Database {
    #[salsa::input]
    fn source_text(&self, file: FileId) -> Arc<String>;
    
    fn parse(&self, file: FileId) -> Arc<ast::File>;
}

#[salsa::database(ParserDatabaseStorage)]
#[derive(Default)]
pub struct DatabaseImpl {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for DatabaseImpl {}
```

#### New (0.23)
```rust
#[salsa::input]
pub struct SourceFile {
    #[returns(ref)]
    pub text: String,
    pub path: PathBuf,
}

#[salsa::tracked]
fn parse(db: &dyn Database, file: SourceFile) -> Ast {
    let text = file.text(db);
    parse_text(text)
}

#[salsa::db]
#[derive(Default)]
pub struct DatabaseImpl {
    storage: salsa::Storage<Self>,
}

#[salsa::db]
impl salsa::Database for DatabaseImpl {}
```

### Input Definition Changes

#### Old (0.16/0.17)
```rust
#[salsa::query_group(InputsStorage)]
pub trait Inputs: salsa::Database {
    #[salsa::input]
    fn source_text(&self, file: FileId) -> Arc<String>;
    
    #[salsa::input]
    fn file_path(&self, file: FileId) -> PathBuf;
}

// Usage
db.set_source_text(file_id, Arc::new("new content".to_string()));
let content = db.source_text(file_id);
```

#### New (0.23)
```rust
#[salsa::input]
pub struct SourceFile {
    #[returns(ref)]
    pub text: String,
    pub path: PathBuf,
}

// Usage
let file = SourceFile::new(&db, "content".to_string(), "path.rs".into());
file.set_text(&mut db).to("new content".to_string());
let content = file.text(&db);
```

### Query Definition Changes

#### Old (0.16/0.17)
```rust
#[salsa::query_group(ParserStorage)]
pub trait Parser: Inputs {
    fn parse(&self, file: FileId) -> Arc<ast::File>;
    
    #[salsa::dependencies]
    fn parse_dependencies(&self, file: FileId) -> Vec<FileId>;
}

fn parse(db: &dyn Parser, file: FileId) -> Arc<ast::File> {
    let text = db.source_text(file);
    Arc::new(parse_text(&text))
}
```

#### New (0.23)
```rust
#[salsa::tracked]
fn parse(db: &dyn Database, file: SourceFile) -> Ast {
    let text = file.text(db);
    parse_text(text)
}

#[salsa::tracked]
fn parse_dependencies(db: &dyn Database, file: SourceFile) -> Vec<SourceFile> {
    let ast = parse(db, file);
    extract_dependencies(db, ast)
}
```

## Migration from Salsa 0.20+

### Entity to Tracked Struct Migration

#### Old (0.20+)
```rust
#[salsa::entity]
pub struct Function {
    #[id] pub name: String,
    #[value] pub body: ast::Block,
    #[value] pub return_type: Type,
}

// Usage
let func = Function::new(&db, "main".to_string(), body, return_type);
let body = func.body(&db);
```

#### New (0.23)
```rust
#[salsa::tracked]
pub struct Function<'db> {
    pub name: String,  // ID field (untracked)
    
    #[tracked]
    #[returns(ref)]
    pub body: ast::Block,  // Tracked field
    
    #[tracked]
    pub return_type: Type,  // Tracked field
}

// Usage
let func = Function::new(&db, "main".to_string(), body, return_type);
let body = func.body(&db);
```

### Interned Value Migration

#### Old (0.20+)
```rust
#[salsa::interned]
pub struct InternedString {
    #[value] pub text: String,
}

// Usage
let interned = InternedString::new(&db, "hello".to_string());
let text = interned.text(&db);
```

#### New (0.23)
```rust
#[salsa::interned]
pub struct InternedString<'db> {
    #[returns(ref)]
    pub text: String,
}

// Usage
let interned = InternedString::new(&db, "hello".to_string());
let text = interned.text(&db);
```

## Step-by-Step Migration Process

### Step 1: Update Dependencies
```toml
[dependencies]
salsa = "0.23"
```

### Step 2: Migrate Database Structure

1. **Remove query groups**: Replace `#[salsa::query_group]` with individual `#[salsa::tracked]` functions
2. **Update database macro**: Change `#[salsa::database]` to `#[salsa::db]`
3. **Simplify database implementation**: Remove storage type parameters

#### Before
```rust
#[salsa::query_group(CompilerDatabaseStorage)]
pub trait CompilerDatabase: salsa::Database {
    #[salsa::input]
    fn source_text(&self, file: FileId) -> Arc<String>;
    
    fn parse(&self, file: FileId) -> Arc<ast::File>;
    fn type_check(&self, file: FileId) -> Arc<TypedAst>;
}

#[salsa::database(CompilerDatabaseStorage)]
#[derive(Default)]
pub struct DatabaseImpl {
    storage: salsa::Storage<Self>,
}
```

#### After
```rust
#[salsa::input]
pub struct SourceFile {
    #[returns(ref)]
    pub text: String,
}

#[salsa::tracked]
fn parse(db: &dyn Database, file: SourceFile) -> Ast { /* ... */ }

#[salsa::tracked]
fn type_check(db: &dyn Database, file: SourceFile) -> TypedAst { /* ... */ }

#[salsa::db]
#[derive(Default)]
pub struct DatabaseImpl {
    storage: salsa::Storage<Self>,
}

#[salsa::db]
impl salsa::Database for DatabaseImpl {}
```

### Step 3: Migrate Input Definitions

1. **Convert input queries to input structs**
2. **Add appropriate field attributes**
3. **Update usage sites**

#### Migration Pattern
```rust
// Old
#[salsa::input]
fn source_text(&self, file: FileId) -> Arc<String>;

// New
#[salsa::input]
pub struct SourceFile {
    #[returns(ref)]
    pub text: String,
}
```

### Step 4: Migrate Entities to Tracked Structs

1. **Replace `#[salsa::entity]` with `#[salsa::tracked]`**
2. **Mark value fields with `#[tracked]`**
3. **Add lifetime parameter**

#### Migration Pattern
```rust
// Old
#[salsa::entity]
pub struct MyEntity {
    #[id] pub name: String,
    #[value] pub data: Data,
}

// New
#[salsa::tracked]
pub struct MyEntity<'db> {
    pub name: String,  // ID field
    
    #[tracked]
    pub data: Data,    // Tracked field
}
```

### Step 5: Update Query Functions

1. **Add `#[salsa::tracked]` attribute**
2. **Update function signatures**
3. **Remove manual dependency tracking**

#### Migration Pattern
```rust
// Old
fn my_query(db: &dyn MyDatabase, input: FileId) -> Result {
    // Manual implementation
}

// New
#[salsa::tracked]
fn my_query(db: &dyn Database, input: SourceFile) -> Result {
    // Automatic dependency tracking
}
```

### Step 6: Migrate Accumulator Usage

#### Old (if using custom accumulator pattern)
```rust
// Manual accumulator implementation
pub struct DiagnosticAccumulator {
    diagnostics: Vec<Diagnostic>,
}

fn collect_diagnostics(db: &dyn Database, file: FileId) -> Vec<Diagnostic> {
    let mut acc = DiagnosticAccumulator::new();
    analyze_file(db, file, &mut acc);
    acc.diagnostics
}
```

#### New (0.23)
```rust
#[salsa::accumulator]
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub message: String,
    pub span: Span,
}

#[salsa::tracked]
fn analyze_file(db: &dyn Database, file: SourceFile) {
    // Accumulate diagnostics during analysis
    if some_error {
        Diagnostic {
            message: "Error message".to_string(),
            span: error_span,
        }.accumulate(db);
    }
}

// Collect accumulated diagnostics
let diagnostics = analyze_file::accumulated::<Diagnostic>(&db, file);
```

## Common Migration Issues and Solutions

### Issue 1: Lifetime Errors with Tracked Structs

#### Problem
```rust
// This won't compile
fn store_tracked_struct(tracked: MyTrackedStruct) {
    // Error: tracked struct has lifetime tied to database
}
```

#### Solution
```rust
// Store the ID instead
fn store_tracked_struct_id(id: salsa::Id) {
    // IDs can be stored and used later
}

// Or use within database lifetime
fn process_tracked_struct(db: &dyn Database, tracked: MyTrackedStruct<'_>) {
    // Use tracked struct within database lifetime
}
```

### Issue 2: Return Type Changes

#### Problem
```rust
// Old: returned Arc<T>
fn old_query(db: &dyn Database, input: FileId) -> Arc<Ast> { /* ... */ }

// New: returns T directly
#[salsa::tracked]
fn new_query(db: &dyn Database, input: SourceFile) -> Ast { /* ... */ }
```

#### Solution
```rust
// Use #[returns(ref)] for reference types
#[salsa::tracked(return_ref)]
fn new_query(db: &dyn Database, input: SourceFile) -> Vec<Item> { /* ... */ }

// This returns &Vec<Item> instead of Vec<Item>
```

### Issue 3: Database Trait Composition

#### Problem
```rust
// Old: complex trait hierarchies
pub trait ParserDatabase: salsa::Database { /* ... */ }
pub trait TypeCheckerDatabase: ParserDatabase { /* ... */ }
```

#### Solution
```rust
// New: simpler trait composition
#[salsa::db]
pub trait ParserDatabase: Database { /* ... */ }

#[salsa::db]
pub trait TypeCheckerDatabase: ParserDatabase { /* ... */ }

// Implementation
#[salsa::db]
impl ParserDatabase for MyDatabase {}

#[salsa::db]
impl TypeCheckerDatabase for MyDatabase {}
```

### Issue 4: Parallel Execution

#### Problem
```rust
// Old: manual parallel execution
use rayon::prelude::*;

files.par_iter().map(|file| process_file(db, *file)).collect()
```

#### Solution
```rust
// New: use Salsa's parallel utilities
#[salsa::tracked]
fn process_files(db: &dyn Database, files: Vec<SourceFile>) -> Vec<Result> {
    salsa::par_map(files, |file| process_file(db, file))
}
```

## Performance Considerations

### Memory Usage
- **Tracked structs** use more memory than simple values but provide better incrementality
- **LRU limits** should be configured based on available memory
- **Durability levels** should be used to minimize recomputation

### Compilation Time
- **Macro expansion** in 0.23 is faster than previous versions
- **Incremental compilation** benefits are more pronounced
- **Parallel execution** is more efficient with the new work-stealing scheduler

## Testing Migration

### Create Migration Tests
```rust
#[cfg(test)]
mod migration_tests {
    use super::*;
    
    #[test]
    fn test_equivalent_behavior() {
        // Test that new implementation produces same results as old
        let mut old_db = OldDatabase::default();
        let mut new_db = NewDatabase::default();
        
        // Set up equivalent inputs
        let old_input = setup_old_input(&mut old_db);
        let new_input = setup_new_input(&mut new_db);
        
        // Compare results
        let old_result = old_computation(&old_db, old_input);
        let new_result = new_computation(&new_db, new_input);
        
        assert_eq!(old_result, new_result);
    }
    
    #[test]
    fn test_incremental_behavior() {
        // Test that incremental behavior is preserved
        let mut db = NewDatabase::default();
        
        let input = SourceFile::new(&db, "initial".to_string());
        let result1 = expensive_computation(&db, input);
        
        // Modify input
        input.set_text(&mut db).to("modified".to_string());
        let result2 = expensive_computation(&db, input);
        
        // Verify different results
        assert_ne!(result1, result2);
    }
}
```

## Gradual Migration Strategy

For large codebases, consider a gradual migration:

1. **Start with new database structure** - migrate the database definition first
2. **Migrate inputs one by one** - convert input queries to input structs
3. **Convert leaf queries first** - start with queries that don't depend on others
4. **Work up the dependency chain** - migrate queries in dependency order
5. **Test incrementally** - ensure each step maintains correctness

This migration guide should help you successfully upgrade to Salsa 0.23 while taking advantage of its improved performance and ergonomics.