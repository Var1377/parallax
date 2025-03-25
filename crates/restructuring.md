# Parallax Compiler Restructuring with Salsa

## Overview

This document outlines a comprehensive plan for restructuring the Parallax compiler using the Salsa framework to achieve efficient incremental compilation. The restructuring aims to leverage Salsa's capabilities for dependency tracking, memoization, and parallel evaluation while maintaining the existing architectural components of Parallax.

## Current Architecture

Currently, the Parallax compiler consists of these core crates:

- `parallax-lang`: Parser and AST
- `parallax-resolve`: Name resolution
- `parallax-typeck`: Type checking
- `parallax-hir`: High-level IR
- `parallax-mir`: Mid-level IR
- `parallax-codegen`: Code generation
- `parallax-net`: Runtime
- `parallax-hvm`: HVM integration
- `parallax-cli`: CLI

## Understanding Salsa

### Salsa Core Concepts

Salsa is an incremental computation framework designed specifically for compiler-like applications. It provides several key features:

1. **Query-based Architecture**: Computation is organized as queries with inputs and functions.
2. **Automatic Dependency Tracking**: Salsa automatically tracks dependencies between queries.
3. **Incremental Recomputation**: Only recomputes what's necessary when inputs change.
4. **Parallel Execution**: Supports concurrent query evaluation.
5. **Cycle Detection**: Handles cyclic dependencies in the query graph.

### Key Salsa Components

#### Salsa Database

The database is the central component that stores all intermediate state:

```rust
#[salsa::db(Jar1, Jar2, ...)]
pub struct Database {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for Database {
    fn salsa_event(&self, event: salsa::Event) {
        // Optional event handling
    }
}
```

#### Jars

Jars group related functionality and allow modular, crate-based organization:

```rust
#[salsa::jar(db = Db)]
pub struct Jar(
    InputQuery1,
    InputQuery2,
    TrackedFunction1,
    TrackedFunction2,
    TrackedStruct1,
);
```

#### Database Trait

Each jar defines a database trait that provides dependency isolation:

```rust
pub trait Db: salsa::DbWithJar<Jar> {}
```

#### Inputs

Inputs are the base values that can be modified from outside the system:

```rust
#[salsa::input]
pub struct SourceFile {
    pub path: PathBuf,
    #[return_ref]
    pub text: String,
}
```

#### Tracked Functions

Functions whose results are cached and dependencies tracked:

```rust
#[salsa::tracked]
fn parse_file(db: &dyn Db, file: SourceFile) -> Ast {
    // Parsing logic
}
```

#### Tracked Structs

Intermediate data structures created during computation:

```rust
#[salsa::tracked]
struct ResolvedModule {
    #[return_ref]
    symbols: Vec<Symbol>,
    #[return_ref]
    errors: Vec<ResolveError>,
}
```

## Advanced Salsa Features and Their Applications

### 1. Durability

Salsa's durability feature allows you to specify how persistent a value should be across database revisions:

```rust
file.set_contents(&mut db)
    .with_durability(salsa::Durability::HIGH)
    .to(String::from("New content"));
```

**Application**: Use different durability levels for:
- **HIGH**: Project configuration that rarely changes
- **MEDIUM** (default): Normal source code
- **LOW**: Temporary files or experimental code

### 2. On-Demand Inputs

On-demand inputs allow you to compute input values lazily:

```rust
#[salsa::input]
fn file_contents(db: &dyn Db, path: PathBuf) -> String {
    // Load file contents on demand
    std::fs::read_to_string(&path).unwrap_or_default()
}
```

**Application**: Loading external dependencies or standard library files only when needed, improving startup performance.

### 3. Interning

Salsa provides efficient interning to create canonical representations of values:

```rust
#[salsa::interned]
struct Symbol {
    text: String,
}
```

**Application**: Interning identifiers, types, and other common structures to reduce memory usage and enable fast equality comparisons.

### 4. Accumulators

Accumulators allow collecting data across multiple queries:

```rust
#[salsa::accumulator]
struct Diagnostics(Diagnostic);
```

**Application**: Gathering compiler errors, warnings, and other diagnostics from different compilation phases without passing them explicitly.

### 5. Cycle Handling

Salsa can detect cycles in the dependency graph:

```rust
#[salsa::tracked(cycle(strategy = "panic"))]
fn check_item(db: &dyn Db, item: Item) -> Type {
    // Logic that might form a cycle
}
```

**Application**: Handling potential cycles in type inference, mutual recursion, and module dependencies.

### 6. Debug Support

Salsa provides debugging utilities to visualize the dependency graph:

```rust
db.debug_with(|db| {
    println!("{:#?}", db.debug_query(query));
});
```

**Application**: Troubleshooting incremental compilation issues and understanding performance bottlenecks.

### 7. #[return_ref] Optimization

The `#[return_ref]` attribute returns references to database-stored values instead of cloning them:

```rust
#[salsa::tracked]
struct TypedAst {
    #[return_ref]
    nodes: Vec<Node>,  // Returns &Vec<Node> instead of Vec<Node>
}
```

**Application**: Avoiding expensive clones for large data structures like ASTs, symbol tables, and type information.

### 8. #[id] Fields

Marked fields help with more intelligent entity tracking across revisions:

```rust
#[salsa::tracked]
struct Symbol {
    #[id] name: String,  // Use name to match symbols across revisions
    data: SymbolData,
}
```

**Application**: Better incremental performance by tracking entities by logical identifiers rather than creation order.

## Design Decisions and Justifications

### 1. Fine-grained Module Tracking

**Decision**: Track at the module level rather than the whole program.

**Justification**: This balances granularity with overhead. Tracking each expression would create too much overhead, while tracking the entire program would lose most incremental benefits. Modules provide a natural boundary that aligns with how developers think about code organization.

### 2. Explicit Query Dependencies

**Decision**: Make dependencies explicit in function parameters.

**Justification**: While Salsa can track dependencies automatically through database access, explicit parameters make the dependency graph clearer and easier to maintain. This also enables better compiler warnings about unused parameters.

### 3. Strategic Return Reference Usage

**Decision**: Use `#[return_ref]` for large data structures but not for small values.

**Justification**: References avoid expensive clones for large structures but add indirection overhead for small values. For example, returning a reference to a `Vec<Node>` makes sense, but returning a reference to a `bool` or `u32` is counterproductive.

### 4. Parallel Database Implementation

**Decision**: Implement `ParallelDatabase` for all database types.

**Justification**: The parallel interaction net runtime in Parallax pairs naturally with parallel query execution in Salsa. This allows effective use of multiple cores during compilation with minimal additional code.

### 5. Clear Separation of Inputs and Derived Values

**Decision**: Use `#[salsa::input]` only for true external inputs.

**Justification**: Clear separation makes it easier to understand what can trigger recomputation and simplifies testing by providing well-defined input points.

### 6. Accumulator Usage for Diagnostics

**Decision**: Use accumulators for error and warning collection.

**Justification**: Errors can be generated at multiple points in the compilation pipeline. Accumulators provide a clean way to collect them without threading error lists through every function call.

### 7. Module-level Database Traits

**Decision**: Keep database traits focused on specific modules with minimal dependencies.

**Justification**: This enables better separation of concerns and allows parts of the compiler to be used independently. For example, the type checker can be used without code generation.

### 8. Snapshots for LSP

**Decision**: Use Salsa's snapshot functionality for LSP operations.

**Justification**: LSP operations need to query the database without modifying it, and potentially in parallel from multiple threads. Snapshots provide a safe, immutable view of the database that can be shared across threads without locking.

### 9. IDE-First Architecture

**Decision**: Design the database structure with IDE use cases in mind from the start.

**Justification**: Retrofitting IDE support into a compiler is difficult. By designing with both batch compilation and interactive IDE scenarios in mind from the beginning, we can ensure good performance for both use cases.

## Crate Design Principles

The restructuring will follow these key principles for crate design:

### 1. Clear Separation of Concerns

Each crate should have a single, well-defined responsibility in the compilation pipeline:

- **Data Structures**: Define the core data structures for the phase
- **Analysis Logic**: Implement the analysis or transformation logic
- **Public API**: Expose a clean, minimal API for other crates to use
- **Salsa Integration**: Keep Salsa-related code separate from core logic

### 2. Standard Crate Structure

All crates should follow this standard directory structure:

```
parallax-{phase}/
├── src/
│   ├── **/*.rs          # Core logic with colocated unit tests
│   ├── salsa/         # Salsa integration
│   │   ├── jar.rs     # Salsa jar definition
│   │   ├── db.rs      # Database trait definition
│   │   ├── input.rs   # Input query definitions
│   │   ├── tracked.rs # Tracked struct definitions
│   │   └── query.rs   # Query function definitions
│   ├── utils.rs       # Utility functions if needed
│   ├── lib.rs         # Public API
│   └── error.rs       # Error types
│   └── tests.rs       # Unit test utilities
├── tests/             # Integration tests
│   ├── **.rs          # Integration tests of the public API
│   ├── utils.rs       # Integration tests utilities
│   └── mod.rs         # Test module definition
└── benches/           # Performance benchmarks
```

### 3. Code Organization

Code within a crate should be organized according to these principles:

1. **Business Logic First**: Core logic should be implemented independent of Salsa
2. **Salsa as Glue**: Salsa should be used to glue components together, not define them
3. **Limited Dependencies**: Each module should have minimal dependencies on other modules
4. **Avoid Circular Dependencies**: Design modules to avoid circular dependencies

### 4. API Design

Public APIs should follow these guidelines:

1. **Minimal Surface Area**: Expose only what other crates need to use
2. **Database Abstraction**: Use traits to abstract database requirements
3. **Error Handling**: Use consistent error types and propagation
4. **Documentation**: All public items must be documented

## Comprehensive Testing Strategy

Testing is a critical component of the Parallax restructuring. This section outlines a comprehensive testing strategy for the Salsa-based architecture.

### 1. Test Organization

#### Unit Tests

Unit tests should be co-located with the code they test using Rust's module system:

```rust
// In src/analysis/resolver.rs
pub fn resolve_name(name: &str, scope: &Scope) -> Resolution {
    // Implementation
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_resolve_simple_name() {
        let scope = Scope::new();
        scope.add_name("x", Type::Int);
        let resolution = resolve_name("x", &scope);
        assert_eq!(resolution, Resolution::Variable { ty: Type::Int });
    }
}
```

Alternatively, for larger test suites, create a tests module within the same directory:

```
src/analysis/
├── resolver.rs         # Implementation
└── tests/              # Unit tests
    ├── mod.rs          # Test module definition
    └── resolver_tests.rs # Tests for resolver
```

With this content in resolver_tests.rs:

```rust
use crate::analysis::resolver::*;

#[test]
fn test_resolve_simple_name() {
    let scope = Scope::new();
    scope.add_name("x", Type::Int);
    let resolution = resolve_name("x", &scope);
    assert_eq!(resolution, Resolution::Variable { ty: Type::Int });
}
```

#### Integration Tests

Integration tests should be placed in the top-level `tests/` directory and focus on testing the public API of the crate:

```rust
// In tests/integration/resolve_tests.rs
use parallax_resolve::{ResolveDb, resolve_module};
use parallax_lang::{SourceFile, parse_file};

#[test]
fn test_resolve_complete_module() {
    let db = TestDatabase::default();
    let source = SourceFile::new(
        &mut db,
        "test.plx".into(),
        "fn foo(x: i32) -> i32 { x + 1 }".into(),
    );
    
    let ast = parse_file(&db, source);
    let resolved = resolve_module(&db, ast);
    
    assert_eq!(resolved.errors(&db).len(), 0);
    assert!(resolved.symbols(&db).contains_key("foo"));
}
```

Integration tests should verify that the components of a crate work together correctly through the public API only.

#### End-to-End Tests

End-to-end tests should be placed in a dedicated crate in the workspace:

```
parallax/
├── crates/
│   ├── parallax-lang/    # Language crate
│   ├── parallax-resolve/ # Resolution crate
│   │   ...
│   └── parallax-tests/   # End-to-end test crate
│       ├── src/
│       │   ├── compiler_tests.rs  # Full compilation pipeline tests
│       │   ├── snapshot_tests.rs  # Tests output against saved snapshots
│       │   ├── regression_tests.rs # Tests for specific bug fixes
│       │   ├── utils/             # Test utilities
│       │   │   ├── mod.rs         # Test utilities module
│       │   │   ├── test_db.rs     # Database implementation for testing
│       │   │   └── fixtures.rs    # Test fixtures loader
│       │   └── lib.rs             # Test suite organization
│       └── tests/                 # Test fixtures and resources
│           ├── fixtures/          # Test case files
│           │   ├── parsing/       # Parser test cases
│           │   ├── typechecking/  # Type checker test cases
│           │   └── codegen/       # Code generation test cases
│           └── snapshots/         # Expected output for snapshot tests
└── Cargo.toml             # Workspace definition
```

End-to-end tests should verify that the entire compiler pipeline works correctly for a variety of input programs and should be run on CI for every commit.

### 2. Testing Salsa Components

Testing Salsa-based code requires special consideration:

#### Query Function Tests

Query functions should be tested in isolation from their Salsa integration:

```rust
// Original query function
#[salsa::tracked]
pub fn typecheck_expr(db: &dyn crate::Db, expr: Expr) -> TypedExpr {
    // Implementation that uses database
    typecheck_expr_impl(db, expr)
}

// Internal implementation function - easier to test
fn typecheck_expr_impl(db: &dyn crate::Db, expr: Expr) -> TypedExpr {
    // Implementation
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_typecheck_simple_expr() {
        let db = TestDatabase::default();
        let expr = Expr::Literal(LiteralExpr::Int(42));
        let typed_expr = typecheck_expr_impl(&db, expr);
        assert_eq!(typed_expr.ty(), Type::Int);
    }
}
```

#### Mock Database

Create a test-specific database implementation:

```rust
// In tests/common/test_db.rs
#[salsa::db(
    parallax_lang::Jar,
    parallax_resolve::Jar,
    parallax_typeck::Jar,
)]
#[derive(Default)]
pub struct TestDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for TestDatabase {
    fn salsa_event(&self, _event: salsa::Event) {}
}

// Optional: implement a parallel database for testing concurrent behavior
impl salsa::ParallelDatabase for TestDatabase {
    fn fork(&self) -> Self {
        Self {
            storage: self.storage.fork(),
        }
    }
}
```

### 3. Testing Strategies

#### Property-Based Testing

Use property-based testing for complex logic:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    
    proptest! {
        #[test]
        fn typecheck_preserves_literals(val: i32) {
            let db = TestDatabase::default();
            let expr = Expr::Literal(LiteralExpr::Int(val));
            let typed_expr = typecheck_expr_impl(&db, expr);
            assert_eq!(typed_expr.ty(), Type::Int);
            if let Expr::Literal(LiteralExpr::Int(result)) = typed_expr.expr() {
                assert_eq!(val, result);
            } else {
                panic!("Expected integer literal");
            }
        }
    }
}
```

#### Snapshot Testing

Use snapshot testing for larger outputs:

```rust
#[test]
fn test_mir_generation() {
    let db = TestDatabase::default();
    let source = create_test_source(&mut db, "example.plx");
    
    // Run pipeline to generate MIR
    let ast = parse_file(&db, source);
    let resolved = resolve_module(&db, ast);
    let typechecked = typecheck_module(&db, resolved);
    let hir = lower_to_hir(&db, typechecked);
    let mir = lower_to_mir(&db, hir);
    
    // Convert MIR to string representation
    let mir_string = format!("{:#?}", mir);
    
    // Compare with stored snapshot
    insta::assert_snapshot!(mir_string);
}
```

#### Fault Injection

Test error handling by deliberately injecting faults:

```rust
#[test]
fn test_recovery_from_parse_errors() {
    let mut db = TestDatabase::default();
    let source = SourceFile::new(
        &mut db,
        "invalid.plx".into(),
        "fn foo() { missing_semicolon }".into(),
    );
    
    // Run pipeline with invalid source
    let ast = parse_file(&db, source);
    assert!(!ast.errors(&db).is_empty());
    
    // Test that we can still resolve as much as possible
    let resolved = resolve_module(&db, ast);
    assert!(resolved.symbols(&db).contains_key("foo"));
}
```

### 4. Benchmark Tests

Create benchmarks to track performance:

```rust
// In benches/resolver_benchmark.rs
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use parallax_resolve::{resolve_module, TestDatabase};
use parallax_lang::{parse_file, SourceFile};

fn bench_resolve(c: &Criterion) {
    let mut db = TestDatabase::default();
    let source = create_large_test_source(&mut db, 1000); // Source with 1000 declarations
    let ast = parse_file(&db, source);
    
    c.bench_function("resolve_large_module", |b| {
        b.iter(|| resolve_module(&black_box(&db), black_box(ast)))
    });
}

criterion_group!(benches, bench_resolve);
criterion_main!(benches);
```

### 5. Test Coverage Requirements

- **Unit Test Coverage**: Minimum 85% for all modules
- **Integration Test Coverage**: Minimum 90% for public APIs
- **End-to-End Test**: Must cover all major compiler features and language constructs
- **Regression Tests**: Every bug fix must include a test case

## Specific Testing Requirements for Each Component

### Parser Testing

- **Input Validation**: Test with valid and invalid inputs
- **Error Recovery**: Verify parser can recover from syntax errors
- **Location Tracking**: Test that source locations are correct
- **AST Structure**: Verify AST matches input structure

### Name Resolution Testing

- **Symbol Binding**: Test successful symbol binding
- **Scope Handling**: Test nested scopes and shadowing
- **Import Resolution**: Test module import resolution
- **Error Detection**: Test unresolved symbols, ambiguous references

### Type Checking Testing

- **Type Inference**: Test type inference for expressions
- **Type Errors**: Test detection of type mismatches
- **Generic Instantiation**: Test generic type instantiation
- **Trait Conformance**: Test trait bounds enforcement

### HIR Testing

- **Lowering**: Test AST to HIR conversion
- **Type Annotations**: Verify all nodes have type annotations
- **Desugaring**: Test expansion of syntax sugar

### MIR Testing

- **Control Flow**: Test control flow graph construction
- **Optimizations**: Test optimization passes
- **Effect Tracking**: Test side effect annotations

### Code Generation Testing

- **LLVM IR**: Test generation of valid LLVM IR
- **Interaction Nets**: Test creation of correct interaction nets
- **Runtime Integration**: Test integration with runtime

## Implementing a Language Server Protocol (LSP)

A key benefit of adopting Salsa for Parallax is the ability to efficiently power an IDE experience through the Language Server Protocol (LSP). This section outlines a plan for implementing LSP support using our Salsa-based architecture.

### New Crate: `parallax-lsp`

**Purpose**: Language server implementation for IDE integration

**Structure**:
```
parallax-lsp/
├── src/
│   ├── handlers/          # LSP method handlers 
│   │   ├── completion.rs  # Code completion
│   │   ├── diagnostics.rs # Error reporting
│   │   ├── hover.rs       # Hover information
│   │   ├── navigation.rs  # Go to definition, references
│   │   └── symbols.rs     # Symbol provider
│   ├── server.rs          # Main LSP server implementation
│   ├── document.rs        # Document management
│   ├── snapshot.rs        # Database snapshot management
│   ├── utils.rs           # Utility functions
│   └── main.rs            # Entry point
```

**Implementation**:

```rust
// server.rs
pub struct ParallaxLanguageServer {
    // Connection to client
    connection: Connection,
    // Main salsa database
    database: Mutex<parallax_db::ParallaxDatabase>,
    // Document state
    documents: Mutex<HashMap<Url, Document>>,
    // Cancellation support
    cancelation_map: Mutex<HashMap<RequestId, Cancellation>>,
}

// document.rs
pub struct Document {
    uri: Url,
    version: i32,
    text: String,
    source_file: Option<parallax_lang::SourceFile>,
    // Track errors for each file
    errors: Vec<Diagnostic>,
}

// snapshot.rs
pub struct Snapshot {
    db: Arc<parallax_db::ParallaxDatabase>,
}
```

### LSP Features Enabled by Salsa

#### 1. Efficient Document Synchronization

When a document changes, we need to update our database:

```rust
// document.rs
impl Document {
    pub fn update(&mut self, db: &mut parallax_db::ParallaxDatabase, changes: Vec<TextDocumentContentChangeEvent>) {
        // Apply changes to text
        apply_changes(&mut self.text, changes);
        self.version += 1;
        
        // Update source file in Salsa database
        if let Some(source_file) = self.source_file {
            source_file.set_text(db).to(self.text.clone());
        } else {
            let path = uri_to_path(&self.uri);
            self.source_file = Some(parallax_lang::SourceFile::new(
                db,
                path,
                self.text.clone(),
            ));
        }
    }
}
```

#### 2. Incremental Analysis for Real-time Feedback

Salsa's incremental computation model ensures only affected parts are reanalyzed:

```rust
// handlers/diagnostics.rs
pub fn publish_diagnostics(
    snapshot: &Snapshot,
    document: &Document,
    client: &Client,
) -> Result<()> {
    let source_file = match document.source_file {
        Some(file) => file,
        None => return Ok(()),
    };
    
    // Parse - will be cached if unchanged
    let ast = parallax_lang::parse_file(&snapshot.db, source_file);
    
    // Resolve - will be cached if AST hasn't changed
    let resolved = parallax_resolve::resolve_module(&snapshot.db, ast);
    
    // Type check - will be cached if resolution hasn't changed
    let typechecked = parallax_typeck::typecheck_module(&snapshot.db, resolved);
    
    // Extract diagnostics from each step and send to client
    let diagnostics = collect_diagnostics(&snapshot.db, source_file, ast, resolved, typechecked);
    
    client.publish_diagnostics(
        document.uri.clone(),
        diagnostics,
        Some(document.version),
    )?;
    
    Ok(())
}
```

#### 3. Code Completion

Utilize existing type information for accurate completions:

```rust
// handlers/completion.rs
pub fn handle_completion(
    snapshot: &Snapshot,
    params: CompletionParams,
) -> Result<Option<CompletionResponse>> {
    let uri = params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;
    
    // Get source file from URI
    let source_file = get_source_file_from_uri(&snapshot.db, &uri)?;
    
    // Convert LSP position to file offset
    let offset = position_to_offset(&source_file.text(&snapshot.db), position)?;
    
    // Find node at position
    let ast = parallax_lang::parse_file(&snapshot.db, source_file);
    let node = find_node_at_offset(&ast, offset)?;
    
    // Get type information for context-aware completions
    let resolved = parallax_resolve::resolve_module(&snapshot.db, ast);
    let typechecked = parallax_typeck::typecheck_module(&snapshot.db, resolved);
    
    // Generate completions based on context and type information
    let items = generate_completion_items(&snapshot.db, node, &typechecked, offset)?;
    
    Ok(Some(CompletionResponse::Array(items)))
}
```

#### 4. Hover Information

Show type information and documentation on hover:

```rust
// handlers/hover.rs
pub fn handle_hover(
    snapshot: &Snapshot,
    params: HoverParams,
) -> Result<Option<Hover>> {
    let uri = params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;
    
    // Get source file from URI
    let source_file = get_source_file_from_uri(&snapshot.db, &uri)?;
    
    // Get AST
    let ast = parallax_lang::parse_file(&snapshot.db, source_file);
    
    // Convert LSP position to file offset
    let offset = position_to_offset(&source_file.text(&snapshot.db), position)?;
    
    // Find node at position
    let node = find_node_at_offset(&ast, offset)?;
    
    // Get type information
    let resolved = parallax_resolve::resolve_module(&snapshot.db, ast);
    let typechecked = parallax_typeck::typecheck_module(&snapshot.db, resolved);
    
    // Create hover content with type information and docs
    let hover_content = create_hover_content(&snapshot.db, node, &typechecked)?;
    
    Ok(Some(Hover {
        contents: hover_content,
        range: None,
    }))
}
```

#### 5. Navigation (Go to Definition)

Jump to definitions using Salsa's symbol tracking:

```rust
// handlers/navigation.rs
pub fn handle_definition(
    snapshot: &Snapshot,
    params: GotoDefinitionParams,
) -> Result<Option<GotoDefinitionResponse>> {
    let uri = params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;
    
    // Get source file from URI
    let source_file = get_source_file_from_uri(&snapshot.db, &uri)?;
    
    // Get AST
    let ast = parallax_lang::parse_file(&snapshot.db, source_file);
    
    // Convert LSP position to file offset
    let offset = position_to_offset(&source_file.text(&snapshot.db), position)?;
    
    // Find identifier at position
    let identifier = find_identifier_at_offset(&ast, offset)?;
    
    // Use name resolution to find definition
    let resolved = parallax_resolve::resolve_module(&snapshot.db, ast);
    let definition = find_definition(&snapshot.db, &resolved, identifier)?;
    
    if let Some(def_location) = definition {
        // Convert definition location to LSP format
        let lsp_location = convert_to_lsp_location(&def_location)?;
        
        Ok(Some(GotoDefinitionResponse::Scalar(lsp_location)))
    } else {
        Ok(None)
    }
}
```

### Threaded Architecture for Responsive UI

The LSP implementation should use a threaded architecture to keep the UI responsive:

```rust
// main.rs
fn main() -> Result<()> {
    // Setup logging
    setup_logging()?;
    
    // Create our main connection to the client
    let (connection, io_threads) = Connection::stdio();
    
    // Create the initial database
    let db = parallax_db::ParallaxDatabase::default();
    
    // Create the language server
    let server = ParallaxLanguageServer::new(connection, db);
    
    // Run the main loop in a separate thread
    let server_thread = std::thread::spawn(move || {
        server.main_loop().unwrap();
    });
    
    // Wait for both threads to complete
    io_threads.join()?;
    server_thread.join().unwrap();
    
    Ok(())
}
```

### Integration with Database Snapshots

To prevent blocking the main thread during long operations, use Salsa's snapshot capabilities:

```rust
// snapshot.rs
impl Snapshot {
    pub fn new(db: &parallax_db::ParallaxDatabase) -> Self {
        Self {
            db: Arc::new(db.snapshot()),
        }
    }
    
    pub fn run_in_snapshot<F, T>(db: &parallax_db::ParallaxDatabase, f: F) -> T 
    where
        F: FnOnce(&Snapshot) -> T,
    {
        let snapshot = Snapshot::new(db);
        f(&snapshot)
    }
}
```

### Utilizing Salsa's Parallel Database for Background Analysis

For expensive operations, use Salsa's parallel capabilities:

```rust
// server.rs
impl ParallaxLanguageServer {
    pub fn analyze_in_background(&self, document: Document) {
        let db_snapshot = {
            let db = self.database.lock().unwrap();
            db.snapshot()
        };
        
        let client = self.client.clone();
        let uri = document.uri.clone();
        
        std::thread::spawn(move || {
            // Run full analysis in background
            let source_file = get_source_file_from_uri(&db_snapshot, &uri).unwrap();
            let ast = parallax_lang::parse_file(&db_snapshot, source_file);
            let resolved = parallax_resolve::resolve_module(&db_snapshot, ast);
            let typechecked = parallax_typeck::typecheck_module(&db_snapshot, resolved);
            
            if let Some(hir) = hir_if_no_errors(&db_snapshot, typechecked) {
                let mir = parallax_mir::lower_to_mir(&db_snapshot, hir);
                let optimized = parallax_mir::optimize_mir(&db_snapshot, mir);
                
                // Report advanced analysis results
                client.send_notification::<AnalysisResults>(AnalysisResultsParams {
                    uri,
                    is_optimizable: calculate_optimization_potential(&optimized),
                    complexity_metrics: calculate_complexity(&optimized),
                }).unwrap();
            }
        });
    }
}
```

### Cached Query Results for Editor Features

Leverage Salsa's caching to implement editor features efficiently:

```rust
// handlers/symbols.rs
pub fn handle_document_symbols(
    snapshot: &Snapshot,
    params: DocumentSymbolParams,
) -> Result<Option<DocumentSymbolResponse>> {
    let uri = params.text_document.uri;
    
    // Get source file from URI
    let source_file = get_source_file_from_uri(&snapshot.db, &uri)?;
    
    // This cached query extracts all symbols from a file
    let symbols = extract_file_symbols(&snapshot.db, source_file);
    
    // Convert to LSP format
    let lsp_symbols = symbols.into_iter()
        .map(convert_to_lsp_symbol)
        .collect::<Vec<_>>();
    
    Ok(Some(DocumentSymbolResponse::Nested(lsp_symbols)))
}

// Salsa tracked function to extract symbols
#[salsa::tracked]
fn extract_file_symbols(db: &dyn parallax_db::Db, file: parallax_lang::SourceFile) -> Vec<Symbol> {
    let ast = parallax_lang::parse_file(db, file);
    collect_symbols_from_ast(ast)
}
```

### Supporting Workspace-Wide Operations

For workspace-wide operations like find references, utilize Salsa's ability to track relationships across files:

```rust
// handlers/navigation.rs
pub fn handle_references(
    snapshot: &Snapshot,
    params: ReferenceParams,
) -> Result<Option<Vec<Location>>> {
    let uri = params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;
    
    // Get source file from URI
    let source_file = get_source_file_from_uri(&snapshot.db, &uri)?;
    
    // Get AST
    let ast = parallax_lang::parse_file(&snapshot.db, source_file);
    
    // Convert LSP position to file offset
    let offset = position_to_offset(&source_file.text(&snapshot.db), position)?;
    
    // Find identifier at position
    let identifier = find_identifier_at_offset(&ast, offset)?;
    
    // Use name resolution to find definition
    let resolved = parallax_resolve::resolve_module(&snapshot.db, ast);
    let symbol = find_symbol_at_position(&snapshot.db, &resolved, identifier)?;
    
    if let Some(symbol) = symbol {
        // Find all references to this symbol across the workspace
        let references = find_all_references(&snapshot.db, symbol)?;
        
        // Convert to LSP locations
        let locations = references.into_iter()
            .map(convert_to_lsp_location)
            .collect::<Result<Vec<_>>>()?;
        
        Ok(Some(locations))
    } else {
        Ok(None)
    }
}

// Find all references to a symbol across the workspace
#[salsa::tracked]
fn find_all_references(db: &dyn parallax_db::Db, symbol: Symbol) -> Vec<SourceLocation> {
    // Get all files in workspace
    let workspace_files = get_workspace_files(db);
    
    // Collect references from all files
    let mut references = Vec::new();
    for file in workspace_files {
        let ast = parallax_lang::parse_file(db, file);
        let resolved = parallax_resolve::resolve_module(db, ast);
        let file_refs = collect_references_to_symbol(&resolved, symbol);
        references.extend(file_refs);
    }
    
    references
}
```

### Implementation Plan

1. **Start With Test Infrastructure**:
   - Implement the test database
   - Create test utilities and helpers
   - Set up CI with test coverage reporting

2. **Core Functionality First**:
   - Implement and test core data structures 
   - Build and test analysis logic independent of Salsa
   - Add and test Salsa integration last

3. **Iterative Development**:
   - Build each phase with its tests before moving to the next
   - Continuously run the full test suite to catch regressions
   - Add benchmarks early to track performance

4. **Documentation**:
   - Document public APIs as they're developed
   - Create examples for key functionality
   - Document test patterns and expectations

## Conclusion

This restructuring plan provides a comprehensive approach to integrating Salsa into the Parallax compiler with a strong focus on testing and maintainability. The resulting architecture will offer:

1. **Efficient Incremental Compilation**: Only recomputing what's necessary when files change
2. **Better Parallel Execution**: Utilizing multiple cores effectively
3. **Clearer Architecture**: With well-defined dependencies between compiler phases
4. **Improved Developer Experience**: Faster feedback loops during development
5. **Foundation for IDE Support**: Enabling rich tooling through incremental computation
6. **High Quality**: Ensured through comprehensive testing at all levels

By following this plan, Parallax will gain significant performance improvements while maintaining its existing architectural strengths.

## Test Abstractions and Utilities

Based on the current test files in `parallax-resolve/tests` and `parallax-typeck/src/tests`, several patterns emerge that could be abstracted into shared testing utilities. This section outlines recommendations for creating reusable test infrastructure.

### Common Test Abstractions

The following abstractions would benefit both unit and integration tests:

#### 1. Test Database Factory

Create a standard factory for test databases:

```rust
// In parallax-testing/src/test_db.rs (or in each crate's test utils)
pub struct TestDatabaseBuilder {
    files: HashMap<PathBuf, String>,
    options: TestOptions,
}

impl TestDatabaseBuilder {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
            options: TestOptions::default(),
        }
    }
    
    pub fn add_file(mut self, path: impl Into<PathBuf>, content: impl Into<String>) -> Self {
        self.files.insert(path.into(), content.into());
        self
    }
    
    pub fn with_options(mut self, options: TestOptions) -> Self {
        self.options = options;
        self
    }
    
    pub fn build(self) -> TestDatabase {
        let mut db = TestDatabase::default();
        
        // Create source files
        let source_files = self.files.into_iter()
            .map(|(path, content)| {
                parallax_lang::SourceFile::new(&mut db, path, content)
            })
            .collect::<Vec<_>>();
            
        // Set any custom options
        db.set_options(self.options);
        
        // Return the database and source files
        db
    }
}
```

#### 2. Source String Test Helpers

Create helpers for concise test inputs:

```rust
pub fn source(s: &str) -> SourceFile {
    let mut db = TestDatabase::default();
    SourceFile::new(&mut db, "test.plx".into(), s.into())
}

pub fn parse(s: &str) -> Ast {
    let mut db = TestDatabase::default();
    let source = SourceFile::new(&mut db, "test.plx".into(), s.into());
    parse_file(&db, source)
}

pub fn resolve(s: &str) -> ResolvedModule {
    let mut db = TestDatabase::default();
    let source = SourceFile::new(&mut db, "test.plx".into(), s.into());
    let ast = parse_file(&db, source);
    resolve_module(&db, ast)
}

pub fn typecheck(s: &str) -> TypeCheckedModule {
    let mut db = TestDatabase::default();
    let source = SourceFile::new(&mut db, "test.plx".into(), s.into());
    let ast = parse_file(&db, source);
    let resolved = resolve_module(&db, ast);
    typecheck_module(&db, resolved)
}
```

#### 3. Error Assertion Helpers

Create helpers for checking expected errors:

```rust
pub fn assert_no_errors(module: &ResolvedModule, db: &dyn Db) {
    let errors = module.errors(db);
    if !errors.is_empty() {
        panic!("Expected no errors, but found: {:?}", errors);
    }
}

pub fn assert_error_contains(module: &ResolvedModule, db: &dyn Db, expected_fragment: &str) {
    let errors = module.errors(db);
    let found = errors.iter().any(|e| e.message.contains(expected_fragment));
    if !found {
        panic!("Expected error containing '{}', but found: {:?}", 
            expected_fragment, errors);
    }
}

pub fn assert_exact_error(module: &ResolvedModule, db: &dyn Db, error_code: ErrorCode) {
    let errors = module.errors(db);
    let found = errors.iter().any(|e| e.code == error_code);
    if !found {
        panic!("Expected error with code {:?}, but found: {:?}", 
            error_code, errors);
    }
}
```

#### 4. Fixture Loading

Create a standard fixture loader:

```rust
pub struct Fixture {
    pub files: HashMap<PathBuf, String>,
}

impl Fixture {
    pub fn load(name: &str) -> Self {
        let fixtures_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("tests/fixtures");
        
        let fixture_dir = fixtures_dir.join(name);
        assert!(fixture_dir.exists(), "Fixture '{}' not found", name);
        
        let files = std::fs::read_dir(&fixture_dir)
            .unwrap()
            .filter_map(Result::ok)
            .filter(|entry| {
                entry.file_type().unwrap().is_file() && 
                entry.path().extension().unwrap_or_default() == "plx"
            })
            .map(|entry| {
                let path = entry.path();
                let content = std::fs::read_to_string(&path).unwrap();
                (path, content)
            })
            .collect();
            
        Self { files }
    }
    
    pub fn create_database(&self) -> (TestDatabase, Vec<SourceFile>) {
        let mut db = TestDatabase::default();
        
        let source_files = self.files.iter()
            .map(|(path, content)| {
                parallax_lang::SourceFile::new(&mut db, path.clone(), content.clone())
            })
            .collect::<Vec<_>>();
            
        (db, source_files)
    }
}
```

#### 5. Type Checking Helpers

Based on the typeck tests:

```rust
pub fn infer_expr_type(expr: &str) -> Type {
    let mut db = TestDatabase::default();
    let ast = parse_expr(&db, expr);
    let ctx = TypeContext::new(&db);
    ctx.infer_expr(&ast).unwrap()
}

pub fn check_function_type(function_decl: &str, expected_type: Type) {
    let mut db = TestDatabase::default();
    let ast = parse_module(&db, function_decl);
    let resolved = resolve_module(&db, ast);
    let typechecked = typecheck_module(&db, resolved);
    
    let function = typechecked.functions(db).next().unwrap();
    assert_eq!(function.ty(db), expected_type);
}
```

### Standardizing Across Crates

To ensure consistent testing across crates, create a dedicated `parallax-testing` crate:

```
parallax-testing/
├── src/
│   ├── db.rs           # Test database implementations
│   ├── fixtures.rs     # Fixture loading and management
│   ├── assertions.rs   # Common assertions
│   ├── builders.rs     # Test case builders
│   ├── mocks.rs        # Mock implementations
│   └── lib.rs          # Public API
└── tests/              # Tests for the testing utilities
    └── fixtures/       # Example fixtures for testing
```

This crate should expose:

1. **TestDatabase**: A standardized test database implementation
2. **TestCase**: A builder for common test cases 
3. **Assertions**: Common assertions for compiler-specific conditions
4. **Fixtures**: Standard fixture loading

### Implementation in Integration Tests

In integration tests, import and use these abstractions:

```rust
// In tests/resolver_tests.rs
use parallax_testing::{
    TestCase, assert_no_errors, assert_error_contains, Fixture
};

#[test]
fn test_basic_resolution() {
    let test = TestCase::new()
        .add_file("main.plx", "fn foo() -> i32 { 42 }")
        .build();
        
    let ast = test.parse("main.plx");
    let resolved = test.resolve(ast);
    
    assert_no_errors(&resolved, &test.db);
    assert!(resolved.symbols(&test.db).contains_key("foo"));
}

#[test]
fn test_fixture_based() {
    let fixture = Fixture::load("basic_module");
    let test = TestCase::from_fixture(fixture);
    
    let ast = test.parse_all();
    let resolved = test.resolve_all();
    
    assert_no_errors(&resolved[0], &test.db);
}
```

### Implementation in Unit Tests

In unit tests, use smaller focused helpers:

```rust
// In src/resolve/tests/resolver_tests.rs
use crate::testing::{mock_scope, mock_symbol, mock_type};

#[test]
fn test_resolve_identifier() {
    let mut scope = mock_scope();
    scope.add_symbol("x", mock_symbol(mock_type("i32")));
    
    let result = resolve_identifier("x", &scope);
    assert!(result.is_some());
    assert_eq!(result.unwrap().name, "x");
}
```

### Test Configuration

Create a standardized way to configure tests:

```rust
// In parallax-testing/src/config.rs
#[derive(Debug, Clone)]
pub struct TestConfig {
    pub trace_resolution: bool,
    pub trace_typechecking: bool,
    pub panic_on_error: bool,
    pub strict_mode: bool,
}

impl Default for TestConfig {
    fn default() -> Self {
        Self {
            trace_resolution: false,
            trace_typechecking: false,
            panic_on_error: false,
            strict_mode: true,
        }
    }
}

impl TestConfig {
    pub fn apply_to_db(&self, db: &mut TestDatabase) {
        db.set_config(self.clone());
    }
}
```

### Test Patterns from Current Code

Based on the current tests, the following patterns appear frequently and should be abstracted:

1. **Module Tests**: Testing complete module resolution and typechecking
2. **Error Tests**: Verifying specific error conditions are detected
3. **Symbol Lookup Tests**: Testing symbol resolution for specific identifiers
4. **Type Inference Tests**: Testing type inference for expressions
5. **Field Access Tests**: Testing field resolution and access
6. **Import Tests**: Testing module imports and visibility

### Example: Abstracting Module Tests

```rust
pub fn test_module<F>(source: &str, assertions: F)
where
    F: FnOnce(&ResolvedModule, &TestDatabase)
{
    let mut db = TestDatabase::default();
    let source_file = SourceFile::new(&mut db, "test.plx".into(), source.into());
    let ast = parse_file(&db, source_file);
    let resolved = resolve_module(&db, ast);
    
    assertions(&resolved, &db);
}

// Usage
#[test]
fn test_function_resolution() {
    test_module(
        "fn foo(x: i32) -> i32 { x + 1 }",
        |resolved, db| {
            assert_no_errors(resolved, db);
            let symbols = resolved.symbols(db);
            assert!(symbols.contains_key("foo"));
            
            let foo = symbols.get("foo").unwrap();
            assert_eq!(foo.kind, SymbolKind::Function);
        }
    );
}
```

### Recommended Abstractions

Based on the examination of current tests, the following abstractions would provide the most value:

1. **TestDatabase**: A unified test database implementation
2. **TestCase**: A high-level test case builder supporting multiple files
3. **Assertions**: Common compiler-specific assertions
4. **Fixtures**: Standard fixture loading
5. **MockScope/MockSymbol**: Test-specific mock implementations
6. **TypeTestHelpers**: Type inference and checking utilities
7. **ErrorTestHelpers**: Error verification utilities

By implementing these abstractions, tests can be more concise, maintainable, and focused on the specific behavior being tested rather than the testing infrastructure. 

## Crate README Template

Each crate in the Parallax compiler should include a comprehensive README.md file that follows this template:

```markdown
# Parallax [Component Name]

## Purpose

Brief 1-2 paragraph description of the crate's responsibility in the compiler pipeline.

### Inputs

- **InputType1**: Description of the first input type
- **InputType2**: Description of the second input type
- ...

### Outputs

- **OutputType1**: Description of what this output represents and how it's used
- **OutputType2**: Description of what this output represents and how it's used
- ...

### Queries Provided

- `query_name(db: &dyn Db, arg: ArgType) -> ReturnType`: Description of what this query does
- ...

## Implementation Details

### Salsa Integration

Description of how this crate integrates with Salsa:

```rust
// Example of the Jar definition
#[salsa::jar(db = Db)]
pub struct Jar(
    // List of inputs, tracked structs, and functions
);

// Example of the Database trait
pub trait Db: salsa::DbWithJar<Jar> + other_crate::Db {}
```

### Key Algorithms

Explanation of the core algorithms implemented in this crate:

1. **Algorithm1**: Explanation of how it works and why this approach was chosen
2. **Algorithm2**: Explanation of how it works and why this approach was chosen
   - Justification for design decisions
   - Trade-offs made

### Data Structures

Overview of the primary data structures:

1. **DataStructure1**:
   ```rust
   struct DataStructure1 {
       field1: Type1,
       field2: Type2,
   }
   ```
   Explanation of this data structure's purpose and design.

2. **DataStructure2**: Similar explanation...

## Crate Structure

```
src/
├── data/            # Core data structures
├── analysis/        # Analysis logic
├── salsa/           # Salsa integration
│   ├── jar.rs       # Jar definition
│   ├── db.rs        # Database trait
│   ├── input.rs     # Input definitions
│   ├── tracked.rs   # Tracked struct definitions
│   └── query.rs     # Query function definitions
├── utils.rs         # Utility functions
├── lib.rs           # Public API
└── error.rs         # Error types
```

### Key Files

- **lib.rs**: Entry point and public API
- **analysis/core.rs**: Main analysis implementation
- **salsa/jar.rs**: Salsa jar definition and integration

## Testing

This crate includes:

- **Unit tests**: Colocated with the implementation code
- **Integration tests**: In the `tests/` directory

Run tests with:

```bash
cargo test -p parallax-[component]
```

## Limitations and Future Work

### Current Limitations

1. **Limitation1**: Description and potential impact
2. **Limitation2**: Description and potential impact

### Planned Improvements

1. **Improvement1**: Description of planned enhancement and benefits
2. **Improvement2**: Description of planned enhancement and benefits

### Research Directions

Areas for further research and exploration:

1. **Research1**: Description of research direction
2. **Research2**: Description of research direction

## Contributing

Guidelines for contributing to this crate:

1. Follow the [Parallax coding standards](../docs/coding-standards.md)
2. Ensure all tests pass with `cargo test`
3. Add tests for new functionality
4. Update documentation for public API changes
```

## README Examples for Key Crates

### Example: `parallax-lang` README.md

```markdown
# Parallax Language Frontend

## Purpose

This crate is responsible for parsing source code into an Abstract Syntax Tree (AST) representation. It serves as the first stage in the Parallax compiler pipeline, converting raw source text into a structured representation that can be analyzed by subsequent compiler stages.

### Inputs

- **SourceFile**: Raw source code with path information
  ```rust
  #[salsa::input]
  pub struct SourceFile {
      pub path: PathBuf,
      #[return_ref]
      pub text: String,
  }
  ```

### Outputs

- **Ast**: Abstract Syntax Tree representing the parsed program
  ```rust
  #[salsa::tracked]
  pub struct Ast {
      #[return_ref]
      pub nodes: Vec<AstNode>,
      #[return_ref]
      pub errors: Vec<ParseError>,
  }
  ```

### Queries Provided

- `parse_file(db: &dyn Db, file: SourceFile) -> Ast`: Parses a source file into an AST
- `parse_expr(db: &dyn Db, text: &str) -> ExprAst`: Parses a string as an expression (useful for testing)

## Implementation Details

### Salsa Integration

```rust
#[salsa::jar(db = Db)]
pub struct Jar(
    crate::input::SourceFile,
    crate::ast::Ast,
    crate::query::parse_file,
    crate::query::parse_expr,
);

pub trait Db: salsa::DbWithJar<Jar> {}
impl<DB: salsa::DbWithJar<Jar>> Db for DB {}
```

### Key Algorithms

1. **Recursive Descent Parsing**: We implement a hand-written recursive descent parser with backtracking for specific grammar constructs. This approach was chosen over parser generators for better error recovery and more precise error messages.

2. **Pratt Parsing for Expressions**: We use Pratt parsing (precedence climbing) for expression parsing, which handles operator precedence elegantly while maintaining a simple implementation.

### Data Structures

1. **AstNode**: Base structure for all AST nodes
   ```rust
   pub enum AstNode {
       Expr(ExprNode),
       Stmt(StmtNode),
       Item(ItemNode),
   }
   ```

2. **SourceLocation**: Tracks the original source location for error reporting
   ```rust
   pub struct SourceLocation {
       pub file_id: SourceFile,
       pub span: TextSpan,
   }
   ```

## Crate Structure

```
src/
├── ast/            # AST node definitions
├── parser/         # Parser implementation
│   ├── expr.rs     # Expression parsing
│   ├── stmt.rs     # Statement parsing
│   └── item.rs     # Module-level item parsing
├── salsa/          # Salsa integration
├── lexer/          # Tokenization
├── error.rs        # Error types and reporting
├── location.rs     # Source location tracking
└── lib.rs          # Public API
```

## Limitations and Future Work

### Current Limitations

1. **Error Recovery**: The current error recovery strategy is basic and sometimes discards too much of the input on error.
2. **Performance**: Large files can be slow to parse due to lack of optimization in the lexer.

### Planned Improvements

1. **Incremental Parsing**: Implement incremental parsing for better IDE performance when only small edits are made.
2. **Better Error Recovery**: Implement smarter error recovery that preserves more of the input structure.

### Research Directions

1. **Parser Combinators**: Investigate using parser combinators like Chumsky while maintaining good error reporting.
2. **Parallel Parsing**: Research opportunities for parallel parsing of independent declarations.
```

### Example: `parallax-resolve` README.md

```markdown
# Parallax Name Resolution

## Purpose

This crate implements name resolution for the Parallax language. It's responsible for connecting identifiers to their definitions, managing scopes, and resolving imports. Name resolution is the second stage of the compiler pipeline, operating on the AST produced by the `parallax-lang` crate.

### Inputs

- **Ast**: Abstract Syntax Tree from the parsing phase
  ```rust
  // From parallax-lang
  pub struct Ast { ... }
  ```

### Outputs

- **ResolvedModule**: A module with all names resolved to their definitions
  ```rust
  #[salsa::tracked]
  pub struct ResolvedModule {
      #[return_ref]
      pub symbols: HashMap<Identifier, Symbol>,
      #[return_ref]
      pub errors: Vec<ResolveError>,
  }
  ```

### Queries Provided

- `resolve_module(db: &dyn Db, ast: parallax_lang::Ast) -> ResolvedModule`: Resolves all names in a module
- `lookup_symbol(db: &dyn Db, module: ResolvedModule, name: &str) -> Option<Symbol>`: Looks up a symbol by name

## Implementation Details

### Salsa Integration

```rust
#[salsa::jar(db = Db)]
pub struct Jar(
    crate::tracked::ResolvedModule,
    crate::query::resolve_module,
    crate::query::lookup_symbol,
);

pub trait Db: salsa::DbWithJar<Jar> + parallax_lang::Db {}
impl<DB: salsa::DbWithJar<Jar> + parallax_lang::Db> Db for DB {}
```

### Key Algorithms

1. **Two-Pass Resolution**: We use a two-pass algorithm that first collects all declarations and then resolves references. This handles forward references and mutual recursion naturally.

2. **Namespace Management**: We maintain separate namespaces for values, types, and traits to support overloaded names across different categories.

### Data Structures

1. **Symbol**: Represents a resolved identifier
   ```rust
   pub struct Symbol {
       pub id: SymbolId,
       pub kind: SymbolKind,
       pub location: SourceLocation,
   }
   ```

2. **Scope**: Represents a lexical scope with symbols
   ```rust
   pub struct Scope {
       pub symbols: HashMap<String, Symbol>,
       pub parent: Option<Arc<Scope>>,
   }
   ```

## Crate Structure

```
src/
├── scope/          # Scope management
├── symbol.rs       # Symbol table implementation
├── resolver/       # Name resolution implementation
│   ├── expr.rs     # Expression resolution
│   ├── stmt.rs     # Statement resolution
│   └── item.rs     # Item resolution
├── salsa/          # Salsa integration
├── error.rs        # Error types
└── lib.rs          # Public API
```

## Limitations and Future Work

### Current Limitations

1. **Circular References**: Circular references between modules can cause resolution failures.
2. **Path Resolution**: Complex path resolution with nested modules could be improved.

### Planned Improvements

1. **Lazy Module Loading**: Implement lazy loading of imported modules to improve performance.
2. **Better Error Recovery**: Provide more contextual information for resolution errors.

### Research Directions

1. **Incremental Resolution**: Research more efficient incremental resolution strategies.
2. **Parallel Resolution**: Investigate opportunities for parallel resolution of independent modules.
```

These README templates provide a comprehensive overview of each crate's purpose, implementation details, and future directions, while also serving as documentation for developers working on the project. The structure ensures that all critical aspects of each crate are documented consistently across the Parallax compiler. 