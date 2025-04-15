# Parallax Compiler Restructuring with Salsa

## Overview

This document outlines a comprehensive plan for restructuring the Parallax compiler using the Salsa 0.19.0 framework to achieve efficient incremental compilation. The restructuring aims to leverage Salsa's capabilities for dependency tracking, memoization, and parallel evaluation while maintaining the existing architectural components of Parallax.

## Current Architecture

Currently, the Parallax compiler consists of these core crates:

- `parallax-syntax`: Parser and AST (renamed from parallax-lang)
- `parallax-resolve`: Name resolution (to be split into more focused crates)
- `parallax-typeck`: Type checking
- `parallax-hir`: High-level IR
- `parallax-mir`: Mid-level IR
- `parallax-codegen`: Code generation
- `parallax-net`: Runtime
- `parallax-hvm`: HVM integration
- `parallax-cli`: CLI

## Understanding Salsa 0.19.0

### Salsa 0.19.0 Implementation Guide

#### Core Concepts

Salsa is an incremental computation framework that allows us to automatically recompute only the parts of our compiler that are affected by code changes. It provides:

1. **Database Storage**: All tracked data is stored in a central database
2. **Automatic Dependency Tracking**: Dependencies between queries are automatically tracked
3. **Incremental Recomputation**: Only recompute what's necessary when inputs change
4. **Parallel Execution**: Execute independent queries concurrently
5. **Cycle Detection**: Handle recursive dependencies properly

#### Salsa Annotations

In Salsa 0.19.0, several key annotations are used to define the incremental computation structure:

##### 1. Database Traits (`#[salsa::db]`)

Database traits define a group of query functions:

```rust
#[salsa::db]
pub trait SourceDatabase: salsa::Database {
    /// Resolves a frame by its configuration path
    fn resolve_frame<'db>(&'db self, config_path: &str) -> Frame<'db> 
    where
        Self: Sized
    {
        let path = Path::new(self, std::path::PathBuf::from(config_path));
        frame::resolve_frame(self, path)
    }
}
```

Key characteristics:
- `#[salsa::db]` marks this as a database trait
- Each method represents a query function
- Default implementations call standalone functions
- The `self` parameter is a reference to the database
- The `where Self: Sized` bound is often needed for lifetime correctness

##### 2. Input Structs (`#[salsa::input]`)

Input structs represent base values that can change from the outside world:

```rust
#[salsa::input]
pub struct SourceFile {
    pub path: String,
    #[return_ref]
    pub contents: String,
}
```

Key characteristics:
- Created with `SourceFile::new(db, "path", "contents")`
- Read with getters: `file.path(db)`, `file.contents(db)`
- Modified with setters: `file.set_contents(db).to("new content")`
- The `#[return_ref]` attribute returns a reference to avoid cloning

##### 3. Tracked Structs (`#[salsa::tracked]`)

Tracked structs represent derived values in the computation:

```rust
#[salsa::tracked]
pub struct Frame<'db> {
    #[tracked]
    pub config: FrameConfig,
    #[tracked]
    pub config_source: Option<SourceFile<'db>>,
    #[tracked]
    pub root: Dir<'db>,
    #[tracked]
    pub dependencies: FxHashMap<String, Frame<'db>>,
}
```

Key characteristics:
- Created with `Frame::new(db, config, config_source, root, dependencies)`
- Accessed with getters: `frame.config(db)`, `frame.root(db)`
- Cannot be modified after creation
- Fields marked with `#[tracked]` create dependencies
- The `'db` lifetime ties the struct to the database

##### 4. Tracked Functions (`#[salsa::tracked]`)

Tracked functions compute values that are memoized:

```rust
#[salsa::tracked]
pub fn resolve_frame<'db>(db: &'db dyn SourceDatabase, input_path: Path<'db>) -> Frame<'db> {
    // Implementation that reads from the database
    // Result is cached based on the inputs
}
```

Key characteristics:
- First parameter is always the database reference
- Results are automatically cached
- Recomputed only when dependencies change
- The `'db` lifetime ensures the database remains valid

##### 5. Interned Values (`#[salsa::interned]`)

Interned values deduplicate identical data:

```rust
#[salsa::interned]
pub struct Symbol {
    #[return_ref]
    name: String,
    kind: SymbolKind,
}
```

Key characteristics:
- Creating the same symbol twice returns identical IDs
- Equality comparison is an efficient integer comparison
- Used for identifiers, symbols, and frequently compared values

#### Attributes for Fields

##### `#[return_ref]`

For large fields, use `#[return_ref]` to avoid cloning:

```rust
#[salsa::tracked]
pub struct ParsedFile<'db> {
    /// Original source file
    pub source_file: SourceFile<'db>,
    
    /// Root AST node
    #[tracked]
    #[return_ref]
    pub ast: Vec<Item>,
}
```

The getter `parsed_file.ast(db)` returns `&Vec<Item>` instead of `Vec<Item>`.

##### `#[tracked]` for Struct Fields

Mark fields that should be tracked for dependencies:

```rust
#[salsa::tracked]
pub struct Module<'db> {
    /// Module name
    pub name: String,
    
    /// Source file (if this is a leaf module)
    pub source: Option<SourceFile<'db>>,
    
    /// Parsed content (if this is a leaf module)
    pub ast: Option<ParsedFile<'db>>,
    
    /// Child modules
    #[tracked]
    pub children: Vec<Module<'db>>,
}
```

Only fields marked with `#[tracked]` create dependencies.

#### Database Implementation

The central database implements all query groups:

```rust
/// The central database for the Parallax compiler that integrates all query groups.
#[salsa::db]
#[derive(Default, Clone)]
pub struct Compiler {
    storage: salsa::Storage<Self>,
    root_config: PathBuf,
}

// Storage implementation
#[salsa::db]
impl salsa::Database for Compiler {
    fn salsa_event(&self, event: &dyn Fn() -> salsa::Event) {
        let event = event();
        eprintln!("Event: {event:?}");
    }
}

// Using a macro to implement multiple traits
macro_rules! impl_source_database {
    ($($db:ident),*) => {
        $(
#[salsa::db]
            impl $db for Compiler {}
        )*
    };
}

impl_source_database!(SourceDatabase, SyntaxDatabase);
```

#### Choosing the Right Granularity

When designing tracked functions, carefully consider:

- **Coarse-grained tracking** (file level) for fast operations like parsing
- **Fine-grained tracking** (function level) for expensive operations like type checking
- The trade-off between tracking overhead and recomputation costs

#### Organizing Your Crate

A typical Salsa-integrated crate structure:

```
src/
├── lib.rs        # Exports public API and database trait
├── frame.rs      # Main implementation of a compiler phase
├── error.rs      # Error types
├── path.rs       # Supporting modules
├── config.rs     # Supporting modules
└── diagnostic.rs # Error reporting utilities
```

The database trait is defined in `lib.rs`, while implementation functions are in their respective modules.

#### Writing Tests

For testing Salsa code, create a test database:

```rust
#[cfg(test)]
pub mod testing {
    use super::*;
    
    /// A simple implementation of the source database for testing
#[salsa::db]
    #[derive(Default, Clone)]
    pub struct TestDatabase {
        storage: salsa::Storage<Self>,
    }
    
    #[salsa::db]
    impl salsa::Database for TestDatabase {
        fn salsa_event(&self, event: &dyn Fn() -> salsa::Event) {
            event();
        }
    }
    
    #[salsa::db]
    impl SourceDatabase for TestDatabase {}
}
```

#### Best Practices

1. **Clearly Define Dependencies**: Use function parameters to make dependencies explicit
2. **Keep Database Traits Focused**: Each database trait should have a single responsibility
3. **Use Helper Functions**: Implement logic in standalone functions referenced by queries
4. **Consistent Naming**: Use verb prefixes for queries (resolve_*, parse_*, etc.)
5. **Error Handling**: Use accumulators or results for error reporting

#### Common Patterns

##### Factory Functions

Create factory functions for input structs:

```rust
// In lib.rs or file.rs
pub fn create_source_file(db: &dyn SourceDatabase, path: String, contents: String) -> SourceFile {
    SourceFile::new(db, path, contents)
}
```

##### Function-Based Queries

Implement complex query logic in standalone functions:

```rust
// In frame.rs
#[salsa::tracked]
pub fn resolve_frame<'db>(db: &'db dyn SourceDatabase, input_path: Path<'db>) -> Frame<'db> {
    // Implementation
}

// In lib.rs
#[salsa::db]
pub trait SourceDatabase: salsa::Database {
    fn resolve_frame<'db>(&'db self, config_path: &str) -> Frame<'db> 
    where
        Self: Sized
    {
        let path = Path::new(self, std::path::PathBuf::from(config_path));
        frame::resolve_frame(self, path)
    }
}
```

##### Error Accumulation

Use accumulators or result types for error reporting:

```rust
// Using a helper function to report errors
pub fn report_error(db: &dyn SourceDatabase, location: String, error: FrameError) {
    // Report the error through an accumulator
    parallax_diagnostic::Diagnostics::push(db, error);
}
```

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

## Error Reporting with miette and thiserror

Error reporting is integrated directly into the parallax-source crate, using a combination of the `miette` and `thiserror` crates for comprehensive, user-friendly error reporting.

### Core Error Handling Components in parallax-source

The following components are provided in the parallax-source crate:

1. **ParallaxDiagnostic Accumulator**: A Salsa accumulator for collecting diagnostics
   ```rust
   #[salsa::accumulator]
   pub struct ParallaxDiagnostic {
       /// The source file where the diagnostic occurred
       pub location: String,
       
       /// The error report containing details about the diagnostic
       pub report: Arc<dyn ParallaxError>,
   }
   ```

2. **ParallaxError Trait**: A trait for errors that can be reported with source context
   ```rust
   pub trait ParallaxError: Diagnostic + Send + Sync + RefUnwindSafe + Display + Error + 'static {
       /// Create a Report with source context for this error
       fn report(self, db: &dyn SourceDatabase, file: SourceFile) -> Report;
   }
   ```

3. **Report Struct**: For rendering diagnostics with source code context
   ```rust
   pub struct Report {
       /// The source code text for context in error displays
       pub source_code: String,
       
       /// The error report
       pub report: Arc<dyn ParallaxError>,
   }
   ```

4. **Helper Functions**: Utilities to report errors via the accumulator
   ```rust
   pub fn report_error<R: ParallaxError>(
       db: &dyn SourceDatabase, 
       location: String, 
       report: R
   ) {
       ParallaxDiagnostic {
           location,
           report: Arc::new(report),
       }.accumulate(db);
   }
   ```

### Workspace-Level Dependencies

**IMPORTANT**: To ensure consistent error reporting, all crates must use the workspace-level dependencies:

```toml
# In workspace Cargo.toml
[workspace.dependencies]
miette = { version = "7.0.0", features = ["fancy"] }
thiserror = "1.0.50"
salsa = "0.19.0"

# In crate Cargo.toml
[dependencies]
miette = { workspace = true }
thiserror = { workspace = true }
salsa = { workspace = true }
```

This ensures consistent behavior and prevents version conflicts across the codebase.

### Error Type Design

Each crate should define its error types using `thiserror` and implement `miette::Diagnostic`:

```rust
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Debug, Error, Diagnostic)]
pub enum SyntaxError {
    #[error("Parser initialization error: {0}")]
    #[diagnostic(code(parallax_syntax::parser_init))]
    ParserInitError(String),
    
    #[error("Parse error: {message}")]
    #[diagnostic(code(parallax_syntax::parse_error))]
    ParseError {
        message: String,
        #[label("error occurred here")]
        span: Option<SourceSpan>,
    },
    
    // Other error variants...
}
```

### Source Span Tracking

**CRUCIAL**: Source spans must be preserved throughout the compilation pipeline using miette's types:

1. **Parsing Phase**: Record precise spans for each AST node
   ```rust
   #[derive(Debug)]
   pub struct ExprNode {
       pub kind: ExprKind,
       pub span: SourceSpan, // Use miette's SourceSpan directly
       pub id: NodeId,
   }
   ```

2. **Preserve spans during transformations**
3. **Error Creation with Spans**

### Implementing SourceCode

To work with miette, source files should implement the `SourceCode` trait:

```rust
impl miette::SourceCode for SourceFile {
    fn read_span<'a>(
        &'a self, 
        span: &SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize
    ) -> Result<Box<dyn miette::SpanContents<'a> + 'a>, miette::Error> {
        // Implement to extract the span contents from text
        miette::SourceCode::read_span(
            &self.contents(db), 
            span, 
            context_lines_before, 
            context_lines_after
        )
    }
}
```

### Error Accumulators in Salsa

Use helper functions to report errors:

```rust
// Report an error
pub fn report_error(db: &dyn SourceDatabase, location: String, error: FrameError) {
    // Report via diagnostic system
    Diagnostics::push(db, ParallaxDiagnostic::new(location, error));
}
```

## Implementation Examples from Parallax

### Source Management

```rust
// Input for source files
#[salsa::tracked]
pub struct SourceFile<'db> {
    pub location: String,
      #[return_ref]
    pub contents: String,
}

// Factory function
impl<'db> SourceFile<'db> {
    pub fn new(db: &'db dyn SourceDatabase, location: String, contents: String) -> Self {
        SourceFile::new(db, location, contents)
    }
}
```

### Syntax Representation

  ```rust
// Tracked struct for parsed files
  #[salsa::tracked]
pub struct ParsedFile<'db> {
    /// Original source file
    pub source_file: SourceFile<'db>,
    
    /// Root AST node
    #[tracked]
    pub ast: Vec<Item>,
    
    /// Parsing errors
    #[tracked]
    pub errors: Vec<SyntaxError>,
}

// Database trait with query function
#[salsa::db]
pub trait SyntaxDatabase: SourceDatabase {
    fn parse_file<'db>(&'db self, file: SourceFile<'db>) -> ParsedFile<'db> 
    where Self: Sized {
        parse_file_query(self, file)
    }
}

// Implementation of the tracked function
#[salsa::tracked]
pub fn parse_file_query<'db>(db: &'db dyn SyntaxDatabase, file: SourceFile<'db>) -> ParsedFile<'db> {
    let mut parser = ParallaxParser::new().unwrap();
    let content = file.contents(db);
    
    // Parse source into AST
    match parser.parse_ast(&content) {
        Ok(ast) => {
            // Store parse result in tracked struct with no errors
            ParsedFile::new(db, file, ast, Vec::new())
        },
        Err(error) => {
            // Return empty AST with the error
            ParsedFile::new(db, file, Vec::new(), vec![error])
        }
    }
}
```

### Tracking at Different Granularities

For parsing, we track at the file level:

```rust
// File-level granularity for parsing
#[salsa::tracked]
pub fn parse_file_query<'db>(db: &'db dyn SyntaxDatabase, file: SourceFile<'db>) -> ParsedFile<'db> {
    // Parse entire file at once
}
```

For more expensive operations like type checking, we would track at a finer granularity:

```rust
// Function-level granularity for type checking
#[salsa::tracked]
fn typecheck_function<'db>(db: &'db dyn TypecheckDatabase, function: &Function) -> TypedFunction<'db> {
    // Type check individual function
}
```

### Return References for Efficiency

For collections and large data structures, use `#[return_ref]`:

```rust
#[salsa::tracked]
pub struct Dir<'db> {
    #[tracked]
    pub files: Vec<SourceFile<'db>>,

    #[tracked]
    #[return_ref]
    pub dirs: Vec<Dir<'db>>,
}
```

### Error Handling

```rust
// Using a helper function to report errors
pub fn report_error(db: &dyn SourceDatabase, location: String, error: FrameError) {
    // Report the error through the diagnostic system
    Diagnostics::push(db, ParallaxDiagnostic::new(location, error));
}

// Usage in a tracked function
#[salsa::tracked]
pub fn resolve_frame<'db>(db: &'db dyn SourceDatabase, input_path: Path<'db>) -> Frame<'db> {
    // If error occurs
    if path_not_found {
        let error = FrameError::ConfigNotFound(path);
        report_error(db, location, error);
        // Continue with default values
    }
    
    // Create frame even in error case
    Frame::new(db, config, config_source, root, dependencies)
}
```

By following these patterns consistently across Parallax, we create a robust incremental compiler that efficiently handles changes to source code.