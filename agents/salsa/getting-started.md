# Getting Started with Salsa 0.23

This guide will walk you through creating your first Salsa program, based on analysis of the actual source code and examples.

## Basic Setup

### 1. Create a Database

Every Salsa program starts with a database. The database stores all your data and coordinates query execution.

```rust
use salsa::{Database, Storage};

#[salsa::db]
#[derive(Default)]
pub struct MyDatabase {
    storage: Storage<Self>,
}

#[salsa::db]
impl Database for MyDatabase {}
```

The `#[salsa::db]` macro generates the necessary boilerplate for your database implementation.

### 2. Define Input Data

Input data represents the base facts that can change over time:

```rust
#[salsa::input]
pub struct SourceFile {
    #[returns(ref)]
    pub text: String,
    pub path: std::path::PathBuf,
}
```

Key points about inputs:
- Use `#[returns(ref)]` for fields that return references (like `String`, `Vec`)
- Input structs automatically get `new()` and setter methods
- Each input gets a unique ID for efficient tracking

### 3. Define Tracked Functions

Tracked functions are the core of Salsa - they're memoized and incrementally recomputed:

```rust
#[salsa::tracked]
fn parse_file(db: &dyn Database, file: SourceFile) -> Vec<Token> {
    // This function will only re-run if `file` changes
    let text = file.text(db);
    tokenize(text)
}

#[salsa::tracked]
fn analyze_tokens(db: &dyn Database, file: SourceFile) -> Analysis {
    let tokens = parse_file(db, file);
    // This will reuse the memoized result from parse_file
    // unless file.text changed
    perform_analysis(tokens)
}
```

### 4. Using the System

```rust
fn main() {
    let mut db = MyDatabase::default();
    
    // Create input data
    let file = SourceFile::new(&db, "let x = 42;".to_string(), "main.rs".into());
    
    // Run queries - these will execute and be memoized
    let tokens = parse_file(&db, file);
    let analysis = analyze_tokens(&db, file);
    
    // Modify input data
    file.set_text(&mut db).to("let y = 24;".to_string());
    
    // Re-run queries - only what changed will be recomputed
    let new_tokens = parse_file(&db, file);  // Will re-execute
    let new_analysis = analyze_tokens(&db, file);  // Will re-execute
}
```

## Complete Example: Calculator

Here's a complete example based on the calc example in the Salsa repository:

```rust
use salsa::{Database, Storage};

// Database definition
#[salsa::db]
#[derive(Default)]
pub struct CalcDatabase {
    storage: Storage<Self>,
}

#[salsa::db]
impl Database for CalcDatabase {}

// Input: source code
#[salsa::input]
pub struct SourceProgram {
    #[returns(ref)]
    pub text: String,
}

// Tracked struct: parsed expression
#[salsa::tracked]
pub struct Expression<'db> {
    #[tracked]
    pub value: i32,
    #[tracked]
    pub operation: Operation,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operation {
    Add(i32, i32),
    Multiply(i32, i32),
    Literal(i32),
}

// Tracked functions
#[salsa::tracked]
fn parse(db: &dyn Database, program: SourceProgram) -> Expression<'_> {
    let text = program.text(db);
    // Simple parser for demonstration
    if let Some(pos) = text.find('+') {
        let left: i32 = text[..pos].trim().parse().unwrap_or(0);
        let right: i32 = text[pos+1..].trim().parse().unwrap_or(0);
        Expression::new(db, left + right, Operation::Add(left, right))
    } else {
        let value: i32 = text.trim().parse().unwrap_or(0);
        Expression::new(db, value, Operation::Literal(value))
    }
}

#[salsa::tracked]
fn evaluate(db: &dyn Database, program: SourceProgram) -> i32 {
    let expr = parse(db, program);
    expr.value(db)
}

fn main() {
    let mut db = CalcDatabase::default();
    
    // Create and evaluate expression
    let program = SourceProgram::new(&db, "10 + 5".to_string());
    println!("Result: {}", evaluate(&db, program)); // Output: 15
    
    // Change the input
    program.set_text(&mut db).to("20 + 3".to_string());
    println!("Result: {}", evaluate(&db, program)); // Output: 23
    // Only parse() and evaluate() re-run, any other dependent queries reuse memoized results
}
```

## Key Concepts Demonstrated

### Automatic Dependency Tracking
When `evaluate` calls `parse`, Salsa automatically tracks this dependency. If the input to `parse` changes, `evaluate` will be re-executed.

### Memoization
Results are automatically cached. If you call `evaluate` again with the same input, it returns the cached result without re-execution.

### Incremental Updates
When you change an input (using setters like `set_text`), only the queries that depend on that specific change are re-executed.

### Database Lifetime Management
The `'db` lifetime in tracked structs ties their lifetime to the database, ensuring memory safety.

## Next Steps

- Learn about [Core Concepts](core-concepts.md) for deeper understanding
- Explore [Query System](query-system.md) for different types of queries
- See [Advanced Patterns](advanced-patterns.md) for complex scenarios

## Common Patterns

### Error Handling
```rust
#[salsa::tracked]
fn parse_with_errors(db: &dyn Database, file: SourceFile) -> Result<Ast, ParseError> {
    // Salsa works with any return type, including Results
    let text = file.text(db);
    parse_text(text)
}
```

### Multiple Dependencies
```rust
#[salsa::tracked]
fn compile(db: &dyn Database, main_file: SourceFile, config: Config) -> Binary {
    // This function depends on both main_file and config
    let ast = parse_file(db, main_file);
    let settings = config.settings(db);
    generate_binary(ast, settings)
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
    // Accumulate diagnostics during checking
    if some_error_condition {
        Diagnostic { 
            message: "Error found".to_string(), 
            span: error_span 
        }.accumulate(db);
    }
}

// Later, collect all diagnostics
let diagnostics = check_file::accumulated::<Diagnostic>(&db, file);
```