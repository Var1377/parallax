# Architecture Overview

The Parallax compiler and runtime system are designed as a modular pipeline where each phase has clear responsibilities and interfaces.

## High-Level Architecture

```
Source Code
    ↓
┌─────────────┐
│   Lexer     │ ← Logos-based tokenization
├─────────────┤
│   Parser    │ ← Rowan-based error-recovering parser
├─────────────┤
│Type Checker │ ← Bidirectional inference with refinement
├─────────────┤
│     HIR     │ ← High-level IR with full type information
├─────────────┤
│     MIR     │ ← Mid-level IR for optimization
├─────────────┤
│   Codegen   │ ← LLVM-based native code generation
├─────────────┤
│   Runtime   │ ← Work-stealing scheduler
└─────────────┘
    ↓
Native Binary
```

## Key Components

### 1. Frontend (Lexer + Parser)

The frontend converts source text into a typed abstract syntax tree:

- **Lexer**: Fast, zero-copy tokenization using logos
- **Parser**: Error-recovering parser producing rowan syntax trees
- **AST**: Immutable, persistent syntax trees with full fidelity

### 2. Type System

The type checker implements:

- **Bidirectional type checking**: Efficient inference algorithm
- **Trait resolution**: Rust-style trait system with enhancements
- **Type refinement**: Pattern matching refines type information
- **Const evaluation**: Compile-time function execution

### 3. IR Pipeline

Two intermediate representations optimize different aspects:

- **HIR (High-level IR)**: Close to source with full type information
- **MIR (Mid-level IR)**: SSA-based representation for optimization

### 4. Code Generation

LLVM-based backend that generates:

- **Function agents**: Functions that return work items
- **Parallel annotations**: Metadata for runtime scheduling
- **Native code**: Optimized machine code

### 5. Runtime System

Work-stealing runtime that provides:

- **Scheduler**: Distributes work across CPU cores
- **Memory management**: Arena allocators and reference counting
- **FFI**: C-compatible interface for interop

## Design Decisions

### Incremental Compilation

Using Salsa for query-based compilation:

```rust
#[salsa::query_group(ParserStorage)]
trait Parser {
    #[salsa::input]
    fn source_text(&self, file: FileId) -> Arc<String>;
    
    fn parse(&self, file: FileId) -> Arc<SyntaxTree>;
    
    fn errors(&self, file: FileId) -> Arc<Vec<ParseError>>;
}
```

### Error Recovery

The parser continues after errors to provide better diagnostics:

```rust
fn parse_expression(&mut self) -> Expression {
    let expr = match self.current() {
        Token::Number(n) => self.parse_number(),
        Token::String(s) => self.parse_string(),
        _ => {
            self.error("expected expression");
            self.recover();
            Expression::Error
        }
    };
    expr
}
```

### Type Inference

Bidirectional type checking for efficient inference:

```rust
enum Mode {
    Infer,
    Check(Type),
}

fn infer_expr(&mut self, expr: &Expr, mode: Mode) -> Type {
    match (expr, mode) {
        (Expr::Lambda(param, body), Check(Type::Function(param_ty, ret_ty))) => {
            // Check mode: we know the expected type
            self.check_pattern(param, param_ty);
            self.check_expr(body, ret_ty);
            Type::Function(param_ty, ret_ty)
        }
        (Expr::Lambda(param, body), Infer) => {
            // Infer mode: synthesize the type
            let param_ty = self.fresh_type_var();
            self.infer_pattern(param, param_ty);
            let ret_ty = self.infer_expr(body, Infer);
            Type::Function(param_ty, ret_ty)
        }
        // ... other cases
    }
}
```

## Module Structure

The implementation is organized into crates:

```
parallax/
├── parallax-lexer/      # Tokenization
├── parallax-parser/     # Parsing and AST
├── parallax-types/      # Type system core
├── parallax-typeck/     # Type checking
├── parallax-hir/        # High-level IR
├── parallax-mir/        # Mid-level IR  
├── parallax-codegen/    # LLVM code generation
├── parallax-runtime/    # Runtime system
└── parallax/            # Main compiler driver
```

## Performance Considerations

### Memory Layout

Optimize data structures for cache efficiency:

```rust
// Good: struct of arrays for cache efficiency
struct Functions {
    names: Vec<String>,
    params: Vec<Vec<Param>>,
    bodies: Vec<Body>,
    types: Vec<Type>,
}

// Bad: array of structs causes cache misses
struct Function {
    name: String,
    params: Vec<Param>,
    body: Body,
    ty: Type,
}
```

### Parallelism

The compiler itself uses parallelism where beneficial:

- Parallel parsing of independent modules
- Parallel type checking of independent functions  
- Parallel LLVM optimization and codegen

### Incremental Compilation

Salsa ensures we only recompute what changes:

```rust
// First compilation
db.set_source_text(file1, "const x = 42");
let ast1 = db.parse(file1); // Computed

// Edit different file  
db.set_source_text(file2, "const y = 99");
let ast1_again = db.parse(file1); // Cached!
let ast2 = db.parse(file2); // Computed
```

## Next Steps

The following chapters dive deep into each component, starting with the lexer and parser implementation.