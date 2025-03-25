# Parallax Crates Overview

This directory contains the core crates that make up the Parallax compiler and runtime. Below is an overview of each crate and its role in the compilation pipeline.

## Compilation Pipeline

```
Source Code
     ↓
[parallax-lang] → AST
     ↓
[parallax-resolve] → Resolved AST
     ↓
[parallax-typeck] → Type-checked AST
     ↓
[parallax-hir] → High-level IR
     ↓
[parallax-mir] → Optimized IR
     ↓
[parallax-codegen] → Interaction Net + LLVM IR
     ↓
[parallax-net] → Runtime Execution
```

## Core Crates

### `parallax-lang`
The frontend of the compiler responsible for parsing source code into an AST.

```
parallax-lang/
├── src/
│   ├── ast/           # AST node definitions
│   │   ├── expr.rs    # Expression nodes
│   │   ├── stmt.rs    # Statement nodes
│   │   ├── pattern.rs # Pattern matching nodes
│   │   ├── types.rs   # Type system nodes
│   │   └── common.rs  # Shared AST components
│   ├── parser/        # Parsing implementation
│   │   ├── expr.rs    # Expression parsing
│   │   ├── stmt.rs    # Statement parsing
│   │   ├── pattern.rs # Pattern parsing
│   │   └── items.rs   # Module-level item parsing
│   ├── error.rs       # Error types and handling
│   ├── location.rs    # Source location tracking
│   └── visitor.rs     # AST visitor traits
```

### `parallax-resolve`
Handles name resolution and symbol binding.

```
parallax-resolve/
├── src/
│   ├── scope/         # Scope management
│   │   ├── global.rs  # Global scope handling
│   │   └── local.rs   # Local scope handling
│   ├── symbol.rs      # Symbol table implementation
│   ├── error.rs       # Resolution error types
│   └── visitor.rs     # Name resolution visitor
```

### `parallax-typeck`
Performs type checking and type inference on the resolved AST.

```
parallax-typeck/
├── src/
│   ├── context/       # Type checking context
│   ├── infer/         # Type inference engine
│   ├── unify/         # Type unification
│   ├── traits/        # Trait checking
│   ├── error.rs       # Type error definitions
│   └── hir.rs         # HIR generation
```

### `parallax-hir`
High-level Intermediate Representation - a simplified, type-annotated form of the AST.

```
parallax-hir/
├── src/
│   ├── hir.rs         # HIR data structures
│   ├── lower.rs       # AST to HIR lowering
│   ├── visitor.rs     # HIR visitor traits
│   └── db.rs          # Database interface
```

The HIR represents a program after name resolution and type checking have been performed.
It has the following key characteristics:
- All identifiers are fully resolved to their declarations
- Every expression, pattern, and declaration has complete type information
- No more visibility or scope information (handled during resolution)
- Simplified structure with implicit elements made explicit
- Serves as the foundation for optimization and code generation

### `parallax-mir`
Mid-level Intermediate Representation for optimization.

```
parallax-mir/
├── src/
│   ├── ir/           # IR definition
│   │   ├── basic_block.rs
│   │   ├── function.rs
│   │   └── instruction.rs
│   ├── opt/          # Optimization passes
│   │   ├── dce.rs    # Dead code elimination
│   │   ├── inline.rs # Function inlining
│   │   └── fold.rs   # Constant folding
│   └── visitor.rs    # IR visitor traits
```

### `parallax-codegen`
Code generation targeting interaction nets and LLVM.

```
parallax-codegen/
├── src/
│   ├── llvm/         # LLVM interface
│   │   ├── types.rs
│   │   └── builder.rs
│   ├── net/          # Interaction net generation
│   │   ├── builder.rs
│   │   └── optimize.rs
│   └── target/       # Target-specific code
```

### `parallax-net`
Core interaction net runtime and reduction engine.

```
parallax-net/
├── src/
│   ├── runtime/      # Runtime implementation
│   │   ├── types.rs  # Runtime type definitions
│   │   ├── reduce.rs # Reduction rules
│   │   └── gc.rs     # Garbage collection
│   ├── compile/      # Network compilation
│   │   ├── builder.rs
│   │   └── memory_layout.rs
│   ├── config.rs     # Runtime configuration
│   └── strings.rs    # String interning
```

### `parallax-hvm`
HVM (Higher-order Virtual Machine) integration.

```
parallax-hvm/
├── src/
│   ├── parser/       # HVM syntax parser
│   ├── lexer/        # HVM lexical analysis
│   ├── ast.rs        # HVM AST definition
│   └── translate/    # Translation to Parallax nets
```

### `parallax-cli`
Command-line interface for the compiler.

```
parallax-cli/
├── src/
│   ├── commands/     # CLI command implementations
│   ├── config.rs     # CLI configuration
│   └── main.rs       # Entry point
```

### `tree-sitter-parallax`
Tree-sitter grammar for syntax highlighting and parsing.

```
tree-sitter-parallax/
├── grammar.js        # Tree-sitter grammar definition
├── src/              # Generated parser
└── queries/          # Syntax highlighting queries
```

## Development Status

- ✅ `parallax-lang`: Core AST and parsing implementation
- ✅ `parallax-resolve`: Name resolution implementation
- ✅ `parallax-typeck`: Type checking and inference implementation
- ✅ `parallax-hir`: HIR definition and lowering implementation
- 🚧 `parallax-mir`: IR definition and optimization passes
- 🚧 `parallax-codegen`: LLVM and interaction net generation
- 🚧 `parallax-net`: Runtime and reduction engine
- 🚧 `parallax-hvm`: HVM integration
- ✅ `tree-sitter-parallax`: Grammar definition
- 🚧 `parallax-cli`: Command-line interface

## Contributing

Each crate maintains its own set of tests and documentation. When contributing:

1. Ensure tests pass for the crate you're modifying
2. Update documentation if you change public interfaces
3. Add new tests for new functionality
4. Follow the Rust style guide

## Building

```bash
# Build all crates
cargo build

# Run tests for all crates
cargo test

# Build and run the compiler
cargo run -p parallax-cli
``` 