# Parallax Crates Overview

This directory contains the core crates that make up the Parallax compiler and runtime. Below is an overview of each crate and its role in the compilation pipeline.

## Compilation Pipeline

```
Source Code (Files + frame.toml)
     ↓
[parallax-source] → Frame (Config + Source Files)
     ↓
[parallax-syntax] → ModuleStructure (AST + Errors)
     ↓
[parallax-resolve] → ResolvedModuleStructure (Resolved Definitions + Scopes)
     ↓
[parallax-types] → TypedModule (Typed Definitions + Typed AST + Trait Info)
     ↓
[parallax-hir] → HirModule (ANF HIR + Optimizations)
     │                           
     ├──────────────────────> [parallax-native] → CompiledArtifact (Native Code)
     │                           │
     │                           │          [parallax-hvm]
     │                           │                │
     │                           │                ↓
     └─> [parallax-mir] ─────→ MirGraph ──> [parallax-net] → Reduced Net
                                  │               │
                                  │               │
           [parallax-codegen] <───┴───────────────┘
           (Orchestrator)
                    │
                    └───────────────> [parallax-rt]
                                   (Runtime Execution)
```

## Core Crates

### `parallax-source`
Manages source code "Frames" (compilation units with `frame.toml`), loading source files, handling dependencies, and providing the initial input (`Frame`) to the compiler pipeline. Uses Salsa for incremental tracking.

### `parallax-syntax`
The frontend parser, taking `Frame`s from `parallax-source`. Uses `tree-sitter-parallax` to parse source files into Abstract Syntax Trees (ASTs), handles module structures (file/directory/inline), and produces a `ModuleStructure` containing the AST and parsing errors. Uses Salsa.

### `parallax-resolve`
Takes the `ModuleStructure` from `parallax-syntax`. Performs name resolution, builds scopes, resolves imports and paths, handles the standard library, and produces a `ResolvedModuleStructure` containing resolved definitions and symbols. Uses Salsa.

### `parallax-types` (formerly `parallax-typeck`)
Takes the `ResolvedModuleStructure` from `parallax-resolve`. Performs type checking and inference (using Hindley-Milner), resolves trait implementations and bounds, and produces a `TypedModule` containing fully typed definitions, AST, and trait information. Uses Salsa.

### `parallax-hir`
Takes the `TypedModule` from `parallax-types`. Lowers the typed AST into a High-level Intermediate Representation (HIR) based on A-Normal Form (ANF). Provides optimizations. Feeds into both `parallax-mir` (for net backend) and `parallax-native` (for native backend).

### `parallax-mir`
Takes the `HirModule` from `parallax-hir`. Lowers the HIR into a graph-based Mid-level Intermediate Representation (MIR) focused on data flow and explicit resource handling. Feeds into `parallax-net`.

### `parallax-codegen`
Orchestrates the code generation phase. Takes the `HirModule` and decides which backend(s) to invoke (`parallax-native`, `parallax-mir` -> `parallax-net`). Collects results for the runtime.

### `parallax-native`
Native code generation backend. Takes the `HirModule` from `parallax-hir` (via `parallax-codegen`) and compiles it to native machine code using Cranelift JIT. Produces a `CompiledArtifact` for `parallax-rt`.

### `parallax-net`
Interaction net runtime. Takes the `MirGraph` from `parallax-mir` (via `parallax-codegen`) or translated code from `parallax-hvm`. Performs parallel graph reduction. Provides reduced results to `parallax-rt`.

### `parallax-hvm`
HVM (Higher-order Virtual Machine) integration. Translates HVM source or representation directly into the interaction net format used by `parallax-net`.

### `parallax-rt`
The unified runtime environment. Takes compiled artifacts from `parallax-native` and/or reduced nets from `parallax-net`. Manages execution, potentially coordinating between the native and net execution models, and produces the final program result.

### `parallax-cli`
Command-line interface (`plx`) for managing Parallax projects (frames) and driving the compiler (which uses `parallax-db`).

### `tree-sitter-parallax`
Tree-sitter grammar definition for the Parallax language, used by `parallax-syntax`.

### `parallax-db`
Provides the central Salsa database (`Compiler`) that integrates queries from all compiler stages, enabling incremental compilation.

## Development Status

- ✅ `parallax-source`: Frame management and source loading.
- ✅ `parallax-syntax`: Core AST and parsing implementation.
- ✅ `parallax-resolve`: Name resolution implementation.
- ✅ `parallax-types`: Type checking and inference implementation.
- ✅ `parallax-hir`: HIR definition and lowering implementation.
- ✅ `parallax-mir`: MIR definition and HIR lowering implementation.
- 🚧 `parallax-codegen`: Orchestration requires significant updates for dual backends.
- 🚧 `parallax-native`: Native code generation via Cranelift JIT (takes HIR).
- 🚧 `parallax-net`: Runtime and reduction engine (takes MIR).
- 🚧 `parallax-hvm`: HVM integration (translates to net).
- 🔴 `parallax-rt`: New runtime crate, needs implementation.
- ✅ `tree-sitter-parallax`: Grammar definition.
- ✅ `parallax-cli`: Command-line interface structure and basic commands.
- ✅ `parallax-db`: Core Salsa database integration.

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

# Build and run the compiler CLI
cargo run -p parallax-cli -- help
``` 