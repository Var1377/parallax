# parallax-native

## Overview

This crate is responsible for compiling the Parallax High-level Intermediate Representation (HIR) into native machine code. It utilizes the Cranelift code generation library and its JIT (Just-In-Time) capabilities.

Key responsibilities include:

- Translating HIR functions and expressions into Cranelift Intermediate Representation (IR).
- Integrating with a garbage collector (`rsgc`) via FFI functions and providing mechanisms for root tracking (shadow stack, global roots).
- Computing type layouts using `repc` and manual calculations for enums.
- Defining runtime type representations (`StringRef`, `ClosureRef`).
- Orchestrating the compilation process via the `backend.rs` module.
- Implementing specific optimizations like Tail Call Optimization (TCO) for certain call patterns.

## Key Features

- **HIR to Native Code:** Compiles HIR directly to machine code using Cranelift JIT.
- **Garbage Collection Integration:** Works with `rsgc` for memory management.
    - FFI functions for runtime allocation (`parallax_alloc_closure`, `parallax_alloc_string_ref`).
    - Shadow stack mechanism for rooting stack variables in JIT code.
    - Global root registration for GC-managed static variables.
- **Type Layout Computation:** Uses `repc` for basic layouts and performs manual calculations for enums.
- **Runtime Representations:** Defines `#[repr(C)]` structs (`StringRef`, `ClosureRef`) for interaction with compiled code.
- **Tail Call Optimization (TCO):** Implemented for direct and indirect calls in tail position matching specific HIR patterns.

## Structure

The crate is organized as follows:

- `src/lib.rs` - Main entry point, exports, and runtime type definitions (`StringRef`).
- `src/backend.rs` - Orchestrates the compilation using `JITModule`, translates HIR module, produces `CompiledArtifact`.
- `src/error.rs` - Defines the `NativeError` enum.
- `src/translator/` - Modules responsible for translating HIR to Cranelift IR:
    - `context.rs`: `TranslationContext` for managing state during translation.
    - `expr.rs`: Translates HIR expressions and values.
    - `func.rs`: Translates HIR functions.
    - `layout.rs`: Computes type layouts using `repc` and manual enum calculations.
    - `types.rs`: Translates HIR types and signatures to Cranelift types.
- `src/gc/mod.rs` - Handles Garbage Collection integration with `rsgc`, including FFI allocation functions, shadow stack, and global roots.

## Usage

The primary entry point is the `compile_hir` function in `backend.rs`. It takes a `HirModule` and returns a `CompiledArtifact` containing function pointers to the compiled native code.

```rust
// Example (Conceptual)
use parallax_native::{compile_hir, CompiledArtifact, NativeError};
use parallax_hir::hir::HirModule;

fn compile(hir_module: &HirModule) -> Result<CompiledArtifact, NativeError> {
    let artifact = compile_hir(hir_module)?;
    // artifact.functions contains mapping from Symbol to CompiledFunction
    // Use artifact.get_function::<fn(...) -> ...>(symbol) to get callable pointers
    Ok(artifact)
}
```

## Dependencies

- `parallax-hir`: Provides the HIR data structures.
- `parallax-resolve`: Provides `Symbol` definition.
- `cranelift-codegen`, `cranelift-module`, `cranelift-native`, `cranelift-jit`, `cranelift-frontend`: Core Cranelift libraries for code generation and JIT compilation.
- `target-lexicon`: For target triple information.
- `repc`: For ABI layout computation.
- `rsgc`: For garbage collection integration.
- `memoffset`: Used in GC struct definitions.
- `rustc-hash`: For efficient hashing.
- `thiserror`: For error definition.

## Contributing

See the repository's main README for general contribution guidelines. 