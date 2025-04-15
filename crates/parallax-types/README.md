# parallax-types

## Overview

This crate implements type checking and inference for the Parallax programming language. It takes the resolved module structure from `parallax-resolve`, verifies type correctness according to the language rules, infers types where necessary, checks trait bounds and implementations, and produces a fully typed Abstract Syntax Tree (AST) representation.

The core responsibilities include:

- Defining the internal representation of types (`Ty`, `TyKind`, `TypeDef`, etc.).
- Implementing the type inference algorithm (unification, substitution) via `InferenceContext`.
- Managing trait definitions and implementations (`TraitRepository`, `TraitDef`, `ImplDef`).
- Orchestrating the type checking process (`TypeChecker`) over declarations and expressions.
- Reporting type errors (`TypeError`) using `miette` diagnostics.
- Producing a typed output (`TypedModule`, `TypedDefinitions`, `TypedExpr`, `TypedPattern`).

## Structure

The crate is organized as follows:

- `src/lib.rs` - Main entry point, `TypeDatabase` trait definition, and `type_check_definitions` query.
- `src/types.rs` - Core type representations (`Ty`, `TyKind`, `TypeDef`, `FunctionSignature`, `TypedModule`, `TypedDefinitions`, `TypedExpr`, etc.) and `TypeContext`.
- `src/inference.rs` - Type inference engine (`InferenceContext`, `Substitution`, `TypeEnvironment`, unification logic).
- `src/traits.rs` - Trait system (`TraitRepository`, `TraitId`, `ImplId`, `TraitDef`, `ImplDef`).
- `src/error.rs` - Error types (`TypeError`) and `TypeResult`.
- `src/typecheck/` - The main type checking logic:
    - `mod.rs`: `TypeChecker` struct, orchestration.
    - `declarations.rs`: Checking structs, enums, functions, traits, impls.
    - `expressions.rs`: Checking various expression kinds.
    - `patterns.rs`: Checking pattern matching.
    *   `operators.rs`: Checking binary/unary operators via trait dispatch.
    *   `invocations.rs`: Checking function/method calls.
    *   `control_flow.rs`: Checking `if`, `match`, `block`.
    *   `aggregates.rs`: Checking literals like arrays, tuples, structs.
    *   `helpers.rs`: Utility functions.

## Core Concepts

- **`Ty` / `TyKind`**: Represents types, including primitives, named types (structs/enums), functions, tuples, arrays, type variables (`Var(TypeId)`), and special types like `SelfType`.
- **`InferenceContext`**: Manages type variables and substitutions (`Substitution`). Provides the `unify` method.
- **`TypeChecker`**: The central struct that drives the type checking process. It holds the `InferenceContext`, `TypeContext`, `TraitRepository`, resolved definitions, and accumulates errors.
- **`TraitRepository`**: Stores definitions of traits (`TraitDef`) and their implementations (`ImplDef`), indexed by `TraitId` and `ImplId`.
- **`TypeContext`**: Stores definitions of types (`TypeDef`), function signatures, and inherent methods, accessible by name or symbol.
- **Two-Pass Checking**: Declarations are checked in two passes: first signatures (to populate `TypeContext`), then bodies (using the established signatures).
- **Typed Output**: The final result is a `TypedModule` containing `TypedDefinitions` (maps of Symbols to `TypedStruct`, `TypedEnum`, `TypedFunction` with resolved types and typed bodies), the `TraitRepository`, and any errors.

## Salsa Integration

- Defines the `TypeDatabase` trait extending `ResolveDatabase`.
- The main entry point is the `type_check_definitions` query, which takes a `ResolvedModuleStructure` and returns a `TypedModule`.
- Salsa tracks dependencies, allowing incremental type checking when the resolved input changes.

## Usage

This crate is used internally by the Parallax compiler after the name resolution phase (`parallax-resolve`). It takes the `ResolvedModuleStructure` and produces a `TypedModule` which is then used by subsequent phases like High-level IR (HIR) generation or code generation.

## Dependencies

- `parallax-resolve`: Provides the resolved AST, definitions, and `ResolveDatabase`.
- `parallax-syntax`: Provides AST structures used in error reporting or literals.
- `salsa`: For incremental computation.
- `miette`: For diagnostic error reporting.
- `thiserror`: For defining the `TypeError` enum.
- `serde`: For potential serialization needs (e.g., trait IDs).
- `log`: For logging during the type checking process.
- `triomphe`: (Potentially used for efficient Arc cloning, based on Cargo.toml).

## Contributing

See the repository's main README for general contribution guidelines. 