# parallax-resolve

## Overview

The `parallax-resolve` crate handles name resolution, type checking, and semantic analysis for the Parallax programming language. It processes the Abstract Syntax Tree (AST) produced by `parallax-syntax` and constructs a fully resolved representation of the code, including type information and unique symbols for all definitions.

## Architecture

The resolver follows a multi-pass design, orchestrated by the `Resolver` struct in `core.rs`:

1.  **Definition Collection (`definitions.rs`)**: Traverses the AST of the target module *and* all dependency frames (including the standard library) to find top-level definitions (structs, enums, functions, traits, impls, modules). Basic information about each definition (`DefinitionInfo`) is collected into a map keyed by a generated unique `Symbol`.
2.  **Module Scope Building (`scopes.rs`)**: For each module (including those from dependencies), resolves `use` declarations by looking up paths in the `definitions_map`. Builds a `ModuleScope` for each module, mapping visible names (both locally defined and imported) to their resolved `Symbol`.
3.  **Prelude Scope Building (`resolve_types.rs`)**: Constructs a prelude scope by gathering items exported from the standard library's `std::prelude` module.
4.  **Signature Resolution (`resolve_types.rs`)**: Iterates through the collected definitions. Resolves type references within the signatures of structs, enums, functions, traits, and impls using the module scopes and prelude scope. Populates the `ResolvedDefinitions` structure.
5.  **Body Resolution (`resolve_expr.rs`)**: Resolves expressions within function bodies. Performs type checking, resolves local variable references using a `ScopeStack`, handles pattern matching, and identifies unused variables or imports.

## Key Components

### Symbol System (`types.rs`)

- Uses a unique `Symbol` (a wrapped `u32`) generated via an atomic counter for each definition (struct, function, module, enum variant, field, parameter, etc.).
- Avoids ambiguity and enables efficient lookups.

### Definition Info (`definitions.rs`)

- Intermediate `DefinitionInfo` struct stores metadata during Pass 1: name, kind, parent symbol, visibility, AST reference, generic parameter names, etc.

### Module Scopes (`scopes.rs`)

- `ModuleScope` maps visible names (strings) to `ScopeEntry`.
- `ScopeEntry` contains the resolved `Symbol` and import information (originating module, `use` span, usage tracking).
- Handles path resolution (`resolve_path`) relative to module scopes, including `self`, `super`, and `crate`.

### Accessibility Rules (`scopes.rs`)

- Implements Rust-like visibility rules via `is_accessible` function.
- Public (`pub`) items are generally accessible (path permitting).
- Private items are accessible within their defining module and its descendants.

### Type System (`types.rs`)

- `ResolvedType` enum represents fully resolved types (Primitive, UserDefined, Function, Tuple, Array, GenericParam, Never, SelfType).
- `ResolvedDefinitions` aggregates resolved structs, enums, functions, traits, impls, and intrinsics.
- `ResolvedModuleStructure` is the final Salsa-tracked output, containing `ResolvedDefinitions`, entry point symbol, core traits, intrinsics, errors, and warnings.

### Expression Resolution (`resolve_expr.rs`)

- Uses `ScopeStack` with `LocalBinding` to manage local variable scopes, shadowing, and usage tracking.
- Produces `ResolvedExpr` tree nodes, annotated with their `ResolvedType`.
- Handles type checking for binary/unary ops, calls, matches, struct literals, etc.
- Resolves patterns (`ResolvedPattern`) against expected types.

## Implementation Details

### Salsa Integration (`lib.rs`)

- Defines the `ResolveDatabase` trait extending `SyntaxDatabase`.
- The main entry point is the `resolve_definitions` query method, backed by the `#[salsa::tracked]` function `resolve_definitions_query`.
- This enables incremental re-resolution when source code changes.

### Error Handling (`error.rs`)

- `ResolutionError` enum captures fatal errors (e.g., NameNotFound, TypeMismatch, DuplicateDefinition, PrivateItemAccess).
- `ResolverWarning` enum captures non-fatal issues (e.g., UnusedVariable, UnusedImport, ShadowedVariable).
- Resolution attempts to continue after errors to report as many issues as possible.

### Dependency & Stdlib Handling (`core.rs`, `lib.rs`)

- Explicitly loads the standard library frame (`parallax-stdlib`).
- Processes definitions and builds scopes for all dependency frames provided to the `Resolver`.

## Usage

The primary interface is the `resolve_definitions` query on a type implementing `ResolveDatabase`. It takes the root `ModuleUnit` of the crate to be compiled.

```rust
use parallax_resolve::{ResolveDatabase, ResolvedModuleStructure};
use parallax_syntax::ModuleUnit;

fn resolve_crate(db: &dyn ResolveDatabase, root_module: ModuleUnit)
    -> ResolvedModuleStructure
{
    // This query triggers the multi-pass resolution process,
    // including loading stdlib and handling dependencies.
    db.resolve_definitions(root_module)
}
```

The returned `ResolvedModuleStructure` contains the resolved definitions, potential entry point, core traits/intrinsics, errors, and warnings.

## Internal Structure

The crate is organized into the following modules:

- `core.rs`: Main `Resolver` struct and orchestration of the passes.
- `definitions.rs`: Pass 1 - `DefinitionInfo` struct and collection logic.
- `scopes.rs`: Pass 2 - `ModuleScope`, `ScopeEntry`, `use` resolution, path resolution helpers, visibility checks.
- `resolve_types.rs`: Pass 3 - Logic for resolving type references in signatures; builds prelude scope.
- `resolve_expr.rs`: Pass 4 - Logic for resolving function bodies, expressions, patterns, and local scopes.
- `types.rs`: Core data structures for resolved items (`Symbol`, `ResolvedType`, `ResolvedFunction`, etc.) and the final `ResolvedModuleStructure`.
- `error.rs`: `ResolutionError` and `ResolverWarning` enums.
- `lib.rs`: Salsa database trait (`ResolveDatabase`) and query definition.

## Dependencies

- `parallax-syntax`: Provides the AST (`ModuleUnit`, `Item`, `Type`, `Expr`, etc.) and `SyntaxDatabase`.
- `parallax-source`: Provides `SourceDatabase` for file/frame access.
- `parallax-stdlib`: Used to load the standard library frame.
- `salsa`: Core incremental computation framework.
- `miette`: For diagnostic error reporting framework.
- `thiserror`: For deriving standard error traits.
- `fxhash`: Faster hash map implementation.
