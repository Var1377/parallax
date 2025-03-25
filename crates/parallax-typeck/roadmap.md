# Parallax Resolve & Typeck Integration TODO

This document outlines the remaining tasks to fully integrate the `parallax-resolve` and `parallax-typeck` crates. The goal is for `parallax-resolve` to produce a resolved Abstract Syntax Tree (AST) and a `SymbolTable`, which `parallax-typeck` then consumes to perform type inference, constraint solving, and generate a type-checked High-level Intermediate Representation (HIR).

## Tasks

1.  **✅ Clarify and Centralize Type Resolution:**
    *   **Issue:** Significant overlap between `parallax-resolve/src/types.rs` (`ResolvedType`, `TypeEnv`) and `parallax-typeck` (`Ty`, `TypeContext`). Type resolution (`ast::Type` -> `typeck::Ty`) should primarily be `parallax-typeck`'s responsibility.
    *   **Action:** Decide on the final responsibility boundary. Likely refactor/remove most of `parallax-resolve/src/types.rs` and consolidate type structure logic within `parallax-typeck`, potentially using `parallax-typeck/src/resolve.rs` as the bridge using the `SymbolTable`.
    *   **Status:** Completed. The `parallax-typeck/src/resolve.rs` file now handles type resolution directly from AST to `Ty`, using the `SymbolTable` for lookup.

2.  **✅ Connect Resolver Output to Typechecker Input:**
    *   **Need:** `parallax-typeck` needs the `ResolvedCrate` (resolved AST + `SymbolTable`) from `parallax-resolve`.
    *   **Mechanism:** Via the `TypeCheckingDatabase` trait (`parallax-typeck/src/db.rs`).
    *   **Action:** Implement `TypeCheckingDatabase` (likely using Salsa) to query `resolved_crate` from `parallax-resolve` and make the `SymbolTable` and resolved AST available.
    *   **Status:** Completed. Implemented `TypeCheckDb` trait that extends `ResolverDatabase` to seamlessly integrate with the resolver. Also created the `type_check_crate_impl` function to perform the full type checking process.

3.  **✅ Implement `SymbolResolver` Interface:**
    *   **Need:** `parallax-typeck/src/db.rs` defines `SymbolResolver` for type checking lookups. The current implementation is a stub.
    *   **Action:** Create a proper `SymbolResolver` implementation that queries the `SymbolTable` (from `parallax-resolve` via `TypeCheckingDatabase`) to provide type information for names/paths.
    *   **Status:** Completed. Created `TableBasedSymbolResolver` that uses the `SymbolTable` from `ResolvedCrate` to look up symbols and their types. Also updated `TypeContext` to use the resolver for symbol lookups and type resolution.

4.  **Complete Type Inference Logic (`infer.rs`):**
    *   **Need:** Several aspects are incomplete or use placeholders.
    *   **Action:**
        *   Implement full handling for generics (parameters, bounds).
        *   Generate `Constraint::HasTrait` correctly.
        *   Replace placeholder lookups with calls to the real `SymbolResolver`.
        *   Cover all remaining AST node types.
        *   Remove debug `println!` statements.

5.  **Implement Full Unification and Constraint Solving (`unify.rs`):**
    *   **Need:** Robust handling of all `Ty` variants, substitutions, trait constraints, and potentially subtyping.
    *   **Action:**
        *   Decide on and implement subtyping rules (if applicable).
        *   Test unification with generics and traits.
        *   Integrate trait solving results.

6.  **Implement Trait System (`TraitSolver`, `PredicateObligation`):**
    *   **Need:** Marked as incomplete; core logic for checking implementations is missing.
    *   **Action:** Implement `TraitSolver` to find `impl` blocks, check bounds, resolve associated items, handle coherence, and process `PredicateObligation`s using the `TypeCheckingDatabase`.

7.  **Implement HIR Lowering (`hir.rs`):**
    *   **Need:** `lower_crate` function is a stub.
    *   **Action:** Implement the transformation from the resolved AST + completed `type_map` (post-unification) into the `hir::Crate` structure, annotating HIR nodes with final types.

8.  **Refine Error Handling and Diagnostics:**
    *   **Need:** Ensure errors are comprehensive, accurate, and use correct spans.
    *   **Action:** Verify span propagation, handle `ResolveError`s, and test diagnostic output (`diagnostics.rs`).

9.  **Write Comprehensive Integration Tests (`tests/test_inference.rs`):**
    *   **Need:** Current tests are basic mocks.
    *   **Action:** Create tests exercising the full parse -> resolve -> typecheck pipeline. Verify inferred types (`type_map`), errors, and potentially HIR output.
