# Task 1 Progress Report: Clarify and Centralize Type Resolution

## Changes Made

1. Removed dependency on `parallax_resolve::types::ResolvedType` from `parallax-typeck/src/resolve.rs`:
   - Removed import of `ResolvedType`
   - Removed the `convert_resolved_type` method that translated between `ResolvedType` and `Ty`
   - Enhanced the `TypeResolver` to work directly with the `SymbolTable` for lookups

2. Improved type resolution in `parallax-typeck/src/resolve.rs`:
   - Added support for `KindApp` type structure
   - Added proper symbol table lookups for types, variables and functions
   - Fixed borrowing issues with symbol iteration

## Current State

- The `parallax-typeck` crate can now function without directly depending on `parallax-resolve/src/types.rs`
- The resolver looks up symbols directly in the `SymbolTable` and converts AST types to the typechecker's `Ty` type directly
- Placeholder implementations for some functionality that can be filled in during later stages

## Remaining Work for Task 1

1. **Remove `parallax-resolve/src/types.rs`**:
   - We need to check if any other components in `parallax-resolve` depend on this module
   - Update `parallax-resolve/Cargo.toml` to remove any dependencies needed only for the types module
   - Remove the module and update `lib.rs` to no longer export it

2. **Expand type resolution capabilities in `parallax-typeck/src/resolve.rs`**:
   - Add more comprehensive handling of generic type parameters
   - Implement proper validation against the symbol table
   - Fill in the placeholder implementations with real logic

3. **Update and consolidate type-related error handling**:
   - Ensure span information is properly passed around for error reporting
   - Make error messages more specific and helpful

## Next Steps

1. Update any code in the `parallax-resolve` crate that uses the `types` module and remove or relocate that functionality
2. Remove the `types` module and update `lib.rs` accordingly
3. Update tests that may depend on the removed types module

## Impact on Other Tasks

- Task 3 (Implement `SymbolResolver` interface) is partially completed by the improvements to `resolve.rs`
- The changes improve the architecture by placing type resolution responsibility in the typeck crate, which is more appropriate 