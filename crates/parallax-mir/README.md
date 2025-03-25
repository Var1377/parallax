# Parallax MIR (Mid-level Intermediate Representation)

## Overview

The Parallax MIR crate provides a control-flow-graph-based intermediate representation for the Parallax compiler. It sits between the HIR (High-level Intermediate Representation) and the backend code generation, serving as the primary IR for optimizations.

## Key Features

- **Explicit Control Flow**: Code is organized into basic blocks with explicit terminators
- **SSA-like Properties**: Values are immutable once defined
- **Memory Model**: Explicit "places" represent memory locations
- **Pattern Matching**: First-class support for destructuring and pattern matching
- **Monomorphization**: Generic code is specialized with concrete types
- **Optimization-friendly**: Designed to support standard compiler optimizations

## Core Components

### MIR Structure

The MIR for a function consists of:

- **Function Metadata**: Name, signature, and attributes
- **Control Flow Graph**: A collection of basic blocks
- **Basic Blocks**: Sequences of statements ending with a terminator
- **Statements**: Non-branching operations (assignments, calls, allocations)
- **Terminators**: Control flow operations that end basic blocks (switches, matches, function calls)
- **Places**: Memory locations (local variables with optional projections)
- **Values**: Operands, literals, and operations on them

### Control Flow Model

In Parallax MIR:
- Control flow is represented by transitions between basic blocks
- There are no primitive "jump" or "return" instructions
- Function calls are terminators that can transfer control to another block
- All control flow is explicit and represented in the CFG
- Pattern matching is lowered to decision trees with switch terminators

### Lowering Process

HIR is lowered to MIR through these steps:

1. Create function metadata and an empty CFG
2. Lower expressions to statements and terminators
3. Handle pattern matching by lowering to decision trees
4. Convert mutable variables to places
5. Implement operator calls as function calls

### Monomorphization

Generic code is specialized with concrete types:

1. Identify type parameters in functions
2. Create specialized versions with concrete types
3. Replace generic operations with concrete ones
4. Link specialized function calls

### Optimizations

MIR enables several optimization passes:

- **Constant Folding**: Evaluate constant expressions at compile time
- **Dead Code Elimination**: Remove unreachable or unused code
- **Inlining**: Replace function calls with their bodies
- **Simplification**: Various algebraic simplifications

## Implementation Plan

### Phase 1: Core Structure (Completed)

- [x] Define data structures for MIR
- [x] Implement visitors and utility functions
- [x] Set up the query database interface

### Phase 2: HIR to MIR Lowering (Completed)

- [x] Implement expression lowering
- [x] Add statement lowering
- [x] Support pattern matching lowering
- [x] Handle function definitions and calls
- [x] Implement basic type conversions
- [x] Implement operator lowering to function calls

### Phase 3: Monomorphization (Completed)

- [x] Detect and identify generic parameters
- [x] Implement type substitution
- [x] Create monomorphized function copies
- [x] Update call sites to use specialized functions

### Phase 4: Optimizations (In Progress)

- [x] Implement constant folding
- [ ] Add dead code elimination
- [ ] Support function inlining
- [ ] Implement standard optimizations
- [ ] Add optimization levels

### Phase 5: Integration

- [ ] Connect to the HIR crate
- [ ] Set up integration with code generation backend
- [ ] Implement standard library integration
- [ ] Add comprehensive testing

## Standard Library Integration

The MIR will support a standard library through these mechanisms:

1. Pre-defined MIR functions for core operations
2. Special handling for built-in types and operations
3. Integration with the type system for proper type checking
4. Support for intrinsic functions with special semantics

## Usage Example

```rust
// Using the MIR in a compiler pipeline
fn compile_function(db: &dyn MirDatabase, hir_function_id: u32) -> Result<(), Error> {
    // 1. Lower HIR to MIR
    let mir_function = db.mir_function(hir_function_id)?;
    
    // 2. Optimize the MIR (with specified optimization level)
    let optimized_mir = db.optimized_function(mir_function, OptimizationLevel::Full)?;
    
    // 3. Generate code from the optimized MIR
    // ...
    
    Ok(())
}
```

## Contributing

See the main Parallax repository for contribution guidelines.

## License

Same as the main Parallax project. 