# AGENTS.md - Parallax Compiler

## Overview

Parallax is a native-first parallel programming language where all computation executes as optimized native code. Functions compile to agents that return either direct results or parallel work opportunities, enabling automatic parallelization without annotations.

## Core Architecture

**Compilation Pipeline**: Source → Parser → Type Checker → HIR → MIR → Native Code + Function Agents → Runtime

**Key Innovation**: Functions compile to native code returning enums that indicate:
- `Value(T)` - Direct result (base case)
- `Parallel{...}` - Work to distribute (recursive case)

The runtime's work-stealing scheduler recognizes these patterns and distributes work across cores.

## Implementation Principles

1. **Performance First**: Every decision prioritizes runtime performance. Profile constantly.
2. **Native Execution**: No graph reduction overhead. Pure native code with coordination metadata.
3. **Zero-Cost Parallelism**: Automatic parallelization with no runtime overhead for sequential code.
4. **Incremental Compilation**: Use Salsa for query-based compilation caching.
5. **Type-Rich, RTTI-Free**: Maximum compile-time information, zero runtime type overhead.

## Project Structure

Start simple, grow as needed:
```
parallax/
├── AGENTS.md          # This file
├── README.md          # User documentation
├── Cargo.toml         # Workspace root
├── src/               # Initial monolithic implementation
└── tests/             # Integration tests
```

As complexity grows, split into crates. Each crate maintains its own AGENTS.md documenting its specific responsibilities, architecture, and performance characteristics.

## Testing Strategy

- **Unit Tests**: Colocated with code using `#[cfg(test)]`
- **Integration Tests**: In `tests/` directory
- **Benchmarks**: Use `criterion` for micro-benchmarks
- **Performance Regression**: Track key metrics across commits

Run all tests with `cargo test`. Benchmark with `cargo bench`.

## Performance Guidelines

1. **Measure First**: Profile before optimizing. Use `perf`, `flamegraph`, and `criterion`.
2. **Memory Layout**: Optimize data structures for cache efficiency.
3. **Allocation Strategy**: Prefer arena allocation for compiler passes.
4. **Parallelism Granularity**: Ensure tasks are >10μs to amortize overhead.
5. **SIMD When Possible**: Vectorize hot loops in code generation.

## Development Workflow

1. Start with a working end-to-end prototype
2. Profile to identify bottlenecks
3. Optimize the critical path
4. Extract modules into crates when clear boundaries emerge
5. Each crate gets its own AGENTS.md with performance focus

## Key Components

**Parser**: Fast, error-recovering parser with good diagnostics  
**Type System**: Refinement types with SMT solving for safety  
**Code Generator**: LLVM-based native code generation  
**Runtime**: Work-stealing scheduler with minimal overhead  

## Best Practices

- Profile early and often
- Prefer simple, fast solutions over clever ones
- Document performance characteristics in code
- Use `#[inline]` judiciously based on profiling
- Minimize allocations in hot paths
- Keep IRs simple and cache-friendly

---

*Performance is the primary goal. Measure everything.*