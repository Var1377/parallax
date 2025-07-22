# Core Principles

Parallax is built on several fundamental principles.

## 1. Correctness by Construction

Make invalid states unrepresentable through a powerful and flexible type system:

- **No null**: Use `Option[T]` for optional values
- **No exceptions**: Use `Result[T, E]` for errors
- **Exhaustive matching**: Compiler ensures all cases handled
- **Type-level guarantees**: Express invariants as types
- **Rich type system**: Type refinement, categories, and compile-time computation

## 2. Zero-Cost Abstractions

High-level features with no runtime overhead through ML-style type inference:

- **Type inference**: Write less, infer more
- **Compile-time computation**: Move work from runtime to compile time
- **Monomorphization**: Generic code compiles to specialized versions
- **Inline optimization**: High-level abstractions compile away

## 3. Performance First

Every feature must compile to efficient native code:

- **Automatic parallelization**: Exploit all available cores
- **Native compilation**: Direct to machine code, no interpreter
- **Compile-time Computation**: Compute as much as you can at compile-time
- **Predictable performance**: No hidden allocations or runtime overhead
- **Cache-friendly data structures**: Optimize for modern CPU architectures