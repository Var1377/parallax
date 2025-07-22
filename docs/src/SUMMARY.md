# Summary

[Introduction](./introduction.md)

# Language Overview

- [Core Principles](./overview/principles.md)
- [Syntax](./overview/syntax.md)
  - [Basic Values](./overview/basic-values.md)
  - [Expressions](./overview/expressions.md)
  - [Collections](./overview/collections.md)
  - [Functions](./overview/functions.md)
  - [Types](./overview/types.md)
  - [Comments](./overview/comments.md)
  - [Modules and Visibility](./overview/modules.md)
  - [Keywords](./overview/keywords.md)
- [Pattern Matching](./overview/pattern-matching.md)
- [Type System Basics](./overview/type-system.md)
  - [Type Inference](./overview/type-inference.md)
  - [Traits](./overview/traits.md)
  - [Generics](./overview/generics.md)
- [Memory Model](./overview/memory.md)
- [Error Handling](./overview/errors.md)
- [Parallelism Model](./overview/parallelism.md)

# Advanced Concepts

- [Type Categories and Refinement](./advanced/type-categories.md)
- [Type-Level Programming](./advanced/type-level.md)
  - [Const/Type Duality](./advanced/const-type-duality.md)
  - [Type Functions](./advanced/type-functions.md)
  - [Compile-Time Reflection](./advanced/reflection.md)
- [Advanced Traits](./advanced/traits.md)
  - [Associated Types](./advanced/associated-types.md)
  - [Higher-Kinded Types](./advanced/hkt.md)
  - [Trait Specialization](./advanced/specialization.md)
- [Type Enhancement (.with)](./advanced/type-enhancement.md)
- [Row Polymorphism](./advanced/row-polymorphism.md)
- [Effect System](./advanced/effects.md)

# The Compiler

- [Architecture Overview](./compiler/architecture.md)
- [Frontend](./compiler/frontend.md)
  - [Lexical Analysis](./compiler/lexer.md)
  - [Parsing](./compiler/parser.md)
  - [AST Design](./compiler/ast.md)
- [Middle End](./compiler/middle-end.md)
  - [Type Checking](./compiler/type-checking.md)
  - [Type Inference Algorithm](./compiler/inference.md)
  - [Trait Resolution](./compiler/trait-resolution.md)
  - [HIR Lowering](./compiler/hir.md)
  - [MIR Generation](./compiler/mir.md)
  - [Optimizations](./compiler/optimizations.md)
- [Backend](./compiler/backend.md)
  - [LLVM Code Generation](./compiler/llvm.md)
  - [Function Agents](./compiler/agents.md)
  - [Parallelization Analysis](./compiler/parallel-analysis.md)
- [Runtime System](./compiler/runtime.md)
  - [Work-Stealing Scheduler](./compiler/scheduler.md)
  - [Memory Management](./compiler/memory-management.md)
  - [FFI](./compiler/ffi.md)
- [Tooling](./compiler/tooling.md)
  - [Build System](./compiler/build.md)
  - [Package Manager](./compiler/packages.md)
  - [Language Server](./compiler/lsp.md)

# Language Reference

- [Keywords](./reference/keywords.md)
- [Grammar](./reference/grammar.md)
- [Type System Reference](./reference/types.md)
- [Standard Library](./reference/stdlib.md)
  - [Core Types](./reference/core-types.md)
  - [Collections](./reference/collections.md)
  - [Iterators](./reference/iterators.md)
  - [IO and Effects](./reference/io.md)
  - [Concurrency](./reference/concurrency.md)
- [Attributes](./reference/attributes.md)
- [Error Index](./reference/errors.md)

# Appendices

- [Glossary](./appendix/glossary.md)
- [Bibliography](./appendix/bibliography.md)
- [Comparison with Other Languages](./appendix/comparison.md)
- [Design Rationale](./appendix/rationale.md)
- [Future Directions](./appendix/future.md)
