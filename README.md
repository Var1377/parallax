# Parallax: Functional-First Systems Programming Language

## Overview

Parallax is a functional-first systems programming language that achieves automatic parallelization through a novel native-first execution model. It combines the elegance of functional programming with the performance and control needed for systems development.

## Key Features

- **Expression-Oriented**: Everything is an expression that returns a value
- **Immutable by Default**: Explicit `mut` keyword for controlled mutation
- **No Loops**: Pure functional iteration with iterator combinators
- **Pattern Matching Everywhere**: In bindings, control flow, and function arguments
- **Advanced Type System**: HKTs, GATs, const generics, and type-level programming
- **Automatic Parallelization**: Functions compile to agents that expose parallelism
- **Native Performance**: Zero-cost abstractions with LLVM backend
- **Memory Safety**: Refinement types and lifetime analysis

## Language Design

### Syntax Highlights

```parallax
// Unified generic syntax with square brackets
const map = [T, U] => (list: List[T], f: (T) -> U) -> List[U] => 
    list.iter().map(f).collect();

// Pattern matching in bindings
let { name, age, .. } = person;
let Some(value) = maybe_value else return None;

// No loops - only iterators
let evens = (1..100)
    .filter(n => n % 2 == 0)
    .map(n => n * n)
    .collect[Vec[_]]();

// Everything is an expression
let result = if condition {
    compute_value()
} else {
    default_value
};

// No indexing syntax - methods for safety
let element = array.get(index);  // Returns Option[T]
```

### Key Design Decisions

1. **Square Brackets for Generics**: Avoids parsing ambiguity with comparison operators
2. **No Indexing Syntax**: Use `.get(i)` for safe access, `.get_unsafe(i)` for performance
3. **No Manual Variance**: Compiler automatically infers variance from usage
4. **Unified Generic Syntax**: All generics use `[T] =>` pattern consistently
5. **No Loops**: Replaced with iterator methods for functional purity

### Type System Features

```parallax
// Higher-Kinded Types
type Functor = [F[_]] => trait {
    const map: [A, B] => (F[A], (A) -> B) -> F[B];
};

// Generic Associated Types
type Iterator = trait {
    type Item;
    type IntoIter[T]: Iterator[Item = T];
};

// Const Generics
type Array = [T, const N: usize] => struct {
    data: [T; N],
};

// Type-level Programming
type If = [const B: bool, T, F] => type = 
    match B { true => T, false => F };
```

## Native-First Execution

Functions compile to native code that returns either direct results or parallel work opportunities:

```parallax
// Your code
const fib = (n: i32) -> i32 => 
    if n <= 1 { n } else { fib(n-1) + fib(n-2) };

// Compiles to agents returning:
enum FibResult {
    Value(i32),                        // Base case
    Parallel { left: i32, right: i32 } // Work to distribute
}
```

The runtime's work-stealing scheduler automatically distributes work across cores.

## Language Features

### Core Language

- **Functional Core**: Immutable by default, expression-oriented
- **Pattern Matching**: Exhaustive matching with guards and bindings
- **Algebraic Data Types**: Enums and structs with generics
- **Higher-Order Functions**: First-class functions with closures
- **Type Inference**: Hindley-Milner with extensions

### Type System

- **Refinement Types**: Compile-time predicates on values
- **Effect Tracking**: Pure vs effectful computations  
- **Lifetime Analysis**: Extended borrowing for parallelism
- **Automatic Variance**: Inferred from type usage
- **Zero Runtime Overhead**: Types erased at compilation

### Standard Library

- **Collections**: Lists, vectors, maps with parallel operations
- **Iterators**: Rich combinator library for functional programming
- **Numerics**: Arbitrary precision integers, floating point
- **IO**: Capability-based effect management
- **Concurrency**: Channels, async/await primitives

## Getting Started

### Installation

```bash
# Clone the repository
git clone https://github.com/yourusername/parallax
cd parallax

# Build the compiler
cargo build --release

# Install to PATH
cargo install --path crates/parallax-cli
```

### Hello World

```parallax
// hello.px
const main = () -> () => println("Hello, parallel world!");
```

```bash
# Compile and run
plx run hello.px
```

### Parallel Example

```parallax
// sum_tree.px
type Tree = [T] => enum {
    Leaf(T),
    Node(Tree[T], Tree[T]),
};

const sum_tree = (tree: Tree[i32]) -> i32 => 
    match tree {
        Leaf(n) => n,
        Node(left, right) => {
            // Automatically parallelized!
            sum_tree(left) + sum_tree(right)
        }
    };

const main = () -> () => {
    let tree = build_tree(depth: 20);
    println(sum_tree(tree))
};
```

## Architecture

### Compilation Pipeline

```
Source → Parser → Type Checker → HIR → MIR → Code Generation → Native + Agents
                                                                    ↓
                                                            Runtime Scheduler
```

### Crate Structure

- `parallax-cli`: Command-line interface
- `parallax-syntax`: Parser and AST
- `parallax-types`: Type system and inference
- `parallax-codegen`: Native code generation
- `parallax-runtime`: Work-stealing scheduler
- `parallax-stdlib`: Standard library

See [AGENTS.md](AGENTS.md) for detailed architecture documentation.

## Performance

Parallax achieves:
- **Single-threaded**: Performance comparable to Rust/C++
- **Parallel**: Near-linear scaling up to available cores
- **Memory**: Efficient arena allocation for parallel tasks
- **Compilation**: Incremental compilation with Salsa

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

### Development Setup

```bash
# Install development tools
rustup install nightly
cargo install cargo-watch cargo-expand

# Run tests
cargo test --all

# Watch mode
cargo watch -x "check --all" -x "test --all"
```

## Documentation

- [Implementation Guide](docs/): Comprehensive mdBook documentation
- [AGENTS.md](AGENTS.md): Technical architecture guide
- [Design Documents](design/): Language design specifications
- [Agent Guides](agents/): LLM-oriented implementation guides
- [API Documentation](https://docs.rs/parallax): Generated rustdoc

### Building Documentation

```bash
# Install mdBook
cargo install mdbook

# Build the implementation guide
cd docs && mdbook build

# Serve locally with auto-reload
cd docs && mdbook serve
```

## Status

Parallax is currently in early development. Core features implemented:
- [ ] Basic parser and AST
- [ ] Type inference engine
- [ ] HIR/MIR representations
- [ ] Native code generation
- [ ] Runtime scheduler
- [ ] Standard library

See [ROADMAP.md](ROADMAP.md) for planned features.

## License

Parallax is licensed under the MIT License. See [LICENSE](LICENSE) for details.

## Acknowledgments

Parallax builds on ideas from:
- HVM/Bend for interaction net parallelism
- Rust for memory safety and type system
- Cilk for work-stealing scheduling
- Liquid Haskell for refinement types

Special thanks to the developers of these projects for inspiration.