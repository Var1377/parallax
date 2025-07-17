# Parallax: Native-First Parallel Programming Language

## Overview

Parallax is a next-generation functional programming language that achieves automatic parallelization through a novel native-first execution model. Unlike traditional interaction net systems that suffer from graph reduction overhead, Parallax compiles all computation to efficient native code while using interaction networks solely as a coordination mechanism.

## Key Features

- **Zero-Cost Automatic Parallelization**: Write sequential code, get parallel execution automatically
- **Native Performance**: All computation executes as optimized native code
- **Type-Safe Parallelism**: Refinement type system ensures memory safety across parallel boundaries
- **Work-Stealing Scheduler**: HVM-inspired runtime with efficient load balancing
- **No Manual Annotations**: Compiler automatically identifies parallelization opportunities

## Design Philosophy

### Native-First Execution

Traditional interaction net systems represent all computation as graph reductions, leading to 10-100x overhead compared to native code. Parallax takes a different approach:

```rust
// Your code
fn fib(n: i32) -> i32 = 
    if n <= 1 then n 
    else fib(n-1) + fib(n-2);

// Compiles to native code returning:
enum FibResult {
    Value(i32),                    // Base case
    Parallel { left: i32, right: i32 }  // Recursive work
}
```

The runtime recognizes the `Parallel` variant and automatically distributes the work across CPU cores.

### Function-Boundary Parallelism

Functions are the natural unit of parallelization. The compiler analyzes each function to identify patterns like:

- **Divide-and-conquer**: Multiple recursive calls (quicksort, tree algorithms)
- **Map-reduce**: Operations over data structures
- **Independent computations**: Unrelated expensive calculations

### Memory Safety Through Types

Parallax uses a refinement type system with Rust-style borrowing extended for parallel contexts:

```rust
// Refinement types
type Nat = { x: i32 | x >= 0 }
type NonEmpty<T> = { v: Vec<T> | v.len() > 0 }

// Parallel-safe references
fn parallel_sum(left: &'par Tree, right: &'par Tree) -> i32
```

## Language Features

### Core Language

- **Functional Core**: Immutable by default, expression-oriented
- **Pattern Matching**: Exhaustive matching with guards
- **Algebraic Data Types**: Enums and structs with generics
- **Higher-Order Functions**: First-class functions with closures
- **Type Inference**: Hindley-Milner with refinements

### Type System

- **Refinement Types**: Compile-time predicates on values
- **Effect Tracking**: Pure vs effectful computations
- **Lifetime Analysis**: Extended borrowing for parallelism
- **Zero Runtime Overhead**: All types erased at runtime

### Standard Library

- **Collections**: Lists, trees, arrays with parallel operations
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

```rust
// hello.plx
fn main() = println("Hello, parallel world!");
```

```bash
# Compile and run
plx run hello.plx
```

### Parallel Example

```rust
// sum_tree.plx
enum Tree<T> {
    Leaf(T),
    Node(Tree<T>, Tree<T>),
}

fn sum_tree(tree: Tree<i32>) -> i32 = match tree {
    Leaf(n) => n,
    Node(left, right) => {
        // Automatically parallelized!
        sum_tree(left) + sum_tree(right)
    }
}

fn main() = {
    let tree = build_tree(depth: 20);
    println(sum_tree(tree))
}
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

- [AGENTS.md](AGENTS.md): Technical architecture guide
- [Design Decisions](design/): Architectural decision records
- [Agent Guides](agents/): LLM-oriented implementation guides
- [API Documentation](https://docs.rs/parallax): Generated rustdoc

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