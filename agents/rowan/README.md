# Rowan 0.16 - Comprehensive Documentation

Rowan is a library for lossless syntax trees, inspired by Swift's libsyntax. This documentation covers version 0.16 and provides comprehensive guidance for using Rowan in your projects.

## Table of Contents

1. [Getting Started](./01-getting-started.md) - Introduction to syntax trees and basic concepts
2. [Green and Red Tree Architecture](./02-architecture.md) - Understanding the dual-tree design
3. [Building Syntax Trees](./03-building-trees.md) - Creating trees with GreenNodeBuilder
4. [Tree Traversal and Manipulation](./04-traversal.md) - Working with syntax trees
5. [Parser Integration](./05-parser-integration.md) - Integrating Rowan with parsers
6. [Incremental Parsing](./06-incremental-parsing.md) - Strategies for efficient re-parsing
7. [Memory Efficiency](./07-memory-efficiency.md) - Optimization techniques
8. [Rust-Analyzer Patterns](./08-rust-analyzer-patterns.md) - Real-world usage patterns
9. [API Reference](./09-api-reference.md) - Complete API documentation with examples

## Quick Start

```rust
use rowan::{GreenNodeBuilder, Language, SyntaxNode};

// Define your language
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum SyntaxKind {
    ROOT = 0,
    IDENT,
    WHITESPACE,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Lang {}

impl Language for Lang {
    type Kind = SyntaxKind;
    
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        unsafe { std::mem::transmute(raw.0 as u8) }
    }
    
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind as u16)
    }
}

// Build a tree
let mut builder = GreenNodeBuilder::new();
builder.start_node(SyntaxKind::ROOT.into());
builder.token(SyntaxKind::IDENT.into(), "hello");
builder.finish_node();

let green = builder.finish();
let syntax: SyntaxNode<Lang> = SyntaxNode::new_root(green);
```

## Key Features

- **Lossless**: Preserves all source text including whitespace and comments
- **Immutable**: Trees are immutable and can be shared safely
- **Memory Efficient**: Uses structural sharing and compact representations
- **Incremental**: Supports efficient incremental parsing
- **Generic**: Works with any language through the `Language` trait

## Version Information

This documentation covers Rowan version 0.16.0, which includes:
- Rust edition 2021
- Minimum Rust version 1.77.0
- Enhanced memory efficiency
- Improved API ergonomics
- Better integration with rust-analyzer patterns

## License

Rowan is dual-licensed under MIT OR Apache-2.0.