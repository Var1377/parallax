# Logos 0.15 - Comprehensive Documentation

![Logos Logo](https://raw.githubusercontent.com/maciejhirsz/logos/master/logos.svg?sanitize=true)

**Create ridiculously fast Lexers.**

This comprehensive documentation covers Logos version 0.15, a high-performance lexer generator for Rust that creates blazingly fast tokenizers through compile-time optimization.

## Table of Contents

1. [Getting Started](./01-getting-started.md) - Basic setup and first lexer
2. [Token Definition and Patterns](./02-token-definition.md) - Defining tokens with literals and regex
3. [Performance Optimization](./03-performance-optimization.md) - Techniques for maximum speed
4. [Error Handling and Recovery](./04-error-handling.md) - Custom errors and recovery strategies
5. [Custom Lexer Logic](./05-custom-lexer-logic.md) - Callbacks and advanced features
6. [Parser Integration](./06-parser-integration.md) - Working with parser generators
7. [Advanced Regex Patterns](./07-advanced-regex.md) - Complex pattern matching
8. [Benchmarking and Profiling](./08-benchmarking.md) - Performance measurement
9. [API Reference](./09-api-reference.md) - Complete API documentation

## What is Logos?

**Logos** is a lexer generator that focuses on two primary goals:

1. **Ease of Use**: Make it simple to create lexers so you can focus on complex problems
2. **Performance**: Generate lexers faster than anything you'd write by hand

## Key Features

- **Compile-time Optimization**: All heavy lifting happens at compile time
- **Deterministic State Machine**: Combines all token definitions into a single DFA
- **Lookup Table Optimization**: Optimizes branches into lookup tables or jump tables
- **No Backtracking**: Prevents ReDoS attacks and ensures predictable performance
- **Loop Unrolling**: Unwinds loops and batches reads to minimize bounds checking
- **Zero-cost Abstractions**: Runtime overhead is minimal

## Performance Highlights

Logos generates extremely fast lexers:

```
test identifiers                       ... bench:         647 ns/iter (+/- 27) = 1204 MB/s
test keywords_operators_and_punctators ... bench:       2,054 ns/iter (+/- 78) = 1037 MB/s
test strings                           ... bench:         553 ns/iter (+/- 34) = 1575 MB/s
```

## Quick Example

```rust
use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")] // Ignore whitespace
enum Token {
    #[token("fast")]
    Fast,

    #[token(".")]
    Period,

    #[regex("[a-zA-Z]+")]
    Text,
}

fn main() {
    let mut lex = Token::lexer("Create ridiculously fast Lexers.");
    
    assert_eq!(lex.next(), Some(Ok(Token::Text)));
    assert_eq!(lex.slice(), "Create");
    
    assert_eq!(lex.next(), Some(Ok(Token::Text)));
    assert_eq!(lex.slice(), "ridiculously");
    
    assert_eq!(lex.next(), Some(Ok(Token::Fast)));
    assert_eq!(lex.slice(), "fast");
}
```

## Installation

Add Logos to your `Cargo.toml`:

```toml
[dependencies]
logos = "0.15.0"
```

## Version 0.15 Features

- **Improved Error Handling**: Better error types and recovery mechanisms
- **Enhanced Performance**: Further optimizations to the generated code
- **Better Regex Support**: More comprehensive regex pattern support
- **Context-dependent Lexing**: Support for lexer morphing and state switching
- **Debug Visualization**: Graph export for debugging lexer state machines
- **Custom Source Types**: Support for lexing from different source types

## License

This code is distributed under the terms of both the MIT license and the Apache License (Version 2.0).

---

*Documentation generated for Logos 0.15.0*