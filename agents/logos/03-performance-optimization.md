# Performance Optimization

Logos is designed for maximum performance, but there are techniques to optimize your lexer even further. This chapter covers performance best practices and optimization strategies.

## Understanding Logos Performance

### How Logos Optimizes

Logos achieves high performance through several techniques:

1. **Compile-time State Machine Generation**: Creates a deterministic finite automaton (DFA)
2. **Lookup Table Optimization**: Converts branches into fast lookup tables
3. **Jump Table Generation**: Uses jump tables for efficient state transitions
4. **Loop Unrolling**: Unwinds loops and batches reads to minimize bounds checking
5. **No Backtracking**: Prevents ReDoS attacks and ensures predictable performance

### Performance Characteristics

```
Benchmark Results (Logos 0.15):
test identifiers                       ... bench:         647 ns/iter (+/- 27) = 1204 MB/s
test keywords_operators_and_punctators ... bench:       2,054 ns/iter (+/- 78) = 1037 MB/s
test strings                           ... bench:         553 ns/iter (+/- 34) = 1575 MB/s
```

## Optimization Techniques

### 1. Pattern Design

#### Prefer Literals Over Regex

Literal tokens are faster than regex patterns:

```rust
// ✅ Fast - literal matching
#[derive(Logos, Debug, PartialEq)]
enum FastToken {
    #[token("if")]
    If,
    
    #[token("else")]
    Else,
    
    #[token("while")]
    While,
}

// ❌ Slower - regex for simple literals
enum SlowToken {
    #[regex("if")]
    If,
    
    #[regex("else")]
    Else,
    
    #[regex("while")]
    While,
}
```

#### Optimize Character Classes

Use specific character ranges when possible:

```rust
#[derive(Logos, Debug, PartialEq)]
enum Token {
    // ✅ Fast - specific ranges
    #[regex("[a-zA-Z][a-zA-Z0-9]*")]
    Identifier,
    
    // ❌ Slower - broad Unicode classes
    #[regex(r"[\p{L}][\p{L}\p{N}]*")]
    UnicodeIdentifier,
    
    // ✅ Fast - ASCII digits
    #[regex("[0-9]+")]
    Number,
    
    // ❌ Slower - Unicode digits
    #[regex(r"[\p{N}]+")]
    UnicodeNumber,
}
```

#### Minimize Alternations

Reduce complex alternations in regex patterns:

```rust
// ✅ Fast - separate tokens
#[derive(Logos, Debug, PartialEq)]
enum FastToken {
    #[token("and")]
    And,
    
    #[token("or")]
    Or,
    
    #[token("not")]
    Not,
}

// ❌ Slower - complex alternation
enum SlowToken {
    #[regex("and|or|not")]
    Keyword,
}
```

### 2. Skip Pattern Optimization

#### Use Global Skip Patterns

Global skip patterns are more efficient than token-specific skipping:

```rust
// ✅ Fast - global skip
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(skip r"//[^\n]*")]
enum FastToken {
    #[token("fn")]
    Function,
    
    #[regex("[a-zA-Z]+")]
    Identifier,
}

// ❌ Slower - token-specific skip
enum SlowToken {
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Whitespace,
    
    #[regex(r"//[^\n]*", logos::skip)]
    Comment,
    
    #[token("fn")]
    Function,
    
    #[regex("[a-zA-Z]+")]
    Identifier,
}
```

#### Optimize Skip Patterns

Make skip patterns as specific as possible:

```rust
#[derive(Logos, Debug, PartialEq)]
// ✅ Fast - specific whitespace
#[logos(skip r"[ \t\n\r]+")]
// ❌ Slower - generic whitespace
// #[logos(skip r"\s+")]
enum Token {
    #[token("hello")]
    Hello,
}
```

### 3. Callback Optimization

#### Minimize Callback Complexity

Keep callbacks simple and fast:

```rust
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    // ✅ Fast - simple parsing
    #[regex("[0-9]+", |lex| lex.slice().parse::<u32>().unwrap())]
    Number(u32),
    
    // ❌ Slower - complex processing
    #[regex("[0-9]+", |lex| {
        let s = lex.slice();
        let mut result = 0u32;
        for c in s.chars() {
            result = result * 10 + (c as u32 - '0' as u32);
        }
        result
    })]
    SlowNumber(u32),
}
```

#### Use `unwrap_or_default()` for Error Handling

When appropriate, use default values instead of complex error handling:

```rust
#[derive(Logos, Debug, PartialEq)]
enum Token {
    // ✅ Fast - default on error
    #[regex("[0-9]+", |lex| lex.slice().parse::<u32>().unwrap_or_default())]
    Number(u32),
    
    // ❌ Slower - complex error handling
    #[regex("[0-9]+", |lex| {
        match lex.slice().parse::<u32>() {
            Ok(n) => n,
            Err(_) => {
                eprintln!("Parse error: {}", lex.slice());
                0
            }
        }
    })]
    SlowNumber(u32),
}
```

### 4. Memory Layout Optimization

#### Use Smaller Token Types

Smaller token enums are more cache-friendly:

```rust
// ✅ Fast - compact tokens
#[derive(Logos, Debug, PartialEq)]
enum CompactToken {
    Keyword(u8),    // Use indices instead of strings
    Number(u32),    // Use appropriate size
    Operator(u8),
}

// ❌ Slower - large tokens
enum LargeToken {
    Keyword(String),    // Heap allocation
    Number(u64),        // Unnecessarily large
    Operator(String),   // Heap allocation
}
```

#### Minimize Token Variants

Fewer token variants can improve performance:

```rust
// ✅ Fast - fewer variants
#[derive(Logos, Debug, PartialEq)]
enum FastToken {
    Keyword(KeywordType),
    Operator(OperatorType),
    Literal(LiteralType),
    Identifier,
}

#[derive(Debug, PartialEq)]
enum KeywordType { If, Else, While, For }

// ❌ Slower - many variants
enum SlowToken {
    If, Else, While, For, Let, Fn, Return, Break, Continue,
    Plus, Minus, Multiply, Divide, Assign, Equal, NotEqual,
    // ... many more variants
}
```

### 5. Source Type Optimization

#### Choose Appropriate Source Types

Use the most appropriate source type for your use case:

```rust
// For UTF-8 text
#[derive(Logos, Debug, PartialEq)]
enum StringToken {
    #[regex("[a-zA-Z]+")]
    Word,
}

// For binary data or when you need maximum performance
#[derive(Logos, Debug, PartialEq)]
enum ByteToken {
    #[regex(b"[a-zA-Z]+")]
    Word,
}

fn example() {
    // String lexing
    let text = "hello world";
    let mut string_lexer = StringToken::lexer(text);
    
    // Byte lexing (potentially faster for ASCII-only content)
    let bytes = b"hello world";
    let mut byte_lexer = ByteToken::lexer(bytes);
}
```

## Benchmarking Your Lexer

### Setting Up Benchmarks

Use Criterion for accurate benchmarking:

```toml
[dev-dependencies]
criterion = { version = "0.5", features = ["html_reports"] }

[[bench]]
name = "lexer_bench"
harness = false
```

```rust
// benches/lexer_bench.rs
use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use your_crate::{Token, Logos};

static SAMPLE_CODE: &str = include_str!("../test_data/sample.txt");

fn bench_lexer(c: &mut Criterion) {
    let mut group = c.benchmark_group("lexer");
    group.throughput(Throughput::Bytes(SAMPLE_CODE.len() as u64));
    
    group.bench_function("tokenize", |b| {
        b.iter(|| {
            let mut lexer = Token::lexer(black_box(SAMPLE_CODE));
            while let Some(token) = lexer.next() {
                black_box(token);
            }
        })
    });
    
    group.bench_function("count_tokens", |b| {
        b.iter(|| {
            Token::lexer(black_box(SAMPLE_CODE))
                .filter_map(|t| t.ok())
                .count()
        })
    });
}

criterion_group!(benches, bench_lexer);
criterion_main!(benches);
```

### Profiling with `perf`

Use `perf` for detailed performance analysis:

```bash
# Build with debug info
cargo build --release --bin your_lexer

# Profile with perf
perf record --call-graph=dwarf ./target/release/your_lexer input.txt
perf report

# Generate flamegraph
cargo install flamegraph
cargo flamegraph --bin your_lexer -- input.txt
```

### Memory Profiling

Use `heaptrack` or `valgrind` for memory analysis:

```bash
# Install heaptrack
sudo apt install heaptrack

# Profile memory usage
heaptrack ./target/release/your_lexer input.txt
heaptrack_gui heaptrack.your_lexer.*.gz
```

## Performance Comparison

### Logos vs Other Lexers

Comparative benchmarks (approximate):

| Lexer | Speed (MB/s) | Memory | Compile Time |
|-------|--------------|--------|--------------|
| Logos | 1000-1500 | Low | Fast |
| regex | 200-400 | Medium | Fast |
| nom | 300-600 | Low | Medium |
| pest | 100-300 | Medium | Slow |
| Hand-written | 800-1200 | Low | N/A |

### Real-world Performance

Example performance for different token types:

```rust
// Benchmark results for different patterns
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum BenchToken {
    // ~1500 MB/s - simple literals
    #[token("if")]
    If,
    
    // ~1200 MB/s - identifiers
    #[regex("[a-zA-Z][a-zA-Z0-9]*")]
    Identifier,
    
    // ~1000 MB/s - numbers with parsing
    #[regex("[0-9]+", |lex| lex.slice().parse::<u32>().unwrap())]
    Number(u32),
    
    // ~800 MB/s - complex strings
    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        let s = lex.slice();
        s[1..s.len()-1].to_string()
    })]
    String(String),
}
```

## Debugging Performance Issues

### Enable Debug Features

Use Logos debug features to understand the generated state machine:

```toml
[dependencies]
logos = { version = "0.15", features = ["debug"] }
```

```rust
#[derive(Logos, Debug, PartialEq)]
#[logos(export_dir = "debug_output")]  // Export state machine graphs
enum Token {
    #[token("hello")]
    Hello,
    
    #[regex("[a-zA-Z]+")]
    Word,
}
```

### Analyze Generated Code

Examine the generated code to identify bottlenecks:

```bash
# Expand macros to see generated code
cargo expand --bin your_lexer > expanded.rs

# Look for inefficient patterns in the generated code
grep -n "match" expanded.rs
```

### Common Performance Issues

1. **Complex Regex Patterns**: Simplify or split complex patterns
2. **Excessive Backtracking**: Ensure patterns don't cause backtracking
3. **Large Token Enums**: Reduce the number of variants
4. **Expensive Callbacks**: Optimize callback functions
5. **Inefficient Skip Patterns**: Use global skip patterns

## Best Practices Summary

1. **Use literals over regex** when possible
2. **Optimize character classes** for your specific use case
3. **Minimize callback complexity** and avoid heap allocations
4. **Use global skip patterns** instead of token-specific skipping
5. **Choose appropriate source types** (string vs bytes)
6. **Profile regularly** to identify bottlenecks
7. **Keep token enums compact** for better cache performance
8. **Test with realistic data** that matches your use case

## Advanced Optimization Techniques

### Custom Source Types

Implement custom source types for specialized use cases:

```rust
use logos::Source;

// Custom source for memory-mapped files
struct MmapSource {
    data: &'static [u8],
}

impl Source for MmapSource {
    type Slice = &'static [u8];
    
    fn len(&self) -> usize {
        self.data.len()
    }
    
    fn read<T>(&self, offset: usize) -> Option<T>
    where
        T: logos::source::Chunk<Self::Slice>,
    {
        T::read(self.data, offset)
    }
    
    unsafe fn read_unchecked<T>(&self, offset: usize) -> T
    where
        T: logos::source::Chunk<Self::Slice>,
    {
        T::read_unchecked(self.data, offset)
    }
    
    fn slice(&self, range: core::ops::Range<usize>) -> Option<Self::Slice> {
        self.data.get(range)
    }
    
    unsafe fn slice_unchecked(&self, range: core::ops::Range<usize>) -> Self::Slice {
        self.data.get_unchecked(range)
    }
}
```

### SIMD Optimization

For extremely performance-critical applications, consider SIMD optimizations in callbacks:

```rust
#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::*;

#[derive(Logos, Debug, PartialEq)]
enum Token {
    #[regex("[0-9]+", simd_parse_number)]
    Number(u32),
}

#[cfg(target_arch = "x86_64")]
fn simd_parse_number(lex: &mut logos::Lexer<Token>) -> Option<u32> {
    // SIMD-optimized number parsing for very long numbers
    // Implementation would use SIMD instructions for parsing
    lex.slice().parse().ok()
}

#[cfg(not(target_arch = "x86_64"))]
fn simd_parse_number(lex: &mut logos::Lexer<Token>) -> Option<u32> {
    lex.slice().parse().ok()
}
```

## Next Steps

- [Error Handling](./04-error-handling.md) - Learn about efficient error handling
- [Benchmarking and Profiling](./08-benchmarking.md) - Deep dive into performance measurement
- [Advanced Regex Patterns](./07-advanced-regex.md) - Optimize complex patterns