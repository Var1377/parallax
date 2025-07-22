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

## Required Reading Before Implementation

**CRITICAL**: Before implementing any component, agents MUST read the relevant documentation in the `agents/` directory. This documentation contains essential patterns, best practices, and API details for each library.

### Mandatory Prerequisites by Component:

**Parser Implementation**:
- Read `agents/logos/` - Lexer generation patterns
- Read `agents/rowan/` - Syntax tree architecture
- Read `agents/parsing-patterns/` - Integration strategies
- Read `agents/error-handling/` - Error recovery patterns

**Incremental Compilation**:
- Read `agents/salsa/` - Query-based compilation framework
- Read `agents/performance-patterns/` - Memory optimization

**Error Reporting**:
- Read `agents/ariadne/` - Diagnostic rendering
- Read `agents/error-handling/` - Error type design

**Parallelism & Concurrency**:
- Read `agents/rayon/` - Data parallelism patterns
- Read `agents/dashmap/` - Concurrent data structures

**Testing**:
- Read `agents/testing-patterns/` - Property-based testing
- Read `agents/insta/` - Snapshot testing
- Read `agents/criterion/` - Performance benchmarking

**Performance Optimization**:
- Read `agents/performance-patterns/` - Arena allocation, small vectors
- Read `agents/criterion/` - Benchmarking methodology

### Documentation Structure

Each library documentation in `agents/{library}/` contains:
- Getting started guide
- Core concepts and architecture
- API reference with examples
- Best practices and patterns
- Performance considerations
- Integration examples

**Note**: The Salsa documentation is based on source code analysis as the official docs are outdated for v0.23.

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

### Unit Testing
- **Colocated Tests**: Place unit tests in `#[cfg(test)]` modules within source files
- **Test Organization**: Group related tests in nested modules
- **Naming Convention**: `test_<functionality>_<scenario>` (e.g., `test_parser_recovers_from_missing_semicolon`)

### Integration Testing
- **Location**: `tests/` directory at workspace root
- **File Naming**: `<feature>_test.rs` (e.g., `incremental_compilation_test.rs`)
- **Test Data**: Store test fixtures in `tests/fixtures/`

### Snapshot Testing
- **Parser Output**: Use `insta` for AST snapshots
- **Error Messages**: Use `expect-test` for inline error message testing
- **Update Snapshots**: `cargo insta review` for reviewing changes

### Property-Based Testing
- **Parser Invariants**: Use `proptest` to verify parser never panics
- **Type System**: Verify type safety properties hold
- **Optimization**: Ensure optimizations preserve semantics

### Test Execution
```bash
cargo test                    # Run all tests
cargo test -p parallax-parser # Test specific crate
cargo test -- --nocapture    # See println! output
cargo test -- --test-threads=1 # Sequential execution for debugging
```

## Benchmarking

### Micro-benchmarks with Criterion
```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn parse_benchmark(c: &mut Criterion) {
    let source = include_str!("../benches/fixtures/large_file.px");
    
    c.bench_function("parse_large_file", |b| {
        b.iter(|| {
            let parsed = parse(black_box(source));
            black_box(parsed);
        })
    });
}

criterion_group!(benches, parse_benchmark);
criterion_main!(benches);
```

### Benchmark Organization
- **Location**: `benches/` directory in each crate
- **Fixtures**: Real-world code samples in `benches/fixtures/`
- **Baselines**: Store baseline results in git for regression detection

### Performance Profiling
```bash
# CPU profiling with perf
cargo build --release
perf record --call-graph=dwarf target/release/parallax compile large.px
perf report

# Generate flamegraphs
cargo install flamegraph
cargo flamegraph --bench parser_bench

# Memory profiling with heaptrack
heaptrack target/release/parallax compile large.px
heaptrack_gui heaptrack.parallax.*.gz
```

### Continuous Benchmarking
- Run benchmarks in CI on every PR
- Compare against baseline from main branch
- Fail PR if regression > 5%
- Use `criterion`'s built-in regression detection

## Error Handling

### Error Types Philosophy
1. **Library Errors**: Use custom error types with `thiserror`
2. **Application Errors**: Use `anyhow` for error propagation
3. **Compiler Errors**: Rich diagnostic types with location info

### Error Design Patterns
```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CompileError {
    #[error("syntax error: {message}")]
    Syntax {
        message: String,
        location: SourceLocation,
        help: Option<String>,
    },
    
    #[error("type mismatch: expected {expected}, found {found}")]
    TypeMismatch {
        expected: String,
        found: String,
        location: SourceLocation,
    },
    
    #[error(transparent)]
    Io(#[from] std::io::Error),
}
```

### Error Recovery Principles
1. **Parse Recovery**: Continue parsing after errors to find more issues
2. **Type Recovery**: Use placeholder types to continue type checking
3. **Limit Cascading**: Track error origins to avoid duplicate reports
4. **Recovery Points**: Synchronize at statement/block boundaries

### Error Collection
```rust
pub struct ErrorCollector {
    errors: Vec<Diagnostic>,
    error_limit: usize,
    has_fatal_error: bool,
}

impl ErrorCollector {
    pub fn report(&mut self, error: Diagnostic) {
        if self.errors.len() < self.error_limit && !self.has_fatal_error {
            self.errors.push(error);
        }
    }
    
    pub fn report_fatal(&mut self, error: Diagnostic) {
        self.errors.push(error);
        self.has_fatal_error = true;
    }
}
```

## Error Reporting

### Diagnostic Structure
```rust
use ariadne::{Report, ReportKind, Label, Source};

pub struct Diagnostic {
    pub level: DiagnosticLevel,
    pub code: Option<String>,
    pub message: String,
    pub labels: Vec<DiagnosticLabel>,
    pub notes: Vec<String>,
    pub help: Option<String>,
}

pub fn render_diagnostic(diag: &Diagnostic, source: &Source) -> Report {
    let mut report = Report::build(diag.level.into(), "file.px", 0)
        .with_message(&diag.message);
    
    if let Some(code) = &diag.code {
        report = report.with_code(code);
    }
    
    for label in &diag.labels {
        report = report.with_label(
            Label::new(("file.px", label.span.start..label.span.end))
                .with_message(&label.message)
                .with_color(label.color)
        );
    }
    
    report.finish()
}
```

### Error Message Guidelines
1. **Be Specific**: "expected `;` after expression" not "syntax error"
2. **Show Context**: Include relevant code snippets with highlighting
3. **Suggest Fixes**: "did you mean `==` instead of `=`?"
4. **Educational**: Explain why something is wrong, not just that it's wrong
5. **Consistent Style**: Use consistent terminology and formatting

### Error Rendering Example
```
error[E0308]: mismatched types
  --> src/main.px:5:9
   |
 4 |     let x: i32 = "hello";
   |            ---   ^^^^^^^ expected `i32`, found `&str`
   |            |
   |            expected due to this type annotation
   |
   = help: try parsing the string: `"hello".parse::<i32>()?`
```

### LSP Integration
```rust
pub fn diagnostic_to_lsp(diag: &Diagnostic) -> lsp_types::Diagnostic {
    lsp_types::Diagnostic {
        range: span_to_range(diag.primary_span()),
        severity: Some(match diag.level {
            DiagnosticLevel::Error => DiagnosticSeverity::ERROR,
            DiagnosticLevel::Warning => DiagnosticSeverity::WARNING,
            DiagnosticLevel::Info => DiagnosticSeverity::INFORMATION,
        }),
        code: diag.code.clone().map(NumberOrString::String),
        source: Some("parallax".to_string()),
        message: diag.message.clone(),
        related_information: Some(diag.labels.iter().map(to_lsp_related).collect()),
        ..Default::default()
    }
}
```

### Testing Error Messages
```rust
#[test]
fn test_type_error_message() {
    let source = r#"let x: i32 = "hello";"#;
    let errors = compile(source).unwrap_err();
    
    expect_test::expect![[r#"
        error[E0308]: mismatched types
          --> src/main.px:1:14
           |
         1 | let x: i32 = "hello";
           |        ---   ^^^^^^^ expected `i32`, found `&str`
           |        |
           |        expected due to this type annotation
    "#]]
    .assert_eq(&format_errors(&errors));
}
```

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