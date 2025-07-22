# Ariadne v0.5 Documentation Index

Welcome to the comprehensive documentation for Ariadne v0.5, a powerful Rust crate for creating beautiful compiler diagnostics and error reports.

## Documentation Structure

### 📚 [Main Documentation](README.md)
The complete guide covering all aspects of Ariadne v0.5:
- Getting started with error reporting
- Creating beautiful error messages
- Label system and spans
- Color customization
- Multi-file error reporting
- Language server integration
- Custom report builders
- Performance considerations
- Complete API reference

### 💡 [Examples Collection](examples.md)
Practical examples for different use cases:
- Basic error reporting examples
- Compiler error patterns
- Multi-file diagnostics
- Advanced formatting techniques
- Language server integration examples
- Performance optimization examples

### ⚡ [Quick Reference](quick-reference.md)
Concise reference for common operations:
- Basic usage patterns
- Configuration options
- Color and formatting
- Source management
- Performance tips
- Testing strategies

### 🎯 [Best Practices Guide](best-practices.md)
Production-ready guidelines:
- Error message design principles
- Performance optimization strategies
- Code organization patterns
- Testing approaches
- Accessibility considerations
- Common pitfalls to avoid

## Key Features of Ariadne v0.5

### ✨ Beautiful Error Messages
- Professional-quality diagnostic output
- Rich color support (8-bit and 24-bit)
- Unicode and ASCII character sets
- Customizable themes and styling

### 🎨 Advanced Label System
- Multi-line span support
- Priority-based label ordering
- Flexible attachment points
- Cross-file references

### 🚀 Performance Optimized
- Efficient source caching
- Streaming output support
- Memory-conscious design
- Batch processing capabilities

### 🔧 Developer Friendly
- Type-safe span handling
- Flexible cache implementations
- LSP integration support
- Comprehensive testing utilities

## Quick Start

Add Ariadne to your project:

```toml
[dependencies]
ariadne = "0.5"

# Optional: Enable automatic color detection
ariadne = { version = "0.5", features = ["auto-color"] }
```

Create your first error report:

```rust
use ariadne::{Color, Label, Report, ReportKind, Source};

fn main() {
    let source = "let x = 42 + \"hello\";";
    
    Report::build(ReportKind::Error, 12..19)
        .with_code("E0308")
        .with_message("Type mismatch")
        .with_label(
            Label::new(8..10)
                .with_message("This is a number")
                .with_color(Color::Blue)
        )
        .with_label(
            Label::new(13..20)
                .with_message("This is a string")
                .with_color(Color::Red)
        )
        .with_note("Cannot add number and string")
        .with_help("Consider converting types")
        .finish()
        .print(Source::from(source))
        .unwrap();
}
```

## Use Cases

### Compiler Development
- Syntax error reporting
- Type checking diagnostics
- Semantic analysis warnings
- Code generation errors

### Language Servers
- Real-time diagnostics
- IDE integration
- Code action suggestions
- Hover information

### Static Analysis Tools
- Linting warnings
- Security vulnerability reports
- Code quality metrics
- Performance suggestions

### Build Systems
- Compilation error aggregation
- Multi-project diagnostics
- CI/CD integration
- Error filtering and formatting

## Version Information

This documentation covers **Ariadne v0.5.1**, which includes:

- Enhanced color generation with const-friendly `ColorGenerator`
- Improved Unicode width handling (v0.2.0)
- Multiple and multiline help support
- RPITIT (Return Position Impl Trait In Traits) optimizations
- Updated dependency compatibility

## Community and Support

- **Repository**: [github.com/zesterer/ariadne](https://github.com/zesterer/ariadne)
- **Documentation**: [docs.rs/ariadne](https://docs.rs/ariadne)
- **Crate**: [crates.io/crates/ariadne](https://crates.io/crates/ariadne)
- **License**: MIT

## Related Projects

- **[chumsky](https://github.com/zesterer/chumsky)**: Parser combinator library that pairs well with Ariadne
- **[codespan](https://github.com/brendanzab/codespan)**: Alternative diagnostic library that inspired Ariadne
- **[yansi](https://github.com/SergioBenitez/yansi)**: Color formatting library used by Ariadne

---

*This documentation was generated for the Parallax compiler project. For the latest updates and official documentation, visit the Ariadne repository.*