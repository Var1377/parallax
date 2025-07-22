# Insta Snapshot Testing Documentation

Comprehensive documentation for Insta v1.43 - A powerful snapshot testing library for Rust.

## Table of Contents

1. [Getting Started](./getting-started.md) - Quick introduction and basic usage
2. [Snapshot Formats](./snapshot-formats.md) - YAML, JSON, CSV, TOML, and more
3. [Review and Update Workflows](./review-workflows.md) - Managing snapshot changes
4. [CI/CD Integration](./ci-cd-integration.md) - Continuous integration best practices
5. [Redactions and Filters](./redactions-filters.md) - Dynamic content handling
6. [Custom Serialization](./custom-serialization.md) - Advanced serialization techniques
7. [Glob Patterns](./glob-patterns.md) - Testing multiple files efficiently
8. [Best Practices](./best-practices.md) - Patterns and recommendations
9. [API Reference](./api-reference.md) - Complete API documentation with examples

## What is Insta?

Insta is a snapshot testing library for Rust that allows you to test complex values against reference snapshots. Unlike simple assertions, snapshot tests:

- Handle complex data structures automatically
- Provide comprehensive diff tools for reviewing changes
- Store reference values in separate files for easy inspection
- Support multiple serialization formats
- Include powerful redaction and filtering capabilities

## Quick Example

```rust
use insta::assert_debug_snapshot;

#[test]
fn test_user_data() {
    let user = User {
        id: 42,
        name: "Alice".to_string(),
        email: "alice@example.com".to_string(),
    };
    
    assert_debug_snapshot!(user);
}
```

## Key Features

- **Multiple Formats**: YAML, JSON, CSV, TOML, RON support
- **Inline Snapshots**: Store snapshots directly in source code
- **Glob Testing**: Test multiple files with pattern matching
- **Redactions**: Hide sensitive or dynamic data
- **Filters**: Transform content before comparison
- **CI/CD Ready**: Environment variable controls for automation
- **Editor Support**: VSCode extension for snapshot management

## Installation

Add to your `Cargo.toml`:

```toml
[dev-dependencies]
insta = { version = "1.43", features = ["yaml", "json"] }

# Optional: cargo-insta for enhanced workflow
# Install with: cargo install cargo-insta
```

## Version Information

This documentation covers Insta version 1.43.1, released with the following key features:

- Enhanced glob pattern support
- Improved redaction system
- Better CI/CD integration
- Performance optimizations
- Extended serialization format support

For the latest updates and changes, see the [CHANGELOG](https://github.com/mitsuhiko/insta/blob/master/CHANGELOG.md).