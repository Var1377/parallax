# Error Handling in Rust: A Comprehensive Guide

This directory contains comprehensive documentation for error handling patterns in Rust, focusing on the `thiserror` (2.0) and `anyhow` (1.0) libraries. These are the de facto standard libraries for error handling in the Rust ecosystem.

## Table of Contents

1. [Error Handling Philosophy](#error-handling-philosophy)
2. [Library vs Application Errors](#library-vs-application-errors)
3. [Quick Reference](#quick-reference)
4. [Documentation Structure](#documentation-structure)

## Error Handling Philosophy

### The Rust Error Handling Model

Rust's error handling is built around the principle of **explicit error handling** without exceptions. This approach provides:

- **Compile-time guarantees**: All error paths must be handled
- **Zero-cost abstractions**: No runtime overhead for error handling
- **Composability**: Errors can be easily transformed and propagated
- **Transparency**: Error types are part of function signatures

### Core Principles

#### 1. **Fail Fast, Fail Explicitly**
```rust
// Good: Explicit error handling
fn parse_config(path: &str) -> Result<Config, ConfigError> {
    let content = std::fs::read_to_string(path)?;
    serde_json::from_str(&content).map_err(ConfigError::ParseError)
}

// Bad: Panicking on errors (except for unrecoverable situations)
fn parse_config_bad(path: &str) -> Config {
    let content = std::fs::read_to_string(path).unwrap();
    serde_json::from_str(&content).unwrap()
}
```

#### 2. **Error Types Should Be Informative**
Errors should provide enough context for debugging and user-facing messages:

```rust
#[derive(Error, Debug)]
pub enum ConfigError {
    #[error("Configuration file not found at {path}")]
    FileNotFound { path: String },
    
    #[error("Invalid configuration format")]
    ParseError(#[from] serde_json::Error),
    
    #[error("Missing required field: {field}")]
    MissingField { field: String },
}
```

#### 3. **Distinguish Between Recoverable and Unrecoverable Errors**
- **Recoverable**: Use `Result<T, E>` for errors that callers can handle
- **Unrecoverable**: Use `panic!` for programming errors and invariant violations

#### 4. **Error Propagation Should Be Ergonomic**
The `?` operator makes error propagation seamless:

```rust
fn process_data() -> Result<ProcessedData, Error> {
    let raw_data = fetch_data()?;           // Propagates fetch errors
    let validated = validate_data(raw_data)?; // Propagates validation errors
    let processed = transform_data(validated)?; // Propagates transformation errors
    Ok(processed)
}
```

## Library vs Application Errors

### When to Use `thiserror`

**Use `thiserror` for libraries** when you want to:
- Design specific error types that provide precise information
- Give callers control over error handling
- Create a stable public API
- Provide structured error information

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum DatabaseError {
    #[error("Connection failed: {reason}")]
    ConnectionFailed { reason: String },
    
    #[error("Query timeout after {seconds}s")]
    Timeout { seconds: u64 },
    
    #[error("Invalid query: {query}")]
    InvalidQuery { query: String },
    
    #[error(transparent)]
    Io(#[from] std::io::Error),
}
```

### When to Use `anyhow`

**Use `anyhow` for applications** when you want to:
- Focus on rapid development over precise error types
- Easily propagate any error type
- Add context to errors without defining custom types
- Simplify error handling in application code

```rust
use anyhow::{Context, Result};

fn main() -> Result<()> {
    let config = load_config()
        .context("Failed to load application configuration")?;
    
    let database = connect_to_database(&config.db_url)
        .context("Failed to connect to database")?;
    
    run_application(config, database)
        .context("Application execution failed")?;
    
    Ok(())
}
```

## Quick Reference

### `thiserror` Quick Start

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum MyError {
    #[error("IO error")]
    Io(#[from] std::io::Error),
    
    #[error("Parse error: {message}")]
    Parse { message: String },
    
    #[error("Invalid value: {value} (expected {expected})")]
    InvalidValue { value: String, expected: String },
    
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}
```

### `anyhow` Quick Start

```rust
use anyhow::{anyhow, bail, Context, Result};

fn example() -> Result<()> {
    // Create ad-hoc errors
    let error = anyhow!("Something went wrong: {}", reason);
    
    // Early return with error
    if condition {
        bail!("Invalid condition: {}", condition);
    }
    
    // Add context to existing errors
    some_operation()
        .context("Failed to perform operation")?;
    
    // Add dynamic context
    some_operation()
        .with_context(|| format!("Failed with input: {}", input))?;
    
    Ok(())
}
```

### Error Propagation Patterns

```rust
// Basic propagation
fn basic_propagation() -> Result<String, std::io::Error> {
    let content = std::fs::read_to_string("file.txt")?;
    Ok(content.trim().to_string())
}

// With transformation
fn with_transformation() -> Result<String, MyError> {
    let content = std::fs::read_to_string("file.txt")
        .map_err(MyError::Io)?;
    Ok(content.trim().to_string())
}

// With context (anyhow)
fn with_context() -> anyhow::Result<String> {
    let content = std::fs::read_to_string("file.txt")
        .context("Failed to read configuration file")?;
    Ok(content.trim().to_string())
}
```

## Documentation Structure

This directory contains the following documentation files:

- **[README.md](README.md)** - This overview and philosophy guide
- **[thiserror-guide.md](thiserror-guide.md)** - Complete guide to using `thiserror` for library errors
- **[anyhow-guide.md](anyhow-guide.md)** - Complete guide to using `anyhow` for application errors
- **[error-propagation.md](error-propagation.md)** - Patterns for error propagation and transformation
- **[custom-error-types.md](custom-error-types.md)** - Designing effective custom error types
- **[context-and-backtrace.md](context-and-backtrace.md)** - Working with error context and backtraces
- **[integration-patterns.md](integration-patterns.md)** - Integrating `thiserror` and `anyhow`
- **[compiler-errors.md](compiler-errors.md)** - Best practices for compiler error handling
- **[api-reference.md](api-reference.md)** - Complete API reference with examples

## Best Practices Summary

### For Libraries (use `thiserror`)
1. Define specific error types for your domain
2. Use `#[from]` for automatic conversions
3. Provide meaningful error messages with context
4. Use `#[source]` to chain errors appropriately
5. Consider using `#[error(transparent)]` for pass-through errors

### For Applications (use `anyhow`)
1. Use `anyhow::Result<T>` as your default return type
2. Add context with `.context()` and `.with_context()`
3. Use `anyhow!` and `bail!` for ad-hoc errors
4. Enable backtraces in development with `RUST_BACKTRACE=1`
5. Consider downcasting for specific error handling

### General Guidelines
1. Prefer `Result<T, E>` over panicking
2. Use the `?` operator for ergonomic error propagation
3. Provide helpful error messages for end users
4. Include relevant context in error messages
5. Test error paths as thoroughly as success paths
6. Document error conditions in your API

---

*This documentation is based on analysis of `thiserror` 2.0.12 and `anyhow` 1.0.98.*