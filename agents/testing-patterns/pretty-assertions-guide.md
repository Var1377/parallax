# Pretty Assertions Complete Guide

Pretty Assertions is a drop-in replacement for Rust's standard assertion macros that provides colorful, easy-to-read diff output when tests fail. This guide covers installation, usage patterns, and integration strategies.

## Table of Contents

- [What is Pretty Assertions?](#what-is-pretty-assertions)
- [Installation and Setup](#installation-and-setup)
- [Basic Usage](#basic-usage)
- [Advanced Features](#advanced-features)
- [Integration Patterns](#integration-patterns)
- [Customization](#customization)
- [Performance Considerations](#performance-considerations)
- [Best Practices](#best-practices)
- [Troubleshooting](#troubleshooting)

## What is Pretty Assertions?

Pretty Assertions enhances Rust's testing experience by providing:

1. **Colorful Diffs**: Visual highlighting of differences between expected and actual values
2. **Better Formatting**: Multi-line output for complex data structures
3. **Zero Runtime Cost**: Only affects test compilation, not production code
4. **Drop-in Replacement**: Simply replace import statements
5. **Cross-Platform**: Works on all platforms with proper terminal support

### Before and After Comparison

**Standard assertions:**
```
assertion failed: `(left == right)`
  left: `Foo { lorem: "Hello World!", ipsum: 42, dolor: Ok("hey") }`,
 right: `Foo { lorem: "Hello Wrold!", ipsum: 42, dolor: Ok("hey ho!") }`
```

**Pretty assertions:**
```
assertion failed: `(left == right)`

[1m[38;5;1mDiff[0m [1m[38;5;1m< left / right >[0m [1m[38;5;1m:[0m
 Foo {
     lorem: [1m[48;5;1;38;5;15m"Hello W[48;5;9;38;5;16mo[48;5;1;38;5;15mrld!"[0m,
     ipsum: 42,
     dolor: Ok(
         [1m[48;5;1;38;5;15m"hey[48;5;9;38;5;16m ho![48;5;1;38;5;15m"[0m,
     ),
 }
```

## Installation and Setup

### Cargo.toml Configuration

```toml
[dev-dependencies]
pretty_assertions = "1.4.1"
```

### Feature Flags

```toml
[dev-dependencies]
# Standard library support (default)
pretty_assertions = "1.4.1"

# For no_std environments
pretty_assertions = { version = "1.4.1", default-features = false, features = ["alloc"] }

# With unstable features
pretty_assertions = { version = "1.4.1", features = ["unstable"] }
```

### Basic Import Pattern

```rust
#[cfg(test)]
use pretty_assertions::{assert_eq, assert_ne};
```

### Module-Level Import

```rust
#[cfg(test)]
mod tests {
    use pretty_assertions::{assert_eq, assert_ne};
    
    #[test]
    fn my_test() {
        assert_eq!(2 + 2, 4);
    }
}
```

## Basic Usage

### Simple Assertions

```rust
#[cfg(test)]
use pretty_assertions::{assert_eq, assert_ne};

#[test]
fn test_basic_equality() {
    let actual = 2 + 2;
    let expected = 4;
    assert_eq!(actual, expected);
}

#[test]
fn test_basic_inequality() {
    let actual = 2 + 2;
    let unexpected = 5;
    assert_ne!(actual, unexpected);
}
```

### String Comparisons

```rust
#[cfg(test)]
use pretty_assertions::assert_eq;

#[test]
fn test_string_diff() {
    let actual = "Hello, World!";
    let expected = "Hello, Rust!";
    
    // This will show a clear diff highlighting the differences
    assert_eq!(actual, expected);
}

#[test]
fn test_multiline_strings() {
    let actual = r#"
        line 1
        line 2 modified
        line 3
    "#;
    
    let expected = r#"
        line 1
        line 2 original
        line 3
    "#;
    
    assert_eq!(actual, expected);
}
```

### Complex Data Structures

```rust
#[cfg(test)]
use pretty_assertions::assert_eq;

#[derive(Debug, PartialEq)]
struct Person {
    name: String,
    age: u32,
    email: String,
    address: Address,
}

#[derive(Debug, PartialEq)]
struct Address {
    street: String,
    city: String,
    zip: String,
}

#[test]
fn test_complex_structure() {
    let actual = Person {
        name: "Alice Johnson".to_string(),
        age: 30,
        email: "alice@example.com".to_string(),
        address: Address {
            street: "123 Main St".to_string(),
            city: "Springfield".to_string(),
            zip: "12345".to_string(),
        },
    };
    
    let expected = Person {
        name: "Alice Smith".to_string(),  // Different
        age: 30,
        email: "alice@example.org".to_string(),  // Different
        address: Address {
            street: "123 Main St".to_string(),
            city: "Springfield".to_string(),
            zip: "54321".to_string(),  // Different
        },
    };
    
    // Shows exactly which fields differ
    assert_eq!(actual, expected);
}
```

### Collections

```rust
#[cfg(test)]
use pretty_assertions::assert_eq;

#[test]
fn test_vector_diff() {
    let actual = vec![1, 2, 3, 4, 5];
    let expected = vec![1, 2, 9, 4, 5];  // 3 changed to 9
    
    assert_eq!(actual, expected);
}

#[test]
fn test_hashmap_diff() {
    use std::collections::HashMap;
    
    let mut actual = HashMap::new();
    actual.insert("key1", "value1");
    actual.insert("key2", "value2");
    actual.insert("key3", "value3");
    
    let mut expected = HashMap::new();
    expected.insert("key1", "value1");
    expected.insert("key2", "different");  // Changed
    expected.insert("key4", "value4");     // Added, key3 missing
    
    assert_eq!(actual, expected);
}
```

## Advanced Features

### Custom Comparison Types

```rust
#[cfg(test)]
use pretty_assertions::Comparison;

#[test]
fn test_custom_comparison() {
    let left = vec![1, 2, 3];
    let right = vec![1, 2, 4];
    
    // Create a comparison object for custom display
    let comparison = Comparison::new(&left, &right);
    println!("{}", comparison);
}
```

### Working with Results

```rust
#[cfg(test)]
use pretty_assertions::assert_eq;

#[derive(Debug, PartialEq)]
enum MyError {
    InvalidInput(String),
    NetworkError,
}

fn parse_number(s: &str) -> Result<i32, MyError> {
    s.parse().map_err(|_| MyError::InvalidInput(s.to_string()))
}

#[test]
fn test_result_success() {
    let result = parse_number("42");
    assert_eq!(result, Ok(42));
}

#[test]
fn test_result_error() {
    let result = parse_number("invalid");
    assert_eq!(result, Err(MyError::InvalidInput("invalid".to_string())));
}
```

### Option Types

```rust
#[cfg(test)]
use pretty_assertions::assert_eq;

#[test]
fn test_option_some() {
    let result = Some(42);
    assert_eq!(result, Some(42));
}

#[test]
fn test_option_none() {
    let result: Option<i32> = None;
    assert_eq!(result, None);
}

#[test]
fn test_option_mismatch() {
    let result = Some(42);
    assert_eq!(result, Some(43));  // Shows clear diff
}
```

### Nested Structures

```rust
#[cfg(test)]
use pretty_assertions::assert_eq;

#[derive(Debug, PartialEq)]
struct Config {
    database: DatabaseConfig,
    server: ServerConfig,
    features: Vec<String>,
}

#[derive(Debug, PartialEq)]
struct DatabaseConfig {
    host: String,
    port: u16,
    name: String,
}

#[derive(Debug, PartialEq)]
struct ServerConfig {
    host: String,
    port: u16,
    workers: u32,
}

#[test]
fn test_nested_config() {
    let actual = Config {
        database: DatabaseConfig {
            host: "localhost".to_string(),
            port: 5432,
            name: "myapp".to_string(),
        },
        server: ServerConfig {
            host: "0.0.0.0".to_string(),
            port: 8080,
            workers: 4,
        },
        features: vec!["logging".to_string(), "metrics".to_string()],
    };
    
    let expected = Config {
        database: DatabaseConfig {
            host: "localhost".to_string(),
            port: 5432,
            name: "myapp_test".to_string(),  // Different
        },
        server: ServerConfig {
            host: "127.0.0.1".to_string(),  // Different
            port: 8080,
            workers: 8,  // Different
        },
        features: vec!["logging".to_string(), "auth".to_string()],  // Different
    };
    
    assert_eq!(actual, expected);
}
```

## Integration Patterns

### With Proptest

```rust
#[cfg(test)]
use pretty_assertions::assert_eq;
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_string_reverse_property(s in ".*") {
        let reversed_twice: String = s.chars().rev().rev().collect();
        // Pretty assertions work great with property tests
        assert_eq!(s, reversed_twice);
    }
}
```

### With Expect-Test

```rust
#[cfg(test)]
use pretty_assertions::assert_eq;
use expect_test::expect;

#[derive(Debug, PartialEq)]
struct ParseResult {
    tokens: Vec<String>,
    errors: Vec<String>,
}

#[test]
fn test_parser_with_both_libraries() {
    let input = "fn main() { println!(\"hello\"); }";
    let result = parse_code(input);
    
    // Use expect-test for snapshot testing
    expect![[r#"
        ParseResult {
            tokens: [
                "fn",
                "main",
                "(",
                ")",
                "{",
                "println!",
                "(",
                "\"hello\"",
                ")",
                ";",
                "}",
            ],
            errors: [],
        }
    "#]].assert_debug_eq(&result);
    
    // Use pretty_assertions for specific comparisons
    let expected_tokens = vec![
        "fn", "main", "(", ")", "{", 
        "println!", "(", "\"hello\"", ")", ";", "}"
    ];
    assert_eq!(result.tokens, expected_tokens);
}
```

### With Serde

```rust
#[cfg(test)]
use pretty_assertions::assert_eq;
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct ApiResponse {
    status: String,
    data: serde_json::Value,
    timestamp: u64,
}

#[test]
fn test_json_serialization() {
    let response = ApiResponse {
        status: "success".to_string(),
        data: serde_json::json!({
            "users": [
                {"id": 1, "name": "Alice"},
                {"id": 2, "name": "Bob"}
            ]
        }),
        timestamp: 1234567890,
    };
    
    let json = serde_json::to_string_pretty(&response).unwrap();
    let deserialized: ApiResponse = serde_json::from_str(&json).unwrap();
    
    // Pretty assertions will show exactly where serialization differs
    assert_eq!(response, deserialized);
}
```

### With Custom Display

```rust
#[cfg(test)]
use pretty_assertions::assert_eq;
use std::fmt;

#[derive(Debug, PartialEq)]
struct Matrix {
    data: Vec<Vec<i32>>,
}

impl fmt::Display for Matrix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for row in &self.data {
            for (i, &val) in row.iter().enumerate() {
                if i > 0 { write!(f, " ")?; }
                write!(f, "{:3}", val)?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

#[test]
fn test_matrix_display() {
    let actual = Matrix {
        data: vec![
            vec![1, 2, 3],
            vec![4, 5, 6],
            vec![7, 8, 9],
        ],
    };
    
    let expected = Matrix {
        data: vec![
            vec![1, 2, 3],
            vec![4, 0, 6],  // Changed 5 to 0
            vec![7, 8, 9],
        ],
    };
    
    // Shows diff in both Debug and Display representations
    assert_eq!(actual, expected);
}
```

## Customization

### Clippy Integration

Add to your `clippy.toml`:

```toml
disallowed-macros = [
  { path = "std::assert_eq", reason = "use `pretty_assertions::assert_eq` instead" },
  { path = "std::assert_ne", reason = "use `pretty_assertions::assert_ne` instead" },
]
```

### Custom Test Macros

```rust
#[cfg(test)]
use pretty_assertions::{assert_eq, assert_ne};

macro_rules! assert_json_eq {
    ($left:expr, $right:expr) => {
        let left_json: serde_json::Value = serde_json::from_str($left).unwrap();
        let right_json: serde_json::Value = serde_json::from_str($right).unwrap();
        assert_eq!(left_json, right_json);
    };
}

#[test]
fn test_json_macro() {
    let actual = r#"{"name": "Alice", "age": 30}"#;
    let expected = r#"{"age": 30, "name": "Alice"}"#;
    
    assert_json_eq!(actual, expected);
}
```

### Environment-Based Configuration

```rust
#[cfg(test)]
mod tests {
    // Use pretty_assertions only in development
    #[cfg(debug_assertions)]
    use pretty_assertions::{assert_eq, assert_ne};
    
    #[cfg(not(debug_assertions))]
    use std::{assert_eq, assert_ne};
    
    #[test]
    fn test_conditional_pretty() {
        assert_eq!(2 + 2, 4);
    }
}
```

### Custom Assertion Functions

```rust
#[cfg(test)]
use pretty_assertions::assert_eq;

fn assert_vectors_equal_unordered<T>(mut actual: Vec<T>, mut expected: Vec<T>)
where
    T: std::fmt::Debug + PartialEq + Ord,
{
    actual.sort();
    expected.sort();
    assert_eq!(actual, expected);
}

#[test]
fn test_unordered_vectors() {
    let actual = vec![3, 1, 4, 1, 5];
    let expected = vec![1, 1, 3, 4, 5];
    
    assert_vectors_equal_unordered(actual, expected);
}
```

## Performance Considerations

### Compilation Impact

```rust
// Pretty assertions only affect test compilation
// No impact on release builds

#[cfg(test)]
use pretty_assertions::assert_eq;

#[cfg(not(test))]
use std::assert_eq;  // Standard assertions in production
```

### Large Data Structures

```rust
#[cfg(test)]
use pretty_assertions::assert_eq;

#[test]
fn test_large_structure() {
    let large_vec: Vec<i32> = (0..10000).collect();
    let expected: Vec<i32> = (0..10000).collect();
    
    // Pretty assertions handle large structures efficiently
    assert_eq!(large_vec, expected);
}

// For very large structures, consider sampling
#[test]
fn test_large_structure_sampled() {
    let large_vec: Vec<i32> = (0..100000).collect();
    let expected: Vec<i32> = (0..100000).collect();
    
    // Compare length first
    assert_eq!(large_vec.len(), expected.len());
    
    // Sample key elements
    assert_eq!(large_vec[0], expected[0]);
    assert_eq!(large_vec[large_vec.len() / 2], expected[expected.len() / 2]);
    assert_eq!(large_vec[large_vec.len() - 1], expected[expected.len() - 1]);
}
```

### Memory Usage

```rust
#[cfg(test)]
use pretty_assertions::assert_eq;

// For memory-intensive tests, consider using references
#[test]
fn test_memory_efficient() {
    let large_string = "x".repeat(1000000);
    let expected = "x".repeat(1000000);
    
    // Compare references to avoid cloning
    assert_eq!(&large_string, &expected);
}
```

## Best Practices

### 1. Consistent Import Pattern

```rust
// Good: Consistent across all test modules
#[cfg(test)]
use pretty_assertions::{assert_eq, assert_ne};

// Avoid: Mixing standard and pretty assertions
```

### 2. Meaningful Test Names

```rust
#[cfg(test)]
use pretty_assertions::assert_eq;

#[test]
fn test_user_serialization_preserves_all_fields() {
    let user = User { /* ... */ };
    let json = serde_json::to_string(&user).unwrap();
    let deserialized: User = serde_json::from_str(&json).unwrap();
    assert_eq!(user, deserialized);
}
```

### 3. Group Related Assertions

```rust
#[cfg(test)]
use pretty_assertions::assert_eq;

#[test]
fn test_calculator_operations() {
    let calc = Calculator::new();
    
    // Group related assertions
    assert_eq!(calc.add(2, 3), 5);
    assert_eq!(calc.subtract(5, 3), 2);
    assert_eq!(calc.multiply(3, 4), 12);
    assert_eq!(calc.divide(12, 3), 4);
}
```

### 4. Use Descriptive Error Messages

```rust
#[cfg(test)]
use pretty_assertions::assert_eq;

#[test]
fn test_with_custom_message() {
    let result = complex_calculation();
    assert_eq!(
        result, 
        expected_value,
        "Complex calculation should return expected value for input: {:?}",
        input_data
    );
}
```

### 5. Combine with Other Testing Tools

```rust
#[cfg(test)]
use pretty_assertions::assert_eq;
use proptest::prelude::*;
use expect_test::expect;

#[test]
fn comprehensive_test() {
    // Property-based testing
    proptest!(|(x in 0..100i32)| {
        let result = double(x);
        assert_eq!(result, x * 2);
    });
    
    // Snapshot testing
    let complex_output = generate_output();
    expect![[r#"..."#]].assert_debug_eq(&complex_output);
    
    // Pretty assertions for specific cases
    assert_eq!(double(21), 42);
}
```

## Troubleshooting

### Terminal Color Issues

```bash
# Force color output
FORCE_COLOR=1 cargo test

# Disable color output
NO_COLOR=1 cargo test
```

### Windows Terminal Support

```rust
// For Windows compatibility
#[cfg(test)]
mod tests {
    #[cfg(windows)]
    use pretty_assertions::{assert_eq, assert_ne};
    
    #[cfg(not(windows))]
    use std::{assert_eq, assert_ne};
}
```

### CI/CD Integration

```yaml
# GitHub Actions example
- name: Run tests with color
  run: FORCE_COLOR=1 cargo test
  env:
    TERM: xterm-256color
```

### Large Diff Output

```rust
#[cfg(test)]
use pretty_assertions::assert_eq;

// For very large diffs, consider using expect-test instead
#[test]
fn test_large_diff() {
    let large_output = generate_large_output();
    
    // Instead of:
    // assert_eq!(large_output, expected_large_output);
    
    // Consider:
    expect_test::expect_file!["large_output.txt"].assert_eq(&large_output);
}
```

---

Pretty Assertions significantly improves the debugging experience when tests fail by providing clear, colorful diffs that make it easy to spot differences. By following these patterns and best practices, you can create more maintainable and debuggable test suites.