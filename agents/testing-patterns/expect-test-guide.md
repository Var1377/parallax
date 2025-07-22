# Expect-Test Complete Guide

Expect-test is a minimalistic snapshot testing library for Rust that allows you to automatically update test expectations. This guide covers everything from basic usage to advanced patterns and integration strategies.

## Table of Contents

- [What is Snapshot Testing?](#what-is-snapshot-testing)
- [Installation and Setup](#installation-and-setup)
- [Basic Usage](#basic-usage)
- [Advanced Patterns](#advanced-patterns)
- [File-Based Snapshots](#file-based-snapshots)
- [Integration Strategies](#integration-strategies)
- [Best Practices](#best-practices)
- [Workflow Optimization](#workflow-optimization)
- [Common Pitfalls](#common-pitfalls)

## What is Snapshot Testing?

Snapshot testing captures the output of your code and compares it against a saved "snapshot" of the expected output. When the output changes, you can easily review and update the snapshots.

### Benefits of Snapshot Testing

1. **Effortless Updates**: Update expectations with environment variables
2. **Comprehensive Coverage**: Capture complex output structures easily
3. **Regression Detection**: Automatically detect unexpected changes
4. **Documentation**: Snapshots serve as living documentation
5. **Refactoring Safety**: Ensure behavior doesn't change during refactoring

### When to Use Snapshot Testing

- **Parser Output**: AST structures, parse trees
- **Code Generation**: Generated code, templates
- **Serialization**: JSON, XML, binary formats
- **Error Messages**: Compiler diagnostics, validation errors
- **Complex Data Structures**: Large objects, nested structures

## Installation and Setup

### Cargo.toml Configuration

```toml
[dev-dependencies]
expect-test = "1.5.1"
```

### Basic Import

```rust
use expect_test::{expect, expect_file, Expect};
```

## Basic Usage

### Inline Snapshots with expect!

The `expect!` macro creates inline snapshots directly in your source code:

```rust
use expect_test::expect;

#[test]
fn test_basic_arithmetic() {
    let result = 2 + 2;
    let expected = expect!["4"];
    expected.assert_eq(&result.to_string());
}
```

### Multi-line Snapshots

Use double brackets for multi-line content:

```rust
use expect_test::expect;

#[derive(Debug)]
struct Person {
    name: String,
    age: u32,
}

#[test]
fn test_debug_output() {
    let person = Person {
        name: "Alice".to_string(),
        age: 30,
    };
    
    let expected = expect![[r#"
        Person {
            name: "Alice",
            age: 30,
        }
    "#]];
    expected.assert_debug_eq(&person);
}
```

### Updating Snapshots

When tests fail due to changed output, update them with:

```bash
UPDATE_EXPECT=1 cargo test
```

### Core Assertion Methods

```rust
use expect_test::expect;

#[test]
fn assertion_methods() {
    let data = vec![1, 2, 3];
    
    // String comparison
    expect!["[1, 2, 3]"].assert_eq(&format!("{:?}", data));
    
    // Debug comparison (automatically formats)
    expect![[r#"
        [
            1,
            2,
            3,
        ]
    "#]].assert_debug_eq(&data);
}
```

## Advanced Patterns

### Testing Parser Output

```rust
use expect_test::expect;

#[derive(Debug, PartialEq)]
enum Expr {
    Number(i32),
    Add(Box<Expr>, Box<Expr>),
    Multiply(Box<Expr>, Box<Expr>),
}

fn parse_expression(input: &str) -> Result<Expr, String> {
    // Simplified parser implementation
    match input {
        "42" => Ok(Expr::Number(42)),
        "1+2" => Ok(Expr::Add(
            Box::new(Expr::Number(1)),
            Box::new(Expr::Number(2))
        )),
        _ => Err(format!("Cannot parse: {}", input)),
    }
}

#[test]
fn test_parser_success() {
    let result = parse_expression("1+2");
    expect![[r#"
        Ok(
            Add(
                Number(
                    1,
                ),
                Number(
                    2,
                ),
            ),
        )
    "#]].assert_debug_eq(&result);
}

#[test]
fn test_parser_error() {
    let result = parse_expression("invalid");
    expect![[r#"
        Err(
            "Cannot parse: invalid",
        )
    "#]].assert_debug_eq(&result);
}
```

### Testing Code Generation

```rust
use expect_test::expect;

struct CodeGenerator {
    indent_level: usize,
}

impl CodeGenerator {
    fn new() -> Self {
        CodeGenerator { indent_level: 0 }
    }
    
    fn generate_function(&self, name: &str, params: &[&str], body: &str) -> String {
        let indent = "    ".repeat(self.indent_level);
        let params_str = params.join(", ");
        format!(
            "{}fn {}({}) {{\n{}    {}\n{}}}",
            indent, name, params_str, indent, body, indent
        )
    }
}

#[test]
fn test_function_generation() {
    let generator = CodeGenerator::new();
    let result = generator.generate_function(
        "add",
        &["a: i32", "b: i32"],
        "a + b"
    );
    
    expect![[r#"
        fn add(a: i32, b: i32) {
            a + b
        }
    "#]].assert_eq(&result);
}
```

### Testing Error Messages

```rust
use expect_test::expect;

#[derive(Debug)]
struct ValidationError {
    field: String,
    message: String,
    code: u32,
}

fn validate_email(email: &str) -> Result<(), ValidationError> {
    if email.is_empty() {
        return Err(ValidationError {
            field: "email".to_string(),
            message: "Email cannot be empty".to_string(),
            code: 1001,
        });
    }
    
    if !email.contains('@') {
        return Err(ValidationError {
            field: "email".to_string(),
            message: "Email must contain @ symbol".to_string(),
            code: 1002,
        });
    }
    
    Ok(())
}

#[test]
fn test_email_validation_errors() {
    let empty_result = validate_email("");
    expect![[r#"
        Err(
            ValidationError {
                field: "email",
                message: "Email cannot be empty",
                code: 1001,
            },
        )
    "#]].assert_debug_eq(&empty_result);
    
    let invalid_result = validate_email("invalid");
    expect![[r#"
        Err(
            ValidationError {
                field: "email",
                message: "Email must contain @ symbol",
                code: 1002,
            },
        )
    "#]].assert_debug_eq(&invalid_result);
}
```

### Testing Serialization

```rust
use expect_test::expect;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
struct Config {
    database_url: String,
    max_connections: u32,
    features: Vec<String>,
}

#[test]
fn test_config_serialization() {
    let config = Config {
        database_url: "postgresql://localhost/mydb".to_string(),
        max_connections: 100,
        features: vec!["logging".to_string(), "metrics".to_string()],
    };
    
    let json = serde_json::to_string_pretty(&config).unwrap();
    expect![[r#"
        {
          "database_url": "postgresql://localhost/mydb",
          "max_connections": 100,
          "features": [
            "logging",
            "metrics"
          ]
        }
    "#]].assert_eq(&json);
}
```

### Data-Driven Testing

```rust
use expect_test::{expect, Expect};

fn check_calculation(input: i32, expected: Expect) {
    let result = input * 2 + 1;
    expected.assert_eq(&result.to_string());
}

#[test]
fn test_calculations() {
    check_calculation(0, expect!["1"]);
    check_calculation(5, expect!["11"]);
    check_calculation(-3, expect!["-5"]);
    check_calculation(100, expect!["201"]);
}
```

### Testing with Custom Formatting

```rust
use expect_test::expect;
use std::fmt;

#[derive(Debug)]
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
    let matrix = Matrix {
        data: vec![
            vec![1, 2, 3],
            vec![4, 5, 6],
            vec![7, 8, 9],
        ],
    };
    
    expect![[r#"
          1   2   3
          4   5   6
          7   8   9
    "#]].assert_eq(&matrix.to_string());
}
```

## File-Based Snapshots

For large outputs, use external files with `expect_file!`:

### Basic File Snapshots

```rust
use expect_test::expect_file;

#[test]
fn test_large_output() {
    let large_data = generate_large_json();
    expect_file!["../test-data/large_output.json"].assert_eq(&large_data);
}
```

### Organizing Test Files

```
tests/
├── snapshots/
│   ├── parser/
│   │   ├── expressions.txt
│   │   └── statements.txt
│   ├── codegen/
│   │   ├── functions.rs
│   │   └── structs.rs
│   └── errors/
│       └── validation.txt
└── integration_tests.rs
```

```rust
use expect_test::expect_file;

#[test]
fn test_expression_parsing() {
    let ast = parse_complex_expression("(a + b) * (c - d)");
    expect_file!["snapshots/parser/expressions.txt"].assert_debug_eq(&ast);
}

#[test]
fn test_function_codegen() {
    let code = generate_function_code(&function_def);
    expect_file!["snapshots/codegen/functions.rs"].assert_eq(&code);
}
```

### Binary Data Snapshots

```rust
use expect_test::expect_file;

#[test]
fn test_binary_serialization() {
    let data = MyStruct { /* ... */ };
    let serialized = bincode::serialize(&data).unwrap();
    
    // Convert to hex string for readable snapshots
    let hex_string = serialized.iter()
        .map(|b| format!("{:02x}", b))
        .collect::<Vec<_>>()
        .join(" ");
    
    expect_file!["snapshots/binary/mystruct.hex"].assert_eq(&hex_string);
}
```

## Integration Strategies

### With Proptest

Combine snapshot testing with property-based testing:

```rust
use expect_test::expect;
use proptest::prelude::*;

fn normalize_whitespace(s: &str) -> String {
    s.split_whitespace().collect::<Vec<_>>().join(" ")
}

proptest! {
    #[test]
    fn test_whitespace_normalization_properties(
        s in "[ \t\n]*[a-zA-Z]+[ \t\n]*[a-zA-Z]*[ \t\n]*"
    ) {
        let normalized = normalize_whitespace(&s);
        
        // Property: no leading/trailing whitespace
        prop_assert!(!normalized.starts_with(' '));
        prop_assert!(!normalized.ends_with(' '));
        
        // Property: no consecutive spaces
        prop_assert!(!normalized.contains("  "));
    }
}

#[test]
fn test_whitespace_normalization_snapshots() {
    // Test specific cases with snapshots
    expect!["hello world"].assert_eq(&normalize_whitespace("  hello   world  "));
    expect!["a b c"].assert_eq(&normalize_whitespace("a\t\tb\n\nc"));
    expect!["single"].assert_eq(&normalize_whitespace("\n\tsingle\n\t"));
}
```

### With Async Code

```rust
use expect_test::expect;
use tokio_test;

async fn fetch_user_data(id: u32) -> Result<String, String> {
    // Simulate async operation
    tokio::time::sleep(std::time::Duration::from_millis(10)).await;
    
    match id {
        1 => Ok(r#"{"id": 1, "name": "Alice", "email": "alice@example.com"}"#.to_string()),
        2 => Ok(r#"{"id": 2, "name": "Bob", "email": "bob@example.com"}"#.to_string()),
        _ => Err("User not found".to_string()),
    }
}

#[tokio::test]
async fn test_async_user_fetch() {
    let result = fetch_user_data(1).await;
    expect![[r#"
        Ok(
            "{\"id\": 1, \"name\": \"Alice\", \"email\": \"alice@example.com\"}",
        )
    "#]].assert_debug_eq(&result);
}
```

### With Custom Test Harness

```rust
use expect_test::{expect, Expect};

struct TestCase {
    name: &'static str,
    input: &'static str,
    expected: Expect,
}

fn run_compiler_tests(tests: &[TestCase]) {
    for test in tests {
        println!("Running test: {}", test.name);
        let result = compile_code(test.input);
        test.expected.assert_debug_eq(&result);
    }
}

#[test]
fn test_compiler_suite() {
    let tests = vec![
        TestCase {
            name: "simple_function",
            input: "fn main() { println!(\"Hello\"); }",
            expected: expect![[r#"
                Ok(
                    Program {
                        functions: [
                            Function {
                                name: "main",
                                body: [
                                    PrintStatement("Hello"),
                                ],
                            },
                        ],
                    },
                )
            "#]],
        },
        TestCase {
            name: "syntax_error",
            input: "fn main( { }",
            expected: expect![[r#"
                Err(
                    SyntaxError {
                        message: "Expected ')' after parameter list",
                        line: 1,
                        column: 10,
                    },
                )
            "#]],
        },
    ];
    
    run_compiler_tests(&tests);
}
```

## Best Practices

### 1. Keep Snapshots Readable

```rust
// Good: Clear, formatted output
expect![[r#"
    User {
        id: 123,
        name: "Alice",
        email: "alice@example.com",
        active: true,
    }
"#]].assert_debug_eq(&user);

// Avoid: Unformatted, hard to read
expect!["User { id: 123, name: \"Alice\", email: \"alice@example.com\", active: true }"]
    .assert_eq(&format!("{:?}", user));
```

### 2. Use Meaningful Test Names

```rust
#[test]
fn test_parse_valid_email_address() {
    let result = parse_email("user@domain.com");
    expect![[r#"
        Ok(
            Email {
                local: "user",
                domain: "domain.com",
            },
        )
    "#]].assert_debug_eq(&result);
}

#[test]
fn test_parse_email_missing_at_symbol() {
    let result = parse_email("invalid-email");
    expect![[r#"
        Err(
            "Missing @ symbol",
        )
    "#]].assert_debug_eq(&result);
}
```

### 3. Normalize Non-Deterministic Output

```rust
use expect_test::expect;
use std::collections::HashMap;

fn normalize_hashmap_output(map: &HashMap<String, i32>) -> String {
    let mut items: Vec<_> = map.iter().collect();
    items.sort_by_key(|(k, _)| *k);
    format!("{:#?}", items)
}

#[test]
fn test_hashmap_operations() {
    let mut map = HashMap::new();
    map.insert("apple".to_string(), 5);
    map.insert("banana".to_string(), 3);
    
    // Normalize to ensure consistent ordering
    let normalized = normalize_hashmap_output(&map);
    expect![[r#"
        [
            (
                "apple",
                5,
            ),
            (
                "banana",
                3,
            ),
        ]
    "#]].assert_eq(&normalized);
}
```

### 4. Handle Timestamps and UUIDs

```rust
use expect_test::expect;
use regex::Regex;

fn normalize_log_output(log: &str) -> String {
    let timestamp_re = Regex::new(r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}").unwrap();
    let uuid_re = Regex::new(r"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}").unwrap();
    
    let normalized = timestamp_re.replace_all(log, "TIMESTAMP");
    let normalized = uuid_re.replace_all(&normalized, "UUID");
    normalized.to_string()
}

#[test]
fn test_log_output() {
    let log = generate_log_entry();
    let normalized = normalize_log_output(&log);
    
    expect![[r#"
        [TIMESTAMP] INFO request_id=UUID Processing user request
        [TIMESTAMP] DEBUG request_id=UUID Database query completed
        [TIMESTAMP] INFO request_id=UUID Response sent
    "#]].assert_eq(&normalized);
}
```

### 5. Group Related Snapshots

```rust
use expect_test::expect;

mod parser_tests {
    use super::*;
    
    #[test]
    fn expressions() {
        expect![[r#"..."#]].assert_debug_eq(&parse_expr("1 + 2"));
        expect![[r#"..."#]].assert_debug_eq(&parse_expr("a * b"));
    }
    
    #[test]
    fn statements() {
        expect![[r#"..."#]].assert_debug_eq(&parse_stmt("let x = 5;"));
        expect![[r#"..."#]].assert_debug_eq(&parse_stmt("return x;"));
    }
}

mod codegen_tests {
    use super::*;
    
    #[test]
    fn functions() {
        expect![[r#"..."#]].assert_eq(&generate_function(&func_def));
    }
    
    #[test]
    fn structs() {
        expect![[r#"..."#]].assert_eq(&generate_struct(&struct_def));
    }
}
```

## Workflow Optimization

### 1. Efficient Update Workflow

```bash
# Run tests and update all snapshots
UPDATE_EXPECT=1 cargo test

# Update specific test
UPDATE_EXPECT=1 cargo test test_parser_output

# Review changes before committing
git diff

# Commit snapshot updates separately
git add tests/
git commit -m "Update test snapshots"
```

### 2. IDE Integration

Most Rust IDEs support expect-test:

- **rust-analyzer**: Provides inline snapshot updates
- **VS Code**: Shows diff when snapshots don't match
- **IntelliJ**: Highlights snapshot mismatches

### 3. CI/CD Integration

```yaml
# .github/workflows/test.yml
name: Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
      
      # Run tests without updating snapshots
      - name: Run tests
        run: cargo test
      
      # Check that snapshots are up to date
      - name: Check snapshots
        run: |
          UPDATE_EXPECT=1 cargo test
          if ! git diff --quiet; then
            echo "Snapshots are out of date. Run 'UPDATE_EXPECT=1 cargo test' locally."
            exit 1
          fi
```

### 4. Snapshot Review Process

```bash
# Before making changes
git checkout -b feature/new-parser

# Make code changes
# ...

# Update snapshots
UPDATE_EXPECT=1 cargo test

# Review snapshot changes
git diff --no-index old_snapshot.txt new_snapshot.txt

# Commit changes
git add .
git commit -m "Implement new parser features

- Add support for lambda expressions
- Update error messages for better clarity
- Update test snapshots"
```

## Common Pitfalls

### 1. Over-Snapshotting

```rust
// Bad: Snapshotting internal implementation details
#[test]
fn bad_internal_snapshot() {
    let parser = Parser::new();
    // Don't snapshot internal state
    expect![[r#"..."#]].assert_debug_eq(&parser.internal_state);
}

// Good: Snapshot observable behavior
#[test]
fn good_behavior_snapshot() {
    let result = parse("input");
    expect![[r#"..."#]].assert_debug_eq(&result);
}
```

### 2. Platform-Specific Snapshots

```rust
// Bad: Platform-specific paths
#[test]
fn bad_platform_specific() {
    let path = std::env::current_dir().unwrap();
    expect![[r#"/home/user/project"#]].assert_eq(&path.to_string_lossy());
}

// Good: Normalize platform differences
#[test]
fn good_normalized() {
    let path = std::env::current_dir().unwrap();
    let normalized = path.file_name().unwrap().to_string_lossy();
    expect!["project"].assert_eq(&normalized);
}
```

### 3. Ignoring Snapshot Diffs

```rust
// Always review snapshot changes carefully
// Don't blindly run UPDATE_EXPECT=1 without understanding changes

// Good practice: Add comments explaining expected changes
#[test]
fn test_after_refactor() {
    // After refactoring the error message format,
    // this snapshot should show improved error details
    let result = validate_input("invalid");
    expect![[r#"
        Err(
            ValidationError {
                field: "input",
                message: "Input must be alphanumeric",
                suggestion: "Try using only letters and numbers",
            },
        )
    "#]].assert_debug_eq(&result);
}
```

### 4. Large Snapshots

```rust
// Bad: Huge inline snapshots
#[test]
fn bad_huge_snapshot() {
    let result = generate_large_output();
    expect![[r#"
        // 1000 lines of output...
    "#]].assert_eq(&result);
}

// Good: Use file snapshots for large output
#[test]
fn good_file_snapshot() {
    let result = generate_large_output();
    expect_file!["snapshots/large_output.txt"].assert_eq(&result);
}
```

---

Expect-test provides a powerful and ergonomic way to test complex outputs and ensure your code's behavior remains consistent. By following these patterns and best practices, you can create maintainable test suites that catch regressions while being easy to update when behavior legitimately changes.