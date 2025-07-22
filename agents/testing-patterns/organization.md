# Test Organization Strategies

Effective test organization is crucial for maintaining large, complex test suites. This guide covers strategies for structuring tests, managing test data, and creating maintainable test architectures.

## Table of Contents

- [Test Structure Principles](#test-structure-principles)
- [File Organization Patterns](#file-organization-patterns)
- [Module Organization](#module-organization)
- [Test Data Management](#test-data-management)
- [Shared Test Utilities](#shared-test-utilities)
- [Integration Test Patterns](#integration-test-patterns)
- [Performance Test Organization](#performance-test-organization)
- [CI/CD Test Organization](#cicd-test-organization)

## Test Structure Principles

### 1. Separation of Concerns

Organize tests by functionality, not by test type:

```rust
// Good: Organized by feature
src/
├── parser/
│   ├── mod.rs
│   ├── expressions.rs
│   └── statements.rs
tests/
├── parser/
│   ├── expression_tests.rs
│   ├── statement_tests.rs
│   └── integration_tests.rs

// Avoid: Organized by test type
tests/
├── unit_tests.rs
├── integration_tests.rs
└── property_tests.rs
```

### 2. Test Hierarchy

Structure tests from specific to general:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    mod unit_tests {
        use super::*;
        
        mod parser {
            use super::*;
            
            mod expressions {
                use super::*;
                
                #[test]
                fn test_number_literal() { /* ... */ }
                
                #[test]
                fn test_string_literal() { /* ... */ }
                
                mod binary_operations {
                    use super::*;
                    
                    #[test]
                    fn test_addition() { /* ... */ }
                    
                    #[test]
                    fn test_multiplication() { /* ... */ }
                }
            }
        }
    }
    
    mod integration_tests {
        use super::*;
        
        #[test]
        fn test_parse_and_evaluate() { /* ... */ }
    }
}
```

### 3. Naming Conventions

Use consistent, descriptive naming:

```rust
// Good: Descriptive test names
#[test]
fn test_parser_handles_empty_input() { /* ... */ }

#[test]
fn test_parser_recovers_from_missing_semicolon() { /* ... */ }

#[test]
fn test_parser_reports_unexpected_token_error() { /* ... */ }

// Avoid: Generic names
#[test]
fn test_parser() { /* ... */ }

#[test]
fn test_error() { /* ... */ }
```

## File Organization Patterns

### 1. Colocated Tests

Place unit tests in the same file as the code being tested:

```rust
// src/calculator.rs
pub struct Calculator;

impl Calculator {
    pub fn add(&self, a: i32, b: i32) -> i32 {
        a + b
    }
    
    pub fn divide(&self, a: i32, b: i32) -> Result<i32, String> {
        if b == 0 {
            Err("Division by zero".to_string())
        } else {
            Ok(a / b)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use proptest::prelude::*;
    
    mod unit_tests {
        use super::*;
        
        #[test]
        fn test_add_positive_numbers() {
            let calc = Calculator;
            assert_eq!(calc.add(2, 3), 5);
        }
        
        #[test]
        fn test_divide_by_zero() {
            let calc = Calculator;
            assert_eq!(calc.divide(5, 0), Err("Division by zero".to_string()));
        }
    }
    
    mod property_tests {
        use super::*;
        
        proptest! {
            #[test]
            fn test_add_commutative(a in any::<i32>(), b in any::<i32>()) {
                let calc = Calculator;
                prop_assert_eq!(calc.add(a, b), calc.add(b, a));
            }
        }
    }
}
```

### 2. Separate Test Files

For integration tests and large test suites:

```
project/
├── src/
│   ├── lib.rs
│   ├── parser/
│   │   ├── mod.rs
│   │   ├── lexer.rs
│   │   └── ast.rs
│   └── compiler/
│       ├── mod.rs
│       └── codegen.rs
├── tests/
│   ├── integration/
│   │   ├── mod.rs
│   │   ├── end_to_end.rs
│   │   └── performance.rs
│   ├── fixtures/
│   │   ├── valid_programs/
│   │   └── invalid_programs/
│   └── common/
│       ├── mod.rs
│       └── test_helpers.rs
└── benches/
    ├── parser_bench.rs
    └── compiler_bench.rs
```

### 3. Test Module Structure

```rust
// tests/integration/mod.rs
pub mod common;
pub mod end_to_end;
pub mod performance;

// tests/integration/common/mod.rs
use std::path::Path;

pub fn load_test_file(name: &str) -> String {
    std::fs::read_to_string(
        Path::new("tests/fixtures").join(name)
    ).expect("Failed to load test file")
}

pub fn setup_test_environment() -> TestEnvironment {
    TestEnvironment::new()
}

pub struct TestEnvironment {
    // Test setup state
}

impl TestEnvironment {
    pub fn new() -> Self {
        Self { /* ... */ }
    }
}
```

## Module Organization

### 1. Feature-Based Organization

```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    mod lexer_tests {
        use super::*;
        
        mod tokenization {
            use super::*;
            
            #[test]
            fn test_keywords() { /* ... */ }
            
            #[test]
            fn test_identifiers() { /* ... */ }
            
            #[test]
            fn test_numbers() { /* ... */ }
        }
        
        mod error_handling {
            use super::*;
            
            #[test]
            fn test_invalid_character() { /* ... */ }
            
            #[test]
            fn test_unterminated_string() { /* ... */ }
        }
    }
    
    mod parser_tests {
        use super::*;
        
        mod expressions {
            use super::*;
            
            #[test]
            fn test_binary_expressions() { /* ... */ }
            
            #[test]
            fn test_unary_expressions() { /* ... */ }
        }
        
        mod statements {
            use super::*;
            
            #[test]
            fn test_variable_declarations() { /* ... */ }
            
            #[test]
            fn test_function_definitions() { /* ... */ }
        }
    }
}
```

### 2. Test Type Organization

```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    mod unit_tests {
        use super::*;
        use pretty_assertions::assert_eq;
        
        #[test]
        fn test_individual_functions() { /* ... */ }
    }
    
    mod integration_tests {
        use super::*;
        use expect_test::expect;
        
        #[test]
        fn test_component_interactions() { /* ... */ }
    }
    
    mod property_tests {
        use super::*;
        use proptest::prelude::*;
        
        proptest! {
            #[test]
            fn test_invariants(input in any::<String>()) {
                // Property-based tests
            }
        }
    }
    
    mod snapshot_tests {
        use super::*;
        use expect_test::expect;
        
        #[test]
        fn test_output_format() {
            let result = generate_output();
            expect![[r#"..."#]].assert_debug_eq(&result);
        }
    }
}
```

## Test Data Management

### 1. Test Fixtures

```rust
// tests/fixtures/mod.rs
use std::path::{Path, PathBuf};

pub struct TestFixtures;

impl TestFixtures {
    pub fn load_program(name: &str) -> String {
        let path = Self::fixture_path("programs").join(format!("{}.px", name));
        std::fs::read_to_string(path).expect("Failed to load test program")
    }
    
    pub fn load_expected_output(name: &str) -> String {
        let path = Self::fixture_path("expected").join(format!("{}.txt", name));
        std::fs::read_to_string(path).expect("Failed to load expected output")
    }
    
    fn fixture_path(category: &str) -> PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests")
            .join("fixtures")
            .join(category)
    }
}

// Usage in tests
#[test]
fn test_fibonacci_program() {
    let program = TestFixtures::load_program("fibonacci");
    let expected = TestFixtures::load_expected_output("fibonacci");
    
    let result = compile_and_run(&program);
    assert_eq!(result, expected);
}
```

### 2. Test Data Builders

```rust
// tests/common/builders.rs
pub struct ProgramBuilder {
    functions: Vec<Function>,
    imports: Vec<String>,
}

impl ProgramBuilder {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            imports: Vec::new(),
        }
    }
    
    pub fn with_function(mut self, name: &str, body: &str) -> Self {
        self.functions.push(Function {
            name: name.to_string(),
            body: body.to_string(),
        });
        self
    }
    
    pub fn with_import(mut self, import: &str) -> Self {
        self.imports.push(import.to_string());
        self
    }
    
    pub fn build(self) -> Program {
        Program {
            functions: self.functions,
            imports: self.imports,
        }
    }
}

// Usage in tests
#[test]
fn test_program_compilation() {
    let program = ProgramBuilder::new()
        .with_import("std::io")
        .with_function("main", "println!(\"Hello, world!\");")
        .build();
    
    let result = compile_program(&program);
    assert!(result.is_ok());
}
```

### 3. Parameterized Tests

```rust
use expect_test::{expect, Expect};

struct TestCase {
    name: &'static str,
    input: &'static str,
    expected: Expect,
}

fn run_parser_tests(tests: &[TestCase]) {
    for test in tests {
        println!("Running test: {}", test.name);
        let result = parse(test.input);
        test.expected.assert_debug_eq(&result);
    }
}

#[test]
fn test_expression_parsing() {
    let tests = vec![
        TestCase {
            name: "number_literal",
            input: "42",
            expected: expect![[r#"
                Ok(
                    Number(42),
                )
            "#]],
        },
        TestCase {
            name: "string_literal",
            input: "\"hello\"",
            expected: expect![[r#"
                Ok(
                    String("hello"),
                )
            "#]],
        },
        TestCase {
            name: "binary_addition",
            input: "1 + 2",
            expected: expect![[r#"
                Ok(
                    BinaryOp {
                        left: Number(1),
                        op: Add,
                        right: Number(2),
                    },
                )
            "#]],
        },
    ];
    
    run_parser_tests(&tests);
}
```

## Shared Test Utilities

### 1. Common Test Functions

```rust
// tests/common/mod.rs
use std::path::Path;

pub fn compile_program(source: &str) -> Result<CompiledProgram, CompileError> {
    let mut compiler = Compiler::new();
    compiler.compile(source)
}

pub fn run_program(program: &CompiledProgram) -> Result<String, RuntimeError> {
    let mut runtime = Runtime::new();
    runtime.execute(program)
}

pub fn compile_and_run(source: &str) -> Result<String, Box<dyn std::error::Error>> {
    let program = compile_program(source)?;
    let output = run_program(&program)?;
    Ok(output)
}

pub fn assert_compile_error(source: &str, expected_error: &str) {
    match compile_program(source) {
        Ok(_) => panic!("Expected compilation to fail"),
        Err(error) => assert!(
            error.to_string().contains(expected_error),
            "Expected error containing '{}', got '{}'",
            expected_error,
            error
        ),
    }
}

pub fn load_test_program(name: &str) -> String {
    let path = Path::new("tests/programs").join(format!("{}.px", name));
    std::fs::read_to_string(path).expect("Failed to load test program")
}
```

### 2. Test Macros

```rust
// tests/common/macros.rs
macro_rules! test_cases {
    ($test_name:ident, $function:expr, [$(($name:expr, $input:expr, $expected:expr)),* $(,)?]) => {
        #[test]
        fn $test_name() {
            $(
                println!("Testing case: {}", $name);
                let result = $function($input);
                assert_eq!(result, $expected, "Failed test case: {}", $name);
            )*
        }
    };
}

// Usage
test_cases!(
    test_arithmetic_operations,
    |expr| evaluate_expression(expr),
    [
        ("addition", "2 + 3", 5),
        ("subtraction", "5 - 2", 3),
        ("multiplication", "3 * 4", 12),
        ("division", "8 / 2", 4),
    ]
);
```

### 3. Test Configuration

```rust
// tests/common/config.rs
use std::sync::Once;

static INIT: Once = Once::new();

pub fn setup_test_environment() {
    INIT.call_once(|| {
        env_logger::init();
        // Other global setup
    });
}

pub struct TestConfig {
    pub verbose: bool,
    pub timeout: std::time::Duration,
    pub temp_dir: tempfile::TempDir,
}

impl TestConfig {
    pub fn new() -> Self {
        setup_test_environment();
        
        Self {
            verbose: std::env::var("TEST_VERBOSE").is_ok(),
            timeout: std::time::Duration::from_secs(30),
            temp_dir: tempfile::tempdir().expect("Failed to create temp dir"),
        }
    }
}

impl Default for TestConfig {
    fn default() -> Self {
        Self::new()
    }
}
```

## Integration Test Patterns

### 1. End-to-End Testing

```rust
// tests/integration/end_to_end.rs
use crate::common::*;

#[test]
fn test_complete_compilation_pipeline() {
    let config = TestConfig::new();
    
    // Load test program
    let source = load_test_program("fibonacci");
    
    // Compile
    let program = compile_program(&source)
        .expect("Compilation should succeed");
    
    // Run
    let output = run_program(&program)
        .expect("Execution should succeed");
    
    // Verify output
    let expected = "0 1 1 2 3 5 8 13 21 34";
    assert_eq!(output.trim(), expected);
}

#[test]
fn test_error_reporting_pipeline() {
    let source = r#"
        fn main() {
            let x = 5;
            let y = x + unknown_variable;
        }
    "#;
    
    match compile_program(source) {
        Ok(_) => panic!("Expected compilation to fail"),
        Err(error) => {
            assert!(error.to_string().contains("unknown_variable"));
            assert!(error.to_string().contains("line 4"));
        }
    }
}
```

### 2. Component Integration

```rust
// tests/integration/component_integration.rs
use crate::common::*;

#[test]
fn test_lexer_parser_integration() {
    let source = "fn add(a: i32, b: i32) -> i32 { a + b }";
    
    // Test lexer output
    let tokens = tokenize(source).expect("Tokenization should succeed");
    assert_eq!(tokens.len(), 15);
    
    // Test parser with lexer output
    let ast = parse_tokens(&tokens).expect("Parsing should succeed");
    
    // Verify AST structure
    match ast {
        Program { functions } => {
            assert_eq!(functions.len(), 1);
            assert_eq!(functions[0].name, "add");
        }
    }
}

#[test]
fn test_parser_typechecker_integration() {
    let source = "fn main() { let x: i32 = \"string\"; }";
    
    // Parse should succeed
    let ast = parse(source).expect("Parsing should succeed");
    
    // Type checking should fail
    match typecheck(&ast) {
        Ok(_) => panic!("Expected type checking to fail"),
        Err(error) => {
            assert!(error.to_string().contains("type mismatch"));
            assert!(error.to_string().contains("expected i32, found string"));
        }
    }
}
```

## Performance Test Organization

### 1. Benchmark Structure

```rust
// benches/parser_bench.rs
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use my_compiler::*;

fn benchmark_parser(c: &mut Criterion) {
    let small_program = include_str!("../tests/fixtures/programs/small.px");
    let medium_program = include_str!("../tests/fixtures/programs/medium.px");
    let large_program = include_str!("../tests/fixtures/programs/large.px");
    
    c.bench_function("parse_small_program", |b| {
        b.iter(|| parse(black_box(small_program)))
    });
    
    c.bench_function("parse_medium_program", |b| {
        b.iter(|| parse(black_box(medium_program)))
    });
    
    c.bench_function("parse_large_program", |b| {
        b.iter(|| parse(black_box(large_program)))
    });
}

criterion_group!(benches, benchmark_parser);
criterion_main!(benches);
```

### 2. Performance Regression Tests

```rust
// tests/performance/regression.rs
use std::time::{Duration, Instant};

#[test]
fn test_parser_performance_regression() {
    let source = load_test_program("large_program");
    
    let start = Instant::now();
    let _result = parse(&source).expect("Parsing should succeed");
    let duration = start.elapsed();
    
    // Ensure parsing completes within reasonable time
    assert!(
        duration < Duration::from_millis(100),
        "Parser took too long: {:?}",
        duration
    );
}

#[test]
fn test_memory_usage() {
    let source = load_test_program("memory_intensive");
    
    let initial_memory = get_memory_usage();
    let _result = compile_program(&source).expect("Compilation should succeed");
    let final_memory = get_memory_usage();
    
    let memory_increase = final_memory - initial_memory;
    assert!(
        memory_increase < 100_000_000, // 100MB
        "Memory usage increased by {} bytes",
        memory_increase
    );
}

fn get_memory_usage() -> usize {
    // Platform-specific memory measurement
    #[cfg(target_os = "linux")]
    {
        // Read from /proc/self/status
        unimplemented!()
    }
    
    #[cfg(not(target_os = "linux"))]
    {
        0 // Placeholder
    }
}
```

## CI/CD Test Organization

### 1. Test Categories

```rust
// tests/categories.rs

// Fast tests that run on every commit
#[cfg(test)]
mod fast_tests {
    use super::*;
    
    #[test]
    fn test_basic_functionality() {
        // Quick unit tests
    }
}

// Slower tests that run on pull requests
#[cfg(test)]
#[cfg(feature = "slow-tests")]
mod slow_tests {
    use super::*;
    
    #[test]
    fn test_comprehensive_integration() {
        // Comprehensive integration tests
    }
}

// Very slow tests that run nightly
#[cfg(test)]
#[cfg(feature = "nightly-tests")]
mod nightly_tests {
    use super::*;
    
    #[test]
    fn test_stress_scenarios() {
        // Stress tests and fuzzing
    }
}
```

### 2. Environment-Specific Tests

```rust
// tests/platform_specific.rs

#[cfg(test)]
#[cfg(target_os = "linux")]
mod linux_tests {
    #[test]
    fn test_linux_specific_features() {
        // Linux-specific functionality
    }
}

#[cfg(test)]
#[cfg(target_os = "windows")]
mod windows_tests {
    #[test]
    fn test_windows_specific_features() {
        // Windows-specific functionality
    }
}

#[cfg(test)]
#[cfg(feature = "network-tests")]
mod network_tests {
    #[test]
    fn test_network_functionality() {
        // Tests requiring network access
    }
}
```

### 3. Test Execution Scripts

```bash
#!/bin/bash
# scripts/run-tests.sh

set -e

echo "Running fast tests..."
cargo test --lib --bins

echo "Running integration tests..."
cargo test --test '*'

if [ "$CI" = "true" ]; then
    echo "Running slow tests in CI..."
    cargo test --features slow-tests
fi

if [ "$NIGHTLY" = "true" ]; then
    echo "Running nightly tests..."
    cargo test --features nightly-tests
fi

echo "Running benchmarks..."
cargo bench

echo "All tests passed!"
```

---

Effective test organization makes your test suite maintainable, discoverable, and efficient. By following these patterns, you can create a robust testing infrastructure that scales with your project and provides clear feedback when things go wrong.