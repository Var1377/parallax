# Complete API Reference

This comprehensive reference covers all the APIs and patterns for proptest, expect-test, and pretty_assertions, with practical examples and usage patterns.

## Table of Contents

- [Proptest API Reference](#proptest-api-reference)
- [Expect-Test API Reference](#expect-test-api-reference)
- [Pretty Assertions API Reference](#pretty-assertions-api-reference)
- [Integration Patterns](#integration-patterns)
- [Configuration Reference](#configuration-reference)
- [Error Handling Reference](#error-handling-reference)

## Proptest API Reference

### Core Macros

#### `proptest!`

The main macro for defining property-based tests.

```rust
use proptest::prelude::*;

proptest! {
    // Basic syntax
    #[test]
    fn test_name(param in strategy) {
        // Test body
        prop_assert!(condition);
    }
    
    // Multiple parameters
    #[test]
    fn test_multiple_params(
        x in 0..100i32,
        y in 0..100i32,
        s in ".*"
    ) {
        prop_assert!(x + y >= x);
        prop_assert!(x + y >= y);
        prop_assert!(!s.is_empty() || s.len() == 0);
    }
    
    // With configuration
    #![proptest_config(ProptestConfig::with_cases(1000))]
    
    #[test]
    fn configured_test(x in any::<i32>()) {
        prop_assert!(x.abs() >= 0);
    }
}
```

#### `prop_assert!`

Assertion macro for property tests.

```rust
proptest! {
    #[test]
    fn assertion_examples(x in 0..100i32) {
        // Basic assertion
        prop_assert!(x >= 0);
        
        // With custom message
        prop_assert!(x < 100, "x should be less than 100, got {}", x);
        
        // With format arguments
        prop_assert!(x >= 0, "x = {} should be non-negative", x);
    }
}
```

#### `prop_assert_eq!` and `prop_assert_ne!`

Equality and inequality assertions.

```rust
proptest! {
    #[test]
    fn equality_assertions(x in any::<i32>()) {
        prop_assert_eq!(x + 0, x);
        prop_assert_eq!(x * 1, x, "multiplication by 1 should be identity");
        
        prop_assert_ne!(x, x + 1);
        prop_assert_ne!(x * 2, x, "doubling should change value (unless x=0)");
    }
}
```

#### `prop_assume!`

Skip test cases that don't meet preconditions.

```rust
proptest! {
    #[test]
    fn division_test(a in any::<i32>(), b in any::<i32>()) {
        prop_assume!(b != 0);  // Skip when b is 0
        
        let result = a / b;
        prop_assert_eq!(result * b + a % b, a);
    }
}
```

### Strategy API

#### Basic Strategies

```rust
use proptest::prelude::*;

// Primitive types
any::<i32>()           // Any i32 value
any::<String>()        // Any String
any::<bool>()          // true or false

// Ranges
0..100i32              // Integers from 0 to 99
0..=100i32             // Integers from 0 to 100 (inclusive)
-1000.0..1000.0f64     // Floating point range

// Characters
'a'..'z'               // Lowercase letters
any::<char>()          // Any Unicode character

// Specific values
Just(42)               // Always generates 42
prop::sample::select(vec![1, 2, 3])  // Choose from list
```

#### String Strategies

```rust
use proptest::string::*;

// Regex-based strings
"[a-zA-Z]{5,10}"                    // 5-10 alphabetic characters
r"\d{3}-\d{3}-\d{4}"               // Phone number pattern
"[a-z]+@[a-z]+\.[a-z]{2,4}"       // Email pattern

// String with specific properties
string_regex("[0-9a-fA-F]{8}").unwrap()  // Hex string
"[ -~]*"                           // ASCII printable characters
"\\PC*"                            // Any Unicode string

// Size-constrained strings
prop::string::string_regex("[a-z]{1,20}").unwrap()
```

#### Collection Strategies

```rust
use proptest::collection::*;

// Vectors
vec(any::<i32>(), 0..10)           // Vector of 0-9 integers
vec(1..100i32, 5)                  // Vector of exactly 5 integers
vec(".*", 0..=20)                  // Vector of 0-20 strings

// Hash sets
hash_set(any::<i32>(), 0..10)      // Set of 0-9 unique integers
hash_set(".*", 5..=15)             // Set of 5-15 unique strings

// Hash maps
hash_map(any::<String>(), any::<i32>(), 0..10)  // Map with 0-9 entries
hash_map("[a-z]+", 1..100i32, 5)   // Map with exactly 5 entries

// B-tree collections (ordered)
btree_set(any::<i32>(), 0..10)     // Ordered set
btree_map(any::<String>(), any::<i32>(), 0..10)  // Ordered map

// Arrays
prop::array::uniform32(any::<u8>())  // Array of 32 u8 values
```

#### Option and Result Strategies

```rust
use proptest::option::*;
use proptest::result::*;

// Options
of(any::<i32>())                   // Option<i32>
weighted(0.8, any::<i32>())        // 80% Some, 20% None

// Results
maybe_ok(any::<i32>(), "error")    // Result<i32, &str>
maybe_err(any::<i32>(), "error")   // Mostly Err values
weighted(0.9, any::<i32>(), "error")  // 90% Ok, 10% Err
```

#### Strategy Combinators

```rust
use proptest::strategy::Strategy;

// prop_map: Transform generated values
(any::<i32>(), any::<i32>())
    .prop_map(|(x, y)| Point { x, y })

// prop_filter: Filter generated values
any::<i32>()
    .prop_filter("must be even", |x| x % 2 == 0)

// prop_flat_map: Generate dependent values
any::<usize>()
    .prop_flat_map(|size| vec(any::<i32>(), size))

// prop_union: Choose between strategies
prop_oneof![
    Just(0),
    1..10i32,
    100..1000i32,
]

// prop_recursive: Generate recursive structures
prop::strategy::LazyJust::new(|| any::<i32>())
    .prop_recursive(8, 256, 10, |inner| {
        prop_oneof![
            inner.clone().prop_map(|x| Tree::Leaf(x)),
            (inner.clone(), inner).prop_map(|(l, r)| Tree::Node(Box::new(l), Box::new(r)))
        ]
    })
```

### Configuration API

```rust
use proptest::test_runner::{Config, TestRunner, TestCaseResult};

// Configuration options
let config = Config {
    cases: 1000,                    // Number of test cases
    max_shrink_iters: 10000,       // Maximum shrinking iterations
    max_shrink_time: 60000,        // Maximum shrinking time (ms)
    max_global_rejects: 1000,      // Maximum global rejections
    max_flat_map_regens: 1000000,  // Maximum flat_map regenerations
    failure_persistence: None,      // Failure persistence configuration
    source_file: Some("test.rs"),  // Source file for persistence
    test_name: Some("my_test"),    // Test name for persistence
    fork: false,                   // Enable forking for isolation
    timeout: 1000,                 // Timeout per test case (ms)
    ..Config::default()
};

// Manual test runner
let mut runner = TestRunner::new(config);
runner.run(&any::<i32>(), |x| {
    if x.abs() < 0 {
        Err(TestCaseError::fail("absolute value cannot be negative"))
    } else {
        Ok(())
    }
}).unwrap();
```

### Custom Strategy Implementation

```rust
use proptest::strategy::{Strategy, ValueTree, NewTree};
use proptest::test_runner::TestRunner;

// Custom strategy for email addresses
#[derive(Debug)]
struct EmailStrategy;

impl Strategy for EmailStrategy {
    type Tree = EmailValueTree;
    type Value = String;
    
    fn new_tree(&self, runner: &mut TestRunner) -> NewTree<Self> {
        let local_part = "[a-z]{3,10}".new_tree(runner)?;
        let domain_part = "[a-z]{3,10}".new_tree(runner)?;
        let tld = prop::sample::select(vec!["com", "org", "net"]).new_tree(runner)?;
        
        Ok(EmailValueTree {
            local_part,
            domain_part,
            tld,
        })
    }
}

#[derive(Debug)]
struct EmailValueTree {
    local_part: <&'static str as Strategy>::Tree,
    domain_part: <&'static str as Strategy>::Tree,
    tld: <prop::sample::Select<Vec<&'static str>> as Strategy>::Tree,
}

impl ValueTree for EmailValueTree {
    type Value = String;
    
    fn current(&self) -> Self::Value {
        format!("{}@{}.{}", 
                self.local_part.current(),
                self.domain_part.current(),
                self.tld.current())
    }
    
    fn simplify(&mut self) -> bool {
        self.local_part.simplify() | 
        self.domain_part.simplify() | 
        self.tld.simplify()
    }
    
    fn complicate(&mut self) -> bool {
        self.local_part.complicate() | 
        self.domain_part.complicate() | 
        self.tld.complicate()
    }
}

// Usage
proptest! {
    #[test]
    fn test_email_validation(email in EmailStrategy) {
        prop_assert!(validate_email(&email).is_ok());
        prop_assert!(email.contains('@'));
        prop_assert!(email.contains('.'));
    }
}
```

## Expect-Test API Reference

### Core Macros

#### `expect!`

Create inline snapshots.

```rust
use expect_test::expect;

#[test]
fn inline_snapshot_examples() {
    // Single line
    let result = 2 + 2;
    expect!["4"].assert_eq(&result.to_string());
    
    // Multi-line with raw strings
    let data = vec![1, 2, 3];
    expect![[r#"
        [
            1,
            2,
            3,
        ]
    "#]].assert_debug_eq(&data);
    
    // With custom formatting
    let person = Person { name: "Alice".to_string(), age: 30 };
    expect![[r#"
        Person {
            name: "Alice",
            age: 30,
        }
    "#]].assert_debug_eq(&person);
}
```

#### `expect_file!`

Create file-based snapshots.

```rust
use expect_test::expect_file;

#[test]
fn file_snapshot_examples() {
    // Relative to current file
    let large_output = generate_large_json();
    expect_file!["../test-data/large_output.json"].assert_eq(&large_output);
    
    // With subdirectories
    let ast = parse_complex_expression();
    expect_file!["snapshots/parser/complex_expression.txt"].assert_debug_eq(&ast);
    
    // Binary data (as hex)
    let binary_data = serialize_to_binary(&data);
    let hex_string = binary_data.iter()
        .map(|b| format!("{:02x}", b))
        .collect::<Vec<_>>()
        .join(" ");
    expect_file!["snapshots/binary/data.hex"].assert_eq(&hex_string);
}
```

### Expect Methods

#### `assert_eq`

String equality assertion.

```rust
use expect_test::expect;

#[test]
fn assert_eq_examples() {
    let result = format_output(&data);
    
    // Basic string comparison
    expect!["formatted output"].assert_eq(&result);
    
    // With normalization
    let normalized = normalize_whitespace(&result);
    expect!["normalized output"].assert_eq(&normalized);
}
```

#### `assert_debug_eq`

Debug formatting assertion.

```rust
use expect_test::expect;

#[test]
fn assert_debug_eq_examples() {
    let result = parse_expression("1 + 2");
    
    expect![[r#"
        Ok(
            BinaryOp {
                left: Number(1),
                op: Add,
                right: Number(2),
            },
        )
    "#]].assert_debug_eq(&result);
}
```

### Expect Struct

```rust
use expect_test::{expect, Expect};

// Function that takes Expect parameter
fn check_calculation(input: i32, expected: Expect) {
    let result = input * 2 + 1;
    expected.assert_eq(&result.to_string());
}

#[test]
fn parameterized_tests() {
    check_calculation(0, expect!["1"]);
    check_calculation(5, expect!["11"]);
    check_calculation(-3, expect!["-5"]);
}

// Data-driven testing
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
```

### Environment Variables

```bash
# Update all snapshots
UPDATE_EXPECT=1 cargo test

# Update specific test
UPDATE_EXPECT=1 cargo test test_name

# Verbose output
EXPECT_TEST_VERBOSE=1 cargo test
```

## Pretty Assertions API Reference

### Core Macros

#### `assert_eq!`

Enhanced equality assertion with diff output.

```rust
#[cfg(test)]
use pretty_assertions::assert_eq;

#[test]
fn pretty_assert_eq_examples() {
    // Basic usage
    let actual = vec![1, 2, 3];
    let expected = vec![1, 2, 4];  // Will show diff
    assert_eq!(actual, expected);
    
    // With custom message
    let result = calculate_result();
    assert_eq!(result, expected_result, "Calculation failed for input: {:?}", input);
    
    // Complex structures
    let actual_config = Config { /* ... */ };
    let expected_config = Config { /* ... */ };
    assert_eq!(actual_config, expected_config);
}
```

#### `assert_ne!`

Enhanced inequality assertion.

```rust
#[cfg(test)]
use pretty_assertions::assert_ne;

#[test]
fn pretty_assert_ne_examples() {
    let result1 = process_data(&input1);
    let result2 = process_data(&input2);
    
    assert_ne!(result1, result2, "Results should be different for different inputs");
}
```

### Comparison Struct

```rust
use pretty_assertions::Comparison;

#[test]
fn comparison_examples() {
    let left = vec![1, 2, 3];
    let right = vec![1, 2, 4];
    
    // Create comparison for custom display
    let comparison = Comparison::new(&left, &right);
    println!("{}", comparison);
    
    // Use in custom assertion functions
    fn assert_vectors_similar<T>(actual: &[T], expected: &[T], tolerance: f64)
    where
        T: std::fmt::Debug + PartialEq,
    {
        if actual != expected {
            let comparison = Comparison::new(actual, expected);
            panic!("Vectors differ:\n{}", comparison);
        }
    }
}
```

### Feature Configuration

```toml
# Cargo.toml
[dev-dependencies]
# Standard library support (default)
pretty_assertions = "1.4.1"

# For no_std environments
pretty_assertions = { version = "1.4.1", default-features = false, features = ["alloc"] }

# With unstable features
pretty_assertions = { version = "1.4.1", features = ["unstable"] }
```

## Integration Patterns

### Proptest + Expect-Test

```rust
use proptest::prelude::*;
use expect_test::expect;

proptest! {
    #[test]
    fn property_with_snapshot(input in "[a-z]{1,10}") {
        let result = process_string(&input);
        
        // Property: result should always be uppercase
        prop_assert!(result.chars().all(|c| c.is_uppercase()));
        
        // Snapshot specific cases for regression testing
        if input == "hello" {
            expect!["HELLO"].assert_eq(&result);
        }
    }
}

#[test]
fn snapshot_with_property_validation() {
    let test_cases = vec![
        "simple",
        "complex_case",
        "edge_case_123",
    ];
    
    for case in test_cases {
        let result = parse_and_format(case);
        
        // Validate properties
        assert!(result.is_ok());
        let formatted = result.unwrap();
        assert!(!formatted.is_empty());
        
        // Snapshot the exact output
        let snapshot_name = format!("snapshots/{}.txt", case);
        expect_test::expect_file![snapshot_name].assert_eq(&formatted);
    }
}
```

### Proptest + Pretty Assertions

```rust
use proptest::prelude::*;

#[cfg(test)]
use pretty_assertions::assert_eq;

proptest! {
    #[test]
    fn property_with_pretty_assertions(
        data in prop::collection::vec(any::<i32>(), 0..20)
    ) {
        let processed = process_data(&data);
        let reference = reference_implementation(&data);
        
        // Pretty assertions show clear diffs when property fails
        assert_eq!(processed, reference);
    }
}

// Custom assertion with pretty output
fn assert_parse_results_equal(actual: &ParseResult, expected: &ParseResult) {
    // Use pretty_assertions for better error messages
    assert_eq!(actual.tokens, expected.tokens, "Token lists differ");
    assert_eq!(actual.errors, expected.errors, "Error lists differ");
    assert_eq!(actual.warnings, expected.warnings, "Warning lists differ");
}
```

### All Three Together

```rust
use proptest::prelude::*;
use expect_test::{expect, Expect};

#[cfg(test)]
use pretty_assertions::assert_eq;

struct ComprehensiveTestCase {
    name: &'static str,
    input: &'static str,
    expected_snapshot: Expect,
}

fn run_comprehensive_tests(cases: &[ComprehensiveTestCase]) {
    for case in cases {
        println!("Running comprehensive test: {}", case.name);
        
        // Parse the input
        let parse_result = parse(case.input);
        
        // Property: parsing should always succeed for valid input
        assert!(parse_result.is_ok(), "Parsing failed for case: {}", case.name);
        
        let ast = parse_result.unwrap();
        
        // Property: AST should round-trip correctly
        let regenerated = ast.to_string();
        let reparsed = parse(&regenerated).unwrap();
        assert_eq!(ast, reparsed, "AST round-trip failed for case: {}", case.name);
        
        // Snapshot: exact AST structure
        case.expected_snapshot.assert_debug_eq(&ast);
    }
}

proptest! {
    #[test]
    fn property_based_comprehensive_test(
        input in "[a-zA-Z0-9 ]{1,50}"
    ) {
        if let Ok(ast) = parse(&input) {
            // Property: valid parse should round-trip
            let regenerated = ast.to_string();
            let reparsed = parse(&regenerated);
            
            prop_assert!(reparsed.is_ok());
            let reparsed_ast = reparsed.unwrap();
            
            // Use pretty assertions for clear diff on failure
            assert_eq!(ast, reparsed_ast);
        }
    }
}

#[test]
fn comprehensive_test_suite() {
    let test_cases = vec![
        ComprehensiveTestCase {
            name: "simple_expression",
            input: "1 + 2",
            expected_snapshot: expect![[r#"
                BinaryOp {
                    left: Number(1),
                    op: Add,
                    right: Number(2),
                }
            "#]],
        },
        ComprehensiveTestCase {
            name: "function_call",
            input: "foo(bar, baz)",
            expected_snapshot: expect![[r#"
                FunctionCall {
                    name: "foo",
                    args: [
                        Identifier("bar"),
                        Identifier("baz"),
                    ],
                }
            "#]],
        },
    ];
    
    run_comprehensive_tests(&test_cases);
}
```

## Configuration Reference

### Proptest Configuration

```rust
use proptest::test_runner::Config;

// Complete configuration options
let config = Config {
    // Test execution
    cases: 256,                     // Number of test cases to run
    max_shrink_iters: 1024,        // Maximum shrinking iterations
    max_shrink_time: 0,            // Maximum shrinking time (0 = no limit)
    max_global_rejects: 65536,     // Maximum global rejections
    max_flat_map_regens: 1000000,  // Maximum flat_map regenerations
    
    // Failure handling
    failure_persistence: Some(Box::new(
        proptest::test_runner::FileFailurePersistence::SourceParallel("proptest-regressions")
    )),
    source_file: Some("tests/my_test.rs"),
    test_name: Some("my_property_test"),
    
    // Execution environment
    fork: false,                   // Fork test cases for isolation
    timeout: 0,                    // Timeout per test case (0 = no timeout)
    
    // Random number generation
    rng_algorithm: proptest::test_runner::RngAlgorithm::ChaCha,
    
    // Verbose output
    verbose: 0,                    // Verbosity level
};

// Environment variable configuration
// PROPTEST_CASES=1000
// PROPTEST_MAX_SHRINK_ITERS=10000
// PROPTEST_DISABLE_SHRINKING=1
// PROPTEST_VERBOSE=1
```

### Expect-Test Configuration

```bash
# Environment variables
UPDATE_EXPECT=1              # Update all snapshots
EXPECT_TEST_VERBOSE=1        # Verbose output
```

### Pretty Assertions Configuration

```rust
// Conditional compilation
#[cfg(test)]
use pretty_assertions::{assert_eq, assert_ne};

#[cfg(not(test))]
use std::{assert_eq, assert_ne};

// Feature flags in Cargo.toml
// pretty_assertions = { version = "1.4.1", default-features = false, features = ["alloc"] }
```

## Error Handling Reference

### Proptest Error Types

```rust
use proptest::test_runner::{TestCaseError, TestCaseResult};

// Creating test case errors
fn validate_input(x: i32) -> TestCaseResult {
    if x < 0 {
        Err(TestCaseError::fail("input must be non-negative"))
    } else if x > 1000 {
        Err(TestCaseError::reject("input too large"))
    } else {
        Ok(())
    }
}

// Error types
TestCaseError::fail("assertion failed")     // Test failure
TestCaseError::reject("invalid input")      // Reject this input
TestCaseError::Fail { .. }                  // Detailed failure info
TestCaseError::Reject { .. }                // Detailed rejection info
```

### Expect-Test Error Handling

```rust
use expect_test::expect;

#[test]
fn error_handling_examples() {
    // Handle parsing errors
    let result = parse_with_error_recovery("invalid input");
    
    match result {
        Ok(ast) => {
            expect![[r#"
                ValidAst { .. }
            "#]].assert_debug_eq(&ast);
        }
        Err(errors) => {
            expect![[r#"
                [
                    ParseError {
                        message: "Unexpected token",
                        location: Location { line: 1, column: 8 },
                    },
                ]
            "#]].assert_debug_eq(&errors);
        }
    }
}
```

### Pretty Assertions Error Output

```rust
#[cfg(test)]
use pretty_assertions::assert_eq;

#[test]
fn error_output_examples() {
    // This will produce colorful diff output
    let actual = ComplexStruct {
        field1: "value1",
        field2: vec![1, 2, 3],
        nested: NestedStruct {
            inner: 42,
            flag: true,
        },
    };
    
    let expected = ComplexStruct {
        field1: "value1",
        field2: vec![1, 2, 4],  // Different
        nested: NestedStruct {
            inner: 42,
            flag: false,  // Different
        },
    };
    
    // Shows exactly which fields differ with colors
    assert_eq!(actual, expected);
}
```

---

This comprehensive API reference provides detailed coverage of all three testing libraries with practical examples. Use it as a quick reference when implementing tests or exploring advanced features.