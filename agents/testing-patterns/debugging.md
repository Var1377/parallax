# Shrinking and Debugging Test Failures

Understanding how to debug test failures, especially in property-based testing, is crucial for effective testing. This guide covers shrinking mechanisms, debugging strategies, and failure analysis techniques.

## Table of Contents

- [Understanding Shrinking](#understanding-shrinking)
- [Debugging Property Test Failures](#debugging-property-test-failures)
- [Analyzing Snapshot Test Failures](#analyzing-snapshot-test-failures)
- [Debugging Complex Assertions](#debugging-complex-assertions)
- [Failure Reproduction](#failure-reproduction)
- [Advanced Debugging Techniques](#advanced-debugging-techniques)
- [Performance Debugging](#performance-debugging)
- [Common Debugging Patterns](#common-debugging-patterns)

## Understanding Shrinking

Shrinking is the process of automatically reducing failing test inputs to their minimal form, making it easier to understand and fix the underlying issue.

### How Shrinking Works

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_with_shrinking_example(x in 0..1000i32) {
        // This will fail when x >= 42
        prop_assert!(x < 42, "Value {} should be less than 42", x);
    }
}
```

**Shrinking Process:**
1. **Initial Failure**: Test fails with `x = 789`
2. **Binary Search**: Proptest tries smaller values
3. **Minimal Case**: Eventually finds `x = 42` as the minimal failing case
4. **Persistence**: Saves the minimal case for regression testing

### Shrinking Output Analysis

```
Test failed: assertion failed: `x < 42` at line 5
minimal failing input: x = 42
successes: 41
local rejects: 0
global rejects: 0
```

**Understanding the Output:**
- **minimal failing input**: The smallest input that causes failure
- **successes**: Number of test cases that passed before finding failure
- **local rejects**: Inputs rejected by `prop_filter`
- **global rejects**: Inputs rejected by `prop_assume`

### Custom Shrinking Strategies

```rust
use proptest::strategy::{Strategy, Just};
use proptest::arbitrary::Arbitrary;

#[derive(Debug, Clone)]
struct Point {
    x: i32,
    y: i32,
}

// Custom strategy with better shrinking behavior
fn point_strategy() -> impl Strategy<Value = Point> {
    (any::<i32>(), any::<i32>())
        .prop_map(|(x, y)| Point { x, y })
        // This will shrink both coordinates independently
}

// Alternative: Custom shrinking for specific patterns
fn centered_point_strategy() -> impl Strategy<Value = Point> {
    (-100..100i32, -100..100i32)
        .prop_map(|(x, y)| Point { x, y })
        // Shrinks towards (0, 0)
}

proptest! {
    #[test]
    fn test_point_distance(point in point_strategy()) {
        let distance = ((point.x * point.x + point.y * point.y) as f64).sqrt();
        
        // Add debug output to understand shrinking
        println!("Testing point: {:?}, distance: {}", point, distance);
        
        // This will fail for points far from origin
        prop_assert!(distance < 50.0, 
                    "Point {:?} is too far from origin (distance: {})", 
                    point, distance);
    }
}
```

## Debugging Property Test Failures

### 1. Adding Debug Output

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn debug_sorting_algorithm(mut data in prop::collection::vec(any::<i32>(), 0..100)) {
        println!("Original data: {:?}", data);
        
        let original_len = data.len();
        let original_sum: i64 = data.iter().map(|&x| x as i64).sum();
        
        // Sort the data
        data.sort();
        
        println!("Sorted data: {:?}", data);
        println!("Length: {} -> {}", original_len, data.len());
        println!("Sum: {}", original_sum);
        
        // Check properties
        prop_assert_eq!(data.len(), original_len, "Length should be preserved");
        
        // Check if sorted
        for i in 1..data.len() {
            prop_assert!(data[i-1] <= data[i], 
                        "Array not sorted at index {}: {} > {}", 
                        i, data[i-1], data[i]);
        }
        
        // Check sum preservation
        let new_sum: i64 = data.iter().map(|&x| x as i64).sum();
        prop_assert_eq!(original_sum, new_sum, "Sum should be preserved");
    }
}
```

### 2. Conditional Debugging

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn conditional_debug_example(
        data in prop::collection::vec(any::<i32>(), 0..50)
    ) {
        // Only debug when we have interesting cases
        if data.len() > 10 && data.iter().any(|&x| x.abs() > 1000) {
            println!("Debugging large/extreme case: {:?}", data);
        }
        
        let result = process_data(&data);
        
        // Add context to assertions
        prop_assert!(result.is_valid(), 
                    "Result should be valid for input: {:?}, got: {:?}", 
                    data, result);
    }
}
```

### 3. Incremental Debugging

```rust
use proptest::prelude::*;

#[derive(Debug)]
struct ProcessingState {
    step: usize,
    intermediate_results: Vec<i32>,
    final_result: Option<i32>,
}

fn debug_process_data(data: &[i32]) -> ProcessingState {
    let mut state = ProcessingState {
        step: 0,
        intermediate_results: Vec::new(),
        final_result: None,
    };
    
    for &value in data {
        state.step += 1;
        let processed = value * 2;
        state.intermediate_results.push(processed);
        
        println!("Step {}: {} -> {}", state.step, value, processed);
    }
    
    state.final_result = Some(state.intermediate_results.iter().sum());
    state
}

proptest! {
    #[test]
    fn test_with_state_tracking(data in prop::collection::vec(any::<i32>(), 0..20)) {
        let state = debug_process_data(&data);
        
        prop_assert_eq!(state.intermediate_results.len(), data.len());
        
        if let Some(final_result) = state.final_result {
            let expected: i32 = data.iter().map(|&x| x * 2).sum();
            prop_assert_eq!(final_result, expected, 
                           "Final result mismatch. State: {:?}", state);
        }
    }
}
```

### 4. Bisection Debugging

```rust
use proptest::prelude::*;

// When you have a complex failing case, bisect to find the problematic part
proptest! {
    #[test]
    fn bisection_debug_example(
        operations in prop::collection::vec(
            prop::sample::select(vec!["add", "multiply", "divide"]),
            0..100
        )
    ) {
        let mut result = 1.0;
        let mut operation_count = 0;
        
        for (i, op) in operations.iter().enumerate() {
            operation_count += 1;
            
            match op.as_str() {
                "add" => result += 1.0,
                "multiply" => result *= 2.0,
                "divide" => {
                    if result != 0.0 {
                        result /= 2.0;
                    }
                }
                _ => unreachable!(),
            }
            
            // Check invariants at each step
            prop_assert!(result.is_finite(), 
                        "Result became non-finite at step {} (operation: {}): {}", 
                        i, op, result);
            
            // Add bounds checking
            prop_assert!(result.abs() < 1e10, 
                        "Result too large at step {} (operation: {}): {}", 
                        i, op, result);
        }
    }
}
```

## Analyzing Snapshot Test Failures

### 1. Understanding Snapshot Diffs

```rust
use expect_test::expect;

#[derive(Debug)]
struct ParseResult {
    tokens: Vec<String>,
    errors: Vec<String>,
}

#[test]
fn analyze_snapshot_failure() {
    let input = "fn main() { let x = 42 }";  // Missing semicolon
    let result = parse_code(input);
    
    // When this fails, examine the diff carefully
    expect![[r#"
        ParseResult {
            tokens: [
                "fn",
                "main",
                "(",
                ")",
                "{",
                "let",
                "x",
                "=",
                "42",
                "}",
            ],
            errors: [
                "Expected ';' after expression at line 1, column 19",
            ],
        }
    "#]].assert_debug_eq(&result);
}
```

**Analyzing the Diff:**
- **Added lines**: New tokens or errors that weren't expected
- **Removed lines**: Expected tokens or errors that are missing
- **Changed lines**: Tokens or errors that have different values
- **Context**: Surrounding unchanged lines help locate the issue

### 2. Debugging Snapshot Updates

```bash
# Run with UPDATE_EXPECT to see what changed
UPDATE_EXPECT=1 cargo test analyze_snapshot_failure

# Then examine the diff
git diff
```

```rust
// Before update
expect![[r#"
    ParseResult {
        tokens: ["fn", "main"],
        errors: [],
    }
"#]].assert_debug_eq(&result);

// After update (what actually happened)
expect![[r#"
    ParseResult {
        tokens: [
            "fn",
            "main",
            "(",
            ")",
            "{",
            "let",
            "x",
            "=",
            "42",
            "}",
        ],
        errors: [
            "Expected ';' after expression at line 1, column 19",
        ],
    }
"#]].assert_debug_eq(&result);
```

### 3. Normalizing Non-Deterministic Output

```rust
use expect_test::expect;
use regex::Regex;

fn normalize_output(output: &str) -> String {
    let timestamp_re = Regex::new(r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}").unwrap();
    let memory_re = Regex::new(r"0x[0-9a-f]+").unwrap();
    
    let normalized = timestamp_re.replace_all(output, "TIMESTAMP");
    let normalized = memory_re.replace_all(&normalized, "MEMORY_ADDR");
    normalized.to_string()
}

#[test]
fn test_with_normalization() {
    let output = generate_debug_output();
    let normalized = normalize_output(&output);
    
    expect![[r#"
        Debug output at TIMESTAMP:
        Object at MEMORY_ADDR: { value: 42 }
        Processing completed
    "#]].assert_eq(&normalized);
}
```

## Debugging Complex Assertions

### 1. Breaking Down Complex Assertions

```rust
#[cfg(test)]
use pretty_assertions::assert_eq;

#[test]
fn debug_complex_assertion() {
    let actual = ComplexStruct {
        field1: "value1".to_string(),
        field2: vec![1, 2, 3],
        field3: NestedStruct {
            inner: 42,
            flag: true,
        },
    };
    
    let expected = ComplexStruct {
        field1: "value1".to_string(),
        field2: vec![1, 2, 4],  // Different!
        field3: NestedStruct {
            inner: 42,
            flag: false,  // Different!
        },
    };
    
    // Instead of one big assertion, break it down:
    
    // Check each field separately for better error messages
    assert_eq!(actual.field1, expected.field1, "field1 mismatch");
    assert_eq!(actual.field2, expected.field2, "field2 mismatch");
    assert_eq!(actual.field3.inner, expected.field3.inner, "field3.inner mismatch");
    assert_eq!(actual.field3.flag, expected.field3.flag, "field3.flag mismatch");
    
    // Or use the full comparison for the complete diff
    assert_eq!(actual, expected);
}
```

### 2. Custom Debug Implementations

```rust
use std::fmt;

#[derive(PartialEq)]
struct Matrix {
    data: Vec<Vec<i32>>,
}

impl fmt::Debug for Matrix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Matrix {{")?;
        for (i, row) in self.data.iter().enumerate() {
            write!(f, "  row {}: [", i)?;
            for (j, &val) in row.iter().enumerate() {
                if j > 0 { write!(f, ", ")?; }
                write!(f, "{:3}", val)?;
            }
            writeln!(f, "]")?;
        }
        write!(f, "}}")
    }
}

#[test]
fn test_matrix_with_custom_debug() {
    let actual = Matrix {
        data: vec![
            vec![1, 2, 3],
            vec![4, 5, 6],
        ],
    };
    
    let expected = Matrix {
        data: vec![
            vec![1, 2, 3],
            vec![4, 0, 6],  // Different value
        ],
    };
    
    // Custom Debug makes the diff much clearer
    assert_eq!(actual, expected);
}
```

### 3. Assertion Helpers

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

fn assert_float_eq(actual: f64, expected: f64, epsilon: f64) {
    assert!(
        (actual - expected).abs() < epsilon,
        "Float values not equal: actual={}, expected={}, diff={}",
        actual, expected, (actual - expected).abs()
    );
}

fn assert_contains_error(result: &Result<(), String>, expected_error: &str) {
    match result {
        Ok(_) => panic!("Expected error containing '{}', but got Ok", expected_error),
        Err(actual_error) => assert!(
            actual_error.contains(expected_error),
            "Expected error containing '{}', but got '{}'",
            expected_error, actual_error
        ),
    }
}

#[test]
fn test_with_helpers() {
    let actual_vec = vec![3, 1, 4, 1, 5];
    let expected_vec = vec![1, 1, 3, 4, 5];
    assert_vectors_equal_unordered(actual_vec, expected_vec);
    
    let actual_float = 3.14159;
    let expected_float = 3.14160;
    assert_float_eq(actual_float, expected_float, 0.001);
    
    let result = validate_input("invalid");
    assert_contains_error(&result, "must be alphanumeric");
}
```

## Failure Reproduction

### 1. Reproducing Property Test Failures

```rust
use proptest::prelude::*;
use proptest::test_runner::TestRunner;

// When a property test fails, you get a seed
// Use it to reproduce the exact failure
#[test]
fn reproduce_specific_failure() {
    let mut runner = TestRunner::new(
        ProptestConfig {
            // Use the seed from the failure output
            rng_algorithm: proptest::test_runner::RngAlgorithm::ChaCha,
            // Set other config as needed
            ..ProptestConfig::default()
        }
    );
    
    // Set the specific seed that caused the failure
    runner.set_seed(0x1234567890abcdef);
    
    runner.run(&any::<i32>(), |x| {
        // Your failing property test
        prop_assert!(x < 42);
        Ok(())
    }).unwrap_err(); // We expect this to fail
}

// Alternative: Use the exact failing input
#[test]
fn test_specific_failing_case() {
    let failing_input = 42; // From the shrinking output
    
    // Test the specific case that failed
    assert!(failing_input < 42); // This will fail, showing the exact issue
}
```

### 2. Regression Test Creation

```rust
// After fixing a bug, create a regression test
#[test]
fn regression_test_issue_123() {
    // This is the exact input that caused the original failure
    let problematic_input = vec![42, -1, 0, 999];
    
    // This should now pass after the fix
    let result = process_data(&problematic_input);
    assert!(result.is_ok(), "Issue #123 regression: {:?}", result);
}

// For property tests, add the failing case to regular tests
proptest! {
    #[test]
    fn property_test_with_regression_protection(
        data in prop::collection::vec(any::<i32>(), 0..100)
    ) {
        let result = process_data(&data);
        prop_assert!(result.is_ok());
    }
}

#[test]
fn regression_test_property_failure() {
    // Specific case that failed in property testing
    let failing_case = vec![i32::MIN, i32::MAX];
    let result = process_data(&failing_case);
    assert!(result.is_ok(), "Regression: property test failure case");
}
```

## Advanced Debugging Techniques

### 1. Debugging with GDB/LLDB

```rust
// Add debug symbols and disable optimizations for debugging
// Cargo.toml:
// [profile.test]
// debug = true
// opt-level = 0

#[test]
fn debug_with_gdb() {
    let data = vec![1, 2, 3, 4, 5];
    
    // Set a breakpoint here in GDB: break debug_with_gdb
    let result = complex_function(&data);
    
    assert_eq!(result, expected_value);
}

// Run with: rust-gdb target/debug/deps/my_test-<hash>
// Or: rust-lldb target/debug/deps/my_test-<hash>
```

### 2. Memory Debugging

```rust
#[test]
fn debug_memory_issues() {
    // Use tools like valgrind, AddressSanitizer
    let mut data = Vec::with_capacity(1000);
    
    for i in 0..1000 {
        data.push(i);
    }
    
    // This might reveal memory issues
    let result = process_large_data(&data);
    assert!(result.is_ok());
}

// Run with: RUSTFLAGS="-Z sanitizer=address" cargo test
```

### 3. Logging and Tracing

```rust
use log::{debug, info, warn, error};

#[test]
fn test_with_logging() {
    env_logger::init();
    
    info!("Starting test with logging");
    
    let input = "test input";
    debug!("Processing input: {}", input);
    
    let result = process_input(input);
    
    match result {
        Ok(output) => {
            info!("Processing succeeded: {}", output);
            assert_eq!(output, "expected output");
        }
        Err(e) => {
            error!("Processing failed: {}", e);
            panic!("Unexpected error: {}", e);
        }
    }
}

// Run with: RUST_LOG=debug cargo test
```

## Performance Debugging

### 1. Identifying Slow Tests

```rust
use std::time::{Duration, Instant};

#[test]
fn performance_debug_test() {
    let start = Instant::now();
    
    let result = expensive_operation();
    
    let duration = start.elapsed();
    println!("Operation took: {:?}", duration);
    
    // Fail if too slow
    assert!(duration < Duration::from_millis(100), 
           "Operation too slow: {:?}", duration);
    
    assert!(result.is_ok());
}
```

### 2. Memory Usage Debugging

```rust
#[test]
fn memory_usage_debug() {
    let initial_memory = get_memory_usage();
    
    let large_data = create_large_data_structure();
    let after_creation = get_memory_usage();
    
    process_data(&large_data);
    let after_processing = get_memory_usage();
    
    drop(large_data);
    let after_drop = get_memory_usage();
    
    println!("Memory usage:");
    println!("  Initial: {} bytes", initial_memory);
    println!("  After creation: {} bytes (+{})", 
             after_creation, after_creation - initial_memory);
    println!("  After processing: {} bytes (+{})", 
             after_processing, after_processing - after_creation);
    println!("  After drop: {} bytes ({})", 
             after_drop, after_drop as i64 - initial_memory as i64);
    
    // Assert memory was properly freed
    assert!(after_drop <= initial_memory + 1000, "Memory leak detected");
}

#[cfg(target_os = "linux")]
fn get_memory_usage() -> usize {
    // Read from /proc/self/status
    let status = std::fs::read_to_string("/proc/self/status").unwrap();
    for line in status.lines() {
        if line.starts_with("VmRSS:") {
            let parts: Vec<&str> = line.split_whitespace().collect();
            return parts[1].parse::<usize>().unwrap() * 1024; // Convert KB to bytes
        }
    }
    0
}

#[cfg(not(target_os = "linux"))]
fn get_memory_usage() -> usize {
    0 // Placeholder for other platforms
}
```

## Common Debugging Patterns

### 1. The "Minimal Reproduction" Pattern

```rust
// Start with the failing case and remove complexity until you find the root cause

#[test]
fn minimal_reproduction() {
    // Original failing case (complex)
    let complex_input = ComplexInput {
        field1: vec![1, 2, 3, 4, 5],
        field2: "complex string with special chars: !@#$%",
        field3: Some(NestedStruct { /* ... */ }),
    };
    
    // Simplified case 1: Remove field3
    let simpler_input = ComplexInput {
        field1: vec![1, 2, 3, 4, 5],
        field2: "complex string with special chars: !@#$%",
        field3: None,
    };
    
    // Simplified case 2: Simplify field2
    let even_simpler = ComplexInput {
        field1: vec![1, 2, 3, 4, 5],
        field2: "simple",
        field3: None,
    };
    
    // Minimal case: Single element
    let minimal = ComplexInput {
        field1: vec![1],
        field2: "",
        field3: None,
    };
    
    // Test each level to find where it breaks
    assert!(process_input(&minimal).is_ok());
    assert!(process_input(&even_simpler).is_ok());
    assert!(process_input(&simpler_input).is_ok());
    assert!(process_input(&complex_input).is_ok()); // This might fail
}
```

### 2. The "State Inspection" Pattern

```rust
#[derive(Debug)]
struct ProcessingState {
    current_step: String,
    intermediate_values: Vec<i32>,
    flags: std::collections::HashMap<String, bool>,
}

fn debug_process_with_state(input: &[i32]) -> (Result<i32, String>, ProcessingState) {
    let mut state = ProcessingState {
        current_step: "initialization".to_string(),
        intermediate_values: Vec::new(),
        flags: std::collections::HashMap::new(),
    };
    
    state.flags.insert("started".to_string(), true);
    
    for &value in input {
        state.current_step = format!("processing value {}", value);
        state.intermediate_values.push(value * 2);
        
        if value < 0 {
            state.flags.insert("negative_found".to_string(), true);
        }
    }
    
    state.current_step = "finalization".to_string();
    let result = state.intermediate_values.iter().sum();
    
    (Ok(result), state)
}

#[test]
fn test_with_state_inspection() {
    let input = vec![1, -2, 3];
    let (result, state) = debug_process_with_state(&input);
    
    println!("Final state: {:?}", state);
    
    match result {
        Ok(value) => assert_eq!(value, 4), // 2 + (-4) + 6 = 4
        Err(e) => panic!("Unexpected error: {}. State: {:?}", e, state),
    }
}
```

### 3. The "Comparative Testing" Pattern

```rust
// Compare against a known-good implementation
fn reference_implementation(input: &[i32]) -> i32 {
    // Simple, obviously correct implementation
    input.iter().map(|&x| x * 2).sum()
}

fn optimized_implementation(input: &[i32]) -> i32 {
    // Complex, optimized implementation that might have bugs
    let mut sum = 0;
    for &x in input {
        sum += x << 1; // Bit shift instead of multiplication
    }
    sum
}

#[test]
fn comparative_test() {
    let test_cases = vec![
        vec![],
        vec![1],
        vec![1, 2, 3],
        vec![-1, -2, -3],
        vec![i32::MAX / 2], // Edge case
        vec![i32::MIN / 2], // Edge case
    ];
    
    for (i, case) in test_cases.iter().enumerate() {
        let reference_result = reference_implementation(case);
        let optimized_result = optimized_implementation(case);
        
        assert_eq!(
            reference_result, 
            optimized_result,
            "Mismatch in test case {}: input={:?}", 
            i, case
        );
    }
}
```

---

Effective debugging is essential for maintaining robust test suites. By understanding shrinking mechanisms, using appropriate debugging techniques, and following systematic debugging patterns, you can quickly identify and fix issues in your code and tests.