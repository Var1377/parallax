# Proptest Complete Guide

Proptest is Rust's premier property-based testing framework, inspired by QuickCheck and Hypothesis. This comprehensive guide covers everything from basic usage to advanced patterns and optimization techniques.

## Table of Contents

- [Installation and Setup](#installation-and-setup)
- [Basic Usage](#basic-usage)
- [Strategies Deep Dive](#strategies-deep-dive)
- [Advanced Patterns](#advanced-patterns)
- [Configuration and Tuning](#configuration-and-tuning)
- [Debugging and Shrinking](#debugging-and-shrinking)
- [Integration Patterns](#integration-patterns)
- [Performance Optimization](#performance-optimization)
- [Common Pitfalls](#common-pitfalls)

## Installation and Setup

### Cargo.toml Configuration

```toml
[dev-dependencies]
proptest = "1.6.0"

# Optional: For derive macros
proptest-derive = "0.5.0"

# Optional: For state machine testing
proptest-state-machine = "0.2.0"
```

### Basic Import Pattern

```rust
use proptest::prelude::*;

// For more control, import specific modules
use proptest::{
    strategy::{Strategy, Just},
    test_runner::{Config, TestRunner},
    prop_assert, prop_assert_eq, prop_assume,
};
```

## Basic Usage

### The proptest! Macro

The `proptest!` macro is the primary interface for property-based tests:

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_addition_commutative(a in any::<i32>(), b in any::<i32>()) {
        prop_assert_eq!(a + b, b + a);
    }
}
```

### Manual Test Runner

For more control, use the test runner directly:

```rust
use proptest::test_runner::TestRunner;

#[test]
fn manual_test_runner() {
    let mut runner = TestRunner::default();
    
    runner.run(&(any::<i32>(), any::<i32>()), |(a, b)| {
        assert_eq!(a + b, b + a);
        Ok(())
    }).unwrap();
}
```

### Assertion Macros

Proptest provides specialized assertion macros:

```rust
proptest! {
    #[test]
    fn assertion_examples(x in 0..100i32, y in 0..100i32) {
        // Basic assertion
        prop_assert!(x >= 0);
        
        // Equality assertion with better error messages
        prop_assert_eq!(x + y, y + x);
        
        // Inequality assertion
        prop_assert_ne!(x, x + 1);
        
        // Conditional testing with assumptions
        prop_assume!(y != 0);
        prop_assert!((x / y) * y + (x % y) == x);
    }
}
```

## Strategies Deep Dive

Strategies are the heart of proptest - they define how test inputs are generated.

### Primitive Strategies

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn primitive_strategies(
        // Any value of the type
        any_int in any::<i32>(),
        
        // Range of values
        small_int in 0..100i32,
        
        // Inclusive range
        inclusive_range in 0..=100i32,
        
        // Boolean values
        flag in any::<bool>(),
        
        // Characters
        letter in 'a'..'z',
        
        // Floating point
        ratio in 0.0..1.0f64,
    ) {
        prop_assert!(small_int >= 0 && small_int < 100);
        prop_assert!(inclusive_range >= 0 && inclusive_range <= 100);
        prop_assert!(letter >= 'a' && letter <= 'z');
        prop_assert!(ratio >= 0.0 && ratio < 1.0);
    }
}
```

### String Strategies

```rust
proptest! {
    #[test]
    fn string_strategies(
        // Any string
        any_string in any::<String>(),
        
        // Regex-based generation
        email in r"[a-z]{3,10}@[a-z]{3,10}\.[a-z]{2,4}",
        
        // Fixed-length strings
        fixed_len in prop::string::string_regex("[a-zA-Z]{10}").unwrap(),
        
        // ASCII strings
        ascii_str in "[ -~]*",
        
        // Specific character sets
        hex_string in "[0-9a-fA-F]{8}",
    ) {
        prop_assert!(email.contains('@'));
        prop_assert_eq!(fixed_len.len(), 10);
        prop_assert!(hex_string.chars().all(|c| c.is_ascii_hexdigit()));
    }
}
```

### Collection Strategies

```rust
use proptest::collection::*;

proptest! {
    #[test]
    fn collection_strategies(
        // Vectors with size constraints
        small_vec in vec(any::<i32>(), 0..10),
        
        // Hash sets
        unique_numbers in hash_set(1..100i32, 0..20),
        
        // Hash maps
        mapping in hash_map("[a-z]+", 0..100i32, 0..10),
        
        // B-tree collections (ordered)
        ordered_set in btree_set(any::<i32>(), 0..15),
        
        // Arrays (fixed size)
        fixed_array in prop::array::uniform32(any::<u8>()),
    ) {
        prop_assert!(small_vec.len() < 10);
        prop_assert!(unique_numbers.len() <= 20);
        prop_assert!(mapping.len() <= 10);
        prop_assert_eq!(fixed_array.len(), 32);
    }
}
```

### Option and Result Strategies

```rust
proptest! {
    #[test]
    fn option_result_strategies(
        // Optional values
        maybe_int in prop::option::of(any::<i32>()),
        
        // Weighted optional (90% Some, 10% None)
        likely_some in prop::option::weighted(0.9, any::<i32>()),
        
        // Result types
        result_val in prop::result::maybe_ok(any::<i32>(), "error"),
        
        // Weighted results
        likely_ok in prop::result::weighted(0.8, any::<i32>(), "error"),
    ) {
        match maybe_int {
            Some(val) => prop_assert!(val.abs() >= 0),
            None => {}
        }
        
        match result_val {
            Ok(val) => prop_assert!(val.abs() >= 0),
            Err(e) => prop_assert_eq!(e, "error"),
        }
    }
}
```

### Custom Strategies with prop_map

Transform existing strategies to create new ones:

```rust
#[derive(Debug, Clone, PartialEq)]
struct Point {
    x: f64,
    y: f64,
}

fn point_strategy() -> impl Strategy<Value = Point> {
    (-100.0..100.0f64, -100.0..100.0f64)
        .prop_map(|(x, y)| Point { x, y })
}

#[derive(Debug, Clone, PartialEq)]
struct Circle {
    center: Point,
    radius: f64,
}

fn circle_strategy() -> impl Strategy<Value = Circle> {
    (point_strategy(), 1.0..50.0f64)
        .prop_map(|(center, radius)| Circle { center, radius })
}

proptest! {
    #[test]
    fn test_circle_properties(circle in circle_strategy()) {
        prop_assert!(circle.radius > 0.0);
        prop_assert!(circle.center.x.abs() <= 100.0);
        prop_assert!(circle.center.y.abs() <= 100.0);
    }
}
```

### Conditional Strategies with prop_filter

Filter generated values based on conditions:

```rust
fn even_number_strategy() -> impl Strategy<Value = i32> {
    any::<i32>().prop_filter("must be even", |x| x % 2 == 0)
}

fn prime_strategy() -> impl Strategy<Value = u32> {
    (2u32..1000).prop_filter("must be prime", |&x| is_prime(x))
}

fn is_prime(n: u32) -> bool {
    if n < 2 { return false; }
    for i in 2..=(n as f64).sqrt() as u32 {
        if n % i == 0 { return false; }
    }
    true
}

proptest! {
    #[test]
    fn test_filtered_strategies(
        even in even_number_strategy(),
        prime in prime_strategy(),
    ) {
        prop_assert_eq!(even % 2, 0);
        prop_assert!(is_prime(prime));
    }
}
```

### Recursive Strategies

Generate recursive data structures like trees:

```rust
#[derive(Debug, Clone, PartialEq)]
enum Tree {
    Leaf(i32),
    Node(Box<Tree>, Box<Tree>),
}

fn tree_strategy() -> impl Strategy<Value = Tree> {
    let leaf = any::<i32>().prop_map(Tree::Leaf);
    leaf.prop_recursive(
        8,   // Maximum depth
        256, // Maximum number of nodes
        10,  // Items per collection
        |inner| {
            (inner.clone(), inner)
                .prop_map(|(left, right)| Tree::Node(Box::new(left), Box::new(right)))
        },
    )
}

fn tree_size(tree: &Tree) -> usize {
    match tree {
        Tree::Leaf(_) => 1,
        Tree::Node(left, right) => 1 + tree_size(left) + tree_size(right),
    }
}

proptest! {
    #[test]
    fn test_tree_properties(tree in tree_strategy()) {
        let size = tree_size(&tree);
        prop_assert!(size >= 1);
        prop_assert!(size <= 256);
    }
}
```

## Advanced Patterns

### State Machine Testing

Test stateful systems by generating sequences of operations:

```rust
use proptest::prelude::*;

#[derive(Debug, Clone)]
enum StackOp {
    Push(i32),
    Pop,
    Peek,
}

fn stack_op_strategy() -> impl Strategy<Value = StackOp> {
    prop_oneof![
        any::<i32>().prop_map(StackOp::Push),
        Just(StackOp::Pop),
        Just(StackOp::Peek),
    ]
}

proptest! {
    #[test]
    fn test_stack_operations(
        ops in prop::collection::vec(stack_op_strategy(), 0..100)
    ) {
        let mut stack = Vec::new();
        let mut model = Vec::new();
        
        for op in ops {
            match op {
                StackOp::Push(val) => {
                    stack.push(val);
                    model.push(val);
                }
                StackOp::Pop => {
                    let stack_result = stack.pop();
                    let model_result = model.pop();
                    prop_assert_eq!(stack_result, model_result);
                }
                StackOp::Peek => {
                    let stack_result = stack.last().copied();
                    let model_result = model.last().copied();
                    prop_assert_eq!(stack_result, model_result);
                }
            }
        }
    }
}
```

### Metamorphic Testing

Test relationships between different inputs:

```rust
fn sort_algorithm(mut data: Vec<i32>) -> Vec<i32> {
    data.sort();
    data
}

proptest! {
    #[test]
    fn test_sort_metamorphic_properties(
        data in prop::collection::vec(any::<i32>(), 0..100),
        extra_element in any::<i32>(),
    ) {
        let sorted1 = sort_algorithm(data.clone());
        
        // Adding an element and sorting should give same result as
        // sorting and inserting in correct position
        let mut data_with_extra = data;
        data_with_extra.push(extra_element);
        let sorted2 = sort_algorithm(data_with_extra);
        
        // Verify the relationship
        prop_assert_eq!(sorted2.len(), sorted1.len() + 1);
        
        // All elements from sorted1 should be in sorted2
        for &elem in &sorted1 {
            prop_assert!(sorted2.contains(&elem));
        }
        
        // sorted2 should contain extra_element
        prop_assert!(sorted2.contains(&extra_element));
    }
}
```

### Oracle Testing

Compare against a reference implementation:

```rust
// Reference implementation (simple but correct)
fn reference_gcd(mut a: u64, mut b: u64) -> u64 {
    while b != 0 {
        let temp = b;
        b = a % b;
        a = temp;
    }
    a
}

// Optimized implementation to test
fn optimized_gcd(mut a: u64, mut b: u64) -> u64 {
    if a == 0 { return b; }
    if b == 0 { return a; }
    
    let shift = (a | b).trailing_zeros();
    a >>= shift;
    b >>= shift;
    
    while a != b {
        if a > b {
            a -= b;
        } else {
            b -= a;
        }
    }
    
    a << shift
}

proptest! {
    #[test]
    fn test_gcd_oracle(a in 1u64..1000, b in 1u64..1000) {
        let reference_result = reference_gcd(a, b);
        let optimized_result = optimized_gcd(a, b);
        prop_assert_eq!(reference_result, optimized_result);
    }
}
```

### Invariant Testing

Test that invariants hold across operations:

```rust
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct Cache {
    data: HashMap<String, String>,
    max_size: usize,
}

impl Cache {
    fn new(max_size: usize) -> Self {
        Cache {
            data: HashMap::new(),
            max_size,
        }
    }
    
    fn insert(&mut self, key: String, value: String) {
        if self.data.len() >= self.max_size && !self.data.contains_key(&key) {
            // Remove a random element (simplified eviction)
            if let Some(k) = self.data.keys().next().cloned() {
                self.data.remove(&k);
            }
        }
        self.data.insert(key, value);
    }
    
    fn get(&self, key: &str) -> Option<&String> {
        self.data.get(key)
    }
}

proptest! {
    #[test]
    fn test_cache_invariants(
        operations in prop::collection::vec(
            ("[a-z]{1,5}", "[a-z]{1,10}"),
            0..50
        )
    ) {
        let mut cache = Cache::new(10);
        
        for (key, value) in operations {
            cache.insert(key.clone(), value.clone());
            
            // Invariant 1: Cache never exceeds max size
            prop_assert!(cache.data.len() <= cache.max_size);
            
            // Invariant 2: If we just inserted, the key should be present
            prop_assert!(cache.get(&key).is_some());
            
            // Invariant 3: Retrieved value should match inserted value
            prop_assert_eq!(cache.get(&key), Some(&value));
        }
    }
}
```

## Configuration and Tuning

### Test Configuration

```rust
use proptest::test_runner::{Config, TestRunner};

proptest! {
    // Configure for this specific test block
    #![proptest_config(ProptestConfig {
        cases: 1000,                    // Number of test cases
        max_shrink_iters: 10000,       // Maximum shrinking iterations
        max_shrink_time: 60000,        // Maximum shrinking time (ms)
        max_global_rejects: 1000,      // Maximum global rejections
        failure_persistence: None,      // Disable failure persistence
        source_file: Some("test.rs"),  // Source file for persistence
        test_name: Some("my_test"),    // Test name for persistence
        fork: false,                   // Disable forking
        timeout: 1000,                 // Timeout per test case (ms)
        max_flat_map_regens: 1000000,  // Maximum flat_map regenerations
    })]
    
    #[test]
    fn configured_test(x in any::<i32>()) {
        prop_assert!(x.abs() >= 0);
    }
}
```

### Global Configuration

```rust
// In your test module
use proptest::test_runner::Config;

fn custom_config() -> Config {
    Config {
        cases: 500,
        max_shrink_iters: 5000,
        ..Config::default()
    }
}

#[test]
fn test_with_custom_config() {
    let mut runner = TestRunner::new(custom_config());
    
    runner.run(&any::<i32>(), |x| {
        assert!(x.abs() >= 0);
        Ok(())
    }).unwrap();
}
```

### Environment Variables

Control proptest behavior via environment variables:

```bash
# Number of test cases
PROPTEST_CASES=1000 cargo test

# Maximum shrinking iterations
PROPTEST_MAX_SHRINK_ITERS=10000 cargo test

# Disable shrinking
PROPTEST_DISABLE_SHRINKING=1 cargo test

# Verbose output
PROPTEST_VERBOSE=1 cargo test

# Specific seed for reproducibility
PROPTEST_SEED=12345 cargo test
```

## Debugging and Shrinking

### Understanding Shrinking

When a test fails, proptest automatically tries to find the smallest input that still fails:

```rust
proptest! {
    #[test]
    fn failing_test(x in 0..1000i32) {
        // This will fail for x >= 42
        prop_assert!(x < 42);
    }
}
```

Output shows the shrinking process:
```
Test failed: assertion failed: `x < 42` at line 4
minimal failing input: x = 42
successes: 41
local rejects: 0
global rejects: 0
```

### Custom Shrinking

Implement custom shrinking for your types:

```rust
use proptest::strategy::{Strategy, ValueTree};
use proptest::test_runner::TestRunner;

#[derive(Debug, Clone)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn distance_from_origin(&self) -> f64 {
        ((self.x * self.x + self.y * self.y) as f64).sqrt()
    }
}

// Custom strategy with better shrinking
fn point_strategy() -> impl Strategy<Value = Point> {
    (any::<i32>(), any::<i32>())
        .prop_map(|(x, y)| Point { x, y })
        .prop_filter("reasonable coordinates", |p| {
            p.x.abs() < 1000 && p.y.abs() < 1000
        })
}

proptest! {
    #[test]
    fn test_point_distance(point in point_strategy()) {
        let distance = point.distance_from_origin();
        prop_assert!(distance >= 0.0);
        
        // This will fail for points far from origin
        prop_assert!(distance < 100.0);
    }
}
```

### Debugging Failed Tests

Add debug output to understand failures:

```rust
proptest! {
    #[test]
    fn debug_example(
        data in prop::collection::vec(any::<i32>(), 1..100)
    ) {
        println!("Testing with data: {:?}", data);
        
        let sum: i64 = data.iter().map(|&x| x as i64).sum();
        println!("Sum: {}", sum);
        
        // This might overflow for large inputs
        prop_assert!(sum < i32::MAX as i64);
    }
}
```

### Regression Testing

Proptest automatically saves failing cases for regression testing:

```rust
// Failed cases are saved in proptest-regressions/
// Add this directory to version control

// To replay a specific failure:
proptest! {
    #[test]
    fn regression_test() {
        // This will automatically include saved failing cases
        // in addition to generating new ones
    }
}
```

## Integration Patterns

### With Serde

Test serialization/deserialization:

```rust
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
struct Person {
    name: String,
    age: u8,
    email: String,
}

fn person_strategy() -> impl Strategy<Value = Person> {
    (
        "[a-zA-Z ]{2,30}",
        18u8..100,
        "[a-z]{3,10}@[a-z]{3,10}\\.[a-z]{2,4}",
    ).prop_map(|(name, age, email)| Person { name, age, email })
}

proptest! {
    #[test]
    fn test_json_roundtrip(person in person_strategy()) {
        let json = serde_json::to_string(&person).unwrap();
        let deserialized: Person = serde_json::from_str(&json).unwrap();
        prop_assert_eq!(person, deserialized);
    }
    
    #[test]
    fn test_bincode_roundtrip(person in person_strategy()) {
        let encoded = bincode::serialize(&person).unwrap();
        let decoded: Person = bincode::deserialize(&encoded).unwrap();
        prop_assert_eq!(person, decoded);
    }
}
```

### With Async Code

Test async functions:

```rust
use tokio_test;

async fn async_function(x: i32) -> Result<i32, String> {
    if x < 0 {
        Err("negative input".to_string())
    } else {
        Ok(x * 2)
    }
}

proptest! {
    #[test]
    fn test_async_function(x in any::<i32>()) {
        tokio_test::block_on(async {
            let result = async_function(x).await;
            
            if x < 0 {
                prop_assert!(result.is_err());
            } else {
                prop_assert_eq!(result.unwrap(), x * 2);
            }
        });
    }
}
```

### With Custom Derive

Use proptest-derive for automatic strategy generation:

```rust
use proptest_derive::Arbitrary;

#[derive(Debug, Clone, PartialEq, Arbitrary)]
struct Config {
    #[proptest(strategy = "1..=100usize")]
    max_connections: usize,
    
    #[proptest(strategy = "\"[a-z]{3,20}\"")]
    hostname: String,
    
    #[proptest(strategy = "1024..=65535u16")]
    port: u16,
    
    enable_ssl: bool,
}

proptest! {
    #[test]
    fn test_config_validation(config in any::<Config>()) {
        prop_assert!(config.max_connections > 0);
        prop_assert!(config.max_connections <= 100);
        prop_assert!(config.port >= 1024);
        prop_assert!(!config.hostname.is_empty());
    }
}
```

## Performance Optimization

### Strategy Performance

Optimize strategy generation for better performance:

```rust
// Slow: Creates new regex each time
fn slow_email_strategy() -> impl Strategy<Value = String> {
    "[a-z]{3,10}@[a-z]{3,10}\\.[a-z]{2,4}"
}

// Fast: Reuse compiled regex
use once_cell::sync::Lazy;
use proptest::string::string_regex;

static EMAIL_REGEX: Lazy<proptest::string::StringParam> = Lazy::new(|| {
    string_regex("[a-z]{3,10}@[a-z]{3,10}\\.[a-z]{2,4}").unwrap()
});

fn fast_email_strategy() -> impl Strategy<Value = String> {
    EMAIL_REGEX.clone()
}
```

### Reduce Test Cases for Expensive Tests

```rust
proptest! {
    // Reduce cases for expensive operations
    #![proptest_config(ProptestConfig::with_cases(50))]
    
    #[test]
    fn expensive_test(data in prop::collection::vec(any::<i32>(), 0..1000)) {
        // Expensive operation
        let result = expensive_computation(&data);
        prop_assert!(validate_result(&result));
    }
}
```

### Parallel Testing

Run tests in parallel when possible:

```rust
use rayon::prelude::*;

proptest! {
    #[test]
    fn parallel_test(
        data in prop::collection::vec(any::<i32>(), 100..1000)
    ) {
        // Process in parallel
        let results: Vec<_> = data
            .par_iter()
            .map(|&x| expensive_function(x))
            .collect();
        
        prop_assert_eq!(results.len(), data.len());
        prop_assert!(results.iter().all(|&r| r >= 0));
    }
}
```

## Common Pitfalls

### 1. Over-filtering

```rust
// Bad: Too restrictive filter
fn bad_strategy() -> impl Strategy<Value = i32> {
    any::<i32>().prop_filter("very specific", |&x| {
        x > 1000 && x < 1001 && x % 7 == 0
    })
}

// Good: Generate what you need directly
fn good_strategy() -> impl Strategy<Value = i32> {
    Just(1001) // If you need a specific value
}
```

### 2. Ignoring Shrinking

```rust
// Bad: Complex generation that doesn't shrink well
fn bad_complex_strategy() -> impl Strategy<Value = Vec<i32>> {
    any::<Vec<i32>>().prop_map(|mut v| {
        v.sort();
        v.dedup();
        v.reverse();
        v
    })
}

// Good: Build complexity step by step
fn good_complex_strategy() -> impl Strategy<Value = Vec<i32>> {
    prop::collection::vec(any::<i32>(), 0..20)
        .prop_map(|mut v| {
            v.sort();
            v.dedup();
            v
        })
}
```

### 3. Not Using prop_assume

```rust
// Bad: Assertion that should be assumption
proptest! {
    #[test]
    fn bad_division_test(a in any::<i32>(), b in any::<i32>()) {
        prop_assert!(b != 0); // This will fail when b == 0
        prop_assert_eq!(a / b * b + a % b, a);
    }
}

// Good: Use assumption for preconditions
proptest! {
    #[test]
    fn good_division_test(a in any::<i32>(), b in any::<i32>()) {
        prop_assume!(b != 0); // Skip test case when b == 0
        prop_assert_eq!(a / b * b + a % b, a);
    }
}
```

### 4. Testing Implementation Details

```rust
// Bad: Testing internal implementation
proptest! {
    #[test]
    fn bad_sort_test(mut data in prop::collection::vec(any::<i32>(), 0..100)) {
        data.sort();
        // Don't test that it uses quicksort specifically
        prop_assert!(uses_quicksort_algorithm(&data));
    }
}

// Good: Test observable behavior
proptest! {
    #[test]
    fn good_sort_test(mut data in prop::collection::vec(any::<i32>(), 0..100)) {
        let original = data.clone();
        data.sort();
        
        // Test that it's actually sorted
        prop_assert!(data.windows(2).all(|w| w[0] <= w[1]));
        
        // Test that all elements are preserved
        let mut orig_sorted = original;
        orig_sorted.sort();
        prop_assert_eq!(data, orig_sorted);
    }
}
```

---

This comprehensive guide covers the essential patterns and techniques for effective property-based testing with proptest. Remember to start simple, focus on meaningful properties, and gradually build complexity as you become more comfortable with the framework.