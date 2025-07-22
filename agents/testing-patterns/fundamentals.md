# Property-Based Testing Fundamentals

Property-based testing is a powerful testing methodology that automatically generates test inputs and verifies that certain properties hold for all inputs. This document covers the core concepts, theory, and practical applications.

## Table of Contents

- [What is Property-Based Testing?](#what-is-property-based-testing)
- [Core Concepts](#core-concepts)
- [Property Types](#property-types)
- [Advantages and Limitations](#advantages-and-limitations)
- [When to Use Property-Based Testing](#when-to-use-property-based-testing)
- [Comparison with Traditional Testing](#comparison-with-traditional-testing)
- [Mathematical Foundations](#mathematical-foundations)
- [Best Practices](#best-practices)

## What is Property-Based Testing?

Property-based testing is a testing methodology where you:

1. **Define Properties**: Specify invariants or relationships that should always hold
2. **Generate Inputs**: Automatically create diverse test inputs
3. **Verify Properties**: Check that properties hold for all generated inputs
4. **Shrink Failures**: Automatically reduce failing inputs to minimal examples

### Example: Testing a Reverse Function

Instead of testing specific cases:

```rust
#[test]
fn test_reverse_specific_cases() {
    assert_eq!(reverse(&[1, 2, 3]), [3, 2, 1]);
    assert_eq!(reverse(&[]), []);
    assert_eq!(reverse(&[42]), [42]);
}
```

You test properties:

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_reverse_properties(xs in prop::collection::vec(any::<i32>(), 0..100)) {
        let reversed = reverse(&xs);
        
        // Property 1: Length is preserved
        prop_assert_eq!(xs.len(), reversed.len());
        
        // Property 2: Reversing twice gives original
        prop_assert_eq!(xs, reverse(&reversed));
        
        // Property 3: First element becomes last (if non-empty)
        if !xs.is_empty() {
            prop_assert_eq!(xs[0], reversed[xs.len() - 1]);
        }
    }
}
```

## Core Concepts

### 1. Properties

Properties are statements about your code that should always be true. They fall into several categories:

#### Invariant Properties
Properties that never change regardless of input:

```rust
proptest! {
    #[test]
    fn sort_preserves_length(mut xs in prop::collection::vec(any::<i32>(), 0..100)) {
        let original_len = xs.len();
        xs.sort();
        prop_assert_eq!(xs.len(), original_len);
    }
}
```

#### Postcondition Properties
Properties about the output given certain inputs:

```rust
proptest! {
    #[test]
    fn sorted_array_is_ordered(mut xs in prop::collection::vec(any::<i32>(), 0..100)) {
        xs.sort();
        for i in 1..xs.len() {
            prop_assert!(xs[i-1] <= xs[i]);
        }
    }
}
```

#### Metamorphic Properties
Relationships between different inputs and outputs:

```rust
proptest! {
    #[test]
    fn sort_is_idempotent(mut xs in prop::collection::vec(any::<i32>(), 0..100)) {
        xs.sort();
        let first_sort = xs.clone();
        xs.sort();
        prop_assert_eq!(first_sort, xs);
    }
}
```

### 2. Strategies

Strategies define how test inputs are generated. They are composable and can be combined to create complex test data.

#### Basic Strategies

```rust
use proptest::prelude::*;

// Generate any i32
any::<i32>()

// Generate integers in a range
0..100

// Generate strings matching a regex
"[a-zA-Z]{1,10}"

// Generate optional values
prop::option::of(any::<i32>())

// Generate collections
prop::collection::vec(any::<i32>(), 0..10)
```

#### Composite Strategies

```rust
#[derive(Debug, Clone)]
struct Person {
    name: String,
    age: u8,
    email: String,
}

fn person_strategy() -> impl Strategy<Value = Person> {
    (
        "[a-zA-Z]{2,20}",           // name
        18u8..100,                  // age
        "[a-z]{3,10}@[a-z]{3,10}\\.[a-z]{2,3}", // email
    ).prop_map(|(name, age, email)| Person {
        name,
        age,
        email,
    })
}

proptest! {
    #[test]
    fn test_person_validation(person in person_strategy()) {
        prop_assert!(validate_person(&person).is_ok());
    }
}
```

### 3. Shrinking

When a property test fails, proptest automatically tries to find the smallest input that still causes the failure. This process is called shrinking.

#### How Shrinking Works

1. **Initial Failure**: A generated input causes the test to fail
2. **Systematic Reduction**: Proptest tries smaller variations of the input
3. **Minimal Example**: The process continues until no further reduction is possible
4. **Reproducible Failure**: The minimal case is saved for regression testing

#### Example of Shrinking in Action

```rust
proptest! {
    #[test]
    fn buggy_function_test(x in 0..1000i32) {
        // This function has a bug when x >= 42
        prop_assert!(buggy_function(x) > 0);
    }
}
```

If `buggy_function(500)` fails, proptest will shrink it down to `buggy_function(42)` - the minimal failing case.

### 4. Test Case Generation

Proptest uses sophisticated algorithms to generate diverse test cases:

#### Random Generation
- Uses high-quality random number generators
- Ensures good distribution across the input space
- Balances between edge cases and typical values

#### Guided Generation
- Focuses on boundary conditions
- Generates both simple and complex cases
- Adapts based on the strategy definition

## Property Types

### 1. Algebraic Properties

Properties based on mathematical laws:

```rust
proptest! {
    #[test]
    fn addition_is_commutative(a in any::<i32>(), b in any::<i32>()) {
        prop_assert_eq!(a + b, b + a);
    }
    
    #[test]
    fn addition_is_associative(a in any::<i32>(), b in any::<i32>(), c in any::<i32>()) {
        prop_assert_eq!((a + b) + c, a + (b + c));
    }
}
```

### 2. Round-trip Properties

Testing serialization/deserialization, encoding/decoding:

```rust
proptest! {
    #[test]
    fn json_roundtrip(data in person_strategy()) {
        let json = serde_json::to_string(&data)?;
        let parsed: Person = serde_json::from_str(&json)?;
        prop_assert_eq!(data, parsed);
    }
}
```

### 3. Oracle Properties

Comparing against a known-correct but possibly inefficient implementation:

```rust
fn naive_sort(mut xs: Vec<i32>) -> Vec<i32> {
    // Simple but slow bubble sort
    for i in 0..xs.len() {
        for j in 0..xs.len() - 1 - i {
            if xs[j] > xs[j + 1] {
                xs.swap(j, j + 1);
            }
        }
    }
    xs
}

proptest! {
    #[test]
    fn fast_sort_matches_naive(mut xs in prop::collection::vec(any::<i32>(), 0..100)) {
        let naive_result = naive_sort(xs.clone());
        xs.sort(); // Fast sort
        prop_assert_eq!(xs, naive_result);
    }
}
```

### 4. Invariant Properties

Properties that should always hold regardless of operations:

```rust
proptest! {
    #[test]
    fn hash_map_invariants(
        operations in prop::collection::vec(
            (any::<String>(), any::<i32>()),
            0..100
        )
    ) {
        let mut map = HashMap::new();
        
        for (key, value) in operations {
            map.insert(key.clone(), value);
            
            // Invariant: inserted key should be present
            prop_assert!(map.contains_key(&key));
            
            // Invariant: retrieved value should match inserted value
            prop_assert_eq!(map.get(&key), Some(&value));
        }
    }
}
```

## Advantages and Limitations

### Advantages

1. **Comprehensive Coverage**: Tests many more cases than manual testing
2. **Edge Case Discovery**: Finds corner cases you wouldn't think of
3. **Automatic Minimization**: Provides minimal failing examples
4. **Regression Prevention**: Saves failing cases for future runs
5. **Documentation**: Properties serve as executable specifications
6. **Confidence**: High confidence in correctness across input space

### Limitations

1. **Property Definition**: Requires thinking about what properties to test
2. **Performance**: Can be slower than unit tests due to many iterations
3. **Complexity**: More complex to set up than simple unit tests
4. **False Positives**: May find "failures" that are actually acceptable behavior
5. **Limited Scope**: Cannot find all bugs (e.g., single-value edge cases in large spaces)

### When Property-Based Testing Excels

- **Pure Functions**: Functions without side effects
- **Data Transformations**: Parsing, serialization, encoding
- **Mathematical Operations**: Arithmetic, geometric calculations
- **Data Structures**: Collections, trees, graphs
- **Algorithms**: Sorting, searching, optimization

### When to Use Traditional Testing

- **Specific Business Logic**: Domain-specific rules and edge cases
- **Integration Testing**: Testing interactions between components
- **Error Handling**: Specific error conditions and recovery
- **Performance Testing**: Timing and resource usage
- **User Interface**: Visual and interaction testing

## Mathematical Foundations

### Formal Verification Concepts

Property-based testing is inspired by formal verification methods:

#### Preconditions and Postconditions
```rust
proptest! {
    #[test]
    fn divide_properties(a in any::<f64>(), b in any::<f64>()) {
        // Precondition: b != 0
        prop_assume!(b != 0.0);
        
        let result = a / b;
        
        // Postcondition: result * b ≈ a (within floating point precision)
        prop_assert!((result * b - a).abs() < f64::EPSILON * 10.0);
    }
}
```

#### Invariants
```rust
proptest! {
    #[test]
    fn stack_invariants(
        operations in prop::collection::vec(
            prop::sample::select(vec!["push", "pop"]),
            0..100
        )
    ) {
        let mut stack = Vec::new();
        let mut expected_size = 0;
        
        for op in operations {
            match op.as_str() {
                "push" => {
                    stack.push(42);
                    expected_size += 1;
                }
                "pop" => {
                    if !stack.is_empty() {
                        stack.pop();
                        expected_size -= 1;
                    }
                }
                _ => unreachable!(),
            }
            
            // Invariant: size tracking is correct
            prop_assert_eq!(stack.len(), expected_size);
        }
    }
}
```

### Statistical Confidence

Property-based testing provides statistical confidence rather than mathematical proof:

- **Default**: 256 test cases per property
- **Configurable**: Can increase for higher confidence
- **Trade-off**: More cases = higher confidence but slower tests

```rust
proptest! {
    #![proptest_config(ProptestConfig::with_cases(1000))]
    
    #[test]
    fn high_confidence_test(x in any::<i32>()) {
        // This test runs 1000 cases instead of the default 256
        prop_assert!(expensive_property_check(x));
    }
}
```

## Best Practices

### 1. Start Simple

Begin with basic properties and gradually add complexity:

```rust
// Start with this
proptest! {
    #[test]
    fn sort_preserves_elements(mut xs in prop::collection::vec(any::<i32>(), 0..10)) {
        let original = xs.clone();
        xs.sort();
        
        // Simple property: all original elements are still present
        for &elem in &original {
            prop_assert!(xs.contains(&elem));
        }
    }
}

// Then evolve to this
proptest! {
    #[test]
    fn sort_comprehensive(mut xs in prop::collection::vec(any::<i32>(), 0..100)) {
        let original = xs.clone();
        xs.sort();
        
        // Multiple properties
        prop_assert_eq!(xs.len(), original.len()); // Length preserved
        prop_assert!(xs.windows(2).all(|w| w[0] <= w[1])); // Ordered
        
        // All elements preserved with correct counts
        let mut orig_sorted = original;
        orig_sorted.sort();
        prop_assert_eq!(xs, orig_sorted);
    }
}
```

### 2. Use Meaningful Property Names

```rust
proptest! {
    #[test]
    fn reverse_twice_is_identity(xs in prop::collection::vec(any::<i32>(), 0..100)) {
        prop_assert_eq!(xs, reverse(&reverse(&xs)));
    }
    
    #[test]
    fn reverse_preserves_length(xs in prop::collection::vec(any::<i32>(), 0..100)) {
        prop_assert_eq!(xs.len(), reverse(&xs).len());
    }
    
    #[test]
    fn reverse_inverts_order(xs in prop::collection::vec(any::<i32>(), 1..100)) {
        let reversed = reverse(&xs);
        prop_assert_eq!(xs[0], reversed[xs.len() - 1]);
        prop_assert_eq!(xs[xs.len() - 1], reversed[0]);
    }
}
```

### 3. Handle Edge Cases Explicitly

```rust
proptest! {
    #[test]
    fn division_properties(a in any::<f64>(), b in any::<f64>()) {
        // Handle the edge case explicitly
        if b == 0.0 {
            // Test that division by zero behaves as expected
            prop_assert!(divide(a, b).is_err());
        } else {
            let result = divide(a, b)?;
            prop_assert!((result * b - a).abs() < f64::EPSILON * 10.0);
        }
    }
}
```

### 4. Use Appropriate Strategies

Choose strategies that match your domain:

```rust
// For testing a URL parser
proptest! {
    #[test]
    fn url_parsing(url in "https?://[a-z0-9.-]+\\.[a-z]{2,4}(/[a-zA-Z0-9._~:/?#\\[\\]@!$&'()*+,;=-]*)?") {
        let parsed = parse_url(&url);
        prop_assert!(parsed.is_ok());
    }
}

// For testing numeric algorithms
proptest! {
    #[test]
    fn numeric_stability(x in -1000.0..1000.0f64) {
        let result = complex_calculation(x);
        prop_assert!(result.is_finite());
        prop_assert!(!result.is_nan());
    }
}
```

### 5. Document Your Properties

```rust
proptest! {
    /// Property: Sorting preserves all elements
    /// 
    /// This test verifies that after sorting a vector:
    /// 1. No elements are lost
    /// 2. No elements are added
    /// 3. Element counts are preserved
    #[test]
    fn sort_preserves_multiset(mut xs in prop::collection::vec(any::<i32>(), 0..100)) {
        let mut original = xs.clone();
        original.sort();
        xs.sort();
        prop_assert_eq!(xs, original);
    }
}
```

### 6. Balance Test Complexity

```rust
// Good: Simple, focused property
proptest! {
    #[test]
    fn map_preserves_length(xs in prop::collection::vec(any::<i32>(), 0..100)) {
        let doubled: Vec<i32> = xs.iter().map(|x| x * 2).collect();
        prop_assert_eq!(xs.len(), doubled.len());
    }
}

// Avoid: Overly complex property that's hard to understand
proptest! {
    #[test]
    fn complex_property_avoid(
        xs in prop::collection::vec(any::<i32>(), 0..100),
        ys in prop::collection::vec(any::<i32>(), 0..100),
        f in prop::sample::select(vec![|x: i32| x * 2, |x: i32| x + 1, |x: i32| x - 1])
    ) {
        // Too many variables and complex logic makes this hard to debug
        // when it fails
    }
}
```

---

Property-based testing is a powerful complement to traditional testing methods. By understanding these fundamentals, you can write more robust tests that catch bugs traditional testing might miss while providing executable documentation of your code's behavior.