# Comprehensive Testing Strategies for Rust

This directory contains comprehensive documentation for advanced testing strategies in Rust, focusing on three powerful testing libraries:

- **[proptest](./proptest-guide.md)** - Property-based testing framework
- **[expect-test](./expect-test-guide.md)** - Minimalistic snapshot testing
- **[pretty_assertions](./pretty-assertions-guide.md)** - Enhanced assertion output with colorful diffs

## Quick Start

### Dependencies

Add these to your `Cargo.toml`:

```toml
[dev-dependencies]
proptest = "1.6.0"
expect-test = "1.5.1"
pretty_assertions = "1.4.1"
```

### Basic Usage

```rust
// Property-based testing
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_reverse_twice_is_identity(xs in prop::collection::vec(any::<i32>(), 0..100)) {
        let reversed_twice: Vec<i32> = xs.iter().cloned().rev().rev().collect();
        prop_assert_eq!(xs, reversed_twice);
    }
}

// Snapshot testing
use expect_test::expect;

#[test]
fn test_parser_output() {
    let result = parse("fn main() {}");
    expect![[r#"
        Function {
            name: "main",
            params: [],
            body: Block([])
        }
    "#]].assert_debug_eq(&result);
}

// Enhanced assertions
#[cfg(test)]
use pretty_assertions::{assert_eq, assert_ne};

#[test]
fn test_complex_structure() {
    let actual = ComplexStruct { /* ... */ };
    let expected = ComplexStruct { /* ... */ };
    assert_eq!(actual, expected); // Shows colorful diff on failure
}
```

## Documentation Structure

### Core Concepts
- **[Property-Based Testing Fundamentals](./fundamentals.md)** - Core concepts and theory
- **[Test Organization Strategies](./organization.md)** - Structuring and organizing tests
- **[Shrinking and Debugging](./debugging.md)** - Understanding test failures and debugging

### Library-Specific Guides
- **[Proptest Complete Guide](./proptest-guide.md)** - Comprehensive proptest usage
- **[Expect-Test Guide](./expect-test-guide.md)** - Snapshot testing patterns
- **[Pretty Assertions Guide](./pretty-assertions-guide.md)** - Enhanced assertion techniques

### Advanced Topics
- **[Performance Testing](./performance-testing.md)** - Benchmarking and performance validation
- **[CI/CD Integration](./cicd-integration.md)** - Continuous integration patterns
- **[API Reference](./api-reference.md)** - Complete API documentation with examples

## Testing Philosophy

This documentation follows the testing philosophy outlined in the Parallax compiler's AGENTS.md:

1. **Performance First**: Every testing decision prioritizes runtime performance
2. **Comprehensive Coverage**: Use multiple testing strategies for complete validation
3. **Fast Feedback**: Optimize for quick test execution and clear failure reporting
4. **Maintainable Tests**: Write tests that are easy to understand and maintain

## Key Benefits

### Property-Based Testing (proptest)
- **Automatic Test Case Generation**: Discovers edge cases you wouldn't think of
- **Intelligent Shrinking**: Automatically reduces failing inputs to minimal cases
- **Composable Strategies**: Build complex test data from simple components
- **Regression Testing**: Automatically saves and replays failing cases

### Snapshot Testing (expect-test)
- **Effortless Updates**: Update expected outputs with environment variable
- **Version Control Friendly**: Inline snapshots in source code
- **Minimal Boilerplate**: Simple macro-based API
- **IDE Integration**: Works seamlessly with rust-analyzer

### Enhanced Assertions (pretty_assertions)
- **Visual Diff Output**: Colorful, easy-to-read failure messages
- **Zero Runtime Cost**: Only affects test compilation
- **Drop-in Replacement**: Simply replace `std::assert_eq!` imports
- **Cross-Platform**: Works on all platforms with proper terminal support

## Best Practices

1. **Combine Strategies**: Use property-based testing for logic validation, snapshot testing for output verification, and enhanced assertions for debugging
2. **Start Simple**: Begin with basic property tests and gradually add complexity
3. **Document Properties**: Clearly document what properties your tests verify
4. **Use Appropriate Tools**: Choose the right testing approach for each scenario
5. **Optimize for Maintainability**: Write tests that are easy to understand and update

## Performance Considerations

- **Property Test Complexity**: Balance test thoroughness with execution time
- **Snapshot Size**: Keep snapshots reasonably sized for maintainability
- **Parallel Execution**: Ensure tests can run in parallel safely
- **Resource Usage**: Monitor memory and CPU usage for large test suites

## Getting Started

1. Read the [Property-Based Testing Fundamentals](./fundamentals.md)
2. Follow the [Proptest Guide](./proptest-guide.md) for hands-on examples
3. Explore [Test Organization Strategies](./organization.md) for structuring your test suite
4. Integrate with your CI/CD pipeline using the [CI/CD Integration Guide](./cicd-integration.md)

---

*This documentation is part of the Parallax compiler project and follows its performance-first philosophy.*