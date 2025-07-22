# Getting Started with Insta

This guide will help you get up and running with Insta snapshot testing in just a few minutes.

## Installation

Add Insta to your `Cargo.toml` as a development dependency:

```toml
[dev-dependencies]
insta = "1.43"

# Enable specific features as needed
insta = { version = "1.43", features = ["yaml", "json", "glob", "redactions"] }
```

### Optional: Install cargo-insta

For the best experience, install the `cargo-insta` tool:

```bash
cargo install cargo-insta
```

This provides commands like `cargo insta review` for managing snapshots interactively.

## Your First Snapshot Test

Let's create a simple snapshot test:

```rust
// tests/basic_test.rs
use insta::assert_debug_snapshot;

#[derive(Debug)]
struct User {
    id: u32,
    name: String,
    email: String,
}

#[test]
fn test_user_creation() {
    let user = User {
        id: 42,
        name: "Alice Johnson".to_string(),
        email: "alice@example.com".to_string(),
    };
    
    assert_debug_snapshot!(user);
}
```

## Running Your First Test

1. **Run the test** (it will fail the first time):
   ```bash
   cargo test
   ```

2. **Review the snapshot**:
   ```bash
   cargo insta review
   ```
   
   Or manually check the generated `.snap.new` file in `tests/snapshots/`.

3. **Accept the snapshot** if it looks correct:
   - With cargo-insta: Press 'a' in the review interface
   - Manually: Rename `test_file__test_user_creation.snap.new` to `test_file__test_user_creation.snap`

## Understanding Snapshot Files

After accepting your first snapshot, you'll find a file like this:

```
tests/snapshots/basic_test__test_user_creation.snap
```

Contents:
```
---
source: tests/basic_test.rs
expression: user
---
User {
    id: 42,
    name: "Alice Johnson",
    email: "alice@example.com",
}
```

## Basic Assertion Macros

### assert_debug_snapshot!

Tests the `Debug` representation of a value:

```rust
#[test]
fn test_debug_output() {
    let data = vec![1, 2, 3, 4, 5];
    assert_debug_snapshot!(data);
}
```

### assert_snapshot!

Tests the `Display` representation (for strings and displayable types):

```rust
#[test]
fn test_string_output() {
    let message = format!("Hello, {}!", "World");
    assert_snapshot!(message);
}
```

### Named Snapshots

You can provide custom names for your snapshots:

```rust
#[test]
fn test_multiple_scenarios() {
    let empty_vec: Vec<i32> = vec![];
    assert_debug_snapshot!("empty_vector", empty_vec);
    
    let small_vec = vec![1, 2, 3];
    assert_debug_snapshot!("small_vector", small_vec);
    
    let large_vec = (1..=100).collect::<Vec<_>>();
    assert_debug_snapshot!("large_vector", large_vec);
}
```

## Working with Serializable Data

For structured data, use format-specific macros:

```rust
use serde::Serialize;
use insta::assert_yaml_snapshot;

#[derive(Serialize)]
struct Config {
    database_url: String,
    port: u16,
    debug: bool,
}

#[test]
fn test_config_serialization() {
    let config = Config {
        database_url: "postgresql://localhost/myapp".to_string(),
        port: 5432,
        debug: true,
    };
    
    assert_yaml_snapshot!(config);
}
```

## Environment Variables

Control Insta's behavior with environment variables:

```bash
# Don't create new snapshots, just fail tests
INSTA_UPDATE=no cargo test

# Always update snapshots without prompting
INSTA_UPDATE=always cargo test

# Update snapshots only for new tests
INSTA_UPDATE=new cargo test

# Use a different snapshot directory
INSTA_SNAPSHOT_PATH=my_snapshots cargo test
```

## Common Workflow

1. **Write your test** with a snapshot assertion
2. **Run tests** - they'll fail initially
3. **Review snapshots** with `cargo insta review` or manually
4. **Accept good snapshots** and fix issues with bad ones
5. **Commit snapshots** to version control alongside your code
6. **Future runs** will compare against the stored snapshots

## Inline Snapshots

Store snapshots directly in your source code:

```rust
use insta::assert_snapshot;

#[test]
fn test_inline_snapshot() {
    let result = format_greeting("Alice");
    assert_snapshot!(result, @"Hello, Alice! Welcome to our application.");
}

fn format_greeting(name: &str) -> String {
    format!("Hello, {}! Welcome to our application.", name)
}
```

The `@"..."` syntax creates an inline snapshot that's updated directly in your source file.

## Next Steps

Now that you understand the basics:

1. Explore [Snapshot Formats](./snapshot-formats.md) for different serialization options
2. Learn about [Review Workflows](./review-workflows.md) for managing changes
3. Check out [Best Practices](./best-practices.md) for effective snapshot testing

## Troubleshooting

### Test Fails with "snapshot does not exist"

This is normal for new tests. Run `cargo insta review` to create the initial snapshot.

### Snapshots aren't updating

Check that you're using the correct environment variable:
```bash
INSTA_UPDATE=always cargo test
```

### Snapshot files in wrong location

Set the snapshot path explicitly:
```rust
use insta::Settings;

#[test]
fn test_with_custom_path() {
    let settings = Settings::clone_current();
    settings.bind(|| {
        settings.set_snapshot_path("custom_snapshots");
        assert_debug_snapshot!(vec![1, 2, 3]);
    });
}
```

### Large diffs are hard to read

Consider using redactions to hide irrelevant data or breaking large tests into smaller, focused ones.