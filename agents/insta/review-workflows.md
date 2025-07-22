# Review and Update Workflows

Managing snapshot changes is a crucial part of snapshot testing. Insta provides several tools and workflows to make this process efficient and reliable.

## Overview

When tests run, Insta compares current output against stored snapshots. When differences are found:

1. **New snapshots** are created with `.snap.new` extension
2. **You review** the changes to determine if they're correct
3. **Accept or reject** the changes based on your review
4. **Commit** accepted snapshots to version control

## The cargo-insta Tool

The recommended way to manage snapshots is with `cargo-insta`:

```bash
# Install cargo-insta
cargo install cargo-insta

# Basic workflow
cargo test                    # Run tests, generate .snap.new files
cargo insta review           # Interactive review of changes
```

### Interactive Review Interface

The `cargo insta review` command provides an interactive interface:

```
┌─ test_user_creation.snap ─────────────────────────────────────────┐
│ --- old                                                           │
│ +++ new                                                           │
│ @@ -1,5 +1,5 @@                                                   │
│  User {                                                           │
│ -    id: 42,                                                      │
│ +    id: 43,                                                      │
│      name: "Alice Johnson",                                       │
│      email: "alice@example.com",                                  │
│  }                                                                │
└───────────────────────────────────────────────────────────────────┘

[a]ccept [r]eject [s]kip [q]uit [d]iff
```

**Commands:**
- `a` - Accept this snapshot
- `r` - Reject this snapshot (delete .snap.new file)
- `s` - Skip this snapshot for now
- `q` - Quit the review session
- `d` - Show detailed diff

## Environment Variable Control

You can control Insta's behavior without `cargo-insta` using environment variables:

### INSTA_UPDATE

Controls when snapshots are updated:

```bash
# Never update snapshots (default)
INSTA_UPDATE=no cargo test

# Always update snapshots automatically
INSTA_UPDATE=always cargo test

# Only create snapshots for new tests
INSTA_UPDATE=new cargo test

# Update snapshots but don't overwrite existing ones
INSTA_UPDATE=auto cargo test
```

### INSTA_FORCE_PASS

Force tests to pass even with snapshot mismatches:

```bash
# Useful for CI environments where you want to see all failures
INSTA_FORCE_PASS=1 cargo test
```

### INSTA_SNAPSHOT_PATH

Override the default snapshot directory:

```bash
# Use custom snapshot directory
INSTA_SNAPSHOT_PATH=custom_snapshots cargo test
```

## Manual Review Process

If you prefer not to use `cargo-insta`, you can manage snapshots manually:

### 1. Run Tests

```bash
cargo test
```

### 2. Examine .snap.new Files

Check the generated files in your snapshots directory:

```bash
ls tests/snapshots/
# test_user_creation.snap
# test_user_creation.snap.new  <- New snapshot
```

### 3. Compare Changes

Use your preferred diff tool:

```bash
# Using diff
diff tests/snapshots/test_user_creation.snap tests/snapshots/test_user_creation.snap.new

# Using git diff
git diff --no-index tests/snapshots/test_user_creation.snap tests/snapshots/test_user_creation.snap.new
```

### 4. Accept or Reject

```bash
# Accept: rename .snap.new to .snap
mv tests/snapshots/test_user_creation.snap.new tests/snapshots/test_user_creation.snap

# Reject: delete .snap.new file
rm tests/snapshots/test_user_creation.snap.new
```

## Advanced cargo-insta Commands

### Review Specific Tests

```bash
# Review only specific test files
cargo insta review tests/user_tests.rs

# Review specific test functions
cargo insta review --test test_user_creation
```

### Batch Operations

```bash
# Accept all pending snapshots
cargo insta accept

# Reject all pending snapshots
cargo insta reject

# Show pending snapshots without reviewing
cargo insta pending-snapshots
```

### Workspace Support

```bash
# Review snapshots across entire workspace
cargo insta review --workspace

# Review specific package in workspace
cargo insta review --package my-package
```

## Inline Snapshot Updates

For inline snapshots (using `@"..."` syntax), cargo-insta can update them directly in source files:

```rust
#[test]
fn test_inline_snapshot() {
    let result = format_greeting("Alice");
    assert_snapshot!(result, @"Hello, Alice!");
}
```

When the output changes:

```bash
cargo insta review
```

The tool will show the diff and update the source file directly when accepted.

## Handling Large Snapshots

### Splitting Large Tests

Break large snapshots into smaller, focused tests:

```rust
// Instead of one large test
#[test]
fn test_entire_system() {
    let system = build_complex_system();
    assert_debug_snapshot!(system); // Very large snapshot
}

// Use multiple focused tests
#[test]
fn test_system_users() {
    let system = build_complex_system();
    assert_debug_snapshot!(system.users);
}

#[test]
fn test_system_config() {
    let system = build_complex_system();
    assert_debug_snapshot!(system.config);
}
```

### Using Redactions

Hide irrelevant or dynamic data:

```rust
use insta::assert_yaml_snapshot;

#[test]
fn test_with_redactions() {
    let response = api_call();
    assert_yaml_snapshot!(response, {
        ".timestamp" => "[timestamp]",
        ".id" => "[id]",
        ".metadata.created_at" => "[datetime]"
    });
}
```

## CI/CD Integration

### GitHub Actions Example

```yaml
name: Tests
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
      
      # Run tests without updating snapshots
      - name: Run tests
        run: INSTA_UPDATE=no cargo test
        
      # Install cargo-insta for review
      - name: Install cargo-insta
        run: cargo install cargo-insta
        
      # Check for pending snapshots
      - name: Check snapshots
        run: cargo insta pending-snapshots --check
```

### Handling Snapshot Updates in CI

```bash
# In CI: fail if snapshots need updates
INSTA_UPDATE=no cargo test

# Check for pending snapshots
cargo insta pending-snapshots --check
if [ $? -ne 0 ]; then
    echo "Snapshots need review!"
    exit 1
fi
```

## Best Practices

### 1. Review Snapshots Carefully

Always review snapshot changes to ensure they represent intended behavior:

```bash
# Don't blindly accept all changes
cargo insta accept  # ❌ Dangerous

# Review each change individually
cargo insta review  # ✅ Safe
```

### 2. Commit Snapshots with Code

Always commit snapshot files alongside the code changes that created them:

```bash
git add tests/snapshots/
git add src/
git commit -m "Add user validation with snapshot tests"
```

### 3. Use Descriptive Test Names

Good test names make snapshot files easier to understand:

```rust
// ❌ Unclear
#[test]
fn test_user() { /* ... */ }

// ✅ Clear
#[test]
fn test_user_serialization_with_all_fields() { /* ... */ }
```

### 4. Keep Snapshots Focused

Each snapshot should test one specific aspect:

```rust
// ❌ Too broad
#[test]
fn test_everything() {
    assert_debug_snapshot!(entire_application_state);
}

// ✅ Focused
#[test]
fn test_user_profile_rendering() {
    assert_debug_snapshot!(render_user_profile(user));
}
```

### 5. Use Settings for Consistency

Configure snapshot behavior consistently:

```rust
use insta::Settings;

fn setup_test_settings() -> Settings {
    let mut settings = Settings::clone_current();
    settings.set_sort_maps(true);
    settings.set_snapshot_path("custom_snapshots");
    settings
}

#[test]
fn test_with_settings() {
    setup_test_settings().bind(|| {
        assert_yaml_snapshot!(data);
    });
}
```

## Troubleshooting

### Snapshots Not Updating

Check environment variables:
```bash
echo $INSTA_UPDATE
# Should be "always", "new", or "auto" to update
```

### Permission Issues

Ensure write permissions to snapshot directory:
```bash
chmod -R u+w tests/snapshots/
```

### Large Diffs

Use external diff tools for better visualization:
```bash
# Configure git to use your preferred diff tool
git config --global diff.tool vimdiff
cargo insta review
```

### Workspace Issues

Ensure you're running commands from the correct directory:
```bash
# From workspace root
cargo insta review --workspace

# From package directory
cd my-package
cargo insta review
```