# API Reference

Complete reference for all Insta macros, functions, types, and features with practical examples.

## Table of Contents

- [Assertion Macros](#assertion-macros)
- [Settings API](#settings-api)
- [Redactions API](#redactions-api)
- [Filters API](#filters-api)
- [Glob API](#glob-api)
- [Environment Variables](#environment-variables)
- [Types and Traits](#types-and-traits)
- [Utility Functions](#utility-functions)

## Assertion Macros

### assert_snapshot!

Tests the `Display` representation of a value.

**Syntax:**
```rust
assert_snapshot!([name,] value [, @"inline_snapshot"])
```

**Examples:**
```rust
use insta::assert_snapshot;

// Basic usage
#[test]
fn test_string_output() {
    let message = "Hello, World!";
    assert_snapshot!(message);
}

// With custom name
#[test]
fn test_formatted_output() {
    let formatted = format!("User: {}, Age: {}", "Alice", 30);
    assert_snapshot!("user_info", formatted);
}

// Inline snapshot
#[test]
fn test_inline() {
    let greeting = format_greeting("Alice");
    assert_snapshot!(greeting, @"Hello, Alice!");
}
```

### assert_debug_snapshot!

Tests the `Debug` representation of a value.

**Syntax:**
```rust
assert_debug_snapshot!([name,] value)
```

**Examples:**
```rust
use insta::assert_debug_snapshot;

#[derive(Debug)]
struct User {
    id: u32,
    name: String,
}

#[test]
fn test_user_debug() {
    let user = User {
        id: 42,
        name: "Alice".to_string(),
    };
    assert_debug_snapshot!(user);
}

// With custom name
#[test]
fn test_multiple_users() {
    let admin = User { id: 1, name: "Admin".to_string() };
    let guest = User { id: 2, name: "Guest".to_string() };
    
    assert_debug_snapshot!("admin_user", admin);
    assert_debug_snapshot!("guest_user", guest);
}
```

### assert_yaml_snapshot!

Tests YAML serialization of a `Serialize` value.

**Feature:** `yaml`

**Syntax:**
```rust
assert_yaml_snapshot!([name,] value [, redactions])
```

**Examples:**
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
fn test_config_yaml() {
    let config = Config {
        database_url: "postgresql://localhost/myapp".to_string(),
        port: 5432,
        debug: true,
    };
    assert_yaml_snapshot!(config);
}

// With redactions
#[test]
fn test_config_with_redactions() {
    let config = Config {
        database_url: "postgresql://secret:password@localhost/myapp".to_string(),
        port: 5432,
        debug: true,
    };
    
    assert_yaml_snapshot!(config, {
        ".database_url" => "[database_url]"
    });
}
```

### assert_json_snapshot!

Tests JSON serialization of a `Serialize` value.

**Feature:** `json`

**Syntax:**
```rust
assert_json_snapshot!([name,] value [, redactions])
```

**Examples:**
```rust
use serde::Serialize;
use insta::assert_json_snapshot;

#[derive(Serialize)]
struct ApiResponse {
    status: String,
    data: Vec<String>,
    count: usize,
}

#[test]
fn test_api_response() {
    let response = ApiResponse {
        status: "success".to_string(),
        data: vec!["item1".to_string(), "item2".to_string()],
        count: 2,
    };
    assert_json_snapshot!(response);
}

// With redactions for dynamic data
#[test]
fn test_api_response_with_timestamp() {
    let response = serde_json::json!({
        "status": "success",
        "timestamp": "2023-01-15T10:30:00Z",
        "request_id": "req_abc123",
        "data": ["item1", "item2"]
    });
    
    assert_json_snapshot!(response, {
        ".timestamp" => "[timestamp]",
        ".request_id" => "[request_id]"
    });
}
```

### assert_csv_snapshot!

Tests CSV serialization of a `Serialize` value.

**Feature:** `csv`

**Examples:**
```rust
use serde::Serialize;
use insta::assert_csv_snapshot;

#[derive(Serialize)]
struct Record {
    id: u32,
    name: String,
    score: f64,
}

#[test]
fn test_csv_records() {
    let records = vec![
        Record { id: 1, name: "Alice".to_string(), score: 95.5 },
        Record { id: 2, name: "Bob".to_string(), score: 87.2 },
    ];
    assert_csv_snapshot!(records);
}
```

### assert_toml_snapshot!

Tests TOML serialization of a `Serialize` value.

**Feature:** `toml`

**Examples:**
```rust
use serde::Serialize;
use insta::assert_toml_snapshot;

#[derive(Serialize)]
struct AppConfig {
    name: String,
    version: String,
    database: DatabaseConfig,
}

#[derive(Serialize)]
struct DatabaseConfig {
    host: String,
    port: u16,
}

#[test]
fn test_toml_config() {
    let config = AppConfig {
        name: "MyApp".to_string(),
        version: "1.0.0".to_string(),
        database: DatabaseConfig {
            host: "localhost".to_string(),
            port: 5432,
        },
    };
    assert_toml_snapshot!(config);
}
```

### assert_ron_snapshot!

Tests RON (Rust Object Notation) serialization.

**Feature:** `ron`

**Examples:**
```rust
use serde::Serialize;
use insta::assert_ron_snapshot;

#[derive(Serialize)]
struct GameState {
    level: u32,
    score: u64,
    player_position: (f32, f32),
}

#[test]
fn test_game_state() {
    let state = GameState {
        level: 5,
        score: 12500,
        player_position: (10.5, 20.3),
    };
    assert_ron_snapshot!(state);
}
```

## Settings API

### Settings

The `Settings` struct controls snapshot behavior.

**Methods:**

#### Settings::clone_current()

Creates a copy of the current settings.

```rust
use insta::Settings;

let settings = Settings::clone_current();
```

#### Settings::new()

Creates new default settings.

```rust
let settings = Settings::new();
```

#### bind()

Executes a closure with these settings active.

```rust
use insta::Settings;

let mut settings = Settings::clone_current();
settings.set_sort_maps(true);

settings.bind(|| {
    // All snapshots in this closure use the custom settings
    insta::assert_yaml_snapshot!(data);
});
```

#### set_sort_maps()

Controls whether maps/objects are sorted by key.

```rust
let mut settings = Settings::clone_current();
settings.set_sort_maps(true); // Sort for consistent output
```

#### set_snapshot_path()

Sets custom snapshot directory.

```rust
settings.set_snapshot_path("custom_snapshots");
```

#### set_snapshot_suffix()

Adds suffix to snapshot names.

```rust
settings.set_snapshot_suffix("_debug");
// Results in: test_name_debug.snap
```

#### set_input_file()

Sets the input file for glob tests.

```rust
settings.set_input_file("inputs/test.txt");
```

#### set_description()

Adds description to snapshot metadata.

```rust
settings.set_description("Test with special configuration");
```

#### set_info()

Adds structured info to snapshot metadata.

```rust
settings.set_info(&serde_json::json!({
    "test_type": "integration",
    "version": "1.0"
}));
```

#### set_omit_expression()

Controls whether the expression is included in snapshots.

```rust
settings.set_omit_expression(true); // Don't show expression in snapshot
```

#### set_prepend_module_to_snapshot()

Controls module name prepending to snapshot names.

```rust
settings.set_prepend_module_to_snapshot(false);
```

### with_settings! macro

Temporarily applies settings for a block.

```rust
use insta::with_settings;

with_settings!({sort_maps => true}, {
    insta::assert_yaml_snapshot!(data);
});

// Multiple settings
with_settings!({
    sort_maps => true,
    snapshot_path => "custom_snapshots",
    description => "Custom test"
}, {
    insta::assert_yaml_snapshot!(data);
});
```

## Redactions API

**Feature:** `redactions`

### Basic Redaction Syntax

```rust
assert_yaml_snapshot!(data, {
    ".field" => "replacement",
    ".nested.field" => "[redacted]",
    ".array[0]" => "[first_item]",
    ".array[]" => "[array_item]"
});
```

### Selector Patterns

#### Field Selection
```rust
".field_name"           // Top-level field
".nested.field"         // Nested field
".user[\"name\"]"       // Bracket notation
```

#### Array Selection
```rust
".items[0]"             // Specific index
".items[]"              // All items
".items[1:3]"           // Range (items 1 and 2)
".items[2:]"            // From index 2 to end
".items[:2]"            // From start to index 2
```

#### Deep Selection
```rust
".**.id"                // All 'id' fields at any depth
".users.**.email"       // All 'email' fields under 'users'
```

### Dynamic Redactions

#### dynamic_redaction()

Creates a dynamic redaction with custom logic.

```rust
use insta::dynamic_redaction;

assert_yaml_snapshot!(data, {
    ".email" => dynamic_redaction(|value, _path| {
        if let Some(email) = value.as_str() {
            if let Some(at_pos) = email.find('@') {
                format!("{}@[domain]", &email[..at_pos])
            } else {
                "[invalid_email]".to_string()
            }
        } else {
            "[not_string]".to_string()
        }
    })
});
```

#### sorted_redaction()

Redacts with sorted content.

```rust
use insta::sorted_redaction;

assert_yaml_snapshot!(data, {
    ".tags" => sorted_redaction("[sorted_tags]"),
    ".categories" => sorted_redaction("[sorted_categories]")
});
```

### Redaction Types

The `Redaction` enum supports various replacement types:

```rust
use insta::Redaction;

// Static replacements
Redaction::from("replacement_string");
Redaction::from(42);
Redaction::from(true);

// Dynamic replacements
Redaction::Dynamic(Box::new(|value, path| {
    // Custom logic here
    Content::from("dynamic_replacement")
}));
```

## Filters API

**Feature:** `filters`

### Adding Filters

Filters apply regex replacements to the entire snapshot content.

```rust
use insta::Settings;

let mut settings = Settings::clone_current();

// Basic filter
settings.add_filter(r"\d{4}-\d{2}-\d{2}", "[date]");

// Multiple filters
settings.add_filter(r"user_\d+", "[user_id]");
settings.add_filter(r"req_[a-zA-Z0-9]+", "[request_id]");
settings.add_filter(r"\b[0-9a-f]{32}\b", "[hash]");

settings.bind(|| {
    insta::assert_snapshot!(content);
});
```

### Common Filter Patterns

```rust
let mut settings = Settings::clone_current();

// Timestamps
settings.add_filter(r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?Z?", "[timestamp]");

// UUIDs
settings.add_filter(
    r"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}",
    "[uuid]"
);

// IP addresses
settings.add_filter(r"\b\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}\b", "[ip]");

// File paths
settings.add_filter(r"/[a-zA-Z0-9_/.-]+", "[path]");

// Memory addresses
settings.add_filter(r"0x[a-f0-9]+", "[address]");
```

### Filters from Collections

```rust
use insta::Filters;

let filters = Filters::from([
    (r"\d+ms", "[duration]"),
    (r"thread-\d+", "[thread]"),
    (r"port \d+", "port [port]"),
]);

let mut settings = Settings::clone_current();
settings.set_filters(filters);
```

## Glob API

**Feature:** `glob`

### glob! macro

Tests multiple files matching a pattern.

**Syntax:**
```rust
glob!(pattern, |path| { /* test code */ })
glob!(base_path, pattern, |path| { /* test code */ })
```

**Examples:**

#### Basic Usage
```rust
use insta::glob;

#[test]
fn test_input_files() {
    glob!("inputs/*.txt", |path| {
        let content = std::fs::read_to_string(path).unwrap();
        insta::assert_snapshot!(content);
    });
}
```

#### With Custom Base Path
```rust
use std::path::Path;

#[test]
fn test_with_base_path() {
    let base = Path::new("test_data");
    glob!(base, "**/*.json", |path| {
        let content = std::fs::read_to_string(path).unwrap();
        let json: serde_json::Value = serde_json::from_str(&content).unwrap();
        insta::assert_json_snapshot!(json);
    });
}
```

#### Complex Processing
```rust
#[test]
fn test_parser_files() {
    glob!("parser_tests/**/*.input", |path| {
        let input = std::fs::read_to_string(path).unwrap();
        
        match parse_expression(&input) {
            Ok(ast) => {
                insta::assert_debug_snapshot!("ast", ast);
            }
            Err(error) => {
                insta::assert_snapshot!("error", format!("Parse error: {}", error));
            }
        }
    });
}
```

### Glob Settings

#### set_allow_empty_glob()

Allows globs that match no files.

```rust
let mut settings = Settings::clone_current();
settings.set_allow_empty_glob(true);

settings.bind(|| {
    // Won't panic if no files match
    glob!("optional_files/*.txt", |path| {
        // Process files
    });
});
```

## Environment Variables

### INSTA_UPDATE

Controls snapshot update behavior.

```bash
# Never update (default)
INSTA_UPDATE=no cargo test

# Always update
INSTA_UPDATE=always cargo test

# Only create new snapshots
INSTA_UPDATE=new cargo test

# Auto-update when appropriate
INSTA_UPDATE=auto cargo test
```

### INSTA_FORCE_PASS

Forces tests to pass even with mismatches.

```bash
INSTA_FORCE_PASS=1 cargo test
```

### INSTA_SNAPSHOT_PATH

Override snapshot directory.

```bash
INSTA_SNAPSHOT_PATH=custom_snapshots cargo test
```

### INSTA_GLOB_FILTER

Filter glob matches.

```bash
# Single filter
INSTA_GLOB_FILTER="*.rs" cargo test

# Multiple filters
INSTA_GLOB_FILTER="*.rs;*.toml;*.json" cargo test
```

### INSTA_WORKSPACE_ROOT

Set workspace root at compile time.

```bash
INSTA_WORKSPACE_ROOT=/workspace cargo test
```

### INSTA_FORCE_NO_COLOR

Disable colored output.

```bash
INSTA_FORCE_NO_COLOR=1 cargo test
```

## Types and Traits

### Content

Represents snapshot content in various formats.

```rust
use insta::Content;

// Create from various types
let content = Content::from("string");
let content = Content::from(42);
let content = Content::from(vec![1, 2, 3]);
```

### ContentPath

Represents a path in structured data for redactions.

```rust
use insta::ContentPath;

// Used in dynamic redactions
dynamic_redaction(|value, path: ContentPath| {
    println!("Path: {}", path); // e.g., ".user.profile.name"
    // Return replacement content
})
```

### Redaction

Enum for different redaction types.

```rust
use insta::Redaction;

// Static redaction
let redaction = Redaction::from("replacement");

// Dynamic redaction
let redaction = Redaction::Dynamic(Box::new(|value, path| {
    // Custom logic
    Content::from("dynamic_replacement")
}));
```

### Filters

Collection of regex-based filters.

```rust
use insta::Filters;

let filters = Filters::from([
    (r"\d+", "[number]"),
    (r"[a-f0-9]{32}", "[hash]"),
]);
```

## Utility Functions

### _function_name!

Internal macro to get current function name.

```rust
// Used internally by assertion macros
let name = insta::_function_name!();
```

### _get_workspace_root!

Internal macro to get workspace root.

```rust
// Used internally for snapshot path resolution
let root = insta::_get_workspace_root!();
```

## Advanced Usage Examples

### Custom Test Harness

```rust
use insta::Settings;

struct TestHarness {
    settings: Settings,
}

impl TestHarness {
    fn new() -> Self {
        let mut settings = Settings::clone_current();
        settings.set_sort_maps(true);
        settings.add_filter(r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z", "[timestamp]");
        
        Self { settings }
    }
    
    fn test_api_response<T: serde::Serialize>(&self, response: T) {
        self.settings.bind(|| {
            insta::assert_json_snapshot!(response, {
                ".request_id" => "[request_id]",
                ".timestamp" => "[timestamp]"
            });
        });
    }
    
    fn test_with_redactions<T: serde::Serialize>(
        &self,
        data: T,
        redactions: &[(&str, &str)],
    ) {
        self.settings.bind(|| {
            let redaction_map: std::collections::HashMap<_, _> = 
                redactions.iter().cloned().collect();
            
            // Apply redactions dynamically
            insta::assert_yaml_snapshot!(data, redaction_map);
        });
    }
}

#[test]
fn test_with_harness() {
    let harness = TestHarness::new();
    
    let response = serde_json::json!({
        "status": "success",
        "request_id": "req_123",
        "timestamp": "2023-01-15T10:30:00Z",
        "data": {"user": "alice"}
    });
    
    harness.test_api_response(response);
}
```

### Conditional Snapshots

```rust
fn conditional_snapshot<T: serde::Serialize>(name: &str, data: T, condition: bool) {
    if condition {
        insta::assert_yaml_snapshot!(name, data);
    } else {
        // Skip snapshot but still validate serialization
        let _ = serde_yaml::to_string(&data).expect("Serialization failed");
    }
}

#[test]
fn test_conditional() {
    let data = create_test_data();
    let should_snapshot = std::env::var("FULL_SNAPSHOTS").is_ok();
    
    conditional_snapshot("test_data", data, should_snapshot);
}
```

### Snapshot Comparison

```rust
fn compare_snapshots<T: serde::Serialize>(name: &str, old_data: T, new_data: T) {
    insta::assert_yaml_snapshot!(format!("{}_old", name), old_data);
    insta::assert_yaml_snapshot!(format!("{}_new", name), new_data);
}

#[test]
fn test_migration() {
    let old_format = OldDataFormat { /* ... */ };
    let new_format = migrate_data(old_format);
    
    compare_snapshots("migration_test", old_format, new_format);
}
```

This API reference covers all major features and functions available in Insta v1.43. For the most up-to-date information, always refer to the [official documentation](https://docs.rs/insta/).