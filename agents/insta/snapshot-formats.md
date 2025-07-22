# Snapshot Formats

Insta supports multiple serialization formats for different use cases. Each format has its own macro and specific advantages.

## Available Formats

| Format | Feature Flag | Macro | Best For |
|--------|-------------|-------|----------|
| Debug | (built-in) | `assert_debug_snapshot!` | Rust types, debugging |
| Display | (built-in) | `assert_snapshot!` | Strings, simple output |
| YAML | `yaml` | `assert_yaml_snapshot!` | Human-readable structured data |
| JSON | `json` | `assert_json_snapshot!` | API responses, web data |
| CSV | `csv` | `assert_csv_snapshot!` | Tabular data |
| TOML | `toml` | `assert_toml_snapshot!` | Configuration files |
| RON | `ron` | `assert_ron_snapshot!` | Rust-native serialization |

## Debug Format

The most basic format, using Rust's `Debug` trait:

```rust
use insta::assert_debug_snapshot;

#[derive(Debug)]
struct Point {
    x: f64,
    y: f64,
}

#[test]
fn test_debug_format() {
    let point = Point { x: 3.14, y: 2.71 };
    assert_debug_snapshot!(point);
}
```

**Snapshot output:**
```
---
source: tests/formats.rs
expression: point
---
Point {
    x: 3.14,
    y: 2.71,
}
```

## Display Format

For types implementing `Display` or string content:

```rust
use insta::assert_snapshot;

#[test]
fn test_display_format() {
    let message = format!("User {} has {} points", "Alice", 1250);
    assert_snapshot!(message);
}

#[test]
fn test_multiline_string() {
    let config = r#"
[database]
host = "localhost"
port = 5432
name = "myapp"
"#;
    assert_snapshot!(config);
}
```

## YAML Format

Human-readable format, excellent for complex nested structures:

```rust
use serde::Serialize;
use insta::assert_yaml_snapshot;

#[derive(Serialize)]
struct User {
    id: u32,
    name: String,
    email: String,
    preferences: UserPreferences,
}

#[derive(Serialize)]
struct UserPreferences {
    theme: String,
    notifications: bool,
    language: String,
}

#[test]
fn test_yaml_format() {
    let user = User {
        id: 42,
        name: "Alice Johnson".to_string(),
        email: "alice@example.com".to_string(),
        preferences: UserPreferences {
            theme: "dark".to_string(),
            notifications: true,
            language: "en".to_string(),
        },
    };
    
    assert_yaml_snapshot!(user);
}
```

**Snapshot output:**
```yaml
---
source: tests/formats.rs
expression: user
---
id: 42
name: Alice Johnson
email: alice@example.com
preferences:
  theme: dark
  notifications: true
  language: en
```

## JSON Format

Perfect for API testing and web applications:

```rust
use serde::Serialize;
use insta::assert_json_snapshot;

#[derive(Serialize)]
struct ApiResponse {
    status: String,
    data: Vec<Item>,
    meta: Meta,
}

#[derive(Serialize)]
struct Item {
    id: u32,
    title: String,
    published: bool,
}

#[derive(Serialize)]
struct Meta {
    total: u32,
    page: u32,
    per_page: u32,
}

#[test]
fn test_api_response() {
    let response = ApiResponse {
        status: "success".to_string(),
        data: vec![
            Item {
                id: 1,
                title: "First Post".to_string(),
                published: true,
            },
            Item {
                id: 2,
                title: "Draft Post".to_string(),
                published: false,
            },
        ],
        meta: Meta {
            total: 2,
            page: 1,
            per_page: 10,
        },
    };
    
    assert_json_snapshot!(response);
}
```

**Snapshot output:**
```json
---
source: tests/formats.rs
expression: response
---
{
  "status": "success",
  "data": [
    {
      "id": 1,
      "title": "First Post",
      "published": true
    },
    {
      "id": 2,
      "title": "Draft Post",
      "published": false
    }
  ],
  "meta": {
    "total": 2,
    "page": 1,
    "per_page": 10
  }
}
```

## CSV Format

Ideal for tabular data and spreadsheet-like structures:

```rust
use serde::Serialize;
use insta::assert_csv_snapshot;

#[derive(Serialize)]
struct SalesRecord {
    date: String,
    product: String,
    quantity: u32,
    price: f64,
}

#[test]
fn test_csv_format() {
    let records = vec![
        SalesRecord {
            date: "2023-01-15".to_string(),
            product: "Widget A".to_string(),
            quantity: 10,
            price: 29.99,
        },
        SalesRecord {
            date: "2023-01-16".to_string(),
            product: "Widget B".to_string(),
            quantity: 5,
            price: 49.99,
        },
    ];
    
    assert_csv_snapshot!(records);
}
```

**Snapshot output:**
```csv
---
source: tests/formats.rs
expression: records
---
date,product,quantity,price
2023-01-15,Widget A,10,29.99
2023-01-16,Widget B,5,49.99
```

## TOML Format

Great for configuration files and settings:

```rust
use serde::Serialize;
use insta::assert_toml_snapshot;

#[derive(Serialize)]
struct AppConfig {
    app_name: String,
    version: String,
    database: DatabaseConfig,
    features: Vec<String>,
}

#[derive(Serialize)]
struct DatabaseConfig {
    host: String,
    port: u16,
    name: String,
    ssl: bool,
}

#[test]
fn test_toml_format() {
    let config = AppConfig {
        app_name: "MyApp".to_string(),
        version: "1.0.0".to_string(),
        database: DatabaseConfig {
            host: "localhost".to_string(),
            port: 5432,
            name: "myapp_db".to_string(),
            ssl: false,
        },
        features: vec!["auth".to_string(), "api".to_string(), "admin".to_string()],
    };
    
    assert_toml_snapshot!(config);
}
```

**Snapshot output:**
```toml
---
source: tests/formats.rs
expression: config
---
app_name = "MyApp"
version = "1.0.0"
features = ["auth", "api", "admin"]

[database]
host = "localhost"
port = 5432
name = "myapp_db"
ssl = false
```

## RON Format

Rust Object Notation - native Rust serialization:

```rust
use serde::Serialize;
use insta::assert_ron_snapshot;

#[derive(Serialize)]
struct GameState {
    player: Player,
    enemies: Vec<Enemy>,
    level: u32,
}

#[derive(Serialize)]
struct Player {
    name: String,
    health: u32,
    position: (f32, f32),
}

#[derive(Serialize)]
struct Enemy {
    enemy_type: String,
    health: u32,
    position: (f32, f32),
}

#[test]
fn test_ron_format() {
    let game_state = GameState {
        player: Player {
            name: "Hero".to_string(),
            health: 100,
            position: (10.5, 20.3),
        },
        enemies: vec![
            Enemy {
                enemy_type: "Goblin".to_string(),
                health: 30,
                position: (15.0, 25.0),
            },
        ],
        level: 1,
    };
    
    assert_ron_snapshot!(game_state);
}
```

## Format-Specific Features

### JSON Pretty Printing

JSON snapshots are automatically pretty-printed with proper indentation.

### YAML Comments

YAML format preserves structure and is highly readable for complex nested data.

### CSV Headers

CSV format automatically includes headers based on struct field names.

### TOML Sections

TOML format organizes data into logical sections, perfect for configuration files.

## Choosing the Right Format

### Use Debug Format When:
- Testing Rust-specific types
- Debugging complex data structures
- Working with types that don't implement Serialize

### Use Display Format When:
- Testing string output
- Working with simple text content
- Testing formatted output

### Use YAML Format When:
- Human readability is important
- Working with complex nested structures
- Configuration-like data
- Need to manually edit snapshots

### Use JSON Format When:
- Testing API responses
- Working with web applications
- Need compatibility with other tools
- Structured data exchange

### Use CSV Format When:
- Testing tabular data
- Working with spreadsheet-like structures
- Simple data analysis
- Reporting functionality

### Use TOML Format When:
- Testing configuration files
- Application settings
- Simple key-value structures

### Use RON Format When:
- Working with Rust-native data
- Need precise type representation
- Complex Rust-specific structures

## Enabling Features

Add the required features to your `Cargo.toml`:

```toml
[dev-dependencies]
insta = { 
    version = "1.43", 
    features = [
        "yaml",      # assert_yaml_snapshot!
        "json",      # assert_json_snapshot!
        "csv",       # assert_csv_snapshot!
        "toml",      # assert_toml_snapshot!
        "ron",       # assert_ron_snapshot!
    ]
}
```

## Format Conversion

You can easily switch between formats by changing the macro:

```rust
// Same data, different formats
let data = MyStruct { /* ... */ };

assert_yaml_snapshot!(data);  // YAML format
assert_json_snapshot!(data);  // JSON format
assert_toml_snapshot!(data);  // TOML format
```

Each will create a separate snapshot file with the appropriate extension and formatting.