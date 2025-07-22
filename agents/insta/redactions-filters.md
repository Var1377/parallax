# Redactions and Filters

Redactions and filters are powerful features that allow you to handle dynamic or sensitive data in snapshot tests. They help you focus on the important parts of your data while hiding irrelevant or changing values.

## Overview

- **Redactions**: Replace specific values in structured data (YAML, JSON, etc.) using selectors
- **Filters**: Apply regex-based transformations to the entire snapshot content

## Enabling Features

Add the required features to your `Cargo.toml`:

```toml
[dev-dependencies]
insta = { version = "1.43", features = ["redactions", "filters"] }
```

## Redactions

Redactions allow you to replace specific values in structured data using CSS-like selectors.

### Basic Redaction Syntax

```rust
use serde::Serialize;
use insta::assert_yaml_snapshot;

#[derive(Serialize)]
struct User {
    id: u32,
    name: String,
    email: String,
    created_at: String,
}

#[test]
fn test_user_with_redactions() {
    let user = User {
        id: 42,
        name: "Alice Johnson".to_string(),
        email: "alice@example.com".to_string(),
        created_at: "2023-01-15T10:30:00Z".to_string(),
    };
    
    assert_yaml_snapshot!(user, {
        ".id" => "[user_id]",
        ".created_at" => "[timestamp]"
    });
}
```

**Snapshot output:**
```yaml
---
source: tests/redaction_test.rs
expression: user
---
id: "[user_id]"
name: Alice Johnson
email: alice@example.com
created_at: "[timestamp]"
```

### Selector Syntax

#### Field Selection

```rust
// Select top-level field
".field_name" => "replacement"

// Select nested field
".user.profile.name" => "[name]"

// Alternative bracket notation
".user[\"profile\"][\"name\"]" => "[name]"
```

#### Array/Index Selection

```rust
// Select specific array index
".users[0].id" => "[first_user_id]"

// Select all array elements
".users[].id" => "[user_id]"

// Select range of elements
".items[1:3]" => "[items]"

// Select from index to end
".items[2:]" => "[remaining_items]"

// Select from start to index
".items[:2]" => "[first_items]"
```

#### Deep Selection

```rust
// Select all nested occurrences
".**.id" => "[id]"

// Select all timestamps anywhere in the structure
".**.timestamp" => "[timestamp]"
```

### Complex Redaction Examples

#### API Response with Dynamic Data

```rust
use serde::Serialize;
use insta::assert_json_snapshot;

#[derive(Serialize)]
struct ApiResponse {
    request_id: String,
    timestamp: String,
    data: Vec<Item>,
    metadata: Metadata,
}

#[derive(Serialize)]
struct Item {
    id: u32,
    name: String,
    created_at: String,
}

#[derive(Serialize)]
struct Metadata {
    total: u32,
    processing_time_ms: u64,
}

#[test]
fn test_api_response_redacted() {
    let response = ApiResponse {
        request_id: "req_abc123".to_string(),
        timestamp: "2023-01-15T10:30:00Z".to_string(),
        data: vec![
            Item {
                id: 1,
                name: "Item 1".to_string(),
                created_at: "2023-01-15T09:00:00Z".to_string(),
            },
            Item {
                id: 2,
                name: "Item 2".to_string(),
                created_at: "2023-01-15T09:15:00Z".to_string(),
            },
        ],
        metadata: Metadata {
            total: 2,
            processing_time_ms: 45,
        },
    };
    
    assert_json_snapshot!(response, {
        ".request_id" => "[request_id]",
        ".timestamp" => "[timestamp]",
        ".data[].id" => "[item_id]",
        ".data[].created_at" => "[created_at]",
        ".metadata.processing_time_ms" => "[processing_time]"
    });
}
```

#### Database Records with Sensitive Data

```rust
#[derive(Serialize)]
struct UserRecord {
    id: u32,
    username: String,
    email: String,
    password_hash: String,
    api_key: String,
    profile: UserProfile,
}

#[derive(Serialize)]
struct UserProfile {
    first_name: String,
    last_name: String,
    phone: Option<String>,
    address: Address,
}

#[derive(Serialize)]
struct Address {
    street: String,
    city: String,
    postal_code: String,
}

#[test]
fn test_user_record_privacy() {
    let user = UserRecord {
        id: 42,
        username: "alice_j".to_string(),
        email: "alice@example.com".to_string(),
        password_hash: "bcrypt$12$abcdef...".to_string(),
        api_key: "sk_live_abc123...".to_string(),
        profile: UserProfile {
            first_name: "Alice".to_string(),
            last_name: "Johnson".to_string(),
            phone: Some("+1-555-0123".to_string()),
            address: Address {
                street: "123 Main St".to_string(),
                city: "Springfield".to_string(),
                postal_code: "12345".to_string(),
            },
        },
    };
    
    assert_yaml_snapshot!(user, {
        ".id" => "[user_id]",
        ".email" => "[email]",
        ".password_hash" => "[password_hash]",
        ".api_key" => "[api_key]",
        ".profile.phone" => "[phone]",
        ".profile.address.street" => "[street]",
        ".profile.address.postal_code" => "[postal_code]"
    });
}
```

### Dynamic Redactions

For more complex redaction logic, use dynamic redactions:

```rust
use insta::{assert_yaml_snapshot, dynamic_redaction};

#[test]
fn test_dynamic_redactions() {
    let data = serde_json::json!({
        "users": [
            {"id": 1, "email": "alice@example.com"},
            {"id": 2, "email": "bob@company.org"},
            {"id": 3, "email": "charlie@test.net"}
        ]
    });
    
    assert_yaml_snapshot!(data, {
        ".users[].email" => dynamic_redaction(|value, _path| {
            // Redact email domains but keep local part
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
}
```

### Sorted Redactions

For consistent output when order doesn't matter:

```rust
use insta::{assert_yaml_snapshot, sorted_redaction};

#[test]
fn test_sorted_redactions() {
    let data = serde_json::json!({
        "tags": ["rust", "testing", "snapshot", "insta"],
        "categories": ["development", "tools", "testing"]
    });
    
    assert_yaml_snapshot!(data, {
        ".tags" => sorted_redaction("[sorted_tags]"),
        ".categories" => sorted_redaction("[sorted_categories]")
    });
}
```

## Filters

Filters apply regex-based transformations to the entire snapshot content, useful for handling timestamps, IDs, and other dynamic content in text-based snapshots.

### Basic Filter Usage

```rust
use insta::{assert_snapshot, Settings};

#[test]
fn test_with_filters() {
    let output = r#"
Request ID: req_abc123
Timestamp: 2023-01-15T10:30:00Z
Processing time: 45ms
User ID: user_789
"#;
    
    let mut settings = Settings::clone_current();
    settings.add_filter(r"req_\w+", "[request_id]");
    settings.add_filter(r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z", "[timestamp]");
    settings.add_filter(r"\d+ms", "[duration]");
    settings.add_filter(r"user_\d+", "[user_id]");
    
    settings.bind(|| {
        assert_snapshot!(output);
    });
}
```

**Snapshot output:**
```
---
source: tests/filter_test.rs
expression: output
---

Request ID: [request_id]
Timestamp: [timestamp]
Processing time: [duration]
User ID: [user_id]
```

### Common Filter Patterns

#### Timestamps and Dates

```rust
let mut settings = Settings::clone_current();

// ISO 8601 timestamps
settings.add_filter(r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?Z?", "[timestamp]");

// Unix timestamps
settings.add_filter(r"\b\d{10}\b", "[unix_timestamp]");

// Human readable dates
settings.add_filter(r"\b\d{1,2}/\d{1,2}/\d{4}\b", "[date]");

// Time durations
settings.add_filter(r"\d+(?:\.\d+)?(?:ms|s|m|h)", "[duration]");
```

#### IDs and Hashes

```rust
// UUIDs
settings.add_filter(
    r"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}", 
    "[uuid]"
);

// Short IDs
settings.add_filter(r"\b[a-zA-Z0-9]{8,}\b", "[id]");

// Hash values
settings.add_filter(r"\b[a-f0-9]{32,}\b", "[hash]");

// API keys
settings.add_filter(r"sk_live_[a-zA-Z0-9]+", "[api_key]");
settings.add_filter(r"pk_test_[a-zA-Z0-9]+", "[test_key]");
```

#### Network and System Info

```rust
// IP addresses
settings.add_filter(r"\b\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}\b", "[ip_address]");

// Ports
settings.add_filter(r":\d{4,5}\b", ":[port]");

// File paths
settings.add_filter(r"/[a-zA-Z0-9_/.-]+", "[file_path]");

// Memory addresses
settings.add_filter(r"0x[a-f0-9]+", "[memory_address]");
```

### Advanced Filter Examples

#### Log File Processing

```rust
#[test]
fn test_log_processing() {
    let log_content = r#"
2023-01-15 10:30:00.123 INFO [thread-1] Request req_abc123 started
2023-01-15 10:30:00.145 DEBUG [thread-1] Database query took 15ms
2023-01-15 10:30:00.167 INFO [thread-1] Request req_abc123 completed in 44ms
2023-01-15 10:30:01.234 ERROR [thread-2] Failed to connect to 192.168.1.100:5432
"#;
    
    let mut settings = Settings::clone_current();
    settings.add_filter(r"\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}\.\d{3}", "[timestamp]");
    settings.add_filter(r"thread-\d+", "[thread]");
    settings.add_filter(r"req_\w+", "[request_id]");
    settings.add_filter(r"\d+ms", "[duration]");
    settings.add_filter(r"\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}:\d+", "[address]");
    
    settings.bind(|| {
        assert_snapshot!(log_content);
    });
}
```

#### HTTP Response Headers

```rust
#[test]
fn test_http_headers() {
    let headers = r#"
HTTP/1.1 200 OK
Date: Mon, 15 Jan 2023 10:30:00 GMT
Server: nginx/1.18.0
Content-Length: 1234
ETag: "abc123def456"
X-Request-ID: req_xyz789
Set-Cookie: session=abc123; Path=/; HttpOnly
"#;
    
    let mut settings = Settings::clone_current();
    settings.add_filter(r"Date: [^\r\n]+", "Date: [date]");
    settings.add_filter(r"Server: [^\r\n]+", "Server: [server]");
    settings.add_filter(r"Content-Length: \d+", "Content-Length: [length]");
    settings.add_filter(r#"ETag: "[^"]+""#, r#"ETag: "[etag]""#);
    settings.add_filter(r"X-Request-ID: [^\r\n]+", "X-Request-ID: [request_id]");
    settings.add_filter(r"session=[^;]+", "session=[session]");
    
    settings.bind(|| {
        assert_snapshot!(headers);
    });
}
```

## Combining Redactions and Filters

You can use both redactions and filters together for comprehensive data sanitization:

```rust
#[test]
fn test_combined_redactions_and_filters() {
    let api_log = serde_json::json!({
        "request_id": "req_abc123",
        "timestamp": "2023-01-15T10:30:00Z",
        "user": {
            "id": 42,
            "email": "alice@example.com"
        },
        "response_body": "User alice@example.com logged in at 2023-01-15T10:30:00Z"
    });
    
    let mut settings = Settings::clone_current();
    // Filter for the response_body string content
    settings.add_filter(r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z", "[timestamp]");
    settings.add_filter(r"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}", "[email]");
    
    settings.bind(|| {
        assert_json_snapshot!(api_log, {
            // Redactions for structured data
            ".request_id" => "[request_id]",
            ".timestamp" => "[timestamp]",
            ".user.id" => "[user_id]",
            ".user.email" => "[email]"
            // response_body will be filtered by regex
        });
    });
}
```

## Settings Configuration

### Global Settings

```rust
use insta::Settings;

fn setup_test_settings() -> Settings {
    let mut settings = Settings::clone_current();
    
    // Add common redactions
    settings.set_sort_maps(true);
    
    // Add common filters
    settings.add_filter(r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z", "[timestamp]");
    settings.add_filter(r"[a-f0-9]{32,}", "[hash]");
    
    settings
}

#[test]
fn test_with_global_settings() {
    setup_test_settings().bind(|| {
        assert_yaml_snapshot!(data);
    });
}
```

### Per-Test Settings

```rust
#[test]
fn test_specific_redactions() {
    let mut settings = Settings::clone_current();
    settings.add_redaction(".sensitive_field", "[redacted]");
    settings.add_filter(r"secret_\w+", "[secret]");
    
    settings.bind(|| {
        assert_yaml_snapshot!(sensitive_data);
    });
}
```

## Best Practices

### 1. Be Specific with Selectors

```rust
// ❌ Too broad - might redact unintended fields
".id" => "[id]"

// ✅ More specific
".user.id" => "[user_id]"
".order.id" => "[order_id]"
```

### 2. Use Meaningful Replacement Values

```rust
// ❌ Generic replacements
".timestamp" => "XXX"
".email" => "YYY"

// ✅ Descriptive replacements
".timestamp" => "[timestamp]"
".email" => "[email_address]"
```

### 3. Document Your Redactions

```rust
#[test]
fn test_user_api_response() {
    let response = get_user_response();
    
    assert_json_snapshot!(response, {
        // Hide dynamic server-generated values
        ".request_id" => "[request_id]",
        ".timestamp" => "[timestamp]",
        
        // Hide sensitive user data
        ".user.email" => "[email]",
        ".user.phone" => "[phone]",
        
        // Hide variable performance metrics
        ".metadata.processing_time_ms" => "[processing_time]"
    });
}
```

### 4. Test Your Redactions

```rust
#[test]
fn test_redaction_patterns() {
    // Verify that redactions work as expected
    let test_data = serde_json::json!({
        "user_id": 123,
        "admin_id": 456,
        "guest_id": 789
    });
    
    assert_json_snapshot!(test_data, {
        ".user_id" => "[user_id]",
        ".admin_id" => "[admin_id]",
        ".guest_id" => "[guest_id]"
    });
}
```

### 5. Use Filters for Text Content

```rust
// ✅ Use filters for unstructured text
let mut settings = Settings::clone_current();
settings.add_filter(r"Session: \w+", "Session: [session_id]");

// ✅ Use redactions for structured data
assert_json_snapshot!(data, {
    ".session_id" => "[session_id]"
});
```

## Troubleshooting

### Redaction Not Working

1. **Check selector syntax**:
   ```rust
   // ❌ Missing dot prefix
   "field" => "[value]"
   
   // ✅ Correct syntax
   ".field" => "[value]"
   ```

2. **Verify data structure**:
   ```rust
   // Print the structure to understand the path
   println!("{:#?}", serde_json::to_value(&data));
   ```

3. **Test selectors individually**:
   ```rust
   assert_json_snapshot!(data, {
       ".field1" => "[test1]"
   });
   // Then add more selectors one by one
   ```

### Filter Not Matching

1. **Test regex patterns**:
   ```rust
   let regex = regex::Regex::new(r"your_pattern").unwrap();
   assert!(regex.is_match("test_string"));
   ```

2. **Escape special characters**:
   ```rust
   // ❌ Unescaped dots match any character
   settings.add_filter(r"192.168.1.1", "[ip]");
   
   // ✅ Escaped dots match literal dots
   settings.add_filter(r"192\.168\.1\.1", "[ip]");
   ```

3. **Check filter order**:
   ```rust
   // Filters are applied in order - more specific first
   settings.add_filter(r"special_id_\d+", "[special_id]");
   settings.add_filter(r"id_\d+", "[id]");  // This should come after
   ```