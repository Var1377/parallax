# Custom Serialization

Insta provides flexible serialization options beyond the built-in formats. You can customize how your data is serialized for snapshot testing, implement custom serializers, and control the serialization process.

## Overview

Custom serialization in Insta allows you to:

- Implement custom `Serialize` traits for your types
- Control serialization behavior with serde attributes
- Create custom serialization functions
- Handle complex data structures that don't serialize well by default
- Optimize snapshot readability and maintainability

## Custom Serialize Implementations

### Basic Custom Serialization

```rust
use serde::{Serialize, Serializer};
use insta::assert_yaml_snapshot;

#[derive(Debug)]
struct Point {
    x: f64,
    y: f64,
}

impl Serialize for Point {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // Custom serialization as a string
        let point_str = format!("({}, {})", self.x, self.y);
        serializer.serialize_str(&point_str)
    }
}

#[test]
fn test_custom_point_serialization() {
    let point = Point { x: 3.14, y: 2.71 };
    assert_yaml_snapshot!(point);
}
```

**Snapshot output:**
```yaml
---
source: tests/custom_serialization.rs
expression: point
---
"(3.14, 2.71)"
```

### Complex Custom Serialization

```rust
use serde::{Serialize, Serializer};
use serde::ser::{SerializeStruct, SerializeSeq};
use std::collections::HashMap;

#[derive(Debug)]
struct Database {
    tables: HashMap<String, Table>,
    connections: u32,
    version: String,
}

#[derive(Debug)]
struct Table {
    name: String,
    rows: u64,
    columns: Vec<String>,
}

impl Serialize for Database {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("Database", 4)?;
        
        // Custom field ordering and formatting
        state.serialize_field("version", &self.version)?;
        state.serialize_field("active_connections", &self.connections)?;
        state.serialize_field("table_count", &self.tables.len())?;
        
        // Custom table summary
        let table_summary: Vec<_> = self.tables.values()
            .map(|table| format!("{} ({} rows)", table.name, table.rows))
            .collect();
        state.serialize_field("tables", &table_summary)?;
        
        state.end()
    }
}

#[test]
fn test_database_serialization() {
    let mut tables = HashMap::new();
    tables.insert("users".to_string(), Table {
        name: "users".to_string(),
        rows: 1000,
        columns: vec!["id".to_string(), "name".to_string(), "email".to_string()],
    });
    tables.insert("orders".to_string(), Table {
        name: "orders".to_string(),
        rows: 5000,
        columns: vec!["id".to_string(), "user_id".to_string(), "total".to_string()],
    });
    
    let db = Database {
        tables,
        connections: 25,
        version: "1.2.3".to_string(),
    };
    
    assert_yaml_snapshot!(db);
}
```

## Serde Attributes for Customization

### Field Control

```rust
use serde::Serialize;
use insta::assert_yaml_snapshot;

#[derive(Serialize)]
struct User {
    id: u32,
    
    #[serde(rename = "full_name")]
    name: String,
    
    #[serde(skip)]
    password_hash: String,
    
    #[serde(skip_serializing_if = "Option::is_none")]
    phone: Option<String>,
    
    #[serde(flatten)]
    metadata: UserMetadata,
}

#[derive(Serialize)]
struct UserMetadata {
    created_at: String,
    last_login: String,
}

#[test]
fn test_user_with_attributes() {
    let user = User {
        id: 42,
        name: "Alice Johnson".to_string(),
        password_hash: "secret_hash".to_string(),
        phone: None,
        metadata: UserMetadata {
            created_at: "2023-01-15T10:30:00Z".to_string(),
            last_login: "2023-01-20T14:15:00Z".to_string(),
        },
    };
    
    assert_yaml_snapshot!(user);
}
```

**Snapshot output:**
```yaml
---
source: tests/custom_serialization.rs
expression: user
---
id: 42
full_name: Alice Johnson
created_at: "2023-01-15T10:30:00Z"
last_login: "2023-01-20T14:15:00Z"
```

### Custom Serialization Functions

```rust
use serde::{Serialize, Serializer};
use std::collections::HashMap;

fn serialize_sorted_map<S>(
    map: &HashMap<String, i32>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let mut sorted: Vec<_> = map.iter().collect();
    sorted.sort_by_key(|&(k, _)| k);
    sorted.serialize(serializer)
}

#[derive(Serialize)]
struct Config {
    name: String,
    
    #[serde(serialize_with = "serialize_sorted_map")]
    settings: HashMap<String, i32>,
}

#[test]
fn test_sorted_map_serialization() {
    let mut settings = HashMap::new();
    settings.insert("timeout".to_string(), 30);
    settings.insert("retries".to_string(), 3);
    settings.insert("buffer_size".to_string(), 1024);
    
    let config = Config {
        name: "MyApp".to_string(),
        settings,
    };
    
    assert_yaml_snapshot!(config);
}
```

## Wrapper Types for Custom Serialization

### Display Wrapper

```rust
use serde::Serialize;
use std::fmt;

struct DisplayWrapper<T>(T);

impl<T: fmt::Display> Serialize for DisplayWrapper<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.0.to_string())
    }
}

#[derive(Debug)]
struct ComplexNumber {
    real: f64,
    imag: f64,
}

impl fmt::Display for ComplexNumber {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.imag >= 0.0 {
            write!(f, "{}+{}i", self.real, self.imag)
        } else {
            write!(f, "{}{}i", self.real, self.imag)
        }
    }
}

#[test]
fn test_display_wrapper() {
    let numbers = vec![
        DisplayWrapper(ComplexNumber { real: 1.0, imag: 2.0 }),
        DisplayWrapper(ComplexNumber { real: 3.0, imag: -4.0 }),
    ];
    
    insta::assert_yaml_snapshot!(numbers);
}
```

### Summary Wrapper

```rust
use serde::Serialize;

struct SummaryWrapper<T> {
    data: T,
    summary_fn: fn(&T) -> String,
}

impl<T> SummaryWrapper<T> {
    fn new(data: T, summary_fn: fn(&T) -> String) -> Self {
        Self { data, summary_fn }
    }
}

impl<T> Serialize for SummaryWrapper<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let summary = (self.summary_fn)(&self.data);
        serializer.serialize_str(&summary)
    }
}

#[test]
fn test_summary_wrapper() {
    let large_vec: Vec<i32> = (1..=1000).collect();
    
    let wrapped = SummaryWrapper::new(large_vec, |v| {
        format!("Vec with {} elements, sum: {}", v.len(), v.iter().sum::<i32>())
    });
    
    insta::assert_yaml_snapshot!(wrapped);
}
```

## Handling Non-Serializable Types

### Converting to Serializable Forms

```rust
use serde::Serialize;
use std::sync::{Arc, Mutex};
use std::thread;

// Non-serializable type
struct ThreadPool {
    workers: Vec<thread::JoinHandle<()>>,
    sender: Arc<Mutex<Option<String>>>,
}

// Serializable representation
#[derive(Serialize)]
struct ThreadPoolSnapshot {
    worker_count: usize,
    has_sender: bool,
    status: String,
}

impl From<&ThreadPool> for ThreadPoolSnapshot {
    fn from(pool: &ThreadPool) -> Self {
        ThreadPoolSnapshot {
            worker_count: pool.workers.len(),
            has_sender: pool.sender.lock().unwrap().is_some(),
            status: "active".to_string(),
        }
    }
}

#[test]
fn test_thread_pool_snapshot() {
    // Simulate a thread pool (simplified)
    let pool = ThreadPool {
        workers: vec![], // Empty for test
        sender: Arc::new(Mutex::new(Some("channel".to_string()))),
    };
    
    let snapshot = ThreadPoolSnapshot::from(&pool);
    insta::assert_yaml_snapshot!(snapshot);
}
```

### Custom Debug for Snapshot Testing

```rust
use serde::Serialize;
use std::fmt;

#[derive(Serialize)]
struct DebugSnapshot {
    type_name: String,
    summary: String,
    details: serde_json::Value,
}

trait SnapshotDebug {
    fn to_snapshot(&self) -> DebugSnapshot;
}

struct FileHandle {
    path: String,
    size: u64,
    permissions: u32,
}

impl SnapshotDebug for FileHandle {
    fn to_snapshot(&self) -> DebugSnapshot {
        DebugSnapshot {
            type_name: "FileHandle".to_string(),
            summary: format!("{} ({} bytes)", self.path, self.size),
            details: serde_json::json!({
                "path": self.path,
                "size": self.size,
                "permissions": format!("{:o}", self.permissions),
                "readable": self.permissions & 0o444 != 0,
                "writable": self.permissions & 0o222 != 0,
            }),
        }
    }
}

#[test]
fn test_file_handle_debug() {
    let file = FileHandle {
        path: "/tmp/test.txt".to_string(),
        size: 1024,
        permissions: 0o644,
    };
    
    insta::assert_yaml_snapshot!(file.to_snapshot());
}
```

## Advanced Serialization Patterns

### Conditional Serialization

```rust
use serde::{Serialize, Serializer};

#[derive(Serialize)]
struct ApiResponse {
    status: String,
    data: serde_json::Value,
    
    #[serde(serialize_with = "serialize_debug_info")]
    debug_info: Option<DebugInfo>,
}

#[derive(Debug)]
struct DebugInfo {
    query_time: f64,
    cache_hit: bool,
    server_id: String,
}

fn serialize_debug_info<S>(
    debug_info: &Option<DebugInfo>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    match debug_info {
        Some(info) if std::env::var("DEBUG").is_ok() => {
            // Only serialize debug info if DEBUG env var is set
            serde_json::json!({
                "query_time_ms": (info.query_time * 1000.0) as u32,
                "cache_hit": info.cache_hit,
                "server": info.server_id
            }).serialize(serializer)
        }
        _ => serializer.serialize_none(),
    }
}

#[test]
fn test_conditional_debug_info() {
    std::env::set_var("DEBUG", "1");
    
    let response = ApiResponse {
        status: "success".to_string(),
        data: serde_json::json!({"users": []}),
        debug_info: Some(DebugInfo {
            query_time: 0.045,
            cache_hit: true,
            server_id: "web-01".to_string(),
        }),
    };
    
    insta::assert_yaml_snapshot!(response);
    
    std::env::remove_var("DEBUG");
}
```

### Hierarchical Serialization

```rust
use serde::Serialize;

#[derive(Serialize)]
#[serde(tag = "type", content = "data")]
enum Node {
    Leaf { value: i32 },
    Branch { 
        left: Box<Node>, 
        right: Box<Node> 
    },
}

impl Node {
    fn leaf(value: i32) -> Self {
        Node::Leaf { value }
    }
    
    fn branch(left: Node, right: Node) -> Self {
        Node::Branch {
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}

#[test]
fn test_tree_serialization() {
    let tree = Node::branch(
        Node::branch(
            Node::leaf(1),
            Node::leaf(2)
        ),
        Node::leaf(3)
    );
    
    insta::assert_yaml_snapshot!(tree);
}
```

## Performance Considerations

### Lazy Serialization

```rust
use serde::Serialize;
use std::sync::Arc;

#[derive(Clone)]
struct ExpensiveData {
    data: Arc<Vec<u8>>,
}

#[derive(Serialize)]
struct ExpensiveDataSnapshot {
    size: usize,
    checksum: u32,
    sample: Vec<u8>,
}

impl From<&ExpensiveData> for ExpensiveDataSnapshot {
    fn from(data: &ExpensiveData) -> Self {
        ExpensiveDataSnapshot {
            size: data.data.len(),
            checksum: simple_checksum(&data.data),
            sample: data.data.iter().take(10).cloned().collect(),
        }
    }
}

fn simple_checksum(data: &[u8]) -> u32 {
    data.iter().fold(0u32, |acc, &b| acc.wrapping_add(b as u32))
}

#[test]
fn test_expensive_data() {
    let data = ExpensiveData {
        data: Arc::new(vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]),
    };
    
    let snapshot = ExpensiveDataSnapshot::from(&data);
    insta::assert_yaml_snapshot!(snapshot);
}
```

### Streaming Serialization for Large Data

```rust
use serde::Serialize;

#[derive(Serialize)]
struct DatasetSummary {
    total_records: usize,
    schema: Vec<String>,
    sample_records: Vec<serde_json::Value>,
    statistics: DatasetStats,
}

#[derive(Serialize)]
struct DatasetStats {
    min_id: u32,
    max_id: u32,
    avg_size: f64,
}

struct LargeDataset {
    records: Vec<Record>,
}

struct Record {
    id: u32,
    data: String,
}

impl LargeDataset {
    fn to_summary(&self, sample_size: usize) -> DatasetSummary {
        let sample_records: Vec<_> = self.records
            .iter()
            .take(sample_size)
            .map(|r| serde_json::json!({
                "id": r.id,
                "data_length": r.data.len(),
                "data_preview": r.data.chars().take(20).collect::<String>()
            }))
            .collect();
            
        let stats = DatasetStats {
            min_id: self.records.iter().map(|r| r.id).min().unwrap_or(0),
            max_id: self.records.iter().map(|r| r.id).max().unwrap_or(0),
            avg_size: self.records.iter()
                .map(|r| r.data.len())
                .sum::<usize>() as f64 / self.records.len() as f64,
        };
        
        DatasetSummary {
            total_records: self.records.len(),
            schema: vec!["id".to_string(), "data".to_string()],
            sample_records,
            statistics: stats,
        }
    }
}

#[test]
fn test_large_dataset() {
    let dataset = LargeDataset {
        records: (1..=1000).map(|i| Record {
            id: i,
            data: format!("Record {} with some data", i),
        }).collect(),
    };
    
    let summary = dataset.to_summary(3);
    insta::assert_yaml_snapshot!(summary);
}
```

## Best Practices

### 1. Keep Snapshots Readable

```rust
// ❌ Hard to read
impl Serialize for ComplexStruct {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: Serializer {
        serializer.serialize_str(&format!("{:?}", self))
    }
}

// ✅ Clear and structured
impl Serialize for ComplexStruct {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: Serializer {
        use serde::ser::SerializeStruct;
        let mut state = serializer.serialize_struct("ComplexStruct", 3)?;
        state.serialize_field("summary", &self.get_summary())?;
        state.serialize_field("key_metrics", &self.get_metrics())?;
        state.serialize_field("status", &self.status.to_string())?;
        state.end()
    }
}
```

### 2. Use Consistent Formatting

```rust
// Create helper functions for consistent formatting
fn serialize_timestamp<S>(timestamp: &SystemTime, serializer: S) -> Result<S::Ok, S::Error>
where S: Serializer {
    let formatted = timestamp
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs();
    serializer.serialize_str(&format!("timestamp_{}", formatted))
}

fn serialize_size<S>(size: &u64, serializer: S) -> Result<S::Ok, S::Error>
where S: Serializer {
    let formatted = match *size {
        s if s < 1024 => format!("{}B", s),
        s if s < 1024 * 1024 => format!("{}KB", s / 1024),
        s => format!("{}MB", s / (1024 * 1024)),
    };
    serializer.serialize_str(&formatted)
}
```

### 3. Handle Edge Cases

```rust
impl Serialize for MyStruct {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: Serializer {
        // Handle empty collections
        if self.items.is_empty() {
            return serializer.serialize_str("empty");
        }
        
        // Handle very large collections
        if self.items.len() > 100 {
            return serializer.serialize_str(&format!("large_collection_{}_items", self.items.len()));
        }
        
        // Normal serialization
        self.items.serialize(serializer)
    }
}
```

### 4. Document Custom Serialization

```rust
/// Custom serialization for database connections.
/// 
/// Serializes only the connection metadata, not the actual connection
/// objects which are not serializable. Includes connection count,
/// pool status, and configuration summary.
impl Serialize for ConnectionPool {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: Serializer {
        // Implementation...
    }
}
```

### 5. Test Serialization Separately

```rust
#[test]
fn test_custom_serialization_format() {
    let data = MyStruct::new();
    let serialized = serde_yaml::to_string(&data).unwrap();
    
    // Test that serialization produces expected format
    assert!(serialized.contains("expected_field"));
    assert!(!serialized.contains("internal_field"));
    
    // Then test with insta
    insta::assert_yaml_snapshot!(data);
}
```

## Troubleshooting

### Serialization Errors

```rust
// Debug serialization issues
#[test]
fn debug_serialization() {
    let data = MyStruct::new();
    
    // Test individual serialization
    match serde_yaml::to_string(&data) {
        Ok(yaml) => println!("Serialized YAML:\n{}", yaml),
        Err(e) => panic!("Serialization failed: {}", e),
    }
    
    // Then use in snapshot
    insta::assert_yaml_snapshot!(data);
}
```

### Performance Issues

```rust
// Profile serialization performance
#[test]
fn profile_serialization() {
    let data = create_large_data();
    
    let start = std::time::Instant::now();
    let _serialized = serde_yaml::to_string(&data).unwrap();
    let duration = start.elapsed();
    
    println!("Serialization took: {:?}", duration);
    assert!(duration < std::time::Duration::from_millis(100));
}
```