# Error Propagation Patterns in Rust

Error propagation is the process of passing errors up the call stack from where they occur to where they can be appropriately handled. Rust provides several powerful mechanisms for error propagation that maintain type safety while being ergonomic to use.

## Table of Contents

1. [The `?` Operator](#the--operator)
2. [Error Transformation Patterns](#error-transformation-patterns)
3. [Propagation with Context](#propagation-with-context)
4. [Conditional Propagation](#conditional-propagation)
5. [Async Error Propagation](#async-error-propagation)
6. [Error Aggregation](#error-aggregation)
7. [Performance Considerations](#performance-considerations)
8. [Best Practices](#best-practices)

## The `?` Operator

The `?` operator is Rust's primary mechanism for error propagation. It provides a concise way to handle the common pattern of "if this operation fails, return the error; otherwise, continue with the success value."

### Basic Usage

```rust
use std::fs;
use std::io;

fn read_username_from_file() -> Result<String, io::Error> {
    let mut username = fs::read_to_string("username.txt")?;
    username.pop(); // Remove trailing newline
    Ok(username)
}

// Equivalent to:
fn read_username_from_file_verbose() -> Result<String, io::Error> {
    let username_result = fs::read_to_string("username.txt");
    let mut username = match username_result {
        Ok(username) => username,
        Err(error) => return Err(error),
    };
    username.pop();
    Ok(username)
}
```

### Chaining Operations

```rust
use anyhow::Result;

fn process_config_file() -> Result<ProcessedConfig> {
    let content = std::fs::read_to_string("config.toml")?;
    let raw_config: RawConfig = toml::from_str(&content)?;
    let validated_config = validate_config(raw_config)?;
    let processed_config = process_config(validated_config)?;
    Ok(processed_config)
}
```

### How `?` Works

The `?` operator is syntactic sugar for the `Try` trait. It:

1. Calls `Into::into` on the error type if conversion is needed
2. Returns early if the result is an error
3. Unwraps the success value if the result is ok

```rust
// This:
let value = some_operation()?;

// Expands to approximately:
let value = match some_operation() {
    Ok(val) => val,
    Err(err) => return Err(err.into()),
};
```

## Error Transformation Patterns

### Manual Error Conversion with `map_err`

```rust
use std::num::ParseIntError;

#[derive(Debug)]
enum MyError {
    Io(std::io::Error),
    Parse(ParseIntError),
    Custom(String),
}

impl From<std::io::Error> for MyError {
    fn from(error: std::io::Error) -> Self {
        MyError::Io(error)
    }
}

fn read_number_from_file(path: &str) -> Result<i32, MyError> {
    let content = std::fs::read_to_string(path)?; // io::Error -> MyError
    let number = content.trim().parse::<i32>()
        .map_err(MyError::Parse)?; // ParseIntError -> MyError
    Ok(number)
}
```

### Using `thiserror` for Automatic Conversion

```rust
use thiserror::Error;

#[derive(Error, Debug)]
enum ConfigError {
    #[error("IO error")]
    Io(#[from] std::io::Error),
    
    #[error("TOML parsing error")]
    Toml(#[from] toml::de::Error),
    
    #[error("Validation error: {message}")]
    Validation { message: String },
}

fn load_config(path: &str) -> Result<Config, ConfigError> {
    let content = std::fs::read_to_string(path)?; // Auto-converts io::Error
    let config: Config = toml::from_str(&content)?; // Auto-converts toml::de::Error
    
    if config.version < MIN_VERSION {
        return Err(ConfigError::Validation {
            message: format!("Config version {} too old", config.version),
        });
    }
    
    Ok(config)
}
```

### Error Wrapping Patterns

```rust
use anyhow::{Context, Result};

// Wrapping with additional context
fn load_user_data(user_id: u64) -> Result<UserData> {
    let path = format!("users/{}.json", user_id);
    let content = std::fs::read_to_string(&path)
        .with_context(|| format!("Failed to read user file: {}", path))?;
    
    let user_data: UserData = serde_json::from_str(&content)
        .with_context(|| format!("Failed to parse user data for user {}", user_id))?;
    
    Ok(user_data)
}

// Wrapping with error transformation
fn parse_config_with_defaults(content: &str) -> Result<Config> {
    toml::from_str(content)
        .map_err(|e| anyhow::anyhow!("Invalid TOML configuration: {}", e))
        .and_then(|mut config: Config| {
            apply_defaults(&mut config)?;
            validate_config(&config)?;
            Ok(config)
        })
}
```

## Propagation with Context

### Static Context

```rust
use anyhow::{Context, Result};

fn backup_database() -> Result<()> {
    create_backup_directory()
        .context("Failed to create backup directory")?;
    
    dump_database_schema()
        .context("Failed to dump database schema")?;
    
    dump_database_data()
        .context("Failed to dump database data")?;
    
    compress_backup()
        .context("Failed to compress backup files")?;
    
    Ok(())
}
```

### Dynamic Context

```rust
use anyhow::{Context, Result};

fn process_files(files: &[PathBuf]) -> Result<Vec<ProcessedFile>> {
    let mut results = Vec::new();
    
    for (index, file) in files.iter().enumerate() {
        let processed = process_single_file(file)
            .with_context(|| {
                format!("Failed to process file {} (index {}): {}", 
                       file.display(), index, file.display())
            })?;
        
        results.push(processed);
    }
    
    Ok(results)
}

fn download_and_process(urls: &[String]) -> Result<()> {
    for (i, url) in urls.iter().enumerate() {
        let content = download_content(url)
            .with_context(|| format!("Failed to download from {} (item {})", url, i))?;
        
        process_content(&content)
            .with_context(|| format!("Failed to process content from {}", url))?;
    }
    Ok(())
}
```

### Contextual Error Chains

```rust
use anyhow::{Context, Result};

fn deploy_application(config: &DeployConfig) -> Result<()> {
    build_application()
        .context("Application build failed")
        .context("Deployment preparation failed")?;
    
    upload_artifacts(&config.target)
        .context("Artifact upload failed")
        .context("Deployment failed")?;
    
    restart_services(&config.services)
        .context("Service restart failed")
        .context("Deployment failed")?;
    
    run_health_checks()
        .context("Health check failed")
        .context("Deployment verification failed")?;
    
    Ok(())
}
```

## Conditional Propagation

### Error Recovery Patterns

```rust
use anyhow::{Context, Result};

fn load_config_with_fallback() -> Result<Config> {
    // Try to load from primary location
    match load_config_from_file("config.toml") {
        Ok(config) => return Ok(config),
        Err(e) => {
            log::warn!("Failed to load config from file: {}", e);
        }
    }
    
    // Try to load from environment
    match load_config_from_env() {
        Ok(config) => return Ok(config),
        Err(e) => {
            log::warn!("Failed to load config from environment: {}", e);
        }
    }
    
    // Fall back to defaults
    load_default_config()
        .context("Failed to load default configuration")
}

fn try_multiple_endpoints(endpoints: &[String]) -> Result<Response> {
    let mut last_error = None;
    
    for endpoint in endpoints {
        match make_request(endpoint) {
            Ok(response) => return Ok(response),
            Err(error) => {
                log::warn!("Request to {} failed: {}", endpoint, error);
                last_error = Some(error);
            }
        }
    }
    
    Err(last_error.unwrap())
        .context("All endpoints failed")
}
```

### Partial Success Handling

```rust
use anyhow::{Context, Result};

#[derive(Debug)]
struct BatchResult<T> {
    successes: Vec<T>,
    failures: Vec<(usize, anyhow::Error)>,
}

fn process_batch<T, F>(items: Vec<T>, processor: F) -> Result<BatchResult<ProcessedItem>>
where
    F: Fn(T) -> Result<ProcessedItem>,
{
    let mut successes = Vec::new();
    let mut failures = Vec::new();
    
    for (index, item) in items.into_iter().enumerate() {
        match processor(item) {
            Ok(processed) => successes.push(processed),
            Err(error) => failures.push((index, error)),
        }
    }
    
    if failures.len() > successes.len() {
        anyhow::bail!("Batch processing failed: {} failures, {} successes", 
                     failures.len(), successes.len());
    }
    
    if !failures.is_empty() {
        log::warn!("Partial batch failure: {} items failed", failures.len());
    }
    
    Ok(BatchResult { successes, failures })
}
```

### Conditional Error Transformation

```rust
use anyhow::{Context, Result};

fn handle_database_error(error: DatabaseError) -> Result<()> {
    match error {
        DatabaseError::ConnectionLost => {
            log::info!("Database connection lost, attempting reconnect...");
            reconnect_database()
                .context("Failed to reconnect to database")?;
            Ok(())
        }
        DatabaseError::Timeout { query, duration } => {
            if duration > Duration::from_secs(30) {
                log::error!("Query timeout exceeded reasonable limit: {:?}", duration);
                Err(error).context("Database query timeout")
            } else {
                log::warn!("Query timeout, retrying: {}", query);
                retry_query(&query)
                    .context("Query retry failed")
            }
        }
        DatabaseError::ConstraintViolation { .. } => {
            // User error, don't retry
            Err(error).context("Data constraint violation")
        }
        _ => Err(error).context("Database operation failed"),
    }
}
```

## Async Error Propagation

### Basic Async Propagation

```rust
use anyhow::{Context, Result};

async fn fetch_and_process_data(url: &str) -> Result<ProcessedData> {
    let response = reqwest::get(url)
        .await
        .context("Failed to fetch data")?;
    
    let raw_data = response.bytes()
        .await
        .context("Failed to read response body")?;
    
    let parsed_data = parse_data(&raw_data)
        .context("Failed to parse response data")?;
    
    let processed_data = process_data(parsed_data)
        .await
        .context("Failed to process data")?;
    
    Ok(processed_data)
}
```

### Concurrent Error Handling

```rust
use anyhow::{Context, Result};
use tokio::try_join;

async fn fetch_multiple_sources() -> Result<CombinedData> {
    let (user_data, config_data, metrics_data) = try_join!(
        fetch_user_data().context("Failed to fetch user data"),
        fetch_config_data().context("Failed to fetch config data"),
        fetch_metrics_data().context("Failed to fetch metrics data"),
    )?;
    
    Ok(CombinedData {
        user_data,
        config_data,
        metrics_data,
    })
}

// Alternative with manual error handling
async fn fetch_multiple_sources_manual() -> Result<CombinedData> {
    let user_future = fetch_user_data();
    let config_future = fetch_config_data();
    let metrics_future = fetch_metrics_data();
    
    let (user_result, config_result, metrics_result) = 
        tokio::join!(user_future, config_future, metrics_future);
    
    let user_data = user_result
        .context("Failed to fetch user data")?;
    let config_data = config_result
        .context("Failed to fetch config data")?;
    let metrics_data = metrics_result
        .context("Failed to fetch metrics data")?;
    
    Ok(CombinedData {
        user_data,
        config_data,
        metrics_data,
    })
}
```

### Stream Error Propagation

```rust
use anyhow::{Context, Result};
use futures::stream::{Stream, StreamExt, TryStreamExt};

async fn process_data_stream<S>(stream: S) -> Result<Vec<ProcessedItem>>
where
    S: Stream<Item = Result<RawItem>>,
{
    stream
        .map_err(|e| anyhow::anyhow!("Stream error: {}", e))
        .and_then(|item| async move {
            process_item(item)
                .await
                .context("Failed to process stream item")
        })
        .try_collect()
        .await
        .context("Failed to process data stream")
}

async fn parallel_stream_processing<S>(stream: S, concurrency: usize) -> Result<Vec<ProcessedItem>>
where
    S: Stream<Item = Result<RawItem>>,
{
    stream
        .map(|item_result| async move {
            let item = item_result?;
            process_item(item)
                .await
                .context("Failed to process item")
        })
        .buffer_unordered(concurrency)
        .try_collect()
        .await
        .context("Parallel stream processing failed")
}
```

## Error Aggregation

### Collecting Multiple Errors

```rust
use anyhow::{Context, Result};

fn validate_all_fields(data: &InputData) -> Result<()> {
    let mut errors = Vec::new();
    
    if let Err(e) = validate_name(&data.name) {
        errors.push(format!("Name validation failed: {}", e));
    }
    
    if let Err(e) = validate_email(&data.email) {
        errors.push(format!("Email validation failed: {}", e));
    }
    
    if let Err(e) = validate_age(data.age) {
        errors.push(format!("Age validation failed: {}", e));
    }
    
    if !errors.is_empty() {
        anyhow::bail!("Validation failed:\n{}", errors.join("\n"));
    }
    
    Ok(())
}

// Using a custom error collector
struct ErrorCollector {
    errors: Vec<anyhow::Error>,
    context: String,
}

impl ErrorCollector {
    fn new(context: impl Into<String>) -> Self {
        Self {
            errors: Vec::new(),
            context: context.into(),
        }
    }
    
    fn try_operation<F, T>(&mut self, operation: F) -> Option<T>
    where
        F: FnOnce() -> Result<T>,
    {
        match operation() {
            Ok(value) => Some(value),
            Err(error) => {
                self.errors.push(error);
                None
            }
        }
    }
    
    fn finish(self) -> Result<()> {
        if self.errors.is_empty() {
            Ok(())
        } else {
            let error_messages: Vec<String> = self.errors
                .iter()
                .map(|e| e.to_string())
                .collect();
            
            anyhow::bail!("{} failed with {} errors:\n{}", 
                         self.context,
                         self.errors.len(),
                         error_messages.join("\n"));
        }
    }
}

fn process_with_error_collection() -> Result<ProcessedData> {
    let mut collector = ErrorCollector::new("Data processing");
    
    let validated_input = collector.try_operation(|| validate_input());
    let loaded_config = collector.try_operation(|| load_config());
    let initialized_db = collector.try_operation(|| initialize_database());
    
    collector.finish()?;
    
    // All operations succeeded, safe to unwrap
    let input = validated_input.unwrap();
    let config = loaded_config.unwrap();
    let db = initialized_db.unwrap();
    
    process_data(input, config, db)
}
```

### Parallel Error Aggregation

```rust
use anyhow::{Context, Result};
use std::sync::{Arc, Mutex};

async fn process_items_parallel(items: Vec<Item>) -> Result<Vec<ProcessedItem>> {
    let errors = Arc::new(Mutex::new(Vec::new()));
    let results = Arc::new(Mutex::new(Vec::new()));
    
    let tasks: Vec<_> = items.into_iter().enumerate().map(|(index, item)| {
        let errors = Arc::clone(&errors);
        let results = Arc::clone(&results);
        
        tokio::spawn(async move {
            match process_item(item).await {
                Ok(processed) => {
                    results.lock().unwrap().push((index, processed));
                }
                Err(error) => {
                    errors.lock().unwrap().push((index, error));
                }
            }
        })
    }).collect();
    
    // Wait for all tasks
    for task in tasks {
        task.await.context("Task panicked")?;
    }
    
    let errors = errors.lock().unwrap();
    if !errors.is_empty() {
        let error_messages: Vec<String> = errors
            .iter()
            .map(|(index, error)| format!("Item {}: {}", index, error))
            .collect();
        
        anyhow::bail!("Failed to process {} items:\n{}", 
                     errors.len(),
                     error_messages.join("\n"));
    }
    
    let mut results = results.lock().unwrap();
    results.sort_by_key(|(index, _)| *index);
    Ok(results.into_iter().map(|(_, item)| item).collect())
}
```

## Performance Considerations

### Avoiding Unnecessary Allocations

```rust
// Good: Avoid string allocation in hot paths
fn parse_numbers(input: &str) -> Result<Vec<i32>, ParseError> {
    input
        .lines()
        .enumerate()
        .map(|(line_num, line)| {
            line.trim().parse::<i32>()
                .map_err(|_| ParseError::InvalidNumber { line: line_num + 1 })
        })
        .collect()
}

// Avoid: Creating strings for every error
fn parse_numbers_slow(input: &str) -> Result<Vec<i32>, String> {
    input
        .lines()
        .enumerate()
        .map(|(line_num, line)| {
            line.trim().parse::<i32>()
                .map_err(|e| format!("Parse error on line {}: {}", line_num + 1, e))
        })
        .collect()
}
```

### Lazy Error Context

```rust
use anyhow::{Context, Result};

// Good: Lazy context evaluation
fn process_file(path: &Path) -> Result<ProcessedData> {
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Failed to read file: {}", path.display()))?;
    
    parse_content(&content)
        .with_context(|| format!("Failed to parse file: {}", path.display()))
}

// Avoid: Eager context evaluation
fn process_file_eager(path: &Path) -> Result<ProcessedData> {
    let context = format!("Failed to read file: {}", path.display());
    let content = std::fs::read_to_string(path)
        .context(context)?; // String is created even on success
    
    parse_content(&content)
}
```

### Error Type Size Optimization

```rust
// Good: Keep error types small
#[derive(thiserror::Error, Debug)]
pub enum OptimizedError {
    #[error("IO error")]
    Io(#[from] std::io::Error),
    
    #[error("Parse error at position {pos}")]
    Parse { pos: u32 }, // u32 instead of usize
    
    #[error("Network error: {code}")]
    Network { code: u16 }, // u16 instead of String
}

// Avoid: Large error types
#[derive(thiserror::Error, Debug)]
pub enum LargeError {
    #[error("Parse error")]
    Parse {
        input: String,        // Large field
        context: Vec<String>, // Even larger
        metadata: HashMap<String, String>, // Very large
    },
}
```

## Best Practices

### 1. Use `?` for Simple Propagation

```rust
// Good: Simple and clear
fn load_and_parse_config() -> Result<Config, ConfigError> {
    let content = std::fs::read_to_string("config.toml")?;
    let config = toml::from_str(&content)?;
    Ok(config)
}

// Avoid: Verbose match statements for simple cases
fn load_and_parse_config_verbose() -> Result<Config, ConfigError> {
    let content = match std::fs::read_to_string("config.toml") {
        Ok(content) => content,
        Err(error) => return Err(error.into()),
    };
    
    let config = match toml::from_str(&content) {
        Ok(config) => config,
        Err(error) => return Err(error.into()),
    };
    
    Ok(config)
}
```

### 2. Add Context at the Right Level

```rust
// Good: Context at the operation level
fn backup_user_data(user_id: u64) -> Result<()> {
    let user_data = load_user_data(user_id)
        .with_context(|| format!("Failed to backup data for user {}", user_id))?;
    
    save_backup(&user_data)
        .with_context(|| format!("Failed to save backup for user {}", user_id))?;
    
    Ok(())
}

// Good: Low-level functions without context
fn load_user_data(user_id: u64) -> Result<UserData> {
    let path = format!("users/{}.json", user_id);
    let content = std::fs::read_to_string(&path)?;
    let data = serde_json::from_str(&content)?;
    Ok(data)
}
```

### 3. Handle Errors at Appropriate Boundaries

```rust
// Good: Handle errors where you have enough context
fn main() -> Result<()> {
    match run_application() {
        Ok(()) => {
            println!("Application completed successfully");
            Ok(())
        }
        Err(error) => {
            eprintln!("Application failed: {:#}", error);
            std::process::exit(1);
        }
    }
}

fn run_application() -> Result<()> {
    let config = load_config()
        .context("Failed to load configuration")?;
    
    let app = Application::new(config)
        .context("Failed to initialize application")?;
    
    app.run()
        .context("Application execution failed")?;
    
    Ok(())
}
```

### 4. Use Appropriate Error Types

```rust
// Good: Use anyhow for applications
fn main() -> anyhow::Result<()> {
    let config = load_config()
        .context("Failed to load config")?;
    
    run_with_config(config)
        .context("Application failed")?;
    
    Ok(())
}

// Good: Use custom errors for libraries
fn parse_data(input: &str) -> Result<ParsedData, ParseError> {
    // Library code with specific error types
    Ok(ParsedData::new())
}
```

### 5. Avoid Error Swallowing

```rust
// Good: Propagate or handle errors appropriately
fn process_optional_file(path: Option<&str>) -> Result<Option<Data>> {
    match path {
        Some(path) => {
            let data = load_file(path)
                .with_context(|| format!("Failed to load optional file: {}", path))?;
            Ok(Some(data))
        }
        None => Ok(None),
    }
}

// Avoid: Silently ignoring errors
fn process_optional_file_bad(path: Option<&str>) -> Option<Data> {
    path.and_then(|p| load_file(p).ok()) // Loses error information
}

// Better: Log errors even if continuing
fn process_optional_file_better(path: Option<&str>) -> Option<Data> {
    match path {
        Some(path) => match load_file(path) {
            Ok(data) => Some(data),
            Err(error) => {
                log::warn!("Failed to load optional file {}: {}", path, error);
                None
            }
        },
        None => None,
    }
}
```

---

*Error propagation is a fundamental aspect of robust Rust programming. Master these patterns to write reliable, maintainable code that handles failures gracefully.*