# The Complete Guide to `anyhow` 1.0

`anyhow` provides a flexible concrete error type built on `std::error::Error`. It's designed for **application developers** who want easy, ergonomic error handling without designing specific error types.

## Table of Contents

1. [Installation and Setup](#installation-and-setup)
2. [Basic Usage](#basic-usage)
3. [The `anyhow!` and `bail!` Macros](#the-anyhow-and-bail-macros)
4. [Adding Context](#adding-context)
5. [Error Propagation](#error-propagation)
6. [Downcasting and Error Inspection](#downcasting-and-error-inspection)
7. [Backtrace Support](#backtrace-support)
8. [Advanced Patterns](#advanced-patterns)
9. [Best Practices](#best-practices)
10. [Real-World Examples](#real-world-examples)

## Installation and Setup

Add `anyhow` to your `Cargo.toml`:

```toml
[dependencies]
anyhow = "1.0"
```

**Minimum Rust Version**: 1.39+

### Optional Features

```toml
[dependencies]
# For older Rust versions (< 1.65) to enable backtraces
anyhow = { version = "1.0", features = ["backtrace"] }

# For no_std environments
anyhow = { version = "1.0", default-features = false }
```

## Basic Usage

### The `anyhow::Result<T>` Type

Use `anyhow::Result<T>` as your default return type for fallible functions:

```rust
use anyhow::Result;

fn main() -> Result<()> {
    let config = load_config()?;
    let database = connect_database(&config)?;
    run_application(config, database)?;
    Ok(())
}

fn load_config() -> Result<Config> {
    let content = std::fs::read_to_string("config.toml")?;
    let config: Config = toml::from_str(&content)?;
    Ok(config)
}

fn connect_database(config: &Config) -> Result<Database> {
    Database::connect(&config.database_url)
        .map_err(|e| anyhow::anyhow!("Failed to connect to database: {}", e))
}
```

### Automatic Error Conversion

`anyhow::Error` can be created from any type that implements `std::error::Error`:

```rust
use anyhow::Result;

fn example() -> Result<String> {
    // All of these work automatically with ?
    let content = std::fs::read_to_string("file.txt")?;     // io::Error
    let data: serde_json::Value = serde_json::from_str(&content)?; // serde_json::Error
    let response = reqwest::get("https://api.example.com").await?; // reqwest::Error
    Ok(response.text().await?)
}
```

## The `anyhow!` and `bail!` Macros

### Creating Ad-Hoc Errors with `anyhow!`

```rust
use anyhow::{anyhow, Result};

fn validate_age(age: i32) -> Result<()> {
    if age < 0 {
        return Err(anyhow!("Age cannot be negative: {}", age));
    }
    if age > 150 {
        return Err(anyhow!("Age seems unrealistic: {}", age));
    }
    Ok(())
}

// With complex formatting
fn validate_email(email: &str) -> Result<()> {
    if !email.contains('@') {
        return Err(anyhow!(
            "Invalid email address '{}': missing @ symbol", 
            email
        ));
    }
    Ok(())
}
```

### Early Return with `bail!`

`bail!` is a shorthand for returning an error immediately:

```rust
use anyhow::{bail, Result};

fn process_data(data: &[u8]) -> Result<ProcessedData> {
    if data.is_empty() {
        bail!("Cannot process empty data");
    }
    
    if data.len() > MAX_SIZE {
        bail!("Data too large: {} bytes (max: {})", data.len(), MAX_SIZE);
    }
    
    // Process data...
    Ok(ProcessedData::new(data))
}

// Equivalent to:
fn process_data_verbose(data: &[u8]) -> Result<ProcessedData> {
    if data.is_empty() {
        return Err(anyhow::anyhow!("Cannot process empty data"));
    }
    // ...
    Ok(ProcessedData::new(data))
}
```

### Dynamic Error Messages

```rust
use anyhow::{bail, Result};

fn validate_config(config: &Config) -> Result<()> {
    for (key, value) in &config.settings {
        if value.is_empty() {
            bail!("Configuration key '{}' cannot be empty", key);
        }
        
        if key.starts_with("secret_") && value.len() < 32 {
            bail!(
                "Secret configuration '{}' too short: {} characters (minimum: 32)",
                key,
                value.len()
            );
        }
    }
    Ok(())
}
```

## Adding Context

### Static Context with `.context()`

```rust
use anyhow::{Context, Result};

fn load_user_data(user_id: u64) -> Result<UserData> {
    let path = format!("users/{}.json", user_id);
    
    let content = std::fs::read_to_string(&path)
        .context("Failed to read user data file")?;
    
    let user_data: UserData = serde_json::from_str(&content)
        .context("Failed to parse user data JSON")?;
    
    Ok(user_data)
}
```

### Dynamic Context with `.with_context()`

```rust
use anyhow::{Context, Result};

fn process_files(files: &[String]) -> Result<()> {
    for (index, file) in files.iter().enumerate() {
        std::fs::read_to_string(file)
            .with_context(|| format!("Failed to read file {} (index {})", file, index))?;
    }
    Ok(())
}

fn download_file(url: &str, path: &str) -> Result<()> {
    let response = reqwest::blocking::get(url)
        .with_context(|| format!("Failed to download from {}", url))?;
    
    let content = response.bytes()
        .with_context(|| format!("Failed to read response body from {}", url))?;
    
    std::fs::write(path, content)
        .with_context(|| format!("Failed to write downloaded content to {}", path))?;
    
    Ok(())
}
```

### Chaining Context

```rust
use anyhow::{Context, Result};

fn backup_database() -> Result<()> {
    create_backup_directory()
        .context("Failed to prepare backup environment")?;
    
    export_database_schema()
        .context("Failed to export database schema")
        .context("Database backup failed")?;
    
    export_database_data()
        .context("Failed to export database data")
        .context("Database backup failed")?;
    
    Ok(())
}
```

## Error Propagation

### Basic Propagation with `?`

```rust
use anyhow::Result;

fn read_and_parse_config() -> Result<Config> {
    let content = std::fs::read_to_string("config.toml")?;
    let config = toml::from_str(&content)?;
    Ok(config)
}

fn setup_application() -> Result<Application> {
    let config = read_and_parse_config()?;
    let database = Database::connect(&config.db_url)?;
    let cache = Cache::new(&config.cache_settings)?;
    
    Ok(Application::new(config, database, cache))
}
```

### Propagation with Context

```rust
use anyhow::{Context, Result};

fn initialize_system() -> Result<System> {
    let config = read_and_parse_config()
        .context("Failed to load system configuration")?;
    
    let logger = setup_logging(&config.log_level)
        .context("Failed to initialize logging system")?;
    
    let metrics = setup_metrics(&config.metrics)
        .context("Failed to initialize metrics collection")?;
    
    Ok(System::new(config, logger, metrics))
}
```

### Conditional Error Handling

```rust
use anyhow::{Context, Result};

fn try_multiple_sources() -> Result<Data> {
    // Try primary source
    match load_from_primary_source() {
        Ok(data) => return Ok(data),
        Err(e) => {
            log::warn!("Primary source failed: {}", e);
        }
    }
    
    // Try backup source
    match load_from_backup_source() {
        Ok(data) => return Ok(data),
        Err(e) => {
            log::warn!("Backup source failed: {}", e);
        }
    }
    
    // Try cache as last resort
    load_from_cache()
        .context("All data sources failed, including cache")
}
```

## Downcasting and Error Inspection

### Downcasting to Specific Error Types

```rust
use anyhow::{Error, Result};
use std::io;

fn handle_file_operation() -> Result<()> {
    match perform_file_operation() {
        Ok(()) => Ok(()),
        Err(error) => {
            // Check if it's an IO error
            if let Some(io_error) = error.downcast_ref::<io::Error>() {
                match io_error.kind() {
                    io::ErrorKind::NotFound => {
                        println!("File not found, creating default...");
                        create_default_file()?;
                        Ok(())
                    }
                    io::ErrorKind::PermissionDenied => {
                        bail!("Permission denied. Please check file permissions.");
                    }
                    _ => Err(error),
                }
            } else {
                Err(error)
            }
        }
    }
}
```

### Extracting Root Causes

```rust
use anyhow::Error;

fn analyze_error(error: &Error) {
    println!("Error: {}", error);
    
    // Walk the error chain
    let mut source = error.source();
    let mut level = 1;
    
    while let Some(err) = source {
        println!("  Caused by (level {}): {}", level, err);
        source = err.source();
        level += 1;
    }
    
    // Find root cause
    let root_cause = error.root_cause();
    println!("Root cause: {}", root_cause);
    
    // Check for specific error types in the chain
    if error.chain().any(|e| e.is::<io::Error>()) {
        println!("This error chain contains an IO error");
    }
}
```

### Pattern Matching on Error Types

```rust
use anyhow::{Error, Result};

fn handle_database_operation() -> Result<()> {
    match perform_database_operation() {
        Ok(()) => Ok(()),
        Err(error) => {
            // Try to downcast to specific database errors
            if let Some(db_error) = error.downcast_ref::<DatabaseError>() {
                match db_error {
                    DatabaseError::ConnectionLost => {
                        println!("Connection lost, attempting to reconnect...");
                        reconnect_database()?;
                        perform_database_operation() // Retry
                    }
                    DatabaseError::ConstraintViolation { .. } => {
                        println!("Data constraint violation - this is a user error");
                        Err(error)
                    }
                    _ => Err(error),
                }
            } else {
                Err(error)
            }
        }
    }
}
```

## Backtrace Support

### Automatic Backtrace Capture

Backtraces are automatically captured on Rust 1.65+ when the error doesn't already provide one:

```rust
use anyhow::Result;

fn main() -> Result<()> {
    // Set environment variable to see backtraces
    // RUST_BACKTRACE=1 for panics and errors
    // RUST_LIB_BACKTRACE=1 for errors only
    
    deep_function_call()?;
    Ok(())
}

fn deep_function_call() -> Result<()> {
    level_1()?;
    Ok(())
}

fn level_1() -> Result<()> {
    level_2()?;
    Ok(())
}

fn level_2() -> Result<()> {
    level_3()?;
    Ok(())
}

fn level_3() -> Result<()> {
    anyhow::bail!("Something went wrong at level 3");
}
```

### Accessing Backtraces Programmatically

```rust
use anyhow::Error;
use std::backtrace::Backtrace;

fn log_error_with_backtrace(error: &Error) {
    log::error!("Error occurred: {}", error);
    
    // Log the error chain
    for (i, cause) in error.chain().enumerate() {
        if i == 0 {
            log::error!("  Error: {}", cause);
        } else {
            log::error!("  Caused by: {}", cause);
        }
    }
    
    // Log backtrace if available
    let backtrace = error.backtrace();
    if backtrace.status() == std::backtrace::BacktraceStatus::Captured {
        log::debug!("Backtrace:\n{}", backtrace);
    }
}
```

## Advanced Patterns

### Custom Error Types with `anyhow`

You can still use custom error types with `anyhow` for specific cases:

```rust
use anyhow::{Error, Result};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ValidationError {
    #[error("Field '{field}' is required")]
    RequiredField { field: String },
    
    #[error("Field '{field}' has invalid format")]
    InvalidFormat { field: String },
}

fn validate_and_process(data: &InputData) -> Result<ProcessedData> {
    // Use custom error for validation
    validate_input(data)?;
    
    // Use anyhow for other operations
    let processed = process_data(data)
        .context("Failed to process validated data")?;
    
    Ok(processed)
}

fn validate_input(data: &InputData) -> Result<(), ValidationError> {
    if data.name.is_empty() {
        return Err(ValidationError::RequiredField {
            field: "name".to_string(),
        });
    }
    Ok(())
}
```

### Error Aggregation

```rust
use anyhow::{Error, Result};

fn process_multiple_items(items: &[Item]) -> Result<Vec<ProcessedItem>> {
    let mut results = Vec::new();
    let mut errors = Vec::new();
    
    for (index, item) in items.iter().enumerate() {
        match process_item(item) {
            Ok(processed) => results.push(processed),
            Err(error) => {
                errors.push(format!("Item {}: {}", index, error));
            }
        }
    }
    
    if !errors.is_empty() {
        anyhow::bail!("Failed to process {} items:\n{}", 
                     errors.len(), 
                     errors.join("\n"));
    }
    
    Ok(results)
}
```

### Retry Logic with Context

```rust
use anyhow::{Context, Result};
use std::time::Duration;

async fn retry_operation<F, T>(
    operation: F,
    max_attempts: usize,
    delay: Duration,
) -> Result<T>
where
    F: Fn() -> Result<T>,
{
    let mut last_error = None;
    
    for attempt in 1..=max_attempts {
        match operation() {
            Ok(result) => return Ok(result),
            Err(error) => {
                log::warn!("Attempt {} failed: {}", attempt, error);
                last_error = Some(error);
                
                if attempt < max_attempts {
                    tokio::time::sleep(delay).await;
                }
            }
        }
    }
    
    Err(last_error.unwrap())
        .with_context(|| format!("Operation failed after {} attempts", max_attempts))
}
```

### Conditional Error Conversion

```rust
use anyhow::{Context, Result};

fn smart_file_operation(path: &str, create_if_missing: bool) -> Result<String> {
    match std::fs::read_to_string(path) {
        Ok(content) => Ok(content),
        Err(error) => {
            if error.kind() == std::io::ErrorKind::NotFound && create_if_missing {
                log::info!("File not found, creating default: {}", path);
                let default_content = create_default_content();
                std::fs::write(path, &default_content)
                    .with_context(|| format!("Failed to create default file: {}", path))?;
                Ok(default_content)
            } else {
                Err(error)
                    .with_context(|| format!("Failed to read file: {}", path))
            }
        }
    }
}
```

## Best Practices

### 1. Use Descriptive Context Messages

```rust
// Good: Specific, actionable context
std::fs::read_to_string(&config_path)
    .with_context(|| format!("Failed to read configuration file: {}", config_path))?;

database.execute_query(&sql)
    .with_context(|| format!("Failed to execute query: {}", sql))?;

// Avoid: Generic, unhelpful context
std::fs::read_to_string(&config_path)
    .context("File operation failed")?;
```

### 2. Provide Context at the Right Level

```rust
// Good: Context at the operation level
fn backup_user_data(user_id: u64) -> Result<()> {
    let user_data = load_user_data(user_id)
        .with_context(|| format!("Failed to backup data for user {}", user_id))?;
    
    save_backup(&user_data)
        .with_context(|| format!("Failed to save backup for user {}", user_id))?;
    
    Ok(())
}

// Good: Low-level functions can be context-free
fn load_user_data(user_id: u64) -> Result<UserData> {
    let path = format!("users/{}.json", user_id);
    let content = std::fs::read_to_string(&path)?;
    let data = serde_json::from_str(&content)?;
    Ok(data)
}
```

### 3. Use `bail!` for Early Returns

```rust
// Good: Clear early return
fn validate_request(request: &Request) -> Result<()> {
    if request.auth_token.is_empty() {
        bail!("Authentication token is required");
    }
    
    if request.payload.len() > MAX_PAYLOAD_SIZE {
        bail!("Payload too large: {} bytes (max: {})", 
              request.payload.len(), MAX_PAYLOAD_SIZE);
    }
    
    Ok(())
}

// Avoid: Nested error handling
fn validate_request_nested(request: &Request) -> Result<()> {
    if !request.auth_token.is_empty() {
        if request.payload.len() <= MAX_PAYLOAD_SIZE {
            Ok(())
        } else {
            Err(anyhow::anyhow!("Payload too large"))
        }
    } else {
        Err(anyhow::anyhow!("Authentication token required"))
    }
}
```

### 4. Handle Errors at the Right Abstraction Level

```rust
// Good: Handle errors where you have enough context
fn main() -> Result<()> {
    match run_application() {
        Ok(()) => {
            println!("Application completed successfully");
            Ok(())
        }
        Err(error) => {
            eprintln!("Application failed: {}", error);
            
            // Log detailed error information
            log::error!("Application error: {:#}", error);
            
            // Exit with appropriate code
            std::process::exit(1);
        }
    }
}

fn run_application() -> Result<()> {
    let config = load_config()
        .context("Failed to initialize application configuration")?;
    
    let app = Application::new(config)
        .context("Failed to create application instance")?;
    
    app.run()
        .context("Application execution failed")?;
    
    Ok(())
}
```

### 5. Use Downcasting Sparingly

```rust
// Good: Downcast only when you need specific handling
fn handle_network_error(error: anyhow::Error) -> Result<()> {
    if let Some(reqwest_error) = error.downcast_ref::<reqwest::Error>() {
        if reqwest_error.is_timeout() {
            log::warn!("Request timed out, will retry later");
            return schedule_retry();
        }
    }
    
    // For all other errors, just propagate
    Err(error)
}

// Avoid: Excessive downcasting
fn bad_error_handling(error: anyhow::Error) -> Result<()> {
    if let Some(io_error) = error.downcast_ref::<std::io::Error>() {
        // Handle IO error
    } else if let Some(json_error) = error.downcast_ref::<serde_json::Error>() {
        // Handle JSON error
    } else if let Some(http_error) = error.downcast_ref::<reqwest::Error>() {
        // Handle HTTP error
    } else {
        // Handle everything else
    }
    // This is probably too much specific handling
    Ok(())
}
```

## Real-World Examples

### CLI Application

```rust
use anyhow::{Context, Result};
use clap::Parser;

#[derive(Parser)]
struct Args {
    #[arg(short, long)]
    config: Option<String>,
    
    #[arg(short, long)]
    output: String,
    
    input: String,
}

fn main() -> Result<()> {
    let args = Args::parse();
    
    // Load configuration
    let config = if let Some(config_path) = args.config {
        load_config_file(&config_path)
            .with_context(|| format!("Failed to load config from {}", config_path))?
    } else {
        load_default_config()
            .context("Failed to load default configuration")?
    };
    
    // Process input
    let input_data = std::fs::read_to_string(&args.input)
        .with_context(|| format!("Failed to read input file: {}", args.input))?;
    
    let processed_data = process_data(&input_data, &config)
        .context("Failed to process input data")?;
    
    // Write output
    std::fs::write(&args.output, processed_data)
        .with_context(|| format!("Failed to write output to: {}", args.output))?;
    
    println!("Successfully processed {} -> {}", args.input, args.output);
    Ok(())
}

fn process_data(data: &str, config: &Config) -> Result<String> {
    if data.is_empty() {
        anyhow::bail!("Input data is empty");
    }
    
    let parsed = parse_input_format(data)
        .context("Failed to parse input data")?;
    
    let transformed = apply_transformations(parsed, &config.transformations)
        .context("Failed to apply data transformations")?;
    
    let output = format_output(transformed, &config.output_format)
        .context("Failed to format output data")?;
    
    Ok(output)
}
```

### Web Service

```rust
use anyhow::{Context, Result};
use axum::{extract::Path, http::StatusCode, response::Json, routing::get, Router};
use serde_json::json;

async fn get_user(Path(user_id): Path<u64>) -> Result<Json<serde_json::Value>, AppError> {
    let user = fetch_user_from_database(user_id)
        .await
        .with_context(|| format!("Failed to fetch user {}", user_id))?;
    
    let profile = fetch_user_profile(user_id)
        .await
        .context("Failed to fetch user profile")?;
    
    let response = json!({
        "user": user,
        "profile": profile,
    });
    
    Ok(Json(response))
}

// Custom error type for web responses
struct AppError(anyhow::Error);

impl axum::response::IntoResponse for AppError {
    fn into_response(self) -> axum::response::Response {
        let status = if self.0.downcast_ref::<UserNotFoundError>().is_some() {
            StatusCode::NOT_FOUND
        } else if self.0.downcast_ref::<ValidationError>().is_some() {
            StatusCode::BAD_REQUEST
        } else {
            StatusCode::INTERNAL_SERVER_ERROR
        };
        
        let body = json!({
            "error": self.0.to_string(),
        });
        
        (status, Json(body)).into_response()
    }
}

impl<E> From<E> for AppError
where
    E: Into<anyhow::Error>,
{
    fn from(err: E) -> Self {
        Self(err.into())
    }
}
```

### Async Data Pipeline

```rust
use anyhow::{Context, Result};
use tokio::sync::mpsc;

async fn run_data_pipeline(input_path: &str, output_path: &str) -> Result<()> {
    let (tx, mut rx) = mpsc::channel(100);
    
    // Spawn reader task
    let reader_handle = {
        let tx = tx.clone();
        let input_path = input_path.to_string();
        tokio::spawn(async move {
            read_input_data(&input_path, tx)
                .await
                .context("Data reader task failed")
        })
    };
    
    // Spawn processor tasks
    let mut processor_handles = Vec::new();
    for i in 0..4 {
        let mut rx = rx.clone();
        let handle = tokio::spawn(async move {
            process_data_stream(i, &mut rx)
                .await
                .with_context(|| format!("Processor {} failed", i))
        });
        processor_handles.push(handle);
    }
    
    // Spawn writer task
    let writer_handle = {
        let output_path = output_path.to_string();
        tokio::spawn(async move {
            write_processed_data(&output_path, &mut rx)
                .await
                .context("Data writer task failed")
        })
    };
    
    // Wait for all tasks and collect any errors
    let reader_result = reader_handle.await
        .context("Reader task panicked")?;
    
    let mut processor_errors = Vec::new();
    for (i, handle) in processor_handles.into_iter().enumerate() {
        if let Err(error) = handle.await.context("Processor task panicked")? {
            processor_errors.push(format!("Processor {}: {}", i, error));
        }
    }
    
    let writer_result = writer_handle.await
        .context("Writer task panicked")?;
    
    // Report any errors
    reader_result?;
    writer_result?;
    
    if !processor_errors.is_empty() {
        anyhow::bail!("Pipeline failed with processor errors:\n{}", 
                     processor_errors.join("\n"));
    }
    
    Ok(())
}
```

---

*This guide covers `anyhow` 1.0.98. For the latest features and updates, see the [official documentation](https://docs.rs/anyhow).*