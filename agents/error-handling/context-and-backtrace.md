# Error Context and Backtrace in Rust

Error context and backtraces are essential for debugging and understanding the flow of errors through your application. This guide covers how to effectively use context with `anyhow`, backtraces with both `thiserror` and `anyhow`, and best practices for error debugging.

## Table of Contents

1. [Error Context with `anyhow`](#error-context-with-anyhow)
2. [Backtrace Support](#backtrace-support)
3. [Context Patterns](#context-patterns)
4. [Backtrace Configuration](#backtrace-configuration)
5. [Debugging Techniques](#debugging-techniques)
6. [Performance Considerations](#performance-considerations)
7. [Best Practices](#best-practices)
8. [Real-World Examples](#real-world-examples)

## Error Context with `anyhow`

### Basic Context Addition

The `Context` trait provides methods to add contextual information to errors:

```rust
use anyhow::{Context, Result};

fn read_config() -> Result<Config> {
    let content = std::fs::read_to_string("config.toml")
        .context("Failed to read configuration file")?;
    
    let config: Config = toml::from_str(&content)
        .context("Failed to parse configuration file")?;
    
    Ok(config)
}

// Error output:
// Failed to parse configuration file
// 
// Caused by:
//     missing field `database_url` at line 1 column 1
```

### Dynamic Context with `with_context`

Use `with_context` for dynamic context that's computed only when an error occurs:

```rust
use anyhow::{Context, Result};

fn process_files(files: &[String]) -> Result<()> {
    for (index, file) in files.iter().enumerate() {
        process_single_file(file)
            .with_context(|| format!("Failed to process file {} (index {})", file, index))?;
    }
    Ok(())
}

fn download_and_save(url: &str, path: &str) -> Result<()> {
    let response = reqwest::blocking::get(url)
        .with_context(|| format!("Failed to download from {}", url))?;
    
    let content = response.bytes()
        .with_context(|| format!("Failed to read response body from {}", url))?;
    
    std::fs::write(path, content)
        .with_context(|| format!("Failed to write downloaded content to {}", path))?;
    
    Ok(())
}
```

### Context Chaining

Context can be chained to provide multiple levels of information:

```rust
use anyhow::{Context, Result};

fn backup_user_data(user_id: u64) -> Result<()> {
    load_user_data(user_id)
        .context("Failed to load user data")
        .with_context(|| format!("Backup failed for user {}", user_id))?;
    
    create_backup_archive()
        .context("Failed to create backup archive")
        .context("Backup process failed")?;
    
    Ok(())
}

// Error output:
// Backup process failed
//
// Caused by:
//     Failed to create backup archive
//
// Caused by:
//     Permission denied (os error 13)
```

### Conditional Context

Add context based on conditions or error types:

```rust
use anyhow::{Context, Result};

fn smart_file_operation(path: &str) -> Result<String> {
    match std::fs::read_to_string(path) {
        Ok(content) => Ok(content),
        Err(error) => {
            let context = match error.kind() {
                std::io::ErrorKind::NotFound => {
                    format!("File not found: {}. Check if the path is correct.", path)
                },
                std::io::ErrorKind::PermissionDenied => {
                    format!("Permission denied: {}. Check file permissions.", path)
                },
                _ => {
                    format!("Failed to read file: {}", path)
                }
            };
            Err(error).context(context)
        }
    }
}
```

## Backtrace Support

### Automatic Backtrace Capture

Rust 1.65+ automatically captures backtraces for `anyhow::Error`:

```rust
use anyhow::{bail, Result};

fn main() -> Result<()> {
    // Set environment variable to see backtraces
    std::env::set_var("RUST_LIB_BACKTRACE", "1");
    
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
    bail!("Something went wrong at level 3");
}

// Output with RUST_LIB_BACKTRACE=1:
// Error: Something went wrong at level 3
//
// Stack backtrace:
//    0: anyhow::error::<impl anyhow::Error>::msg
//    1: example::level_3
//    2: example::level_2
//    3: example::level_1
//    4: example::deep_function_call
//    5: example::main
```

### Backtrace with `thiserror`

`thiserror` supports backtraces through the `Backtrace` type:

```rust
use std::backtrace::Backtrace;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum MyError {
    #[error("Operation failed")]
    OperationFailed {
        message: String,
        backtrace: Backtrace,  // Automatically captured
    },
    
    #[error("IO error")]
    Io {
        #[from]
        source: std::io::Error,
        backtrace: Backtrace,  // Captured in From impl
    },
}

fn failing_operation() -> Result<(), MyError> {
    Err(MyError::OperationFailed {
        message: "Something went wrong".to_string(),
        backtrace: Backtrace::capture(),
    })
}
```

### Forwarding Backtraces from Sources

Use `#[backtrace]` to forward backtraces from source errors:

```rust
use std::backtrace::Backtrace;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum NetworkError {
    #[error("Connection failed")]
    Connection {
        #[backtrace]  // Forwards backtrace from source
        source: std::io::Error,
        host: String,
    },
    
    #[error("Timeout")]
    Timeout {
        #[source]
        #[backtrace]  // Can combine #[source] and #[backtrace]
        cause: TimeoutError,
        duration: std::time::Duration,
    },
}

#[derive(Error, Debug)]
#[error("Operation timed out")]
pub struct TimeoutError {
    backtrace: Backtrace,
}
```

### Manual Backtrace Handling

```rust
use anyhow::Error;
use std::backtrace::Backtrace;

fn analyze_error_with_backtrace(error: &Error) {
    println!("Error: {}", error);
    
    // Print error chain
    for (i, cause) in error.chain().enumerate() {
        if i == 0 {
            println!("  Error: {}", cause);
        } else {
            println!("  Caused by: {}", cause);
        }
    }
    
    // Print backtrace if available
    let backtrace = error.backtrace();
    match backtrace.status() {
        std::backtrace::BacktraceStatus::Captured => {
            println!("\nBacktrace:\n{}", backtrace);
        },
        std::backtrace::BacktraceStatus::Disabled => {
            println!("\nBacktrace disabled. Set RUST_LIB_BACKTRACE=1 to enable.");
        },
        std::backtrace::BacktraceStatus::Unsupported => {
            println!("\nBacktrace not supported on this platform.");
        },
        _ => {},
    }
}
```

## Context Patterns

### Hierarchical Context

Build context hierarchically from low-level to high-level operations:

```rust
use anyhow::{Context, Result};

// High-level operation
fn deploy_application(config: &DeployConfig) -> Result<()> {
    prepare_deployment(config)
        .context("Deployment preparation failed")?;
    
    execute_deployment(config)
        .context("Deployment execution failed")?;
    
    verify_deployment(config)
        .context("Deployment verification failed")?;
    
    Ok(())
}

// Mid-level operations
fn prepare_deployment(config: &DeployConfig) -> Result<()> {
    build_application()
        .context("Application build failed")
        .context("Deployment preparation failed")?;
    
    package_artifacts()
        .context("Artifact packaging failed")
        .context("Deployment preparation failed")?;
    
    Ok(())
}

// Low-level operations (minimal context)
fn build_application() -> Result<()> {
    std::process::Command::new("cargo")
        .args(&["build", "--release"])
        .status()
        .context("Failed to execute cargo build")?;
    
    Ok(())
}
```

### Contextual Error Transformation

Transform errors while preserving context:

```rust
use anyhow::{Context, Result};

fn load_and_validate_config(path: &str) -> Result<Config> {
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Cannot read config file: {}", path))?;
    
    let raw_config: RawConfig = toml::from_str(&content)
        .with_context(|| format!("Invalid TOML syntax in config file: {}", path))?;
    
    validate_config(&raw_config)
        .with_context(|| format!("Configuration validation failed for: {}", path))?;
    
    Ok(Config::from(raw_config))
}

fn validate_config(config: &RawConfig) -> Result<()> {
    if config.database_url.is_empty() {
        anyhow::bail!("database_url cannot be empty");
    }
    
    if config.port == 0 || config.port > 65535 {
        anyhow::bail!("port must be between 1 and 65535, got {}", config.port);
    }
    
    Ok(())
}
```

### Context with Structured Data

Include structured data in context for better debugging:

```rust
use anyhow::{Context, Result};
use serde_json::json;

fn process_user_request(user_id: u64, request: &Request) -> Result<Response> {
    let context = json!({
        "user_id": user_id,
        "request_id": request.id,
        "endpoint": request.endpoint,
        "timestamp": chrono::Utc::now().to_rfc3339()
    });
    
    validate_request(request)
        .with_context(|| format!("Request validation failed: {}", context))?;
    
    let user = load_user(user_id)
        .with_context(|| format!("Failed to load user data: {}", context))?;
    
    authorize_request(&user, request)
        .with_context(|| format!("Authorization failed: {}", context))?;
    
    execute_request(&user, request)
        .with_context(|| format!("Request execution failed: {}", context))
}
```

## Backtrace Configuration

### Environment Variables

Control backtrace behavior with environment variables:

```rust
fn configure_backtraces() {
    // For errors only (recommended for production)
    std::env::set_var("RUST_LIB_BACKTRACE", "1");
    
    // For both panics and errors
    std::env::set_var("RUST_BACKTRACE", "1");
    
    // Full backtraces (more verbose)
    std::env::set_var("RUST_BACKTRACE", "full");
    
    // Disable backtraces
    std::env::set_var("RUST_LIB_BACKTRACE", "0");
}

fn main() -> anyhow::Result<()> {
    // Configure based on build type
    #[cfg(debug_assertions)]
    std::env::set_var("RUST_LIB_BACKTRACE", "1");
    
    #[cfg(not(debug_assertions))]
    if std::env::var("RUST_LIB_BACKTRACE").is_err() {
        std::env::set_var("RUST_LIB_BACKTRACE", "0");
    }
    
    run_application()
}
```

### Conditional Backtrace Capture

```rust
use std::backtrace::Backtrace;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ConditionalError {
    #[error("Critical error (with backtrace)")]
    Critical {
        message: String,
        backtrace: Backtrace,
    },
    
    #[error("Warning (no backtrace): {message}")]
    Warning { message: String },
}

impl ConditionalError {
    pub fn critical(message: impl Into<String>) -> Self {
        Self::Critical {
            message: message.into(),
            backtrace: Backtrace::capture(),
        }
    }
    
    pub fn warning(message: impl Into<String>) -> Self {
        Self::Warning {
            message: message.into(),
        }
    }
}
```

### Custom Backtrace Formatting

```rust
use anyhow::Error;
use std::backtrace::Backtrace;

fn format_error_for_logging(error: &Error) -> String {
    let mut output = String::new();
    
    // Add main error
    output.push_str(&format!("Error: {}\n", error));
    
    // Add error chain
    for (i, cause) in error.chain().skip(1).enumerate() {
        output.push_str(&format!("  Cause {}: {}\n", i + 1, cause));
    }
    
    // Add backtrace if available and in debug mode
    #[cfg(debug_assertions)]
    {
        let backtrace = error.backtrace();
        if backtrace.status() == std::backtrace::BacktraceStatus::Captured {
            output.push_str("\nBacktrace:\n");
            
            // Format backtrace frames
            let backtrace_str = backtrace.to_string();
            for (i, line) in backtrace_str.lines().enumerate() {
                if i < 10 {  // Limit to first 10 frames
                    output.push_str(&format!("  {}\n", line));
                }
            }
            
            if backtrace_str.lines().count() > 10 {
                output.push_str("  ... (truncated)\n");
            }
        }
    }
    
    output
}
```

## Debugging Techniques

### Error Chain Analysis

```rust
use anyhow::Error;

fn debug_error_chain(error: &Error) {
    println!("=== Error Analysis ===");
    
    // Main error
    println!("Main error: {}", error);
    println!("Error type: {}", std::any::type_name_of_val(error));
    
    // Error chain
    println!("\n=== Error Chain ===");
    for (depth, cause) in error.chain().enumerate() {
        println!("  Level {}: {} ({})", 
                depth, 
                cause, 
                std::any::type_name_of_val(cause));
    }
    
    // Root cause
    let root = error.root_cause();
    println!("\n=== Root Cause ===");
    println!("Root cause: {} ({})", 
            root, 
            std::any::type_name_of_val(root));
    
    // Check for specific error types
    println!("\n=== Error Type Analysis ===");
    if error.chain().any(|e| e.is::<std::io::Error>()) {
        println!("Contains IO error");
    }
    if error.chain().any(|e| e.is::<serde_json::Error>()) {
        println!("Contains JSON parsing error");
    }
    
    // Backtrace
    println!("\n=== Backtrace ===");
    let backtrace = error.backtrace();
    match backtrace.status() {
        std::backtrace::BacktraceStatus::Captured => {
            println!("Backtrace available");
        },
        std::backtrace::BacktraceStatus::Disabled => {
            println!("Backtrace disabled (set RUST_LIB_BACKTRACE=1)");
        },
        std::backtrace::BacktraceStatus::Unsupported => {
            println!("Backtrace not supported");
        },
        _ => println!("Backtrace status unknown"),
    }
}
```

### Error Context Extraction

```rust
use anyhow::Error;
use std::collections::HashMap;

fn extract_error_metadata(error: &Error) -> HashMap<String, String> {
    let mut metadata = HashMap::new();
    
    // Extract from error message
    let message = error.to_string();
    metadata.insert("message".to_string(), message.clone());
    
    // Try to extract structured information
    if let Some(captures) = regex::Regex::new(r"user (\d+)")
        .unwrap()
        .captures(&message) 
    {
        metadata.insert("user_id".to_string(), captures[1].to_string());
    }
    
    if let Some(captures) = regex::Regex::new(r"file: ([^\s]+)")
        .unwrap()
        .captures(&message) 
    {
        metadata.insert("file_path".to_string(), captures[1].to_string());
    }
    
    // Extract error types in chain
    let error_types: Vec<String> = error.chain()
        .map(|e| std::any::type_name_of_val(e).to_string())
        .collect();
    metadata.insert("error_types".to_string(), error_types.join(" -> "));
    
    // Check for specific patterns
    if error.chain().any(|e| e.to_string().contains("timeout")) {
        metadata.insert("category".to_string(), "timeout".to_string());
    }
    if error.chain().any(|e| e.to_string().contains("permission")) {
        metadata.insert("category".to_string(), "permission".to_string());
    }
    
    metadata
}
```

### Structured Error Logging

```rust
use anyhow::Error;
use serde_json::json;
use tracing::{error, warn};

fn log_error_structured(error: &Error, context: &str) {
    let error_data = json!({
        "context": context,
        "error": error.to_string(),
        "error_chain": error.chain().map(|e| e.to_string()).collect::<Vec<_>>(),
        "root_cause": error.root_cause().to_string(),
        "backtrace_available": error.backtrace().status() == std::backtrace::BacktraceStatus::Captured,
        "timestamp": chrono::Utc::now().to_rfc3339(),
    });
    
    // Log at appropriate level based on error type
    if is_user_error(error) {
        warn!(error = %error_data, "User error occurred");
    } else if is_external_error(error) {
        warn!(error = %error_data, "External service error");
    } else {
        error!(error = %error_data, "Internal error occurred");
    }
}

fn is_user_error(error: &Error) -> bool {
    error.chain().any(|e| {
        let msg = e.to_string().to_lowercase();
        msg.contains("validation") || 
        msg.contains("invalid") || 
        msg.contains("bad request")
    })
}

fn is_external_error(error: &Error) -> bool {
    error.chain().any(|e| {
        e.is::<reqwest::Error>() || 
        e.to_string().contains("timeout") ||
        e.to_string().contains("connection refused")
    })
}
```

## Performance Considerations

### Lazy Context Evaluation

```rust
use anyhow::{Context, Result};

// Good: Lazy evaluation
fn expensive_context_good(data: &[u8]) -> Result<ProcessedData> {
    process_data(data)
        .with_context(|| format!("Failed to process {} bytes of data", data.len()))?;
    Ok(ProcessedData::new())
}

// Avoid: Eager evaluation
fn expensive_context_bad(data: &[u8]) -> Result<ProcessedData> {
    let context = format!("Failed to process {} bytes of data", data.len());
    process_data(data)
        .context(context)?;  // String created even on success
    Ok(ProcessedData::new())
}
```

### Backtrace Performance Impact

```rust
use std::backtrace::Backtrace;

// Measure backtrace capture overhead
fn benchmark_backtrace_capture() {
    let start = std::time::Instant::now();
    
    for _ in 0..1000 {
        let _backtrace = Backtrace::capture();
    }
    
    let duration = start.elapsed();
    println!("1000 backtrace captures took: {:?}", duration);
    println!("Average per capture: {:?}", duration / 1000);
}

// Conditional backtrace capture for performance
fn conditional_backtrace_capture(severity: ErrorSeverity) -> Backtrace {
    match severity {
        ErrorSeverity::Critical | ErrorSeverity::Error => Backtrace::capture(),
        _ => Backtrace::disabled(),
    }
}

#[derive(Debug)]
enum ErrorSeverity {
    Critical,
    Error,
    Warning,
    Info,
}
```

### Memory-Efficient Context

```rust
use std::sync::Arc;

// For frequently occurring errors, use Arc to share context
#[derive(Clone)]
struct SharedContext {
    operation: Arc<str>,
    user_id: u64,
    request_id: Arc<str>,
}

impl std::fmt::Display for SharedContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "operation: {}, user: {}, request: {}", 
               self.operation, self.user_id, self.request_id)
    }
}

fn create_error_with_shared_context(context: &SharedContext) -> anyhow::Error {
    anyhow::anyhow!("Operation failed: {}", context)
}
```

## Best Practices

### 1. **Use Context Appropriately**

```rust
// Good: Context at operation boundaries
fn backup_database() -> Result<()> {
    create_backup_directory()
        .context("Failed to prepare backup environment")?;
    
    dump_database()
        .context("Failed to dump database contents")?;
    
    compress_backup()
        .context("Failed to compress backup files")?;
    
    Ok(())
}

// Avoid: Too much context
fn over_contextualized() -> Result<()> {
    let file = std::fs::File::open("data.txt")
        .context("Opening file")
        .context("File operation")
        .context("IO operation")
        .context("System call")?;  // Too many layers
    
    Ok(())
}
```

### 2. **Provide Actionable Context**

```rust
// Good: Actionable context
fn load_config(path: &str) -> Result<Config> {
    std::fs::read_to_string(path)
        .with_context(|| format!(
            "Failed to read config file '{}'. Check if the file exists and is readable.", 
            path
        ))?;
    // ...
    Ok(Config::default())
}

// Avoid: Vague context
fn load_config_vague(path: &str) -> Result<Config> {
    std::fs::read_to_string(path)
        .context("Config error")?;  // Not helpful
    // ...
    Ok(Config::default())
}
```

### 3. **Control Backtrace Verbosity**

```rust
fn setup_error_handling() {
    // In development: full backtraces
    #[cfg(debug_assertions)]
    {
        std::env::set_var("RUST_LIB_BACKTRACE", "full");
    }
    
    // In production: limited backtraces
    #[cfg(not(debug_assertions))]
    {
        if std::env::var("RUST_LIB_BACKTRACE").is_err() {
            std::env::set_var("RUST_LIB_BACKTRACE", "1");
        }
    }
}
```

### 4. **Structure Error Information**

```rust
use anyhow::{Context, Result};
use serde_json::json;

fn structured_error_context(user_id: u64, operation: &str) -> Result<()> {
    perform_operation()
        .with_context(|| {
            json!({
                "user_id": user_id,
                "operation": operation,
                "timestamp": chrono::Utc::now().to_rfc3339(),
                "version": env!("CARGO_PKG_VERSION")
            }).to_string()
        })?;
    
    Ok(())
}
```

## Real-World Examples

### Web Application Error Handling

```rust
use anyhow::{Context, Result};
use axum::{extract::Path, http::StatusCode, response::Json};
use serde_json::json;
use tracing::{error, info, warn};

async fn get_user_profile(
    Path(user_id): Path<u64>
) -> Result<Json<serde_json::Value>, AppError> {
    let request_id = uuid::Uuid::new_v4().to_string();
    
    info!(user_id = user_id, request_id = %request_id, "Fetching user profile");
    
    let user = fetch_user(user_id)
        .await
        .with_context(|| format!("Failed to fetch user {} (request: {})", user_id, request_id))?;
    
    let profile = fetch_user_profile(user_id)
        .await
        .with_context(|| format!("Failed to fetch profile for user {} (request: {})", user_id, request_id))?;
    
    let permissions = fetch_user_permissions(user_id)
        .await
        .with_context(|| format!("Failed to fetch permissions for user {} (request: {})", user_id, request_id))?;
    
    info!(user_id = user_id, request_id = %request_id, "Successfully fetched user profile");
    
    Ok(Json(json!({
        "user": user,
        "profile": profile,
        "permissions": permissions,
        "request_id": request_id
    })))
}

struct AppError(anyhow::Error);

impl axum::response::IntoResponse for AppError {
    fn into_response(self) -> axum::response::Response {
        let error_id = uuid::Uuid::new_v4().to_string();
        
        // Log the full error with context
        error!(
            error_id = %error_id,
            error = %self.0,
            backtrace = ?self.0.backtrace(),
            "Request failed"
        );
        
        // Return user-friendly error
        let status = if self.0.to_string().contains("not found") {
            StatusCode::NOT_FOUND
        } else {
            StatusCode::INTERNAL_SERVER_ERROR
        };
        
        let body = json!({
            "error": "Request failed",
            "error_id": error_id,
            "message": "Please contact support if the problem persists"
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

### CLI Application with Rich Error Context

```rust
use anyhow::{Context, Result};
use clap::Parser;

#[derive(Parser)]
struct Args {
    #[arg(short, long)]
    input: String,
    
    #[arg(short, long)]
    output: String,
    
    #[arg(long)]
    verbose: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();
    
    // Configure error handling based on verbosity
    if args.verbose {
        std::env::set_var("RUST_LIB_BACKTRACE", "full");
    }
    
    match run_application(&args) {
        Ok(()) => {
            println!("✅ Processing completed successfully");
            Ok(())
        }
        Err(error) => {
            eprintln!("❌ Application failed");
            
            if args.verbose {
                // Verbose error output with full context
                eprintln!("\n🔍 Detailed error information:");
                eprintln!("{:#}", error);
                
                if error.backtrace().status() == std::backtrace::BacktraceStatus::Captured {
                    eprintln!("\n📍 Backtrace:");
                    eprintln!("{}", error.backtrace());
                }
            } else {
                // User-friendly error output
                eprintln!("Error: {}", error);
                eprintln!("\n💡 Use --verbose for detailed error information");
            }
            
            std::process::exit(1);
        }
    }
}

fn run_application(args: &Args) -> Result<()> {
    let input_data = std::fs::read_to_string(&args.input)
        .with_context(|| format!("Failed to read input file: {}", args.input))?;
    
    let processed_data = process_data(&input_data)
        .context("Data processing failed")?;
    
    std::fs::write(&args.output, processed_data)
        .with_context(|| format!("Failed to write output file: {}", args.output))?;
    
    Ok(())
}

fn process_data(data: &str) -> Result<String> {
    if data.is_empty() {
        anyhow::bail!("Input data is empty");
    }
    
    let lines: Vec<&str> = data.lines().collect();
    
    if lines.len() > 10000 {
        anyhow::bail!("Input too large: {} lines (maximum: 10000)", lines.len());
    }
    
    // Simulate processing with potential failure
    for (i, line) in lines.iter().enumerate() {
        if line.len() > 1000 {
            anyhow::bail!("Line {} too long: {} characters (maximum: 1000)", i + 1, line.len());
        }
    }
    
    Ok(format!("Processed {} lines", lines.len()))
}
```

---

*Effective use of context and backtraces transforms cryptic error messages into actionable debugging information. Use them judiciously to create applications that are both robust and debuggable.*