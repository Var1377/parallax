# Complete API Reference

This comprehensive reference covers all the essential APIs and patterns for error handling in Rust using `thiserror` 2.0 and `anyhow` 1.0.

## Table of Contents

1. [`thiserror` API Reference](#thiserror-api-reference)
2. [`anyhow` API Reference](#anyhow-api-reference)
3. [Standard Library Error Traits](#standard-library-error-traits)
4. [Common Patterns Quick Reference](#common-patterns-quick-reference)
5. [Cargo.toml Configuration](#cargotoml-configuration)
6. [Environment Variables](#environment-variables)

## `thiserror` API Reference

### Derive Macro

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum MyError {
    // Basic error variants
    #[error("simple error message")]
    Simple,
    
    #[error("error with data: {value}")]
    WithData { value: String },
    
    #[error("error with positional data: {0}")]
    WithPositional(i32),
}
```

### Attributes

#### `#[error("...")]`

Defines the display message for the error:

```rust
#[derive(Error, Debug)]
pub enum FormatError {
    // Field interpolation
    #[error("Invalid value: {value}")]
    InvalidValue { value: String },
    
    // Positional interpolation
    #[error("Parse error: {0}")]
    ParseError(String),
    
    // Debug formatting
    #[error("Data error: {data:?}")]
    DataError { data: Vec<u8> },
    
    // Complex formatting with expressions
    #[error("Range error: {value} not in {min}..{max}")]
    RangeError { value: i32, min: i32, max: i32 },
    
    // Expressions with field references
    #[error("Invalid length: {len} (expected {}.{})", .range.start, .range.end)]
    InvalidLength { len: usize, range: std::ops::Range<usize> },
}
```

#### `#[from]`

Generates `From` implementations for automatic error conversion:

```rust
#[derive(Error, Debug)]
pub enum MyError {
    #[error("IO error")]
    Io(#[from] std::io::Error),
    
    #[error("JSON error")]
    Json(#[from] serde_json::Error),
    
    // With named fields
    #[error("Network error")]
    Network {
        #[from]
        source: reqwest::Error,
        context: String,
    },
}

// Generated implementations:
impl From<std::io::Error> for MyError {
    fn from(error: std::io::Error) -> Self {
        MyError::Io(error)
    }
}

impl From<serde_json::Error> for MyError {
    fn from(error: serde_json::Error) -> Self {
        MyError::Json(error)
    }
}
```

#### `#[source]`

Marks a field as the error source (for `Error::source()`):

```rust
#[derive(Error, Debug)]
pub enum MyError {
    #[error("High-level error")]
    HighLevel {
        #[source]  // Optional if field is named 'source'
        source: LowerError,
        context: String,
    },
    
    #[error("Another error")]
    Another {
        cause: SomeError,  // Not automatically a source
        #[source]
        real_source: AnotherError,  // This is the source
    },
}
```

#### `#[backtrace]`

Forwards backtraces from source errors (requires nightly Rust):

```rust
use std::backtrace::Backtrace;

#[derive(Error, Debug)]
pub enum MyError {
    #[error("Error with backtrace")]
    WithBacktrace {
        #[backtrace]
        source: std::io::Error,
        context: String,
    },
    
    #[error("Error with own backtrace")]
    OwnBacktrace {
        message: String,
        backtrace: Backtrace,  // Automatically detected
    },
}
```

#### `#[error(transparent)]`

Forwards `Display` and `source()` to the wrapped error:

```rust
#[derive(Error, Debug)]
pub enum ApiError {
    #[error("Validation failed: {message}")]
    Validation { message: String },
    
    // Transparent forwarding
    #[error(transparent)]
    Network(#[from] reqwest::Error),
    
    #[error(transparent)]
    Unexpected(#[from] anyhow::Error),
}
```

### Error Types

#### Enum Errors

```rust
#[derive(Error, Debug)]
pub enum MyError {
    #[error("Variant A: {msg}")]
    VariantA { msg: String },
    
    #[error("Variant B")]
    VariantB(#[from] std::io::Error),
    
    #[error("Variant C: {0}")]
    VariantC(String),
}
```

#### Struct Errors

```rust
#[derive(Error, Debug)]
#[error("Struct error: {message} at {location}")]
pub struct MyStructError {
    pub message: String,
    pub location: String,
    #[source]
    pub cause: Option<Box<dyn std::error::Error + Send + Sync>>,
}
```

#### Unit Struct Errors

```rust
#[derive(Error, Debug)]
#[error("Something went wrong")]
pub struct SimpleError;
```

#### Tuple Struct Errors

```rust
#[derive(Error, Debug)]
#[error("Tuple error: {0}")]
pub struct TupleError(pub String);
```

### Generic Error Types

```rust
#[derive(Error, Debug)]
pub enum GenericError<T>
where
    T: std::error::Error + Send + Sync + 'static,
{
    #[error("Specific error: {message}")]
    Specific { message: String },
    
    #[error("Generic error")]
    Generic(#[from] T),
}

// Usage
type DatabaseError = GenericError<sqlx::Error>;
type NetworkError = GenericError<reqwest::Error>;
```

## `anyhow` API Reference

### Core Types

#### `anyhow::Error`

The main error type:

```rust
use anyhow::Error;

// Create from any std::error::Error
let error: Error = std::io::Error::new(std::io::ErrorKind::NotFound, "file not found").into();

// Create from string
let error: Error = anyhow::anyhow!("Something went wrong");

// Methods
let message: String = error.to_string();
let source: Option<&(dyn std::error::Error + 'static)> = error.source();
let root_cause: &(dyn std::error::Error + 'static) = error.root_cause();
let chain: impl Iterator<Item = &(dyn std::error::Error + 'static)> = error.chain();
let backtrace: &std::backtrace::Backtrace = error.backtrace();
```

#### `anyhow::Result<T>`

Type alias for `Result<T, anyhow::Error>`:

```rust
use anyhow::Result;

fn my_function() -> Result<String> {
    Ok("success".to_string())
}

// Equivalent to:
fn my_function_explicit() -> Result<String, anyhow::Error> {
    Ok("success".to_string())
}
```

### Macros

#### `anyhow!`

Creates an `anyhow::Error` with formatted message:

```rust
use anyhow::anyhow;

// Simple message
let error = anyhow!("Operation failed");

// Formatted message
let error = anyhow!("Failed to process {} items", count);

// With complex formatting
let error = anyhow!(
    "Invalid configuration: expected {expected}, found {found}",
    expected = "string",
    found = "number"
);
```

#### `bail!`

Early return with an error:

```rust
use anyhow::{bail, Result};

fn validate_input(value: i32) -> Result<()> {
    if value < 0 {
        bail!("Value cannot be negative: {}", value);
    }
    
    if value > 100 {
        bail!("Value too large: {} (max: 100)", value);
    }
    
    Ok(())
}

// Equivalent to:
fn validate_input_explicit(value: i32) -> Result<()> {
    if value < 0 {
        return Err(anyhow::anyhow!("Value cannot be negative: {}", value));
    }
    
    Ok(())
}
```

#### `ensure!`

Conditional error with assertion-like syntax:

```rust
use anyhow::{ensure, Result};

fn process_data(data: &[u8]) -> Result<()> {
    ensure!(!data.is_empty(), "Data cannot be empty");
    ensure!(data.len() <= 1024, "Data too large: {} bytes", data.len());
    
    Ok(())
}

// Equivalent to:
fn process_data_explicit(data: &[u8]) -> Result<()> {
    if data.is_empty() {
        anyhow::bail!("Data cannot be empty");
    }
    if data.len() > 1024 {
        anyhow::bail!("Data too large: {} bytes", data.len());
    }
    
    Ok(())
}
```

### Context Trait

#### `.context()`

Adds static context to an error:

```rust
use anyhow::{Context, Result};

fn load_config() -> Result<Config> {
    let content = std::fs::read_to_string("config.toml")
        .context("Failed to read configuration file")?;
    
    let config: Config = toml::from_str(&content)
        .context("Failed to parse configuration file")?;
    
    Ok(config)
}
```

#### `.with_context()`

Adds dynamic context to an error:

```rust
use anyhow::{Context, Result};

fn process_files(files: &[String]) -> Result<()> {
    for (i, file) in files.iter().enumerate() {
        process_file(file)
            .with_context(|| format!("Failed to process file {} (index {})", file, i))?;
    }
    Ok(())
}
```

### Error Inspection

#### Downcasting

```rust
use anyhow::Error;

fn handle_error(error: Error) -> Result<(), Error> {
    // Downcast by reference
    if let Some(io_error) = error.downcast_ref::<std::io::Error>() {
        match io_error.kind() {
            std::io::ErrorKind::NotFound => {
                println!("File not found");
                return Ok(());
            }
            _ => {}
        }
    }
    
    // Downcast by value
    match error.downcast::<std::io::Error>() {
        Ok(io_error) => {
            println!("IO error: {}", io_error);
            Ok(())
        }
        Err(original_error) => Err(original_error),
    }
}
```

#### Chain Inspection

```rust
use anyhow::Error;

fn analyze_error_chain(error: &Error) {
    // Iterate through error chain
    for (i, cause) in error.chain().enumerate() {
        println!("Level {}: {}", i, cause);
    }
    
    // Check for specific error types in chain
    let has_io_error = error.chain().any(|e| e.is::<std::io::Error>());
    let has_json_error = error.chain().any(|e| e.is::<serde_json::Error>());
    
    // Find specific error in chain
    let io_error = error.chain()
        .find_map(|e| e.downcast_ref::<std::io::Error>());
}
```

### Configuration

#### Features

```toml
[dependencies]
# Default features (includes std)
anyhow = "1.0"

# No-std support
anyhow = { version = "1.0", default-features = false }

# Backtrace support for older Rust versions
anyhow = { version = "1.0", features = ["backtrace"] }
```

## Standard Library Error Traits

### `std::error::Error`

The fundamental error trait:

```rust
use std::error::Error;

pub trait Error: Debug + Display {
    // Optional: provide the lower-level source of this error
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
    
    // Optional: provide additional information (nightly only)
    fn provide<'a>(&'a self, request: &mut std::error::Request<'a>) {}
    
    // Deprecated methods (don't use)
    fn description(&self) -> &str { "description() is deprecated; use Display" }
    fn cause(&self) -> Option<&dyn Error> { self.source() }
}
```

### Manual Implementation

```rust
use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub struct MyError {
    message: String,
    source: Option<Box<dyn Error + Send + Sync>>,
}

impl fmt::Display for MyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for MyError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.source.as_ref().map(|e| e.as_ref())
    }
}

impl MyError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            source: None,
        }
    }
    
    pub fn with_source(mut self, source: impl Error + Send + Sync + 'static) -> Self {
        self.source = Some(Box::new(source));
        self
    }
}
```

## Common Patterns Quick Reference

### Error Type Selection

```rust
// Library code: Use thiserror
#[derive(thiserror::Error, Debug)]
pub enum LibraryError {
    #[error("Validation failed: {message}")]
    Validation { message: String },
    
    #[error("IO error")]
    Io(#[from] std::io::Error),
}

// Application code: Use anyhow
use anyhow::{Context, Result};

fn main() -> Result<()> {
    library_function()
        .context("Application startup failed")?;
    Ok(())
}
```

### Error Conversion

```rust
// Automatic conversion
#[derive(thiserror::Error, Debug)]
pub enum MyError {
    #[error("IO error")]
    Io(#[from] std::io::Error),
    
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

// Manual conversion
impl From<serde_json::Error> for MyError {
    fn from(error: serde_json::Error) -> Self {
        MyError::Other(anyhow::Error::new(error))
    }
}
```

### Error Propagation

```rust
use anyhow::{Context, Result};

// Basic propagation
fn basic() -> Result<String> {
    let content = std::fs::read_to_string("file.txt")?;
    Ok(content)
}

// With context
fn with_context() -> Result<String> {
    let content = std::fs::read_to_string("file.txt")
        .context("Failed to read input file")?;
    Ok(content)
}

// With dynamic context
fn with_dynamic_context(path: &str) -> Result<String> {
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Failed to read file: {}", path))?;
    Ok(content)
}
```

### Error Handling

```rust
use anyhow::{Context, Result};

// Specific error handling
fn handle_specific_errors() -> Result<()> {
    match risky_operation() {
        Ok(result) => process_result(result),
        Err(error) => {
            if let Some(io_error) = error.downcast_ref::<std::io::Error>() {
                match io_error.kind() {
                    std::io::ErrorKind::NotFound => create_default_file(),
                    std::io::ErrorKind::PermissionDenied => {
                        anyhow::bail!("Permission denied. Please check file permissions.");
                    }
                    _ => Err(error),
                }
            } else {
                Err(error)
            }
        }
    }
}

// Generic error handling
fn handle_generic_errors() -> Result<()> {
    risky_operation()
        .context("Operation failed")?;
    Ok(())
}
```

### Testing Patterns

```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_error_creation() {
        let error = MyError::Validation {
            message: "test".to_string(),
        };
        assert_eq!(error.to_string(), "Validation failed: test");
    }
    
    #[test]
    fn test_error_chain() {
        let io_error = std::io::Error::new(std::io::ErrorKind::NotFound, "file not found");
        let my_error = MyError::Io(io_error);
        
        assert!(my_error.source().is_some());
        assert!(my_error.source().unwrap().is::<std::io::Error>());
    }
    
    #[test]
    fn test_anyhow_downcast() -> anyhow::Result<()> {
        let error: anyhow::Error = MyError::Validation {
            message: "test".to_string(),
        }.into();
        
        let my_error = error.downcast::<MyError>()?;
        match my_error {
            MyError::Validation { message } => assert_eq!(message, "test"),
            _ => panic!("Wrong error variant"),
        }
        
        Ok(())
    }
}
```

## Cargo.toml Configuration

### Basic Dependencies

```toml
[dependencies]
# For libraries
thiserror = "2.0"

# For applications
anyhow = "1.0"

# Both together
thiserror = "2.0"
anyhow = "1.0"
```

### Feature Configuration

```toml
[dependencies]
# No-std support
anyhow = { version = "1.0", default-features = false }
thiserror = { version = "2.0", default-features = false }

# Backtrace support for older Rust
anyhow = { version = "1.0", features = ["backtrace"] }

# Standard library support (default)
thiserror = { version = "2.0", features = ["std"] }
```

### Development Dependencies

```toml
[dev-dependencies]
# For testing error messages
expect-test = "1.4"

# For property-based testing
proptest = "1.0"

# For snapshot testing
insta = "1.0"

# For error reporting in tests
anyhow = "1.0"
```

## Environment Variables

### Backtrace Configuration

```bash
# Enable backtraces for errors only
export RUST_LIB_BACKTRACE=1

# Enable backtraces for both panics and errors
export RUST_BACKTRACE=1

# Full backtraces (more verbose)
export RUST_BACKTRACE=full

# Disable backtraces
export RUST_LIB_BACKTRACE=0
export RUST_BACKTRACE=0
```

### Runtime Configuration

```rust
fn configure_error_handling() {
    // Set programmatically
    std::env::set_var("RUST_LIB_BACKTRACE", "1");
    
    // Conditional configuration
    #[cfg(debug_assertions)]
    std::env::set_var("RUST_LIB_BACKTRACE", "1");
    
    #[cfg(not(debug_assertions))]
    if std::env::var("RUST_LIB_BACKTRACE").is_err() {
        std::env::set_var("RUST_LIB_BACKTRACE", "0");
    }
}
```

### Logging Integration

```toml
[dependencies]
log = "0.4"
env_logger = "0.10"
# or
tracing = "0.1"
tracing-subscriber = "0.3"
```

```rust
use anyhow::{Context, Result};
use tracing::{error, info, warn};

fn main() -> Result<()> {
    tracing_subscriber::init();
    
    match run_application() {
        Ok(()) => {
            info!("Application completed successfully");
            Ok(())
        }
        Err(error) => {
            error!(error = %error, "Application failed");
            
            // Log error chain
            for (i, cause) in error.chain().enumerate() {
                if i == 0 {
                    error!("Error: {}", cause);
                } else {
                    error!("  Caused by: {}", cause);
                }
            }
            
            Err(error)
        }
    }
}
```

---

*This API reference covers the essential patterns and APIs for robust error handling in Rust. Use it as a quick reference while developing your applications and libraries.*