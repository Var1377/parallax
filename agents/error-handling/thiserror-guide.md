# The Complete Guide to `thiserror` 2.0

`thiserror` is a derive macro for the standard library's `std::error::Error` trait. It's designed for **library authors** who want to create well-designed, specific error types that provide precise information to callers.

## Table of Contents

1. [Installation and Setup](#installation-and-setup)
2. [Basic Usage](#basic-usage)
3. [Error Message Formatting](#error-message-formatting)
4. [Automatic Conversions with `#[from]`](#automatic-conversions-with-from)
5. [Error Sources and Chaining](#error-sources-and-chaining)
6. [Transparent Error Forwarding](#transparent-error-forwarding)
7. [Backtrace Support](#backtrace-support)
8. [Advanced Patterns](#advanced-patterns)
9. [Best Practices](#best-practices)
10. [Real-World Examples](#real-world-examples)

## Installation and Setup

Add `thiserror` to your `Cargo.toml`:

```toml
[dependencies]
thiserror = "2.0"
```

**Minimum Rust Version**: 1.61+

## Basic Usage

### Simple Enum Errors

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum DataStoreError {
    #[error("data store disconnected")]
    Disconnect,
    
    #[error("the data for key `{0}` is not available")]
    Redaction(String),
    
    #[error("invalid header (expected {expected:?}, found {found:?})")]
    InvalidHeader {
        expected: String,
        found: String,
    },
    
    #[error("unknown data store error")]
    Unknown,
}
```

### Struct Errors

```rust
#[derive(Error, Debug)]
#[error("Configuration error: {message}")]
pub struct ConfigError {
    pub message: String,
    pub line: usize,
    pub column: usize,
}

// Usage
let error = ConfigError {
    message: "Invalid syntax".to_string(),
    line: 42,
    column: 15,
};
```

### Unit Struct Errors

```rust
#[derive(Error, Debug)]
#[error("Network timeout occurred")]
pub struct TimeoutError;
```

### Tuple Struct Errors

```rust
#[derive(Error, Debug)]
#[error("Parse error: {0}")]
pub struct ParseError(pub String);
```

## Error Message Formatting

### Field Interpolation

`thiserror` provides convenient shorthand for interpolating fields:

```rust
#[derive(Error, Debug)]
pub enum ValidationError {
    // Shorthand: {var} → write!("{}", self.var)
    #[error("Invalid email: {email}")]
    InvalidEmail { email: String },
    
    // Shorthand: {0} → write!("{}", self.0)
    #[error("Invalid age: {0}")]
    InvalidAge(u32),
    
    // Debug formatting: {var:?} → write!("{:?}", self.var)
    #[error("Invalid data: {data:?}")]
    InvalidData { data: Vec<u8> },
    
    // Tuple debug: {0:?} → write!("{:?}", self.0)
    #[error("Parse error: {0:?}")]
    ParseError(serde_json::Error),
}
```

### Complex Formatting with Additional Arguments

```rust
#[derive(Error, Debug)]
pub enum RangeError {
    #[error("invalid value {value} (expected between {min} and {max})", 
            min = 0, max = 100)]
    OutOfRange { value: i32 },
    
    #[error("invalid lookahead {0} (expected < {max})", max = i32::MAX)]
    InvalidLookahead(u32),
}
```

### Referencing Fields in Expressions

Use `.field` syntax to reference struct/enum fields in expressions:

```rust
#[derive(Error, Debug)]
pub enum ProcessError {
    #[error("first character must be lowercase but was {:?}", first_char(.0))]
    WrongCase(String),
    
    #[error("index {idx} out of bounds (expected {}.{})", .limits.min, .limits.max)]
    OutOfBounds { 
        idx: usize, 
        limits: Range,
    },
}

struct Range {
    min: usize,
    max: usize,
}

fn first_char(s: &str) -> Option<char> {
    s.chars().next()
}
```

## Automatic Conversions with `#[from]`

The `#[from]` attribute generates `From` implementations for automatic error conversion:

```rust
#[derive(Error, Debug)]
pub enum MyError {
    #[error("IO error")]
    Io(#[from] std::io::Error),
    
    #[error("JSON parsing error")]
    Json(#[from] serde_json::Error),
    
    #[error("HTTP error")]
    Http(#[from] reqwest::Error),
    
    #[error("Custom error: {message}")]
    Custom { message: String },
}

// Generated implementations allow this:
fn example() -> Result<(), MyError> {
    let _content = std::fs::read_to_string("file.txt")?; // io::Error → MyError::Io
    let _data: Value = serde_json::from_str("{}")?;      // serde_json::Error → MyError::Json
    Ok(())
}
```

### `#[from]` with Named Fields

```rust
#[derive(Error, Debug)]
pub enum DatabaseError {
    #[error("Connection failed")]
    Connection {
        #[from]
        source: std::io::Error,
        host: String,
    },
}

// You need to implement From manually for named fields with additional data:
impl From<std::io::Error> for DatabaseError {
    fn from(error: std::io::Error) -> Self {
        DatabaseError::Connection {
            source: error,
            host: "localhost".to_string(), // Default or derive from context
        }
    }
}
```

## Error Sources and Chaining

### Automatic Source Detection

`thiserror` automatically implements `Error::source()` for:
- Fields named `source`
- Fields with `#[source]` attribute
- Fields with `#[from]` attribute (implies `#[source]`)

```rust
#[derive(Error, Debug)]
pub enum ChainedError {
    #[error("High-level operation failed")]
    HighLevel {
        #[source]  // Optional if field is named 'source'
        source: MiddleError,
        operation: String,
    },
    
    #[error("IO operation failed")]
    Io(#[from] std::io::Error), // #[from] implies #[source]
}

#[derive(Error, Debug)]
#[error("Middle-level error")]
pub struct MiddleError {
    #[source]
    pub cause: LowLevelError,
}

#[derive(Error, Debug)]
#[error("Low-level error occurred")]
pub struct LowLevelError;
```

### Manual Source Implementation

```rust
#[derive(Error, Debug)]
#[error("Complex error with multiple potential sources")]
pub struct ComplexError {
    pub primary_cause: Option<Box<dyn std::error::Error + Send + Sync>>,
    pub secondary_cause: Option<std::io::Error>,
}

// Manual implementation for complex source logic
impl std::error::Error for ComplexError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.primary_cause
            .as_ref()
            .map(|e| e.as_ref() as &(dyn std::error::Error + 'static))
            .or_else(|| self.secondary_cause.as_ref().map(|e| e as &(dyn std::error::Error + 'static)))
    }
}
```

## Transparent Error Forwarding

Use `#[error(transparent)]` to forward `Display` and `source()` implementations:

```rust
#[derive(Error, Debug)]
pub enum ApiError {
    #[error("Authentication failed: {message}")]
    Auth { message: String },
    
    #[error("Rate limit exceeded")]
    RateLimit,
    
    // Forwards Display and source() to the wrapped error
    #[error(transparent)]
    Network(#[from] reqwest::Error),
    
    // Catch-all for unexpected errors
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}
```

### Opaque Error Types

Use transparent errors to hide implementation details:

```rust
// Public API - stable and opaque
#[derive(Error, Debug)]
#[error(transparent)]
pub struct PublicError(#[from] ErrorRepr);

impl PublicError {
    // Expose only what you want in the public API
    pub fn is_retryable(&self) -> bool {
        matches!(self.0, ErrorRepr::Network(_) | ErrorRepr::Timeout)
    }
}

// Private implementation - can change freely
#[derive(Error, Debug)]
enum ErrorRepr {
    #[error("Network error")]
    Network(#[from] std::io::Error),
    
    #[error("Timeout after {seconds}s")]
    Timeout { seconds: u64 },
    
    #[error("Invalid input: {input}")]
    InvalidInput { input: String },
}
```

## Backtrace Support

### Automatic Backtrace Detection

`thiserror` automatically provides backtraces for fields named `Backtrace`:

```rust
use std::backtrace::Backtrace;

#[derive(Error, Debug)]
pub enum MyError {
    #[error("Operation failed")]
    OperationFailed {
        message: String,
        backtrace: Backtrace,  // Automatically detected
    },
    
    #[error("IO error")]
    Io {
        #[from]
        source: std::io::Error,
        backtrace: Backtrace,  // Captured in From impl
    },
}
```

### Forwarding Backtraces from Sources

Use `#[backtrace]` to forward backtraces from source errors:

```rust
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
```

**Note**: Backtrace support requires nightly Rust (1.73+) or the `backtrace` feature.

## Advanced Patterns

### Generic Error Types

```rust
#[derive(Error, Debug)]
pub enum GenericError<T>
where
    T: std::error::Error + Send + Sync + 'static,
{
    #[error("Validation failed")]
    Validation(#[from] ValidationError),
    
    #[error("External error")]
    External(#[from] T),
    
    #[error("Internal error: {message}")]
    Internal { message: String },
}

// Usage with specific error types
type DatabaseResult<T> = Result<T, GenericError<diesel::result::Error>>;
type ApiResult<T> = Result<T, GenericError<reqwest::Error>>;
```

### Error Enums with Shared Fields

```rust
#[derive(Error, Debug)]
pub enum RequestError {
    #[error("Bad request: {message} (request_id: {request_id})")]
    BadRequest { 
        message: String, 
        request_id: String,
    },
    
    #[error("Unauthorized (request_id: {request_id})")]
    Unauthorized { 
        request_id: String,
    },
    
    #[error("Internal server error (request_id: {request_id})")]
    Internal { 
        request_id: String,
        #[source]
        cause: Box<dyn std::error::Error + Send + Sync>,
    },
}

impl RequestError {
    pub fn request_id(&self) -> &str {
        match self {
            RequestError::BadRequest { request_id, .. } => request_id,
            RequestError::Unauthorized { request_id } => request_id,
            RequestError::Internal { request_id, .. } => request_id,
        }
    }
}
```

### Conditional Error Variants

```rust
#[derive(Error, Debug)]
pub enum ConfigError {
    #[error("File not found: {path}")]
    FileNotFound { path: String },
    
    #[error("Parse error: {message}")]
    ParseError { message: String },
    
    #[cfg(feature = "validation")]
    #[error("Validation error: {details}")]
    ValidationError { details: String },
    
    #[cfg(feature = "remote")]
    #[error("Remote config error")]
    RemoteError(#[from] RemoteConfigError),
}
```

## Best Practices

### 1. Design Error Types for Your Domain

Create error types that match your problem domain:

```rust
// Good: Domain-specific errors
#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("Syntax error at line {line}, column {column}: {message}")]
    Syntax { line: usize, column: usize, message: String },
    
    #[error("Type error: expected {expected}, found {found}")]
    Type { expected: String, found: String },
    
    #[error("Undefined variable: {name}")]
    UndefinedVariable { name: String },
}

// Avoid: Generic, unhelpful errors
#[derive(Error, Debug)]
pub enum BadError {
    #[error("Error occurred")]
    Error,
    
    #[error("Something went wrong: {0}")]
    Generic(String),
}
```

### 2. Provide Helpful Error Messages

```rust
#[derive(Error, Debug)]
pub enum ValidationError {
    // Good: Specific, actionable message
    #[error("Email address '{email}' is invalid (missing @ symbol)")]
    InvalidEmail { email: String },
    
    // Good: Include expected vs actual
    #[error("Password too short (got {actual} characters, minimum {minimum})")]
    PasswordTooShort { actual: usize, minimum: usize },
    
    // Good: Suggest solutions
    #[error("Invalid date format '{input}' (expected YYYY-MM-DD)")]
    InvalidDateFormat { input: String },
}
```

### 3. Use Appropriate Error Granularity

```rust
// Good: Balanced granularity
#[derive(Error, Debug)]
pub enum DatabaseError {
    #[error("Connection failed: {reason}")]
    ConnectionFailed { reason: String },
    
    #[error("Query failed: {query}")]
    QueryFailed { query: String, #[source] cause: sqlx::Error },
    
    #[error("Transaction failed")]
    TransactionFailed(#[from] TransactionError),
    
    #[error("Migration error")]
    Migration(#[from] MigrationError),
}

// Too granular: Hard to handle
#[derive(Error, Debug)]
pub enum OverlySpecificError {
    #[error("Connection failed on Monday")]
    ConnectionFailedMonday,
    #[error("Connection failed on Tuesday")]
    ConnectionFailedTuesday,
    // ... one for each day
}

// Too generic: Not helpful
#[derive(Error, Debug)]
pub enum TooGenericError {
    #[error("Database error")]
    Database(#[from] Box<dyn std::error::Error>),
}
```

### 4. Implement Helper Methods

```rust
#[derive(Error, Debug)]
pub enum HttpError {
    #[error("Client error: {status}")]
    Client { status: u16, body: String },
    
    #[error("Server error: {status}")]
    Server { status: u16, body: String },
    
    #[error("Network error")]
    Network(#[from] std::io::Error),
}

impl HttpError {
    pub fn is_retryable(&self) -> bool {
        match self {
            HttpError::Client { status, .. } => *status == 429, // Rate limit
            HttpError::Server { .. } => true,
            HttpError::Network(_) => true,
        }
    }
    
    pub fn status_code(&self) -> Option<u16> {
        match self {
            HttpError::Client { status, .. } | HttpError::Server { status, .. } => Some(*status),
            HttpError::Network(_) => None,
        }
    }
}
```

### 5. Consider Error Recovery

Design errors to support recovery when possible:

```rust
#[derive(Error, Debug)]
pub enum ProcessingError {
    #[error("Recoverable error: {message}")]
    Recoverable { 
        message: String,
        retry_after: Option<std::time::Duration>,
    },
    
    #[error("Fatal error: {message}")]
    Fatal { message: String },
    
    #[error("Partial failure: {successful} of {total} items processed")]
    Partial { 
        successful: usize, 
        total: usize, 
        failures: Vec<ItemError>,
    },
}

impl ProcessingError {
    pub fn can_retry(&self) -> bool {
        matches!(self, ProcessingError::Recoverable { .. })
    }
    
    pub fn retry_delay(&self) -> Option<std::time::Duration> {
        match self {
            ProcessingError::Recoverable { retry_after, .. } => *retry_after,
            _ => None,
        }
    }
}
```

## Real-World Examples

### HTTP Client Library

```rust
use thiserror::Error;
use std::time::Duration;

#[derive(Error, Debug)]
pub enum HttpClientError {
    #[error("Request timeout after {timeout:?}")]
    Timeout { timeout: Duration },
    
    #[error("Invalid URL: {url}")]
    InvalidUrl { url: String },
    
    #[error("HTTP {status}: {message}")]
    Http { status: u16, message: String },
    
    #[error("Network error")]
    Network(#[from] std::io::Error),
    
    #[error("TLS error")]
    Tls(#[from] rustls::Error),
    
    #[error("JSON parsing error")]
    Json(#[from] serde_json::Error),
    
    #[error("Too many redirects (limit: {limit})")]
    TooManyRedirects { limit: usize },
}

impl HttpClientError {
    pub fn is_retryable(&self) -> bool {
        match self {
            HttpClientError::Timeout { .. } => true,
            HttpClientError::Http { status, .. } => {
                *status >= 500 || *status == 429
            },
            HttpClientError::Network(_) => true,
            _ => false,
        }
    }
}
```

### Configuration Parser

```rust
#[derive(Error, Debug)]
pub enum ConfigError {
    #[error("Configuration file not found: {path}")]
    FileNotFound { path: std::path::PathBuf },
    
    #[error("Permission denied reading config file: {path}")]
    PermissionDenied { path: std::path::PathBuf },
    
    #[error("Invalid TOML syntax at line {line}: {message}")]
    TomlSyntax { line: usize, message: String },
    
    #[error("Missing required field: {field}")]
    MissingField { field: String },
    
    #[error("Invalid value for {field}: {value} (expected {expected})")]
    InvalidValue { 
        field: String, 
        value: String, 
        expected: String,
    },
    
    #[error("Validation failed: {message}")]
    Validation { message: String },
    
    #[error("IO error")]
    Io(#[from] std::io::Error),
}

impl From<toml::de::Error> for ConfigError {
    fn from(error: toml::de::Error) -> Self {
        if let Some(span) = error.span() {
            ConfigError::TomlSyntax {
                line: span.start,
                message: error.message().to_string(),
            }
        } else {
            ConfigError::TomlSyntax {
                line: 0,
                message: error.message().to_string(),
            }
        }
    }
}
```

### Database Abstraction Layer

```rust
#[derive(Error, Debug)]
pub enum DatabaseError {
    #[error("Connection pool exhausted (max: {max_connections})")]
    PoolExhausted { max_connections: u32 },
    
    #[error("Connection failed to {host}:{port}")]
    ConnectionFailed { host: String, port: u16 },
    
    #[error("Query execution failed: {query}")]
    QueryFailed { 
        query: String,
        #[source] 
        cause: Box<dyn std::error::Error + Send + Sync>,
    },
    
    #[error("Transaction rolled back: {reason}")]
    TransactionRollback { reason: String },
    
    #[error("Constraint violation: {constraint}")]
    ConstraintViolation { constraint: String },
    
    #[error("Record not found")]
    NotFound,
    
    #[error("Serialization error")]
    Serialization(#[from] serde_json::Error),
    
    #[error("Migration error: {version} -> {target}")]
    Migration { version: u32, target: u32 },
}

impl DatabaseError {
    pub fn is_transient(&self) -> bool {
        matches!(
            self,
            DatabaseError::PoolExhausted { .. } | 
            DatabaseError::ConnectionFailed { .. }
        )
    }
    
    pub fn is_user_error(&self) -> bool {
        matches!(
            self,
            DatabaseError::ConstraintViolation { .. } |
            DatabaseError::NotFound
        )
    }
}
```

---

*This guide covers `thiserror` 2.0.12. For the latest features and updates, see the [official documentation](https://docs.rs/thiserror).*