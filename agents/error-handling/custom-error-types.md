# Custom Error Types and Design Patterns

Designing effective custom error types is crucial for creating maintainable and user-friendly Rust libraries and applications. This guide covers patterns and best practices for creating error types that are both ergonomic to use and informative for debugging.

## Table of Contents

1. [Error Type Design Principles](#error-type-design-principles)
2. [Basic Error Type Patterns](#basic-error-type-patterns)
3. [Domain-Specific Error Design](#domain-specific-error-design)
4. [Error Hierarchies](#error-hierarchies)
5. [Error Context and Metadata](#error-context-and-metadata)
6. [Error Recovery Patterns](#error-recovery-patterns)
7. [Performance Considerations](#performance-considerations)
8. [Testing Error Types](#testing-error-types)
9. [Real-World Examples](#real-world-examples)

## Error Type Design Principles

### 1. **Errors Should Be Actionable**

Design errors that tell users what went wrong and what they can do about it:

```rust
use thiserror::Error;

// Good: Actionable error messages
#[derive(Error, Debug)]
pub enum ConfigError {
    #[error("Configuration file not found at '{path}'. Create the file or specify a different path with --config")]
    FileNotFound { path: String },
    
    #[error("Permission denied reading '{path}'. Check file permissions or run with appropriate privileges")]
    PermissionDenied { path: String },
    
    #[error("Invalid configuration: {field} must be between {min} and {max}, got {value}")]
    InvalidRange { field: String, min: i32, max: i32, value: i32 },
}

// Avoid: Vague, unhelpful errors
#[derive(Error, Debug)]
pub enum BadConfigError {
    #[error("File error")]
    File,
    
    #[error("Bad value")]
    BadValue,
}
```

### 2. **Match Your Domain Model**

Error types should reflect the conceptual model of your domain:

```rust
// Good: Matches HTTP domain concepts
#[derive(Error, Debug)]
pub enum HttpClientError {
    #[error("Request timeout after {timeout:?}")]
    Timeout { timeout: Duration },
    
    #[error("HTTP {status}: {message}")]
    HttpStatus { status: u16, message: String },
    
    #[error("Invalid URL: {url}")]
    InvalidUrl { url: String },
    
    #[error("Network connectivity error")]
    Network(#[from] std::io::Error),
}

// Good: Matches database domain concepts
#[derive(Error, Debug)]
pub enum DatabaseError {
    #[error("Connection failed to {host}:{port}")]
    ConnectionFailed { host: String, port: u16 },
    
    #[error("Query failed: {query}")]
    QueryFailed { query: String },
    
    #[error("Transaction rolled back: {reason}")]
    TransactionRollback { reason: String },
    
    #[error("Constraint violation: {constraint}")]
    ConstraintViolation { constraint: String },
}
```

### 3. **Provide Appropriate Granularity**

Balance between too specific (hard to handle) and too generic (not helpful):

```rust
// Good: Balanced granularity
#[derive(Error, Debug)]
pub enum ValidationError {
    #[error("Required field missing: {field}")]
    RequiredField { field: String },
    
    #[error("Invalid format for {field}: {reason}")]
    InvalidFormat { field: String, reason: String },
    
    #[error("Value out of range for {field}: {value} (expected {min}-{max})")]
    OutOfRange { field: String, value: i64, min: i64, max: i64 },
    
    #[error("Custom validation failed: {message}")]
    Custom { message: String },
}

// Too specific: Hard to handle exhaustively
#[derive(Error, Debug)]
pub enum OverSpecificError {
    #[error("Email missing @ symbol")]
    EmailMissingAt,
    #[error("Email missing domain")]
    EmailMissingDomain,
    #[error("Email domain too short")]
    EmailDomainTooShort,
    // ... dozens more specific cases
}

// Too generic: Not helpful for debugging
#[derive(Error, Debug)]
pub enum TooGenericError {
    #[error("Validation failed")]
    ValidationFailed,
    #[error("Processing error")]
    ProcessingError,
}
```

### 4. **Support Error Recovery**

Design errors to enable recovery when possible:

```rust
#[derive(Error, Debug)]
pub enum ProcessingError {
    #[error("Temporary failure: {reason}. Retry recommended.")]
    Temporary { 
        reason: String,
        retry_after: Option<Duration>,
    },
    
    #[error("Rate limit exceeded. Retry after {retry_after:?}")]
    RateLimit { retry_after: Duration },
    
    #[error("Permanent failure: {reason}. Manual intervention required.")]
    Permanent { reason: String },
    
    #[error("Partial failure: {successful}/{total} items processed")]
    Partial { 
        successful: usize, 
        total: usize,
        failed_items: Vec<(usize, Box<ProcessingError>)>,
    },
}

impl ProcessingError {
    pub fn is_retryable(&self) -> bool {
        matches!(self, 
            ProcessingError::Temporary { .. } | 
            ProcessingError::RateLimit { .. }
        )
    }
    
    pub fn retry_delay(&self) -> Option<Duration> {
        match self {
            ProcessingError::Temporary { retry_after, .. } => *retry_after,
            ProcessingError::RateLimit { retry_after } => Some(*retry_after),
            _ => None,
        }
    }
}
```

## Basic Error Type Patterns

### Simple Enum Pattern

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum SimpleError {
    #[error("IO error")]
    Io(#[from] std::io::Error),
    
    #[error("Parse error: {message}")]
    Parse { message: String },
    
    #[error("Not found: {item}")]
    NotFound { item: String },
}
```

### Struct Error Pattern

```rust
#[derive(Error, Debug)]
#[error("Validation failed at line {line}, column {column}: {message}")]
pub struct ValidationError {
    pub line: usize,
    pub column: usize,
    pub message: String,
    #[source]
    pub cause: Option<Box<dyn std::error::Error + Send + Sync>>,
}

impl ValidationError {
    pub fn new(line: usize, column: usize, message: impl Into<String>) -> Self {
        Self {
            line,
            column,
            message: message.into(),
            cause: None,
        }
    }
    
    pub fn with_cause(mut self, cause: impl std::error::Error + Send + Sync + 'static) -> Self {
        self.cause = Some(Box::new(cause));
        self
    }
}
```

### Hybrid Pattern (Enum with Struct Variants)

```rust
#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("Syntax error at {location}: {message}")]
    Syntax {
        location: SourceLocation,
        message: String,
        help: Option<String>,
    },
    
    #[error("Type error: expected {expected}, found {found}")]
    Type {
        expected: String,
        found: String,
        location: SourceLocation,
    },
    
    #[error("IO error")]
    Io(#[from] std::io::Error),
    
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

#[derive(Debug, Clone)]
pub struct SourceLocation {
    pub file: String,
    pub line: usize,
    pub column: usize,
}

impl std::fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.column)
    }
}
```

## Domain-Specific Error Design

### HTTP Client Errors

```rust
use thiserror::Error;
use std::time::Duration;

#[derive(Error, Debug)]
pub enum HttpError {
    #[error("Request timeout after {timeout:?}")]
    Timeout { timeout: Duration },
    
    #[error("Invalid URL: {url} - {reason}")]
    InvalidUrl { url: String, reason: String },
    
    #[error("HTTP {status}: {message}")]
    Http { 
        status: u16, 
        message: String,
        headers: Option<HeaderMap>,
    },
    
    #[error("Network error: {kind}")]
    Network { 
        kind: NetworkErrorKind,
        #[source]
        source: std::io::Error,
    },
    
    #[error("TLS error")]
    Tls(#[from] rustls::Error),
    
    #[error("JSON parsing error")]
    Json(#[from] serde_json::Error),
    
    #[error("Too many redirects (limit: {limit})")]
    TooManyRedirects { limit: usize },
}

#[derive(Debug, Clone)]
pub enum NetworkErrorKind {
    ConnectionRefused,
    ConnectionTimeout,
    DnsResolution,
    Other,
}

impl HttpError {
    pub fn is_retryable(&self) -> bool {
        match self {
            HttpError::Timeout { .. } => true,
            HttpError::Http { status, .. } => {
                *status >= 500 || *status == 429 // Server errors or rate limiting
            },
            HttpError::Network { kind, .. } => {
                matches!(kind, NetworkErrorKind::ConnectionTimeout | NetworkErrorKind::DnsResolution)
            },
            _ => false,
        }
    }
    
    pub fn status_code(&self) -> Option<u16> {
        match self {
            HttpError::Http { status, .. } => Some(*status),
            _ => None,
        }
    }
}
```

### Database Errors

```rust
#[derive(Error, Debug)]
pub enum DatabaseError {
    #[error("Connection failed: {reason}")]
    Connection { 
        reason: String,
        host: String,
        port: u16,
        database: String,
    },
    
    #[error("Query execution failed")]
    Query {
        query: String,
        #[source]
        cause: Box<dyn std::error::Error + Send + Sync>,
    },
    
    #[error("Transaction failed: {operation}")]
    Transaction {
        operation: TransactionOperation,
        #[source]
        cause: Option<Box<dyn std::error::Error + Send + Sync>>,
    },
    
    #[error("Constraint violation: {constraint_type}")]
    Constraint {
        constraint_type: ConstraintType,
        table: String,
        column: Option<String>,
        value: Option<String>,
    },
    
    #[error("Migration failed: version {from} -> {to}")]
    Migration {
        from: u32,
        to: u32,
        #[source]
        cause: Box<dyn std::error::Error + Send + Sync>,
    },
    
    #[error("Connection pool exhausted (active: {active}, max: {max})")]
    PoolExhausted { active: u32, max: u32 },
}

#[derive(Debug, Clone)]
pub enum TransactionOperation {
    Begin,
    Commit,
    Rollback,
}

#[derive(Debug, Clone)]
pub enum ConstraintType {
    PrimaryKey,
    ForeignKey,
    Unique,
    Check,
    NotNull,
}

impl DatabaseError {
    pub fn is_transient(&self) -> bool {
        matches!(self,
            DatabaseError::Connection { .. } |
            DatabaseError::PoolExhausted { .. }
        )
    }
    
    pub fn is_user_error(&self) -> bool {
        matches!(self, DatabaseError::Constraint { .. })
    }
}
```

### File System Errors

```rust
#[derive(Error, Debug)]
pub enum FileSystemError {
    #[error("File not found: {path}")]
    NotFound { path: PathBuf },
    
    #[error("Permission denied: {path} (operation: {operation})")]
    PermissionDenied { 
        path: PathBuf, 
        operation: FileOperation,
    },
    
    #[error("Disk full: cannot write to {path} (needed: {needed} bytes, available: {available} bytes)")]
    DiskFull { 
        path: PathBuf, 
        needed: u64, 
        available: u64,
    },
    
    #[error("File already exists: {path}")]
    AlreadyExists { path: PathBuf },
    
    #[error("Invalid filename: {filename} - {reason}")]
    InvalidFilename { filename: String, reason: String },
    
    #[error("Directory not empty: {path}")]
    DirectoryNotEmpty { path: PathBuf },
    
    #[error("Symlink loop detected: {path}")]
    SymlinkLoop { path: PathBuf },
    
    #[error("IO error")]
    Io(#[from] std::io::Error),
}

#[derive(Debug, Clone)]
pub enum FileOperation {
    Read,
    Write,
    Create,
    Delete,
    Move,
    Copy,
}

impl FileSystemError {
    pub fn is_recoverable(&self) -> bool {
        matches!(self,
            FileSystemError::PermissionDenied { .. } |
            FileSystemError::DiskFull { .. }
        )
    }
    
    pub fn suggested_action(&self) -> Option<&'static str> {
        match self {
            FileSystemError::NotFound { .. } => Some("Check if the file path is correct"),
            FileSystemError::PermissionDenied { .. } => Some("Check file permissions or run with elevated privileges"),
            FileSystemError::DiskFull { .. } => Some("Free up disk space and try again"),
            FileSystemError::AlreadyExists { .. } => Some("Choose a different filename or remove the existing file"),
            _ => None,
        }
    }
}
```

## Error Hierarchies

### Layered Error Architecture

```rust
// Top-level application error
#[derive(Error, Debug)]
pub enum AppError {
    #[error("Configuration error")]
    Config(#[from] ConfigError),
    
    #[error("Database error")]
    Database(#[from] DatabaseError),
    
    #[error("Network error")]
    Network(#[from] NetworkError),
    
    #[error("Business logic error")]
    Business(#[from] BusinessError),
    
    #[error("System error")]
    System(#[from] SystemError),
}

// Mid-level domain errors
#[derive(Error, Debug)]
pub enum BusinessError {
    #[error("User not found: {user_id}")]
    UserNotFound { user_id: u64 },
    
    #[error("Insufficient permissions: {required}")]
    InsufficientPermissions { required: Permission },
    
    #[error("Business rule violation: {rule}")]
    RuleViolation { rule: String },
    
    #[error("Workflow error")]
    Workflow(#[from] WorkflowError),
}

// Low-level specific errors
#[derive(Error, Debug)]
pub enum WorkflowError {
    #[error("Invalid state transition: {from} -> {to}")]
    InvalidTransition { from: State, to: State },
    
    #[error("Missing required step: {step}")]
    MissingStep { step: String },
    
    #[error("Step timeout: {step} (after {duration:?})")]
    StepTimeout { step: String, duration: Duration },
}
```

### Error Conversion Patterns

```rust
// Automatic conversion with context
impl From<std::io::Error> for AppError {
    fn from(error: std::io::Error) -> Self {
        match error.kind() {
            std::io::ErrorKind::NotFound => {
                AppError::System(SystemError::FileNotFound { 
                    path: "unknown".to_string() 
                })
            },
            std::io::ErrorKind::PermissionDenied => {
                AppError::System(SystemError::PermissionDenied { 
                    resource: "file".to_string() 
                })
            },
            _ => AppError::System(SystemError::Io(error)),
        }
    }
}

// Context-aware conversion
impl DatabaseError {
    pub fn from_sqlx_error(error: sqlx::Error, context: &str) -> Self {
        match error {
            sqlx::Error::RowNotFound => DatabaseError::NotFound {
                table: context.to_string(),
            },
            sqlx::Error::Database(db_err) if db_err.is_unique_violation() => {
                DatabaseError::Constraint {
                    constraint_type: ConstraintType::Unique,
                    table: context.to_string(),
                    column: None,
                    value: None,
                }
            },
            _ => DatabaseError::Query {
                query: context.to_string(),
                cause: Box::new(error),
            },
        }
    }
}
```

## Error Context and Metadata

### Rich Error Context

```rust
#[derive(Error, Debug)]
pub enum ProcessingError {
    #[error("Validation failed")]
    Validation {
        errors: Vec<ValidationIssue>,
        input_size: usize,
        processing_time: Duration,
    },
    
    #[error("Processing failed at step {step}")]
    ProcessingFailed {
        step: ProcessingStep,
        progress: ProcessingProgress,
        #[source]
        cause: Box<dyn std::error::Error + Send + Sync>,
    },
}

#[derive(Debug, Clone)]
pub struct ValidationIssue {
    pub field: String,
    pub message: String,
    pub severity: Severity,
    pub suggestion: Option<String>,
}

#[derive(Debug, Clone)]
pub struct ProcessingProgress {
    pub completed_steps: Vec<ProcessingStep>,
    pub current_step: ProcessingStep,
    pub remaining_steps: Vec<ProcessingStep>,
    pub start_time: std::time::Instant,
}

#[derive(Debug, Clone)]
pub enum Severity {
    Error,
    Warning,
    Info,
}
```

### Error Metadata for Debugging

```rust
#[derive(Error, Debug)]
pub struct DetailedError {
    #[source]
    pub cause: Box<dyn std::error::Error + Send + Sync>,
    pub context: ErrorContext,
    pub metadata: ErrorMetadata,
}

#[derive(Debug)]
pub struct ErrorContext {
    pub operation: String,
    pub user_id: Option<u64>,
    pub request_id: Option<String>,
    pub timestamp: std::time::SystemTime,
}

#[derive(Debug)]
pub struct ErrorMetadata {
    pub severity: ErrorSeverity,
    pub category: ErrorCategory,
    pub tags: Vec<String>,
    pub metrics: std::collections::HashMap<String, f64>,
}

#[derive(Debug, Clone)]
pub enum ErrorSeverity {
    Critical,
    High,
    Medium,
    Low,
}

#[derive(Debug, Clone)]
pub enum ErrorCategory {
    UserError,
    SystemError,
    ExternalError,
    ConfigurationError,
}

impl std::fmt::Display for DetailedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} (severity: {:?}, category: {:?})", 
               self.cause, self.metadata.severity, self.metadata.category)
    }
}
```

## Error Recovery Patterns

### Recoverable Error Design

```rust
#[derive(Error, Debug)]
pub enum RecoverableError {
    #[error("Temporary failure: {reason}")]
    Temporary {
        reason: String,
        retry_strategy: RetryStrategy,
        attempt: u32,
    },
    
    #[error("Degraded service: {service}")]
    Degraded {
        service: String,
        fallback_available: bool,
        estimated_recovery: Option<Duration>,
    },
    
    #[error("Partial failure: {successful}/{total} operations completed")]
    Partial {
        successful: usize,
        total: usize,
        failures: Vec<(usize, Box<RecoverableError>)>,
        can_continue: bool,
    },
}

#[derive(Debug, Clone)]
pub struct RetryStrategy {
    pub max_attempts: u32,
    pub base_delay: Duration,
    pub max_delay: Duration,
    pub backoff_multiplier: f64,
}

impl RecoverableError {
    pub fn should_retry(&self) -> bool {
        match self {
            RecoverableError::Temporary { attempt, retry_strategy, .. } => {
                *attempt < retry_strategy.max_attempts
            },
            RecoverableError::Degraded { fallback_available, .. } => !fallback_available,
            RecoverableError::Partial { can_continue, .. } => *can_continue,
        }
    }
    
    pub fn next_retry_delay(&self) -> Option<Duration> {
        match self {
            RecoverableError::Temporary { attempt, retry_strategy, .. } => {
                if *attempt < retry_strategy.max_attempts {
                    let delay = retry_strategy.base_delay.as_millis() as f64 
                        * retry_strategy.backoff_multiplier.powi(*attempt as i32);
                    Some(Duration::from_millis(delay.min(retry_strategy.max_delay.as_millis() as f64) as u64))
                } else {
                    None
                }
            },
            _ => None,
        }
    }
}
```

### Circuit Breaker Error Pattern

```rust
#[derive(Error, Debug)]
pub enum CircuitBreakerError {
    #[error("Circuit breaker open: {service} (failures: {failure_count}, last_failure: {last_failure:?})")]
    Open {
        service: String,
        failure_count: u32,
        last_failure: std::time::Instant,
        retry_after: Duration,
    },
    
    #[error("Circuit breaker half-open: {service} (testing recovery)")]
    HalfOpen {
        service: String,
        test_request_count: u32,
    },
    
    #[error("Service failure: {service}")]
    ServiceFailure {
        service: String,
        #[source]
        cause: Box<dyn std::error::Error + Send + Sync>,
    },
}

impl CircuitBreakerError {
    pub fn can_retry_after(&self) -> Option<Duration> {
        match self {
            CircuitBreakerError::Open { retry_after, .. } => Some(*retry_after),
            _ => None,
        }
    }
}
```

## Performance Considerations

### Memory-Efficient Error Types

```rust
// Good: Compact error representation
#[derive(Error, Debug)]
pub enum CompactError {
    #[error("IO error: {kind}")]
    Io { kind: IoErrorKind }, // 1 byte instead of full io::Error
    
    #[error("Parse error at position {pos}")]
    Parse { pos: u32 }, // u32 instead of usize
    
    #[error("Network error: {code}")]
    Network { code: u16 }, // u16 instead of String
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum IoErrorKind {
    NotFound,
    PermissionDenied,
    ConnectionRefused,
    Other,
}

// Avoid: Large error types
#[derive(Error, Debug)]
pub enum LargeError {
    #[error("Error with large context")]
    WithContext {
        context: String,           // Potentially large
        metadata: HashMap<String, String>, // Very large
        stack_trace: Vec<String>,  // Extremely large
    },
}
```

### Lazy Error Construction

```rust
use std::sync::Arc;

#[derive(Error, Debug)]
pub enum LazyError {
    #[error("Expensive error: {}", .0())]
    Expensive(Arc<dyn Fn() -> String + Send + Sync>),
    
    #[error("Simple error")]
    Simple,
}

impl LazyError {
    pub fn expensive(f: impl Fn() -> String + Send + Sync + 'static) -> Self {
        LazyError::Expensive(Arc::new(f))
    }
}

// Usage
fn create_error() -> LazyError {
    LazyError::expensive(|| {
        // This expensive computation only runs when the error is displayed
        format!("Complex error message with computation: {}", expensive_computation())
    })
}
```

## Testing Error Types

### Unit Testing Error Creation

```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_error_creation() {
        let error = ValidationError::RequiredField {
            field: "email".to_string(),
        };
        
        assert_eq!(error.to_string(), "Required field missing: email");
    }
    
    #[test]
    fn test_error_conversion() {
        let io_error = std::io::Error::new(std::io::ErrorKind::NotFound, "file not found");
        let app_error: AppError = io_error.into();
        
        match app_error {
            AppError::System(SystemError::FileNotFound { .. }) => {},
            _ => panic!("Expected FileNotFound error"),
        }
    }
    
    #[test]
    fn test_error_chain() {
        let root_cause = std::io::Error::new(std::io::ErrorKind::PermissionDenied, "access denied");
        let mid_error = DatabaseError::Connection {
            reason: "Failed to connect".to_string(),
            host: "localhost".to_string(),
            port: 5432,
            database: "test".to_string(),
        };
        let top_error = AppError::Database(mid_error);
        
        // Test error chain walking
        let mut source = top_error.source();
        let mut depth = 0;
        while let Some(err) = source {
            depth += 1;
            source = err.source();
        }
        
        assert!(depth > 0);
    }
}
```

### Property-Based Testing

```rust
#[cfg(test)]
mod property_tests {
    use super::*;
    use proptest::prelude::*;
    
    proptest! {
        #[test]
        fn error_display_never_panics(
            field in "[a-zA-Z_][a-zA-Z0-9_]*",
            value in any::<i32>(),
            min in any::<i32>(),
            max in any::<i32>(),
        ) {
            let error = ValidationError::OutOfRange {
                field,
                value: value as i64,
                min: min as i64,
                max: max as i64,
            };
            
            // Should never panic
            let _ = error.to_string();
            let _ = format!("{:?}", error);
        }
        
        #[test]
        fn error_source_chain_terminates(
            reason in ".*",
            host in "[a-zA-Z0-9.-]+",
            port in 1u16..65535,
        ) {
            let error = DatabaseError::Connection {
                reason,
                host,
                port,
                database: "test".to_string(),
            };
            
            // Error chain should terminate
            let mut source = error.source();
            let mut depth = 0;
            while let Some(err) = source {
                depth += 1;
                source = err.source();
                // Prevent infinite loops in tests
                prop_assert!(depth < 100);
            }
        }
    }
}
```

## Real-World Examples

### Web API Error Types

```rust
use axum::http::StatusCode;
use serde::Serialize;

#[derive(Error, Debug)]
pub enum ApiError {
    #[error("Authentication required")]
    Unauthorized,
    
    #[error("Insufficient permissions: {required}")]
    Forbidden { required: String },
    
    #[error("Resource not found: {resource_type} with id {id}")]
    NotFound { resource_type: String, id: String },
    
    #[error("Validation failed")]
    Validation { errors: Vec<ValidationError> },
    
    #[error("Rate limit exceeded: {limit} requests per {window:?}")]
    RateLimit { limit: u32, window: Duration },
    
    #[error("Internal server error")]
    Internal(#[from] anyhow::Error),
}

#[derive(Debug, Serialize)]
pub struct ValidationError {
    pub field: String,
    pub message: String,
    pub code: String,
}

impl ApiError {
    pub fn status_code(&self) -> StatusCode {
        match self {
            ApiError::Unauthorized => StatusCode::UNAUTHORIZED,
            ApiError::Forbidden { .. } => StatusCode::FORBIDDEN,
            ApiError::NotFound { .. } => StatusCode::NOT_FOUND,
            ApiError::Validation { .. } => StatusCode::BAD_REQUEST,
            ApiError::RateLimit { .. } => StatusCode::TOO_MANY_REQUESTS,
            ApiError::Internal(_) => StatusCode::INTERNAL_SERVER_ERROR,
        }
    }
    
    pub fn error_code(&self) -> &'static str {
        match self {
            ApiError::Unauthorized => "UNAUTHORIZED",
            ApiError::Forbidden { .. } => "FORBIDDEN",
            ApiError::NotFound { .. } => "NOT_FOUND",
            ApiError::Validation { .. } => "VALIDATION_FAILED",
            ApiError::RateLimit { .. } => "RATE_LIMIT_EXCEEDED",
            ApiError::Internal(_) => "INTERNAL_ERROR",
        }
    }
}

// Response serialization
#[derive(Serialize)]
pub struct ErrorResponse {
    pub error: String,
    pub code: String,
    pub details: Option<serde_json::Value>,
}

impl From<ApiError> for ErrorResponse {
    fn from(error: ApiError) -> Self {
        let details = match &error {
            ApiError::Validation { errors } => {
                Some(serde_json::to_value(errors).unwrap_or_default())
            },
            ApiError::NotFound { resource_type, id } => {
                Some(serde_json::json!({
                    "resource_type": resource_type,
                    "id": id
                }))
            },
            _ => None,
        };
        
        ErrorResponse {
            error: error.to_string(),
            code: error.error_code().to_string(),
            details,
        }
    }
}
```

### CLI Application Error Types

```rust
#[derive(Error, Debug)]
pub enum CliError {
    #[error("Invalid command line arguments")]
    InvalidArgs {
        #[source]
        source: clap::Error,
        suggestion: Option<String>,
    },
    
    #[error("Configuration error: {message}")]
    Config {
        message: String,
        config_path: Option<PathBuf>,
        line_number: Option<usize>,
    },
    
    #[error("File operation failed: {operation} on {path}")]
    FileOperation {
        operation: String,
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },
    
    #[error("Processing failed: {stage}")]
    Processing {
        stage: ProcessingStage,
        input_file: Option<PathBuf>,
        progress: Option<ProcessingProgress>,
        #[source]
        source: Box<dyn std::error::Error + Send + Sync>,
    },
    
    #[error("External command failed: {command}")]
    ExternalCommand {
        command: String,
        exit_code: Option<i32>,
        stdout: String,
        stderr: String,
    },
}

impl CliError {
    pub fn exit_code(&self) -> i32 {
        match self {
            CliError::InvalidArgs { .. } => 2,
            CliError::Config { .. } => 3,
            CliError::FileOperation { .. } => 4,
            CliError::Processing { .. } => 5,
            CliError::ExternalCommand { exit_code, .. } => exit_code.unwrap_or(1),
        }
    }
    
    pub fn user_facing_message(&self) -> String {
        match self {
            CliError::InvalidArgs { suggestion, .. } => {
                if let Some(suggestion) = suggestion {
                    format!("{}\n\nTry: {}", self, suggestion)
                } else {
                    format!("{}\n\nUse --help for usage information", self)
                }
            },
            CliError::Config { config_path, line_number, .. } => {
                let location = match (config_path, line_number) {
                    (Some(path), Some(line)) => format!(" in {} at line {}", path.display(), line),
                    (Some(path), None) => format!(" in {}", path.display()),
                    _ => String::new(),
                };
                format!("{}{}", self, location)
            },
            _ => self.to_string(),
        }
    }
}
```

---

*Well-designed error types are the foundation of robust Rust applications. They should be informative, actionable, and designed with both developers and end-users in mind.*