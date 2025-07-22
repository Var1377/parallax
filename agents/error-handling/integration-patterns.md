# Integrating `thiserror` and `anyhow`

While `thiserror` and `anyhow` serve different purposes, they work excellently together. This guide shows how to effectively combine them in real-world applications, creating a robust error handling strategy that leverages the strengths of both libraries.

## Table of Contents

1. [Integration Philosophy](#integration-philosophy)
2. [Library-Application Boundary](#library-application-boundary)
3. [Error Conversion Patterns](#error-conversion-patterns)
4. [Transparent Error Forwarding](#transparent-error-forwarding)
5. [Mixed Error Handling](#mixed-error-handling)
6. [Testing Integration](#testing-integration)
7. [Performance Considerations](#performance-considerations)
8. [Real-World Examples](#real-world-examples)

## Integration Philosophy

### When to Use Each

The key principle is to use the right tool for the right layer:

```rust
// Library layer: Use thiserror for specific, typed errors
use thiserror::Error;

#[derive(Error, Debug)]
pub enum DatabaseError {
    #[error("Connection failed: {reason}")]
    ConnectionFailed { reason: String },
    
    #[error("Query failed: {query}")]
    QueryFailed { query: String },
    
    #[error("Transaction rolled back")]
    TransactionRollback,
}

// Application layer: Use anyhow for easy error handling
use anyhow::{Context, Result};

fn main() -> Result<()> {
    let db = connect_database()
        .context("Failed to initialize database connection")?;
    
    run_application(db)
        .context("Application execution failed")?;
    
    Ok(())
}
```

### Conversion Strategy

Design your error types to work seamlessly with `anyhow`:

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum LibraryError {
    #[error("Validation failed: {message}")]
    Validation { message: String },
    
    #[error("IO error")]
    Io(#[from] std::io::Error),
    
    // Transparent forwarding for unexpected errors
    #[error(transparent)]
    Unexpected(#[from] anyhow::Error),
}

// This automatically works with anyhow
fn application_code() -> anyhow::Result<()> {
    library_function()?; // LibraryError automatically converts to anyhow::Error
    Ok(())
}
```

## Library-Application Boundary

### Library Design with `thiserror`

Design your library errors to be specific and actionable:

```rust
// src/lib.rs - Library code
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ConfigError {
    #[error("Configuration file not found: {path}")]
    FileNotFound { path: String },
    
    #[error("Invalid configuration format")]
    InvalidFormat(#[from] toml::de::Error),
    
    #[error("Missing required field: {field}")]
    MissingField { field: String },
    
    #[error("Invalid value for {field}: {value}")]
    InvalidValue { field: String, value: String },
}

#[derive(Error, Debug)]
pub enum ProcessingError {
    #[error("Configuration error")]
    Config(#[from] ConfigError),
    
    #[error("Network error")]
    Network(#[from] reqwest::Error),
    
    #[error("Processing failed: {reason}")]
    Processing { reason: String },
}

// Public API uses specific error types
pub fn load_config(path: &str) -> Result<Config, ConfigError> {
    // Implementation
    Ok(Config::default())
}

pub fn process_data(config: &Config) -> Result<ProcessedData, ProcessingError> {
    // Implementation
    Ok(ProcessedData::default())
}
```

### Application Integration with `anyhow`

Use `anyhow` in your application to handle library errors:

```rust
// src/main.rs - Application code
use anyhow::{Context, Result};
use my_library::{load_config, process_data, ConfigError, ProcessingError};

fn main() -> Result<()> {
    let config = load_config("config.toml")
        .context("Failed to load application configuration")?;
    
    let result = process_data(&config)
        .context("Data processing failed")?;
    
    println!("Processing completed: {:?}", result);
    Ok(())
}

// Handle specific library errors when needed
fn handle_config_errors() -> Result<()> {
    match load_config("config.toml") {
        Ok(config) => {
            // Use config
            Ok(())
        }
        Err(ConfigError::FileNotFound { path }) => {
            println!("Config file not found at {}, creating default...", path);
            create_default_config(&path)
                .context("Failed to create default configuration")
        }
        Err(ConfigError::MissingField { field }) => {
            anyhow::bail!("Configuration incomplete: missing required field '{}'", field);
        }
        Err(error) => {
            Err(error).context("Configuration loading failed")
        }
    }
}
```

## Error Conversion Patterns

### Automatic Conversion with `From`

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum LibraryError {
    #[error("Validation error: {message}")]
    Validation { message: String },
    
    #[error("IO error")]
    Io(#[from] std::io::Error),
    
    #[error("JSON error")]
    Json(#[from] serde_json::Error),
}

// Automatic conversion to anyhow::Error
impl From<LibraryError> for anyhow::Error {
    fn from(error: LibraryError) -> Self {
        anyhow::Error::new(error)
    }
}

// Or use the built-in conversion (this is automatic)
fn use_library() -> anyhow::Result<()> {
    library_function()?; // LibraryError -> anyhow::Error automatically
    Ok(())
}
```

### Manual Conversion with Context

```rust
use anyhow::{Context, Result};

fn convert_with_context() -> Result<()> {
    library_function()
        .map_err(|e| match e {
            LibraryError::Validation { message } => {
                anyhow::anyhow!("Input validation failed: {}", message)
            }
            LibraryError::Io(io_err) => {
                anyhow::Error::new(io_err).context("File operation failed")
            }
            other => anyhow::Error::new(other),
        })?;
    
    Ok(())
}

// Or use with_context for uniform handling
fn convert_with_uniform_context() -> Result<()> {
    library_function()
        .with_context(|| "Library operation failed")?;
    
    Ok(())
}
```

### Conditional Error Handling

```rust
use anyhow::{Context, Result};

fn handle_library_errors_conditionally() -> Result<()> {
    match library_function() {
        Ok(result) => {
            // Handle success
            Ok(())
        }
        Err(LibraryError::Validation { message }) => {
            // Handle validation errors specifically
            println!("Validation failed: {}", message);
            prompt_user_for_correction()
                .context("Failed to get user input")
        }
        Err(LibraryError::Io(io_err)) if io_err.kind() == std::io::ErrorKind::NotFound => {
            // Handle file not found specifically
            create_default_file()
                .context("Failed to create default file")
        }
        Err(error) => {
            // Handle all other errors generically
            Err(error).context("Library operation failed")
        }
    }
}
```

## Transparent Error Forwarding

### Library with Transparent Errors

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ApiError {
    #[error("Authentication failed")]
    AuthenticationFailed,
    
    #[error("Rate limit exceeded")]
    RateLimit,
    
    #[error("Invalid request: {message}")]
    InvalidRequest { message: String },
    
    // Forward network errors transparently
    #[error(transparent)]
    Network(#[from] reqwest::Error),
    
    // Forward unexpected errors transparently
    #[error(transparent)]
    Unexpected(#[from] anyhow::Error),
}

impl ApiError {
    pub fn is_retryable(&self) -> bool {
        match self {
            ApiError::RateLimit => true,
            ApiError::Network(req_err) => req_err.is_timeout() || req_err.is_connect(),
            _ => false,
        }
    }
}
```

### Application Using Transparent Errors

```rust
use anyhow::{Context, Result};

async fn make_api_calls() -> Result<()> {
    let mut retry_count = 0;
    const MAX_RETRIES: u32 = 3;
    
    loop {
        match api_call().await {
            Ok(response) => {
                process_response(response)?;
                break;
            }
            Err(error) => {
                // Check if the error is retryable
                if let Some(api_error) = error.downcast_ref::<ApiError>() {
                    if api_error.is_retryable() && retry_count < MAX_RETRIES {
                        retry_count += 1;
                        println!("Retrying API call ({}/{})", retry_count, MAX_RETRIES);
                        tokio::time::sleep(std::time::Duration::from_secs(1)).await;
                        continue;
                    }
                }
                
                // Not retryable or max retries exceeded
                return Err(error).context("API call failed");
            }
        }
    }
    
    Ok(())
}
```

## Mixed Error Handling

### Layered Error Architecture

```rust
use thiserror::Error;
use anyhow::{Context, Result};

// Domain-specific errors (thiserror)
#[derive(Error, Debug)]
pub enum UserError {
    #[error("User not found: {id}")]
    NotFound { id: u64 },
    
    #[error("Invalid email: {email}")]
    InvalidEmail { email: String },
    
    #[error("Permission denied")]
    PermissionDenied,
}

#[derive(Error, Debug)]
pub enum DatabaseError {
    #[error("Connection failed")]
    ConnectionFailed(#[from] sqlx::Error),
    
    #[error("Query timeout")]
    Timeout,
    
    #[error("Constraint violation")]
    ConstraintViolation,
}

// High-level application errors (thiserror + anyhow)
#[derive(Error, Debug)]
pub enum AppError {
    #[error("User operation failed")]
    User(#[from] UserError),
    
    #[error("Database operation failed")]
    Database(#[from] DatabaseError),
    
    #[error("Configuration error")]
    Config(#[from] ConfigError),
    
    // Catch-all for unexpected errors
    #[error(transparent)]
    Unexpected(#[from] anyhow::Error),
}

// Service layer using mixed approach
struct UserService {
    db: Database,
}

impl UserService {
    // Returns specific error type
    pub fn get_user(&self, id: u64) -> Result<User, UserError> {
        self.db.find_user(id)
            .map_err(|_| UserError::NotFound { id })?;
        // Implementation
        Ok(User::default())
    }
    
    // Uses anyhow for complex operations
    pub fn create_user_with_profile(&self, data: CreateUserData) -> Result<User> {
        // Validate input
        self.validate_user_data(&data)
            .context("User data validation failed")?;
        
        // Create user (specific error handling)
        let user = self.create_user(data.user_info)
            .map_err(AppError::User)
            .context("Failed to create user")?;
        
        // Create profile (generic error handling)
        self.create_user_profile(user.id, data.profile_info)
            .context("Failed to create user profile")?;
        
        // Send welcome email (best effort)
        if let Err(e) = self.send_welcome_email(&user) {
            log::warn!("Failed to send welcome email: {}", e);
        }
        
        Ok(user)
    }
}
```

### Error Boundary Pattern

```rust
use anyhow::{Context, Result};

// Define error boundaries in your application
trait ErrorBoundary {
    type Input;
    type Output;
    type Error;
    
    fn execute(&self, input: Self::Input) -> Result<Self::Output, Self::Error>;
}

// Service boundary: converts all errors to service errors
struct UserServiceBoundary;

impl ErrorBoundary for UserServiceBoundary {
    type Input = CreateUserRequest;
    type Output = User;
    type Error = UserServiceError;
    
    fn execute(&self, request: Self::Input) -> Result<Self::Output, Self::Error> {
        // All internal errors are converted to UserServiceError
        let user_service = UserService::new();
        
        user_service.create_user(request)
            .map_err(|e| match e.downcast_ref::<UserError>() {
                Some(UserError::InvalidEmail { .. }) => {
                    UserServiceError::ValidationFailed { 
                        field: "email".to_string() 
                    }
                }
                _ => UserServiceError::InternalError { 
                    message: e.to_string() 
                }
            })
    }
}

// Application boundary: converts all errors to anyhow
struct ApplicationBoundary;

impl ApplicationBoundary {
    fn handle_request(&self, request: Request) -> Result<Response> {
        match request.endpoint.as_str() {
            "/users" => {
                let boundary = UserServiceBoundary;
                let user = boundary.execute(request.into())
                    .context("User service failed")?;
                Ok(Response::from(user))
            }
            _ => anyhow::bail!("Unknown endpoint: {}", request.endpoint),
        }
    }
}
```

## Testing Integration

### Testing Library Errors in Applications

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::Result;
    
    #[test]
    fn test_library_error_conversion() {
        let library_error = LibraryError::Validation {
            message: "Invalid input".to_string(),
        };
        
        let anyhow_error: anyhow::Error = library_error.into();
        assert_eq!(anyhow_error.to_string(), "Validation error: Invalid input");
    }
    
    #[test]
    fn test_error_chain_preservation() -> Result<()> {
        let result = function_that_uses_library();
        
        match result {
            Err(error) => {
                // Check that the error chain is preserved
                let mut found_library_error = false;
                for cause in error.chain() {
                    if cause.downcast_ref::<LibraryError>().is_some() {
                        found_library_error = true;
                        break;
                    }
                }
                assert!(found_library_error, "Library error not found in chain");
            }
            Ok(_) => panic!("Expected error"),
        }
        
        Ok(())
    }
    
    #[test]
    fn test_specific_error_handling() {
        let result = handle_library_errors_conditionally();
        
        // Test that specific errors are handled appropriately
        match result {
            Err(error) => {
                if let Some(library_error) = error.downcast_ref::<LibraryError>() {
                    match library_error {
                        LibraryError::Validation { .. } => {
                            // Should have been handled
                            panic!("Validation error should have been handled");
                        }
                        _ => {
                            // Other errors are expected to propagate
                        }
                    }
                }
            }
            Ok(_) => {
                // Success is also valid
            }
        }
    }
}
```

### Mock Testing with Error Integration

```rust
#[cfg(test)]
mod integration_tests {
    use super::*;
    use mockall::predicate::*;
    use mockall::mock;
    
    mock! {
        Database {}
        
        impl Database {
            fn find_user(&self, id: u64) -> Result<User, DatabaseError>;
            fn create_user(&self, data: UserData) -> Result<User, DatabaseError>;
        }
    }
    
    #[tokio::test]
    async fn test_error_propagation() {
        let mut mock_db = MockDatabase::new();
        
        // Setup mock to return database error
        mock_db
            .expect_find_user()
            .with(eq(123))
            .returning(|_| Err(DatabaseError::ConnectionFailed(
                sqlx::Error::PoolClosed
            )));
        
        let service = UserService::new(mock_db);
        
        // Test that database error propagates correctly
        let result = service.get_user_with_context(123).await;
        
        assert!(result.is_err());
        let error = result.unwrap_err();
        
        // Check error chain
        assert!(error.chain().any(|e| e.downcast_ref::<DatabaseError>().is_some()));
        assert!(error.to_string().contains("Failed to get user"));
    }
}
```

## Performance Considerations

### Efficient Error Conversion

```rust
use std::sync::Arc;

// For frequently converted errors, consider using Arc
#[derive(Error, Debug, Clone)]
pub enum SharedError {
    #[error("Common error: {message}")]
    Common { message: Arc<str> },
    
    #[error("Rare error: {message}")]
    Rare { message: String },
}

// Efficient conversion that avoids cloning large data
impl From<SharedError> for anyhow::Error {
    fn from(error: SharedError) -> Self {
        match error {
            SharedError::Common { .. } => {
                // Arc allows cheap cloning
                anyhow::Error::new(error)
            }
            SharedError::Rare { .. } => {
                // Move the error to avoid cloning
                anyhow::Error::new(error)
            }
        }
    }
}
```

### Lazy Error Context

```rust
use anyhow::{Context, Result};

// Efficient context that's only computed when needed
fn efficient_error_handling(data: &LargeData) -> Result<()> {
    library_function(data)
        .with_context(|| {
            // This closure is only called if an error occurs
            format!("Failed to process data with {} items", data.items.len())
        })?;
    
    Ok(())
}

// Avoid eager context computation
fn inefficient_error_handling(data: &LargeData) -> Result<()> {
    let context = format!("Failed to process data with {} items", data.items.len());
    
    library_function(data)
        .context(context)?; // String is created even on success
    
    Ok(())
}
```

## Real-World Examples

### Web API with Mixed Error Handling

```rust
use axum::{extract::Path, http::StatusCode, response::Json, routing::get, Router};
use serde_json::json;
use thiserror::Error;
use anyhow::{Context, Result};

// Domain errors (thiserror)
#[derive(Error, Debug)]
pub enum UserError {
    #[error("User not found: {id}")]
    NotFound { id: u64 },
    
    #[error("Invalid email format: {email}")]
    InvalidEmail { email: String },
    
    #[error("Username already taken: {username}")]
    UsernameTaken { username: String },
}

#[derive(Error, Debug)]
pub enum DatabaseError {
    #[error("Database connection failed")]
    ConnectionFailed(#[from] sqlx::Error),
    
    #[error("Query timeout")]
    Timeout,
}

// Application error wrapper
#[derive(Error, Debug)]
pub enum ApiError {
    #[error("User error")]
    User(#[from] UserError),
    
    #[error("Database error")]
    Database(#[from] DatabaseError),
    
    #[error(transparent)]
    Internal(#[from] anyhow::Error),
}

// HTTP error conversion
impl axum::response::IntoResponse for ApiError {
    fn into_response(self) -> axum::response::Response {
        let (status, error_code, message) = match &self {
            ApiError::User(UserError::NotFound { .. }) => {
                (StatusCode::NOT_FOUND, "USER_NOT_FOUND", self.to_string())
            }
            ApiError::User(UserError::InvalidEmail { .. }) => {
                (StatusCode::BAD_REQUEST, "INVALID_EMAIL", self.to_string())
            }
            ApiError::User(UserError::UsernameTaken { .. }) => {
                (StatusCode::CONFLICT, "USERNAME_TAKEN", self.to_string())
            }
            ApiError::Database(_) => {
                (StatusCode::SERVICE_UNAVAILABLE, "DATABASE_ERROR", "Service temporarily unavailable".to_string())
            }
            ApiError::Internal(_) => {
                (StatusCode::INTERNAL_SERVER_ERROR, "INTERNAL_ERROR", "Internal server error".to_string())
            }
        };
        
        // Log internal errors
        if matches!(self, ApiError::Database(_) | ApiError::Internal(_)) {
            tracing::error!(error = %self, "API error occurred");
        }
        
        let body = json!({
            "error": error_code,
            "message": message
        });
        
        (status, Json(body)).into_response()
    }
}

// Handler using mixed error handling
async fn get_user(Path(user_id): Path<u64>) -> Result<Json<serde_json::Value>, ApiError> {
    // Use specific error handling for domain logic
    let user = user_service::get_user(user_id)?; // UserError -> ApiError
    
    // Use anyhow for complex operations
    let profile = profile_service::get_profile(user_id)
        .await
        .context("Failed to fetch user profile")?; // anyhow::Error -> ApiError
    
    let permissions = permission_service::get_permissions(user_id)
        .await
        .context("Failed to fetch user permissions")?;
    
    Ok(Json(json!({
        "user": user,
        "profile": profile,
        "permissions": permissions
    })))
}

// Service layer
mod user_service {
    use super::*;
    
    pub fn get_user(id: u64) -> Result<User, UserError> {
        // Domain-specific error handling
        if id == 0 {
            return Err(UserError::NotFound { id });
        }
        
        // Simulate database call
        Ok(User { id, name: "John".to_string() })
    }
}

mod profile_service {
    use super::*;
    
    pub async fn get_profile(user_id: u64) -> Result<Profile> {
        // Generic error handling with context
        let profile_data = fetch_profile_data(user_id)
            .await
            .context("Failed to fetch profile data from external service")?;
        
        parse_profile_data(profile_data)
            .context("Failed to parse profile data")
    }
}
```

### CLI Application with Library Integration

```rust
use anyhow::{Context, Result};
use clap::Parser;
use thiserror::Error;

// Library errors (thiserror)
#[derive(Error, Debug)]
pub enum ProcessingError {
    #[error("Invalid input format: {message}")]
    InvalidFormat { message: String },
    
    #[error("Processing failed at step {step}: {reason}")]
    ProcessingFailed { step: String, reason: String },
    
    #[error("Output error")]
    Output(#[from] std::io::Error),
}

// CLI application (anyhow)
#[derive(Parser)]
struct Args {
    #[arg(short, long)]
    input: String,
    
    #[arg(short, long)]
    output: String,
    
    #[arg(long)]
    format: Option<String>,
}

fn main() -> Result<()> {
    let args = Args::parse();
    
    // Load input with context
    let input_data = std::fs::read_to_string(&args.input)
        .with_context(|| format!("Failed to read input file: {}", args.input))?;
    
    // Process with library (specific error handling)
    let processed_data = match process_data(&input_data, args.format.as_deref()) {
        Ok(data) => data,
        Err(ProcessingError::InvalidFormat { message }) => {
            eprintln!("❌ Invalid input format: {}", message);
            eprintln!("💡 Try using --format to specify the input format");
            std::process::exit(1);
        }
        Err(error) => {
            return Err(error).context("Data processing failed");
        }
    };
    
    // Write output with context
    std::fs::write(&args.output, processed_data)
        .with_context(|| format!("Failed to write output file: {}", args.output))?;
    
    println!("✅ Processing completed successfully");
    Ok(())
}

// Library function
fn process_data(input: &str, format: Option<&str>) -> Result<String, ProcessingError> {
    if input.is_empty() {
        return Err(ProcessingError::InvalidFormat {
            message: "Input cannot be empty".to_string(),
        });
    }
    
    let format = format.unwrap_or("auto");
    
    match format {
        "json" => process_json(input),
        "csv" => process_csv(input),
        "auto" => detect_and_process(input),
        _ => Err(ProcessingError::InvalidFormat {
            message: format!("Unsupported format: {}", format),
        }),
    }
}

fn process_json(input: &str) -> Result<String, ProcessingError> {
    // Simulate JSON processing
    if !input.trim_start().starts_with('{') {
        return Err(ProcessingError::InvalidFormat {
            message: "Expected JSON object starting with '{'".to_string(),
        });
    }
    
    Ok(format!("Processed JSON: {} items", input.lines().count()))
}

fn process_csv(input: &str) -> Result<String, ProcessingError> {
    // Simulate CSV processing
    let lines: Vec<&str> = input.lines().collect();
    if lines.is_empty() {
        return Err(ProcessingError::ProcessingFailed {
            step: "CSV parsing".to_string(),
            reason: "No data lines found".to_string(),
        });
    }
    
    Ok(format!("Processed CSV: {} rows", lines.len()))
}

fn detect_and_process(input: &str) -> Result<String, ProcessingError> {
    if input.trim_start().starts_with('{') {
        process_json(input)
    } else if input.contains(',') {
        process_csv(input)
    } else {
        Err(ProcessingError::InvalidFormat {
            message: "Could not detect format. Please specify with --format".to_string(),
        })
    }
}
```

---

*The combination of `thiserror` and `anyhow` provides a powerful, flexible error handling strategy. Use `thiserror` for precise, typed errors in your libraries and `anyhow` for ergonomic error handling in your applications.*