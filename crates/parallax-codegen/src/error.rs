use thiserror::Error;
use parallax_native::NativeError;

/// Errors that can occur during the code generation orchestration phase.
#[derive(Error, Debug)]
pub enum CodegenError {
    #[error("Error from native backend: {0}")]
    NativeBackend(#[from] NativeError),

    // TODO: Add other error types specific to the orchestration logic
    #[error("Orchestration feature not yet implemented: {0}")]
    Unimplemented(String),
} 