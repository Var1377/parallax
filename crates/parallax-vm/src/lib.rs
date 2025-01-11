pub use crate::compile::compile;
use crate::graph::GlobalNetwork;

pub mod graph;
pub mod compile;
pub mod config;

use thiserror::Error;

/// VM runtime errors
#[derive(Debug, Error, PartialEq)]
pub enum VMError {
    #[error("Runtime error: {message}")]
    Runtime { message: String },
    
    #[error("Out of memory")]
    OutOfMemory,
    
    #[error("Invalid operation: {message}")]
    InvalidOperation { message: String },
}

impl From<&str> for VMError {
    fn from(s: &str) -> Self {
        VMError::Runtime { message: s.to_string() }
    }
}

/// Result type for VM operations
pub type VMResult<T> = std::result::Result<T, VMError>;

pub fn run(_net: &GlobalNetwork) -> VMResult<()> {
    todo!("Implement VM runtime")
}