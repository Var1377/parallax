//! Error types for MIR.

use std::fmt;

/// Error type for MIR-related operations
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MirError {
    /// Error during lowering from HIR to MIR
    LoweringError(String),
    /// Error during monomorphization
    MonomorphizationError(String),
    /// Error during optimization
    OptimizationError(String),
    /// Error during validation
    ValidationError(String),
    /// Invalid HIR
    InvalidHir(String),
}

impl fmt::Display for MirError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MirError::LoweringError(msg) => write!(f, "Lowering error: {}", msg),
            MirError::MonomorphizationError(msg) => write!(f, "Monomorphization error: {}", msg),
            MirError::OptimizationError(msg) => write!(f, "Optimization error: {}", msg),
            MirError::ValidationError(msg) => write!(f, "Validation error: {}", msg),
            MirError::InvalidHir(msg) => write!(f, "Invalid HIR: {}", msg),
        }
    }
}

impl std::error::Error for MirError {}

impl MirError {
    /// Create a new lowering error
    pub fn lowering_error(msg: impl Into<String>) -> Self {
        MirError::LoweringError(msg.into())
    }

    /// Create a new monomorphization error
    pub fn monomorphization_error(msg: impl Into<String>) -> Self {
        MirError::MonomorphizationError(msg.into())
    }

    /// Create a new optimization error
    pub fn optimization_error(msg: impl Into<String>) -> Self {
        MirError::OptimizationError(msg.into())
    }

    /// Create a new validation error
    pub fn validation_error(msg: impl Into<String>) -> Self {
        MirError::ValidationError(msg.into())
    }

    /// Create a new invalid HIR error
    pub fn invalid_hir(msg: impl Into<String>) -> Self {
        MirError::InvalidHir(msg.into())
    }
} 