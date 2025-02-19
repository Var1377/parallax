use std::path::PathBuf;

use miette::{Diagnostic, SourceSpan};
use parallax_hvm::IRError;
use parallax_vm::VMError;
use thiserror::Error;

/// CLI-specific error type that provides rich diagnostics
#[derive(Debug, Error, Diagnostic)]
pub enum CliError {
    #[error("IR error: {message}")]
    #[diagnostic(code(parallax::cli::ir_error))]
    IRError {
        #[source_code]
        src: String,
        #[label("{message}")]
        span: SourceSpan,
        message: String,
        #[source]
        source: IRError,
    },

    #[error("Failed to read file {path}")]
    #[diagnostic(code(parallax::cli::io_error))]
    IoError {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("Runtime error: {message}")]
    #[diagnostic(code(parallax::cli::runtime_error))]
    RuntimeError {
        message: String,
        #[source]
        source: VMError,
    },
}

/// Helper struct to provide context for error conversion
#[derive(Debug, Clone, Copy)]
pub struct ErrorContext<'a> {
    pub source: &'a str,
}

/// Convert IR errors with context
pub fn convert_ir_error(error: IRError, ctx: ErrorContext) -> CliError {
    let (span, message) = match &error {
        IRError::Lexer { span, message } => (span, message),
        IRError::Parser { span, message } => (span, message),
        IRError::InvalidToken { span, message } => (span, message),
        IRError::UndefinedReference { span, name } => (span, name),
    };

    CliError::IRError {
        src: ctx.source.to_string(),
        span: *span,
        message: message.clone(),
        source: error,
    }
}

impl From<VMError> for CliError {
    fn from(error: VMError) -> Self {
        CliError::RuntimeError {
            message: error.to_string(),
            source: error,
        }
    }
}

/// Convert IO errors with context
pub fn convert_io_error(error: std::io::Error, path: PathBuf) -> CliError {
    CliError::IoError {
        path,
        source: error,
    }
}