use std::path::PathBuf;

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;
use parallax_source::FrameError;
use parallax_db::DatabaseError;

/// CLI-specific error type that provides rich diagnostics
#[derive(Debug, Error, Diagnostic)]
pub enum CliError {
    // #[error("IR error: {message}")]
    // #[diagnostic(code(parallax::cli::ir_error))]
    // IRError {
    //     #[source_code]
    //     src: String,
    //     #[label("{message}")]
    //     span: SourceSpan,
    //     message: String,
    //     #[source]
    //     source: IRError,
    // },

    // #[error("Runtime error: {message}")]
    // #[diagnostic(code(parallax::cli::runtime_error))]
    // RuntimeError {
    //     message: String,
    //     #[source]
    //     source: VMError,
    // },

    #[error("Frame not found")]
    #[diagnostic(
        code(parallax::cli::frame_not_found),
        help("Ensure you are running `plx` inside a Parallax project directory containing a 'frame.toml' file, or that the specified path is correct.")
    )]
    FrameNotFound {
        searched_path: PathBuf,
    },

    #[error("Failed {operation} on {path}")]
    #[diagnostic(code(parallax::cli::io_error))]
    IoError {
        path: PathBuf,
        operation: String,
        #[source]
        source: std::io::Error,
    },

    #[error("Check failed")]
    #[diagnostic(
        code(parallax::cli::check_failed),
        severity(Error),
        help("Review the diagnostics reported by the compiler.")
    )]
    CheckFailed,

    #[error("Build failed")]
    #[diagnostic(
        code(parallax::cli::build_failed),
        severity(Error),
        help("Build process encountered errors. Check previous diagnostics.")
    )]
    BuildFailed,

    #[error("Run failed")]
    #[diagnostic(
        code(parallax::cli::run_failed),
        severity(Error),
        help("Execution of the program failed. See output for details.")
    )]
    RunFailed,

    #[error("Internal CLI error: {0}")]
    #[diagnostic(code(parallax::cli::internal_error))]
    InternalError(String),

    #[error(transparent)]
    #[diagnostic(transparent)]
    FrameLoadError(#[from] FrameError),

    #[error("Compiler database error")]
    #[diagnostic(code(parallax::cli::db_error))]
    DbError { #[source] source: DatabaseError },

    #[error("Runtime error: {0}")]
    #[diagnostic(code(parallax::cli::runtime_error), help("Execution of the compiled code failed."))]
    RuntimeError(String),
}

/// Helper struct to provide context for error conversion
#[derive(Debug, Clone, Copy)]
pub struct ErrorContext<'a> {
    pub source: &'a str,
}

/// Convert IO errors with context
pub fn convert_io_error(error: std::io::Error, path: PathBuf) -> CliError {
    CliError::IoError {
        path,
        operation: String::new(),
        source: error,
    }
}

impl From<DatabaseError> for CliError {
    fn from(db_err: DatabaseError) -> Self {
        CliError::DbError { source: db_err }
    }
}