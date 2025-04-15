use miette::Diagnostic;
use parallax_source::FrameError;
use thiserror::Error;
use parallax_codegen::CodegenError;

/// Database-related errors that can occur during compiler operation.
#[derive(Debug, Error, Diagnostic)]
pub enum DatabaseError {
    #[error("Frame error: {0}")]
    #[diagnostic(code("DB-FRAME-001"))]
    FrameError(#[from] FrameError),

    #[error("Code generation error: {0}")]
    #[diagnostic(code("DB-CODEGEN-001"))]
    CodegenError(#[from] CodegenError),

    #[error("Entry point function 'main' not found in the project.")]
    #[diagnostic(code("DB-ENTRY-001"), help("Define a function `fn main() -> i64 {{ ... }}` or `fn main() -> () {{ ... }}`"))]
    MainNotFound,

    #[error("Entry point function 'main' has incorrect signature.")]
    #[diagnostic(
        code("DB-ENTRY-002"),
        help("The 'main' function must have the signature `fn() -> i64` or `fn() -> ()`")
    )]
    MainIncorrectSignature,

    // Add other potential database errors here
}

/// Result type for database operations.
pub type DatabaseResult<T> = Result<T, DatabaseError>; 