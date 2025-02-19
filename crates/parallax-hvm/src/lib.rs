pub mod lexer;
pub mod parser;
pub mod ast;
mod compile;

use thiserror::Error;
pub use miette::SourceSpan;

/// IR-related errors
#[derive(Debug, Error)]
pub enum IRError {
    #[error("Lexer error at {span:?}: {message}")]
    Lexer { span: SourceSpan, message: String },
    
    #[error("Parser error at {span:?}: {message}")]
    Parser { span: SourceSpan, message: String },
    
    #[error("Invalid token at {span:?}: {message}")]
    InvalidToken { span: SourceSpan, message: String },
    
    #[error("Undefined reference at {span:?}: {name}")]
    UndefinedReference { span: SourceSpan, name: String },
}

/// Result type for IR operations
pub type IRResult<T> = std::result::Result<T, IRError>;
