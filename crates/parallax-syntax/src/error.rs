use thiserror::Error;
use miette::{Diagnostic, SourceSpan};

#[derive(Debug, Clone, Error, Diagnostic, PartialEq, Eq)]
pub enum SyntaxError {
    #[error("Parser initialization error: {0}")]
    #[diagnostic(code(parallax_syntax::parser_init))]
    ParserInitError(String),
    
    #[error("Parse error: {message}")]
    #[diagnostic(code(parallax_syntax::parse_error))]
    ParseError {
        message: String,
        #[label("error occurred here")]
        span: Option<SourceSpan>,
    },
    
    #[error("Node error in {node_type}: {message}")]
    #[diagnostic(code(parallax_syntax::node_error))]
    NodeError {
        message: String,
        #[label("in this node")]
        span: Option<SourceSpan>,
        node_type: String,
    },
    
    #[error("AST error: {message}")]
    #[diagnostic(code(parallax_syntax::ast_error))]
    AstError {
        message: String,
        #[label("error occurred here")]
        span: Option<SourceSpan>,
    },
    
    #[error("Syntax error: {message}")]
    #[diagnostic(code(parallax_syntax::syntax_error))]
    SyntaxError {
        message: String,
        #[label("error occurred here")]
        span: Option<SourceSpan>,
        #[help("expected: {expected:?}")]
        expected: Option<String>,
        #[help("found: {found:?}")]
        found: Option<String>,
    },
}