use std::error::Error;
use std::fmt;
use crate::ast::common::Span;

#[derive(Debug)]
pub enum ParallaxError {
    ParserInitError(String),
    ParseError {
        message: String,
        span: Option<Span>,
    },
    NodeError {
        message: String,
        span: Option<Span>,
        node_type: String,
    },
    AstError {
        message: String,
        span: Option<Span>,
    },
    SyntaxError {
        message: String,
        span: Option<Span>,
        expected: Option<String>,
        found: Option<String>,
    },
}

impl fmt::Display for ParallaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParallaxError::ParserInitError(msg) => write!(f, "Parser initialization error: {}", msg),
            ParallaxError::ParseError { message, span } => {
                if let Some(span) = span {
                    write!(f, "Parse error at {}:{}: {}", span.start, span.end, message)
                } else {
                    write!(f, "Parse error: {}", message)
                }
            },
            ParallaxError::NodeError { message, span, node_type } => {
                if let Some(span) = span {
                    write!(f, "Node error in {} at {}:{}: {}", node_type, span.start, span.end, message)
                } else {
                    write!(f, "Node error in {}: {}", node_type, message)
                }
            },
            ParallaxError::AstError { message, span } => {
                if let Some(span) = span {
                    write!(f, "AST error at {}:{}: {}", span.start, span.end, message)
                } else {
                    write!(f, "AST error: {}", message)
                }
            },
            ParallaxError::SyntaxError { message, span, expected, found } => {
                let mut error = if let Some(span) = span {
                    format!("Syntax error at {}:{}: {}", span.start, span.end, message)
                } else {
                    format!("Syntax error: {}", message)
                };
                
                if let Some(expected) = expected {
                    error.push_str(&format!("\nExpected: {}", expected));
                }
                if let Some(found) = found {
                    error.push_str(&format!("\nFound: {}", found));
                }
                write!(f, "{}", error)
            }
        }
    }
}

impl Error for ParallaxError {} 