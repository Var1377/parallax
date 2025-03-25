use thiserror::Error;
use crate::context::Ty;
use parallax_lang::ast::Span;
use parallax_resolve::error::ResolveError;

#[derive(Debug, Error, Clone, Eq, PartialEq)]
pub enum TypeError {
    #[error("type mismatch: expected {expected}, found {found}")]
    Mismatch {
        expected: Ty,
        found: Ty,
        span: Span,
    },

    #[error("cannot unify types {first} and {second}")]
    UnificationError {
        first: Ty,
        second: Ty,
        span: Span,
    },

    #[error("trait {trait_} not implemented for type {ty}")]
    TraitNotImplemented {
        trait_: String,
        ty: Ty,
        span: Span,
    },

    #[error("cannot resolve type {ty}")]
    UnresolvedType {
        ty: String,
        span: Span,
    },

    #[error("recursive type detected")]
    RecursiveType {
        ty: Ty,
        span: Span,
    },

    #[error("name resolution error: {0}")]
    ResolveError(#[from] ResolveError),

    #[error("internal error: {0}")]
    Internal(String),
}

impl TypeError {
    pub fn span(&self) -> Option<Span> {
        match self {
            TypeError::Mismatch { span, .. } => Some(*span),
            TypeError::UnificationError { span, .. } => Some(*span),
            TypeError::TraitNotImplemented { span, .. } => Some(*span),
            TypeError::UnresolvedType { span, .. } => Some(*span),
            TypeError::RecursiveType { span, .. } => Some(*span),
            TypeError::ResolveError(_) => None,
            TypeError::Internal(_) => None,
        }
    }
}