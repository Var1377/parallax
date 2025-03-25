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

    // New field access errors
    #[error("field '{field_name}' not found in type '{ty:?}'")]
    FieldNotFound {
        field_name: String,
        ty: Ty,
        span: Span,
    },
    #[error("invalid tuple field '{field_name}': expected numeric index")]
    InvalidTupleField {
        field_name: String,
        span: Span,
    },
    #[error("field access not supported on type '{ty:?}'")]
    FieldAccessOnNonStruct {
        ty: Ty,
        span: Span,
    },

    // Struct instantiation errors
    #[error("invalid base struct: expected '{struct_name}', got '{base_ty:?}'")]
    InvalidBaseStruct {
        base_ty: Ty,
        struct_name: String,
        span: Span,
    },
    #[error("missing field '{field_name}' in struct '{struct_name}'")]
    MissingField {
        field_name: String,
        struct_name: String,
        span: Span,
    },
    #[error("extra field '{field_name}' in struct '{struct_name}'")]
    ExtraField {
        field_name: String,
        struct_name: String,
        span: Span,
    },
    #[error("type mismatch for field '{field_name}': expected '{expected:?}', got '{actual:?}'")]
    FieldTypeMismatch {
        field_name: String,
        expected: Ty,
        actual: Ty,
        span: Span,
    },

    // Pattern matching errors
    #[error("tuple pattern length mismatch: expected {expected}, got {actual}")]
    TuplePatternLengthMismatch {
        expected: usize,
        actual: usize,
        span: Span,
    },
    #[error("Pattern does not match expected type: {expected}, found {pattern_desc}")]
    PatternTypeMismatch {
        expected: Ty,
        pattern_desc: String,
        span: Span,
    },

    /// Error indicating argument count mismatch in function call
    #[error("Function call has wrong number of arguments: expected {expected}, found {actual}")]
    ArgumentCountMismatch {
        expected: usize,
        actual: usize,
        span: Span,
    },

    #[error("unknown type '{name}'")]
    UnknownType {
        name: String,
        span: Span,
    },
    
    #[error("unknown trait '{name}'")]
    UnknownTrait {
        name: String,
        span: Span,
    },
    
    #[error("unknown method '{name}' on type '{on_type:?}'")]
    UnknownMethod {
        name: String,
        on_type: Ty,
        span: Span,
    },
    
    #[error("no matching method '{name}' found for type '{on_type:?}'")]
    NoMatchingMethod {
        name: String,
        on_type: Ty,
        span: Span,
    },
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
            TypeError::FieldNotFound { span, .. } => Some(*span),
            TypeError::InvalidTupleField { span, .. } => Some(*span),
            TypeError::FieldAccessOnNonStruct { span, .. } => Some(*span),
            TypeError::InvalidBaseStruct { span, .. } => Some(*span),
            TypeError::MissingField { span, .. } => Some(*span),
            TypeError::ExtraField { span, .. } => Some(*span),
            TypeError::FieldTypeMismatch { span, .. } => Some(*span),
            TypeError::TuplePatternLengthMismatch { span, .. } => Some(*span),
            TypeError::PatternTypeMismatch { span, .. } => Some(*span),
            TypeError::ArgumentCountMismatch { span, .. } => Some(*span),
            TypeError::UnknownType { span, .. } => Some(*span),
            TypeError::UnknownTrait { span, .. } => Some(*span),
            TypeError::UnknownMethod { span, .. } => Some(*span),
            TypeError::NoMatchingMethod { span, .. } => Some(*span),
        }
    }
}