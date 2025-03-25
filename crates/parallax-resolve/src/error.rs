//! Error types for the name resolution phase.

use miette::{Diagnostic, NamedSource, SourceSpan};
use parallax_lang::ast::common::Span;
use std::path::PathBuf;
use thiserror::Error;

/// Convert a parallax Span to a miette SourceSpan
fn span_to_source_span(span: &Span) -> SourceSpan {
    (span.start as usize, (span.end - span.start) as usize).into()
}

/// The error type for name resolution errors.
#[derive(Debug, Error, Diagnostic)]
pub enum ResolveError {
    /// Error for when a name can't be found in scope
    #[error("undefined name `{name}`")]
    #[diagnostic(code(parallax::resolve::undefined_name))]
    UndefinedName {
        /// The name that couldn't be found
        name: String,
        /// The source code where the error occurred
        #[source_code]
        src: NamedSource,
        /// The span of the error
        #[label("unknown identifier")]
        span: SourceSpan,
    },

    /// Error for when a reference is ambiguous
    #[error("ambiguous reference to `{name}`")]
    #[diagnostic(code(parallax::resolve::ambiguous_reference))]
    AmbiguousReference {
        /// The ambiguous name
        name: String,
        /// The possible candidates
        candidates: Vec<String>,
        /// The source code where the error occurred
        #[source_code]
        src: NamedSource,
        /// The span of the error
        #[label("ambiguous reference")]
        span: SourceSpan,
    },

    /// Error for duplicate definitions
    #[error("duplicate definition of `{name}`")]
    #[diagnostic(code(parallax::resolve::duplicate_definition))]
    DuplicateDefinition {
        /// The name that was defined multiple times
        name: String,
        /// The source code where the error occurred
        #[source_code]
        src: NamedSource,
        /// The span of the error
        #[label("duplicate definition")]
        span: SourceSpan,
        /// The span of the first definition
        #[label("first defined here")]
        first_def_span: SourceSpan,
    },

    /// Error for visibility violations
    #[error("cannot access private item `{name}`")]
    #[diagnostic(code(parallax::resolve::visibility_violation))]
    VisibilityViolation {
        /// The name that couldn't be accessed
        name: String,
        /// The source code where the error occurred
        #[source_code]
        src: NamedSource,
        /// The span of the error
        #[label("private item")]
        span: SourceSpan,
    },

    /// Error for cyclic dependencies
    #[error("cyclic dependency detected")]
    #[diagnostic(code(parallax::resolve::cyclic_dependency))]
    CyclicDependency {
        /// The source code where the error occurred
        #[source_code]
        src: NamedSource,
        /// The span of the error
        #[label("cyclic dependency")]
        span: SourceSpan,
    },

    /// Error for import errors
    #[error("cannot import `{name}`")]
    #[diagnostic(code(parallax::resolve::import_error))]
    ImportError {
        /// The name that couldn't be imported
        name: String,
        /// The source code where the error occurred
        #[source_code]
        src: NamedSource,
        /// The span of the error
        #[label("import error")]
        span: SourceSpan,
        /// The reason for the error
        reason: String,
    },

    /// Generic resolution error
    #[error("{message}")]
    #[diagnostic(code(parallax::resolve::generic_error))]
    GenericError {
        /// The error message
        message: String,
        /// The source code where the error occurred
        #[source_code]
        src: NamedSource,
        /// The span of the error
        #[label("error")]
        span: SourceSpan,
    },
}

impl Clone for ResolveError {
    fn clone(&self) -> Self {
        match self {
            Self::UndefinedName { name, .. } => {
                // We can't clone NamedSource, so we create a new error with empty source info
                // This clone implementation is only for Salsa's equality checks and not for error reporting
                ResolveError::UndefinedName {
                    name: name.clone(),
                    src: NamedSource::new("<cloned>", String::new()),
                    span: SourceSpan::from((0, 0)),
                }
            },
            Self::AmbiguousReference { name, candidates, .. } => {
                ResolveError::AmbiguousReference {
                    name: name.clone(),
                    candidates: candidates.clone(),
                    src: NamedSource::new("<cloned>", String::new()),
                    span: SourceSpan::from((0, 0)),
                }
            },
            Self::DuplicateDefinition { name, .. } => {
                ResolveError::DuplicateDefinition {
                    name: name.clone(),
                    src: NamedSource::new("<cloned>", String::new()),
                    span: SourceSpan::from((0, 0)),
                    first_def_span: SourceSpan::from((0, 0)),
                }
            },
            Self::VisibilityViolation { name, .. } => {
                ResolveError::VisibilityViolation {
                    name: name.clone(),
                    src: NamedSource::new("<cloned>", String::new()),
                    span: SourceSpan::from((0, 0)),
                }
            },
            Self::CyclicDependency { .. } => {
                ResolveError::CyclicDependency {
                    src: NamedSource::new("<cloned>", String::new()),
                    span: SourceSpan::from((0, 0)),
                }
            },
            Self::ImportError { name, reason, .. } => {
                ResolveError::ImportError {
                    name: name.clone(),
                    reason: reason.clone(),
                    src: NamedSource::new("<cloned>", String::new()),
                    span: SourceSpan::from((0, 0)),
                }
            },
            Self::GenericError { message, .. } => {
                ResolveError::GenericError {
                    message: message.clone(),
                    src: NamedSource::new("<cloned>", String::new()),
                    span: SourceSpan::from((0, 0)),
                }
            },
        }
    }
}

impl PartialEq for ResolveError {
    fn eq(&self, other: &Self) -> bool {
        // Compare error variants by their kind and basic fields, ignoring source code and spans which are diagnostic details
        match (self, other) {
            (Self::UndefinedName { name: n1, .. }, Self::UndefinedName { name: n2, .. }) => {
                n1 == n2
            },
            (Self::AmbiguousReference { name: n1, candidates: c1, .. }, Self::AmbiguousReference { name: n2, candidates: c2, .. }) => {
                n1 == n2 && c1 == c2
            },
            (Self::DuplicateDefinition { name: n1, .. }, Self::DuplicateDefinition { name: n2, .. }) => {
                n1 == n2
            },
            (Self::VisibilityViolation { name: n1, .. }, Self::VisibilityViolation { name: n2, .. }) => {
                n1 == n2
            },
            (Self::CyclicDependency { .. }, Self::CyclicDependency { .. }) => {
                true // All cyclic dependencies are considered equal
            },
            (Self::ImportError { name: n1, reason: r1, .. }, Self::ImportError { name: n2, reason: r2, .. }) => {
                n1 == n2 && r1 == r2
            },
            (Self::GenericError { message: m1, .. }, Self::GenericError { message: m2, .. }) => {
                m1 == m2
            },
            _ => false,
        }
    }
}

impl Eq for ResolveError {}

impl ResolveError {
    /// Create a new undefined name error
    pub fn undefined_name(
        name: String,
        span: Span,
        source_path: PathBuf,
        source_code: String,
    ) -> Self {
        ResolveError::UndefinedName {
            name,
            src: NamedSource::new(
                source_path.to_string_lossy().to_string(),
                source_code,
            ),
            span: span_to_source_span(&span),
        }
    }

    /// Create a new ambiguous reference error
    pub fn ambiguous_reference(
        name: String,
        candidates: Vec<String>,
        span: Span,
        source_path: PathBuf,
        source_code: String,
    ) -> Self {
        ResolveError::AmbiguousReference {
            name,
            candidates,
            src: NamedSource::new(
                source_path.to_string_lossy().to_string(),
                source_code,
            ),
            span: span_to_source_span(&span),
        }
    }

    /// Create a new duplicate definition error
    pub fn duplicate_definition(
        name: String,
        span: Span,
        first_def_span: Span,
        source_path: PathBuf,
        source_code: String,
    ) -> Self {
        ResolveError::DuplicateDefinition {
            name,
            src: NamedSource::new(
                source_path.to_string_lossy().to_string(),
                source_code,
            ),
            span: span_to_source_span(&span),
            first_def_span: span_to_source_span(&first_def_span),
        }
    }

    /// Create a new visibility violation error
    pub fn visibility_violation(
        name: String,
        span: Span,
        source_path: PathBuf,
        source_code: String,
    ) -> Self {
        ResolveError::VisibilityViolation {
            name,
            src: NamedSource::new(
                source_path.to_string_lossy().to_string(),
                source_code,
            ),
            span: span_to_source_span(&span),
        }
    }

    /// Create a new cyclic dependency error
    pub fn cyclic_dependency(
        span: Span,
        source_path: PathBuf,
        source_code: String,
    ) -> Self {
        ResolveError::CyclicDependency {
            src: NamedSource::new(
                source_path.to_string_lossy().to_string(),
                source_code,
            ),
            span: span_to_source_span(&span),
        }
    }

    /// Create a new import error
    pub fn import_error(
        name: String,
        reason: String,
        span: Span,
        source_path: PathBuf,
        source_code: String,
    ) -> Self {
        ResolveError::ImportError {
            name,
            reason,
            src: NamedSource::new(
                source_path.to_string_lossy().to_string(),
                source_code,
            ),
            span: span_to_source_span(&span),
        }
    }

    /// Create a new generic error
    pub fn generic_error(
        message: String,
        span: Span,
        source_path: PathBuf,
        source_code: String,
    ) -> Self {
        ResolveError::GenericError {
            message,
            src: NamedSource::new(
                source_path.to_string_lossy().to_string(),
                source_code,
            ),
            span: span_to_source_span(&span),
        }
    }
} 