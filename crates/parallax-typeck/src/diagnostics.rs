use std::fmt;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};

use crate::{
    context::Ty,
    error::TypeError,
};

/// A type printer for formatting types in error messages
pub struct TypePrinter<'tcx> {
    indent: usize,
    _tcx: std::marker::PhantomData<&'tcx ()>,
}

impl<'tcx> TypePrinter<'tcx> {
    pub fn new() -> Self {
        Self {
            indent: 0,
            _tcx: std::marker::PhantomData,
        }
    }

    pub fn pretty_print(&self, ty: &Ty) -> String {
        match ty {
            Ty::Var(vid) => format!("?{}", vid.0),
            Ty::Concrete(c) => format!("{:?}", c),
            Ty::Function { params, ret } => {
                let params = params
                    .iter()
                    .map(|t| self.pretty_print(t))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({}) -> {}", params, self.pretty_print(ret))
            }
            Ty::Tuple(tys) => {
                let tys = tys
                    .iter()
                    .map(|t| self.pretty_print(t))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({})", tys)
            }
            Ty::Generic { ref name, ref bounds } => {
                let bounds_str = if bounds.is_empty() {
                    String::new()
                } else {
                    format!(": {}", bounds.join(" + "))
                };
                format!("{}{}", name, bounds_str)
            }
            Ty::Error => "<error>".to_string(),
        }
    }
}

impl<'tcx> fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", TypePrinter::new().pretty_print(self))
    }
}

/// Convert a type error into a codespan diagnostic
pub fn type_error_diagnostic(error: &TypeError) -> Diagnostic<usize> {
    match error {
        TypeError::Mismatch {
            expected,
            found,
            span,
        } => Diagnostic::error()
            .with_message(format!(
                "type mismatch: expected {}, found {}",
                expected, found
            ))
            .with_labels(vec![Label::primary(0, span.start..span.end)
                .with_message("expected type here")]),

        TypeError::UnificationError { first, second, span } => {
            Diagnostic::error()
                .with_message(format!(
                    "cannot unify types {} and {}",
                    first, second
                ))
                .with_labels(vec![Label::primary(0, span.start..span.end)
                    .with_message("incompatible types")])
        }

        TypeError::TraitNotImplemented { trait_, ty, span } => {
            Diagnostic::error()
                .with_message(format!(
                    "trait {} is not implemented for type {}",
                    trait_, ty
                ))
                .with_labels(vec![Label::primary(0, span.start..span.end)
                    .with_message("trait implementation required here")])
        }

        TypeError::UnresolvedType { ty, span } => Diagnostic::error()
            .with_message(format!("cannot resolve type {}", ty))
            .with_labels(vec![Label::primary(0, span.start..span.end)
                .with_message("unknown type")]),

        TypeError::RecursiveType { ty, span } => Diagnostic::error()
            .with_message(format!("recursive type {} is not allowed", ty))
            .with_labels(vec![Label::primary(0, span.start..span.end)
                .with_message("recursive type detected here")]),

        TypeError::ResolveError(e) => Diagnostic::error()
            .with_message(format!("name resolution error: {}", e)),

        TypeError::Internal(msg) => Diagnostic::error()
            .with_message(format!("internal compiler error: {}", msg)),
            
        // Field access related errors
        TypeError::FieldNotFound { field_name, ty, span } => Diagnostic::error()
            .with_message(format!("field '{}' not found in type '{}'", field_name, ty))
            .with_labels(vec![Label::primary(0, span.start..span.end)
                .with_message("field not found")]),
                
        TypeError::InvalidTupleField { field_name, span } => Diagnostic::error()
            .with_message(format!("invalid tuple field '{}': expected numeric index", field_name))
            .with_labels(vec![Label::primary(0, span.start..span.end)
                .with_message("invalid tuple field access")]),
                
        TypeError::FieldAccessOnNonStruct { ty, span } => Diagnostic::error()
            .with_message(format!("field access not supported on type '{}'", ty))
            .with_labels(vec![Label::primary(0, span.start..span.end)
                .with_message("not a struct or tuple type")]),
        
        // Struct instantiation errors
        TypeError::InvalidBaseStruct { base_ty, struct_name, span } => Diagnostic::error()
            .with_message(format!("invalid base struct: expected '{}', got '{}'", struct_name, base_ty))
            .with_labels(vec![Label::primary(0, span.start..span.end)
                .with_message("incompatible base struct")]),
                
        TypeError::MissingField { field_name, struct_name, span } => Diagnostic::error()
            .with_message(format!("missing field '{}' in struct '{}'", field_name, struct_name))
            .with_labels(vec![Label::primary(0, span.start..span.end)
                .with_message("missing required field")]),
                
        TypeError::ExtraField { field_name, struct_name, span } => Diagnostic::error()
            .with_message(format!("extra field '{}' in struct '{}'", field_name, struct_name))
            .with_labels(vec![Label::primary(0, span.start..span.end)
                .with_message("field does not exist on struct")]),
                
        TypeError::FieldTypeMismatch { field_name, expected, actual, span } => Diagnostic::error()
            .with_message(format!("type mismatch for field '{}': expected '{}', got '{}'", 
                field_name, expected, actual))
            .with_labels(vec![Label::primary(0, span.start..span.end)
                .with_message("field type mismatch")]),
        
        // Pattern matching errors
        TypeError::TuplePatternLengthMismatch { expected, actual, span } => Diagnostic::error()
            .with_message(format!("tuple pattern length mismatch: expected {}, got {}", 
                expected, actual))
            .with_labels(vec![Label::primary(0, span.start..span.end)
                .with_message("incorrect number of elements in pattern")]),
                
        TypeError::PatternTypeMismatch { expected, pattern_desc, span } => Diagnostic::error()
            .with_message(format!("pattern type mismatch: cannot match {} against {}", 
                pattern_desc, expected))
            .with_labels(vec![Label::primary(0, span.start..span.end)
                .with_message("incompatible pattern type")]),
                
        // Added variants for the missing cases:
        TypeError::ArgumentCountMismatch { expected, actual, span } => Diagnostic::error()
            .with_message(format!("function call has wrong number of arguments: expected {}, got {}", 
                expected, actual))
            .with_labels(vec![Label::primary(0, span.start..span.end)
                .with_message("incorrect number of arguments")]),
                
        TypeError::UnknownType { name, span } => Diagnostic::error()
            .with_message(format!("unknown type '{}'", name))
            .with_labels(vec![Label::primary(0, span.start..span.end)
                .with_message("unknown type")]),
                
        TypeError::UnknownTrait { name, span } => Diagnostic::error()
            .with_message(format!("unknown trait '{}'", name))
            .with_labels(vec![Label::primary(0, span.start..span.end)
                .with_message("unknown trait")]),
                
        TypeError::UnknownMethod { name, on_type, span } => Diagnostic::error()
            .with_message(format!("unknown method '{}' on type '{}'", name, on_type))
            .with_labels(vec![Label::primary(0, span.start..span.end)
                .with_message("method not found")]),
                
        TypeError::NoMatchingMethod { name, on_type, span } => Diagnostic::error()
            .with_message(format!("no matching method '{}' found for type '{}'", name, on_type))
            .with_labels(vec![Label::primary(0, span.start..span.end)
                .with_message("no matching method")]),
    }
}

/// Print type errors to stderr with colors and source context
pub fn emit_errors(files: &SimpleFiles<String, String>, errors: &[TypeError]) {
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    for error in errors {
        let diagnostic = type_error_diagnostic(error);
        term::emit(&mut writer.lock(), &config, files, &diagnostic)
            .expect("failed to emit diagnostic");
    }
} 