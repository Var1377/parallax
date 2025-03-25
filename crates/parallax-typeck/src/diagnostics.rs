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