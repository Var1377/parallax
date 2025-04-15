//! Diagnostic types for error reporting in the Parallax compiler
//!
//! This module provides a diagnostic system built on Salsa accumulators that:
//! - Enables tracked functions to report errors without side effects
//! - Integrates with the miette crate for rich error display with source context
//! - Supports structured error reporting with source spans
//! - Preserves diagnostic context across incremental compilation cycles
//!
//! ## Salsa Accumulators
//!
//! Salsa accumulators allow memoized functions to "push" values that can be collected later.
//! This pattern is particularly useful for error reporting, as it allows functions to report
//! errors without direct side effects (which would break memoization).
//!
//! When a memoized function (or a function it calls) pushes a diagnostic, that diagnostic
//! is associated with that function's invocation. Later, you can collect all diagnostics
//! produced by a specific function call using the `accumulated` function.
//!
//! ## Miette Integration
//!
//! The diagnostic types in this module integrate with miette to provide rich error
//! reporting with source code context, highlighting, and labels.

use crate::{SourceDatabase, SourceFile};
use std::{error::Error, fmt::{self, Display}, panic::RefUnwindSafe, sync::Arc};
use miette::{Diagnostic, Severity};
use salsa::Accumulator;
use thiserror::Error;

/// A diagnostic message produced during compilation.
///
/// This is defined as a Salsa accumulator, allowing diagnostics to be
/// pushed from Salsa-tracked functions and collected later.
///
/// # Salsa Accumulator Pattern
///
/// Diagnostics are pushed onto this accumulator with:
/// ```rust,ignore
/// ParallaxDiagnostic::push(db, ParallaxDiagnostic { file, report });
/// ```
///
/// And can be collected from a function later with:
/// ```rust,ignore
/// let diagnostics = some_query_function::accumulated::<ParallaxDiagnostic>(db);
/// ```
///
/// # Fields
///
/// * `file` - The source file the diagnostic is associated with
/// * `report` - The error report containing the diagnostic details
#[salsa::accumulator]
pub struct ParallaxDiagnostic {
    /// The source file where the diagnostic occurred (using string to avoid lifetime issues)
    pub location: String,
    
    /// The error report containing details about the diagnostic
    pub report: Arc<dyn ParallaxError>,
}

/// A report for rendering diagnostics with source code context.
///
/// This struct combines an error report with the source code text
/// to provide rich error reporting via miette.
///
/// # Fields
///
/// * `source_code` - The source code text to display with the error
/// * `report` - The error report containing diagnostic details
#[derive(Debug, Clone, Error)]
pub struct Report {
    /// The source code text for context in error displays
    pub source_code: String,
    
    /// The error report
    pub report: Arc<dyn ParallaxError>,
}

/// Display implementation that delegates to the underlying error report
impl fmt::Display for Report {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.report.fmt(f)
    }
}

/// Implementation of miette's Diagnostic trait for Report
///
/// This implementation delegates most methods to the underlying report
/// but provides the source code text for miette's rendering.
impl miette::Diagnostic for Report {
    /// Returns the error code
    fn code<'b>(&'b self) -> Option<Box<dyn fmt::Display + 'b>> {
        self.report.code()
    }

    /// Returns the underlying diagnostic source
    fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
        self.report.diagnostic_source()
    }

    /// Returns help text for resolving the error
    fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.report.help()
    }

    /// Returns labeled spans in the source code
    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        self.report.labels()
    }

    /// Returns related diagnostics
    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        self.report.related()
    }

    /// Returns the source code for display
    ///
    /// This is where our implementation differs from the delegate -
    /// we provide the source code text from our Report struct.
    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        Some(&self.source_code)
    }

    /// Returns a URL for more information
    fn url<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.report.url()
    }

    /// Returns the severity level of the diagnostic
    fn severity(&self) -> Option<Severity> {
        self.report.severity()  
    }
}

/// A trait for errors that can be reported with source context.
///
/// This trait extends miette's Diagnostic trait with the ability
/// to create a Report that includes source code context.
///
/// # Type Bounds
///
/// * `Diagnostic` - For rich error display capabilities
/// * `Send + Sync` - For thread safety
/// * `RefUnwindSafe` - For safety across panic boundaries
/// * `Display + Error` - Standard error trait requirements
/// * `'static` - Lifetime bound for storage in containers
///
/// # Methods
///
/// * `report` - Creates a Report with source context
pub trait ParallaxError: Diagnostic + Send + Sync + RefUnwindSafe + Display + Error + 'static {
    /// Create a Report with source context for this error
    ///
    /// # Parameters
    ///
    /// * `db` - The source database for accessing file content
    /// * `file` - The source file where the error occurred
    ///
    /// # Returns
    ///
    /// A Report with this error and the source context
    fn report(self, db: &dyn SourceDatabase, file: SourceFile) -> Report;
}

/// Blanket implementation of ParallaxError for all types that implement required traits
///
/// This allows any type that already implements miette's Diagnostic (and other required traits)
/// to be used as a ParallaxError without additional implementation work.
impl<T: Diagnostic + Send + Sync + RefUnwindSafe + Display + Error + 'static> ParallaxError for T {
    /// Create a Report with source context for this error
    ///
    /// # Implementation
    ///
    /// This creates a Report with the source code from the file
    /// and this error wrapped in an Arc.
    fn report(self, db: &dyn SourceDatabase, file: SourceFile) -> Report {
        Report { 
            source_code: file.contents(db).to_string(),
            report: Arc::new(self)
        }
    }
}

pub fn report_error<'db, R : ParallaxError>(db: &'db dyn SourceDatabase, location: String, report: R) {
    ParallaxDiagnostic {
        location,
        report: Arc::new(report),
    }.accumulate(db);
}