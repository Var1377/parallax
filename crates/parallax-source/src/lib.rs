//! Source file management for the Parallax compiler.
//!
//! This module is responsible for:
//! - Loading source files from the filesystem
//! - Managing source file contents and tracking changes
//! - Providing a foundation for incremental compilation through Salsa
//! - Supporting diagnostic reporting via a specialized accumulator
//!
//! ## Salsa Integration
//!
//! This crate uses Salsa 0.19.0 for incremental computation:
//! - `SourceFile` is defined as a `#[salsa::input]` struct, allowing changes to be tracked
//! - The `SourceDatabase` trait defines query functions that can be memoized
//! - Helper functions provide implementation details for the query functions
//!
//! ## Diagnostics
//!
//! The diagnostic system uses Salsa accumulators for error reporting, with optional
//! miette integration for rich error display with source spans and context.

mod diagnostic;
mod config;
mod file;
mod frame;
mod error;
mod path;


pub use diagnostic::{ParallaxDiagnostic, ParallaxError, Report, report_error};
pub use config::{FrameConfig, FrameConfigInner, PackageInfo, Dependency, DependencyDetails};
pub use file::SourceFile;
pub use frame::{Frame, Dir};
pub use error::FrameError;
pub use path::Path;

/// Database trait for the source module.
///
/// This trait defines query functions for working with source files.
/// It's implemented by the central Parallax database to provide
/// source file handling capabilities.
///
/// # Salsa Integration
///
/// The `#[salsa::db]` attribute marks this as a database trait for Salsa,
/// allowing Salsa to generate query function implementations.
#[salsa::db]
pub trait SourceDatabase: salsa::Database {
    /// Loads a frame by its configuration path
    ///
    /// This query function loads a frame from the filesystem and processes
    /// its structure, including its configuration and source files.
    ///
    /// # Parameters
    ///
    /// * `config_path` - The path to the directory containing the frame configuration
    ///
    /// # Returns
    ///
    /// A `Frame` representing the resolved frame structure, with errors reported via accumulators
    fn load_frame<'db>(&'db self, config_path: &str) -> Frame<'db> 
    where
        Self: Sized
    {
        let path = Path::new(self, std::path::PathBuf::from(config_path));
        frame::load_frame(self, path)
    }
}

/// Test utilities for working with source files
///
/// This module provides utilities for creating test databases and source files
/// for unit testing compiler components that depend on source files.
#[cfg(test)]
pub mod testing {
    use super::*;
    
    /// A simple implementation of the source database for testing
    ///
    /// This database implements the `SourceDatabase` trait and can be used
    /// in unit tests that require source file functionality.
    #[salsa::db]
    #[derive(Default, Clone)]
    pub struct TestDatabase {
        /// Salsa storage for query memoization
        storage: salsa::Storage<Self>,
    }
    
    /// Implementation of the Salsa Database trait for TestDatabase
    #[salsa::db]
    impl salsa::Database for TestDatabase {
        /// Handle Salsa events
        ///
        /// This implementation simply executes the event function.
        fn salsa_event(&self, event: &dyn Fn() -> salsa::Event) {
            event();
        }
    }
    
    /// Implementation of the SourceDatabase trait for TestDatabase
    #[salsa::db]
    impl SourceDatabase for TestDatabase {}
}