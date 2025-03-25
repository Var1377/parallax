//! High-level Intermediate Representation (HIR) for Parallax
//! 
//! This crate provides the High-level Intermediate Representation (HIR)
//! for the Parallax compiler, as well as functionality to lower AST to HIR.

pub mod db;
pub mod hir;
pub mod lower;

// Re-export the main public API
pub use crate::hir::*;
pub use crate::db::{HirDatabase, HirStorage, HirError};
pub use crate::lower::lower_ast_to_hir;

/// The result of lowering an AST to HIR
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirResult {
    /// The root HIR crate
    pub hir: Crate,
    /// Any warnings generated during lowering
    pub warnings: Vec<String>,
} 