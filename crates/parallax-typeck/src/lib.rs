// Parallax Type Checking Crate
//
// This crate provides the type inference and checking system for the Parallax language.
// It uses a constraint-based type inference algorithm with unification, 
// combined with trait solving for handling type classes.

//! # Parallax Type Checking
//!
//! This crate implements the type checking and inference system for Parallax.
//! The core approach uses:
//!
//! 1. Constraint generation during traversal
//! 2. Unification-based type inference
//! 3. Trait obligation solving
//!
//! The main entry point is the `type_check_crate` function, which takes an AST
//! and produces a type-checked HIR representation.

// Public modules - these need to be declared first
pub mod context;
pub mod db;
pub mod error;
pub mod hir;
pub mod unify;
pub mod traits;
pub mod diagnostics;
pub mod infer;
pub mod resolve;
pub mod symbol_resolver;
pub mod salsa_db;

// Test modules - only compiled in test mode
#[cfg(test)]
mod tests;

// Re-exports
pub use context::{Ty, ConcreteTy, TypeContext, Constraint};
pub use error::TypeError;
pub use db::TypeCheckingDatabase;
pub use traits::{TraitRef, PredicateObligation, TraitSolver};
pub use salsa_db::{TypeCheckDb, TypeCheckStorage};

/// The result of type checking a crate
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckedCrate {
    /// The high-level IR with type information
    pub hir: hir::Crate,
    /// Any warnings that were generated
    pub warnings: Vec<error::TypeError>,
}
