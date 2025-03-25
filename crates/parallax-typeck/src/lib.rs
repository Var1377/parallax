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

use parallax_lang::ast;

// Public modules - these need to be declared first
pub mod context;
pub mod db;
pub mod error;
pub mod hir;
pub mod unify;
pub mod traits;
pub mod diagnostics;
pub mod infer;

// Re-exports
pub use context::{Ty, ConcreteTy, TypeContext, Constraint};
pub use error::TypeError;
pub use db::TypeCheckingDatabase;
pub use traits::{TraitRef, PredicateObligation};

/// The result of type checking a crate
#[derive(Debug)]
pub struct CheckedCrate {
    /// The high-level IR with type information
    pub hir: hir::Crate,
    /// Any warnings that were generated
    pub warnings: Vec<error::TypeError>,
}

/// Type check a crate
///
/// This is the main entry point for the type checking system. It:
/// 1. Generates constraints by walking the AST
/// 2. Solves constraints through unification
/// 3. Verifies trait bounds
/// 4. Produces a typed HIR
///
/// # Errors
///
/// Returns a list of type errors if any were encountered during type checking
pub fn type_check_crate(db: &dyn TypeCheckingDatabase) -> Result<CheckedCrate, Vec<error::TypeError>> {
    // Get the resolved AST
    let _resolved_ast = match db.resolved_ast() {
        Ok(ast) => ast,
        Err(errors) => return Err(errors),
    };
    
    // Create a dummy crate for now
    let dummy_crate = ast::items::Crate { 
        items: vec![] 
    };
    
    // --- Phase 1: Collect errors from constraint generation ---
    
    // For now, we'll create a fresh context and perform a minimal type check
    // In a future PR, we would:
    // 1. Properly integrate with the constraint generator
    // 2. Connect the unification and trait solving properly
    
    // The real implementation would build up constraints while traversing
    // the AST, then solve those constraints in a later phase.
    
    // For now, just construct a simple HIR representation
    let hir = hir::Crate {
        items: Vec::new(),
    };
    
    Ok(CheckedCrate {
        hir,
        warnings: Vec::new(),
    })
}

/// Helper functions for tests
#[cfg(test)]
pub mod test_utils {
    use super::*;
    
    /// Create a type context for testing
    pub fn create_test_context() -> context::TypeContext<'static> {
        // This is a stub - it would be expanded in real tests
        unimplemented!("Test utilities are not implemented yet")
    }
}
