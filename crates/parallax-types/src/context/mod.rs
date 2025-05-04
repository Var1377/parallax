// src/context/mod.rs
//! Defines structures for managing context during type checking,
//! including scopes, inference state, and trait/impl information.

pub mod env;
pub mod inference;
pub mod type_context; // Renamed from repo
pub mod trait_repo; // Renamed from repo

// Re-export key context types
pub use env::TypeEnvironment;
pub use inference::{InferenceContext, InferenceSnapshot, Substitution};
// Re-export Id types directly for convenience
pub use trait_repo::{TraitId, ImplId, TraitRepository};
pub use type_context::TypeContext; 