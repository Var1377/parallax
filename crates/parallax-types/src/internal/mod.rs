// src/internal/mod.rs
//! Internal implementation details for the type checker.
//! Not intended for public use outside the `parallax-types` crate.

// Re-export necessary items from checker submodules for internal use
pub(crate) use crate::checker::substitute::{substitute_signature_self, substitute_self_in_trait_ref};
// Remove unused import resolve_type_to_ty
// pub(crate) use crate::checker::resolve::{resolve_type_to_ty};
// Add other internal helpers or re-exports as needed. 