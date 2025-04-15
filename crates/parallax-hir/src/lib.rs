pub mod hir;
pub mod dce;
pub mod inlining;
pub mod lower;

pub mod tests;


pub use hir::*; // Re-export core HIR types
pub use parallax_resolve::types::Symbol; // Publicly re-export Symbol
pub use dce::perform_dce;
pub use inlining::perform_inlining;
pub use lower::lower_module_to_anf_hir;