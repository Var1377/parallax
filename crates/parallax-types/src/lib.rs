//! Type checking and inference for the Parallax programming language.
//!
//! This crate implements type inference, type checking, and trait solving for Parallax.
//! It builds on the name resolution phase and provides a fully typed AST.

pub mod error;
pub mod inference;
pub mod traits;
pub mod types;
pub mod typecheck;

// Re-export commonly used types
pub use error::{TypeError, TypeResult};
pub use inference::{InferenceContext, TypeEnvironment};
pub use types::{PrimitiveType, Ty, TyKind, TypeId, TypeContext, TypeDef, StructDef, EnumDef, FunctionSignature, TypedModule};
pub use traits::TraitRepository;

use parallax_resolve::{ResolveDatabase, types::ResolvedModuleStructure};

/// Database trait for type checking operations
#[salsa::db]
pub trait TypeDatabase: ResolveDatabase {
    /// Type check a resolved module structure
    fn type_check_definitions<'db>(&'db self, resolved_module: ResolvedModuleStructure) -> TypedModule where Self: Sized {
        crate::typecheck::type_check_definitions(self, resolved_module)
    }
}