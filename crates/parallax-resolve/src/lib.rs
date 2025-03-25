//! The name resolution pass for the Parallax compiler.
//!
//! This crate handles transforming an AST from `parallax-lang` into a resolved AST,
//! where all identifiers are linked to their declarations, and type checking is performed.

pub mod db;
pub mod error;
pub mod imports;
pub mod namespace;
pub mod resolver;
pub mod symbol;
pub mod types;

// Re-export the main types for external use
pub use db::{ResolverDatabase, ResolverStorage};
pub use error::ResolveError;
pub use resolver::{Resolver, ResolvedCrate, resolve_crate};
pub use symbol::{Symbol, SymbolTable, ScopeId, ModuleId};
pub use types::{ResolvedType, TypeEnv};

/// Result type for resolution operations
pub type Result<T> = std::result::Result<T, error::ResolveError>;
