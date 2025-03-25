//! Core name resolution logic for the Parallax compiler.
//!
//! This module contains the main resolver that processes the AST and links
//! all identifiers to their declarations.

// Re-export core types
mod core;
mod expr;
mod item;
mod path;
mod pattern;
mod types;

pub use core::{Resolver, ResolvedCrate, resolve_crate};
pub use path::PathResolver;
pub use expr::ExprResolver;
pub use item::ItemResolver;
pub use pattern::PatternResolver;
pub use types::TypeResolver; 