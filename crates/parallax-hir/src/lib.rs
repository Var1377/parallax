use parallax_types::Symbol;
use parallax_resolve::types::PrimitiveType as ResolvePrimitiveType;
use parallax_resolve::definitions::DefinitionKind; // Use DefinitionKind from resolve
use fxhash::FxHashMap;
use miette::SourceSpan;
use std::sync::Arc;

pub mod hir;
pub mod lower;
// pub mod transform;

pub use hir::*; // Re-export core HIR types