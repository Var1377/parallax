// src/types/mod.rs

//! Defines the core types, definitions, and typed HIR used in the Parallax type system.

// Declare submodules
pub mod core;
pub mod defs;
pub mod hir;

pub use core::{
    Ty,
    TyKind,
    PrimitiveType,
    PointerType,
};
pub use defs::*;
pub use hir::*;