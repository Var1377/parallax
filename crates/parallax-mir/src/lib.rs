//! Mid-level Intermediate Representation (MIR) for the Parallax compiler.
//!
//! The MIR is a control-flow-graph based representation of functions,
//! suitable for optimizations and further lowering to a backend IR.
//!
//! Key features of the MIR:
//! - Explicit control flow through basic blocks
//! - Immutable SSA-like representation for values
//! - Explicit memory locations (places)
//! - Explicit pattern matching
//! - Monomorphized generic functions
//! - Optimizable representation

// Re-export the database trait
pub use db::MirDatabase;

// Public modules
pub mod db;
pub mod error;
pub mod ir;
pub mod lower;
pub mod mono;
pub mod opt;
pub mod util;

// Tests module
#[cfg(test)]
mod tests;
