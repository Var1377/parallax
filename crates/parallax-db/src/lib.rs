//! Database and incremental compilation infrastructure for the Parallax compiler.
//!
//! This crate provides the core database infrastructure used throughout the compiler
//! for incremental compilation. It defines the database traits and query system that
//! allows different compiler phases to cache and reuse their results efficiently. 

mod database;
mod error;
// These modules will be implemented later:
// mod query;
// mod storage;
// mod cache;
// mod dependency;
// mod input;
// mod output;
// mod updates;
// mod serialization;

// Re-export the central database types
pub use database::Compiler;

// Re-export error types
pub use error::{DatabaseError, DatabaseResult};