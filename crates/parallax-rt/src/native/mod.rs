// src/native/mod.rs

// Make the native runtime implementation private to this module
mod runtime; 
 
// Re-export the public entry point for running native artifacts
pub use runtime::run_artifact; 