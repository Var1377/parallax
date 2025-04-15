//! Parallax MIR (Mid-level Intermediate Representation)
//! 
//! This crate defines the MIR representation used in the Parallax compiler.

pub mod mir;
pub mod lowering;

pub use mir::*; 
pub use lowering::lower_module; // Expose the main entry point 