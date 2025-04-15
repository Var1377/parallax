#![allow(missing_docs)] // TODO: Remove this

pub mod error;
pub mod generator;

pub use error::CodegenError;
pub use generator::{generate_module, CompiledOutput};