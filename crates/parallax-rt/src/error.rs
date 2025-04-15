use thiserror::Error;
use parallax_hir::Symbol;

/// Errors specific to the Parallax runtime.
#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("Runtime initialization failed.")]
    InitializationFailed, // TODO: Add source if needed

    #[error("Entry point function with symbol {0:?} not found in compiled artifact.")]
    EntryPointNotFound(Symbol),

    #[error("Execution of entry point failed.")] // TODO: Capture more specific JIT errors?
    ExecutionFailed,
    // Add more specific runtime errors as needed (e.g., unhandled exceptions from JIT code)
} 