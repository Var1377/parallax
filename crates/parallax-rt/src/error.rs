use thiserror::Error;
use parallax_hir::Symbol;
use crate::inet::PartitionIdx;

/// Errors specific to the Parallax runtime.
#[derive(Error, Debug, Clone)]
pub enum RuntimeError {
    #[error("Runtime initialization failed.")]
    InitializationFailed, // TODO: Add source if needed

    #[error("Entry point function with symbol {0:?} not found in compiled artifact.")]
    EntryPointNotFound(Symbol),

    #[error("Execution of entry point failed.")] // TODO: Capture more specific JIT errors?
    ExecutionFailed,

    #[error("Required partition {0} not found during runtime. Context: {1}")]
    PartitionNotFound(PartitionIdx, String),

    #[error("Runtime attempted to start while already running.")]
    AlreadyRunning,

    #[error("An unspecified runtime error occurred: {0}")]
    Other(String),

    // Add more specific runtime errors as needed (e.g., unhandled exceptions from JIT code)
}