use crate::error::RuntimeError;
use crate::native; // Import the native module
use crate::ExecutionResult; // Import ExecutionResult
// parallax_gc is no longer needed here
// use parallax_gc; 
use parallax_hir::Symbol;
use parallax_hir::hir::{HirType, PrimitiveType as HirPrimitiveType};
use parallax_native::CompiledArtifact;
use std::mem;

/// Runs the compiled Parallax program using the appropriate backend logic.
///
/// This function currently delegates directly to the native backend's `run_artifact`.
/// Takes the compiled artifact, the symbol and HIR type of the entry function,
/// and arguments (currently unused). It executes the entry function based on its
/// type signature and returns an ExecutionResult.
///
/// # Safety
///
/// Relies on the safety guarantees of the underlying backend execution function.
pub fn run_artifact(
    artifact: CompiledArtifact, // Takes ownership to keep JIT module alive
    entry_symbol: Symbol,
    entry_type: &HirType, // Accept the actual HIR type
    args: Vec<String>, // Pass args through
) -> Result<ExecutionResult, RuntimeError> {
    // Delegate directly to the native implementation
    // The native::run_artifact already handles GC init/shutdown and primitive types.
    native::run_artifact(artifact, entry_symbol, entry_type, args)
} 