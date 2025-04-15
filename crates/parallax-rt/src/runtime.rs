use crate::error::RuntimeError;
use parallax_gc;
use parallax_hir::Symbol;
use parallax_hir::hir::{HirType, ResolvePrimitiveType};
use parallax_native::CompiledArtifact;
use std::mem;

/// Initializes the Parallax runtime environment.
///
/// Currently, this only initializes the garbage collector.
pub fn init_runtime() -> Result<(), RuntimeError> {
    log::debug!("Initializing Parallax runtime...");
    // TODO: Check if already initialized?
    parallax_gc::init_gc();
    log::info!("Parallax runtime initialized (GC started).");
    Ok(())
}

/// Runs the compiled Parallax program.
///
/// Takes the compiled artifact, the symbol and HIR type of the entry function,
/// and arguments (currently unused). It executes the entry function based on its
/// type signature and returns a normalized exit code (`i64`).
///
/// # Safety
///
/// See function documentation.
pub fn run_artifact(
    artifact: CompiledArtifact, // Takes ownership to keep JIT module alive
    entry_symbol: Symbol,
    entry_type: &HirType, // Accept the actual HIR type
    _args: Vec<String>,
) -> Result<i64, RuntimeError> {
    log::debug!("Attempting to run artifact entry point: {:?} with type {:?}", entry_symbol, entry_type);

    let func_ptr = artifact
        .get_function_ptr(entry_symbol)
        .ok_or(RuntimeError::EntryPointNotFound(entry_symbol))?;

    // Execute based on the known return type
    let exit_code = match entry_type {
        // Case 1: fn() -> i64
        HirType::Primitive(ResolvePrimitiveType::I64) => {
            // Define the expected function signature type
            type EntryFnI64 = fn() -> i64;
            // Transmute and call
            let entry_fn: EntryFnI64 = unsafe {
                mem::transmute::<*const u8, EntryFnI64>(func_ptr)
            };
            log::info!("Executing JIT'd entry point (-> i64)...");
            let result = entry_fn();
            log::info!("Execution finished. Result: {}", result);
            result // Return the i64 result directly
        }
        // Case 2: fn() -> ()
        HirType::Tuple(elements) if elements.is_empty() => {
            // Define the expected function signature type
            type EntryFnUnit = fn();
            // Transmute and call
            let entry_fn: EntryFnUnit = unsafe {
                mem::transmute::<*const u8, EntryFnUnit>(func_ptr)
            };
            log::info!("Executing JIT'd entry point (-> ())...");
            entry_fn(); // Call the function
            log::info!("Execution finished (returned unit).");
            0 // Map unit return to exit code 0
        }
        // Should not happen if parallax-db check is correct, but handle defensively
        _ => {
            log::error!("Runtime Error: Unexpected entry point type {:?}", entry_type);
            return Err(RuntimeError::ExecutionFailed);
        }
    };

    Ok(exit_code)
} 