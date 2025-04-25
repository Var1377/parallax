use crate::error::RuntimeError;
use crate::ExecutionResult; // Import ExecutionResult
use parallax_gc; // Need this again for direct call
// No longer needed here, handled via crate::init_gc_internal
// use parallax_gc; 
use parallax_hir::Symbol;
// Import HirType and HirPrimitiveType from hir
use parallax_hir::hir::{HirType, PrimitiveType as HirPrimitiveType};
// ResolvePrimitiveType is not needed here based on current usage
// use parallax_resolve::types::PrimitiveType as ResolvePrimitiveType;
use parallax_native::CompiledArtifact;
use std::mem;
use log; // Added log import

/// Initializes the Parallax runtime environment.
///
/// Currently, this only initializes the garbage collector.
// NOTE: This function might belong in lib.rs now, keeping it here temporarily
// pub fn init_runtime() -> Result<(), RuntimeError> {
//     log::debug!("Initializing Parallax runtime...");
//     // TODO: Check if already initialized?
//     parallax_gc::init_gc();
//     log::info!("Parallax runtime initialized (GC started).");
//     Ok(())
// }

/// Runs the compiled Parallax program using the native backend.
///
/// Takes the compiled artifact, the symbol and HIR type of the entry function,
/// and arguments (currently unused). It executes the entry function based on its
/// type signature and returns an ExecutionResult.
///
/// Initializes the GC at the start and attempts shutdown at the end.
///
/// # Safety
///
/// See function documentation.
pub fn run_artifact(
    artifact: CompiledArtifact, // Takes ownership to keep JIT module alive
    entry_symbol: Symbol,
    entry_type: &HirType, // Accept the actual HIR type
    _args: Vec<String>,
) -> Result<ExecutionResult, RuntimeError> { // Changed return type
    // Initialize GC directly at the start of this run function.
    log::debug!("Native Runtime: Initializing GC...");
    parallax_gc::init_gc();
    log::info!("Native Runtime: GC Initialized.");

    log::debug!("Native Runtime: Attempting to run artifact entry point: {:?} with type {:?}", entry_symbol, entry_type);

    let func_ptr = artifact
        .get_function_ptr(entry_symbol)
        .ok_or(RuntimeError::EntryPointNotFound(entry_symbol))?;

    // Execute based on the known return type
    let result = match entry_type {
        // Unit Type
        HirType::Tuple(elements) if elements.is_empty() => {
            type EntryFnUnit = fn();
            let entry_fn: EntryFnUnit = unsafe { mem::transmute(func_ptr) };
            log::info!("Native Runtime: Executing JIT'd entry point (-> ())...");
            entry_fn();
            log::info!("Native Runtime: Execution finished (returned unit).");
            Ok(ExecutionResult::Unit)
        }
        // Primitive Types
        HirType::Primitive(prim_type) => {
            match prim_type {
                HirPrimitiveType::I8 => {
                    type EntryFn = fn() -> i8;
                    let entry_fn: EntryFn = unsafe { mem::transmute(func_ptr) };
                    let value = entry_fn();
                    log::info!("Native Runtime: Execution finished (returned i8: {}).", value);
                    Ok(ExecutionResult::PrimitiveI8(value))
                }
                HirPrimitiveType::I16 => {
                    type EntryFn = fn() -> i16;
                    let entry_fn: EntryFn = unsafe { mem::transmute(func_ptr) };
                    let value = entry_fn();
                    log::info!("Native Runtime: Execution finished (returned i16: {}).", value);
                    Ok(ExecutionResult::PrimitiveI16(value))
                }
                HirPrimitiveType::I32 => {
                    type EntryFn = fn() -> i32;
                    let entry_fn: EntryFn = unsafe { mem::transmute(func_ptr) };
                    let value = entry_fn();
                    log::info!("Native Runtime: Execution finished (returned i32: {}).", value);
                    Ok(ExecutionResult::PrimitiveI32(value))
                }
                HirPrimitiveType::I64 => {
                    type EntryFn = fn() -> i64;
                    let entry_fn: EntryFn = unsafe { mem::transmute(func_ptr) };
                    let value = entry_fn();
                    log::info!("Native Runtime: Execution finished (returned i64: {}).", value);
                    Ok(ExecutionResult::PrimitiveI64(value))
                }
                HirPrimitiveType::I128 => {
                    type EntryFn = fn() -> i128;
                    let entry_fn: EntryFn = unsafe { mem::transmute(func_ptr) };
                    let value = entry_fn();
                    log::info!("Native Runtime: Execution finished (returned i128: {}).", value);
                    Ok(ExecutionResult::PrimitiveI128(value))
                }
                 HirPrimitiveType::U8 => {
                    type EntryFn = fn() -> u8;
                    let entry_fn: EntryFn = unsafe { mem::transmute(func_ptr) };
                    let value = entry_fn();
                    log::info!("Native Runtime: Execution finished (returned u8: {}).", value);
                    Ok(ExecutionResult::PrimitiveU8(value))
                }
                HirPrimitiveType::U16 => {
                    type EntryFn = fn() -> u16;
                    let entry_fn: EntryFn = unsafe { mem::transmute(func_ptr) };
                    let value = entry_fn();
                    log::info!("Native Runtime: Execution finished (returned u16: {}).", value);
                    Ok(ExecutionResult::PrimitiveU16(value))
                }
                HirPrimitiveType::U32 => {
                    type EntryFn = fn() -> u32;
                    let entry_fn: EntryFn = unsafe { mem::transmute(func_ptr) };
                    let value = entry_fn();
                    log::info!("Native Runtime: Execution finished (returned u32: {}).", value);
                    Ok(ExecutionResult::PrimitiveU32(value))
                }
                HirPrimitiveType::U64 => {
                    type EntryFn = fn() -> u64;
                    let entry_fn: EntryFn = unsafe { mem::transmute(func_ptr) };
                    let value = entry_fn();
                    log::info!("Native Runtime: Execution finished (returned u64: {}).", value);
                    Ok(ExecutionResult::PrimitiveU64(value))
                }
                HirPrimitiveType::U128 => {
                    type EntryFn = fn() -> u128;
                    let entry_fn: EntryFn = unsafe { mem::transmute(func_ptr) };
                    let value = entry_fn();
                    log::info!("Native Runtime: Execution finished (returned u128: {}).", value);
                    Ok(ExecutionResult::PrimitiveU128(value))
                }
                HirPrimitiveType::F32 => {
                    type EntryFn = fn() -> f32;
                    let entry_fn: EntryFn = unsafe { mem::transmute(func_ptr) };
                    let value = entry_fn();
                    log::info!("Native Runtime: Execution finished (returned f32: {}).", value);
                    Ok(ExecutionResult::PrimitiveF32(value))
                }
                HirPrimitiveType::F64 => {
                    type EntryFn = fn() -> f64;
                    let entry_fn: EntryFn = unsafe { mem::transmute(func_ptr) };
                    let value = entry_fn();
                    log::info!("Native Runtime: Execution finished (returned f64: {}).", value);
                    Ok(ExecutionResult::PrimitiveF64(value))
                }
                HirPrimitiveType::Bool => {
                    // Bools are represented as i8 (0 or 1) by Cranelift
                    type EntryFn = fn() -> i8; 
                    let entry_fn: EntryFn = unsafe { mem::transmute(func_ptr) };
                    let value_i8 = entry_fn();
                    let value = value_i8 != 0;
                    log::info!("Native Runtime: Execution finished (returned bool: {} from i8: {}).", value, value_i8);
                    Ok(ExecutionResult::PrimitiveBool(value))
                }
                HirPrimitiveType::Char => {
                    // Chars are represented as i32 by Cranelift
                    type EntryFn = fn() -> i32;
                    let entry_fn: EntryFn = unsafe { mem::transmute(func_ptr) };
                    let value_i32 = entry_fn();
                    // TODO: Check if conversion can fail? char::from_u32 returns Option
                    let value = std::char::from_u32(value_i32 as u32).unwrap_or('?'); // Handle potential invalid char
                    log::info!("Native Runtime: Execution finished (returned char: '{}' from i32: {}).", value, value_i32);
                    Ok(ExecutionResult::PrimitiveChar(value))
                }
                HirPrimitiveType::Unit => {
                    // Should be caught by the HirType::Tuple branch, but handle defensively
                     log::info!("Native Runtime: Executing JIT'd entry point (-> Primitive::Unit)...");
                     // Assume the function takes no args and returns nothing (or ZST)
                     type EntryFnUnit = fn();
                     let entry_fn: EntryFnUnit = unsafe { mem::transmute(func_ptr) };
                     entry_fn();
                     log::info!("Native Runtime: Execution finished (returned unit).");
                     Ok(ExecutionResult::Unit)
                }
                HirPrimitiveType::String => {
                     // String should return a Handle<GcObject>
                     log::error!("Native Runtime: Expected Handle return for String, but matched Primitive branch.");
                     Err(RuntimeError::ExecutionFailed)
                }
            }
        }
        // Potential Future: GC Handles
        // HirType::Adt(_) | HirType::Array(_, _) | HirType::FunctionPointer(_, _) ... => { ... }
        _ => {
            log::error!("Native Runtime: Unsupported entry point type for native execution: {:?}", entry_type);
            Err(RuntimeError::ExecutionFailed)
        }
    };

    // Attempt GC shutdown before returning result
    log::debug!("Native Runtime: Shutting down GC...");
    parallax_gc::shutdown_gc();
    log::info!("Native Runtime: GC Shutdown requested.");

    result // Return the ExecutionResult (Ok or Err)
} 