use crate::error::RuntimeError;
use crate::ExecutionResult; // Import ExecutionResult
// Use rsgc thread module and HeapArguments from prelude
use rsgc::prelude::HeapArguments; 
use rsgc::heap::thread;
use parallax_gc; // Need this for set_descriptor_store and closure init
// No longer needed here, handled via crate::init_gc_internal
// use parallax_gc; 
use parallax_hir::Symbol;
// Import HirType and HirPrimitiveType from hir
use parallax_hir::hir::{HirType, PrimitiveType as HirPrimitiveType};
// ResolvePrimitiveType is not needed here based on current usage
// use parallax_resolve::types::PrimitiveType as ResolvePrimitiveType;
use parallax_native::CompiledArtifact;
// Import DescriptorStore from layout crate
use parallax_layout::DescriptorStore; 
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
/// and arguments (currently unused). It initializes the GC using `main_thread`,
/// sets up the descriptor store, executes the entry function based on its
/// type signature, and returns an ExecutionResult.
///
/// # Safety
///
/// See function documentation. Transmutes function pointers based on expected HIR type.
/// Assumes the `entry_symbol` exists in the artifact and matches `entry_type`.
/// Requires careful management of the GLOBAL_DESCRIPTOR_STORE lifetime.
pub fn run_artifact(
    artifact: CompiledArtifact, // Takes ownership to keep JIT module alive
    entry_symbol: Symbol,
    entry_type: &HirType, // Accept the actual HIR type
    _args: Vec<String>,
) -> Result<ExecutionResult, RuntimeError> {
    log::debug!("Native Runtime: Preparing GC arguments...");

    // --- Extract JIT info *before* entering GC thread --- 
    log::debug!("Native Runtime: Getting entry point function pointer for {:?}...", entry_symbol);
    let func_ptr = artifact
        .get_function_ptr(entry_symbol)
        .ok_or(RuntimeError::EntryPointNotFound(entry_symbol))?;
    // Clone the entry type so we can move it into the closure 
    let entry_type_clone = entry_type.clone();
    // Keep artifact alive outside the closure
    let _artifact_guard = artifact; 

    let gc_args = HeapArguments::default(); // Configure as needed

    log::info!("Native Runtime: Entering GC main_thread...");

    // Type annotation might not be necessary if inference works
    let gc_thread_result = thread::main_thread(gc_args, move |heap| {
        // --- GC is now initialized by rsgc, 'heap' is the handle ---
        log::info!("Native Runtime: Inside GC main_thread. Heap initialized.");

        // --- Descriptor Store Initialization ---
        log::debug!("Native Runtime: Initializing descriptor store...");
        let mut descriptor_store = DescriptorStore {
            descriptors: Vec::new(),
        };
        // SAFETY: initialize_closure_ref_descriptor expects a mutable DescriptorStore
        // It also relies on the GC heap being initialized.
        unsafe {
            parallax_gc::collections::closure::initialize_closure_ref_descriptor(&mut descriptor_store);
        }
        // Box the store and get a raw pointer
        let store_box = Box::new(descriptor_store);
        let store_ptr = Box::into_raw(store_box);
        // Set the thread-local pointer.
        // SAFETY: Requires careful lifetime management. Store must outlive GC operations.
        unsafe { parallax_gc::set_current_descriptor_store(store_ptr); }
        log::info!("Native Runtime: Descriptor store initialized and set in thread-local.");

        // Optional: Register core roots if using conservative stack scanning
        // heap.add_core_root_set();
        // log::debug!("Native Runtime: Core GC roots registered.");

        // --- Execute the JIT'd code (using captured func_ptr and entry_type_clone) ---
        let execution_result = (|| { 
            // Use the captured func_ptr and entry_type_clone
            log::debug!("Native Runtime: Executing entry point with type {:?}...", entry_type_clone);
            match &entry_type_clone { // Match on the cloned type
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
                            type EntryFn = fn() -> i8; // Bool -> i8
                    let entry_fn: EntryFn = unsafe { mem::transmute(func_ptr) };
                    let value_i8 = entry_fn();
                    let value = value_i8 != 0;
                    log::info!("Native Runtime: Execution finished (returned bool: {} from i8: {}).", value, value_i8);
                    Ok(ExecutionResult::PrimitiveBool(value))
                }
                HirPrimitiveType::Char => {
                            type EntryFn = fn() -> i32; // Char -> i32
                    let entry_fn: EntryFn = unsafe { mem::transmute(func_ptr) };
                    let value_i32 = entry_fn();
                            let value = std::char::from_u32(value_i32 as u32).unwrap_or('?');
                    log::info!("Native Runtime: Execution finished (returned char: '{}' from i32: {}).", value, value_i32);
                    Ok(ExecutionResult::PrimitiveChar(value))
                }
                HirPrimitiveType::Unit => {
                     type EntryFnUnit = fn();
                     let entry_fn: EntryFnUnit = unsafe { mem::transmute(func_ptr) };
                             log::info!("Native Runtime: Executing JIT'd entry point (-> Primitive::Unit)...");
                     entry_fn();
                     log::info!("Native Runtime: Execution finished (returned unit).");
                     Ok(ExecutionResult::Unit)
                }
                HirPrimitiveType::String => {
                     log::error!("Native Runtime: Expected Handle return for String, but matched Primitive branch.");
                     Err(RuntimeError::ExecutionFailed)
                }
            }
        }
        _ => {
            log::error!("Native Runtime: Unsupported entry point type for native execution: {:?}", entry_type_clone);
            Err(RuntimeError::ExecutionFailed)
        }
            }
        })(); // End of immediate closure

        // --- Cleanup ---
        // Crucially, free the descriptor store memory *before* main_thread potentially shuts down GC internals.
        // Also clear the thread-local pointer.
        log::debug!("Native Runtime: Cleaning up descriptor store...");
        unsafe {
            let _ = Box::from_raw(store_ptr); // Take back ownership and drop the Box
            parallax_gc::clear_current_descriptor_store(); // Clear the thread-local pointer
        }
        log::info!("Native Runtime: Descriptor store cleaned up.");

        // Wrap the Result<ExecutionResult, RuntimeError> for the main_thread signature
        match execution_result {
            Ok(res) => Ok(res), // Closure returns Ok(ExecutionResult) on success
            Err(e) => Err(Box::new(e) as Box<dyn std::error::Error + Send + Sync>) // Closure returns Err(Box<dyn Error>) on failure
        }
    }); // End of main_thread closure

    log::info!("Native Runtime: Exited GC main_thread.");

    // Process the Result<Result<ExecutionResult, Box<dyn Error>>, thread::Error>
    match gc_thread_result {
        Ok(execution_result) => Ok(execution_result), // Return the inner ExecutionResult on success
        Err(gc_err) => { // GC thread failed
            log::error!("Native Runtime: GC main_thread error: {:?}", gc_err);
            Err(RuntimeError::Other(format!("GC thread error: {:?}", gc_err)))
        }
    }
} 