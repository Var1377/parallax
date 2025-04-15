// Contains helper functions for declaring runtime support functions (GC, etc.)

use crate::NativeError;
use cranelift_codegen::ir::{Function, Type, Signature, AbiParam, FuncRef};
use cranelift_jit::JITModule;
use cranelift_module::{Linkage, Module};

/// Declares the FFI signature for pushing a root onto the shadow stack.
/// The actual implementation lives in `parallax_gc`.
pub fn declare_shadow_stack_push_fn(
    jit_module: &mut JITModule,
    func: &mut Function, // Need Function to declare in func
    pointer_type: Type,
) -> Result<FuncRef, NativeError> { // Use imported FuncRef
    let mut sig = jit_module.make_signature();
    sig.params.push(AbiParam::new(pointer_type)); // ptr: *mut u8
    // Returns nothing
    let sig_ref = func.import_signature(sig);

    // Declare the function signature as an import
    let func_id = jit_module.declare_function("push_shadow_stack", Linkage::Import, &func.dfg.signatures[sig_ref])
        .map_err(|_| NativeError::FunctionDeclarationError)?; // Use specific error
    // Get a reference usable within the current function
    Ok(jit_module.declare_func_in_func(func_id, func))
}

/// Declares the FFI signature for popping roots from the shadow stack.
/// The actual implementation lives in `parallax_gc`.
pub fn declare_shadow_stack_pop_fn(
    jit_module: &mut JITModule,
    func: &mut Function,
    pointer_type: Type, // Assuming count is passed as pointer size integer
) -> Result<FuncRef, NativeError> { // Use imported FuncRef
    let mut sig = jit_module.make_signature();
    sig.params.push(AbiParam::new(pointer_type)); // count: usize
    // Returns nothing
    let sig_ref = func.import_signature(sig);

    let func_id = jit_module.declare_function("pop_shadow_stack", Linkage::Import, &func.dfg.signatures[sig_ref])
        .map_err(|_| NativeError::FunctionDeclarationError)?;
    Ok(jit_module.declare_func_in_func(func_id, func))
}

/// Declares the FFI signature for allocating a closure.
/// The actual implementation lives in `parallax_gc`.
pub fn declare_alloc_closure_fn(
    jit_module: &mut JITModule,
    func: &mut Function,
    pointer_type: Type,
) -> Result<FuncRef, NativeError> { // Use imported FuncRef
     let mut alloc_sig = jit_module.make_signature();
     alloc_sig.params.push(AbiParam::new(pointer_type)); // func_ptr: *const u8
     alloc_sig.params.push(AbiParam::new(pointer_type)); // captures_ptr: *const CaptureItem
     alloc_sig.params.push(AbiParam::new(pointer_type)); // captures_len: usize
     // Returns a Handle<ClosureRef>, which is treated as a tagged pointer
     alloc_sig.returns.push(AbiParam::new(pointer_type)); 
     let alloc_sig_ref = func.import_signature(alloc_sig);

     let func_id = jit_module.declare_function("parallax_alloc_closure", Linkage::Import, &func.dfg.signatures[alloc_sig_ref])
         .map_err(|_| NativeError::FunctionDeclarationError)?;
     Ok(jit_module.declare_func_in_func(func_id, func))
}

// TODO: Add declarations for other necessary GC FFI functions if used directly by translator:
// - parallax_alloc_string_ref(ptr, len) -> Handle<StringRef>
// - parallax_alloc_string_from_rust_buffer(ptr, len) -> Handle<StringRef> 