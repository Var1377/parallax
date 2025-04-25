// Contains helper functions for declaring runtime support functions (GC, etc.)

use crate::NativeError;
use cranelift_codegen::ir::{Function, Type, Signature, AbiParam, FuncRef};
use cranelift_jit::JITModule;
use cranelift_module::{Linkage, Module};
use parallax_gc::LayoutDescriptor; // Use LayoutDescriptor instead of TypeDescriptor

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
     alloc_sig.params.push(AbiParam::new(pointer_type)); // env_handle: *mut u8
     // Returns a tagged closure handle (LSB set to 1)
     alloc_sig.returns.push(AbiParam::new(pointer_type)); 
     let alloc_sig_ref = func.import_signature(alloc_sig);

     let func_id = jit_module.declare_function("parallax_alloc_closure", Linkage::Import, &func.dfg.signatures[alloc_sig_ref])
         .map_err(|_| NativeError::FunctionDeclarationError)?;
     Ok(jit_module.declare_func_in_func(func_id, func))
}

/// Declares the FFI signature for allocating raw, untraced memory (leaky).
/// The actual implementation lives in `parallax_gc`.
/// WARNING: This is a temporary development function. Memory is NOT GC-managed.
/// TODO: Remove this once proper GC allocation is integrated.
pub fn declare_leaky_alloc_fn(
    jit_module: &mut JITModule,
    func: &mut Function,
    pointer_type: Type, // Assuming size/align are passed as pointer size integers
) -> Result<FuncRef, NativeError> {
    let mut sig = jit_module.make_signature();
    sig.params.push(AbiParam::new(pointer_type)); // size: usize
    sig.params.push(AbiParam::new(pointer_type)); // align: usize
    sig.returns.push(AbiParam::new(pointer_type)); // Returns *mut u8
    let sig_ref = func.import_signature(sig);

    let func_id = jit_module.declare_function("parallax_gc_alloc", Linkage::Import, &func.dfg.signatures[sig_ref])
        .map_err(|_| NativeError::FunctionDeclarationError)?;
    Ok(jit_module.declare_func_in_func(func_id, func))
}

/// Declares the FFI signature for allocating a generic GC object.
/// The actual implementation lives in `parallax_gc`.
pub fn declare_alloc_object_fn(
    jit_module: &mut JITModule,
    func: &mut Function,
    pointer_type: Type, // Assuming pointers are the native pointer type
) -> Result<FuncRef, NativeError> {
    let mut sig = jit_module.make_signature();
    // Arg 0: descriptor_index: DescriptorIndex (represented as pointer_type)
    sig.params.push(AbiParam::new(pointer_type));
    // Arg 1: init_data_ptr: *const u8 (represented as pointer_type)
    sig.params.push(AbiParam::new(pointer_type));
    // Returns: Handle<GcObject> (represented as pointer_type)
    sig.returns.push(AbiParam::new(pointer_type));
    let sig_ref = func.import_signature(sig);

    let func_id = jit_module.declare_function(
        "parallax_alloc_object",
        Linkage::Import,
        &func.dfg.signatures[sig_ref]
    ).map_err(|_| NativeError::FunctionDeclarationError)?;

    Ok(jit_module.declare_func_in_func(func_id, func))
}

/// Declares the FFI signature for allocating a StringRef from a Rust buffer.
/// The actual implementation lives in `parallax_gc`.
pub fn declare_alloc_string_from_buffer_fn(
    jit_module: &mut JITModule,
    func: &mut Function,
    pointer_type: Type, // Assuming pointers are the native pointer type
) -> Result<FuncRef, NativeError> {
    let mut sig = jit_module.make_signature();
    // Arg 0: buffer_ptr: *const u8 (represented as pointer_type)
    sig.params.push(AbiParam::new(pointer_type));
    // Arg 1: buffer_len: usize (represented as pointer_type)
    sig.params.push(AbiParam::new(pointer_type));
    // Returns: Handle<StringRef> (represented as pointer_type, assuming StringRef is now GcObject)
    sig.returns.push(AbiParam::new(pointer_type));
    let sig_ref = func.import_signature(sig);

    let func_id = jit_module.declare_function(
        "parallax_alloc_string_from_rust_buffer",
        Linkage::Import,
        &func.dfg.signatures[sig_ref]
    ).map_err(|_| NativeError::FunctionDeclarationError)?;

    Ok(jit_module.declare_func_in_func(func_id, func))
}

/// Declares the FFI signature for allocating an array.
/// The actual implementation lives in `parallax_gc`.
pub fn declare_alloc_array_fn(
    jit_module: &mut JITModule,
    func: &mut Function,
    pointer_type: Type, // Assuming pointers are the native pointer type
) -> Result<FuncRef, NativeError> {
    let mut sig = jit_module.make_signature();
    // Arg 0: element_descriptor_index: DescriptorIndex (represented as pointer_type)
    sig.params.push(AbiParam::new(pointer_type));
    // Arg 1: capacity: usize (represented as pointer_type)
    sig.params.push(AbiParam::new(pointer_type));
    // Returns: Handle<GcRawArray> (represented as pointer_type)
    sig.returns.push(AbiParam::new(pointer_type));
    let sig_ref = func.import_signature(sig);

    let func_id = jit_module.declare_function(
        "parallax_alloc_array",
        Linkage::Import,
        &func.dfg.signatures[sig_ref]
    ).map_err(|_| NativeError::FunctionDeclarationError)?;

    Ok(jit_module.declare_func_in_func(func_id, func))
}

// TODO: Add declarations for other necessary GC FFI functions if used directly by translator:
// - parallax_alloc_string_ref(ptr, len) -> Handle<StringRef> (now likely GcObject)
// - parallax_write_field(container_handle, offset, value_handle) 