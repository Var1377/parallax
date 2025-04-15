use crate::CodegenError;
use parallax_hir::{HirModule, Symbol};
use parallax_native::{compile_hir, CompiledArtifact};
use std::collections::HashMap;

/// Represents the output of the code generation process.
#[derive(Debug)]
pub struct CompiledOutput {
    /// The native artifact produced (e.g., machine code, object file).
    pub native_artifact: CompiledArtifact,
    // TODO: Add other potential outputs (e.g., interaction net representation)
}

impl CompiledOutput {
    /// Get a function pointer for a specified symbol
    pub fn get_function_ptr(&self, symbol: Symbol) -> Option<*const u8> {
        self.native_artifact.get_function_ptr(symbol)
    }
    
    /// Cast a function pointer to a callable Rust function.
    /// 
    /// # Safety
    ///
    /// This is unsafe because the caller must ensure:
    /// 1. The function signature matches what was compiled
    /// 2. The function is correctly implemented and won't cause undefined behavior
    /// 3. The JIT module is still alive (contained in self)
    pub unsafe fn get_function<F>(&self, symbol: Symbol) -> Option<F> 
    where
        F: Copy, // Function pointers are Copy
    {
        self.native_artifact.get_function::<F>(symbol)
    }
    
    /// Helper to call a nullary (no arguments) function that returns an i64
    /// 
    /// # Safety
    ///
    /// Caller must ensure the function was compiled with a matching signature
    pub unsafe fn call_nullary_i64(&self, symbol: Symbol) -> Option<i64> {
        self.native_artifact.call_nullary_i64(symbol)
    }
}

/// Orchestrates the code generation process for a given HIR module.
///
/// This function currently delegates to the native backend.
pub fn generate_module(hir_module: &HirModule) -> Result<CompiledOutput, CodegenError> {
    println!("Starting native code generation...");

    let native_artifact = compile_hir(hir_module)?;

    println!("Native code generation finished.");

    // TODO: Potentially invoke other backends (like interaction nets) here

    Ok(CompiledOutput {
        native_artifact,
        // Initialize other fields if they exist
    })
} 