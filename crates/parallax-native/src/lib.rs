pub mod backend;
pub mod error;
pub mod translator;

// --- Runtime Type Representation (Still needed by translator/backend) --- 

/// Simple struct to represent a string reference at runtime.
/// Corresponds to { pointer: *const u8, length: usize }
/// NOTE: The actual GC-managed StringRef struct lives in `parallax-gc`.
/// This definition might be needed temporarily for layout calculation or FFI signatures
/// before full integration. Ideally, this could be removed later if `parallax-gc`'s types
/// can be used directly or via a shared types crate.
#[repr(C)] // Ensure predictable layout
#[derive(Debug, Copy, Clone)]
pub struct StringRef {
    pub ptr: *const u8,
    pub len: usize,
}

/// Simple struct to represent a closure reference at runtime.
/// Corresponds to { function_pointer: Ptr, environment: Ptr }
/// NOTE: The actual GC-managed ClosureRef struct lives in `parallax-gc`.
/// See notes for `StringRef` above.
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct ClosureRef {
    pub func_ptr: *const u8, // Pointer to the lambda's function body
    pub env_ptr: *const u8, // Pointer to the captured environment data (Handle bits)
}

// --- Exports --- 
pub use backend::{compile_hir, CompiledArtifact};
pub use error::NativeError;
