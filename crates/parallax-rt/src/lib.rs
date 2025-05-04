// Re-cleaned version: Only keep necessary imports for re-exports,
// module declarations, and the re-exports themselves.

// Imports potentially needed by consumers if types are re-exported (keep minimal)
// use parallax_hir::Symbol;
// use parallax_native::CompiledArtifact;
use parallax_hir::hir::PrimitiveType; // Import PrimitiveType

// Declare top-level modules
pub mod error;
pub mod runtime;
pub mod native;
pub mod inet;
// pub mod hybrid; // Declare later if implemented

// Re-export common error type
pub use error::RuntimeError;

// Re-export primary entry points for different runtime modes
pub use runtime::run_artifact;
pub use native::run_artifact as run_native_artifact; // Renamed for clarity
pub use inet::run_inet_partitioned_lazy; // Fixed function name

/// Represents the result of executing a compiled Parallax program.
#[derive(Debug)]
pub enum ExecutionResult {
    PrimitiveI8(i8),
    PrimitiveI16(i16),
    PrimitiveI32(i32),
    PrimitiveI64(i64),
    PrimitiveI128(i128),
    PrimitiveU8(u8),
    PrimitiveU16(u16),
    PrimitiveU32(u32),
    PrimitiveU64(u64),
    PrimitiveU128(u128),
    PrimitiveF32(f32),
    PrimitiveF64(f64),
    PrimitiveBool(bool),
    PrimitiveChar(char),
    Unit,
    GcHandle(usize), // Placeholder for future GC object returns
}
