//! # Parallax MIR (Mid-level Intermediate Representation)
//!
//! This crate defines the Mid-level Intermediate Representation (MIR) used in the
//! Parallax compiler. It serves as a bridge between the High-Level IR (HIR)
//! and subsequent backend phases like optimization or code generation (e.g., lowering
//! to interaction nets or LLVM).
//!
//! ## Core Components
//!
//! *   **[`mir`] Module:** Defines the core MIR data structures, primarily:
//!     *   [`MirModule`]: Represents a whole compiled crate.
//!     *   [`MirGraph`]: Represents a function as a dataflow graph.
//!     *   [`MirNode`]: The different kinds of operations/values in the graph.
//!     *   [`MirType`], [`MirStructDef`], [`MirEnumDef`], etc.: MIR-level type definitions.
//! *   **[`lowering`] Module:** Contains the logic to transform an [`HirModule`]
//!     (from the `parallax-hir` crate) into a [`MirModule`]. The main entry point
//!     for this process is the [`lower_module`] function.
//!
//! ## Usage
//!
//! The primary way to interact with this crate is typically through the
//! [`lower_module`] function, which takes an `&HirModule` and returns a
//! `Result<MirModule, LoweringError>`.
//!
//! ```ignore
//! // Example (conceptual)
//! use parallax_hir::HirModule;
//! use parallax_mir::{lower_module, MirModule};
//! # use parallax_mir::LoweringError;
//! 
//! fn compile_to_mir(hir_module: &HirModule) -> Result<MirModule, LoweringError> {
//!     let mir_module = lower_module(hir_module)?;
//!     // ... further processing on mir_module ...
//!     Ok(mir_module)
//! }
//! ```

// Public modules
pub mod mir;
pub mod lowering;

// Re-export key items for easier access
pub use mir::*; // Re-export all items from the mir module (structs, enums, etc.)
pub use lowering::lower_module; // Re-export the main lowering entry point
pub use lowering::LoweringError; // Re-export the error type 