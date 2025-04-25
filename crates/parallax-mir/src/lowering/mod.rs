//! # MIR Lowering (`parallax-mir::lowering`)
//!
//! This module orchestrates the transformation of the High-Level Intermediate Representation (HIR)
//! of a Parallax program into the graph-based Mid-Level Intermediate Representation (MIR).
//! The MIR is designed to be closer to a dataflow or interaction net execution model,
//! facilitating analysis, optimization, and eventual code generation for various backends.
//!
//! ## Key Concepts & Design
//!
//! *   **Dataflow Graph:** The primary MIR structure ([`MirGraph`]) represents functions as directed graphs
//!     where nodes ([`MirNode`]) are operations or values, and edges ([`MirEdge`]) signify the flow of
//!     data between node ports ([`PortIndex`]). This contrasts with traditional CFG-based IRs that
//!     emphasize control flow through basic blocks.
//! *   **Closure Specialization:** A key transformation performed during lowering. HIR closures
//!     (`HirValue::Closure`) are not directly represented in MIR. Instead, each distinct closure
//!     *instance* (considering its captured variables) potentially generates a *specialized*
//!     [`MirGraph`]. Captured variables are treated as additional leading parameters to this
//!     specialized graph. A pre-pass (`prepass` submodule) identifies closures and their captures
//!     to prepare for this specialization.
//! *   **Lowering Context:** The [`FunctionLoweringContext`] (`context` submodule) encapsulates the state
//!     needed to lower a single function body (either a regular function or a specialized closure).
//!     It manages the [`MirGraph`] construction, maps HIR variables to MIR nodes/ports, provides
//!     type lowering utilities, and interacts with the layout computer.
//! *   **Layout Computation:** Relies on the `parallax-layout` crate (via [`LayoutComputer`]) to
//!     determine the memory layout of structs and enums, storing this information in
//!     [`MirStructDef`] and [`MirEnumDef`].
//!
//! ## Overall Lowering Process ([`lower_module`])
//!
//! 1.  **Closure Pre-computation (`prepass`):** Traverses the entire HIR module to find all
//!     `HirValue::Closure` instances. For each unique original closure definition ([`Symbol`]), it
//!     records details like captured operands and assigns a new, unique `Symbol` for the potential
//!     specialized [`MirGraph`]. This information is stored in a `ClosureSpecialization` map.
//!     Crucially, it *doesn't* compute capture types yet.
//! 2.  **Struct/Enum Definition Lowering:** Iterates through HIR struct and enum definitions.
//!     For each, it computes the memory layout using the [`LayoutComputer`] and creates the
//!     corresponding [`MirStructDef`] or [`MirEnumDef`], lowering field/variant types using
//!     [`types::lower_hir_type_to_mir_type`].
//! 3.  **Regular Function Lowering:** Iterates through HIR functions.
//!     *   If a function has a body and is *not* an original closure definition (identified in the pre-pass),
//!         it calls [`lower_function`] to generate its [`MirGraph`]. The `target_graph_symbol` passed
//!         is the function's own `Symbol`.
//!     *   If a function is `extern` (no body), an empty [`MirGraph`] is created.
//! 4.  **Closure Specialization Lowering:** Iterates through the `ClosureSpecialization` map populated
//!     by the pre-pass. For each entry:
//!     *   Retrieves the *original* HIR function definition corresponding to the closure's body.
//!     *   Calls [`lower_function`] using the *original* body but passing the *specialized* `Symbol`
//!         as the `target_graph_symbol`.
//!     *   Inside `lower_function` (and specifically `lower_value` when it encounters the `HirValue::Closure`),
//!         the capture types are determined and stored back in the `ClosureSpecialization` map. This handles
//!         cases where the types might depend on the context where the closure is defined.
//!     *   The resulting [`MirGraph`] represents the specialized closure body, taking captures as parameters.
//! 5.  **Static Variable Lowering:** Lowers [`HirGlobalStatic`] definitions to [`MirGlobalStatic`].
//!     Handles simple literal initializers directly. Complex initializers might require future extensions.
//! 6.  **Module Assembly:** Collects all generated [`MirGraph`]s, [`MirStructDef`]s, [`MirEnumDef`]s,
//!     and [`MirGlobalStatic`]s into the final [`MirModule`].
//!
//! ## Submodules
//!
//! *   [`context`]: Defines [`FunctionLoweringContext`] for managing state during single-function lowering.
//! *   [`expr`]: Contains the core logic (`lower_value`, `lower_expr`, `lower_operand`, etc.) for translating HIR expressions into MIR graph nodes and edges.
//! *   [`module`]: Provides the main entry points ([`lower_module`], [`lower_function`]).
//! *   [`prepass`]: Implements the closure identification and specialization pre-pass.
//! *   [`types`]: Handles the translation of [`HirType`] to [`MirType`].
//! *   [`tests`]: Contains unit tests for the lowering functionality.

// Module declarations
pub mod context;
pub mod expr;
pub mod module; // Contains the actual lower_module/lower_function implementations
pub mod prepass;
pub mod types;
#[cfg(test)]
pub mod tests;

// Re-exports for convenience
pub use module::lower_module; // Re-export the public interface
// pub use module::lower_function; // Keep lower_function internal to the lowering module

// Internal imports used across the lowering submodules
// Remove old layout/native imports
// use parallax_native::translator::layout::{self as layout, get_enum_discriminant_type, get_layout, get_variant_payload_layout, get_variant_payload_offset_bytes, LayoutComputer, TranslationContext, LayoutError};
// use parallax_native::translator::layout::{self as layout, get_enum_discriminant_type, get_layout, get_variant_payload_layout, get_variant_payload_offset_bytes, LayoutComputer};
// use parallax_native::error::NativeError;

// Add imports for new GC layout types
use parallax_gc::{layout::LayoutError, DescriptorStore};

use crate::mir::*; // Bring all MIR definitions into scope
use context::FunctionLoweringContext; // Context is frequently used
// Correct hir import: using the dependency directly
// Correct Literal import again
// use parallax_hir::{self as hir, HirFunction, HirModule, HirType, hir::HirLiteral, Operand, Symbol, HirValue, HirFunctionSignature, HirExpr, HirExprKind, HirTailExpr, ProjectionKind, HirVar};
use parallax_hir::{self as hir, hir::HirFunction, hir::HirModule, hir::HirType, hir::HirLiteral, hir::Operand, hir::HirValue, hir::HirFunctionSignature, hir::HirExpr, hir::HirExprKind, hir::HirTailExpr, hir::ProjectionKind, hir::HirVar, hir::HirPattern, hir::AggregateKind};
// Correct resolve import: using the dependency directly
// use parallax_resolve::types::PrimitiveType as ResolvePrimitiveType; // Alias for clarity
use parallax_resolve::types::{PrimitiveType as ResolvePrimitiveType, Symbol};
use std::{collections::HashMap, sync::Arc};
use prepass::ClosureSpecialization; // Needed for closure handling


/// Represents errors that can occur during the HIR-to-MIR lowering process.
#[derive(Debug, thiserror::Error)]
pub enum LoweringError {
    /// An error occurred during memory layout computation for a type.
    /// This typically originates from the `parallax-layout` crate.
    #[error("Layout computation error: {0}")]
    Layout(#[from] LayoutError), // Use parallax_gc::LayoutError

    /// An attempt was made to use an HIR variable (`HirVar`) or temporary
    /// that was not defined or available in the current lowering scope.
    /// This often indicates a logic error in the preceding HIR phases or in the lowering context.
    #[error("Undefined variable or temporary used: {0:?}")]
    UndefinedVariable(hir::HirVar), // Use qualified path

    /// A type mismatch was detected during lowering, making an operation invalid.
    /// For example, attempting to call a non-function value, or providing arguments
    /// of the wrong type to a node.
    #[error("Type mismatch: {0}")]
    TypeMismatch(String), // Keep message flexible

    /// The lowering process encountered an HIR construct or feature that it
    /// currently does not support translating to MIR.
    #[error("Unsupported feature during lowering: {0}")]
    Unsupported(String),

    /// An internal invariant or assertion failed within the lowering code,
    /// indicating a bug in the lowering logic itself.
    #[error("Internal lowering error: {0}")]
    Internal(String),

    /// A required item (like a function, struct, or enum definition) referenced
    /// by a [`Symbol`] could not be found within the [`HirModule`].
    #[error("Symbol definition not found: {0:?}")]
    SymbolNotFound(hir::Symbol), // Use qualified path

    /// An error occurred specifically during the closure pre-pass analysis.
    /// This might involve issues resolving closure function symbols or other
    /// inconsistencies detected before main lowering begins.
    #[error("Closure pre-pass error: {0}")]
    ClosurePrepass(String),

    // Placeholder for potential future errors related to more complex features.
    // #[error("Borrow checking error during lowering: {0}")]
    // BorrowCheck(String),
}