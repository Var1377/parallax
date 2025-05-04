//! # Closure Pre-pass (`lowering::prepass`)
//!
//! Implements the pre-computation step for handling closures during HIR-to-MIR lowering.
//!
//! ## Purpose
//!
//! The main goal of this pass is to identify all closure definitions (`HirValue::Closure`)
//! within an [`HirModule`] *before* the main lowering process begins. For each unique
//! original closure definition (identified by its [`Symbol`]), this pass:
//!
//! 1.  **Assigns a Unique Specialization Symbol:** Generates a fresh [`Symbol`] (`specialized_symbol`)
//!     that will be used to identify the [`MirGraph`] generated for this closure's body.
//!     This allows multiple instances of the same closure definition in the source code to potentially
//!     (though not currently implemented) map to different specialized MIR graphs if their capture
//!     contexts differ significantly.
//! 2.  **Records Original Signature and Captures:** Stores the [`HirFunctionSignature`] of the
//!     original lambda function and the list of [`Operand`]s captured at the closure creation site.
//! 3.  **Populates the Specialization Map:** Adds an entry to the `closure_spec_map` (provided by
//!     [`lower_module`]), mapping the original lambda's [`Symbol`] to a [`ClosureSpecialization`]
//!     struct containing the generated `specialized_symbol`, the original signature, and the
//!     captured operands.
//!
//! ## Limitations & Future Work
//!
//! *   **Capture Types:** This pass *does not* determine the [`MirType`]s of the captured variables.
//!     These types might depend on the context where the closure is defined, which is only fully
//!     known during the main `lower_value` phase. The `capture_types` field in
//!     [`ClosureSpecialization`] is intentionally left empty by this pre-pass and filled later.
//! *   **Single Specialization:** The current implementation assumes only *one* specialized MIR graph
//!     is needed per original lambda definition. If future requirements demand different
//!     specializations based on varying capture types for the same lambda, this logic (and the
//!     `closure_spec_map` structure) would need refinement.
//!
//! ## Entry Point
//!
//! The main entry point is [`find_closures_in_expr`], which recursively traverses HIR expressions.

use super::*; // Import necessary items from parent `lowering` module

// --- Closure Specialization Info ---

/// Holds information gathered about a specific closure definition, used to guide
/// its transformation into a specialized MIR function graph.
///
/// This struct is populated partly by the `prepass` (assigning `specialized_symbol`,
/// recording `original_signature` and `captured_operands`) and partly by `lower_value`
/// (determining `capture_types`).
#[derive(Debug, Clone)]
pub struct ClosureSpecialization {
    /// A unique symbol generated to identify the specialized [`MirGraph`] for this closure.
    pub specialized_symbol: Symbol,
    /// The [`MirType`]s of the captured variables, in the order they appear in `captured_operands`.
    /// **Important:** This field is populated by [`expr::lower_value`] when it processes the
    /// corresponding `HirValue::Closure`, *not* during the pre-pass itself.
    pub capture_types: Vec<MirType>,
    /// A copy of the signature of the *original* HIR lambda function definition.
    pub original_signature: HirFunctionSignature,
    /// The list of [`Operand`]s captured when this closure was created in the HIR.
    /// This order corresponds to the `capture_types` and the initial parameters
    /// added to the specialized function's graph in [`module::lower_function`].
    pub captured_operands: Vec<Operand>,
}

// --- Pre-pass Traversal Functions ---

/// Recursively finds `HirValue::Closure` within an [`HirExpr`] and populates the `map`.
///
/// This is the main recursive function for the pre-pass, traversing `let` bindings
/// and tail expressions.
///
/// # Arguments
/// * `expr`: The HIR expression to traverse.
/// * `hir_module`: Read-only access to the full HIR module (needed to find original function defs).
/// * `map`: The mutable `ClosureSpecialization` map being built.
///
/// # Returns
/// * `Ok(())`: If traversal completes successfully.
/// * `Err(LoweringError)`: If an error occurs (e.g., finding a closure referencing a non-existent function).
pub(super) fn find_closures_in_expr(
    expr: &HirExpr,
    hir_module: &HirModule,
    map: &mut HashMap<Symbol, ClosureSpecialization>,
) -> Result<(), LoweringError> {
    match &expr.kind {
        HirExprKind::Let { value, rest, .. } => {
            // Recurse into the value being bound.
            find_closures_in_value(value, hir_module, map)?;
            // Recurse into the rest of the expression.
            find_closures_in_expr(rest, hir_module, map)?;
        }
        HirExprKind::Tail(tail_expr) => {
            // Recurse into the tail expression.
            find_closures_in_tail_expr(tail_expr, hir_module, map)?;
        }
    }
    Ok(())
}

/// Recursively finds `HirValue::Closure` within an [`HirValue`] and populates the `map`.
///
/// This function handles the different kinds of value computations.
/// When a `HirValue::Closure` is found, it checks if specialization info for the
/// original lambda symbol already exists in the map. If not, it creates a new
/// [`ClosureSpecialization`] entry, generating a fresh `specialized_symbol`.
fn find_closures_in_value(
    value: &HirValue,
    hir_module: &HirModule,
    map: &mut HashMap<Symbol, ClosureSpecialization>,
) -> Result<(), LoweringError> {
    match value {
        HirValue::Project { base, projection: ProjectionKind::ArrayIndex(index_op) } => {
             find_closures_in_operand(base, hir_module, map)?;
             find_closures_in_operand(index_op, hir_module, map)?;
        }
        // Values that might contain operands:
        HirValue::Use(op) |
        HirValue::Project { base: op, .. } => {
            find_closures_in_operand(op, hir_module, map)?; // Check the operand
        }
        HirValue::Call { func, args } => {
            find_closures_in_operand(func, hir_module, map)?; // Check function operand
            for arg in args {
                find_closures_in_operand(arg, hir_module, map)?; // Check argument operands
            }
        }
        HirValue::Aggregate { fields, .. } => {
            for field in fields {
                find_closures_in_operand(field, hir_module, map)?; // Check field operands
            }
        }
        // Special handling for Array Index in Projection, as it's an operand

        // The core case: Found a closure definition.
        HirValue::Closure { function_symbol, captures } => {
            // Check if we've already created specialization info for this original lambda.
            if !map.contains_key(function_symbol) {
                // First time encountering this lambda symbol in the pre-pass.
                let specialized_symbol = Symbol::fresh(); // Generate a unique symbol for the specialized MIR graph.

                // Find the original HIR function definition for the lambda's body.
                let original_func = hir_module.functions.iter()
                    .find(|f| f.symbol == *function_symbol)
                    .ok_or_else(|| {
                        // This indicates an inconsistency in the HIR.
                        LoweringError::ClosurePrepass(format!(
                            "Pre-pass: Closure {:?} references non-existent original function symbol {:?}",
                            value, function_symbol
                        ))
                    })?;

                // Create and store the specialization details.
                let spec = ClosureSpecialization {
                    specialized_symbol,
                    capture_types: Vec::new(), // NOTE: Capture types are determined later by lower_value.
                    original_signature: original_func.signature.clone(),
                    captured_operands: captures.clone(), // Record which operands were captured here.
                };
                map.insert(*function_symbol, spec);
            }
            // Recursively check the captured operands themselves. Although unlikely to contain
            // further closures in the current HIR structure, this ensures completeness.
            for capture in captures {
                 find_closures_in_operand(capture, hir_module, map)?;
            }
        }
    }
    Ok(())
}

/// Recursively checks if an [`Operand`] contains nested closure definitions.
///
/// In the current ANF-like HIR structure, Operands (`Var`, `Const`, `Global`) are simple
/// and do not contain nested expressions or closures themselves. This function is primarily
/// a placeholder for future HIR extensions or for completeness.
/// If Operands could become more complex, this function would need actual recursion.
fn find_closures_in_operand(
    operand: &Operand,
    _hir_module: &HirModule, // Currently unused
    _map: &mut HashMap<Symbol, ClosureSpecialization>, // Currently unused
) -> Result<(), LoweringError> {
    match operand {
        Operand::Var(_) => { /* No nested closures */ }
        Operand::Const(_) => { /* No nested closures */ }
        Operand::Global(_) => { /* No nested closures (HirValue::Closure handled directly) */ }
    }
    // If Operands could contain expressions: recurse here.
    // e.g., if Operand::ComplexExpr(expr) existed:
    // find_closures_in_expr(expr, _hir_module, _map)?;
    Ok(())
}

/// Recursively finds `HirValue::Closure` within an [`HirTailExpr`] and populates the `map`.
fn find_closures_in_tail_expr(
    tail_expr: &HirTailExpr,
    hir_module: &HirModule,
    map: &mut HashMap<Symbol, ClosureSpecialization>,
) -> Result<(), LoweringError> {
    match tail_expr {
        HirTailExpr::Value(op) => {
            find_closures_in_operand(op, hir_module, map)?;
        }
        HirTailExpr::If { condition, then_branch, else_branch } => {
            find_closures_in_operand(condition, hir_module, map)?;
            find_closures_in_expr(then_branch, hir_module, map)?;
            find_closures_in_expr(else_branch, hir_module, map)?;
        }
        HirTailExpr::Match { scrutinee, arms, otherwise } => {
            find_closures_in_operand(scrutinee, hir_module, map)?;
            for (_, arm_expr) in arms {
                find_closures_in_expr(arm_expr, hir_module, map)?;
            }
            if let Some(otherwise_expr) = otherwise {
                find_closures_in_expr(otherwise_expr, hir_module, map)?;
            }
        }
        HirTailExpr::Never => { /* No operands or sub-expressions */ }
    }
    Ok(())
} 