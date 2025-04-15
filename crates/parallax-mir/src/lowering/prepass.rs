//!
//! Contains the closure pre-pass logic for identifying closures
//! and preparing specialization information.

use super::*;

// --- Closure Specialization Info --- (Moved from mod.rs)

/// Holds information about a specialized closure function.
#[derive(Debug, Clone)]
pub struct ClosureSpecialization {
    pub specialized_symbol: Symbol,
    /// Types of the captured variables (in order).
    /// This is populated by `lower_value` when it encounters the `HirValue::Closure`,
    /// *not* during the pre-pass itself.
    pub capture_types: Vec<MirType>,
    /// Signature of the *original* lambda function.
    pub original_signature: HirFunctionSignature,
    /// List of operands captured by the original closure value.
    /// Needed in lower_function to map captures to parameters.
    pub captured_operands: Vec<Operand>,
}

// --- Closure Pre-computation Pass --- (Moved from mod.rs)

/// Helper to recursively check operands during the pre-pass.
/// This identifies closures nested within other constructs (e.g., array index operands).
fn find_closures_in_operand(
    operand: &Operand,
    hir_module: &HirModule,
    map: &mut HashMap<Symbol, ClosureSpecialization>,
) -> Result<(), LoweringError> {
    match operand {
        Operand::Var(_) => {} // Vars themselves don't contain closures
        Operand::Const(_) => {} // Consts don't contain closures
        Operand::Global(_) => {} // Globals don't contain closures (we handle HirValue::Closure directly)
    }
    // In the future, if Operands could contain complex expressions (they don't in ANF),
    // we might need recursion here.
    Ok(())
}

/// Entry point for the closure pre-pass for a single expression.
pub(super) fn find_closures_in_expr(
    expr: &HirExpr,
    hir_module: &HirModule,
    map: &mut HashMap<Symbol, ClosureSpecialization>,
) -> Result<(), LoweringError> { // Return Result
    match &expr.kind {
        HirExprKind::Let { value, rest, .. } => {
            find_closures_in_value(value, hir_module, map)?; // Use ?
            find_closures_in_expr(rest, hir_module, map)?;  // Use ?
        }
        HirExprKind::Tail(tail_expr) => {
            find_closures_in_tail_expr(tail_expr, hir_module, map)?; // Use ?
        }
    }
    Ok(())
}

/// Pre-pass check for closures within an `HirValue`.
///
/// This is where `HirValue::Closure` is actually processed to populate the
/// `ClosureSpecialization` map.
fn find_closures_in_value(
    value: &HirValue,
    hir_module: &HirModule,
    map: &mut HashMap<Symbol, ClosureSpecialization>,
) -> Result<(), LoweringError> { // Return Result
    match value {
        HirValue::Use(op) => {
            find_closures_in_operand(op, hir_module, map)?; // Recurse
        }
        HirValue::Call { func, args } => {
            find_closures_in_operand(func, hir_module, map)?;
            for arg in args {
                find_closures_in_operand(arg, hir_module, map)?;
            }
        }
        HirValue::Aggregate { fields, .. } => {
            for field in fields {
                find_closures_in_operand(field, hir_module, map)?;
            }
        }
        HirValue::Project { base, projection } => {
            find_closures_in_operand(base, hir_module, map)?;
            if let ProjectionKind::ArrayIndex(index_op) = projection {
                find_closures_in_operand(index_op, hir_module, map)?;
            }
        }
        HirValue::Closure { function_symbol, captures } => {
            // Check if we've already processed this original lambda function
            if !map.contains_key(function_symbol) {
                let specialized_symbol = Symbol::fresh(); // Generate a unique symbol for the MIR function
                // Find the original HIR function definition for the lambda
                let original_func = hir_module.functions.iter()
                    .find(|f| f.symbol == *function_symbol)
                    .ok_or_else(|| LoweringError::Internal(format!(
                        "Closure references non-existent function symbol {:?}", function_symbol
                    )))?; // Use ? instead of expect

                // Store the specialization details
                let spec = ClosureSpecialization {
                    specialized_symbol,
                    capture_types: Vec::new(), // Will be populated later by lower_value
                    original_signature: original_func.signature.clone(),
                    captured_operands: captures.clone(), // Store the captured operands
                };
                map.insert(*function_symbol, spec);
            }
            // Recursively check the captured operands themselves for nested closures.
            for capture in captures {
                 find_closures_in_operand(capture, hir_module, map)?;
            }
        }
    }
    Ok(())
}

/// Pre-pass check for closures within an `HirTailExpr`.
fn find_closures_in_tail_expr(
    tail_expr: &HirTailExpr,
    hir_module: &HirModule,
    map: &mut HashMap<Symbol, ClosureSpecialization>,
) -> Result<(), LoweringError> { // Return Result
    match tail_expr {
        HirTailExpr::Return(op) => {
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
        HirTailExpr::Never => {} // No operands or sub-expressions
    }
    Ok(())
} 