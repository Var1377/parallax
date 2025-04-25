//! Tests for expected error conditions during lowering.

use super::helpers::*;
use crate::lowering::LoweringError;
use crate::mir::*;
use parallax_hir::hir::{ HirFunction, HirFunctionSignature, HirExpr, HirExprKind, HirTailExpr, HirValue, Operand, HirLiteral, HirVar, AggregateKind, HirType, PrimitiveType};
use parallax_resolve::types::{PrimitiveType as ResolvePrimitiveType, Symbol};

#[test]
fn test_lower_call_unbound_var() {
    let main_symbol = Symbol::new(0);
    let unbound_var_id = 207; // Match ID from user error
    let unbound_var = HirVar(unbound_var_id);
    let hir_i32_ty = PrimitiveType::I32;
    let i32_ty = HirType::Primitive(hir_i32_ty);
    let hir_i64_ty = PrimitiveType::I64;

    let var_res = HirVar(208);

    // fn main() -> i32 { let res = unbound_var(1, 1i64); res }
    // This HIR is intentionally malformed because unbound_var is never defined.
    let main_func = HirFunction {
        symbol: main_symbol,
        name: "main".to_string(),
        signature: HirFunctionSignature {
            params: vec![],
            return_type: i32_ty.clone(),
            is_effectful: false,
        },
        body: Some(HirExpr {
            kind: HirExprKind::Let {
                var: var_res,
                var_ty: i32_ty.clone(),
                value: Box::new(HirValue::Call {
                    // Critical: Calling an Operand::Var that has no binding
                    func: Operand::Var(unbound_var),
                    args: vec![
                        Operand::Const(HirLiteral::IntLiteral { value: 1, ty: hir_i32_ty }),
                        Operand::Const(HirLiteral::IntLiteral { value: 1, ty: hir_i64_ty }),
                    ],
                }),
                rest: Box::new(HirExpr {
                    kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(var_res))),
                    ty: i32_ty.clone(),
                    span: dummy_span(),
                }),
            },
            ty: i32_ty.clone(),
            span: dummy_span(),
        }),
        span: dummy_span(),
    };

    let hir_module = create_test_module(main_func.clone());

    // --- Assertion: Expect UndefinedVariable error ---
    let result = test_lower_function(&hir_module, &main_func);
    assert!(matches!(result, Err(LoweringError::UndefinedVariable(v)) if v == unbound_var),
            "Expected UndefinedVariable error for {:?}, got {:?}", unbound_var, result);
}

// TODO: Add more error tests:
// - TypeMismatch (e.g., calling non-function, if branches mismatch, projection from wrong type)
// - Unsupported (e.g., non-exhaustive match without otherwise, const pattern in match)
// - SymbolNotFound (although less likely with HIR->MIR directly)
// - Layout errors (e.g., recursive struct without indirection - might need specific setup) 