// tests/aggregates/tuple_literal.rs
use crate::*; // Import helpers from tests/mod.rs
use parallax_types::{
    error::TypeError,
    types::{PrimitiveType, Ty, TyKind, TypedExpr, TypedExprKind},
};
use parallax_resolve::types::{ResolvedExpr, ResolvedExprKind, ResolvedType};
use std::sync::Arc;

// Helper to create a ResolvedExpr for TupleLiteral
fn resolved_tuple_literal(elements: Vec<ResolvedExpr>) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Tuple(elements),
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown, // Type checker will determine this
    }
}

#[test]
fn test_tuple_empty() {
    let mut checker = setup_checker();
    let tuple_expr = resolved_tuple_literal(vec![]);
    let expected_ty = ty_prim(PrimitiveType::Unit); // Empty tuple is Unit

    let result = checker::expr::type_check_expression(&mut checker, &tuple_expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(matches!(typed_expr.kind, TypedExprKind::Tuple(ref elems) if elems.is_empty()));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_tuple_simple_match() {
    let mut checker = setup_checker();
    let tuple_expr = resolved_tuple_literal(vec![
        resolved_lit_int(1),
        resolved_lit_bool(true),
    ]);
    // Should infer to ({integer}, bool)
    let expected_ty = ty_tuple(vec![
        ty_prim(PrimitiveType::IntegerLiteral),
        ty_prim(PrimitiveType::Bool),
    ]);

    let result = checker::expr::type_check_expression(&mut checker, &tuple_expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(matches!(typed_expr.kind, TypedExprKind::Tuple(ref elems) if elems.len() == 2));
    if let TypedExprKind::Tuple(elems) = typed_expr.kind {
        assert_eq!(elems[0].ty, ty_prim(PrimitiveType::IntegerLiteral));
        assert_eq!(elems[1].ty, ty_prim(PrimitiveType::Bool));
    }
    assert!(checker.errors.is_empty());
}

#[test]
fn test_tuple_type_mismatch_in_element() {
    let mut checker = setup_checker();
    let tuple_expr = resolved_tuple_literal(vec![
        resolved_lit_int(1),
        resolved_lit_bool(true),
    ]);
    // Expect (i32, i32) - second element will mismatch
    let expected_ty = ty_tuple(vec![
        ty_prim(PrimitiveType::I32),
        ty_prim(PrimitiveType::I32),
    ]);

    let result = checker::expr::type_check_expression(&mut checker, &tuple_expr, Some(&expected_ty));
    assert!(result.is_err(), "Expected Err for tuple element type mismatch");
    assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
    // Expect one error from the bool vs i32 mismatch
    assert_eq!(checker.errors.len(), 1);
}

#[test]
fn test_tuple_coercion() {
    let mut checker = setup_checker();
    let tuple_expr = resolved_tuple_literal(vec![
        resolved_lit_int(10),
        resolved_lit_float(2.5),
    ]);
    // Expect ({integer}, {float}) to be coercible to (i64, f32)
    let expected_ty = ty_tuple(vec![
        ty_prim(PrimitiveType::I64),
        ty_prim(PrimitiveType::F32),
    ]);

    let result = checker::expr::type_check_expression(&mut checker, &tuple_expr, Some(&expected_ty));
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(matches!(typed_expr.kind, TypedExprKind::Tuple(ref elems) if elems.len() == 2));
    if let TypedExprKind::Tuple(elems) = typed_expr.kind {
        assert_eq!(elems[0].ty, ty_prim(PrimitiveType::I64));
        assert_eq!(elems[1].ty, ty_prim(PrimitiveType::F32));
    }
    assert!(checker.errors.is_empty());
}

#[test]
fn test_tuple_wrong_expected_type() {
    let mut checker = setup_checker();
    let tuple_expr = resolved_tuple_literal(vec![resolved_lit_int(1)]);
    let expected_ty = ty_prim(PrimitiveType::I32); // Expecting i32, not tuple

    let result = checker::expr::type_check_expression(&mut checker, &tuple_expr, Some(&expected_ty));
    assert!(result.is_err(), "Expected Err for tuple literal against non-tuple type");
    assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
}

#[test]
fn test_tuple_arity_mismatch() {
    let mut checker = setup_checker();
    let tuple_expr = resolved_tuple_literal(vec![resolved_lit_int(1), resolved_lit_bool(false)]); // Tuple has 2 elements
    let expected_ty = ty_tuple(vec![ty_prim(PrimitiveType::I32)]); // Expecting tuple with 1 element

    let result = checker::expr::type_check_expression(&mut checker, &tuple_expr, Some(&expected_ty));
    assert!(result.is_err(), "Expected Err for tuple arity mismatch");
    // Arity mismatch is often caught during unification
    assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
} 