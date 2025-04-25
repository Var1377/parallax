// tests/aggregates/hashset_literal.rs
use crate::*; // Import helpers from tests/mod.rs
use parallax_types::{
    error::TypeError,
    types::{PrimitiveType, Ty, TyKind, TypedExpr, TypedExprKind},
};
use parallax_resolve::types::{ResolvedExpr, ResolvedExprKind, ResolvedType};
use std::sync::Arc;

// Helper to create a ResolvedExpr for HashSet literal
fn resolved_hashset_literal(elements: Vec<ResolvedExpr>) -> ResolvedExpr {
    // Assume resolver marks as Unknown
    ResolvedExpr {
        kind: ResolvedExprKind::HashSet(elements),
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    }
}

#[test]
fn test_hashset_empty_no_context() {
    let mut checker = setup_checker();
    let set_expr = resolved_hashset_literal(vec![]);

    let result = checker::expr::type_check_expression(&mut checker, &set_expr, None);
    assert!(result.is_ok(), "Expected Ok for empty set, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    // Empty set infers to Set<T> where T is a fresh var
    if let TyKind::Set(elem_ty) = typed_expr.ty.kind {
        assert!(matches!(elem_ty.kind, TyKind::Var(_)), "Expected element type var, got {:?}", elem_ty.kind);
    } else {
        panic!("Expected Set type, got {:?}", typed_expr.ty.kind);
    }
    assert!(checker.errors.is_empty());
}

#[test]
fn test_hashset_empty_with_context() {
    let mut checker = setup_checker();
    let set_expr = resolved_hashset_literal(vec![]);
    let expected_ty = ty_set(ty_prim(PrimitiveType::I32));

    let result = checker::expr::type_check_expression(&mut checker, &set_expr, Some(&expected_ty));
    assert!(result.is_ok(), "Expected Ok for empty set with context, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(checker.errors.is_empty());
}

#[test]
fn test_hashset_simple() {
    let mut checker = setup_checker();
    let set_expr = resolved_hashset_literal(vec![
        resolved_lit_int(1),
        resolved_lit_int(2),
        resolved_lit_int(1),
    ]);
    // Should infer Set<IntegerLiteral>
    let expected_ty = ty_set(ty_prim(PrimitiveType::IntegerLiteral));

    let result = checker::expr::type_check_expression(&mut checker, &set_expr, None);
    assert!(result.is_ok(), "Expected Ok for simple set, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(matches!(typed_expr.kind, TypedExprKind::HashSet(ref elems) if elems.len() == 3)); // Check original number of elements
    assert!(checker.errors.is_empty());
}

#[test]
fn test_hashset_type_mismatch() {
    let mut checker = setup_checker();
    let set_expr = resolved_hashset_literal(vec![
        resolved_lit_int(1),
        resolved_lit_bool(true), // Mismatch: Int vs Bool
    ]);

    let result = checker::expr::type_check_expression(&mut checker, &set_expr, None);
    assert!(result.is_err(), "Expected Err for set type mismatch");
    assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
    assert_eq!(checker.errors.len(), 1);
}

#[test]
fn test_hashset_coercion() {
    let mut checker = setup_checker();
    let set_expr = resolved_hashset_literal(vec![
        resolved_lit_int(10),
        resolved_lit_int(20),
    ]);
    // Expect coercion to Set<f32>
    let expected_ty = ty_set(ty_prim(PrimitiveType::F32));

    let result = checker::expr::type_check_expression(&mut checker, &set_expr, Some(&expected_ty));
    assert!(result.is_ok(), "Expected Ok for coerced set, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    if let TypedExprKind::HashSet(elems) = typed_expr.kind {
        assert_eq!(elems[0].ty, ty_prim(PrimitiveType::F32));
        assert_eq!(elems[1].ty, ty_prim(PrimitiveType::F32));
    } else {
        panic!("Expected HashSet kind");
    }
    assert!(checker.errors.is_empty());
} 