// tests/aggregates/map_literal.rs
use crate::*; // Import helpers from tests/mod.rs
use parallax_types::{
    error::TypeError,
    types::{PrimitiveType, Ty, TyKind, TypedExpr, TypedExprKind},
};
use parallax_resolve::types::{ResolvedExpr, ResolvedExprKind, ResolvedType};
use std::sync::Arc;

// Helper to create a ResolvedExpr for Map literal
fn resolved_map_literal(entries: Vec<(ResolvedExpr, ResolvedExpr)>) -> ResolvedExpr {
    // For simplicity, assume the resolver marks map literals as Unknown.
    // The type checker will infer the key/value types.
    ResolvedExpr {
        kind: ResolvedExprKind::Map(entries),
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    }
}

#[test]
fn test_map_empty_no_context() {
    let mut checker = setup_checker();
    let map_expr = resolved_map_literal(vec![]);

    let result = checker::expr::type_check_expression(&mut checker, &map_expr, None);
    assert!(result.is_ok(), "Expected Ok for empty map, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    // Empty map with no context infers to Map<K, V> where K and V are fresh vars
    if let TyKind::Map(key_ty, val_ty) = typed_expr.ty.kind {
        assert!(matches!(key_ty.kind, TyKind::Var(_)), "Expected key type var, got {:?}", key_ty.kind);
        assert!(matches!(val_ty.kind, TyKind::Var(_)), "Expected value type var, got {:?}", val_ty.kind);
    } else {
        panic!("Expected Map type, got {:?}", typed_expr.ty.kind);
    }
    assert!(checker.errors.is_empty());
}

#[test]
fn test_map_empty_with_context() {
    let mut checker = setup_checker();
    let map_expr = resolved_map_literal(vec![]);
    let expected_ty = ty_map(ty_prim(PrimitiveType::String), ty_prim(PrimitiveType::I32));

    let result = checker::expr::type_check_expression(&mut checker, &map_expr, Some(&expected_ty));
    assert!(result.is_ok(), "Expected Ok for empty map with context, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(checker.errors.is_empty());
}

#[test]
fn test_map_simple() {
    let mut checker = setup_checker();
    let map_expr = resolved_map_literal(vec![
        (resolved_lit_string("a"), resolved_lit_int(1)),
        (resolved_lit_string("b"), resolved_lit_int(2)),
    ]);
    // Should infer Map<String, IntegerLiteral>
    let expected_ty = ty_map(ty_prim(PrimitiveType::String), ty_prim(PrimitiveType::IntegerLiteral));

    let result = checker::expr::type_check_expression(&mut checker, &map_expr, None);
    assert!(result.is_ok(), "Expected Ok for simple map, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(matches!(typed_expr.kind, TypedExprKind::Map(ref entries) if entries.len() == 2));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_map_key_type_mismatch() {
    let mut checker = setup_checker();
    let map_expr = resolved_map_literal(vec![
        (resolved_lit_string("a"), resolved_lit_int(1)),
        (resolved_lit_int(0), resolved_lit_int(2)), // Key mismatch: String vs Int
    ]);

    let result = checker::expr::type_check_expression(&mut checker, &map_expr, None);
    assert!(result.is_err(), "Expected Err for map key type mismatch");
    assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
    assert_eq!(checker.errors.len(), 1);
}

#[test]
fn test_map_value_type_mismatch() {
    let mut checker = setup_checker();
    let map_expr = resolved_map_literal(vec![
        (resolved_lit_string("a"), resolved_lit_int(1)),
        (resolved_lit_string("b"), resolved_lit_bool(true)), // Value mismatch: Int vs Bool
    ]);

    let result = checker::expr::type_check_expression(&mut checker, &map_expr, None);
    assert!(result.is_err(), "Expected Err for map value type mismatch");
    assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
    assert_eq!(checker.errors.len(), 1);
}

#[test]
fn test_map_coercion() {
    let mut checker = setup_checker();
    let map_expr = resolved_map_literal(vec![
        (resolved_lit_string("one"), resolved_lit_int(1)),
        (resolved_lit_string("two"), resolved_lit_int(2)),
    ]);
    // Expect coercion to Map<String, i32>
    let expected_ty = ty_map(ty_prim(PrimitiveType::String), ty_prim(PrimitiveType::I32));

    let result = checker::expr::type_check_expression(&mut checker, &map_expr, Some(&expected_ty));
    assert!(result.is_ok(), "Expected Ok for coerced map, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    if let TypedExprKind::Map(entries) = typed_expr.kind {
        assert_eq!(entries[0].0.ty, ty_prim(PrimitiveType::String));
        assert_eq!(entries[0].1.ty, ty_prim(PrimitiveType::I32));
        assert_eq!(entries[1].0.ty, ty_prim(PrimitiveType::String));
        assert_eq!(entries[1].1.ty, ty_prim(PrimitiveType::I32));
    } else {
        panic!("Expected Map kind");
    }
    assert!(checker.errors.is_empty());
} 