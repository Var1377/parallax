// tests/aggregates/array_literal.rs
use crate::*; // Import helpers from tests/mod.rs
use parallax_types::{
    error::TypeError,
    types::{PrimitiveType, Ty, TyKind, TypedExpr, TypedExprKind},
};
use parallax_resolve::types::{ResolvedExpr, ResolvedExprKind, ResolvedType};
use std::sync::Arc;

// Helper to create a ResolvedExpr for ArrayLiteral
fn resolved_array_literal(elements: Vec<ResolvedExpr>) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Array(elements),
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown, // Type checker will determine this
    }
}


#[test]
fn test_array_empty_no_context() {
    let mut checker = setup_checker();
    let array_expr = resolved_array_literal(vec![]);

    let result = checker::expr::type_check_expression(&mut checker, &array_expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    // An empty array literal with no context should infer to [?; 0] (unknown element type)
    // Check if the type is Array with size 0 and element type is a variable
    if let TyKind::Array(elem_ty, size) = typed_expr.ty.kind {
        assert_eq!(size, 0);
        assert!(matches!(elem_ty.kind, TyKind::Var(_)), "Expected element type to be inferred variable, got {:?}", elem_ty.kind);
    } else {
        panic!("Expected Array type, got {:?}", typed_expr.ty.kind);
    }
    assert!(checker.errors.is_empty());
}

#[test]
fn test_array_empty_with_context() {
    let mut checker = setup_checker();
    let array_expr = resolved_array_literal(vec![]);
    let expected_ty = ty_array(ty_prim(PrimitiveType::String), 0); // Expect [String; 0]

    let result = checker::expr::type_check_expression(&mut checker, &array_expr, Some(&expected_ty));
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(checker.errors.is_empty());
}


#[test]
fn test_array_simple_match() {
    let mut checker = setup_checker();
    let array_expr = resolved_array_literal(vec![
        resolved_lit_int(1),
        resolved_lit_int(2),
    ]);
    // Should infer to IntegerLiteral initially
    let expected_ty = ty_array(ty_prim(PrimitiveType::IntegerLiteral), 2);

    let result = checker::expr::type_check_expression(&mut checker, &array_expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(matches!(typed_expr.kind, TypedExprKind::Array(ref elems) if elems.len() == 2));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_array_type_mismatch() {
    let mut checker = setup_checker();
    let array_expr = resolved_array_literal(vec![
        resolved_lit_int(1),
        resolved_lit_bool(true), // Mismatch: int vs bool
    ]);

    let result = checker::expr::type_check_expression(&mut checker, &array_expr, None);
    assert!(result.is_err(), "Expected Err for array type mismatch");
    assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
    // Expect one error from the mismatch
    assert_eq!(checker.errors.len(), 1);
}

#[test]
fn test_array_coercion() {
    let mut checker = setup_checker();
    let array_expr = resolved_array_literal(vec![
        resolved_lit_int(1),
        resolved_lit_int(2),
    ]);
    // Expect the array to be coerced to [i32; 2]
    let expected_ty = ty_array(ty_prim(PrimitiveType::I32), 2);

    let result = checker::expr::type_check_expression(&mut checker, &array_expr, Some(&expected_ty));
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(matches!(typed_expr.kind, TypedExprKind::Array(ref elems) if elems.len() == 2));
    if let TypedExprKind::Array(elems) = typed_expr.kind {
        assert_eq!(elems[0].ty, ty_prim(PrimitiveType::I32));
        assert_eq!(elems[1].ty, ty_prim(PrimitiveType::I32));
    }
    assert!(checker.errors.is_empty());
}

#[test]
fn test_array_with_inferred_variable() {
    let mut checker = setup_checker();
    let var_sym = Symbol::new(10);
    // Add a variable 'x' of type i32 to the environment
    checker._type_env = Arc::new({
        let mut env = TypeEnvironment::new();
        env.add("x".to_string(), ty_prim(PrimitiveType::I32));
        env
    });


    let array_expr = resolved_array_literal(vec![
        resolved_lit_int(5), // IntegerLiteral
        ResolvedExpr { // x
            kind: ResolvedExprKind::Variable { binding_symbol: var_sym, name: "x".to_string() },
            span: dummy_span(),
            resolved_type: ResolvedType::Primitive(ResolvePrimitiveType::I32), // Resolver knows x is i32
        }
    ]);
    // Expect the array type to unify to [i32; 2]
    let expected_ty = ty_array(ty_prim(PrimitiveType::I32), 2);

    let result = checker::expr::type_check_expression(&mut checker, &array_expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(checker.errors.is_empty());
}

#[test]
fn test_array_mismatch_with_inferred_variable() {
    let mut checker = setup_checker();
    let var_sym = Symbol::new(10);
    // Add a variable 'b' of type bool to the environment
     checker._type_env = Arc::new({
        let mut env = TypeEnvironment::new();
        env.add("b".to_string(), ty_prim(PrimitiveType::Bool));
        env
    });


    let array_expr = resolved_array_literal(vec![
        resolved_lit_int(5), // IntegerLiteral
        ResolvedExpr { // b
            kind: ResolvedExprKind::Variable { binding_symbol: var_sym, name: "b".to_string() },
            span: dummy_span(),
            resolved_type: ResolvedType::Primitive(ResolvePrimitiveType::Bool), // Resolver knows b is bool
        }
    ]);

    let result = checker::expr::type_check_expression(&mut checker, &array_expr, None);
    assert!(result.is_err(), "Expected Err for array type mismatch with variable");
    assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
    assert_eq!(checker.errors.len(), 1);
} 