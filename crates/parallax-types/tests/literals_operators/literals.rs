// tests/literals_operators/literals.rs
use crate::*; // Import helpers from tests/mod.rs
use parallax_types::types::{TypedExpr, TypedExprKind, Ty, TyKind, PrimitiveType};
use miette::SourceSpan;
use parallax_types::error::TypeError;

// Helper to create a dummy span
fn dummy_span() -> SourceSpan {
    SourceSpan::from((0, 0))
}

// Helper to create a dummy type
fn dummy_ty(kind: TyKind) -> Ty {
    Ty { kind, span: Some(dummy_span()) }
}

// Helper to create a dummy typed expression
fn dummy_expr(kind: TypedExprKind, ty_kind: TyKind) -> TypedExpr {
    TypedExpr {
        kind,
        ty: dummy_ty(ty_kind),
        span: dummy_span(),
    }
}

// Helper function to create a ResolvedExpr char literal
fn resolved_lit_char(val: char) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Literal(AstLiteral::Char(val)),
        span: dummy_span(),
        resolved_type: ResolvedType::Primitive(ResolvePrimitiveType::Char),
    }
}

#[test]
fn test_integer_literal_unconstrained() {
    let mut checker = setup_checker();
    let resolved_expr = resolved_lit_int(42);

    let result = checker::expr::type_check_expression(&mut checker, &resolved_expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    // Unconstrained integer literal defaults to IntegerLiteral type
    assert!(matches!(
        typed_expr.kind,
        TypedExprKind::IntLiteral { value: 42, .. }
    ));
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::IntegerLiteral));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_integer_literal_constrained() {
    let mut checker = setup_checker();
    let resolved_expr = resolved_lit_int(123);
    let expected_ty = ty_prim(PrimitiveType::I32); // Expect i32

    let result = checker::expr::type_check_expression(&mut checker, &resolved_expr, Some(&expected_ty));
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    // Constrained integer literal should have the expected type
    assert!(matches!(
        typed_expr.kind,
        TypedExprKind::IntLiteral { value: 123, .. }
    ));
    // Type checker should unify the literal with the expected type
    assert_eq!(typed_expr.ty, expected_ty);
    assert!(checker.errors.is_empty());
}

#[test]
fn test_integer_literal_with_suffix() {
    let mut checker = setup_checker();
    // Resolver should provide the type based on suffix
    let resolved_expr = resolved_lit_int_suffix(99, "u64", ResolvedType::Primitive(ResolvePrimitiveType::U64));

    let result = checker::expr::type_check_expression(&mut checker, &resolved_expr, None); // No constraint needed
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    assert!(matches!(
        typed_expr.kind,
        TypedExprKind::IntLiteral { value: 99, .. }
    ));
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::U64));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_float_literal_unconstrained() {
    let mut checker = setup_checker();
    let resolved_expr = resolved_lit_float(3.14);

    let result = checker::expr::type_check_expression(&mut checker, &resolved_expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    // Unconstrained float literal defaults to FloatLiteral type
    assert!(matches!(
        typed_expr.kind,
        TypedExprKind::FloatLiteral { value, .. } if (value - 3.14).abs() < f64::EPSILON
    ));
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::FloatLiteral));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_float_literal_constrained_f64() {
    let mut checker = setup_checker();
    let resolved_expr = resolved_lit_float(2.718);
    let expected_ty = ty_prim(PrimitiveType::F64);

    let result = checker::expr::type_check_expression(&mut checker, &resolved_expr, Some(&expected_ty));
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    assert!(matches!(
        typed_expr.kind,
        TypedExprKind::FloatLiteral { value, .. } if (value - 2.718).abs() < f64::EPSILON
    ));
    // Type checker should unify the literal with the expected type
    assert_eq!(typed_expr.ty, expected_ty);
    assert!(checker.errors.is_empty());
}

#[test]
fn test_float_literal_constrained_f32() {
    let mut checker = setup_checker();
    let resolved_expr = resolved_lit_float(1.618);
    let expected_ty = ty_prim(PrimitiveType::F32);

    let result = checker::expr::type_check_expression(&mut checker, &resolved_expr, Some(&expected_ty));
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    assert!(matches!(
        typed_expr.kind,
        TypedExprKind::FloatLiteral { value, .. } if (value - 1.618).abs() < f64::EPSILON
    ));
    assert_eq!(typed_expr.ty, expected_ty);
    assert!(checker.errors.is_empty());
}

#[test]
fn test_float_literal_with_suffix() {
    let mut checker = setup_checker();
    // Resolver provides type based on suffix
    let resolved_expr = resolved_lit_float_suffix(1.0, "f32", ResolvedType::Primitive(ResolvePrimitiveType::F32));

    let result = checker::expr::type_check_expression(&mut checker, &resolved_expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    assert!(matches!(
        typed_expr.kind,
        TypedExprKind::FloatLiteral { value, .. } if (value - 1.0).abs() < f64::EPSILON
    ));
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::F32));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_bool_literal() {
    let mut checker = setup_checker();
    let resolved_expr = resolved_lit_bool(true);
    let expected_ty = ty_prim(PrimitiveType::Bool);

    let result = checker::expr::type_check_expression(&mut checker, &resolved_expr, None); // Bool doesn't need constraint here
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    assert!(matches!(typed_expr.kind, TypedExprKind::BoolLiteral(true)));
    assert_eq!(typed_expr.ty, expected_ty);
    assert!(checker.errors.is_empty());
}

#[test]
fn test_string_literal() {
    let mut checker = setup_checker();
    let resolved_expr = resolved_lit_string("hello");
    let expected_ty = ty_prim(PrimitiveType::String);

    let result = checker::expr::type_check_expression(&mut checker, &resolved_expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    match typed_expr.kind {
        TypedExprKind::StringLiteral(s) => assert_eq!(s, "hello"),
        _ => panic!("Expected StringLiteral, got {:?}", typed_expr.kind),
    }
    assert_eq!(typed_expr.ty, expected_ty);
    assert!(checker.errors.is_empty());
}

#[test]
fn test_char_literal() {
    let mut checker = setup_checker();
    let resolved_expr = resolved_lit_char('a');
    let expected_ty = ty_prim(PrimitiveType::Char);

    let result = checker::expr::type_check_expression(&mut checker, &resolved_expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    assert!(matches!(typed_expr.kind, TypedExprKind::CharLiteral('a')));
    assert_eq!(typed_expr.ty, expected_ty);
    assert!(checker.errors.is_empty());
}

// TODO: Add tests for float, char literals, potentially with constraints

// Add more tests for float, char, string literals... 

// --- Error Cases ---

#[test]
fn test_integer_literal_invalid_constraint() {
    let mut checker = setup_checker();
    let resolved_expr = resolved_lit_int(100);
    let expected_ty = ty_prim(PrimitiveType::Bool); // Invalid constraint

    let result = checker::expr::type_check_expression(&mut checker, &resolved_expr, Some(&expected_ty));
    assert!(result.is_err(), "Expected Err for int constrained to bool");
    assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
}

#[test]
fn test_float_literal_invalid_constraint() {
    let mut checker = setup_checker();
    let resolved_expr = resolved_lit_float(1.23);
    let expected_ty = ty_prim(PrimitiveType::I64); // Invalid constraint

    let result = checker::expr::type_check_expression(&mut checker, &resolved_expr, Some(&expected_ty));
    assert!(result.is_err(), "Expected Err for float constrained to int");
    assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
} 