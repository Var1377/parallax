// tests/patterns/basic.rs
use crate::*; // Import helpers from tests/mod.rs
use parallax_types::error::TypeError;

#[test]
fn test_pat_wildcard() {
    let mut checker = setup_checker();
    let pattern = pat_wildcard();
    let expected = ty_prim(PrimitiveType::I32);

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_pattern = result.unwrap();

    assert!(matches!(typed_pattern.kind, TypedPatternKind::Wildcard));
    assert_eq!(typed_pattern.ty, expected); // Wildcard takes the expected type
    assert!(checker.errors.is_empty());
}

#[test]
fn test_pat_ident() {
    let mut checker = setup_checker();
    let pattern = pat_ident("x");
    let expected = ty_prim(PrimitiveType::String);

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_pattern = result.unwrap();

    assert!(matches!(typed_pattern.kind, TypedPatternKind::Identifier { ref name, .. } if name == "x"));
    assert_eq!(typed_pattern.ty, expected); // Identifier takes the expected type
    assert!(checker.errors.is_empty());

    // Note: We don't check the environment bindings directly here,
    // assume add_pattern_bindings works if check_pattern succeeds.
}

#[test]
fn test_pat_ident_wildcard() {
    let mut checker = setup_checker();
    let pattern = pat_ident("_"); // Special case identifier
    let expected = ty_prim(PrimitiveType::F64);

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_pattern = result.unwrap();

    assert!(matches!(typed_pattern.kind, TypedPatternKind::Wildcard));
    assert_eq!(typed_pattern.ty, expected); // Should be treated as wildcard
    assert!(checker.errors.is_empty());
}

#[test]
fn test_pat_literal_match() {
    let mut checker = setup_checker();
    let pattern = pat_lit_int(10);
    let expected = ty_prim(PrimitiveType::I32); // Should unify with IntegerLiteral

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_pattern = result.unwrap();

    assert!(matches!(typed_pattern.kind, TypedPatternKind::Literal(AstLiteral::Int { value: 10, .. })));
    assert_eq!(typed_pattern.ty, expected); // Should unify to I32
    assert!(checker.errors.is_empty());
}

#[test]
fn test_pat_literal_mismatch() {
    let mut checker = setup_checker();
    let pattern = pat_lit_int(10);
    let expected = ty_prim(PrimitiveType::Bool);

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_err(), "Expected Err for literal type mismatch");
    assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
} 