// tests/patterns/array.rs
use crate::*; // Import helpers from tests/mod.rs
use parallax_types::error::TypeError;

#[test]
fn test_pat_array_match_fixed() {
    let mut checker = setup_checker();
    let pattern = pat_array(vec![pat_ident("a"), pat_lit_int(0)]);
    let expected = ty_array(ty_prim(PrimitiveType::I32), 2);

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_pattern = result.unwrap();

    assert!(matches!(typed_pattern.kind, TypedPatternKind::Array(ref elems) if elems.len() == 2));
    if let TypedPatternKind::Array(elems) = typed_pattern.kind {
        assert!(matches!(elems[0].kind, TypedPatternKind::Identifier { ref name, .. } if name == "a"));
        assert_eq!(elems[0].ty, ty_prim(PrimitiveType::I32));
        assert!(matches!(elems[1].kind, TypedPatternKind::Literal(AstLiteral::Int { value: 0, .. })));
        assert_eq!(elems[1].ty, ty_prim(PrimitiveType::I32));
    }
    assert_eq!(typed_pattern.ty.kind, expected.kind);
    assert!(checker.errors.is_empty());
}

#[test]
fn test_pat_array_match_rest() {
    let mut checker = setup_checker();
    let pattern = pat_array(vec![pat_ident("first"), pat_rest()]);
    let expected = ty_array(ty_prim(PrimitiveType::String), 3);

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_pattern = result.unwrap();

    assert!(matches!(typed_pattern.kind, TypedPatternKind::Array(ref elems) if elems.len() == 2));
    if let TypedPatternKind::Array(elems) = typed_pattern.kind {
        assert!(matches!(elems[0].kind, TypedPatternKind::Identifier { ref name, .. } if name == "first"));
        assert_eq!(elems[0].ty, ty_prim(PrimitiveType::String));
        assert!(matches!(elems[1].kind, TypedPatternKind::Rest));
        assert_eq!(elems[1].ty, ty_prim(PrimitiveType::String)); // Rest gets element type
    }
    assert_eq!(typed_pattern.ty.kind, expected.kind);
    assert!(checker.errors.is_empty());
}

#[test]
fn test_pat_array_arity_mismatch_fixed() {
    let mut checker = setup_checker();
    let pattern = pat_array(vec![pat_wildcard()]); // Pattern has 1
    let expected = ty_array(ty_prim(PrimitiveType::I32), 2); // Expected has 2

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_err(), "Expected Err for fixed array arity mismatch");
    assert!(matches!(result.err().unwrap(), TypeError::InternalError { .. }));
}

#[test]
fn test_pat_array_arity_mismatch_rest() {
    let mut checker = setup_checker();
    // Pattern requires at least 2 elements, expected type only has 1
    let pattern = pat_array(vec![pat_wildcard(), pat_ident("x"), pat_rest()]);
    let expected = ty_array(ty_prim(PrimitiveType::I32), 1);

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_err(), "Expected Err for rest array arity mismatch");
    assert!(matches!(result.err().unwrap(), TypeError::InternalError { .. }));
}

#[test]
fn test_pat_array_multiple_rest() {
    let mut checker = setup_checker();
    let pattern = pat_array(vec![pat_rest(), pat_wildcard(), pat_rest()]);
    let expected = ty_array(ty_prim(PrimitiveType::I32), 3);

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_err(), "Expected Err for multiple rest patterns");
    assert!(matches!(result.err().unwrap(), TypeError::InternalError { .. }));
}

#[test]
fn test_pat_array_element_type_mismatch() {
    let mut checker = setup_checker();
    let pattern = pat_array(vec![pat_ident("a"), pat_lit_int(0)]);
    let expected = ty_array(ty_prim(PrimitiveType::Bool), 2); // Expecting bool elements

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_err(), "Expected Err for array element type mismatch");
    assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
}

#[test]
fn test_pat_array_wrong_expected_type() {
    let mut checker = setup_checker();
    let pattern = pat_array(vec![pat_wildcard()]);
    let expected = ty_prim(PrimitiveType::I32); // Expecting i32, not array

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_err(), "Expected Err for array pattern against non-array type");
    assert!(matches!(result.err().unwrap(), TypeError::InternalError { .. } | TypeError::TypeMismatch { .. }));
} 