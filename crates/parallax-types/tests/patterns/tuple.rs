// tests/patterns/tuple.rs
use crate::*; // Import helpers from tests/mod.rs
use parallax_types::error::TypeError;

#[test]
fn test_pat_tuple_match() {
    let mut checker = setup_checker();
    let pattern = pat_tuple(vec![pat_ident("a"), pat_wildcard()]);
    let expected = ty_tuple(vec![ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::Bool)]);

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_pattern = result.unwrap();

    assert!(matches!(typed_pattern.kind, TypedPatternKind::Tuple(ref elems) if elems.len() == 2));
    if let TypedPatternKind::Tuple(elems) = typed_pattern.kind {
        assert!(matches!(elems[0].kind, TypedPatternKind::Identifier { ref name, .. } if name == "a"));
        assert_eq!(elems[0].ty, ty_prim(PrimitiveType::I32));
        assert!(matches!(elems[1].kind, TypedPatternKind::Wildcard));
        assert_eq!(elems[1].ty, ty_prim(PrimitiveType::Bool));
    }
    assert_eq!(typed_pattern.ty, expected);
    assert!(checker.errors.is_empty());
}

#[test]
fn test_pat_tuple_arity_mismatch() {
    let mut checker = setup_checker();
    let pattern = pat_tuple(vec![pat_ident("a")]); // Pattern has 1 element
    let expected = ty_tuple(vec![ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::Bool)]); // Expected type has 2

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_err(), "Expected Err for tuple arity mismatch");
    // Expect an internal error because arity mismatch is checked before unification
    assert!(matches!(result.err().unwrap(), TypeError::InternalError { .. }));
}

#[test]
fn test_pat_tuple_type_mismatch() {
    let mut checker = setup_checker();
    let pattern = pat_tuple(vec![pat_ident("a"), pat_lit_int(10)]);
    let expected = ty_tuple(vec![ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::Bool)]); // Second element expects Bool

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_err(), "Expected Err for tuple element type mismatch");
    // The error should be a TypeMismatch from unifying the literal int with bool
    assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
}

#[test]
fn test_pat_tuple_wrong_expected_type() {
    let mut checker = setup_checker();
    let pattern = pat_tuple(vec![pat_wildcard()]);
    let expected = ty_prim(PrimitiveType::I32); // Expecting i32, not tuple

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_err(), "Expected Err for tuple pattern against non-tuple type");
    assert!(matches!(result.err().unwrap(), TypeError::InternalError { .. } | TypeError::TypeMismatch { .. }));
} 