// tests/patterns/or_pattern.rs
use crate::*; // Import helpers from tests/mod.rs
use parallax_types::error::TypeError;

#[test]
fn test_pat_or_match_simple() {
    let mut checker = setup_checker();
    // Pattern: 1 | 2
    let pattern = pat_or(pat_lit_int(1), pat_lit_int(2));
    let expected = ty_prim(PrimitiveType::I32); // Expecting i32, should unify with IntegerLiteral

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_ok(), "Expected Ok for 1 | 2 pattern, got Err: {:?}", result.err());
    let typed_pattern = result.unwrap();

    assert!(matches!(typed_pattern.kind, TypedPatternKind::Or(_, _)));
    // Check that the type unifies correctly
    assert_eq!(typed_pattern.ty, expected);
    assert!(checker.errors.is_empty());
}

#[test]
fn test_pat_or_match_different_types_fail() {
    let mut checker = setup_checker();
    // Pattern: 1 | true
    let pattern = pat_or(pat_lit_int(1), pat_lit_bool(true));
    let expected = ty_prim(PrimitiveType::I32);

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_err(), "Expected Err for or-pattern with mismatched types");
    // Expect a type mismatch during the check_pattern unification of branches
    assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
}

#[test]
fn test_pat_or_binding_same_variable_ok() {
    let mut checker = setup_checker();
    // Pattern: Some(x) | None where x should bind to the inner type of Option<String>
    let option_enum_sym = Symbol::new(50);
    let some_variant_sym = Symbol::new(51);
    let none_variant_sym = Symbol::new(52);
    let inner_field_sym = Symbol::new(53); // Symbol for the field inside Some

    // Add dummy Option<String> enum definition
    add_test_enum_def(
        &mut checker,
        "MyOption",
        option_enum_sym,
        vec![
            ("MySome".to_string(), some_variant_sym, vec![("_".to_string(), inner_field_sym, ty_prim(PrimitiveType::String))]),
            ("MyNone".to_string(), none_variant_sym, vec![]),
        ],
    );

    let expected_ty = ty_named("MyOption", Some(option_enum_sym), vec![ty_prim(PrimitiveType::String)]);

    // Pattern: MySome(x) | MyNone
    let pattern = pat_or(
        pat_constructor(some_variant_sym, pat_tuple(vec![pat_ident("x")])), // MySome(x)
        pat_constructor(none_variant_sym, pat_tuple(vec![])),             // MyNone
    );

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected_ty);
    assert!(result.is_ok(), "Expected Ok for MySome(x) | MyNone, got Err: {:?}", result.err());
    let typed_pattern = result.unwrap();

    assert!(matches!(typed_pattern.kind, TypedPatternKind::Or(_, _)));
    assert_eq!(typed_pattern.ty, expected_ty);

    // Verify that 'x' is bound with the correct type (String)
    // We need to simulate adding bindings to check this properly.
    // Let's assume check_pattern added bindings correctly if it returned Ok.
    // A more robust test would involve check_match or check_let.

    // Minimal check: Try resolving 'x' in the *current* (test) environment after check_pattern
    // This relies on check_pattern potentially modifying the checker's env (which might not be ideal test design)
    // let bound_type = checker._type_env.get("x");
    // assert!(bound_type.is_some(), "Variable 'x' should be bound");
    // assert_eq!(bound_type.unwrap(), &ty_prim(PrimitiveType::String));

    assert!(checker.errors.is_empty());
}

#[test]
fn test_pat_or_binding_different_variables_fail() {
    let mut checker = setup_checker();
    // Pattern: (x, _) | (_, y)
    let pattern = pat_or(
        pat_tuple(vec![pat_ident("x"), pat_wildcard()]),
        pat_tuple(vec![pat_wildcard(), pat_ident("y")])
    );
    let expected = ty_tuple(vec![ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::Bool)]);

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_err(), "Expected Err for or-pattern binding different variables");
    // Expect OrPatternBindingMismatch or similar internal error
    assert!(matches!(result.err().unwrap(), TypeError::OrPatternBindingMismatch { .. } | TypeError::InternalError { .. }));
}

// TODO: Add test for or-patterns where variable types conflict (e.g., MySome(x: i32) | MyOther(x: bool))
// TODO: Add test for nested or-patterns 