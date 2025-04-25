// tests/patterns/enum.rs
use crate::*; // Import helpers from tests/mod.rs
use parallax_types::error::TypeError;

#[test]
fn test_pat_enum_unit_match() {
    let mut checker = setup_checker();
    let enum_sym = Symbol::new(1);
    let variant_a_sym = Symbol::new(2);
    add_test_enum_def(&mut checker, "MyEnum", enum_sym, vec![("A".to_string(), variant_a_sym, vec![])]);

    let pattern = pat_constructor(variant_a_sym, pat_tuple(vec![])); // Unit variant represented by empty tuple pattern args
    let expected = ty_named("MyEnum", Some(enum_sym), vec![]);

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_pattern = result.unwrap();

    assert!(matches!(typed_pattern.kind, TypedPatternKind::Constructor { enum_name, variant_name, .. } if enum_name == "MyEnum" && variant_name == "A"));
    assert_eq!(typed_pattern.ty.kind, expected.kind);
    assert!(checker.errors.is_empty());
}

#[test]
fn test_pat_enum_tuple_match() {
    let mut checker = setup_checker();
    let enum_sym = Symbol::new(1);
    let variant_b_sym = Symbol::new(3);
    let field1_sym = Symbol::new(4);
    let field2_sym = Symbol::new(5);
    add_test_enum_def(&mut checker, "MyEnum", enum_sym, vec![(
        "B".to_string(),
        variant_b_sym,
        vec![
            ("_".to_string(), field1_sym, ty_prim(PrimitiveType::I32)),
            ("_".to_string(), field2_sym, ty_prim(PrimitiveType::Bool)),
        ],
    )]);

    let pattern = pat_constructor(variant_b_sym, pat_tuple(vec![pat_ident("x"), pat_wildcard()]));
    let expected = ty_named("MyEnum", Some(enum_sym), vec![]);

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_pattern = result.unwrap();

    if let TypedPatternKind::Constructor { enum_name, variant_name, args, .. } = typed_pattern.kind {
        assert_eq!(enum_name, "MyEnum");
        assert_eq!(variant_name, "B");
        assert!(matches!(args.kind, TypedPatternKind::Tuple(ref elems) if elems.len() == 2));
        if let TypedPatternKind::Tuple(elems) = args.kind {
            assert!(matches!(elems[0].kind, TypedPatternKind::Identifier { ref name, .. } if name == "x"));
            assert_eq!(elems[0].ty, ty_prim(PrimitiveType::I32));
            assert!(matches!(elems[1].kind, TypedPatternKind::Wildcard));
            assert_eq!(elems[1].ty, ty_prim(PrimitiveType::Bool));
        }
    }
    assert_eq!(typed_pattern.ty.kind, expected.kind);
    assert!(checker.errors.is_empty());
}

#[test]
fn test_pat_enum_wrong_variant() {
    let mut checker = setup_checker();
    let enum_sym = Symbol::new(1);
    let variant_a_sym = Symbol::new(2);
    let variant_c_sym = Symbol::new(6); // Different variant symbol
    add_test_enum_def(&mut checker, "MyEnum", enum_sym, vec![("A".to_string(), variant_a_sym, vec![])]);

    let pattern = pat_constructor(variant_c_sym, pat_tuple(vec![])); // Pattern for variant C
    let expected = ty_named("MyEnum", Some(enum_sym), vec![]); // Expecting MyEnum

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_err(), "Expected Err for wrong enum variant");
    // This might manifest as a TypeMismatch or InternalError depending on checks
    assert!(matches!(result.err().unwrap(), TypeError::InternalError { .. } | TypeError::TypeMismatch { .. }));
}

#[test]
fn test_pat_enum_arity_mismatch() {
    let mut checker = setup_checker();
    let enum_sym = Symbol::new(1);
    let variant_b_sym = Symbol::new(3);
    let field1_sym = Symbol::new(4);
    add_test_enum_def(&mut checker, "MyEnum", enum_sym, vec![(
        "B".to_string(),
        variant_b_sym,
        vec![("_".to_string(), field1_sym, ty_prim(PrimitiveType::I32))],
    )]); // Variant B takes 1 arg

    let pattern = pat_constructor(variant_b_sym, pat_tuple(vec![pat_ident("x"), pat_wildcard()])); // Pattern provides 2 args
    let expected = ty_named("MyEnum", Some(enum_sym), vec![]);

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_err(), "Expected Err for enum arity mismatch");
    assert!(matches!(result.err().unwrap(), TypeError::InternalError { .. })); // Arity checked internally
}

#[test]
fn test_pat_enum_arg_type_mismatch() {
    let mut checker = setup_checker();
    let enum_sym = Symbol::new(1);
    let variant_b_sym = Symbol::new(3);
    let field1_sym = Symbol::new(4);
    add_test_enum_def(&mut checker, "MyEnum", enum_sym, vec![(
        "B".to_string(),
        variant_b_sym,
        vec![("_".to_string(), field1_sym, ty_prim(PrimitiveType::I32))],
    )]); // Variant B expects I32

    let pattern = pat_constructor(variant_b_sym, pat_tuple(vec![pat_lit_bool(true)])); // Pattern provides bool
    let expected = ty_named("MyEnum", Some(enum_sym), vec![]);

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_err(), "Expected Err for enum arg type mismatch");
    assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
}

#[test]
fn test_pat_enum_wrong_expected_type() {
    let mut checker = setup_checker();
    let enum_sym = Symbol::new(1);
    let variant_a_sym = Symbol::new(2);
    add_test_enum_def(&mut checker, "MyEnum", enum_sym, vec![("A".to_string(), variant_a_sym, vec![])]);

    let pattern = pat_constructor(variant_a_sym, pat_tuple(vec![]));
    let expected = ty_prim(PrimitiveType::I32); // Expecting i32, not enum

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_err(), "Expected Err for enum pattern against non-enum type");
    assert!(matches!(result.err().unwrap(), TypeError::InternalError { .. } | TypeError::TypeMismatch { .. }));
} 