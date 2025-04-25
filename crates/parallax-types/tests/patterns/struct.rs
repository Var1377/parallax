// tests/patterns/struct.rs
use crate::*; // Import helpers from tests/mod.rs
use parallax_types::error::TypeError;

#[test]
fn test_pat_struct_match() {
    let mut checker = setup_checker();
    let struct_sym = Symbol::new(1);
    let field_x_sym = Symbol::new(2);
    let field_y_sym = Symbol::new(3);

    add_test_struct_def(&mut checker, "Point", struct_sym, vec![
        ("x".to_string(), field_x_sym, ty_prim(PrimitiveType::I32)),
        ("y".to_string(), field_y_sym, ty_prim(PrimitiveType::Bool)),
    ]);

    let pattern = pat_struct(struct_sym, vec![
        pat_field("y", Some(pat_ident("b"))),
        pat_field("x", Some(pat_wildcard())),
    ]);
    let expected = ty_named("Point", Some(struct_sym), vec![]);

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_pattern = result.unwrap();

    assert!(matches!(typed_pattern.kind, TypedPatternKind::Struct { ref struct_name, ref fields } if struct_name == "Point" && fields.len() == 2));
    if let TypedPatternKind::Struct { fields, .. } = typed_pattern.kind {
        // Order might not be preserved, so check names
        let field_y = fields.iter().find(|f| f.name == "y").unwrap();
        let field_x = fields.iter().find(|f| f.name == "x").unwrap();
        assert!(matches!(field_y.pattern.as_ref().unwrap().kind, TypedPatternKind::Identifier { ref name, .. } if name == "b"));
        assert_eq!(field_y.pattern.as_ref().unwrap().ty, ty_prim(PrimitiveType::Bool));
        assert!(matches!(field_x.pattern.as_ref().unwrap().kind, TypedPatternKind::Wildcard));
        assert_eq!(field_x.pattern.as_ref().unwrap().ty, ty_prim(PrimitiveType::I32));
    }
    assert_eq!(typed_pattern.ty.kind, expected.kind);
    assert!(checker.errors.is_empty());
}

#[test]
fn test_pat_struct_shorthand() {
    let mut checker = setup_checker();
    let struct_sym = Symbol::new(1);
    let field_x_sym = Symbol::new(2);
    add_test_struct_def(&mut checker, "Data", struct_sym, vec![
        ("x".to_string(), field_x_sym, ty_prim(PrimitiveType::String)),
    ]);

    let pattern = pat_struct(struct_sym, vec![pat_field("x", None)]); // Shorthand { x }
    let expected = ty_named("Data", Some(struct_sym), vec![]);

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_pattern = result.unwrap();

    if let TypedPatternKind::Struct { fields, .. } = typed_pattern.kind {
        assert_eq!(fields.len(), 1);
        assert!(fields[0].pattern.is_some(), "Shorthand should generate an identifier pattern");
        assert!(matches!(fields[0].pattern.as_ref().unwrap().kind, TypedPatternKind::Identifier { ref name, .. } if name == "x"));
        assert_eq!(fields[0].pattern.as_ref().unwrap().ty, ty_prim(PrimitiveType::String));
    }
    assert!(checker.errors.is_empty());
}

#[test]
fn test_pat_struct_unknown_field() {
    let mut checker = setup_checker();
    let struct_sym = Symbol::new(1);
    add_test_struct_def(&mut checker, "Point", struct_sym, vec![]);

    let pattern = pat_struct(struct_sym, vec![pat_field("z", None)]); // Unknown field
    let expected = ty_named("Point", Some(struct_sym), vec![]);

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_err(), "Expected Err for unknown struct field");
    assert!(matches!(result.err().unwrap(), TypeError::UnknownStructField { field, .. } if field == "z"));
}

#[test]
fn test_pat_struct_duplicate_field() {
    let mut checker = setup_checker();
    let struct_sym = Symbol::new(1);
    let field_x_sym = Symbol::new(2);
    add_test_struct_def(&mut checker, "Point", struct_sym, vec![
        ("x".to_string(), field_x_sym, ty_prim(PrimitiveType::I32)),
    ]);

    let pattern = pat_struct(struct_sym, vec![
        pat_field("x", Some(pat_wildcard())),
        pat_field("x", Some(pat_ident("a"))), // Duplicate
    ]);
    let expected = ty_named("Point", Some(struct_sym), vec![]);

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_err(), "Expected Err for duplicate struct field");
    assert!(matches!(result.err().unwrap(), TypeError::InternalError { .. })); // Currently caught as Internal
}

#[test]
fn test_pat_struct_field_type_mismatch() {
    let mut checker = setup_checker();
    let struct_sym = Symbol::new(1);
    let field_x_sym = Symbol::new(2);
    add_test_struct_def(&mut checker, "Point", struct_sym, vec![
        ("x".to_string(), field_x_sym, ty_prim(PrimitiveType::I32)),
    ]);

    let pattern = pat_struct(struct_sym, vec![
        pat_field("x", Some(pat_lit_bool(true))), // Expecting i32, got bool pattern
    ]);
    let expected = ty_named("Point", Some(struct_sym), vec![]);

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_err(), "Expected Err for struct field type mismatch");
    assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
}

#[test]
fn test_pat_struct_wrong_expected_type() {
    let mut checker = setup_checker();
    let struct_sym = Symbol::new(1);
    add_test_struct_def(&mut checker, "Point", struct_sym, vec![]);

    let pattern = pat_struct(struct_sym, vec![]);
    let expected = ty_prim(PrimitiveType::I32); // Expecting i32, not struct Point

    let result = checker::pattern::check_pattern(&mut checker, &pattern, &expected);
    assert!(result.is_err(), "Expected Err for struct pattern against non-struct type");
    assert!(matches!(result.err().unwrap(), TypeError::InternalError { .. } | TypeError::TypeMismatch { .. }));
} 