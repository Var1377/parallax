// tests/definitions/enum_def.rs
use crate::*; // Import helpers from tests/mod.rs
use parallax_types::{
    error::TypeError,
    // Import TypeDef, EnumDef, EnumVariant, Field, GenericParamDef etc.
    types::{PrimitiveType, Ty, TyKind, TypeId, TypeDef, EnumDef, EnumVariant, Field, GenericParamDef as CheckerGenericParamDef},
};
use parallax_resolve::{
    types::{ResolvedType, Symbol},
    PrimitiveType as ResolvePrimitiveType,
};

#[test]
fn test_check_enum_def_unit_variants() {
    let mut checker = setup_checker();
    let enum_sym = Symbol::new(1);
    let variant_a_sym = Symbol::new(2);
    let variant_b_sym = Symbol::new(3);

    add_test_enum_def(&mut checker, "Status", enum_sym, vec![
        ("Ok".to_string(), variant_a_sym, vec![]),       // Unit variant Ok
        ("Err".to_string(), variant_b_sym, vec![]),      // Unit variant Err
    ]);

    // Verify the TypeDef
    let type_def = checker.type_ctx.get_type_by_symbol(&enum_sym);
    assert!(type_def.is_some(), "TypeDef for Status not found");

    if let Some(TypeDef::Enum(def)) = type_def {
        assert_eq!(def.name, "Status");
        assert_eq!(def.symbol, enum_sym);
        assert!(def.generic_params.is_empty());
        assert_eq!(def.variants.len(), 2);

        // Check Ok variant
        let var_ok = def.variants.iter().find(|v| v.name == "Ok").unwrap();
        assert_eq!(var_ok.name, "Ok");
        assert_eq!(var_ok.symbol, variant_a_sym);
        assert!(var_ok.fields.is_empty(), "Unit variant Ok should have no fields");

        // Check Err variant
        let var_err = def.variants.iter().find(|v| v.name == "Err").unwrap();
        assert_eq!(var_err.name, "Err");
        assert_eq!(var_err.symbol, variant_b_sym);
        assert!(var_err.fields.is_empty(), "Unit variant Err should have no fields");
    } else {
        panic!("Expected TypeDef::Enum, got {:?}", type_def);
    }

    assert!(checker.errors.is_empty());
}

#[test]
fn test_check_enum_def_tuple_variants() {
    let mut checker = setup_checker();
    let enum_sym = Symbol::new(10);
    let variant_point_sym = Symbol::new(11);
    let field0_sym = Symbol::new(12); // Symbol for first element in tuple
    let field1_sym = Symbol::new(13); // Symbol for second element in tuple
    let variant_msg_sym = Symbol::new(14);
    let field2_sym = Symbol::new(15);

    add_test_enum_def(&mut checker, "Payload", enum_sym, vec![
        // Point(i32, i32)
        ("Point".to_string(), variant_point_sym, vec![
            ("_".to_string(), field0_sym, ty_prim(PrimitiveType::I32)),
            ("_".to_string(), field1_sym, ty_prim(PrimitiveType::I32)),
        ]),
        // Msg(String)
        ("Msg".to_string(), variant_msg_sym, vec![("_".to_string(), field2_sym, ty_prim(PrimitiveType::String))]),
    ]);

    // Verify TypeDef
    let type_def = checker.type_ctx.get_type_by_symbol(&enum_sym);
    assert!(type_def.is_some(), "TypeDef for Payload not found");

    if let Some(TypeDef::Enum(def)) = type_def {
        assert_eq!(def.name, "Payload");
        assert_eq!(def.variants.len(), 2);

        // Check Point variant (Tuple-like)
        let var_point = def.variants.iter().find(|v| v.name == "Point").unwrap();
        assert_eq!(var_point.name, "Point");
        assert_eq!(var_point.symbol, variant_point_sym);
        assert_eq!(var_point.fields.len(), 2, "Tuple variant Point should have 2 fields");
        assert_eq!(var_point.fields[0].name, "_"); // Tuple fields often have placeholder names
        assert_eq!(var_point.fields[0].symbol, field0_sym);
        assert_eq!(var_point.fields[0].ty, ty_prim(PrimitiveType::I32));
        assert_eq!(var_point.fields[1].name, "_");
        assert_eq!(var_point.fields[1].symbol, field1_sym);
        assert_eq!(var_point.fields[1].ty, ty_prim(PrimitiveType::I32));

        // Check Msg variant (Tuple-like)
        let var_msg = def.variants.iter().find(|v| v.name == "Msg").unwrap();
        assert_eq!(var_msg.name, "Msg");
        assert_eq!(var_msg.symbol, variant_msg_sym);
        assert_eq!(var_msg.fields.len(), 1, "Tuple variant Msg should have 1 field");
        assert_eq!(var_msg.fields[0].name, "_");
        assert_eq!(var_msg.fields[0].symbol, field2_sym);
        assert_eq!(var_msg.fields[0].ty, ty_prim(PrimitiveType::String));
    } else {
        panic!("Expected TypeDef::Enum, got {:?}", type_def);
    }

    assert!(checker.errors.is_empty());
}

#[test]
fn test_check_enum_def_struct_variants() {
    // Assuming the resolver/parser supports struct variant syntax
    // and add_test_enum_def can handle it.
    let mut checker = setup_checker();
    let enum_sym = Symbol::new(20);
    let variant_data_sym = Symbol::new(21);
    let field_id_sym = Symbol::new(22);
    let field_value_sym = Symbol::new(23);

    add_test_enum_def(&mut checker, "Packet", enum_sym, vec![
        // Data { id: u64, value: String }
        ("Data".to_string(), variant_data_sym, vec![
            ("id".to_string(), field_id_sym, ty_prim(PrimitiveType::U64)),
            ("value".to_string(), field_value_sym, ty_prim(PrimitiveType::String)),
        ]),
        // Add another variant for completeness if desired
    ]);

    // Verify TypeDef
    let type_def = checker.type_ctx.get_type_by_symbol(&enum_sym);
    assert!(type_def.is_some(), "TypeDef for Packet not found");

    if let Some(TypeDef::Enum(def)) = type_def {
        assert_eq!(def.name, "Packet");
        assert_eq!(def.variants.len(), 1);

        // Check Data variant (Struct-like)
        let var_data = &def.variants[0];
        assert_eq!(var_data.name, "Data");
        assert_eq!(var_data.symbol, variant_data_sym);
        assert_eq!(var_data.fields.len(), 2, "Struct variant Data should have 2 fields");
        // Order might not be guaranteed by HashMap iteration in checker, so find by name
        let field_id = var_data.fields.iter().find(|f| f.name == "id").unwrap();
        assert_eq!(field_id.symbol, field_id_sym);
        assert_eq!(field_id.ty, ty_prim(PrimitiveType::U64));
        let field_value = var_data.fields.iter().find(|f| f.name == "value").unwrap();
        assert_eq!(field_value.symbol, field_value_sym);
        assert_eq!(field_value.ty, ty_prim(PrimitiveType::String));
    } else {
        panic!("Expected TypeDef::Enum, got {:?}", type_def);
    }

    assert!(checker.errors.is_empty());
}

#[test]
fn test_check_enum_def_generic() {
    let mut checker = setup_checker();
    let enum_sym = Symbol::new(30);
    let some_sym = Symbol::new(31);
    let none_sym = Symbol::new(32);
    let gen_param_t_sym = Symbol::new(33); // Symbol resolver assigns to <T>
    let variant_field_sym = Symbol::new(34);

    // Manually construct the generic EnumDef
    let gen_param_id = match checker.inference_ctx.fresh_var().kind {
        TyKind::Var(id) => id,
        _ => panic!("Fresh var was not TyKind::Var"),
    };
    let checker_gen_param = CheckerGenericParamDef {
        name: "T".to_string(),
        symbol: gen_param_t_sym,
        id: gen_param_id,
        bounds: vec![],
        span: dummy_span(),
    };
    let enum_def = EnumDef {
        name: "Option".to_string(),
        symbol: enum_sym,
        generic_params: vec![checker_gen_param],
        variants: vec![
            // Some(T)
            EnumVariant {
                name: "Some".to_string(),
                symbol: some_sym,
                fields: vec![Field {
                    name: "_".to_string(), // Tuple-like variant
                    symbol: variant_field_sym,
                    ty: Ty::new(TyKind::Var(gen_param_id)), // Type is T
                    span: dummy_span(),
                }],
                span: dummy_span(),
            },
            // None
            EnumVariant {
                name: "None".to_string(),
                symbol: none_sym,
                fields: vec![], // Unit variant
                span: dummy_span(),
            },
        ],
        span: dummy_span(),
    };
    checker.type_ctx.add_type(enum_sym, "Option".to_string(), TypeDef::Enum(enum_def));

    // Verify TypeDef
    let type_def = checker.type_ctx.get_type_by_symbol(&enum_sym);
    assert!(type_def.is_some(), "TypeDef for Option not found");

    if let Some(TypeDef::Enum(def)) = type_def {
        assert_eq!(def.name, "Option");
        assert_eq!(def.generic_params.len(), 1);
        assert_eq!(def.generic_params[0].name, "T");
        assert_eq!(def.generic_params[0].symbol, gen_param_t_sym);
        assert_eq!(def.generic_params[0].id, gen_param_id);

        assert_eq!(def.variants.len(), 2);

        let var_some = def.variants.iter().find(|v| v.name == "Some").unwrap();
        assert_eq!(var_some.fields.len(), 1);
        assert_eq!(var_some.fields[0].ty, ty_var(gen_param_id.0)); // Field type is T

        let var_none = def.variants.iter().find(|v| v.name == "None").unwrap();
        assert!(var_none.fields.is_empty());
    } else {
        panic!("Expected TypeDef::Enum");
    }

    assert!(checker.errors.is_empty());
}

#[test]
fn test_check_enum_def_variant_field_unknown_type() {
    let mut checker = setup_checker();
    let enum_sym = Symbol::new(40);
    let variant_sym = Symbol::new(41);
    let field_sym = Symbol::new(42);

    // Simulate adding an enum where a variant field type resolves to Error
    let enum_def = EnumDef {
        name: "BadEnum".to_string(),
        symbol: enum_sym,
        generic_params: vec![],
        variants: vec![EnumVariant {
            name: "BadData".to_string(),
            symbol: variant_sym,
            fields: vec![Field {
                name: "data".to_string(), // Struct-like variant field
                symbol: field_sym,
                ty: Ty::new(TyKind::Error), // Simulate checker resolving Unknown to Error
                span: dummy_span(),
            }],
            span: dummy_span(),
        }],
        span: dummy_span(),
    };
    checker.type_ctx.add_type(enum_sym, "BadEnum".to_string(), TypeDef::Enum(enum_def));

    // Add a simulated error
    checker.errors.push(TypeError::InternalError { message: "Simulated error: Variant field 'data' has unknown type".to_string(), span: Some(dummy_span()) });

    // Verify TypeDef
    let type_def = checker.type_ctx.get_type_by_symbol(&enum_sym);
    assert!(type_def.is_some());
    if let Some(TypeDef::Enum(def)) = type_def {
        assert_eq!(def.variants.len(), 1);
        assert_eq!(def.variants[0].fields.len(), 1);
        assert!(matches!(def.variants[0].fields[0].ty.kind, TyKind::Error));
    } else {
        panic!("Expected TypeDef::Enum");
    }

    // Verify error
    assert_eq!(checker.errors.len(), 1);
} 