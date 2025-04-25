// tests/definitions/struct_def.rs
use crate::*; // Import helpers from tests/mod.rs
use parallax_types::{
    context::TypeEnvironment, // Import TypeEnvironment
    error::TypeError,
    // Import TypeDef, StructDef, GenericParamDef, Field, EnumDef
    types::{PrimitiveType, Ty, TyKind, TypeId, TypeDef, StructDef, Field, EnumDef, GenericParamDef as CheckerGenericParamDef},
};
use parallax_resolve::{
    types::{ResolvedExpr, ResolvedExprKind, ResolvedType, Symbol, ResolvedGenericParamDef, ResolvedStruct, ResolvedField},
    PrimitiveType as ResolvePrimitiveType,
};
use std::sync::Arc;

// NO NEED for resolved_struct, resolved_field, resolved_generic_param helpers here,
// as we use add_test_struct_def or manually construct TypeDef for the checker.

#[test]
fn test_check_struct_def_simple() {
    let mut checker = setup_checker();
    let struct_sym = Symbol::new(1);
    let field_x_sym = Symbol::new(2);
    let field_y_sym = Symbol::new(3);

    // Use the existing helper which populates the checker's context
    add_test_struct_def(&mut checker, "Point", struct_sym, vec![
        ("x".to_string(), field_x_sym, ty_prim(PrimitiveType::I32)),
        ("y".to_string(), field_y_sym, ty_prim(PrimitiveType::Bool)),
    ]);

    // Verify the TypeDef was added correctly
    let type_def = checker.type_ctx.get_type_by_symbol(&struct_sym);
    assert!(type_def.is_some(), "TypeDef for Point not found");

    if let Some(TypeDef::Struct(def)) = type_def {
        assert_eq!(def.name, "Point");
        assert_eq!(def.symbol, struct_sym);
        assert!(def.generic_params.is_empty());
        assert_eq!(def.fields.len(), 2);
        assert_eq!(def.fields[0].name, "x");
        assert_eq!(def.fields[0].symbol, field_x_sym);
        assert_eq!(def.fields[0].ty, ty_prim(PrimitiveType::I32));
        assert_eq!(def.fields[1].name, "y");
        assert_eq!(def.fields[1].symbol, field_y_sym);
        assert_eq!(def.fields[1].ty, ty_prim(PrimitiveType::Bool));
    } else {
        panic!("Expected TypeDef::Struct, got {:?}", type_def);
    }

    assert!(checker.errors.is_empty(), "Expected no errors for simple struct def");
}

#[test]
fn test_check_struct_def_generic() {
    let mut checker = setup_checker();
    let struct_sym = Symbol::new(10);
    let field_val_sym = Symbol::new(11);
    let gen_param_t_sym = Symbol::new(12); // Symbol resolver would assign to <T>

    // Manually construct the TypeDef::Struct as it would be after checking
    // Get TypeId by creating a fresh var and matching its kind
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
    let struct_def = StructDef {
        name: "Wrapper".to_string(),
        symbol: struct_sym,
        generic_params: vec![checker_gen_param],
        fields: vec![Field {
            name: "value".to_string(),
            symbol: field_val_sym,
            ty: Ty::new(TyKind::Var(gen_param_id)), // Field type is the generic var T
            span: dummy_span(),
        }],
        span: dummy_span(),
    };
    // Add the constructed TypeDef to the checker's context
    checker.type_ctx.add_type(struct_sym, "Wrapper".to_string(), TypeDef::Struct(struct_def));

    // Verify the TypeDef exists and has the correct structure
    let type_def = checker.type_ctx.get_type_by_symbol(&struct_sym);
    assert!(type_def.is_some(), "TypeDef for Wrapper not found");

    if let Some(TypeDef::Struct(def)) = type_def {
        assert_eq!(def.name, "Wrapper");
        assert_eq!(def.symbol, struct_sym);
        assert_eq!(def.generic_params.len(), 1);
        let gen_param = &def.generic_params[0];
        assert_eq!(gen_param.name, "T");
        assert_eq!(gen_param.symbol, gen_param_t_sym);
        assert_eq!(gen_param.id, gen_param_id);

        assert_eq!(def.fields.len(), 1);
        assert_eq!(def.fields[0].name, "value");
        assert_eq!(def.fields[0].symbol, field_val_sym);
        assert_eq!(def.fields[0].ty, ty_var(gen_param.id.0));
    } else {
        panic!("Expected TypeDef::Struct, got {:?}", type_def);
    }

    // Manually constructing shouldn't produce errors here
    assert!(checker.errors.is_empty());
}


#[test]
fn test_check_struct_def_field_unknown_type() {
    let mut checker = setup_checker();
    let struct_sym = Symbol::new(20);
    let field_a_sym = Symbol::new(21);

    // Simulate the result of check_struct_definition encountering an unknown type.
    // It should create a StructDef but with TyKind::Error for the field.
    let struct_def = StructDef {
        name: "MyStruct".to_string(),
        symbol: struct_sym,
        generic_params: vec![],
        fields: vec![Field {
            name: "a".to_string(),
            symbol: field_a_sym,
            ty: Ty::new(TyKind::Error), // Simulate checker resolving Unknown to Error
            span: dummy_span(),
        }],
        span: dummy_span(),
    };
    checker.type_ctx.add_type(struct_sym, "MyStruct".to_string(), TypeDef::Struct(struct_def));

    // Add a simulated error that check_struct_definition would have produced
    checker.errors.push(TypeError::InternalError { message: "Simulated error: Field 'a' has unknown type".to_string(), span: Some(dummy_span())});

    // Verify the TypeDef structure
    let type_def = checker.type_ctx.get_type_by_symbol(&struct_sym);
    assert!(type_def.is_some(), "TypeDef for MyStruct not found");
    if let Some(TypeDef::Struct(def)) = type_def {
         assert_eq!(def.fields.len(), 1);
         assert_eq!(def.fields[0].name, "a");
         assert!(matches!(def.fields[0].ty.kind, TyKind::Error), "Expected TyKind::Error");
    } else {
        panic!("Expected TypeDef::Struct");
    }

    // Verify the error was recorded
    assert_eq!(checker.errors.len(), 1, "Expected one error for unknown field type");
}

#[test]
fn test_check_struct_def_potentially_recursive() {
    let mut checker = setup_checker();
    let struct_sym = Symbol::new(30);
    let field_next_sym = Symbol::new(31);
    let option_sym = Symbol::new(100);
    let option_gen_param_sym = Symbol::new(101);
    let option_gen_param_id = TypeId(0); // Dummy ID for test setup

    // Add Option<T> definition manually
    let option_checker_gen_param = CheckerGenericParamDef { name: "T".to_string(), symbol: option_gen_param_sym, id: option_gen_param_id, bounds: vec![], span: dummy_span() };
    let option_def = EnumDef { // Use EnumDef from types module
        name: "Option".to_string(),
        symbol: option_sym,
        generic_params: vec![option_checker_gen_param],
        variants: vec![], // Variants not needed for this test
        span: dummy_span(),
    };
    checker.type_ctx.add_type(option_sym, "Option".to_string(), TypeDef::Enum(option_def));

    // Add Node definition manually, referencing Option<Node>
    let node_struct_def = StructDef {
        name: "Node".to_string(),
        symbol: struct_sym,
        generic_params: vec![],
        fields: vec![Field {
            name: "next".to_string(),
            symbol: field_next_sym,
            // Type is Option<Node>
            ty: ty_named("Option", Some(option_sym), vec![ty_named("Node", Some(struct_sym), vec![])]),
            span: dummy_span(),
        }],
        span: dummy_span(),
    };
    checker.type_ctx.add_type(struct_sym, "Node".to_string(), TypeDef::Struct(node_struct_def));

    // Verify the TypeDef
    let type_def = checker.type_ctx.get_type_by_symbol(&struct_sym);
    assert!(type_def.is_some(), "TypeDef for Node not found");

    if let Some(TypeDef::Struct(def)) = type_def {
        assert_eq!(def.name, "Node");
        assert_eq!(def.fields.len(), 1);
        assert_eq!(def.fields[0].name, "next");
        // Check the resolved type Ty for the field
        let field_ty = &def.fields[0].ty;
        assert!(matches!(field_ty.kind, TyKind::Named { ref name, symbol: Some(s), ref args } if name == "Option" && s == option_sym && args.len() == 1));
        if let TyKind::Named { args, .. } = &field_ty.kind {
             let inner_ty = &args[0];
             // Inner type should resolve back to Node
             assert!(matches!(inner_ty.kind, TyKind::Named { ref name, symbol: Some(s), .. } if name == "Node" && s == struct_sym));
        }
    } else {
        panic!("Expected TypeDef::Struct, got {:?}", type_def);
    }

    // Expect no errors just from adding the definition
    assert!(checker.errors.is_empty(), "Expected no errors for potentially recursive struct definition check");
} 