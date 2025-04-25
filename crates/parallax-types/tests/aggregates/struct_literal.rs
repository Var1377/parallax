// tests/aggregates/struct_literal.rs
use crate::*; // Import helpers from tests/mod.rs
use parallax_types::{
    context::TypeEnvironment, // Import TypeEnvironment
    error::TypeError,
    // Import TypeDef, StructDef, GenericParamDef, Field
    types::{PrimitiveType, Ty, TyKind, TypedExpr, TypedExprKind, TypeId, TypeDef, StructDef, GenericParamDef as CheckerGenericParamDef, Field},
};
use parallax_resolve::{
    types::{ResolvedExpr, ResolvedExprKind, ResolvedType, Symbol, ResolvedGenericParamDef, ResolvedStruct, ResolvedField},
    PrimitiveType as ResolvePrimitiveType,
};
use std::sync::Arc;
use std::collections::HashMap;

// Helper to create a ResolvedExpr for Struct literal
fn resolved_struct_literal(
    struct_symbol: Symbol,
    fields: Vec<(String, ResolvedExpr)>,
    base: Option<ResolvedExpr>,
    rt: ResolvedType, // Need resolved type for the literal itself
) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Struct {
            struct_symbol,
            fields,
            base: base.map(Box::new),
        },
        span: dummy_span(),
        resolved_type: rt,
    }
}

// Helper to create a ResolvedExpr for a variable path
fn resolved_var(name: &str, symbol: Symbol, rt: ResolvedType) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Variable { binding_symbol: symbol, name: name.to_string() },
        span: dummy_span(),
        resolved_type: rt,
    }
}

#[test]
fn test_struct_simple() {
    let mut checker = setup_checker();
    let struct_sym = Symbol::new(1);
    let field_x_sym = Symbol::new(2);
    let field_y_sym = Symbol::new(3);

    add_test_struct_def(&mut checker, "Point", struct_sym, vec![
        ("x".to_string(), field_x_sym, ty_prim(PrimitiveType::I32)),
        ("y".to_string(), field_y_sym, ty_prim(PrimitiveType::Bool)),
    ]);

    let struct_rt = ResolvedType::UserDefined { symbol: struct_sym, type_args: None };
    let struct_expr = resolved_struct_literal(
        struct_sym,
        vec![
            ("x".to_string(), resolved_lit_int(10)),
            ("y".to_string(), resolved_lit_bool(true)),
        ],
        None,
        struct_rt.clone()
    );

    let expected_ty = ty_named("Point", Some(struct_sym), vec![]);

    let result = checker::expr::type_check_expression(&mut checker, &struct_expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(matches!(typed_expr.kind, TypedExprKind::Struct { ref name, ref fields, .. } if name == "Point" && fields.len() == 2));
    if let TypedExprKind::Struct { ref fields, .. } = typed_expr.kind {
        assert_eq!(fields.iter().find(|(n, _)| n == "x").unwrap().1.ty, ty_prim(PrimitiveType::I32));
        assert_eq!(fields.iter().find(|(n, _)| n == "y").unwrap().1.ty, ty_prim(PrimitiveType::Bool));
    }
    assert!(checker.errors.is_empty());
}

#[test]
fn test_struct_missing_field() {
    let mut checker = setup_checker();
    let struct_sym = Symbol::new(1);
    let field_x_sym = Symbol::new(2);
    let field_y_sym = Symbol::new(3);

    add_test_struct_def(&mut checker, "Point", struct_sym, vec![
        ("x".to_string(), field_x_sym, ty_prim(PrimitiveType::I32)),
        ("y".to_string(), field_y_sym, ty_prim(PrimitiveType::Bool)),
    ]);

    let struct_rt = ResolvedType::UserDefined { symbol: struct_sym, type_args: None };
    let struct_expr = resolved_struct_literal(
        struct_sym,
        vec![("x".to_string(), resolved_lit_int(10))], // Missing 'y'
        None,
        struct_rt.clone()
    );

    let result = checker::expr::type_check_expression(&mut checker, &struct_expr, None);
    assert!(result.is_err(), "Expected Err for missing struct field");
    assert!(matches!(result.err().unwrap(), TypeError::MissingField { field, struct_name, .. } if field == "y" && struct_name == "Point"));
}

#[test]
fn test_struct_unknown_field() {
    let mut checker = setup_checker();
    let struct_sym = Symbol::new(1);
    let field_x_sym = Symbol::new(2);

    add_test_struct_def(&mut checker, "Point", struct_sym, vec![
        ("x".to_string(), field_x_sym, ty_prim(PrimitiveType::I32)),
    ]);

    let struct_rt = ResolvedType::UserDefined { symbol: struct_sym, type_args: None };
    let struct_expr = resolved_struct_literal(
        struct_sym,
        vec![
            ("x".to_string(), resolved_lit_int(10)),
            ("z".to_string(), resolved_lit_int(0)), // Unknown field 'z'
        ],
        None,
        struct_rt.clone()
    );

    let result = checker::expr::type_check_expression(&mut checker, &struct_expr, None);
    assert!(result.is_err(), "Expected Err for unknown struct field");
    assert!(matches!(result.err().unwrap(), TypeError::UnknownStructField { field, struct_name, .. } if field == "z" && struct_name == "Point"));
}

#[test]
fn test_struct_field_type_mismatch() {
    let mut checker = setup_checker();
    let struct_sym = Symbol::new(1);
    let field_x_sym = Symbol::new(2);

    add_test_struct_def(&mut checker, "Point", struct_sym, vec![
        ("x".to_string(), field_x_sym, ty_prim(PrimitiveType::I32)),
    ]);

    let struct_rt = ResolvedType::UserDefined { symbol: struct_sym, type_args: None };
    let struct_expr = resolved_struct_literal(
        struct_sym,
        vec![("x".to_string(), resolved_lit_bool(false))], // 'x' expects i32, got bool
        None,
        struct_rt.clone()
    );

    let result = checker::expr::type_check_expression(&mut checker, &struct_expr, None);
    assert!(result.is_err(), "Expected Err for struct field type mismatch");
    assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
}

#[test]
fn test_struct_update_syntax() {
    let mut checker = setup_checker();
    let struct_sym = Symbol::new(1);
    let field_x_sym = Symbol::new(2);
    let field_y_sym = Symbol::new(3);
    let base_var_sym = Symbol::new(4);

    add_test_struct_def(&mut checker, "Point", struct_sym, vec![
        ("x".to_string(), field_x_sym, ty_prim(PrimitiveType::I32)),
        ("y".to_string(), field_y_sym, ty_prim(PrimitiveType::Bool)),
    ]);

    let point_ty = ty_named("Point", Some(struct_sym), vec![]);
    let point_rt = ResolvedType::UserDefined { symbol: struct_sym, type_args: None };

    // Add base variable 'base_point' to env
    checker._type_env = Arc::new({
        let mut env = TypeEnvironment::new();
        env.add("base_point".to_string(), point_ty.clone());
        env
    });

    let base_expr = resolved_var("base_point", base_var_sym, point_rt.clone());

    // Struct literal: Point { y: false, ..base_point }
    let struct_expr = resolved_struct_literal(
        struct_sym,
        vec![("y".to_string(), resolved_lit_bool(false))], // Override 'y'
        Some(base_expr),
        point_rt.clone()
    );

    let result = checker::expr::type_check_expression(&mut checker, &struct_expr, None);
    assert!(result.is_ok(), "Expected Ok for struct update syntax, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, point_ty);
    assert!(matches!(typed_expr.kind, TypedExprKind::Struct { ref name, base: Some(_), .. } if name == "Point" ));
    if let TypedExprKind::Struct { ref fields, .. } = typed_expr.kind {
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].0, "y"); // Only the overridden field should be listed explicitly
        assert_eq!(fields[0].1.ty, ty_prim(PrimitiveType::Bool));
    }
    assert!(checker.errors.is_empty());
}

#[test]
fn test_struct_update_syntax_type_mismatch() {
    let mut checker = setup_checker();
    let struct_sym = Symbol::new(1);
    let field_x_sym = Symbol::new(2);
    let base_var_sym = Symbol::new(4);

    add_test_struct_def(&mut checker, "Point", struct_sym, vec![
        ("x".to_string(), field_x_sym, ty_prim(PrimitiveType::I32)),
    ]);

    let point_rt = ResolvedType::UserDefined { symbol: struct_sym, type_args: None };

    // Add base variable 'base_bool' of wrong type (bool)
    checker._type_env = Arc::new({
        let mut env = TypeEnvironment::new();
        env.add("base_bool".to_string(), ty_prim(PrimitiveType::Bool));
        env
    });

    let base_expr = resolved_var("base_bool", base_var_sym, ResolvedType::Primitive(ResolvePrimitiveType::Bool));

    // Struct literal: Point { ..base_bool }
    let struct_expr = resolved_struct_literal(
        struct_sym,
        vec![],
        Some(base_expr),
        point_rt.clone()
    );

    let result = checker::expr::type_check_expression(&mut checker, &struct_expr, None);
    assert!(result.is_err(), "Expected Err for struct update syntax type mismatch");
    assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { expected, found, .. } if expected == "Point" && found == "bool"));
}

#[test]
fn test_struct_generic() {
    let mut checker = setup_checker();
    let struct_sym = Symbol::new(1);
    let field_val_sym = Symbol::new(2);
    // Symbol for the generic param 'T' as defined in the struct
    let struct_gen_param_sym = Symbol::new(100);
    // Get a TypeId for the checker by creating a fresh var and extracting its ID
    let gen_param_id = match checker.inference_ctx.fresh_var().kind {
        TyKind::Var(id) => id,
        _ => panic!("Fresh var was not TyKind::Var"),
    };

    // Define the generic struct in the checker's context
    let checker_gen_param = CheckerGenericParamDef {
        name: "T".to_string(),
        symbol: struct_gen_param_sym,
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
    checker.type_ctx.add_type(struct_sym, "Wrapper".to_string(), TypeDef::Struct(struct_def));

    // Instantiate: Wrapper<i32> { value: 42 }
    let struct_rt = ResolvedType::UserDefined {
        symbol: struct_sym,
        type_args: Some(vec![ResolvedType::Primitive(ResolvePrimitiveType::I32)]),
    };
    let struct_expr = resolved_struct_literal(
        struct_sym,
        vec![("value".to_string(), resolved_lit_int(42))], // value: 42
        None,
        struct_rt.clone()
    );

    let expected_ty = ty_named("Wrapper", Some(struct_sym), vec![ty_prim(PrimitiveType::I32)]);

    let result = checker::expr::type_check_expression(&mut checker, &struct_expr, None);
    assert!(result.is_ok(), "Expected Ok for generic struct literal, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    if let TypedExprKind::Struct { ref fields, .. } = typed_expr.kind {
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].0, "value");
        assert_eq!(fields[0].1.ty, ty_prim(PrimitiveType::I32)); // Field type should be i32
    }
    assert!(checker.errors.is_empty());
} 