use parallax_hir::tests::{dummy_span, dummy_ty, create_typed_module, create_typed_module_with_defs, dummy_expr}; // Explicit import of helpers
use parallax_hir::hir::{HirValue, HirTailExpr, HirType, HirLiteral, Operand, AggregateKind, ProjectionKind, ResolvePrimitiveType};
use parallax_hir::lower::{flatten_hir_expr, lower_module_to_anf_hir};
use parallax_types::types::{TypedFunction, TypedExpr, TypedExprKind, TyKind, PrimitiveType, TypedPattern, TypedPatternKind, TypedStruct, TypedField}; // Added Struct/Field/Argument
use parallax_resolve::types::Symbol;
use std::collections::HashMap;
use std::sync::Arc;

// Placeholder - tests will be moved here later.

#[test]
fn test_lower_tuple_construct_and_project() {
    use parallax_syntax::ast::common::Literal as AstLiteral;
    let tuple_ty = dummy_ty(TyKind::Tuple(vec![
        dummy_ty(TyKind::Primitive(PrimitiveType::I32)),
        dummy_ty(TyKind::Primitive(PrimitiveType::Bool)),
    ]));
    let i32_ty = dummy_ty(TyKind::Primitive(PrimitiveType::I32));

    let let_symbol = Symbol::new(2);
    let let_pattern = TypedPattern {
        kind: TypedPatternKind::Identifier { symbol: let_symbol, name: "t".to_string() },
        ty: tuple_ty.clone(),
        span: dummy_span(),
    };
    let let_value = TypedExpr {
        kind: TypedExprKind::Tuple(vec![
            dummy_expr(TypedExprKind::Literal(AstLiteral::Int(1)), TyKind::Primitive(PrimitiveType::I32)),
            dummy_expr(TypedExprKind::Literal(AstLiteral::Bool(true)), TyKind::Primitive(PrimitiveType::Bool)),
        ]),
        ty: tuple_ty.clone(),
        span: dummy_span(),
    };
    let let_expr = dummy_expr(TypedExprKind::Let { pattern: let_pattern, value: Box::new(let_value) }, TyKind::Tuple(vec![]));

    let field_expr = TypedExpr {
        kind: TypedExprKind::Field {
            object: Box::new(TypedExpr {
                kind: TypedExprKind::Variable { symbol: let_symbol, name: "t".to_string() },
                ty: tuple_ty.clone(),
                span: dummy_span(),
            }),
            field_name: "0".to_string(),
            field_symbol: Symbol::new(99), 
        },
        ty: i32_ty.clone(),
        span: dummy_span(),
    };

    let block_expr = dummy_expr(TypedExprKind::Block(vec![let_expr, field_expr]), TyKind::Primitive(PrimitiveType::I32));

    let func_sym = Symbol::new(1);
    let mut functions = HashMap::new();
    functions.insert(func_sym, TypedFunction {
        name: "test".to_string(),
        params: vec![],
        return_type: i32_ty.clone(),
        body: Some(block_expr),
        generic_params: vec![],
        span: dummy_span(),
        is_effectful: false,
    });
    
    let typed_module = create_typed_module(functions, Some(func_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);

    assert_eq!(hir_module.functions.len(), 1, "Should have one function");
    let hir_expr = hir_module.functions[0].body.as_ref().expect("Body should exist");

    let (bindings, tail) = flatten_hir_expr(hir_expr.clone());

    let aggregate_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Aggregate { kind: AggregateKind::Tuple, fields } 
        if fields.len() == 2 && matches!(fields[0], Operand::Const(HirLiteral::Int(1))) && matches!(fields[1], Operand::Const(HirLiteral::Bool(true)))
    ));
    assert!(aggregate_binding.is_some(), "Binding for Aggregate Tuple not found");
    let (agg_var_ref, agg_ty, _) = aggregate_binding.unwrap(); 
    let agg_var = *agg_var_ref;
    assert_eq!(agg_ty, &HirType::Tuple(vec![HirType::Primitive(ResolvePrimitiveType::I32), HirType::Primitive(ResolvePrimitiveType::Bool)]));

    let t_var_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Use(Operand::Var(used_var)) if matches!(used_var, agg_var)));
    assert!(t_var_binding.is_some(), "Binding for variable 't' not found");
    let (t_var_ref, t_ty, _) = t_var_binding.unwrap();
    let t_var = *t_var_ref;
    assert_eq!(t_ty, agg_ty);

    let projection_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Project { base, projection } 
        if matches!(base, Operand::Var(base_var) if *base_var == t_var) && matches!(projection, ProjectionKind::TupleIndex(0))
    ));
    assert!(projection_binding.is_some(), "Binding for Projection TupleIndex(0) not found");
    let (proj_var_ref, proj_ty, _) = projection_binding.unwrap(); 
    let proj_var = *proj_var_ref;
    assert_eq!(proj_ty, &HirType::Primitive(ResolvePrimitiveType::I32));

    match tail {
        HirTailExpr::Return(op) => assert_eq!(op, Operand::Var(proj_var)),
        _ => panic!("Expected Tail Return at the end, found {:?}", tail)
    }
}

#[test]
fn test_lower_struct_construct_and_project() {
    let func_sym = Symbol::new(1);
    let var_sym_p = Symbol::new(2);
    let struct_sym = Symbol::new(10);
    let field_sym_x = Symbol::new(11);
    let field_sym_y = Symbol::new(12);
    let mut functions = HashMap::new();
    let mut structs = HashMap::new();

    structs.insert(struct_sym, TypedStruct {
        symbol: struct_sym,
        name: "Point".to_string(),
        fields: vec![
            TypedField { name: "x".to_string(), symbol: field_sym_x, ty: dummy_ty(TyKind::Primitive(PrimitiveType::I32)), is_public: true, span: dummy_span() }, 
            TypedField { name: "y".to_string(), symbol: field_sym_y, ty: dummy_ty(TyKind::Primitive(PrimitiveType::Bool)), is_public: true, span: dummy_span() }, 
        ],
        generic_params: vec![],
        span: dummy_span(),
    });

    let let_p = TypedExpr {
        kind: TypedExprKind::Let {
            pattern: TypedPattern {
                kind: TypedPatternKind::Identifier { symbol: var_sym_p, name: "p".to_string() },
                ty: dummy_ty(TyKind::Named { name: "Point".to_string(), args: vec![] }),
                span: dummy_span(),
            },
            value: Box::new(TypedExpr { 
                kind: TypedExprKind::Struct {
                    name: "Point".to_string(),
                    fields: vec![
                        ("x".to_string(), TypedExpr {
                            kind: TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Int(10)),
                            ty: dummy_ty(TyKind::Primitive(PrimitiveType::I32)),
                            span: dummy_span(),
                        }),
                        ("y".to_string(), TypedExpr {
                            kind: TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Bool(false)),
                            ty: dummy_ty(TyKind::Primitive(PrimitiveType::Bool)),
                            span: dummy_span(),
                        }),
                    ],
                    base: None,
                },
                ty: dummy_ty(TyKind::Named { name: "Point".to_string(), args: vec![] }),
                span: dummy_span(),
            }),
        },
        ty: dummy_ty(TyKind::Primitive(PrimitiveType::Unit)), 
        span: dummy_span(),
    };

    let proj_px = TypedExpr {
        kind: TypedExprKind::Field {
            object: Box::new(TypedExpr {
                kind: TypedExprKind::Variable { symbol: var_sym_p, name: "p".to_string() },
                ty: dummy_ty(TyKind::Named { name: "Point".to_string(), args: vec![] }),
                span: dummy_span(),
            }),
            field_name: "x".to_string(), 
            field_symbol: field_sym_x, 
        },
        ty: dummy_ty(TyKind::Primitive(PrimitiveType::I32)), 
        span: dummy_span(),
    };

    let block_expr = TypedExpr {
        kind: TypedExprKind::Block(vec![let_p, proj_px]),
        ty: dummy_ty(TyKind::Primitive(PrimitiveType::I32)),
        span: dummy_span(),
    };

    functions.insert(func_sym, TypedFunction {
        name: "main".to_string(),
        params: vec![],
        return_type: dummy_ty(TyKind::Primitive(PrimitiveType::I32)),
        body: Some(block_expr),
        generic_params: vec![],
        span: dummy_span(),
        is_effectful: false,
    });

    let typed_module = create_typed_module_with_defs(functions, structs, HashMap::new(), Some(func_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);

    assert_eq!(hir_module.functions.len(), 1);
    let hir_expr = hir_module.functions[0].body.as_ref().expect("Body should exist");

    let (bindings, tail) = flatten_hir_expr(hir_expr.clone());

    let aggregate_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Aggregate { kind: AggregateKind::Struct(s), fields } 
        if *s == struct_sym && fields.len() == 2 && matches!(fields[0], Operand::Const(HirLiteral::Int(10))) && matches!(fields[1], Operand::Const(HirLiteral::Bool(false)))
    ));
    assert!(aggregate_binding.is_some(), "Binding for Aggregate Struct not found");
    let (agg_var_ref, agg_ty, _) = aggregate_binding.unwrap(); 
    let agg_var = *agg_var_ref;
    assert_eq!(agg_ty, &HirType::Adt(struct_sym));

    let p_var_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Use(Operand::Var(used_var)) if matches!(used_var, agg_var)));
    assert!(p_var_binding.is_some(), "Binding for variable 'p' not found");
    let (p_var_ref, p_ty, _) = p_var_binding.unwrap();
    let p_var = *p_var_ref;
    assert_eq!(p_ty, agg_ty);

    let projection_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Project { base, projection } 
        if matches!(base, Operand::Var(base_var) if *base_var == p_var) && matches!(projection, ProjectionKind::Field(f) if *f == field_sym_x)
    ));
    assert!(projection_binding.is_some(), "Binding for Projection Field(x) not found");
    let (proj_var_ref, proj_ty, _) = projection_binding.unwrap(); 
    let proj_var = *proj_var_ref;
    assert_eq!(proj_ty, &HirType::Primitive(ResolvePrimitiveType::I32));

    match tail {
        HirTailExpr::Return(op) => assert_eq!(op, Operand::Var(proj_var)),
        _ => panic!("Expected Tail Return at the end, found {:?}", tail)
    }
}

#[test]
fn test_lower_array_construct_and_project() {
    let func_sym = Symbol::new(1);
    let var_sym_arr = Symbol::new(2);
    let mut functions = HashMap::new();

    let ty_i32 = dummy_ty(TyKind::Primitive(PrimitiveType::I32));
    let ty_array = dummy_ty(TyKind::Array(Arc::new(ty_i32.clone()), 2));

    let let_arr = TypedExpr {
        kind: TypedExprKind::Let {
            pattern: TypedPattern {
                kind: TypedPatternKind::Identifier { symbol: var_sym_arr, name: "arr".to_string() },
                ty: ty_array.clone(),
                span: dummy_span(),
            },
            value: Box::new(TypedExpr { 
                kind: TypedExprKind::Array(vec![
                    dummy_expr(TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Int(10)), TyKind::Primitive(PrimitiveType::I32)),
                    dummy_expr(TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Int(20)), TyKind::Primitive(PrimitiveType::I32)),
                ]),
                ty: ty_array.clone(),
                span: dummy_span(),
            }),
        },
        ty: dummy_ty(TyKind::Primitive(PrimitiveType::Unit)), 
        span: dummy_span(),
    };

    let proj_arr1 = TypedExpr {
        kind: TypedExprKind::Field { 
            object: Box::new(TypedExpr {
                kind: TypedExprKind::Variable { symbol: var_sym_arr, name: "arr".to_string() },
                ty: ty_array.clone(),
                span: dummy_span(),
            }),
            field_name: "1".to_string(), 
            field_symbol: Symbol::new(99),
        },
        ty: ty_i32.clone(), 
        span: dummy_span(),
    };

    let block_expr = TypedExpr {
        kind: TypedExprKind::Block(vec![let_arr, proj_arr1]),
        ty: ty_i32.clone(),
        span: dummy_span(),
    };

    functions.insert(func_sym, TypedFunction {
        name: "main".to_string(),
        params: vec![],
        return_type: ty_i32.clone(),
        body: Some(block_expr),
        generic_params: vec![],
        span: dummy_span(),
        is_effectful: false,
    });

    let typed_module = create_typed_module(functions, Some(func_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);

    assert_eq!(hir_module.functions.len(), 1);
    let hir_expr = hir_module.functions[0].body.as_ref().expect("Body should exist");

    let (bindings, tail) = flatten_hir_expr(hir_expr.clone());

    let aggregate_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Aggregate { kind: AggregateKind::Array, fields } 
        if fields.len() == 2 && matches!(fields[0], Operand::Const(HirLiteral::Int(10))) && matches!(fields[1], Operand::Const(HirLiteral::Int(20)))
    ));
    assert!(aggregate_binding.is_some(), "Binding for Aggregate Array not found");
    let (agg_var_ref, agg_ty, _) = aggregate_binding.unwrap(); 
    let agg_var = *agg_var_ref;
    assert!(matches!(agg_ty, HirType::Array(elem_ty, 2) if **elem_ty == HirType::Primitive(ResolvePrimitiveType::I32)), "Incorrect aggregate type");

    let arr_var_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Use(Operand::Var(used_var)) if matches!(used_var, agg_var)));
    assert!(arr_var_binding.is_some(), "Binding for variable 'arr' not found");
    let (arr_var_ref, arr_ty, _) = arr_var_binding.unwrap();
    let arr_var = *arr_var_ref;
    assert_eq!(arr_ty, agg_ty);

    let projection_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Project { base, projection } 
        if matches!(base, Operand::Var(base_var) if *base_var == arr_var) && matches!(projection, ProjectionKind::ArrayIndex(Operand::Const(HirLiteral::Int(1))))
    ));
    assert!(projection_binding.is_some(), "Binding for Project(arr[Const(1)]) not found");
    let (proj_var_ref, proj_ty, _) = projection_binding.unwrap(); 
    let proj_var = *proj_var_ref;
    assert_eq!(proj_ty, &HirType::Primitive(ResolvePrimitiveType::I32));

    match tail {
        HirTailExpr::Return(op) => assert_eq!(op, Operand::Var(proj_var)),
        _ => panic!("Expected Tail Return at the end, found {:?}", tail)
    }
}

#[test]
fn test_lower_array_index_variable() {
    let func_sym = Symbol::new(1);
    let var_sym_arr = Symbol::new(2);
    let var_sym_i = Symbol::new(3);
    let mut functions = HashMap::new();

    let ty_i32 = dummy_ty(TyKind::Primitive(PrimitiveType::I32));
    let ty_usize = dummy_ty(TyKind::Primitive(PrimitiveType::U64)); 
    let ty_array = dummy_ty(TyKind::Array(Arc::new(ty_i32.clone()), 2));

    let let_arr = TypedExpr {
        kind: TypedExprKind::Let {
            pattern: TypedPattern {
                kind: TypedPatternKind::Identifier { symbol: var_sym_arr, name: "arr".to_string() },
                ty: ty_array.clone(),
                span: dummy_span(),
            },
            value: Box::new(TypedExpr { 
                kind: TypedExprKind::Array(vec![
                    dummy_expr(TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Int(5)), TyKind::Primitive(PrimitiveType::I32)),
                    dummy_expr(TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Int(6)), TyKind::Primitive(PrimitiveType::I32)),
                ]),
                ty: ty_array.clone(),
                span: dummy_span(),
            }),
        },
        ty: dummy_ty(TyKind::Primitive(PrimitiveType::Unit)), 
        span: dummy_span(),
    };

    let let_i = TypedExpr {
        kind: TypedExprKind::Let {
            pattern: TypedPattern {
                kind: TypedPatternKind::Identifier { symbol: var_sym_i, name: "i".to_string() },
                ty: ty_usize.clone(),
                span: dummy_span(),
            },
            value: Box::new(TypedExpr { 
                kind: TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Int(1)), 
                ty: ty_usize.clone(),
                span: dummy_span(),
            }),
        },
        ty: dummy_ty(TyKind::Primitive(PrimitiveType::Unit)), 
        span: dummy_span(),
    };

    let proj_arri = TypedExpr {
        kind: TypedExprKind::Field { 
            object: Box::new(TypedExpr {
                kind: TypedExprKind::Variable { symbol: var_sym_arr, name: "arr".to_string() },
                ty: ty_array.clone(),
                span: dummy_span(),
            }),
            field_name: "i".to_string(), 
            field_symbol: var_sym_i, 
        },
        ty: ty_i32.clone(), 
        span: dummy_span(),
    };


    let block_expr = TypedExpr {
        kind: TypedExprKind::Block(vec![let_arr, let_i, proj_arri]),
        ty: ty_i32.clone(),
        span: dummy_span(),
    };

    functions.insert(func_sym, TypedFunction {
        name: "main".to_string(),
        params: vec![],
        return_type: ty_i32.clone(),
        body: Some(block_expr),
        generic_params: vec![],
        span: dummy_span(),
        is_effectful: false,
    });

    let typed_module = create_typed_module(functions, Some(func_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);

    let main_fn = &hir_module.functions[0];
    let body = main_fn.body.as_ref().expect("Body should exist");

    let (bindings, tail) = flatten_hir_expr(body.clone());

    let aggregate_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Aggregate { kind: AggregateKind::Array, fields } 
        if fields.len() == 2 && matches!(fields[0], Operand::Const(HirLiteral::Int(5))) && matches!(fields[1], Operand::Const(HirLiteral::Int(6)))
    ));
    assert!(aggregate_binding.is_some(), "Binding for Aggregate Array not found");
    let (agg_var_ref, _, _) = aggregate_binding.unwrap();
    let agg_var = *agg_var_ref;

    let arr_var_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Use(Operand::Var(used_var)) if matches!(used_var, agg_var)));
    assert!(arr_var_binding.is_some(), "Binding for variable 'arr' not found");
    let (arr_var_ref, _, _) = arr_var_binding.unwrap();
    let arr_var = *arr_var_ref;

    let index_i_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Use(Operand::Const(HirLiteral::Int(1)))));
    assert!(index_i_binding.is_some(), "Binding for 'let i = 1' not found");
    let (index_var_ref, index_ty, _) = index_i_binding.unwrap();
    let index_var = *index_var_ref;
    assert_eq!(index_ty, &HirType::Primitive(ResolvePrimitiveType::U64), "Variable 'i' should have type U64");

    let projection_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Project { base, projection }
        if matches!(base, Operand::Var(base_var) if *base_var == arr_var) && matches!(projection, ProjectionKind::ArrayIndex(Operand::Var(idx_var)) if *idx_var == index_var)
    ));
    assert!(projection_binding.is_some(), "Binding for Project(arr[i]) not found");
    let (proj_var_ref, proj_ty, _) = projection_binding.unwrap();
    let proj_var = *proj_var_ref;
    assert_eq!(proj_ty, &HirType::Primitive(ResolvePrimitiveType::I32));

    match tail {
        HirTailExpr::Return(op) => assert_eq!(op, Operand::Var(proj_var)),
        _ => panic!("Expected Tail Return at the end, found {:?}", tail)
    }
}

#[test]
fn test_lower_nested_tuple_project() {
    use parallax_syntax::ast::common::Literal as AstLiteral;

    let ty_i32 = dummy_ty(TyKind::Primitive(PrimitiveType::I32));
    let ty_bool = dummy_ty(TyKind::Primitive(PrimitiveType::Bool));
    let ty_inner_tuple = dummy_ty(TyKind::Tuple(vec![ty_i32.clone(), ty_bool.clone()]));
    let ty_outer_tuple = dummy_ty(TyKind::Tuple(vec![ty_inner_tuple.clone(), ty_i32.clone()]));

    let let_symbol = Symbol::new(2);
    let let_pattern = TypedPattern {
        kind: TypedPatternKind::Identifier { symbol: let_symbol, name: "t".to_string() },
        ty: ty_outer_tuple.clone(),
        span: dummy_span(),
    };

    // let t = ((1, true), 2);
    let let_value = TypedExpr {
        kind: TypedExprKind::Tuple(vec![
            TypedExpr { // Inner tuple (1, true)
                kind: TypedExprKind::Tuple(vec![
                    dummy_expr(TypedExprKind::Literal(AstLiteral::Int(1)), TyKind::Primitive(PrimitiveType::I32)),
                    dummy_expr(TypedExprKind::Literal(AstLiteral::Bool(true)), TyKind::Primitive(PrimitiveType::Bool)),
                ]),
                ty: ty_inner_tuple.clone(),
                span: dummy_span(),
            },
            dummy_expr(TypedExprKind::Literal(AstLiteral::Int(2)), TyKind::Primitive(PrimitiveType::I32)), // Outer element 2
        ]),
        ty: ty_outer_tuple.clone(),
        span: dummy_span(),
    };

    let let_expr = dummy_expr(TypedExprKind::Let { pattern: let_pattern, value: Box::new(let_value) }, TyKind::Primitive(PrimitiveType::Unit)); // Let returns unit

    // Access t.0
    let proj_t0 = TypedExpr {
        kind: TypedExprKind::Field {
            object: Box::new(TypedExpr {
                kind: TypedExprKind::Variable { symbol: let_symbol, name: "t".to_string() },
                ty: ty_outer_tuple.clone(),
                span: dummy_span(),
            }),
            field_name: "0".to_string(),
            field_symbol: Symbol::new(98), // Placeholder symbol for field access
        },
        ty: ty_inner_tuple.clone(), // Type of t.0
        span: dummy_span(),
    };

    // Access (t.0).1
    let proj_t01 = TypedExpr {
        kind: TypedExprKind::Field {
            object: Box::new(proj_t0.clone()), // Use the result of the previous projection
            field_name: "1".to_string(),
            field_symbol: Symbol::new(99), // Placeholder symbol for field access
        },
        ty: ty_bool.clone(), // Type of t.0.1
        span: dummy_span(),
    };

    // Block: let t = ...; t.0.1
    let block_expr = dummy_expr(TypedExprKind::Block(vec![let_expr, proj_t01]), ty_bool.kind.clone()); // Pass TyKind

    let func_sym = Symbol::new(1);
    let mut functions = HashMap::new();
    functions.insert(func_sym, TypedFunction {
        name: "test_nested".to_string(),
        params: vec![],
        return_type: ty_bool.clone(),
        body: Some(block_expr),
        generic_params: vec![],
        span: dummy_span(),
        is_effectful: false,
    });
    
    let typed_module = create_typed_module(functions, Some(func_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);

    assert_eq!(hir_module.functions.len(), 1);
    let hir_expr = hir_module.functions[0].body.as_ref().expect("Body should exist");

    let (bindings, tail) = flatten_hir_expr(hir_expr.clone());

    // 1. Find the binding for the inner tuple: let inner_tup = (1, true)
    let inner_agg_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Aggregate { kind: AggregateKind::Tuple, fields } 
        if fields.len() == 2 && matches!(fields[0], Operand::Const(HirLiteral::Int(1))) && matches!(fields[1], Operand::Const(HirLiteral::Bool(true)))
    ));
    assert!(inner_agg_binding.is_some(), "Binding for inner tuple Aggregate (1, true) not found");
    let (inner_tup_var, inner_tup_ty, _) = inner_agg_binding.unwrap(); // inner_tup_var is &HirVar
    assert!(matches!(inner_tup_ty, HirType::Tuple(elems) if elems.len() == 2));

    // 2. Find the binding for the outer tuple: let outer_tup = (inner_tup_var, 2)
    let outer_agg_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Aggregate { kind: AggregateKind::Tuple, fields } 
        if fields.len() == 2 && matches!(fields[0], Operand::Var(v) if v == *inner_tup_var) && matches!(fields[1], Operand::Const(HirLiteral::Int(2)))
    ));
    assert!(outer_agg_binding.is_some(), "Binding for outer tuple Aggregate (inner_tup, 2) not found");
    let (outer_tup_var, outer_tup_ty, _) = outer_agg_binding.unwrap(); // outer_tup_var is &HirVar
    assert!(matches!(outer_tup_ty, HirType::Tuple(elems) if elems.len() == 2));

    // 3. Find the binding for t = use outer_tup
    let t_var_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Use(Operand::Var(used_var)) if used_var == outer_tup_var)); // Dereference outer_tup_var here
    assert!(t_var_binding.is_some(), "Binding for variable 't' not found");
    let (t_var, t_ty, _) = t_var_binding.unwrap(); // t_var is &HirVar
    assert_eq!(t_ty, outer_tup_ty);

    // 4. Find the projection t.0: let proj0 = t.0
    let projection0_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Project { base, projection } 
        if matches!(base, Operand::Var(base_var) if base_var == t_var) && matches!(projection, ProjectionKind::TupleIndex(0))
    ));
    assert!(projection0_binding.is_some(), "Binding for Projection t.0 not found");
    let (proj0_var, proj0_ty, _) = projection0_binding.unwrap(); // proj0_var is &HirVar
    assert_eq!(proj0_ty, inner_tup_ty); // Type should be the inner tuple type

    // 5. Find the projection (t.0).1: let proj1 = proj0.1
    let projection1_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Project { base, projection } 
        if matches!(base, Operand::Var(base_var) if base_var == proj0_var) && matches!(projection, ProjectionKind::TupleIndex(1))
    ));
    assert!(projection1_binding.is_some(), "Binding for Projection proj0.1 not found");
    let (proj1_var, proj1_ty, _) = projection1_binding.unwrap(); // proj1_var is &HirVar
    assert_eq!(proj1_ty, &HirType::Primitive(ResolvePrimitiveType::Bool)); // Type should be bool

    // 6. Check final return
    match tail {
        // Match the Operand::Var directly and extract the HirVar inside
        HirTailExpr::Return(Operand::Var(ret_var)) => {
            // Compare the extracted HirVar (ret_var) with the dereferenced expected HirVar (&proj1_var)
            assert_eq!(ret_var, *proj1_var, "Returned variable should be the result of the final projection");
        }
        // Handle other Operand types or unexpected TailExpr kinds
        HirTailExpr::Return(op) => panic!("Expected final return to be Operand::Var, found {:?}", op),
        _ => panic!("Expected TailExpr::Return at the end, found {:?}", tail)
    }
}