use parallax_hir::tests::{dummy_span, dummy_ty, create_typed_module, create_typed_module_with_defs, dummy_expr};
use parallax_hir::hir::{HirExprKind, HirValue, HirTailExpr, HirType, HirLiteral, Operand, ResolvePrimitiveType, HirPattern, HirExpr, AggregateKind};
use parallax_hir::lower::{flatten_hir_expr, lower_module_to_anf_hir};
use parallax_types::types::{TypedFunction, TypedExpr, TypedExprKind, TyKind, PrimitiveType, TypedPattern, TypedPatternKind, TypedMatchArm, TypedEnum, TypedVariant, TypedArgument};
use parallax_resolve::types::Symbol;
use std::collections::HashMap;

#[test]
fn test_lower_if_expression() {
    let func_sym = Symbol::new(1);
    let mut functions = HashMap::new();

    let condition_expr = TypedExpr {
        kind: TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Bool(true)),
        ty: dummy_ty(TyKind::Primitive(PrimitiveType::Bool)),
        span: dummy_span(),
    };
    let then_expr = TypedExpr {
        kind: TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Int(1)),
        ty: dummy_ty(TyKind::Primitive(PrimitiveType::I32)),
        span: dummy_span(),
    };
    let else_expr = TypedExpr {
        kind: TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Int(2)),
        ty: dummy_ty(TyKind::Primitive(PrimitiveType::I32)),
        span: dummy_span(),
    };

    let if_expr = TypedExpr {
        kind: TypedExprKind::If {
            condition: Box::new(condition_expr),
            then_branch: Box::new(then_expr),
            else_branch: Some(Box::new(else_expr)),
        },
        ty: dummy_ty(TyKind::Primitive(PrimitiveType::I32)), 
        span: dummy_span(),
    };

    functions.insert(func_sym, TypedFunction {
        name: "main".to_string(),
        params: vec![],
        return_type: dummy_ty(TyKind::Primitive(PrimitiveType::I32)),
        body: Some(if_expr),
        generic_params: vec![],
        span: dummy_span(),
        is_effectful: false,
    });

    let typed_module = create_typed_module(functions, Some(func_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);

    let main_fn = &hir_module.functions[0];
    let body = main_fn.body.as_ref().expect("Body should exist");

    match &body.kind {
        HirExprKind::Let { var: cond_var, value: cond_value, rest: if_rest, .. } => {
            match &**cond_value { HirValue::Use(Operand::Const(HirLiteral::Bool(true))) => { /* ok */ }, _ => panic!("Expected condition value binding") }
            match &if_rest.kind {
                HirExprKind::Tail(HirTailExpr::If { condition, then_branch, else_branch }) => {
                    assert_eq!(condition, &Operand::Var(*cond_var));
                    match &then_branch.kind {
                        HirExprKind::Tail(HirTailExpr::Return(Operand::Const(HirLiteral::Int(1)))) => { /* ok */ }
                        HirExprKind::Let { value, rest, .. } => { 
                            match &**value { HirValue::Use(Operand::Const(HirLiteral::Int(1))) => {}, _ => panic!("Then branch expected let 1") }
                            match &rest.kind { HirExprKind::Tail(HirTailExpr::Return(Operand::Var(_))) => {}, _=> panic!("Then branch rest expected return")}
                        }
                        _ => panic!("Unexpected then branch kind: {:?}", then_branch.kind)
                    }
                     match &else_branch.kind {
                        HirExprKind::Tail(HirTailExpr::Return(Operand::Const(HirLiteral::Int(2)))) => { /* ok */ }
                         HirExprKind::Let { value, rest, .. } => { 
                             match &**value { HirValue::Use(Operand::Const(HirLiteral::Int(2))) => {}, _ => panic!("Else branch expected let 2") }
                             match &rest.kind { HirExprKind::Tail(HirTailExpr::Return(Operand::Var(_))) => {}, _=> panic!("Else branch rest expected return")}
                         }
                         _ => panic!("Unexpected else branch kind: {:?}", else_branch.kind)
                    }
                }
                _ => panic!("Expected if_rest to be Tail(If), got {:?}", if_rest.kind)
            }
        }
        _ => panic!("Expected top level to be Let for condition, got {:?}", body.kind)
    }
}

#[test]
fn test_lower_block_expression() {
    let func_sym = Symbol::new(1);
    let var_sym1 = Symbol::new(2);
    let var_sym2 = Symbol::new(3);
    let mut functions = HashMap::new();

    let expr1 = TypedExpr {
        kind: TypedExprKind::Let {
            pattern: TypedPattern {
                kind: TypedPatternKind::Identifier { symbol: var_sym1, name: "x".to_string() },
                ty: dummy_ty(TyKind::Primitive(PrimitiveType::I32)),
                span: dummy_span(),
            },
            value: Box::new(TypedExpr {
                kind: TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Int(1)),
                ty: dummy_ty(TyKind::Primitive(PrimitiveType::I32)),
                span: dummy_span(),
            }),
        },
        ty: dummy_ty(TyKind::Primitive(PrimitiveType::Unit)),
        span: dummy_span(),
    };
    let expr2 = TypedExpr {
        kind: TypedExprKind::Let {
            pattern: TypedPattern {
                kind: TypedPatternKind::Identifier { symbol: var_sym2, name: "y".to_string() },
                ty: dummy_ty(TyKind::Primitive(PrimitiveType::Bool)),
                span: dummy_span(),
            },
            value: Box::new(TypedExpr {
                kind: TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Bool(true)),
                ty: dummy_ty(TyKind::Primitive(PrimitiveType::Bool)),
                span: dummy_span(), 
            }),
        },
        ty: dummy_ty(TyKind::Primitive(PrimitiveType::Unit)), 
        span: dummy_span(),
    };
    let expr3 = TypedExpr { 
        kind: TypedExprKind::Variable { symbol: var_sym1, name: "x".to_string() },
        ty: dummy_ty(TyKind::Primitive(PrimitiveType::I32)),
        span: dummy_span(),
    };

    let block_expr = TypedExpr {
        kind: TypedExprKind::Block(vec![expr1, expr2, expr3]),
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

    let typed_module = create_typed_module(functions, Some(func_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);

    let main_fn = &hir_module.functions[0];
    let body = main_fn.body.as_ref().expect("Body should exist");

    let (var_x, rest1) = match &body.kind {
        HirExprKind::Let { var, var_ty, value, rest } => {
            assert_eq!(var_ty, &HirType::Primitive(ResolvePrimitiveType::I32));
            match &**value { HirValue::Use(Operand::Const(HirLiteral::Int(1))) => {}, _ => panic!("Expected let value 1") }
            (var, rest)
        }
        _ => panic!("Expected outer Let for x")
    };

    let (_var_y, rest2) = match &rest1.kind {
        HirExprKind::Let { var, var_ty, value, rest } => {
             assert_eq!(var_ty, &HirType::Primitive(ResolvePrimitiveType::Bool));
             match &**value { HirValue::Use(Operand::Const(HirLiteral::Bool(true))) => {}, _ => panic!("Expected let value true") }
             (var, rest)
        }
         _ => panic!("Expected inner Let for y")
    };

    match &rest2.kind {
        HirExprKind::Tail(HirTailExpr::Return(Operand::Var(ret_var))) => {
             assert_eq!(*ret_var, *var_x);
        }
        _ => panic!("Expected final return")
    }
}

#[test]
fn test_lower_enum_match() {
    let func_sym = Symbol::new(1);
    let var_sym_opt = Symbol::new(2);
    let var_sym_x = Symbol::new(3);
    let enum_sym = Symbol::new(10);
    let variant_sym_some = Symbol::new(11);
    let variant_sym_none = Symbol::new(12);
    let mut functions = HashMap::new();
    let mut enums = HashMap::new();

    let ty_i32 = dummy_ty(TyKind::Primitive(PrimitiveType::I32));
    let ty_option_i32 = dummy_ty(TyKind::Named { name: "Option".to_string(), args: vec![] });

    enums.insert(enum_sym, TypedEnum {
        symbol: enum_sym,
        name: "Option".to_string(),
        variants: vec![
            TypedVariant::Tuple { 
                name: "Some".to_string(), 
                symbol: variant_sym_some, 
                types: vec![ty_i32.clone()], 
                span: dummy_span() 
            },
            TypedVariant::Unit { 
                name: "None".to_string(), 
                symbol: variant_sym_none, 
                span: dummy_span() 
            },
        ],
        generic_params: vec![],
        span: dummy_span(),
    });

    let let_opt = TypedExpr {
        kind: TypedExprKind::Let {
            pattern: TypedPattern {
                kind: TypedPatternKind::Identifier { symbol: var_sym_opt, name: "opt".to_string() },
                ty: ty_option_i32.clone(),
                span: dummy_span(),
            },
            value: Box::new(TypedExpr { 
                kind: TypedExprKind::VariantConstructor {
                    enum_name: "Option".to_string(),
                    variant_name: "Some".to_string(),
                    args: vec![TypedArgument { 
                        name: None,
                        value: TypedExpr {
                            kind: TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Int(5)),
                            ty: ty_i32.clone(),
                            span: dummy_span(),
                        },
                        span: dummy_span(),
                    }]
                },
                ty: ty_option_i32.clone(),
                span: dummy_span(),
            }),
        },
        ty: dummy_ty(TyKind::Primitive(PrimitiveType::Unit)), 
        span: dummy_span(),
    };

    let match_expr = TypedExpr {
        kind: TypedExprKind::Match {
            scrutinee: Box::new(TypedExpr {
                kind: TypedExprKind::Variable { symbol: var_sym_opt, name: "opt".to_string() },
                ty: ty_option_i32.clone(),
                span: dummy_span(),
            }),
            arms: vec![
                TypedMatchArm {
                    pattern: TypedPattern {
                        kind: TypedPatternKind::Constructor {
                            enum_name: "Option".to_string(),
                            variant_name: "Some".to_string(),
                            args: Box::new(TypedPattern {
                                kind: TypedPatternKind::Tuple(vec![ 
                                    TypedPattern {
                                        kind: TypedPatternKind::Identifier { symbol: var_sym_x, name: "x".to_string() },
                                        ty: ty_i32.clone(),
                                        span: dummy_span(),
                                    }
                                ]),
                                ty: dummy_ty(TyKind::Tuple(vec![ty_i32.clone()])),
                                span: dummy_span(),
                            })
                        },
                        ty: ty_option_i32.clone(),
                        span: dummy_span(),
                    },
                    body: TypedExpr { 
                        kind: TypedExprKind::Variable { symbol: var_sym_x, name: "x".to_string() },
                        ty: ty_i32.clone(),
                        span: dummy_span(),
                    },
                },
                TypedMatchArm {
                    pattern: TypedPattern {
                        kind: TypedPatternKind::Wildcard,
                        ty: ty_option_i32.clone(),
                        span: dummy_span(),
                    },
                    body: TypedExpr { 
                        kind: TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Int(0)),
                        ty: ty_i32.clone(),
                        span: dummy_span(),
                    },
                },
            ],
        },
        ty: ty_i32.clone(), 
        span: dummy_span(),
    };

    let block_expr = TypedExpr {
        kind: TypedExprKind::Block(vec![let_opt, match_expr]),
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

    let typed_module = create_typed_module_with_defs(functions, HashMap::new(), enums, Some(func_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);

    let main_fn = &hir_module.functions[0];
    let body = main_fn.body.as_ref().expect("Body should exist");

    let (bindings, tail) = flatten_hir_expr(body.clone());

    let aggregate_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Aggregate { kind: AggregateKind::EnumVariant(v), fields } 
        if *v == variant_sym_some && fields.len() == 1 && matches!(fields[0], Operand::Const(HirLiteral::Int(5)))
    ));
    assert!(aggregate_binding.is_some(), "Binding for Aggregate Some(5) not found");
    let (agg_var, agg_ty, _) = aggregate_binding.unwrap(); 
    assert_eq!(agg_ty, &HirType::Adt(enum_sym));

    let opt_var_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Use(Operand::Var(used_var)) if used_var == agg_var));
    assert!(opt_var_binding.is_some(), "Binding for variable 'opt' not found");
    let (opt_var, opt_ty, _) = opt_var_binding.unwrap();
    assert_eq!(opt_ty, agg_ty);

    match tail {
        HirTailExpr::Match { scrutinee, arms, otherwise } => {
            assert_eq!(scrutinee, Operand::Var(*opt_var));
            assert_eq!(arms.len(), 1, "Should have one explicit arm (Some)"); // Because wildcard becomes otherwise

            // Check Arm 1: Some(x) => x
            let (pattern1, body1) = &arms[0];
            match pattern1 {
                HirPattern::Variant { variant_symbol, bindings: pattern_bindings } => {
                    assert_eq!(variant_symbol, &variant_sym_some);
                    assert_eq!(pattern_bindings.len(), 1, "Arm 1 should bind x");
                    let (bound_var_x, bound_ty_x) = &pattern_bindings[0];
                    assert_eq!(*bound_ty_x, HirType::Primitive(ResolvePrimitiveType::I32));

                    match &body1.kind {
                        HirExprKind::Tail(HirTailExpr::Return(Operand::Var(ret_var))) => {
                            assert_eq!(*ret_var, *bound_var_x, "Arm 1 body should return bound var x");
                        },
                        _ => panic!("Arm 1 body is not Tail(Return(Var(x)))")
                    }
                }
                _ => panic!("Arm 1 pattern is not Variant")
            }
            
            // Check Otherwise Branch: _ => 0
            let else_body = otherwise.as_ref().unwrap();
            match &else_body.kind {
                HirExprKind::Tail(HirTailExpr::Return(Operand::Const(HirLiteral::Int(0)))) => {}, // Correct
                _ => panic!("Otherwise body is not Tail(Return(Const(0)))")
            }
        }
        _ => panic!("Expected Tail Match at the end, found {:?}", tail)
    }
}

#[test]
fn test_lower_match_wildcard_patterns() {
    let func_sym = Symbol::new(1);
    let var_sym_opt = Symbol::new(2);
    let wildcard_sym_some = Symbol::new(3); // Symbol for _ in Some(_)
    let enum_sym = Symbol::new(10);
    let variant_sym_some = Symbol::new(11);
    let variant_sym_none = Symbol::new(12);
    let mut functions = HashMap::new();
    let mut enums = HashMap::new();

    let ty_i32 = dummy_ty(TyKind::Primitive(PrimitiveType::I32));
    let ty_option_i32 = dummy_ty(TyKind::Named { name: "Option".to_string(), args: vec![] });

    enums.insert(enum_sym, TypedEnum {
        symbol: enum_sym,
        name: "Option".to_string(),
        variants: vec![
            TypedVariant::Tuple { name: "Some".to_string(), symbol: variant_sym_some, types: vec![ty_i32.clone()], span: dummy_span() },
            TypedVariant::Unit { name: "None".to_string(), symbol: variant_sym_none, span: dummy_span() },
        ],
        generic_params: vec![],
        span: dummy_span(),
    });

    let let_opt = TypedExpr {
        kind: TypedExprKind::Let {
            pattern: TypedPattern {
                kind: TypedPatternKind::Identifier { symbol: var_sym_opt, name: "opt".to_string() },
                ty: ty_option_i32.clone(),
                span: dummy_span(),
            },
            value: Box::new(TypedExpr { 
                kind: TypedExprKind::VariantConstructor {
                    enum_name: "Option".to_string(),
                    variant_name: "None".to_string(),
                    args: vec![] 
                },
                ty: ty_option_i32.clone(),
                span: dummy_span(),
            }),
        },
        ty: dummy_ty(TyKind::Primitive(PrimitiveType::Unit)), 
        span: dummy_span(),
    };

    let match_expr = TypedExpr {
        kind: TypedExprKind::Match {
            scrutinee: Box::new(TypedExpr {
                kind: TypedExprKind::Variable { symbol: var_sym_opt, name: "opt".to_string() },
                ty: ty_option_i32.clone(),
                span: dummy_span(),
            }),
            arms: vec![
                TypedMatchArm {
                    pattern: TypedPattern {
                        kind: TypedPatternKind::Constructor {
                            enum_name: "Option".to_string(),
                            variant_name: "Some".to_string(),
                            args: Box::new(TypedPattern {
                                kind: TypedPatternKind::Tuple(vec![ 
                                    TypedPattern {
                                        kind: TypedPatternKind::Identifier { symbol: wildcard_sym_some, name: "_".to_string() }, 
                                        ty: ty_i32.clone(),
                                        span: dummy_span(),
                                    }
                                ]),
                                ty: dummy_ty(TyKind::Tuple(vec![ty_i32.clone()])),
                                span: dummy_span(),
                            })
                        },
                        ty: ty_option_i32.clone(),
                        span: dummy_span(),
                    },
                    body: TypedExpr { 
                        kind: TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Int(1)),
                        ty: ty_i32.clone(),
                        span: dummy_span(),
                    },
                },
                TypedMatchArm {
                    pattern: TypedPattern {
                        kind: TypedPatternKind::Wildcard, 
                        ty: ty_option_i32.clone(), 
                        span: dummy_span(),
                    },
                    body: TypedExpr { 
                        kind: TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Int(0)),
                        ty: ty_i32.clone(),
                        span: dummy_span(),
                    },
                },
            ],
        },
        ty: ty_i32.clone(), 
        span: dummy_span(),
    };

    let block_expr = TypedExpr {
        kind: TypedExprKind::Block(vec![let_opt, match_expr]),
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

    let typed_module = create_typed_module_with_defs(functions, HashMap::new(), enums, Some(func_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);

    let main_fn = &hir_module.functions[0];
    let body = main_fn.body.as_ref().expect("Body should exist");

    let (bindings, tail) = flatten_hir_expr(body.clone());

    let aggregate_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Aggregate { kind: AggregateKind::EnumVariant(v), fields } 
        if *v == variant_sym_none && fields.is_empty()
    ));
    assert!(aggregate_binding.is_some(), "Binding for Aggregate None not found");
    let (agg_var, _, _) = aggregate_binding.unwrap();

    let opt_var_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Use(Operand::Var(used_var)) if used_var == agg_var));
    assert!(opt_var_binding.is_some(), "Binding for variable 'opt' not found");
    let (opt_var, _, _) = opt_var_binding.unwrap();

    match tail {
        HirTailExpr::Match { scrutinee, arms, otherwise } => {
            assert_eq!(scrutinee, Operand::Var(*opt_var));
            assert_eq!(arms.len(), 1, "Should have one explicit arm (Some)"); // Wildcard becomes otherwise
            assert!(otherwise.is_some(), "Wildcard should become an otherwise branch");

            // Check Arm 1: Some(_) => 1
            let (pattern1, body1) = &arms[0];
            match pattern1 {
                HirPattern::Variant { variant_symbol, bindings: pattern_bindings } => {
                    assert_eq!(variant_symbol, &variant_sym_some);
                    assert!(pattern_bindings.is_empty(), "Arm 1 pattern Some(_) should have empty bindings");
                    match &body1.kind {
                        HirExprKind::Tail(HirTailExpr::Return(Operand::Const(HirLiteral::Int(1)))) => {}, 
                        _ => panic!("Arm 1 body is not Tail(Return(Const(1)))")
                    }
                }
                _ => panic!("Arm 1 pattern is not Variant")
            }
            
            // Check Otherwise Branch Body: _ => 0
            let else_body = otherwise.as_ref().unwrap();
            match &else_body.kind {
                HirExprKind::Tail(HirTailExpr::Return(Operand::Const(HirLiteral::Int(0)))) => {}, 
                _ => panic!("Otherwise body is not Tail(Return(Const(0)))")
            }
        }
        _ => panic!("Expected Tail Match at the end, found {:?}", tail)
    }
}

#[test]
fn test_lower_nested_if() {
    let func_sym = Symbol::new(1);
    let var_sym_a = Symbol::new(2);
    let var_sym_b = Symbol::new(3);
    let mut functions = HashMap::new();

    let ty_i32 = dummy_ty(TyKind::Primitive(PrimitiveType::I32));
    let ty_bool = dummy_ty(TyKind::Primitive(PrimitiveType::Bool));
    let ty_unit = dummy_ty(TyKind::Primitive(PrimitiveType::Unit));

    // let a = true;
    let let_a = TypedExpr {
        kind: TypedExprKind::Let {
            pattern: TypedPattern { kind: TypedPatternKind::Identifier { symbol: var_sym_a, name: "a".to_string() }, ty: ty_bool.clone(), span: dummy_span() },
            value: Box::new(TypedExpr { kind: TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Bool(true)), ty: ty_bool.clone(), span: dummy_span() })
        },
        ty: ty_unit.clone(),
        span: dummy_span()
    };
    // let b = false;
    let let_b = TypedExpr {
        kind: TypedExprKind::Let {
            pattern: TypedPattern { kind: TypedPatternKind::Identifier { symbol: var_sym_b, name: "b".to_string() }, ty: ty_bool.clone(), span: dummy_span() },
            value: Box::new(TypedExpr { kind: TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Bool(false)), ty: ty_bool.clone(), span: dummy_span() })
        },
        ty: ty_unit.clone(),
        span: dummy_span()
    };

    // Outer if: if a { ... } else { 4 }
    let outer_if = TypedExpr {
        kind: TypedExprKind::If {
            condition: Box::new(TypedExpr { kind: TypedExprKind::Variable { symbol: var_sym_a, name: "a".to_string() }, ty: ty_bool.clone(), span: dummy_span() }),
            then_branch: Box::new(TypedExpr { // Inner if: if b { 1 } else { 2 }
                kind: TypedExprKind::If {
                    condition: Box::new(TypedExpr { kind: TypedExprKind::Variable { symbol: var_sym_b, name: "b".to_string() }, ty: ty_bool.clone(), span: dummy_span() }),
                    then_branch: Box::new(TypedExpr { kind: TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Int(1)), ty: ty_i32.clone(), span: dummy_span() }),
                    else_branch: Some(Box::new(TypedExpr { kind: TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Int(2)), ty: ty_i32.clone(), span: dummy_span() }))
                },
                ty: ty_i32.clone(),
                span: dummy_span()
            }),
            else_branch: Some(Box::new(TypedExpr { kind: TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Int(4)), ty: ty_i32.clone(), span: dummy_span() }))
        },
        ty: ty_i32.clone(),
        span: dummy_span()
    };

    let block_expr = TypedExpr {
        kind: TypedExprKind::Block(vec![let_a, let_b, outer_if]),
        ty: ty_i32.clone(),
        span: dummy_span()
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

    // Find bindings for a and b
    let a_binding = bindings.iter().find(|(_, _, val)| matches!(val, HirValue::Use(Operand::Const(HirLiteral::Bool(true)))));
    assert!(a_binding.is_some(), "Binding for 'a' not found");
    let (a_var, _, _) = a_binding.unwrap();

    let b_binding = bindings.iter().find(|(_, _, val)| matches!(val, HirValue::Use(Operand::Const(HirLiteral::Bool(false)))));
    assert!(b_binding.is_some(), "Binding for 'b' not found");
    let (b_var, _, _) = b_binding.unwrap();

    // Check the tail expression (outer if)
    // Find the intermediate binding for the condition 'a'
    let cond_a_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Use(Operand::Var(v)) if v == a_var));
    assert!(cond_a_binding.is_some(), "Intermediate binding for condition 'a' not found");
    let (cond_a_temp_var, _, _) = cond_a_binding.unwrap();

    match tail {
        HirTailExpr::If { condition: outer_cond, then_branch: outer_then, else_branch: outer_else } => {
            // Assert the condition uses the intermediate variable
            assert_eq!(outer_cond, Operand::Var(*cond_a_temp_var), "Outer condition should use intermediate var for 'a'");

            // Check outer else branch -> return 4
            match &outer_else.kind {
                HirExprKind::Tail(HirTailExpr::Return(Operand::Const(HirLiteral::Int(4)))) => { /* ok */ },
                _ => panic!("Outer else branch should return 4, found: {:?}", outer_else.kind)
            }

            // Check outer then branch (should be a Let whose rest is the inner If)
            match &outer_then.kind {
                HirExprKind::Let { var: inner_cond_var, value: inner_cond_value, rest: inner_if_rest, .. } => {
                     // Check the Let binding corresponds to the inner condition 'b'
                     assert!(matches!(inner_cond_value.as_ref(), HirValue::Use(Operand::Var(v)) if v == b_var),
                         "Let value should be Use(Var(b_var)) for inner condition");
                    
                     // Check the rest of the Let is the inner If tail
                     match &inner_if_rest.kind {
                         HirExprKind::Tail(HirTailExpr::If { condition: inner_cond, then_branch: inner_then, else_branch: inner_else }) => {
                             assert_eq!(inner_cond, &Operand::Var(*inner_cond_var), "Inner condition should use intermediate var for 'b'");

                             // Check inner then branch -> return 1
                             match &inner_then.kind {
                                 HirExprKind::Tail(HirTailExpr::Return(Operand::Const(HirLiteral::Int(1)))) => { /* ok */ },
                                 _ => panic!("Inner then branch should return 1, found: {:?}", inner_then.kind)
                             }
                             // Check inner else branch -> return 2
                              match &inner_else.kind {
                                  HirExprKind::Tail(HirTailExpr::Return(Operand::Const(HirLiteral::Int(2)))) => { /* ok */ },
                                  _ => panic!("Inner else branch should return 2, found: {:?}", inner_else.kind)
                             }
                         }
                         _ => panic!("Rest of Let in outer then branch should be Tail(If), found: {:?}", inner_if_rest.kind)
                     }
                }
                _ => panic!("Outer then branch should be a Let binding for the inner condition, found: {:?}", outer_then.kind)
            }
        }
        _ => panic!("Tail expression should be an If, found: {:?}", tail)
    }
}

#[test]
fn test_lower_match_literals() {
    let func_sym = Symbol::new(1);
    let var_sym_val = Symbol::new(2);
    let mut functions = HashMap::new();

    let ty_i32 = dummy_ty(TyKind::Primitive(PrimitiveType::I32));
    let ty_bool = dummy_ty(TyKind::Primitive(PrimitiveType::Bool));
    let ty_unit = dummy_ty(TyKind::Primitive(PrimitiveType::Unit));

    // let val = 1;
    let let_val = TypedExpr {
        kind: TypedExprKind::Let {
            pattern: TypedPattern { kind: TypedPatternKind::Identifier { symbol: var_sym_val, name: "val".to_string() }, ty: ty_i32.clone(), span: dummy_span() },
            value: Box::new(TypedExpr { kind: TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Int(1)), ty: ty_i32.clone(), span: dummy_span() })
        },
        ty: ty_unit.clone(),
        span: dummy_span()
    };

    // match val { 1 => true, _ => false }
    let match_expr = TypedExpr {
        kind: TypedExprKind::Match {
            scrutinee: Box::new(TypedExpr {
                kind: TypedExprKind::Variable { symbol: var_sym_val, name: "val".to_string() },
                ty: ty_i32.clone(),
                span: dummy_span(),
            }),
            arms: vec![
                TypedMatchArm {
                    pattern: TypedPattern {
                        kind: TypedPatternKind::Literal(parallax_syntax::ast::common::Literal::Int(1)),
                        ty: ty_i32.clone(),
                        span: dummy_span(),
                    },
                    body: TypedExpr { 
                        kind: TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Bool(true)),
                        ty: ty_bool.clone(),
                        span: dummy_span(),
                    },
                },
                TypedMatchArm {
                    pattern: TypedPattern {
                        kind: TypedPatternKind::Wildcard,
                        ty: ty_i32.clone(), // Type matches scrutinee
                        span: dummy_span(),
                    },
                    body: TypedExpr { 
                        kind: TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Bool(false)),
                        ty: ty_bool.clone(),
                        span: dummy_span(),
                    },
                },
            ],
        },
        ty: ty_bool.clone(), 
        span: dummy_span(),
    };

    let block_expr = TypedExpr {
        kind: TypedExprKind::Block(vec![let_val, match_expr]),
        ty: ty_bool.clone(),
        span: dummy_span()
    };

    functions.insert(func_sym, TypedFunction {
        name: "main".to_string(),
        params: vec![],
        return_type: ty_bool.clone(),
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

    // Find binding for val = 1
    let val_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Use(Operand::Const(HirLiteral::Int(1)))));
    assert!(val_binding.is_some(), "Binding for 'val' not found");
    let (val_var, _, _) = val_binding.unwrap();

    // Check the tail expression (Match)
    match tail {
        HirTailExpr::Match { scrutinee, arms, otherwise } => {
            assert_eq!(scrutinee, Operand::Var(*val_var), "Match scrutinee should be 'val'");
            assert_eq!(arms.len(), 1, "Should have one explicit arm (literal 1)");
            assert!(otherwise.is_some(), "Should have an otherwise branch for wildcard");

            // Check Arm 1: 1 => true
            let (pattern1, body1) = &arms[0];
            assert_eq!(pattern1, &HirPattern::Const(HirLiteral::Int(1)), "Arm 1 pattern should be Const(1)");
            match &body1.kind {
                HirExprKind::Tail(HirTailExpr::Return(Operand::Const(HirLiteral::Bool(true)))) => { /* ok */ },
                _ => panic!("Arm 1 body should return true, found: {:?}", body1.kind)
            }
            
            // Check Otherwise Branch: _ => false
            let else_body = otherwise.as_ref().unwrap();
            match &else_body.kind {
                HirExprKind::Tail(HirTailExpr::Return(Operand::Const(HirLiteral::Bool(false)))) => { /* ok */ },
                _ => panic!("Otherwise body should return false, found: {:?}", else_body.kind)
            }
        }
        _ => panic!("Tail expression should be Match, found: {:?}", tail)
    }
}

#[test]
fn test_lower_match_binding() {
    let func_sym = Symbol::new(1);
    let var_sym_opt = Symbol::new(2);
    let var_sym_x = Symbol::new(3);
    let enum_sym = Symbol::new(10);
    let variant_sym_some = Symbol::new(11);
    let variant_sym_none = Symbol::new(12);
    let add_intrinsic_sym = Symbol::new(100); // Define add_intrinsic_sym
    let mut functions = HashMap::new();
    let mut enums = HashMap::new();

    let ty_i32 = dummy_ty(TyKind::Primitive(PrimitiveType::I32));
    let ty_option_i32 = dummy_ty(TyKind::Named { name: "Option".to_string(), args: vec![] });
    let ty_unit = dummy_ty(TyKind::Primitive(PrimitiveType::Unit));

    enums.insert(enum_sym, TypedEnum {
        symbol: enum_sym,
        name: "Option".to_string(),
        variants: vec![
            TypedVariant::Tuple { name: "Some".to_string(), symbol: variant_sym_some, types: vec![ty_i32.clone()], span: dummy_span() },
            TypedVariant::Unit { name: "None".to_string(), symbol: variant_sym_none, span: dummy_span() },
        ],
        generic_params: vec![],
        span: dummy_span(),
    });

    // let opt = Some(42);
    let let_opt = TypedExpr {
        kind: TypedExprKind::Let {
            pattern: TypedPattern { kind: TypedPatternKind::Identifier { symbol: var_sym_opt, name: "opt".to_string() }, ty: ty_option_i32.clone(), span: dummy_span() },
            value: Box::new(TypedExpr { 
                kind: TypedExprKind::VariantConstructor {
                    enum_name: "Option".to_string(),
                    variant_name: "Some".to_string(),
                    args: vec![TypedArgument { name: None, value: dummy_expr(TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Int(42)), TyKind::Primitive(PrimitiveType::I32)), span: dummy_span() }]
                },
                ty: ty_option_i32.clone(),
                span: dummy_span(),
            }),
        },
        ty: ty_unit.clone(), 
        span: dummy_span(),
    };

    // match opt { Some(x) => x, None => 0 }
    let match_expr = TypedExpr {
        kind: TypedExprKind::Match {
            scrutinee: Box::new(TypedExpr {
                kind: TypedExprKind::Variable { symbol: var_sym_opt, name: "opt".to_string() },
                ty: ty_option_i32.clone(),
                span: dummy_span(),
            }),
            arms: vec![
                TypedMatchArm {
                    pattern: TypedPattern { // Some(x)
                        kind: TypedPatternKind::Constructor {
                            enum_name: "Option".to_string(),
                            variant_name: "Some".to_string(),
                            args: Box::new(TypedPattern { // Represents the (x) part
                                kind: TypedPatternKind::Tuple(vec![ 
                                    TypedPattern {
                                        kind: TypedPatternKind::Identifier { symbol: var_sym_x, name: "x".to_string() },
                                        ty: ty_i32.clone(),
                                        span: dummy_span(),
                                    }
                                ]),
                                ty: dummy_ty(TyKind::Tuple(vec![ty_i32.clone()])), // Type of (i32)
                                span: dummy_span(),
                            })
                        },
                        ty: ty_option_i32.clone(),
                        span: dummy_span(),
                    },
                    body: TypedExpr { // => x
                        kind: TypedExprKind::Variable { symbol: var_sym_x, name: "x".to_string() },
                        ty: ty_i32.clone(),
                        span: dummy_span(),
                    },
                },
                TypedMatchArm {
                    pattern: TypedPattern { // None
                         kind: TypedPatternKind::Constructor {
                            enum_name: "Option".to_string(),
                            variant_name: "None".to_string(),
                            args: Box::new(TypedPattern { // Represents the () part for unit
                                kind: TypedPatternKind::Tuple(vec![]),
                                ty: dummy_ty(TyKind::Tuple(vec![])),
                                span: dummy_span(),
                            })
                        },
                        ty: ty_option_i32.clone(),
                        span: dummy_span(),
                    },
                    body: TypedExpr { // => 0
                        kind: TypedExprKind::Literal(parallax_syntax::ast::common::Literal::Int(0)),
                        ty: ty_i32.clone(),
                        span: dummy_span(),
                    },
                },
            ],
        },
        ty: ty_i32.clone(), 
        span: dummy_span(),
    };

    let block_expr = TypedExpr {
        kind: TypedExprKind::Block(vec![let_opt, match_expr]),
        ty: ty_i32.clone(),
        span: dummy_span()
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

    let typed_module = create_typed_module_with_defs(functions, HashMap::new(), enums, Some(func_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);

    let main_fn = &hir_module.functions[0];
    let body = main_fn.body.as_ref().expect("Body should exist");

    let (bindings, tail) = flatten_hir_expr(body.clone());

    // Find binding for opt = Some(42)
    let agg_some_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Aggregate { kind: AggregateKind::EnumVariant(v), fields } 
        if *v == variant_sym_some && fields.len() == 1 && matches!(fields[0], Operand::Const(HirLiteral::Int(42)))
    ));
    assert!(agg_some_binding.is_some(), "Binding for Aggregate Some(42) not found");
    let (v_agg_some, agg_ty, _) = agg_some_binding.unwrap(); 
    let opt_var_binding = bindings.iter().find(|(_, _, value)| matches!(value, HirValue::Use(Operand::Var(used_var)) if *used_var == *v_agg_some));
    assert!(opt_var_binding.is_some(), "Binding for 'opt' not found");
    let (opt_var, _, _) = opt_var_binding.unwrap();

    // Check the tail expression (Match)
    match tail {
        HirTailExpr::Match { scrutinee, arms, otherwise } => {
            assert_eq!(scrutinee, Operand::Var(*opt_var), "Match scrutinee should be 'opt'");
            assert_eq!(arms.len(), 2, "Should have two explicit arms (Some, None)");
            assert!(otherwise.is_none(), "Should not have an otherwise branch");

            // Check Arm 1: Some(x) => x
            let (pattern1, body1) = &arms[0];
            match pattern1 {
                HirPattern::Variant { variant_symbol, bindings: pattern_bindings } => {
                    assert_eq!(variant_symbol, &variant_sym_some);
                    assert_eq!(pattern_bindings.len(), 1, "Arm 1 should bind x");
                    let (bound_var_x, bound_ty_x) = &pattern_bindings[0];
                    assert_eq!(*bound_ty_x, HirType::Primitive(ResolvePrimitiveType::I32));

                    match &body1.kind {
                        HirExprKind::Tail(HirTailExpr::Return(Operand::Var(ret_var))) => {
                            assert_eq!(*ret_var, *bound_var_x, "Arm 1 body should return bound var x");
                        },
                        _ => panic!("Arm 1 body is not Tail(Return(Var(x)))")
                    }
                }
                _ => panic!("Arm 1 pattern is not Variant")
            }
        }
        _ => panic!("Tail expression should be Match, found: {:?}", tail)
    }
}