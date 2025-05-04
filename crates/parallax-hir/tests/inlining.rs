#![allow(unused_imports)] // Temp allow while building tests
#![allow(dead_code)] // Allow unused helper funcs/vars during dev
use parallax_hir::hir::*;
use parallax_hir::hir::PrimitiveType;
use parallax_hir::inlining::perform_inlining;
use parallax_hir::lower::{flatten_hir_expr, lower_module_to_anf_hir};
use parallax_hir::tests::{
    create_typed_module, create_typed_module_with_defs, dummy_span, dummy_ty,
};
use parallax_resolve::types::Symbol;
use parallax_types::types::{
    PrimitiveType as TyKindPrimitiveType, Ty, TyKind, TypedArgument, TypedDefinitions, TypedEnum, TypedExpr,
    TypedExprKind, TypedField, TypedFunction, TypedModule, TypedParameter, TypedPattern,
    TypedPatternKind, TypedStruct, TypedVariant,
};
use std::collections::BTreeMap;
use std::sync::Arc;

// --- Helper to find function by name ---
fn find_fn<'a>(module: &'a HirModule, name: &str) -> Option<&'a HirFunction> {
    module.functions.iter().find(|f| f.name == name)
}

// --- Helper to count nodes roughly (using flatten) ---
fn count_nodes(expr: &HirExpr) -> usize {
    let (bindings, tail) = flatten_hir_expr(expr.clone());
    bindings.len() + 1 // Number of let bindings + 1 for the tail
}

// --- Common Types ---
fn ty_i32() -> Ty {
    dummy_ty(TyKind::Primitive(TyKindPrimitiveType::I32))
}
fn ty_bool() -> Ty {
    dummy_ty(TyKind::Primitive(TyKindPrimitiveType::Bool))
}
fn ty_unit() -> Ty {
    dummy_ty(TyKind::Primitive(TyKindPrimitiveType::Unit))
}
fn ty_fn(params: Vec<Ty>, ret: Ty) -> Ty {
    dummy_ty(TyKind::Function(params, Arc::new(ret)))
}

// --- Test Cases ---

#[test]
fn test_inline_simple_call() {
    // main() { small_callee() }; small_callee() { 1 } -> inline small_callee into main
    let main_sym = Symbol::new(1);
    let callee_sym = Symbol::new(2);
    let mut functions = BTreeMap::new();
    let fn_type = ty_fn(vec![], ty_i32());

    functions.insert(
        callee_sym,
        TypedFunction {
            name: "small_callee".to_string(),
            params: vec![],
            return_type: ty_i32(),
            body: Some(TypedExpr {
                kind: TypedExprKind::IntLiteral {
                    value: 1,
                    suffix: None,
                },
                ty: ty_i32(),
                span: dummy_span(),
            }),
            generic_params: vec![],
            span: dummy_span(),
            is_effectful: false,
        },
    );
    functions.insert(
        main_sym,
        TypedFunction {
            name: "main".to_string(),
            params: vec![],
            return_type: ty_i32(),
        body: Some(TypedExpr {
                kind: TypedExprKind::Call {
                    func_expr: Box::new(TypedExpr {
                        kind: TypedExprKind::Variable {
                            symbol: callee_sym,
                            name: "small_callee".to_string(),
                        },
                        ty: fn_type,
                        span: dummy_span(),
                    }),
                    func_symbol: None,
                    type_args: None,
                    args: vec![],
                },
                ty: ty_i32(),
                span: dummy_span(),
        }),
            generic_params: vec![],
            span: dummy_span(),
            is_effectful: false,
        },
    );

    let typed_module = create_typed_module(functions, Some(main_sym));
    let mut hir_module = lower_module_to_anf_hir(&typed_module);
    let original_node_count_main =
        count_nodes(find_fn(&hir_module, "main").unwrap().body.as_ref().unwrap());

    perform_inlining(&mut hir_module);

    let main_fn = find_fn(&hir_module, "main").expect("Main function missing after inlining");
    let main_body = main_fn
        .body
        .as_ref()
        .expect("Main body missing after inlining");
    let (bindings, tail) = flatten_hir_expr(main_body.clone());

    match tail {
        HirTailExpr::Value(Operand::Const(HirLiteral::IntLiteral { value: 1, ty: PrimitiveType::I32 })) if bindings.is_empty() => { /* OK: Directly returned */
        }
        HirTailExpr::Value(Operand::Var(ret_var)) => {
            assert_eq!(
                bindings.len(),
                1,
                "Expected one binding for the inlined literal"
            );
            let (var, ty, val) = &bindings[0];
            assert_eq!(*var, ret_var);
            assert_eq!(*ty, HirType::Primitive(parallax_hir::PrimitiveType::I32));
            assert_eq!(*val, HirValue::Use(Operand::Const(HirLiteral::IntLiteral { value: 1, ty: PrimitiveType::I32 })));
        }
        _ => panic!(
            "Unexpected tail expression in main after inlining: {:?}",
            tail
        ),
    }
}

#[test]
fn test_no_inline_recursive() {
    // main() { recurse(5) }; recurse(n) { recurse(0) }
    let main_sym = Symbol::new(1);
    let recurse_sym = Symbol::new(2);
    let param_n_sym = Symbol::new(3);
    let mut functions = BTreeMap::new();

    let recurse_params = vec![TypedParameter {
        name: "n".to_string(),
        symbol: param_n_sym,
        ty: ty_i32(),
        is_variadic: false,
        has_default: false,
        span: dummy_span(),
    }];
    let recurse_body_ast = TypedExpr {
        kind: TypedExprKind::IntLiteral {
            value: 0,
            suffix: None,
        },
        ty: ty_i32(),
        span: dummy_span(),
    };
    functions.insert(
        recurse_sym,
        TypedFunction {
            name: "recurse".to_string(),
            params: recurse_params.clone(),
            return_type: ty_i32(),
            body: Some(recurse_body_ast),
            generic_params: vec![],
            span: dummy_span(),
            is_effectful: false,
        },
    );
    
    let main_body_ast = TypedExpr {
        kind: TypedExprKind::Call {
            func_expr: Box::new(TypedExpr {
                kind: TypedExprKind::Variable {
                    symbol: recurse_sym,
                    name: "recurse".to_string(),
                },
                ty: ty_fn(vec![ty_i32()], ty_i32()),
                span: dummy_span(),
            }),
            func_symbol: None,
            type_args: None,
            args: vec![TypedArgument {
                name: None,
                value: TypedExpr {
                    kind: TypedExprKind::IntLiteral {
                        value: 5,
                        suffix: None,
                    },
                    ty: ty_i32(),
                    span: dummy_span(),
                },
                span: dummy_span(),
            }],
        },
        ty: ty_i32(),
        span: dummy_span(),
    };
    functions.insert(
        main_sym,
        TypedFunction {
            name: "main".to_string(),
            params: vec![],
            return_type: ty_i32(),
            body: Some(main_body_ast),
            generic_params: vec![],
            span: dummy_span(),
            is_effectful: false,
        },
    );

    let typed_module = create_typed_module(functions, Some(main_sym));
    let mut hir_module = lower_module_to_anf_hir(&typed_module);
    
    // Add recursive call to HIR body
    let recurse_fn_hir = hir_module
        .functions
        .iter_mut()
        .find(|f| f.symbol == recurse_sym)
        .unwrap();
    let recurse_call_val = HirValue::Call {
        func: Operand::Global(recurse_sym),
        args: vec![Operand::Const(HirLiteral::IntLiteral { value: 0, ty: PrimitiveType::I32 })],
    };
    let next_var_id = hir_module.next_var_id;
    let binding_var = HirVar(next_var_id);
    hir_module.next_var_id += 1;
    let tail_return = HirExpr {
        kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(binding_var))),
        ty: HirType::Primitive(parallax_hir::PrimitiveType::I32),
        span: dummy_span(),
    };
    let recurse_call_expr = HirExpr {
        kind: HirExprKind::Let {
            var: binding_var,
            var_ty: HirType::Primitive(parallax_hir::PrimitiveType::I32),
            value: Box::new(recurse_call_val),
            rest: Box::new(tail_return),
        },
        ty: HirType::Primitive(parallax_hir::PrimitiveType::I32),
        span: dummy_span(),
    };
    recurse_fn_hir.body = Some(recurse_call_expr);

    let main_body_before = find_fn(&hir_module, "main").unwrap().body.clone();
    perform_inlining(&mut hir_module);
    let main_body_after = find_fn(&hir_module, "main").unwrap().body.clone();

    // Assert that the main function body did NOT change, as recursive function shouldn't be inlined
    assert_eq!(
        main_body_before, main_body_after,
        "Recursive function should not be inlined"
    );
}

#[test]
fn test_no_inline_large_function() {
    // main() { large_callee() }; large_callee() { lots_of_lets } -> no inline
    let main_sym = Symbol::new(1);
    let callee_sym = Symbol::new(2);
    let mut functions = BTreeMap::new();
    let fn_type = ty_fn(vec![], ty_i32());

    // Create a large body programmatically (more than INLINING_SIZE_THRESHOLD lets)
    let mut large_body_exprs = vec![];
    let mut current_var_sym = Symbol::new(100);
    let mut prev_var_sym = Symbol::new(99); // Dummy start
    for i in 0..50 {
        // Assuming threshold is around 25
        let let_binding = TypedExpr {
            kind: TypedExprKind::Let { 
                pattern: TypedPattern {
                    kind: TypedPatternKind::Identifier {
                        symbol: current_var_sym,
                        name: format!("v{}", i),
                    },
                    ty: ty_i32(),
                    span: dummy_span(),
                },
                value: Box::new(TypedExpr {
                    kind: TypedExprKind::IntLiteral {
                        value: i,
                        suffix: None,
                    },
                    ty: ty_i32(),
                    span: dummy_span(),
                }), // Simplified value
            },
            ty: ty_unit(), 
            span: dummy_span(),
        };
        large_body_exprs.push(let_binding);
        prev_var_sym = current_var_sym;
        current_var_sym = Symbol::new(current_var_sym.id() + 1);
    }
    // Final return uses the last bound variable
    large_body_exprs.push(TypedExpr {
        kind: TypedExprKind::Variable {
            symbol: prev_var_sym,
            name: format!("v{}", 49),
        },
        ty: ty_i32(),
        span: dummy_span(),
    });
    let large_body = TypedExpr {
        kind: TypedExprKind::Block(large_body_exprs),
        ty: ty_i32(),
        span: dummy_span(),
    };

    functions.insert(
        callee_sym,
        TypedFunction {
            name: "large_callee".to_string(),
            params: vec![],
            return_type: ty_i32(),
            body: Some(large_body),
            generic_params: vec![],
            span: dummy_span(),
            is_effectful: false,
        },
    );
    functions.insert(
        main_sym,
        TypedFunction {
            name: "main".to_string(),
            params: vec![],
            return_type: ty_i32(),
            body: Some(TypedExpr {
                kind: TypedExprKind::Call {
                    func_expr: Box::new(TypedExpr {
                        kind: TypedExprKind::Variable {
                            symbol: callee_sym,
                            name: "large_callee".to_string(),
                        },
                        ty: fn_type,
                        span: dummy_span(),
                    }),
                    func_symbol: None,
                    type_args: None,
                    args: vec![],
                },
                ty: ty_i32(),
                span: dummy_span(),
            }),
            generic_params: vec![],
            span: dummy_span(),
            is_effectful: false,
        },
    );

    let typed_module = create_typed_module(functions, Some(main_sym));
    let mut hir_module = lower_module_to_anf_hir(&typed_module);
    
    let main_body_before = find_fn(&hir_module, "main").unwrap().body.clone();
    perform_inlining(&mut hir_module);
    let main_body_after = find_fn(&hir_module, "main").unwrap().body.clone();

    // Assert main function body is unchanged
    assert_eq!(
        main_body_before, main_body_after,
        "Large function should not be inlined"
    );
}

#[test]
fn test_no_inline_mutually_recursive() {
    // main->A; A->B; B->A. Should not inline A or B.
    let main_sym = Symbol::new(1);
    let a_sym = Symbol::new(2);
    let b_sym = Symbol::new(3);
    let mut functions = BTreeMap::new();
    let fn_type = ty_fn(vec![], ty_i32());

    let body_a = TypedExpr {
        kind: TypedExprKind::Call {
            func_expr: Box::new(TypedExpr {
                kind: TypedExprKind::Variable {
                    symbol: b_sym,
                    name: "B".to_string(),
                },
                ty: fn_type.clone(),
                span: dummy_span(),
            }),
            func_symbol: None,
            type_args: None,
            args: vec![],
        },
        ty: ty_i32(),
        span: dummy_span(),
    };
    let body_b = TypedExpr {
        kind: TypedExprKind::Call {
            func_expr: Box::new(TypedExpr {
                kind: TypedExprKind::Variable {
                    symbol: a_sym,
                    name: "A".to_string(),
                },
                ty: fn_type.clone(),
                span: dummy_span(),
            }),
            func_symbol: None,
            type_args: None,
            args: vec![],
        },
        ty: ty_i32(),
        span: dummy_span(),
    };
    let body_main = TypedExpr {
        kind: TypedExprKind::Call {
            func_expr: Box::new(TypedExpr {
                kind: TypedExprKind::Variable {
                    symbol: a_sym,
                    name: "A".to_string(),
                },
                ty: fn_type.clone(),
                span: dummy_span(),
            }),
            func_symbol: None,
            type_args: None,
            args: vec![],
        },
        ty: ty_i32(),
        span: dummy_span(),
    };

    functions.insert(
        a_sym,
        TypedFunction {
            name: "A".to_string(),
            params: vec![],
            return_type: ty_i32(),
            body: Some(body_a),
            generic_params: vec![],
            span: dummy_span(),
            is_effectful: false,
        },
    );
    functions.insert(
        b_sym,
        TypedFunction {
            name: "B".to_string(),
            params: vec![],
            return_type: ty_i32(),
            body: Some(body_b),
            generic_params: vec![],
            span: dummy_span(),
            is_effectful: false,
        },
    );
    functions.insert(
        main_sym,
        TypedFunction {
            name: "main".to_string(),
            params: vec![],
            return_type: ty_i32(),
            body: Some(body_main),
            generic_params: vec![],
            span: dummy_span(),
            is_effectful: false,
        },
    );

    let typed_module = create_typed_module(functions, Some(main_sym));
    let mut hir_module = lower_module_to_anf_hir(&typed_module);

    let main_body_before = find_fn(&hir_module, "main").unwrap().body.clone();
    perform_inlining(&mut hir_module);
    let main_body_after = find_fn(&hir_module, "main").unwrap().body.clone();

    assert_eq!(
        main_body_before, main_body_after,
        "Mutually recursive functions should not be inlined"
    );
}

#[test]
fn test_inline_multiple_calls() {
    // main calls small_callee twice
    let main_sym = Symbol::new(1);
    let callee_sym = Symbol::new(2);
    let var_c1 = Symbol::new(3);
    let var_c2 = Symbol::new(4);
    let mut functions = BTreeMap::new();
    let fn_type = ty_fn(vec![], ty_i32());

    functions.insert(
        callee_sym,
        TypedFunction {
            name: "callee".to_string(),
            params: vec![],
            return_type: ty_i32(),
            body: Some(TypedExpr {
                kind: TypedExprKind::IntLiteral {
                    value: 1,
                    suffix: None,
                },
                ty: ty_i32(),
                span: dummy_span(),
            }),
            generic_params: vec![],
            span: dummy_span(),
            is_effectful: false,
        },
    );
    
    let call1 = TypedExpr {
        kind: TypedExprKind::Call {
            func_expr: Box::new(TypedExpr {
                kind: TypedExprKind::Variable {
                    symbol: callee_sym,
                    name: "callee".to_string(),
                },
                ty: fn_type.clone(),
                span: dummy_span(),
            }),
            func_symbol: None,
            type_args: None,
            args: vec![],
        },
        ty: ty_i32(),
        span: dummy_span(),
    };
    let let1 = TypedExpr {
        kind: TypedExprKind::Let {
            pattern: TypedPattern {
                kind: TypedPatternKind::Identifier {
                    symbol: var_c1,
                    name: "c1".to_string(),
                },
                ty: ty_i32(),
                span: dummy_span(),
            },
            value: Box::new(call1),
        },
        ty: ty_unit(),
        span: dummy_span(),
    };
    let call2 = TypedExpr {
        kind: TypedExprKind::Call {
            func_expr: Box::new(TypedExpr {
                kind: TypedExprKind::Variable {
                    symbol: callee_sym,
                    name: "callee".to_string(),
                },
                ty: fn_type.clone(),
                span: dummy_span(),
            }),
            func_symbol: None,
            type_args: None,
            args: vec![],
        },
        ty: ty_i32(),
        span: dummy_span(),
    };
    // Simplified return, just return second call result
    let main_body = TypedExpr {
        kind: TypedExprKind::Block(vec![let1, call2]),
        ty: ty_i32(),
        span: dummy_span(),
    };
    functions.insert(
        main_sym,
        TypedFunction {
            name: "main".to_string(),
            params: vec![],
            return_type: ty_i32(),
            body: Some(main_body),
            generic_params: vec![],
            span: dummy_span(),
            is_effectful: false,
        },
    );

    let typed_module = create_typed_module(functions, Some(main_sym));
    let mut hir_module = lower_module_to_anf_hir(&typed_module);
    perform_inlining(&mut hir_module);

    let main_fn = find_fn(&hir_module, "main").expect("Main function missing after inlining");
    let main_body = main_fn
        .body
        .as_ref()
        .expect("Main body missing after inlining");
    let (bindings, tail) = flatten_hir_expr(main_body.clone());

    // Expected bindings (in original order): [let v0 = 1; let v_c1 = v0; let v1 = 1]
    // flatten_hir_expr reverses the order, so bindings vec is: [v1 binding, v_c1 binding, v0 binding]
    assert_eq!(
        bindings.len(),
        3,
        "Expected three bindings after inlining (including the unused one)"
    );

    // Check binding structure (reversed order)
    // bindings[0]: let v1 = 1 (from second inlined call)
    assert!(
        matches!(
            bindings[0].2,
            HirValue::Use(Operand::Const(HirLiteral::IntLiteral { value: 1, ty: PrimitiveType::I32 }))
        ),
        "Binding 0 (v1) should be inlined constant 1"
    );
    let v1 = bindings[0].0; // Get the HirVar for v1

    // bindings[2]: let v0 = 1 (from first inlined call)
    assert!(
        matches!(
            bindings[2].2,
            HirValue::Use(Operand::Const(HirLiteral::IntLiteral { value: 1, ty: PrimitiveType::I32 }))
        ),
        "Binding 2 (v0) should be inlined constant 1"
    );
    let v0 = bindings[2].0; // Get the HirVar for v0

    // bindings[1]: let v_c1 = v0 (assignment of unused var_c1)
    assert!(
        matches!(bindings[1].2, HirValue::Use(Operand::Var(var)) if var == v0),
        "Binding 1 (v_c1) should be assignment from first call's result (v0)"
    );

    // Check tail returns the variable from the second inlined call's result binding (v1)
    assert!(
        matches!(tail, HirTailExpr::Value(Operand::Var(ret_var)) if ret_var == v1),
        "Tail should return result of second call (v1)"
    );
}

// TODO: test_inline_nested_calls
// TODO: test_inline_with_conditionals
// TODO: test_inline_with_match
// TODO: test_inline_preserves_bindings (more complex variable renaming)
