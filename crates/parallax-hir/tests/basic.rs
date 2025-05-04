use parallax_hir::tests::{dummy_span, dummy_ty, create_typed_module}; // Explicit import of helpers
use parallax_hir::hir::{HirExprKind, HirValue, HirTailExpr, HirType, HirLiteral, Operand, PrimitiveType as HirPrimitiveType};
use parallax_hir::lower::lower_module_to_anf_hir;
use parallax_types::types::{TypedFunction, TypedExpr, TypedExprKind, TyKind, PrimitiveType, TypedPattern, TypedPatternKind, TypedParameter};
use parallax_resolve::types::Symbol;
use std::collections::BTreeMap;

#[test]
fn test_lower_empty_module() {
    let typed_module = create_typed_module(BTreeMap::new(), None);
    let hir_module = lower_module_to_anf_hir(&typed_module);

    assert_eq!(hir_module.name, "main"); // Lowering currently hardcodes name
    assert!(hir_module.functions.is_empty());
    assert!(hir_module.structs.is_empty());
    assert!(hir_module.enums.is_empty());
    assert_eq!(hir_module.entry_point, None);
    // next_var_id might be 0 or higher depending on LoweringContext internal init
}

#[test]
fn test_lower_simple_return_literal() {
    let func_sym = Symbol::new(1);
    let mut functions = BTreeMap::new();
    functions.insert(func_sym, TypedFunction {
        name: "main".to_string(),
        params: vec![],
        return_type: dummy_ty(TyKind::Primitive(PrimitiveType::I32)),
        body: Some(TypedExpr {
            kind: TypedExprKind::IntLiteral { value: 42, suffix: None },
            ty: dummy_ty(TyKind::Primitive(PrimitiveType::I32)),
            span: dummy_span(),
        }),
        generic_params: vec![],
        span: dummy_span(),
        is_effectful: false,
    });

    let typed_module = create_typed_module(functions, Some(func_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);

    assert_eq!(hir_module.entry_point, Some(func_sym));
    assert_eq!(hir_module.functions.len(), 1);

    let main_fn = &hir_module.functions[0];
    assert_eq!(main_fn.symbol, func_sym);
    assert_eq!(main_fn.name, "main");
    assert!(main_fn.signature.params.is_empty());
    assert_eq!(main_fn.signature.return_type, HirType::Primitive(HirPrimitiveType::I32));

    // --- Check Body Structure --- 
    let body = main_fn.body.as_ref().expect("Function body should exist");
    assert_eq!(body.ty, HirType::Primitive(HirPrimitiveType::I32));
    
    match &body.kind {
        HirExprKind::Tail(HirTailExpr::Value(Operand::Const(HirLiteral::IntLiteral { value: 42, .. }))) => { /* OK */ }
        HirExprKind::Let { var, var_ty, value: val_box, rest } => {
            assert_eq!(var_ty, &HirType::Primitive(HirPrimitiveType::I32));
            match &**val_box { HirValue::Use(Operand::Const(HirLiteral::IntLiteral { value: 42, .. })) => {}, _ => panic!("Expected let value Use(Const(42)) got {:?}", val_box) }
            match &rest.kind { HirExprKind::Tail(HirTailExpr::Value(Operand::Var(ret_var))) => assert_eq!(ret_var, var), _ => panic!("Expected rest to be Tail(Return(Var)) got {:?}", rest.kind) }
        }
        _ => panic!("Unexpected HIR body kind: {:?}", body.kind),
    }
}

#[test]
fn test_lower_simple_let() {
    let func_sym = Symbol::new(1);
    let var_sym = Symbol::new(2);
    let mut functions = BTreeMap::new();

    let let_expr = TypedExpr {
        kind: TypedExprKind::Let {
            pattern: TypedPattern {
                kind: TypedPatternKind::Identifier { symbol: var_sym, name: "x".to_string() },
                ty: dummy_ty(TyKind::Primitive(PrimitiveType::I64)),
                span: dummy_span(),
            },
            value: Box::new(TypedExpr {
                kind: TypedExprKind::IntLiteral { value: 10, suffix: None },
                ty: dummy_ty(TyKind::Primitive(PrimitiveType::I64)),
                span: dummy_span(),
            }),
        },
        ty: dummy_ty(TyKind::Primitive(PrimitiveType::I64)), 
        span: dummy_span(),
    };
    let block_expr = TypedExpr {
        kind: TypedExprKind::Block(vec![let_expr,
            TypedExpr { 
                kind: TypedExprKind::Variable { symbol: var_sym, name: "x".to_string() },
                ty: dummy_ty(TyKind::Primitive(PrimitiveType::I64)),
                span: dummy_span(),
            }
        ]),
        ty: dummy_ty(TyKind::Primitive(PrimitiveType::I64)),
        span: dummy_span(),
    };

    functions.insert(func_sym, TypedFunction {
        name: "main".to_string(),
        params: vec![],
        return_type: dummy_ty(TyKind::Primitive(PrimitiveType::I64)),
        body: Some(block_expr),
        generic_params: vec![],
        span: dummy_span(),
        is_effectful: false,
    });

    let typed_module = create_typed_module(functions, Some(func_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);

    let main_fn = &hir_module.functions[0];
    let body = main_fn.body.as_ref().expect("Body should exist");

    match &body.kind {
        HirExprKind::Let { var: let_var, var_ty, value, rest } => {
            assert_eq!(var_ty, &HirType::Primitive(HirPrimitiveType::I64));
            match &**value { HirValue::Use(Operand::Const(HirLiteral::IntLiteral { value: 10, .. })) => {}, _ => panic!("Expected let value Use(Const(10)) got {:?}", value) }
            match &rest.kind { HirExprKind::Tail(HirTailExpr::Value(Operand::Var(ret_var))) => assert_eq!(ret_var, let_var), _ => panic!("Expected rest to be Tail(Return(Var))") }
        }
        _ => panic!("Unexpected HIR body kind for simple let: {:?}", body.kind)
    }
}

#[test]
fn test_lower_variable_use() {
    let func_sym = Symbol::new(1);
    let var_sym_a = Symbol::new(2);
    let var_sym_b = Symbol::new(3);
    let mut functions = BTreeMap::new();

    let let_a = TypedExpr {
        kind: TypedExprKind::Let {
            pattern: TypedPattern {
                kind: TypedPatternKind::Identifier { symbol: var_sym_a, name: "a".to_string() },
                ty: dummy_ty(TyKind::Primitive(PrimitiveType::I32)),
                span: dummy_span(),
            },
            value: Box::new(TypedExpr {
                kind: TypedExprKind::IntLiteral { value: 5, suffix: None },
                ty: dummy_ty(TyKind::Primitive(PrimitiveType::I32)),
                span: dummy_span(),
            }),
        },
        ty: dummy_ty(TyKind::Primitive(PrimitiveType::Unit)),
        span: dummy_span(),
    };
    let let_b = TypedExpr {
        kind: TypedExprKind::Let {
            pattern: TypedPattern {
                kind: TypedPatternKind::Identifier { symbol: var_sym_b, name: "b".to_string() },
                ty: dummy_ty(TyKind::Primitive(PrimitiveType::I32)),
                span: dummy_span(),
            },
            value: Box::new(TypedExpr { 
                kind: TypedExprKind::Variable { symbol: var_sym_a, name: "a".to_string() },
                ty: dummy_ty(TyKind::Primitive(PrimitiveType::I32)),
                span: dummy_span(), 
            }), 
        },
        ty: dummy_ty(TyKind::Primitive(PrimitiveType::Unit)),
        span: dummy_span(),
    };
     let return_b = TypedExpr { 
        kind: TypedExprKind::Variable { symbol: var_sym_b, name: "b".to_string() },
        ty: dummy_ty(TyKind::Primitive(PrimitiveType::I32)),
        span: dummy_span(),
    };

    let block_expr = TypedExpr {
        kind: TypedExprKind::Block(vec![let_a, let_b, return_b]),
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

    let (hir_var_a, rest1) = match &body.kind {
        HirExprKind::Let { var, var_ty, value, rest } => {
            assert_eq!(var_ty, &HirType::Primitive(HirPrimitiveType::I32));
            match &**value { HirValue::Use(Operand::Const(HirLiteral::IntLiteral { value: 5, .. })) => {}, _ => panic!("Expected let value 5") }
            (var, rest)
        }
        _ => panic!("Expected outer Let for a")
    };

    let (hir_var_b, rest2) = match &rest1.kind {
        HirExprKind::Let { var, var_ty, value, rest } => {
             assert_eq!(var_ty, &HirType::Primitive(HirPrimitiveType::I32));
             match &**value { 
                HirValue::Use(Operand::Var(used_var)) => assert_eq!(used_var, hir_var_a), 
                 _ => panic!("Expected let value Use(Var) for a")
             }
             (var, rest)
        }
         _ => panic!("Expected inner Let for b")
    };

    match &rest2.kind {
        HirExprKind::Tail(HirTailExpr::Value(Operand::Var(ret_var))) => assert_eq!(ret_var, hir_var_b), 
        _ => panic!("Expected final return b")
    }
}

#[test]
fn test_lower_function_with_params() {
    let func_sym = Symbol::new(1);
    let param_sym_x = Symbol::new(2);
    let mut functions = BTreeMap::new();

    let params = vec![TypedParameter {
        name: "x".to_string(),
        symbol: param_sym_x,
        ty: dummy_ty(TyKind::Primitive(PrimitiveType::I32)),
        is_variadic: false,
        has_default: false,
        span: dummy_span(),
    }];

    functions.insert(func_sym, TypedFunction {
        name: "identity".to_string(),
        params: params.clone(), // Clone params here
        return_type: dummy_ty(TyKind::Primitive(PrimitiveType::I32)),
        body: Some(TypedExpr { // Body simply returns the parameter
            kind: TypedExprKind::Variable { symbol: param_sym_x, name: "x".to_string() },
            ty: dummy_ty(TyKind::Primitive(PrimitiveType::I32)),
            span: dummy_span(),
        }),
        generic_params: vec![],
        span: dummy_span(),
        is_effectful: false,
    });

    let typed_module = create_typed_module(functions, Some(func_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);

    assert_eq!(hir_module.functions.len(), 1);
    let id_fn = &hir_module.functions[0];

    // Check signature
    assert_eq!(id_fn.signature.params.len(), 1);
    let (param_hir_var, param_hir_ty) = &id_fn.signature.params[0];
    assert_eq!(param_hir_ty, &HirType::Primitive(HirPrimitiveType::I32));
    // We expect the parameter HirVar to be HirVar(0) as it's the first var created
    // assert_eq!(param_hir_var, &parallax_hir::hir::HirVar(0)); // Removed assertion - ID check is fragile

    // Check body
    let body = id_fn.body.as_ref().expect("Function body should exist");
    match &body.kind {
        // The body should directly return the parameter variable
        HirExprKind::Tail(HirTailExpr::Value(Operand::Var(ret_var))) => {
            assert_eq!(ret_var, param_hir_var);
        }
        _ => panic!("Unexpected HIR body kind: {:?}", body.kind),
    }
}