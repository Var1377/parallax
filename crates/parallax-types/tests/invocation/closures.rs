// tests/invocation/closures.rs
use crate::*; // Import helpers from tests/mod.rs
use parallax_resolve::{
    types::{ResolvedExpr, ResolvedExprKind, ResolvedParameter, ResolvedType, Symbol},
    PrimitiveType as ResolvePrimitiveType,
};
use parallax_types::{
    error::TypeError,
    types::{PrimitiveType, Ty, TyKind, TypeId, TypedExpr, TypedExprKind, FunctionSignature, ParamType, TypeDef},
};
use std::sync::Arc;

// --- Type Helpers (Copied from mod.rs / other test files) ---
fn ty_func(params: Vec<Ty>, ret: Ty) -> Ty {
    Ty::with_span(TyKind::Function(params, Arc::new(ret)), dummy_span())
}

// --- Block/Let Helpers (Copied from block_expr.rs) ---
fn resolved_block(exprs: Vec<ResolvedExpr>) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Block(exprs),
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    }
}

fn resolved_let(
    pattern: ResolvedPattern,
    value: ResolvedExpr,
    type_annotation: Option<ResolvedType>,
) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Let {
            pattern,
            value: Box::new(value),
            type_annotation,
        },
        span: dummy_span(),
        resolved_type: ResolvedType::Primitive(ResolvePrimitiveType::Unit),
    }
}

// --- Helper to create ResolvedExpr for Lambda ---
fn resolved_lambda(params: Vec<ResolvedParameter>, body: ResolvedExpr) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Lambda {
            params,
            body: Box::new(body),
        },
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown, // Type checker will determine this
    }
}

// --- Helper to create ResolvedParameter ---
// Note: This might need adjustment based on the actual ResolvedParameter definition
fn resolved_param(
    name: &str,
    symbol: Symbol,
    param_type: Option<ResolvedType>,
) -> ResolvedParameter {
    ResolvedParameter {
        name: name.to_string(),
        symbol,
        param_type: param_type.unwrap_or(ResolvedType::Unknown),
        is_variadic: false,
        has_default: false,
        span: dummy_span(),
    }
}

#[test]
fn test_lambda_infer_simple() {
    let mut checker = setup_checker();
    let param_sym = Symbol::new(1);

    // |x| x + 1
    let lambda_expr = resolved_lambda(
        vec![resolved_param("x", param_sym, None)], // Infer param type
        resolved_binary_expr(
            ResolvedExpr { // x
                kind: ResolvedExprKind::Variable { binding_symbol: param_sym, name: "x".to_string() },
                span: dummy_span(),
                resolved_type: ResolvedType::Unknown,
            },
            parallax_syntax::ast::BinaryOp::Add,
            resolved_lit_int(1) // 1
        )
    );

    // Setup Add impl for i32
    let i32_ty = ty_prim(PrimitiveType::I32);
    let add_trait_sym = Symbol::new(100);
    let add_method_sym = Symbol::new(101);
    let add_impl_method_sym = Symbol::new(102);
    add_simple_binary_trait_impl(
        &mut checker,
        "Add", "add",
        i32_ty.clone(), i32_ty.clone(), i32_ty.clone(),
        add_trait_sym, add_method_sym, add_impl_method_sym
    );

    let expected_lambda_ty = ty_func(vec![i32_ty.clone()], i32_ty.clone());

    // Check lambda itself
    let result = checker::expr::type_check_expression(&mut checker, &lambda_expr, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    // Check the inferred type by assigning to a variable with expected type
    let let_sym = Symbol::new(500);
    let let_pat = ResolvedPattern {
        kind: parallax_resolve::types::ResolvedPatternKind::Identifier( "f".to_string()),
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    };
    let let_expr = resolved_let(
        let_pat,
        lambda_expr, // Assign the lambda expr
        Some(ResolvedType::Function { // Explicit type annotation
            param_types: vec![ResolvedType::Primitive(ResolvePrimitiveType::I32)],
            return_type: Box::new(ResolvedType::Primitive(ResolvePrimitiveType::I32)),
        })
    );

    let block_result = checker::expr::type_check_expression(&mut checker, &resolved_block(vec![let_expr]), None);
    assert!(block_result.is_ok(), "Block assignment check failed: {:?}", block_result.err());
    assert!(checker.errors.is_empty(), "Checker errors: {:?}", checker.errors);
}

#[test]
fn test_lambda_explicit_param_type() {
    let mut checker = setup_checker();
    let param_sym = Symbol::new(1);

    // |x: bool| !x
    let lambda_expr = resolved_lambda(
        vec![resolved_param("x", param_sym, Some(ResolvedType::Primitive(ResolvePrimitiveType::Bool)))],
        resolved_unary_expr(
            parallax_syntax::ast::UnaryOp::Not,
            ResolvedExpr { // x
                kind: ResolvedExprKind::Variable { binding_symbol: param_sym, name: "x".to_string() },
                span: dummy_span(),
                resolved_type: ResolvedType::Unknown,
            }
        )
    );

    // Setup Not impl for bool
    let bool_ty = ty_prim(PrimitiveType::Bool);
    let not_trait_sym = Symbol::new(200);
    let not_method_sym = Symbol::new(201);
    let not_impl_method_sym = Symbol::new(202);
    add_simple_unary_trait_impl(
        &mut checker,
        "Not", "not",
        bool_ty.clone(), bool_ty.clone(),
        not_trait_sym, not_method_sym, not_impl_method_sym
    );

    let expected_lambda_ty = ty_func(vec![bool_ty.clone()], bool_ty.clone());

    let result = checker::expr::type_check_expression(&mut checker, &lambda_expr, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_lambda_ty);
    assert!(checker.errors.is_empty());
}


#[test]
fn test_lambda_body_mismatch_return() {
    let mut checker = setup_checker();
    let param_sym = Symbol::new(1);

    // |x: i32| true // Body returns bool
    let lambda_expr = resolved_lambda(
        vec![resolved_param("x", param_sym, Some(ResolvedType::Primitive(ResolvePrimitiveType::I32)))],
        resolved_lit_bool(true)
    );

    // Expect fn(i32) -> bool based on the body
    let inferred_lambda_ty = ty_func(vec![ty_prim(PrimitiveType::I32)], ty_prim(PrimitiveType::Bool));

    // Check the lambda itself
    let result_lambda = checker::expr::type_check_expression(&mut checker, &lambda_expr, None);
    assert!(result_lambda.is_ok(), "Lambda creation should be Ok: {:?}", result_lambda.err());
    let typed_lambda_expr = result_lambda.unwrap();
    assert_eq!(typed_lambda_expr.ty, inferred_lambda_ty);
    assert!(checker.errors.is_empty());

    // Now, try to use it where fn(i32) -> i32 is expected via a let binding
    let let_sym = Symbol::new(501);
     let let_pat = ResolvedPattern {
        kind: parallax_resolve::types::ResolvedPatternKind::Identifier("g".to_string()),
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    };
    let let_expr_mismatch = resolved_let(
        let_pat,
        lambda_expr, // Use the same lambda expression
        Some(ResolvedType::Function { // Explicit INCOMPATIBLE type annotation
            param_types: vec![ResolvedType::Primitive(ResolvePrimitiveType::I32)],
            return_type: Box::new(ResolvedType::Primitive(ResolvePrimitiveType::I32)), // Expect i32 return
        })
    );

    // Check the let binding within a block
    let block_result = checker::expr::type_check_expression(&mut checker, &resolved_block(vec![let_expr_mismatch]), None);
    // The block itself type checks (as Unit), but an error should be logged.
    assert!(block_result.is_ok(), "Block check should be Ok: {:?}", block_result.err());
    assert!(!checker.errors.is_empty(), "Expected type mismatch error");
    assert_eq!(checker.errors.len(), 1);
    assert!(matches!(checker.errors[0], TypeError::TypeMismatch { .. }));
}

#[test]
fn test_lambda_no_params() {
    let mut checker = setup_checker();

    // || "hello"
    let lambda_expr = resolved_lambda(
        vec![], // No params
        resolved_lit_string("hello")
    );

    let expected_lambda_ty = ty_func(vec![], ty_prim(PrimitiveType::String));

    let result = checker::expr::type_check_expression(&mut checker, &lambda_expr, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_lambda_ty);
    assert!(checker.errors.is_empty());
}

#[test]
fn test_lambda_explicit_return_type() {
    // NOTE: Requires parser/resolver support for `-> ReturnType` on lambdas.
    // Assuming ResolvedLambda might have an optional `return_type_annotation` field.
    // If not, this test setup needs modification based on actual AST/Resolved structure.
    let mut checker = setup_checker();
    let param_sym = Symbol::new(1);

    // Define i32_to_string function for the body
    let i32_to_string_sym = Symbol::new(300);
    let i32_to_string_sig = FunctionSignature {
        name: "i32_to_string".to_string(), self_param: None, generic_params: vec![],
        params: vec![ParamType { name: "i".to_string(), ty: ty_prim(PrimitiveType::I32), span: dummy_span() }],
        return_type: ty_prim(PrimitiveType::String),
        span: dummy_span(),
    };
    checker.type_ctx.add_type(i32_to_string_sym, "i32_to_string".to_string(), TypeDef::Function(i32_to_string_sig));

    // |x: i32| -> String { i32_to_string(x) }
    let lambda_expr = resolved_lambda(
        vec![resolved_param("x", param_sym, Some(ResolvedType::Primitive(ResolvePrimitiveType::I32)))],
        ResolvedExpr { // Body: call i32_to_string(x)
            kind: ResolvedExprKind::Call {
                func_symbol: Some(i32_to_string_sym),
                args: vec![ResolvedArgument {
                    name: None,
                    value: ResolvedExpr { // x
                        kind: ResolvedExprKind::Variable { binding_symbol: param_sym, name: "x".to_string() },
                        span: dummy_span(),
                        resolved_type: ResolvedType::Unknown, // Let checker resolve param type
                    },
                    span: dummy_span(),
                }]
            },
            span: dummy_span(),
            resolved_type: ResolvedType::Unknown, // Let checker resolve call result
        }
        // Assume there's a way to attach the explicit return type annotation here
        // e.g., resolved_type: ResolvedType::Function { ..., return_type: Box::new(ResolvedType::Primitive(ResolvePrimitiveType::String)) }
        // If not, this test mainly checks if the body matches the signature, not the explicit annotation handling.
    );

    let expected_lambda_ty = ty_func(vec![ty_prim(PrimitiveType::I32)], ty_prim(PrimitiveType::String));

    let result = checker::expr::type_check_expression(&mut checker, &lambda_expr, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    // The inferred type should match the explicit annotation (if supported) or the body's type
    assert_eq!(typed_expr.ty, expected_lambda_ty);
    assert!(checker.errors.is_empty());
}

#[test]
fn test_lambda_infer_unit_return() {
    let mut checker = setup_checker();
    let param_sym = Symbol::new(1);

    // |x: String| { let _ = x; } // Body is a block ending implicitly with ()
    let lambda_expr = resolved_lambda(
        vec![resolved_param("x", param_sym, Some(ResolvedType::Primitive(ResolvePrimitiveType::String)))],
        resolved_block(vec![
            // Let binding results in Unit for the statement
            resolved_let(
                ResolvedPattern {
                    kind: parallax_resolve::types::ResolvedPatternKind::Identifier( "_".to_string()), // Wildcard binding
                    span: dummy_span(),
                    resolved_type: ResolvedType::Unknown,
                },
                ResolvedExpr { // x
                    kind: ResolvedExprKind::Variable { binding_symbol: param_sym, name: "x".to_string() },
                    span: dummy_span(),
                    resolved_type: ResolvedType::Unknown,
                },
                None // No type annotation for wildcard
            )
            // No final expression, so block returns Unit
        ])
    );

    let expected_lambda_ty = ty_func(vec![ty_prim(PrimitiveType::String)], ty_prim(PrimitiveType::Unit));

    let result = checker::expr::type_check_expression(&mut checker, &lambda_expr, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_lambda_ty);
    assert!(checker.errors.is_empty());
}

#[test]
fn test_lambda_capture_simple_value() {
    let mut checker = setup_checker();
    let capture_sym = Symbol::new(50);
    let param_sym = Symbol::new(51);

    // Setup environment: let y = 10;
    let initial_env = {
        let mut env = TypeEnvironment::new();
        // Assume y is i32 for this test
        env.add("y".to_string(), ty_prim(PrimitiveType::I32));
        Arc::new(env)
    };
    checker._type_env = initial_env;

    // Lambda: |x| x + y
    let lambda_expr = resolved_lambda(
        vec![resolved_param("x", param_sym, None)], // Infer x
        resolved_binary_expr(
            ResolvedExpr { // x
                kind: ResolvedExprKind::Variable { binding_symbol: param_sym, name: "x".to_string() },
                span: dummy_span(),
                resolved_type: ResolvedType::Unknown,
            },
            parallax_syntax::ast::BinaryOp::Add,
            ResolvedExpr { // y (captured)
                kind: ResolvedExprKind::Variable { binding_symbol: capture_sym, name: "y".to_string() },
                span: dummy_span(),
                resolved_type: ResolvedType::Unknown, // Checker looks up y in env
            }
        )
    );

    // Setup Add impl for i32
    let i32_ty = ty_prim(PrimitiveType::I32);
    let add_trait_sym = Symbol::new(100);
    let add_method_sym = Symbol::new(101);
    let add_impl_method_sym = Symbol::new(102);
    add_simple_binary_trait_impl(
        &mut checker,
        "Add", "add",
        i32_ty.clone(), i32_ty.clone(), i32_ty.clone(),
        add_trait_sym, add_method_sym, add_impl_method_sym
    );

    // Expect fn(i32) -> i32 (x inferred as i32 because y is i32 and they are added)
    let expected_lambda_ty = ty_func(vec![i32_ty.clone()], i32_ty.clone());

    let result = checker::expr::type_check_expression(&mut checker, &lambda_expr, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_lambda_ty);
    // TODO: Need a way to inspect the TypedExprKind::Lambda to verify captures if the structure stores them.
    assert!(checker.errors.is_empty(), "Checker errors: {:?}", checker.errors);
}

#[test]
fn test_lambda_passed_as_argument() {
    let mut checker = setup_checker();

    // Define `apply(f: fn(i32) -> String, val: i32) -> String`
    let apply_func_sym = Symbol::new(400);
    let fn_ty = ty_func(vec![ty_prim(PrimitiveType::I32)], ty_prim(PrimitiveType::String));
    let apply_sig = FunctionSignature {
        name: "apply".to_string(), self_param: None, generic_params: vec![],
        params: vec![
            ParamType { name: "f".to_string(), ty: fn_ty.clone(), span: dummy_span() },
            ParamType { name: "val".to_string(), ty: ty_prim(PrimitiveType::I32), span: dummy_span() },
        ],
        return_type: ty_prim(PrimitiveType::String),
        span: dummy_span(),
    };
    checker.type_ctx.add_type(apply_func_sym, "apply".to_string(), TypeDef::Function(apply_sig));

    // Define i32_to_string for lambda body
    let i32_to_string_sym = Symbol::new(300); // Reused from previous test ok?
    let i32_to_string_sig = FunctionSignature {
        name: "i32_to_string".to_string(), self_param: None, generic_params: vec![],
        params: vec![ParamType { name: "i".to_string(), ty: ty_prim(PrimitiveType::I32), span: dummy_span() }],
        return_type: ty_prim(PrimitiveType::String),
        span: dummy_span(),
    };
    checker.type_ctx.add_type(i32_to_string_sym, "i32_to_string".to_string(), TypeDef::Function(i32_to_string_sig));

    // Lambda: |x: i32| i32_to_string(x)
    let param_sym = Symbol::new(401);
    let lambda_expr = resolved_lambda(
        vec![resolved_param("x", param_sym, Some(ResolvedType::Primitive(ResolvePrimitiveType::I32)))],
        ResolvedExpr { // Body: call i32_to_string(x)
            kind: ResolvedExprKind::Call {
                func_symbol: Some(i32_to_string_sym),
                args: vec![ResolvedArgument {
                    name: None,
                    value: ResolvedExpr { kind: ResolvedExprKind::Variable { binding_symbol: param_sym, name: "x".to_string() }, span: dummy_span(), resolved_type: ResolvedType::Unknown },
                    span: dummy_span(),
                }]
            },
            span: dummy_span(),
            resolved_type: ResolvedType::Unknown,
        }
    );

    // Call apply(lambda, 5)
    let call_expr = ResolvedExpr {
        kind: ResolvedExprKind::Call {
            func_symbol: Some(apply_func_sym),
            args: vec![
                ResolvedArgument { name: None, value: lambda_expr, span: dummy_span() },
                ResolvedArgument { name: None, value: resolved_lit_int(5), span: dummy_span() },
            ]
        },
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    };

    let result = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result.is_ok(), "apply(lambda, 5) failed: {:?}", result.err());
    let typed_expr = result.unwrap();

    // apply returns String
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::String));
    assert!(checker.errors.is_empty(), "Checker errors: {:?}", checker.errors);
}

#[test]
fn test_lambda_duplicate_param_names() {
    let mut checker = setup_checker();
    let param_sym_1 = Symbol::new(600);
    let param_sym_2 = Symbol::new(601); // Different symbol, same name

    // |x, x| x
    let lambda_expr = resolved_lambda(
        vec![
            resolved_param("x", param_sym_1, None),
            resolved_param("x", param_sym_2, None), // Duplicate name "x"
        ],
        ResolvedExpr { // Body: return the second x
            kind: ResolvedExprKind::Variable { binding_symbol: param_sym_2, name: "x".to_string() },
            span: dummy_span(),
            resolved_type: ResolvedType::Unknown,
        }
    );

    // Type checking the lambda definition itself should potentially catch this.
    // The exact error might depend on when/how duplicates are checked (resolver vs type checker).
    let result = checker::expr::type_check_expression(&mut checker, &lambda_expr, None);

    // We expect an error, but the specific kind might vary.
    // It could be a specific duplicate parameter error, or an internal error.
    assert!(result.is_err(), "Expected error for duplicate lambda parameter names");
    // Example check (adjust based on actual expected error):
    // assert!(matches!(result.err().unwrap(), TypeError::DuplicateParameter { .. } | TypeError::InternalError { .. }));
}

// TODO: Test errors like duplicate parameter names in lambda
