// tests/invocation/function_calls.rs
use crate::*; // Import helpers from tests/mod.rs
use parallax_resolve::{
    types::{ResolvedArgument, ResolvedExpr, ResolvedExprKind, ResolvedType, Symbol},
    PrimitiveType as ResolvePrimitiveType,
};
use parallax_types::{
    error::TypeError,
    types::*,
};
use std::sync::Arc;

// --- Type Helpers (Copied from mod.rs / other test files) ---
fn ty_func(params: Vec<Ty>, ret: Ty) -> Ty {
    Ty::with_span(TyKind::Function(params, Arc::new(ret)), dummy_span())
}

// --- Path Helper ---
fn resolved_path(sym: Symbol, rt: ResolvedType) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Path(sym),
        span: dummy_span(),
        resolved_type: rt,
    }
}

// --- Helper to create ResolvedExpr for Function Call ---
fn resolved_fn_call(
    func_symbol: Symbol,
    args: Vec<ResolvedArgument>,
) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Call { func_symbol: Some(func_symbol), args },
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown, // Type checker will determine this
    }
}

// --- Helper to create ResolvedArgument ---
fn resolved_arg(name: Option<&str>, value: ResolvedExpr) -> ResolvedArgument {
    ResolvedArgument {
        name: name.map(String::from),
        value,
        span: dummy_span(),
    }
}

// --- Helper to add a simple function definition ---
fn add_function_def(
    checker: &mut TypeChecker,
    name: &str,
    sym: Symbol,
    params: Vec<ParamType>,
    return_ty: Ty,
) {
    let func_sig = FunctionSignature {
        name: name.to_string(),
        self_param: None,
        generic_params: vec![], // Add generic param support later
        params,
        return_type: return_ty,
        span: dummy_span(),
    };
    checker
        .type_ctx
        .add_type(sym, name.to_string(), TypeDef::Function(func_sig));
}

// --- Tests ---

#[test]
fn test_function_call_simple() {
    let mut checker = setup_checker();
    let func_sym = Symbol::new(1);
    add_function_def(
        &mut checker,
        "add",
        func_sym,
        vec![
            ParamType { name: "x".to_string(), ty: ty_prim(PrimitiveType::I32), span: dummy_span() },
            ParamType { name: "y".to_string(), ty: ty_prim(PrimitiveType::I32), span: dummy_span() },
        ],
        ty_prim(PrimitiveType::I32),
    );

    let args = vec![
        resolved_arg(None, resolved_lit_int(10)),
        resolved_arg(None, resolved_lit_int(5)),
    ];
    let call_expr = resolved_fn_call(func_sym, args);

    let result = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result.is_ok(), "Expected Ok for add(10, 5), got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::I32));
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_function_call_arg_type_mismatch() {
    let mut checker = setup_checker();
    let func_sym = Symbol::new(2);
    add_function_def(
        &mut checker,
        "greet",
        func_sym,
        vec![ParamType { name: "name".to_string(), ty: ty_prim(PrimitiveType::String), span: dummy_span() }],
        ty_prim(PrimitiveType::Unit),
    );

    // Call greet(123) - expects String
    let args = vec![resolved_arg(None, resolved_lit_int(123))];
    let call_expr = resolved_fn_call(func_sym, args);

    let result = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result.is_err());
    assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
}

#[test]
fn test_function_call_wrong_arg_count_too_few() {
    let mut checker = setup_checker();
    let func_sym = Symbol::new(3);
    add_function_def(
        &mut checker,
        "multiply",
        func_sym,
        vec![
            ParamType { name: "a".to_string(), ty: ty_prim(PrimitiveType::F64), span: dummy_span() },
            ParamType { name: "b".to_string(), ty: ty_prim(PrimitiveType::F64), span: dummy_span() },
        ],
        ty_prim(PrimitiveType::F64),
    );

    // Call multiply(2.0) - expects two args
    let args = vec![resolved_arg(None, resolved_lit_float(2.0))];
    let call_expr = resolved_fn_call(func_sym, args);

    let result = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result.is_err());
    assert!(matches!(result.err().unwrap(), TypeError::WrongNumberOfArguments { expected: 2, found: 1, .. }));
}

#[test]
fn test_function_call_wrong_arg_count_too_many() {
    let mut checker = setup_checker();
    let func_sym = Symbol::new(4);
    add_function_def(
        &mut checker,
        "identity",
        func_sym,
        vec![ParamType { name: "val".to_string(), ty: ty_prim(PrimitiveType::Bool), span: dummy_span() }],
        ty_prim(PrimitiveType::Bool),
    );

    // Call identity(true, false) - expects one arg
    let args = vec![
        resolved_arg(None, resolved_lit_bool(true)),
        resolved_arg(None, resolved_lit_bool(false)),
    ];
    let call_expr = resolved_fn_call(func_sym, args);

    let result = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result.is_err());
    assert!(matches!(result.err().unwrap(), TypeError::WrongNumberOfArguments { expected: 1, found: 2, .. }));
}

#[test]
fn test_function_call_arg_coercion() {
    let mut checker = setup_checker();
    let func_sym = Symbol::new(5);
    add_function_def(
        &mut checker,
        "print_num",
        func_sym,
        vec![ParamType { name: "num".to_string(), ty: ty_prim(PrimitiveType::I64), span: dummy_span() }],
        ty_prim(PrimitiveType::Unit),
    );

    // Call print_num(100) - 100 is IntegerLiteral, expects i64
    let args = vec![resolved_arg(None, resolved_lit_int(100))];
    let call_expr = resolved_fn_call(func_sym, args);

    let result = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result.is_ok(), "Expected Ok for print_num(100), got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::Unit));
    // Check that the argument inside the typed call expression was coerced
    if let TypedExprKind::Call { ref args, .. } = typed_expr.kind {
        assert_eq!(args.len(), 1);
        assert_eq!(args[0].value.ty, ty_prim(PrimitiveType::I64));
    } else {
        panic!("Expected TypedExprKind::Call");
    }
    assert!(checker.errors.is_empty());
}

#[test]
fn test_function_call_unknown_function() {
    let mut checker = setup_checker();
    let unknown_sym = Symbol::new(99);

    let args = vec![resolved_arg(None, resolved_lit_int(1))];
    let call_expr = resolved_fn_call(unknown_sym, args);

    let result = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result.is_err());
    // This might manifest as NotAFunction or InternalError depending on lookup failure point
    assert!(matches!(result.err().unwrap(), TypeError::NotAFunction { .. } | TypeError::InternalError { .. }));
}


// --- Helper to add a generic function definition ---
fn add_generic_function_def(
    checker: &mut TypeChecker,
    name: &str,
    func_sym: Symbol,
    gen_params: Vec<GenericParamDef>,
    params: Vec<ParamType>,
    return_ty: Ty,
) {
    let func_sig = FunctionSignature {
        name: name.to_string(),
        self_param: None,
        generic_params: gen_params,
        params,
        return_type: return_ty,
        span: dummy_span(),
    };
    checker
        .type_ctx
        .add_type(func_sym, name.to_string(), TypeDef::Function(func_sig));
}

#[test]
fn test_function_call_generic_identity() {
    let mut checker = setup_checker();
    let func_sym = Symbol::new(10);
    let gen_param_sym = Symbol::new(11);
    let gen_param_id = TypeId(0);
    let ty_t = ty_var(0); // Type variable T

    add_generic_function_def(
        &mut checker,
        "identity",
        func_sym,
        vec![GenericParamDef {
            name: "T".to_string(),
            symbol: gen_param_sym,
            id: gen_param_id,
            bounds: vec![],
            span: dummy_span(),
        }],
        vec![ParamType { name: "x".to_string(), ty: ty_t.clone(), span: dummy_span() }],
        ty_t.clone(), // Returns T
    );

    // Call identity(true)
    let args = vec![resolved_arg(None, resolved_lit_bool(true))];
    let call_expr = resolved_fn_call(func_sym, args);

    let result = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result.is_ok(), "Expected Ok for identity(true), got {:?}", result.err());
    let typed_expr = result.unwrap();

    // Expect result type to be bool (T instantiated to bool)
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::Bool));
    assert!(checker.errors.is_empty());

    // Call identity(123)
    let args_int = vec![resolved_arg(None, resolved_lit_int(123))];
    let call_expr_int = resolved_fn_call(func_sym, args_int);

    // Need to reset checker or use a new one if state matters across calls within one test
    // For simplicity, let's assume checker state is ok or create a new one.
    let mut checker2 = setup_checker();
    add_generic_function_def(
        &mut checker2, "identity", func_sym, vec![GenericParamDef { name: "T".to_string(), symbol: gen_param_sym, id: gen_param_id, bounds: vec![], span: dummy_span() }],
        vec![ParamType { name: "x".to_string(), ty: ty_t.clone(), span: dummy_span() }], ty_t.clone());

    let result_int = checker::expr::type_check_expression(&mut checker2, &call_expr_int, None);
    assert!(result_int.is_ok(), "Expected Ok for identity(123), got {:?}", result_int.err());
    let typed_expr_int = result_int.unwrap();

    // Expect result type to be IntegerLiteral (T instantiated to IntegerLiteral)
    assert_eq!(typed_expr_int.ty, ty_prim(PrimitiveType::IntegerLiteral));
    assert!(checker2.errors.is_empty());
}

#[test]
fn test_function_call_generic_explicit_turbofish() {
    let mut checker = setup_checker();
    let func_sym = Symbol::new(11);
    let gen_param_sym = Symbol::new(12);
    let gen_param_id = TypeId(0);
    let ty_t = ty_var(0); // Type variable T

    add_generic_function_def(
        &mut checker,
        "make_default",
        func_sym,
        vec![GenericParamDef {
            name: "T".to_string(),
            symbol: gen_param_sym,
            id: gen_param_id,
            bounds: vec![], // Add Default bound later if needed
            span: dummy_span(),
        }],
        vec![], // No params
        ty_t.clone(), // Returns T
    );

    // Call make_default::<String>()
    let call_expr = resolved_fn_call(func_sym, vec![]);

    // We need a `Default` trait and impl for String for this to *fully* work,
    // but the type checker should at least resolve the return type based on turbofish.
    // Let's assume `make_default` can magically create a String for now.
    let result = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result.is_ok(), "Expected Ok for make_default::<String>(), got {:?}", result.err());
    let typed_expr = result.unwrap();

    // Expected result type is String due to turbofish
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::String));
    // We might get an error later if Default trait bound check is added and String doesn't impl it.
    assert!(checker.errors.is_empty()); // For now, expect no errors from type resolution itself.
}

#[test]
fn test_function_call_generic_with_bound() {
    let mut checker = setup_checker();
    let func_sym = Symbol::new(20);
    let gen_param_sym_t = Symbol::new(21);
    let gen_param_id_t = TypeId(1);
    let ty_t = ty_var(1);

    // Define a dummy trait "Clonable"
    let clonable_trait_sym = Symbol::new(1000);
    // Get the ID first, then use it
    let clonable_trait_id = checker.trait_repo.next_trait_id();
    checker.trait_repo.add_trait(TraitDef {
        id: clonable_trait_id,
        trait_symbol: clonable_trait_sym,
        name: "Clonable".to_string(),
        generic_params: vec![],
        methods: HashMap::new(), // No methods needed for this test
        associated_types: HashMap::new(),
        span: dummy_span(),
    });
    checker.type_ctx.add_trait_symbol(clonable_trait_sym, "Clonable".to_string(), clonable_trait_id);

    // Define impl Clonable for i32
    let i32_ty = ty_prim(PrimitiveType::I32);
    let impl_sym = Symbol::new(1001);
    // Get impl ID before adding
    let impl_id = checker.trait_repo.next_impl_id();
    checker.trait_repo.add_impl(ImplDef {
        id: impl_id, // Use the obtained ID
        impl_symbol: impl_sym,
        trait_ref: Some(TraitRef { trait_id: clonable_trait_id, type_arguments: vec![], span: dummy_span() }),
        implementing_type: i32_ty.clone(),
        generic_params: vec![],
        methods: HashMap::new(),
        associated_type_bindings: HashMap::new(),
        span: dummy_span(),
    });

    // Add function `clone_val<T: Clonable>(val: T) -> T`
    add_generic_function_def(
        &mut checker,
        "clone_val",
        func_sym,
        vec![GenericParamDef {
            name: "T".to_string(),
            symbol: gen_param_sym_t,
            id: gen_param_id_t,
            bounds: vec![TraitRef { trait_id: clonable_trait_id, type_arguments: vec![], span: dummy_span() }],
            span: dummy_span(),
        }],
        vec![ParamType { name: "val".to_string(), ty: ty_t.clone(), span: dummy_span() }],
        ty_t.clone(),
    );

    // Call clone_val(10)
    let args_ok = vec![resolved_arg(None, resolved_lit_int(10))];
    let call_ok = resolved_fn_call(func_sym, args_ok);
    let result_ok = checker::expr::type_check_expression(&mut checker, &call_ok, None);
    assert!(result_ok.is_ok(), "Expected Ok for clone_val(10), got {:?}", result_ok.err());
    let typed_ok = result_ok.unwrap();
    assert_eq!(typed_ok.ty, ty_prim(PrimitiveType::I32)); // T instantiated to i32
    assert!(checker.errors.is_empty());

    // Call clone_val(true)
    let args_err = vec![resolved_arg(None, resolved_lit_bool(true))];
    let call_err = resolved_fn_call(func_sym, args_err);
    let result_err = checker::expr::type_check_expression(&mut checker, &call_err, None);
    assert!(result_err.is_err());
    assert!(matches!(result_err.err().unwrap(), TypeError::TypeDoesNotImplementTrait { .. } | TypeError::RequiredTraitNotImplemented { .. }));
}

// --- Higher Order Function Test --- (Requires Lambda support)
#[test]
fn test_function_call_higher_order() {
    let mut checker = setup_checker();

    // Define `apply<T, U>(f: fn(T) -> U, val: T) -> U`
    let apply_func_sym = Symbol::new(30);
    let gen_t_sym = Symbol::new(31);
    let gen_u_sym = Symbol::new(32);
    let gen_t_id = TypeId(0);
    let gen_u_id = TypeId(1);
    let ty_t = ty_var(0);
    let ty_u = ty_var(1);
    let fn_t_u = ty_func(vec![ty_t.clone()], ty_u.clone());

    add_generic_function_def(
        &mut checker,
        "apply",
        apply_func_sym,
        vec![
            GenericParamDef { name: "T".to_string(), symbol: gen_t_sym, id: gen_t_id, bounds: vec![], span: dummy_span() },
            GenericParamDef { name: "U".to_string(), symbol: gen_u_sym, id: gen_u_id, bounds: vec![], span: dummy_span() },
        ],
        vec![
            ParamType { name: "f".to_string(), ty: fn_t_u.clone(), span: dummy_span() },
            ParamType { name: "val".to_string(), ty: ty_t.clone(), span: dummy_span() },
        ],
        ty_u.clone(), // Returns U
    );

    // Define a simple function `inc(x: i32) -> i32`
    let inc_func_sym = Symbol::new(40);
    add_function_def(
        &mut checker,
        "inc",
        inc_func_sym,
        vec![ParamType { name: "x".to_string(), ty: ty_prim(PrimitiveType::I32), span: dummy_span() }],
        ty_prim(PrimitiveType::I32),
    );

    // Call apply(inc, 5)
    let args = vec![
        resolved_arg(None, resolved_path(inc_func_sym, ResolvedType::Unknown)), // Pass inc func
        resolved_arg(None, resolved_lit_int(5)),
    ];
    let call_expr = resolved_fn_call(apply_func_sym, args);

    let result = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result.is_ok(), "Expected Ok for apply(inc, 5), got {:?}", result.err());
    let typed_expr = result.unwrap();

    // Expect result type to be i32 (U instantiated to i32)
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::I32));
    assert!(checker.errors.is_empty());

    // TODO: Add test calling `apply` with a lambda once lambda exprs are checkable
}

// TODO: Add tests for generic function argument count mismatch
// TODO: Add tests for recursive function calls
// TODO: Add tests for named arguments if supported
// TODO: Add tests for variadic arguments if supported
// TODO: Test calling a symbol that is not a function (e.g., a variable or type) 