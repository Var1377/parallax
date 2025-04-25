// tests/invocation/method_calls.rs
use crate::*; // Import helpers from tests/mod.rs
use parallax_resolve::types::{ResolvedArgument, ResolvedExpr, ResolvedExprKind, ResolvedType, Symbol};
use parallax_resolve::PrimitiveType as ResolvePrimitiveType; // Corrected import again
use parallax_types::{
    context::trait_repo::TraitId,
    error::TypeError,
    types::*,
};
use parallax_types::types::{TraitDef, TraitMethod, ImplDef, TraitRef};
use std::{collections::HashMap, sync::Arc};

// --- Helper to create ResolvedExpr for MethodCall ---
fn resolved_method_call(
    object: ResolvedExpr,
    method_name: &str,
    args: Vec<ResolvedArgument>,
) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::MethodCall {
            object: Box::new(object),
            method_name: method_name.to_string(),
            resolved_method_symbol: None,
            args,
        },
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown, // Type checker determines this
    }
}

// --- Helper to create ResolvedArgument (reuse from function_calls) ---
fn resolved_arg(name: Option<&str>, value: ResolvedExpr) -> ResolvedArgument {
    ResolvedArgument {
        name: name.map(String::from),
        value,
        span: dummy_span(),
    }
}

// --- Helper Struct Def --- 
fn add_counter_struct(checker: &mut TypeChecker) -> Symbol {
    let struct_sym = Symbol::new(1000);
    let field_sym = Symbol::new(1001);
    let struct_def = StructDef {
        name: "Counter".to_string(),
        symbol: struct_sym,
        generic_params: vec![],
        fields: vec![Field {
            name: "count".to_string(),
            symbol: field_sym,
            ty: ty_prim(PrimitiveType::I32),
            span: dummy_span(),
        }],
        span: dummy_span(),
    };
    checker
        .type_ctx
        .add_type(struct_sym, "Counter".to_string(), TypeDef::Struct(struct_def));
    struct_sym
}

// --- Helper Point Struct Def ---
fn add_point_struct(checker: &mut TypeChecker) -> Symbol {
    let struct_sym = Symbol::new(3000);
    let x_sym = Symbol::new(3001);
    let y_sym = Symbol::new(3002);
    let struct_def = StructDef {
        name: "Point".to_string(),
        symbol: struct_sym,
        generic_params: vec![],
        fields: vec![
            Field { name: "x".to_string(), symbol: x_sym, ty: ty_prim(PrimitiveType::F64), span: dummy_span() },
            Field { name: "y".to_string(), symbol: y_sym, ty: ty_prim(PrimitiveType::F64), span: dummy_span() },
        ],
        span: dummy_span(),
    };
    checker.type_ctx.add_type(struct_sym, "Point".to_string(), TypeDef::Struct(struct_def));
    struct_sym
}

// --- Helper Generic Wrapper Struct Def ---
fn add_generic_wrapper_struct(checker: &mut TypeChecker) -> (Symbol, GenericParamDef) {
    let struct_sym = Symbol::new(4000);
    let value_sym = Symbol::new(4001);
    let gen_param_sym = Symbol::new(4002);
    let gen_param_id = TypeId(4003);

    let gen_param_def = GenericParamDef {
        name: "T".to_string(),
        symbol: gen_param_sym,
        id: gen_param_id,
        bounds: vec![],
        span: dummy_span(),
    };

    let struct_def = StructDef {
        name: "Wrapper".to_string(),
        symbol: struct_sym,
        generic_params: vec![gen_param_def.clone()],
        fields: vec![Field {
            name: "value".to_string(),
            symbol: value_sym,
            ty: ty_var(gen_param_id.0), // Use TyKind::Var for the generic type
            span: dummy_span(),
        }],
        span: dummy_span(),
    };
    checker.type_ctx.add_type(struct_sym, "Wrapper".to_string(), TypeDef::Struct(struct_def));
    (struct_sym, gen_param_def)
}

// --- Helper TyKind::SelfType ---
fn ty_self() -> Ty {
    Ty::with_span(TyKind::SelfType, dummy_span())
}

// --- Helper to add Method Impl ---
fn add_counter_methods(
    checker: &mut TypeChecker,
    counter_sym: Symbol,
    inc_sym: Symbol,
    get_sym: Symbol,
) {
    // --- Impl block for Counter --- 
    let impl_symbol = Symbol::new(1010);
    let impl_id = checker.trait_repo.next_impl_id();
    let impl_def = ImplDef {
        id: impl_id,
        impl_symbol,
        trait_ref: None, // Inherent impl
        implementing_type: ty_named("Counter", Some(counter_sym), vec![]),
        generic_params: vec![],
        methods: HashMap::from([
            (inc_sym, inc_sym),
            (get_sym, get_sym),
        ]),
        associated_type_bindings: HashMap::new(),
        span: dummy_span(),
    };
    checker.trait_repo.add_impl(impl_def);

    // --- inc method signature --- 
    let inc_sig = FunctionSignature {
        name: "inc".to_string(),
        self_param: Some(SelfParamKind::Value),
        generic_params: vec![],
        params: vec![ParamType {
            name: "amount".to_string(),
            ty: ty_prim(PrimitiveType::I32),
            span: dummy_span(),
        }],
        return_type: ty_prim(PrimitiveType::Unit),
        span: dummy_span(),
    };
    checker
        .type_ctx
        .add_type(inc_sym, "inc".to_string(), TypeDef::Function(inc_sig));

    // --- get method signature --- 
    let get_sig = FunctionSignature {
        name: "get".to_string(),
        self_param: Some(SelfParamKind::Value),
        generic_params: vec![],
        params: vec![],
        return_type: ty_prim(PrimitiveType::I32),
        span: dummy_span(),
    };
    checker
        .type_ctx
        .add_type(get_sym, "get".to_string(), TypeDef::Function(get_sig));
}

// --- Helper to add Static Method Impl ---
fn add_point_static_method(checker: &mut TypeChecker, point_sym: Symbol, origin_sym: Symbol) {
    // --- Impl block for Point ---
    let impl_symbol = Symbol::new(3010);
    let impl_id = checker.trait_repo.next_impl_id();
    let impl_def = ImplDef {
        id: impl_id,
        impl_symbol,
        trait_ref: None, // Inherent impl
        implementing_type: ty_named("Point", Some(point_sym), vec![]),
        generic_params: vec![],
        methods: HashMap::from([(origin_sym, origin_sym)]), // Map method symbol to impl method symbol
        associated_type_bindings: HashMap::new(),
        span: dummy_span(),
    };
    checker.trait_repo.add_impl(impl_def);

    // --- origin static method signature ---
    let origin_sig = FunctionSignature {
        name: "origin".to_string(),
        self_param: None, // Static method, no self
        generic_params: vec![],
        params: vec![], // No parameters
        return_type: ty_named("Point", Some(point_sym), vec![]), // Returns Point
        span: dummy_span(),
    };
    checker
        .type_ctx
        .add_type(origin_sym, "origin".to_string(), TypeDef::Function(origin_sig));
}

// --- Helper to add Generic Method Impl ---
fn add_generic_wrapper_method(
    checker: &mut TypeChecker,
    wrapper_sym: Symbol,
    unwrap_sym: Symbol,
    gen_param_def: GenericParamDef, // Receive the generic param def
) {
    // --- Impl block for Wrapper<T> ---
    let impl_symbol = Symbol::new(4010);
    let impl_id = checker.trait_repo.next_impl_id();
    let impl_def = ImplDef {
        id: impl_id,
        impl_symbol,
        trait_ref: None, // Inherent impl
        implementing_type: ty_named("Wrapper", Some(wrapper_sym), vec![ty_var(gen_param_def.id.0)]),
        generic_params: vec![gen_param_def.clone()], // Generic param defined on the impl
        methods: HashMap::from([(unwrap_sym, unwrap_sym)]),
        associated_type_bindings: HashMap::new(),
        span: dummy_span(),
    };
    checker.trait_repo.add_impl(impl_def);

    // --- unwrap method signature ---
    // Note: The signature itself is generic, using the generic param `T`
    let unwrap_sig = FunctionSignature {
        name: "unwrap".to_string(),
        self_param: Some(SelfParamKind::Value), // Takes self by value
        generic_params: vec![], // No *additional* generics on the method itself
        params: vec![],
        return_type: ty_var(gen_param_def.id.0), // Returns T
        span: dummy_span(),
    };
    checker
        .type_ctx
        .add_type(unwrap_sym, "unwrap".to_string(), TypeDef::Function(unwrap_sig));
}

// --- Tests ---

#[test]
fn test_method_call_simple() {
    let mut checker = setup_checker();
    let counter_sym = add_counter_struct(&mut checker);
    let inc_sym = Symbol::new(1002);
    let get_sym = Symbol::new(1003);
    add_counter_methods(&mut checker, counter_sym, inc_sym, get_sym);

    // Assume `c` is a variable of type `Counter`
    let var_sym = Symbol::new(1004);
    let receiver_ty = ty_named("Counter", Some(counter_sym), vec![]);
    checker._type_env = Arc::new({
        let mut env = TypeEnvironment::new();
        env.add("c".to_string(), receiver_ty.clone());
        env
    });
    let receiver_expr = ResolvedExpr {
        kind: ResolvedExprKind::Variable { binding_symbol: var_sym, name: "c".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::UserDefined { symbol: counter_sym, type_args: None },
    };

    // Call c.get()
    let call_expr = resolved_method_call(receiver_expr, "get", vec![]);

    let result = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result.is_ok(), "Expected Ok for c.get(), got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::I32)); // get returns i32
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_method_call_with_args() {
    let mut checker = setup_checker();
    let counter_sym = add_counter_struct(&mut checker);
    let inc_sym = Symbol::new(1002);
    let get_sym = Symbol::new(1003);
    add_counter_methods(&mut checker, counter_sym, inc_sym, get_sym);

    let var_sym = Symbol::new(1004);
    let receiver_ty = ty_named("Counter", Some(counter_sym), vec![]);
     checker._type_env = Arc::new({
        let mut env = TypeEnvironment::new();
        env.add("c".to_string(), receiver_ty.clone());
        env
    });
    let receiver_expr = ResolvedExpr {
        kind: ResolvedExprKind::Variable { binding_symbol: var_sym, name: "c".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::UserDefined { symbol: counter_sym, type_args: None },
    };

    // Call c.inc(5)
    let args = vec![resolved_arg(None, resolved_lit_int(5))];
    let call_expr = resolved_method_call(receiver_expr, "inc", args);

    let result = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result.is_ok(), "Expected Ok for c.inc(5), got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::Unit)); // inc returns unit
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_method_call_arg_type_mismatch() {
    let mut checker = setup_checker();
    let counter_sym = add_counter_struct(&mut checker);
    let inc_sym = Symbol::new(1002);
    let get_sym = Symbol::new(1003);
    add_counter_methods(&mut checker, counter_sym, inc_sym, get_sym);

    let var_sym = Symbol::new(1004);
    let receiver_ty = ty_named("Counter", Some(counter_sym), vec![]);
     checker._type_env = Arc::new({
        let mut env = TypeEnvironment::new();
        env.add("c".to_string(), receiver_ty.clone());
        env
    });
    let receiver_expr = ResolvedExpr {
        kind: ResolvedExprKind::Variable { binding_symbol: var_sym, name: "c".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::UserDefined { symbol: counter_sym, type_args: None },
    };

    // Call c.inc("wrong") - expects i32
    let args = vec![resolved_arg(None, resolved_lit_string("wrong"))];
    let call_expr = resolved_method_call(receiver_expr, "inc", args);

    let result = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result.is_err());
    assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. } | TypeError::NoMatchingMethod { .. } | TypeError::AmbiguousMethodCall { .. }));
}

#[test]
fn test_method_call_wrong_arg_count() {
    let mut checker = setup_checker();
    let counter_sym = add_counter_struct(&mut checker);
    let inc_sym = Symbol::new(1002);
    let get_sym = Symbol::new(1003);
    add_counter_methods(&mut checker, counter_sym, inc_sym, get_sym);

    let var_sym = Symbol::new(1004);
    let receiver_ty = ty_named("Counter", Some(counter_sym), vec![]);
     checker._type_env = Arc::new({
        let mut env = TypeEnvironment::new();
        env.add("c".to_string(), receiver_ty.clone());
        env
    });
    let receiver_expr = ResolvedExpr {
        kind: ResolvedExprKind::Variable { binding_symbol: var_sym, name: "c".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::UserDefined { symbol: counter_sym, type_args: None },
    };

    // Call c.inc() - expects one arg
    let call_expr = resolved_method_call(receiver_expr.clone(), "inc", vec![]);
    let result1 = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result1.is_err());
    assert!(matches!(result1.err().unwrap(), TypeError::WrongNumberOfArguments { expected: 1, found: 0, .. } | TypeError::NoMatchingMethod { .. } | TypeError::AmbiguousMethodCall { .. }));

    // Call c.get(123) - expects zero args
    let args_extra = vec![resolved_arg(None, resolved_lit_int(123))];
    let call_expr_extra = resolved_method_call(receiver_expr, "get", args_extra);
    let result2 = checker::expr::type_check_expression(&mut checker, &call_expr_extra, None);
    assert!(result2.is_err());
     assert!(matches!(result2.err().unwrap(), TypeError::WrongNumberOfArguments { expected: 0, found: 1, .. } | TypeError::NoMatchingMethod { .. } | TypeError::AmbiguousMethodCall { .. }));
}

#[test]
fn test_method_call_unknown_method() {
    let mut checker = setup_checker();
    let counter_sym = add_counter_struct(&mut checker);
    let inc_sym = Symbol::new(1002);
    let get_sym = Symbol::new(1003);
    add_counter_methods(&mut checker, counter_sym, inc_sym, get_sym);

    let var_sym = Symbol::new(1004);
    let receiver_ty = ty_named("Counter", Some(counter_sym), vec![]);
     checker._type_env = Arc::new({
        let mut env = TypeEnvironment::new();
        env.add("c".to_string(), receiver_ty.clone());
        env
    });
    let receiver_expr = ResolvedExpr {
        kind: ResolvedExprKind::Variable { binding_symbol: var_sym, name: "c".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::UserDefined { symbol: counter_sym, type_args: None },
    };

    // Call c.decrement() - method doesn't exist
    let call_expr = resolved_method_call(receiver_expr, "decrement", vec![]);

    let result = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result.is_err());
    assert!(matches!(result.err().unwrap(), TypeError::NoMatchingMethod { .. } | TypeError::AmbiguousMethodCall { .. }));
}

#[test]
fn test_method_call_on_primitive() {
    let mut checker = setup_checker();
    // Add a trait impl for i32, e.g., a hypothetical "abs" method
    let trait_sym = Symbol::new(2000);
    let method_sym = Symbol::new(2001);
    let impl_method_sym = Symbol::new(2002);
    add_simple_unary_trait_impl(
        &mut checker, "Abs", "abs",
        ty_prim(PrimitiveType::I32), // Returns i32
        ty_prim(PrimitiveType::I32), // Impl for i32
        trait_sym, method_sym, impl_method_sym
    );

    let receiver_expr = resolved_lit_int(-5);
    let call_expr = resolved_method_call(receiver_expr, "abs", vec![]);

    let result = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result.is_ok(), "Expected Ok for (-5).abs(), got {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::I32));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_static_method_call() {
    let mut checker = setup_checker();
    let point_sym = add_point_struct(&mut checker);
    let origin_sym = Symbol::new(3003);
    add_point_static_method(&mut checker, point_sym, origin_sym);

    // Represent Point::origin() call
    // The resolver would typically give a Path expression resolving to the static method's symbol.
    let static_call_expr = ResolvedExpr {
        kind: ResolvedExprKind::Call {
            func_symbol: Some(origin_sym), // Resolver identifies the specific function symbol
            args: vec![],
        },
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown, // Type checker resolves the call result type
    };

    let result = checker::expr::type_check_expression(&mut checker, &static_call_expr, None);
    assert!(result.is_ok(), "Expected Ok for Point::origin(), got {:?}", result.err());
    let typed_expr = result.unwrap();

    let expected_type = ty_named("Point", Some(point_sym), vec![]);
    assert_eq!(typed_expr.ty, expected_type); // Call returns Point
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_method_call_generic_receiver() {
    let mut checker = setup_checker();
    let (wrapper_sym, gen_param_def) = add_generic_wrapper_struct(&mut checker);
    let unwrap_sym = Symbol::new(4004);
    add_generic_wrapper_method(&mut checker, wrapper_sym, unwrap_sym, gen_param_def.clone());

    // Assume `w` is a variable of type `Wrapper<i32>`
    let var_sym = Symbol::new(4005);
    // Manually construct the resolved type for Wrapper<i32>
    let receiver_ty = ty_named("Wrapper", Some(wrapper_sym), vec![ty_prim(PrimitiveType::I32)]); 
    checker._type_env = Arc::new({
        let mut env = TypeEnvironment::new();
        env.add("w".to_string(), receiver_ty.clone());
        env
    });

    let receiver_expr = ResolvedExpr {
        kind: ResolvedExprKind::Variable {
            name: "w".to_string(),
            binding_symbol: var_sym,
        },
        span: dummy_span(),
        // Construct the ResolvedType for the variable `w`, which is Wrapper<i32>
        resolved_type: ResolvedType::UserDefined { // Use the enum variant directly
            symbol: wrapper_sym, 
            type_args: Some(vec![ResolvedType::Primitive(ResolvePrimitiveType::I32)]) // Use ResolvePrimitiveType
        },
    };

    // Expression: w.unwrap()
    let expr = resolved_method_call(receiver_expr, "unwrap", vec![]);

    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    println!("{:#?}", result); // Debug print

    assert!(result.is_ok(), "Expected successful type check");
    let checked_expr = result.unwrap();

    // Check the type of the overall expression (should be i32)
    assert_eq!(
        checked_expr.ty,
        ty_prim(PrimitiveType::I32),
        "Expected return type to be i32"
    );

    // Verify it's a call expression (specific check for method symbol might need adjustment)
    assert!(matches!(checked_expr.kind, TypedExprKind::Call { .. }));
}

// --- Trait Method Tests ---

// Helper trait definition
fn add_display_trait(checker: &mut TypeChecker) -> (Symbol, Symbol, TraitId) {
    let trait_sym = Symbol::new(2000);
    let method_sym = Symbol::new(2001);
    let trait_id = checker.trait_repo.next_trait_id();
    let trait_def = TraitDef {
        id: trait_id,
        trait_symbol: trait_sym,
        name: "Display".to_string(),
        generic_params: vec![],
        methods: HashMap::from([(method_sym, TraitMethod {
            name: "display".to_string(),
            method_symbol: method_sym,
            signature: FunctionSignature {
                name: "display".to_string(),
                self_param: Some(SelfParamKind::Value),
                generic_params: vec![],
                params: vec![],
                return_type: ty_prim(PrimitiveType::String),
                span: dummy_span(),
            }
        })]),
        associated_types: HashMap::new(),
        span: dummy_span(),
    };
    checker.trait_repo.add_trait(trait_def);
    checker.type_ctx.add_trait_symbol(trait_sym, "Display".to_string(), trait_id);
    (trait_sym, method_sym, trait_id)
}

// Helper impl for Counter
fn add_display_impl_for_counter(
    checker: &mut TypeChecker,
    trait_id: TraitId,
    trait_method_sym: Symbol,
    counter_sym: Symbol,
    impl_method_sym: Symbol,
) {
    let impl_symbol = Symbol::new(2010);
    let impl_id = checker.trait_repo.next_impl_id();
    let impl_def = ImplDef {
        id: impl_id,
        impl_symbol,
        trait_ref: Some(TraitRef { trait_id, type_arguments: vec![], span: dummy_span() }),
        implementing_type: ty_named("Counter", Some(counter_sym), vec![]),
        generic_params: vec![],
        methods: HashMap::from([(trait_method_sym, impl_method_sym)]),
        associated_type_bindings: HashMap::new(),
        span: dummy_span(),
    };
    checker.trait_repo.add_impl(impl_def);

    // Define the signature for the impl method
    let impl_method_sig = FunctionSignature {
        name: "display".to_string(),
        self_param: Some(SelfParamKind::Value),
        generic_params: vec![],
        params: vec![],
        return_type: ty_prim(PrimitiveType::String),
        span: dummy_span(),
    };
    checker.type_ctx.add_type(impl_method_sym, "display".to_string(), TypeDef::Function(impl_method_sig));
}

#[test]
fn test_method_call_trait_method() {
    let mut checker = setup_checker();
    let counter_sym = add_counter_struct(&mut checker);
    let (_trait_sym, trait_method_sym, trait_id) = add_display_trait(&mut checker);
    let impl_method_sym = Symbol::new(2011);
    add_display_impl_for_counter(&mut checker, trait_id, trait_method_sym, counter_sym, impl_method_sym);

    let var_sym = Symbol::new(1004);
    let receiver_ty = ty_named("Counter", Some(counter_sym), vec![]);
    checker._type_env = Arc::new({
        let mut env = TypeEnvironment::new();
        env.add("c".to_string(), receiver_ty.clone());
        env
    });
    let receiver_expr = ResolvedExpr {
        kind: ResolvedExprKind::Variable { binding_symbol: var_sym, name: "c".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::UserDefined { symbol: counter_sym, type_args: None },
    };

    // Call c.display()
    let call_expr = resolved_method_call(receiver_expr, "display", vec![]);

    let result = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result.is_ok(), "Expected Ok for c.display(), got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::String)); // display returns String
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_method_call_trait_method_not_implemented() {
    let mut checker = setup_checker();
    let counter_sym = add_counter_struct(&mut checker);
    let (_trait_sym, _trait_method_sym, _trait_id) = add_display_trait(&mut checker);
    // DO NOT add the impl

    let var_sym = Symbol::new(1004);
    let receiver_ty = ty_named("Counter", Some(counter_sym), vec![]);
    checker._type_env = Arc::new({
        let mut env = TypeEnvironment::new();
        env.add("c".to_string(), receiver_ty.clone());
        env
    });
    let receiver_expr = ResolvedExpr {
        kind: ResolvedExprKind::Variable { binding_symbol: var_sym, name: "c".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::UserDefined { symbol: counter_sym, type_args: None },
    };

    // Call c.display()
    let call_expr = resolved_method_call(receiver_expr, "display", vec![]);

    let result = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result.is_err());
    // Expect NoMatchingMethod because the trait is defined but not implemented for Counter
    assert!(matches!(result.err().unwrap(), TypeError::NoMatchingMethod { .. }));
}

// --- Helper Trait Def (for bounds test) ---
fn add_clonable_trait(checker: &mut TypeChecker) -> (Symbol, Symbol, TraitId) {
    let trait_sym = Symbol::new(5000);
    let method_sym = Symbol::new(5001);
    let trait_id = checker.trait_repo.next_trait_id();
    let trait_def = TraitDef {
        id: trait_id,
        trait_symbol: trait_sym,
        name: "Clonable".to_string(),
        generic_params: vec![], // Trait Clonable<T> is not generic itself
        methods: HashMap::from([(method_sym, TraitMethod {
            name: "clone".to_string(),
            method_symbol: method_sym,
            signature: FunctionSignature {
                name: "clone".to_string(),
                self_param: Some(SelfParamKind::Value), // Takes &self conceptually, but signature uses SelfType
                generic_params: vec![],
                params: vec![],
                return_type: ty_self(), // Returns Self
                span: dummy_span(),
            }
        })]),
        associated_types: HashMap::new(),
        span: dummy_span(),
    };
    checker.trait_repo.add_trait(trait_def);
    checker.type_ctx.add_trait_symbol(trait_sym, "Clonable".to_string(), trait_id);
    (trait_sym, method_sym, trait_id)
}

// --- Helper Impl for Clonable<i32> ---
fn add_clonable_impl_for_i32(
    checker: &mut TypeChecker,
    trait_id: TraitId,
    trait_method_sym: Symbol,
    impl_method_sym: Symbol,
) {
    let impl_symbol = Symbol::new(5010);
    let impl_id = checker.trait_repo.next_impl_id();
    let impl_def = ImplDef {
        id: impl_id,
        impl_symbol,
        trait_ref: Some(TraitRef { trait_id, type_arguments: vec![], span: dummy_span() }),
        implementing_type: ty_prim(PrimitiveType::I32),
        generic_params: vec![],
        methods: HashMap::from([(trait_method_sym, impl_method_sym)]),
        associated_type_bindings: HashMap::new(),
        span: dummy_span(),
    };
    checker.trait_repo.add_impl(impl_def);

    // Define the signature for the impl method `i32::clone`
    let impl_method_sig = FunctionSignature {
        name: "clone".to_string(),
        self_param: Some(SelfParamKind::Value),
        generic_params: vec![],
        params: vec![],
        return_type: ty_prim(PrimitiveType::I32), // Explicitly returns i32
        span: dummy_span(),
    };
    checker.type_ctx.add_type(impl_method_sym, "clone".to_string(), TypeDef::Function(impl_method_sig));
}

// --- Helper Generic Struct with Bounded Method ---
fn add_maybe_cloned_struct_and_method(
    checker: &mut TypeChecker,
    clonable_trait_id: TraitId,
) -> (Symbol, Symbol, GenericParamDef) { // Returns (StructSym, MethodSym, GenericParamDef)
    let struct_sym = Symbol::new(6000);
    let value_sym = Symbol::new(6001);
    let clone_value_sym = Symbol::new(6002);
    let gen_param_sym = Symbol::new(6003);
    let gen_param_id = TypeId(6004);

    let gen_param_def = GenericParamDef {
        name: "T".to_string(),
        symbol: gen_param_sym,
        id: gen_param_id,
        bounds: vec![TraitRef { // Bound: T must implement Clonable
            trait_id: clonable_trait_id,
            type_arguments: vec![], // Clonable trait has no generics
            span: dummy_span(),
        }],
        span: dummy_span(),
    };

    // Define the struct MaybeCloned<T: Clonable>
    let struct_def = StructDef {
        name: "MaybeCloned".to_string(),
        symbol: struct_sym,
        generic_params: vec![gen_param_def.clone()],
        fields: vec![Field {
            name: "value".to_string(),
            symbol: value_sym,
            ty: ty_var(gen_param_id.0),
            span: dummy_span(),
        }],
        span: dummy_span(),
    };
    checker.type_ctx.add_type(struct_sym, "MaybeCloned".to_string(), TypeDef::Struct(struct_def));

    // Define the impl block for MaybeCloned<T>
    let impl_symbol = Symbol::new(6010);
    let impl_id = checker.trait_repo.next_impl_id();
    let impl_def = ImplDef {
        id: impl_id,
        impl_symbol,
        trait_ref: None, // Inherent impl
        implementing_type: ty_named("MaybeCloned", Some(struct_sym), vec![ty_var(gen_param_def.id.0)]),
        generic_params: vec![gen_param_def.clone()], // Impl is generic over T: Clonable
        methods: HashMap::from([(clone_value_sym, clone_value_sym)]),
        associated_type_bindings: HashMap::new(),
        span: dummy_span(),
    };
    checker.trait_repo.add_impl(impl_def);

    // Define the signature for the `clone_value` method
    let clone_value_sig = FunctionSignature {
        name: "clone_value".to_string(),
        self_param: Some(SelfParamKind::Value),
        generic_params: vec![], // Method itself isn't generic
        params: vec![],
        return_type: ty_var(gen_param_id.0), // Returns T (which must be Clonable)
        span: dummy_span(),
    };
    checker.type_ctx.add_type(clone_value_sym, "clone_value".to_string(), TypeDef::Function(clone_value_sig));

    (struct_sym, clone_value_sym, gen_param_def)
}

#[test]
fn test_method_call_with_trait_bound_success() {
    let mut checker = setup_checker();
    let (_clonable_trait_sym, clonable_method_sym, clonable_trait_id) = add_clonable_trait(&mut checker);
    let clonable_impl_method_sym = Symbol::new(5011);
    add_clonable_impl_for_i32(&mut checker, clonable_trait_id, clonable_method_sym, clonable_impl_method_sym);
    let (maybe_cloned_sym, clone_value_sym, _gen_param_def) = add_maybe_cloned_struct_and_method(&mut checker, clonable_trait_id);

    // Assume `mc` is MaybeCloned<i32>
    let var_sym = Symbol::new(7000);
    let receiver_ty = ty_named("MaybeCloned", Some(maybe_cloned_sym), vec![ty_prim(PrimitiveType::I32)]);
    checker._type_env = Arc::new({
        let mut env = TypeEnvironment::new();
        env.add("mc".to_string(), receiver_ty.clone());
        env
    });
    let receiver_expr = ResolvedExpr {
        kind: ResolvedExprKind::Variable { binding_symbol: var_sym, name: "mc".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::UserDefined {
            symbol: maybe_cloned_sym,
            type_args: Some(vec![ResolvedType::Primitive(ResolvePrimitiveType::I32)])
        },
    };

    // Call mc.clone_value()
    let call_expr = resolved_method_call(receiver_expr, "clone_value", vec![]);

    let result = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result.is_ok(), "Expected Ok for mc.clone_value() where T=i32 (Clonable), got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::I32)); // Returns T, which is i32
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty(), "Checker errors: {:?}", checker.errors);
}

#[test]
fn test_method_call_with_trait_bound_failure() {
    let mut checker = setup_checker();
    let (_clonable_trait_sym, _clonable_method_sym, clonable_trait_id) = add_clonable_trait(&mut checker);
    // Don't add the impl for i32 this time
    let (maybe_cloned_sym, _clone_value_sym, _gen_param_def) = add_maybe_cloned_struct_and_method(&mut checker, clonable_trait_id);

    // Define a new struct that does NOT implement Clonable
    let non_clonable_sym = Symbol::new(7010);
    let non_clonable_def = StructDef {
        name: "NonClonableData".to_string(), symbol: non_clonable_sym, generic_params: vec![], fields: vec![], span: dummy_span()
    };
    checker.type_ctx.add_type(non_clonable_sym, "NonClonableData".to_string(), TypeDef::Struct(non_clonable_def));


    // Assume `nc` is MaybeCloned<NonClonableData>
    let var_sym = Symbol::new(7011);
    let receiver_ty = ty_named("MaybeCloned", Some(maybe_cloned_sym), vec![ty_named("NonClonableData", Some(non_clonable_sym), vec![])]);
    checker._type_env = Arc::new({
        let mut env = TypeEnvironment::new();
        env.add("nc".to_string(), receiver_ty.clone());
        env
    });
    let receiver_expr = ResolvedExpr {
        kind: ResolvedExprKind::Variable { binding_symbol: var_sym, name: "nc".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::UserDefined {
            symbol: maybe_cloned_sym,
            // Use Unknown here as ResolvePrimitiveType causes issues
            // This requires the type checker to infer the inner type correctly from the variable's type
            type_args: Some(vec![ResolvedType::UserDefined { symbol: non_clonable_sym, type_args: None}])
        },
    };

    // Call nc.clone_value()
    let call_expr = resolved_method_call(receiver_expr, "clone_value", vec![]);

    let result = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result.is_err(), "Expected Err for mc.clone_value() where T=NonClonableData");

    // Simplified assertion: Check if it's one of the expected error kinds
    let err = result.err().unwrap();
    assert!(matches!(err,
        TypeError::TypeDoesNotImplementTrait { .. }
        | TypeError::RequiredTraitNotImplemented { .. }
        | TypeError::NoMatchingMethod { .. }
    ), "Expected Clonable trait error for bool, got {:?}", err);
}

// TODO: Test Method Calls with different SelfParamKinds (&self, &mut self, self)

// TODO: Test Method Resolution Ambiguity

// --- Helper Traits/Impls for Ambiguity Test ---
fn add_ambiguity_traits_and_impl(
    checker: &mut TypeChecker,
    target_sym: Symbol, // The symbol of the struct implementing the traits
) -> (Symbol, Symbol) { // Returns (Trait1MethodSym, Trait2MethodSym)
    // Trait A
    let trait_a_sym = Symbol::new(8000);
    let method_a_sym = Symbol::new(8001);
    let trait_a_id = checker.trait_repo.next_trait_id();
    checker.trait_repo.add_trait(TraitDef {
        id: trait_a_id, trait_symbol: trait_a_sym, name: "TraitA".to_string(), generic_params: vec![],
        methods: HashMap::from([(method_a_sym, TraitMethod { name: "ambiguous_method".to_string(), method_symbol: method_a_sym,
            signature: FunctionSignature { name: "ambiguous_method".to_string(), self_param: Some(SelfParamKind::Value), generic_params: vec![], params: vec![], return_type: ty_prim(PrimitiveType::I32), span: dummy_span() }
        })]), associated_types: HashMap::new(), span: dummy_span(),
    });
    checker.type_ctx.add_trait_symbol(trait_a_sym, "TraitA".to_string(), trait_a_id);

    // Trait B
    let trait_b_sym = Symbol::new(8010);
    let method_b_sym = Symbol::new(8011);
    let trait_b_id = checker.trait_repo.next_trait_id();
    checker.trait_repo.add_trait(TraitDef {
        id: trait_b_id, trait_symbol: trait_b_sym, name: "TraitB".to_string(), generic_params: vec![],
        methods: HashMap::from([(method_b_sym, TraitMethod { name: "ambiguous_method".to_string(), method_symbol: method_b_sym,
            signature: FunctionSignature { name: "ambiguous_method".to_string(), self_param: Some(SelfParamKind::Value), generic_params: vec![], params: vec![], return_type: ty_prim(PrimitiveType::Bool), span: dummy_span() } // Different return type
        })]), associated_types: HashMap::new(), span: dummy_span(),
    });
     checker.type_ctx.add_trait_symbol(trait_b_sym, "TraitB".to_string(), trait_b_id);

    // Impl TraitA for Target
    let impl_a_sym = Symbol::new(8020);
    let impl_method_a_sym = Symbol::new(8021);
    let impl_a_id = checker.trait_repo.next_impl_id();
    checker.trait_repo.add_impl(ImplDef {
        id: impl_a_id, impl_symbol: impl_a_sym,
        trait_ref: Some(TraitRef { trait_id: trait_a_id, type_arguments: vec![], span: dummy_span() }),
        implementing_type: ty_named("Target", Some(target_sym), vec![]),
        generic_params: vec![], methods: HashMap::from([(method_a_sym, impl_method_a_sym)]), associated_type_bindings: HashMap::new(), span: dummy_span(),
    });
    checker.type_ctx.add_type(impl_method_a_sym, "ambiguous_method_impl_a".to_string(), TypeDef::Function(FunctionSignature {
        name: "ambiguous_method".to_string(), self_param: Some(SelfParamKind::Value), generic_params: vec![], params: vec![], return_type: ty_prim(PrimitiveType::I32), span: dummy_span()
    }));

    // Impl TraitB for Target
    let impl_b_sym = Symbol::new(8030);
    let impl_method_b_sym = Symbol::new(8031);
    let impl_b_id = checker.trait_repo.next_impl_id();
    checker.trait_repo.add_impl(ImplDef {
        id: impl_b_id, impl_symbol: impl_b_sym,
        trait_ref: Some(TraitRef { trait_id: trait_b_id, type_arguments: vec![], span: dummy_span() }),
        implementing_type: ty_named("Target", Some(target_sym), vec![]),
        generic_params: vec![], methods: HashMap::from([(method_b_sym, impl_method_b_sym)]), associated_type_bindings: HashMap::new(), span: dummy_span(),
    });
     checker.type_ctx.add_type(impl_method_b_sym, "ambiguous_method_impl_b".to_string(), TypeDef::Function(FunctionSignature {
        name: "ambiguous_method".to_string(), self_param: Some(SelfParamKind::Value), generic_params: vec![], params: vec![], return_type: ty_prim(PrimitiveType::Bool), span: dummy_span()
    }));

    (impl_method_a_sym, impl_method_b_sym) // Return impl method symbols for potential checks
}

#[test]
fn test_method_call_ambiguous() {
    let mut checker = setup_checker();
    // Define a dummy struct
    let target_sym = Symbol::new(8050);
    checker.type_ctx.add_type(target_sym, "Target".to_string(), TypeDef::Struct(StructDef {
        name: "Target".to_string(), symbol: target_sym, generic_params: vec![], fields: vec![], span: dummy_span()
    }));

    add_ambiguity_traits_and_impl(&mut checker, target_sym);

    // Assume `t` is Target
    let var_sym = Symbol::new(8051);
    let receiver_ty = ty_named("Target", Some(target_sym), vec![]);
     checker._type_env = Arc::new({
        let mut env = TypeEnvironment::new();
        env.add("t".to_string(), receiver_ty.clone());
        env
    });
    let receiver_expr = ResolvedExpr {
        kind: ResolvedExprKind::Variable { binding_symbol: var_sym, name: "t".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::UserDefined { symbol: target_sym, type_args: None },
    };

    // Call t.ambiguous_method()
    let call_expr = resolved_method_call(receiver_expr, "ambiguous_method", vec![]);

    let result = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result.is_err(), "Expected Err for ambiguous method call");
    // Check for the specific AmbiguousMethodCall error
    assert!(matches!(result.err().unwrap(), TypeError::AmbiguousMethodCall { method, ty, .. } if method == "ambiguous_method" && ty == "Target"));
}

// TODO: Test Method Calls with different SelfParamKinds (&self, &mut self, self)

