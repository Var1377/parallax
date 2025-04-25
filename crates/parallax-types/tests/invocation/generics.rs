// tests/invocation/generics.rs
use crate::*; // Import helpers from tests/mod.rs
use parallax_resolve::{
    types::{ResolvedArgument, ResolvedExpr, ResolvedExprKind, ResolvedType, Symbol},
    PrimitiveType as ResolvePrimitiveType,
};
use parallax_types::{
    context::trait_repo::TraitId,
    error::TypeError,
    types::*,
};
use parallax_types::types::{TraitDef, TraitMethod, ImplDef, TraitRef, GenericParamDef}; // Added GenericParamDef
use std::{collections::HashMap, sync::Arc};

// --- Helpers (Copied/adapted from function_calls.rs and method_calls.rs) ---

// Helper to create ResolvedExpr for MethodCall
fn resolved_method_call(
    object: ResolvedExpr,
    method_name: &str,
    args: Vec<ResolvedArgument>,
) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::MethodCall {
            object: Box::new(object),
            method_name: method_name.to_string(),
            resolved_method_symbol: None, // Type checker resolves this
            args,
        },
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    }
}

// TODO: Refactor common helpers into tests/mod.rs or a dedicated helper module

fn ty_var(id: u32) -> Ty {
    Ty::with_span(TyKind::Var(TypeId(id)), dummy_span())
}

fn ty_func(params: Vec<Ty>, ret: Ty) -> Ty {
    Ty::with_span(TyKind::Function(params, Arc::new(ret)), dummy_span())
}

fn resolved_fn_call(
    func_symbol: Symbol,
    args: Vec<ResolvedArgument>,
    // TODO: Add explicit type arguments (turbofish) if needed
) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Call { func_symbol: Some(func_symbol), args },
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    }
}

fn resolved_arg(name: Option<&str>, value: ResolvedExpr) -> ResolvedArgument {
    ResolvedArgument {
        name: name.map(String::from),
        value,
        span: dummy_span(),
    }
}

// Helper to add a (potentially generic) function definition
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

// --- Generic Function Call Tests ---

#[test]
fn test_generic_identity_call_concrete_types() {
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
    let call_bool = resolved_fn_call(func_sym, vec![resolved_arg(None, resolved_lit_bool(true))]);
    let result_bool = checker::expr::type_check_expression(&mut checker, &call_bool, None);
    assert!(result_bool.is_ok(), "identity(bool) failed: {:?}", result_bool.err());
    let typed_bool = result_bool.unwrap();
    assert_eq!(typed_bool.ty, ty_prim(PrimitiveType::Bool), "Expected bool return");

    // Call identity(123) - Use a fresh checker instance to avoid interference
    let mut checker2 = setup_checker();
     add_generic_function_def(
        &mut checker2,
        "identity",
        func_sym,
        vec![GenericParamDef { name: "T".to_string(), symbol: gen_param_sym, id: gen_param_id, bounds: vec![], span: dummy_span(), }],
        vec![ParamType { name: "x".to_string(), ty: ty_t.clone(), span: dummy_span() }],
        ty_t.clone(),
    );
    let call_int = resolved_fn_call(func_sym, vec![resolved_arg(None, resolved_lit_int(123))]);
    let result_int = checker::expr::type_check_expression(&mut checker2, &call_int, None);
    assert!(result_int.is_ok(), "identity(int) failed: {:?}", result_int.err());
    let typed_int = result_int.unwrap();
    // Expect IntegerLiteral because the input literal hasn't been forced to a specific type yet.
    assert_eq!(typed_int.ty, ty_prim(PrimitiveType::IntegerLiteral), "Expected IntegerLiteral return");

    // Call identity("hello")
    let mut checker3 = setup_checker();
     add_generic_function_def(
        &mut checker3,
        "identity",
        func_sym,
        vec![GenericParamDef { name: "T".to_string(), symbol: gen_param_sym, id: gen_param_id, bounds: vec![], span: dummy_span(), }],
        vec![ParamType { name: "x".to_string(), ty: ty_t.clone(), span: dummy_span() }],
        ty_t.clone(),
    );
    let call_str = resolved_fn_call(func_sym, vec![resolved_arg(None, resolved_lit_string("hello"))]);
    let result_str = checker::expr::type_check_expression(&mut checker3, &call_str, None);
    assert!(result_str.is_ok(), "identity(string) failed: {:?}", result_str.err());
    let typed_str = result_str.unwrap();
    assert_eq!(typed_str.ty, ty_prim(PrimitiveType::String), "Expected String return");
}

#[test]
fn test_generic_pair_call() {
    let mut checker = setup_checker();
    let func_sym = Symbol::new(20);
    let gen_t_sym = Symbol::new(21);
    let gen_u_sym = Symbol::new(22);
    let gen_t_id = TypeId(0);
    let gen_u_id = TypeId(1);
    let ty_t = ty_var(0);
    let ty_u = ty_var(1);

    add_generic_function_def(
        &mut checker,
        "pair",
        func_sym,
        vec![
            GenericParamDef { name: "T".to_string(), symbol: gen_t_sym, id: gen_t_id, bounds: vec![], span: dummy_span() },
            GenericParamDef { name: "U".to_string(), symbol: gen_u_sym, id: gen_u_id, bounds: vec![], span: dummy_span() },
        ],
        vec![
            ParamType { name: "a".to_string(), ty: ty_t.clone(), span: dummy_span() },
            ParamType { name: "b".to_string(), ty: ty_u.clone(), span: dummy_span() },
        ],
        ty_tuple(vec![ty_t.clone(), ty_u.clone()]), // Returns (T, U)
    );

    // Call pair(10, true)
    let call_expr = resolved_fn_call(func_sym, vec![
        resolved_arg(None, resolved_lit_int(10)),   // T -> IntegerLiteral
        resolved_arg(None, resolved_lit_bool(true)), // U -> Bool
    ]);
    let result = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result.is_ok(), "pair(int, bool) failed: {:?}", result.err());
    let typed_expr = result.unwrap();

    let expected_ty = ty_tuple(vec![ty_prim(PrimitiveType::IntegerLiteral), ty_prim(PrimitiveType::Bool)]);
    assert_eq!(typed_expr.ty, expected_ty, "Expected (IntegerLiteral, bool) return");
    assert!(checker.errors.is_empty());
}

// --- Helpers for Trait Bound Tests ---

// Adds a dummy "Displayable" trait
fn add_dummy_displayable_trait(checker: &mut TypeChecker) -> (Symbol, TraitId) {
    let trait_sym = Symbol::new(5000);
    let trait_id = checker.trait_repo.next_trait_id();
    let trait_def = TraitDef {
        id: trait_id,
        trait_symbol: trait_sym,
        name: "Displayable".to_string(),
        generic_params: vec![],
        methods: HashMap::new(), // No methods needed for bound checking test
        associated_types: HashMap::new(),
        span: dummy_span(),
    };
    checker.trait_repo.add_trait(trait_def);
    checker.type_ctx.add_trait_symbol(trait_sym, "Displayable".to_string(), trait_id);
    (trait_sym, trait_id)
}

// Adds a dummy "Debuggable" trait
fn add_dummy_debuggable_trait(checker: &mut TypeChecker) -> (Symbol, TraitId) {
    let trait_sym = Symbol::new(6000);
    let trait_id = checker.trait_repo.next_trait_id();
    let trait_def = TraitDef {
        id: trait_id,
        trait_symbol: trait_sym,
        name: "Debuggable".to_string(),
        generic_params: vec![],
        methods: HashMap::new(),
        associated_types: HashMap::new(),
        span: dummy_span(),
    };
    checker.trait_repo.add_trait(trait_def);
    checker.type_ctx.add_trait_symbol(trait_sym, "Debuggable".to_string(), trait_id);
    (trait_sym, trait_id)
}

// Adds an impl of a dummy trait for a primitive type
fn add_dummy_trait_impl_for_prim(checker: &mut TypeChecker, trait_id: TraitId, prim_ty: PrimitiveType) {
    let impl_symbol = Symbol::new(7000 + trait_id.0 + prim_ty as u32); // Simple unique ID generation for test
    let impl_id = checker.trait_repo.next_impl_id();
    let impl_def = ImplDef {
        id: impl_id,
        impl_symbol,
        trait_ref: Some(TraitRef { trait_id, type_arguments: vec![], span: dummy_span() }),
        implementing_type: ty_prim(prim_ty),
        generic_params: vec![],
        methods: HashMap::new(),
        associated_type_bindings: HashMap::new(),
        span: dummy_span(),
    };
    checker.trait_repo.add_impl(impl_def);
}

// --- Tests for Generic Functions with Trait Bounds ---

#[test]
fn test_generic_call_bound_satisfied() {
    let mut checker = setup_checker();
    let (_display_trait_sym, display_trait_id) = add_dummy_displayable_trait(&mut checker);
    // Implement Displayable for String
    add_dummy_trait_impl_for_prim(&mut checker, display_trait_id, PrimitiveType::String);

    let func_sym = Symbol::new(30);
    let gen_t_sym = Symbol::new(31);
    let gen_t_id = TypeId(0);
    let ty_t = ty_var(0);

    // Define `print_displayable<T: Displayable>(val: T)`
    add_generic_function_def(
        &mut checker,
        "print_displayable",
        func_sym,
        vec![GenericParamDef { name: "T".to_string(), symbol: gen_t_sym, id: gen_t_id,
                               bounds: vec![TraitRef { trait_id: display_trait_id, type_arguments: vec![], span: dummy_span() }],
                               span: dummy_span() }],
        vec![ParamType { name: "val".to_string(), ty: ty_t.clone(), span: dummy_span() }],
        ty_prim(PrimitiveType::Unit),
    );

    // Call print_displayable("hello") - String implements Displayable
    let call_expr = resolved_fn_call(func_sym, vec![resolved_arg(None, resolved_lit_string("hello"))]);
    let result = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result.is_ok(), "Bound satisfied call failed: {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::Unit));
    assert!(checker.errors.is_empty(), "Expected no errors, got: {:?}", checker.errors);
}

#[test]
fn test_generic_call_bound_not_satisfied() {
    let mut checker = setup_checker();
    let (_display_trait_sym, display_trait_id) = add_dummy_displayable_trait(&mut checker);
    // DO NOT implement Displayable for I32

    let func_sym = Symbol::new(30);
    let gen_t_sym = Symbol::new(31);
    let gen_t_id = TypeId(0);
    let ty_t = ty_var(0);

    // Define `print_displayable<T: Displayable>(val: T)`
    add_generic_function_def(
        &mut checker,
        "print_displayable",
        func_sym,
        vec![GenericParamDef { name: "T".to_string(), symbol: gen_t_sym, id: gen_t_id,
                               bounds: vec![TraitRef { trait_id: display_trait_id, type_arguments: vec![], span: dummy_span() }],
                               span: dummy_span() }],
        vec![ParamType { name: "val".to_string(), ty: ty_t.clone(), span: dummy_span() }],
        ty_prim(PrimitiveType::Unit),
    );

    // Call print_displayable(123) - I32 does NOT implement Displayable
    let call_expr = resolved_fn_call(func_sym, vec![resolved_arg(None, resolved_lit_int(123))]);
    let result = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result.is_err(), "Expected error for unsatisfied bound");
    // The specific error might vary, but it should relate to the missing trait impl
    assert!(matches!(result.err().unwrap(), TypeError::TypeDoesNotImplementTrait { .. } | TypeError::RequiredTraitNotImplemented { .. }),
            "Expected TraitNotImplemented error");
}

#[test]
fn test_generic_call_multiple_bounds() {
    let mut checker = setup_checker();
    let (_display_trait_sym, display_trait_id) = add_dummy_displayable_trait(&mut checker);
    let (_debug_trait_sym, debug_trait_id) = add_dummy_debuggable_trait(&mut checker);

    // Implement Displayable AND Debuggable for String
    add_dummy_trait_impl_for_prim(&mut checker, display_trait_id, PrimitiveType::String);
    add_dummy_trait_impl_for_prim(&mut checker, debug_trait_id, PrimitiveType::String);
    // Implement ONLY Displayable for I32
    add_dummy_trait_impl_for_prim(&mut checker, display_trait_id, PrimitiveType::I32);

    let func_sym = Symbol::new(40);
    let gen_t_sym = Symbol::new(41);
    let gen_t_id = TypeId(0);
    let ty_t = ty_var(0);

    // Define `process<T: Displayable + Debuggable>(data: T)`
    add_generic_function_def(
        &mut checker,
        "process",
        func_sym,
        vec![GenericParamDef { name: "T".to_string(), symbol: gen_t_sym, id: gen_t_id,
                               bounds: vec![
                                   TraitRef { trait_id: display_trait_id, type_arguments: vec![], span: dummy_span() },
                                   TraitRef { trait_id: debug_trait_id, type_arguments: vec![], span: dummy_span() },
                               ],
                               span: dummy_span() }],
        vec![ParamType { name: "data".to_string(), ty: ty_t.clone(), span: dummy_span() }],
        ty_prim(PrimitiveType::Unit),
    );

    // Call process("works") - String implements both
    let call_ok = resolved_fn_call(func_sym, vec![resolved_arg(None, resolved_lit_string("works"))]);
    let result_ok = checker::expr::type_check_expression(&mut checker, &call_ok, None);
    assert!(result_ok.is_ok(), "Multiple bounds satisfied call failed: {:?}", result_ok.err());
    assert_eq!(result_ok.unwrap().ty, ty_prim(PrimitiveType::Unit));
    assert!(checker.errors.is_empty(), "Expected no errors for satisfied multiple bounds, got: {:?}", checker.errors);

    // Call process(123) - I32 implements Displayable but NOT Debuggable
    // Need a fresh checker because the previous call might have affected inference state
    let mut checker2 = setup_checker();
    let (_display_trait_sym2, display_trait_id2) = add_dummy_displayable_trait(&mut checker2);
    let (_debug_trait_sym2, debug_trait_id2) = add_dummy_debuggable_trait(&mut checker2);
    add_dummy_trait_impl_for_prim(&mut checker2, display_trait_id2, PrimitiveType::I32);
    add_generic_function_def(
        &mut checker2,
        "process",
        func_sym, // Reuse symbol
        vec![GenericParamDef { name: "T".to_string(), symbol: gen_t_sym, id: gen_t_id,
                               bounds: vec![
                                   TraitRef { trait_id: display_trait_id2, type_arguments: vec![], span: dummy_span() },
                                   TraitRef { trait_id: debug_trait_id2, type_arguments: vec![], span: dummy_span() },
                               ],
                               span: dummy_span() }],
        vec![ParamType { name: "data".to_string(), ty: ty_t.clone(), span: dummy_span() }],
        ty_prim(PrimitiveType::Unit),
    );

    let call_err = resolved_fn_call(func_sym, vec![resolved_arg(None, resolved_lit_int(123))]);
    let result_err = checker::expr::type_check_expression(&mut checker2, &call_err, None);
    assert!(result_err.is_err(), "Expected error for unsatisfied second bound");
    assert!(matches!(result_err.err().unwrap(), TypeError::TypeDoesNotImplementTrait { trait_name, .. } | TypeError::RequiredTraitNotImplemented { trait_name, .. } if trait_name == "Debuggable"),
            "Expected TraitNotImplemented error for Debuggable");
}

// --- Helpers for Generic Method Tests ---

// Helper Generic Wrapper Struct Def
fn add_generic_wrapper_struct(checker: &mut TypeChecker) -> (Symbol, GenericParamDef) {
    let struct_sym = Symbol::new(8000);
    let value_sym = Symbol::new(8001);
    let gen_param_sym = Symbol::new(8002);
    // Use a TypeId known not to be used by inference in simple tests (e.g., starting high)
    let gen_param_id = TypeId(8003);

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
            ty: ty_var(gen_param_id.0), // Use TyKind::Var for the generic type T
            span: dummy_span(),
        }],
        span: dummy_span(),
    };
    checker.type_ctx.add_type(struct_sym, "Wrapper".to_string(), TypeDef::Struct(struct_def));
    (struct_sym, gen_param_def)
}

// Helper to add a method impl for Wrapper<T>
fn add_generic_wrapper_get_method(
    checker: &mut TypeChecker,
    wrapper_sym: Symbol,
    get_method_sym: Symbol,
    gen_param_def: GenericParamDef, // Receive the generic param def
) {
    let impl_symbol = Symbol::new(8010);
    let impl_id = checker.trait_repo.next_impl_id();
    let impl_def = ImplDef {
        id: impl_id,
        impl_symbol,
        trait_ref: None, // Inherent impl
        // The implementing type is Wrapper<T>
        implementing_type: ty_named("Wrapper", Some(wrapper_sym), vec![ty_var(gen_param_def.id.0)]),
        generic_params: vec![gen_param_def.clone()], // Generic param defined on the impl
        methods: HashMap::from([(get_method_sym, get_method_sym)]), // Map method symbol to itself
        associated_type_bindings: HashMap::new(),
        span: dummy_span(),
    };
    checker.trait_repo.add_impl(impl_def);

    // Signature for `get(&self) -> T` (assuming reference for now)
    // NOTE: `Self` type needs proper handling/substitution during check if used
    let get_sig = FunctionSignature {
        name: "get".to_string(),
        self_param: Some(SelfParamKind::Value), // Assuming `self` for simplicity, adjust if needed
        generic_params: vec![], // No *additional* generics on the method itself
        params: vec![],
        return_type: ty_var(gen_param_def.id.0), // Returns T
        span: dummy_span(),
    };
    checker.type_ctx.add_type(get_method_sym, "get".to_string(), TypeDef::Function(get_sig));
}

// --- Tests for Generic Method Calls ---

#[test]
fn test_method_call_on_generic_struct() {
    let mut checker = setup_checker();
    let (wrapper_sym, gen_param_def) = add_generic_wrapper_struct(&mut checker);
    let get_method_sym = Symbol::new(8011);
    add_generic_wrapper_get_method(&mut checker, wrapper_sym, get_method_sym, gen_param_def);

    // Assume `w_i32` is a variable of type `Wrapper<i32>`
    let var_i32_sym = Symbol::new(8020);
    let receiver_i32_ty = ty_named("Wrapper", Some(wrapper_sym), vec![ty_prim(PrimitiveType::I32)]);
    let mut env = TypeEnvironment::new();
    env.add("w_i32".to_string(), receiver_i32_ty.clone());
    checker._type_env = Arc::new(env);

    let receiver_i32_expr = ResolvedExpr {
        kind: ResolvedExprKind::Variable { binding_symbol: var_i32_sym, name: "w_i32".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::UserDefined { symbol: wrapper_sym, type_args: Some(vec![ResolvedType::Primitive(ResolvePrimitiveType::I32)]) },
    };

    // Call w_i32.get()
    let call_expr_i32 = resolved_method_call(receiver_i32_expr, "get", vec![]);
    let result_i32 = checker::expr::type_check_expression(&mut checker, &call_expr_i32, None);
    assert!(result_i32.is_ok(), "w_i32.get() failed: {:?}", result_i32.err());
    let typed_i32 = result_i32.unwrap();
    assert_eq!(typed_i32.ty, ty_prim(PrimitiveType::I32), "Expected get() to return i32");
    assert!(checker.errors.is_empty(), "Errors found: {:?}", checker.errors);

    // Assume `w_bool` is a variable of type `Wrapper<bool>`
    // Need a separate checker instance or reset environment
    let mut checker2 = setup_checker();
    let (wrapper_sym2, gen_param_def2) = add_generic_wrapper_struct(&mut checker2);
    let get_method_sym2 = Symbol::new(8011); // Can reuse symbol if definition is identical
    add_generic_wrapper_get_method(&mut checker2, wrapper_sym2, get_method_sym2, gen_param_def2);

    let var_bool_sym = Symbol::new(8030);
    let receiver_bool_ty = ty_named("Wrapper", Some(wrapper_sym2), vec![ty_prim(PrimitiveType::Bool)]);
    let mut env_bool = TypeEnvironment::new();
    env_bool.add("w_bool".to_string(), receiver_bool_ty.clone());
    checker2._type_env = Arc::new(env_bool);

    let receiver_bool_expr = ResolvedExpr {
        kind: ResolvedExprKind::Variable { binding_symbol: var_bool_sym, name: "w_bool".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::UserDefined { symbol: wrapper_sym2, type_args: Some(vec![ResolvedType::Primitive(ResolvePrimitiveType::Bool)]) },
    };

    // Call w_bool.get()
    let call_expr_bool = resolved_method_call(receiver_bool_expr, "get", vec![]);
    let result_bool = checker::expr::type_check_expression(&mut checker2, &call_expr_bool, None);
    assert!(result_bool.is_ok(), "w_bool.get() failed: {:?}", result_bool.err());
    let typed_bool = result_bool.unwrap();
    assert_eq!(typed_bool.ty, ty_prim(PrimitiveType::Bool), "Expected get() to return bool");
    assert!(checker2.errors.is_empty(), "Errors found: {:?}", checker2.errors);
}

// --- Helpers for Generic Methods on Concrete Types ---

// Helper Concrete Struct Def
fn add_processor_struct(checker: &mut TypeChecker) -> Symbol {
    let struct_sym = Symbol::new(9000);
    let struct_def = StructDef {
        name: "Processor".to_string(),
        symbol: struct_sym,
        generic_params: vec![], // Concrete type
        fields: vec![],         // No fields needed
        span: dummy_span(),
    };
    checker.type_ctx.add_type(struct_sym, "Processor".to_string(), TypeDef::Struct(struct_def));
    struct_sym
}

// Helper to add a generic method `process<T: Displayable>` to Processor
fn add_processor_generic_process_method(
    checker: &mut TypeChecker,
    processor_sym: Symbol,
    process_method_sym: Symbol,
    displayable_trait_id: TraitId, // Pass in the trait ID for the bound
) -> GenericParamDef {
    let impl_symbol = Symbol::new(9010);
    let impl_id = checker.trait_repo.next_impl_id();
    let impl_def = ImplDef {
        id: impl_id,
        impl_symbol,
        trait_ref: None, // Inherent impl for Processor
        implementing_type: ty_named("Processor", Some(processor_sym), vec![]),
        generic_params: vec![], // No generics on the impl block itself
        methods: HashMap::from([(process_method_sym, process_method_sym)]),
        associated_type_bindings: HashMap::new(),
        span: dummy_span(),
    };
    checker.trait_repo.add_impl(impl_def);

    // Define the generic parameter T for the method
    let gen_t_sym = Symbol::new(9011);
    let gen_t_id = TypeId(9012);
    let gen_param_def = GenericParamDef {
        name: "T".to_string(),
        symbol: gen_t_sym,
        id: gen_t_id,
        bounds: vec![TraitRef { trait_id: displayable_trait_id, type_arguments: vec![], span: dummy_span() }],
        span: dummy_span(),
    };

    // Signature for `process<T: Displayable>(&self, data: T) -> String`
    let process_sig = FunctionSignature {
        name: "process".to_string(),
        self_param: Some(SelfParamKind::Value), // Assuming `self`, adjust if needed
        generic_params: vec![gen_param_def.clone()], // Generic param defined on the method
        params: vec![ParamType { name: "data".to_string(), ty: ty_var(gen_t_id.0), span: dummy_span() }],
        return_type: ty_prim(PrimitiveType::String), // Example return type
        span: dummy_span(),
    };
    checker.type_ctx.add_type(process_method_sym, "process".to_string(), TypeDef::Function(process_sig));

    gen_param_def // Return the generic param def for potential use in tests
}

// --- Tests for Generic Methods on Concrete Types ---

#[test]
fn test_generic_method_on_concrete_type() {
    let mut checker = setup_checker();
    let processor_sym = add_processor_struct(&mut checker);
    let process_method_sym = Symbol::new(9013);
    let (_display_trait_sym, display_trait_id) = add_dummy_displayable_trait(&mut checker);
    add_processor_generic_process_method(&mut checker, processor_sym, process_method_sym, display_trait_id);

    // Implement Displayable for String and I32
    add_dummy_trait_impl_for_prim(&mut checker, display_trait_id, PrimitiveType::String);
    add_dummy_trait_impl_for_prim(&mut checker, display_trait_id, PrimitiveType::I32);
    // Bool does NOT implement Displayable

    // Assume `p` is a variable of type `Processor`
    let var_p_sym = Symbol::new(9020);
    let processor_ty = ty_named("Processor", Some(processor_sym), vec![]);
    let mut env = TypeEnvironment::new();
    env.add("p".to_string(), processor_ty.clone());
    checker._type_env = Arc::new(env);

    let receiver_expr = ResolvedExpr {
        kind: ResolvedExprKind::Variable { binding_symbol: var_p_sym, name: "p".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::UserDefined { symbol: processor_sym, type_args: None },
    };

    // Call p.process("hello") - String implements Displayable
    let call_ok_str = resolved_method_call(receiver_expr.clone(), "process", vec![resolved_arg(None, resolved_lit_string("hello"))]);
    let result_ok_str = checker::expr::type_check_expression(&mut checker, &call_ok_str, None);
    assert!(result_ok_str.is_ok(), "p.process(String) failed: {:?}", result_ok_str.err());
    assert_eq!(result_ok_str.unwrap().ty, ty_prim(PrimitiveType::String));
    assert!(checker.errors.is_empty(), "Errors found (String): {:?}", checker.errors);

    // Call p.process(123) - I32 implements Displayable
    // Use fresh checker for isolation
    let mut checker2 = setup_checker();
    let processor_sym2 = add_processor_struct(&mut checker2);
    let (_display_trait_sym2, display_trait_id2) = add_dummy_displayable_trait(&mut checker2);
    add_processor_generic_process_method(&mut checker2, processor_sym2, process_method_sym, display_trait_id2);
    add_dummy_trait_impl_for_prim(&mut checker2, display_trait_id2, PrimitiveType::I32);
    let mut env2 = TypeEnvironment::new();
    env2.add("p".to_string(), ty_named("Processor", Some(processor_sym2), vec![]));
    checker2._type_env = Arc::new(env2);
    let receiver_expr2 = ResolvedExpr {
        kind: ResolvedExprKind::Variable { binding_symbol: var_p_sym, name: "p".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::UserDefined { symbol: processor_sym2, type_args: None },
    };

    let call_ok_int = resolved_method_call(receiver_expr2.clone(), "process", vec![resolved_arg(None, resolved_lit_int(123))]);
    let result_ok_int = checker::expr::type_check_expression(&mut checker2, &call_ok_int, None);
    assert!(result_ok_int.is_ok(), "p.process(i32) failed: {:?}", result_ok_int.err());
    assert_eq!(result_ok_int.unwrap().ty, ty_prim(PrimitiveType::String));
    assert!(checker2.errors.is_empty(), "Errors found (i32): {:?}", checker2.errors);

    // Call p.process(true) - Bool does NOT implement Displayable
    // Use fresh checker
    let mut checker3 = setup_checker();
    let processor_sym3 = add_processor_struct(&mut checker3);
    let (_display_trait_sym3, display_trait_id3) = add_dummy_displayable_trait(&mut checker3);
    add_processor_generic_process_method(&mut checker3, processor_sym3, process_method_sym, display_trait_id3);
    // No impl for Bool
    let mut env3 = TypeEnvironment::new();
    env3.add("p".to_string(), ty_named("Processor", Some(processor_sym3), vec![]));
    checker3._type_env = Arc::new(env3);
    let receiver_expr3 = ResolvedExpr {
        kind: ResolvedExprKind::Variable { binding_symbol: var_p_sym, name: "p".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::UserDefined { symbol: processor_sym3, type_args: None },
    };

    let call_err_bool = resolved_method_call(receiver_expr3, "process", vec![resolved_arg(None, resolved_lit_bool(true))]);
    let result_err_bool = checker::expr::type_check_expression(&mut checker3, &call_err_bool, None);
    assert!(result_err_bool.is_err(), "Expected error for p.process(bool)");
    assert!(matches!(result_err_bool.err().unwrap(), TypeError::TypeDoesNotImplementTrait { trait_name, .. } | TypeError::RequiredTraitNotImplemented { trait_name, .. } if trait_name == "Displayable"),
            "Expected TraitNotImplemented error for Displayable on bool");
}

// --- Helper for Static Generic Method ---

fn add_generic_wrapper_static_new_method(
    checker: &mut TypeChecker,
    wrapper_sym: Symbol,
    new_method_sym: Symbol,
    gen_param_def: GenericParamDef, // The <T> from Wrapper<T>
) {
    // Impl block for Wrapper<T> (needed to associate the static method)
    let impl_symbol = Symbol::new(8050); // Unique symbol for this impl
    let impl_id = checker.trait_repo.next_impl_id();
    let impl_def = ImplDef {
        id: impl_id,
        impl_symbol,
        trait_ref: None,
        implementing_type: ty_named("Wrapper", Some(wrapper_sym), vec![ty_var(gen_param_def.id.0)]),
        generic_params: vec![gen_param_def.clone()], // Impl is generic over T
        methods: HashMap::from([(new_method_sym, new_method_sym)]), // Associate static method
        associated_type_bindings: HashMap::new(),
        span: dummy_span(),
    };
    checker.trait_repo.add_impl(impl_def);

    // Signature for static `new<T>(value: T) -> Wrapper<T>`
    // Note: The method itself might not need to *re-declare* the generic T
    // if it's implicitly captured from the surrounding impl<T> scope.
    // However, FunctionSignature currently requires explicitly listing generic_params.
    // For a static method associated with `Wrapper<T>`, the signature should reflect that.
    let new_sig = FunctionSignature {
        name: "new".to_string(),
        self_param: None, // Static method
        // Re-declare T here, or assume context? Let's re-declare for clarity in the test setup.
        generic_params: vec![gen_param_def.clone()],
        params: vec![ParamType { name: "value".to_string(), ty: ty_var(gen_param_def.id.0), span: dummy_span() }],
        // Return type is Wrapper<T>
        return_type: ty_named("Wrapper", Some(wrapper_sym), vec![ty_var(gen_param_def.id.0)]),
        span: dummy_span(),
    };
    checker.type_ctx.add_type(new_method_sym, "new".to_string(), TypeDef::Function(new_sig));
}

// --- Test for Static Generic Methods ---

#[test]
fn test_static_generic_method_call() {
    let mut checker = setup_checker();
    let (wrapper_sym, gen_param_def) = add_generic_wrapper_struct(&mut checker);
    let new_method_sym = Symbol::new(8051);
    add_generic_wrapper_static_new_method(&mut checker, wrapper_sym, new_method_sym, gen_param_def);

    // Represent Wrapper::<i32>::new(10)
    // The resolver links the path `Wrapper::new` to `new_method_sym`.
    // Explicit type args `::<i32>` are handled during type checking the call.
    let call_i32_expr = resolved_fn_call(
        new_method_sym, // Symbol of the static `new` method
        vec![resolved_arg(None, resolved_lit_int(10))] // Argument `value: 10`
        // We need a way to represent the `::<i32>` in ResolvedExpr for the checker
        // TODO: Extend ResolvedExprKind::Call to include explicit type arguments
        // For now, assume the checker can infer T=i32 from the argument type.
    );

    let result_i32 = checker::expr::type_check_expression(&mut checker, &call_i32_expr, None);
    assert!(result_i32.is_ok(), "Wrapper::<i32>::new(10) failed: {:?}", result_i32.err());
    let typed_i32 = result_i32.unwrap();
    let expected_i32_ty = ty_named("Wrapper", Some(wrapper_sym), vec![ty_prim(PrimitiveType::I32)]);
    assert_eq!(typed_i32.ty, expected_i32_ty, "Expected return type Wrapper<i32>");
    assert!(checker.errors.is_empty(), "Errors found (i32): {:?}", checker.errors);

    // Represent Wrapper::<String>::new("hello")
    // Use a fresh checker
    let mut checker2 = setup_checker();
    let (wrapper_sym2, gen_param_def2) = add_generic_wrapper_struct(&mut checker2);
    add_generic_wrapper_static_new_method(&mut checker2, wrapper_sym2, new_method_sym, gen_param_def2);

    let call_str_expr = resolved_fn_call(
        new_method_sym,
        vec![resolved_arg(None, resolved_lit_string("hello"))]
    );
    let result_str = checker::expr::type_check_expression(&mut checker2, &call_str_expr, None);
    assert!(result_str.is_ok(), "Wrapper::<String>::new(\"hello\") failed: {:?}", result_str.err());
    let typed_str = result_str.unwrap();
    let expected_str_ty = ty_named("Wrapper", Some(wrapper_sym2), vec![ty_prim(PrimitiveType::String)]);
    assert_eq!(typed_str.ty, expected_str_ty, "Expected return type Wrapper<String>");
    assert!(checker2.errors.is_empty(), "Errors found (String): {:?}", checker2.errors);
}

// --- Helpers for Generic Methods on Generic Types ---

// Dummy Fn trait for testing `map`
fn add_dummy_fn_trait(checker: &mut TypeChecker) -> (Symbol, TraitId, GenericParamDef, GenericParamDef) {
    let trait_sym = Symbol::new(10000);
    let trait_id = checker.trait_repo.next_trait_id();

    // Generic parameters for the trait: Arg, Ret
    let gen_arg_sym = Symbol::new(10001);
    let gen_ret_sym = Symbol::new(10002);
    let gen_arg_id = TypeId(10003);
    let gen_ret_id = TypeId(10004);

    let gen_arg_def = GenericParamDef { name: "Arg".to_string(), symbol: gen_arg_sym, id: gen_arg_id, bounds: vec![], span: dummy_span() };
    let gen_ret_def = GenericParamDef { name: "Ret".to_string(), symbol: gen_ret_sym, id: gen_ret_id, bounds: vec![], span: dummy_span() };

    // We don't need actual methods for this test, just the trait definition with generics
    let trait_def = TraitDef {
        id: trait_id,
        trait_symbol: trait_sym,
        name: "DummyFn".to_string(),
        generic_params: vec![gen_arg_def.clone(), gen_ret_def.clone()],
        methods: HashMap::new(),
        associated_types: HashMap::new(),
        span: dummy_span(),
    };
    checker.trait_repo.add_trait(trait_def);
    checker.type_ctx.add_trait_symbol(trait_sym, "DummyFn".to_string(), trait_id);
    (trait_sym, trait_id, gen_arg_def, gen_ret_def)
}

// Helper to add the generic `map` method to `Wrapper<T>`
fn add_generic_wrapper_map_method(
    checker: &mut TypeChecker,
    wrapper_sym: Symbol,
    map_method_sym: Symbol,
    wrapper_t_param: GenericParamDef, // Generic param <T> from Wrapper<T>
    dummy_fn_trait_id: TraitId,      // The ID of our dummy Fn trait
) -> (GenericParamDef, GenericParamDef) { // Returns the <U> and <F> params of map
    let impl_symbol = Symbol::new(8070); // Unique symbol for this impl
    let impl_id = checker.trait_repo.next_impl_id();

    // Impl block is generic over T (from Wrapper<T>)
    let impl_def = ImplDef {
        id: impl_id,
        impl_symbol,
        trait_ref: None,
        implementing_type: ty_named("Wrapper", Some(wrapper_sym), vec![ty_var(wrapper_t_param.id.0)]),
        generic_params: vec![wrapper_t_param.clone()],
        methods: HashMap::from([(map_method_sym, map_method_sym)]),
        associated_type_bindings: HashMap::new(),
        span: dummy_span(),
    };
    checker.trait_repo.add_impl(impl_def);

    // Define generic parameters for the `map` method: <U, F: DummyFn<T, U>>
    let gen_u_sym = Symbol::new(8071);
    let gen_f_sym = Symbol::new(8072);
    let gen_u_id = TypeId(8073);
    let gen_f_id = TypeId(8074);

    let gen_u_def = GenericParamDef { name: "U".to_string(), symbol: gen_u_sym, id: gen_u_id, bounds: vec![], span: dummy_span() };
    let gen_f_def = GenericParamDef {
        name: "F".to_string(),
        symbol: gen_f_sym,
        id: gen_f_id,
        // Bound: F: DummyFn<T, U>
        bounds: vec![TraitRef {
            trait_id: dummy_fn_trait_id,
            // Trait arguments are the types T (from Wrapper) and U (from map)
            type_arguments: vec![ty_var(wrapper_t_param.id.0), ty_var(gen_u_id.0)],
            span: dummy_span(),
        }],
        span: dummy_span(),
    };

    // Signature for `map<U, F: DummyFn<T, U>>(self, f: F) -> Wrapper<U>`
    let map_sig = FunctionSignature {
        name: "map".to_string(),
        self_param: Some(SelfParamKind::Value), // Assuming takes self by value
        generic_params: vec![gen_u_def.clone(), gen_f_def.clone()],
        params: vec![ParamType { name: "f".to_string(), ty: ty_var(gen_f_id.0), span: dummy_span() }],
        // Return type is Wrapper<U>
        return_type: ty_named("Wrapper", Some(wrapper_sym), vec![ty_var(gen_u_id.0)]),
        span: dummy_span(),
    };
    checker.type_ctx.add_type(map_method_sym, "map".to_string(), TypeDef::Function(map_sig));

    (gen_u_def, gen_f_def)
}

// --- Test for Generic Methods on Generic Types ---

#[test]
fn test_generic_method_on_generic_type() {
    let mut checker = setup_checker();
    let (wrapper_sym, wrapper_t_param) = add_generic_wrapper_struct(&mut checker);
    let map_method_sym = Symbol::new(8075);
    let (_fn_trait_sym, fn_trait_id, fn_arg_param, fn_ret_param) = add_dummy_fn_trait(&mut checker);
    add_generic_wrapper_map_method(&mut checker, wrapper_sym, map_method_sym, wrapper_t_param.clone(), fn_trait_id);

    // Define a dummy function `i32_to_string(i: i32) -> String`
    let i32_to_string_sym = Symbol::new(11000);
    let i32_to_string_sig = FunctionSignature {
        name: "i32_to_string".to_string(),
        self_param: None,
        generic_params: vec![],
        params: vec![ParamType { name: "i".to_string(), ty: ty_prim(PrimitiveType::I32), span: dummy_span() }],
        return_type: ty_prim(PrimitiveType::String),
        span: dummy_span(),
    };
    checker.type_ctx.add_type(i32_to_string_sym, "i32_to_string".to_string(), TypeDef::Function(i32_to_string_sig));

    // Implement DummyFn<i32, String> for the type of our dummy function
    // We need a concrete type for the function itself. Let's represent it with its own unique type for the test.
    let fn_type_sym = Symbol::new(11001);
    let fn_type = ty_named("FnType_i32_to_string", Some(fn_type_sym), vec![]);
    let fn_impl_symbol = Symbol::new(11002);
    let fn_impl_id = checker.trait_repo.next_impl_id();
    let fn_impl_def = ImplDef {
        id: fn_impl_id,
        impl_symbol: fn_impl_symbol,
        // Implementing DummyFn<i32, String>
        trait_ref: Some(TraitRef {
            trait_id: fn_trait_id,
            type_arguments: vec![ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::String)],
            span: dummy_span(),
        }),
        implementing_type: fn_type.clone(), // Impl for the specific function's type
        generic_params: vec![],
        methods: HashMap::new(),
        associated_type_bindings: HashMap::new(),
        span: dummy_span(),
    };
    checker.trait_repo.add_impl(fn_impl_def);

    // Assume `w_i32` is a variable of type `Wrapper<i32>`
    let var_w_sym = Symbol::new(8080);
    let receiver_ty = ty_named("Wrapper", Some(wrapper_sym), vec![ty_prim(PrimitiveType::I32)]);
    // Assume `func` is a variable holding our `i32_to_string` function (represented by fn_type)
    let var_f_sym = Symbol::new(8081);
    let mut env = TypeEnvironment::new();
    env.add("w_i32".to_string(), receiver_ty.clone());
    env.add("func".to_string(), fn_type.clone()); // Add the function variable
    checker._type_env = Arc::new(env);

    let receiver_expr = ResolvedExpr {
        kind: ResolvedExprKind::Variable { binding_symbol: var_w_sym, name: "w_i32".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::UserDefined { symbol: wrapper_sym, type_args: Some(vec![ResolvedType::Primitive(ResolvePrimitiveType::I32)]) },
    };
    // Argument is the function `func`
    let func_arg_expr = ResolvedExpr {
        kind: ResolvedExprKind::Variable { binding_symbol: var_f_sym, name: "func".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::UserDefined { symbol: fn_type_sym, type_args: None }, // Representing the function type
    };

    // Call w_i32.map(func)
    let call_expr = resolved_method_call(receiver_expr, "map", vec![resolved_arg(None, func_arg_expr)]);

    let result = checker::expr::type_check_expression(&mut checker, &call_expr, None);
    assert!(result.is_ok(), "w_i32.map(func) failed: {:?}", result.err());
    let typed_expr = result.unwrap();

    // Expected return type: Wrapper<String>
    let expected_ty = ty_named("Wrapper", Some(wrapper_sym), vec![ty_prim(PrimitiveType::String)]);
    assert_eq!(typed_expr.ty, expected_ty, "Expected return type Wrapper<String>");
    assert!(checker.errors.is_empty(), "Errors found: {:?}", checker.errors);
}

// TODO: Add tests for functions where inference requires explicit annotation (turbofish or context)
// TODO: Add tests for turbofish calls
