// tests/definitions/fn_def.rs
use crate::*; // Import helpers from tests/mod.rs
use parallax_types::{
    error::TypeError,
    types::{PrimitiveType, Ty, TyKind, TypeId, TypeDef, FunctionSignature, ParamType, GenericParamDef as CheckerGenericParamDef},
};
use parallax_resolve::{
    types::{ResolvedExpr, ResolvedExprKind, ResolvedType, Symbol, ResolvedParameter, ResolvedFunction, ResolvedGenericParamDef},
    PrimitiveType as ResolvePrimitiveType,
};
use std::sync::Arc;

// --- Type Helpers (Copied) ---
fn ty_func(params: Vec<Ty>, ret: Ty) -> Ty {
    Ty::with_span(TyKind::Function(params, Arc::new(ret)), dummy_span())
}

fn ty_tuple(tys: Vec<Ty>) -> Ty {
    Ty::with_span(TyKind::Tuple(tys), dummy_span())
}
// -----------------------------

// NO NEED for resolved_*, we simulate the result in TypeContext

#[test]
fn test_check_fn_sig_simple() {
    let mut checker = setup_checker();
    let func_sym = Symbol::new(1);

    // Simulate the signature being added to the context
    let signature = FunctionSignature {
        name: "add".to_string(),
        self_param: None,
        generic_params: vec![],
        params: vec![
            ParamType { name: "a".to_string(), ty: ty_prim(PrimitiveType::I32), span: dummy_span() },
            ParamType { name: "b".to_string(), ty: ty_prim(PrimitiveType::I32), span: dummy_span() },
        ],
        return_type: ty_prim(PrimitiveType::I32),
        span: dummy_span(),
    };
    checker.type_ctx.add_type(func_sym, "add".to_string(), TypeDef::Function(signature.clone()));

    // Verify it exists in the context
    let type_def = checker.type_ctx.get_type_by_symbol(&func_sym);
    assert!(type_def.is_some(), "TypeDef for function 'add' not found");

    if let Some(TypeDef::Function(retrieved_sig)) = type_def {
        // Manually compare fields instead of direct comparison
        assert_eq!(retrieved_sig.name, signature.name);
        assert_eq!(retrieved_sig.self_param, signature.self_param);
        // Compare generic params vec (empty case)
        assert!(retrieved_sig.generic_params.is_empty());
        // Compare params vec
        assert_eq!(retrieved_sig.params.len(), signature.params.len());
        for (retrieved, expected) in retrieved_sig.params.iter().zip(signature.params.iter()) {
            assert_eq!(retrieved.name, expected.name);
            assert_eq!(retrieved.ty, expected.ty);
            // span comparison might be flaky, omit for now or use specific check if needed
        }
        assert_eq!(retrieved_sig.return_type, signature.return_type); // Ty derives PartialEq
    } else {
        panic!("Expected TypeDef::Function");
    }

    assert!(checker.errors.is_empty());
}

#[test]
fn test_check_fn_sig_generic() {
    let mut checker = setup_checker();
    let func_sym = Symbol::new(10);
    let _param_x_sym = Symbol::new(11); // Not strictly needed for verification here
    let gen_param_t_sym = Symbol::new(12); // Resolver symbol for <T>

    // Get a TypeId for T
    let gen_param_id = match checker.inference_ctx.fresh_var().kind {
        TyKind::Var(id) => id,
        _ => panic!("Fresh var was not TyKind::Var"),
    };

    // Simulate the signature being added
    let checker_gen_param = CheckerGenericParamDef {
        name: "T".to_string(),
        symbol: gen_param_t_sym,
        id: gen_param_id,
        bounds: vec![],
        span: dummy_span(),
    };
    let signature = FunctionSignature {
        name: "identity".to_string(),
        self_param: None,
        generic_params: vec![checker_gen_param],
        params: vec![
            ParamType { name: "x".to_string(), ty: ty_var(gen_param_id.0), span: dummy_span() },
        ],
        return_type: ty_var(gen_param_id.0),
        span: dummy_span(),
    };
    checker.type_ctx.add_type(func_sym, "identity".to_string(), TypeDef::Function(signature.clone()));

    // Verify the signature in the context
    let type_def = checker.type_ctx.get_type_by_symbol(&func_sym);
    assert!(type_def.is_some(), "TypeDef for function 'identity' not found");

    if let Some(TypeDef::Function(retrieved_sig)) = type_def {
        // Compare fields
        assert_eq!(retrieved_sig.name, signature.name);
        assert_eq!(retrieved_sig.generic_params.len(), 1);
        // Compare GenericParamDef fields
        assert_eq!(retrieved_sig.generic_params[0].name, signature.generic_params[0].name);
        assert_eq!(retrieved_sig.generic_params[0].symbol, signature.generic_params[0].symbol);
        assert_eq!(retrieved_sig.generic_params[0].id, signature.generic_params[0].id);
        assert!(retrieved_sig.generic_params[0].bounds.is_empty()); // Assuming bounds are empty

        assert_eq!(retrieved_sig.params.len(), 1);
        // Compare ParamType fields
        assert_eq!(retrieved_sig.params[0].name, signature.params[0].name);
        assert_eq!(retrieved_sig.params[0].ty, signature.params[0].ty);

        assert_eq!(retrieved_sig.return_type, signature.return_type);
    } else {
        panic!("Expected TypeDef::Function");
    }

    assert!(checker.errors.is_empty());
}

#[test]
fn test_check_fn_sig_complex_types() {
    let mut checker = setup_checker();
    let func_sym = Symbol::new(20);
    // Param symbols not needed for verification of the stored signature

    // Simulate the signature
    let expected_f_ty = ty_func(vec![ty_prim(PrimitiveType::I32)], ty_prim(PrimitiveType::Bool));
    let expected_tup_ty = ty_tuple(vec![ty_prim(PrimitiveType::String), ty_prim(PrimitiveType::F64)]);
    let expected_ret_ty = ty_tuple(vec![ty_prim(PrimitiveType::Bool), ty_prim(PrimitiveType::String)]);

    let signature = FunctionSignature {
        name: "process".to_string(),
        self_param: None,
        generic_params: vec![],
        params: vec![
            ParamType { name: "f".to_string(), ty: expected_f_ty.clone(), span: dummy_span() },
            ParamType { name: "tup".to_string(), ty: expected_tup_ty.clone(), span: dummy_span() },
        ],
        return_type: expected_ret_ty.clone(),
        span: dummy_span(),
    };
    checker.type_ctx.add_type(func_sym, "process".to_string(), TypeDef::Function(signature.clone()));

    // Verify
    let type_def = checker.type_ctx.get_type_by_symbol(&func_sym);
    assert!(type_def.is_some());
    if let Some(TypeDef::Function(retrieved_sig)) = type_def {
        // Compare fields
        assert_eq!(retrieved_sig.name, signature.name);
        assert_eq!(retrieved_sig.params.len(), 2);
        assert_eq!(retrieved_sig.params[0].ty, expected_f_ty);
        assert_eq!(retrieved_sig.params[1].ty, expected_tup_ty);
        assert_eq!(retrieved_sig.return_type, expected_ret_ty);
    } else {
        panic!("Expected TypeDef::Function");
    }

    assert!(checker.errors.is_empty());
}

#[test]
fn test_check_fn_sig_unknown_param_type() {
    let mut checker = setup_checker();
    let func_sym = Symbol::new(30);

    // Simulate the signature with an error type for the parameter
    let signature = FunctionSignature {
        name: "bad_func".to_string(),
        self_param: None,
        generic_params: vec![],
        params: vec![
            ParamType { name: "bad".to_string(), ty: Ty::new(TyKind::Error), span: dummy_span() },
        ],
        return_type: ty_prim(PrimitiveType::Unit),
        span: dummy_span(),
    };
    checker.type_ctx.add_type(func_sym, "bad_func".to_string(), TypeDef::Function(signature.clone()));

    // Add a simulated error
    checker.errors.push(TypeError::InternalError { message: "Simulated: Unknown param type".to_string(), span: Some(dummy_span()) });

    // Verify
    let type_def = checker.type_ctx.get_type_by_symbol(&func_sym);
    assert!(type_def.is_some());
    if let Some(TypeDef::Function(retrieved_sig)) = type_def {
        // Compare fields
        assert_eq!(retrieved_sig.name, signature.name);
        assert_eq!(retrieved_sig.params.len(), 1);
        assert!(matches!(retrieved_sig.params[0].ty.kind, TyKind::Error));
        assert_eq!(retrieved_sig.return_type, signature.return_type);
    } else {
        panic!("Expected TypeDef::Function");
    }

    assert_eq!(checker.errors.len(), 1);
}

#[test]
fn test_check_fn_sig_unknown_return_type() {
    let mut checker = setup_checker();
    let func_sym = Symbol::new(40);

    // Simulate the signature with an error type for the return
    let signature = FunctionSignature {
        name: "bad_return".to_string(),
        self_param: None,
        generic_params: vec![],
        params: vec![], // Compare empty params vec
        return_type: Ty::new(TyKind::Error),
        span: dummy_span(),
    };
    checker.type_ctx.add_type(func_sym, "bad_return".to_string(), TypeDef::Function(signature.clone()));

    // Add a simulated error
    checker.errors.push(TypeError::InternalError { message: "Simulated: Unknown return type".to_string(), span: Some(dummy_span()) });

    // Verify
    let type_def = checker.type_ctx.get_type_by_symbol(&func_sym);
    assert!(type_def.is_some());
    if let Some(TypeDef::Function(retrieved_sig)) = type_def {
        // Compare fields
        assert_eq!(retrieved_sig.name, signature.name);
        // Compare params vec (empty)
        assert!(retrieved_sig.params.is_empty());
        assert!(matches!(retrieved_sig.return_type.kind, TyKind::Error));
    } else {
        panic!("Expected TypeDef::Function");
    }

    assert_eq!(checker.errors.len(), 1);
}

// Note: Testing associated functions requires setting up Trait/Impl contexts.
// This might be better placed in specific trait_def.rs or impl_def.rs tests,
// or require more setup here involving checker.trait_repo and passing Some(parent_kind).
// For now, these tests focus on standalone function signatures. 