// tests/definitions/trait_def.rs
use crate::*; // Import helpers from tests/mod.rs
use parallax_types::{
    error::TypeError,
    context::trait_repo::{TraitId, TraitRepository}, // Import TraitRepository
    // Import SelfParamKind directly
    types::{PrimitiveType, Ty, TyKind, TypeId, TypeDef, FunctionSignature, ParamType, TraitDef, TraitMethod, AssociatedTypeDef, GenericParamDef as CheckerGenericParamDef, TraitRef, SelfParamKind},
};
use parallax_resolve::{
    types::{ResolvedType, Symbol, ResolvedAssociatedFunction, ResolvedAssociatedType, ResolvedTrait, ResolvedFunction, ResolvedGenericParamDef},
    PrimitiveType as ResolvePrimitiveType,
};
use std::collections::HashMap;

// Helper to create a ResolvedAssociatedFunction (minimal)
fn resolved_assoc_fn(func_symbol: Symbol) -> ResolvedAssociatedFunction {
    ResolvedAssociatedFunction {
        func_symbol,
        trait_method_symbol: None, // Not relevant for trait def itself
    }
}

// Helper to create a ResolvedAssociatedType (minimal)
fn resolved_assoc_type(name: &str, symbol: Symbol) -> ResolvedAssociatedType {
    ResolvedAssociatedType {
        name: name.to_string(),
        symbol,
        bounds: vec![], // Add bounds if needed for tests
        span: dummy_span(),
    }
}

// Helper to create ResolvedTrait - CANNOT use this for generic test anymore
// fn resolved_trait(
//     name: &str,
//     symbol: Symbol,
//     generic_params: Vec<ResolvedGenericParamDef>,
//     methods: Vec<ResolvedAssociatedFunction>,
//     associated_types: Vec<ResolvedAssociatedType>,
//     supertraits: Vec<Symbol>, // Symbols of supertraits
// ) -> ResolvedTrait {
//     ResolvedTrait {
//         name: name.to_string(),
//         symbol,
//         module_symbol: Symbol::new(0), // Dummy
//         is_public: true,
//         generic_params,
//         methods,
//         associated_types,
//         supertraits,
//         span: dummy_span(),
//     }
// }

#[test]
fn test_check_trait_def_simple() {
    let mut checker = setup_checker();
    let trait_sym = Symbol::new(1);

    // Simulate adding the trait def
    let trait_id = checker.trait_repo.next_trait_id();
    let trait_def = TraitDef {
        id: trait_id,
        trait_symbol: trait_sym,
        name: "MyTrait".to_string(),
        generic_params: vec![],
        methods: HashMap::new(),
        associated_types: HashMap::new(),
        span: dummy_span(),
    };
    checker.trait_repo.add_trait(trait_def.clone()); // Add to repo
    checker.type_ctx.add_trait_symbol(trait_sym, "MyTrait".to_string(), trait_id); // Add symbol mapping

    // Verify TraitDef in repo
    let retrieved_trait_id = checker.trait_repo.get_trait_id_by_symbol(trait_sym).unwrap();
    assert_eq!(retrieved_trait_id, trait_id);
    let retrieved_trait_def = checker.trait_repo.traits.get(&trait_id);
    assert!(retrieved_trait_def.is_some(), "TraitDef for MyTrait not found");

    if let Some(def) = retrieved_trait_def {
        assert_eq!(def.name, "MyTrait");
        assert_eq!(def.trait_symbol, trait_sym);
        assert!(def.generic_params.is_empty());
        assert!(def.methods.is_empty());
        assert!(def.associated_types.is_empty());
    }

    assert!(checker.errors.is_empty());
}

#[test]
fn test_check_trait_def_with_method() {
    let mut checker = setup_checker();
    let trait_sym = Symbol::new(10);
    let method_sym = Symbol::new(11);

    // Pre-add the function signature
    let method_sig = FunctionSignature {
        name: "do_thing".to_string(),
        self_param: Some(SelfParamKind::Value), // Use directly qualified SelfParamKind
        generic_params: vec![],
        params: vec![ParamType { name: "x".to_string(), ty: ty_prim(PrimitiveType::Bool), span: dummy_span() }],
        return_type: ty_prim(PrimitiveType::Unit),
        span: dummy_span(),
    };
    checker.type_ctx.add_type(method_sym, "do_thing".to_string(), TypeDef::Function(method_sig.clone()));

    // Simulate adding the trait def
    let trait_id = checker.trait_repo.next_trait_id();
    let trait_method = TraitMethod {
        name: "do_thing".to_string(),
        method_symbol: method_sym,
        signature: method_sig.clone(), // Store the signature
    };
    let trait_def = TraitDef {
        id: trait_id,
        trait_symbol: trait_sym,
        name: "Doer".to_string(),
        generic_params: vec![],
        methods: HashMap::from([(method_sym, trait_method)]),
        associated_types: HashMap::new(),
        span: dummy_span(),
    };
    checker.trait_repo.add_trait(trait_def.clone());
    checker.type_ctx.add_trait_symbol(trait_sym, "Doer".to_string(), trait_id);

    // Verify
    let retrieved_trait_id = checker.trait_repo.get_trait_id_by_symbol(trait_sym).unwrap();
    let retrieved_trait_def = checker.trait_repo.traits.get(&retrieved_trait_id);
    assert!(retrieved_trait_def.is_some());

    if let Some(def) = retrieved_trait_def {
        assert_eq!(def.methods.len(), 1);
        assert!(def.methods.contains_key(&method_sym));
        let stored_method = def.methods.get(&method_sym).unwrap();
        assert_eq!(stored_method.name, "do_thing");
        assert_eq!(stored_method.method_symbol, method_sym);
        // Compare relevant fields of the signature
        assert_eq!(stored_method.signature.name, method_sig.name);
        // Compare ParamType vec element by element
        assert_eq!(stored_method.signature.params.len(), method_sig.params.len());
        for (retrieved, expected) in stored_method.signature.params.iter().zip(method_sig.params.iter()) {
             assert_eq!(retrieved.name, expected.name);
             assert_eq!(retrieved.ty, expected.ty);
        }
        assert_eq!(stored_method.signature.return_type, method_sig.return_type);
    }

    assert!(checker.errors.is_empty());
}

#[test]
fn test_check_trait_def_with_assoc_type() {
    let mut checker = setup_checker();
    let trait_sym = Symbol::new(20);
    let assoc_type_sym = Symbol::new(21);

    // Simulate adding the trait def
    let trait_id = checker.trait_repo.next_trait_id();
    let assoc_type_def = AssociatedTypeDef {
        name: "Item".to_string(),
        symbol: assoc_type_sym,
        bounds: vec![],
        default: None,
        span: dummy_span(),
    };
    let trait_def = TraitDef {
        id: trait_id,
        trait_symbol: trait_sym,
        name: "Iterable".to_string(),
        generic_params: vec![],
        methods: HashMap::new(),
        associated_types: HashMap::from([(assoc_type_sym, assoc_type_def)]),
        span: dummy_span(),
    };
    checker.trait_repo.add_trait(trait_def.clone());
    checker.type_ctx.add_trait_symbol(trait_sym, "Iterable".to_string(), trait_id);

    // Verify
    let retrieved_trait_id = checker.trait_repo.get_trait_id_by_symbol(trait_sym).unwrap();
    let retrieved_trait_def = checker.trait_repo.traits.get(&retrieved_trait_id);
    assert!(retrieved_trait_def.is_some());

    if let Some(def) = retrieved_trait_def {
        assert_eq!(def.associated_types.len(), 1);
        assert!(def.associated_types.contains_key(&assoc_type_sym));
        let stored_assoc_type = def.associated_types.get(&assoc_type_sym).unwrap();
        assert_eq!(stored_assoc_type.name, "Item");
        assert_eq!(stored_assoc_type.symbol, assoc_type_sym);
        assert!(stored_assoc_type.bounds.is_empty());
    }

    assert!(checker.errors.is_empty());
}

#[test]
fn test_check_trait_def_generic() {
    let mut checker = setup_checker();
    let trait_sym = Symbol::new(30);
    let gen_param_t_sym = Symbol::new(31);
    let method_sym = Symbol::new(32);

    // Get TypeId for T
    let gen_param_id = match checker.inference_ctx.fresh_var().kind {
        TyKind::Var(id) => id,
        _ => panic!("Fresh var was not TyKind::Var"),
    };
    let checker_gen_param = CheckerGenericParamDef { name: "T".to_string(), symbol: gen_param_t_sym, id: gen_param_id, bounds: vec![], span: dummy_span() };

    // Pre-add generic method signature
    let method_sig = FunctionSignature {
        name: "process".to_string(),
        self_param: None, // Assuming static method for simplicity
        generic_params: vec![], // Generic param belongs to trait, not duplicated here usually
        params: vec![ParamType { name: "input".to_string(), ty: ty_var(gen_param_id.0), span: dummy_span() }], // input: T
        return_type: ty_var(gen_param_id.0), // returns T
        span: dummy_span(),
    };
    checker.type_ctx.add_type(method_sym, "process".to_string(), TypeDef::Function(method_sig.clone()));

    // Simulate adding the generic trait def
    let trait_id = checker.trait_repo.next_trait_id();
    let trait_method = TraitMethod {
        name: "process".to_string(),
        method_symbol: method_sym,
        signature: method_sig.clone(),
    };
    let trait_def = TraitDef {
        id: trait_id,
        trait_symbol: trait_sym,
        name: "Processor".to_string(),
        generic_params: vec![checker_gen_param.clone()], // trait Processor<T>
        methods: HashMap::from([(method_sym, trait_method)]),
        associated_types: HashMap::new(),
        span: dummy_span(),
    };
    checker.trait_repo.add_trait(trait_def.clone());
    checker.type_ctx.add_trait_symbol(trait_sym, "Processor".to_string(), trait_id);

    // Verify
    let retrieved_trait_id = checker.trait_repo.get_trait_id_by_symbol(trait_sym).unwrap();
    let retrieved_trait_def = checker.trait_repo.traits.get(&retrieved_trait_id);
    assert!(retrieved_trait_def.is_some());

    if let Some(def) = retrieved_trait_def {
        assert_eq!(def.generic_params.len(), 1);
        assert_eq!(def.generic_params[0].name, "T");
        assert_eq!(def.generic_params[0].symbol, gen_param_t_sym);
        assert_eq!(def.generic_params[0].id, gen_param_id);

        assert!(def.methods.contains_key(&method_sym));
        let stored_method = def.methods.get(&method_sym).unwrap();
        assert_eq!(stored_method.signature.name, method_sig.name);
        // Method sig itself doesn't repeat the trait generic usually
        assert!(stored_method.signature.generic_params.is_empty());
        assert_eq!(stored_method.signature.params.len(), 1);
        // Param type should refer to the generic param ID T
        assert_eq!(stored_method.signature.params[0].ty, ty_var(gen_param_id.0));
        assert_eq!(stored_method.signature.return_type, ty_var(gen_param_id.0));
    }

    assert!(checker.errors.is_empty());
}

// TODO: Add test_check_trait_def_with_supertraits (requires setting up supertrait defs first)
// TODO: Add test_check_trait_def_method_unknown_type