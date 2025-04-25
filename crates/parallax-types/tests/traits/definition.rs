// tests/traits/definition.rs
use crate::*; // Import helpers from tests/mod.rs

// Specific imports for parallax_types items
use parallax_types::{
    error::TypeError,
    types::{FunctionSignature, GenericParamDef, ParamType, SelfParamKind, TraitRef, AssociatedTypeDef, TypeDef, TyKind, PrimitiveType},
    TypedModule,
    TypeDatabase, // Explicitly import trait
};

// Specific imports for parallax_resolve items based on search results
use parallax_resolve::{
    definitions::{DefinitionInfo, DefinitionKind},
    types::{ResolvedTrait, ResolvedFunction, ResolvedGenericParamDef, ResolvedParameter, ResolvedDefinitions, ResolvedModuleStructure, ResolvedImpl, ResolvedStruct, ResolvedEnum, ResolvedType, PrimitiveType as ResolvePrimitiveType, Symbol, ResolvedAssociatedFunction, ResolvedAssociatedType},
    error::{ResolutionError, ResolverWarning},
    ResolveDatabase, // Explicitly import trait
};

use std::collections::HashMap;
use std::sync::Arc;

// TODO: Add tests for check_trait_definition
// - Simple trait definition
// - Trait with associated functions
// - Trait with generic parameters
// - Trait with associated types
// - Trait with bounds on parameters/Self 

// --- Helper Functions --- (Adapted for ResolvedModuleStructure)

// Takes &DummyDb to avoid Sized errors
fn module_with_trait(db: &DummyDb, trait_def: ResolvedTrait) -> ResolvedModuleStructure {
    let mut definitions = ResolvedDefinitions::default();
    definitions.traits.push(trait_def);
    ResolvedModuleStructure::new(
        db as &dyn ResolveDatabase, // Cast to trait object
        definitions,
        None, // entry_point
        vec![], // core_traits
        vec![], // intrinsics
        vec![], // errors
        vec![]  // warnings
    )
}

// Helper to create a ResolvedTrait matching types::ResolvedTrait
fn resolved_trait(
    name: &str,
    symbol: Symbol,
    generic_params: Vec<ResolvedGenericParamDef>,
    methods: Vec<ResolvedAssociatedFunction>,
    associated_types: Vec<ResolvedAssociatedType>,
) -> ResolvedTrait {
    ResolvedTrait {
        symbol,
        name: name.to_string(),
        module_symbol: Symbol::new(0),
        generic_params,
        methods,
        associated_types,
        supertraits: vec![],
        is_public: true,
        span: dummy_span(),
    }
}

// Helper function for creating ResolvedFunction
fn resolved_function(
    name: &str,
    symbol: Symbol,
    generic_params: Vec<ResolvedGenericParamDef>,
    params: Vec<ResolvedParameter>,
    return_type: ResolvedType,
) -> ResolvedFunction {
    ResolvedFunction {
        symbol,
        name: name.to_string(),
        module_symbol: Symbol::new(0), // Dummy
        parameters: params,
        return_type,
        body: None, // No body in trait defs
        generic_params,
        is_public: true,
        span: dummy_span(),
        is_effectful: false,
    }
}

// Helper function for creating ResolvedAssociatedFunction
fn resolved_associated_function(func_symbol: Symbol, trait_method_symbol: Option<Symbol>) -> ResolvedAssociatedFunction {
    ResolvedAssociatedFunction { func_symbol, trait_method_symbol }
}

// Helper function for creating ResolvedAssociatedType
fn resolved_associated_type(name: &str, symbol: Symbol, bounds: Vec<Symbol>) -> ResolvedAssociatedType {
    ResolvedAssociatedType {
        symbol,
        name: name.to_string(),
        bounds, // Expects Vec<Symbol>
        span: dummy_span(),
    }
}

// Helper for generic param defs - takes Vec<Symbol> for bounds now
fn resolved_generic_param(name: &str, bounds: Vec<Symbol>) -> ResolvedGenericParamDef {
    ResolvedGenericParamDef { name: name.to_string(), bounds, span: dummy_span() }
}

// Takes &DummyDb to avoid Sized error
fn check_single_trait(trait_def: ResolvedTrait) -> TypedModule {
    let db_mock = DummyDb::default();
    let module = module_with_trait(&db_mock, trait_def);
    db_mock.type_check_definitions(module)
}

// Takes &DummyDb to avoid Sized error
fn check_defs(definitions: ResolvedDefinitions) -> TypedModule {
     let db_mock = DummyDb::default();
     let module = ResolvedModuleStructure::new(
         &db_mock as &dyn ResolveDatabase,
         definitions,
         None, // entry_point
         vec![], // core_traits
         vec![], // intrinsics
         vec![], // errors
         vec![]  // warnings
     );
    db_mock.type_check_definitions(module)
}

// --- Tests --- (Using check_single_trait or check_defs)

#[test]
fn test_trait_def_simple() {
    let trait_sym = Symbol::new(1);
    let trait_res = resolved_trait("Simple", trait_sym, vec![], vec![], vec![]);
    let typed_module = check_single_trait(trait_res);

    assert!(typed_module.errors.is_empty(), "Errors: {:?}", typed_module.errors);
    let trait_id = typed_module.trait_repo.get_trait_id_by_symbol(trait_sym).expect("Trait ID not found");
    let checker_trait_def = typed_module.trait_repo.get_trait_def_by_symbol(trait_sym).expect("Trait def not found");
    assert_eq!(checker_trait_def.name, "Simple");
    assert!(checker_trait_def.generic_params.is_empty());
    assert!(checker_trait_def.methods.is_empty());
    assert!(checker_trait_def.associated_types.is_empty());
}

#[test]
fn test_trait_def_with_method() {
    let trait_sym = Symbol::new(1);
    let method_sym = Symbol::new(2);
    let param_sym = Symbol::new(3);

    // 1. Create the ResolvedFunction for the method
    let mut params = vec![ResolvedParameter {
        name: "arg".to_string(),
        symbol: param_sym,
        param_type: ResolvedType::Primitive(ResolvePrimitiveType::I32),
        is_variadic: false, has_default: false, span: dummy_span()
    }];
    let self_sym = Symbol::new(999);
    params.insert(0, ResolvedParameter {
        name: "self".to_string(), symbol: self_sym, param_type: ResolvedType::SelfType,
        is_variadic: false, has_default: false, span: dummy_span()
    });
    let func_def = resolved_function(
        "do_thing",
        method_sym,
        vec![],
        params,
        ResolvedType::Primitive(ResolvePrimitiveType::Bool)
    );

    // 2. Create the ResolvedAssociatedFunction
    let assoc_func = resolved_associated_function(method_sym, None);

    // 3. Create the ResolvedTrait
    let trait_res = resolved_trait("HasMethod", trait_sym, vec![], vec![assoc_func], vec![]);

    // 4. Need to add the ResolvedFunction to the definitions used by check_defs
    let mut definitions = ResolvedDefinitions::default();
    definitions.traits.push(trait_res);
    definitions.functions.push(func_def);

    // 5. Use check_defs
    let typed_module = check_defs(definitions);
    assert!(typed_module.errors.is_empty(), "Errors: {:?}", typed_module.errors);

    let checker_trait_def = typed_module.trait_repo.get_trait_def_by_symbol(trait_sym).unwrap();
    assert_eq!(checker_trait_def.methods.len(), 1);
    let method = checker_trait_def.methods.get(&method_sym).unwrap();
    assert_eq!(method.name, "do_thing");
    assert_eq!(method.method_symbol, method_sym);
    assert!(method.signature.self_param.is_some());
    assert_eq!(method.signature.params.len(), 1);
    assert_eq!(method.signature.params[0].name, "arg");
    assert_eq!(method.signature.params[0].ty.kind, TyKind::Primitive(parallax_types::PrimitiveType::I32));
    assert_eq!(method.signature.return_type.kind, TyKind::Primitive(parallax_types::PrimitiveType::Bool));
}

#[test]
fn test_trait_def_with_generic_param() {
    let trait_sym = Symbol::new(1);
    let trait_res = resolved_trait(
        "GenericTrait",
        trait_sym,
        vec![resolved_generic_param("T", vec![])], // Pass empty Vec<Symbol> for bounds
        vec![],
        vec![],
    );
    let typed_module = check_single_trait(trait_res);
    assert!(typed_module.errors.is_empty(), "Errors: {:?}", typed_module.errors);

    let checker_trait_def = typed_module.trait_repo.get_trait_def_by_symbol(trait_sym).unwrap();
    assert_eq!(checker_trait_def.generic_params.len(), 1);
    assert_eq!(checker_trait_def.generic_params[0].name, "T");
    assert!(checker_trait_def.generic_params[0].bounds.is_empty());
}

#[test]
fn test_trait_def_with_associated_type() {
    let trait_sym = Symbol::new(1);
    let assoc_ty_sym = Symbol::new(5);

    let assoc_ty = resolved_associated_type("Item", assoc_ty_sym, vec![]);
    let trait_res = resolved_trait(
        "Iterator",
        trait_sym,
        vec![],
        vec![],
        vec![assoc_ty],
    );
    let typed_module = check_single_trait(trait_res);
    assert!(typed_module.errors.is_empty(), "Errors: {:?}", typed_module.errors);

    let checker_trait_def = typed_module.trait_repo.get_trait_def_by_symbol(trait_sym).unwrap();
    assert_eq!(checker_trait_def.associated_types.len(), 1);
    let checker_assoc_ty = checker_trait_def.associated_types.get(&assoc_ty_sym).unwrap();
    assert_eq!(checker_assoc_ty.name, "Item");
    assert_eq!(checker_assoc_ty.symbol, assoc_ty_sym);
    assert!(checker_assoc_ty.bounds.is_empty());
}

#[test]
fn test_trait_def_assoc_type_with_bounds() {
    let main_trait_sym = Symbol::new(1);
    let assoc_ty_sym = Symbol::new(5);
    let bound_trait_sym = Symbol::new(20);

    let bound_trait_res = resolved_trait("Debug", bound_trait_sym, vec![], vec![], vec![]);
    let assoc_ty = resolved_associated_type("Element", assoc_ty_sym, vec![bound_trait_sym]);
    let main_trait_res = resolved_trait(
        "Container",
        main_trait_sym,
        vec![],
        vec![],
        vec![assoc_ty],
    );

    let mut definitions = ResolvedDefinitions::default();
    definitions.traits.push(bound_trait_res);
    definitions.traits.push(main_trait_res);
    let typed_module = check_defs(definitions);

    assert!(typed_module.errors.is_empty(), "Errors: {:?}", typed_module.errors);

    let bound_trait_id = typed_module.trait_repo.get_trait_id_by_symbol(bound_trait_sym).expect("Bound trait ID not found");
    let main_checker_trait_def = typed_module.trait_repo.get_trait_def_by_symbol(main_trait_sym).unwrap();
    let checker_assoc_ty = main_checker_trait_def.associated_types.get(&assoc_ty_sym).unwrap();
    assert_eq!(checker_assoc_ty.bounds.len(), 1);
    assert_eq!(checker_assoc_ty.bounds[0].trait_id, bound_trait_id);
    assert!(checker_assoc_ty.bounds[0].type_arguments.is_empty());
}

#[test]
fn test_trait_def_generic_param_with_bounds() {
    let main_trait_sym = Symbol::new(1);
    let bound_trait_sym = Symbol::new(20);

    let bound_trait_res = resolved_trait("Clone", bound_trait_sym, vec![], vec![], vec![]);
    let main_trait_res = resolved_trait(
        "GenericTraitBound",
        main_trait_sym,
        vec![resolved_generic_param("T", vec![bound_trait_sym])], // Pass bound symbol directly
        vec![],
        vec![],
    );

    let mut definitions = ResolvedDefinitions::default();
    definitions.traits.push(bound_trait_res);
    definitions.traits.push(main_trait_res);
    let typed_module = check_defs(definitions);

    assert!(typed_module.errors.is_empty(), "Errors: {:?}", typed_module.errors);

    let bound_trait_id = typed_module.trait_repo.get_trait_id_by_symbol(bound_trait_sym).expect("Bound trait ID not found");
    let main_checker_trait_def = typed_module.trait_repo.get_trait_def_by_symbol(main_trait_sym).unwrap();
    assert_eq!(main_checker_trait_def.generic_params.len(), 1);
    assert_eq!(main_checker_trait_def.generic_params[0].name, "T");
    assert_eq!(main_checker_trait_def.generic_params[0].bounds.len(), 1);
    assert_eq!(main_checker_trait_def.generic_params[0].bounds[0].trait_id, bound_trait_id);
}

#[test]
fn test_trait_def_self_bound() {
    let trait_sym = Symbol::new(1);
    let method_sym = Symbol::new(2);
    let other_param_sym = Symbol::new(3);

    // 1. Create ResolvedFunction for the method
    let mut params = vec![ResolvedParameter {
        name: "other".to_string(), symbol: other_param_sym, param_type: ResolvedType::SelfType,
        is_variadic: false, has_default: false, span: dummy_span()
    }];
    let self_sym = Symbol::new(999);
    params.insert(0, ResolvedParameter {
        name: "self".to_string(), symbol: self_sym, param_type: ResolvedType::SelfType,
        is_variadic: false, has_default: false, span: dummy_span()
    });
    let func_def = resolved_function("eq", method_sym, vec![], params, ResolvedType::Primitive(ResolvePrimitiveType::Bool));

    // 2. Create ResolvedAssociatedFunction
    let assoc_func = resolved_associated_function(method_sym, None);

    // 3. Create ResolvedTrait
    let trait_res = resolved_trait("MyEq", trait_sym, vec![], vec![assoc_func], vec![]);

    // 4. Add definitions
    let mut definitions = ResolvedDefinitions::default();
    definitions.traits.push(trait_res);
    definitions.functions.push(func_def);

    // 5. Check
    let typed_module = check_defs(definitions);
    assert!(typed_module.errors.is_empty(), "Errors: {:?}", typed_module.errors);

    let checker_trait_def = typed_module.trait_repo.get_trait_def_by_symbol(trait_sym).unwrap();
    let method = checker_trait_def.methods.get(&method_sym).unwrap();
    assert_eq!(method.signature.params.len(), 1);
    assert_eq!(method.signature.params[0].name, "other");
    assert!(matches!(method.signature.params[0].ty.kind, TyKind::SelfType));
    assert!(method.signature.self_param.is_some());
}

#[test]
fn test_trait_def_error_unknown_param_type() {
    let trait_sym = Symbol::new(1);
    let method_sym = Symbol::new(2);
    let param_sym = Symbol::new(3);
    let unknown_type_sym = Symbol::new(99);

    // 1. Create ResolvedFunction with unknown type
    let params = vec![ResolvedParameter {
        name: "arg".to_string(),
        symbol: param_sym,
        param_type: ResolvedType::UserDefined { symbol: unknown_type_sym, type_args: Some(vec![]) },
        is_variadic: false, has_default: false, span: dummy_span()
    }];
    let func_def = resolved_function("bad", method_sym, vec![], params, ResolvedType::Primitive(ResolvePrimitiveType::Bool));

    // 2. Create ResolvedAssociatedFunction
    let assoc_func = resolved_associated_function(method_sym, None);

    // 3. Create ResolvedTrait
    let trait_res = resolved_trait("BadMethod", trait_sym, vec![], vec![assoc_func], vec![]);

    // 4. Add definitions
    let mut definitions = ResolvedDefinitions::default();
    definitions.traits.push(trait_res);
    definitions.functions.push(func_def);

    // 5. Check
    let typed_module = check_defs(definitions);

    assert!(!typed_module.errors.is_empty(), "Expected an error for unknown type");
    println!("Errors: {:?}", typed_module.errors);
    assert!(typed_module.errors.iter().any(|e| e.contains("Unknown identifier") || e.contains(&format!("{:?}", unknown_type_sym))));
}

#[test]
fn test_trait_def_error_unknown_bound_trait() {
    let main_trait_sym = Symbol::new(1);
    let unknown_bound_trait_sym = Symbol::new(98);

    let trait_res = resolved_trait(
        "BadBound",
        main_trait_sym,
        vec![resolved_generic_param("T", vec![unknown_bound_trait_sym])], // Pass unknown symbol
        vec![],
        vec![],
    );
    let typed_module = check_single_trait(trait_res);

    assert!(!typed_module.errors.is_empty(), "Expected an error for unknown bound trait");
    println!("Errors: {:?}", typed_module.errors);
    assert!(typed_module.errors.iter().any(|e| e.contains("Unknown trait") || e.contains(&format!("{:?}", unknown_bound_trait_sym))));
} 