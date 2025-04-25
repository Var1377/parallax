// Contains logic for generic instantiation and generalization

use std::collections::HashMap;
use miette::SourceSpan;

// Removed TODO, updated imports
use crate::context::inference::Substitution;
use crate::error::{TypeError, TypeResult};
use crate::types::{Ty, TyKind, TypeId, GenericParamDef};
use super::TypeChecker; // Updated path

/// Helper function to instantiate a polymorphic type with fresh type variables.
/// Returns the instantiated type and a map from original type IDs to fresh type IDs.
#[allow(dead_code)]
pub(crate) fn instantiate(
    checker: &mut TypeChecker,
    ty: &Ty,
    span: SourceSpan,
    generic_params: &[GenericParamDef],
) -> TypeResult<(Ty, HashMap<TypeId, TypeId>)> {
    let mut subst = Substitution::default();
    let mut generic_to_fresh_map = HashMap::new();

    // Create fresh type variables for each generic parameter
    for param in generic_params {
        let fresh_var = checker.fresh_infer_var(span);
        if let TyKind::Var(id) = fresh_var.kind {
            // Store the mapping from original type ID to fresh type ID
            generic_to_fresh_map.insert(param.id, id);
            subst.insert(param.id, fresh_var);
        } else {
            return Err(TypeError::InternalError {
                message: format!("Expected fresh var to be TyKind::Var, but got {:?}", fresh_var.kind),
                span: Some(span),
            });
        }
    }

    // Apply the substitution to create the instantiated type
    let instantiated_ty = ty.apply_subst(&subst);
    Ok((instantiated_ty, generic_to_fresh_map))
}

// TODO: Implement generalize function (for let bindings)

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        context::{inference::InferenceContext, trait_repo::TraitRepository},
        types::{Ty, TyKind, PrimitiveType, TypeId, GenericParamDef, TraitRef, TypeContext, TypeDef},
        checker::TypeChecker,
        TypeDatabase,
        ResolveDatabase,
        error::TypeResult,
        error::TypeError,
    };
    use parallax_source::SourceDatabase;
    use parallax_syntax::SyntaxDatabase;
    use parallax_resolve::types::{ResolvedDefinitions, Symbol};
    use miette::SourceSpan;
    use salsa::Database;
    use std::collections::HashMap;
    use std::sync::Arc;

    // --- Dummy DB Setup ---
    #[derive(Default)] // Add Default derive for simpler creation
    #[salsa::db]
    pub struct DummyDb {
        storage: salsa::Storage<Self>,
    }

    // Implement Clone manually or derive if storage allows
    impl Clone for DummyDb {
        fn clone(&self) -> Self {
            // This might be incorrect depending on salsa's internal requirements.
            // For testing purposes, a basic clone might suffice.
            // If tests fail due to DB state, a more robust clone or setup is needed.
            DummyDb {
                storage: salsa::Storage::default(), // Create new storage? Or clone existing? Salsa docs needed.
            }
        }
    }

    impl salsa::Database for DummyDb {
        // Add the missing salsa_event method
        fn salsa_event(&self, _event: &dyn Fn() -> salsa::Event) {
            // No-op for dummy implementation
        }
    }
    // Implement required methods for SourceDatabase (assuming it needs Zalsa stuff)
    impl SourceDatabase for DummyDb {
        // Correct Zalsa method signatures
        fn zalsa_register_downcaster(&self) { /* No-op */ }
        unsafe fn downcast<'db>(db: &'db (dyn salsa::Database + 'static)) -> &'db (dyn SourceDatabase + 'static) {
            // Safety: This is unsafe because it relies on the caller providing the correct DB type.
            // For dummy tests, this might be okay, but be cautious.
            // A better approach might involve storing a pointer or using a registry if complex downcasting is needed.
            std::mem::transmute(db)
        }
    }
    // Implement required methods for SyntaxDatabase
    impl SyntaxDatabase for DummyDb {
         fn zalsa_register_downcaster(&self) { /* No-op */ }
         unsafe fn downcast<'db>(db: &'db (dyn salsa::Database + 'static)) -> &'db (dyn SyntaxDatabase + 'static) {
             std::mem::transmute(db)
         }
    }
    // Implement required methods for ResolveDatabase
    impl ResolveDatabase for DummyDb {
         fn zalsa_register_downcaster(&self) { /* No-op */ }
         unsafe fn downcast<'db>(db: &'db (dyn salsa::Database + 'static)) -> &'db (dyn ResolveDatabase + 'static) {
             std::mem::transmute(db)
         }
    }
    // Implement required methods for TypeDatabase
    impl TypeDatabase for DummyDb {
         fn zalsa_register_downcaster(&self) { /* No-op */ }
         unsafe fn downcast<'db>(db: &'db (dyn salsa::Database + 'static)) -> &'db (dyn TypeDatabase + 'static) {
             std::mem::transmute(db)
         }
    }

    fn dummy_span() -> SourceSpan {
        SourceSpan::from((0, 0))
    }

    fn ty_prim(prim: PrimitiveType) -> Ty {
        Ty::with_span(TyKind::Primitive(prim), dummy_span())
    }

    fn ty_var(id: u32) -> Ty {
        Ty::with_span(TyKind::Var(TypeId(id)), dummy_span())
    }

    fn ty_named(name: &str, args: Vec<Ty>) -> Ty {
        Ty::with_span(TyKind::Named { name: name.to_string(), symbol: None, args }, dummy_span())
    }

    // Add missing helper functions
    fn ty_tuple(elems: Vec<Ty>) -> Ty {
        Ty::with_span(TyKind::Tuple(elems), dummy_span())
    }

    fn ty_func(params: Vec<Ty>, ret: Ty) -> Ty {
        Ty::with_span(TyKind::Function(params, Arc::new(ret)), dummy_span())
    }

    fn setup_checker<'db>(db_mock: &'db DummyDb, definitions: &'db ResolvedDefinitions) -> TypeChecker<'db> {
        let type_ctx = TypeContext::new();
        let trait_repo = TraitRepository::new();
        TypeChecker::new(db_mock, definitions, type_ctx, trait_repo)
    }

    #[test]
    fn test_instantiate_no_generics() {
        let db_mock = DummyDb::default();
        let definitions = ResolvedDefinitions::default();
        let mut checker = setup_checker(&db_mock, &definitions);

        let concrete_ty = ty_prim(PrimitiveType::I32);
        let generic_params: Vec<GenericParamDef> = vec![];

        let result = instantiate(&mut checker, &concrete_ty, dummy_span(), &generic_params);
        assert!(result.is_ok());
        let (instantiated_ty, generic_map) = result.unwrap();

        assert_eq!(instantiated_ty, concrete_ty); // Should be unchanged
        assert!(generic_map.is_empty()); // No generics, map should be empty
    }

    #[test]
    fn test_instantiate_simple_generic() {
        let db_mock = DummyDb::default();
        let definitions = ResolvedDefinitions::default();
        let mut checker = setup_checker(&db_mock, &definitions);

        let original_param_id = TypeId(100);
        let generic_ty = ty_var(original_param_id.0); // The generic type T itself
        let generic_params = vec![
            GenericParamDef {
                name: "T".to_string(),
                symbol: Symbol::new(1), // Dummy symbol
                id: original_param_id,
                bounds: vec![],
                span: dummy_span(),
            }
        ];

        let result = instantiate(&mut checker, &generic_ty, dummy_span(), &generic_params);
        assert!(result.is_ok());
        let (instantiated_ty, generic_map) = result.unwrap();

        // Instantiated type should be a fresh variable
        assert!(matches!(instantiated_ty.kind, TyKind::Var(_)), "Expected fresh variable, got {:?}", instantiated_ty.kind);
        let fresh_var_id = match instantiated_ty.kind { TyKind::Var(id) => id, _ => unreachable!() };
        assert_ne!(fresh_var_id, original_param_id); // Ensure it's a *fresh* var

        // Map should contain the mapping from original ID to fresh ID
        assert_eq!(generic_map.len(), 1);
        assert_eq!(generic_map.get(&original_param_id), Some(&fresh_var_id));
    }

    #[test]
    fn test_instantiate_struct_with_generics() {
        let db_mock = DummyDb::default();
        let definitions = ResolvedDefinitions::default();
        let mut checker = setup_checker(&db_mock, &definitions);

        let original_param_id_t = TypeId(100);
        let original_param_id_u = TypeId(101);
        let generic_struct_ty = ty_named("Pair", vec![ty_var(original_param_id_t.0), ty_var(original_param_id_u.0)]);
        let generic_params = vec![
            GenericParamDef {
                name: "T".to_string(),
                symbol: Symbol::new(1),
                id: original_param_id_t,
                bounds: vec![],
                span: dummy_span(),
            },
            GenericParamDef {
                name: "U".to_string(),
                symbol: Symbol::new(2),
                id: original_param_id_u,
                bounds: vec![],
                span: dummy_span(),
            }
        ];

        let result = instantiate(&mut checker, &generic_struct_ty, dummy_span(), &generic_params);
        assert!(result.is_ok());
        let (instantiated_ty, generic_map) = result.unwrap();

        // Instantiated type should be Pair<t_fresh1, t_fresh2>
        assert!(matches!(instantiated_ty.kind, TyKind::Named { ref name, ref args, .. } if name == "Pair" && args.len() == 2));
        let args = match instantiated_ty.kind { TyKind::Named { ref args, .. } => args, _ => unreachable!() };

        assert!(matches!(args[0].kind, TyKind::Var(_)));
        assert!(matches!(args[1].kind, TyKind::Var(_)));
        let fresh_var_id_1 = match args[0].kind { TyKind::Var(id) => id, _ => unreachable!() };
        let fresh_var_id_2 = match args[1].kind { TyKind::Var(id) => id, _ => unreachable!() };

        assert_ne!(fresh_var_id_1, original_param_id_t);
        assert_ne!(fresh_var_id_2, original_param_id_u);
        assert_ne!(fresh_var_id_1, fresh_var_id_2); // Ensure fresh vars are distinct

        // Check map
        assert_eq!(generic_map.len(), 2);
        assert_eq!(generic_map.get(&original_param_id_t), Some(&fresh_var_id_1));
        assert_eq!(generic_map.get(&original_param_id_u), Some(&fresh_var_id_2));
    }

    #[test]
    fn test_instantiate_complex_type() {
        let db_mock = DummyDb::default();
        let definitions = ResolvedDefinitions::default();
        let mut checker = setup_checker(&db_mock, &definitions);

        let original_param_id_k = TypeId(50);
        let original_param_id_v = TypeId(51);
        let complex_ty = ty_func(
            vec![ty_named("Map", vec![ty_var(original_param_id_k.0), ty_var(original_param_id_v.0)])],
            ty_tuple(vec![ty_var(original_param_id_k.0), ty_var(original_param_id_v.0)])
        );

        let generic_params = vec![
            GenericParamDef {
                name: "K".to_string(),
                symbol: Symbol::new(10),
                id: original_param_id_k,
                bounds: vec![], // Assume Hash + Eq checked elsewhere
                span: dummy_span(),
            },
            GenericParamDef {
                name: "V".to_string(),
                symbol: Symbol::new(11),
                id: original_param_id_v,
                bounds: vec![],
                span: dummy_span(),
            }
        ];

        let result = instantiate(&mut checker, &complex_ty, dummy_span(), &generic_params);
        assert!(result.is_ok());
        let (instantiated_ty, generic_map) = result.unwrap();

        // Expected: fn(Map<k_fresh, v_fresh>) -> (k_fresh, v_fresh)
        assert!(matches!(instantiated_ty.kind, TyKind::Function(ref params, ref ret) if params.len() == 1));
        let (params, ret) = match instantiated_ty.kind { TyKind::Function(p, r) => (p, r), _ => unreachable!() };

        // Check param type: Map<k_fresh, v_fresh>
        assert!(matches!(params[0].kind, TyKind::Named { ref name, ref args, .. } if name == "Map" && args.len() == 2));
        let map_args = match params[0].kind { TyKind::Named { ref args, .. } => args, _ => unreachable!() };
        assert!(matches!(map_args[0].kind, TyKind::Var(_)));
        assert!(matches!(map_args[1].kind, TyKind::Var(_)));
        let fresh_k_in_map = match map_args[0].kind { TyKind::Var(id) => id, _ => unreachable!() };
        let fresh_v_in_map = match map_args[1].kind { TyKind::Var(id) => id, _ => unreachable!() };

        // Check return type: (k_fresh, v_fresh)
        assert!(matches!(ret.kind, TyKind::Tuple(ref elems) if elems.len() == 2));
        let tuple_elems = match ret.kind { TyKind::Tuple(ref elems) => elems, _ => unreachable!() };
        assert!(matches!(tuple_elems[0].kind, TyKind::Var(_)));
        assert!(matches!(tuple_elems[1].kind, TyKind::Var(_)));
        let fresh_k_in_ret = match tuple_elems[0].kind { TyKind::Var(id) => id, _ => unreachable!() };
        let fresh_v_in_ret = match tuple_elems[1].kind { TyKind::Var(id) => id, _ => unreachable!() };

        // Ensure the *same* fresh vars were used
        assert_eq!(fresh_k_in_map, fresh_k_in_ret);
        assert_eq!(fresh_v_in_map, fresh_v_in_ret);

        // Check map
        assert_eq!(generic_map.len(), 2);
        assert_eq!(generic_map.get(&original_param_id_k), Some(&fresh_k_in_map));
        assert_eq!(generic_map.get(&original_param_id_v), Some(&fresh_v_in_map));
    }
}