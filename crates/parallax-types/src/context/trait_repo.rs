use std::collections::{HashMap, HashSet};
use serde::{Deserialize, Serialize};

use crate::context::inference::InferenceContext;
// Use the checker-internal types defined in types.rs
pub use crate::types::{Ty, TypedDefinitions, TypedFunction, TraitDef as CheckerTraitDef, TraitMethod as CheckerTraitMethod, ImplDef as CheckerImplDef, GenericParamDef, ParamType, TraitRef};
// Import TypedFunction specifically

use parallax_resolve::types::Symbol;

/// A unique identifier for a trait
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TraitId(pub u32);

/// A unique identifier for a trait implementation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ImplId(pub u32);

// Placeholder Trait IDs (replace with actual mechanism if possible)
// REMOVED: const ADD_TRAIT_ID: TraitId = TraitId(0);
// REMOVED: const SUB_TRAIT_ID: TraitId = TraitId(1);
// REMOVED: const MUL_TRAIT_ID: TraitId = TraitId(2);
// REMOVED: const DIV_TRAIT_ID: TraitId = TraitId(3);
// REMOVED: const EQ_TRAIT_ID: TraitId = TraitId(4);
// REMOVED: const PARTIAL_ORD_TRAIT_ID: TraitId = TraitId(5);
// REMOVED: const REM_TRAIT_ID: TraitId = TraitId(6);
// REMOVED: const BITAND_TRAIT_ID: TraitId = TraitId(7);
// REMOVED: const BITOR_TRAIT_ID: TraitId = TraitId(8);
// REMOVED: const BITXOR_TRAIT_ID: TraitId = TraitId(9);
// REMOVED: const SHL_TRAIT_ID: TraitId = TraitId(10);
// REMOVED: const SHR_TRAIT_ID: TraitId = TraitId(11);
// REMOVED: const NEG_TRAIT_ID: TraitId = TraitId(12);
// REMOVED: const NOT_TRAIT_ID: TraitId = TraitId(13);

// Use a getter function for OUTPUT_ASSOC_TYPE_SYMBOL instead of a const
pub fn output_assoc_type_symbol() -> Symbol {
    Symbol::new(3000)
}

/// A repository of traits and their implementations
#[derive(Debug, Clone, Default)]
pub struct TraitRepository {
    /// All trait definitions (maps TraitId -> TraitDef from types.rs)
    pub traits: HashMap<TraitId, CheckerTraitDef>,
    /// All trait implementations (maps ImplId -> ImplDef from types.rs)
    pub impls: HashMap<ImplId, CheckerImplDef>,
    /// Map from trait ID to implementations of that trait
    pub trait_impls: HashMap<TraitId, HashSet<ImplId>>,
    /// Map from Trait Symbol to TraitId (for quick lookup during checking)
    trait_symbol_to_id: HashMap<Symbol, TraitId>,
    /// Map from Impl Symbol to ImplId
    symbol_to_impl_id: HashMap<Symbol, ImplId>,
    /// Next available TraitId
    next_trait_id: u32,
    /// Next available ImplId
    next_impl_id: u32,
}

impl TraitRepository {
    /// Create a new trait repository
    pub fn new() -> Self {
        Self {
            traits: HashMap::new(),
            impls: HashMap::new(),
            trait_impls: HashMap::new(),
            trait_symbol_to_id: HashMap::new(),
            symbol_to_impl_id: HashMap::new(),
            next_trait_id: 0,
            next_impl_id: 0,
        }
    }

    /// Get the next available TraitId
    pub fn next_trait_id(&mut self) -> TraitId {
        let id = TraitId(self.next_trait_id);
        self.next_trait_id += 1;
        id
    }

    /// Get the next available ImplId
    pub fn next_impl_id(&mut self) -> ImplId {
        let id = ImplId(self.next_impl_id);
        self.next_impl_id += 1;
        id
    }

    /// Add a trait definition to the repository
    pub fn add_trait(&mut self, trait_def: CheckerTraitDef) {
        // Ensure ID is assigned (caller should do this via next_trait_id)
        assert!(trait_def.id.0 != u32::MAX, "TraitDef ID not assigned before adding");
        let trait_id = trait_def.id;
        let trait_symbol = trait_def.trait_symbol;

        self.trait_symbol_to_id.insert(trait_symbol, trait_id);
        self.traits.insert(trait_id, trait_def);
        self.trait_impls.entry(trait_id).or_default(); // Ensure entry exists
    }

    /// Lookup TraitId by its original Symbol
    pub fn get_trait_id_by_symbol(&self, symbol: Symbol) -> Option<TraitId> {
        self.trait_symbol_to_id.get(&symbol).cloned()
    }

    /// Lookup TraitDef by its original Symbol
    pub fn get_trait_def_by_symbol(&self, symbol: Symbol) -> Option<&CheckerTraitDef> {
         self.get_trait_id_by_symbol(symbol)
             .and_then(|id| self.traits.get(&id))
    }

    /// Add a trait implementation to the repository
    pub fn add_impl(&mut self, mut impl_def: CheckerImplDef) {
        let impl_symbol = impl_def.impl_symbol; // Get symbol before moving

        // Assign the next ID
        impl_def.id = self.next_impl_id();
        let impl_id = impl_def.id;

        // Add to symbol mapping
        self.symbol_to_impl_id.insert(impl_symbol, impl_id);

        // Add to the trait_impls map only if it's implementing a trait
        if let Some(trait_ref) = &impl_def.trait_ref {
           self.trait_impls.entry(trait_ref.trait_id).or_default().insert(impl_id);
        }

        // Store the impl definition itself
        self.impls.insert(impl_id, impl_def);
    }

    /// Lookup ImplDef by its original Symbol
    pub fn get_impl_def_by_symbol(&self, symbol: Symbol) -> Option<&CheckerImplDef> {
        self.symbol_to_impl_id.get(&symbol)
            .and_then(|id| self.impls.get(id))
    }

    /// Find implementations of a specific trait (by ID) for a given type
    pub fn find_impls_for_type(&self, trait_id: TraitId, ty: &Ty, inference_ctx: &mut InferenceContext) -> Vec<&CheckerImplDef> {
        if let Some(impl_ids) = self.trait_impls.get(&trait_id) {
            impl_ids.iter()
                .filter_map(|impl_id| self.impls.get(impl_id))
                // Use unification check instead of direct equality (placeholder)
                .filter(|impl_def| self.types_potentially_match(&impl_def.implementing_type, ty, inference_ctx))
                .collect()
        } else {
            Vec::new()
        }
    }

    // Placeholder for unification check - replace with actual call to inference context
    fn types_potentially_match(&self, ty1: &Ty, ty2: &Ty, inference_ctx: &mut InferenceContext) -> bool {
        // Use snapshot/rollback to avoid persistent side effects from unification check
        let snapshot = inference_ctx.snapshot();
        let result = inference_ctx.unify(ty1, ty2).is_ok();
        inference_ctx.rollback(snapshot);
        result
    }

    /// Checks if two types are structurally compatible for impl candidate selection.
    /// This is stricter than unification: it doesn't allow primitive coercion.
    /// It mainly checks if the base types match (e.g., both are I32, or both are the same struct/enum)
    /// and allows for unification only on generic arguments.
    fn types_structurally_match_for_impl(
         &self, 
         impl_ty: &Ty, 
         receiver_ty: &Ty, 
         inference_ctx: &mut InferenceContext
     ) -> bool {
         let snapshot = inference_ctx.snapshot(); // Snapshot to isolate unification check
 
         // Use a helper function to perform the recursive check
         let result = Self::check_structural_match_recursive(impl_ty, receiver_ty, inference_ctx, 0);
 
         inference_ctx.rollback(snapshot); // Rollback any temporary bindings
         result
     }

    // Recursive helper for structural matching
    fn check_structural_match_recursive(
        impl_ty: &Ty, 
        receiver_ty: &Ty, 
        inference_ctx: &mut InferenceContext, 
        depth: u32
    ) -> bool {
        if depth > 10 { return false; } // Recursion guard

        let resolved_impl_ty = inference_ctx.resolve_type(impl_ty);
        let resolved_receiver_ty = inference_ctx.resolve_type(receiver_ty);

        use crate::types::TyKind::*;
        match (&resolved_impl_ty.kind, &resolved_receiver_ty.kind) {
            (Var(_), _) | (_, Var(_)) => {
                // Allow variables to unify, this is needed for generic impls like `impl<T> Add for T` (hypothetically)
                // The actual unification happens temporarily.
                inference_ctx.unify(&resolved_impl_ty, &resolved_receiver_ty).is_ok()
            }
            (Primitive(p1), Primitive(p2)) => {
                // Primitives must match exactly, no coercion allowed here.
                p1 == p2
            }
            (Named { symbol: s1, args: args1, .. }, Named { symbol: s2, args: args2, .. }) => {
                // Named types must have the same symbol and structurally matching arguments.
                s1 == s2 && 
                args1.len() == args2.len() &&
                args1.iter().zip(args2.iter()).all(|(a1, a2)| {
                    Self::check_structural_match_recursive(a1, a2, inference_ctx, depth + 1)
                })
            }
            (Array(e1, size1), Array(e2, size2)) => {
                // Arrays must have same size and structurally matching element types.
                size1 == size2 && Self::check_structural_match_recursive(e1, e2, inference_ctx, depth + 1)
            }
            (Tuple(t1), Tuple(t2)) => {
                // Tuples must have same arity and structurally matching elements.
                t1.len() == t2.len() &&
                t1.iter().zip(t2.iter()).all(|(el1, el2)| {
                    Self::check_structural_match_recursive(el1, el2, inference_ctx, depth + 1)
                })
            }
            (Function(p1, r1), Function(p2, r2)) => {
                // Functions must have same arity and structurally matching params/return types.
                p1.len() == p2.len() &&
                p1.iter().zip(p2.iter()).all(|(param1, param2)| {
                    Self::check_structural_match_recursive(param1, param2, inference_ctx, depth + 1)
                }) &&
                Self::check_structural_match_recursive(r1, r2, inference_ctx, depth + 1)
            }
            // TODO: Add cases for Map, Set if they can have impls.

            // Different kinds don't match structurally.
            _ => false,
        }
    }

    /// Find a method definition within a specific trait (by ID), searching by Symbol
    pub fn find_method_by_symbol(&self, trait_id: TraitId, method_symbol: Symbol) -> Option<&CheckerTraitMethod> {
        self.traits.get(&trait_id)
            .and_then(|trait_def| trait_def.methods.get(&method_symbol))
    }

     /// Find a method definition within a specific trait (by ID), searching by name
     pub fn find_method_by_name(&self, trait_id: TraitId, method_name: &str) -> Option<&CheckerTraitMethod> {
         self.traits.get(&trait_id)
             .and_then(|trait_def| {
                 trait_def.methods.values().find(|method| method.name == method_name)
             })
     }

    /// Find implementations that provide a method with the given name and whose implementing type
    /// *might* match the receiver type (using placeholder unification).
    /// Returns Vec<(ImplId, TraitMethodSymbol, TraitId)>
    pub fn find_implementations_providing_method(
        &self, 
        receiver_ty: &Ty, 
        method_name: &str, 
        inference_ctx: &mut InferenceContext
    ) -> Vec<(ImplId, Symbol, TraitId)> {
        let mut results = Vec::new();
        // Iterate through all known implementations
        for (impl_id, impl_def) in &self.impls {
            // Only consider trait implementations
            if let Some(trait_ref) = &impl_def.trait_ref {
                let trait_id = trait_ref.trait_id;
                // Get the corresponding trait definition
                if let Some(trait_def) = self.traits.get(&trait_id) {
                    // Check if this trait defines a method with the required name
                    if let Some(trait_method) = trait_def.methods.values().find(|m| m.name == method_name) {
                        // Check if the implementing type *might* match (using very basic placeholder)
                        if self.types_structurally_match_for_impl(&impl_def.implementing_type, receiver_ty, inference_ctx) {
                             results.push((*impl_id, trait_method.method_symbol, trait_id));
                        }
                    }
                }
            }
        }
        results
    }
}

#[cfg(test)]
mod tests {
    use super::*; // Import items from parent module
    use crate::types::{Ty, TyKind, PrimitiveType, TraitRef, FunctionSignature, TraitMethod, ImplDef, TraitDef};
    use parallax_resolve::types::Symbol;
    use miette::SourceSpan;
    use std::collections::HashMap;

    // Helper for dummy span
    fn dummy_span() -> SourceSpan {
        SourceSpan::from((0, 0))
    }

    // Helper for primitive type
    fn ty_prim(prim: PrimitiveType) -> Ty {
        Ty::new(TyKind::Primitive(prim))
    }

    // Helper to create a dummy trait def
    fn dummy_trait_def(repo: &mut TraitRepository, name: &str, symbol: Symbol) -> TraitDef {
        let trait_id = repo.next_trait_id();
        TraitDef {
            id: trait_id,
            trait_symbol: symbol,
            name: name.to_string(),
            generic_params: vec![],
            methods: HashMap::new(),
            associated_types: HashMap::new(),
            span: dummy_span(),
        }
    }

     // Helper to create a dummy impl def
     fn dummy_impl_def(
        repo: &mut TraitRepository,
        impl_symbol: Symbol,
        trait_id: Option<TraitId>,
        impl_ty: Ty
    ) -> ImplDef {
         let trait_ref = trait_id.map(|tid| TraitRef {
             trait_id: tid,
             type_arguments: vec![],
             span: dummy_span(),
         });
         // Assign ID later in add_impl
         ImplDef {
            id: ImplId(u32::MAX), // Placeholder ID
            impl_symbol,
            trait_ref,
            implementing_type: impl_ty,
            generic_params: vec![],
            methods: HashMap::new(),
            associated_type_bindings: HashMap::new(),
            span: dummy_span(),
         }
     }

    #[test]
    fn test_add_and_get_trait() {
        let mut repo = TraitRepository::new();
        let trait_symbol = Symbol::new(10);
        let trait_def = dummy_trait_def(&mut repo, "MyTrait", trait_symbol);
        let trait_id = trait_def.id;

        repo.add_trait(trait_def.clone());

        assert_eq!(repo.traits.len(), 1);
        assert_eq!(repo.trait_symbol_to_id.len(), 1);
        assert!(repo.trait_impls.contains_key(&trait_id));

        assert_eq!(repo.get_trait_id_by_symbol(trait_symbol), Some(trait_id));
        assert!(repo.get_trait_def_by_symbol(trait_symbol).is_some());
        assert_eq!(repo.get_trait_def_by_symbol(trait_symbol).unwrap().id, trait_id);
        assert_eq!(repo.get_trait_def_by_symbol(trait_symbol).unwrap().name, "MyTrait");
        assert_eq!(repo.get_trait_id_by_symbol(Symbol::new(99)), None);
        assert!(repo.get_trait_def_by_symbol(Symbol::new(99)).is_none());
    }

    #[test]
    fn test_add_and_find_impl() { // Reverted test name
        let mut repo = TraitRepository::new();
        let trait_symbol = Symbol::new(10);
        let trait_def = dummy_trait_def(&mut repo, "MyTrait", trait_symbol);
        let trait_id = trait_def.id;
        repo.add_trait(trait_def);

        let impl_symbol = Symbol::new(20);
        let i32_ty = ty_prim(PrimitiveType::I32);
        let impl_def = dummy_impl_def(&mut repo, impl_symbol, Some(trait_id), i32_ty.clone());

        repo.add_impl(impl_def.clone());
        let assigned_impl_id = ImplId(0); // First impl gets ID 0

        assert_eq!(repo.impls.len(), 1);
        assert!(repo.impls.contains_key(&assigned_impl_id));
        assert_eq!(repo.trait_impls.get(&trait_id).map(|s| s.len()), Some(1));
        assert!(repo.trait_impls.get(&trait_id).map_or(false, |s| s.contains(&assigned_impl_id)));

        // find_impls_for_type uses strict equality placeholder currently
        let found_impls = repo.find_impls_for_type(trait_id, &i32_ty, &mut InferenceContext::new());
        assert_eq!(found_impls.len(), 1);
        assert_eq!(found_impls[0].id, assigned_impl_id);

        let bool_ty = ty_prim(PrimitiveType::Bool);
        let found_impls_bool = repo.find_impls_for_type(trait_id, &bool_ty, &mut InferenceContext::new());
        assert!(found_impls_bool.is_empty());

        let unknown_trait_id = TraitId(99);
        let found_impls_unknown = repo.find_impls_for_type(unknown_trait_id, &i32_ty, &mut InferenceContext::new());
        assert!(found_impls_unknown.is_empty());
    }
    
     #[test]
     fn test_find_method() {
         let mut repo = TraitRepository::new();
         let trait_symbol = Symbol::new(10);
         let method_symbol = Symbol::new(11);
         let method_name = "do_thing";

         let mut trait_def = dummy_trait_def(&mut repo, "MyTrait", trait_symbol);
         let trait_id = trait_def.id;
         
         let method_sig = FunctionSignature {
             name: method_name.to_string(),
             self_param: None,
             generic_params: vec![],
             params: vec![],
             return_type: ty_prim(PrimitiveType::Unit),
             span: dummy_span(),
         };
         let trait_method = TraitMethod {
             name: method_name.to_string(),
             method_symbol,
             signature: method_sig,
         };
         trait_def.methods.insert(method_symbol, trait_method.clone());
         
         repo.add_trait(trait_def);

         assert!(repo.find_method_by_symbol(trait_id, method_symbol).is_some());
         assert_eq!(repo.find_method_by_symbol(trait_id, method_symbol).unwrap().method_symbol, method_symbol);
         assert_eq!(repo.find_method_by_symbol(trait_id, method_symbol).unwrap().name, method_name);
         assert!(repo.find_method_by_symbol(trait_id, Symbol::new(99)).is_none());
         assert!(repo.find_method_by_name(trait_id, method_name).is_some());
         assert_eq!(repo.find_method_by_name(trait_id, method_name).unwrap().method_symbol, method_symbol);
         assert_eq!(repo.find_method_by_name(trait_id, method_name).unwrap().name, method_name);
         assert!(repo.find_method_by_name(trait_id, "other_method").is_none());
     }

    #[test]
    fn test_find_implementations_providing_method() {
        let mut repo = TraitRepository::new();
        let mut inference_ctx = InferenceContext::new();

        // Trait 1: Foo with method bar()
        let trait1_symbol = Symbol::new(100);
        let trait1_method_symbol = Symbol::new(101);
        let mut trait1_def = dummy_trait_def(&mut repo, "Foo", trait1_symbol);
        let trait1_id = trait1_def.id;
        trait1_def.methods.insert(trait1_method_symbol, TraitMethod {
            name: "bar".to_string(),
            method_symbol: trait1_method_symbol,
            signature: FunctionSignature { // Basic signature
                name: "bar".to_string(), self_param: None, generic_params: vec![],
                params: vec![], return_type: ty_prim(PrimitiveType::Unit), span: dummy_span()
            }
        });
        repo.add_trait(trait1_def);

        // Trait 2: Baz with method bar()
        let trait2_symbol = Symbol::new(200);
        let trait2_method_symbol = Symbol::new(201);
        let mut trait2_def = dummy_trait_def(&mut repo, "Baz", trait2_symbol);
        let trait2_id = trait2_def.id;
        trait2_def.methods.insert(trait2_method_symbol, TraitMethod {
            name: "bar".to_string(), // Same method name as Foo
            method_symbol: trait2_method_symbol,
            signature: FunctionSignature { // Basic signature
                name: "bar".to_string(), self_param: None, generic_params: vec![],
                params: vec![], return_type: ty_prim(PrimitiveType::Unit), span: dummy_span()
            }
        });
        repo.add_trait(trait2_def);

        // Impl 1: impl Foo for i32
        let impl1_symbol = Symbol::new(1000);
        let i32_ty = ty_prim(PrimitiveType::I32);
        let impl1_def = dummy_impl_def(&mut repo, impl1_symbol, Some(trait1_id), i32_ty.clone());
        repo.add_impl(impl1_def);
        let impl1_id = repo.symbol_to_impl_id[&impl1_symbol];

        // Impl 2: impl Baz for i32
        let impl2_symbol = Symbol::new(2000);
        let impl2_def = dummy_impl_def(&mut repo, impl2_symbol, Some(trait2_id), i32_ty.clone());
        repo.add_impl(impl2_def);
        let impl2_id = repo.symbol_to_impl_id[&impl2_symbol];

        // Impl 3: impl Foo for bool
        let impl3_symbol = Symbol::new(3000);
        let bool_ty = ty_prim(PrimitiveType::Bool);
        let impl3_def = dummy_impl_def(&mut repo, impl3_symbol, Some(trait1_id), bool_ty.clone());
        repo.add_impl(impl3_def);
        let impl3_id = repo.symbol_to_impl_id[&impl3_symbol];

        // --- Assertions ---

        // Find method "bar" for i32
        let results_i32 = repo.find_implementations_providing_method(&i32_ty, "bar", &mut inference_ctx);
        assert_eq!(results_i32.len(), 2); // Should find Impl1 (Foo) and Impl2 (Baz)
        assert!(results_i32.contains(&(impl1_id, trait1_method_symbol, trait1_id)));
        assert!(results_i32.contains(&(impl2_id, trait2_method_symbol, trait2_id)));

        // Find method "bar" for bool
        let results_bool = repo.find_implementations_providing_method(&bool_ty, "bar", &mut inference_ctx);
        assert_eq!(results_bool.len(), 1); // Should find Impl3 (Foo)
        assert!(results_bool.contains(&(impl3_id, trait1_method_symbol, trait1_id)));

        // Find method "unknown_method" for i32
        let results_unknown = repo.find_implementations_providing_method(&i32_ty, "unknown_method", &mut inference_ctx);
        assert!(results_unknown.is_empty());

        // Find method "bar" for String (no impls)
        let string_ty = ty_prim(PrimitiveType::String);
        let results_string = repo.find_implementations_providing_method(&string_ty, "bar", &mut inference_ctx);
        assert!(results_string.is_empty());
    }

    // --- Tests for Generic Impls ---
    use crate::types::TypeId;

    // Helper to create a generic parameter definition
    fn dummy_generic_param(name: &str, id: u32, symbol: u32) -> GenericParamDef {
        GenericParamDef {
            name: name.to_string(),
            symbol: Symbol::new(symbol), // Needs a unique symbol
            id: TypeId(id),
            bounds: vec![], // Keep bounds empty for simplicity
            span: dummy_span(),
        }
    }

    // Helper to create a TyKind::Var
    fn ty_var(id: u32) -> Ty {
        Ty::new(TyKind::Var(TypeId(id)))
    }

    // Helper to create a TyKind::Named
    fn ty_named(name: &str, symbol: Option<Symbol>, args: Vec<Ty>) -> Ty {
        Ty::new(TyKind::Named { name: name.to_string(), symbol, args })
    }

    #[test]
    fn test_find_method_with_generic_impl() {
        let mut repo = TraitRepository::new();
        let mut inference_ctx = InferenceContext::new();

        // Trait: Debug with method fmt()
        let debug_trait_sym = Symbol::new(500);
        let debug_method_sym = Symbol::new(501);
        let mut debug_trait_def = dummy_trait_def(&mut repo, "Debug", debug_trait_sym);
        let debug_trait_id = debug_trait_def.id;
        debug_trait_def.methods.insert(debug_method_sym, TraitMethod {
            name: "fmt".to_string(),
            method_symbol: debug_method_sym,
            signature: FunctionSignature { // Basic signature
                name: "fmt".to_string(), self_param: None, generic_params: vec![],
                params: vec![], return_type: ty_prim(PrimitiveType::String), span: dummy_span()
            }
        });
        repo.add_trait(debug_trait_def);

        // Generic Impl: impl<T> Debug for Vec<T>
        let vec_impl_sym = Symbol::new(5000);
        let generic_param_t = dummy_generic_param("T", 0, 5001); // T has TypeId(0)
        let vec_t_ty = ty_named("Vec", Some(Symbol::new(600)), vec![ty_var(0)]); // Vec<T> where T is TypeId(0)
        let mut vec_impl_def = dummy_impl_def(&mut repo, vec_impl_sym, Some(debug_trait_id), vec_t_ty);
        vec_impl_def.generic_params = vec![generic_param_t]; // Add generic param T to impl
        repo.add_impl(vec_impl_def);
        let vec_impl_id = repo.symbol_to_impl_id[&vec_impl_sym];

        // Concrete Impl: impl Debug for i32
        let i32_impl_sym = Symbol::new(6000);
        let i32_ty = ty_prim(PrimitiveType::I32);
        let i32_impl_def = dummy_impl_def(&mut repo, i32_impl_sym, Some(debug_trait_id), i32_ty.clone());
        repo.add_impl(i32_impl_def);
        let i32_impl_id = repo.symbol_to_impl_id[&i32_impl_sym];

        // --- Assertions ---

        // Find method "fmt" for i32 (should match concrete impl)
        let results_i32 = repo.find_implementations_providing_method(&i32_ty, "fmt", &mut inference_ctx);
        assert_eq!(results_i32.len(), 1);
        assert!(results_i32.contains(&(i32_impl_id, debug_method_sym, debug_trait_id)));

        // Find method "fmt" for Vec<i32> (should match generic impl)
        let vec_i32_ty = ty_named("Vec", Some(Symbol::new(600)), vec![i32_ty.clone()]);
        let results_vec_i32 = repo.find_implementations_providing_method(&vec_i32_ty, "fmt", &mut inference_ctx);
        // The structural match should succeed because Vec<i32> can unify with Vec<T>
        assert_eq!(results_vec_i32.len(), 1, "Expected generic impl to match Vec<i32>");
        assert!(results_vec_i32.contains(&(vec_impl_id, debug_method_sym, debug_trait_id)));

         // Find method "fmt" for Vec<bool> (should also match generic impl)
         let bool_ty = ty_prim(PrimitiveType::Bool);
         let vec_bool_ty = ty_named("Vec", Some(Symbol::new(600)), vec![bool_ty]);
         let results_vec_bool = repo.find_implementations_providing_method(&vec_bool_ty, "fmt", &mut inference_ctx);
         assert_eq!(results_vec_bool.len(), 1, "Expected generic impl to match Vec<bool>");
         assert!(results_vec_bool.contains(&(vec_impl_id, debug_method_sym, debug_trait_id)));

        // Find method "fmt" for String (no matching impls)
        let string_ty = ty_prim(PrimitiveType::String);
        let results_string = repo.find_implementations_providing_method(&string_ty, "fmt", &mut inference_ctx);
        assert!(results_string.is_empty());
    }
} 