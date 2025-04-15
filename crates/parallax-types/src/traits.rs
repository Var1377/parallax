use std::collections::{HashMap, HashSet};
use serde::{Deserialize, Serialize};

// Use the checker-internal types defined in types.rs
pub use crate::types::{Ty, TypedDefinitions, TypedFunction, TraitDef as CheckerTraitDef, TraitMethod as CheckerTraitMethod, ImplDef as CheckerImplDef, GenericParamDef, ParamType};

use parallax_resolve::types::Symbol;

/// A unique identifier for a trait
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TraitId(pub u32);

/// A unique identifier for a trait implementation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ImplId(pub u32);

// Placeholder Trait IDs (replace with actual mechanism if possible)
pub const ADD_TRAIT_ID: TraitId = TraitId(0);
pub const SUB_TRAIT_ID: TraitId = TraitId(1);
pub const MUL_TRAIT_ID: TraitId = TraitId(2);
pub const DIV_TRAIT_ID: TraitId = TraitId(3);
pub const EQ_TRAIT_ID: TraitId = TraitId(4);
pub const PARTIAL_ORD_TRAIT_ID: TraitId = TraitId(5);
// Add missing IDs
pub const REM_TRAIT_ID: TraitId = TraitId(6);
pub const BITAND_TRAIT_ID: TraitId = TraitId(7);
pub const BITOR_TRAIT_ID: TraitId = TraitId(8);
pub const BITXOR_TRAIT_ID: TraitId = TraitId(9);
pub const SHL_TRAIT_ID: TraitId = TraitId(10);
pub const SHR_TRAIT_ID: TraitId = TraitId(11);
pub const NEG_TRAIT_ID: TraitId = TraitId(12);
pub const NOT_TRAIT_ID: TraitId = TraitId(13);

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
    pub fn add_trait(&mut self, mut trait_def: CheckerTraitDef) {
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
        // Assign the next ID
        impl_def.id = self.next_impl_id();
        let impl_id = impl_def.id;

        // Add to the trait_impls map only if it's implementing a trait
        if let Some(trait_ref) = &impl_def.trait_ref {
           self.trait_impls.entry(trait_ref.trait_id).or_default().insert(impl_id);
        }

        // Store the impl definition itself
        self.impls.insert(impl_id, impl_def);
    }

    /// Find implementations of a specific trait (by ID) for a given type
    pub fn find_impls_for_type(&self, trait_id: TraitId, ty: &Ty) -> Vec<&CheckerImplDef> {
        // TODO: This equality check is too strict. Needs unification.
        if let Some(impl_ids) = self.trait_impls.get(&trait_id) {
            impl_ids.iter()
                .filter_map(|impl_id| self.impls.get(impl_id))
                // Use unification check instead of direct equality (placeholder)
                .filter(|impl_def| self.types_unify_placeholder(&impl_def.implementing_type, ty))
                .collect()
        } else {
            Vec::new()
        }
    }

    // Placeholder for unification check - replace with actual call to inference context
    fn types_unify_placeholder(&self, _ty1: &Ty, _ty2: &Ty) -> bool {
        // eprintln!("Warning: Using placeholder unification check in find_impls_for_type");
        // Replace with: inference_ctx.unify(ty1, ty2).is_ok()
        _ty1 == _ty2 // Current strict equality
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
    pub fn find_implementations_providing_method(&self, receiver_ty: &Ty, method_name: &str) -> Vec<(ImplId, Symbol, TraitId)> {
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
                        // Check if the implementing type *might* match (using placeholder)
                        // TODO: Replace placeholder with actual unification check!
                        if self.types_unify_placeholder(&impl_def.implementing_type, receiver_ty) {
                             results.push((*impl_id, trait_method.method_symbol, trait_id));
                        }
                    }
                }
            }
        }
        results
    }
} 