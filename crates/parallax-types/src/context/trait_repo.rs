// src/context/trait_repo.rs
//! Manages definitions of traits and their implementations (impls).

use std::collections::{HashMap, HashSet};
use serde::{Deserialize, Serialize};
use miette::SourceSpan;
use parallax_resolve::types::Symbol;

// Use crate paths
use crate::types::{TraitDef, ImplDef, TraitRef, TyKind, Ty, PrimitiveType, FunctionSignature, TraitMethod};
use crate::error::{TypeResult, TypeError, display_type};
use crate::context::inference::Substitution;

/// Unique identifier for a Trait definition within the repository.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct TraitId(pub u32);

/// Unique identifier for an Impl definition within the repository.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct ImplId(pub u32);

/// Stores all known trait and implementation definitions.
/// This structure is crucial for trait resolution and checking trait bounds.
#[derive(Debug, Clone, Default)]
pub struct TraitRepository {
    /// Mapping from a trait's Symbol (from resolver) to its TraitId.
    trait_symbol_to_id: HashMap<Symbol, TraitId>,
    /// Mapping from a trait's name (String) to its Symbol.
    name_to_symbol: HashMap<String, Symbol>,
    /// Storage for TraitDef, indexed by TraitId.
    traits: HashMap<TraitId, TraitDef>,
    /// Counter for assigning the next TraitId.
    next_trait_id: u32,

    /// Mapping from an impl block's Symbol (from resolver) to its ImplId.
    impl_symbol_to_id: HashMap<Symbol, ImplId>,
    /// Storage for ImplDef, indexed by ImplId.
    impls: HashMap<ImplId, ImplDef>,
    /// Counter for assigning the next ImplId.
    next_impl_id: u32,

    /// Index: Maps a type Symbol (for structs/enums) to the set of ImplIds
    /// that implement *something* for that type (either inherent or trait impls).
    /// Used for finding candidate impls during method resolution.
    impls_by_type: HashMap<Symbol, HashSet<ImplId>>,

    /// Index: Maps a TraitId to the set of ImplIds that implement that trait.
    /// Used for checking trait bounds and finding implementations.
    impls_by_trait: HashMap<TraitId, HashSet<ImplId>>,
}

impl TraitRepository {
    /// Creates a new, empty trait repository.
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a trait definition to the repository.
    /// Assigns a fresh `TraitId` and updates relevant maps.
    ///
    /// Preconditions: `trait_def` contains valid definition information.
    ///                `trait_def.trait_symbol` should be unique among trait symbols.
    /// Postconditions: The trait is stored and indexed. `trait_def.id` is updated.
    /// Returns the assigned `TraitId`.
    /// Assertions: Checks for trait symbol collisions.
    pub fn add_trait(
        &mut self, 
        trait_symbol: Symbol,
        name: String,
        generic_params: Vec<crate::types::GenericParamDef>,
        bounds: Vec<crate::types::TraitRef>,
        methods: std::collections::HashMap<Symbol, crate::types::TraitMethod>,
        associated_types: std::collections::HashMap<Symbol, crate::types::AssociatedTypeDef>,
        span: SourceSpan,
    ) -> TypeResult<TraitId> {
        // Assertion: Ensure this trait symbol hasn't been added already.
        if self.trait_symbol_to_id.contains_key(&trait_symbol) {
            return Err(TypeError::InternalError {
                message: format!("Duplicate trait symbol {:?} added to repository", trait_symbol),
                span: Some(span),
            });
        }
        // Check for name collision too
        if self.name_to_symbol.contains_key(&name) {
             return Err(TypeError::InternalError {
                 message: format!("Duplicate trait name '{}' added to repository", name),
                 span: Some(span),
             });
        }

        let id = TraitId(self.next_trait_id);
        self.next_trait_id += 1;
        // Assertion: Check for potential overflow.
        assert!(self.next_trait_id < u32::MAX, "TraitId counter overflow");

        // Construct TraitDef here
        let trait_def = TraitDef {
            id, // Assign the generated id
            trait_symbol,
            name: name.clone(), // Clone name for insertion
            generic_params,
            bounds,
            methods,
            associated_types,
            span,
        };

        self.trait_symbol_to_id.insert(trait_symbol, id);
        self.traits.insert(id, trait_def); // Store the constructed trait_def
        self.name_to_symbol.insert(name, trait_symbol); // Populate name map
        Ok(id)
    }

    /// Adds an implementation definition (`impl Trait for Type` or `impl Type`) to the repository.
    /// Assigns a fresh `ImplId` and updates relevant maps and indices.
    ///
    /// Preconditions: `impl_def` contains valid definition information.
    ///                `impl_def.impl_symbol` should be unique among impl symbols.
    ///                `impl_def.implementing_type` should resolve to a concrete type or Self.
    ///                If `impl_def.trait_ref` is Some, the `TraitId` must be valid.
    /// Postconditions: The impl is stored and indexed. `impl_def.id` is updated.
    /// Returns the assigned `ImplId`.
    /// Assertions: Checks for impl symbol collisions, valid TraitId.
    pub fn add_impl(
        &mut self, 
        impl_symbol: Symbol,
        trait_symbol_opt: Option<Symbol>,
        implementing_type: Ty,
        generic_params: Vec<crate::types::GenericParamDef>,
        method_signatures: HashMap<Symbol, crate::types::FunctionSignature>,
        methods: HashMap<Symbol, Symbol>,
        default_method_impl_symbols: HashMap<Symbol, Symbol>,
        associated_type_bindings: HashMap<Symbol, Ty>,
        span: SourceSpan,
    ) -> TypeResult<ImplId> {
        // let symbol = impl_def.impl_symbol; // Use impl_symbol directly
        // Assertion: Ensure this impl symbol hasn't been added already.
        if self.impl_symbol_to_id.contains_key(&impl_symbol) {
            return Err(TypeError::InternalError {
                message: format!("Duplicate impl symbol {:?} added to repository", impl_symbol),
                span: Some(span),
            });
        }

        // Resolve trait_symbol to TraitId and construct TraitRef
        let final_trait_ref = if let Some(trait_symbol) = trait_symbol_opt {
            if let Some(trait_id) = self.trait_symbol_to_id.get(&trait_symbol).copied() {
                // TODO: We need the type arguments here! They are lost in the current flow.
                // We need to pass them from check_single_impl_stub through to here.
                // For now, using empty Vec which is likely incorrect for generic traits.
                println!("Warning: Reconstructing TraitRef in add_impl with potentially missing type arguments.");
                Some(TraitRef {
                    trait_id, // Use the resolved ID
                    type_arguments: vec![], // <<< PROBLEM: Need actual type args!
                    span, // Use impl block span
                })
            } else {
                // Trait symbol provided but not found in repository (should be caught by check_single_impl_stub ideally)
                return Err(TypeError::InternalError {
                    message: format!("Impl {:?} references unknown trait symbol {:?}", impl_symbol, trait_symbol),
                    span: Some(span),
                });
            }
        } else {
            None // Inherent impl
        };

        let id = ImplId(self.next_impl_id);
        self.next_impl_id += 1;
        // Assertion: Check for potential overflow.
        assert!(self.next_impl_id < u32::MAX, "ImplId counter overflow");

        // Construct ImplDef here
        let impl_def = ImplDef {
            id, // Assign the generated ID
            impl_symbol,
            trait_symbol: trait_symbol_opt, // Store the symbol
            implementing_type: implementing_type.clone(), // Clone the type
            generic_params,
            method_signatures, // Use passed map
            methods, // Use passed map
            associated_type_bindings,
            default_method_impl_symbols, // <<< STORE the passed map
            checked_default_bodies: HashMap::new(),
            span,
        };

        // Update indices
        // Index by type: Only index if the implementing type is a named type (struct/enum).
        // We can't reliably index by primitive types or complex types like functions/tuples directly
        // in this simple map. Resolution for those might need different strategies.
        if let TyKind::Named { symbol: Some(type_symbol), .. } = &impl_def.implementing_type.kind {
            self.impls_by_type.entry(*type_symbol).or_default().insert(id);
        }
        // Index by trait (if it's a trait impl)
        // Use the constructed final_trait_ref
        if let Some(tr) = &final_trait_ref {
            self.impls_by_trait.entry(tr.trait_id).or_default().insert(id);
        }

        self.impl_symbol_to_id.insert(impl_symbol, id);
        self.impls.insert(id, impl_def);

        Ok(id)
    }

    /// Retrieves a trait definition by its `TraitId`.
    ///
    /// Preconditions: `id` is the ID to look up.
    /// Postconditions: Returns `Some(&TraitDef)` if found, `None` otherwise.
    pub fn get_trait(&self, id: TraitId) -> Option<&TraitDef> {
        self.traits.get(&id)
    }

    /// Retrieves a trait definition by its original `Symbol`.
    ///
    /// Preconditions: `symbol` is the symbol to look up.
    /// Postconditions: Returns `Some(&TraitDef)` if found, `None` otherwise.
    pub fn get_trait_by_symbol(&self, symbol: &Symbol) -> Option<&TraitDef> {
        self.trait_symbol_to_id.get(symbol).and_then(|id| self.traits.get(id))
    }

    /// Retrieves a trait definition by its source name.
    pub fn get_trait_by_name(&self, name: &str) -> Option<&TraitDef> {
        self.name_to_symbol.get(name)
            .and_then(|symbol| self.get_trait_by_symbol(symbol))
    }

    /// Retrieves an implementation definition by its `ImplId`.
    ///
    /// Preconditions: `id` is the ID to look up.
    /// Postconditions: Returns `Some(&ImplDef)` if found, `None` otherwise.
    pub fn get_impl(&self, id: ImplId) -> Option<&ImplDef> {
        self.impls.get(&id)
    }

    /// Retrieves an implementation definition by its original `Symbol`.
    ///
    /// Preconditions: `symbol` is the symbol to look up.
    /// Postconditions: Returns `Some(&ImplDef)` if found, `None` otherwise.
    pub fn get_impl_by_symbol(&self, symbol: &Symbol) -> Option<&ImplDef> {
        self.impl_symbol_to_id.get(symbol).and_then(|id| self.impls.get(id))
    }

    /// Retrieves a mutable implementation definition by its `ImplId`.
    /// Used internally to store results like checked default bodies.
    pub fn get_impl_mut(&mut self, id: ImplId) -> Option<&mut ImplDef> {
        self.impls.get_mut(&id)
    }

    /// Finds all implementations for a specific type symbol.
    /// Includes both inherent impls (`impl Type`) and trait impls (`impl Trait for Type`).
    ///
    /// Preconditions: `type_symbol` is the symbol of the struct/enum type.
    /// Postconditions: Returns an iterator over `ImplId`s for relevant impls.
    /// Note: This only works for named types indexed by `impls_by_type`.
    pub fn get_impls_for_type(&self, type_symbol: &Symbol) -> impl Iterator<Item = ImplId> + '_ {
        self.impls_by_type.get(type_symbol)
            .map(|ids| ids.iter().cloned().collect::<Vec<_>>().into_iter())
            .unwrap_or_else(|| Vec::new().into_iter()) // Return empty vec iterator
    }

    /// Finds all implementations of a specific trait.
    ///
    /// Preconditions: `trait_id` identifies the trait.
    /// Postconditions: Returns an iterator over `ImplId`s for impls of that trait.
    pub fn get_impls_for_trait(&self, trait_id: TraitId) -> impl Iterator<Item = ImplId> + '_ {
        self.impls_by_trait.get(&trait_id)
            .map(|ids| ids.iter().cloned().collect::<Vec<_>>().into_iter())
            .unwrap_or_else(|| Vec::new().into_iter()) // Return empty vec iterator
    }

    /// Performs trait resolution: checks if `target_ty` implements the trait specified by `required_trait_ref`.
    /// This is a core function for checking trait bounds and resolving associated items.
    ///
    /// Preconditions: `target_ty` is the type to check. `required_trait_ref` specifies the trait and its args.
    ///                `infctx` is needed for potential unification during matching.
    ///                `error_span` is used for reporting errors.
    /// Postconditions: Returns `Ok(Option<ImplId>)` where `Some(ImplId)` indicates the specific impl that satisfies
    ///                 the requirement, and `None` might indicate satisfaction via supertraits or built-ins (TODO).
    ///                 Returns `Err(TypeError)` if the trait is not implemented or resolution is ambiguous.
    /// TODO: Handle supertrait propagation (if `T: U` and `U: V`, then `T: V`).
    /// TODO: Handle built-in impls (e.g., primitives implementing `Copy`).
    /// TODO: Refine ambiguity checking.
    pub fn resolve_trait_implementation(
        &self,
        target_ty: &Ty,
        required_trait_ref: &TraitRef,
        // Need inference context for unification
        infctx: &mut crate::context::inference::InferenceContext,
        error_span: SourceSpan,
    ) -> TypeResult<Option<ImplId>> {
        println!(
            "Resolving trait impl: {} for trait {}",
            display_type(target_ty),
            self.get_trait(required_trait_ref.trait_id).map_or("<???>", |t| &t.name)
        );

        let required_trait_id = required_trait_ref.trait_id;
        let mut matching_impls = Vec::new();

        // 1. Find candidate impls for the required trait.
        let candidate_impl_ids = self.get_impls_for_trait(required_trait_id);

        for impl_id in candidate_impl_ids {
            let impl_def = self.get_impl(impl_id).ok_or_else(|| TypeError::InternalError {
                message: format!("ImplId {:?} not found in repository during trait resolution", impl_id),
                span: Some(error_span),
            })?;

            // --- Instantiate Impl Generics ---
            let impl_generic_snapshot = infctx.snapshot(); // Snapshot *before* generic instantiation
            let mut impl_generic_subst = Substitution::new();
            let _impl_generic_args: Vec<Ty> = impl_def.generic_params.iter().map(|gp| {
                let fresh_var = infctx.fresh_var(); // Use the passed infctx
                impl_generic_subst.insert(gp.id, fresh_var.clone());
                fresh_var
            }).collect();

            // Substitute generics in the impl's target type
            let instantiated_impl_target_ty = impl_def.implementing_type.apply_subst(&impl_generic_subst);

            // --- Unify Target Type ---
            let mut should_check_trait_args = false; // Initialize to false
            if target_ty.kind == TyKind::SelfType {
                 println!("WARN: Allowing SelfType match in resolve_trait_implementation (TEMPORARY)");
                 should_check_trait_args = true;
                 // NOTE: We DON'T unify, so inference variables related to Self might remain.
            } else {
                 // For concrete types, perform unification.
                 match infctx.unify(target_ty, &instantiated_impl_target_ty, error_span) {
                     Ok(_) => {
                         should_check_trait_args = true; // Unification succeeded, proceed.
                         // Keep the unification changes for now.
                     },
                     Err(_) => {
                         // Type unification failed, this impl is not a match.
                         println!("  - Candidate impl {:?} type mismatch ({} vs {})", impl_id, display_type(target_ty), display_type(&instantiated_impl_target_ty));
                         infctx.rollback_to(impl_generic_snapshot); // Rollback generic instantiation changes
                         continue; // Skip to the next candidate impl
                     }
                 }
            };

            if should_check_trait_args {
                    // Types match (or assumed to match for SelfType)! 
                    // Snapshot *before* checking trait arguments, in case *they* fail.
                    let trait_args_snapshot = infctx.snapshot(); 

                    // Now check if trait arguments also match.
                    // Get the trait ref *from the impl*, substituted with impl generics
                    let instantiated_impl_trait_ref = if let Some(impl_trait_sym) = impl_def.trait_symbol {
                        // Lookup trait ID using the symbol
                        self.trait_symbol_to_id.get(&impl_trait_sym).map(|impl_trait_id| {
                            // PROBLEM: We don't have the original type arguments from the impl header
                            // to substitute here. Need to refactor how this info is stored/passed.
                            // Using empty vec as placeholder.
                            TraitRef {
                                trait_id: *impl_trait_id,
                                type_arguments: vec![], // <<< Placeholder
                                span: impl_def.span,
                            }
                        })
                    } else {
                        None // Inherent impl
                    };

                    // Ensure this impl actually implements the required trait (should always be true
                    // if we got the impl_id from impls_by_trait, but check defensively).
                    if instantiated_impl_trait_ref.as_ref().map(|tr| tr.trait_id) != Some(required_trait_id) {
                        infctx.rollback_to(trait_args_snapshot);
                        continue; // Should not happen, but skip if it does
                    }

                    // --- Unify Trait Arguments ---
                    if let Some(impl_tr) = instantiated_impl_trait_ref {
                        if required_trait_ref.type_arguments.len() == impl_tr.type_arguments.len() {
                            let mut trait_args_unified = true;
                            for (req_arg, impl_arg) in required_trait_ref.type_arguments.iter().zip(impl_tr.type_arguments.iter()) {
                                if infctx.unify(req_arg, impl_arg, error_span).is_err() {
                                    trait_args_unified = false;
                                    break; // Stop unification on first error
                                }
                            }

                            if trait_args_unified {
                                // Both type and trait arguments match!
                                println!("  - Candidate impl {:?} matches type {} AND trait args", impl_id, display_type(&instantiated_impl_target_ty));
                                matching_impls.push(impl_id);
                                // Don't rollback yet, keep successful unification changes.
                            } else {
                                // Trait arguments didn't unify
                                println!("  - Candidate impl {:?} trait arg mismatch", impl_id);
                                infctx.rollback_to(trait_args_snapshot); // Rollback to state *before* trait arg unification
                            }
                        } else {
                            // Trait argument count mismatch (should be caught earlier?)
                            println!("  - Candidate impl {:?} trait arg count mismatch", impl_id);
                            infctx.rollback_to(trait_args_snapshot); // Rollback
                        }
                    } else { 
                        // This is an inherent impl, can't satisfy a trait requirement
                         println!("  - Candidate impl {:?} is inherent, cannot satisfy trait", impl_id);
                         infctx.rollback_to(trait_args_snapshot); // Rollback 
                    }
            } // End if should_check_trait_args
        } // End loop over candidate impls

        // 3. Analyze results.
        match matching_impls.len() {
            0 => {
                // No matching impl found. Check supertraits.
                if let Some(trait_def) = self.get_trait(required_trait_id) {
                    // Build substitution map from the *required trait's* generics to its *provided arguments*
                    let mut trait_arg_subst = Substitution::new();
                    if trait_def.generic_params.len() == required_trait_ref.type_arguments.len() {
                        for (param, arg) in trait_def.generic_params.iter().zip(&required_trait_ref.type_arguments) {
                            trait_arg_subst.insert(param.id, arg.clone());
                        }
                    } else {
                        // Mismatch should ideally be caught earlier, but handle defensively
                        // Returning Ok(None) here as supertrait check can't proceed
                        return Ok(None);
                    }

                    for supertrait_ref_template in &trait_def.bounds { // Iterate over supertrait bounds
                        // Apply the substitution to the supertrait reference
                        let subst_supertrait_ref = crate::checker::substitute::substitute_trait_ref(supertrait_ref_template, &trait_arg_subst);

                        println!("    Checking supertrait {:?} for {}", subst_supertrait_ref.trait_id, trait_def.name);
                        
                        // Recursively check if the target type implements the substituted supertrait
                        // Use a snapshot as recursive call might unify variables
                        let super_snapshot = infctx.snapshot();
                        match self.resolve_trait_implementation(target_ty, &subst_supertrait_ref, infctx, error_span) {
                            Ok(Some(_)) => {
                                // If ANY supertrait is implemented, the original trait is considered implemented (via supertrait).
                                // We return Ok(None) because no *direct* impl was found for the *required* trait.
                                println!("      Satisfied via supertrait {:?}", subst_supertrait_ref.trait_id);
                                // Rollback changes made by the recursive call, as the *overall* result is just satisfaction, not a specific impl ID.
                                infctx.rollback_to(super_snapshot);
                                return Ok(None); 
                            }
                            Ok(None) => {
                                // This specific supertrait wasn't implemented (directly or via *its* supertraits).
                                // Rollback and continue checking other supertraits.
                                infctx.rollback_to(super_snapshot);
                            }
                            Err(e) => {
                                // Error during supertrait resolution. Rollback and propagate error.
                                infctx.rollback_to(super_snapshot);
                                return Err(e); 
                            }
                        }
                    }
                }
                // No direct impl, no supertrait impl found. Check built-ins in `solve`.
                Ok(None)
            }
            1 => {
                // Exactly one match found.
                println!("  - Unique match found: ImplId({:?})", matching_impls[0]);
                Ok(Some(matching_impls[0]))
            }
            _ => {
                // Multiple potentially matching impls found -> Ambiguity.
                // This can happen with overlapping blanket impls, which might be disallowed later.
                let trait_name = self.get_trait(required_trait_id).map_or(format!("TraitId({})", required_trait_id.0), |t| t.name.clone());
                 println!("  - Ambiguous match found: {:?}", matching_impls);
                Err(TypeError::AmbiguousTraitImpl {
                    ty: display_type(target_ty),
                    trait_name,
                    span: error_span,
                })
            }
        }
    }

    /// Returns an iterator over all defined traits.
    pub fn all_traits(&self) -> impl Iterator<Item = &TraitDef> {
        self.traits.values()
    }

    /// Returns an iterator over all defined impls.
    pub fn all_impls(&self) -> impl Iterator<Item = &ImplDef> {
        self.impls.values()
    }

    /// Finds the signature of an impl method given its symbol.
    pub fn find_impl_method_sig(&self, method_symbol: &Symbol) -> Option<&FunctionSignature> {
        for impl_def in self.impls.values() {
            if let Some(sig) = impl_def.method_signatures.get(method_symbol) {
                return Some(sig);
            }
        }
        None
    }

    /// Finds the signature of a trait method given its symbol.
    pub fn find_trait_method_sig(&self, method_symbol: &Symbol) -> Option<&FunctionSignature> {
        for trait_def in self.traits.values() {
            if let Some(trait_method) = trait_def.methods.get(method_symbol) {
                return Some(&trait_method.signature);
            }
        }
        None
    }

    /// Checks if a symbol corresponds to a method defined in any trait.
    pub fn is_trait_method_symbol(&self, symbol: &Symbol) -> bool {
        self.traits.values().any(|trait_def| trait_def.methods.contains_key(symbol))
    }

    /// Finds the TraitId that contains the given method symbol.
    pub fn get_trait_id_for_method(&self, method_symbol: &Symbol) -> Option<TraitId> {
        for trait_def in self.traits.values() {
            if trait_def.methods.contains_key(method_symbol) {
                return Some(trait_def.id);
            }
        }
        None
    }
} 