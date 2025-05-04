//! The main type checker implementation.

pub mod defs;
pub mod expr;
pub mod impls;
pub mod invocation;
pub mod operators;
pub mod pattern;
pub mod resolve;
pub mod substitute;

use crate::TypeDatabase; // <<< ADD IMPORT for Salsa DB trait
use crate::error::{display_type, TypeError}; // <<< ADD BACK TypeError
use crate::types::{ // Keep used
    Ty, GenericParamDef
};
use crate::context::{TypeContext, TraitRepository, InferenceContext, ImplId, TraitId}; // <<< ADD TraitId
use crate::context::env::TypeEnvironment; // <<< REMOVED EnvSnapshot
use crate::types::*; // Keep used
use crate::context::inference::TypeId;
use parallax_resolve::{types::{Symbol, ResolvedDefinitions}, definitions::DefinitionKind}; // <<< FIX PATH for DefinitionKind
use miette::SourceSpan;
use std::collections::HashMap;
use std::sync::Arc; // Keep used
use std::cell::RefCell;

/// The main structure orchestrating the type checking process.
/// Holds references to context, resolved definitions, and accumulates errors.
pub(crate) struct TypeChecker<'db> {
    /// Reference to the database for potentially querying other information.
    db: &'db dyn TypeDatabase,
    /// Resolved definitions from the name resolution phase.
    resolved_defs: &'db ResolvedDefinitions,
    /// Context storing type and standalone function definitions.
    pub(crate) type_ctx: &'db TypeContext, // <<< Use immutable borrow
    /// Repository storing trait and impl definitions.
    pub(crate) trait_repo: &'db TraitRepository, // <<< Use immutable borrow
    /// Current type inference context (includes substitution state).
    pub(crate) infctx: InferenceContext, // Made pub(crate)
    /// Current lexical environment for variable types.
    pub(crate) env: Arc<TypeEnvironment>, // Made pub(crate)
    /// Stack managing generic parameter scopes (TypeId -> GenericParamDef).
    /// Used to resolve generic parameter references during checking.
    pub(crate) generic_scopes: Vec<HashMap<String, GenericParamDef>>, // Using String as key for parameter names
    /// The type that `Self` currently refers to, if any.
    /// `Some(Ty)` when inside an `impl` block.
    pub(crate) current_self_type: Option<Ty>, // Made pub(crate)
    /// Accumulated type errors.
    pub(crate) errors: Vec<TypeError>, // Made pub(crate)
    /// Context flag: If Some((trait_id, impl_id)), we are currently checking
    /// the default body of a method from trait `trait_id` within the context
    /// of implementation `impl_id`.
    pub(crate) current_default_body_context: Option<(TraitId, ImplId)>,
}

impl<'db> TypeChecker<'db> {
    /// Creates a new `TypeChecker` instance.
    ///
    /// Preconditions: `db`, `resolved_defs`, `type_ctx`, `trait_repo` are initialized.
    /// Postconditions: Returns a `TypeChecker` ready for the first pass.
    pub fn new(
        db: &'db dyn TypeDatabase,
        resolved_defs: &'db ResolvedDefinitions,
        type_ctx: &'db TypeContext,
        trait_repo: &'db TraitRepository,
    ) -> Self {
        Self {
            db,
            resolved_defs,
            type_ctx,
            trait_repo, // <<< Store the reference >>>
            infctx: InferenceContext::new(),
            env: Arc::new(TypeEnvironment::new()), // Start with empty root env
            generic_scopes: Vec::new(),
            current_self_type: None,
            errors: Vec::new(),
            current_default_body_context: None, // Initialize context flag
        }
    }

    /// Records a type error.
    ///
    /// Preconditions: `error` is the error to record.
    /// Postconditions: `error` is added to the `errors` list.
    pub(crate) fn report_error(&mut self, error: TypeError) {
        self.errors.push(error);
    }

    /// Enters a new lexical scope (e.g., for a function body or block).
    /// Creates a new `TypeEnvironment` with the current one as parent.
    ///
    /// Preconditions: None.
    /// Postconditions: `self.env` is updated to a new environment wrapping the old one.
    pub(crate) fn enter_scope(&mut self) {
        self.env = Arc::new(TypeEnvironment::with_parent(Arc::clone(&self.env)));
    }

    /// Exits the current lexical scope.
    /// Restores the parent `TypeEnvironment`.
    ///
    /// Preconditions: Must be inside a scope entered with `enter_scope`.
    /// Postconditions: `self.env` is restored to the parent environment.
    /// Panics: If called without a corresponding `enter_scope` (i.e., at the root).
    pub(crate) fn exit_scope(&mut self) {
        let parent_env = self.env.parent.clone()
            .expect("Cannot exit the root scope");
        self.env = parent_env;
    }

    /// Enters a new scope specifically for generic parameters.
    ///
    /// Preconditions: `params` is the list of generic parameters for this scope.
    /// Postconditions: A new scope is pushed onto `self.generic_scopes` containing mappings
    ///                 from the parameter names to their `GenericParamDef`.
    pub(crate) fn enter_generic_scope(&mut self, params: &[GenericParamDef]) {
        let mut scope = HashMap::new();
        for param in params {
            // Assertion: Ensure parameter names are unique within the scope
            assert!(!scope.contains_key(&param.name), "Duplicate GenericParamDef name in scope");
            scope.insert(param.name.clone(), param.clone());
        }
        self.generic_scopes.push(scope);
    }

    /// Exits the current generic parameter scope.
    ///
    /// Preconditions: Must be inside a scope entered with `enter_generic_scope`.
    /// Postconditions: The top scope is popped from `self.generic_scopes`.
    /// Panics: If called without a corresponding `enter_generic_scope`.
    pub(crate) fn exit_generic_scope(&mut self) {
        self.generic_scopes.pop().expect("Cannot exit the root generic scope");
    }

    /// Looks up a generic parameter definition by its `TypeId`.
    /// Searches from the innermost scope outwards.
    ///
    /// Preconditions: `id` is the `TypeId` of the generic parameter variable to find.
    /// Postconditions: Returns `Some(&GenericParamDef)` if found, `None` otherwise.
    #[allow(dead_code)]
    pub(crate) fn lookup_generic_param(&self, id: TypeId) -> Option<&GenericParamDef> {
        for scope in self.generic_scopes.iter().rev() {
            for param_def in scope.values() {
                if param_def.id == id {
                    return Some(param_def);
                }
            }
        }
        None
    }

    /// Sets the `Self` type for the current context (e.g., entering an `impl` block).
    ///
    /// Preconditions: `self_ty` is the type `Self` should resolve to.
    /// Postconditions: `self.current_self_type` is set to `Some(self_ty)`.
    /// Panics: If `current_self_type` is already set (nested impls not supported yet).
    pub(crate) fn set_self_type(&mut self, self_ty: Ty) {
        assert!(self.current_self_type.is_none(), "Nested setting of Self type is not supported");
        self.current_self_type = Some(self_ty);
    }

    /// Clears the `Self` type (e.g., exiting an `impl` block).
    ///
    /// Preconditions: `current_self_type` should be `Some`.
    /// Postconditions: `self.current_self_type` is set to `None`.
    /// Panics: If `current_self_type` is already `None`.
    pub(crate) fn clear_self_type(&mut self) {
        assert!(self.current_self_type.is_some(), "Cannot clear Self type when it is not set");
        self.current_self_type = None;
    }

    /// Creates a fresh type variable using the inference context.
    ///
    /// Preconditions: None.
    /// Postconditions: Returns a new `Ty` representing an inference variable.
    pub(crate) fn fresh_var(&mut self) -> Ty {
        self.infctx.fresh_var()
    }

    /// Creates a fresh type variable, potentially marked for defaulting.
    pub(crate) fn fresh_var_with_defaulting(&mut self, default_kind: Option<crate::context::inference::DefaultKind>) -> Ty {
        self.infctx.fresh_var_with_defaulting(default_kind)
    }

    /// Attempts to unify two types using the inference context.
    /// Reports an error if unification fails.
    ///
    /// Preconditions: `ty1`, `ty2` are types to unify, `span` for error reporting.
    /// Postconditions: Returns `true` on successful unification, `false` otherwise.
    ///                 Errors are added to `self.errors`.
    pub(crate) fn unify(&mut self, ty1: &Ty, ty2: &Ty, span: SourceSpan) -> bool {
        match self.infctx.unify(ty1, ty2, span) {
            Ok(_) => true,
            Err(err) => {
                self.report_error(err);
                false
            }
        }
    }

    /// Adds a trait constraint to the inference context.
    pub(crate) fn add_constraint(&mut self, ty: Ty, trait_ref: TraitRef) {
        self.infctx.add_constraint(ty, trait_ref);
    }

    /// Sets the context indicating we are checking a default method body.
    pub(crate) fn set_default_body_context(&mut self, trait_id: TraitId, impl_id: ImplId) {
        assert!(self.current_default_body_context.is_none(), "Nested setting of default body context");
        self.current_default_body_context = Some((trait_id, impl_id));
    }

    /// Clears the context indicating we are no longer checking a default method body.
    pub(crate) fn clear_default_body_context(&mut self) {
        assert!(self.current_default_body_context.is_some(), "Cannot clear default body context when not set");
        self.current_default_body_context = None;
    }

    // Finds inherent methods (defined directly in `impl ReceiverType { ... }`)
    fn find_inherent_method_candidates(
        &mut self, // Needs mutable checker for resolve_type_to_ty and unify
        receiver_ty: &Ty,
        method_name: &str,
        span: SourceSpan, // Use span for errors/resolution
    ) -> Vec<Symbol> {
        let mut candidates = Vec::new();
        let concrete_receiver_ty = self.infctx.apply_substitution(receiver_ty);

        // Iterate through all impl blocks in the resolved definitions
        for ri in &self.resolved_defs.impls { 
            // Check if it's an inherent impl (no trait symbol)
            if ri.trait_symbol.is_none() {
                // --- Check if impl target type matches receiver type --- 
                let snapshot = self.infctx.snapshot(); // Snapshot before potential unification
                let mut impl_matches = false;
                
                // Temporarily enter generic scope for the impl block itself
                let mut impl_generic_params_checker = Vec::new();
                let mut temp_checker = TypeChecker {
                    db: self.db,
                    resolved_defs: self.resolved_defs,
                    type_ctx: self.type_ctx,
                    trait_repo: self.trait_repo,
                    infctx: InferenceContext::new(), // Use temp infctx for resolution
                    env: self.env.clone(),
                    generic_scopes: self.generic_scopes.clone(),
                    current_self_type: None, // Should be None when resolving impl type
                    current_default_body_context: None, // <<< Initialize field >>>
                    errors: vec![], // Temp errors
                };

                for rgp in &ri.generic_params {
                    match resolve::resolve_single_generic_param(&mut temp_checker, rgp, Some((DefinitionKind::Impl, ri.impl_symbol))) {
                        Ok(gp) => impl_generic_params_checker.push(gp),
                        Err(e) => { /* TODO: Handle error reporting REMOVED */ 
                            // Use the new specific error type
                            self.report_error(TypeError::ImplGenericParamResolutionError {
                                param_name: rgp.name.clone(), // Assuming ResolvedGenericParamDef has a name field
                                span: rgp.span,
                                source_error: Box::new(e),
                            });
                        }
                    }
                }
                temp_checker.enter_generic_scope(&impl_generic_params_checker);

                // Resolve the implementing type *within the impl's generic context*
                match resolve::resolve_type_to_ty(&mut temp_checker, &ri.implementing_type) {
                    Ok(impl_target_ty) => {
                        // Now unify the concrete receiver type with the resolved impl target type
                         // We need to use the main checker's infctx for this unification
                        if self.infctx.unify(&concrete_receiver_ty, &impl_target_ty, span).is_ok() {
                             impl_matches = true;
                             println!("  Inherent impl {:?} potentially matches receiver type {} (after unification)", ri.impl_symbol, display_type(&concrete_receiver_ty));
                        } else {
                             self.infctx.rollback_to(snapshot.clone()); // Clone snapshot here
                             println!("  Inherent impl {:?} type mismatch: {} vs {}", ri.impl_symbol, display_type(&concrete_receiver_ty), display_type(&impl_target_ty));
                        }
                    }
                    Err(e) => {
                        println!("Error resolving impl implementing type: {}", e);
                        self.infctx.rollback_to(snapshot.clone()); // Clone snapshot here
                    }
                }
                // No need to exit TempChecker scope explicitly

                if impl_matches {
                    // Check methods defined in this impl block
                    for assoc_func in &ri.methods {
                        if let Some(method_rf) = self.resolved_defs.functions.iter().find(|f| f.symbol == assoc_func.func_symbol) {
                            if method_rf.name == method_name {
                                candidates.push(assoc_func.func_symbol);
                            }
                        }
                    }
                    // If match succeeded, do NOT rollback. Keep the unification results.
                } else {
                     self.infctx.rollback_to(snapshot); // Original snapshot can be moved here
                }
            }
        }

        println!("Final Inherent candidates for {}::{}: {:?}", display_type(&concrete_receiver_ty), method_name, candidates);
        candidates
    }

    /// Finds trait methods candidates for a given receiver type and method name.
    /// Uses trait resolution to find applicable impls.
    fn find_trait_method_candidates(
        &mut self,
        receiver_ty: &Ty,
        method_name: &str,
        span: SourceSpan,
    ) -> Vec<(Symbol, ImplId)> { // Returns (Method Symbol, ImplId)
        let mut candidates = Vec::new();
        let mut errors = Vec::new(); // Collect errors here
        let concrete_receiver_ty = self.infctx.apply_substitution(receiver_ty);
        println!("Finding trait method candidates for {}::{}", display_type(&concrete_receiver_ty), method_name);

        // Iterate through all known traits
        // Clone the trait repo data needed to avoid borrowing checker while calling resolver
        let all_traits: Vec<_> = self.trait_repo.all_traits().cloned().collect();

        for trait_def in all_traits {
            // Check if this trait defines a method with the given name
            if let Some(trait_method) = trait_def.methods.values().find(|tm| tm.name == method_name) {
                let trait_method_symbol = trait_method.method_symbol;
                println!("  Checking trait '{}' which has method '{}' ({:?})", trait_def.name, method_name, trait_method_symbol);

                // Construct a TraitRef with fresh variables for the trait's generic parameters
                let trait_args: Vec<_> = trait_def.generic_params.iter()
                    .map(|_| self.fresh_var()) // Use checker's fresh_var
                    .collect();
                let required_trait_ref = TraitRef {
                    trait_id: trait_def.id,
                    type_arguments: trait_args,
                    span, // Use call site span
                };

                // Attempt to resolve the implementation using the repository's logic
                let resolve_result = self.trait_repo.resolve_trait_implementation(
                    &concrete_receiver_ty,
                    &required_trait_ref,
                    &mut self.infctx, // Pass mutable infctx
                    span,
                );
                match resolve_result {
                    Ok(Some(impl_id)) => {
                        println!("    Found matching impl: {:?}", impl_id);
                        let mut found_symbol: Option<Symbol> = None;
                        // <<< Scope the borrow >>>
                        {
                            if let Some(impl_def) = self.trait_repo.get_impl(impl_id) {
                                // Find the symbol for the method implementation within the impl
                                found_symbol = impl_def.methods.get(&trait_method_symbol)
                                    .copied()
                                    .or_else(|| {
                                        // Check if the trait method itself has a default body
                                        // Need trait_def again here
                                        let trait_def_opt = self.trait_repo.get_trait(trait_def.id);
                                        trait_def_opt.and_then(|td| td.methods.get(&trait_method_symbol))
                                            .and_then(|tm| tm.default_body.as_ref().map(|_| trait_method_symbol))
                                    });
                            }
                        }

                        if let Some(_func_symbol) = found_symbol {
                            candidates.push((trait_method_symbol, impl_id));
                        } else {
                            println!("Warning: Impl {:?} matched trait '{}' but method '{}' symbol not found (explicitly or default).", impl_id, trait_def.name, method_name);
                        }
                    }
                    Ok(None) => {
                        println!("    Trait '{}' not implemented for {}", trait_def.name, display_type(&concrete_receiver_ty));
                    }
                    Err(e) => {
                        // Collect error instead of reporting immediately
                        errors.push(e);
                    }
                }
            }
        }

        // Report collected errors after all borrows are dropped
        for e in errors {
            self.report_error(e);
        }

        println!("Final Trait candidates for {}::{}: {:?}", display_type(&concrete_receiver_ty), method_name, candidates);
        candidates
    }
}