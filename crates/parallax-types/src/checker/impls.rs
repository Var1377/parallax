// src/checker/impls.rs
//! Type checking passes for impl blocks (trait and inherent).

use super::TypeChecker;
use crate::error::{TypeError, TypeResult, display_type};
use crate::types::{
    FunctionSignature, ImplDef, TraitDef, TraitRef, ParamType, GenericParamDef,
    Ty, TyKind, TypedDefinitions, TypedFunction, TypedParameter,
    TypedExpr
};
use crate::context::env::TypeEnvironment;
use crate::context::inference::Substitution;
use crate::context::{ImplId, TraitId};
use crate::checker::substitute::substitute_signature_self;
use parallax_resolve::types::{Symbol, ResolvedImpl, ResolvedFunction};
use parallax_resolve::types::{ResolvedDefinitions, ResolvedExpr};
use parallax_resolve::definitions::DefinitionKind;
use crate::context::TraitRepository;
use std::collections::HashMap;
use std::sync::Arc;
use crate::types::TypedExprKind;

/// Pass 1: Check impl block headers and method signatures (stubs).
/// Populates `checker.trait_repo` with `ImplDef` stubs.
/// Does *not* check method bodies.
///
/// Preconditions: `check_definitions_pass1` has run.
/// Postconditions: `checker.trait_repo` contains stub `ImplDef`s.
///                 Errors related to impl header/method signature compatibility are added.
pub(crate) fn check_impl_stubs_pass1(checker: &mut TypeChecker, collected_trait_defs: &[TraitDef]) -> Vec<ImplDef> {
    let mut collected_impls = Vec::new();
    // Iterate over impls stored in resolved_defs
    for ri in &checker.resolved_defs.impls {
        // <<< Collect Ok results >>>
        match check_single_impl_stub(checker, ri, collected_trait_defs) {
            Ok(impl_def) => collected_impls.push(impl_def),
            Err(e) => checker.report_error(e),
        }
    }
    // <<< Return the collected impls >>>
    collected_impls
}

/// Pass 2: Check impl method bodies.
/// Populates `typed_defs` with fully typed method bodies.
///
/// Preconditions: `check_definitions_pass1` and `check_impl_stubs_pass1` have run.
/// Postconditions: Method bodies are checked, errors added to `checker.errors`.
///                 `typed_defs.functions` is populated with method `TypedFunction`s.
pub(crate) fn generate_default_method_bodies_pass2(checker: &mut TypeChecker, typed_defs: &mut TypedDefinitions) {
    // Iterate over impls stored in resolved_defs
    // Clone the list to avoid borrowing issues if check_single_impl_body modifies checker state indirectly
    let impls_to_check = checker.resolved_defs.impls.clone();

    for ri in &impls_to_check {
        // Use a helper function to generate default bodies for a single impl
        if let Err(e) = generate_defaults_for_single_impl(checker, ri, typed_defs) {
            checker.report_error(e);
        }
    }
}

/// Checks the stub of a single `impl` block.
/// Resolves the implementing type, trait reference (if any), generic parameters,
/// checks associated type bindings, and checks method signatures against the trait definition.
fn check_single_impl_stub(checker: &mut TypeChecker, ri: &ResolvedImpl, collected_trait_defs: &[TraitDef]) -> TypeResult<ImplDef> {
    println!("Checking impl stub for symbol: {:?}", ri.impl_symbol);
    let mut checker_generic_params = Vec::new();
    let mut generic_param_scope = HashMap::new();
    let definition_context = Some((DefinitionKind::Impl, ri.impl_symbol));
    let mut local_errors: Vec<TypeError> = Vec::new(); // <<< Collect errors locally >>>

    // 1. Resolve generic parameters defined on the impl block.
    for resolved_gen_param in &ri.generic_params {
        match super::resolve::resolve_single_generic_param(checker, resolved_gen_param, definition_context) {
            Ok(checker_param) => {
                generic_param_scope.insert(checker_param.name.clone(), checker_param.id);
                checker_generic_params.push(checker_param);
            }
            Err(e) => local_errors.push(e), // <<< Push error >>>
        }
    }

    checker.enter_generic_scope(&checker_generic_params);

    // 2. Resolve the implementing type (`Self` type for this impl).
    let implementing_ty = match super::resolve::resolve_type_to_ty(checker, &ri.implementing_type) {
        Ok(ty) => ty,
        Err(e) => {
            local_errors.push(e); // <<< Push error >>>
            checker.exit_generic_scope();
            // Report collected errors before returning
            for err in local_errors { checker.report_error(err); }
            // <<< Return a dummy ImplDef on error >>>
            return Ok(ImplDef {
                id: ImplId(u32::MAX), // Placeholder ID
                impl_symbol: ri.impl_symbol,
                trait_symbol: None, // <<< Add trait_symbol: None >>>
                // <<< Use Ty::new(TyKind::Error) for implementing_ty >>>
                implementing_type: Ty::new(TyKind::Error),
                generic_params: Vec::new(),
                method_signatures: HashMap::new(),
                // <<< Use HashMap::new() for methods >>>
                methods: HashMap::new(),
                associated_type_bindings: HashMap::new(),
                default_method_impl_symbols: HashMap::new(),
                checked_default_bodies: HashMap::new(),
                span: ri.span,
            }); 
        }
    };
    println!("  Implementing type resolved to: {}", display_type(&implementing_ty));

    // Set the Self type in the checker context *before* resolving trait refs or method signatures.
    checker.set_self_type(implementing_ty.clone());

    // 3. Resolve the trait reference, if applicable.
    let mut trait_symbol_opt: Option<Symbol> = None;
    let mut trait_def: Option<TraitDef> = None; // Store TraitDef if found
    let mut temp_checker_trait_ref: Option<TraitRef> = None; // <<< Initialize here >>>

    if let Some(trait_symbol) = ri.trait_symbol {
        let mut get_trait_errors: Vec<TypeError> = Vec::new(); // <<< Temp vec for errors inside borrow >>>
        let trait_def_result = collected_trait_defs.iter().find(|td| td.trait_symbol == trait_symbol).cloned();
        match trait_def_result { // Check result after lookup
            Some(td) => {
                trait_def = Some(td.clone());
                trait_symbol_opt = Some(trait_symbol); // Store the symbol

                // Resolve trait type arguments provided in the impl block.
                let mut type_arguments = Vec::new();
                let resolved_args_opt = ri.trait_type_arguments.as_ref(); // Borrow resolved args

                if resolved_args_opt.map_or(0, |v| v.len()) != td.generic_params.len() {
                    local_errors.push(TypeError::GenericArgCountMismatch { 
                        kind: "Trait implementation".to_string(),
                        name: td.name.clone(),
                        expected: td.generic_params.len(),
                        found: resolved_args_opt.map_or(0, |v| v.len()),
                        span: ri.span,
                    });
                } else if let Some(resolved_args) = resolved_args_opt {
                    for arg_res_type in resolved_args {
                        let resolve_result = super::resolve::resolve_type_to_ty(checker, arg_res_type);
                        match resolve_result {
                            Ok(ty) => type_arguments.push(ty),
                            Err(e) => {
                                get_trait_errors.push(e); // <<< Push to temp vec >>>
                                type_arguments.push(Ty::new(TyKind::Error));
                            }
                        }
                    }
                }

                // Construct a temporary TraitRef for compatibility checks below
                // <<< Assign to the outer variable >>>
                temp_checker_trait_ref = Some(TraitRef {
                    trait_id: TraitId(u32::MAX), // Still use placeholder ID here
                    type_arguments,
                    span: ri.span, 
                });
                println!("  Resolved trait ref (temporary): {:?}", temp_checker_trait_ref);
            }
            None => {
                local_errors.push(TypeError::UnknownTrait { 
                    name: format!("Symbol {:?}", trait_symbol),
                    span: ri.span,
                });
            }
        }
        // <<< Report errors collected during borrow >>>
        local_errors.extend(get_trait_errors);
    }

    // 4. Resolve associated type bindings.
    let mut associated_type_bindings = HashMap::new();
    // Check against trait def only if it exists
    if let Some(ref td) = trait_def {
        for binding in &ri.associated_type_bindings {
            // Check if the assoc type symbol actually belongs to this trait
            if !td.associated_types.contains_key(&binding.assoc_type_symbol) {
                local_errors.push(TypeError::InternalError { // <<< Push error >>>
                     message: format!(
                         "Impl block for trait '{}' provides binding for non-associated type symbol {:?}",
                         td.name,
                         binding.assoc_type_symbol
                     ),
                     span: Some(binding.span),
                 });
                 continue;
            }

            match super::resolve::resolve_type_to_ty(checker, &binding.bound_type) {
                Ok(bound_ty) => {
                    // Get the associated type definition from the trait
                    let assoc_def = td.associated_types.get(&binding.assoc_type_symbol).unwrap(); // Should exist due to check above

                    // Substitute Self and trait generics in the bounds defined on the associated type
                    let mut trait_generic_subst = Substitution::new();
                    for (param, arg) in td.generic_params.iter().zip(temp_checker_trait_ref.as_ref().unwrap().type_arguments.iter()) {
                        trait_generic_subst.insert(param.id, arg.clone());
                    }

                    for bound_template in &assoc_def.bounds {
                        // Substitute Self in the bound template
                        let self_subst_bound = match super::substitute::substitute_self_in_trait_ref(bound_template, &implementing_ty, 10) {
                            Ok(b) => b,
                            Err(e) => { local_errors.push(e); continue; } // <<< Push error >>>
                        };
                        // Substitute trait generics in the bound template
                        let concrete_bound = super::substitute::substitute_trait_ref(&self_subst_bound, &trait_generic_subst);
                        
                        // Add the constraint: The type provided in the impl (`bound_ty`) must satisfy the concrete bound.
                        checker.add_constraint(bound_ty.clone(), concrete_bound);
                    }
                    
                    associated_type_bindings.insert(binding.assoc_type_symbol, bound_ty);
                }
                Err(e) => {
                    local_errors.push(e); // <<< Push error >>>
                    associated_type_bindings.insert(binding.assoc_type_symbol, Ty::new(TyKind::Error));
                }
            }
        }
        // Check for missing associated type bindings
        for (assoc_symbol, assoc_def) in &td.associated_types {
             if !associated_type_bindings.contains_key(assoc_symbol) && assoc_def.default.is_none() {
                 local_errors.push(TypeError::MissingAssocTypeImpl { // <<< Push error >>>
                    trait_name: td.name.clone(),
                    type_name: assoc_def.name.clone(),
                    span: ri.span, // Use span of the impl block
                 });
             }
        }
    }

    // 5. Check method signatures within the impl context.
    let mut checker_method_signatures: HashMap<Symbol, FunctionSignature> = HashMap::new();
    let mut missing_trait_methods: HashMap<Symbol, &crate::types::TraitMethod> = 
        trait_def.as_ref().map_or_else(HashMap::new, |td| td.methods.iter().map(|(k, v)| (*k, v)).collect()); // <<< Use map_or_else >>>

    // Find the function definition from the resolved_defs.functions list
    let get_resolved_fn = |func_symbol: &Symbol| -> Option<&ResolvedFunction> {
         checker.resolved_defs.functions.iter().find(|f| f.symbol == *func_symbol)
    };

    let mut checker_methods: HashMap<Symbol, Symbol> = HashMap::new(); // Trait symbol -> Impl symbol

    for assoc_func in &ri.methods {
        // Find the ResolvedFunction for this method symbol
        if let Some(method_rf) = get_resolved_fn(&assoc_func.func_symbol) {
            // Check the signature within the impl context (Self type is set)
            let impl_method_sig_option: Option<FunctionSignature>;
            if let Err(e) = super::defs::check_single_function_signature(checker, method_rf, Some(DefinitionKind::Impl)) {
                local_errors.push(e); // <<< Push error >>>
                impl_method_sig_option = None;
            } else {
                impl_method_sig_option = match super::defs::check_single_function_signature(checker, method_rf, Some(DefinitionKind::Impl)) {
                    Ok(Some(sig)) => Some(sig),
                    Ok(None) => { /* Error should be handled internally by check_single_function_signature */ None },
                    Err(e) => { local_errors.push(e); None } // <<< Push error >>>
                };
            }

            let impl_method_sig = if let Some(sig) = impl_method_sig_option {
                sig
            } else {
                continue; // Skip if signature resolution failed
            };

            let final_impl_sig = match super::substitute::substitute_signature_self(&impl_method_sig, &implementing_ty, 20) {
                Ok(sig) => sig,
                Err(e) => {
                    local_errors.push(e); // <<< Push error >>>
                    continue; // Cannot store if substitution fails
                }
            };

            checker_method_signatures.insert(assoc_func.func_symbol, final_impl_sig.clone());

            if let (Some(ref td), Some(trait_method_symbol)) = (&trait_def, assoc_func.trait_method_symbol) {
                 missing_trait_methods.remove(&trait_method_symbol); 

                 if let Some(_trait_method) = td.methods.get(&trait_method_symbol) {
                     let trait_method_sig = &_trait_method.signature;
                     // Temporarily collect errors from compatibility check
                     let mut compat_errors = Vec::new();
                     check_method_signature_compatibility(
                         checker,
                         td, 
                         trait_method_sig,
                         &final_impl_sig,
                         &temp_checker_trait_ref.as_ref().unwrap(), // Use the temporary TraitRef
                         &checker_generic_params, 
                         &implementing_ty,
                         &mut compat_errors, // Pass vec to collect errors
                     );
                     local_errors.extend(compat_errors); // Add compatibility errors
                     checker_methods.insert(trait_method_symbol, assoc_func.func_symbol);
                 } else {
                     local_errors.push(TypeError::InternalError { // <<< Push error >>>
                         message: format!("Impl method {:?} references unknown trait method symbol {:?}", assoc_func.func_symbol, trait_method_symbol),
                         span: Some(method_rf.span),
                     });
                 }
             } else if assoc_func.trait_method_symbol.is_some() {
                  local_errors.push(TypeError::InternalError { // <<< Push error >>>
                     message: format!("Method {:?} has trait_method_symbol but is not in a trait impl", assoc_func.func_symbol),
                     span: Some(method_rf.span),
                 });
             }
        } else {
            local_errors.push(TypeError::InternalError { // <<< Push error >>>
                message: format!("ResolvedFunction not found for impl method symbol {:?}", assoc_func.func_symbol),
                span: Some(ri.span),
            });
        }
    }

     if let Some(ref td) = trait_def {
         for (_missing_symbol, missing_method) in missing_trait_methods {
             if missing_method.default_body.is_none() {
                 local_errors.push(TypeError::MissingMethodImpl { // <<< Push error >>>
                     trait_name: td.name.clone(),
                     method_name: missing_method.name.clone(),
                     span: ri.span, 
                 });
             }
         }
     }

    // 6. Construct the ImplDef stub (without adding to repo yet).
    let impl_stub = ImplDef {
        // <<< Assign placeholder ID, repo will assign final one >>>
        id: ImplId(u32::MAX), // Placeholder ID
        impl_symbol: ri.impl_symbol,
        trait_symbol: trait_symbol_opt,
        implementing_type: implementing_ty.clone(),
        generic_params: checker_generic_params,
        method_signatures: checker_method_signatures,
        methods: checker_methods,
        associated_type_bindings, 
        // <<< Initialize default symbols map as empty >>>
        default_method_impl_symbols: HashMap::new(),
        checked_default_bodies: HashMap::new(), // Initialize empty
        span: ri.span,
    };

    // Clear Self type and exit generic scope
    checker.clear_self_type();
    checker.exit_generic_scope();

    // Report all collected errors at the end
    for err in local_errors {
        checker.report_error(err);
    }

    println!("Finished checking impl stub for symbol: {:?}", ri.impl_symbol);
    // <<< Return the constructed ImplDef stub >>>
    Ok(impl_stub)
}

/// Helper function to generate default method bodies for a *single* impl block.
/// This function is called during Pass 2.
/// It checks for trait methods with default bodies that were *not* explicitly implemented,
/// type checks the default body, generates a fresh symbol, creates a TypedFunction,
/// adds it to `typed_defs`, and updates the `ImplDef` in the `TraitRepository`.
fn generate_defaults_for_single_impl(
    checker: &mut TypeChecker,
    ri: &ResolvedImpl,
    typed_defs: &mut TypedDefinitions,
) -> TypeResult<()> {
    println!("[generate_defaults_for_single_impl] Checking impl block: {:?}", ri.impl_symbol);

    let mut collected_errors = Vec::new(); // Collect errors locally

    // Retrieve the ImplDef stub created in pass 1
    let impl_id = checker.trait_repo.get_impl_by_symbol(&ri.impl_symbol).map(|imp| imp.id);
    if impl_id.is_none() {
        // Should be caught in pass 1, but handle defensively
        return Err(TypeError::InternalError {
            message: format!("ImplDef not found for symbol {:?} during default body generation", ri.impl_symbol),
            span: Some(ri.span),
        });
    }
    let impl_id = impl_id.unwrap();

    // Clone necessary info from the ImplDef 
    let (implementing_ty, impl_generic_params, trait_symbol_opt, method_impl_map, _existing_default_symbols) = {
        let impl_def = checker.trait_repo.get_impl(impl_id).ok_or_else(|| TypeError::InternalError {
            message: format!("ImplDef clone failed for {:?} during default body generation", impl_id),
            span: Some(ri.span),
        })?;
        (
            impl_def.implementing_type.clone(),
            impl_def.generic_params.clone(),
            impl_def.trait_symbol, // Clone the Option<Symbol>
            impl_def.methods.clone(), // Map of trait method symbol -> explicit impl function symbol
            impl_def.default_method_impl_symbols.clone(), // Existing map (should be empty before this pass)
        )
    };

    // Self param symbol is not strictly needed here if we fetch it inside check_default_method_body, 
    // but it might be useful if we refactor further.
    let self_param_symbol = checker.env.get("self").map(|(_, sym)| sym).unwrap_or_else(Symbol::fresh);

    checker.enter_generic_scope(&impl_generic_params);
    checker.set_self_type(implementing_ty.clone());

    // <<< Look up TraitDef using trait_symbol_opt >>>
    let trait_def_opt = trait_symbol_opt.and_then(|sym| checker.trait_repo.get_trait_by_symbol(&sym));
    
    if let Some(trait_def) = trait_def_opt {
        // NOTE: Removed the `if let (Some(trait_ref), ...)` as we don't have trait_ref anymore.
        // We use trait_def directly obtained from the symbol lookup.
        
        // checker.enter_generic_scope(&impl_generic_params); // Already entered
        // checker.set_self_type(implementing_ty.clone()); // Already set

        let mut default_method_impl_symbols_update: HashMap<Symbol, Symbol> = HashMap::new();

        // --- Check for and generate missing default methods --- 
        for (trait_method_symbol, trait_method) in &trait_def.methods {
            // Check if this trait method was explicitly implemented in the impl block
            if !method_impl_map.contains_key(trait_method_symbol) {
                // It was NOT explicitly implemented. Check if it has a default body.
                if let Some(default_body_expr) = &trait_method.default_body {
                    println!("  [Default Check] Generating default impl for '{}' ({:?}) in impl {:?}", trait_method.name, trait_method_symbol, impl_id);

                    // Substitute Self in the original trait method signature
                    let concrete_trait_method_sig_result = super::substitute::substitute_signature_self(
                        &trait_method.signature,
                        &implementing_ty,
                        50, // Increased recursion depth
                    );

                    match concrete_trait_method_sig_result {
                        Ok(concrete_trait_method_sig) => {
                            // Check the default body using the concrete signature
                            let checked_default_body_result = check_default_method_body(
                                checker,
                                &mut collected_errors,
                                default_body_expr,
                                &concrete_trait_method_sig, 
                                &implementing_ty,
                                trait_method_symbol,
                                Some(self_param_symbol),
                            );

                            match checked_default_body_result {
                                Ok(Some(typed_body)) => {
                                    // Find the ResolvedFunction corresponding to the *trait method* symbol
                                    // to get parameter symbols, is_effectful, etc.
                                    let resolved_trait_fn_opt = checker.resolved_defs.functions.iter().find(|f| f.symbol == *trait_method_symbol);
                                    if let Some(resolved_trait_fn) = resolved_trait_fn_opt {
                                        // Create the TypedFunction for the generated default body
                                        let typed_fn = create_typed_function(
                                            &concrete_trait_method_sig,
                                            resolved_trait_fn,
                                            Some(typed_body.clone()),
                                            Some(self_param_symbol),
                                            &implementing_ty,
                                        );
                                        // Generate a fresh symbol for this specific implementation
                                        let fresh_symbol = Symbol::fresh();
                                        println!("    [Default Body Gen] Generated fresh symbol {:?} for default body of trait method {:?} (impl {:?})", fresh_symbol, trait_method_symbol, impl_id);
                                        // Add the TypedFunction to the global definitions map
                                        typed_defs.add_function(fresh_symbol, typed_fn);
                                        // Record the mapping: Trait Method Symbol -> Generated Body Symbol
                                        println!("    [Default Body Gen] Recording update: default_method_impl_symbols_update.insert({:?}, {:?})", trait_method_symbol, fresh_symbol);
                                        default_method_impl_symbols_update.insert(*trait_method_symbol, fresh_symbol);
                                    } else {
                                         collected_errors.push(TypeError::InternalError {
                                             message: format!("ResolvedFunction not found for trait method symbol {:?} when generating default body", trait_method_symbol),
                                             span: Some(trait_method.signature.span),
                                         });
                                    }
                                }
                                Ok(None) => { /* No body generated (e.g., error during check) */ }
                                Err(_e) => { /* Error already collected by helper */ }
                            }
                        } 
                        Err(e) => {
                            // Error substituting Self in signature
                            collected_errors.push(e);
                        }
                    }
                }
                // else: No default body provided, and not implemented -> Error should be caught in Pass 1 (check_single_impl_stub)
            }
        }

        // --- Update the ImplDef with the generated default method symbols --- 
        println!("  [Generate Defaults End] Before RefCell update: default_method_impl_symbols_update = {:?}", default_method_impl_symbols_update);
        if !default_method_impl_symbols_update.is_empty() {
            {
                // NOTE: This update now needs to happen in finalize_impl_defaults
                // We can remove this block entirely from this function.
                /* 
                if let Some(impl_def) = checker.trait_repo.get_impl_mut(impl_id) { // This won't work with &TraitRepository
                    println!("    [Generate Defaults Update] Updating ImplDef {:?} with default_method_impl_symbols_update: {:?}", impl_id, default_method_impl_symbols_update);
                    // Extend the map in the ImplDef
                    impl_def.default_method_impl_symbols.extend(default_method_impl_symbols_update.iter());
                    println!("    [Generate Defaults Update] After extend, impl_def.default_method_impl_symbols = {:?}", impl_def.default_method_impl_symbols);
                } else {
                    collected_errors.push(TypeError::InternalError {
                        message: format!("Failed to get mutable ImplDef {:?} for default symbol update", impl_id),
                        span: Some(ri.span),
                    });
                }
                */
            } // Mutable borrow dropped here
        }

        // --- Report Collected Errors --- 
        for e in collected_errors {
            checker.report_error(e);
        }

        // --- Cleanup --- 
        checker.clear_self_type();
        checker.exit_generic_scope();
        
        Ok(()) // Return Ok, errors reported to checker
    } else {
        // Not a trait impl, or trait lookup failed (shouldn't happen if Pass 2 added it)
        if trait_symbol_opt.is_some() {
            collected_errors.push(TypeError::InternalError {
                message: format!("TraitDef not found for symbol {:?} in generate_defaults", trait_symbol_opt.unwrap()),
                span: Some(ri.span)
            });
        }
        // Still need to cleanup scope even if not a trait impl
        checker.clear_self_type();
        checker.exit_generic_scope();
        Ok(())
    }
}

/// Helper to check the body of an explicitly defined impl method.
fn check_explicit_method_body(
    checker: &mut TypeChecker,
    errors: &mut Vec<TypeError>, // Collect errors here
    resolved_fn: &ResolvedFunction,
    signature: &FunctionSignature,
    implementing_ty: &Ty, // Needed for Self binding
    self_symbol_opt: Option<Symbol>, // Receive self symbol
) -> Result<Option<TypedExpr>, ()> { // Return Err(()) on fatal error inside
    let mut body_check_result: Option<TypedExpr> = None;

    if let Some(body_ast) = resolved_fn.body.as_ref() {
        let signature_clone = signature.clone(); 
        let parameters_clone = resolved_fn.parameters.clone();
        let body_clone = body_ast.clone();

        // --- Scope for environment modification ---
        let original_env = Arc::clone(&checker.env);
        checker.enter_scope();
        let gen_params_clone = signature_clone.generic_params.clone();
        checker.enter_generic_scope(&gen_params_clone);
        {
            let mut new_env = super::TypeEnvironment::with_parent(checker.env.clone());

                    if signature_clone.self_param.is_some() { 
                if let Some(self_symbol) = self_symbol_opt {
                     new_env.add("self".to_string(), self_symbol, implementing_ty.clone());
                } else {
                        // Cannot proceed without self symbol if required
                        errors.push(TypeError::InternalError{
                            message: "Self binding symbol not found for method body".to_string(),
                            span: Some(signature_clone.span)
                        });
                        // Restore env and exit scopes before returning Err
                        checker.exit_generic_scope();
                        checker.exit_scope();
                        checker.env = original_env; // Restore original env
                        return Err(()); 
                }
            }

            for (param_sig, param_res) in signature_clone.params.iter().zip(parameters_clone.iter()) {
                new_env.add(param_sig.name.clone(), param_res.symbol, param_sig.ty.clone());
                    }
            checker.env = std::sync::Arc::new(new_env);
        }
        // --- End Scope for environment modification ---

                    let expected_ret_ty = signature_clone.return_type.clone();
        
        // --- Check body expression (might borrow checker mutably) ---
        let check_result = crate::checker::expr::check_expr(checker, &body_clone, Some(&expected_ret_ty));
        // --- End body check ---
        
        // --- Process check result (might borrow checker mutably for unify) ---
        match check_result {
            Ok(typed_body_expr) => {
                let unify_result = checker.unify(&typed_body_expr.ty, &expected_ret_ty, body_ast.span);
                if !unify_result {
                     errors.push(TypeError::ReturnTypeMismatch{
                         expected: display_type(&expected_ret_ty),
                         found: display_type(&typed_body_expr.ty),
                         span: typed_body_expr.span, 
                         error: Box::new(TypeError::InternalError{ message: "Unification failed".to_string(), span: Some(body_ast.span)}) 
                     });
                }
                body_check_result = Some(typed_body_expr);
            }
            Err(e) => {
                errors.push(e); // Collect the error from check_expr
            }
        }
        // --- End process check result ---

        // --- Restore environment and scopes --- 
        checker.exit_generic_scope();
        checker.exit_scope();
        checker.env = original_env; // Restore original env
    }

    Ok(body_check_result)
}

/// Helper to check the body of a default trait method.
fn check_default_method_body(
    checker: &mut TypeChecker,
    errors: &mut Vec<TypeError>, // Collect errors here
    default_body_expr: &ResolvedExpr, 
    concrete_sig: &FunctionSignature, 
    implementing_ty: &Ty,
    trait_method_symbol: &Symbol, 
    self_symbol_opt: Option<Symbol>, 
) -> Result<Option<TypedExpr>, ()> { // Return Err(()) on fatal error
    let mut body_result: Option<TypedExpr> = None;

    // --- Scope for environment modification ---
    let original_env = Arc::clone(&checker.env);
    checker.enter_scope();
    let gen_params_clone = concrete_sig.generic_params.clone();
    checker.enter_generic_scope(&gen_params_clone);
    {
        let mut new_env = super::TypeEnvironment::with_parent(checker.env.clone());

        if concrete_sig.self_param.is_some() {
            if let Some(self_symbol) = self_symbol_opt {
                new_env.add("self".to_string(), self_symbol, implementing_ty.clone());
                } else {
                    errors.push(TypeError::InternalError{
                        message: "Self binding symbol not found for default method body".to_string(),
                        span: Some(concrete_sig.span)
                    });
                    // Restore env and exit scopes before returning Err
                    checker.exit_generic_scope();
                    checker.exit_scope();
                    checker.env = original_env; // Restore original env
                    return Err(()); // Indicate fatal error
            }
        }

        // Need immutable borrow of resolved_defs here
        let resolved_trait_fn_opt = checker.resolved_defs.functions.iter().find(|f| f.symbol == *trait_method_symbol);
        let resolved_params = resolved_trait_fn_opt.map_or(Vec::new(), |rf| rf.parameters.clone());
        // Drop immutable borrow implicitly

        for (i, param_sig) in concrete_sig.params.iter().enumerate() {
            let param_symbol = resolved_params.get(i).map_or_else(Symbol::fresh, |rp| rp.symbol);
            println!("      [Default Body Env Setup] Adding param: '{}' (Symbol {:?}) with concrete type: {}",
                     param_sig.name,
                     param_symbol,
                     display_type(&param_sig.ty));
            new_env.add(param_sig.name.clone(), param_symbol, param_sig.ty.clone());
                         }
        checker.env = std::sync::Arc::new(new_env);
    }
    // --- End Scope for environment modification ---

    let expected_ret_ty = concrete_sig.return_type.clone();

    // --- Check body expression (might borrow checker mutably) ---
    let check_result = crate::checker::expr::check_expr(checker, default_body_expr, Some(&expected_ret_ty));
    // --- End body check ---

    // --- Process check result (might borrow checker mutably for unify) ---
    match check_result {
                             Ok(typed_body_expr) => {
            let unify_result = checker.unify(&typed_body_expr.ty, &expected_ret_ty, default_body_expr.span);
            if !unify_result {
                 errors.push(TypeError::ReturnTypeMismatch{
                     expected: display_type(&expected_ret_ty),
                     found: display_type(&typed_body_expr.ty),
                     span: typed_body_expr.span,
                     error: Box::new(TypeError::InternalError{ message: "Unification failed".to_string(), span: Some(default_body_expr.span)}) 
                 });
            }
            body_result = Some(typed_body_expr);
        }
        Err(e) => {
            errors.push(e);
        }
                         }
    // --- End process check result ---
                         
    // --- Restore environment and scopes --- 
    checker.exit_generic_scope();
    checker.exit_scope();
    checker.env = original_env; // Restore original env

    Ok(body_result)
}

// Helper to create TypedFunction, ensuring self param is included if needed
fn create_typed_function(
    concrete_signature: &FunctionSignature,
    resolved_fn: &ResolvedFunction, 
    body: Option<TypedExpr>,
    self_symbol_opt: Option<Symbol>,
    implementing_ty: &Ty,
) -> TypedFunction {
    let mut params = Vec::new();

    if concrete_signature.self_param.is_some() {
        if let Some(self_symbol) = self_symbol_opt {
            params.push(TypedParameter {
                name: "self".to_string(),
                symbol: self_symbol,
                ty: implementing_ty.clone(),
                is_variadic: false,
                has_default: false,
                span: concrete_signature.span, 
            });
                } else {
            println!("Warning: Missing self symbol when creating TypedFunction for method {}", concrete_signature.name);
        }
    }

    params.extend(concrete_signature.params.iter().zip(resolved_fn.parameters.iter()).map(|(p, rp)| {
        TypedParameter {
            name: p.name.clone(),
            symbol: rp.symbol,
            ty: p.ty.clone(),
            is_variadic: rp.is_variadic, // Use resolver info 
            has_default: rp.has_default, // Use resolver info
            span: p.span,
        }
    }));

    TypedFunction {
        name: concrete_signature.name.clone(),
        params,
        return_type: concrete_signature.return_type.clone(),
        generic_params: concrete_signature.generic_params.iter().map(|gp| gp.name.clone()).collect(),
        span: concrete_signature.span,
        body,
        is_effectful: resolved_fn.is_effectful,
    }
}

/// Checks if an impl method's signature is compatible with the corresponding trait method signature.
/// This involves potentially complex substitution and unification.
fn check_method_signature_compatibility(
    checker: &mut TypeChecker,
    trait_def: &TraitDef,
    trait_method_sig: &FunctionSignature,
    impl_method_sig: &FunctionSignature,
    impl_trait_ref: &TraitRef,
    _impl_generic_params: &[GenericParamDef],
    implementing_ty: &Ty,
    errors: &mut Vec<TypeError>, // <<< Add error vector argument >>>
) {
    // 1. Substitute Self in the trait method signature using the impl's implementing type
    let expected_sig_after_self = match super::substitute::substitute_signature_self(trait_method_sig, implementing_ty, 20) {
        Ok(sig) => sig,
        Err(e) => {
            errors.push(e); // <<< Push error >>>
            return;
        }
    };

    // 2. Instantiate trait generic parameters in the expected signature using the impl's trait ref arguments
    let mut trait_generic_subst = Substitution::new();
    if trait_def.generic_params.len() != impl_trait_ref.type_arguments.len() {
         errors.push(TypeError::GenericArgCountMismatch { // <<< Push error >>>
             kind: "Trait implementation".to_string(),
             name: trait_def.name.clone(),
             expected: trait_def.generic_params.len(),
             found: impl_trait_ref.type_arguments.len(),
             span: impl_trait_ref.span, // Span of the trait reference in the impl
         });
         return;
    }
    for (gp_def, ty_arg) in trait_def.generic_params.iter().zip(impl_trait_ref.type_arguments.iter()) {
        trait_generic_subst.insert(gp_def.id, ty_arg.clone());
    }

    let expected_sig_final = FunctionSignature {
        params: expected_sig_after_self.params.iter().map(|p| ParamType {
            ty: p.ty.apply_subst(&trait_generic_subst),
            ..p.clone()
        }).collect(),
        return_type: expected_sig_after_self.return_type.apply_subst(&trait_generic_subst),
        generic_params: expected_sig_after_self.generic_params.clone(), // Method generics are separate
        ..expected_sig_after_self
    };

    // 3. Compare parameter counts (including self)
    if expected_sig_final.self_param.is_some() != impl_method_sig.self_param.is_some() {
        errors.push(TypeError::MethodParamMismatch {
            trait_name: trait_def.name.clone(),
            method_name: trait_method_sig.name.clone(),
            param_index: 0, // Indicate 'self'
            expected: if expected_sig_final.self_param.is_some() { "self" } else { "no self" }.to_string(),
            found: if impl_method_sig.self_param.is_some() { "self" } else { "no self" }.to_string(),
            span: impl_method_sig.span, // Span of the impl method
            error: Box::new(TypeError::InternalError { message: "Self parameter mismatch".to_string(), span: None }),
        });
        return;
    }
    if expected_sig_final.params.len() != impl_method_sig.params.len() {
        errors.push(TypeError::ParamCountMismatch {
             name: impl_method_sig.name.clone(),
             expected: expected_sig_final.params.len(),
             found: impl_method_sig.params.len(),
             span: impl_method_sig.span,
        });
        return;
    }

    // 4. Compare method generic parameter counts
    if expected_sig_final.generic_params.len() != impl_method_sig.generic_params.len() {
         errors.push(TypeError::GenericParamCountMismatch {
             kind: "Method".to_string(),
             name: impl_method_sig.name.clone(),
             expected: expected_sig_final.generic_params.len(),
             found: impl_method_sig.generic_params.len(),
             span: impl_method_sig.span,
         });
         return;
    }

    // 5. Instantiate method generic parameters for the impl method signature
    let mut impl_method_generic_subst = Substitution::new();
    let fresh_vars_for_impl: Vec<_> = (0..impl_method_sig.generic_params.len())
        .map(|_| checker.fresh_var())
        .collect();

    for (gp_def, fresh_var) in impl_method_sig.generic_params.iter().zip(fresh_vars_for_impl.iter()) {
        impl_method_generic_subst.insert(gp_def.id, fresh_var.clone());
    }

    let instantiated_impl_sig = FunctionSignature {
         params: impl_method_sig.params.iter().map(|p| ParamType {
             ty: p.ty.apply_subst(&impl_method_generic_subst),
             ..p.clone()
         }).collect(),
         return_type: impl_method_sig.return_type.apply_subst(&impl_method_generic_subst),
         ..impl_method_sig.clone()
    };

    // <<< ADDED: Substitute Self in instantiated_impl_sig >>>
    let final_impl_sig = match super::substitute::substitute_signature_self(&instantiated_impl_sig, implementing_ty, 20) {
        Ok(sig) => sig,
        Err(e) => {
            errors.push(e); // <<< Push error >>>
            return; // Cannot compare if substitution fails
        }
    };

    // 5b. Compare bounds and add constraints for impl method generics
    for ((trait_gp_def, impl_gp_def), fresh_var) in expected_sig_final.generic_params.iter()
                                                   .zip(impl_method_sig.generic_params.iter())
                                                   .zip(fresh_vars_for_impl.iter()) {

        // Check number of bounds
        if trait_gp_def.bounds.len() != impl_gp_def.bounds.len() {
            errors.push(TypeError::GenericBoundMismatch {
                kind: "Method generic parameter".to_string(),
                param_name: impl_gp_def.name.clone(),
                trait_name: trait_def.name.clone(),
                method_name: trait_method_sig.name.clone(),
                span: impl_gp_def.span, // Or impl_method_sig.span?
                message: format!("Expected {} bounds, found {}", trait_gp_def.bounds.len(), impl_gp_def.bounds.len()),
            });
            // Continue checking other bounds even if count mismatches
        }

        // Add constraints for the *impl* method's bounds
        for (_bound_index, bound) in impl_gp_def.bounds.iter().enumerate() {
            // Substitute Self within the bound using the implementing type
            let concrete_bound = match super::substitute::substitute_self_in_trait_ref(bound, implementing_ty, 10) {
                Ok(b) => b,
                Err(e) => { errors.push(e); bound.clone() }
            };
            // Substitute using the impl method's generic substitution map (fresh_vars_for_impl -> impl_method_generic_subst)
            let instantiated_bound = super::substitute::substitute_trait_ref(&concrete_bound, &impl_method_generic_subst);
            checker.add_constraint(fresh_var.clone(), instantiated_bound.clone());

            // --- End Incomplete Check --- 

            // Full entailment check requires more complex trait system queries.
        }
    }

    // 6. Unify parameter types
    for (i, (expected_param, impl_param)) in expected_sig_final.params.iter().zip(final_impl_sig.params.iter()).enumerate() {
         if !checker.unify(&expected_param.ty, &impl_param.ty, impl_param.span) {
             errors.push(TypeError::MethodParamMismatch {
                 trait_name: trait_def.name.clone(),
                 method_name: trait_method_sig.name.clone(),
                 param_index: i + 1, // 1-based index for user messages
                 expected: display_type(&expected_param.ty),
                 found: display_type(&impl_param.ty),
                 span: impl_param.span,
                 error: Box::new(TypeError::TypeMismatch { // Wrap unification failure
                     expected: display_type(&expected_param.ty),
                     found: display_type(&impl_param.ty),
                     span: impl_param.span,
                 }),
             });
             // Continue checking other parameters even if one fails
         }
    }

    // 7. Unify return types
    if !checker.unify(&expected_sig_final.return_type, &final_impl_sig.return_type, impl_method_sig.span) { // Use overall span
         errors.push(TypeError::MethodReturnTypeMismatch {
             trait_name: trait_def.name.clone(),
             method_name: trait_method_sig.name.clone(),
             expected: display_type(&expected_sig_final.return_type),
             found: display_type(&final_impl_sig.return_type),
             span: impl_method_sig.span, // Span of the impl method's return type or whole signature
             error: Box::new(TypeError::TypeMismatch { // Wrap unification failure
                 expected: display_type(&expected_sig_final.return_type),
                 found: display_type(&final_impl_sig.return_type),
                 span: impl_method_sig.span,
             }),
         });
    }
}

// <<< NEW FUNCTION FOR PASS 3 >>>
/// Pass 3: Check the bodies of explicitly defined methods in impl blocks.
/// Relies on Pass 2 having generated default method symbols.
pub(crate) fn check_explicit_impl_bodies_pass3(checker: &mut TypeChecker, typed_defs: &mut TypedDefinitions) {
    // Iterate over impls stored in resolved_defs
    let impls_to_check = checker.resolved_defs.impls.clone();

    for ri in &impls_to_check {
        if let Err(e) = check_explicit_methods_for_single_impl(checker, ri, typed_defs) {
            checker.report_error(e);
        }
    }
}

/// Helper function to check explicitly defined method bodies for a *single* impl block.
/// This function is called during Pass 3.
fn check_explicit_methods_for_single_impl(
    checker: &mut TypeChecker,
    ri: &ResolvedImpl,
    typed_defs: &mut TypedDefinitions,
) -> TypeResult<()> {
    println!("[check_explicit_methods_for_single_impl] Checking impl block: {:?}", ri.impl_symbol);

    let mut collected_errors = Vec::new(); // Collect errors locally

    // Retrieve the ImplDef
    let impl_id = checker.trait_repo.get_impl_by_symbol(&ri.impl_symbol).map(|imp| imp.id);
    if impl_id.is_none() {
        return Err(TypeError::InternalError {
            message: format!("ImplDef not found for symbol {:?} during explicit body checking", ri.impl_symbol),
            span: Some(ri.span),
        });
    }
    let impl_id = impl_id.unwrap();

    // Clone necessary info 
    let (implementing_ty, impl_generic_params) = {
        let impl_def = checker.trait_repo.get_impl(impl_id).ok_or_else(|| TypeError::InternalError {
            message: format!("ImplDef clone failed for {:?} during explicit body checking", impl_id),
            span: Some(ri.span),
        })?;
        (
            impl_def.implementing_type.clone(),
            impl_def.generic_params.clone(),
        )
    };

    let self_param_symbol = checker.env.get("self").map(|(_, sym)| sym).unwrap_or_else(Symbol::fresh);

    checker.enter_generic_scope(&impl_generic_params);
    checker.set_self_type(implementing_ty.clone());

    // --- Iterate over explicitly defined methods --- 
    for assoc_func in &ri.methods {
        let impl_func_symbol = assoc_func.func_symbol;
        let resolved_fn_opt = checker.resolved_defs.functions.iter().find(|f| f.symbol == impl_func_symbol);

        if let Some(resolved_fn) = resolved_fn_opt {
            // Retrieve the signature stored in the ImplDef during Pass 1
            let stored_sig = {
                checker.trait_repo.get_impl(impl_id)
                    .and_then(|imp| imp.method_signatures.get(&impl_func_symbol))
                    .cloned()
            };

            if let Some(mut signature) = stored_sig {
                // Instantiate impl-level generic parameters with fresh variables if needed?
                // Or assume they are handled correctly by the generic scope?
                // Let's assume the signature already reflects the correct generic context.
                // Example: impl<T> Foo for Bar<T> { fn baz(&self, x: T) {} } -> signature should have T as generic
                // Apply substitution for impl-level generics
                let mut impl_generic_subst = Substitution::new();
                for gp in &impl_generic_params {
                    let fresh_var = checker.fresh_var();
                    impl_generic_subst.insert(gp.id, fresh_var);
                }
                signature.apply_subst_mut(&impl_generic_subst);

                // Check the body using the (potentially instantiated) signature
                let typed_body_opt_result = check_explicit_method_body(
                    checker,
                    &mut collected_errors,
                    resolved_fn,
                    &signature, // Pass the potentially substituted signature
                    &implementing_ty,
                    Some(self_param_symbol),
                );

                match typed_body_opt_result { 
                    Ok(typed_body_opt) => {
                        // Create the final TypedFunction for this explicit method
                        let typed_fn = create_typed_function(
                            &signature, // Use the same substituted signature
                            resolved_fn,
                            typed_body_opt,
                            Some(self_param_symbol),
                            &implementing_ty,
                        );
                         // Add it to the global definitions map using the *original* impl function symbol
                         typed_defs.add_function(impl_func_symbol, typed_fn);
                    }
                    Err(e) => {
                        // Error collected by helper
                    }
                }
            } else {
                 collected_errors.push(TypeError::InternalError {
                     message: format!("Signature missing for impl method {:?} in ImplDef {:?}", impl_func_symbol, impl_id),
                     span: Some(resolved_fn.span),
                 });
            }
        } else {
             collected_errors.push(TypeError::InternalError {
                 message: format!("ResolvedFunction missing for impl method symbol {:?}", impl_func_symbol),
                 span: Some(ri.span),
             });
        }
    }

    // --- Report Collected Errors --- 
    for e in collected_errors {
        checker.report_error(e);
    }

    // --- Cleanup --- 
    checker.clear_self_type();
    checker.exit_generic_scope();
    
    Ok(())
}

// <<< NEW FUNCTION FOR PASS 2 >>>
/// Pass 2: Finalize impls by generating symbols for default methods.
/// Mutates the `TraitRepository` by adding mappings to `default_method_impl_symbols`.
/// Does *not* check method bodies.
pub(crate) fn finalize_impl_defaults(
    repo: &mut TraitRepository,
    resolved_defs: &ResolvedDefinitions,
) {
    // Need to collect updates separately because we can't easily mutate `repo.impls` while iterating traits
    let mut all_updates: HashMap<ImplId, HashMap<Symbol, Symbol>> = HashMap::new();

    // Iterate through all *traits* to find methods with defaults
    for trait_def in repo.all_traits() { // Use immutable borrow here
        for (trait_method_symbol, trait_method) in &trait_def.methods {
            // Check if this method has a default body
            if trait_method.default_body.is_some() {
                // Find all impls of this specific trait
                let impl_ids = repo.get_impls_for_trait(trait_def.id).collect::<Vec<_>>();
                for impl_id in impl_ids {
                    // Check if this default method was *not* explicitly implemented in this impl
                    let is_explicitly_implemented = repo
                        .get_impl(impl_id)
                        .map_or(false, |imp| imp.methods.contains_key(trait_method_symbol));

                    if !is_explicitly_implemented {
                        // Generate a fresh symbol for this specific impl's version of the default method
                        let generated_symbol = Symbol::fresh();
                        println!(
                            "  [finalize_impl_defaults] Generating symbol {:?} for default method {:?} in impl {:?}",
                            generated_symbol, trait_method_symbol, impl_id
                        );
                        // Store the update required for this impl
                        all_updates
                            .entry(impl_id)
                            .or_default()
                            .insert(*trait_method_symbol, generated_symbol);
                    }
                }
            }
        }
    }

    // Apply all collected updates to the mutable repository
    for (impl_id, updates) in all_updates {
        if let Some(impl_def) = repo.get_impl_mut(impl_id) {
            impl_def.default_method_impl_symbols.extend(updates);
        }
    }
}

// <<< NEW FUNCTION FOR CHECKING DEFAULT BODIES IN PASS 3 >>>
/// Pass 3: Check the bodies of default trait methods used by impl blocks.
/// Relies on Pass 2 (`finalize_impl_defaults`) having populated `default_method_impl_symbols`.
pub(crate) fn check_default_impl_bodies_pass3(checker: &mut TypeChecker, typed_defs: &mut TypedDefinitions) {
    // Collect impls to check to avoid holding borrow on checker.trait_repo
    let all_impl_ids: Vec<ImplId> = checker.trait_repo.all_impls().map(|imp| imp.id).collect();

    for impl_id in all_impl_ids {
        let mut collected_errors = Vec::new(); // Errors for this impl

        // Clone necessary info from the impl def
        let (implementing_ty, impl_generic_params, trait_symbol_opt, default_symbols) = {
            let impl_def = checker.trait_repo.get_impl(impl_id).unwrap(); // Should exist
            (
                impl_def.implementing_type.clone(),
                impl_def.generic_params.clone(),
                impl_def.trait_symbol, // Clone Option<Symbol>
                impl_def.default_method_impl_symbols.clone(),
            )
        };

        // <<< Look up TraitDef using trait_symbol_opt >>>
        let trait_def_opt = trait_symbol_opt.and_then(|sym| checker.trait_repo.get_trait_by_symbol(&sym));
        
        if let Some(trait_def) = trait_def_opt {
             // NOTE: Removed the `if let (Some(trait_ref), ...)`
             
            // <<< SET CONTEXT before checking bodies for this impl >>>
            checker.set_default_body_context(trait_def.id, impl_id);

            checker.enter_generic_scope(&impl_generic_params);
            checker.set_self_type(implementing_ty.clone());

            // Create self symbol for the context
            let self_param_symbol = checker.env.get("self").map(|(_, sym)| sym).unwrap_or_else(Symbol::fresh);

            // Iterate through the generated default symbols for this impl
            for (trait_method_symbol, generated_symbol) in &default_symbols {
                if let Some(trait_method) = trait_def.methods.get(trait_method_symbol) {
                    if let Some(default_body_expr) = &trait_method.default_body {
                        println!(
                            "  [check_default_impl_bodies_pass3] Checking default body for trait method {:?} (impl {:?}, using generated symbol {:?})",
                            trait_method_symbol, impl_id, generated_symbol
                        );
                        // Substitute Self in the original trait method signature
                        let concrete_sig_result = substitute_signature_self(
                            &trait_method.signature,
                            &implementing_ty,
                            50,
                        );

                        match concrete_sig_result {
                            Ok(concrete_sig) => {
                                // Check the body using the helper
                                let checked_body_result = check_default_method_body(
                                    checker,
                                    &mut collected_errors,
                                    default_body_expr,
                                    &concrete_sig, // Use concrete sig for context
                                    &implementing_ty,
                                    trait_method_symbol, // Pass trait symbol for context?
                                    Some(self_param_symbol),
                                );

                                match checked_body_result {
                                    Ok(Some(typed_body)) => {
                                        // Find ResolvedFunction for the *trait method* to get param symbols etc.
                                        let resolved_trait_fn = checker.resolved_defs.functions.iter()
                                            .find(|f| f.symbol == *trait_method_symbol)
                                            .ok_or_else(|| TypeError::InternalError {
                                                message: format!("ResolvedFunction missing for trait method {:?}", trait_method_symbol),
                                                span: Some(trait_method.signature.span),
                                            });
                                        
                                        match resolved_trait_fn {
                                            Ok(rtf) => {
                                                // Create TypedFunction using the GENERATED symbol
                                                let typed_fn = create_typed_function(
                                                    &concrete_sig, // Use the concrete sig from trait method
                                                    rtf, // Use trait method's ResolvedFunction
                                                    Some(typed_body),
                                                    Some(self_param_symbol),
                                                    &implementing_ty,
                                                );
                                                typed_defs.add_function(*generated_symbol, typed_fn);
                                                println!("    -> Added TypedFunction for generated symbol {:?}", generated_symbol);
                                            }
                                            Err(e) => collected_errors.push(e),
                                        }
                                    }
                                    Ok(None) => { /* Error during check, already collected */ }
                                    Err(()) => { /* Fatal error in helper */ }
                                }
                            }
                            Err(e) => collected_errors.push(e), // Error substituting Self
                        }
                    } else {
                        // Should not happen if finalize generated a symbol
                        collected_errors.push(TypeError::InternalError {
                            message: format!("Default body missing for trait method {:?} despite generated symbol", trait_method_symbol),
                            span: Some(trait_method.signature.span),
                        });
                    }
                } else {
                     // Should not happen if finalize generated a symbol
                     collected_errors.push(TypeError::InternalError {
                         message: format!("Trait method {:?} not found in TraitDef despite generated symbol", trait_method_symbol),
                         span: checker.trait_repo.get_impl(impl_id).map(|imp| imp.span), // Use impl span
                     });
                }
            }
            // Clean up scope for this impl
            checker.clear_self_type();
            checker.exit_generic_scope();
            // <<< CLEAR CONTEXT after checking bodies for this impl >>>
            checker.clear_default_body_context();
        } else {
            // Not a trait impl, or trait lookup failed.
            if trait_symbol_opt.is_some() && default_symbols.is_empty() {
                // If it claimed to be a trait impl but trait wasn't found, report internal error.
                // But only if there weren't any default symbols generated (which implies trait *was* found earlier)
                collected_errors.push(TypeError::InternalError {
                    message: format!("TraitDef not found for symbol {:?} in check_defaults_pass3", trait_symbol_opt.unwrap()),
                    span: checker.trait_repo.get_impl(impl_id).map(|imp| imp.span), // Use impl span
                });
            }
            // No cleanup needed here as scope wasn't entered if trait_def was None
        }
        // Report errors collected for this impl
        for e in collected_errors {
            checker.report_error(e);
        }
    }
}