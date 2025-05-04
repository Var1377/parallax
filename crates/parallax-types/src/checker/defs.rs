// src/checker/defs.rs
//! Type checking pass 1: Checking definition signatures (structs, enums, functions).

use super::TypeChecker;
use crate::error::{TypeError, TypeResult};
use crate::types::{
    TypeDef, StructDef, EnumDef, FunctionSignature, EnumVariant, Field, ParamType,
    Ty, TyKind, TypedDefinitions, TypedFunction, TypedParameter,
    GenericParamDef, TraitDef, AssociatedTypeDef
};
use parallax_resolve::types::{
    Symbol, ResolvedStruct, ResolvedEnum, ResolvedFunction, ResolvedTrait,
    ResolvedAssociatedFunction
};
use parallax_resolve::definitions::DefinitionKind;
use parallax_resolve::ResolvedType;
use std::collections::HashMap;
use crate::types::*;
use miette::SourceSpan;
use crate::context::{TraitId, ImplId};

/// Pass 1: Iterate through resolved definitions and check their signatures.
/// Populates `checker.type_ctx` and `checker.trait_repo` (via `check_impl_stubs_pass1`).
/// Does *not* check function bodies or resolve traits yet.
///
/// Preconditions: `checker` is initialized.
/// Postconditions: `checker.type_ctx` contains signatures for structs, enums, functions.
///                 Errors related to signature checking are added to `checker.errors`.
pub(crate) fn check_definitions_pass1(checker: &mut TypeChecker) -> (Vec<StructDef>, Vec<EnumDef>, Vec<(Symbol, FunctionSignature)>, Vec<TraitDef>) {
    // Clone Arc to avoid borrowing issues if ResolvedDefinitions needs mutation later (unlikely but safer)
    let resolved_defs = checker.resolved_defs.clone();
    let mut collected_structs = Vec::new();
    let mut collected_enums = Vec::new();
    let mut collected_functions = Vec::new(); // Renamed from collected_standalone_fns
    let mut collected_traits = Vec::new();

    // Process structs
    for rs in &resolved_defs.structs {
        match check_single_struct_signature(checker, rs) {
            Ok(struct_def) => collected_structs.push(struct_def),
            Err(e) => checker.report_error(e),
        }
    }

    // Process enums
    for re in &resolved_defs.enums {
        match check_single_enum_signature(checker, re) {
            Ok(enum_def) => collected_enums.push(enum_def),
            Err(e) => checker.report_error(e),
        }
    }

    // Process traits
    for rt in &resolved_defs.traits {
        match check_single_trait_signature(checker, rt, &resolved_defs) {
            Ok(trait_def) => collected_traits.push(trait_def),
            Err(e) => checker.report_error(e),
        }
    }

    // Process ALL functions (standalone and associated) to collect signatures
    for rf in &resolved_defs.functions {
        // Determine parent_kind based on whether it belongs to a trait or impl
        let parent_kind = if resolved_defs.traits.iter().any(|t| t.methods.iter().any(|m| m.func_symbol == rf.symbol)) {
            Some(DefinitionKind::Trait)
        } else if resolved_defs.impls.iter().any(|i| i.methods.iter().any(|m| m.func_symbol == rf.symbol)) {
            Some(DefinitionKind::Impl)
        } else {
            None // Standalone function
        };

        // Check signature regardless of whether it's associated or standalone
        match check_single_function_signature(checker, rf, parent_kind) {
            Ok(Some(sig)) => collected_functions.push((rf.symbol, sig)), // Collect (Symbol, FunctionSignature)
            Ok(None) => { /* Should not happen if check_single_function_signature returns Some for standalone */ }
            Err(e) => checker.report_error(e),
        }
    }

    (collected_structs, collected_enums, collected_functions, collected_traits)
}

/// Pass 2: Check function bodies after all signatures are known.
/// Populates `typed_defs` with fully typed function bodies.
///
/// Preconditions: `check_definitions_pass1` and `check_impl_stubs_pass1` have run.
///                `checker.type_ctx` and `checker.trait_repo` are populated.
/// Postconditions: Function bodies are checked, errors added to `checker.errors`.
///                 `typed_defs.functions` is populated with `TypedFunction` including bodies.
pub(crate) fn check_function_bodies_pass3(checker: &mut TypeChecker, typed_defs: &mut TypedDefinitions) {
     let resolved_defs = checker.resolved_defs.clone(); // Clone Arc

     // Iterate through resolved function definitions again for body checking
     for rf in &resolved_defs.functions {
         // Skip functions associated with traits/impls - handled by check_impl_bodies_pass2
         let is_associated = resolved_defs.traits.iter().any(|t| t.methods.iter().any(|m| m.func_symbol == rf.symbol))
             || resolved_defs.impls.iter().any(|i| i.methods.iter().any(|m| m.func_symbol == rf.symbol));

         if !is_associated {
             // --- Check Body ---
             // Retrieve the signature stored in pass 1
             match checker.type_ctx.get_function_sig(&rf.symbol, rf.span) {
                 Ok(signature) => {
                     // Create TypedFunction structure (start with no body)
                     let mut typed_function = TypedFunction {
                         name: rf.name.clone(), // Use name from ResolvedFunction
                         // Convert checker::ParamType to hir::TypedParameter
                         params: signature.params.iter().zip(rf.parameters.iter()).map(|(p, rp)| TypedParameter {
                             name: p.name.clone(),
                             // Use symbol from ResolvedParameter
                             symbol: rp.symbol,
                             ty: p.ty.clone(),
                             is_variadic: false, // Placeholder
                             has_default: false, // Placeholder
                             span: p.span,
                         }).collect(),
                         return_type: signature.return_type.clone(),
                         generic_params: signature.generic_params.iter().map(|gp| gp.name.clone()).collect(),
                         span: signature.span,
                         body: None, // Will be filled below
                         is_effectful: rf.is_effectful, // Use resolver info
                     };

                     if let Some(body_ast) = rf.body.as_ref() {
                         // Clone necessary data before modifying the environment/checker state
                         let signature_clone = signature.clone();
                         let parameters_clone = rf.parameters.clone();
                         let body_clone = body_ast.clone(); // Clone the ResolvedExpr body

                         // Enter scopes FIRST
                         checker.enter_scope(); 
                         let gen_params_clone = signature_clone.generic_params.clone();
                         checker.enter_generic_scope(&gen_params_clone);

                         // Create and set the environment for the function body
                         {
                             // Create new env with the CURRENT checker env (which includes the new scope) as parent
                             let mut new_env = super::TypeEnvironment::with_parent(checker.env.clone()); 

                             // Add parameter bindings to the new environment
                             for (param_sig, param_res) in signature_clone.params.iter().zip(parameters_clone.iter()) {
                                 // Use the new_env directly
                                 new_env.add(param_sig.name.clone(), param_res.symbol, param_sig.ty.clone());
                             }
                             
                             // Update the checker's environment to this new one for body checking
                             checker.env = std::sync::Arc::new(new_env); 
                         }

                         // Check the body expression using the cloned body and signature
                         let expected_ret_ty = signature_clone.return_type.clone(); // Use cloned signature
                         match super::expr::check_expr(checker, &body_clone, Some(&expected_ret_ty)) { // Pass cloned body
                             Ok(typed_body_expr) => {
                                 // Unify body type with expected return type
                                 // Pass the original body_ast span here
                                 if !checker.unify(&typed_body_expr.ty, &expected_ret_ty, body_ast.span) {
                                     // Error already reported by unify
                                 }
                                 typed_function.body = Some(typed_body_expr);
                             }
                             Err(_e) => {
                                 checker.report_error(_e);
                                 // Leave body as None
                             }
                         }

                         // Exit scopes (checker is mutably borrowed again here)
                         checker.exit_generic_scope();
                         checker.exit_scope();
                         // Environment is implicitly restored because the Arc was replaced
                     } else {
                         // Function has no body (e.g., external/intrinsic)
                     }

                     // Add the fully typed function (with or without body) to the final output
                     typed_defs.add_function(rf.symbol, typed_function);
                 }
                 Err(_e) => {
                     // Signature wasn't found/valid in pass 1, error already reported.
                     // We might want to log this specific case as well.
                     println!("Skipping body check for fn {:?} due to missing signature.", rf.symbol);
                 }
             }
         }
     }
}

/// Check the signature of a single struct.
pub(crate) fn check_single_struct_signature(checker: &mut TypeChecker, rs: &ResolvedStruct) -> TypeResult<StructDef> {
    println!("Checking struct signature: {}", rs.name);
    let mut generic_param_scope = HashMap::new();
    let definition_context = Some((DefinitionKind::Struct, rs.symbol));
    let mut checker_generic_params = Vec::new();

    // 1. Resolve generic parameters first, adding them to a temporary map for the scope.
    for resolved_gen_param in &rs.generic_params {
        match super::resolve::resolve_single_generic_param(checker, resolved_gen_param, definition_context) {
            Ok(checker_param) => {
                // Map param name to TypeId for the scope
                generic_param_scope.insert(checker_param.name.clone(), checker_param.id);
                checker_generic_params.push(checker_param);
            }
            Err(e) => checker.report_error(e),
        }
    }

    // Enter the generic scope for resolving field types.
    checker.enter_generic_scope(&checker_generic_params);

    // 2. Resolve field types within the generic scope.
    let mut checker_fields = Vec::new();
    for resolved_field in &rs.fields {
        match super::resolve::resolve_type_to_ty(checker, &resolved_field.field_type) {
            Ok(ty) => {
                checker_fields.push(Field {
                    name: resolved_field.name.clone(),
                    symbol: resolved_field.symbol,
                    ty,
                    span: resolved_field.span,
                });
            }
            Err(e) => checker.report_error(e),
        }
    }

    // Exit the generic scope.
    checker.exit_generic_scope();

    // 3. Create the StructDef and add it to the TypeContext.
    let struct_def = StructDef {
        name: rs.name.clone(),
        symbol: rs.symbol,
        generic_params: checker_generic_params,
        fields: checker_fields,
        span: rs.span,
    };

    Ok(struct_def)
}

/// Check the signature of a single enum.
pub(crate) fn check_single_enum_signature(checker: &mut TypeChecker, re: &ResolvedEnum) -> TypeResult<EnumDef> {
    println!("Checking enum signature: {}", re.name);
    let mut generic_param_scope = HashMap::new();
    let definition_context = Some((DefinitionKind::Enum, re.symbol));
    let mut checker_generic_params = Vec::new();

    // 1. Resolve generic parameters.
    for resolved_gen_param in &re.generic_params {
        match super::resolve::resolve_single_generic_param(checker, resolved_gen_param, definition_context) {
            Ok(checker_param) => {
                generic_param_scope.insert(checker_param.name.clone(), checker_param.id);
                checker_generic_params.push(checker_param);
            }
            Err(e) => checker.report_error(e),
        }
    }

    checker.enter_generic_scope(&checker_generic_params);

    // 2. Resolve variant signatures within the generic scope.
    let mut checker_variants = Vec::new();
    for resolved_variant in &re.variants {
         let (variant_symbol, variant_name, variant_span) = match resolved_variant {
             parallax_resolve::types::ResolvedEnumVariant::Unit { symbol, name, span } => (*symbol, name.clone(), *span),
             parallax_resolve::types::ResolvedEnumVariant::Tuple { symbol, name, span, .. } => (*symbol, name.clone(), *span),
             parallax_resolve::types::ResolvedEnumVariant::Struct { symbol, name, span, .. } => (*symbol, name.clone(), *span),
         };

         let mut checker_fields = Vec::new();
         match resolved_variant {
            parallax_resolve::types::ResolvedEnumVariant::Unit { .. } => {},
            parallax_resolve::types::ResolvedEnumVariant::Tuple { fields: resolved_field_types, .. } => {
                 for (i, resolved_field_ty) in resolved_field_types.iter().enumerate() {
                     match super::resolve::resolve_type_to_ty(checker, resolved_field_ty) {
                         Ok(field_ty) => {
                             // Placeholder symbol for tuple fields, might need resolver support?
                             let field_symbol = Symbol::new(u32::MAX); // Placeholder
                             checker_fields.push(Field {
                                 name: i.to_string(), // Use index as name
                                 symbol: field_symbol,
                                 ty: field_ty,
                                 span: variant_span, // Approximate span
                             });
                         }
                         Err(e) => checker.report_error(e),
                     }
                 }
             }
             parallax_resolve::types::ResolvedEnumVariant::Struct { fields: resolved_fields, .. } => {
                 for resolved_field in resolved_fields {
                     match super::resolve::resolve_type_to_ty(checker, &resolved_field.field_type) {
                         Ok(field_ty) => {
                             checker_fields.push(Field {
                                 name: resolved_field.name.clone(),
                                 symbol: resolved_field.symbol,
                                 ty: field_ty,
                                 span: resolved_field.span,
                             });
                         }
                         Err(e) => checker.report_error(e),
                     }
                 }
             }
         }

         checker_variants.push(EnumVariant {
             name: variant_name.clone(),
             symbol: variant_symbol,
             fields: checker_fields,
             span: variant_span,
         });
     }

    checker.exit_generic_scope();

    // 3. Create EnumDef and add to TypeContext.
    let enum_def = EnumDef {
        name: re.name.clone(),
        symbol: re.symbol,
        generic_params: checker_generic_params,
        variants: checker_variants,
        span: re.span,
    };

    Ok(enum_def)
}

/// Check the signature of a single trait definition.
pub(crate) fn check_single_trait_signature(
    checker: &mut TypeChecker,
    rt: &ResolvedTrait,
    resolved_defs: &parallax_resolve::types::ResolvedDefinitions, // Pass resolved_defs
) -> TypeResult<TraitDef> {
    println!("Checking trait signature: {}", rt.name);
    let mut generic_param_scope = HashMap::new();
    let definition_context = Some((DefinitionKind::Trait, rt.symbol));
    let mut checker_generic_params = Vec::new();

    // 1. Resolve generic parameters for the trait itself.
    for resolved_gen_param in &rt.generic_params {
        match super::resolve::resolve_single_generic_param(checker, resolved_gen_param, definition_context) {
            Ok(checker_param) => {
                // Add symbol mapping for generic params
                generic_param_scope.insert(resolved_gen_param.name.clone(), checker_param.id);
                checker_generic_params.push(checker_param);
            }
            Err(e) => checker.report_error(e),
        }
    }

    // Enter the generic scope for resolving method signatures, associated types, etc.
    checker.enter_generic_scope(&checker_generic_params);

    let mut checker_methods = HashMap::new();
    let mut associated_types: HashMap<Symbol, AssociatedTypeDef> = HashMap::new();
    let mut trait_repo_errors = Vec::new();

    // 2. Resolve method signatures within the trait's scope.
    for resolved_assoc_func in &rt.methods {
        // Find the full ResolvedFunction definition for this associated function symbol.
        if let Some(resolved_func_def) = resolved_defs.functions.iter().find(|f| f.symbol == resolved_assoc_func.func_symbol) {
             // Pass Some(DefinitionKind::Trait) as parent_kind
             match check_single_function_signature(checker, resolved_func_def, Some(DefinitionKind::Trait)) {
                 Ok(Some(func_sig)) => { // Expect Some(sig) because we passed Some(DefinitionKind::Trait)
                    // Create TraitMethod
                    let trait_method = TraitMethod {
                        name: resolved_func_def.name.clone(),
                        method_symbol: resolved_func_def.symbol, // Use the function's symbol
                        signature: func_sig,
                        // Store the *ResolvedExpr* body, not the TypedExpr yet
                        default_body: resolved_func_def.resolved_default_body.clone(),
                    };
                    checker_methods.insert(resolved_assoc_func.func_symbol, trait_method);
                 },
                 Ok(None) => {
                     // This shouldn't happen if parent_kind is Some(Trait)
                    checker.report_error(TypeError::InternalError {
                        message: format!("check_single_function_signature returned None for trait method {:?}", resolved_assoc_func.func_symbol),
                        span: Some(resolved_func_def.span),
                    });
                 }
                 Err(e) => checker.report_error(e),
            }
        } else {
             checker.report_error(TypeError::InternalError {
                message: format!("Could not find ResolvedFunction for trait method symbol {:?}", resolved_assoc_func.func_symbol),
                span: Some(rt.span), // Use trait span as fallback
             });
        }
    }

    // Resolve associated types within the generic scope.
    for rat in &rt.associated_types {
        // Resolve bounds
        let mut assoc_bounds = Vec::new();
        for resolved_bound_type in &rat.bounds {
            match super::resolve::resolve_type_to_ty(checker, resolved_bound_type) {
                Ok(ty) => {
                    if let TyKind::Named { symbol: Some(trait_symbol), args, .. } = &ty.kind {
                        let type_args = args.clone();
                        let trait_def_opt = checker.trait_repo.get_trait_by_symbol(trait_symbol).cloned();
                        if let Some(trait_def) = trait_def_opt {
                            if trait_def.generic_params.len() == type_args.len() {
                                assoc_bounds.push(TraitRef {
                                    trait_id: trait_def.id,
                                    type_arguments: type_args,
                                    span: ty.span.unwrap_or(rat.span),
                                });
                            } else {
                                trait_repo_errors.push(TypeError::GenericArgCountMismatch {
                                    kind: "AssociatedType bound".to_string(),
                                    name: trait_def.name.clone(),
                                    expected: trait_def.generic_params.len(),
                                    found: type_args.len(),
                                    span: ty.span.unwrap_or(rat.span),
                                });
                            }
                        } else {
                            trait_repo_errors.push(TypeError::NotATrait {
                                found: format!("{:?}", ty),
                                span: ty.span.unwrap_or(rat.span),
                            });
                        }
                    } else {
                        trait_repo_errors.push(TypeError::NotATrait {
                            found: format!("{:?}", ty),
                            span: ty.span.unwrap_or(rat.span),
                        });
                    }
                }
                Err(e) => trait_repo_errors.push(e),
            }
        }
        let assoc_default = None;
        let associated_type_def = AssociatedTypeDef {
            name: rat.name.clone(),
            symbol: rat.symbol,
            bounds: assoc_bounds,
            default: assoc_default,
            span: rat.span,
        };
        associated_types.insert(rat.symbol, associated_type_def);
    }

    // Resolve supertrait bounds within the generic scope.
    let mut checker_bounds: Vec<TraitRef> = Vec::new();
    for resolved_supertrait_type in &rt.supertraits {
        match super::resolve::resolve_type_to_ty(checker, resolved_supertrait_type) {
            Ok(ty) => {
                if let TyKind::Named { symbol: Some(trait_symbol), args, .. } = &ty.kind {
                    let type_args = args.clone();
                    let trait_def_opt = checker.trait_repo.get_trait_by_symbol(trait_symbol).cloned();
                    if let Some(trait_def) = trait_def_opt {
                        if trait_def.generic_params.len() == type_args.len() {
                            checker_bounds.push(TraitRef {
                                trait_id: trait_def.id,
                                type_arguments: type_args,
                                span: ty.span.unwrap_or(rt.span),
                            });
                        } else {
                            trait_repo_errors.push(TypeError::GenericArgCountMismatch {
                                kind: "SuperTrait bound".to_string(),
                                name: trait_def.name.clone(),
                                expected: trait_def.generic_params.len(),
                                found: type_args.len(),
                                span: ty.span.unwrap_or(rt.span),
                            });
                        }
                    } else {
                        trait_repo_errors.push(TypeError::NotATrait {
                            found: format!("{:?}", ty),
                            span: ty.span.unwrap_or(rt.span),
                        });
                    }
                } else {
                    trait_repo_errors.push(TypeError::NotATrait {
                        found: format!("{:?}", ty),
                        span: ty.span.unwrap_or(rt.span),
                    });
                }
            }
            Err(e) => trait_repo_errors.push(e),
        }
    }

    // Exit the generic scope.
    checker.exit_generic_scope();

    // 3. Create the TraitDef but DO NOT add it here.
    let trait_def = TraitDef {
        id: TraitId(u32::MAX), // Placeholder ID
        trait_symbol: rt.symbol,
        name: rt.name.clone(),
        generic_params: checker_generic_params,
        bounds: checker_bounds,
        methods: checker_methods,
        associated_types: associated_types, // Use the renamed variable
        span: rt.span,
    };

    // <<< REMOVE adding to repo >>>
    /*
    let add_trait_result = checker.trait_repo.borrow_mut().add_trait(
        rt.symbol,
        rt.name.clone(),
        checker_generic_params,
        checker_bounds,
        checker_methods,
        associated_types, // Use the renamed variable
        rt.span,
    );
    */
    // Report errors after all borrows are dropped
    for e in trait_repo_errors {
        checker.report_error(e);
    }
    /*
    if let Err(e) = add_trait_result {
        checker.report_error(e); // Report error if adding fails (e.g., duplicate)
    }
    */

    Ok(trait_def) // <<< Return the constructed TraitDef
}

/// Check the signature of a single function (standalone or associated method stub).
/// `parent_kind` indicates if it's defined within a Trait or Impl context.
/// Made public for use by impls.rs
pub(crate) fn check_single_function_signature(
    checker: &mut TypeChecker,
    rf: &ResolvedFunction,
    parent_kind: Option<DefinitionKind>,
) -> TypeResult<Option<FunctionSignature>> {
    let mut generic_param_scope = HashMap::new();
    let definition_context = Some((DefinitionKind::Function, rf.symbol));
    let mut checker_generic_params = Vec::new();

    // --- Determine self_param kind ---
    let mut self_param_kind = None;
    let mut regular_params_resolved = Vec::new();
    for p in &rf.parameters {
        if p.name == "self" {
            if parent_kind.is_none() {
                // 'self' in standalone function is an error
                checker.report_error(TypeError::NotAMethod {
                    method: rf.name.clone(),
                    ty: "<standalone function>".to_string(),
                    span: p.span, // Span of the 'self' parameter
                });
                // Continue processing other params, but the signature might be invalid
            } else {
                // TODO: Handle &self, &mut self by inspecting p.param_type
                self_param_kind = Some(SelfParamKind::Value); // Assume Value for now
            }
        } else {
            regular_params_resolved.push(p); // Collect non-self params
        }
    }

    // 1. Resolve generic parameters first.
    for resolved_gen_param in &rf.generic_params {
        match super::resolve::resolve_single_generic_param(checker, resolved_gen_param, definition_context) {
            Ok(checker_param) => {
                generic_param_scope.insert(checker_param.name.clone(), checker_param.id);
                checker_generic_params.push(checker_param);
            }
            Err(e) => checker.report_error(e),
        }
    }

    // Enter the generic scope for resolving parameter and return types.
    checker.enter_generic_scope(&checker_generic_params);

    // If inside a trait, add 'Self' to the scope temporarily
    if parent_kind == Some(DefinitionKind::Trait) {
         // Need a way to represent the trait's Self type parameter here.
         // Maybe add a placeholder generic param ID? Or use a specific TypeId?
         // For now, let's assume resolve_type_to_ty handles 'Self' correctly in trait context.
         // checker.env.add("Self", self_type_id?); // Need the type ID for Self
    }
    // If inside an impl, add 'Self' mapped to the implementing type.
    // This should ideally happen *before* calling this function, in check_single_impl_stub.

    // 2. Resolve parameter types (only for non-self parameters).
    let mut checker_params = Vec::new();
    for resolved_param in regular_params_resolved { // Use collected non-self params
        // Use resolve_type_to_ty for simplicity, ParamType might need adjustment
        match super::resolve::resolve_type_to_ty(checker, &resolved_param.param_type) {
            Ok(ty) => {
                checker_params.push(ParamType {
                    name: resolved_param.name.clone(),
                    ty: ty, // Use the resolved Ty
                    span: resolved_param.span,
                });
            }
            Err(e) => checker.report_error(e),
        }
    }

    // 3. Resolve return type.
    let return_ty = match super::resolve::resolve_type_to_ty(checker, &rf.return_type) {
        Ok(ty) => ty,
        Err(e) => {
            checker.report_error(e);
            // Use Error as a fallback if resolution fails
            Ty::new(TyKind::Error) // Try TyKind::Error
        }
    };

    // Exit the generic scope.
    checker.exit_generic_scope();

    // 4. Create the FunctionSignature.
    let func_sig = FunctionSignature {
        name: rf.name.clone(),
        self_param: self_param_kind, // Use determined kind
        params: checker_params, // Use checker_params for non-self
        return_type: return_ty,
        generic_params: checker_generic_params,
        span: rf.span,
        // Removed: symbol, is_public
    };

    // 5. Add to TypeContext ONLY if it's a standalone function. Otherwise, return it.
    if parent_kind.is_none() {
        // <<< REMOVE add_definition call >>>
        /* 
        if let Err(e) = checker.type_ctx.add_definition(rf.symbol, DefinitionKind::Function, TypeDef::Function(func_sig.clone())) {
            checker.report_error(e);
        }
        */
        // <<< If standalone, return the signature anyway, let caller handle storage >>>
        // Ok(None) 
        Ok(Some(func_sig))
    } else {
        Ok(Some(func_sig)) // Return the signature for the caller (trait/impl checker)
    }
} 