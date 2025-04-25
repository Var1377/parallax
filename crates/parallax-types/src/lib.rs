//! Type checking and inference for the Parallax programming language.
//!
//! This crate implements type inference, type checking, and trait solving for Parallax.
//! It builds on the name resolution phase and provides a fully typed AST.

pub mod error;
pub mod context;
pub mod types;
pub mod checker;

// Re-export commonly used types
pub use error::{TypeError, TypeResult};
pub use context::{InferenceContext, TypeEnvironment, TraitRepository};
pub use types::{PrimitiveType, Ty, TyKind, TypeId, TypeContext, TypeDef, StructDef, EnumDef, FunctionSignature, TypedModule, TypedFunction, TypedParameter};

use parallax_resolve::{ResolveDatabase, types::ResolvedModuleStructure};
use std::sync::Arc;
use crate::checker::expr::type_check_expression;
use std::collections::HashMap;
use crate::checker::resolve::resolve_single_generic_param;
use crate::checker::substitute;
use crate::error::display_type;

/// Database trait for type checking operations
#[salsa::db]
pub trait TypeDatabase: ResolveDatabase {
    /// Type check a resolved module structure
    fn type_check_definitions(&self, resolved_module: ResolvedModuleStructure) -> TypedModule where Self: Sized {
        println!("[TypeCheckDefs] Starting type checking...");
        // Import necessary items
        use crate::checker::TypeChecker;
        use crate::types::{TypedDefinitions, TypeContext, TypeDef, TypedFunction, TypedParameter};
        use parallax_resolve::definitions::{DefinitionKind};

        // Initialize context and checker
        println!("[TypeCheckDefs] Initializing checker...");
        let definitions = resolved_module.definitions(self);
        let mut typed_definitions = TypedDefinitions::default();

        let type_ctx = TypeContext::new();
        let trait_repo = TraitRepository::new();
        let mut checker = TypeChecker::new(self, definitions, type_ctx, trait_repo);
        println!("[TypeCheckDefs] Checker initialized.");

        // Check Structs
        println!("[TypeCheckDefs] Checking {} structs...", definitions.structs.len());
        for resolved_struct in &definitions.structs {
            println!("[TypeCheckDefs] Checking struct: {}", resolved_struct.name);
            if let Err(e) = checker.check_struct_definition(resolved_struct) {
                 checker.errors.push(e);
                 println!("[TypeCheckDefs]   ERROR checking struct {}: {:?}", resolved_struct.name, checker.errors.last().unwrap());
            }
        }
        // Check Enums
        println!("[TypeCheckDefs] Checking {} enums...", definitions.enums.len());
        for resolved_enum in &definitions.enums {
             println!("[TypeCheckDefs] Checking enum: {}", resolved_enum.name);
              if let Err(e) = checker.check_enum_definition(resolved_enum) {
                  checker.errors.push(e);
                  println!("[TypeCheckDefs]   ERROR checking enum {}: {:?}", resolved_enum.name, checker.errors.last().unwrap());
              }
         }
         // Check Traits
         println!("[TypeCheckDefs] Checking {} traits...", definitions.traits.len());
         for resolved_trait in &definitions.traits {
              println!("[TypeCheckDefs] Checking trait: {}", resolved_trait.name);
              if let Err(e) = checker.check_trait_definition(resolved_trait) {
                  checker.errors.push(e);
                  println!("[TypeCheckDefs]   ERROR checking trait {}: {:?}", resolved_trait.name, checker.errors.last().unwrap());
              }
         }

        println!("[TypeCheckDefs] --- Starting Pass 1: Signatures/Stubs --- ({} funcs, {} impls)", definitions.functions.len(), definitions.impls.len());

        // Create set of associated function symbols *before* checking standalone signatures
        let mut associated_function_symbols = std::collections::HashSet::new();
        for r#trait in &definitions.traits {
            for assoc_func in &r#trait.methods {
                associated_function_symbols.insert(assoc_func.func_symbol);
            }
        }
        for r#impl in &definitions.impls {
            for assoc_func in &r#impl.methods {
                associated_function_symbols.insert(assoc_func.func_symbol);
            }
        }
        println!("[TypeCheckDefs]   Found {} associated function symbols.", associated_function_symbols.len());

        // Check Function Signatures (Standalone ONLY)
        println!("[TypeCheckDefs] Checking standalone function signatures...");
        for resolved_func in &definitions.functions {
            // Only check standalone functions here. Associated functions are checked within their trait/impl context.
            if !associated_function_symbols.contains(&resolved_func.symbol) {
                println!("[TypeCheckDefs]   Checking standalone signature: {}", resolved_func.name);
                if let Err(e) = checker.check_function_signature(resolved_func, None) {
                   checker.errors.push(e);
                   println!("[TypeCheckDefs]     ERROR checking standalone signature {}: {:?}", resolved_func.name, checker.errors.last().unwrap());
                }
            }
        }
        // Check Impl Stubs (Trait and Inherent)
        // This loop now handles checking associated function signatures within the impl context.
        println!("[TypeCheckDefs] Checking impl definition stubs ({} impls)...", definitions.impls.len());
        for resolved_impl in &definitions.impls {
            let impl_desc = format!("impl for symbol {:?}", resolved_impl.impl_symbol);
            println!("[TypeCheckDefs]   Checking impl stub: {}", impl_desc);
            if let Err(e) = checker.check_impl_definition_stub(resolved_impl) {
                checker.errors.push(e);
                println!("[TypeCheckDefs]     ERROR checking impl stub {}: {:?}", impl_desc, checker.errors.last().unwrap());
            }
        }

        println!("[TypeCheckDefs] --- Finished Pass 1 --- Errors: {}", checker.errors.len());
        if !checker.errors.is_empty() {
            println!("!!! ERRORS FOUND IN PASS 1, CONTINUING TO PASS 2 FOR DEBUGGING !!!");
        }
        // --- Pass 2: Check function bodies and impl method bodies --- 
        println!("[TypeCheckDefs] --- Starting Pass 2: Body Checks --- ({} funcs, {} impls)", definitions.functions.len(), definitions.impls.len());
        // Now that all signatures and types are known.
        // Check Standalone Function Bodies
        println!("[TypeCheckDefs] Checking standalone function bodies...");
        for resolved_func in &definitions.functions { 
            // Only check bodies for standalone functions here.
            if !associated_function_symbols.contains(&resolved_func.symbol) {
                println!("[TypeCheckDefs]   Checking body for standalone func: {}", resolved_func.name);
                // Check if the function has a body
                if let Some(body_expr) = &resolved_func.body {
                    // Resolve expected return type
                    let expected_ret_ty = match checker.resolve_type_to_ty(&resolved_func.return_type) {
                        Ok(ty) => {
                            println!("[TypeCheckDefs]     Resolved expected return type: {}", display_type(&ty));
                            Some(ty)
                        }
                        Err(e) => {
                            checker.errors.push(e);
                            println!("[TypeCheckDefs]     ERROR resolving expected return type: {:?}", checker.errors.last().unwrap());
                            None
                        }
                    };

                    let original_env = checker._type_env.clone(); 

                    let typed_body_result: Option<crate::types::TypedExpr> = 
                        if let Some(expected_ret_ty_val) = expected_ret_ty {
                            // --- Environment Setup --- 
                            let mut func_scope_env = TypeEnvironment::with_parent(original_env.clone());
                            let mut generic_param_scope = HashMap::new();
                            let definition_context = Some((DefinitionKind::Function, resolved_func.symbol));
                            let mut setup_failed = false;
                            println!("[TypeCheckDefs]     Setting up environment for body check...");

                            // Resolve and add generic parameters
                            for resolved_gen_param in &resolved_func.generic_params {
                                println!("[TypeCheckDefs]       Resolving generic param: {}", resolved_gen_param.name);
                                match resolve_single_generic_param(&mut checker, resolved_gen_param, definition_context) {
                                    Ok(checker_param) => {
                                        generic_param_scope.insert(checker_param.name.clone(), checker_param.id);
                                    }
                                    Err(e) => { checker.errors.push(e); setup_failed = true; }
                                }
                            }
                            if !setup_failed { checker.generic_scopes.push(generic_param_scope); }

                            // Add function parameters to scope env
                            for rp in &resolved_func.parameters {
                                println!("[TypeCheckDefs]       Adding parameter '{}' to scope...", rp.name);
                                match checker.resolve_type_to_ty(&rp.param_type) {
                                    Ok(param_ty) => {
                                        println!("[TypeCheckDefs]         Resolved param type: {}", display_type(&param_ty));
                                        func_scope_env.add(rp.name.clone(), param_ty);
                                    },
                                    Err(e) => { checker.errors.push(e); setup_failed = true; }
                                }
                            }

                            let body_check_result = if !setup_failed {
                                checker._type_env = Arc::new(func_scope_env);
                                // --- End Environment Setup ---

                                // Check the expression body within the new env
                                println!("[TypeCheckDefs]     Checking body expression... Expecting: {}", display_type(&expected_ret_ty_val));
                                let check_result = match type_check_expression(&mut checker, body_expr, Some(&expected_ret_ty_val)) {
                                    Ok(typed_body) => Some(typed_body),
                                    Err(e) => {
                                        println!("[TypeCheckDefs]       ERROR during body check: {:?}", e);
                                        checker.errors.push(e);
                                        None
                                    }
                                };
                                
                                // Restore environment and pop generic scope 
                                checker._type_env = original_env.clone(); // Clone original_env again for restoration
                                checker.generic_scopes.pop();
                                println!("[TypeCheckDefs]     Restored environment.");
                                check_result
                            } else { None }; // Env setup failed, skip body check
                            body_check_result
                        } else { None }; // Return type resolution failed, skip body check

                    // Retrieve the signature (already checked in pass 1)
                    let func_sig = checker.type_ctx.get_type_by_symbol(&resolved_func.symbol)
                        .and_then(|td| match td { TypeDef::Function(sig) => Some(sig.clone()), _ => None });
                    
                    let func_name_clone = func_sig.as_ref().map(|s| s.name.clone()).unwrap_or_else(|| "<unknown>".to_string());
                    if let Some(sig) = func_sig {
                        // Construct TypedFunction (even if body check failed, store signature info)
                        let typed_func = TypedFunction {
                            name: sig.name,
                            // Use symbols from ResolvedFunction parameters
                            params: resolved_func.parameters.iter().map(|rp| {
                                // Find corresponding ParamType in sig to get resolved Ty
                                let ty = sig.params.iter().find(|sp| sp.name == rp.name)
                                    .map(|sp| sp.ty.clone())
                                    .unwrap_or_else(|| {
                                        let func_name_clone = func_name_clone.clone();
                                        checker.errors.push(TypeError::InternalError { 
                                            message: format!("Param '{}' from ResolvedFunction not found in concrete signature for function '{}' (symbol {:?})", rp.name, func_name_clone, resolved_func.symbol),
                                            span: Some(rp.span)
                                        });
                                        Ty::new(TyKind::Error) 
                                    });
                                TypedParameter { 
                                    name: rp.name.clone(), 
                                    symbol: rp.symbol, // Use resolved param symbol
                                    ty: ty, 
                                    is_variadic: rp.is_variadic, 
                                    has_default: rp.has_default,
                                    span: rp.span 
                                }
                            }).collect(),
                            return_type: sig.return_type,
                            generic_params: sig.generic_params.iter().map(|gp| gp.name.clone()).collect(), // Store names for now
                            span: sig.span,
                            body: typed_body_result, // Assign the checked body result here
                            is_effectful: resolved_func.is_effectful, // Carry over effect info
                        };
                        // Use entry API to potentially update if already inserted by signature check
                        typed_definitions.functions.insert(resolved_func.symbol, typed_func);
                    } else {
                         checker.errors.push(TypeError::InternalError { 
                             message: format!("Signature not found for standalone function {:?}", resolved_func.symbol), 
                             span: Some(resolved_func.span) 
                          });
                    }
                } else {
                    // No body -> Possibly a declaration or error during resolution, signature stored if resolved.
                    // If signature exists, ensure it's stored in typed_definitions
                     if let Some(TypeDef::Function(sig)) = checker.type_ctx.get_type_by_symbol(&resolved_func.symbol).cloned() {
                        if !typed_definitions.functions.contains_key(&resolved_func.symbol) {
                             let func_name_clone = sig.name.clone(); // Clone name outside map
                             let typed_func = TypedFunction {
                                name: sig.name,
                                // Also update here
                                params: resolved_func.parameters.iter().map(|rp| {
                                    let ty = sig.params.iter().find(|sp| sp.name == rp.name)
                                        .map(|sp| sp.ty.clone())
                                        .unwrap_or_else(|| {
                                            let func_name_clone = func_name_clone.clone();
                                            checker.errors.push(TypeError::InternalError { 
                                                message: format!("Param '{}' from ResolvedFunction not found in concrete signature for function '{}' (symbol {:?})", rp.name, func_name_clone, resolved_func.symbol),
                                                span: Some(rp.span)
                                            });
                                            Ty::new(TyKind::Error) 
                                        });
                                    TypedParameter { 
                                        name: rp.name.clone(), 
                                        symbol: rp.symbol, // Use resolved param symbol
                                        ty: ty, 
                                        is_variadic: rp.is_variadic, 
                                        has_default: rp.has_default, 
                                        span: rp.span 
                                    }
                                }).collect(),
                                return_type: sig.return_type,
                                generic_params: sig.generic_params.iter().map(|gp| gp.name.clone()).collect(), 
                                span: sig.span,
                                body: None,
                                is_effectful: resolved_func.is_effectful,
                            };
                            typed_definitions.add_function(resolved_func.symbol, typed_func);
                        }
                     } // else: No signature found either, likely resolution error handled elsewhere
                }
            }
            println!("[TypeCheckDefs]   Finished checking body for standalone func: {}", resolved_func.name);
        }
        // Check Impl Bodies (Signatures were checked in `check_impl_definition_stub`, bodies are checked here)
        // Note: checker.trait_repo.impls now contains the ImplDef stubs
        println!("[TypeCheckDefs] Checking impl method bodies...");
        for resolved_impl in &definitions.impls {
             // Check signature compatibility and collect potential typed body
             if let Err(e) = checker.check_impl_body(resolved_impl) { 
                 checker.errors.push(e); 
                 // If signature check fails, maybe skip body check? 
                 continue; 
             }
             
             // Now, retrieve the potentially updated ImplDef and check method bodies 
             if let Some(impl_def) = checker.trait_repo.get_impl_def_by_symbol(resolved_impl.impl_symbol) {
                 // Clone necessary info from impl_def to avoid borrowing issues
                 let implementing_type = impl_def.implementing_type.clone();
                 let impl_methods = impl_def.methods.clone(); // Clone the method map
                 let impl_span = impl_def.span; // Copy span
                 // TODO: Clone generic_params if needed for env setup
                 let impl_generic_params = impl_def.generic_params.clone(); // Clone generics

                 for (_trait_method_symbol, impl_method_symbol) in impl_methods {
                     if let Some(method_resolved_func) = definitions.functions.iter().find(|f| f.symbol == impl_method_symbol) {
                         if let Some(body_expr) = &method_resolved_func.body {
                             // Create a scope for the impl's generics and the implementing type (Self)
                             let mut impl_scope_env = TypeEnvironment::with_parent(checker._type_env.clone());
                             let mut generic_param_scope = HashMap::new(); // <<< Create NEW map here
                             let _impl_context = Some((DefinitionKind::Impl, resolved_impl.impl_symbol));

                             impl_scope_env.add("Self".to_string(), implementing_type.clone()); // Use cloned type

                             // Add impl generic parameters to scope map
                             for impl_gen_param in &impl_generic_params {
                                 // Assume impl generics are already resolved GenericParamDef from checker
                                 generic_param_scope.insert(impl_gen_param.name.clone(), impl_gen_param.id);
                             }

                             // Add method-specific generic parameters to scope map
                             for resolved_gen_param in &method_resolved_func.generic_params {
                                 match resolve_single_generic_param(&mut checker, resolved_gen_param, _impl_context) {
                                     Ok(checker_param) => {
                                         generic_param_scope.insert(checker_param.name.clone(), checker_param.id);
                                     }
                                     Err(e) => {
                                         checker.errors.push(e)
                                     },
                                 }
                             }
                             checker.generic_scopes.push(generic_param_scope);

                             // Add function parameters to scope env
                             for rp in &method_resolved_func.parameters {
                                 // Skip `self` parameter if present, already handled by `Self` type
                                 if rp.name == "self" {
                                     continue;
                                 }
                                 match checker.resolve_type_to_ty(&rp.param_type) {
                                     Ok(param_ty) => {
                                         impl_scope_env.add(rp.name.clone(), param_ty)
                                     },
                                     Err(e) => {
                                         checker.errors.push(e)
                                     },
                                 }
                             }

                             let original_env = checker._type_env.clone();
                             checker._type_env = Arc::new(impl_scope_env);
                             
                             // Resolve expected return type
                             let expected_ret_ty = match checker.resolve_type_to_ty(&method_resolved_func.return_type) {
                                 Ok(ty) => {
                                     Some(ty)
                                 },
                                 Err(e) => {
                                     checker.errors.push(e);
                                     None
                                 }
                             };
                             
                             // Check the expression body only if return type resolved
                             if let Some(expected_ret_ty_val) = expected_ret_ty {
                                 match type_check_expression(&mut checker, body_expr, Some(&expected_ret_ty_val)) {
                                     Ok(typed_body) => {
                                         // Update the body in the existing TypedFunction stored in typed_definitions
                                         if let Some(typed_func) = typed_definitions.functions.get_mut(&impl_method_symbol) {
                                             typed_func.body = Some(typed_body);
                                         } else {
                                             // This shouldn't happen if signature check succeeded and stored the function
                                             checker.errors.push(TypeError::InternalError {
                                                 message: format!("TypedFunction missing for impl method {:?} when trying to store body", impl_method_symbol),
                                                 span: Some(impl_span),
                                             });
                                         }
                                     }
                                     Err(e) => checker.errors.push(e),
                                 }
                             }

                             // Restore original environment
                             checker._type_env = original_env;
                             checker.generic_scopes.pop(); // Pop generic scope
                         } // else: No body to check (e.g., trait method definition)
                     } else {
                          println!("[TypeCheckDefs]     ERROR: ResolvedFunction not found for impl method symbol {:?}", impl_method_symbol);
                          checker.errors.push(TypeError::InternalError { 
                             message: format!("ResolvedFunction not found for impl method symbol {:?} during body check", impl_method_symbol), 
                             span: Some(impl_span) // Use copied span
                          });
                     }
                 }
             } // else: ImplDef not found, error handled in check_impl_body
        }

        println!("[TypeCheckDefs] --- Finished Pass 2 --- Errors: {}", checker.errors.len());
        if !checker.errors.is_empty() {
            println!("!!! ERRORS FOUND IN PASS 2 !!!");
        }

        // --- Merge needed functions into typed_definitions ---
        // This ensures that concrete function instances (impl methods, instantiated generics)
        // discovered during body checking are present in the final output.
        println!("[TypeCheckDefs] Merging {} needed function signatures into final definitions...", checker.needed_functions.borrow().len());
        for (symbol, concrete_signature) in checker.needed_functions.borrow().iter() {
            // Check if the function already exists (e.g., added during Pass 1)
            if let Some(existing_typed_func) = typed_definitions.functions.get_mut(symbol) {
                // TODO: Decide if we need to update the existing entry. 
                // For now, just ensure it exists. If the signature is different
                // (e.g., due to instantiation), how do we store that? 
                // Maybe TypeContext/TypedFunction needs adjustment for instantiation.
                println!("[TypeCheckDefs]   Symbol {:?} already exists, ensuring signature consistency (TBD).", symbol);
                // Example check (replace with proper logic):
                // if existing_typed_func.params.len() != concrete_signature.params.len() { // Simple check
                //     println!("[TypeCheckDefs]     WARN: Signature mismatch for {:?}", symbol);
                // }
            } else {
                // Function wasn't added in Pass 1 (e.g., maybe an impl method missed?)
                // Create a new TypedFunction entry from the needed signature.
                println!("[TypeCheckDefs]   Adding new entry for symbol {:?}", symbol);
                 // We need the ResolvedFunction corresponding to the symbol to get parameter symbols etc.
                 let resolved_func_opt = definitions.functions.iter().find(|f| f.symbol == *symbol);
                 if let Some(resolved_func) = resolved_func_opt {
                     let typed_params = resolved_func.parameters
                         .iter()
                         // <<< FILTER OUT 'self' parameter >>>
                         .filter(|rp| rp.name != "self") 
                         .map(|rp| {
                         let ty = concrete_signature.params.iter().find(|sp| sp.name == rp.name)
                             .map(|sp| sp.ty.clone())
                             .unwrap_or_else(|| { // <<< Replace Panic with Error >>>
                                 checker.errors.push(TypeError::InternalError { 
                                     message: format!("Param '{}' from ResolvedFunction not found in concrete signature for function '{}' (symbol {:?})", rp.name, concrete_signature.name.clone(), symbol),
                                     span: Some(rp.span)
                                 });
                                 Ty::new(TyKind::Error) 
                             });
                         TypedParameter { 
                             name: rp.name.clone(), 
                             symbol: rp.symbol,
                             ty: ty, 
                             is_variadic: rp.is_variadic, 
                             has_default: rp.has_default,
                             span: rp.span 
                         }
                     }).collect();

                     let new_typed_func = TypedFunction {
                         name: concrete_signature.name.clone(),
                         params: typed_params, 
                         return_type: concrete_signature.return_type.clone(),
                         generic_params: concrete_signature.generic_params.iter().map(|gp| gp.name.clone()).collect(),
                         span: concrete_signature.span,
                         body: None, // Body belongs to the original definition, not this instantiation record
                         is_effectful: resolved_func.is_effectful, // Get effect from ResolvedFunction
                     };
                     typed_definitions.add_function(*symbol, new_typed_func);
                 } else {
                      checker.errors.push(TypeError::InternalError {
                          message: format!("Cannot find ResolvedFunction for needed symbol {:?} to create TypedFunction", symbol),
                          span: Some(concrete_signature.span),
                      });
                 }
            }
        }
        println!("[TypeCheckDefs] --- Finished merging needed functions ---");

        // Collect errors (convert from TypeError to String for TypedModule)
        // TODO: Improve error reporting structure
        println!("[TypeCheckDefs] Collecting {} errors...", checker.errors.len());
        let error_strings = checker.errors.iter().map(|e| format!("{:?}", e)).collect();

        // Return the TypedModule
        println!("[TypeCheckDefs] Type checking finished.");
        TypedModule {
            definitions: typed_definitions,
            trait_repo: checker.trait_repo, // Return the populated repo
            entry_point: resolved_module.entry_point(self), // Pass through entry point
            intrinsics: resolved_module.intrinsics(self).to_vec(), // Populate intrinsics
            errors: error_strings,
        }
    }
}