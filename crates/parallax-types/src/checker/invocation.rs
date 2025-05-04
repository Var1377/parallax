// src/checker/invocation.rs
//! Type checking pass 2: Checking function/method invocations.

use super::TypeChecker;
use crate::error::{TypeError, TypeResult, display_type};
use crate::types::{self, Ty, TyKind, TypedArgument, TypedExpr, TypedExprKind, FunctionSignature, PrimitiveType, TraitRef, ParamType};
use parallax_resolve::types::Symbol;
use parallax_resolve::definitions::DefinitionKind;
use miette::SourceSpan;
use crate::context::Substitution;
use crate::context::ImplId;
use crate::checker::substitute::substitute_signature_self;

/// Type checks a function or method invocation.
/// Handles overload resolution (if applicable), generic instantiation, and argument checking.
/// Dispatches to specific handlers based on the type of the callable expression.
///
/// Preconditions: `checker` context is set up.
///                `func_expr` is the typed expression for the function/method being called.
///                `args` are the typed arguments passed to the call.
///                `arg_tys` are the types of the arguments (parallel to `args`).
///                `span` is the source span of the entire call expression.
/// Postconditions: Returns `Ok((TypedExprKind::Call or MethodCall?, Ty))` representing the typed call and its return type.
///                 Returns `Err(TypeError)` if checking fails.
pub(crate) fn check_invocation(
    checker: &mut TypeChecker,
    func_expr: &TypedExpr,
    args: Vec<TypedArgument>,
    arg_tys: Vec<Ty>,
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    println!(
        "Checking invocation for: {} (expr kind: {:?}) with {} args",
        display_type(&func_expr.ty),
        func_expr.kind,
        args.len()
    );

    // Apply current substitution to the function expression's type.
    let func_ty = checker.infctx.apply_substitution(&func_expr.ty);

    match func_ty.kind {
        TyKind::Function(ref param_tys, ref ret_ty) => {
            // Direct function call (or lambda call).
            check_direct_function_call(checker, func_expr, param_tys, ret_ty, args, &arg_tys, span)
        }
        _ => {
            // If func_ty is not TyKind::Function, it must be a method call or UFCS.
            match &func_expr.kind {
                TypedExprKind::Field { object, field_name, .. } => {
                    // Method call: object.method(...)
                    check_method_call(checker, object, field_name, args, arg_tys, span)
                }
                TypedExprKind::Variable { name, symbol } => {
                     if let Some(def_kind) = checker.type_ctx.get_definition_kind(symbol) {
                         match def_kind {
                             DefinitionKind::Struct | DefinitionKind::Enum => {
                                 check_ufcs_type_method(checker, *symbol, name, args, arg_tys, span)
                             }
                             DefinitionKind::Trait => {
                                 check_ufcs_trait_method(checker, *symbol, name, args, arg_tys, span)
                             }
                             DefinitionKind::Function => {
                                // It's just a variable holding a function, but the type wasn't TyKind::Function?
                                // This might happen if the variable's type is currently an inference variable.
                                // Try unifying func_ty with a generic function type and retry direct call check.
                                println!("Warning: Variable call on non-fn type ({}) - potential inference delay?", display_type(&func_ty));
                                return Err(TypeError::NotAFunction { found: display_type(&func_ty), span: func_expr.span });
                             }
                             _ => { // Impl, AssociatedType? Should not be callable directly.
                                return Err(TypeError::NotAFunction { found: format!("definition kind {:?}", def_kind), span: func_expr.span });
                             }
                         }
                     } else {
                         // Variable symbol not found in type context - likely a local var?
                         // If its type is not Function, it's an error.
                         return Err(TypeError::NotAFunction { found: display_type(&func_ty), span: func_expr.span });
                     }
                }
                _ => {
                    Err(TypeError::NotAFunction {
                        found: display_type(&func_ty),
                        span: func_expr.span,
                    })
                }
            }
        }
    }
}

/// Checks a direct function call against its signature and arguments.
fn check_direct_function_call(
    checker: &mut TypeChecker,
    func_expr: &TypedExpr, // Keep func_expr for TypedExprKind::Call
    param_tys: &[Ty],
    ret_ty: &Ty,
    args: Vec<TypedArgument>,
    arg_tys: &[Ty],
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    // Attempt to get the function symbol if it's a direct variable/path reference
    let func_symbol_opt = if let TypedExprKind::Variable { symbol, .. } = func_expr.kind {
        Some(symbol)
    } else {
        None // Could be a lambda or other complex expression
    };

    if let Some(func_symbol) = func_symbol_opt {
        // Get function name from signature
        let func_name = checker.type_ctx.get_function_sig(&func_symbol, span)
            .map_or_else(|_| "<unknown_fn>".to_string(), |sig| sig.name.clone());

        // Call the common helper
        check_resolved_invocation(
            checker,
            func_symbol, // Signature comes from the func_symbol itself
            func_symbol, // Actual function is also the func_symbol
            &func_name,
            None, // Not a method call
            None, // <<< Add None for impl_id_opt >>>
            args,
            arg_tys.to_vec(), // Clone arg_tys into Vec
            span,
        )
    } else {
        // --- Handle non-symbol callables (e.g., lambdas) --- 
        // This part retains the original logic for function types that aren't direct symbols
        // TODO: Consider refactoring this part too if lambdas get more complex
        
        // Re-check arg count using param_tys from the TyKind::Function
    if param_tys.len() != arg_tys.len() {
        return Err(TypeError::WrongNumberOfArguments {
            expected: param_tys.len(),
            found: arg_tys.len(),
            span,
        });
    }

        // Unify argument types with parameter types from TyKind::Function
    for (i, (arg_ty, param_ty)) in arg_tys.iter().zip(param_tys.iter()).enumerate() {
        let arg_span = args.get(i).map_or(span, |a| a.span); // Use arg span if possible
        if !checker.unify(arg_ty, param_ty, arg_span) {
            // Error reported by unify
        }
    }

        // Determine final return type (already available in ret_ty)
    let final_ret_ty = checker.infctx.apply_substitution(ret_ty);

        // Construct Call HIR node
    let typed_call_kind = TypedExprKind::Call {
        func_expr: Box::new(func_expr.clone()),
            func_symbol: None, // No symbol for lambda/complex expr
            type_args: None,   // No generics known from type alone
        args,
    };

    Ok((typed_call_kind, final_ret_ty))
    }
}

/// Checks method calls (e.g., object.method(...)).
pub(crate) fn check_method_call(
    checker: &mut TypeChecker,
    object_expr: &TypedExpr,
    method_name: &str,
    args: Vec<TypedArgument>,
    arg_tys: Vec<Ty>,
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    // 1. Get Receiver Type
    let receiver_ty = checker.infctx.apply_substitution(&object_expr.ty);
    println!(
        "Checking method call: '{}' on receiver type {}",
        method_name,
        display_type(&receiver_ty)
    );

    // 2. Find Candidates (using TypeChecker methods)
    // Pass mutable checker because find_trait_method_candidates needs mutable infctx
    let inherent_candidates = checker.find_inherent_method_candidates(&receiver_ty, method_name, span);
    let trait_candidates = checker.find_trait_method_candidates(&receiver_ty, method_name, span);

    // 3. Prioritize & Select
    // Store the selected (Symbol, Option<ImplId>) pair
    let selected_candidate: Option<(Symbol, Option<ImplId>)> =
        // <<<< MODIFIED SELECTION LOGIC >>>>
        // Prioritize Trait methods if BOTH inherent and trait candidates exist.
        // This handles cases like `ne` where a default trait method might conflict
        // with an auto-derived inherent method (e.g., from PartialEq).
        if !trait_candidates.is_empty() {
            // If trait candidates exist, prefer them over inherent ones.
            if trait_candidates.len() > 1 {
                println!("Warning: Ambiguous trait method candidates for {}::{}: {:?}", display_type(&receiver_ty), method_name, trait_candidates);
                checker.report_error(TypeError::AmbiguousMethodCall {
                    method: method_name.to_string(),
                    ty: display_type(&receiver_ty),
                    span,
                });
                return Ok((TypedExprKind::Error, Ty::new(TyKind::Error)));
            }
            println!("  Selected trait candidate: {:?} (from impl {:?})", trait_candidates[0].0, trait_candidates[0].1);
            Some((trait_candidates[0].0, Some(trait_candidates[0].1))) // Store method symbol and ImplId
        } else if !inherent_candidates.is_empty() {
            // Only use inherent candidates if NO trait candidates were found.
            if inherent_candidates.len() > 1 {
                 println!("Warning: Ambiguous inherent method candidates for {}::{}: {:?}", display_type(&receiver_ty), method_name, inherent_candidates);
                checker.report_error(TypeError::AmbiguousMethodCall {
                     method: method_name.to_string(),
                     ty: display_type(&receiver_ty),
                     span,
                 });
                return Ok((TypedExprKind::Error, Ty::new(TyKind::Error)));
            }
            println!("  Selected inherent candidate: {:?}", inherent_candidates[0]);
            Some((inherent_candidates[0], None)) // Store symbol, no ImplId needed for inherent
        } else {
            // Neither trait nor inherent candidates found.
             None
        };
        // <<<< END MODIFIED SELECTION LOGIC >>>>

    // 4. Call the common invocation checker
    if let Some((candidate_symbol, impl_id_opt)) = selected_candidate {
        // Get the method name from the resolved function def if possible, fallback to input
        // Use candidate_symbol (which could be trait method or inherent method symbol) to get the name
        let method_name_str = checker
            .type_ctx
            .get_any_function_sig(checker.trait_repo, &candidate_symbol, span)
            .map_or(method_name.to_string(), |sig| sig.name.clone());

        // --- DEFERRED Determination of the Actual Function Symbol ---
        // For trait methods, we pass the trait method symbol (`candidate_symbol`)
        // and the ImplId to check_resolved_invocation. The final resolution
        // happens there after default bodies have been generated.
        // For inherent methods, candidate_symbol is the actual symbol.

        // Prepend the receiver object as the first argument for the helper.
        let mut final_args = args;
        let mut final_arg_tys = arg_tys;
        final_args.insert(0, TypedArgument {
            name: None, // Implicit self argument
            value: object_expr.clone(),
                    span: object_expr.span,
                });
        final_arg_tys.insert(0, receiver_ty.clone());
        
        check_resolved_invocation(
            checker,
            candidate_symbol,   // Always pass the candidate (trait or inherent) for signature lookup
            candidate_symbol,   // <<< Pass candidate_symbol here; will be resolved later if trait >>>
            &method_name_str,
            Some(&receiver_ty), // Pass receiver type
            impl_id_opt,        // <<< Pass the ImplId (or None for inherent) >>>
            final_args,
            final_arg_tys,
            span,
        )
    } else {
        // No candidates found
        Err(TypeError::NoMatchingMethod {
            method: method_name.to_string(),
            ty: display_type(&receiver_ty),
            candidates: None,
            span,
        })
    }
}

// TODO: Add helper functions for method resolution:
// - find_inherent_methods
// - find_trait_methods
// - resolve_method_overloads
// - instantiate_generic_method 

// Helper on FunctionSignature (needs implementation in types/defs.rs)
// impl FunctionSignature {
//     pub(crate) fn contains_self_type(&self) -> bool {
//         self.params.iter().any(|p| p.ty.contains_self_type()) || self.return_type.contains_self_type()
//         // Also check bounds on generic params if they can contain SelfType?
//     }
// } 

// --- UFCS Helpers (Placeholders) ---

fn check_ufcs_type_method(
    checker: &mut TypeChecker,
    _type_symbol: Symbol, // Symbol of the Struct/Enum (used for error messages)
    method_name: &str,
    args: Vec<TypedArgument>, // Includes receiver as first arg
    arg_tys: Vec<Ty>,         // Includes receiver type
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    if args.is_empty() || arg_tys.is_empty() {
        // This shouldn't happen if called correctly, but guard anyway
        return Err(TypeError::InternalError { message: "UFCS call missing receiver argument".to_string(), span: Some(span) });
    }

    let receiver_ty = &arg_tys[0];
    let _receiver_arg_value = &args[0].value; // Get the TypedExpr for the receiver
    let _other_args = &args[1..];        // Arguments excluding the receiver
    let _other_arg_tys = &arg_tys[1..];    // Types excluding the receiver

    println!(
        "Checking UFCS call: Type::{} on receiver type {}",
        method_name,
        display_type(receiver_ty)
    );

    // Find Candidates (similar to check_method_call)
    let inherent_candidates = checker.find_inherent_method_candidates(receiver_ty, method_name, span);
    let trait_candidates = checker.find_trait_method_candidates(receiver_ty, method_name, span);

    // Prioritize & Select (same logic as check_method_call)
    let selected_candidate_symbol: Option<Symbol> =
        if !inherent_candidates.is_empty() {
            if inherent_candidates.len() > 1 {
                checker.report_error(TypeError::AmbiguousMethodCall { method: method_name.to_string(), ty: display_type(receiver_ty), span });
                return Ok((TypedExprKind::Error, Ty::new(TyKind::Error)));
            }
            Some(inherent_candidates[0])
        } else if !trait_candidates.is_empty() {
             if trait_candidates.len() > 1 {
                 checker.report_error(TypeError::AmbiguousMethodCall { method: method_name.to_string(), ty: display_type(receiver_ty), span });
                 return Ok((TypedExprKind::Error, Ty::new(TyKind::Error)));
             }
             Some(trait_candidates[0].0) // Just the method symbol
        } else {
             None
        };

    // <<< Handle UFCS type method resolution >>>
    if let Some(method_symbol) = selected_candidate_symbol {
        // Need to determine if inherent or trait to pass correct ImplId
        let impl_id_opt = if inherent_candidates.contains(&method_symbol) {
            None // Inherent method
        } else {
            // Must be a trait method, find the corresponding ImplId
            trait_candidates.iter()
                .find(|(sym, _)| *sym == method_symbol)
                .map(|(_, id)| *id)
        };

        let method_name_str = checker.type_ctx.get_any_function_sig(checker.trait_repo, &method_symbol, span)
            .map_or(method_name.to_string(), |sig| sig.name.clone());
            
        check_resolved_invocation(
            checker,
            method_symbol,      // signature_symbol (used to fetch initial sig)
            method_symbol,      // <<< Pass method_symbol, resolved later if trait >>>
            &method_name_str,
            Some(receiver_ty), // Pass receiver type
            impl_id_opt,        // <<< Pass the ImplId (or None for inherent) >>>
            args,              // Pass original args (includes receiver)
            arg_tys.to_vec(), // Clone arg_tys into a Vec
                        span,
        )
    } else {
        // No candidates found
        Err(TypeError::NoMatchingMethod { method: method_name.to_string(), ty: display_type(receiver_ty), candidates: None, span })
    }
}

fn check_ufcs_trait_method(
    checker: &mut TypeChecker,
    trait_symbol: Symbol,
    method_name: &str,
    args: Vec<TypedArgument>, // Includes receiver as first arg
    arg_tys: Vec<Ty>,
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    if args.is_empty() || arg_tys.is_empty() {
        return Err(TypeError::InternalError { message: "UFCS trait call missing receiver argument".to_string(), span: Some(span) });
    }

    let receiver_ty = &arg_tys[0];
    let _receiver_arg_value = &args[0].value; // Keep for potential use in errors

    // Find the definition of the trait itself
    let trait_def = checker.trait_repo.get_trait_by_symbol(&trait_symbol)
        .cloned()
        .ok_or_else(|| TypeError::UnknownTrait { name: format!("Symbol {:?}", trait_symbol), span })?;

    // Find the specific method definition within the trait (use the cloned trait_def)
    let trait_method = trait_def.methods.values().find(|tm| tm.name == method_name)
        .ok_or_else(|| TypeError::NoMatchingMethod { method: method_name.to_string(), ty: trait_def.name.clone(), candidates: None, span })?;
    let trait_method_symbol = trait_method.method_symbol;

    // 3. Find the implementation using the receiver type
    // Construct a TraitRef for the required trait (potentially with fresh vars for trait generics)
    let trait_args: Vec<_> = (0..trait_def.generic_params.len()).map(|_| checker.fresh_var()).collect();
    let required_trait_ref = TraitRef {
        trait_id: trait_def.id,
        type_arguments: trait_args,
        span, // Use call site span
    };

    let impl_id_opt = checker.trait_repo.resolve_trait_implementation(receiver_ty, &required_trait_ref, &mut checker.infctx, span)?;

    if let Some(impl_id) = impl_id_opt {
        // --- Borrow checker fix: Get impl_func_symbol and generics count *before* borrowing ImplDef ---
            let temp_impl_def = checker.trait_repo.get_impl(impl_id).ok_or_else(|| TypeError::InternalError { message: "ImplDef not found during pre-borrow step".to_string(), span: Some(span)})?;
            let func_sym = temp_impl_def.methods.get(&trait_method_symbol)
                .or_else(|| temp_impl_def.default_method_impl_symbols.get(&trait_method_symbol))
            .copied();
        let impl_func_symbol = func_sym.ok_or_else(|| TypeError::InternalError { message: format!("Impl func symbol not found in ImplDef (UFCS pre-borrow) for trait method {:?}", trait_method_symbol), span: Some(span) })?;
            let generics_count = if let Some(sym) = func_sym {
            checker.type_ctx.get_any_function_sig(checker.trait_repo, &sym, span).map(|sig| sig.generic_params.len()).ok()
            } else {
                None
        };
        // --- End borrow checker fix ---

        // <<< ADD DETAILED DEBUG PRINT: LOOKUP DETAILS (TRAIT) >>>
        println!(
            "    Lookup details: impl_id={:?}, trait_method_symbol={:?}, methods={:?}, defaults={:?}",
            impl_id,
            trait_method_symbol,
            temp_impl_def.methods,
            temp_impl_def.default_method_impl_symbols
        );

        // Get method name from signature using the *trait_method_symbol* for consistency
        // before it potentially gets resolved to an impl_func_symbol inside check_resolved_invocation
        let method_name_str = checker.type_ctx.get_any_function_sig(checker.trait_repo, &trait_method_symbol, span)
            .map_or(method_name.to_string(), |sig| sig.name.clone());

        // Call the common invocation checker
        check_resolved_invocation(
            checker,              // 1
            trait_method_symbol, // 2: signature_symbol
            trait_method_symbol, // 3: <<< Pass trait_method_symbol, resolved later >>>
            &method_name_str,   // 4: func_name
            Some(receiver_ty),  // 5: receiver_ty_opt
            Some(impl_id),      // 6: <<< Pass the found ImplId >>>
            args,               // 7: args
            arg_tys.to_vec(),   // 8: arg_tys
            span,               // 9: span
        )

    } else {
        // No implementation found
        Err(TypeError::RequiredTraitNotImplemented { ty: display_type(receiver_ty), trait_name: trait_def.name, span })
    }
}

/// Internal helper to check an invocation once the specific function/method symbol is resolved.
/// Handles `Self` substitution, generic instantiation, argument checking, and HIR construction.
/// `receiver_ty_opt`: Should be Some(ty) for method calls (including UFCS), None for direct function calls.
/// `args`: Should include the receiver as the first argument for UFCS calls, but NOT for direct method calls (`object.method()`).
///         The `check_method_call` function adds the receiver before calling this.
/// `arg_tys`: Types corresponding to `args`.
pub(crate) fn check_resolved_invocation(
    checker: &mut TypeChecker,
    signature_symbol: Symbol,
    initial_actual_symbol: Symbol,
    func_name: &str,
    receiver_ty_opt: Option<&Ty>,
    impl_id_opt: Option<ImplId>,
    args: Vec<TypedArgument>,
    arg_tys: Vec<Ty>,
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    // 1. Get Original Signature & Clone necessary data
    let mut original_signature = checker.type_ctx.get_any_function_sig(
        checker.trait_repo,
        &signature_symbol, // Use signature_symbol to get the initial signature
        span,
    )?.clone(); // Clone here

    // <<< REFACTOR Self Substitution Logic >>>
    // Prioritize receiver_ty_opt if this call originates from method call syntax 
    // (including operator desugaring or UFCS), otherwise use the ambient Self type from the impl context.
    let self_ty_for_substitution = if receiver_ty_opt.is_some() {
        receiver_ty_opt
    } else {
        checker.current_self_type.as_ref()
    };

    if original_signature.contains_self_type() {
        if let Some(self_ty) = self_ty_for_substitution {
            let substituted = substitute_signature_self(
            &original_signature,
                self_ty,
            10,
        ).unwrap_or_else(|e| {
            checker.report_error(e);
            original_signature.clone()
        });
        original_signature = substituted;
    } else {
            // Contains Self, but no receiver provided and not in an impl context.
            return Err(TypeError::InternalError {
                message: format!(
                    "Signature for function {:?} contains Self but called without receiver or impl context",
                    signature_symbol
                ),
                span: Some(original_signature.span),
            });
        }
    }
    // <<< END REFACTOR >>>

    let original_generic_params = original_signature.generic_params.clone(); // Clone generics
    let _original_params = original_signature.params.clone(); // Clone params
    let _original_ret_type = original_signature.return_type.clone(); // Clone return type

    // 2. Instantiate Generic Parameters (Method or Function Generics)
    let mut generic_instantiation_subst = Substitution::new();
    let mut inferred_generic_args: Option<Vec<Ty>> = None;

    // Use the cloned generic params
    if !original_generic_params.is_empty() {
         let mut fresh_vars = Vec::with_capacity(original_generic_params.len());
        for gen_param in &original_generic_params {
            let fresh_var = checker.fresh_var(); // MUTABLE BORROW START
            generic_instantiation_subst.insert(gen_param.id, fresh_var.clone());
            fresh_vars.push(fresh_var.clone());
            // Add constraints from bounds, substituting Self within the bound if it's a method call
            for bound in &gen_param.bounds {
                 let concrete_bound = if let Some(receiver_ty) = receiver_ty_opt {
                     // Need to call substitute_self_in_trait_ref without borrowing checker mutably
                     // Assuming it doesn't need mutable access
                     match crate::internal::substitute_self_in_trait_ref(bound, receiver_ty, 10) {
                         Ok(b) => b,
                         Err(e) => { checker.report_error(e); bound.clone() }
                     }
                 } else {
                     bound.clone() // No Self to substitute
                 };
                  checker.add_constraint(fresh_var.clone(), concrete_bound); // MUTABLE BORROW
            } // MUTABLE BORROW END (potentially)
        }
        inferred_generic_args = Some(fresh_vars);
                 }

    // Apply generic instantiation substitution to the (potentially self-substituted) signature
    // Use the cloned params/return type from after self-substitution
    let instantiated_sig_params: Vec<_> = original_signature.params.iter().map(|p| ParamType {
        ty: p.ty.apply_subst(&generic_instantiation_subst),
                         ..p.clone()
    }).collect();
    let instantiated_sig_ret_type = original_signature.return_type.apply_subst(&generic_instantiation_subst);

    // 4. Determine Arguments to Check (excluding receiver if it's a method call with self)
    let is_method_call_with_self = receiver_ty_opt.is_some() && original_signature.self_param.is_some();

    // 5. Check Argument Count
    let num_expected_params = instantiated_sig_params.len();
    let num_expected_args = if is_method_call_with_self {
        num_expected_params + 1 
    } else {
        num_expected_params
    };
    let num_found_args = arg_tys.len(); // Compare against the full length of the received args

    if num_expected_args != num_found_args {
                     return Err(TypeError::WrongNumberOfArguments {
             expected: num_expected_args,
             found: num_found_args,
                         span,
                     });
                 }

    // 6. Unify Argument Types with Parameter Types
    let arg_tys_to_unify = if is_method_call_with_self {
        arg_tys.get(1..).unwrap_or(&[]) // Unify args *after* self
    } else {
        &arg_tys[..] // Unify all args
    };

    for (i, (arg_ty, param_info)) in arg_tys_to_unify.iter().zip(instantiated_sig_params.iter()).enumerate() {
        let arg_index = if receiver_ty_opt.is_some() && original_signature.self_param.is_some() { i + 1 } else { i };
        let arg_span = args.get(arg_index).map_or(span, |a| a.span);
        if !checker.unify(arg_ty, &param_info.ty, arg_span) {
        }
    }

    // <<< ADD: Resolve final function symbol AFTER unification >>>
    let final_actual_func_symbol = if let Some(impl_id) = impl_id_opt {
        // It's a trait method call, resolve the actual implementation symbol now.
        // `signature_symbol` holds the trait method symbol when impl_id is Some.
        let trait_method_symbol = signature_symbol;
        let impl_def = checker.trait_repo
            .get_impl(impl_id)
            .ok_or_else(|| TypeError::InternalError {
                message: format!(
                    "ImplDef {:?} not found during final symbol resolution",
                    impl_id
                ),
                span: Some(span),
            })?;

        // <<< ADD DETAILED DEBUG PRINT: LOOKUP DETAILS (TRAIT) >>>
        println!(
            "    Lookup details: impl_id={:?}, trait_method_symbol={:?}, methods={:?}, defaults={:?}",
            impl_id,
            trait_method_symbol,
            impl_def.methods,
            impl_def.default_method_impl_symbols
        );

        // Look up the trait method symbol first in explicitly implemented methods,
        // then in the generated default method symbols (populated in Pass 2).
        let resolved_symbol = impl_def.methods.get(&trait_method_symbol)
            .or_else(|| impl_def.default_method_impl_symbols.get(&trait_method_symbol))
            .copied()
            .ok_or_else(|| TypeError::InternalError {
                message: format!(
                    "Final impl func symbol not found in ImplDef {:?} for trait method {:?}",
                    impl_id,
                    trait_method_symbol
                ),
                span: Some(span),
            })?;

        // <<< ADD DEBUG PRINT: RESOLVED SYMBOL (TRAIT) >>>
        println!(
            "  [check_resolved_invocation] Resolved TRAIT method {:?} (impl {:?}) to actual symbol: {:?}",
            trait_method_symbol,
            impl_id,
            resolved_symbol
        );
        resolved_symbol
    } else {
        // It's an inherent method or direct function call.
        // `signature_symbol` already holds the correct concrete function symbol.
        // <<< ADD DEBUG PRINT: RESOLVED SYMBOL (INHERENT/DIRECT) >>>
        println!(
            "  [check_resolved_invocation] Using inherent/direct function symbol: {:?}",
            signature_symbol
        );
        signature_symbol
    };

    // 6. Determine Final Return Type and Type Arguments
    let final_ret_ty = checker.infctx.apply_substitution(&instantiated_sig_ret_type);
    
    let final_type_args = inferred_generic_args.map(|vars| {
        vars.iter().map(|v| checker.infctx.apply_substitution(v)).collect::<Vec<_>>()
    });

    // 7. Construct HIR Call Node
    //    The `func_expr` needs to represent the function/method itself.
    //    For direct calls, it's the original `func_expr`.
    //    For method/UFCS calls, it needs to be synthesized.
    let final_func_ty = Ty::with_span(
                     TyKind::Function(
            instantiated_sig_params.iter().map(|p| p.ty.clone()).collect(),
            final_ret_ty.clone().into() // Use the final substituted return type
                     ),
        original_signature.span // Use original signature span
                 );

    let final_func_expr = TypedExpr {
         // Use the resolved actual function symbol for the Variable kind in HIR
         kind: TypedExprKind::Variable { symbol: final_actual_func_symbol, name: func_name.to_string() }, // <<< USE final_actual_func_symbol
         ty: final_func_ty,
         span, // Use call site span
                 };

                let typed_call_kind = TypedExprKind::Call {
        func_expr: Box::new(final_func_expr),
        // Store the final actual function symbol in the Call node
        func_symbol: Some(final_actual_func_symbol), // <<< USE final_actual_func_symbol
        type_args: final_type_args,
        args: args,
                 };

                Ok((typed_call_kind, final_ret_ty))
}

// Helper method to extract func_symbol for debugging
// <<< Make public within crate for use in print >>>
impl TypedExprKind {
    pub(crate) fn get_func_symbol(&self) -> Option<Symbol> {
        match self {
            TypedExprKind::Call { func_symbol, .. } => *func_symbol,
            TypedExprKind::Variable { symbol, .. } => Some(*symbol),
            _ => None,
        }
    }
}