use std::sync::Arc;
use std::collections::HashMap;

use miette::SourceSpan;
use parallax_resolve::types::{ResolvedExpr, ResolvedExprKind, ResolvedArgument, ResolvedParameter, ResolvedType, Symbol};

use crate::{
    error::{TypeError, TypeResult},
    inference::{Substitution, TypeEnvironment},
    typecheck::{helpers::instantiate, TypeChecker},
    types::{
        FunctionSignature, ImplDef, TraitDef, TraitRef as CheckerTraitRef,
        Ty, TyKind, TypeDef, TypedArgument, TypedExpr, TypedExprKind,
    },
};

// Function to check lambda expressions
pub(crate) fn check_lambda(
    checker: &mut TypeChecker,
    params: &[ResolvedParameter],
    body: &ResolvedExpr,
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    // TODO: Handle generics defined *on* the lambda itself, if supported.
    let lambda_generic_param_map = HashMap::<String, crate::types::TypeId>::new();

    checker.generic_scopes.push(lambda_generic_param_map);

    let original_env = checker.type_env.clone();
    let mut lambda_env = TypeEnvironment::with_parent(original_env.clone());

    let mut typed_lambda_params = Vec::with_capacity(params.len());
    let mut param_tys = Vec::with_capacity(params.len());

    for param in params {
        let param_ty = match &param.param_type {
            ResolvedType::Unknown => checker.fresh_infer_var(param.span),
            resolved_annotation => checker.resolve_type_to_ty(resolved_annotation)?,
        };
        lambda_env.add(param.name.clone(), param_ty.clone());
        param_tys.push(param_ty.clone());
        typed_lambda_params.push((param.symbol, param_ty));
    }

    checker.type_env = Arc::new(lambda_env);
    let typed_body = type_check_expression(checker, body, None)?;
    let body_ty = checker.resolve_type(&typed_body.ty);

    checker.type_env = original_env;
    checker.generic_scopes.pop();

    let lambda_func_ty = Ty::with_span(TyKind::Function(param_tys, Arc::new(body_ty)), span);

    Ok((
        TypedExprKind::Lambda {
            params: typed_lambda_params,
            body: Box::new(typed_body),
        },
        lambda_func_ty,
    ))
}

/// Type check a function call, method call, or field access resulting in a call.
/// Returns a TypedExprKind::Call targeting the concrete implementing function.
pub(crate) fn check_invocation(
    checker: &mut TypeChecker,
    func_expr: &ResolvedExpr, // The expression producing the function/method
    args: &[ResolvedArgument],
    call_span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    match &func_expr.kind {
        // Case 1: Field Access like `receiver.method(...)`
        ResolvedExprKind::Field { object, field_name } => {
            check_explicit_method_call(checker, object, field_name, args, call_span)
        }

        // Case 2: Path like `function_name(...)` or `module::function(...)`
        ResolvedExprKind::Path(symbol) => {
            let func_symbol = *symbol;
            check_direct_function_call(checker, func_symbol, args, func_expr.span, call_span)
        }

        // Case 3: Other expression producing a function value, e.g., `(get_closure())(...)`
        _ => {
            let typed_func_val_expr = type_check_expression(checker, func_expr, None)?;
            let func_ty = checker.resolve_type(&typed_func_val_expr.ty);

            if let TyKind::Function(expected_param_tys, expected_ret_ty) = func_ty.kind {
                 // TODO: Handle generics defined *on the closure type* itself, if applicable.

                 if args.len() != expected_param_tys.len() {
                     return Err(TypeError::WrongNumberOfArguments {
                         expected: expected_param_tys.len(),
                         found: args.len(),
                         span: call_span,
                     });
                 }

                 let mut typed_args = Vec::with_capacity(args.len());
                 for (arg, expected_ty) in args.iter().zip(expected_param_tys.iter()) {
                      let typed_arg_value = type_check_expression(checker, &arg.value, Some(expected_ty))?;
                      typed_args.push(TypedArgument {
                          name: arg.name.clone(),
                          value: typed_arg_value,
                          span: arg.span,
                      });
                 }

                let final_return_type = checker.resolve_type(&expected_ret_ty);

                let call_kind = TypedExprKind::Call {
                    func: Box::new(typed_func_val_expr),
                    args: typed_args,
                };

                Ok((call_kind, final_return_type))

            } else {
                Err(TypeError::NotAFunction {
                    found: crate::error::display_type(&func_ty),
                    span: func_expr.span,
                })
            }
        }
    }
}

/// Handles `receiver.method(args)` syntax. Finds inherent/trait method, resolves implementation.
fn check_explicit_method_call(
     checker: &mut TypeChecker,
     receiver_expr: &ResolvedExpr,
     method_name: &str,
     args: &[ResolvedArgument],
     call_span: SourceSpan,
 ) -> TypeResult<(TypedExprKind, Ty)> {
     let typed_receiver = type_check_expression(checker, receiver_expr, None)?;
     let receiver_ty = checker.resolve_type(&typed_receiver.ty);

     println!("Attempting method call: {}.{}(...) on type {}", crate::error::display_type(&receiver_ty), method_name, crate::error::display_type(&receiver_ty));

     // --- Method Lookup (Inherent first, then Trait) ---
     let mut found_method: Option<(Symbol, FunctionSignature, Option<(ImplDef, TraitDef)>)> = None;

     // A. Inherent Method Lookup
     if let TyKind::Named { name: type_name, .. } = &receiver_ty.kind {
         if let Some(type_symbol) = checker.type_ctx.get_symbol_for_name(type_name) {
             if let Some(inherent_methods) = checker.type_ctx.get_inherent_methods(&type_symbol) {
                 if let Some((method_symbol, inherent_sig)) = inherent_methods.iter().find(|(_, sig)| sig.name == *method_name) {
                      println!("  Found inherent method: {}", inherent_sig.name);
                      found_method = Some((*method_symbol, inherent_sig.clone(), None));
                 }
             }
         }
     }

     // B. Trait Method Lookup (if inherent not found)
     if found_method.is_none() {
        println!("  No inherent method '{}' found. Searching traits...", method_name);
         // Use updated trait repo method
         let potential_impls = checker.trait_repo.find_implementations_providing_method(&receiver_ty, method_name);

         if potential_impls.len() == 1 {
             let (impl_id, trait_method_symbol, trait_id) = potential_impls[0];
             // These unwraps are safe because find_implementations_providing_method should only return valid IDs
             let impl_def = checker.trait_repo.impls.get(&impl_id).unwrap();
             let trait_def = checker.trait_repo.traits.get(&trait_id).unwrap();
             let implementing_method_symbol = *impl_def.methods.get(&trait_method_symbol).unwrap();

             let implementing_sig = match checker.type_ctx.get_type_by_symbol(&implementing_method_symbol) {
                 Some(TypeDef::Function(sig)) => sig.clone(),
                 _ => return Err(TypeError::InternalError{ message: format!("Signature not found for implementing method symbol {:?} for trait method '{}'", implementing_method_symbol, method_name), span: Some(call_span) })
             };

             println!("  Found method via trait '{}' impl {:?}, implementing symbol: {:?}", trait_def.name, impl_def.impl_symbol, implementing_method_symbol);
             found_method = Some((implementing_method_symbol, implementing_sig, Some((impl_def.clone(), trait_def.clone()))));

         } else if potential_impls.is_empty() {
             println!("  No trait implementations found providing method '{}' for type {}", method_name, crate::error::display_type(&receiver_ty));
         } else {
              return Err(TypeError::AmbiguousMethodCall {
                   method: method_name.to_string(),
                   ty: crate::error::display_type(&receiver_ty),
                   span: call_span,
              });
         }
     }

     // --- Process Found Method ---
     if let Some((implementing_method_symbol, base_method_sig, method_origin)) = found_method {

         let mut current_sig = base_method_sig;

         // 1. Apply Trait Substitution (if from trait impl)
         if let Some((impl_def, trait_def)) = &method_origin {
              let mut subst = Substitution::new();
              if let Some(trait_ref) = &impl_def.trait_ref {
                  if trait_def.generic_params.len() != trait_ref.type_arguments.len() {
                      return Err(TypeError::GenericArgCountMismatch {
                          kind: "Trait implementation".to_string(),
                          name: trait_def.name.clone(),
                          expected: trait_def.generic_params.len(),
                          found: trait_ref.type_arguments.len(),
                          span: impl_def.span,
                      });
                  } else {
                      for (trait_param, impl_arg_ty) in trait_def.generic_params.iter().zip(trait_ref.type_arguments.iter()) {
                          subst.insert(trait_param.id, impl_arg_ty.clone());
                      }
                  }
              }
              current_sig = checker.manual_substitute_signature(&current_sig, &subst)?;
         }

         // 2. Apply Self Substitution
         let mut self_subst = Substitution::new();
         let has_self = current_sig.self_param.is_some();
         if has_self {
             self_subst.insert_self(&receiver_ty);
         } else {
              return Err(TypeError::NotAMethod {
                  method: method_name.to_string(),
                  ty: crate::error::display_type(&receiver_ty),
                  span: call_span,
              });
         }
         current_sig = checker.manual_substitute_signature(&current_sig, &self_subst)?;

         // 3. Instantiate Method's Own Generics
         let (_instantiated_sig_ty, generic_to_fresh_map) = instantiate(
             checker,
             &Ty::new(TyKind::Function(vec![], Arc::new(Ty::new(TyKind::Error)))), // Dummy base
             call_span,
             &current_sig.generic_params
         )?;
          let mut method_generic_subst = Substitution::new();
         for (gen_id, fresh_id) in &generic_to_fresh_map {
             method_generic_subst.insert(*gen_id, Ty::new(TyKind::Var(*fresh_id)));
         }
         current_sig = checker.manual_substitute_signature(&current_sig, &method_generic_subst)?;

         let expected_param_tys = current_sig.params.iter().map(|p| p.ty.clone()).collect::<Vec<_>>();
         let concrete_return_ty = current_sig.return_type.clone();

         // 4. Check Argument Count
         if args.len() != expected_param_tys.len() {
              return Err(TypeError::WrongNumberOfArguments {
                 expected: expected_param_tys.len(),
                 found: args.len(),
                 span: call_span,
             });
         }

         // 5. Check Arguments & Prepare Call Args (including receiver)
         let mut typed_call_args = Vec::with_capacity(args.len() + 1);
         // Prepend receiver
         typed_call_args.push(TypedArgument { name: None, value: typed_receiver, span: receiver_expr.span });

         for (arg, expected_ty) in args.iter().zip(expected_param_tys.iter()) {
             let typed_arg_val = type_check_expression(checker, &arg.value, Some(expected_ty))?;
             typed_call_args.push(TypedArgument { name: arg.name.clone(), value: typed_arg_val, span: arg.span });
         }

         // 6. Check method generic bounds AFTER arguments are unified
          if !current_sig.generic_params.is_empty() {
             for gen_param in &current_sig.generic_params {
                 if let Some(fresh_var_id) = generic_to_fresh_map.get(&gen_param.id) {
                     let fresh_var_ty = Ty::new(TyKind::Var(*fresh_var_id));
                     let concrete_arg_ty = checker.resolve_type(&fresh_var_ty);
                     for bound in &gen_param.bounds {
                         let obligation = CheckerTraitRef {
                             trait_id: bound.trait_id,
                             type_arguments: vec![], // TODO: Instantiate bound args
                             span: bound.span,
                         };
                          // Call pub(crate) method directly
                          checker.check_trait_implementation(&concrete_arg_ty, &obligation)?;
                     }
                 }
             }
         }

         // 7. Determine Final Return Type
         let final_return_type = checker.resolve_type(&concrete_return_ty);

         // 8. Construct TypedExprKind::Call targeting the implementing function symbol
         let implementing_func_ty = match checker.type_ctx.get_type_by_symbol(&implementing_method_symbol) {
             Some(TypeDef::Function(sig)) => Ty::new(TyKind::Function(
                 sig.params.iter().map(|p| p.ty.clone()).collect(),
                 Arc::new(sig.return_type.clone()),
             )),
             _ => Ty::new(TyKind::Error),
         };
         let typed_impl_func_expr = TypedExpr {
             kind: TypedExprKind::Variable { symbol: implementing_method_symbol, name: current_sig.name.clone() },
             ty: checker.resolve_type(&implementing_func_ty),
             span: call_span,
         };

         let call_kind = TypedExprKind::Call {
             func: Box::new(typed_impl_func_expr),
             args: typed_call_args, // Includes receiver
         };

         Ok((call_kind, final_return_type))

     } else {
          Err(TypeError::NoMatchingMethod {
             method: method_name.to_string(),
             ty: crate::error::display_type(&receiver_ty),
             candidates: None,
             span: call_span,
         })
     }
}

/// Handles direct function calls like `func(args)`.
fn check_direct_function_call(
    checker: &mut TypeChecker,
    func_symbol: Symbol,
    args: &[ResolvedArgument],
    func_expr_span: SourceSpan,
    call_span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    let base_func_sig = match checker.type_ctx.get_type_by_symbol(&func_symbol) {
         Some(TypeDef::Function(sig)) => sig.clone(),
         _ => {
             let name = checker.get_name_for_symbol(func_symbol)?;
             // TODO: Check environment for closure variable by symbol?
             return Err(TypeError::NotAFunction { found: name, span: call_span });
         }
     };

    // Check for self param - direct calls cannot have one
    if base_func_sig.self_param.is_some() {
        return Err(TypeError::InternalError {
            message: format!("Function '{}' requires 'self' but was called directly", base_func_sig.name),
            span: Some(call_span),
        });
    }

    // Instantiate Function Generics
    let (_instantiated_sig_ty, generic_to_fresh_map) = instantiate(
        checker,
         &Ty::new(TyKind::Function(vec![], Arc::new(Ty::new(TyKind::Error)))), // Dummy base
        call_span,
        &base_func_sig.generic_params,
    )?;
     let mut func_generic_subst = Substitution::new();
     for (gen_id, fresh_id) in &generic_to_fresh_map {
         func_generic_subst.insert(*gen_id, Ty::new(TyKind::Var(*fresh_id)));
     }
     let current_sig = checker.manual_substitute_signature(&base_func_sig, &func_generic_subst)?;
     let expected_param_tys = current_sig.params.iter().map(|p| p.ty.clone()).collect::<Vec<_>>();
     let concrete_return_ty = current_sig.return_type.clone();

    // Check Argument Count
    if args.len() != expected_param_tys.len() {
         return Err(TypeError::WrongNumberOfArguments {
             expected: expected_param_tys.len(),
             found: args.len(),
             span: call_span,
         });
     }

    // Check Arguments
    let mut typed_call_args = Vec::with_capacity(args.len());
    for (arg, expected_ty) in args.iter().zip(expected_param_tys.iter()) {
         let typed_arg_val = type_check_expression(checker, &arg.value, Some(expected_ty))?;
         typed_call_args.push(TypedArgument { name: arg.name.clone(), value: typed_arg_val, span: arg.span });
    }

    // Check Function Generic Bounds
     if !base_func_sig.generic_params.is_empty() {
        for gen_param in &base_func_sig.generic_params {
            if let Some(fresh_var_id) = generic_to_fresh_map.get(&gen_param.id) {
                 let fresh_var_ty = Ty::new(TyKind::Var(*fresh_var_id));
                 let concrete_arg_ty = checker.resolve_type(&fresh_var_ty);
                 for bound in &gen_param.bounds {
                     let obligation = CheckerTraitRef {
                         trait_id: bound.trait_id,
                         type_arguments: vec![], // TODO: Instantiate bound args
                         span: bound.span,
                     };
                     // Call pub(crate) method directly
                     checker.check_trait_implementation(&concrete_arg_ty, &obligation)?;
                 }
             }
        }
    }

    // Determine Final Return Type
    let final_return_type = checker.resolve_type(&concrete_return_ty);

    // Construct TypedExprKind::Call
    let func_ref_ty = match checker.type_ctx.get_type_by_symbol(&func_symbol) {
        Some(TypeDef::Function(sig)) => Ty::new(TyKind::Function(
             sig.params.iter().map(|p|p.ty.clone()).collect(),
             Arc::new(sig.return_type.clone())
         )),
         _ => Ty::new(TyKind::Error),
     };
    let typed_func_expr = TypedExpr {
        kind: TypedExprKind::Variable { symbol: func_symbol, name: base_func_sig.name },
        ty: checker.resolve_type(&func_ref_ty),
        span: func_expr_span,
    };

    let call_kind = TypedExprKind::Call {
        func: Box::new(typed_func_expr),
        args: typed_call_args,
    };

    Ok((call_kind, final_return_type))
}

// Forward declaration needed because functions are moved
use crate::typecheck::expressions::type_check_expression;