use std::sync::Arc;
use std::collections::HashMap;

use miette::SourceSpan;
use parallax_resolve::types::{ResolvedExpr, ResolvedExprKind, ResolvedArgument, ResolvedParameter, ResolvedType, Symbol};

// Update imports
use crate::{
    error::{TypeError, TypeResult},
    context::{inference::{Substitution, TypeEnvironment}, trait_repo::{TraitRef as CheckerTraitRef, TraitRepository, TraitId}},
    types::{
        FunctionSignature, 
        Ty, TyKind, TypeDef, TypedArgument, TypedExpr, TypedExprKind, 
        // Use crate::types::PrimitiveType consistently for checker types
        PrimitiveType, 
        SelfParamKind, TypeId, ParamType, Field, StructDef, TraitDef, ImplDef, 
    },
};
// Use alias for PrimitiveType from resolver
use parallax_resolve::types::PrimitiveType as ResolverPrimitiveType;
use super::{generics::instantiate, TypeChecker, expr::type_check_expression};

// Function to check lambda expressions
#[allow(dead_code)]
pub(crate) fn check_lambda(
    checker: &mut TypeChecker,
    params: &[ResolvedParameter],
    body: &ResolvedExpr,
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    // TODO: Handle generics defined *on* the lambda itself, if supported.
    let lambda_generic_param_map = HashMap::<String, crate::types::TypeId>::new();

    checker.generic_scopes.push(lambda_generic_param_map);

    let original_env = checker._type_env.clone();
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

    checker._type_env = Arc::new(lambda_env);
    let typed_body = type_check_expression(checker, body, None)?;
    let body_ty = checker.resolve_type(&typed_body.ty);

    checker._type_env = original_env;
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
#[allow(dead_code)]
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

/// Checks an explicit method call like `receiver.method_name(arg1, arg2)`.
/// Finds implementations providing the method, handles self parameter, generics, and arguments.
pub(crate) fn check_explicit_method_call(
     checker: &mut TypeChecker,
     receiver_expr: &ResolvedExpr,
     method_name: &str,
     args: &[ResolvedArgument],
     call_span: SourceSpan,
 ) -> TypeResult<(TypedExprKind, Ty)> {
     let typed_receiver = type_check_expression(checker, receiver_expr, None)?;
     let receiver_ty = checker.resolve_type(&typed_receiver.ty);
     // --- Method Lookup (Inherent first, then Trait) ---
     let mut found_method: Option<(Symbol, FunctionSignature, Option<(ImplDef, TraitDef)>)> = None;

     // A. Inherent Method Lookup
     if let TyKind::Named { name: type_name, .. } = &receiver_ty.kind {
         if let Some(type_symbol) = checker.type_ctx.get_symbol_for_name(type_name) {
             if let Some(inherent_methods) = checker.type_ctx.get_inherent_methods(&type_symbol) {
                 if let Some((method_symbol, inherent_sig)) = inherent_methods.iter().find(|(_, sig)| sig.name == *method_name) {
                      found_method = Some((*method_symbol, inherent_sig.clone(), None));
                 }
             }
         }
     }

     // B. Trait Method Lookup (if inherent not found)
     if found_method.is_none() {
         // Use updated trait repo method
         let potential_impls = checker.trait_repo.find_implementations_providing_method(
             &receiver_ty,
             method_name,
             &mut checker.inference_ctx // Pass inference context
         );

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

             found_method = Some((implementing_method_symbol, implementing_sig, Some((impl_def.clone(), trait_def.clone()))));
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
                     let _concrete_arg_ty = checker.resolve_type(&fresh_var_ty);
                     for bound in &gen_param.bounds {
                         let _obligation = CheckerTraitRef {
                             trait_id: bound.trait_id,
                             type_arguments: vec![], // TODO: Instantiate bound args
                             span: bound.span,
                         };
                          // Call pub(crate) method directly
                          // TODO: Uncomment when check_trait_implementation is available
                          // checker.check_trait_implementation(&concrete_arg_ty, &obligation)?;
                     }
                 }
             }
         }

         // 7. Determine Final Return Type
         let final_return_type = checker.resolve_type(&concrete_return_ty);

         // --- Add concrete signature to needed_functions ---
         // We need to store the signature *after* Self substitution and *after* method generic instantiation,
         // as this represents the concrete signature used at the call site.
         checker.needed_functions.borrow_mut().insert(implementing_method_symbol, current_sig.clone());
         // --- End Recording ---

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

/// Checks a direct function call like `my_function(arg1, arg2)`.
/// Handles resolving the function symbol, checking generic arguments, and unifying parameter/argument types.
pub(super) fn check_direct_function_call(
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
                 let _concrete_arg_ty = checker.resolve_type(&fresh_var_ty);
                 for bound in &gen_param.bounds {
                     let _obligation = CheckerTraitRef {
                         trait_id: bound.trait_id,
                         type_arguments: vec![], // TODO: Instantiate bound args
                         span: bound.span,
                     };
                     // Call pub(crate) method directly
                     // TODO: Uncomment when check_trait_implementation is available
                     // checker.check_trait_implementation(&concrete_arg_ty, &obligation)?;
                 }
             }
        }
    }

    // Determine Final Return Type
    let final_return_type = checker.resolve_type(&concrete_return_ty);

    // --- Add concrete signature to needed_functions ---
    checker.needed_functions.borrow_mut().insert(func_symbol, current_sig.clone());
    // --- End Recording ---

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{checker::TypeChecker, context::*, types::*, TypeDatabase, error::*};
    use parallax_resolve::{ResolveDatabase, types::*, definitions::*};
    use parallax_syntax::{SyntaxDatabase, ast::common::Literal as AstLiteral};
    use parallax_source::SourceDatabase;
    use miette::SourceSpan;
    use salsa::Database;
    use std::{collections::HashMap, sync::{Arc, Mutex}};

    // --- Test Setup ---
    #[salsa::db]
    #[derive(Default, Clone)]
    pub struct DummyDb { storage: salsa::Storage<Self> }
    impl salsa::Database for DummyDb { fn salsa_event(&self, event: &dyn Fn() -> salsa::Event) {} }
    #[salsa::db] impl SourceDatabase for DummyDb {}
    #[salsa::db] impl SyntaxDatabase for DummyDb {}
    #[salsa::db] impl ResolveDatabase for DummyDb {}
    #[salsa::db] impl TypeDatabase for DummyDb {}

    fn dummy_span() -> SourceSpan { SourceSpan::from((0, 0)) }

    fn setup_checker() -> TypeChecker<'static> {
        let db_mock = DummyDb::default();
        let db_leaked: &'static DummyDb = Box::leak(Box::new(db_mock));
        let defs_leaked: &'static ResolvedDefinitions = Box::leak(Box::new(ResolvedDefinitions::default()));
        let type_ctx = TypeContext::new();
        let trait_repo = TraitRepository::new();
        TypeChecker::new(db_leaked, defs_leaked, type_ctx, trait_repo)
    }

    fn ty_prim(prim: crate::types::PrimitiveType) -> Ty { Ty::with_span(TyKind::Primitive(prim), dummy_span()) }
    fn ty_unit() -> Ty { Ty::with_span(TyKind::Tuple(vec![]), dummy_span()) }
    fn ty_var(id: u32) -> Ty { Ty::with_span(TyKind::Var(TypeId(id)), dummy_span()) }
    fn ty_named(name: &str, symbol: Option<Symbol>, args: Vec<Ty>) -> Ty { Ty::with_span(TyKind::Named { name: name.to_string(), symbol, args }, dummy_span()) }
    fn ty_func(params: Vec<Ty>, ret: Ty) -> Ty { Ty::with_span(TyKind::Function(params, Arc::new(ret)), dummy_span()) }

    fn resolved_lit_int(val: i128) -> ResolvedExpr { ResolvedExpr { kind: ResolvedExprKind::Literal(AstLiteral::Int { value: val, suffix: None }), span: dummy_span(), resolved_type: ResolvedType::IntegerLiteral } }
    fn resolved_lit_bool(val: bool) -> ResolvedExpr { ResolvedExpr { kind: ResolvedExprKind::Literal(AstLiteral::Bool(val)), span: dummy_span(), resolved_type: ResolvedType::Primitive(ResolverPrimitiveType::Bool) } }
    fn resolved_path(sym: Symbol, rt: ResolvedType) -> ResolvedExpr { ResolvedExpr { kind: ResolvedExprKind::Path(sym), span: dummy_span(), resolved_type: rt } }
    fn resolved_ident(name: &str, sym: Symbol, rt: ResolvedType) -> ResolvedExpr { ResolvedExpr { kind: ResolvedExprKind::Variable{ binding_symbol: sym, name: name.to_string() }, span: dummy_span(), resolved_type: rt } }
    fn resolved_field_access(obj: ResolvedExpr, field: &str) -> ResolvedExpr { ResolvedExpr { kind: ResolvedExprKind::Field { object: Box::new(obj), field_name: field.to_string() }, span: dummy_span(), resolved_type: ResolvedType::Unknown /* Field access type is unknown until check */ } }
    fn resolved_call(func: ResolvedExpr, args: Vec<ResolvedArgument>) -> ResolvedExpr { ResolvedExpr { kind: ResolvedExprKind::Call { func_symbol: None, /* TODO: fix this dummy func_symbol or update ResolvedExprKind */ args }, span: dummy_span(), resolved_type: ResolvedType::Unknown /* Call type is unknown until check */ } }
    fn resolved_arg(val: ResolvedExpr) -> ResolvedArgument { ResolvedArgument { name: None, value: val, span: dummy_span() } }
    fn resolved_lambda(params: Vec<ResolvedParameter>, body: ResolvedExpr) -> ResolvedExpr { ResolvedExpr { kind: ResolvedExprKind::Lambda { params, body: Box::new(body) }, span: dummy_span(), resolved_type: ResolvedType::Unknown } }
    fn resolved_param(name: &str, sym: Symbol, ty: ResolvedType) -> ResolvedParameter { ResolvedParameter { name: name.to_string(), symbol: sym, param_type: ty, is_variadic: false, has_default: false, span: dummy_span() } }

    fn add_function_sig(checker: &mut TypeChecker, name: &str, sym: Symbol, params: Vec<(&str, Ty)>, ret: Ty) {
        let param_types = params.into_iter().map(|(n, t)| ParamType { name: n.to_string(), ty: t, span: dummy_span() }).collect();
        let sig = FunctionSignature {
            name: name.to_string(),
            self_param: None,
            generic_params: vec![],
            params: param_types,
            return_type: ret,
            span: dummy_span(),
        };
        checker.type_ctx.add_type(sym, name.to_string(), TypeDef::Function(sig));
    }
     fn add_struct_def(checker: &mut TypeChecker, name: &str, sym: Symbol, fields: Vec<(String, Symbol, Ty)>) {
        let fields_def = fields.into_iter().map(|(n, fs, t)| Field { name: n, symbol: fs, ty: t, span: dummy_span() }).collect();
        let struct_def = StructDef { name: name.to_string(), symbol: sym, generic_params: vec![], fields: fields_def, span: dummy_span() };
        checker.type_ctx.add_type(sym, name.to_string(), TypeDef::Struct(struct_def));
    }
    
    #[test]
    fn test_ty_prim_helper() {
        // Ensure ty_prim uses the correct PrimitiveType (from crate::types)
        let ty = ty_prim(crate::types::PrimitiveType::F32);
        assert!(matches!(ty.kind, TyKind::Primitive(crate::types::PrimitiveType::F32)));
    }
    
    #[test]
    fn test_check_lambda_no_params() {
        let mut checker = setup_checker();
        let body = resolved_lit_int(42);
        let (kind, ty) = check_lambda(&mut checker, &[], &body, dummy_span()).unwrap();
        match kind {
            TypedExprKind::Lambda { params, body: typed_body } => {
                assert!(params.is_empty());
                assert!(matches!(typed_body.kind, TypedExprKind::IntLiteral { value: 42, .. }));
            }
            _ => panic!("Expected Lambda kind"),
        }
        match ty.kind {
            TyKind::Function(param_tys, ret_ty) => {
                assert!(param_tys.is_empty());
                assert_eq!(ret_ty.kind, TyKind::Primitive(crate::types::PrimitiveType::I32));
            }
            _ => panic!("Expected Function type"),
        }
    }

    #[test]
    fn test_check_lambda_with_params_typed() {
        let mut checker = setup_checker();
        let x_sym = Symbol::new(1);
        let params = vec![resolved_param("x", x_sym, ResolvedType::Primitive(ResolverPrimitiveType::Bool))]; 
        let body = resolved_ident("x", x_sym, ResolvedType::Primitive(ResolverPrimitiveType::Bool)); // Return x
        let (kind, ty) = check_lambda(&mut checker, &params, &body, dummy_span()).unwrap();

        match kind {
            TypedExprKind::Lambda { params: typed_params, body: typed_body } => {
                assert_eq!(typed_params.len(), 1);
                assert_eq!(typed_params[0].0, x_sym);
                assert_eq!(typed_params[0].1.kind, TyKind::Primitive(crate::types::PrimitiveType::Bool));
                assert!(matches!(typed_body.kind, TypedExprKind::Variable { symbol, .. } if symbol == x_sym));
            }
            _ => panic!("Expected Lambda kind"),
        }
         match ty.kind {
            TyKind::Function(param_tys, ret_ty) => {
                assert_eq!(param_tys.len(), 1);
                assert_eq!(param_tys[0].kind, TyKind::Primitive(crate::types::PrimitiveType::Bool));
                assert_eq!(ret_ty.kind, TyKind::Primitive(crate::types::PrimitiveType::Bool));
            }
            _ => panic!("Expected Function type"),
        }
    }

    #[test]
    fn test_check_lambda_with_params_inferred() {
        let mut checker = setup_checker();
        let x_sym = Symbol::new(1);
        let params = vec![resolved_param("x", x_sym, ResolvedType::Unknown)]; // Infer x
        let body = resolved_ident("x", x_sym, ResolvedType::Unknown);
        let (kind, ty) = check_lambda(&mut checker, &params, &body, dummy_span()).unwrap();

        // Type remains generic/variable as nothing constrains it
        match ty.kind {
            TyKind::Function(param_tys, ret_ty) => {
                assert_eq!(param_tys.len(), 1);
                assert!(matches!(param_tys[0].kind, TyKind::Var(_)));
                assert!(matches!(ret_ty.kind, TyKind::Var(_)));
                // Check they are the *same* variable
                assert_eq!(param_tys[0].kind, ret_ty.kind);
            }
            _ => panic!("Expected Function type"),
        }
    }

    // --- Invocation Tests (Direct Call) ---
    #[test]
    fn test_check_direct_call_simple() {
        let mut checker = setup_checker();
        let func_sym = Symbol::new(1);
        add_function_sig(&mut checker, "add", func_sym, vec![("a", ty_prim(crate::types::PrimitiveType::I32)), ("b", ty_prim(crate::types::PrimitiveType::I32))], ty_prim(crate::types::PrimitiveType::I32));
        let func_expr = resolved_path(func_sym, ResolvedType::Unknown);
        let args = vec![
            resolved_arg(resolved_lit_int(1)),
            resolved_arg(resolved_lit_int(2)),
        ];

        let (kind, ty) = check_invocation(&mut checker, &func_expr, &args, dummy_span()).unwrap();
        match kind {
            TypedExprKind::Call { func, args: typed_args } => {
                assert!(matches!(func.kind, TypedExprKind::Variable { symbol, .. } if symbol == func_sym));
                assert_eq!(typed_args.len(), 2);
            }
            _ => panic!("Expected Call kind"),
        }
        assert_eq!(ty.kind, TyKind::Primitive(crate::types::PrimitiveType::I32));
    }

     #[test]
    fn test_check_direct_call_wrong_arg_count() {
        let mut checker = setup_checker();
        let func_sym = Symbol::new(1);
        add_function_sig(&mut checker, "add", func_sym, vec![("a", ty_prim(crate::types::PrimitiveType::I32)), ("b", ty_prim(crate::types::PrimitiveType::I32))], ty_prim(crate::types::PrimitiveType::I32));
        let func_expr = resolved_path(func_sym, ResolvedType::Unknown);
        let args = vec![resolved_arg(resolved_lit_int(1))]; // Only one arg

        let result = check_invocation(&mut checker, &func_expr, &args, dummy_span());
        assert!(result.is_err());
        assert!(matches!(result.err().unwrap(), TypeError::WrongNumberOfArguments { expected: 2, found: 1, .. }));
    }

    #[test]
    fn test_check_direct_call_wrong_arg_type() {
        let mut checker = setup_checker();
        let func_sym = Symbol::new(1);
        add_function_sig(&mut checker, "add", func_sym, vec![("a", ty_prim(crate::types::PrimitiveType::I32)), ("b", ty_prim(crate::types::PrimitiveType::I32))], ty_prim(crate::types::PrimitiveType::I32));
        let func_expr = resolved_path(func_sym, ResolvedType::Unknown);
        let args = vec![
            resolved_arg(resolved_lit_int(1)),
            resolved_arg(resolved_lit_bool(false)), // Wrong type
        ];

        let result = check_invocation(&mut checker, &func_expr, &args, dummy_span());
        assert!(result.is_err());
        assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
    }

    // --- Invocation Tests (Method Call) ---
    #[test]
    fn test_check_method_call_inherent() {
        let mut checker = setup_checker();
        let struct_sym = Symbol::new(10);
        let method_sym = Symbol::new(11);
        add_struct_def(&mut checker, "Counter", struct_sym, vec![]);

        // Add inherent method signature
        let sig = FunctionSignature {
            name: "inc".to_string(), self_param: Some(crate::types::SelfParamKind::Value), generic_params: vec![],
            params: vec![], return_type: ty_unit(), span: dummy_span(),
        };
        checker.type_ctx.add_inherent_method(struct_sym, method_sym, sig);

        let receiver_var_sym = Symbol::new(100);
        let receiver_expr = resolved_ident("c", receiver_var_sym, ResolvedType::UserDefined { symbol: struct_sym, type_args: None });
        let mut current_env = (*checker._type_env).clone();
        current_env.add("c".to_string(), ty_named("Counter", Some(struct_sym), vec![]));
        checker._type_env = Arc::new(current_env);

        let method_call_expr = resolved_field_access(receiver_expr, "inc");

        // Invoke check_invocation on the field access expression itself
        let (kind, ty) = check_invocation(&mut checker, &method_call_expr, &[], dummy_span()).unwrap();

        // The check_invocation for a field access should return a Call kind targeting the *implementing* function
        match kind {
             TypedExprKind::Call { func, args } => {
                 assert!(matches!(func.kind, TypedExprKind::Variable { symbol, .. } if symbol == method_sym), "Call func was not Variable {:?}", func.kind);
                 assert_eq!(args.len(), 1); // Receiver is passed as first arg
                 assert!(matches!(&args[0].value.kind, TypedExprKind::Variable { name, .. } if name == "c"));
             }
             _ => panic!("Expected Call kind, got {:?}", kind),
         }
        assert_eq!(ty, ty_unit());
    }

    // TODO: Add tests for trait method calls
    // TODO: Add tests for method calls with generics
    // TODO: Add tests for ambiguous method calls
    // TODO: Add tests for closure calls (non-path, non-field func_expr)
} 