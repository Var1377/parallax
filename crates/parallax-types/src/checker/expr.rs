// src/checker/expr.rs
//! Type checking pass 2: Checking expressions.

use super::TypeChecker;
use crate::error::{TypeError, TypeResult, display_type};
use crate::types::{
    Ty, TyKind, TypedExpr, TypedExprKind, TypedMatchArm, PrimitiveType, TypedArgument,
    Field, TypeDef, EnumDef, EnumVariant, GenericParamDef, TypedParameter
};
use crate::context::{TypeEnvironment, Substitution};
use parallax_resolve::types::{Symbol, ResolvedExpr, ResolvedExprKind, ResolvedArgument, ResolvedPattern, ResolvedParameter, ResolvedType};
use parallax_syntax::ast::{common::Literal as AstLiteral, BinaryOp, UnaryOp};
use miette::SourceSpan;
use std::{sync::Arc, collections::HashSet};

/// Type checks a single resolved expression node.
///
/// Preconditions: `checker` context is set up for the current scope.
///                `resolved_expr` is the expression node to check.
///                `expected_ty` is an optional hint for the expected type (e.g., from assignment or function return).
/// Postconditions: Returns `Ok(TypedExpr)` representing the type-checked expression.
///                 Returns `Err(TypeError)` if checking fails.
///                 `checker.infctx` may be updated with new substitutions/constraints.
pub(crate) fn check_expr(checker: &mut TypeChecker, resolved_expr: &ResolvedExpr, expected_ty: Option<&Ty>) -> TypeResult<TypedExpr> {
    let span = resolved_expr.span;
    println!("Checking expr kind: {:?} at {:?}", resolved_expr.kind, span); // Basic debug

    match &resolved_expr.kind {
        ResolvedExprKind::Literal(lit) => {
            let (kind, ty) = check_literal(checker, lit, span)?;
            Ok(TypedExpr { kind, ty, span })
        }
        ResolvedExprKind::Variable { binding_symbol, name } => {
            let (kind, ty) = check_variable_ref(checker, *binding_symbol, name, span)?;
            Ok(TypedExpr { kind, ty, span })
        }
        ResolvedExprKind::SelfRef => {
            // Look up "self" in the environment to get the correct type and symbol
            if let Some((self_ty, self_symbol)) = checker.env.get("self") {
                Ok(TypedExpr {
                    kind: TypedExprKind::Variable { symbol: self_symbol, name: "self".to_string() },
                    ty: self_ty, // Use the type from the environment
                    span,
                })
            } else {
                // If "self" is not in the environment, it means we are not in a valid method scope.
                Err(TypeError::SelfOutsideImplOrTrait { span })
            }
        }
        ResolvedExprKind::Path(symbol) => {
            let (kind, ty) = check_path(checker, *symbol, span)?;
            Ok(TypedExpr { kind, ty, span })
        }
        ResolvedExprKind::Binary { op, left, right } => {
            let typed_left = check_expr(checker, left, None)?;
            let typed_right = check_expr(checker, right, None)?;
            let (kind, ty) = super::operators::check_binary_op(checker, op, &typed_left, &typed_right, span)?;
            Ok(TypedExpr { kind, ty, span })
        }
        ResolvedExprKind::Unary { op, expr } => {
            println!("[check_expr] Checking operand for Unary Op {:?}: {:?}", op, expr);
            let typed_operand = check_expr(checker, expr, None)?;
            println!(
                "[check_expr] Checked operand for Unary Op {:?}, result type: {}",
                op,
                display_type(&typed_operand.ty)
            );
            let (kind, ty) = super::operators::check_unary_op(checker, op, &typed_operand, span)?;
            Ok(TypedExpr { kind, ty, span })
        }
        ResolvedExprKind::Block(block_exprs) => {
            let (kind, ty) = check_block(checker, block_exprs, expected_ty, span)?;
            Ok(TypedExpr { kind, ty, span })
        }
        ResolvedExprKind::If { condition, then_branch, else_branch } => {
            let (kind, ty) = check_if_expr(checker, condition, then_branch, else_branch.as_deref(), expected_ty, span)?;
            Ok(TypedExpr { kind, ty, span })
        }
        ResolvedExprKind::Match { scrutinee, arms } => {
            let (kind, ty) = check_match_expr(checker, scrutinee, arms, expected_ty, span)?;
            Ok(TypedExpr { kind, ty, span })
        }
        ResolvedExprKind::Call { func_symbol, args } => {
            // <<< Check if this is a call to a trait method *within* a default body check >>>
            if let (Some(fn_sym), Some((current_trait_id, current_impl_id))) = (func_symbol, checker.current_default_body_context) {
                 if let Some(trait_id_of_method) = checker.trait_repo.get_trait_id_for_method(&fn_sym) {
                    if trait_id_of_method == current_trait_id {
                        // This is a call to a method of the SAME trait inside its default body.
                        // <<< REMOVE PRINT RESOLVED ARGS >>>

                        // Check remaining arguments provided by the resolver for the call node
                        let mut typed_call_args = Vec::new();
                        let mut call_arg_tys = Vec::new();
                        for arg_expr in args.iter() { // <<< Iterate over ALL args from resolver >>>
                            let typed_arg = check_expr(checker, &arg_expr.value, None)?; // Check argument
                            typed_call_args.push(TypedArgument { name: arg_expr.name.clone(), value: typed_arg.clone(), span: arg_expr.span });
                            call_arg_tys.push(typed_arg.ty);
                        }

                        // Manually construct the 'self' argument representation
                        let receiver_ty = checker.current_self_type.clone().ok_or_else(|| {
                            TypeError::InternalError { message: "Self type not set in default body context".into(), span: Some(span)}
                        })?;
                        let self_symbol = checker.env.get("self").map(|(_, sym)| sym).ok_or_else(|| {
                             TypeError::InternalError { message: "Self symbol not bound in default body context".into(), span: Some(span)}
                        })?;
                        let self_arg_typed = TypedArgument {
                           name: Some("self".into()), // Indicate it's the self param
                           value: TypedExpr {
                               kind: TypedExprKind::Variable { symbol: self_symbol, name: "self".into() },
                               ty: receiver_ty.clone(),
                               span: span // Use call span as placeholder
                           },
                           span: span // Use call span
                        };

                        // Construct final arguments including self PREPENDED
                        let mut final_args = vec![self_arg_typed]; // <<< Start with self >>>
                        final_args.extend(typed_call_args); // <<< Add the actual call args >>>

                        let mut final_arg_tys = vec![receiver_ty.clone()]; // <<< Start with self type >>>
                        final_arg_tys.extend(call_arg_tys); // <<< Add actual call arg types >>>


                        // Find the method name from the original signature
                        let method_sig = checker.type_ctx.get_any_function_sig(&checker.trait_repo, &fn_sym, span)?;
                        let method_name = method_sig.name.clone();

                        // <<< Call check_resolved_invocation directly >>>
                        let (kind, ty) = super::invocation::check_resolved_invocation(
                            checker,             // 1
                            *fn_sym,             // 2: signature_symbol (the trait method symbol)
                            *fn_sym,             // 3: initial_actual_symbol (will be resolved inside)
                            &method_name,        // 4: func_name
                            Some(&receiver_ty),  // 5: receiver_ty_opt
                            Some(current_impl_id), // 6: impl_id_opt (we are in default body context)
                            final_args,          // 7: args (includes self)
                            final_arg_tys,       // 8: arg_tys (includes self)
                            span,                // 9: span
                        )?;
                        return Ok(TypedExpr { kind, ty, span });
                    }
                }
            }
            // <<< END Default Body Redirect >>>

            // <<< Original direct call logic >>>
            let func_sig = {
                 checker.type_ctx.get_any_function_sig(checker.trait_repo, func_symbol.as_ref().ok_or_else(|| TypeError::InternalError{ message: "Call expects func_symbol".to_string(), span: Some(span) })?, span)
            }; 

            // Process func_sig *after* the borrow
            let (checked_func_expr, _) = if let Ok(sig) = func_sig {
                 let sig = sig.clone(); // Clone the Ok signature
                 let func_ty = Ty::with_span(
                     TyKind::Function(
                         sig.params.iter().map(|p| p.ty.clone()).collect(),
                         Arc::new(sig.return_type.clone()),
                     ),
                     sig.span, // Use signature span for the type
                 );
                 let func_name = sig.name.clone(); 
                 (TypedExpr { 
                     kind: TypedExprKind::Variable { symbol: func_symbol.unwrap(), name: func_name },
                     ty: func_ty,
                     span: sig.span,
                 }, sig.span)
            } else {
                // Complex function expressions (like lambdas) should be handled by other
                // ResolvedExprKind variants before reaching here.
                checker.report_error(TypeError::InternalError { message: "ResolvedExprKind::Call encountered with no func_symbol".to_string(), span: Some(span)});
                // <<< Manually construct error TypedExpr >>>
                return Ok(TypedExpr {
                    kind: TypedExprKind::Error,
                    ty: Ty::new(TyKind::Error),
                    span,
                }); 
            };
            
            // Call check_invocation with the constructed func_expr and checked args
            let mut typed_args = Vec::new();
            let mut arg_tys = Vec::new();
            for arg in args {
                let checked_arg = check_expr(checker, &arg.value, None)?;
                arg_tys.push(checked_arg.ty.clone());
                typed_args.push(TypedArgument {
                    name: arg.name.clone(),
                    value: checked_arg,
                    span: arg.span,
                });
            }
            let (kind, ty) = super::invocation::check_invocation(checker, &checked_func_expr, typed_args, arg_tys, span)?;
            Ok(TypedExpr { kind, ty, span })
        }
        ResolvedExprKind::MethodCall { object, method_name, args, .. } => {
            let checked_object = check_expr(checker, object, None)?;
            let mut typed_args = Vec::new();
            let mut arg_tys = Vec::new();
            for arg in args {
                let checked_arg = check_expr(checker, &arg.value, None)?;
                arg_tys.push(checked_arg.ty.clone());
                typed_args.push(TypedArgument {
                    name: arg.name.clone(),
                    value: checked_arg,
                    span: arg.span,
                });
            }
            let (kind, ty) = super::invocation::check_method_call(checker, &checked_object, method_name, typed_args, arg_tys, span)?;
            Ok(TypedExpr { kind, ty, span })
        }
        ResolvedExprKind::Field { object, field_name } => {
            let (kind, ty) = check_field_access(checker, object, field_name, span)?;
            Ok(TypedExpr { kind, ty, span })
        }
        ResolvedExprKind::Array(elements) => {
            let (kind, ty) = check_array_literal(checker, elements, expected_ty, span)?;
            Ok(TypedExpr { kind, ty, span })
        }
        ResolvedExprKind::Tuple(elements) => {
            let (kind, ty) = check_tuple_literal(checker, elements, expected_ty, span)?;
            Ok(TypedExpr { kind, ty, span })
        }
        ResolvedExprKind::Struct { struct_symbol, fields, base } => {
            let (kind, ty) = check_struct_literal(checker, *struct_symbol, fields, base.as_deref(), span)?;
            Ok(TypedExpr { kind, ty, span })
        }
        ResolvedExprKind::Lambda { params, body } => {
            let (kind, ty) = check_lambda(checker, params, body, expected_ty, span)?;
            Ok(TypedExpr { kind, ty, span })
        }
        ResolvedExprKind::Let { pattern, value, type_annotation } => {
            let (kind, ty) = check_let_expr(checker, pattern, value, type_annotation.as_ref(), span)?;
            Ok(TypedExpr { kind, ty, span })
        }
        ResolvedExprKind::Paren(inner_expr) => {
            let (kind, ty) = check_paren_expr(checker, inner_expr, expected_ty)?;
            Ok(TypedExpr { kind, ty, span })
        }
        ResolvedExprKind::VariantConstructor { variant_symbol, args } => {
            let (kind, ty) = check_variant_constructor(checker, *variant_symbol, args, span)?;
            Ok(TypedExpr { kind, ty, span })
        }
        ResolvedExprKind::Map(_) => {
            checker.report_error(TypeError::UnsupportedExpression { span });
            Ok(TypedExpr { kind: TypedExprKind::Error, ty: Ty::new(TyKind::Error), span })
        }
        ResolvedExprKind::HashSet(_) => {
            checker.report_error(TypeError::UnsupportedExpression { span });
            Ok(TypedExpr { kind: TypedExprKind::Error, ty: Ty::new(TyKind::Error), span })
        }
        _ => {
            checker.report_error(TypeError::UnsupportedExpression { span });
            Ok(TypedExpr { kind: TypedExprKind::Error, ty: Ty::new(TyKind::Error), span })
        }
    }
}

/// Check a literal expression (uses ast::common::Literal).
fn check_literal(checker: &mut TypeChecker, lit: &AstLiteral, span: SourceSpan) -> TypeResult<(TypedExprKind, Ty)> {
    match lit {
        AstLiteral::Int { value, suffix } => {
            let concrete_ty_kind: Option<PrimitiveType> = match suffix.as_deref() {
                Some("i8") => Some(PrimitiveType::I8),
                Some("i16") => Some(PrimitiveType::I16),
                Some("i32") => Some(PrimitiveType::I32),
                Some("i64") => Some(PrimitiveType::I64),
                Some("i128") => Some(PrimitiveType::I128),
                Some("u8") => Some(PrimitiveType::U8),
                Some("u16") => Some(PrimitiveType::U16),
                Some("u32") => Some(PrimitiveType::U32),
                Some("u64") => Some(PrimitiveType::U64),
                Some("u128") => Some(PrimitiveType::U128),
                Some(other) => {
                    checker.report_error(TypeError::UnsupportedLiteral { span });
                    None
                },
                None => None, // No suffix
            };

            let kind = TypedExprKind::IntLiteral { value: *value, suffix: suffix.clone() };
            let final_ty = if let Some(prim) = concrete_ty_kind {
                 Ty::with_span(TyKind::Primitive(prim), span)
            } else {
                // No suffix: create a fresh inference variable marked for integer defaulting
                let fresh_var = checker.fresh_var_with_defaulting(Some(crate::context::inference::DefaultKind::Integer));
                // Extract the TypeId from the TyKind::Var
                let var_id = match fresh_var.kind {
                    TyKind::Var(id) => id,
                    _ => panic!("fresh_var did not return TyKind::Var"),
                };
                // Directly create InferInt with the variable ID
                let infer_int_ty = Ty::with_span(TyKind::InferInt(var_id), span);
                // The final type IS the InferInt type itself
                infer_int_ty // <<< CHANGED
            };
            Ok((kind, final_ty))
        }
        AstLiteral::Float { value, suffix } => {
            let concrete_ty_kind: Option<PrimitiveType> = match suffix.as_deref() {
                Some("f32") => Some(PrimitiveType::F32),
                Some("f64") => Some(PrimitiveType::F64),
                Some(other) => {
                     checker.report_error(TypeError::UnsupportedLiteral { span });
                     None
                },
                None => None,
            };

            let kind = TypedExprKind::FloatLiteral { value: *value, suffix: suffix.clone() };
             let final_ty = if let Some(prim) = concrete_ty_kind {
                Ty::with_span(TyKind::Primitive(prim), span)
            } else {
                // No suffix: create a fresh inference variable marked for float defaulting
                let fresh_var = checker.fresh_var_with_defaulting(Some(crate::context::inference::DefaultKind::Float));
                // Extract the TypeId from the TyKind::Var
                let var_id = match fresh_var.kind {
                    TyKind::Var(id) => id,
                    _ => panic!("fresh_var did not return TyKind::Var"),
                };
                // Directly create InferFloat with the variable ID
                let infer_float_ty = Ty::with_span(TyKind::InferFloat(var_id), span);
                // The final type IS the InferFloat type itself
                infer_float_ty // <<< CHANGED
            };
            Ok((kind, final_ty))
        }
        AstLiteral::String(val) => Ok((TypedExprKind::StringLiteral(val.clone()), Ty::with_span(TyKind::Primitive(PrimitiveType::String), span))),
        AstLiteral::Char(val) => Ok((TypedExprKind::CharLiteral(*val), Ty::with_span(TyKind::Primitive(PrimitiveType::Char), span))),
        AstLiteral::Bool(val) => Ok((TypedExprKind::BoolLiteral(*val), Ty::with_span(TyKind::Primitive(PrimitiveType::Bool), span))),
    }
}

/// Check a variable reference from ResolvedExprKind::Variable.
fn check_variable_ref(checker: &mut TypeChecker, binding_symbol: Symbol, name: &str, span: SourceSpan) -> TypeResult<(TypedExprKind, Ty)> {
    let name_clone = name.to_string();

    // Special case for 'self' keyword
    if name_clone == "self" {
        return if let Some(self_ty) = &checker.current_self_type {
            Ok((TypedExprKind::Variable { symbol: binding_symbol, name: name_clone }, self_ty.clone()))
        } else {
            Err(TypeError::SelfOutsideImplOrTrait { span })
        };
    }

    match checker.env.get(&name_clone) {
        Some((ty, symbol)) => {
            if symbol != binding_symbol {
                println!("Warning: Symbol mismatch for variable '{}': Resolver {:?}, Env {:?}", name_clone, binding_symbol, symbol);
            }
            println!(
                "[check_variable_ref] Found var '{}' ({:?}) with type: {}",
                name_clone,
                symbol,
                display_type(&ty)
            );
            Ok((TypedExprKind::Variable { symbol, name: name_clone }, ty))
        }
        None => {
            Err(TypeError::UndefinedVariable { name: name_clone, span })
        }
    }
}

/// Check a path reference from ResolvedExprKind::Path.
/// This handles references to functions, enum variants, structs (potentially), etc.
fn check_path(checker: &mut TypeChecker, symbol: Symbol, span: SourceSpan) -> TypeResult<(TypedExprKind, Ty)> {
    // Try getting function signature first (checks TypeContext and TraitRepo)
    // <<< TIGHT SCOPE FOR BORROW - REMOVE borrow() >>>
    let sig_result = {
        // <<< REMOVE borrow() >>>
        checker.type_ctx.get_any_function_sig(checker.trait_repo, &symbol, span)
    }; // Borrow drops here

    // Process sig_result AFTER borrow is dropped
    if let Ok(sig) = sig_result {
        let sig = sig.clone(); // Clone the Ok signature
        let func_ty = Ty::with_span(
             TyKind::Function(
                 sig.params.iter().map(|p| p.ty.clone()).collect(),
                 Arc::new(sig.return_type.clone()), 
             ),
             sig.span
         );
        return Ok((TypedExprKind::Variable { symbol, name: sig.name.clone() }, func_ty));
    }

    // Not a function/method, try other definitions in TypeContext
    match checker.type_ctx.get_definition(&symbol) {
        Some(TypeDef::Struct(sd)) => {
            let sd_clone = sd.clone();
            if sd_clone.fields.is_empty() { // Allow path for unit structs
                let args: Vec<_> = (0..sd_clone.generic_params.len()).map(|_| checker.fresh_var()).collect();
                let struct_ty = Ty::with_span(TyKind::Named { name: sd_clone.name.clone(), symbol: Some(symbol), args }, span);
                Ok((TypedExprKind::Variable { symbol, name: sd_clone.name.clone() }, struct_ty))
            } else {
                Err(TypeError::NotAValue { name: sd_clone.name.clone(), span })
            }
        }
        Some(TypeDef::Enum(ed)) => {
            Err(TypeError::NotAValue { name: ed.name.clone(), span })
        }
        None => {
            // Not in TypeContext either, try enum variants (redundant if get_any_func_sig worked? No, variant constructors)
             let mut found_variant_info: Option<(String, String, Symbol, Vec<GenericParamDef>, Vec<Field>)> = None;
              for enum_def in checker.type_ctx.iter_enums() {
                  if let Some(variant) = enum_def.variants.iter().find(|v| v.symbol == symbol) {
                       found_variant_info = Some((
                           enum_def.name.clone(),
                           variant.name.clone(),
                           enum_def.symbol,
                           enum_def.generic_params.clone(),
                           variant.fields.clone(),
                       ));
                       break;
                  }
              }

              if let Some((enum_name, variant_name, enum_symbol, generic_params, fields)) = found_variant_info {
                 let enum_args: Vec<_> = (0..generic_params.len()).map(|_| checker.fresh_var()).collect();
                 let enum_ty = Ty::with_span(TyKind::Named { name: enum_name.clone(), symbol: Some(enum_symbol), args: enum_args.clone() }, span);

                 if fields.is_empty() {
                     // Unit variant
                     Ok((TypedExprKind::Variable { symbol, name: variant_name }, enum_ty))
                 } else {
                     // Non-unit variant constructor function
                     let mut subst = Substitution::new();
                     for (param, arg) in generic_params.iter().zip(&enum_args) {
                        subst.insert(param.id, arg.clone());
                     }
                     let constructor_param_tys: Vec<Ty> = fields.iter()
                         .map(|f| f.ty.apply_subst(&subst))
                         .collect();
                     let constructor_ty = Ty::with_span(
                         TyKind::Function(constructor_param_tys, Arc::new(enum_ty)),
                         span
                     );
                     Ok((TypedExprKind::Variable { symbol, name: variant_name }, constructor_ty))
                 }
              } else {
                  // Truly unknown identifier
                  Err(TypeError::UnknownIdentifier { name: format!("symbol_{}", symbol.0), span })
              }
        }
        // Function case handled by get_any_function_sig above
        Some(TypeDef::Function(_)) => unreachable!("Function should have been handled by get_any_function_sig"),
    }
}

/// Check a binary operation (accepts ResolvedExpr).
fn check_bin_op(checker: &mut TypeChecker, lhs_expr: &ResolvedExpr, op: &BinaryOp, rhs_expr: &ResolvedExpr, span: SourceSpan) -> TypeResult<(TypedExprKind, Ty)> {
    let lhs = check_expr(checker, lhs_expr, None)?;
    let rhs = check_expr(checker, rhs_expr, None)?;

    super::operators::check_binary_op(checker, op, &lhs, &rhs, span)
}

/// Check a unary operation (accepts ResolvedExpr).
fn check_unary_op(checker: &mut TypeChecker, op: &UnaryOp, operand_expr: &ResolvedExpr, span: SourceSpan) -> TypeResult<(TypedExprKind, Ty)> {
    let operand = check_expr(checker, operand_expr, None)?;
    super::operators::check_unary_op(checker, op, &operand, span)
}

/// Check a block expression.
pub(crate) fn check_block(checker: &mut TypeChecker, block_exprs: &[ResolvedExpr], expected_ty: Option<&Ty>, _span: SourceSpan) -> TypeResult<(TypedExprKind, Ty)> {
    checker.enter_scope();
    let mut typed_stmts = Vec::new();
    let mut block_ty = Ty::new(TyKind::Primitive(PrimitiveType::Unit)); // Default to Unit

    let num_stmts = block_exprs.len();

    for (i, expr) in block_exprs.iter().enumerate() {
        let is_last = i == num_stmts - 1;

        // Special handling for Let inside a block
        if let ResolvedExprKind::Let { pattern, value, type_annotation } = &expr.kind {
            // Check the let binding itself (evaluates to Unit)
             let (typed_let_kind, _) = check_let_expr(checker, pattern, value, type_annotation.as_ref(), expr.span)?;
             typed_stmts.push(TypedExpr { kind: typed_let_kind, ty: Ty::new(TyKind::Primitive(PrimitiveType::Unit)), span: expr.span });
             // If let is the last statement, block type is Unit
             if is_last {
                 block_ty = Ty::new(TyKind::Primitive(PrimitiveType::Unit));
             }
        } else {
            // Handle regular expressions
            let current_expected = if is_last { expected_ty } else { None };
            let typed_expr = check_expr(checker, expr, current_expected)?;

            if is_last {
                block_ty = typed_expr.ty.clone();
                typed_stmts.push(typed_expr); // Add the final expression result
            } else {
                // Non-tail expressions must have Unit type or be Never
                let unit_ty = Ty::new(TyKind::Primitive(PrimitiveType::Unit));
                let never_ty = Ty::new(TyKind::Never);
                if !checker.unify(&typed_expr.ty, &unit_ty, expr.span) && !checker.unify(&typed_expr.ty, &never_ty, expr.span) {
                    checker.report_error(TypeError::TypeMismatch {
                        expected: "() or ! (statement effect)".to_string(),
                        found: display_type(&typed_expr.ty),
                        span: expr.span,
                    });
                }
                typed_stmts.push(typed_expr);
            }
        }
    }

    checker.exit_scope();
    Ok((TypedExprKind::Block(typed_stmts), block_ty))
}

/// Check an if expression.
fn check_if_expr(checker: &mut TypeChecker, condition: &ResolvedExpr, then_branch_expr: &ResolvedExpr, else_branch_expr: Option<&ResolvedExpr>, expected_ty: Option<&Ty>, _span: SourceSpan) -> TypeResult<(TypedExprKind, Ty)> {
    let bool_ty = Ty::new(TyKind::Primitive(PrimitiveType::Bool));
    let checked_condition = check_expr(checker, condition, Some(&bool_ty))?;
    // No need to unify again if check_expr was called with Some(&bool_ty)
    // if !checker.unify(&checked_condition.ty, &bool_ty, condition.span) {
    //     // Error would have been reported by check_expr or unify
    // }

    let checked_then_branch = check_expr(checker, then_branch_expr, expected_ty)?;

    let (checked_else_branch, final_ty) = match else_branch_expr {
        Some(else_expr) => {
            let checked_else = check_expr(checker, else_expr, Some(&checked_then_branch.ty))?;
            if !checker.unify(&checked_then_branch.ty, &checked_else.ty, else_expr.span) {
            }
            let unified_ty = checker.infctx.apply_substitution(&checked_then_branch.ty);
            (Some(Box::new(checked_else)), unified_ty)
        }
        None => {
            let unit_ty = Ty::new(TyKind::Primitive(PrimitiveType::Unit));
            if !checker.unify(&checked_then_branch.ty, &unit_ty, then_branch_expr.span) {
            }
            (None, unit_ty)
        }
    };

    Ok((TypedExprKind::If {
        condition: Box::new(checked_condition),
        then_branch: Box::new(checked_then_branch),
        else_branch: checked_else_branch
    }, final_ty))
}

/// Check a match expression.
fn check_match_expr(checker: &mut TypeChecker, scrutinee_expr: &ResolvedExpr, arms: &[(ResolvedPattern, ResolvedExpr)], expected_ty: Option<&Ty>, span: SourceSpan) -> TypeResult<(TypedExprKind, Ty)> {
    let scrutinee = check_expr(checker, scrutinee_expr, None)?;
    let mut typed_arms = Vec::new();
    let mut match_ty: Option<Ty> = None;

    if arms.is_empty() {
        checker.report_error(TypeError::NonExhaustivePatterns { span });
        return Ok((TypedExprKind::Match { scrutinee: Box::new(scrutinee), arms: vec![] }, Ty::new(TyKind::Never)));
    }

    for (pattern_ast, body_expr) in arms {
        checker.enter_scope();
        let typed_pattern = super::pattern::check_pattern(checker, pattern_ast, &scrutinee.ty)?;
        let expected_arm_ty = match_ty.as_ref().or(expected_ty);
        let typed_body = check_expr(checker, body_expr, expected_arm_ty)?;

        if let Some(ref mt) = match_ty {
            if !checker.unify(&typed_body.ty, mt, body_expr.span) {
                // Error reported
            }
        } else {
            match_ty = Some(typed_body.ty.clone());
        }

        typed_arms.push(TypedMatchArm { pattern: typed_pattern, body: typed_body });
        checker.exit_scope();
    }

    let final_match_ty = match_ty.unwrap_or_else(|| {
         checker.report_error(TypeError::InternalError { message: "Match type undetermined".to_string(), span: Some(span) });
         Ty::new(TyKind::Error)
     });

    Ok((TypedExprKind::Match { scrutinee: Box::new(scrutinee), arms: typed_arms }, final_match_ty))
}

/// Check a field access expression.
fn check_field_access(checker: &mut TypeChecker, object_expr: &ResolvedExpr, field_name: &str, span: SourceSpan) -> TypeResult<(TypedExprKind, Ty)> {
    let object = check_expr(checker, object_expr, None)?;
    let object_ty = checker.infctx.apply_substitution(&object.ty);

    match object_ty.kind {
        TyKind::Named { symbol: Some(struct_symbol), args: ref type_args, .. } => {
            // Look up the struct definition
            match checker.type_ctx.get_struct_def(&struct_symbol, object.span) {
                Ok(struct_def) => {
                    // Find the field by name
                    if let Some(field_def) = struct_def.fields.iter().find(|f| f.name == field_name) {
                        // Instantiate the field type with the struct's type arguments
                        let field_ty_instance = if !struct_def.generic_params.is_empty() {
                            assert_eq!(struct_def.generic_params.len(), type_args.len(), "Generic argument count mismatch during field access");
                            let mut subst = Substitution::new();
                            for (param, arg) in struct_def.generic_params.iter().zip(type_args) {
                                subst.insert(param.id, arg.clone());
                            }
                            field_def.ty.apply_subst(&subst)
                        } else {
                            field_def.ty.clone()
                        };
                        Ok((TypedExprKind::Field {
                            object: Box::new(object),
                            field_symbol: field_def.symbol,
                            field_name: field_name.to_string(),
                        }, field_ty_instance))
                    } else {
                        Err(TypeError::UnknownStructField {
                            field: field_name.to_string(),
                            struct_name: struct_def.name.clone(),
                            span,
                        })
                    }
                }
                Err(_) => {
                     // Object type resolves to a symbol, but it's not a known struct
                     Err(TypeError::NotAStruct { found: display_type(&object_ty), span: object.span })
                }
            }
        }
        TyKind::Tuple(ref element_tys) => {
            // Handle tuple field access (.0, .1, etc.)
            if let Ok(index) = field_name.parse::<usize>() {
                if index < element_tys.len() {
                    // Use the new TupleField kind
                    let kind = TypedExprKind::TupleField {
                        tuple: Box::new(object),
                        index,
                    };
                    Ok((kind, element_tys[index].clone()))
                } else {
                    Err(TypeError::UnknownField { field: field_name.to_string(), ty: display_type(&object_ty), span })
                }
            } else {
                Err(TypeError::UnknownField { field: field_name.to_string(), ty: display_type(&object_ty), span })
            }
        }
        // TODO: Handle field access on other types (e.g., module access requires module system integration).
        _ => Err(TypeError::UnknownField { field: field_name.to_string(), ty: display_type(&object_ty), span }),
    }
}

/// Check an array literal.
fn check_array_literal(checker: &mut TypeChecker, elements: &[ResolvedExpr], expected_ty: Option<&Ty>, span: SourceSpan) -> TypeResult<(TypedExprKind, Ty)> {
    let mut typed_elements = Vec::new();
    let mut element_ty: Option<Ty> = None;

    // Infer element type from expected type if possible
    if let Some(expected) = expected_ty {
        if let TyKind::Array(ref expected_elem_ty, _size) = expected.kind {
            element_ty = Some((**expected_elem_ty).clone());
        }
    }

    for element_expr in elements {
        let typed_element = check_expr(checker, element_expr, element_ty.as_ref())?;
        if let Some(ref et) = element_ty {
            // Unify with the established element type
            if !checker.unify(&typed_element.ty, et, element_expr.span) {
                 // Error reported
            }
        } else {
            // First element establishes the type
            element_ty = Some(typed_element.ty.clone());
        }
        typed_elements.push(typed_element);
    }

    let final_element_ty = element_ty.unwrap_or_else(|| {
        // Array with no elements - type cannot be inferred without context
        checker.report_error(TypeError::TypeMismatch { expected: "array type".to_string(), found: "<unknown array element type>".to_string(), span });
        Ty::new(TyKind::Error)
    });

    let array_ty = Ty::with_span(TyKind::Array(Arc::new(final_element_ty), Some(typed_elements.len())), span);
    Ok((TypedExprKind::Array(typed_elements), array_ty))
}

/// Check a tuple literal.
fn check_tuple_literal(checker: &mut TypeChecker, elements: &[ResolvedExpr], expected_ty: Option<&Ty>, span: SourceSpan) -> TypeResult<(TypedExprKind, Ty)> {
    let mut typed_elements = Vec::new();
    let mut element_tys = Vec::new();

    // Get expected element types if available
    let expected_element_tys: Option<&[Ty]> = expected_ty.and_then(|t| {
        if let TyKind::Tuple(ref expected_elems) = t.kind {
            Some(expected_elems.as_slice())
        } else {
            None
        }
    });

    for (i, element_expr) in elements.iter().enumerate() {
        let expected_elem_ty = expected_element_tys.and_then(|e| e.get(i));
        let typed_element = check_expr(checker, element_expr, expected_elem_ty)?;
        element_tys.push(typed_element.ty.clone());
        typed_elements.push(typed_element);
    }

    let tuple_ty = Ty::with_span(TyKind::Tuple(element_tys), span);
    Ok((TypedExprKind::Tuple(typed_elements), tuple_ty))
}

/// Check a struct literal.
fn check_struct_literal(checker: &mut TypeChecker, struct_symbol: Symbol, fields: &[(String, ResolvedExpr)], base: Option<&ResolvedExpr>, span: SourceSpan) -> TypeResult<(TypedExprKind, Ty)> {
    let struct_def = checker.type_ctx.get_struct_def(&struct_symbol, span)?.clone();
    let mut instantiation_subst = Substitution::new();
    let struct_type_args: Vec<Ty> = struct_def.generic_params.iter().map(|gp| {
        let fresh_var = checker.fresh_var();
        instantiation_subst.insert(gp.id, fresh_var.clone());
        fresh_var
    }).collect();

    let struct_ty = Ty::with_span(
        TyKind::Named {
            name: struct_def.name.clone(),
            symbol: Some(struct_symbol),
            args: struct_type_args,
        },
        span
    );

    let mut typed_fields = Vec::new();
    let mut provided_field_names = HashSet::new();
    for (field_name, field_expr) in fields {
         provided_field_names.insert(field_name.clone());
         if let Some(field_def) = struct_def.fields.iter().find(|f| f.name == *field_name) {
             let expected_field_ty = field_def.ty.apply_subst(&instantiation_subst);
             let typed_field_expr = check_expr(checker, field_expr, Some(&expected_field_ty))?;
              if !checker.unify(&typed_field_expr.ty, &expected_field_ty, field_expr.span) {
              }
              typed_fields.push((field_name.clone(), typed_field_expr));
         } else {
             return Err(TypeError::UnknownStructField {
                 field: field_name.clone(),
                 struct_name: struct_def.name.clone(),
                 span: field_expr.span,
             });
         }
    }

    for field_def in &struct_def.fields {
         if !provided_field_names.contains(&field_def.name) {
             return Err(TypeError::MissingField {
                 field: field_def.name.clone(),
                 struct_name: struct_def.name.clone(),
                 span,
             });
         }
    }

    let typed_base = if let Some(base_expr) = base {
         println!("Warning: Struct update syntax (base expression) not yet fully type-checked.");
         let checked_base = check_expr(checker, base_expr, Some(&struct_ty))?;
         if !checker.unify(&checked_base.ty, &struct_ty, base_expr.span) { }
         Some(Box::new(checked_base))
    } else {
        None
    };

    let hir_kind = TypedExprKind::Struct {
         name: struct_def.name,
         struct_symbol,
         fields: typed_fields,
         base: typed_base,
    };
    let final_struct_ty = checker.infctx.apply_substitution(&struct_ty);
    Ok((hir_kind, final_struct_ty))
}

/// Check a lambda expression.
fn check_lambda(checker: &mut TypeChecker, params: &[ResolvedParameter], body: &ResolvedExpr, expected_ty: Option<&Ty>, span: SourceSpan) -> TypeResult<(TypedExprKind, Ty)> {
    println!("Warning: Lambda checking is basic - captures and complex inference not implemented.");
    let mut param_tys = Vec::new();
    let mut param_bindings = Vec::new();
    let mut typed_hir_params = Vec::new(); // For HIR

    for param in params {
        let param_ty = match super::resolve::resolve_type_to_ty(checker, &param.param_type) { 
             Ok(ty) => ty,
             Err(e) => { checker.report_error(e); Ty::new(TyKind::Error) } 
         };
         param_tys.push(param_ty.clone());
         param_bindings.push((param.name.clone(), param.symbol, param_ty.clone())); // Clone ty for bindings
         // Create HIR parameter
         typed_hir_params.push(TypedParameter {
            name: param.name.clone(),
            symbol: param.symbol,
            ty: param_ty,
            is_variadic: param.is_variadic,
            has_default: param.has_default,
            span: param.span,
        });
    }

    // Create a new scope for the lambda body
    let parent_env = Arc::clone(&checker.env); 
    let mut lambda_env = TypeEnvironment::with_parent(parent_env);
    
    for (name, symbol, ty) in param_bindings {
        lambda_env.add(name, symbol, ty); // Mutate the new env
    }
    
    // Temporarily set the checker's env to the lambda's env
    let original_env = std::mem::replace(&mut checker.env, Arc::new(lambda_env));
    
    let typed_body = check_expr(checker, body, None)?; // Check body with the lambda env
    
    let inferred_return_ty = checker.infctx.apply_substitution(&typed_body.ty);
    
    // Restore the original environment
    checker.env = original_env;
    
    let lambda_ty = Ty::with_span(
        TyKind::Function(param_tys, Arc::new(inferred_return_ty.clone())),
        span
    );
    if let Some(expected) = expected_ty {
         if !checker.unify(&lambda_ty, expected, span) {
         }
    }
    let hir_kind = TypedExprKind::Lambda {
         params: typed_hir_params, // Use the collected HIR params
         body: Box::new(typed_body),
    };
    let final_lambda_ty = checker.infctx.apply_substitution(&lambda_ty);
    Ok((hir_kind, final_lambda_ty))
}

/// Check a let expression (now only used internally by check_block).
fn check_let_expr(checker: &mut TypeChecker, pattern: &ResolvedPattern, value_expr: &ResolvedExpr, type_annotation: Option<&ResolvedType>, span: SourceSpan) -> TypeResult<(TypedExprKind, Ty)> {
     let expected_value_ty = if let Some(_annot) = type_annotation {
         // Use resolve::resolve_type_to_ty if available
         // Some(super::resolve::resolve_type_to_ty(checker, annot)?)
         // Temporary placeholder if resolve_type_to_ty isn't ready
         Some(checker.fresh_var())
     } else {
         None
     };

     let value = check_expr(checker, value_expr, expected_value_ty.as_ref())?;
     let value_ty = checker.infctx.apply_substitution(&value.ty);

     if let Some(ref annot_ty) = expected_value_ty {
         if !checker.unify(&value_ty, annot_ty, value_expr.span) {
         }
     }

     let typed_pattern = super::pattern::check_pattern(checker, pattern, &value_ty)?;

     let unit_ty = Ty::with_span(TyKind::Primitive(PrimitiveType::Unit), span);
     Ok((TypedExprKind::Let { pattern: typed_pattern, value: Box::new(value) }, unit_ty))
}

/// Check a parenthesized expression.
fn check_paren_expr(checker: &mut TypeChecker, paren_expr: &ResolvedExpr, expected_ty: Option<&Ty>) -> TypeResult<(TypedExprKind, Ty)> {
     let inner_typed = check_expr(checker, paren_expr, expected_ty)?;
     Ok((TypedExprKind::Paren(Box::new(inner_typed.clone())), inner_typed.ty))
}

/// Check a variant constructor.
fn check_variant_constructor(checker: &mut TypeChecker, variant_symbol: Symbol, args: &[ResolvedArgument], span: SourceSpan) -> TypeResult<(TypedExprKind, Ty)> {
    println!("Warning: Variant constructor checking is basic - generics not fully handled.");
    let mut found_enum: Option<EnumDef> = None; // Store owned EnumDef
    let mut found_variant: Option<EnumVariant> = None; // Store owned EnumVariant

    for enum_def in checker.type_ctx.iter_enums().cloned() { // Clone enums to avoid borrow issues
         if let Some(variant_def) = enum_def.variants.iter().find(|v| v.symbol == variant_symbol).cloned() { // Clone variant
             found_enum = Some(enum_def);
             found_variant = Some(variant_def);
             break;
         }
    }

    let (enum_def, variant_def) = match (found_enum, found_variant) {
        (Some(e), Some(v)) => (e, v),
        _ => {
             return Err(TypeError::InternalError {
                 message: format!("Enum variant definition not found for symbol {:?}", variant_symbol),
                 span: Some(span),
             });
        }
    };

    let mut instantiation_subst = Substitution::new();
    let enum_type_args: Vec<Ty> = enum_def.generic_params.iter().map(|gp| {
        let fresh_var = checker.fresh_var();
        instantiation_subst.insert(gp.id, fresh_var.clone());
        fresh_var
    }).collect();

    let enum_symbol = enum_def.symbol;

    let enum_ty = Ty::with_span(
        TyKind::Named {
            name: enum_def.name.clone(),
            symbol: Some(enum_symbol),
            args: enum_type_args,
        },
        span
    );

    let expected_arg_tys: Vec<Ty> = variant_def.fields.iter()
        .map(|field| field.ty.apply_subst(&instantiation_subst))
        .collect();

    if expected_arg_tys.len() != args.len() {
         return Err(TypeError::WrongNumberOfArguments {
             expected: expected_arg_tys.len(),
             found: args.len(),
             span,
         });
    }

    let mut typed_args = Vec::new();
    for (arg, expected_ty) in args.iter().zip(expected_arg_tys.iter()) {
         let typed_arg_expr = check_expr(checker, &arg.value, Some(expected_ty))?;
         if !checker.unify(&typed_arg_expr.ty, expected_ty, arg.span) {} 
         typed_args.push(TypedArgument {
             name: arg.name.clone(),
             value: typed_arg_expr,
             span: arg.span,
         });
    }

    let hir_kind = TypedExprKind::VariantConstructor {
         enum_name: enum_def.name.clone(),
         variant_name: variant_def.name.clone(),
         enum_symbol,
         variant_symbol,
         args: typed_args,
    };

    let final_enum_ty = checker.infctx.apply_substitution(&enum_ty);
    Ok((hir_kind, final_enum_ty))
} 