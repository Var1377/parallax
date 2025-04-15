use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use miette::SourceSpan;
use parallax_resolve::types::{ResolvedExpr, ResolvedArgument, Symbol};
use crate::{
    error::{TypeError, TypeResult},
    inference::Substitution,
    typecheck::{
        expressions::type_check_expression,
        helpers::{instantiate, resolve_variant_symbol_to_names},
        TypeChecker,
    },
    types::{Ty, TyKind, TypeDef, TypedArgument, TypedExprKind},
};

pub(crate) fn check_array_literal(
    checker: &mut TypeChecker,
    elements: &[ResolvedExpr],
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    if elements.is_empty() {
        let elem_var = checker.fresh_infer_var(span);
        let array_ty = Ty::with_span(TyKind::Array(Arc::new(elem_var), 0), span);
        return Ok((TypedExprKind::Array(vec![]), array_ty));
    }

    let mut typed_elements = Vec::with_capacity(elements.len());
    let mut element_ty: Option<Ty> = None;

    for element in elements {
        let typed_element = type_check_expression(checker, element, element_ty.as_ref())?;

        if element_ty.is_none() {
            element_ty = Some(typed_element.ty.clone());
        }

        typed_elements.push(typed_element);
    }

    let final_element_ty = checker.resolve_type(&element_ty.unwrap());
    let array_len = elements.len();
    let array_ty = Ty::with_span(TyKind::Array(Arc::new(final_element_ty), array_len), span);

    Ok((TypedExprKind::Array(typed_elements), array_ty))
}

pub(crate) fn check_tuple_literal(
    checker: &mut TypeChecker,
    elements: &[ResolvedExpr],
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    let mut typed_elements = Vec::with_capacity(elements.len());
    let mut element_tys = Vec::with_capacity(elements.len());

    for element in elements {
        let typed_element = type_check_expression(checker, element, None)?;
        element_tys.push(typed_element.ty.clone());
        typed_elements.push(typed_element);
    }

    let tuple_ty = Ty::with_span(TyKind::Tuple(element_tys), span);
    Ok((TypedExprKind::Tuple(typed_elements), tuple_ty))
}

pub(crate) fn check_struct_literal(
    checker: &mut TypeChecker,
    struct_symbol: Symbol,
    fields: &[(String, ResolvedExpr)],
    base: Option<&ResolvedExpr>,
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    if base.is_some() {
        return Err(TypeError::InternalError {
            message: "Struct update syntax '..base' not implemented yet".to_string(),
            span: Some(span),
        });
    }

    let struct_name = checker.get_name_for_symbol(struct_symbol)?;

    let struct_def = if let Some(TypeDef::Struct(def)) = checker.type_ctx.get_type(&struct_name).cloned() {
        def
    } else {
        return Err(TypeError::InternalError {
            message: format!("Could not find struct definition for '{}'", struct_name),
            span: Some(span),
        });
    };

    let base_struct_ty = Ty::with_span(TyKind::Named { name: struct_name.clone(), args: vec![] }, span);
    let (instantiated_struct_ty, _generic_map) = instantiate(checker, &base_struct_ty, span, &struct_def.generic_params)?;

    let instance_args = if let TyKind::Named { ref args, .. } = instantiated_struct_ty.kind {
        args.clone()
    } else {
        return Err(TypeError::InternalError {
            message: format!("Internal Error: Instantiated struct type '{}' is not Named", struct_name),
            span: Some(span),
        });
    };

    let mut typed_fields = Vec::with_capacity(fields.len());
    let mut provided_field_names = HashSet::new();

    for (field_name, field_value_expr) in fields {
        if !provided_field_names.insert(field_name.as_str()) {
            return Err(TypeError::InternalError {
                message: format!("Duplicate field '{}' in struct literal", field_name),
                span: Some(field_value_expr.span),
            });
        }

        if let Some(field_def) = struct_def.fields.iter().find(|f| f.name == *field_name) {
            let mut instance_subst = Substitution::new();
            if struct_def.generic_params.len() != instance_args.len() {
                return Err(TypeError::InternalError {
                    message: format!(
                        "Internal Error: Generic argument count mismatch during struct literal field check for '{}'. Expected {}, found {}",
                        struct_name,
                        struct_def.generic_params.len(),
                        instance_args.len()
                    ),
                    span: Some(field_value_expr.span),
                });
            }
            for (gen_param_def, instance_arg_ty) in struct_def.generic_params.iter().zip(instance_args.iter()) {
                instance_subst.insert(gen_param_def.id, instance_arg_ty.clone());
            }
            let expected_field_ty = field_def.ty.apply_subst(&instance_subst);
            let resolved_expected_field_ty = checker.resolve_type(&expected_field_ty);

            let typed_field_value = type_check_expression(checker, field_value_expr, Some(&resolved_expected_field_ty))?;
            typed_fields.push((field_name.clone(), typed_field_value));
        } else {
            return Err(TypeError::UnknownStructField {
                field: field_name.clone(),
                struct_name: struct_name.clone(),
                span: field_value_expr.span,
            });
        }
    }

    if base.is_none() {
        for field_def in &struct_def.fields {
            if !provided_field_names.contains(field_def.name.as_str()) {
                return Err(TypeError::MissingField {
                    field: field_def.name.clone(),
                    struct_name: struct_name.clone(),
                    span,
                });
            }
        }
    }

    let final_struct_ty = checker.resolve_type(&instantiated_struct_ty);

    Ok((
        TypedExprKind::Struct {
            name: struct_name,
            fields: typed_fields,
            base: None,
        },
        final_struct_ty,
    ))
}

pub(crate) fn check_variant_constructor(
    checker: &mut TypeChecker,
    variant_symbol: Symbol,
    args: &[ResolvedArgument],
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    let (enum_name, variant_name) = resolve_variant_symbol_to_names(checker, variant_symbol, span)?;

    let enum_def = if let Some(TypeDef::Enum(def)) = checker.type_ctx.get_type(&enum_name).cloned() {
        def
    } else {
        return Err(TypeError::InternalError {
            message: format!("Enum definition '{}' not found for variant constructor '{}'", enum_name, variant_name),
            span: Some(span),
        });
    };

    let variant_def = if let Some(v_def) = enum_def.variants.iter().find(|v| v.name == variant_name) {
        v_def
    } else {
        return Err(TypeError::InternalError {
            message: format!("Variant definition '{}' not found within enum '{}'", variant_name, enum_name),
            span: Some(span),
        });
    };

    let base_enum_ty = Ty::with_span(TyKind::Named { name: enum_name.clone(), args: vec![] }, span);
    let (instantiated_enum_ty_base, _generic_map) = instantiate(checker, &base_enum_ty, span, &enum_def.generic_params)?;

    let mut typed_args = Vec::with_capacity(args.len());
    match variant_def.fields.is_empty() {
        true => {
            if !args.is_empty() {
                return Err(TypeError::WrongNumberOfArguments { expected: 0, found: args.len(), span });
            }
        }
        false => {
            let struct_fields = &variant_def.fields;
            if args.iter().any(|arg| arg.name.is_none()) {
                return Err(TypeError::InternalError {
                    message: format!("Positional arguments are not supported for struct-like variant constructor '{}'", variant_name),
                    span: Some(args.iter().find(|arg| arg.name.is_none()).map(|arg| arg.span).unwrap_or(span)),
                });
            }

            if args.len() != struct_fields.len() {
                return Err(TypeError::WrongNumberOfArguments { expected: struct_fields.len(), found: args.len(), span });
            }

            let mut provided_args_map = HashMap::new();
            for arg in args {
                let arg_name = arg.name.as_ref().unwrap();
                if provided_args_map.contains_key(arg_name) {
                    return Err(TypeError::InternalError {
                        message: format!("Duplicate argument '{}' provided for variant constructor '{}'", arg_name, variant_name),
                        span: Some(arg.span),
                    });
                }
                provided_args_map.insert(arg_name.clone(), arg);
            }

            for expected_field in struct_fields {
                if let Some(arg) = provided_args_map.get(&expected_field.name) {
                    let mut instance_subst = Substitution::new();
                    let instance_args = if let TyKind::Named { ref args, .. } = instantiated_enum_ty_base.kind {
                        args
                    } else {
                        &vec![]
                    };

                    if enum_def.generic_params.len() != instance_args.len() {
                        return Err(TypeError::InternalError {
                            message: format!("Internal Error: Generic argument mismatch during variant arg check for '{}'", enum_name),
                            span: Some(span),
                        });
                    }
                    for (gen_param_def, instance_arg_ty) in enum_def.generic_params.iter().zip(instance_args.iter()) {
                        instance_subst.insert(gen_param_def.id, instance_arg_ty.clone());
                    }
                    let expected_ty = expected_field.ty.apply_subst(&instance_subst);
                    let resolved_expected_ty = checker.resolve_type(&expected_ty);

                    let typed_arg_value = type_check_expression(checker, &arg.value, Some(&resolved_expected_ty))?;
                    typed_args.push(TypedArgument {
                        name: Some(expected_field.name.clone()),
                        value: typed_arg_value,
                        span: arg.span,
                    });
                } else {
                    return Err(TypeError::MissingField {
                        field: expected_field.name.clone(),
                        struct_name: format!("{}::{} variant", enum_name, variant_name),
                        span,
                    });
                }
            }
        }
    }

    let final_enum_ty = checker.resolve_type(&instantiated_enum_ty_base);

    Ok((
        TypedExprKind::VariantConstructor {
            enum_name,
            variant_name,
            args: typed_args,
        },
        final_enum_ty,
    ))
}

pub(crate) fn check_map_literal(
    checker: &mut TypeChecker,
    entries: &[(ResolvedExpr, ResolvedExpr)],
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    let mut typed_entries = Vec::with_capacity(entries.len());
    let (key_ty, value_ty) = if entries.is_empty() {
        // If empty, create fresh type variables
        let key_var = checker.fresh_infer_var(span);
        let value_var = checker.fresh_infer_var(span);
        (key_var, value_var)
    } else {
        // Infer from the first entry
        let (first_key_expr, first_value_expr) = &entries[0];
        let typed_first_key = type_check_expression(checker, first_key_expr, None)?;
        let typed_first_value = type_check_expression(checker, first_value_expr, None)?;
        let inferred_key_ty = typed_first_key.ty.clone();
        let inferred_value_ty = typed_first_value.ty.clone();
        typed_entries.push((typed_first_key, typed_first_value));

        // Check remaining entries against inferred types
        for (key_expr, value_expr) in entries.iter().skip(1) {
            let typed_key = type_check_expression(checker, key_expr, Some(&inferred_key_ty))?;
            let typed_value = type_check_expression(checker, value_expr, Some(&inferred_value_ty))?;
            typed_entries.push((typed_key, typed_value));
        }
        (inferred_key_ty, inferred_value_ty)
    };

    // Resolve the final types
    let final_key_ty = checker.resolve_type(&key_ty);
    let final_value_ty = checker.resolve_type(&value_ty);

    // TODO: Add Hash + Eq trait bounds check for final_key_ty

    let map_ty = Ty::with_span(
        TyKind::Map(Arc::new(final_key_ty), Arc::new(final_value_ty)),
        span,
    );
    Ok((TypedExprKind::Map(typed_entries), map_ty))
}

pub(crate) fn check_hashset_literal(
    checker: &mut TypeChecker,
    elements: &[ResolvedExpr],
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    let mut typed_elements = Vec::with_capacity(elements.len());
    let element_ty = if elements.is_empty() {
        // If empty, create fresh type variable
        checker.fresh_infer_var(span)
    } else {
        // Infer from the first element
        let first_element_expr = &elements[0];
        let typed_first_element = type_check_expression(checker, first_element_expr, None)?;
        let inferred_element_ty = typed_first_element.ty.clone();
        typed_elements.push(typed_first_element);

        // Check remaining elements against inferred type
        for element_expr in elements.iter().skip(1) {
            let typed_element = type_check_expression(checker, element_expr, Some(&inferred_element_ty))?;
            typed_elements.push(typed_element);
        }
        inferred_element_ty
    };

    // Resolve the final type
    let final_element_ty = checker.resolve_type(&element_ty);

    // TODO: Add Hash + Eq trait bounds check for final_element_ty

    let set_ty = Ty::with_span(TyKind::Set(Arc::new(final_element_ty)), span);
    Ok((TypedExprKind::HashSet(typed_elements), set_ty))
}