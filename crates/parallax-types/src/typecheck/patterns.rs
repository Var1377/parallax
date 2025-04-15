use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use parallax_resolve::types::{ResolvedPattern, ResolvedPatternKind, Symbol};

use crate::{
    error::{TypeError, TypeResult},
    inference::{Substitution, TypeEnvironment},
    typecheck::{helpers::resolve_variant_symbol_to_names, TypeChecker},
    types::{Ty, TyKind, TypeDef, TypedPattern, TypedPatternKind, TypedPatternField},
};

use super::expressions::check_literal; // Assuming check_literal remains in expressions

/*
/// Type check a pattern, performing unification and adding bindings to the *current* scope.
///
/// - `checker`: Mutable TypeChecker instance.
/// - `pattern`: The ResolvedPattern to check.
/// - `expected_ty`: The type the pattern is expected to match against.
// TODO: Remove if truly unused
pub(crate) fn check_pattern(
    checker: &mut TypeChecker<'_>,
    pattern: &ResolvedPattern,
    expected_ty: &Ty,
) -> TypeResult<TypedPattern> {
    let pattern_span = pattern.span;
    let resolved_expected_ty = checker.resolve_type(expected_ty);

    let typed_kind = match &pattern.kind {
        ResolvedPatternKind::Identifier(name) => {
             if name == "_" {
                 TypedPatternKind::Wildcard
             } else {
                 // Create a fresh symbol for the new binding introduced by the pattern
                 let symbol = Symbol::fresh();
                 // Binding is added later by add_pattern_bindings using the TypedPattern
                 TypedPatternKind::Identifier { symbol, name: name.clone() }
             }
         }
        ResolvedPatternKind::Wildcard => TypedPatternKind::Wildcard,
        ResolvedPatternKind::Literal(lit) => {
             let (_lit_expr_kind, lit_ty) = check_literal(checker, lit, pattern.span)?;
             checker.unify(&lit_ty, &resolved_expected_ty)?;
             TypedPatternKind::Literal(lit.clone())
         }
        ResolvedPatternKind::Tuple(element_patterns) => {
            if let TyKind::Tuple(expected_element_tys) = &resolved_expected_ty.kind {
                 if element_patterns.len() != expected_element_tys.len() {
                    return Err(TypeError::InternalError { 
                        message: format!("Tuple pattern expected {} elements, found {}", expected_element_tys.len(), element_patterns.len()),
                        span: Some(pattern.span),
                    });
                 }
                 let mut typed_elements = Vec::with_capacity(element_patterns.len());
                 for (elem_pat, expected_elem_ty) in element_patterns.iter().zip(expected_element_tys.iter()) {
                    let typed_elem = check_pattern(checker, elem_pat, expected_elem_ty)?;
                    typed_elements.push(typed_elem);
                 }
                 TypedPatternKind::Tuple(typed_elements)
            } else {
                 let dummy_expected_tuple = Ty::new(TyKind::Tuple(vec![Ty::new(TyKind::Error); element_patterns.len()]));
                 checker.unify(&resolved_expected_ty, &dummy_expected_tuple)?;
                 return Err(TypeError::InternalError { 
                     message: format!("Pattern type mismatch: Expected Tuple, found {}", crate::error::display_type(&resolved_expected_ty)),
                     span: Some(pattern.span),
                 });
            }
        }
        ResolvedPatternKind::Array(element_patterns) => {
            if let TyKind::Array(expected_elem_ty, expected_size) = &resolved_expected_ty.kind {
                let mut typed_elements = Vec::with_capacity(element_patterns.len());
                let mut has_rest = false;
                let mut non_rest_count = 0;

                for elem_pat in element_patterns {
                    if let ResolvedPatternKind::Rest = elem_pat.kind {
                        if has_rest {
                             return Err(TypeError::InternalError { 
                                 message: "Multiple rest patterns ('...') are not allowed in an array pattern".to_string(),
                                 span: Some(elem_pat.span),
                            });
                        }
                        has_rest = true;
                        // Rest pattern itself doesn't have a type binding, but its element type is known
                        typed_elements.push(TypedPattern {
                            kind: TypedPatternKind::Rest,
                            span: elem_pat.span,
                            ty: (**expected_elem_ty).clone(),
                        });
                    } else {
                        non_rest_count += 1;
                        let typed_elem = check_pattern(checker, elem_pat, expected_elem_ty)?;
                        typed_elements.push(typed_elem);
                    }
                }

                if !has_rest && non_rest_count != *expected_size {
                    return Err(TypeError::InternalError { 
                        message: format!("Array pattern expected {} elements, found {}", expected_size, non_rest_count),
                        span: Some(pattern.span),
                    });
                } else if has_rest && non_rest_count > *expected_size {
                     return Err(TypeError::InternalError { 
                        message: format!("Array pattern has {} elements, but expected array size is only {}", non_rest_count, expected_size),
                        span: Some(pattern.span),
                    });
                }

                TypedPatternKind::Array(typed_elements)
            } else {
                return Err(TypeError::InternalError { 
                     message: format!("Pattern type mismatch: Expected Array, found {}", crate::error::display_type(&resolved_expected_ty)),
                     span: Some(pattern.span),
                 });
            }
        }
        ResolvedPatternKind::Struct { struct_symbol, fields: pattern_fields } => {
            let our_struct_symbol = *struct_symbol;
            let struct_name = checker.get_name_for_symbol(our_struct_symbol)?;
            if let TyKind::Named { name: expected_name, args: instance_type_args } = &resolved_expected_ty.kind {
                if expected_name != &struct_name {
                     return Err(TypeError::InternalError {
                        message: format!("Pattern type mismatch: Expected struct '{}', found struct '{}'", struct_name, expected_name),
                        span: Some(pattern.span),
                    });
                }

                let struct_def = if let Some(TypeDef::Struct(def)) = checker.type_ctx.get_type(&struct_name).cloned() {
                    def
                } else {
                     return Err(TypeError::InternalError {
                        message: format!("Could not find struct definition for '{}'", struct_name),
                        span: Some(pattern.span),
                    });
                };
                
                let mut typed_pattern_fields = Vec::with_capacity(pattern_fields.len());
                let mut provided_field_names = HashSet::new();

                for field_pat in pattern_fields {
                    if !provided_field_names.insert(field_pat.name.clone()) {
                        return Err(TypeError::InternalError {
                            message: format!("Field '{}' specified more than once in pattern for struct '{}'", field_pat.name, struct_name),
                            span: Some(field_pat.span),
                        });
                    }

                    if let Some(field_def) = struct_def.fields.iter().find(|f| f.name == field_pat.name) {
                        let mut instance_subst = Substitution::new();
                        if struct_def.generic_params.len() != instance_type_args.len() {
                            return Err(TypeError::InternalError { message: format!("Generic arity mismatch checking struct pattern field for '{}'", struct_name), span: Some(field_pat.span) });
                        }
                         for (gen_param_def, instance_arg_ty) in struct_def.generic_params.iter().zip(instance_type_args.iter()) {
                             instance_subst.insert(gen_param_def.id, instance_arg_ty.clone());
                         }
                         let expected_field_ty = field_def.ty.apply_subst(&instance_subst);
                         let resolved_expected_field_ty = checker.resolve_type(&expected_field_ty);

                        let typed_nested_pattern = if let Some(nested_pat) = &field_pat.pattern {
                            Some(check_pattern(checker, nested_pat, &resolved_expected_field_ty)?)
                        } else {
                            // Shorthand: { field } implies binding `field` with the field's type
                            // Create a synthetic Identifier pattern for the binding
                             Some(TypedPattern {
                                 kind: TypedPatternKind::Identifier {
                                     symbol: Symbol::fresh(), // Need a symbol for binding
                                     name: field_pat.name.clone(),
                                 },
                                 span: field_pat.span, // Span of the shorthand field name
                                 ty: resolved_expected_field_ty,
                             })
                        };

                        typed_pattern_fields.push(TypedPatternField {
                            name: field_pat.name.clone(),
                            // If shorthand produced Some(TypedPattern), use it
                            // Otherwise (if nested pattern was Some(ResolvedPattern)), use its checked result
                            pattern: typed_nested_pattern, 
                            span: field_pat.span,
                        });

                    } else {
                        return Err(TypeError::UnknownStructField {
                            field: field_pat.name.clone(),
                            struct_name: struct_name.clone(),
                            span: field_pat.span,
                        });
                    }
                }

                TypedPatternKind::Struct { 
                    struct_name: struct_name.clone(),
                    fields: typed_pattern_fields, 
                }

            } else {
                 return Err(TypeError::InternalError { 
                     message: format!("Pattern type mismatch: Expected Struct '{}', found {}", struct_name, crate::error::display_type(&resolved_expected_ty)),
                     span: Some(pattern.span),
                 });
            }
        }
        ResolvedPatternKind::Constructor { symbol: variant_symbol, args: args_pattern } => {
            let our_variant_symbol = *variant_symbol;
            let (enum_name, variant_name) = resolve_variant_symbol_to_names(checker, our_variant_symbol, pattern.span)?;
            
            let enum_def = if let Some(TypeDef::Enum(def)) = checker.type_ctx.get_type(&enum_name).cloned() {
                def
            } else {
                 return Err(TypeError::InternalError { 
                     message: format!("Enum definition '{}' not found for variant pattern '{}'", enum_name, variant_name),
                     span: Some(pattern.span), 
                 });
            };

             let variant_def = if let Some(v_def) = enum_def.variants.iter().find(|v| v.name == variant_name) {
                v_def
            } else {
                 return Err(TypeError::InternalError { 
                    message: format!("Variant definition '{}' not found within enum '{}'", variant_name, enum_name),
                    span: Some(pattern.span), 
                });
            };

            if let TyKind::Named { name: expected_enum_name, args: instance_type_args } = &resolved_expected_ty.kind {
                 if expected_enum_name != &enum_name {
                     return Err(TypeError::InternalError {
                         message: format!("Pattern type mismatch: Expected enum '{}', found enum '{}'", enum_name, expected_enum_name),
                         span: Some(pattern.span),
                     });
                 }
                
                 match variant_def.fields.is_empty() {
                     true => {
                         match &args_pattern.kind {
                            ResolvedPatternKind::Wildcard => { /* Allowed */ }
                            ResolvedPatternKind::Tuple(elements) if elements.is_empty() => { /* Allowed */ }
                            _ => {
                                 return Err(TypeError::InternalError {
                                     message: format!("Pattern for unit variant '{}' should not have arguments (only () or _)", variant_name),
                                     span: Some(args_pattern.span),
                                });
                            }
                         }
                         let typed_args_pat = check_pattern(checker, args_pattern, &Ty::new(TyKind::Tuple(vec![])))?;
                         TypedPatternKind::Constructor { enum_name: enum_name.clone(), variant_name: variant_name.clone(), args: Box::new(typed_args_pat) }
                     }
                     false => {
                         let mut instance_subst = Substitution::new();
                         if enum_def.generic_params.len() != instance_type_args.len() {
                            return Err(TypeError::InternalError { message: "Generic arity mismatch checking constructor pattern".to_string(), span: Some(pattern.span) });
                         }
                          for (gen_param_def, instance_arg_ty) in enum_def.generic_params.iter().zip(instance_type_args.iter()) {
                              instance_subst.insert(gen_param_def.id, instance_arg_ty.clone());
                          }
                          let expected_arg_field_types: Vec<Ty> = variant_def.fields.iter()
                             .map(|f| checker.resolve_type(&f.ty.apply_subst(&instance_subst)))
                             .collect();
                        
                         let expected_args_tuple_ty = Ty::new(TyKind::Tuple(expected_arg_field_types));
                         let typed_args_pat = check_pattern(checker, args_pattern, &expected_args_tuple_ty)?;
                         
                         TypedPatternKind::Constructor { 
                             enum_name: enum_name.clone(), 
                             variant_name: variant_name.clone(), 
                             args: Box::new(typed_args_pat) 
                         }
                     }
                 }
            } else {
                 return Err(TypeError::InternalError { 
                     message: format!("Pattern type mismatch: Expected Enum '{}', found {}", enum_name, crate::error::display_type(&resolved_expected_ty)),
                     span: Some(pattern.span),
                 });
            }
        }
        ResolvedPatternKind::Rest => {
            TypedPatternKind::Rest
        }
        ResolvedPatternKind::Or(left_pat, right_pat) => {
            // Need to check bindings carefully here.
            // Create snapshots to rollback environment modifications from sub-pattern checks.
            let snapshot_before = checker.inference_ctx.snapshot();
            let env_snapshot_before = checker.type_env.clone();

            // Check left, capture bindings
            checker.type_env = Arc::new(TypeEnvironment::with_parent(env_snapshot_before.clone()));
            let typed_left = check_pattern(checker, left_pat, &resolved_expected_ty)?;
            let left_bindings = checker.type_env.get_current_scope_bindings();
            checker.type_env = env_snapshot_before.clone(); // Restore env
            checker.inference_ctx.rollback(snapshot_before.clone()); // Rollback inference state

            // Check right, capture bindings
            checker.type_env = Arc::new(TypeEnvironment::with_parent(env_snapshot_before.clone()));
            let typed_right = check_pattern(checker, right_pat, &resolved_expected_ty)?;
            let right_bindings = checker.type_env.get_current_scope_bindings();
            checker.type_env = env_snapshot_before; // Restore env fully
            checker.inference_ctx.rollback(snapshot_before); // Rollback inference state again

            // --- Compare and unify bindings ---
            if left_bindings.keys().collect::<HashSet<_>>() != right_bindings.keys().collect::<HashSet<_>>() {
                 return Err(TypeError::OrPatternBindingMismatch {
                    span: pattern.span,
                });
            }

            let mut unified_bindings = HashMap::new();
            for (name, left_ty) in left_bindings {
                if let Some(right_ty) = right_bindings.get(&name) {
                    // Use the main checker context for unification now
                    checker.unify(&left_ty, right_ty)?;
                    let unified_ty = checker.resolve_type(&left_ty);
                    unified_bindings.insert(name.clone(), unified_ty);
                } 
            }

            // Add unified bindings to the *outer* scope (the scope where the match arm lives)
            // This happens *after* check_pattern returns, in add_pattern_bindings.
            // check_pattern itself should not modify the outer scope for Or patterns.

            TypedPatternKind::Or(Box::new(typed_left), Box::new(typed_right))
        }
        // Removed unreachable _ => {} arm
    };

    Ok(TypedPattern {
        kind: typed_kind,
        span: pattern_span,
        ty: expected_ty.clone(),
    })
}
*/

// Placeholder for add_pattern_bindings if it needs to be defined here or imported
// (It was defined in control_flow.rs temporarily)
// use crate::typecheck::control_flow::add_pattern_bindings; 