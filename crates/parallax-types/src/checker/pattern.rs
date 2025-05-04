// src/checker/pattern.rs
//! Type checking pass 2: Pattern matching.

use super::TypeChecker;
use crate::error::{TypeError, TypeResult, display_type};
use crate::types::*;
use crate::context::Substitution;
use parallax_resolve::types::{Symbol, ResolvedPattern, ResolvedPatternKind, ResolvedPatternField, ResolvedType};
use parallax_syntax::ast::common::Literal as AstLiteral;
use miette::SourceSpan;
use std::collections::{HashMap, HashSet};

/// Type checks a single resolved pattern node against an expected type.
///
/// Preconditions: `checker` context is set up.
///                `resolved_pat` is the pattern node to check.
///                `expected_ty` is the type the pattern is being matched against.
/// Postconditions: Returns `Ok(TypedPattern)` representing the type-checked pattern.
///                 Returns `Err(TypeError)` if checking fails.
///                 `checker.env` is updated with bindings introduced by the pattern.
///                 `checker.infctx` may be updated.
pub(crate) fn check_pattern(checker: &mut TypeChecker, resolved_pat: &ResolvedPattern, expected_ty: &Ty) -> TypeResult<TypedPattern> {
    println!("Checking pattern kind: {:?} against type {}", resolved_pat.kind, display_type(expected_ty));
    let span = resolved_pat.span;

    let (typed_kind, inferred_ty) = match &resolved_pat.kind {
        ResolvedPatternKind::Wildcard => (TypedPatternKind::Wildcard, expected_ty.clone()),
        ResolvedPatternKind::Literal(lit) => {
            // Check the literal's base type (doesn't introduce bindings)
            let lit_ty_kind = match lit {
                AstLiteral::Int { .. } => {
                    // Use a fresh inference variable for pattern integer literals
                    let fresh_var = checker.fresh_var();
                    let var_id = match fresh_var.kind {
                        TyKind::Var(id) => id,
                        _ => panic!("fresh_var did not return TyKind::Var"),
                    };
                    TyKind::InferInt(var_id)
                },
                AstLiteral::Float { .. } => {
                    let fresh_var = checker.fresh_var();
                    let var_id = match fresh_var.kind {
                        TyKind::Var(id) => id,
                        _ => panic!("fresh_var did not return TyKind::Var"),
                    };
                    TyKind::InferFloat(var_id)
                },
                AstLiteral::String(_) => TyKind::Primitive(PrimitiveType::String),
                AstLiteral::Char(_) => TyKind::Primitive(PrimitiveType::Char),
                AstLiteral::Bool(_) => TyKind::Primitive(PrimitiveType::Bool),
            };
            let inferred_ty = Ty::with_span(lit_ty_kind, span);
            (TypedPatternKind::Literal(lit.clone()), inferred_ty)
        }
        ResolvedPatternKind::Identifier(name) => check_pattern_identifier(checker, name, expected_ty, span)?,
        ResolvedPatternKind::Struct { struct_symbol, fields } => {
            check_pattern_struct(checker, *struct_symbol, fields, expected_ty, span)?
        }
        ResolvedPatternKind::Tuple(patterns) => check_pattern_tuple(checker, patterns, expected_ty, span)?,
        ResolvedPatternKind::Constructor { symbol, args } => {
             check_pattern_constructor(checker, *symbol, args, expected_ty, span)?
        }
        ResolvedPatternKind::Array(patterns) => check_pattern_array(checker, patterns, expected_ty, span)?,
        ResolvedPatternKind::Or(lhs, rhs) => check_pattern_or(checker, lhs, rhs, expected_ty, span)?,
        ResolvedPatternKind::Rest => {
             // Map Rest in ResolvedPatternKind to Rest in TypedPatternKind
             // Type inference/checking for Rest needs context from array/tuple check.
             // For now, assume it matches the expected type segment it replaces.
             println!("Warning: '..' pattern type checking is placeholder");
             (TypedPatternKind::Rest, expected_ty.clone()) // Placeholder type
        }
    };

    // Unify the type derived/inferred from the pattern structure with the expected type.
    if !checker.unify(&inferred_ty, expected_ty, span) {
        // Error already reported by unify
        // Return inferred type even if unification fails, error is already logged.
    }

    let final_ty = checker.infctx.apply_substitution(&inferred_ty);
    Ok(TypedPattern { kind: typed_kind, span, ty: final_ty })
}

/// Check an identifier pattern (variable binding).
fn check_pattern_identifier(checker: &mut TypeChecker, name: &str, expected_ty: &Ty, _span: SourceSpan) -> TypeResult<(TypedPatternKind, Ty)> {
    let symbol = Symbol::fresh();
    let mut current_env = (*checker.env).clone();
    current_env.add(name.to_string(), symbol, expected_ty.clone());
    checker.env = std::sync::Arc::new(current_env);
    Ok((TypedPatternKind::Identifier { name: name.to_string(), symbol }, expected_ty.clone()))
}

/// Check a struct pattern.
fn check_pattern_struct(checker: &mut TypeChecker, struct_symbol: Symbol, fields: &[ResolvedPatternField], expected_ty: &Ty, span: SourceSpan) -> TypeResult<(TypedPatternKind, Ty)> {
    // Expected type must be a Named struct type matching struct_symbol
    if let TyKind::Named { symbol: Some(expected_symbol), name, args: type_args } = &expected_ty.kind {
        if *expected_symbol != struct_symbol {
            return Err(TypeError::WrongPatternType { expected: name.clone(), found: "different struct pattern".to_string(), span });
        }

        // Lookup struct definition
        let struct_def = match checker.type_ctx.get_struct_def(&struct_symbol, span) {
            Ok(def) => def.clone(), // Clone to avoid borrow issues
            Err(e) => return Err(e.into()), // Convert internal error if needed
        };

        // Instantiate generic parameters of the struct
        let mut instantiation_subst = Substitution::new();
        if struct_def.generic_params.len() != type_args.len() {
             return Err(TypeError::GenericArgCountMismatch {
                 kind: "Struct pattern".to_string(),
                 name: struct_def.name.clone(),
                 expected: struct_def.generic_params.len(),
                 found: type_args.len(),
                 span,
             });
        }
        for (param, arg) in struct_def.generic_params.iter().zip(type_args.iter()) {
            instantiation_subst.insert(param.id, arg.clone());
        }

        // Check provided fields against struct definition
        let mut typed_fields = Vec::new();
        let mut provided_field_names = HashSet::new();
        for resolved_field_pat in fields {
             let field_name = &resolved_field_pat.name;
             provided_field_names.insert(field_name.clone());

             if let Some(struct_field_def) = struct_def.fields.iter().find(|f| f.name == *field_name) {
                 let expected_field_ty = struct_field_def.ty.apply_subst(&instantiation_subst);

                 // Handle shorthand `Point { x, y }` vs `Point { x: pat_x, y: pat_y }`
                 let sub_pattern_ast = resolved_field_pat.pattern.as_ref().map_or_else(
                     || {
                         // Shorthand: create an Identifier pattern with the field name
                         ResolvedPattern {
                             kind: ResolvedPatternKind::Identifier(field_name.clone()),
                             span: resolved_field_pat.span,
                             resolved_type: ResolvedType::Unknown, // Provide Unknown
                         }
                     },
                     |p| p.clone() // Use the provided pattern
                 );

                 let typed_sub_pattern = check_pattern(checker, &sub_pattern_ast, &expected_field_ty)?;
                 typed_fields.push(TypedPatternField {
                    name: field_name.clone(),
                    pattern: typed_sub_pattern, // Store the fully checked sub-pattern
                    span: resolved_field_pat.span,
                 });
             } else {
                 return Err(TypeError::UnknownStructField {
                     field: field_name.clone(),
                     struct_name: struct_def.name.clone(),
                     span: resolved_field_pat.span,
                 });
             }
        }

        // Check for missing fields (unless `..` is present)
        // Check if a rest pattern exists in the resolved fields (need resolver support for `..`)
        let has_rest_pattern = fields.iter().any(|f| f.name == ".."); // Placeholder check
        if !has_rest_pattern {
             for struct_field_def in &struct_def.fields {
                 if !provided_field_names.contains(&struct_field_def.name) {
                     // Proper error for missing field without `..`
                      return Err(TypeError::MissingField {
                         field: struct_field_def.name.clone(),
                         struct_name: struct_def.name.clone(),
                         span, // Use overall pattern span
                     });
                 }
             }
        }

        // Inferred type is the original expected type after substitution
        let inferred_ty = checker.infctx.apply_substitution(expected_ty);
        Ok((TypedPatternKind::Struct { 
            struct_name: struct_def.name.clone(), 
            struct_symbol,
            fields: typed_fields 
        }, inferred_ty))

    } else {
        Err(TypeError::WrongPatternType { expected: "struct type".to_string(), found: display_type(expected_ty), span })
    }
}

/// Check a tuple pattern.
fn check_pattern_tuple(checker: &mut TypeChecker, patterns: &[ResolvedPattern], expected_ty: &Ty, span: SourceSpan) -> TypeResult<(TypedPatternKind, Ty)> {
     if let TyKind::Tuple(ref expected_element_tys) = expected_ty.kind {
         let has_rest = patterns.iter().any(|p| matches!(p.kind, ResolvedPatternKind::Rest));

         if !has_rest && patterns.len() != expected_element_tys.len() {
             return Err(TypeError::WrongPatternType {
                 expected: format!("at most {} elements for tuple of size {}", expected_element_tys.len(), expected_element_tys.len()),
                 found: format!("pattern with {} elements (including '..')", patterns.len()),
                 span,
             });
         }

         let mut typed_patterns = Vec::new();
         let mut inferred_element_tys = Vec::new();
         let mut pat_idx = 0;
         let mut type_idx = 0;
         let num_pats_before_rest = patterns.iter().position(|p| matches!(p.kind, ResolvedPatternKind::Rest)).unwrap_or(patterns.len());
         let num_pats_after_rest = if has_rest { patterns.len() - num_pats_before_rest - 1 } else { 0 };

         // Iterate matching patterns to expected types, handling rest
         // Check elements before `..`
         while pat_idx < num_pats_before_rest {
             if type_idx >= expected_element_tys.len() {
                 // Should not happen if size check passed or rest is present
                 return Err(TypeError::InternalError { message: "Tuple pattern index out of bounds (before rest)".to_string(), span: Some(span) });
             }
             let pat = &patterns[pat_idx];
             let expected_elem_ty = &expected_element_tys[type_idx];
             let typed_pat = check_pattern(checker, pat, expected_elem_ty)?;
             inferred_element_tys.push(typed_pat.ty.clone());
             typed_patterns.push(typed_pat);
             pat_idx += 1;
             type_idx += 1;
         }

        // Handle `..`
         if has_rest {
             // Ensure there are enough elements in the expected type to cover the non-rest patterns.
             if expected_element_tys.len() < num_pats_before_rest + num_pats_after_rest {
                 return Err(TypeError::WrongPatternType { expected: format!("at least {} elements", num_pats_before_rest + num_pats_after_rest), found: format!("tuple of size {}", expected_element_tys.len()), span });
             }
             let num_skipped = expected_element_tys.len() - num_pats_before_rest - num_pats_after_rest;
             type_idx += num_skipped; // Skip the types covered by `..`
             // Add the Rest pattern itself
             typed_patterns.push(TypedPattern { kind: TypedPatternKind::Rest, span: patterns[pat_idx].span, ty: Ty::new(TyKind::Error) }); // Type of Rest is irrelevant
             pat_idx += 1;
         }

        // Check elements after `..`
        while pat_idx < patterns.len() {
             if type_idx >= expected_element_tys.len() {
                  return Err(TypeError::WrongPatternType { 
                     expected: format!("at most {} elements for tuple of size {}", expected_element_tys.len(), expected_element_tys.len()),
                     found: format!("pattern with {} elements (including '..')", patterns.len()),
                     span 
                 });
             }
             let pat = &patterns[pat_idx];
             let expected_elem_ty = &expected_element_tys[type_idx];
             let typed_pat = check_pattern(checker, pat, expected_elem_ty)?;
             inferred_element_tys.push(typed_pat.ty.clone());
             typed_patterns.push(typed_pat);
             pat_idx += 1;
             type_idx += 1;
        }

         let inferred_ty = Ty::with_span(TyKind::Tuple(inferred_element_tys), span);
         Ok((TypedPatternKind::Tuple(typed_patterns), inferred_ty))
     } else {
         Err(TypeError::WrongPatternType { expected: "tuple type".to_string(), found: display_type(expected_ty), span })
     }
}

/// Check an enum constructor pattern.
fn check_pattern_constructor(checker: &mut TypeChecker, variant_symbol: Symbol, args_pattern: &ResolvedPattern, expected_ty: &Ty, span: SourceSpan) -> TypeResult<(TypedPatternKind, Ty)> {
     if let TyKind::Named { symbol: Some(enum_symbol), name: _enum_name, args: type_args } = &expected_ty.kind {
         let enum_def = match checker.type_ctx.get_enum_def(enum_symbol, span) {
             Ok(def) => def.clone(),
             Err(e) => return Err(e.into()),
         };

         // Find variant definition using variant_symbol
         let variant_def = match enum_def.variants.iter().find(|v| v.symbol == variant_symbol).cloned() {
            Some(v) => v,
            None => return Err(TypeError::UnknownVariant { variant: format!("Symbol {:?}", variant_symbol), enum_name: enum_def.name.clone(), span })
         };

         // Instantiate generics
         let mut instantiation_subst = Substitution::new();
         if enum_def.generic_params.len() != type_args.len() {
             return Err(TypeError::GenericArgCountMismatch {
                 kind: "Enum pattern".to_string(),
                 name: enum_def.name.clone(),
                 expected: enum_def.generic_params.len(),
                 found: type_args.len(),
                 span
             });
         }
         for (param, arg) in enum_def.generic_params.iter().zip(type_args.iter()) {
             instantiation_subst.insert(param.id, arg.clone());
         }

         let mut typed_args: Vec<TypedPatternArgument> = Vec::new();

         // Check args_pattern based on variant definition kind
         match &variant_def.fields {
             // Unit Variant (no fields)
             fields if fields.is_empty() => {
                 match &args_pattern.kind {
                     ResolvedPatternKind::Tuple(ref elems) if elems.is_empty() => { /* ok, no args */ }
                     ResolvedPatternKind::Wildcard => { /* ok, no args */ }
                     // Explicit () required for unit variant pattern
                     _ => return Err(TypeError::WrongPatternType { expected: "unit pattern () or _".to_string(), found: "non-unit pattern".to_string(), span: args_pattern.span })
                 }
            }
             // Tuple Variant (unnamed fields)
             fields if fields.iter().all(|f| f.name.parse::<usize>().is_ok()) => {
                 let expected_field_tys: Vec<Ty> = fields.iter()
                             .map(|f| f.ty.apply_subst(&instantiation_subst))
                             .collect();
                         
                 // Expect a Tuple pattern for the arguments
                 if let ResolvedPatternKind::Tuple(ref resolved_arg_pats) = args_pattern.kind {
                     let has_rest = resolved_arg_pats.iter().any(|p| matches!(p.kind, ResolvedPatternKind::Rest));

                     if !has_rest && resolved_arg_pats.len() != expected_field_tys.len() {
                            return Err(TypeError::WrongPatternType { expected: format!("tuple pattern size {}", expected_field_tys.len()), found: format!("size {}", resolved_arg_pats.len()), span: args_pattern.span });
                         }
                     if has_rest && resolved_arg_pats.len() > expected_field_tys.len() + 1 {
                          return Err(TypeError::WrongPatternType { expected: format!("at most {} elements for tuple variant {}", expected_field_tys.len(), variant_def.name), found: format!("pattern with {} elements (including '..')", resolved_arg_pats.len()), span: args_pattern.span });
                     }

                     let num_pats_before_rest = resolved_arg_pats.iter().position(|p| matches!(p.kind, ResolvedPatternKind::Rest)).unwrap_or(resolved_arg_pats.len());
                     let num_pats_after_rest = if has_rest { resolved_arg_pats.len() - num_pats_before_rest - 1 } else { 0 };
                     
                     if has_rest && expected_field_tys.len() < num_pats_before_rest + num_pats_after_rest {
                          return Err(TypeError::WrongPatternType { expected: format!("at least {} elements", num_pats_before_rest + num_pats_after_rest), found: format!("tuple variant {} with {} fields", variant_def.name, expected_field_tys.len()), span: args_pattern.span });
                     }

                     let mut pat_idx = 0;
                     let mut type_idx = 0;

                     // Check elements before `..`
                     while pat_idx < num_pats_before_rest {
                         let arg_pat = &resolved_arg_pats[pat_idx];
                         let expected_field_ty = &expected_field_tys[type_idx];
                         let typed_sub_pattern = check_pattern(checker, arg_pat, expected_field_ty)?;
                         typed_args.push(TypedPatternArgument::Positional(typed_sub_pattern));
                         pat_idx += 1;
                         type_idx += 1;
                     }

                     // Handle `..`
                     if has_rest {
                         let num_skipped = expected_field_tys.len() - num_pats_before_rest - num_pats_after_rest;
                         type_idx += num_skipped;
                         // Add Rest argument placeholder
                         typed_args.push(TypedPatternArgument::Rest(args_pattern.span));
                         pat_idx += 1;
                     }

                     // Check elements after `..`
                     while pat_idx < resolved_arg_pats.len() {
                         let arg_pat = &resolved_arg_pats[pat_idx];
                         let expected_field_ty = &expected_field_tys[type_idx];
                             let typed_sub_pattern = check_pattern(checker, arg_pat, expected_field_ty)?;
                             typed_args.push(TypedPatternArgument::Positional(typed_sub_pattern));
                         pat_idx += 1;
                         type_idx += 1;
                         }
                 } else {
                     // Mismatched pattern kind for tuple variant args
                     return Err(TypeError::WrongPatternType { expected: "tuple pattern for variant args".to_string(), found: "other pattern".to_string(), span: args_pattern.span });
                 }
             }
             // Struct Variant (named fields)
             _ => {
                // Expect a Struct pattern for the arguments
                 if let ResolvedPatternKind::Struct { fields: resolved_field_pats, .. } = &args_pattern.kind {
                         let mut provided_field_names = HashSet::new();
                     let variant_fields_map: HashMap<_,_> = variant_def.fields.iter().map(|f| (f.name.clone(), f)).collect();

                         for resolved_field_pat in resolved_field_pats {
                            let field_name = &resolved_field_pat.name;
                            provided_field_names.insert(field_name.clone());

                         if let Some(variant_field_def) = variant_fields_map.get(field_name) {
                                let expected_field_ty = variant_field_def.ty.apply_subst(&instantiation_subst);
                                let sub_pattern_ast = resolved_field_pat.pattern.as_ref().map_or_else(
                                 || ResolvedPattern {
                                            kind: ResolvedPatternKind::Identifier(field_name.clone()),
                                            span: resolved_field_pat.span,
                                     resolved_type: ResolvedType::Unknown,
                                    },
                                 |p| p.clone()
                                );
                                let typed_sub_pattern = check_pattern(checker, &sub_pattern_ast, &expected_field_ty)?;
                                typed_args.push(TypedPatternArgument::Named(TypedPatternField {
                                    name: field_name.clone(),
                                    pattern: typed_sub_pattern,
                                    span: resolved_field_pat.span,
                                }));
                            } else {
                             return Err(TypeError::UnknownField { field: field_name.clone(), ty: format!("variant {}", variant_def.name), span: resolved_field_pat.span });
                            }
                         }

                         // Check for missing fields (unless `..`)
                     let has_rest = resolved_field_pats.iter().any(|f| f.name == "..");
                         if !has_rest {
                            for variant_field_def in &variant_def.fields {
                                if !provided_field_names.contains(&variant_field_def.name) {
                                     return Err(TypeError::MissingField {
                                         field: variant_field_def.name.clone(),
                                     struct_name: format!("{}::{}", enum_def.name, variant_def.name), // Be more specific
                                     span: args_pattern.span, // Span of the struct pattern
                                     });
                                }
                            }
                         }
                      // Add Rest argument placeholder if present
                     if has_rest {
                         // Find the span of the .. pattern
                         let rest_span = resolved_field_pats.iter().find(|f| f.name == "..").map_or(args_pattern.span, |f| f.span);
                         typed_args.push(TypedPatternArgument::Rest(rest_span));
                     }
                 } else {
                    // Mismatched pattern kind for struct variant args
                     return Err(TypeError::WrongPatternType { expected: "struct pattern for variant args".to_string(), found: "other pattern".to_string(), span: args_pattern.span });
                }
            }
         }

         let inferred_ty = checker.infctx.apply_substitution(expected_ty);
         Ok((TypedPatternKind::Constructor {
             enum_name: enum_def.name.clone(),
             variant_name: variant_def.name.clone(),
             enum_symbol: *enum_symbol,
             variant_symbol,
             args: typed_args // Use the collected args
         }, inferred_ty))

     } else {
         Err(TypeError::WrongPatternType { expected: "enum type".to_string(), found: display_type(expected_ty), span })
     }
}

/// Check an array pattern.
fn check_pattern_array(checker: &mut TypeChecker, patterns: &[ResolvedPattern], expected_ty: &Ty, span: SourceSpan) -> TypeResult<(TypedPatternKind, Ty)> {
    if let TyKind::Array(ref expected_element_ty, expected_size) = expected_ty.kind {
        let has_rest = patterns.iter().any(|p| matches!(p.kind, ResolvedPatternKind::Rest));

        let expected_size_val = expected_size.unwrap_or(patterns.len());

        if !has_rest && patterns.len() != expected_size_val {
             return Err(TypeError::WrongPatternType {
                 expected: format!("at least {} elements for array of size {}", expected_size_val, expected_size_val),
                 found: format!("array pattern of size {}", patterns.len()),
                 span
             });
        }

        let mut typed_patterns = Vec::new();
        let mut pat_idx = 0;
        let mut type_idx = 0; // Not strictly needed for arrays, but for consistency
        let num_pats_before_rest = patterns.iter().position(|p| matches!(p.kind, ResolvedPatternKind::Rest)).unwrap_or(patterns.len());
        let num_pats_after_rest = if has_rest { patterns.len() - num_pats_before_rest - 1 } else { 0 };

        // Check patterns before `..`
        for pat in patterns.iter().take(num_pats_before_rest) {
            let typed_pat = check_pattern(checker, pat, expected_element_ty)?;
            typed_patterns.push(typed_pat);
            pat_idx += 1;
            type_idx += 1;
        }
        
        // Add placeholder for `..` if present
        if has_rest {
             typed_patterns.push(TypedPattern{ kind: TypedPatternKind::Rest, span, ty: Ty::new(TyKind::Error) });
             pat_idx += 1;
             // Skip expected types for the `..` part
             type_idx = expected_size_val.saturating_sub(num_pats_after_rest);
        }

        // Check patterns after `..`
        for pat in patterns.iter().skip(pat_idx) {
            if expected_size.is_some() && type_idx >= expected_size_val {
                // More patterns than expected size allows, even with rest
                 return Err(TypeError::WrongPatternType { 
                     expected: format!("array of size {}", expected_size_val),
                     found: format!("pattern for at least {} elements (including '..')", patterns.len() - 1),
                     span
                 });
            }
            let typed_pat = check_pattern(checker, pat, expected_element_ty)?;
            typed_patterns.push(typed_pat);
            type_idx += 1;
        }

        let inferred_ty = expected_ty.clone();
        Ok((TypedPatternKind::Array(typed_patterns), inferred_ty))
    } else {
         Err(TypeError::WrongPatternType {
             expected: "array type".to_string(),
             found: display_type(expected_ty),
             span,
         })
    }
}

// Helper to collect bound variables (symbol and type) from a pattern
fn collect_bindings(pattern: &TypedPattern) -> HashMap<Symbol, Ty> {
    let mut bindings = HashMap::new();
    match &pattern.kind {
        TypedPatternKind::Identifier { symbol, name: _ } => {
            bindings.insert(*symbol, pattern.ty.clone());
        }
        TypedPatternKind::Struct { fields, .. } => {
            for field in fields {
                bindings.extend(collect_bindings(&field.pattern));
            }
        }
        TypedPatternKind::Tuple(pats) | TypedPatternKind::Array(pats) => {
            for pat in pats {
                bindings.extend(collect_bindings(pat));
            }
        }
        TypedPatternKind::Constructor { args, .. } => {
            for arg in args {
                match arg {
                    TypedPatternArgument::Positional(pat) => {
                        bindings.extend(collect_bindings(pat));
                    }
                    TypedPatternArgument::Named(field_pat) => {
                        bindings.extend(collect_bindings(&field_pat.pattern));
                    }
                    TypedPatternArgument::Rest(_) => { /* Rest doesn't bind variables */ }
                }
            }
        }
        TypedPatternKind::Or(lhs, _rhs) => {
            // Bindings must be consistent, collect from one side (e.g., lhs)
            bindings.extend(collect_bindings(lhs));
        }
        // Wildcard, Literal, Rest don't bind variables
        _ => {}
    }
    bindings
}

/// Check an or-pattern.
fn check_pattern_or(checker: &mut TypeChecker, lhs: &ResolvedPattern, _rhs: &ResolvedPattern, expected_ty: &Ty, span: SourceSpan) -> TypeResult<(TypedPatternKind, Ty)> {
    // Check both branches independently.
    // Need to snapshot/restore environment and inference state carefully.

    // Snapshot before LHS
    let env_snapshot_before = checker.env.clone();
    let infctx_snapshot_before = checker.infctx.snapshot();

    let typed_lhs = check_pattern(checker, lhs, expected_ty)?;
    let lhs_bindings = collect_bindings(&typed_lhs);
    let lhs_env = checker.env.clone(); // Env state after LHS check
    checker.infctx.rollback_to(infctx_snapshot_before.clone()); // Rollback inference changes from LHS
    checker.env = env_snapshot_before.clone(); // Restore env before RHS

    // Snapshot before RHS (only infctx needed now)
    let infctx_snapshot_before_rhs = checker.infctx.snapshot();

    let typed_rhs = check_pattern(checker, _rhs, expected_ty)?;
    let rhs_bindings = collect_bindings(&typed_rhs);
    let _rhs_env = checker.env.clone(); // Env state after RHS check
    checker.infctx.rollback_to(infctx_snapshot_before_rhs); // Rollback inference changes from RHS
    checker.env = env_snapshot_before; // Restore original env

    // Unify overall types
    if !checker.unify(&typed_lhs.ty, &typed_rhs.ty, span) {
        // Error reported
    }
    let unified_ty = checker.infctx.apply_substitution(&typed_lhs.ty);

    // Check binding consistency
    if lhs_bindings.len() != rhs_bindings.len() || !lhs_bindings.keys().all(|k| rhs_bindings.contains_key(k)) {
         return Err(TypeError::OrPatternBindingMismatch { span });
    }

    // Unify types of corresponding bindings
    for (symbol, lhs_ty) in &lhs_bindings {
        let rhs_ty = rhs_bindings.get(symbol).unwrap(); // We know the symbol exists
        if !checker.unify(lhs_ty, rhs_ty, span) { // Use overall span for now
            // Could report a more specific binding mismatch error
             return Err(TypeError::OrPatternBindingMismatch { span });
        }
    }

    // If consistent, restore the environment from one branch (e.g., LHS)
    // This assumes the unified binding types are now correct.
    checker.env = lhs_env; // Restore env with bindings from LHS

    Ok((TypedPatternKind::Or(Box::new(typed_lhs), Box::new(typed_rhs)), unified_ty))
} 