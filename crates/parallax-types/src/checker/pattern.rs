use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use parallax_resolve::types::{ResolvedPattern, ResolvedPatternKind, Symbol};

// Removed TODO, updated imports
use crate::{
    error::{TypeError, TypeResult},
    context::inference::{Substitution, TypeEnvironment},
    types::{Ty, TyKind, TypeDef, TypedPattern, TypedPatternKind, TypedPatternField},
};
use super::{resolve::resolve_variant_symbol_to_names, TypeChecker, expr::check_literal}; // Updated paths

/// Type check a pattern, performing unification and adding bindings to the *current* scope.
///
/// - `checker`: Mutable TypeChecker instance.
/// - `pattern`: The ResolvedPattern to check.
/// - `expected_ty`: The type the pattern is expected to match against.
#[allow(dead_code)]
pub fn check_pattern(
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
            if let TyKind::Named { name: expected_name, symbol: _, args: instance_type_args } = &resolved_expected_ty.kind {
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

            if let TyKind::Named { name: expected_enum_name, symbol: _, args: instance_type_args } = &resolved_expected_ty.kind {
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
            let env_snapshot_before = checker._type_env.clone(); // Use _type_env

            // Check left, capture bindings
            checker._type_env = Arc::new(TypeEnvironment::with_parent(env_snapshot_before.clone()));
            let typed_left = check_pattern(checker, left_pat, &resolved_expected_ty)?;
            let left_bindings = checker._type_env.get_current_scope_bindings();
            checker._type_env = env_snapshot_before.clone(); // Restore env
            checker.inference_ctx.rollback(snapshot_before.clone()); // Rollback inference state

            // Check right, capture bindings
            checker._type_env = Arc::new(TypeEnvironment::with_parent(env_snapshot_before.clone()));
            let typed_right = check_pattern(checker, right_pat, &resolved_expected_ty)?;
            let right_bindings = checker._type_env.get_current_scope_bindings();
            checker._type_env = env_snapshot_before; // Restore env fully
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
    };

    Ok(TypedPattern {
        kind: typed_kind,
        span: pattern_span,
        ty: resolved_expected_ty.clone(), // Use the resolved expected type
    })
}

// Placeholder for add_pattern_bindings if it needs to be defined here or imported
// (It was defined in control_flow.rs temporarily)
// use crate::checker::control_flow::add_pattern_bindings;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{checker::TypeChecker, context::*, types::*, TypeDatabase, error::*, types::PrimitiveType as CheckerPrimitiveType };
    use parallax_resolve::{ResolveDatabase, types::*, definitions::*, PrimitiveType as ResolverPrimitiveType };
    use parallax_syntax::{SyntaxDatabase, ast::common::Literal as AstLiteral};
    use parallax_source::SourceDatabase;
    use miette::SourceSpan;
    use salsa::Database;
    use std::{collections::HashMap, sync::{Arc, Mutex}};

    // --- Test Setup ---
    #[salsa::db]
    #[derive(Default, Clone)]
    pub struct DummyDb {
        storage: salsa::Storage<Self>,
    }
    impl salsa::Database for DummyDb {
        fn salsa_event(&self, event: &dyn Fn() -> salsa::Event) {}
    }
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

    fn ty_prim(prim: CheckerPrimitiveType) -> Ty { Ty::with_span(TyKind::Primitive(prim), dummy_span()) }
    fn ty_var(id: u32) -> Ty { Ty::with_span(TyKind::Var(TypeId(id)), dummy_span()) }
    fn ty_named(name: &str, symbol: Option<Symbol>, args: Vec<Ty>) -> Ty { Ty::with_span(TyKind::Named { name: name.to_string(), symbol, args }, dummy_span()) }
    fn ty_tuple(tys: Vec<Ty>) -> Ty { Ty::with_span(TyKind::Tuple(tys), dummy_span()) }
    fn ty_array(elem_ty: Ty, size: usize) -> Ty { Ty::with_span(TyKind::Array(Arc::new(elem_ty), size), dummy_span()) }

    fn pat_wildcard() -> ResolvedPattern { ResolvedPattern { kind: ResolvedPatternKind::Wildcard, span: dummy_span(), resolved_type: ResolvedType::Unknown } }
    fn pat_ident(name: &str) -> ResolvedPattern { ResolvedPattern { kind: ResolvedPatternKind::Identifier(name.to_string()), span: dummy_span(), resolved_type: ResolvedType::Unknown } }
    fn pat_lit_int(val: i128) -> ResolvedPattern { ResolvedPattern { kind: ResolvedPatternKind::Literal(AstLiteral::Int { value: val, suffix: None }), span: dummy_span(), resolved_type: ResolvedType::IntegerLiteral } }
    fn pat_lit_bool(val: bool) -> ResolvedPattern { ResolvedPattern { kind: ResolvedPatternKind::Literal(AstLiteral::Bool(val)), span: dummy_span(), resolved_type: ResolvedType::Primitive(ResolverPrimitiveType::Bool) } }
    fn pat_tuple(pats: Vec<ResolvedPattern>) -> ResolvedPattern { ResolvedPattern { kind: ResolvedPatternKind::Tuple(pats), span: dummy_span(), resolved_type: ResolvedType::Unknown } }
    fn pat_array(pats: Vec<ResolvedPattern>) -> ResolvedPattern { ResolvedPattern { kind: ResolvedPatternKind::Array(pats), span: dummy_span(), resolved_type: ResolvedType::Unknown } }
    fn pat_rest() -> ResolvedPattern { ResolvedPattern { kind: ResolvedPatternKind::Rest, span: dummy_span(), resolved_type: ResolvedType::Unknown } }
    fn pat_or(left: ResolvedPattern, right: ResolvedPattern) -> ResolvedPattern { ResolvedPattern { kind: ResolvedPatternKind::Or(Box::new(left), Box::new(right)), span: dummy_span(), resolved_type: ResolvedType::Unknown } }
    fn pat_struct(sym: Symbol, fields: Vec<ResolvedPatternField>) -> ResolvedPattern { ResolvedPattern { kind: ResolvedPatternKind::Struct { struct_symbol: sym, fields }, span: dummy_span(), resolved_type: ResolvedType::Unknown } }
    fn pat_field(name: &str, pat: Option<ResolvedPattern>) -> ResolvedPatternField { ResolvedPatternField { name: name.to_string(), pattern: pat, span: dummy_span() } }
    fn pat_constructor(sym: Symbol, args: ResolvedPattern) -> ResolvedPattern { ResolvedPattern { kind: ResolvedPatternKind::Constructor { symbol: sym, args: Box::new(args) }, span: dummy_span(), resolved_type: ResolvedType::Unknown } }

    // Helper to add a simple struct def to checker
    fn add_struct_def(checker: &mut TypeChecker, name: &str, sym: Symbol, field_defs: Vec<(String, Symbol, Ty)>) {
        let fields = field_defs.into_iter().map(|(n, fs, t)| Field { name: n, symbol: fs, ty: t, span: dummy_span() }).collect();
        let struct_def = StructDef {
            name: name.to_string(), symbol: sym, generic_params: vec![], fields,
            span: dummy_span(),
        };
        checker.type_ctx.add_type(sym, name.to_string(), TypeDef::Struct(struct_def));
    }

    // Helper to add a simple enum def
    fn add_enum_def(checker: &mut TypeChecker, name: &str, sym: Symbol, variants: Vec<(String, Symbol, Vec<(String, Symbol, Ty)>)>) {
        let enum_variants = variants.into_iter().map(|(v_name, v_sym, v_fields)| {
            let fields = v_fields.into_iter().map(|(f_name, f_sym, f_ty)| {
                Field { name: f_name, symbol: f_sym, ty: f_ty, span: dummy_span() }
            }).collect();
            EnumVariant { name: v_name, symbol: v_sym, fields, span: dummy_span() }
        }).collect();
        let enum_def = EnumDef {
            name: name.to_string(), symbol: sym, generic_params: vec![], variants: enum_variants,
            span: dummy_span(),
        };
        checker.type_ctx.add_type(sym, name.to_string(), TypeDef::Enum(enum_def));
        // Fix borrow checker error: Collect symbols/names first
        let variants_to_add: Vec<(Symbol, String)> = if let Some(TypeDef::Enum(ed)) = checker.type_ctx.get_type(name) {
            ed.variants.iter().map(|v| (v.symbol, v.name.clone())).collect()
        } else {
            vec![] // Should not happen if add_type succeeded
        };
        for (v_sym, v_name) in variants_to_add {
             checker.type_ctx.add_symbol_name(v_sym, v_name);
        }
    }

    #[test]
    fn test_check_pattern_wildcard() {
        let mut checker = setup_checker();
        let pat = pat_wildcard();
        let expected = ty_prim(CheckerPrimitiveType::I32);
        let typed_pat = check_pattern(&mut checker, &pat, &expected).unwrap();
        assert!(matches!(typed_pat.kind, TypedPatternKind::Wildcard));
        assert_eq!(typed_pat.ty, expected);
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_check_pattern_literal_match() {
        let mut checker = setup_checker();
        let pat = pat_lit_int(42);
        let expected = ty_prim(CheckerPrimitiveType::I32);
        let typed_pat = check_pattern(&mut checker, &pat, &expected).unwrap();
        assert!(matches!(typed_pat.kind, TypedPatternKind::Literal(AstLiteral::Int { value: 42, .. })));
        // Unification happens, so pattern ty becomes the expected concrete type
        assert_eq!(typed_pat.ty, expected);
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_check_pattern_literal_mismatch() {
        let mut checker = setup_checker();
        let pat = pat_lit_bool(true);
        let expected = ty_prim(CheckerPrimitiveType::I32);
        let result = check_pattern(&mut checker, &pat, &expected);
        assert!(result.is_err());
        assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
    }

    #[test]
    fn test_check_pattern_identifier() {
        let mut checker = setup_checker();
        let pat = pat_ident("x");
        let expected = ty_prim(CheckerPrimitiveType::String);
        let typed_pat = check_pattern(&mut checker, &pat, &expected).unwrap();
        match typed_pat.kind {
            TypedPatternKind::Identifier { name, symbol } => {
                assert_eq!(name, "x");
                assert_ne!(symbol, Symbol::new(0)); // Ensure a symbol was assigned
            }
            _ => panic!("Expected Identifier kind"),
        }
        assert_eq!(typed_pat.ty, expected);
        // Binding itself happens later in add_pattern_bindings
    }

    #[test]
    fn test_check_pattern_tuple_match() {
        let mut checker = setup_checker();
        let pat = pat_tuple(vec![pat_ident("a"), pat_wildcard()]);
        let expected = ty_tuple(vec![ty_prim(CheckerPrimitiveType::Bool), ty_prim(CheckerPrimitiveType::F64)]);
        let typed_pat = check_pattern(&mut checker, &pat, &expected).unwrap();
        match typed_pat.kind {
            TypedPatternKind::Tuple(elems) => {
                assert_eq!(elems.len(), 2);
                assert!(matches!(elems[0].kind, TypedPatternKind::Identifier { ref name, .. } if name == "a"));
                assert!(matches!(elems[1].kind, TypedPatternKind::Wildcard));
                assert_eq!(elems[0].ty, ty_prim(CheckerPrimitiveType::Bool));
                assert_eq!(elems[1].ty, ty_prim(CheckerPrimitiveType::F64));
            }
            _ => panic!("Expected Tuple kind"),
        }
        assert_eq!(typed_pat.ty, expected);
    }

    #[test]
    fn test_check_pattern_tuple_mismatch_arity() {
        let mut checker = setup_checker();
        let pat = pat_tuple(vec![pat_wildcard()]);
        let expected = ty_tuple(vec![ty_prim(CheckerPrimitiveType::Bool), ty_prim(CheckerPrimitiveType::F64)]);
        let result = check_pattern(&mut checker, &pat, &expected);
        assert!(result.is_err());
        assert!(matches!(result.err().unwrap(), TypeError::InternalError { .. })); // Currently internal error
    }

    #[test]
    fn test_check_pattern_tuple_mismatch_type() {
        let mut checker = setup_checker();
        let pat = pat_tuple(vec![pat_lit_int(1), pat_wildcard()]);
        let expected = ty_tuple(vec![ty_prim(CheckerPrimitiveType::Bool), ty_prim(CheckerPrimitiveType::F64)]);
        let result = check_pattern(&mut checker, &pat, &expected);
        assert!(result.is_err());
        assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. })); // Error during literal check
    }

     #[test]
    fn test_check_pattern_array_fixed_match() {
        let mut checker = setup_checker();
        let pat = pat_array(vec![pat_ident("first"), pat_wildcard()]);
        let expected = ty_array(ty_prim(CheckerPrimitiveType::I16), 2);
        let typed_pat = check_pattern(&mut checker, &pat, &expected).unwrap();
        match typed_pat.kind {
            TypedPatternKind::Array(elems) => {
                assert_eq!(elems.len(), 2);
                assert!(matches!(elems[0].kind, TypedPatternKind::Identifier { ref name, .. } if name == "first"));
                assert!(matches!(elems[1].kind, TypedPatternKind::Wildcard));
                assert_eq!(elems[0].ty, ty_prim(CheckerPrimitiveType::I16));
                assert_eq!(elems[1].ty, ty_prim(CheckerPrimitiveType::I16));
            }
            _ => panic!("Expected Array kind"),
        }
        assert_eq!(typed_pat.ty, expected);
    }

    #[test]
    fn test_check_pattern_array_fixed_mismatch_arity() {
        let mut checker = setup_checker();
        let pat = pat_array(vec![pat_wildcard()]);
        let expected = ty_array(ty_prim(CheckerPrimitiveType::I16), 2);
        let result = check_pattern(&mut checker, &pat, &expected);
        assert!(result.is_err());
        assert!(matches!(result.err().unwrap(), TypeError::InternalError { .. })); // Currently internal
    }

    #[test]
    fn test_check_pattern_array_rest_match() {
        let mut checker = setup_checker();
        let pat = pat_array(vec![pat_ident("x"), pat_rest(), pat_ident("y")]);
        let expected = ty_array(ty_prim(CheckerPrimitiveType::Char), 5);
        let typed_pat = check_pattern(&mut checker, &pat, &expected).unwrap();
        match typed_pat.kind {
            TypedPatternKind::Array(elems) => {
                assert_eq!(elems.len(), 3);
                assert!(matches!(elems[0].kind, TypedPatternKind::Identifier { .. }));
                assert!(matches!(elems[1].kind, TypedPatternKind::Rest));
                assert!(matches!(elems[2].kind, TypedPatternKind::Identifier { .. }));
                assert_eq!(elems[0].ty, ty_prim(CheckerPrimitiveType::Char));
                assert_eq!(elems[1].ty, ty_prim(CheckerPrimitiveType::Char)); // Rest gets element type
                assert_eq!(elems[2].ty, ty_prim(CheckerPrimitiveType::Char));
            }
            _ => panic!("Expected Array kind"),
        }
        assert_eq!(typed_pat.ty, expected);
    }

    #[test]
    fn test_check_pattern_array_rest_too_many_elements() {
        let mut checker = setup_checker();
        let pat = pat_array(vec![pat_wildcard(), pat_wildcard(), pat_rest()]);
        let expected = ty_array(ty_prim(CheckerPrimitiveType::Char), 1); // Only size 1
        let result = check_pattern(&mut checker, &pat, &expected);
        assert!(result.is_err());
        assert!(matches!(result.err().unwrap(), TypeError::InternalError { .. })); // Currently internal
    }

    #[test]
    fn test_check_pattern_struct_match() {
        let mut checker = setup_checker();
        let struct_sym = Symbol::new(10);
        let field_a_sym = Symbol::new(11);
        let field_b_sym = Symbol::new(12);
        add_struct_def(&mut checker, "Point", struct_sym, vec![
            ("x".to_string(), field_a_sym, ty_prim(CheckerPrimitiveType::I32)),
            ("y".to_string(), field_b_sym, ty_prim(CheckerPrimitiveType::I32)),
        ]);
        let expected = ty_named("Point", Some(struct_sym), vec![]);
        let pat = pat_struct(struct_sym, vec![
            pat_field("x", Some(pat_ident("px"))),
            pat_field("y", None), // Shorthand
        ]);

        let typed_pat = check_pattern(&mut checker, &pat, &expected).unwrap();
        match typed_pat.kind {
            TypedPatternKind::Struct { struct_name, fields } => {
                assert_eq!(struct_name, "Point");
                assert_eq!(fields.len(), 2);
                // Field x
                assert_eq!(fields[0].name, "x");
                assert!(fields[0].pattern.is_some());
                let nested_x = fields[0].pattern.as_ref().unwrap();
                assert!(matches!(nested_x.kind, TypedPatternKind::Identifier{ ref name, .. } if name == "px"));
                assert_eq!(nested_x.ty, ty_prim(CheckerPrimitiveType::I32));
                // Field y (shorthand)
                assert_eq!(fields[1].name, "y");
                assert!(fields[1].pattern.is_some());
                 let nested_y = fields[1].pattern.as_ref().unwrap();
                assert!(matches!(nested_y.kind, TypedPatternKind::Identifier{ ref name, .. } if name == "y"));
                assert_eq!(nested_y.ty, ty_prim(CheckerPrimitiveType::I32));
            }
            _ => panic!("Expected Struct pattern kind"),
        }
        assert_eq!(typed_pat.ty, expected);
    }

     #[test]
    fn test_check_pattern_struct_unknown_field() {
        let mut checker = setup_checker();
        let struct_sym = Symbol::new(10);
        add_struct_def(&mut checker, "Point", struct_sym, vec![]);
        let expected = ty_named("Point", Some(struct_sym), vec![]);
        let pat = pat_struct(struct_sym, vec![pat_field("z", None)]);

        let result = check_pattern(&mut checker, &pat, &expected);
        assert!(result.is_err());
        assert!(matches!(result.err().unwrap(), TypeError::UnknownStructField { field, .. } if field == "z"));
    }

    #[test]
    fn test_check_pattern_struct_mismatch_type() {
        let mut checker = setup_checker();
        let struct_sym = Symbol::new(10);
        let other_sym = Symbol::new(20);
        add_struct_def(&mut checker, "Point", struct_sym, vec![]);
        add_struct_def(&mut checker, "Vector", other_sym, vec![]);
        let expected = ty_named("Vector", Some(other_sym), vec![]);
        let pat = pat_struct(struct_sym, vec![]); // Pattern for Point

        let result = check_pattern(&mut checker, &pat, &expected);
        assert!(result.is_err());
        assert!(matches!(result.err().unwrap(), TypeError::InternalError { .. })); // Currently internal
    }

     #[test]
    fn test_check_pattern_constructor_unit_match() {
        let mut checker = setup_checker();
        let enum_sym = Symbol::new(30);
        let var_sym = Symbol::new(31);
        add_enum_def(&mut checker, "Status", enum_sym, vec![
            ("Ok".to_string(), var_sym, vec![])
        ]);
        let expected = ty_named("Status", Some(enum_sym), vec![]);
        let pat = pat_constructor(var_sym, pat_wildcard()); // Status::Ok

        let typed_pat = check_pattern(&mut checker, &pat, &expected).unwrap();
        match typed_pat.kind {
            TypedPatternKind::Constructor { enum_name, variant_name, ref args } => {
                assert_eq!(enum_name, "Status");
                assert_eq!(variant_name, "Ok");
                assert!(matches!(args.kind, TypedPatternKind::Wildcard)); // Args should be checked against unit tuple
            }
            _ => panic!("Expected Constructor kind"),
        }
        assert_eq!(typed_pat.ty, expected);
    }

     #[test]
    fn test_check_pattern_constructor_tuple_match() {
        let mut checker = setup_checker();
        let enum_sym = Symbol::new(40);
        let var_sym = Symbol::new(41);
        let field_sym = Symbol::new(42);
        add_enum_def(&mut checker, "Option", enum_sym, vec![
            ("Some".to_string(), var_sym, vec![("_0".to_string(), field_sym, ty_prim(CheckerPrimitiveType::String))])
        ]);
        let expected = ty_named("Option", Some(enum_sym), vec![]);
        let pat = pat_constructor(var_sym, pat_tuple(vec![pat_ident("val")])); // Option::Some(val)

        let typed_pat = check_pattern(&mut checker, &pat, &expected).unwrap();
        match typed_pat.kind {
            TypedPatternKind::Constructor { enum_name, variant_name, ref args } => {
                assert_eq!(enum_name, "Option");
                assert_eq!(variant_name, "Some");
                match &args.kind {
                     TypedPatternKind::Tuple(elems) => {
                         assert_eq!(elems.len(), 1);
                         assert!(matches!(elems[0].kind, TypedPatternKind::Identifier { ref name, ..} if name == "val"));
                         assert_eq!(elems[0].ty, ty_prim(CheckerPrimitiveType::String));
                     }
                     _ => panic!("Expected Tuple args pattern")
                }
            }
            _ => panic!("Expected Constructor kind"),
        }
        assert_eq!(typed_pat.ty, expected);
    }

    #[test]
    fn test_check_pattern_constructor_mismatch_type() {
        let mut checker = setup_checker();
        let enum_sym = Symbol::new(30);
        let var_sym = Symbol::new(31);
        add_enum_def(&mut checker, "Status", enum_sym, vec![("Ok".to_string(), var_sym, vec![])]);
        let expected = ty_prim(CheckerPrimitiveType::I32); // Expecting i32
        let pat = pat_constructor(var_sym, pat_wildcard()); // Pattern Status::Ok

        let result = check_pattern(&mut checker, &pat, &expected);
        assert!(result.is_err());
        assert!(matches!(result.err().unwrap(), TypeError::InternalError { .. })); // Currently internal
    }

    #[test]
    fn test_check_pattern_or_match() {
        let mut checker = setup_checker();
        let pat = pat_or(pat_lit_int(1), pat_lit_int(2));
        let expected = ty_prim(CheckerPrimitiveType::I32);
        let typed_pat = check_pattern(&mut checker, &pat, &expected).unwrap();
         match typed_pat.kind {
            TypedPatternKind::Or(left, right) => {
                 assert!(matches!(left.kind, TypedPatternKind::Literal(AstLiteral::Int { value: 1, .. })));
                 assert!(matches!(right.kind, TypedPatternKind::Literal(AstLiteral::Int { value: 2, .. })));
                 assert_eq!(left.ty, expected);
                 assert_eq!(right.ty, expected);
            }
            _ => panic!("Expected Or kind"),
        }
         assert_eq!(typed_pat.ty, expected);
    }

    #[test]
    fn test_check_pattern_or_mismatch_bindings() {
        let mut checker = setup_checker();
        let pat = pat_or(pat_ident("x"), pat_lit_int(2)); // x | 2
        let expected = ty_prim(CheckerPrimitiveType::I32);
        let result = check_pattern(&mut checker, &pat, &expected);
        assert!(result.is_err());
         assert!(matches!(result.err().unwrap(), TypeError::OrPatternBindingMismatch { .. }));
    }
} 