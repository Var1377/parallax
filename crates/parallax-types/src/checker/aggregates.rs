use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use miette::SourceSpan;
use parallax_resolve::types::{ResolvedArgument, ResolvedExpr, Symbol};

// Removed TODO, updated imports
use crate::context::inference::Substitution; // Updated path
use crate::error::{TypeError, TypeResult};
use crate::types::{Ty, TyKind, TypeDef, TypedArgument, TypedExprKind};

// Updated checker imports to use super::
use super::expr::type_check_expression;
use super::generics::instantiate; // Moved from resolve
use super::resolve::resolve_variant_symbol_to_names;
use super::TypeChecker;

#[allow(dead_code)]
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

#[allow(dead_code)]
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

#[allow(dead_code)]
pub(crate) fn check_struct_literal(
    checker: &mut TypeChecker,
    struct_symbol: Symbol,
    fields: &[(String, ResolvedExpr)],
    base: Option<&ResolvedExpr>,
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    let struct_name = checker.get_name_for_symbol(struct_symbol)?;

    let struct_def =
        if let Some(TypeDef::Struct(def)) = checker.type_ctx.get_type(&struct_name).cloned() {
            def
        } else {
            return Err(TypeError::InternalError {
                message: format!("Could not find struct definition for '{}'", struct_name),
                span: Some(span),
            });
        };

    let base_struct_ty = Ty::with_span(
        TyKind::Named {
            name: struct_name.clone(),
            symbol: Some(struct_symbol),
            args: vec![],
        },
        span,
    );
    let (instantiated_struct_ty, _generic_map) =
        instantiate(checker, &base_struct_ty, span, &struct_def.generic_params)?;

    let instance_args = if let TyKind::Named { ref args, .. } = instantiated_struct_ty.kind {
        args.clone()
    } else {
        return Err(TypeError::InternalError {
            message: format!(
                "Internal Error: Instantiated struct type '{}' is not Named",
                struct_name
            ),
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
            for (gen_param_def, instance_arg_ty) in
                struct_def.generic_params.iter().zip(instance_args.iter())
            {
                instance_subst.insert(gen_param_def.id, instance_arg_ty.clone());
            }
            let expected_field_ty = field_def.ty.apply_subst(&instance_subst);
            let resolved_expected_field_ty = checker.resolve_type(&expected_field_ty);

            let typed_field_value = type_check_expression(
                checker,
                field_value_expr,
                Some(&resolved_expected_field_ty),
            )?;
            typed_fields.push((field_name.clone(), typed_field_value));
        } else {
            return Err(TypeError::UnknownStructField {
                field: field_name.clone(),
                struct_name: struct_name.clone(),
                span: field_value_expr.span,
            });
        }
    }

    let typed_base_expr = if let Some(base_expr) = base {
        // 1. Check the base expression. It must unify with the struct type.
        let typed_base = type_check_expression(checker, base_expr, Some(&instantiated_struct_ty))?;
        let base_ty = checker.resolve_type(&typed_base.ty);
        // Double check unification after resolving base type
        checker.unify(&base_ty, &instantiated_struct_ty)?;

        // 5. Check base field compatibility for *missing* fields
        let base_struct_def = if let TyKind::Named {
            name: base_name, ..
        } = &base_ty.kind
        {
            if let Some(TypeDef::Struct(def)) = checker.type_ctx.get_type(base_name) {
                def
            } else {
                return Err(TypeError::InternalError {
                    message: format!("Base expression type '{}' is not a known struct", base_name),
                    span: Some(base_expr.span),
                });
            }
        } else {
            return Err(TypeError::NotAStruct {
                found: crate::error::display_type(&base_ty),
                span: base_expr.span,
            });
        };

        // TODO: Need to handle generics correctly when comparing base fields.
        // For now, we assume the base expression *already* has the correct instantiated type due to the unify above.
        for required_field_def in &struct_def.fields {
            if !provided_field_names.contains(required_field_def.name.as_str()) {
                // This field is missing, ensure base provides it compatibly
                if !base_struct_def.fields.iter().any(|base_field| {
                    base_field.name == required_field_def.name
                } /* && types unify */)
                {
                    return Err(TypeError::MissingField {
                        field: required_field_def.name.clone(),
                        struct_name: format!(
                            "{} (needed for update syntax from base)",
                            struct_name
                        ),
                        span: base_expr.span, // Span points to base expr
                    });
                }
                // TODO: Add type unification check between required_field_def.ty (instantiated) and base_field.ty (instantiated)
            }
        }
        Some(Box::new(typed_base))
    } else {
        // No base expression, ensure all fields are provided
        for field_def in &struct_def.fields {
            if !provided_field_names.contains(field_def.name.as_str()) {
                return Err(TypeError::MissingField {
                    field: field_def.name.clone(),
                    struct_name: struct_name.clone(),
                    span,
                });
            }
        }
        None
    };

    let final_struct_ty = checker.resolve_type(&instantiated_struct_ty);

    Ok((
        TypedExprKind::Struct {
            name: struct_name,
            fields: typed_fields,
            base: typed_base_expr,
        },
        final_struct_ty,
    ))
}

#[allow(dead_code)]
pub(crate) fn check_variant_constructor(
    checker: &mut TypeChecker,
    variant_symbol: Symbol,
    args: &[ResolvedArgument],
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    let (enum_name, variant_name) = resolve_variant_symbol_to_names(checker, variant_symbol, span)?;

    let enum_def = if let Some(TypeDef::Enum(def)) = checker.type_ctx.get_type(&enum_name).cloned()
    {
        def
    } else {
        return Err(TypeError::InternalError {
            message: format!(
                "Enum definition '{}' not found for variant constructor '{}'",
                enum_name, variant_name
            ),
            span: Some(span),
        });
    };

    let variant_def = if let Some(v_def) = enum_def.variants.iter().find(|v| v.name == variant_name)
    {
        v_def
    } else {
        return Err(TypeError::InternalError {
            message: format!(
                "Variant definition '{}' not found within enum '{}'",
                variant_name, enum_name
            ),
            span: Some(span),
        });
    };

    let base_enum_ty = Ty::with_span(
        TyKind::Named {
            name: enum_name.clone(),
            symbol: Some(enum_def.symbol),
            args: vec![],
        },
        span,
    );
    let (instantiated_enum_ty_base, _generic_map) =
        instantiate(checker, &base_enum_ty, span, &enum_def.generic_params)?;

    let mut typed_args = Vec::with_capacity(args.len());
    match variant_def.fields.is_empty() {
        true => {
            if !args.is_empty() {
                return Err(TypeError::WrongNumberOfArguments {
                    expected: 0,
                    found: args.len(),
                    span,
                });
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
                return Err(TypeError::WrongNumberOfArguments {
                    expected: struct_fields.len(),
                    found: args.len(),
                    span,
                });
            }

            let mut provided_args_map = HashMap::new();
            for arg in args {
                let arg_name = arg.name.as_ref().unwrap();
                if provided_args_map.contains_key(arg_name) {
                    return Err(TypeError::InternalError {
                        message: format!(
                            "Duplicate argument '{}' provided for variant constructor '{}'",
                            arg_name, variant_name
                        ),
                        span: Some(arg.span),
                    });
                }
                provided_args_map.insert(arg_name.clone(), arg);
            }

            for expected_field in struct_fields {
                if let Some(arg) = provided_args_map.get(&expected_field.name) {
                    let mut instance_subst = Substitution::new();
                    let instance_args =
                        if let TyKind::Named { ref args, .. } = instantiated_enum_ty_base.kind {
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
                    for (gen_param_def, instance_arg_ty) in
                        enum_def.generic_params.iter().zip(instance_args.iter())
                    {
                        instance_subst.insert(gen_param_def.id, instance_arg_ty.clone());
                    }
                    let expected_ty = expected_field.ty.apply_subst(&instance_subst);
                    let resolved_expected_ty = checker.resolve_type(&expected_ty);

                    let typed_arg_value =
                        type_check_expression(checker, &arg.value, Some(&resolved_expected_ty))?;
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

#[allow(dead_code)]
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

#[allow(dead_code)]
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
            let typed_element =
                type_check_expression(checker, element_expr, Some(&inferred_element_ty))?;
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        checker::{expr::type_check_expression, TypeChecker},
        context::*,
        error::*,
        types::*,
        PrimitiveType, TypeDatabase,
    };
    use miette::SourceSpan;
    use parallax_resolve::{
        definitions::*, types::*, PrimitiveType as ResolvePrimitiveType, ResolveDatabase,
    };
    use parallax_source::SourceDatabase;
    use parallax_syntax::{ast::common::Literal as AstLiteral, SyntaxDatabase};
    use salsa::Database;
    use std::{
        collections::HashMap,
        sync::{Arc, Mutex},
    };

    // --- Test Setup ---
    #[salsa::db]
    #[derive(Default, Clone)]
    pub struct DummyDb {
        storage: salsa::Storage<Self>,
    }
    impl salsa::Database for DummyDb {
        fn salsa_event(&self, event: &dyn Fn() -> salsa::Event) {}
    }
    #[salsa::db]
    impl SourceDatabase for DummyDb {}
    #[salsa::db]
    impl SyntaxDatabase for DummyDb {}
    #[salsa::db]
    impl ResolveDatabase for DummyDb {}
    #[salsa::db]
    impl TypeDatabase for DummyDb {}

    fn dummy_span() -> SourceSpan {
        SourceSpan::from((0, 0))
    }

    fn setup_checker() -> TypeChecker<'static> {
        let db_mock = DummyDb::default();
        let db_leaked: &'static DummyDb = Box::leak(Box::new(db_mock));
        let defs_leaked: &'static ResolvedDefinitions =
            Box::leak(Box::new(ResolvedDefinitions::default()));
        let type_ctx = TypeContext::new();
        let trait_repo = TraitRepository::new();
        TypeChecker::new(db_leaked, defs_leaked, type_ctx, trait_repo)
    }

    fn ty_prim(prim: PrimitiveType) -> Ty {
        Ty::with_span(TyKind::Primitive(prim), dummy_span())
    }
    fn ty_var(id: u32) -> Ty {
        Ty::with_span(TyKind::Var(TypeId(id)), dummy_span())
    }
    fn ty_named(name: &str, symbol: Option<Symbol>, args: Vec<Ty>) -> Ty {
        Ty::with_span(
            TyKind::Named {
                name: name.to_string(),
                symbol,
                args,
            },
            dummy_span(),
        )
    }
    fn ty_tuple(tys: Vec<Ty>) -> Ty {
        Ty::with_span(TyKind::Tuple(tys), dummy_span())
    }
    fn ty_array(elem_ty: Ty, size: usize) -> Ty {
        Ty::with_span(TyKind::Array(Arc::new(elem_ty), size), dummy_span())
    }
    fn ty_map(key: Ty, val: Ty) -> Ty {
        Ty::with_span(TyKind::Map(Arc::new(key), Arc::new(val)), dummy_span())
    }
    fn ty_set(elem: Ty) -> Ty {
        Ty::with_span(TyKind::Set(Arc::new(elem)), dummy_span())
    }

    fn resolved_lit_int(val: i128) -> ResolvedExpr {
        ResolvedExpr {
            kind: ResolvedExprKind::Literal(AstLiteral::Int {
                value: val,
                suffix: None,
            }),
            span: dummy_span(),
            resolved_type: ResolvedType::IntegerLiteral,
        }
    }
    fn resolved_lit_bool(val: bool) -> ResolvedExpr {
        ResolvedExpr {
            kind: ResolvedExprKind::Literal(AstLiteral::Bool(val)),
            span: dummy_span(),
            resolved_type: ResolvedType::Primitive(ResolvePrimitiveType::Bool),
        }
    }
    fn resolved_lit_string(val: &str) -> ResolvedExpr {
        ResolvedExpr {
            kind: ResolvedExprKind::Literal(AstLiteral::String(val.to_string())),
            span: dummy_span(),
            resolved_type: ResolvedType::Primitive(ResolvePrimitiveType::String),
        }
    }

    fn add_struct_def(
        checker: &mut TypeChecker,
        name: &str,
        sym: Symbol,
        gen_params: Vec<(String, TypeId)>,
        field_defs: Vec<(String, Symbol, Ty)>,
    ) {
        let fields = field_defs
            .into_iter()
            .map(|(n, fs, t)| Field {
                name: n,
                symbol: fs,
                ty: t,
                span: dummy_span(),
            })
            .collect();
        let generic_params = gen_params
            .into_iter()
            .map(|(n, id)| GenericParamDef {
                name: n,
                symbol: Symbol::fresh(),
                id,
                bounds: vec![],
                span: dummy_span(),
            })
            .collect();
        let struct_def = StructDef {
            name: name.to_string(),
            symbol: sym,
            generic_params,
            fields,
            span: dummy_span(),
        };
        checker
            .type_ctx
            .add_type(sym, name.to_string(), TypeDef::Struct(struct_def));
    }
    fn add_enum_def(
        checker: &mut TypeChecker,
        name: &str,
        sym: Symbol,
        variants: Vec<(String, Symbol, Vec<(String, Symbol, Ty)>)>,
    ) {
        let enum_variants = variants
            .into_iter()
            .map(|(v_name, v_sym, v_fields)| {
                let fields = v_fields
                    .into_iter()
                    .map(|(f_name, f_sym, f_ty)| Field {
                        name: f_name,
                        symbol: f_sym,
                        ty: f_ty,
                        span: dummy_span(),
                    })
                    .collect();
                EnumVariant {
                    name: v_name,
                    symbol: v_sym,
                    fields,
                    span: dummy_span(),
                }
            })
            .collect();
        let enum_def = EnumDef {
            name: name.to_string(),
            symbol: sym,
            generic_params: vec![],
            variants: enum_variants,
            span: dummy_span(),
        };
        checker
            .type_ctx
            .add_type(sym, name.to_string(), TypeDef::Enum(enum_def));
        if let TypeDef::Enum(ed) = checker.type_ctx.get_type(name).unwrap() {
            // Collect variant info first to avoid borrow conflict
            let variants_to_add: Vec<_> = ed.variants.iter().map(|v| (v.symbol, v.name.clone())).collect();
            // Now iterate and modify the context
            for (variant_symbol, variant_name) in variants_to_add {
                checker.type_ctx.add_symbol_name(variant_symbol, variant_name);
            }
        }
    }
    // --- End Setup ---

    // --- Array Literal Tests ---
    #[test]
    fn test_check_array_empty() {
        let mut checker = setup_checker();
        let (kind, ty) = check_array_literal(&mut checker, &[], dummy_span()).unwrap();
        assert!(matches!(kind, TypedExprKind::Array(ref elems) if elems.is_empty()));
        match ty.kind {
            TyKind::Array(elem_ty, size) => {
                assert_eq!(size, 0);
                assert!(
                    matches!(elem_ty.kind, TyKind::Var(_)),
                    "Expected fresh var for empty array"
                );
            }
            _ => panic!("Expected Array type"),
        }
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_check_array_simple() {
        let mut checker = setup_checker();
        let elems = vec![resolved_lit_int(1), resolved_lit_int(2)];
        let (kind, ty) = check_array_literal(&mut checker, &elems, dummy_span()).unwrap();
        match kind {
            TypedExprKind::Array(typed_elems) => {
                assert_eq!(typed_elems.len(), 2);
                assert!(matches!(
                    typed_elems[0].kind,
                    TypedExprKind::IntLiteral { .. }
                ));
                // Type should be unified to i32 after checking all elements
                assert_eq!(
                    typed_elems[0].ty.kind,
                    TyKind::Primitive(PrimitiveType::I32)
                );
            }
            _ => panic!("Expected Array kind"),
        }
        match ty.kind {
            TyKind::Array(elem_ty, size) => {
                assert_eq!(size, 2);
                assert_eq!(elem_ty.kind, TyKind::Primitive(PrimitiveType::I32));
            }
            _ => panic!("Expected Array type"),
        }
    }

    #[test]
    fn test_check_array_mismatch() {
        let mut checker = setup_checker();
        let elems = vec![resolved_lit_int(1), resolved_lit_bool(true)];
        let result = check_array_literal(&mut checker, &elems, dummy_span());
        assert!(result.is_err()); // Error during type_check_expression for the second element
    }

    // --- Tuple Literal Tests ---
    #[test]
    fn test_check_tuple_empty() {
        let mut checker = setup_checker();
        let (kind, ty) = check_tuple_literal(&mut checker, &[], dummy_span()).unwrap();
        assert!(matches!(kind, TypedExprKind::Tuple(ref elems) if elems.is_empty()));
        assert!(matches!(ty.kind, TyKind::Tuple(ref tys) if tys.is_empty()));
    }

    #[test]
    fn test_check_tuple_simple() {
        let mut checker = setup_checker();
        let elems = vec![resolved_lit_int(10), resolved_lit_string("hi")];
        let (kind, ty) = check_tuple_literal(&mut checker, &elems, dummy_span()).unwrap();
        match kind {
            TypedExprKind::Tuple(typed_elems) => {
                assert_eq!(typed_elems.len(), 2);
                assert_eq!(
                    typed_elems[0].ty.kind,
                    TyKind::Primitive(PrimitiveType::I32)
                );
                assert_eq!(
                    typed_elems[1].ty.kind,
                    TyKind::Primitive(PrimitiveType::String)
                );
            }
            _ => panic!("Expected Tuple kind"),
        }
        match ty.kind {
            TyKind::Tuple(tys) => {
                assert_eq!(tys.len(), 2);
                assert_eq!(tys[0].kind, TyKind::Primitive(PrimitiveType::I32));
                assert_eq!(tys[1].kind, TyKind::Primitive(PrimitiveType::String));
            }
            _ => panic!("Expected Tuple type"),
        }
    }

    // --- Struct Literal Tests ---
    #[test]
    fn test_check_struct_simple() {
        let mut checker = setup_checker();
        let struct_sym = Symbol::new(1);
        let field_sym = Symbol::new(2);
        add_struct_def(
            &mut checker,
            "Point",
            struct_sym,
            vec![],
            vec![("x".to_string(), field_sym, ty_prim(PrimitiveType::I32))],
        );
        let fields = vec![("x".to_string(), resolved_lit_int(5))];

        let (kind, ty) =
            check_struct_literal(&mut checker, struct_sym, &fields, None, dummy_span()).unwrap();
        match kind {
            TypedExprKind::Struct {
                name,
                fields: typed_fields,
                base,
            } => {
                assert_eq!(name, "Point");
                assert_eq!(typed_fields.len(), 1);
                assert_eq!(typed_fields[0].0, "x");
                assert_eq!(
                    typed_fields[0].1.ty.kind,
                    TyKind::Primitive(PrimitiveType::I32)
                );
                assert!(base.is_none());
            }
            _ => panic!("Expected Struct kind"),
        }
        assert!(matches!(ty.kind, TyKind::Named { ref name, .. } if name == "Point"));
    }

    #[test]
    fn test_check_struct_missing_field() {
        let mut checker = setup_checker();
        let struct_sym = Symbol::new(1);
        add_struct_def(
            &mut checker,
            "Point",
            struct_sym,
            vec![],
            vec![("x".to_string(), Symbol::new(2), ty_prim(PrimitiveType::I32))],
        );
        let fields = vec![]; // Missing field x
        let result = check_struct_literal(&mut checker, struct_sym, &fields, None, dummy_span());
        assert!(result.is_err());
        assert!(
            matches!(result.err().unwrap(), TypeError::MissingField { field, .. } if field == "x")
        );
    }

    #[test]
    fn test_check_struct_unknown_field() {
        let mut checker = setup_checker();
        let struct_sym = Symbol::new(1);
        add_struct_def(&mut checker, "Point", struct_sym, vec![], vec![]);
        let fields = vec![("x".to_string(), resolved_lit_int(5))]; // Unknown field x
        let result = check_struct_literal(&mut checker, struct_sym, &fields, None, dummy_span());
        assert!(result.is_err());
        assert!(
            matches!(result.err().unwrap(), TypeError::UnknownStructField { field, .. } if field == "x")
        );
    }

    #[test]
    fn test_check_struct_update_syntax() {
        let mut checker = setup_checker();
        let struct_sym = Symbol::new(1);
        let field_x = Symbol::new(2);
        let field_y = Symbol::new(3);
        add_struct_def(
            &mut checker,
            "Point",
            struct_sym,
            vec![],
            vec![
                ("x".to_string(), field_x, ty_prim(PrimitiveType::I32)),
                ("y".to_string(), field_y, ty_prim(PrimitiveType::I32)),
            ],
        );
        // Base expression (variable 'p' of type Point)
        let base_var_sym = Symbol::new(100);
        let base_expr = ResolvedExpr {
            kind: ResolvedExprKind::Variable {
                binding_symbol: base_var_sym,
                name: "p".to_string(),
            },
            span: dummy_span(),
            resolved_type: ResolvedType::UserDefined {
                symbol: struct_sym,
                type_args: None,
            },
        };
        // Add base var using clone-on-write pattern
        let base_ty = ty_named("Point", Some(struct_sym), vec![]);
        if let Some(env) = Arc::get_mut(&mut checker._type_env) {
            env.add("p".to_string(), base_ty);
        } else {
            let mut cloned_env = (*checker._type_env).clone();
            cloned_env.add("p".to_string(), base_ty);
            checker._type_env = Arc::new(cloned_env);
        }

        let fields = vec![("y".to_string(), resolved_lit_int(10))]; // Update only y

        let (kind, ty) = check_struct_literal(
            &mut checker,
            struct_sym,
            &fields,
            Some(&base_expr),
            dummy_span(),
        )
        .unwrap();
        match kind {
            TypedExprKind::Struct {
                name,
                fields: typed_fields,
                base,
            } => {
                assert_eq!(name, "Point");
                assert_eq!(typed_fields.len(), 1);
                assert_eq!(typed_fields[0].0, "y");
                assert!(base.is_some());
                assert!(
                    matches!(base.unwrap().kind, TypedExprKind::Variable { name, .. } if name == "p")
                );
            }
            _ => panic!("Expected Struct kind"),
        }
        assert!(matches!(ty.kind, TyKind::Named { ref name, .. } if name == "Point"));
    }

    // --- Variant Constructor Tests ---
    #[test]
    fn test_check_variant_unit() {
        let mut checker = setup_checker();
        let enum_sym = Symbol::new(1);
        let var_sym = Symbol::new(2);
        add_enum_def(
            &mut checker,
            "Status",
            enum_sym,
            vec![("Ok".to_string(), var_sym, vec![])],
        );
        let args = vec![];

        let (kind, ty) =
            check_variant_constructor(&mut checker, var_sym, &args, dummy_span()).unwrap();
        match kind {
            TypedExprKind::VariantConstructor {
                enum_name,
                variant_name,
                args: typed_args,
            } => {
                assert_eq!(enum_name, "Status");
                assert_eq!(variant_name, "Ok");
                assert!(typed_args.is_empty());
            }
            _ => panic!("Expected VariantConstructor kind"),
        }
        assert!(matches!(ty.kind, TyKind::Named { ref name, .. } if name == "Status"));
    }

    #[test]
    fn test_check_variant_struct_like() {
        let mut checker = setup_checker();
        let enum_sym = Symbol::new(1);
        let var_sym = Symbol::new(2);
        let field_sym = Symbol::new(3);
        add_enum_def(
            &mut checker,
            "Payload",
            enum_sym,
            vec![(
                "Data".to_string(),
                var_sym,
                vec![(
                    "value".to_string(),
                    field_sym,
                    ty_prim(PrimitiveType::String),
                )],
            )],
        );
        let args = vec![ResolvedArgument {
            name: Some("value".to_string()),
            value: resolved_lit_string("test"),
            span: dummy_span(),
        }];

        let (kind, ty) =
            check_variant_constructor(&mut checker, var_sym, &args, dummy_span()).unwrap();
        match kind {
            TypedExprKind::VariantConstructor {
                enum_name,
                variant_name,
                args: typed_args,
            } => {
                assert_eq!(enum_name, "Payload");
                assert_eq!(variant_name, "Data");
                assert_eq!(typed_args.len(), 1);
                assert_eq!(typed_args[0].name, Some("value".to_string()));
                assert_eq!(
                    typed_args[0].value.ty.kind,
                    TyKind::Primitive(PrimitiveType::String)
                );
            }
            _ => panic!("Expected VariantConstructor kind"),
        }
        assert!(matches!(ty.kind, TyKind::Named { ref name, .. } if name == "Payload"));
    }

    // --- Map Literal Tests ---
    #[test]
    fn test_check_map_empty() {
        let mut checker = setup_checker();
        let (kind, ty) = check_map_literal(&mut checker, &[], dummy_span()).unwrap();
        assert!(matches!(kind, TypedExprKind::Map(ref entries) if entries.is_empty()));
        match ty.kind {
            TyKind::Map(key_ty, val_ty) => {
                assert!(
                    matches!(key_ty.kind, TyKind::Var(_)),
                    "Expected fresh var for key"
                );
                assert!(
                    matches!(val_ty.kind, TyKind::Var(_)),
                    "Expected fresh var for value"
                );
            }
            _ => panic!("Expected Map type"),
        }
    }

    #[test]
    fn test_check_map_simple() {
        let mut checker = setup_checker();
        let entries = vec![
            (resolved_lit_string("a"), resolved_lit_int(1)),
            (resolved_lit_string("b"), resolved_lit_int(2)),
        ];
        let (kind, ty) = check_map_literal(&mut checker, &entries, dummy_span()).unwrap();
        assert!(matches!(kind, TypedExprKind::Map(ref typed_entries) if typed_entries.len() == 2));
        match ty.kind {
            TyKind::Map(key_ty, val_ty) => {
                assert_eq!(key_ty.kind, TyKind::Primitive(PrimitiveType::String));
                assert_eq!(val_ty.kind, TyKind::Primitive(PrimitiveType::I32));
            }
            _ => panic!("Expected Map type"),
        }
    }

    // --- HashSet Literal Tests ---
    #[test]
    fn test_check_hashset_empty() {
        let mut checker = setup_checker();
        let (kind, ty) = check_hashset_literal(&mut checker, &[], dummy_span()).unwrap();
        assert!(matches!(kind, TypedExprKind::HashSet(ref elems) if elems.is_empty()));
        match ty.kind {
            TyKind::Set(elem_ty) => {
                assert!(
                    matches!(elem_ty.kind, TyKind::Var(_)),
                    "Expected fresh var for element"
                );
            }
            _ => panic!("Expected Set type"),
        }
    }

    #[test]
    fn test_check_hashset_simple() {
        let mut checker = setup_checker();
        let elements = vec![resolved_lit_int(1), resolved_lit_int(2)];
        let (kind, ty) = check_hashset_literal(&mut checker, &elements, dummy_span()).unwrap();
        assert!(matches!(kind, TypedExprKind::HashSet(ref typed_elems) if typed_elems.len() == 2));
        match ty.kind {
            TyKind::Set(elem_ty) => {
                assert_eq!(elem_ty.kind, TyKind::Primitive(PrimitiveType::I32));
            }
            _ => panic!("Expected Set type"),
        }
    }
}
