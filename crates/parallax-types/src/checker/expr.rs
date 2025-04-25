// Placeholder for expression type checking logic
use parallax_resolve::types::{
    ResolvedExpr, ResolvedExprKind, ResolvedType, PrimitiveType as ResolvePrimitiveType
};
use parallax_syntax::ast::common::Literal as AstLiteral;
use miette::SourceSpan;
use std::sync::Arc;
use parallax_resolve::{definitions::DefinitionKind, types::Symbol};

// Removed TODO, updated imports
use crate::error::{TypeError, TypeResult, display_type};
use crate::context::inference::Substitution; // Updated path
use crate::types::{
    Ty, TyKind, TypeDef, TypedExpr, TypedExprKind,
    PrimitiveType,
};
use super::TypeChecker; // Updated path

// Import functions from sibling checker modules
use super::control_flow::{check_if, check_block, check_match, add_pattern_bindings};
use super::invocation::{check_invocation, check_lambda}; // Renamed
use super::operators::{check_binary, check_unary}; // Uncommented
use super::aggregates::{
    check_array_literal, check_tuple_literal, check_map_literal,
    check_hashset_literal, check_variant_constructor, check_struct_literal,
};
use super::pattern::check_pattern; // Renamed
// Import instantiate helper
use super::generics::instantiate; // Renamed and Moved
use super::resolve::determine_variant_constructor_type; // Import helper

/// Type check an expression and return a typed expression.
#[allow(dead_code)]
pub fn type_check_expression(
    checker: &mut TypeChecker,
    expr: &ResolvedExpr,
    expected_type: Option<&Ty>,
) -> TypeResult<TypedExpr> {
    handle_expression_match(checker, expr, expected_type)
}

/// Helper function that dispatches expression checking based on kind.
#[allow(dead_code)]
fn handle_expression_match(
    checker: &mut TypeChecker,
    expr: &ResolvedExpr,
    expected_type: Option<&Ty>,
) -> TypeResult<TypedExpr> {
    let (typed_kind, initial_ty) = match &expr.kind {
        ResolvedExprKind::Block(exprs) => check_block(checker, exprs, expr.span)?,
        ResolvedExprKind::If { condition, then_branch, else_branch } => {
            check_if(checker, condition, then_branch, else_branch.as_deref(), expr.span)?
        }
        ResolvedExprKind::Binary { left, op, right } => {
            // Type checking operands happens inside check_binary now
            let typed_expr = check_binary(checker, left, op, right, expr.span)?;
            (typed_expr.kind, typed_expr.ty) // Return the kind and ty from the result
        }
        ResolvedExprKind::Unary { op, expr: operand } => {
            // Type checking operand happens inside check_unary now
            let typed_expr = check_unary(checker, op, operand, expr.span)?;
            (typed_expr.kind, typed_expr.ty) // Return the kind and ty from the result
        }
        // Use check_invocation for all call-like expressions
        ResolvedExprKind::Call { func_symbol, args } => {
            // Handle the Option<Symbol>
            if let Some(symbol) = func_symbol {
                // Reconstruct the Path expression
                let func_expr_reconstructed = ResolvedExpr {
                    kind: ResolvedExprKind::Path(*symbol),
                    span: expr.span, // TODO: Get a better span for the path itself?
                    resolved_type: ResolvedType::Unknown, // Add dummy resolved_type
                };
                check_invocation(checker, &func_expr_reconstructed, args, expr.span)?
            } else {
                // // If func_symbol is None, it means the resolver couldn't identify the function.
                // // This should ideally be handled by check_invocation, but we can error here.
                // checker.errors.push(TypeError::NotAFunction {
                //     found: "<unresolved function>".to_string(),
                //     span: expr.span, // Use the call expression's span
                // });
                // (TypedExprKind::Error, panic!("Not a function"))
                panic!("Not a function");
            }
        }
        ResolvedExprKind::Lambda { params, body } => {
            // Lambdas are tricky with expected types. The expected type might be `fn(A, B) -> C`.
            // We need to unify the inferred lambda type with the expectation.
            let (lambda_kind, inferred_lambda_ty) = check_lambda(checker, params, body, expr.span)?;

            // If an expected type is provided, unify it with the inferred lambda type.
            let final_ty = if let Some(expected) = expected_type {
                // Ensure the expected type is a function type or variable
                if matches!(checker.resolve_type(expected).kind, TyKind::Function(_, _) | TyKind::Var(_)) {
                    checker.unify(&inferred_lambda_ty, expected)?;
                    checker.resolve_type(expected) // Return the potentially more specific expected type after unification
                } else {
                    // Expected type is not a function, report mismatch but proceed with inferred type?
                    checker.errors.push(TypeError::TypeMismatch { expected: display_type(expected), found: display_type(&inferred_lambda_ty), span: expr.span });
                    checker.resolve_type(&inferred_lambda_ty) // Fallback to inferred type
                }
            } else {
                checker.resolve_type(&inferred_lambda_ty) // No expectation, just resolve the inferred type
            };

            (lambda_kind, final_ty)
        }
        ResolvedExprKind::Literal(lit) => check_literal(checker, lit, expr.span)?,
        ResolvedExprKind::Path(symbol) => {
            check_path(checker, *symbol, expr.span)?
        }
        ResolvedExprKind::Variable { binding_symbol, name } => {
            // Look up using the variable name in the current type environment
            if let Some(ty) = checker._type_env.get(name) {
                let resolved_ty = checker.resolve_type(ty);
                (TypedExprKind::Variable { symbol: *binding_symbol, name: name.clone() }, resolved_ty)
            } else {
                // Error if the name isn't found (might indicate scope issue or missing binding)
                checker.errors.push(TypeError::InternalError {
                    message: format!(
                        "Resolver indicated local variable '{}' (Symbol {:?}) but it was not found in the type environment.",
                        name,
                        binding_symbol
                    ),
                    span: Some(expr.span),
                });
                panic!("Variable not found in type environment");
            }
        }
        ResolvedExprKind::Field { object, field_name } => {
            check_field(checker, object, field_name, expr.span)?
        }
        ResolvedExprKind::Array(elements) => {
            check_array_literal(checker, elements, expr.span)?
        }
        ResolvedExprKind::Tuple(elements) => {
            check_tuple_literal(checker, elements, expr.span)?
        }
        ResolvedExprKind::Map(entries) => check_map_literal(checker, entries, expr.span)?,
        ResolvedExprKind::HashSet(elements) => {
            check_hashset_literal(checker, elements, expr.span)?
        }
        ResolvedExprKind::Match { scrutinee, arms } => {
            check_match(checker, scrutinee, arms, expr.span)?
        }
        ResolvedExprKind::VariantConstructor { variant_symbol, args } => {
            check_variant_constructor(checker, *variant_symbol, args, expr.span)?
        }
        ResolvedExprKind::Struct { struct_symbol, fields, base } => {
            check_struct_literal(
                checker,
                *struct_symbol,
                fields,
                base.as_ref().map(|b| b.as_ref()),
                expr.span,
            )?
        }
        ResolvedExprKind::Paren(inner_expr) => {
            let typed_inner = type_check_expression(checker, inner_expr, expected_type)?;
            (TypedExprKind::Paren(Box::new(typed_inner.clone())), typed_inner.ty)
        }
        ResolvedExprKind::Let { pattern, value, type_annotation } => {
            let typed_value = type_check_expression(checker, value, None)?;
            let value_ty = checker.resolve_type(&typed_value.ty);
            let expected_pattern_ty = if let Some(annotation) = type_annotation {
                let annotation_ty = checker.resolve_type_to_ty(annotation)?;
                checker.unify(&value_ty, &annotation_ty)?;
                checker.resolve_type(&annotation_ty)
            } else {
                value_ty
            };
            let typed_pattern = check_pattern(checker, pattern, &expected_pattern_ty)?; // From pattern.rs
            // Placeholder - remove when check_pattern uncommented
            //  let typed_pattern = crate::types::TypedPattern {
            //      kind: crate::types::TypedPatternKind::Wildcard, // Placeholder
            //      ty: expected_pattern_ty.clone(),             // Placeholder
            //      span: pattern.span,               // Placeholder
            //  };
            // Add bindings after successfully checking the pattern
            add_pattern_bindings(checker, &typed_pattern)?; // From control_flow.rs

            let unit_ty = Ty::new(TyKind::Tuple(vec![]));
            (
                TypedExprKind::Let {
                    pattern: typed_pattern,
                    value: Box::new(typed_value),
                },
                unit_ty,
            )
        }
        // Handle other expression kinds or return error
        _ => {
            unimplemented!("Unsupported expression kind: {:?}", expr.kind);
            checker.errors.push(TypeError::UnsupportedExpression { span: expr.span });
            (TypedExprKind::Error, Ty::new(TyKind::Error))
        }
    };

    // --- Post-check Unification & Resolution ---
    // The core logic for handling the expected type is now mostly done *within*
    // the specific check_* functions (like check_binary, check_lambda, check_if, etc.).
    // However, we still need a final unification step, especially for cases like
    // literals or variables where the initial type might be less specific than the expectation.

    let mut current_ty = initial_ty;
    if let Some(expected) = expected_type {
        // Attempt unification. Errors are recorded by checker.unify.
        checker.unify(&current_ty, expected)?;
        // After unification, the 'expected' type (potentially refined by unification)
        // is usually the better representation of the final type, especially if it
        // contained inference variables that are now grounded.
        current_ty = checker.resolve_type(expected);

        // Special handling for integer/float literals based on unified expected type
        // Match on the new specific literal kinds
        match &typed_kind {
            TypedExprKind::IntLiteral { .. } | TypedExprKind::FloatLiteral { .. } => {
                // Pass the typed_kind itself to the coercion helper
                if let Some(prim_ty) = try_coerce_literal_type(&typed_kind, &current_ty) {
                    // Coerce the type if possible
                current_ty = Ty::with_span(TyKind::Primitive(prim_ty), expr.span);
                }
            }
            _ => {} // Do nothing for other kinds
        }
    } else {
        current_ty = checker.resolve_type(&current_ty);
    }

    Ok(TypedExpr { kind: typed_kind, ty: current_ty, span: expr.span })
}

/// Type checks a field access expression (e.g., `object.field`).
#[allow(dead_code)]
fn check_field(
    checker: &mut TypeChecker,
    object: &ResolvedExpr,
    field_name: &str,
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    let typed_object = type_check_expression(checker, object, None)?;
    let resolved_object_ty = checker.resolve_type(&typed_object.ty);

    if let TyKind::Named { name: struct_name, symbol: _, args: instance_type_args } = &resolved_object_ty.kind {
        let struct_def = if let Some(TypeDef::Struct(def)) = checker.type_ctx.get_type(struct_name).cloned() {
            def
        } else {
            return Err(TypeError::NotAStruct {
                found: display_type(&resolved_object_ty),
                span: object.span, 
            });
        };

        let field_def = if let Some(f_def) = struct_def.fields.iter().find(|f| f.name == *field_name) {
            f_def
        } else {
            return Err(TypeError::UnknownStructField {
                field: field_name.to_string(),
                struct_name: struct_name.clone(),
                span,
            });
        };

        let field_symbol = field_def.symbol;

        let mut instance_subst = Substitution::new();
        if struct_def.generic_params.len() != instance_type_args.len() {
            return Err(TypeError::InternalError {
                message: format!(
                    "Internal Error: Generic argument count mismatch during field access for struct '{}'. Expected {}, found {}",
                    struct_name,
                    struct_def.generic_params.len(),
                    instance_type_args.len()
                ),
                span: Some(object.span),
            });
        }
        for (gen_param_def, instance_arg_ty) in struct_def.generic_params.iter().zip(instance_type_args.iter()) {
             instance_subst.insert(gen_param_def.id, instance_arg_ty.clone());
        }

        let instance_field_ty = field_def.ty.apply_subst(&instance_subst);
        let final_field_ty = checker.resolve_type(&instance_field_ty);

        Ok((
            TypedExprKind::Field {
                object: Box::new(typed_object),
                field_symbol,
                field_name: field_name.to_string(),
            },
            final_field_ty,
        ))
        
    } else {
        Err(TypeError::NotAStruct {
            found: display_type(&resolved_object_ty),
            span: object.span, 
        })
    }
}

/// Type checks a literal expression.
#[allow(dead_code)]
pub(crate) fn check_literal(
    checker: &mut TypeChecker,
    literal: &AstLiteral,
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    let (kind, ty_kind) = match literal {
        AstLiteral::Int { value, suffix } => (
            TypedExprKind::IntLiteral { value: *value, suffix: suffix.clone() },
            match suffix.as_deref() {
                Some("i8") => TyKind::Primitive(PrimitiveType::I8),
                Some("i16") => TyKind::Primitive(PrimitiveType::I16),
                Some("i32") => TyKind::Primitive(PrimitiveType::I32),
                Some("i64") => TyKind::Primitive(PrimitiveType::I64),
                Some("i128") => TyKind::Primitive(PrimitiveType::I128),
                Some("u8") => TyKind::Primitive(PrimitiveType::U8),
                Some("u16") => TyKind::Primitive(PrimitiveType::U16),
                Some("u32") => TyKind::Primitive(PrimitiveType::U32),
                Some("u64") => TyKind::Primitive(PrimitiveType::U64),
                Some("u128") => TyKind::Primitive(PrimitiveType::U128),
                Some(other) => {
                    // Warning was already issued by resolver
                    // Use generic literal type
                    TyKind::Primitive(PrimitiveType::IntegerLiteral)
                }
                None => TyKind::Primitive(PrimitiveType::IntegerLiteral),
            }
        ),
        AstLiteral::Float { value, suffix } => (
            TypedExprKind::FloatLiteral { value: *value, suffix: suffix.clone() },
            match suffix.as_deref() {
                Some("f32") => TyKind::Primitive(PrimitiveType::F32),
                Some("f64") => TyKind::Primitive(PrimitiveType::F64),
                Some(other) => {
                    // Warning already issued by resolver
                    TyKind::Primitive(PrimitiveType::FloatLiteral)
                }
                None => TyKind::Primitive(PrimitiveType::FloatLiteral),
            }
        ),
        AstLiteral::String(s) => (TypedExprKind::StringLiteral(s.clone()), TyKind::Primitive(PrimitiveType::String)),
        AstLiteral::Char(c) => (TypedExprKind::CharLiteral(*c), TyKind::Primitive(PrimitiveType::Char)),
        AstLiteral::Bool(b) => (TypedExprKind::BoolLiteral(*b), TyKind::Primitive(PrimitiveType::Bool)),
    };

    Ok((kind, Ty::with_span(ty_kind, span)))
}

/// Attempts to coerce a literal type (represented by its original AST kind)
/// to an expected concrete primitive type.
/// This is typically used after unification to refine `{integer}` or `{float}` types.
#[allow(dead_code)]
fn try_coerce_literal_type(literal_kind: &TypedExprKind, expected_ty: &Ty) -> Option<PrimitiveType> {
    let expected_prim = match &expected_ty.kind {
        TyKind::Primitive(prim) => prim,
        _ => return None, // Can only coerce to primitive
    };

    match literal_kind {
        TypedExprKind::IntLiteral { .. } => {
            // Can coerce {integer} to any concrete integer type
            if expected_prim.is_integer() && *expected_prim != PrimitiveType::IntegerLiteral {
                Some(*expected_prim)
            } else {
                None
            }
        }
        TypedExprKind::FloatLiteral { .. } => {
            // Can coerce {float} to f32 or f64
            if expected_prim.is_float() && *expected_prim != PrimitiveType::FloatLiteral {
                Some(*expected_prim)
            } else {
                None
            }
        }
        // Other literals already have concrete types
        _ => None,
    }
}

/// Type checks a path expression (identifier reference).
#[allow(dead_code)]
fn check_path(
    checker: &mut TypeChecker,
    symbol: Symbol, // Input symbol is already resolved (types::Symbol)
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    // Check local environment first
    let local_name_opt = checker.type_ctx.get_name_for_symbol(&symbol).cloned();
    if let Some(ref name) = local_name_opt {
        if let Some(ty) = checker._type_env.get(name) {
            return Ok((TypedExprKind::Variable { symbol, name: name.clone() }, ty.clone()));
        }
    }

    // If not in local env, get definition kind and name
    let (resolved_kind, name) = match checker.get_definition_info(symbol) {
        Some((kind, n, _span)) => (kind, n),
        None => {
            let name_or_sym = local_name_opt.unwrap_or_else(|| format!("symbol_{}", symbol.id()));
            return Err(TypeError::UnknownIdentifier { name: name_or_sym, span });
        }
    };

    // Now handle based on kind, allowing mutable borrows if needed

    match resolved_kind {
        DefinitionKind::Function => {
            // Re-query type context immutably
            if let Some(TypeDef::Function(sig)) = checker.type_ctx.get_type_by_symbol(&symbol).cloned() {
                 let mut func_ty = Ty::new(TyKind::Function(
                     sig.params.iter().map(|p| p.ty.clone()).collect(),
                     Arc::new(sig.return_type.clone())
                 ));
                 func_ty.span = Some(span);
                 // Instantiate if the function is generic (requires mutable borrow for fresh vars)
                 let final_func_ty = if !sig.generic_params.is_empty() {
                     let (instantiated_ty, _generic_map) = instantiate(
                         checker, // Mutable borrow here
                         &func_ty,
                         span,
                         &sig.generic_params
                     )?;
                     instantiated_ty
                 } else {
                     func_ty
                 };
                 Ok((TypedExprKind::Variable { symbol, name }, final_func_ty))
            } else {
                 Err(TypeError::InternalError { message: format!("Function symbol {:?} has no signature in type context", symbol), span: Some(span) })
            }
        },
        DefinitionKind::Struct => {
            // Re-query type context immutably
            if let Some(TypeDef::Struct(s_def)) = checker.type_ctx.get_type_by_symbol(&symbol).cloned() {
                if s_def.fields.is_empty() {
                     // Unit struct: Instantiate type (requires mutable borrow)
                     let base_ty = Ty::with_span(
                         TyKind::Named {
                             name: name.clone(),
                             symbol: Some(symbol),
                             args: vec![]
                         },
                         span
                     );
                     let (instantiated_ty, _) = instantiate(checker, &base_ty, span, &s_def.generic_params)?;
                     let final_ty = checker.resolve_type(&instantiated_ty);
                     Ok((TypedExprKind::Variable { symbol, name }, final_ty))
                } else {
                     Err(TypeError::NotAValue { name, span })
                }
            } else {
                 Err(TypeError::InternalError { message: format!("Struct symbol {:?} has no definition in type context", symbol), span: Some(span) })
            }
        },
        DefinitionKind::Enum => {
            // Disallow using enum name directly as value
            Err(TypeError::NotAValue { name, span })
        },
        DefinitionKind::EnumVariant => {
            // Determine constructor type (requires mutable borrow)
            let constructor_ty = determine_variant_constructor_type(checker, symbol, span)?;
            Ok((TypedExprKind::Variable { symbol, name }, constructor_ty))
        },
        DefinitionKind::Module | DefinitionKind::Trait | DefinitionKind::Impl => {
            Err(TypeError::NotAValue { name, span })
        },
        DefinitionKind::AssociatedType => {
             Err(TypeError::NotAValue { name, span })
        }
    }
}

// Keep empty impl block for potential future helpers
#[allow(dead_code)]
impl TypeChecker<'_> {
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{checker::TypeChecker, context::*, types::*, TypeDatabase, error::*, PrimitiveType};
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

    fn ty_prim(prim: PrimitiveType) -> Ty { Ty::with_span(TyKind::Primitive(prim), dummy_span()) }
    fn ty_var(id: u32) -> Ty { Ty::with_span(TyKind::Var(TypeId(id)), dummy_span()) }
    fn ty_named(name: &str, symbol: Option<Symbol>, args: Vec<Ty>) -> Ty { Ty::with_span(TyKind::Named { name: name.to_string(), symbol, args }, dummy_span()) }
    fn ty_tuple(tys: Vec<Ty>) -> Ty { Ty::with_span(TyKind::Tuple(tys), dummy_span()) }
    fn ty_func(params: Vec<Ty>, ret: Ty) -> Ty { Ty::with_span(TyKind::Function(params, Arc::new(ret)), dummy_span()) }

    fn resolved_lit_int(val: i128) -> ResolvedExpr { ResolvedExpr { kind: ResolvedExprKind::Literal(AstLiteral::Int { value: val, suffix: None }), span: dummy_span(), resolved_type: ResolvedType::IntegerLiteral } }
    fn resolved_lit_float(val: f64) -> ResolvedExpr { ResolvedExpr { kind: ResolvedExprKind::Literal(AstLiteral::Float { value: val, suffix: None }), span: dummy_span(), resolved_type: ResolvedType::FloatLiteral } }
    fn resolved_lit_bool(val: bool) -> ResolvedExpr { ResolvedExpr { kind: ResolvedExprKind::Literal(AstLiteral::Bool(val)), span: dummy_span(), resolved_type: ResolvedType::Primitive(ResolvePrimitiveType::Bool) } }
    fn resolved_path(sym: Symbol, rt: ResolvedType) -> ResolvedExpr { ResolvedExpr { kind: ResolvedExprKind::Path(sym), span: dummy_span(), resolved_type: rt } }
    fn resolved_ident(name: &str, sym: Symbol, rt: ResolvedType) -> ResolvedExpr { ResolvedExpr { kind: ResolvedExprKind::Variable{ binding_symbol: sym, name: name.to_string() }, span: dummy_span(), resolved_type: rt } }

    fn add_function_sig(checker: &mut TypeChecker, name: &str, sym: Symbol, params: Vec<(&str, Ty)>, ret: Ty, generics: Vec<(String, TypeId)>) {
        let param_types = params.into_iter().map(|(n, t)| ParamType { name: n.to_string(), ty: t, span: dummy_span() }).collect();
        let generic_params = generics.into_iter().map(|(n, id)| GenericParamDef { name: n, symbol: Symbol::fresh(), id, bounds: vec![], span: dummy_span() }).collect();
        let sig = FunctionSignature { name: name.to_string(), self_param: None, generic_params, params: param_types, return_type: ret, span: dummy_span() };
        checker.type_ctx.add_type(sym, name.to_string(), TypeDef::Function(sig));
    }
    fn add_struct_def(checker: &mut TypeChecker, name: &str, sym: Symbol, fields: Vec<(String, Symbol, Ty)>, generics: Vec<(String, TypeId)>) {
        let fields_def = fields.into_iter().map(|(n, fs, t)| Field { name: n, symbol: fs, ty: t, span: dummy_span() }).collect();
        let generic_params = generics.into_iter().map(|(n, id)| GenericParamDef { name: n, symbol: Symbol::fresh(), id, bounds: vec![], span: dummy_span() }).collect();
        let struct_def = StructDef { name: name.to_string(), symbol: sym, generic_params, fields: fields_def, span: dummy_span() };
        checker.type_ctx.add_type(sym, name.to_string(), TypeDef::Struct(struct_def));
    }
    fn add_enum_def(checker: &mut TypeChecker, name: &str, sym: Symbol, variants: Vec<(String, Symbol, Vec<(String, Symbol, Ty)>)>) {
        let enum_variants = variants.into_iter().map(|(v_name, v_sym, v_fields)| {
            let fields = v_fields.into_iter().map(|(f_name, f_sym, f_ty)| Field { name: f_name, symbol: f_sym, ty: f_ty, span: dummy_span() }).collect();
            EnumVariant { name: v_name, symbol: v_sym, fields, span: dummy_span() }
        }).collect();
        let enum_def = EnumDef { name: name.to_string(), symbol: sym, generic_params: vec![], variants: enum_variants, span: dummy_span() };
        checker.type_ctx.add_type(sym, name.to_string(), TypeDef::Enum(enum_def));
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

    // --- Literal Tests ---
    #[test]
    fn test_check_literal_int_coerce() {
        let mut checker = setup_checker();
        let expr = resolved_lit_int(123);
        let expected = ty_prim(PrimitiveType::I64);
        let typed_expr = type_check_expression(&mut checker, &expr, Some(&expected)).unwrap();
        assert!(matches!(typed_expr.kind, TypedExprKind::IntLiteral { value: 123, .. }));
        assert_eq!(typed_expr.ty, expected); // Should coerce to expected i64
    }

    #[test]
    fn test_check_literal_float_coerce() {
        let mut checker = setup_checker();
        let expr = resolved_lit_float(1.23);
        let expected = ty_prim(PrimitiveType::F32);
        let typed_expr = type_check_expression(&mut checker, &expr, Some(&expected)).unwrap();
        assert!(matches!(typed_expr.kind, TypedExprKind::FloatLiteral { .. }));
        assert_eq!(typed_expr.ty, expected); // Should coerce to expected f32
    }

    #[test]
    fn test_check_literal_no_coerce_needed() {
        let mut checker = setup_checker();
        let expr = resolved_lit_bool(true);
        let expected = ty_prim(PrimitiveType::Bool);
        let typed_expr = type_check_expression(&mut checker, &expr, Some(&expected)).unwrap();
        assert!(matches!(typed_expr.kind, TypedExprKind::BoolLiteral(true)));
        assert_eq!(typed_expr.ty, expected);
    }

    #[test]
    fn test_check_literal_coerce_fail() {
        let mut checker = setup_checker();
        let expr = resolved_lit_int(123);
        let expected = ty_prim(PrimitiveType::Bool);
        let result = type_check_expression(&mut checker, &expr, Some(&expected));
        assert!(result.is_err()); // Error during unification
        assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
    }

    // --- Path Tests ---
    #[test]
    fn test_check_path_local_variable() {
        let mut checker = setup_checker();
        let var_sym = Symbol::new(1);
        let var_name = "my_var";
        let var_ty = ty_prim(PrimitiveType::String);
        // Add variable using clone-on-write pattern for Arc<TypeEnvironment>
        if let Some(env) = Arc::get_mut(&mut checker._type_env) {
            env.add(var_name.to_string(), var_ty.clone());
        } else {
            let mut cloned_env = (*checker._type_env).clone();
            cloned_env.add(var_name.to_string(), var_ty.clone());
            checker._type_env = Arc::new(cloned_env);
        }
        let expr = resolved_ident(var_name, var_sym, ResolvedType::Unknown); // Variable usage

        let typed_expr = type_check_expression(&mut checker, &expr, None).unwrap();
        match typed_expr.kind {
            TypedExprKind::Variable { symbol, name } => {
                assert_eq!(symbol, var_sym);
                assert_eq!(name, var_name);
            }
            _ => panic!("Expected Variable kind"),
        }
        assert_eq!(typed_expr.ty, var_ty);
    }

    #[test]
    fn test_check_path_function_non_generic() {
        let mut checker = setup_checker();
        let func_sym = Symbol::new(5);
        let func_ty = ty_func(vec![ty_prim(PrimitiveType::I32)], ty_prim(PrimitiveType::Bool));
        add_function_sig(&mut checker, "is_positive", func_sym, vec![("n", ty_prim(PrimitiveType::I32))], ty_prim(PrimitiveType::Bool), vec![]);
        let expr = resolved_path(func_sym, ResolvedType::Unknown);

        let typed_expr = type_check_expression(&mut checker, &expr, None).unwrap();
        match typed_expr.kind {
            TypedExprKind::Variable { symbol, name } => { // Functions are treated as variables when referenced by path
                assert_eq!(symbol, func_sym);
                assert_eq!(name, "is_positive");
            }
            _ => panic!("Expected Variable kind"),
        }
        assert_eq!(typed_expr.ty.kind, func_ty.kind);
    }

    #[test]
    fn test_check_path_function_generic() {
        let mut checker = setup_checker();
        let func_sym = Symbol::new(6);
        let gen_id = TypeId(100);
        add_function_sig(&mut checker, "identity", func_sym, vec![("x", ty_var(gen_id.0))], ty_var(gen_id.0), vec![("T".to_string(), gen_id)]);
        let expr = resolved_path(func_sym, ResolvedType::Unknown);

        let typed_expr = type_check_expression(&mut checker, &expr, None).unwrap();
        // Expected type: fn(t_fresh) -> t_fresh where t_fresh is the instantiation of T
        match typed_expr.ty.kind {
            TyKind::Function(params, ret) => {
                assert_eq!(params.len(), 1);
                assert!(matches!(params[0].kind, TyKind::Var(_)));
                assert!(matches!(ret.kind, TyKind::Var(_)));
                assert_eq!(params[0].kind, ret.kind); // Should be the same fresh variable
                assert_ne!(params[0].kind, TyKind::Var(gen_id)); // Should not be the original generic ID
            }
            _ => panic!("Expected Function type, got {:?}", typed_expr.ty.kind),
        }
    }

    #[test]
    fn test_check_path_struct_unit_constructor() {
         let mut checker = setup_checker();
         let struct_sym = Symbol::new(7);
         add_struct_def(&mut checker, "MyUnit", struct_sym, vec![], vec![]);
         let expr = resolved_path(struct_sym, ResolvedType::Unknown);

         let typed_expr = type_check_expression(&mut checker, &expr, None).unwrap();
         // Referencing a unit struct name gives the type itself
         match typed_expr.kind {
             TypedExprKind::Variable { symbol, name } => {
                 assert_eq!(symbol, struct_sym);
                 assert_eq!(name, "MyUnit");
             }
             _ => panic!("Expected Variable kind"),
         }
          match typed_expr.ty.kind {
             TyKind::Named { name, symbol, args } => {
                 assert_eq!(name, "MyUnit");
                 assert_eq!(symbol, Some(struct_sym));
                 assert!(args.is_empty());
             }
             _ => panic!("Expected Named type"),
         }
    }

    #[test]
    fn test_check_path_struct_non_unit_constructor_error() {
         let mut checker = setup_checker();
         let struct_sym = Symbol::new(8);
         add_struct_def(&mut checker, "Point", struct_sym, vec![("x".to_string(), Symbol::fresh(), ty_prim(PrimitiveType::I32))], vec![]);
         let expr = resolved_path(struct_sym, ResolvedType::Unknown);

         let result = type_check_expression(&mut checker, &expr, None);
         assert!(result.is_err());
         assert!(matches!(result.err().unwrap(), TypeError::NotAValue { name, .. } if name == "Point"));
    }

    #[test]
    fn test_check_path_enum_variant_constructor() {
        let mut checker = setup_checker();
        let enum_sym = Symbol::new(30);
        let var_sym = Symbol::new(31);
        add_enum_def(&mut checker, "Status", enum_sym, vec![("Ok".to_string(), var_sym, vec![])]);
        let expr = resolved_path(var_sym, ResolvedType::Unknown);

        let typed_expr = type_check_expression(&mut checker, &expr, None).unwrap();
        // Expected type: fn() -> Status
         match typed_expr.ty.kind {
            TyKind::Function(params, ret) => {
                assert!(params.is_empty());
                assert!(matches!(ret.kind, TyKind::Named { ref name, .. } if name == "Status"));
            }
            _ => panic!("Expected Function type"),
        }
    }

    #[test]
    fn test_check_path_enum_name_error() {
        let mut checker = setup_checker();
        let enum_sym = Symbol::new(30);
        let var_sym = Symbol::new(31);
        add_enum_def(&mut checker, "Status", enum_sym, vec![("Ok".to_string(), var_sym, vec![])]);
        let expr = resolved_path(enum_sym, ResolvedType::Unknown); // Path to enum itself

        let result = type_check_expression(&mut checker, &expr, None);
        assert!(result.is_err());
        assert!(matches!(result.err().unwrap(), TypeError::NotAValue { name, .. } if name == "Status"));
    }

    // --- Field Tests ---
    #[test]
    fn test_check_field_simple() {
        let mut checker = setup_checker();
        let struct_sym = Symbol::new(1);
        let field_sym = Symbol::new(2);
        let obj_sym = Symbol::new(100);
        let field_ty = ty_prim(PrimitiveType::F64);
        add_struct_def(&mut checker, "MyData", struct_sym, vec![("value".to_string(), field_sym, field_ty.clone())], vec![]);
        let obj_ty = ty_named("MyData", Some(struct_sym), vec![]);
        // Add variable using clone-on-write pattern for Arc<TypeEnvironment>
        if let Some(env) = Arc::get_mut(&mut checker._type_env) {
            env.add("d".to_string(), obj_ty.clone());
        } else {
            let mut cloned_env = (*checker._type_env).clone();
            cloned_env.add("d".to_string(), obj_ty.clone());
            checker._type_env = Arc::new(cloned_env);
        }

        let obj_expr = resolved_ident("d", obj_sym, ResolvedType::Primitive(ResolvePrimitiveType::I32));
        let field_expr = ResolvedExpr { kind: ResolvedExprKind::Field { object: Box::new(obj_expr), field_name: "value".to_string() }, span: dummy_span(), resolved_type: ResolvedType::Unknown };

        let typed_expr = type_check_expression(&mut checker, &field_expr, None).unwrap();
        match typed_expr.kind {
            TypedExprKind::Field { object, field_symbol, field_name } => {
                assert!(matches!(object.kind, TypedExprKind::Variable { name, .. } if name == "d"));
                assert_eq!(field_symbol, field_sym);
                assert_eq!(field_name, "value");
            }
            _ => panic!("Expected Field kind"),
        }
        assert_eq!(typed_expr.ty, field_ty);
    }

    #[test]
    fn test_check_field_unknown() {
        let mut checker = setup_checker();
        let struct_sym = Symbol::new(1);
        let obj_sym = Symbol::new(100);
        add_struct_def(&mut checker, "MyData", struct_sym, vec![], vec![]); // No fields
        let obj_ty = ty_named("MyData", Some(struct_sym), vec![]);
        // Add variable using clone-on-write pattern for Arc<TypeEnvironment>
        if let Some(env) = Arc::get_mut(&mut checker._type_env) {
            env.add("d".to_string(), obj_ty.clone());
        } else {
            let mut cloned_env = (*checker._type_env).clone();
            cloned_env.add("d".to_string(), obj_ty.clone());
            checker._type_env = Arc::new(cloned_env);
        }

        let obj_expr = resolved_ident("d", obj_sym, ResolvedType::Primitive(ResolvePrimitiveType::I32));
        let field_expr = ResolvedExpr { kind: ResolvedExprKind::Field { object: Box::new(obj_expr), field_name: "value".to_string() }, span: dummy_span(), resolved_type: ResolvedType::Unknown };

        let result = type_check_expression(&mut checker, &field_expr, None);
        assert!(result.is_err());
        assert!(matches!(result.err().unwrap(), TypeError::UnknownStructField { field, .. } if field == "value"));
    }

    #[test]
    fn test_check_field_on_non_struct() {
        let mut checker = setup_checker();
        let obj_sym = Symbol::new(100);
        let obj_ty = ty_prim(PrimitiveType::I32);
        // Add variable using clone-on-write pattern for Arc<TypeEnvironment>
        if let Some(env) = Arc::get_mut(&mut checker._type_env) {
            env.add("d".to_string(), obj_ty.clone());
        } else {
            let mut cloned_env = (*checker._type_env).clone();
            cloned_env.add("d".to_string(), obj_ty.clone());
            checker._type_env = Arc::new(cloned_env);
        }

        let obj_expr = resolved_ident("d", obj_sym, ResolvedType::Primitive(ResolvePrimitiveType::I32));
        let field_expr = ResolvedExpr { kind: ResolvedExprKind::Field { object: Box::new(obj_expr), field_name: "value".to_string() }, span: dummy_span(), resolved_type: ResolvedType::Unknown };

        let result = type_check_expression(&mut checker, &field_expr, None);
        assert!(result.is_err());
        assert!(matches!(result.err().unwrap(), TypeError::NotAStruct { .. }));
    }
} 