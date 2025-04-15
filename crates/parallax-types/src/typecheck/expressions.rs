// Placeholder for expression type checking logic
use parallax_resolve::types::{
    ResolvedExpr, ResolvedExprKind, ResolvedType
};
use parallax_syntax::ast::common::Literal as AstLiteral;
use miette::SourceSpan;
use std::sync::Arc;
use parallax_resolve::{definitions::DefinitionKind, types::Symbol};

use crate::{
    error::{TypeError, TypeResult},
    inference::Substitution,
    typecheck::TypeChecker,
    types::{
        Ty, TyKind, TypeDef, TypedExpr, TypedExprKind,
        PrimitiveType,
    },
};
// Import functions from sibling modules
use super::control_flow::{check_if, check_block, check_match, add_pattern_bindings};
use super::invocations::{check_invocation, check_lambda};
use super::operators::{check_binary, check_unary};
use super::aggregates::{
    check_array_literal, check_tuple_literal, check_map_literal,
    check_hashset_literal, check_variant_constructor, check_struct_literal,
};
use super::patterns::check_pattern;
// Import instantiate helper
use super::helpers::instantiate;

/// Type check an expression and return a typed expression.
pub(crate) fn type_check_expression(
    checker: &mut TypeChecker,
    expr: &ResolvedExpr,
    expected_type: Option<&Ty>,
) -> TypeResult<TypedExpr> {
    handle_expression_match(checker, expr, expected_type)
}

/// Helper function that dispatches expression checking based on kind.
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
            // Type check left and right operands first
            let typed_left = type_check_expression(checker, left, None)?;
            let typed_right = type_check_expression(checker, right, None)?;
            // Now call check_binary with TypedExpr
            check_binary(checker, &typed_left, op, &typed_right, expr.span)?
        }
        ResolvedExprKind::Unary { op, expr: operand } => {
            // Type check the operand first
            let typed_operand = type_check_expression(checker, operand, None)?;
            // Now call check_unary with TypedExpr
            check_unary(checker, op, &typed_operand, expr.span)?
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
                // If func_symbol is None, it means the resolver couldn't identify the function.
                // This should ideally be handled by check_invocation, but we can error here.
                checker.errors.push(TypeError::NotAFunction {
                    found: "<unresolved function>".to_string(),
                    span: expr.span, // Use the call expression's span
                });
                (TypedExprKind::Error, Ty::new(TyKind::Error))
            }
        }
        ResolvedExprKind::Lambda { params, body } => {
            check_lambda(checker, params, body, expr.span)?
        }
        ResolvedExprKind::Literal(lit) => check_literal(checker, lit, expr.span)?,
        ResolvedExprKind::Path(symbol) => {
            check_path(checker, *symbol, expr.span)?
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
            let typed_pattern = check_pattern(checker, pattern, &expected_pattern_ty)?;
            // TODO: Add pattern bindings to env (needs careful scope management)
            // Add bindings after successfully checking the pattern
            add_pattern_bindings(checker, &typed_pattern)?;

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
             println!("Type checking not implemented for: {:?}", expr.kind);
            checker.errors.push(TypeError::UnsupportedExpression { span: expr.span });
            (TypedExprKind::Error, Ty::new(TyKind::Error))
        }
    };

    // Unify the initial type with the expected type if provided.
    let mut current_ty = initial_ty;
    if let Some(expected) = expected_type {
        checker.unify(&current_ty, expected)?;
        // Resolve again after unification, potentially using the expected type structure
        current_ty = checker.resolve_type(expected);
    } else {
        // Resolve if no specific expectation
        current_ty = checker.resolve_type(&current_ty);
    }

    Ok(TypedExpr { kind: typed_kind, ty: current_ty, span: expr.span })
}

/// Type checks a field access expression (e.g., `object.field`).
fn check_field(
    checker: &mut TypeChecker,
    object: &ResolvedExpr,
    field_name: &str,
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    let typed_object = type_check_expression(checker, object, None)?;
    let resolved_object_ty = checker.resolve_type(&typed_object.ty);

    if let TyKind::Named { name: struct_name, args: instance_type_args } = &resolved_object_ty.kind {
        let struct_def = if let Some(TypeDef::Struct(def)) = checker.type_ctx.get_type(struct_name).cloned() {
            def
        } else {
            return Err(TypeError::NotAStruct {
                found: crate::error::display_type(&resolved_object_ty),
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
            found: crate::error::display_type(&resolved_object_ty),
            span: object.span, 
        })
    }
}

/// Type checks a literal expression. (Used by check_pattern in patterns.rs)
pub(crate) fn check_literal(
    _checker: &mut TypeChecker,
    literal: &AstLiteral, // Use aliased import
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    let (ty_kind, lit_type) = match literal {
        AstLiteral::Int(_) => (
            TypedExprKind::Literal(literal.clone()),
            Ty::with_span(TyKind::Primitive(PrimitiveType::I32), span), // Default to I32
        ),
        AstLiteral::Float(_) => (
            TypedExprKind::Literal(literal.clone()),
            Ty::with_span(TyKind::Primitive(PrimitiveType::F64), span), // Default to F64
        ),
        AstLiteral::String(_) => (
            TypedExprKind::Literal(literal.clone()),
            Ty::with_span(TyKind::Primitive(PrimitiveType::String), span),
        ),
        AstLiteral::Bool(_) => (
            TypedExprKind::Literal(literal.clone()),
            Ty::with_span(TyKind::Primitive(PrimitiveType::Bool), span),
        ),
        AstLiteral::Char(_) => (
            TypedExprKind::Literal(literal.clone()),
            Ty::with_span(TyKind::Primitive(PrimitiveType::Char), span),
        ),
    };

    Ok((ty_kind, lit_type))
}

/// Type checks a path expression (identifier reference).
fn check_path(
    checker: &mut TypeChecker,
    symbol: Symbol, // Input symbol is already resolved (types::Symbol)
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    // Try environment first (local variables)
    // TODO: Enhance env lookup to use Symbol directly instead of name string.
    let temp_name_for_env_lookup = checker.type_ctx.get_name_for_symbol(&symbol).cloned().unwrap_or_default();
    if let Some(ty) = checker.type_env.get(&temp_name_for_env_lookup) {
         let local_name = checker.type_ctx.get_name_for_symbol(&symbol).cloned().unwrap_or_else(|| "unknown_local".to_string());
         return Ok((TypedExprKind::Variable { symbol, name: local_name }, ty.clone()));
    }

    // Try definitions (functions, constants, types, etc.)
    let (resolved_kind, name, _def_span) = match checker.get_definition_info(symbol) {
        Some(info) => info,
        None => {
             // If not found in env or global defs, it's an error.
             return Err(TypeError::UnknownIdentifier {
                 name: checker.type_ctx.get_name_for_symbol(&symbol).cloned().unwrap_or_else(|| format!("symbol_{}", symbol.id())),
                 span,
             });
        }
    };

    match resolved_kind {
        DefinitionKind::Function => {
            if let Some(TypeDef::Function(sig)) = checker.type_ctx.get_type_by_symbol(&symbol).cloned() {
                 let mut func_ty = Ty::new(TyKind::Function(
                     sig.params.iter().map(|p| p.ty.clone()).collect(),
                     Arc::new(sig.return_type.clone())
                 ));
                 func_ty.span = Some(span);
                 // Instantiate if the function is generic
                 let final_func_ty = if !sig.generic_params.is_empty() {
                     let (instantiated_ty, _generic_map) = instantiate(
                         checker,
                         &func_ty, // Use the base function type
                         span,
                         &sig.generic_params
                     )?;
                     instantiated_ty
                 } else {
                     func_ty
                 };
                 return Ok((TypedExprKind::Variable { symbol, name }, final_func_ty));
            } else {
                 // Function definition exists but signature wasn't stored? Internal error.
                 return Err(TypeError::InternalError { message: format!("Function symbol {:?} has no signature in type context", symbol), span: Some(span) });
            }
        },
        DefinitionKind::Struct | DefinitionKind::Enum => {
             // Referring to the type itself (e.g., `MyStruct` used as a value).
             // This is generally an error unless it's a unit struct/enum constructor?
             // For now, treat as error. Needs refinement for unit constructors.
              return Err(TypeError::NotAValue { name, span });
             // If unit constructors were allowed as values:
             // let type_ty = checker.resolve_type_to_ty(&ResolvedType::UserDefined { symbol: convert_symbol_back(symbol), type_args: None })?;
             // return Ok((TypedExprKind::Variable { symbol, name }, type_ty));
        },
        DefinitionKind::EnumVariant => {
             // Referring to an enum variant constructor (e.g., `Option::Some`).
             // Get its constructor type (which is a function type).
             let constructor_ty = checker.determine_variant_constructor_type(symbol)?;
             return Ok((TypedExprKind::Variable { symbol, name }, constructor_ty));
        },
        DefinitionKind::Module | DefinitionKind::Trait | DefinitionKind::Impl => {
             // These aren't values.
             return Err(TypeError::NotAValue { name, span });
         }
    }
}

// Keep empty impl block for potential future helpers
impl TypeChecker<'_> {
}