use std::sync::Arc;
use miette::SourceSpan;
use parallax_resolve::types::{ResolvedExpr, ResolvedExprKind, ResolvedPattern};

use crate::{
    error::{TypeError, TypeResult},
    inference::TypeEnvironment,
    typecheck::{
        expressions::type_check_expression, // Assuming check_pattern moves to patterns.rs later
        TypeChecker,
        patterns::check_pattern,
    },
    types::{PrimitiveType, Ty, TyKind, TypedExpr, TypedExprKind, TypedMatchArm, TypedPattern, TypedPatternKind},
};

// Function to check 'if' expressions
pub(crate) fn check_if(
    checker: &mut TypeChecker,
    condition: &ResolvedExpr,
    then_branch: &ResolvedExpr,
    else_branch: Option<&ResolvedExpr>,
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    // Type check the condition
    let typed_condition = type_check_expression(checker, condition, None)?;
    let condition_ty = checker.resolve_type(&typed_condition.ty);

    // Ensure the condition is a boolean
    if !matches!(condition_ty.kind, TyKind::Primitive(PrimitiveType::Bool)) {
        return Err(TypeError::TypeMismatch {
            expected: "boolean".to_string(),
            found: crate::error::display_type(&condition_ty),
            span: typed_condition.span,
        });
    }

    // Type check the then branch
    let typed_then = type_check_expression(checker, then_branch, None)?;
    let then_ty = checker.resolve_type(&typed_then.ty);

    // Type check the else branch if it exists
    if let Some(else_expr) = else_branch {
        let typed_else = type_check_expression(checker, else_expr, Some(&then_ty))?; // Expect then_ty
        // Unification happens in the recursive call
        Ok((
            TypedExprKind::If {
                condition: Box::new(typed_condition),
                then_branch: Box::new(typed_then),
                else_branch: Some(Box::new(typed_else)),
            },
            checker.resolve_type(&then_ty), // Type is the unified type of the branches
        ))
    } else {
        // If there is no else branch, the type must be Unit ()
        checker.unify(&then_ty, &Ty::new(TyKind::Tuple(vec![])))?;
        Ok((
            TypedExprKind::If {
                condition: Box::new(typed_condition),
                then_branch: Box::new(typed_then),
                else_branch: None,
            },
            Ty::new(TyKind::Tuple(vec![])), // Result is Unit
        ))
    }
}

// Function to check block expressions
pub(crate) fn check_block(
    checker: &mut TypeChecker,
    exprs: &[ResolvedExpr],
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    println!("Checking block with {} statements", exprs.len());

    let original_env = checker.type_env.clone();
    // Correctly create a mutable binding for block_env
    let mut block_env_inner = TypeEnvironment::with_parent(original_env.clone());
    // Shadow checker.type_env correctly
    checker.type_env = Arc::new(block_env_inner); // Assign the new Arc


    let mut typed_exprs = Vec::with_capacity(exprs.len());
    let mut last_ty = Ty::new(TyKind::Tuple(vec![])); // Default to Unit

    for (i, expr) in exprs.iter().enumerate() {
        let is_last = i == exprs.len() - 1;

        // Check for let bindings specially to modify the block's env
        if let ResolvedExprKind::Let { pattern, value, type_annotation } = &expr.kind {
             // --- Let Binding Handling ---
             // 1. Resolve optional type annotation
             let expected_val_ty = if let Some(annot) = type_annotation {
                 Some(checker.resolve_type_to_ty(annot)?)
             } else {
                 None
             };

             // 2. Check value expression, passing annotation as expectation
             let typed_value = type_check_expression(checker, value, expected_val_ty.as_ref())?;
             let value_ty = checker.resolve_type(&typed_value.ty);

             // 3. Check pattern against the value's type, adding bindings to the block's env
             // Important: check_pattern modifies the *current* checker.type_env
             let typed_pattern = check_pattern(checker, pattern, &value_ty)?; // Assuming check_pattern is accessible

             // Add bindings from pattern to the current environment
             add_pattern_bindings(checker, &typed_pattern)?;


             // 4. Let expression itself results in a TypedExpr of type Unit
             let unit_ty = Ty::new(TyKind::Tuple(vec![]));
             let typed_let_expr = TypedExpr {
                 kind: TypedExprKind::Let {
                     pattern: typed_pattern,
                     value: Box::new(typed_value),
                 },
                 ty: unit_ty.clone(),
                 span: expr.span,
             };
             typed_exprs.push(typed_let_expr);

             // If the let is the last expression, the block's type is Unit
             if is_last {
                last_ty = unit_ty;
             }
             // --- End Let Binding Handling ---
        } else {
            // Handle regular expressions within the block
            let typed_expr = type_check_expression(checker, expr, None)?;
            if is_last {
                last_ty = typed_expr.ty.clone();
            }
            typed_exprs.push(typed_expr);
        }
    }

    // Restore the original environment before returning
    checker.type_env = original_env;

    let final_block_ty = checker.resolve_type(&last_ty);
    Ok((TypedExprKind::Block(typed_exprs), final_block_ty))
}


/// Helper function to add bindings from a pattern to the current environment.
/// This needs mutable access to the checker's environment.
pub(crate) fn add_pattern_bindings(checker: &mut TypeChecker, pattern: &TypedPattern) -> TypeResult<()> {
    match &pattern.kind {
        TypedPatternKind::Identifier { symbol, name } => {
            // Check if Arc can be mutably borrowed. If not, clone and replace.
            if let Some(current_env) = Arc::get_mut(&mut checker.type_env) {
                current_env.add(name.clone(), pattern.ty.clone());
                println!("Bound pattern variable '{}' ({:?}) to type {:?}", name, symbol, pattern.ty);
            } else {
                // This case means the environment Arc is shared elsewhere, which might indicate
                // an issue with how scopes are managed, or it's expected (e.g., closures).
                // We clone the environment to modify it.
                let mut cloned_env = (*checker.type_env).clone();
                cloned_env.add(name.clone(), pattern.ty.clone());
                checker.type_env = Arc::new(cloned_env);
                 println!("Bound pattern variable '{}' ({:?}) to type {:?} (env cloned)", name, symbol, pattern.ty);
                // Potentially add a warning here if cloning the env is unexpected.
                // checker.warnings.push(format!("Cloned environment to add binding for '{}'", name));
            }
        }
        TypedPatternKind::Tuple(elements) | TypedPatternKind::Array(elements) => {
            for elem_pat in elements {
                add_pattern_bindings(checker, elem_pat)?;
            }
        }
        TypedPatternKind::Struct { fields, .. } => {
            for field_pat in fields {
                if let Some(nested_pat) = &field_pat.pattern {
                    add_pattern_bindings(checker, nested_pat)?;
                }
                // Shorthand bindings (`{ field }`) are handled during check_pattern now.
            }
        }
         TypedPatternKind::Constructor { args, .. } => {
            add_pattern_bindings(checker, args)?;
        }
         TypedPatternKind::Or(left, right) => {
            // Bindings for Or patterns should have already been added and unified
            // during the `check_pattern` call for the Or pattern itself.
            // We don't need to add them again here.
        }
        TypedPatternKind::Literal(_) | TypedPatternKind::Wildcard | TypedPatternKind::Rest => {
            // These patterns don't introduce bindings.
        }
    }
    Ok(())
}


// Function to check 'match' expressions
pub(crate) fn check_match(
    checker: &mut TypeChecker,
    scrutinee: &ResolvedExpr,
    arms: &[(ResolvedPattern, ResolvedExpr)],
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    // 1. Check scrutinee expression
    let typed_scrutinee = type_check_expression(checker, scrutinee, None)?;
    let scrutinee_ty = checker.resolve_type(&typed_scrutinee.ty);

    // 2. Infer a common result type for all arms
    let result_ty_var = checker.fresh_infer_var(span);
    let mut typed_arms = Vec::new();

    if arms.is_empty() {
        // Allow empty match only if scrutinee type is Never
        if matches!(scrutinee_ty.kind, TyKind::Never) {
             let never_ty = Ty::with_span(TyKind::Never, span);
             return Ok((
                 TypedExprKind::Match {
                     scrutinee: Box::new(typed_scrutinee),
                     arms: typed_arms, // Empty vec
                 },
                 never_ty, // Result is Never
             ));
        } else {
            // TODO: Refine error message later (e.g., NonExhaustiveMatch)
            return Err(TypeError::InternalError {
                message: format!(
                    "Empty match expression requires scrutinee of type Never, found {}", 
                    crate::error::display_type(&scrutinee_ty)
                ),
                span: Some(span),
            });
        }
    }

    // 3. Check each arm
    for (pattern, body) in arms {
        // 3a. Create a scope for the arm's bindings
        let original_env = checker.type_env.clone();
        // Use a temporary mutable environment for the arm
        let mut arm_env_inner = TypeEnvironment::with_parent(original_env.clone());
        checker.type_env = Arc::new(arm_env_inner); // Shadow checker.type_env

        // 3b. Check the pattern against the scrutinee type
        // This will add bindings to the *temporary* checker.type_env
        let typed_pattern = check_pattern(checker, pattern, &scrutinee_ty)?; // Assuming check_pattern is accessible

        // Add bindings from the pattern to the current environment
        add_pattern_bindings(checker, &typed_pattern)?;


        // 3c. Check the arm body within the arm's env, expecting it to unify with the common result type
        let typed_body = type_check_expression(checker, body, Some(&result_ty_var))?;

        // 3d. Restore original environment before checking next arm
        checker.type_env = original_env;

        typed_arms.push(TypedMatchArm { pattern: typed_pattern, body: typed_body });
    }

    // 4. TODO: Check for exhaustiveness (very complex)

    // 5. Resolve the final result type
    let final_result_ty = checker.resolve_type(&result_ty_var);

    // Construct the final Match expression kind
    Ok((
        TypedExprKind::Match {
            scrutinee: Box::new(typed_scrutinee),
            arms: typed_arms,
        },
        final_result_ty,
    ))
}