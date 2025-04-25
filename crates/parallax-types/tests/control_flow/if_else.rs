// tests/control_flow/if_else.rs
use crate::*; // Import helpers from tests/mod.rs
use parallax_types::{
    error::TypeError,
    types::{PrimitiveType, Ty, TyKind, TypedExpr, TypedExprKind},
};
use parallax_resolve::types::{ResolvedExpr, ResolvedExprKind};
use parallax_syntax::ast::Literal as AstLiteral;
use std::sync::Arc;


// --- Helper to create ResolvedExpr for If ---
fn resolved_if(
    condition: ResolvedExpr,
    then_branch: ResolvedExpr,
    else_branch: Option<ResolvedExpr>,
) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(Box::new),
        },
        span: dummy_span(),
        resolved_type: parallax_resolve::types::ResolvedType::Unknown, // Type checker will determine this
    }
}


#[test]
fn test_if_else_simple_match() {
    let mut checker = setup_checker();
    let cond = resolved_lit_bool(true);
    let then_val = resolved_lit_int(10); // IntegerLiteral
    let else_val = resolved_lit_int(20); // IntegerLiteral

    let if_expr = resolved_if(cond, then_val, Some(else_val));
    let expected_ty = ty_prim(PrimitiveType::IntegerLiteral); // Result should be integer literal

    let result = checker::expr::type_check_expression(&mut checker, &if_expr, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(matches!(typed_expr.kind, TypedExprKind::If { .. }));
    assert!(checker.errors.is_empty());

    // Check that branches were unified (they are both IntegerLiteral initially)
    let type_env = checker.inference_ctx.get_substitution();
    println!("Substitution after simple if/else: {:?}", type_env);
    assert!(type_env.is_empty()); // No inference variables involved here
}


#[test]
fn test_if_else_branch_type_mismatch() {
    let mut checker = setup_checker();
    let cond = resolved_lit_bool(true);
    let then_val = resolved_lit_int(10);    // IntegerLiteral
    let else_val = resolved_lit_bool(false); // Bool

    let if_expr = resolved_if(cond, then_val, Some(else_val));

    let result = checker::expr::type_check_expression(&mut checker, &if_expr, None);
    assert!(result.is_err());

    assert!(matches!(
        result.err().unwrap(),
        TypeError::TypeMismatch { .. }
    ));
    // We expect one error (the mismatch between then and else)
    assert_eq!(checker.errors.len(), 1);
}


#[test]
fn test_if_else_coerce_literal_to_concrete() {
    let mut checker = setup_checker();
    let cond = resolved_lit_bool(true);
    let then_val = resolved_lit_int(10); // IntegerLiteral
    let else_val = resolved_lit_int(20); // IntegerLiteral
    let if_expr = resolved_if(cond, then_val, Some(else_val));

    let expected_if_ty = ty_prim(PrimitiveType::I64); // Expect the whole if-expr to be i64

    let result = checker::expr::type_check_expression(&mut checker, &if_expr, Some(&expected_if_ty));
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    // The overall type should be the expected i64
    assert_eq!(typed_expr.ty, expected_if_ty);
    assert!(matches!(typed_expr.kind, TypedExprKind::If { .. }));
    assert!(checker.errors.is_empty());

    // Check the types of the inner branches after coercion
    if let TypedExprKind::If { ref then_branch, ref else_branch, .. } = typed_expr.kind {
        assert_eq!(then_branch.ty, expected_if_ty);
        assert_eq!(else_branch.as_ref().unwrap().ty, expected_if_ty);
    } else {
        panic!("Expected TypedExprKind::If");
    }
}


#[test]
fn test_if_else_unify_variables() {
    let mut checker = setup_checker();
    let cond = resolved_lit_bool(true);

    // Let's use integer literals which can be coerced.
    // If the overall expression is expected to be i32, both branches should unify to i32.
    let then_val = resolved_lit_int(1); // IntegerLiteral
    let else_val = resolved_lit_int(2); // IntegerLiteral

    let if_expr = resolved_if(cond, then_val, Some(else_val));
    let expected_overall_ty = ty_prim(PrimitiveType::I32); // Expect the whole expression to be i32

    let result = checker::expr::type_check_expression(&mut checker, &if_expr, Some(&expected_overall_ty));
    assert!(result.is_ok(), "Expected Ok when expecting i32, got {:?}", result.err());
    let typed_expr = result.unwrap();

    // Check the overall type and the types of the branches after unification/coercion.
    assert_eq!(typed_expr.ty, expected_overall_ty);
    if let TypedExprKind::If { ref then_branch, ref else_branch, .. } = typed_expr.kind {
        assert_eq!(then_branch.ty, expected_overall_ty, "Then branch should be i32");
        assert!(else_branch.is_some());
        assert_eq!(else_branch.as_ref().unwrap().ty, expected_overall_ty, "Else branch should be i32");
    } else {
        panic!("Expected TypedExprKind::If");
    }
    assert!(checker.errors.is_empty());
}

#[test]
fn test_if_condition_not_bool() {
    let mut checker = setup_checker();
    let cond = resolved_lit_int(0); // Condition is Int, not Bool
    let then_val = resolved_lit_int(10);
    let else_val = resolved_lit_int(20);

    let if_expr = resolved_if(cond, then_val, Some(else_val));

    let result = checker::expr::type_check_expression(&mut checker, &if_expr, None);
    assert!(result.is_err());
    assert!(matches!(
        result.err().unwrap(),
        TypeError::TypeMismatch { ref expected, .. } if expected == "boolean"
    ));
    assert_eq!(checker.errors.len(), 1);
}


#[test]
fn test_if_without_else_unit_result() {
    let mut checker = setup_checker();
    let cond = resolved_lit_bool(true);
    // 'then' branch must evaluate to Unit () if there's no 'else'
    // Represent Unit literal as an empty tuple expression
    let then_val = ResolvedExpr {
        kind: ResolvedExprKind::Tuple(vec![]), // Use empty tuple for Unit literal
        span: dummy_span(),
        resolved_type: parallax_resolve::types::ResolvedType::Tuple(vec![]), // Type is Tuple()
    };

    let if_expr = resolved_if(cond, then_val, None);
    let expected_ty = ty_prim(PrimitiveType::Unit); // TyKind::Tuple([]) also works

    let result = checker::expr::type_check_expression(&mut checker, &if_expr, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty); // Result must be Unit
    assert!(matches!(typed_expr.kind, TypedExprKind::If { else_branch: None, .. }));
    assert!(checker.errors.is_empty());
}


#[test]
fn test_if_without_else_then_not_unit() {
    let mut checker = setup_checker();
    let cond = resolved_lit_bool(true);
    let then_val = resolved_lit_int(10); // Then branch is Int, not Unit

    let if_expr = resolved_if(cond, then_val, None);

    let result = checker::expr::type_check_expression(&mut checker, &if_expr, None);
    assert!(result.is_err());
    assert!(matches!(
        result.err().unwrap(),
        TypeError::TypeMismatch { ref expected, ref found, .. } if expected == "()" && found == "{integer}"
    ));
    assert_eq!(checker.errors.len(), 1);
}

#[test]
fn test_if_nested() {
    let mut checker = setup_checker();
    let outer_cond = resolved_lit_bool(true);
    let inner_cond = resolved_lit_bool(false);

    // if true then (if false then 1 else 2) else 3
    // -> resolves to 2
    let inner_if = resolved_if(
        inner_cond,
        resolved_lit_int(1),
        Some(resolved_lit_int(2)),
    );
    let outer_if = resolved_if(
        outer_cond,
        inner_if, // Nested if
        Some(resolved_lit_int(3)),
    );

    let expected_ty = ty_prim(PrimitiveType::IntegerLiteral);

    let result = checker::expr::type_check_expression(&mut checker, &outer_if, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(checker.errors.is_empty());

    // Check structure
    assert!(matches!(typed_expr.kind, TypedExprKind::If { .. }));
    if let TypedExprKind::If { ref then_branch, ref else_branch, .. } = typed_expr.kind {
        assert!(matches!(then_branch.kind, TypedExprKind::If { .. }));
        assert!(matches!(else_branch.as_ref().unwrap().kind, TypedExprKind::IntLiteral { .. }));
        assert_eq!(then_branch.ty, expected_ty);
        assert_eq!(else_branch.as_ref().unwrap().ty, expected_ty);
    } else {
        panic!("Expected outer If");
    }
}

#[test]
fn test_if_nested_branch_mismatch() {
    let mut checker = setup_checker();
    let outer_cond = resolved_lit_bool(true);
    let inner_cond = resolved_lit_bool(false);

    // if true then (if false then 1 else true) else 3
    // Inner if branches mismatch (int vs bool)
    let inner_if = resolved_if(
        inner_cond,
        resolved_lit_int(1),
        Some(resolved_lit_bool(true)), // Mismatch here
    );
    let outer_if = resolved_if(
        outer_cond,
        inner_if, // Nested if
        Some(resolved_lit_int(3)), // Outer else is int
    );

    let result = checker::expr::type_check_expression(&mut checker, &outer_if, None);
    assert!(result.is_err());
    // Expect error from the inner mismatch
    assert_eq!(checker.errors.len(), 1);
     assert!(matches!(
        checker.errors[0],
        TypeError::TypeMismatch { .. }
    ));

}

// TODO: Add tests involving type inference variables within if conditions or branches
#[test]
fn test_if_else_infer_variable() {
    let mut checker = setup_checker();
    let cond = resolved_lit_bool(true);
    let var_sym = Symbol::new(10); // Symbol for variable 'x'

    // Add 'x: i32' to the initial environment
    checker._type_env = Arc::new({
        let mut env = TypeEnvironment::new();
        env.add("x".to_string(), ty_prim(PrimitiveType::I32));
        env
    });

    // if true then x else 5
    // 'x' is i32, '5' is IntegerLiteral. The result should unify to i32.
    let then_val = ResolvedExpr {
        kind: ResolvedExprKind::Variable { binding_symbol: var_sym, name: "x".to_string() },
        span: dummy_span(),
        resolved_type: parallax_resolve::types::ResolvedType::Primitive(ResolvePrimitiveType::I32),
    };
    let else_val = resolved_lit_int(5); // IntegerLiteral

    let if_expr = resolved_if(cond, then_val, Some(else_val));
    let expected_ty = ty_prim(PrimitiveType::I32); // Expect the expression to resolve to i32

    let result = checker::expr::type_check_expression(&mut checker, &if_expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(checker.errors.is_empty());

    // Check branch types after unification
    if let TypedExprKind::If { ref then_branch, ref else_branch, .. } = typed_expr.kind {
        assert_eq!(then_branch.ty, expected_ty, "Then branch (variable x) should be i32");
        assert_eq!(else_branch.as_ref().unwrap().ty, expected_ty, "Else branch (literal 5) should be coerced to i32");
    } else {
        panic!("Expected TypedExprKind::If");
    }
}
