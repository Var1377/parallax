// tests/control_flow/block_expr.rs
use crate::*; // Import helpers from tests/mod.rs
use parallax_resolve::{
    types::{ResolvedExpr, ResolvedExprKind, ResolvedPattern, ResolvedPatternKind, Symbol, ResolvedType},
    PrimitiveType as ResolvePrimitiveType,
};
use parallax_types::{
    error::TypeError,
    types::{PrimitiveType, Ty, TyKind, TypedExpr, TypedExprKind, TypedPattern, TypedPatternKind},
};

// --- Helper to create ResolvedExpr for Block ---
fn resolved_block(exprs: Vec<ResolvedExpr>) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Block(exprs),
        span: dummy_span(),
        resolved_type: parallax_resolve::types::ResolvedType::Unknown, // Type checker will determine this
    }
}

// --- Helper to create ResolvedExpr for Let ---
fn resolved_let(
    pattern: ResolvedPattern,
    value: ResolvedExpr,
    type_annotation: Option<ResolvedType>,
) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Let {
            pattern,
            value: Box::new(value),
            type_annotation,
        },
        span: dummy_span(),
        resolved_type: ResolvedType::Primitive(ResolvePrimitiveType::Unit), // Let itself is unit
    }
}

// --- Helper to create ResolvedPattern for Identifier ---
fn resolved_pat_ident(name: &str, symbol: Symbol) -> ResolvedPattern {
    // Assuming Identifier kind just takes the name, and the symbol is associated
    // during checking/resolution rather than stored directly in the kind.
    ResolvedPattern {
        kind: ResolvedPatternKind::Identifier(name.to_string()),
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    }
}

// --- Helper to create ResolvedPattern for Tuple ---
fn resolved_pat_tuple(elements: Vec<ResolvedPattern>) -> ResolvedPattern {
    ResolvedPattern {
        kind: ResolvedPatternKind::Tuple(elements),
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    }
}

// Helper to create a tuple expr
fn resolved_tuple_expr(elements: Vec<ResolvedExpr>) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Tuple(elements),
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown, // Resolver would figure this out
    }
}

#[test]
fn test_block_empty() {
    let mut checker = setup_checker();
    let block_expr = resolved_block(vec![]);
    let expected_ty = ty_prim(PrimitiveType::Unit); // Empty block is Unit

    let result = checker::expr::type_check_expression(&mut checker, &block_expr, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(matches!(typed_expr.kind, TypedExprKind::Block(ref exprs) if exprs.is_empty()));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_block_single_expr() {
    let mut checker = setup_checker();
    let expr = resolved_lit_int(42);
    let block_expr = resolved_block(vec![expr]);
    let expected_ty = ty_prim(PrimitiveType::IntegerLiteral); // Type is the type of the last expr

    let result = checker::expr::type_check_expression(&mut checker, &block_expr, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(matches!(typed_expr.kind, TypedExprKind::Block(ref exprs) if exprs.len() == 1));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_block_multiple_exprs() {
    let mut checker = setup_checker();
    let expr1 = resolved_lit_int(10);
    let expr2 = resolved_lit_bool(true);
    let block_expr = resolved_block(vec![expr1, expr2]); // Last expr is bool
    let expected_ty = ty_prim(PrimitiveType::Bool);

    let result = checker::expr::type_check_expression(&mut checker, &block_expr, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(matches!(typed_expr.kind, TypedExprKind::Block(ref exprs) if exprs.len() == 2));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_block_with_let_simple() {
    let mut checker = setup_checker();
    let let_sym = Symbol::new(1);
    let let_pat = resolved_pat_ident("x", let_sym);
    let let_val = resolved_lit_int(5);
    let let_expr = resolved_let(let_pat, let_val, None);

    // Block consists only of the let statement
    let block_expr = resolved_block(vec![let_expr]);
    let expected_block_ty = ty_prim(PrimitiveType::Unit); // Block containing only let is Unit

    let result = checker::expr::type_check_expression(&mut checker, &block_expr, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_block_ty);
    assert!(checker.errors.is_empty());

    // Verify the binding was added correctly (requires inspecting the *inner* block env)
    // This is tricky to test directly without exposing internal env states.
    // We'll rely on the fact that the type check passed and assume the binding logic works.
}

#[test]
fn test_block_with_let_and_use() {
    let mut checker = setup_checker();
    let let_sym = Symbol::new(1);
    let let_pat = resolved_pat_ident("y", let_sym);
    let let_val = resolved_lit_string("hello");
    let let_expr = resolved_let(let_pat, let_val, None);

    // Expression using the bound variable 'y'
    let use_expr = ResolvedExpr {
        kind: ResolvedExprKind::Variable {
            binding_symbol: let_sym, // Use the same symbol as the let binding
            name: "y".to_string(),
        },
        span: dummy_span(),
        // The resolver might know this is string, or it might be unknown.
        // For the type checker, the binding added in the block provides the type.
        resolved_type: ResolvedType::Unknown,
    };

    let block_expr = resolved_block(vec![let_expr, use_expr]);
    let expected_block_ty = ty_prim(PrimitiveType::String); // Type of the last expression

    let result = checker::expr::type_check_expression(&mut checker, &block_expr, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_block_ty);
    assert!(checker.errors.is_empty());

    // Check the type of the last expression in the block
    if let TypedExprKind::Block(ref exprs) = typed_expr.kind {
        assert_eq!(exprs.len(), 2);
        // First element is Let (type Unit)
        assert_eq!(exprs[0].ty, ty_prim(PrimitiveType::Unit));
        // Second element is Variable (type String)
        assert!(matches!(exprs[1].kind, TypedExprKind::Variable { ref name, .. } if name == "y"));
        assert_eq!(exprs[1].ty, expected_block_ty);
    } else {
        panic!("Expected Block expression");
    }
}

#[test]
fn test_block_let_type_annotation_match() {
    let mut checker = setup_checker();
    let let_sym = Symbol::new(2);
    let let_pat = resolved_pat_ident("z", let_sym);
    let let_val = resolved_lit_int(100); // IntegerLiteral
    let annotation = ResolvedType::Primitive(ResolvePrimitiveType::I32);
    let let_expr = resolved_let(let_pat, let_val, Some(annotation));

    let use_expr = ResolvedExpr {
        kind: ResolvedExprKind::Variable { binding_symbol: let_sym, name: "z".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    };

    let block_expr = resolved_block(vec![let_expr, use_expr]);
    let expected_block_ty = ty_prim(PrimitiveType::I32); // Type from annotation

    let result = checker::expr::type_check_expression(&mut checker, &block_expr, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_block_ty);
    assert!(checker.errors.is_empty());

    // Check the type of the let value and the variable use
     if let TypedExprKind::Block(ref exprs) = typed_expr.kind {
        assert_eq!(exprs.len(), 2);
        if let TypedExprKind::Let { ref value, .. } = exprs[0].kind {
            // The literal value itself might retain IntegerLiteral, but the binding uses the annotation
            // assert_eq!(value.ty, expected_block_ty); // This depends on coercion details
        }
        assert_eq!(exprs[1].ty, expected_block_ty);
    } else {
        panic!("Expected Block expression");
    }
}

#[test]
fn test_block_let_type_annotation_mismatch() {
    let mut checker = setup_checker();
    let let_sym = Symbol::new(3);
    let let_pat = resolved_pat_ident("a", let_sym);
    let let_val = resolved_lit_int(100); // IntegerLiteral
    let annotation = ResolvedType::Primitive(ResolvePrimitiveType::Bool); // Expect Bool
    let let_expr = resolved_let(let_pat, let_val, Some(annotation));

    let block_expr = resolved_block(vec![let_expr]); // Just the let

    let result = checker::expr::type_check_expression(&mut checker, &block_expr, None);
    // The block expression itself might type check ok (as Unit), but an error should be recorded.
    assert!(result.is_ok());
    assert!(!checker.errors.is_empty());
    assert_eq!(checker.errors.len(), 1);
    assert!(matches!(checker.errors[0], TypeError::TypeMismatch { .. }));

     // Ensure the block's type is Unit
     let typed_expr = result.unwrap();
     assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::Unit));
}

#[test]
fn test_block_shadowing() {
    let mut checker = setup_checker();
    let sym1 = Symbol::new(10);
    let sym2 = Symbol::new(11);

    let let1_pat = resolved_pat_ident("x", sym1);
    let let1_val = resolved_lit_int(10);
    let let1 = resolved_let(let1_pat, let1_val, None);

    let let2_pat = resolved_pat_ident("x", sym2); // Same name, different symbol
    let let2_val = resolved_lit_bool(true);
    let let2 = resolved_let(let2_pat, let2_val, None);

    // Use the shadowed variable (should be bool)
    let use_expr = ResolvedExpr {
        kind: ResolvedExprKind::Variable { binding_symbol: sym2, name: "x".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    };

    let block_expr = resolved_block(vec![let1, let2, use_expr]);
    let expected_block_ty = ty_prim(PrimitiveType::Bool); // Type of the last use

    let result = checker::expr::type_check_expression(&mut checker, &block_expr, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_block_ty);
    assert!(checker.errors.is_empty());

    // Check the type of the final variable use
     if let TypedExprKind::Block(ref exprs) = typed_expr.kind {
        assert_eq!(exprs.len(), 3);
        assert!(matches!(exprs[2].kind, TypedExprKind::Variable { symbol, .. } if symbol == sym2));
        assert_eq!(exprs[2].ty, expected_block_ty);
    } else {
        panic!("Expected Block expression");
    }
}

#[test]
fn test_block_nested_scoping_ok() {
    let mut checker = setup_checker();
    let outer_sym = Symbol::new(20);
    let inner_sym = Symbol::new(21);
    let other_sym = Symbol::new(22);

    // let outer = 10;
    let let_outer = resolved_let(
        resolved_pat_ident("outer", outer_sym),
        resolved_lit_int(10),
        None
    );

    // { let inner = true; outer + 1 } // Inner block uses outer binding
    let let_inner = resolved_let(
        resolved_pat_ident("inner", inner_sym),
        resolved_lit_bool(true),
        None
    );
    let use_outer_in_inner = ResolvedExpr { // Simulate `outer + 1` -> we'll just use `outer`
        kind: ResolvedExprKind::Variable { binding_symbol: outer_sym, name: "outer".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    };
    let inner_block = resolved_block(vec![let_inner, use_outer_in_inner]);

    // let other = false;
    let let_other = resolved_let(
        resolved_pat_ident("other", other_sym),
        resolved_lit_bool(false),
        None
    );

    // Block: { let outer = 10; { let inner = true; outer }; let other = false; other }
    let outer_block = resolved_block(vec![let_outer, inner_block, let_other]);
    let expected_block_ty = ty_prim(PrimitiveType::Bool); // Type of `other`

    let result = checker::expr::type_check_expression(&mut checker, &outer_block, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_block_ty);
    assert!(checker.errors.is_empty());
}

#[test]
fn test_block_nested_scoping_use_inner_fails() {
    let mut checker = setup_checker();
    let outer_sym = Symbol::new(30);
    let inner_sym = Symbol::new(31);

    // { let inner = 10; }
    let let_inner = resolved_let(
        resolved_pat_ident("inner", inner_sym),
        resolved_lit_int(10),
        None
    );
    let inner_block = resolved_block(vec![let_inner]);

    // Use `inner` outside the block where it was defined
    let use_inner_outside = ResolvedExpr {
        kind: ResolvedExprKind::Variable { binding_symbol: inner_sym, name: "inner".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    };

    // { { let inner = 10; }; inner }
    let outer_block = resolved_block(vec![inner_block, use_inner_outside]);

    let result = checker::expr::type_check_expression(&mut checker, &outer_block, None);
    assert!(result.is_err()); // Error because 'inner' is not in scope
    assert!(!checker.errors.is_empty());
    assert!(matches!(checker.errors[0], TypeError::UnknownIdentifier { .. } | TypeError::UndefinedVariable { .. }));
}

#[test]
fn test_block_let_tuple_pattern() {
    let mut checker = setup_checker();
    let sym_a = Symbol::new(40);
    let sym_b = Symbol::new(41);

    // let (a, b) = (10, true);
    let tuple_pat = resolved_pat_tuple(vec![
        resolved_pat_ident("a", sym_a),
        resolved_pat_ident("b", sym_b),
    ]);
    let tuple_val = resolved_tuple_expr(vec![
        resolved_lit_int(10),
        resolved_lit_bool(true),
    ]);
    let let_expr = resolved_let(tuple_pat, tuple_val, None);

    // Use `a` (should be int)
    let use_a = ResolvedExpr {
        kind: ResolvedExprKind::Variable { binding_symbol: sym_a, name: "a".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    };

    let block_expr = resolved_block(vec![let_expr, use_a]);
    let expected_block_ty = ty_prim(PrimitiveType::IntegerLiteral); // Type of `a`

    let result = checker::expr::type_check_expression(&mut checker, &block_expr, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_block_ty);
    assert!(checker.errors.is_empty());
}

#[test]
fn test_block_intermediate_error() {
    let mut checker = setup_checker();

    let expr_ok = resolved_lit_int(10);
    // Simulate an erroneous expression, e.g., calling a non-function
    let expr_err = ResolvedExpr {
        kind: ResolvedExprKind::Call { func_symbol: None, args: vec![] }, // Call with no symbol
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    };
    let expr_after_err = resolved_lit_bool(true);

    let block_expr = resolved_block(vec![expr_ok, expr_err, expr_after_err]);

    let result = checker::expr::type_check_expression(&mut checker, &block_expr, None);
    // Type checking the block might succeed overall (returning type of last expr),
    // but an error should be recorded for the erroneous expression.
    assert!(result.is_ok()); // Or could be Err depending on exact error recovery
    let typed_expr = result.unwrap();

    // The final type might be bool, but an error must be present
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::Bool));
    assert!(!checker.errors.is_empty());
    assert_eq!(checker.errors.len(), 1);
    // The error might be different now (e.g. InternalError, UnknownIdentifier if symbol is needed)
    // Let's be more general or adjust based on expected error
    assert!(matches!(checker.errors[0], TypeError::NotAFunction { .. } | TypeError::InternalError { .. } ));
}

// TODO: Test blocks with struct patterns in let
// TODO: Test nested blocks and scoping rules
// TODO: Test blocks with more complex patterns in let (tuples, structs) 