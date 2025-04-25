// tests/control_flow/match_expr.rs
use std::sync::Arc;
use crate::*;
use parallax_syntax::ast::{Literal as AstLiteral};
use parallax_resolve::{
    types::{ResolvedExpr, ResolvedExprKind, ResolvedPattern, ResolvedPatternKind, ResolvedPatternField, ResolvedType, Symbol},
    PrimitiveType as ResolvePrimitiveType
};
use parallax_types::{
    context::trait_repo::output_assoc_type_symbol, // Import helper for associated type symbol
    checker::{self, TypeChecker},
    error::TypeError,
    types::{
        PrimitiveType, Ty, TyKind, TypeId, TypedExpr, TypedExprKind, TypedPattern, TypedPatternKind,
        TypedMatchArm, EnumDef, EnumVariant, StructDef, Field, GenericParamDef,
        TypeDef, TypedPatternField
    },
    TypeContext,
};


// --- Helper to create ResolvedExpr for Match ---
fn resolved_match(
    scrutinee: ResolvedExpr,
    arms: Vec<(ResolvedPattern, ResolvedExpr)>,
) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Match { scrutinee: Box::new(scrutinee), arms },
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    }
}

// --- Pattern Helpers ---
fn pat_wildcard() -> ResolvedPattern {
    ResolvedPattern {
        kind: ResolvedPatternKind::Wildcard,
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    }
}

fn pat_lit_int(val: i128) -> ResolvedPattern {
    ResolvedPattern {
        kind: ResolvedPatternKind::Literal(AstLiteral::Int { value: val, suffix: None }),
        span: dummy_span(),
        resolved_type: ResolvedType::IntegerLiteral,
    }
}

fn pat_ident(name: &str, sym: Symbol) -> ResolvedPattern {
    // Assuming Identifier kind just takes the name, and the symbol is associated
    // during checking/resolution rather than stored directly in the kind.
    ResolvedPattern {
        kind: ResolvedPatternKind::Identifier(name.to_string()),
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    }
}

fn pat_or(left: ResolvedPattern, right: ResolvedPattern) -> ResolvedPattern {
    ResolvedPattern {
        kind: ResolvedPatternKind::Or(Box::new(left), Box::new(right)),
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    }
}

fn pat_constructor(sym: Symbol, args: ResolvedPattern) -> ResolvedPattern {
    ResolvedPattern {
        kind: ResolvedPatternKind::Constructor { symbol: sym, args: Box::new(args) },
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    }
}

fn pat_tuple(elements: Vec<ResolvedPattern>) -> ResolvedPattern {
    ResolvedPattern {
        kind: ResolvedPatternKind::Tuple(elements),
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    }
}

// Helper for Struct Pattern
fn pat_struct(struct_sym: Symbol, fields: Vec<ResolvedPatternField>) -> ResolvedPattern {
    ResolvedPattern {
        kind: ResolvedPatternKind::Struct { struct_symbol: struct_sym, fields },
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown, // Checker resolves this
    }
}

// Helper for Struct Field Pattern
fn pat_field(name: &str, pat: Option<ResolvedPattern>) -> ResolvedPatternField {
    ResolvedPatternField {
        name: name.to_string(),
        pattern: pat,
        span: dummy_span(),
    }
}

// Helper to add a simple struct
fn add_point_struct(checker: &mut TypeChecker) -> Symbol {
    let struct_symbol = Symbol::new(200);
    let field_x_symbol = Symbol::new(201);
    let field_y_symbol = Symbol::new(202);

    let struct_def = StructDef {
        name: "Point".to_string(),
        symbol: struct_symbol,
        generic_params: vec![],
        fields: vec![
            Field { name: "x".to_string(), symbol: field_x_symbol, ty: ty_prim(PrimitiveType::I32), span: dummy_span() },
            Field { name: "y".to_string(), symbol: field_y_symbol, ty: ty_prim(PrimitiveType::I32), span: dummy_span() },
        ],
        span: dummy_span(),
    };
    checker.type_ctx.add_type(struct_symbol, "Point".to_string(), TypeDef::Struct(struct_def));
    struct_symbol
}

// Helper to create a tuple expr (defined locally as it's not in mod.rs)
fn resolved_tuple_expr(elements: Vec<ResolvedExpr>) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Tuple(elements),
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown, // Resolver would figure this out
    }
}

// Helper to create a Point struct literal expression
// Use ResolvedExprKind::Struct
fn resolved_point_literal(x: i128, y: i128, struct_sym: Symbol) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Struct {
            struct_symbol: struct_sym,
            fields: vec![
                ("x".to_string(), resolved_lit_int(x)),
                ("y".to_string(), resolved_lit_int(y)),
            ],
            base: None, // Add base field
        },
        span: dummy_span(),
        resolved_type: ResolvedType::UserDefined { symbol: struct_sym, type_args: None },
    }
}

// --- Tests ---

#[test]
fn test_match_simple_literals() {
    let mut checker = setup_checker();
    let scrutinee = resolved_lit_int(10);
    let arm1 = (pat_lit_int(10), resolved_lit_string("ten"));
    let arm2 = (pat_wildcard(), resolved_lit_string("other"));

    let match_expr = resolved_match(scrutinee, vec![arm1, arm2]);
    let expected_ty = ty_prim(PrimitiveType::String); // Both arms return String

    let result = checker::expr::type_check_expression(&mut checker, &match_expr, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(matches!(typed_expr.kind, TypedExprKind::Match { .. }));
    if let TypedExprKind::Match { ref arms, .. } = typed_expr.kind {
        assert_eq!(arms.len(), 2);
        assert_eq!(arms[0].body.ty, expected_ty);
        assert_eq!(arms[1].body.ty, expected_ty);
    } else {
        panic!("Expected Match expr");
    }
    assert!(checker.errors.is_empty());
}


#[test]
fn test_match_arm_type_mismatch() {
    let mut checker = setup_checker();
    let scrutinee = resolved_lit_int(5);
    let arm1 = (pat_lit_int(5), resolved_lit_string("five")); // String
    let arm2 = (pat_wildcard(), resolved_lit_int(0));        // IntegerLiteral

    let match_expr = resolved_match(scrutinee, vec![arm1, arm2]);

    let result = checker::expr::type_check_expression(&mut checker, &match_expr, None);
    assert!(result.is_err());
    assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
    assert_eq!(checker.errors.len(), 1); // Only one error for the mismatch between arms
}


#[test]
fn test_match_binding_pattern() {
    let mut checker = setup_checker();
    let scrutinee = resolved_lit_int(100);
    let bind_sym = Symbol::new(1);
    let arm1 = (pat_ident("n", bind_sym), ResolvedExpr {
        kind: ResolvedExprKind::Variable { binding_symbol: bind_sym, name: "n".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown, // Should be inferred from pattern
    });

    let match_expr = resolved_match(scrutinee, vec![arm1]);
    // The result type is the type of the scrutinee (and the binding)
    let expected_ty = ty_prim(PrimitiveType::IntegerLiteral);

    let result = checker::expr::type_check_expression(&mut checker, &match_expr, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(matches!(typed_expr.kind, TypedExprKind::Match { .. }));
    if let TypedExprKind::Match { ref arms, .. } = typed_expr.kind {
        assert_eq!(arms.len(), 1);
        assert_eq!(arms[0].body.ty, expected_ty);
        // Check binding pattern type
        assert!(matches!(arms[0].pattern.kind, TypedPatternKind::Identifier { .. }));
        assert_eq!(arms[0].pattern.ty, expected_ty);
    } else {
        panic!("Expected Match expr");
    }
    assert!(checker.errors.is_empty());
}


#[test]
fn test_match_or_pattern() {
    let mut checker = setup_checker();
    let scrutinee = resolved_lit_int(1);
    let arm1 = (
        pat_or(pat_lit_int(1), pat_lit_int(2)),
        resolved_lit_string("one or two")
    );
    let arm2 = (pat_wildcard(), resolved_lit_string("other"));

    let match_expr = resolved_match(scrutinee, vec![arm1, arm2]);
    let expected_ty = ty_prim(PrimitiveType::String);

    let result = checker::expr::type_check_expression(&mut checker, &match_expr, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(checker.errors.is_empty());
}


// Helper to add a simple enum Option<T>
fn add_option_enum(checker: &mut TypeChecker, gen_param_id: TypeId) -> (Symbol, Symbol, Symbol) {
    let enum_symbol = Symbol::new(100);
    let some_variant_symbol = Symbol::new(101);
    let none_variant_symbol = Symbol::new(102);
    let generic_param_symbol = Symbol::new(103);

    let generic_param = GenericParamDef {
        name: "T".to_string(),
        symbol: generic_param_symbol,
        id: gen_param_id,
        bounds: vec![],
        span: dummy_span(),
    };
    let ty_t = Ty::new(TyKind::Var(gen_param_id));

    let enum_def = EnumDef {
        name: "Option".to_string(),
        symbol: enum_symbol,
        generic_params: vec![generic_param.clone()],
        variants: vec![
            EnumVariant {
                name: "Some".to_string(),
                symbol: some_variant_symbol,
                fields: vec![Field { name: "_0".to_string(), symbol: Symbol::fresh(), ty: ty_t, span: dummy_span()}], // Tuple-like
                span: dummy_span(),
            },
            EnumVariant {
                name: "None".to_string(),
                symbol: none_variant_symbol,
                fields: vec![], // Unit
                span: dummy_span(),
            },
        ],
        span: dummy_span(),
    };
    checker.type_ctx.add_type(enum_symbol, "Option".to_string(), TypeDef::Enum(enum_def));
    // Add variant constructors to type context (simplified)
    checker.type_ctx.add_symbol_name(some_variant_symbol, "Some".to_string());
    checker.type_ctx.add_symbol_name(none_variant_symbol, "None".to_string());

    (enum_symbol, some_variant_symbol, none_variant_symbol)
}

#[test]
fn test_match_enum_constructor() {
    let mut checker = setup_checker();
    let gen_param_id = TypeId(0);
    let (_enum_sym, some_sym, none_sym) = add_option_enum(&mut checker, gen_param_id);

    // Scrutinee: Option<i32> (we need to construct this)
    // Let's assume a function `get_option()` returns this.
    // For the test, we'll just define the expected type.
    let option_i32_ty = ty_named("Option", Some(_enum_sym), vec![ty_prim(PrimitiveType::I32)]);
    let scrutinee = ResolvedExpr { // Placeholder scrutinee
        kind: ResolvedExprKind::Variable { binding_symbol: Symbol::new(500), name: "opt".to_string() },
        span: dummy_span(),
        // Update to match the likely definition: { symbol, type_args }
        resolved_type: ResolvedType::UserDefined {
            symbol: _enum_sym,
            type_args: Some(vec![ResolvedType::Primitive(ResolvePrimitiveType::I32)]) // Wrap in Some()
        },
    };
    // Add the placeholder variable to the env so the scrutinee itself type checks
    checker._type_env = Arc::new({ let mut env = TypeEnvironment::new(); env.add("opt".to_string(), option_i32_ty.clone()); env });

    let bind_val_sym = Symbol::new(10);
    let pat_some = pat_constructor(some_sym, pat_ident("val", bind_val_sym));
    let pat_none = pat_constructor(none_sym, pat_tuple(vec![])); // None is Unit variant

    // Arms: Match Some(val) and None
    let arm_some = (pat_some, ResolvedExpr { // Body uses 'val'
        kind: ResolvedExprKind::Variable { binding_symbol: bind_val_sym, name: "val".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    });
    let arm_none = (pat_none, resolved_lit_int(-1)); // Return -1 for None

    let match_expr = resolved_match(scrutinee, vec![arm_some, arm_none]);
    let expected_ty = ty_prim(PrimitiveType::I32); // Expect i32 (unified from val: i32 and -1)

    let result = checker::expr::type_check_expression(&mut checker, &match_expr, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    if let TypedExprKind::Match { ref arms, .. } = typed_expr.kind {
        assert_eq!(arms.len(), 2);
        assert_eq!(arms[0].body.ty, expected_ty);
        assert_eq!(arms[1].body.ty, expected_ty);
        // Check pattern types
        assert!(matches!(arms[0].pattern.kind, TypedPatternKind::Constructor { .. }));
        assert!(matches!(arms[1].pattern.kind, TypedPatternKind::Constructor { .. }));
        assert_eq!(arms[0].pattern.ty, option_i32_ty);
        assert_eq!(arms[1].pattern.ty, option_i32_ty);
    } else {
        panic!("Expected Match expr");
    }
    assert!(checker.errors.is_empty());
}


#[test]
fn test_match_scrutinee_type_mismatch_pattern() {
    let mut checker = setup_checker();
    let gen_param_id = TypeId(0);
    let (_enum_sym, some_sym, _none_sym) = add_option_enum(&mut checker, gen_param_id);

    // Scrutinee is i32
    let scrutinee = resolved_lit_int(10);

    // Pattern expects Option<T>
    let bind_val_sym = Symbol::new(10);
    let pat_some = pat_constructor(some_sym, pat_ident("val", bind_val_sym));
    let arm_some = (pat_some, resolved_lit_bool(true));
    let arm_wild = (pat_wildcard(), resolved_lit_bool(false));

    let match_expr = resolved_match(scrutinee, vec![arm_some, arm_wild]);

    let result = checker::expr::type_check_expression(&mut checker, &match_expr, None);
    // Type checking the match expr itself might fail early, or record errors during arm checks.
    // The key is that an error related to mismatching scrutinee and pattern occurs.
    assert!(result.is_err() || !checker.errors.is_empty());
    assert!(checker.errors.iter().any(|e| matches!(e, TypeError::WrongPatternType { .. } | TypeError::TypeMismatch { .. })));
}

#[test]
fn test_match_empty_arms_never_scrutinee() {
    let mut checker = setup_checker();
    // Scrutinee has type Never
    let never_ty = Ty::new(TyKind::Never);
    let scrutinee = ResolvedExpr { // Placeholder
        kind: ResolvedExprKind::Variable { binding_symbol: Symbol::new(99), name: "diverge".to_string() },
        span: dummy_span(),
        resolved_type: ResolvedType::Never,
    };
    // Add placeholder to env
     checker._type_env = Arc::new({ let mut env = TypeEnvironment::new(); env.add("diverge".to_string(), never_ty.clone()); env });

    // Empty arms
    let match_expr = resolved_match(scrutinee, vec![]);
    let expected_ty = never_ty; // Result of empty match on Never is Never

    let result = checker::expr::type_check_expression(&mut checker, &match_expr, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(matches!(typed_expr.kind, TypedExprKind::Match { ref arms, .. } if arms.is_empty()));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_match_empty_arms_non_never_scrutinee() {
    let mut checker = setup_checker();
    let scrutinee = resolved_lit_int(10); // Scrutinee is int

    // Empty arms
    let match_expr = resolved_match(scrutinee, vec![]);

    let result = checker::expr::type_check_expression(&mut checker, &match_expr, None);
    assert!(result.is_err()); // Should be an error (likely InternalError for now)
    // TODO: Refine this assertion when exhaustiveness check is implemented
    // assert!(matches!(result.err().unwrap(), TypeError::NonExhaustivePatterns { .. }));
     assert!(matches!(result.err().unwrap(), TypeError::InternalError { .. }));
}

#[test]
fn test_match_tuple_pattern() {
    let mut checker = setup_checker();
    let scrutinee = resolved_tuple_expr(vec![resolved_lit_int(10), resolved_lit_bool(true)]);
    let sym_a = Symbol::new(50);
    let sym_b = Symbol::new(51);

    let arm1 = (
        pat_tuple(vec![
            pat_ident("a", sym_a),
            pat_wildcard(), // Ignore second element
        ]),
        ResolvedExpr { // Return `a`
            kind: ResolvedExprKind::Variable { binding_symbol: sym_a, name: "a".to_string() },
            span: dummy_span(),
            resolved_type: ResolvedType::Unknown,
        }
    );

    let match_expr = resolved_match(scrutinee, vec![arm1]);
    let expected_ty = ty_prim(PrimitiveType::IntegerLiteral); // Type of `a`

    let result = checker::expr::type_check_expression(&mut checker, &match_expr, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(checker.errors.is_empty());
}

#[test]
fn test_match_struct_pattern_full() {
    let mut checker = setup_checker();
    let point_sym = add_point_struct(&mut checker);

    let scrutinee = resolved_point_literal(5, -5, point_sym);
    let sym_x = Symbol::new(60);
    let sym_y = Symbol::new(61);

    let arm1 = (
        pat_struct(point_sym, vec![
            pat_field("x", Some(pat_ident("px", sym_x))),
            pat_field("y", Some(pat_ident("py", sym_y))),
        ]),
        resolved_lit_bool(true) // Just return bool
    );
    let arm_wild = (pat_wildcard(), resolved_lit_bool(false));

    let match_expr = resolved_match(scrutinee, vec![arm1, arm_wild]);
    let expected_ty = ty_prim(PrimitiveType::Bool);

    let result = checker::expr::type_check_expression(&mut checker, &match_expr, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(checker.errors.is_empty());

    // Check pattern structure
    if let TypedExprKind::Match { ref arms, .. } = typed_expr.kind {
        assert_eq!(arms.len(), 2);
        assert!(matches!(arms[0].pattern.kind, TypedPatternKind::Struct { .. }));
    } else {
        panic!("Expected Match expr");
    }
}

#[test]
fn test_match_struct_pattern_shorthand() {
    let mut checker = setup_checker();
    let point_sym = add_point_struct(&mut checker);

    let scrutinee = resolved_point_literal(1, 2, point_sym);
    let sym_x = Symbol::new(70); // Resolver should associate this with the 'x' binding

    let arm1 = (
        pat_struct(point_sym, vec![
            pat_field("x", None), // Shorthand binding for x
            pat_field("y", Some(pat_wildcard())), // Ignore y
        ]),
        ResolvedExpr { // Return x
            kind: ResolvedExprKind::Variable { binding_symbol: sym_x, name: "x".to_string() },
            span: dummy_span(),
            resolved_type: ResolvedType::Unknown,
        }
    );

    let match_expr = resolved_match(scrutinee, vec![arm1]);
    let expected_ty = ty_prim(PrimitiveType::I32); // Type of x

    let result = checker::expr::type_check_expression(&mut checker, &match_expr, None);
    assert!(result.is_ok(), "Expected Ok, got {:?}", result.err());
    let typed_expr = result.unwrap();

    assert_eq!(typed_expr.ty, expected_ty);
    assert!(checker.errors.is_empty());
}

#[test]
fn test_match_or_pattern_binding_mismatch() {
    let mut checker = setup_checker();
    let scrutinee = resolved_lit_int(1);
    let sym_a = Symbol::new(80);
    let sym_b = Symbol::new(81);

    // (1 | a) => ... // ERROR: 1 doesn't bind, 'a' does
    // (a | b) => ... // ERROR: Different variables bound

    let arm1 = (
        pat_or(
            pat_lit_int(1),
            pat_ident("a", sym_a)
        ),
        resolved_lit_bool(true)
    );

    let match_expr = resolved_match(scrutinee.clone(), vec![arm1]);
    let result1 = checker::expr::type_check_expression(&mut checker, &match_expr, None);
    assert!(result1.is_err() || !checker.errors.is_empty());
    assert!(checker.errors.iter().any(|e| matches!(e, TypeError::OrPatternBindingMismatch { .. })));

    // Reset checker for next case
    checker = setup_checker();
    let arm2 = (
        pat_or(
            pat_ident("a", sym_a),
            pat_ident("b", sym_b) // Different binding name
        ),
        resolved_lit_bool(true)
    );
    let match_expr2 = resolved_match(scrutinee, vec![arm2]);
    let result2 = checker::expr::type_check_expression(&mut checker, &match_expr2, None);
    assert!(result2.is_err() || !checker.errors.is_empty());
     assert!(checker.errors.iter().any(|e| matches!(e, TypeError::OrPatternBindingMismatch { .. })));
}

// TODO: Add tests for array patterns
// TODO: Add tests for exhaustiveness checking (when implemented)
