use std::path::PathBuf;

use parallax_lang::ast::{
    common::{Ident, Span},
    expr::{Expr, ExprKind, BinaryOp},
    items::{Function, Item, ItemKind, Parameter},
    pattern::{Pattern, PatternKind},
};

use parallax_resolve::{
    resolver::Resolver,
    symbol::Symbol,
    ResolveError,
};

// Helper function to create a simple function AST
fn create_simple_function() -> Item {
    let span = Span { start: 0, end: 0 };
    
    // Create a simple function: fn add(a, b) = { a + b };
    
    // Parameter a - without type annotation
    let param_a = Parameter {
        pattern: Pattern {
            kind: PatternKind::Identifier(Ident("a".to_string())),
            span,
        },
        ty: None, // No type annotation
        default_value: None,
        is_variadic: false,
        span,
    };
    
    // Parameter b - without type annotation
    let param_b = Parameter {
        pattern: Pattern {
            kind: PatternKind::Identifier(Ident("b".to_string())),
            span,
        },
        ty: None, // No type annotation
        default_value: None,
        is_variadic: false,
        span,
    };
    
    // No return type
    
    // Body: a + b
    let body = Expr {
        kind: ExprKind::Binary {
            left: Box::new(Expr {
                kind: ExprKind::Path(vec![Ident("a".to_string())]),
                span,
            }),
            op: BinaryOp::Add,
            right: Box::new(Expr {
                kind: ExprKind::Path(vec![Ident("b".to_string())]),
                span,
            }),
        },
        span,
    };
    
    // Create the function
    let function = Function {
        name: Ident("add".to_string()),
        generic_params: None,
        params: vec![param_a, param_b],
        return_type: None, // No return type annotation
        where_clause: None,
        body: Box::new(body),
        span,
    };
    
    // Create the item
    Item {
        kind: ItemKind::Function(function),
        visibility: true,
        span,
    }
}

#[test]
fn test_resolve_simple_function() {
    // Create a simple function
    let item = create_simple_function();
    
    // Create a resolver
    let mut resolver = Resolver::new(
        PathBuf::from("<test>"),
        "fn add(a, b) = { a + b };".to_string(),
    );
    
    // Resolve the item
    let result = resolver.resolve(&item);
    
    // Check that resolution succeeded
    assert!(result.is_ok(), "Resolution failed: {:?}", result.err());
    
    // Check the resolved crate
    let resolved_crate = result.unwrap();
    
    // The symbol table should contain the function
    let symbols = resolved_crate.symbol_table.lookup("add");
    assert!(!symbols.is_empty(), "Function 'add' not found in symbol table");
    
    // Check that the function is public
    let function_symbol = symbols.iter().find(|sym| matches!(sym, Symbol::Function { .. }));
    assert!(function_symbol.is_some(), "Function symbol not found");
    assert!(function_symbol.unwrap().is_public(), "Function should be public");
}

#[test]
fn test_resolve_variable_binding() {
    let span = Span { start: 0, end: 0 };
    
    // Create a let expression: let x = 42;
    let let_expr = Expr {
        kind: ExprKind::Let {
            pattern: Pattern {
                kind: PatternKind::Identifier(Ident("x".to_string())),
                span,
            },
            type_ann: None,
            value: Box::new(Expr {
                kind: ExprKind::Literal(parallax_lang::ast::common::Literal::Int(42)),
                span,
            }),
        },
        span,
    };
    
    // Create a function that uses the variable: fn test() = { let x = 42; x };
    let function = Function {
        name: Ident("test".to_string()),
        generic_params: None,
        params: vec![],
        return_type: None,
        where_clause: None,
        body: Box::new(Expr {
            kind: ExprKind::Block(vec![
                let_expr,
                Expr {
                    kind: ExprKind::Path(vec![Ident("x".to_string())]),
                    span,
                },
            ]),
            span,
        }),
        span,
    };
    
    // Create the item
    let item = Item {
        kind: ItemKind::Function(function),
        visibility: true,
        span,
    };
    
    // Create a resolver
    let mut resolver = Resolver::new(
        PathBuf::from("<test>"),
        "fn test() = { let x = 42; x };".to_string(),
    );
    
    // Resolve the item
    let result = resolver.resolve(&item);
    
    // Check that resolution succeeded
    assert!(result.is_ok(), "Resolution failed: {:?}", result.err());
}

#[test]
fn test_undefined_variable() {
    let span = Span { start: 0, end: 0 };
    
    // Create a function that uses an undefined variable: fn test() = { y };
    let function = Function {
        name: Ident("test".to_string()),
        generic_params: None,
        params: vec![],
        return_type: None,
        where_clause: None,
        body: Box::new(Expr {
            kind: ExprKind::Path(vec![Ident("y".to_string())]),
            span,
        }),
        span,
    };
    
    // Create the item
    let item = Item {
        kind: ItemKind::Function(function),
        visibility: true,
        span,
    };
    
    // Create a resolver
    let mut resolver = Resolver::new(
        PathBuf::from("<test>"),
        "fn test() = { y };".to_string(),
    );
    
    // Resolve the item
    let result = resolver.resolve(&item);
    
    // Check that resolution failed with undefined variable error
    assert!(result.is_err(), "Expected resolution to fail");
    
    match result.err().unwrap() {
        ResolveError::UndefinedName { name, .. } => {
            assert_eq!(name, "y", "Expected error for undefined variable 'y'");
        }
        err => panic!("Unexpected error: {:?}", err),
    }
} 