use parallax_lang::ast::{
    common::{Span, Literal},
    expr::{Expr, ExprKind, BinaryOp, UnaryOp},
};

use parallax_resolve::{
    symbol::SymbolTable,
    types::{ResolvedType, PrimitiveType, TypeResolver},
};

#[test]
fn test_literal_types() {
    let mut type_resolver = TypeResolver::new();
    let symbol_table = SymbolTable::default();
    
    // Integer literal: 42
    let int_expr = Expr {
        kind: ExprKind::Literal(Literal::Int(42)),
        span: Span { start: 0, end: 0 },
    };
    
    // Float literal: 3.14
    let float_expr = Expr {
        kind: ExprKind::Literal(Literal::Float(3.14)),
        span: Span { start: 0, end: 0 },
    };
    
    // Boolean literal: true
    let bool_expr = Expr {
        kind: ExprKind::Literal(Literal::Bool(true)),
        span: Span { start: 0, end: 0 },
    };
    
    // String literal: "hello"
    let string_expr = Expr {
        kind: ExprKind::Literal(Literal::String("hello".to_string())),
        span: Span { start: 0, end: 0 },
    };
    
    // Resolve and check each type
    let int_type = type_resolver.resolve_expr_type(&int_expr, &symbol_table).unwrap();
    let float_type = type_resolver.resolve_expr_type(&float_expr, &symbol_table).unwrap();
    let bool_type = type_resolver.resolve_expr_type(&bool_expr, &symbol_table).unwrap();
    let string_type = type_resolver.resolve_expr_type(&string_expr, &symbol_table).unwrap();
    
    assert!(matches!(int_type, ResolvedType::Primitive(PrimitiveType::I32)));
    assert!(matches!(float_type, ResolvedType::Primitive(PrimitiveType::F64)));
    assert!(matches!(bool_type, ResolvedType::Primitive(PrimitiveType::Bool)));
    assert!(matches!(string_type, ResolvedType::Primitive(PrimitiveType::String)));
}

#[test]
fn test_binary_op_types() {
    let mut type_resolver = TypeResolver::new();
    let symbol_table = SymbolTable::default();
    
    // Create expressions for operands
    let int_expr = Expr {
        kind: ExprKind::Literal(Literal::Int(42)),
        span: Span { start: 0, end: 0 },
    };
    
    let bool_expr = Expr {
        kind: ExprKind::Literal(Literal::Bool(true)),
        span: Span { start: 0, end: 0 },
    };
    
    // Addition: 42 + 42
    let add_expr = Expr {
        kind: ExprKind::Binary {
            left: Box::new(int_expr.clone()),
            op: BinaryOp::Add,
            right: Box::new(int_expr.clone()),
        },
        span: Span { start: 0, end: 0 },
    };
    
    // Comparison: 42 < 42
    let lt_expr = Expr {
        kind: ExprKind::Binary {
            left: Box::new(int_expr.clone()),
            op: BinaryOp::Lt,
            right: Box::new(int_expr.clone()),
        },
        span: Span { start: 0, end: 0 },
    };
    
    // Logical operation: true && true
    let and_expr = Expr {
        kind: ExprKind::Binary {
            left: Box::new(bool_expr.clone()),
            op: BinaryOp::And,
            right: Box::new(bool_expr.clone()),
        },
        span: Span { start: 0, end: 0 },
    };
    
    // Resolve and check each type
    let add_type = type_resolver.resolve_expr_type(&add_expr, &symbol_table).unwrap();
    let lt_type = type_resolver.resolve_expr_type(&lt_expr, &symbol_table).unwrap();
    let and_type = type_resolver.resolve_expr_type(&and_expr, &symbol_table).unwrap();
    
    assert!(matches!(add_type, ResolvedType::Primitive(PrimitiveType::I32)));
    assert!(matches!(lt_type, ResolvedType::Primitive(PrimitiveType::Bool)));
    assert!(matches!(and_type, ResolvedType::Primitive(PrimitiveType::Bool)));
}

#[test]
fn test_unary_op_types() {
    let mut type_resolver = TypeResolver::new();
    let symbol_table = SymbolTable::default();
    
    // Create expressions for operands
    let int_expr = Expr {
        kind: ExprKind::Literal(Literal::Int(42)),
        span: Span { start: 0, end: 0 },
    };
    
    let bool_expr = Expr {
        kind: ExprKind::Literal(Literal::Bool(true)),
        span: Span { start: 0, end: 0 },
    };
    
    // Negation: -42
    let neg_expr = Expr {
        kind: ExprKind::Unary {
            op: UnaryOp::Neg,
            expr: Box::new(int_expr.clone()),
        },
        span: Span { start: 0, end: 0 },
    };
    
    // Logical not: !true
    let not_expr = Expr {
        kind: ExprKind::Unary {
            op: UnaryOp::Not,
            expr: Box::new(bool_expr.clone()),
        },
        span: Span { start: 0, end: 0 },
    };
    
    // Resolve and check each type
    let neg_type = type_resolver.resolve_expr_type(&neg_expr, &symbol_table).unwrap();
    let not_type = type_resolver.resolve_expr_type(&not_expr, &symbol_table).unwrap();
    
    assert!(matches!(neg_type, ResolvedType::Primitive(PrimitiveType::I32)));
    assert!(matches!(not_type, ResolvedType::Primitive(PrimitiveType::Bool)));
}

#[test]
fn test_if_expression_types() {
    let mut type_resolver = TypeResolver::new();
    let symbol_table = SymbolTable::default();
    
    // Create expressions
    let bool_expr = Expr {
        kind: ExprKind::Literal(Literal::Bool(true)),
        span: Span { start: 0, end: 0 },
    };
    
    let int_expr = Expr {
        kind: ExprKind::Literal(Literal::Int(42)),
        span: Span { start: 0, end: 0 },
    };
    
    // If expression with both branches: if true { 42 } else { 99 }
    let if_expr = Expr {
        kind: ExprKind::If {
            condition: Box::new(bool_expr.clone()),
            then_branch: Box::new(int_expr.clone()),
            else_branch: Some(Box::new(Expr {
                kind: ExprKind::Literal(Literal::Int(99)),
                span: Span { start: 0, end: 0 },
            })),
        },
        span: Span { start: 0, end: 0 },
    };
    
    // If expression without else: if true { 42 }
    let if_without_else = Expr {
        kind: ExprKind::If {
            condition: Box::new(bool_expr.clone()),
            then_branch: Box::new(int_expr.clone()),
            else_branch: None,
        },
        span: Span { start: 0, end: 0 },
    };
    
    // Resolve and check each type
    let if_type = type_resolver.resolve_expr_type(&if_expr, &symbol_table).unwrap();
    let if_without_else_type = type_resolver.resolve_expr_type(&if_without_else, &symbol_table).unwrap();
    
    assert!(matches!(if_type, ResolvedType::Primitive(PrimitiveType::I32)));
    assert!(matches!(if_without_else_type, ResolvedType::Primitive(PrimitiveType::Unit)));
}

#[test]
fn test_block_expression_types() {
    let mut type_resolver = TypeResolver::new();
    let symbol_table = SymbolTable::default();
    
    // Empty block: {}
    let empty_block = Expr {
        kind: ExprKind::Block(vec![]),
        span: Span { start: 0, end: 0 },
    };
    
    // Block with single expression: { 42 }
    let single_expr_block = Expr {
        kind: ExprKind::Block(vec![
            Expr {
                kind: ExprKind::Literal(Literal::Int(42)),
                span: Span { start: 0, end: 0 },
            },
        ]),
        span: Span { start: 0, end: 0 },
    };
    
    // Block with multiple expressions: { "hello"; true; 42 }
    let multi_expr_block = Expr {
        kind: ExprKind::Block(vec![
            Expr {
                kind: ExprKind::Literal(Literal::String("hello".to_string())),
                span: Span { start: 0, end: 0 },
            },
            Expr {
                kind: ExprKind::Literal(Literal::Bool(true)),
                span: Span { start: 0, end: 0 },
            },
            Expr {
                kind: ExprKind::Literal(Literal::Int(42)),
                span: Span { start: 0, end: 0 },
            },
        ]),
        span: Span { start: 0, end: 0 },
    };
    
    // Resolve and check each type
    let empty_block_type = type_resolver.resolve_expr_type(&empty_block, &symbol_table).unwrap();
    let single_expr_block_type = type_resolver.resolve_expr_type(&single_expr_block, &symbol_table).unwrap();
    let multi_expr_block_type = type_resolver.resolve_expr_type(&multi_expr_block, &symbol_table).unwrap();
    
    assert!(matches!(empty_block_type, ResolvedType::Primitive(PrimitiveType::Unit)));
    assert!(matches!(single_expr_block_type, ResolvedType::Primitive(PrimitiveType::I32)));
    assert!(matches!(multi_expr_block_type, ResolvedType::Primitive(PrimitiveType::I32)));
} 