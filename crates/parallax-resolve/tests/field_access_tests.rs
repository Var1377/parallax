use parallax_lang::ast::{
    common::{Ident, Span, Literal},
    expr::{Expr, ExprKind},
    items::{Item, ItemKind, StructDef, StructField},
    pattern::{Pattern, PatternKind},
};
use parallax_resolve::{
    resolver::Resolver,
    error::ResolveError,
};
use std::path::PathBuf;

fn create_test_struct() -> Item {
    // Create a test struct: struct Point { x: i32, y: i32 }
    let struct_def = StructDef {
        name: Ident("Point".to_string()),
        generic_params: None,
        where_clause: None,
        fields: vec![
            StructField {
                name: Ident("x".to_string()),
                ty: parallax_lang::ast::types::Type {
                    kind: parallax_lang::ast::types::TypeKind::Path(vec![Ident("i32".to_string())]),
                    span: Span { start: 0, end: 0 },
                },
                visibility: true,
                span: Span { start: 0, end: 0 },
            },
            StructField {
                name: Ident("y".to_string()),
                ty: parallax_lang::ast::types::Type {
                    kind: parallax_lang::ast::types::TypeKind::Path(vec![Ident("i32".to_string())]),
                    span: Span { start: 0, end: 0 },
                },
                visibility: true,
                span: Span { start: 0, end: 0 },
            },
        ],
        span: Span { start: 0, end: 0 },
    };
    
    Item {
        kind: ItemKind::Struct(struct_def),
        visibility: true,
        span: Span { start: 0, end: 0 },
    }
}

fn create_field_access_expr(object_expr: Expr, field_name: &str) -> Expr {
    Expr {
        kind: ExprKind::Field {
            object: Box::new(object_expr),
            name: Ident(field_name.to_string()),
        },
        span: Span { start: 0, end: 0 },
    }
}

#[test]
fn test_valid_field_access() {
    // Create a struct
    let struct_item = create_test_struct();
    
    // Create a let binding for 'p'
    let let_expr = Expr {
        kind: ExprKind::Let {
            pattern: Pattern {
                kind: PatternKind::Identifier(Ident("p".to_string())),
                span: Span { start: 0, end: 0 },
            },
            type_ann: None,
            value: Box::new(Expr {
                kind: ExprKind::Struct {
                    path: vec![Ident("Point".to_string())],
                    fields: vec![
                        (Ident("x".to_string()), Expr {
                            kind: ExprKind::Literal(Literal::Int(1)),
                            span: Span { start: 0, end: 0 },
                        }),
                        (Ident("y".to_string()), Expr {
                            kind: ExprKind::Literal(Literal::Int(2)),
                            span: Span { start: 0, end: 0 },
                        }),
                    ],
                    base: None,
                },
                span: Span { start: 0, end: 0 },
            }),
        },
        span: Span { start: 0, end: 0 },
    };
    
    // Create an expression that accesses a field on an instance of the struct
    // p.x where p is a variable of type Point
    let var_expr = Expr {
        kind: ExprKind::Path(vec![Ident("p".to_string())]),
        span: Span { start: 0, end: 0 },
    };
    
    let field_access = create_field_access_expr(var_expr, "x");
    
    // Create a function that uses the field access
    let function = parallax_lang::ast::items::Function {
        name: Ident("test_function".to_string()),
        generic_params: None,
        params: vec![],
        return_type: None,
        where_clause: None,
        body: Box::new(Expr {
            kind: ExprKind::Block(vec![let_expr, field_access]),
            span: Span { start: 0, end: 0 },
        }),
        span: Span { start: 0, end: 0 },
    };
    
    let function_item = Item {
        kind: ItemKind::Function(function),
        visibility: true,
        span: Span { start: 0, end: 0 },
    };
    
    // Create a module containing both the struct and the function
    let module = parallax_lang::ast::items::Module {
        name: Ident("test_module".to_string()),
        items: vec![struct_item, function_item],
        span: Span { start: 0, end: 0 },
    };
    
    let module_item = Item {
        kind: ItemKind::Module(module),
        visibility: true,
        span: Span { start: 0, end: 0 },
    };
    
    // Create a resolver
    let mut resolver = Resolver::new(
        PathBuf::from("test.plx"),
        "// Field access test".to_string(),
    );
    
    // Resolve the module
    let result = resolver.resolve(&module_item);
    
    // The resolution should succeed without errors
    assert!(result.is_ok(), "Resolution failed: {:?}", result.err());
    
    // Ensure no diagnostics were generated
    let resolved_crate = result.unwrap();
    assert!(resolved_crate.diagnostics.is_empty(), 
            "Unexpected diagnostics: {:?}", resolved_crate.diagnostics);
}

#[test]
fn test_invalid_field_access() {
    // Create a struct
    let struct_item = create_test_struct();
    
    // Create a let binding for 'p'
    let let_expr = Expr {
        kind: ExprKind::Let {
            pattern: Pattern {
                kind: PatternKind::Identifier(Ident("p".to_string())),
                span: Span { start: 0, end: 0 },
            },
            type_ann: None,
            value: Box::new(Expr {
                kind: ExprKind::Struct {
                    path: vec![Ident("Point".to_string())],
                    fields: vec![
                        (Ident("x".to_string()), Expr {
                            kind: ExprKind::Literal(Literal::Int(1)),
                            span: Span { start: 0, end: 0 },
                        }),
                        (Ident("y".to_string()), Expr {
                            kind: ExprKind::Literal(Literal::Int(2)),
                            span: Span { start: 0, end: 0 },
                        }),
                    ],
                    base: None,
                },
                span: Span { start: 0, end: 0 },
            }),
        },
        span: Span { start: 0, end: 0 },
    };
    
    // Create an expression that accesses a non-existent field on an instance of the struct
    // p.z where p is a variable of type Point and z is not a field
    let var_expr = Expr {
        kind: ExprKind::Path(vec![Ident("p".to_string())]),
        span: Span { start: 0, end: 0 },
    };
    
    let field_access = create_field_access_expr(var_expr, "z");
    
    // Create a function that uses the field access
    let function = parallax_lang::ast::items::Function {
        name: Ident("test_function".to_string()),
        generic_params: None,
        params: vec![],
        return_type: None,
        where_clause: None,
        body: Box::new(Expr {
            kind: ExprKind::Block(vec![let_expr, field_access]),
            span: Span { start: 0, end: 0 },
        }),
        span: Span { start: 0, end: 0 },
    };
    
    let function_item = Item {
        kind: ItemKind::Function(function),
        visibility: true,
        span: Span { start: 0, end: 0 },
    };
    
    // Create a module containing both the struct and the function
    let module = parallax_lang::ast::items::Module {
        name: Ident("test_module".to_string()),
        items: vec![struct_item, function_item],
        span: Span { start: 0, end: 0 },
    };
    
    let module_item = Item {
        kind: ItemKind::Module(module),
        visibility: true,
        span: Span { start: 0, end: 0 },
    };
    
    // Create a resolver
    let mut resolver = Resolver::new(
        PathBuf::from("test.plx"),
        "// Field access test".to_string(),
    );
    
    // Resolve the module
    let result = resolver.resolve(&module_item);
    
    // The resolution should succeed since field validation is deferred to type checking
    assert!(result.is_ok(), "Resolution failed unexpectedly: {:?}", result.err());
    
    // For field access tests, we don't expect diagnostics during name resolution
    // Field validation will happen during type checking
    println!("test_invalid_field_access: Field validation would happen during type checking.");
}

#[test]
fn test_empty_field_name() {
    // Create a struct
    let struct_item = create_test_struct();
    
    // Create a let binding for 'p'
    let let_expr = Expr {
        kind: ExprKind::Let {
            pattern: Pattern {
                kind: PatternKind::Identifier(Ident("p".to_string())),
                span: Span { start: 0, end: 0 },
            },
            type_ann: None,
            value: Box::new(Expr {
                kind: ExprKind::Struct {
                    path: vec![Ident("Point".to_string())],
                    fields: vec![
                        (Ident("x".to_string()), Expr {
                            kind: ExprKind::Literal(Literal::Int(1)),
                            span: Span { start: 0, end: 0 },
                        }),
                        (Ident("y".to_string()), Expr {
                            kind: ExprKind::Literal(Literal::Int(2)),
                            span: Span { start: 0, end: 0 },
                        }),
                    ],
                    base: None,
                },
                span: Span { start: 0, end: 0 },
            }),
        },
        span: Span { start: 0, end: 0 },
    };
    
    // Create an expression that accesses a field with an empty name
    let var_expr = Expr {
        kind: ExprKind::Path(vec![Ident("p".to_string())]),
        span: Span { start: 0, end: 0 },
    };
    
    let field_access = create_field_access_expr(var_expr, "");
    
    // Create a function that uses the field access
    let function = parallax_lang::ast::items::Function {
        name: Ident("test_function".to_string()),
        generic_params: None,
        params: vec![],
        return_type: None,
        where_clause: None,
        body: Box::new(Expr {
            kind: ExprKind::Block(vec![let_expr, field_access]),
            span: Span { start: 0, end: 0 },
        }),
        span: Span { start: 0, end: 0 },
    };
    
    let function_item = Item {
        kind: ItemKind::Function(function),
        visibility: true,
        span: Span { start: 0, end: 0 },
    };
    
    // Create a module containing both the struct and the function
    let module = parallax_lang::ast::items::Module {
        name: Ident("test_module".to_string()),
        items: vec![struct_item, function_item],
        span: Span { start: 0, end: 0 },
    };
    
    let module_item = Item {
        kind: ItemKind::Module(module),
        visibility: true,
        span: Span { start: 0, end: 0 },
    };
    
    // Create a resolver
    let mut resolver = Resolver::new(
        PathBuf::from("test.plx"),
        "// Field access test".to_string(),
    );
    
    // Resolve the module
    let result = resolver.resolve(&module_item);
    
    // The resolution should still succeed but with diagnostics for empty field name
    assert!(result.is_ok(), "Resolution failed: {:?}", result.err());
    
    // Empty field names are caught during name resolution
    let resolved_crate = result.unwrap();
    
    // Check that the diagnostic is for an empty field name
    let has_empty_field_error = resolved_crate.diagnostics.iter().any(|err| {
        match err {
            ResolveError::GenericError { message, .. } => {
                message.contains("Empty field name")
            },
            _ => false,
        }
    });
    
    assert!(has_empty_field_error, "Expected an empty field name error");
}

#[test]
fn test_field_access_on_non_struct() {
    // Create a function item to use as a non-struct type
    let test_function_item = Item {
        kind: ItemKind::Function(parallax_lang::ast::items::Function {
            name: Ident("test_function".to_string()),
            generic_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: Box::new(Expr {
                kind: ExprKind::Block(vec![]),
                span: Span { start: 0, end: 0 },
            }),
            span: Span { start: 0, end: 0 },
        }),
        visibility: true,
        span: Span { start: 0, end: 0 },
    };
    
    // Create a let binding for 'f'
    let let_expr = Expr {
        kind: ExprKind::Let {
            pattern: Pattern {
                kind: PatternKind::Identifier(Ident("f".to_string())),
                span: Span { start: 0, end: 0 },
            },
            type_ann: None,
            value: Box::new(Expr {
                kind: ExprKind::Path(vec![Ident("test_function".to_string())]),
                span: Span { start: 0, end: 0 },
            }),
        },
        span: Span { start: 0, end: 0 },
    };
    
    // Create an expression that tries to access a field on a function
    // f.field where f is a function
    let var_expr = Expr {
        kind: ExprKind::Path(vec![Ident("f".to_string())]),
        span: Span { start: 0, end: 0 },
    };
    
    let field_access = create_field_access_expr(var_expr, "field");
    
    // Create a function that uses the field access
    let test_access_function = parallax_lang::ast::items::Function {
        name: Ident("function_with_field_access".to_string()),
        generic_params: None,
        params: vec![],
        return_type: None,
        where_clause: None,
        body: Box::new(Expr {
            kind: ExprKind::Block(vec![let_expr, field_access]),
            span: Span { start: 0, end: 0 },
        }),
        span: Span { start: 0, end: 0 },
    };
    
    let access_function_item = Item {
        kind: ItemKind::Function(test_access_function),
        visibility: true,
        span: Span { start: 0, end: 0 },
    };
    
    let module = parallax_lang::ast::items::Module {
        name: Ident("test_module".to_string()),
        items: vec![test_function_item, access_function_item],
        span: Span { start: 0, end: 0 },
    };
    
    let module_item = Item {
        kind: ItemKind::Module(module),
        visibility: true,
        span: Span { start: 0, end: 0 },
    };
    
    // Create a resolver
    let mut resolver = Resolver::new(
        PathBuf::from("test.plx"),
        "// Field access test".to_string(),
    );
    
    // Resolve the module
    let result = resolver.resolve(&module_item);
    
    // The resolution should succeed since field validation is deferred to type checking
    assert!(result.is_ok(), "Resolution failed unexpectedly: {:?}", result.err());
    
    // For field access tests, we don't expect diagnostics during name resolution 
    // Field validation will happen during type checking
    println!("test_field_access_on_non_struct: Field validation would happen during type checking.");
}

#[test]
fn test_field_access_with_variable_binding() {
    // Create a struct
    let struct_item = create_test_struct();
    
    // Create a let expression that binds a Point variable
    let struct_init = Expr {
        kind: ExprKind::Struct {
            path: vec![Ident("Point".to_string())],
            fields: vec![
                (Ident("x".to_string()), Expr {
                    kind: ExprKind::Literal(Literal::Int(1)),
                    span: Span { start: 0, end: 0 },
                }),
                (Ident("y".to_string()), Expr {
                    kind: ExprKind::Literal(Literal::Int(2)),
                    span: Span { start: 0, end: 0 },
                }),
            ],
            base: None,
        },
        span: Span { start: 0, end: 0 },
    };
    
    let let_expr = Expr {
        kind: ExprKind::Let {
            pattern: Pattern {
                kind: PatternKind::Identifier(Ident("point".to_string())),
                span: Span { start: 0, end: 0 },
            },
            type_ann: None,
            value: Box::new(struct_init),
        },
        span: Span { start: 0, end: 0 },
    };
    
    // Create field access expressions for both fields
    let x_access = create_field_access_expr(
        Expr {
            kind: ExprKind::Path(vec![Ident("point".to_string())]),
            span: Span { start: 0, end: 0 },
        },
        "x"
    );
    
    let y_access = create_field_access_expr(
        Expr {
            kind: ExprKind::Path(vec![Ident("point".to_string())]),
            span: Span { start: 0, end: 0 },
        },
        "y"
    );
    
    // Create a z_access which doesn't exist on Point
    let z_access = create_field_access_expr(
        Expr {
            kind: ExprKind::Path(vec![Ident("point".to_string())]),
            span: Span { start: 0, end: 0 },
        },
        "z"
    );
    
    // Create a function that uses all field accesses
    let function = parallax_lang::ast::items::Function {
        name: Ident("test_function".to_string()),
        generic_params: None,
        params: vec![],
        return_type: None,
        where_clause: None,
        body: Box::new(Expr {
            kind: ExprKind::Block(vec![
                let_expr,
                x_access,   // Valid
                y_access,   // Valid
                z_access,   // Invalid
            ]),
            span: Span { start: 0, end: 0 },
        }),
        span: Span { start: 0, end: 0 },
    };
    
    let function_item = Item {
        kind: ItemKind::Function(function),
        visibility: true,
        span: Span { start: 0, end: 0 },
    };
    
    // Create a module containing both the struct and the function
    let module = parallax_lang::ast::items::Module {
        name: Ident("test_module".to_string()),
        items: vec![struct_item, function_item],
        span: Span { start: 0, end: 0 },
    };
    
    let module_item = Item {
        kind: ItemKind::Module(module),
        visibility: true,
        span: Span { start: 0, end: 0 },
    };
    
    // Create a resolver
    let mut resolver = Resolver::new(
        PathBuf::from("test.plx"),
        "// Field access test".to_string(),
    );
    
    // Resolve the module
    let result = resolver.resolve(&module_item);
    
    // The resolution should succeed since field validation is deferred to type checking
    assert!(result.is_ok(), "Resolution failed unexpectedly: {:?}", result.err());
    
    // For field access tests, we don't expect diagnostics during name resolution
    // Field validation will happen during type checking
    println!("test_field_access_with_variable_binding: Field validation would happen during type checking.");
} 