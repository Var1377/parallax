
use parallax_lang::ast::{
    common::{Ident, Span},
    expr::GenericParam,
    items::{WhereClause, WherePredicate},
    types::{Type, TypeKind},
};

use parallax_resolve::{
    symbol::SymbolTable,
    types::{ResolvedType, PrimitiveType, TypeResolver, TypeEnv},
};

/// Create a simple generic type with a type parameter
fn create_generic_type() -> (Type, Type) {
    // Create a generic type Vec<T>
    let vec_type = Type {
        kind: TypeKind::Path(vec![Ident("Vec".to_string())]),
        span: Span { start: 0, end: 0 },
    };
    
    let t_type = Type {
        kind: TypeKind::Path(vec![Ident("T".to_string())]),
        span: Span { start: 0, end: 0 },
    };
    
    let vec_t_type = Type {
        kind: TypeKind::KindApp(
            Box::new(vec_type.clone()),
            vec![t_type.clone()]
        ),
        span: Span { start: 0, end: 0 },
    };
    
    (vec_t_type, t_type)
}

#[test]
fn test_type_substitution() {
    let mut type_env = TypeEnv::new();
    
    // Add a type variable
    type_env.add_type_var("T".to_string(), Span { start: 0, end: 0 });
    
    // Create a type that uses the type variable
    let t_type = ResolvedType::TypeVar {
        name: "T".to_string(),
        span: Span { start: 0, end: 0 },
    };
    
    let function_type = ResolvedType::Function {
        param: Box::new(t_type.clone()),
        ret: Box::new(ResolvedType::Primitive(PrimitiveType::Bool)),
        span: Span { start: 0, end: 0 },
    };
    
    // Substitute T with i32
    type_env.add_substitution(
        "T".to_string(),
        ResolvedType::Primitive(PrimitiveType::I32)
    );
    
    // Apply the substitution
    let result = type_env.apply_substitutions(&function_type);
    
    // Check the result
    match result {
        ResolvedType::Function { param, ret, .. } => {
            assert!(matches!(*param, ResolvedType::Primitive(PrimitiveType::I32)));
            assert!(matches!(*ret, ResolvedType::Primitive(PrimitiveType::Bool)));
        },
        _ => panic!("Expected function type after substitution"),
    }
}

#[test]
fn test_nested_type_substitution() {
    let mut type_env = TypeEnv::new();
    
    // Add type variables
    type_env.add_type_var("T".to_string(), Span { start: 0, end: 0 });
    type_env.add_type_var("U".to_string(), Span { start: 0, end: 0 });
    
    // Create a nested type with type variables
    let t_type = ResolvedType::TypeVar {
        name: "T".to_string(),
        span: Span { start: 0, end: 0 },
    };
    
    let u_type = ResolvedType::TypeVar {
        name: "U".to_string(),
        span: Span { start: 0, end: 0 },
    };
    
    // Create Vec<T>
    let vec_t_type = ResolvedType::Named {
        name: "Vec".to_string(),
        symbol: None,
        args: vec![t_type.clone()],
        span: Span { start: 0, end: 0 },
    };
    
    // Create Map<T, U>
    let map_t_u_type = ResolvedType::Named {
        name: "Map".to_string(),
        symbol: None,
        args: vec![t_type.clone(), u_type.clone()],
        span: Span { start: 0, end: 0 },
    };
    
    // Create nested type: Function<Vec<T>, Map<T, U>>
    let function_type = ResolvedType::Function {
        param: Box::new(vec_t_type),
        ret: Box::new(map_t_u_type),
        span: Span { start: 0, end: 0 },
    };
    
    // Substitute T with i32
    type_env.add_substitution(
        "T".to_string(),
        ResolvedType::Primitive(PrimitiveType::I32)
    );
    
    // Substitute U with string
    type_env.add_substitution(
        "U".to_string(),
        ResolvedType::Primitive(PrimitiveType::String)
    );
    
    // Apply the substitution
    let result = type_env.apply_substitutions(&function_type);
    
    // Check the result
    match result {
        ResolvedType::Function { param, ret, .. } => {
            // Check param: Vec<i32>
            match *param {
                ResolvedType::Named { name, args, .. } => {
                    assert_eq!(name, "Vec");
                    assert_eq!(args.len(), 1);
                    assert!(matches!(args[0], ResolvedType::Primitive(PrimitiveType::I32)));
                },
                _ => panic!("Expected Vec type for parameter"),
            }
            
            // Check ret: Map<i32, String>
            match *ret {
                ResolvedType::Named { name, args, .. } => {
                    assert_eq!(name, "Map");
                    assert_eq!(args.len(), 2);
                    assert!(matches!(args[0], ResolvedType::Primitive(PrimitiveType::I32)));
                    assert!(matches!(args[1], ResolvedType::Primitive(PrimitiveType::String)));
                },
                _ => panic!("Expected Map type for return"),
            }
        },
        _ => panic!("Expected function type after substitution"),
    }
}

#[test]
fn test_process_where_clause() {
    let mut type_resolver = TypeResolver::new();
    let symbol_table = SymbolTable::default();
    
    // Create a type parameter
    let t_param = GenericParam {
        is_phantom: false,
        name: Ident("T".to_string()),
        kind: None,
    };
    
    // Create a where clause: T: Display
    let t_type = Type {
        kind: TypeKind::Path(vec![Ident("T".to_string())]),
        span: Span { start: 0, end: 0 },
    };
    
    let display_type = Type {
        kind: TypeKind::Path(vec![Ident("Display".to_string())]),
        span: Span { start: 0, end: 0 },
    };
    
    let where_clause = WhereClause {
        predicates: vec![
            WherePredicate {
                ty: t_type.clone(),
                bounds: vec![display_type.clone()],
                span: Span { start: 0, end: 0 },
            }
        ],
        span: Span { start: 0, end: 0 },
    };
    
    // Process the where clause
    let generic_params = Some(vec![t_param]);
    type_resolver.process_generic_params(&generic_params, &symbol_table);
    type_resolver.process_where_clause(&Some(where_clause), &symbol_table).unwrap();
    
    // Check that the constraint was added
    // This would require accessing the private constraints field
    // In a real implementation, we would have methods to check constraints
    
    // For now, just ensure no panic occurred during processing
    // This test will expand as we implement more functionality
} 