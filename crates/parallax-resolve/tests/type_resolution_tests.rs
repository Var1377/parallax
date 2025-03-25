
use parallax_lang::ast::{
    common::{Ident, Span},
    types::{Type, TypeKind},
};

use parallax_resolve::types::{ResolvedType, PrimitiveType, TypeResolver};

#[test]
fn test_resolve_primitive_types() {
    let mut type_resolver = TypeResolver::new();
    let symbol_table = parallax_resolve::symbol::SymbolTable::default();
    
    // Create a simple type AST for each primitive type
    let i32_type = Type {
        kind: TypeKind::Path(vec![Ident("i32".to_string())]),
        span: Span { start: 0, end: 0 },
    };
    
    let bool_type = Type {
        kind: TypeKind::Path(vec![Ident("bool".to_string())]),
        span: Span { start: 0, end: 0 },
    };
    
    let string_type = Type {
        kind: TypeKind::Path(vec![Ident("string".to_string())]),
        span: Span { start: 0, end: 0 },
    };
    
    // Resolve each type
    let resolved_i32 = type_resolver.resolve_type(&i32_type, &symbol_table).unwrap();
    let resolved_bool = type_resolver.resolve_type(&bool_type, &symbol_table).unwrap();
    let resolved_string = type_resolver.resolve_type(&string_type, &symbol_table).unwrap();
    
    // Check that they resolved to the correct primitive types
    assert!(matches!(resolved_i32, ResolvedType::Primitive(PrimitiveType::I32)));
    assert!(matches!(resolved_bool, ResolvedType::Primitive(PrimitiveType::Bool)));
    assert!(matches!(resolved_string, ResolvedType::Primitive(PrimitiveType::String)));
}

#[test]
fn test_resolve_complex_types() {
    let mut type_resolver = TypeResolver::new();
    let symbol_table = parallax_resolve::symbol::SymbolTable::default();
    
    // Create a function type: i32 -> bool
    let fn_type = Type {
        kind: TypeKind::Function(
            Box::new(Type {
                kind: TypeKind::Path(vec![Ident("i32".to_string())]),
                span: Span { start: 0, end: 0 },
            }),
            Box::new(Type {
                kind: TypeKind::Path(vec![Ident("bool".to_string())]),
                span: Span { start: 0, end: 0 },
            }),
        ),
        span: Span { start: 0, end: 0 },
    };
    
    // Create a tuple type: (i32, bool, string)
    let tuple_type = Type {
        kind: TypeKind::Tuple(vec![
            Type {
                kind: TypeKind::Path(vec![Ident("i32".to_string())]),
                span: Span { start: 0, end: 0 },
            },
            Type {
                kind: TypeKind::Path(vec![Ident("bool".to_string())]),
                span: Span { start: 0, end: 0 },
            },
            Type {
                kind: TypeKind::Path(vec![Ident("string".to_string())]),
                span: Span { start: 0, end: 0 },
            },
        ]),
        span: Span { start: 0, end: 0 },
    };
    
    // Create an array type: [i32; 10]
    let array_type = Type {
        kind: TypeKind::Array(
            Box::new(Type {
                kind: TypeKind::Path(vec![Ident("i32".to_string())]),
                span: Span { start: 0, end: 0 },
            }),
            10,
        ),
        span: Span { start: 0, end: 0 },
    };
    
    // Resolve each type
    let resolved_fn = type_resolver.resolve_type(&fn_type, &symbol_table).unwrap();
    let resolved_tuple = type_resolver.resolve_type(&tuple_type, &symbol_table).unwrap();
    let resolved_array = type_resolver.resolve_type(&array_type, &symbol_table).unwrap();
    
    // Check function type
    if let ResolvedType::Function { param, ret, .. } = resolved_fn {
        assert!(matches!(*param, ResolvedType::Primitive(PrimitiveType::I32)));
        assert!(matches!(*ret, ResolvedType::Primitive(PrimitiveType::Bool)));
    } else {
        panic!("Expected function type, got {:?}", resolved_fn);
    }
    
    // Check tuple type
    if let ResolvedType::Tuple { elements, .. } = resolved_tuple {
        assert_eq!(elements.len(), 3);
        assert!(matches!(elements[0], ResolvedType::Primitive(PrimitiveType::I32)));
        assert!(matches!(elements[1], ResolvedType::Primitive(PrimitiveType::Bool)));
        assert!(matches!(elements[2], ResolvedType::Primitive(PrimitiveType::String)));
    } else {
        panic!("Expected tuple type, got {:?}", resolved_tuple);
    }
    
    // Check array type
    if let ResolvedType::Array { element, size, .. } = resolved_array {
        assert_eq!(size, 10);
        assert!(matches!(*element, ResolvedType::Primitive(PrimitiveType::I32)));
    } else {
        panic!("Expected array type, got {:?}", resolved_array);
    }
}

#[test]
fn test_type_compatibility() {
    let type_resolver = TypeResolver::new();
    let env = type_resolver.env;
    
    // Create some types to check compatibility
    let i32_type = ResolvedType::Primitive(PrimitiveType::I32);
    let i32_type2 = ResolvedType::Primitive(PrimitiveType::I32);
    let i64_type = ResolvedType::Primitive(PrimitiveType::I64);
    
    // Same types should be compatible
    assert!(env.check_compatibility(&i32_type, &i32_type2));
    
    // Different types should not be compatible
    assert!(!env.check_compatibility(&i32_type, &i64_type));
    
    // More complex type compatibility
    let fn_i32_bool = ResolvedType::Function {
        param: Box::new(i32_type.clone()),
        ret: Box::new(ResolvedType::Primitive(PrimitiveType::Bool)),
        span: Span { start: 0, end: 0 },
    };
    
    let fn_i32_bool2 = ResolvedType::Function {
        param: Box::new(i32_type.clone()),
        ret: Box::new(ResolvedType::Primitive(PrimitiveType::Bool)),
        span: Span { start: 0, end: 0 },
    };
    
    let fn_i64_bool = ResolvedType::Function {
        param: Box::new(i64_type.clone()),
        ret: Box::new(ResolvedType::Primitive(PrimitiveType::Bool)),
        span: Span { start: 0, end: 0 },
    };
    
    // Same function types should be compatible
    assert!(env.check_compatibility(&fn_i32_bool, &fn_i32_bool2));
    
    // Different function types should not be compatible
    assert!(!env.check_compatibility(&fn_i32_bool, &fn_i64_bool));
} 