use parallax_resolve::error::ResolveError;
use parallax_resolve::symbol::{Symbol, SymbolTable};
use parallax_resolve::imports::ImportResolver;
use parallax_lang::ast::common::{Ident, Span};
use parallax_lang::ast::items::{UseDecl, UseTree, UseTreeKind};
use std::path::PathBuf;
use std::sync::Arc;

#[test]
fn test_undefined_name_error() {
    // Create a simple version of the undefined name error
    let error = ResolveError::undefined_name(
        "unknown_symbol".to_string(),
        Span { start: 10, end: 20 },
        PathBuf::from("test.plx"),
        "let x = unknown_symbol;".to_string(),
    );
    
    // Check the error fields
    match &error {
        ResolveError::UndefinedName { name, .. } => {
            assert_eq!(name, "unknown_symbol");
        },
        _ => panic!("Expected UndefinedName error"),
    }
    
    // Test equality for error comparison (used by Salsa for caching)
    let error2 = ResolveError::undefined_name(
        "unknown_symbol".to_string(),
        Span { start: 10, end: 20 },
        PathBuf::from("test.plx"),
        "let x = unknown_symbol;".to_string(),
    );
    
    assert_eq!(error, error2);
    
    // Different name should not be equal
    let error3 = ResolveError::undefined_name(
        "different_symbol".to_string(),
        Span { start: 10, end: 20 },
        PathBuf::from("test.plx"),
        "let x = unknown_symbol;".to_string(),
    );
    
    assert_ne!(error, error3);
}

#[test]
fn test_ambiguous_reference_error() {
    // Create a simple version of the ambiguous reference error
    let error = ResolveError::ambiguous_reference(
        "ambiguous".to_string(),
        vec!["module1::ambiguous".to_string(), "module2::ambiguous".to_string()],
        Span { start: 10, end: 20 },
        PathBuf::from("test.plx"),
        "let x = ambiguous;".to_string(),
    );
    
    // Check the error fields
    match &error {
        ResolveError::AmbiguousReference { name, candidates, .. } => {
            assert_eq!(name, "ambiguous");
            assert_eq!(candidates.len(), 2);
            assert!(candidates.contains(&"module1::ambiguous".to_string()));
            assert!(candidates.contains(&"module2::ambiguous".to_string()));
        },
        _ => panic!("Expected AmbiguousReference error"),
    }
    
    // Test equality
    let error2 = ResolveError::ambiguous_reference(
        "ambiguous".to_string(),
        vec!["module1::ambiguous".to_string(), "module2::ambiguous".to_string()],
        Span { start: 10, end: 20 },
        PathBuf::from("test.plx"),
        "let x = ambiguous;".to_string(),
    );
    
    assert_eq!(error, error2);
}

#[test]
fn test_duplicate_definition_error() {
    // Create a simple version of the duplicate definition error
    let error = ResolveError::duplicate_definition(
        "duplicate".to_string(),
        Span { start: 50, end: 60 },
        Span { start: 10, end: 20 },
        PathBuf::from("test.plx"),
        "fn duplicate() {}\nfn duplicate() {}".to_string(),
    );
    
    // Check the error fields
    match &error {
        ResolveError::DuplicateDefinition { name, .. } => {
            assert_eq!(name, "duplicate");
        },
        _ => panic!("Expected DuplicateDefinition error"),
    }
    
    // Test equality
    let error2 = ResolveError::duplicate_definition(
        "duplicate".to_string(),
        Span { start: 50, end: 60 },
        Span { start: 10, end: 20 },
        PathBuf::from("test.plx"),
        "fn duplicate() {}\nfn duplicate() {}".to_string(),
    );
    
    assert_eq!(error, error2);
}

#[test]
fn test_visibility_violation_error() {
    // Create a simple version of the visibility violation error
    let error = ResolveError::visibility_violation(
        "private_item".to_string(),
        Span { start: 10, end: 20 },
        PathBuf::from("test.plx"),
        "let x = module::private_item;".to_string(),
    );
    
    // Check the error fields
    match &error {
        ResolveError::VisibilityViolation { name, .. } => {
            assert_eq!(name, "private_item");
        },
        _ => panic!("Expected VisibilityViolation error"),
    }
    
    // Test equality
    let error2 = ResolveError::visibility_violation(
        "private_item".to_string(),
        Span { start: 10, end: 20 },
        PathBuf::from("test.plx"),
        "let x = module::private_item;".to_string(),
    );
    
    assert_eq!(error, error2);
}

#[test]
fn test_cyclic_dependency_error() {
    // Create a simple version of the cyclic dependency error
    let error = ResolveError::cyclic_dependency(
        Span { start: 10, end: 20 },
        PathBuf::from("test.plx"),
        "use a::b;\nuse b::a;".to_string(),
    );
    
    // Test equality
    let error2 = ResolveError::cyclic_dependency(
        Span { start: 10, end: 20 },
        PathBuf::from("test.plx"),
        "use a::b;\nuse b::a;".to_string(),
    );
    
    assert_eq!(error, error2);
}

#[test]
fn test_import_error() {
    // Create a simple version of the import error
    let error = ResolveError::import_error(
        "missing_module".to_string(),
        "Module not found".to_string(),
        Span { start: 10, end: 20 },
        PathBuf::from("test.plx"),
        "use missing_module::item;".to_string(),
    );
    
    // Check the error fields
    match &error {
        ResolveError::ImportError { name, reason, .. } => {
            assert_eq!(name, "missing_module");
            assert_eq!(reason, "Module not found");
        },
        _ => panic!("Expected ImportError error"),
    }
    
    // Test equality
    let error2 = ResolveError::import_error(
        "missing_module".to_string(),
        "Module not found".to_string(),
        Span { start: 10, end: 20 },
        PathBuf::from("test.plx"),
        "use missing_module::item;".to_string(),
    );
    
    assert_eq!(error, error2);
}

#[test]
fn test_generic_error() {
    // Create a simple version of the generic error
    let error = ResolveError::generic_error(
        "Generic error message".to_string(),
        Span { start: 10, end: 20 },
        PathBuf::from("test.plx"),
        "let x = 10;".to_string(),
    );
    
    // Check the error fields
    match &error {
        ResolveError::GenericError { message, .. } => {
            assert_eq!(message, "Generic error message");
        },
        _ => panic!("Expected GenericError error"),
    }
    
    // Test equality
    let error2 = ResolveError::generic_error(
        "Generic error message".to_string(),
        Span { start: 10, end: 20 },
        PathBuf::from("test.plx"),
        "let x = 10;".to_string(),
    );
    
    assert_eq!(error, error2);
}

#[test]
fn test_import_resolver_error_handling() {
    let mut symbol_table = SymbolTable::new();
    
    // Create an import that refers to a non-existent module
    let import_tree = UseTree {
        kind: UseTreeKind::Path {
            segment: Ident("nonexistent_module".to_string()),
            alias: None,
            sub_tree: Some(Box::new(UseTree {
                kind: UseTreeKind::Path {
                    segment: Ident("some_item".to_string()),
                    alias: None,
                    sub_tree: None,
                },
                span: Span { start: 5, end: 15 },
            })),
        },
        span: Span { start: 0, end: 20 },
    };
    
    let use_decl = UseDecl {
        tree: import_tree,
        span: Span { start: 0, end: 20 },
    };
    
    // Sample source code for error reporting
    let source_code = "use nonexistent_module::some_item;".to_string();
    
    // Resolve the import, which should fail
    let import_resolver = ImportResolver::new_with_source(
        PathBuf::from("test.plx"),
        "test".to_string(),
        source_code,
    );
    
    let current_scope = symbol_table.current_scope();
    let result = import_resolver.resolve_use_decl(
        &use_decl,
        current_scope,
        &mut symbol_table
    );
    
    // Check that the result is an error
    assert!(result.is_err());
    match result {
        Err(error) => {
            match &error {
                ResolveError::ImportError { name, src, .. } => {
                    assert_eq!(name, "nonexistent_module");
                    // Verify that the source code is included in the error
                    assert_eq!(src.name(), "test.plx");
                    // Just check the file name to confirm we have source information
                    assert_eq!(src.name(), "test.plx");
                },
                _ => panic!("Expected ImportError"),
            }
        },
        _ => panic!("Expected error result"),
    }
}

#[test]
fn test_private_symbol_import_error() {
    let mut symbol_table = SymbolTable::new();
    
    // Create a module with a private symbol
    let (_module_scope, module_id) = symbol_table.push_module();
    
    let private_symbol = Symbol::Function {
        name: Ident("private_func".to_string()),
        sig: Arc::new(parallax_lang::ast::items::Function {
            name: Ident("private_func".to_string()),
            generic_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: Box::new(parallax_lang::ast::Expr {
                kind: parallax_lang::ast::expr::ExprKind::Block(vec![]),
                span: Span { start: 0, end: 0 },
            }),
            span: Span { start: 0, end: 0 },
        }),
        span: Span { start: 5, end: 15 },
        is_public: false,  // Private!
        defined_in: symbol_table.current_scope(),
    };
    
    symbol_table.add_symbol(private_symbol);
    symbol_table.pop_scope();  // Return to root scope
    
    let module_symbol = Symbol::Module {
        name: Ident("test_module".to_string()),
        id: module_id,
        span: Span { start: 0, end: 0 },
        is_public: true,
        defined_in: symbol_table.current_scope(),
    };
    
    symbol_table.add_symbol(module_symbol);
    
    // Create an import that tries to import the private symbol
    let import_tree = UseTree {
        kind: UseTreeKind::Path {
            segment: Ident("test_module".to_string()),
            alias: None,
            sub_tree: Some(Box::new(UseTree {
                kind: UseTreeKind::Path {
                    segment: Ident("private_func".to_string()),
                    alias: None,
                    sub_tree: None,
                },
                span: Span { start: 5, end: 15 },
            })),
        },
        span: Span { start: 0, end: 20 },
    };
    
    let use_decl = UseDecl {
        tree: import_tree,
        span: Span { start: 0, end: 20 },
    };
    
    // Sample source code for error reporting
    let source_code = "use test_module::private_func;".to_string();
    
    // Resolve the import
    let import_resolver = ImportResolver::new_with_source(
        PathBuf::from("test.plx"),
        "test".to_string(),
        source_code,
    );
    
    let current_scope = symbol_table.current_scope();
    let result = import_resolver.resolve_use_decl(
        &use_decl,
        current_scope,
        &mut symbol_table
    );
    
    // The enhanced implementation should reject private symbols
    assert!(result.is_err(), "Import resolution should fail for private symbols");
    
    if let Err(error) = result {
        match &error {
            ResolveError::ImportError { name, reason, .. } => {
                assert_eq!(name, "private_func", "Error should reference the private symbol name");
                assert!(reason.contains("private"), "Error reason should mention the symbol is private");
            },
            _ => panic!("Expected ImportError"),
        }
    }
    
    // Verify that the symbol wasn't imported
    let imported_symbols = symbol_table.lookup("private_func");
    assert_eq!(imported_symbols.len(), 0, "Private symbol should not be imported");
} 