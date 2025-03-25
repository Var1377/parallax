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
    
    // Resolve the import, which should fail
    let mut import_resolver = ImportResolver::new(
        &mut symbol_table,
        PathBuf::from("test.plx"),
        "use nonexistent_module::some_item;".to_string(),
    );
    
    let result = import_resolver.resolve_import(&use_decl);
    
    // The result could be either Ok or Err depending on whether errors are returned immediately
    // or accumulated. Instead of checking the result, we'll just check that errors were generated.
    
    // Get the accumulated errors
    let errors = import_resolver.take_errors();
    
    // If the result is Err, that means the error was returned directly
    if result.is_err() {
        // In this case, the errors vector might be empty since the error was returned directly
        assert!(matches!(result.unwrap_err(), ResolveError::ImportError { .. }));
    } else {
        // If the result is Ok, errors should be accumulated in the errors vector
        assert!(!errors.is_empty());
        
        // Should be an ImportError with "Module not found"
        match &errors[0] {
            ResolveError::ImportError { name, reason, .. } => {
                assert_eq!(name, "nonexistent_module");
                assert!(reason.contains("not found"));
            },
            _ => panic!("Expected ImportError"),
        }
    }
}

#[test]
fn test_private_symbol_import_error() {
    let mut symbol_table = SymbolTable::new();
    
    // Create a module with a private symbol
    let (module_scope, module_id) = symbol_table.push_module();
    
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
    
    // Resolve the import
    let mut import_resolver = ImportResolver::new(
        &mut symbol_table,
        PathBuf::from("test.plx"),
        "use test_module::private_func;".to_string(),
    );
    
    let result = import_resolver.resolve_import(&use_decl);
    
    // The result could be either Ok or Err depending on whether errors are returned immediately
    // or accumulated. Instead of checking the result, we'll just check that errors were generated.
    
    // Get the accumulated errors
    let errors = import_resolver.take_errors();
    
    // If the result is Err, that means the error was returned directly
    if result.is_err() {
        // In this case, the errors vector might be empty since the error was returned directly
        assert!(matches!(result.unwrap_err(), ResolveError::VisibilityViolation { .. }));
    } else {
        // If the result is Ok, errors should be accumulated in the errors vector
        assert!(!errors.is_empty());
        
        // Should be a VisibilityViolation error
        match &errors[0] {
            ResolveError::VisibilityViolation { name, .. } => {
                assert_eq!(name, "private_func");
            },
            _ => panic!("Expected VisibilityViolation error"),
        }
    }
} 