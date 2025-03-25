use parallax_resolve::imports::ImportResolver;
use parallax_resolve::symbol::{Symbol, SymbolTable};
use parallax_lang::ast::common::{Ident, Span};
use parallax_lang::ast::items::{UseDecl, UseTree, UseTreeKind};
use std::path::PathBuf;
use std::sync::Arc;

#[test]
fn test_simple_import() {
    let mut symbol_table = SymbolTable::new();
    
    // First, create a module with a symbol to import
    let (module_scope, module_id) = symbol_table.push_module();
    
    // Add a symbol to the module
    let target_symbol = Symbol::Function {
        name: Ident("target_func".to_string()),
        sig: Arc::new(parallax_lang::ast::items::Function {
            name: Ident("target_func".to_string()),
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
        span: Span { start: 0, end: 0 },
        is_public: true,
        defined_in: symbol_table.current_scope(),
    };
    
    symbol_table.add_symbol(target_symbol);
    
    // Add the module to the root scope
    symbol_table.pop_scope();  // Return to root scope
    
    let module_symbol = Symbol::Module {
        name: Ident("test_module".to_string()),
        id: module_id,
        span: Span { start: 0, end: 0 },
        is_public: true,
        defined_in: symbol_table.current_scope(),
    };
    
    symbol_table.add_symbol(module_symbol);
    
    // Create a simple import
    let import_tree = UseTree {
        kind: UseTreeKind::Path {
            segment: Ident("test_module".to_string()),
            alias: None,
            sub_tree: Some(Box::new(UseTree {
                kind: UseTreeKind::Path {
                    segment: Ident("target_func".to_string()),
                    alias: None,
                    sub_tree: None,
                },
                span: Span { start: 0, end: 0 },
            })),
        },
        span: Span { start: 0, end: 0 },
    };
    
    let use_decl = UseDecl {
        tree: import_tree,
        span: Span { start: 0, end: 0 },
    };
    
    // Resolve the import
    let import_resolver = ImportResolver::new(
        PathBuf::from("test.plx"),
        "test".to_string(),
    );
    
    let current_scope = symbol_table.current_scope();
    let result = import_resolver.resolve_use_decl(
        &use_decl,
        current_scope,
        &mut symbol_table
    );
    assert!(result.is_ok());
    
    // Check that the symbol is now available in the root scope
    let imported_symbols = symbol_table.lookup("target_func");
    assert_eq!(imported_symbols.len(), 1);
    
    // Should be an Import symbol
    match &imported_symbols[0] {
        Symbol::Import { target, .. } => {
            match &**target {
                Symbol::Function { name, .. } => {
                    assert_eq!(name.0, "target_func");
                },
                _ => panic!("Expected Function target"),
            }
        },
        _ => panic!("Expected Import symbol"),
    }
}

#[test]
fn test_import_with_alias() {
    let mut symbol_table = SymbolTable::new();
    
    // Set up a module with a symbol
    let (module_scope, module_id) = symbol_table.push_module();
    
    let target_symbol = Symbol::Struct {
        name: Ident("TargetStruct".to_string()),
        def: Arc::new(parallax_lang::ast::items::StructDef {
            name: Ident("TargetStruct".to_string()),
            generic_params: None,
            where_clause: None,
            fields: vec![],
            span: Span { start: 0, end: 0 },
        }),
        span: Span { start: 0, end: 0 },
        is_public: true,
        defined_in: symbol_table.current_scope(),
    };
    
    symbol_table.add_symbol(target_symbol);
    symbol_table.pop_scope();  // Return to root scope
    
    let module_symbol = Symbol::Module {
        name: Ident("types".to_string()),
        id: module_id,
        span: Span { start: 0, end: 0 },
        is_public: true,
        defined_in: symbol_table.current_scope(),
    };
    
    symbol_table.add_symbol(module_symbol);
    
    // Create an import with an alias
    let import_tree = UseTree {
        kind: UseTreeKind::Path {
            segment: Ident("types".to_string()),
            alias: None,
            sub_tree: Some(Box::new(UseTree {
                kind: UseTreeKind::Path {
                    segment: Ident("TargetStruct".to_string()),
                    alias: Some(Ident("Renamed".to_string())),
                    sub_tree: None,
                },
                span: Span { start: 0, end: 0 },
            })),
        },
        span: Span { start: 0, end: 0 },
    };
    
    let use_decl = UseDecl {
        tree: import_tree,
        span: Span { start: 0, end: 0 },
    };
    
    // Resolve the import
    let import_resolver = ImportResolver::new(
        PathBuf::from("test.plx"),
        "test".to_string(),
    );
    
    let current_scope = symbol_table.current_scope();
    let result = import_resolver.resolve_use_decl(
        &use_decl,
        current_scope,
        &mut symbol_table
    );
    assert!(result.is_ok());
    
    // Check that the symbol is available with the new name
    let imported_symbols = symbol_table.lookup("Renamed");
    assert_eq!(imported_symbols.len(), 1);
    
    // Should be an Import symbol
    match &imported_symbols[0] {
        Symbol::Import { name, target, .. } => {
            assert_eq!(name.0, "Renamed");
            
            match &**target {
                Symbol::Struct { name, .. } => {
                    assert_eq!(name.0, "TargetStruct");
                },
                _ => panic!("Expected Struct target"),
            }
        },
        _ => panic!("Expected Import symbol"),
    }
    
    // The original name should not be accessible
    let original_symbols = symbol_table.lookup("TargetStruct");
    assert_eq!(original_symbols.len(), 0);
}

#[test]
fn test_glob_import() {
    let mut symbol_table = SymbolTable::new();
    
    // Set up a module with multiple symbols
    let (module_scope, module_id) = symbol_table.push_module();
    
    // Add several symbols to the module
    let symbols = [
        Symbol::Function {
            name: Ident("func1".to_string()),
            sig: Arc::new(parallax_lang::ast::items::Function {
                name: Ident("func1".to_string()),
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
            span: Span { start: 0, end: 0 },
            is_public: true,
            defined_in: symbol_table.current_scope(),
        },
        Symbol::Struct {
            name: Ident("Struct1".to_string()),
            def: Arc::new(parallax_lang::ast::items::StructDef {
                name: Ident("Struct1".to_string()),
                generic_params: None,
                where_clause: None,
                fields: vec![],
                span: Span { start: 0, end: 0 },
            }),
            span: Span { start: 0, end: 0 },
            is_public: true,
            defined_in: symbol_table.current_scope(),
        },
        Symbol::Enum {
            name: Ident("Enum1".to_string()),
            def: Arc::new(parallax_lang::ast::items::EnumDef {
                name: Ident("Enum1".to_string()),
                generic_params: None,
                where_clause: None,
                variants: vec![],
                span: Span { start: 0, end: 0 },
            }),
            span: Span { start: 0, end: 0 },
            is_public: true,
            defined_in: symbol_table.current_scope(),
        },
        Symbol::Function {
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
            span: Span { start: 0, end: 0 },
            is_public: false,  // Private
            defined_in: symbol_table.current_scope(),
        },
    ];
    
    for symbol in &symbols {
        symbol_table.add_symbol(symbol.clone());
    }
    
    symbol_table.pop_scope();  // Return to root scope
    
    let module_symbol = Symbol::Module {
        name: Ident("utils".to_string()),
        id: module_id,
        span: Span { start: 0, end: 0 },
        is_public: true,
        defined_in: symbol_table.current_scope(),
    };
    
    symbol_table.add_symbol(module_symbol);
    
    // Create a glob import
    let import_tree = UseTree {
        kind: UseTreeKind::Path {
            segment: Ident("utils".to_string()),
            alias: None,
            sub_tree: Some(Box::new(UseTree {
                kind: UseTreeKind::Glob,
                span: Span { start: 0, end: 0 },
            })),
        },
        span: Span { start: 0, end: 0 },
    };
    
    let use_decl = UseDecl {
        tree: import_tree,
        span: Span { start: 0, end: 0 },
    };
    
    // Resolve the import
    let import_resolver = ImportResolver::new(
        PathBuf::from("test.plx"),
        "test".to_string(),
    );
    
    let current_scope = symbol_table.current_scope();
    let result = import_resolver.resolve_use_decl(
        &use_decl,
        current_scope,
        &mut symbol_table
    );
    assert!(result.is_ok());
    
    // Debug: Print all symbols in the current scope after resolution
    println!("\nSymbols in scope after resolution:");
    let all_symbols = symbol_table.get_all_symbols_in_scope(current_scope);
    for sym in &all_symbols {
        println!("  Symbol: {}", sym.name().0);
        if let Symbol::Import { target, .. } = sym {
            println!("    -> Import pointing to: {}", target.name().0);
        }
    }
    
    // Check that public symbols are available in the root scope
    let func1_symbols = symbol_table.lookup("func1");
    let struct1_symbols = symbol_table.lookup("Struct1");
    let enum1_symbols = symbol_table.lookup("Enum1");
    let private_func_symbols = symbol_table.lookup("private_func");
    
    assert_eq!(func1_symbols.len(), 1);
    assert_eq!(struct1_symbols.len(), 1);
    assert_eq!(enum1_symbols.len(), 1);
    assert_eq!(private_func_symbols.len(), 0);  // Private symbol should not be imported
}

#[test]
fn test_nested_imports() {
    let mut symbol_table = SymbolTable::new();
    
    // Create a module hierarchy
    // Root -> mod_a -> mod_b -> target
    let (mod_a_scope, mod_a_id) = symbol_table.push_module();
    
    let mod_a_symbol = Symbol::Module {
        name: Ident("mod_a".to_string()),
        id: mod_a_id,
        span: Span { start: 0, end: 0 },
        is_public: true,
        defined_in: symbol_table.root_scope(),
    };
    
    let (mod_b_scope, mod_b_id) = symbol_table.push_module();
    
    let mod_b_symbol = Symbol::Module {
        name: Ident("mod_b".to_string()),
        id: mod_b_id,
        span: Span { start: 0, end: 0 },
        is_public: true,
        defined_in: mod_a_scope,
    };
    
    // Add a target symbol in mod_b
    let target_symbol = Symbol::Function {
        name: Ident("deep_func".to_string()),
        sig: Arc::new(parallax_lang::ast::items::Function {
            name: Ident("deep_func".to_string()),
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
        span: Span { start: 0, end: 0 },
        is_public: true,
        defined_in: symbol_table.current_scope(),
    };
    
    symbol_table.add_symbol(target_symbol);
    
    // Navigate back to root
    symbol_table.pop_scope();  // Back to mod_a
    symbol_table.add_symbol(mod_b_symbol);
    
    symbol_table.pop_scope();  // Back to root
    symbol_table.add_symbol(mod_a_symbol);
    
    // Create a nested import
    let import_tree = UseTree {
        kind: UseTreeKind::Path {
            segment: Ident("mod_a".to_string()),
            alias: None,
            sub_tree: Some(Box::new(UseTree {
                kind: UseTreeKind::Path {
                    segment: Ident("mod_b".to_string()),
                    alias: None,
                    sub_tree: Some(Box::new(UseTree {
                        kind: UseTreeKind::Path {
                            segment: Ident("deep_func".to_string()),
                            alias: None,
                            sub_tree: None,
                        },
                        span: Span { start: 0, end: 0 },
                    })),
                },
                span: Span { start: 0, end: 0 },
            })),
        },
        span: Span { start: 0, end: 0 },
    };
    
    let use_decl = UseDecl {
        tree: import_tree,
        span: Span { start: 0, end: 0 },
    };
    
    // Resolve the import
    let import_resolver = ImportResolver::new(
        PathBuf::from("test.plx"),
        "test".to_string(),
    );
    
    let current_scope = symbol_table.current_scope();
    let result = import_resolver.resolve_use_decl(
        &use_decl,
        current_scope,
        &mut symbol_table
    );
    assert!(result.is_ok());
    
    // Check that the deeply nested symbol is available
    let imported_symbols = symbol_table.lookup("deep_func");
    assert_eq!(imported_symbols.len(), 1);
    
    // Should be an Import symbol pointing to the target
    match &imported_symbols[0] {
        Symbol::Import { name, target, .. } => {
            assert_eq!(name.0, "deep_func");
            
            match &**target {
                Symbol::Function { name, .. } => {
                    assert_eq!(name.0, "deep_func");
                },
                _ => panic!("Expected Function target"),
            }
        },
        _ => panic!("Expected Import symbol"),
    }
}

#[test]
fn test_group_imports() {
    let mut symbol_table = SymbolTable::new();
    
    // Create a module with multiple symbols
    let (module_scope, module_id) = symbol_table.push_module();
    
    let symbols = [
        Symbol::Function {
            name: Ident("func1".to_string()),
            sig: Arc::new(parallax_lang::ast::items::Function {
                name: Ident("func1".to_string()),
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
            span: Span { start: 0, end: 0 },
            is_public: true,
            defined_in: module_scope,
        },
        Symbol::Function {
            name: Ident("func2".to_string()),
            sig: Arc::new(parallax_lang::ast::items::Function {
                name: Ident("func2".to_string()),
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
            span: Span { start: 0, end: 0 },
            is_public: true,
            defined_in: module_scope,
        },
    ];
    
    for symbol in &symbols {
        symbol_table.add_symbol_to_scope(module_scope, symbol.clone());
    }
    
    symbol_table.pop_scope();  // Return to root scope
    
    let module_symbol = Symbol::Module {
        name: Ident("group_mod".to_string()),
        id: module_id,
        span: Span { start: 0, end: 0 },
        is_public: true,
        defined_in: symbol_table.current_scope(),
    };
    
    symbol_table.add_symbol(module_symbol);
    
    // Create a group import
    let import_tree = UseTree {
        kind: UseTreeKind::Path {
            segment: Ident("group_mod".to_string()),
            alias: None,
            sub_tree: Some(Box::new(UseTree {
                kind: UseTreeKind::Group(vec![
                    UseTree {
                        kind: UseTreeKind::Path {
                            segment: Ident("func1".to_string()),
                            alias: None,
                            sub_tree: None,
                        },
                        span: Span { start: 0, end: 0 },
                    },
                    UseTree {
                        kind: UseTreeKind::Path {
                            segment: Ident("func2".to_string()),
                            alias: Some(Ident("renamed_func2".to_string())),
                            sub_tree: None,
                        },
                        span: Span { start: 0, end: 0 },
                    },
                ]),
                span: Span { start: 0, end: 0 },
            })),
        },
        span: Span { start: 0, end: 0 },
    };
    
    let use_decl = UseDecl {
        tree: import_tree,
        span: Span { start: 0, end: 0 },
    };
    
    // Resolve the import
    let import_resolver = ImportResolver::new(
        PathBuf::from("test.plx"),
        "test".to_string(),
    );
    
    let current_scope = symbol_table.current_scope();
    let result = import_resolver.resolve_use_decl(
        &use_decl,
        current_scope,
        &mut symbol_table
    );
    assert!(result.is_ok());
    
    // Debug: Print all symbols in the current scope after resolution
    println!("\nSymbols in scope after resolution:");
    let all_symbols = symbol_table.get_all_symbols_in_scope(current_scope);
    for sym in &all_symbols {
        println!("  Symbol: {}", sym.name().0);
        if let Symbol::Import { target, .. } = sym {
            println!("    -> Import pointing to: {}", target.name().0);
        }
    }
    
    // Check that both symbols are available, with func2 renamed
    let func1_symbols = symbol_table.lookup("func1");
    let renamed_symbols = symbol_table.lookup("renamed_func2");
    
    assert_eq!(func1_symbols.len(), 1);
    assert_eq!(renamed_symbols.len(), 1);
    
    // Debug: Print func2 symbols to see what we're finding
    let func2_symbols = symbol_table.lookup("func2");
    println!("\nFound {} func2 symbols:", func2_symbols.len());
    for sym in &func2_symbols {
        println!("  Symbol: {} (defined_in: {:?})", sym.name().0, sym.defined_in());
        if let Symbol::Import { target, .. } = sym {
            println!("    -> Import pointing to: {}", target.name().0);
        } else if let Symbol::Function { .. } = sym {
            println!("    -> Function symbol");
        }
    }
    
    // Check that func2 is not directly accessible
    assert_eq!(func2_symbols.len(), 0);
} 