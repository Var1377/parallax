use parallax_resolve::namespace::{Namespace, NamespaceManager};
use parallax_resolve::symbol::{Symbol, SymbolTable};
use parallax_lang::ast::common::{Ident, Span};
use std::sync::Arc;

#[test]
fn test_get_namespace() {
    // Create symbols of different kinds
    let symbol_table = SymbolTable::new();
    let current_scope = symbol_table.current_scope();
    
    // Function symbol (Values namespace)
    let func_symbol = Symbol::Function {
        name: Ident("test_func".to_string()),
        sig: Arc::new(parallax_lang::ast::items::Function {
            name: Ident("test_func".to_string()),
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
        defined_in: current_scope,
    };
    
    // Struct symbol (Types namespace)
    let struct_symbol = Symbol::Struct {
        name: Ident("test_struct".to_string()),
        def: Arc::new(parallax_lang::ast::items::StructDef {
            name: Ident("test_struct".to_string()),
            generic_params: None,
            where_clause: None,
            fields: vec![],
            span: Span { start: 0, end: 0 },
        }),
        span: Span { start: 0, end: 0 },
        is_public: true,
        defined_in: current_scope,
    };
    
    // Variable symbol (Values namespace)
    let var_symbol = Symbol::Variable {
        name: Ident("test_var".to_string()),
        ty: None,
        span: Span { start: 0, end: 0 },
        defined_in: current_scope,
    };
    
    // Module symbol (Modules namespace)
    let module_symbol = Symbol::Module {
        name: Ident("test_module".to_string()),
        id: symbol_table.current_module(),
        span: Span { start: 0, end: 0 },
        is_public: true,
        defined_in: current_scope,
    };
    
    // Check namespaces
    assert_eq!(NamespaceManager::get_namespace(&func_symbol), Namespace::Values);
    assert_eq!(NamespaceManager::get_namespace(&struct_symbol), Namespace::Types);
    assert_eq!(NamespaceManager::get_namespace(&var_symbol), Namespace::Values);
    assert_eq!(NamespaceManager::get_namespace(&module_symbol), Namespace::Modules);
}

#[test]
fn test_lookup_in_namespace() {
    let mut symbol_table = SymbolTable::new();
    let current_scope = symbol_table.current_scope();
    
    // Add a struct and a function with the same name
    let struct_symbol = Symbol::Struct {
        name: Ident("test_name".to_string()),
        def: Arc::new(parallax_lang::ast::items::StructDef {
            name: Ident("test_name".to_string()),
            generic_params: None,
            where_clause: None,
            fields: vec![],
            span: Span { start: 0, end: 0 },
        }),
        span: Span { start: 0, end: 0 },
        is_public: true,
        defined_in: current_scope,
    };
    
    let func_symbol = Symbol::Function {
        name: Ident("test_name".to_string()),
        sig: Arc::new(parallax_lang::ast::items::Function {
            name: Ident("test_name".to_string()),
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
        defined_in: current_scope,
    };
    
    symbol_table.add_symbol(struct_symbol);
    symbol_table.add_symbol(func_symbol);
    
    // Look up symbols in the Types namespace
    let types_symbols = NamespaceManager::lookup_in_namespace(&symbol_table, "test_name", Namespace::Types);
    assert_eq!(types_symbols.len(), 1);
    assert!(matches!(types_symbols[0], Symbol::Struct { .. }));
    
    // Look up symbols in the Values namespace
    let values_symbols = NamespaceManager::lookup_in_namespace(&symbol_table, "test_name", Namespace::Values);
    assert_eq!(values_symbols.len(), 1);
    assert!(matches!(values_symbols[0], Symbol::Function { .. }));
    
    // Look up symbols in the Modules namespace (should be empty)
    let modules_symbols = NamespaceManager::lookup_in_namespace(&symbol_table, "test_name", Namespace::Modules);
    assert_eq!(modules_symbols.len(), 0);
}

#[test]
fn test_is_in_namespace() {
    let symbol_table = SymbolTable::new();
    let current_scope = symbol_table.current_scope();
    
    // Create symbols of different kinds
    let struct_symbol = Symbol::Struct {
        name: Ident("test_struct".to_string()),
        def: Arc::new(parallax_lang::ast::items::StructDef {
            name: Ident("test_struct".to_string()),
            generic_params: None,
            where_clause: None,
            fields: vec![],
            span: Span { start: 0, end: 0 },
        }),
        span: Span { start: 0, end: 0 },
        is_public: true,
        defined_in: current_scope,
    };
    
    let var_symbol = Symbol::Variable {
        name: Ident("test_var".to_string()),
        ty: None,
        span: Span { start: 0, end: 0 },
        defined_in: current_scope,
    };
    
    // Check namespace membership
    assert!(NamespaceManager::is_in_namespace(&struct_symbol, Namespace::Types));
    assert!(!NamespaceManager::is_in_namespace(&struct_symbol, Namespace::Values));
    assert!(!NamespaceManager::is_in_namespace(&struct_symbol, Namespace::Modules));
    
    assert!(NamespaceManager::is_in_namespace(&var_symbol, Namespace::Values));
    assert!(!NamespaceManager::is_in_namespace(&var_symbol, Namespace::Types));
    assert!(!NamespaceManager::is_in_namespace(&var_symbol, Namespace::Modules));
}

#[test]
fn test_find_in_namespace() {
    let mut symbol_table = SymbolTable::new();
    let current_scope = symbol_table.current_scope();
    
    // Create symbols of different kinds
    let struct_symbol = Symbol::Struct {
        name: Ident("test_struct".to_string()),
        def: Arc::new(parallax_lang::ast::items::StructDef {
            name: Ident("test_struct".to_string()),
            generic_params: None,
            where_clause: None,
            fields: vec![],
            span: Span { start: 0, end: 0 },
        }),
        span: Span { start: 0, end: 0 },
        is_public: true,
        defined_in: current_scope,
    };
    
    let func_symbol = Symbol::Function {
        name: Ident("test_func".to_string()),
        sig: Arc::new(parallax_lang::ast::items::Function {
            name: Ident("test_func".to_string()),
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
        defined_in: current_scope,
    };
    
    let module_symbol = Symbol::Module {
        name: Ident("test_module".to_string()),
        id: symbol_table.current_module(),
        span: Span { start: 0, end: 0 },
        is_public: true,
        defined_in: current_scope,
    };
    
    // Add symbols to symbol table
    symbol_table.add_symbol(struct_symbol);
    symbol_table.add_symbol(func_symbol);
    symbol_table.add_symbol(module_symbol);
    
    // Get all symbols
    let root_scope = symbol_table.root_scope();
    let all_symbols = symbol_table.get_all_symbols_in_scope(root_scope);
    
    // Find Types namespace symbols
    let types_symbols = NamespaceManager::find_in_namespace(&all_symbols, Namespace::Types);
    assert_eq!(types_symbols.len(), 1);
    assert!(matches!(types_symbols[0], Symbol::Struct { .. }));
    
    // Find Values namespace symbols
    let values_symbols = NamespaceManager::find_in_namespace(&all_symbols, Namespace::Values);
    assert_eq!(values_symbols.len(), 1);
    assert!(matches!(values_symbols[0], Symbol::Function { .. }));
    
    // Find Modules namespace symbols
    let modules_symbols = NamespaceManager::find_in_namespace(&all_symbols, Namespace::Modules);
    assert_eq!(modules_symbols.len(), 1);
    assert!(matches!(modules_symbols[0], Symbol::Module { .. }));
}

#[test]
fn test_can_share_name() {
    let symbol_table = SymbolTable::new();
    let current_scope = symbol_table.current_scope();
    
    // Create symbols of different kinds
    let struct_symbol = Symbol::Struct {
        name: Ident("shared_name".to_string()),
        def: Arc::new(parallax_lang::ast::items::StructDef {
            name: Ident("shared_name".to_string()),
            generic_params: None,
            where_clause: None,
            fields: vec![],
            span: Span { start: 0, end: 0 },
        }),
        span: Span { start: 0, end: 0 },
        is_public: true,
        defined_in: current_scope,
    };
    
    let func_symbol = Symbol::Function {
        name: Ident("shared_name".to_string()),
        sig: Arc::new(parallax_lang::ast::items::Function {
            name: Ident("shared_name".to_string()),
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
        defined_in: current_scope,
    };
    
    let module_symbol = Symbol::Module {
        name: Ident("shared_name".to_string()),
        id: symbol_table.current_module(),
        span: Span { start: 0, end: 0 },
        is_public: true,
        defined_in: current_scope,
    };
    
    let enum_symbol = Symbol::Enum {
        name: Ident("shared_name".to_string()),
        def: Arc::new(parallax_lang::ast::items::EnumDef {
            name: Ident("shared_name".to_string()),
            generic_params: None,
            where_clause: None,
            variants: vec![],
            span: Span { start: 0, end: 0 },
        }),
        span: Span { start: 0, end: 0 },
        is_public: true,
        defined_in: current_scope,
    };
    
    // Check which symbols can share names
    // Types and Values can share names
    assert!(NamespaceManager::can_share_name(&struct_symbol, &func_symbol));
    
    // Types and Types cannot share names
    assert!(!NamespaceManager::can_share_name(&struct_symbol, &enum_symbol));
    
    // Modules cannot share names with anything
    assert!(!NamespaceManager::can_share_name(&module_symbol, &struct_symbol));
    assert!(!NamespaceManager::can_share_name(&module_symbol, &func_symbol));
} 