use parallax_resolve::symbol::{Symbol, SymbolTable, ScopeId, ModuleId};
use parallax_lang::ast::common::{Ident, Span};
use std::sync::Arc;

#[test]
fn test_symbol_table_creation() {
    let symbol_table = SymbolTable::new();
    
    // A new table should have a root scope and root module
    assert_eq!(symbol_table.root_scope(), ScopeId(0));
    assert_eq!(symbol_table.root_module(), ModuleId(0));
    assert_eq!(symbol_table.current_scope(), ScopeId(0));
}

#[test]
fn test_push_pop_scope() {
    let mut symbol_table = SymbolTable::new();
    let root_scope = symbol_table.current_scope();
    
    // Push a new scope
    let new_scope = symbol_table.push_scope(None);
    assert_ne!(new_scope, root_scope);
    assert_eq!(symbol_table.current_scope(), new_scope);
    
    // Pop back to root scope
    let popped_scope = symbol_table.pop_scope();
    assert_eq!(popped_scope, Some(new_scope));
    assert_eq!(symbol_table.current_scope(), root_scope);
}

#[test]
fn test_add_lookup_symbol() {
    let mut symbol_table = SymbolTable::new();
    
    // Create a variable symbol
    let var_symbol = Symbol::Variable {
        name: Ident("test_var".to_string()),
        ty: None,
        span: Span { start: 0, end: 0 },
        defined_in: symbol_table.current_scope(),
    };
    
    // Add the symbol
    symbol_table.add_symbol(var_symbol.clone());
    
    // Look up the symbol
    let symbols = symbol_table.lookup("test_var");
    assert_eq!(symbols.len(), 1);
    
    // Check symbol properties
    match symbols[0] {
        Symbol::Variable { name, .. } => {
            assert_eq!(name.0, "test_var");
        }
        _ => panic!("Expected Variable symbol"),
    }
}

#[test]
fn test_symbol_visibility() {
    let mut symbol_table = SymbolTable::new();
    
    // Add a public function symbol
    let func_symbol = Symbol::Function {
        name: Ident("public_func".to_string()),
        sig: Arc::new(parallax_lang::ast::items::Function {
            name: Ident("public_func".to_string()),
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
    
    symbol_table.add_symbol(func_symbol);
    
    // Add a private function symbol
    let private_func_symbol = Symbol::Function {
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
        is_public: false,
        defined_in: symbol_table.current_scope(),
    };
    
    symbol_table.add_symbol(private_func_symbol);
    
    // Check visibility
    let public_symbols = symbol_table.lookup("public_func");
    let private_symbols = symbol_table.lookup("private_func");
    
    assert!(public_symbols[0].is_public());
    assert!(!private_symbols[0].is_public());
}

#[test]
fn test_scope_hierarchy() {
    let mut symbol_table = SymbolTable::new();
    
    // Add a symbol to the root scope
    let root_symbol = Symbol::Variable {
        name: Ident("root_var".to_string()),
        ty: None,
        span: Span { start: 0, end: 0 },
        defined_in: symbol_table.current_scope(),
    };
    
    symbol_table.add_symbol(root_symbol);
    
    // Push a new scope
    let child_scope = symbol_table.push_scope(None);
    
    // Add a symbol to the child scope
    let child_symbol = Symbol::Variable {
        name: Ident("child_var".to_string()),
        ty: None,
        span: Span { start: 0, end: 0 },
        defined_in: symbol_table.current_scope(),
    };
    
    symbol_table.add_symbol(child_symbol);
    
    // From child scope, both symbols should be visible
    let root_symbols = symbol_table.lookup("root_var");
    let child_symbols = symbol_table.lookup("child_var");
    
    assert_eq!(root_symbols.len(), 1);
    assert_eq!(child_symbols.len(), 1);
    
    // Pop back to root scope
    symbol_table.pop_scope();
    
    // From root scope, only root symbol should be visible
    let root_symbols = symbol_table.lookup("root_var");
    let child_symbols = symbol_table.lookup("child_var");
    
    assert_eq!(root_symbols.len(), 1);
    assert_eq!(child_symbols.len(), 0);
}

#[test]
fn test_module_scopes() {
    let mut symbol_table = SymbolTable::new();
    
    // Create a module
    let (module_scope_id, module_id) = symbol_table.push_module();
    
    // Add a symbol to the module
    let module_symbol = Symbol::Variable {
        name: Ident("module_var".to_string()),
        ty: None,
        span: Span { start: 0, end: 0 },
        defined_in: symbol_table.current_scope(),
    };
    
    symbol_table.add_symbol(module_symbol);
    
    // Check that the module scope is current
    assert_eq!(symbol_table.current_scope(), module_scope_id);
    
    // Look up the symbol in the module
    let symbols = symbol_table.lookup("module_var");
    assert_eq!(symbols.len(), 1);
    
    // Get all scopes for the module
    let module_scopes = symbol_table.get_scopes_for_module(module_id);
    assert!(module_scopes.contains(&module_scope_id));
    
    // Pop back to root scope
    symbol_table.pop_scope();
    
    // From root scope, the module symbol should not be visible
    let symbols = symbol_table.lookup("module_var");
    assert_eq!(symbols.len(), 0);
}

#[test]
fn test_lookup_in_specific_scope() {
    let mut symbol_table = SymbolTable::new();
    
    // Add a symbol to the root scope
    let root_symbol = Symbol::Variable {
        name: Ident("test_var".to_string()),
        ty: None,
        span: Span { start: 0, end: 0 },
        defined_in: symbol_table.current_scope(),
    };
    
    symbol_table.add_symbol(root_symbol);
    
    // Push a new scope
    let child_scope = symbol_table.push_scope(None);
    
    // Add a symbol with the same name to the child scope
    let child_symbol = Symbol::Variable {
        name: Ident("test_var".to_string()),
        ty: None,
        span: Span { start: 0, end: 0 },
        defined_in: symbol_table.current_scope(),
    };
    
    symbol_table.add_symbol(child_symbol);
    
    // Regular lookup should find both symbols (with child scope taking precedence)
    let symbols = symbol_table.lookup("test_var");
    assert_eq!(symbols.len(), 2);
    
    // Lookup specifically in root scope should find only one
    let root_scope = symbol_table.root_scope();
    let root_symbols = symbol_table.lookup_in_scope(root_scope, "test_var");
    assert_eq!(root_symbols.len(), 1);
    
    // Lookup specifically in child scope should find only one
    let child_symbols = symbol_table.lookup_in_scope(child_scope, "test_var");
    assert_eq!(child_symbols.len(), 1);
}

#[test]
fn test_get_all_symbols_in_scope() {
    let mut symbol_table = SymbolTable::new();
    
    // Add multiple symbols to the root scope
    for i in 0..5 {
        let var_name = format!("var_{}", i);
        let var_symbol = Symbol::Variable {
            name: Ident(var_name),
            ty: None,
            span: Span { start: 0, end: 0 },
            defined_in: symbol_table.current_scope(),
        };
        
        symbol_table.add_symbol(var_symbol);
    }
    
    // Get all symbols in the root scope
    let root_scope = symbol_table.root_scope();
    let all_symbols = symbol_table.get_all_symbols_in_scope(root_scope);
    
    // Should have 5 symbols
    assert_eq!(all_symbols.len(), 5);
} 