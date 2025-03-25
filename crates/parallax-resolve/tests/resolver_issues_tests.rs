use parallax_resolve::{
    resolver::Resolver,
    symbol::{ScopeId, Symbol, SymbolTable},
};
use parallax_lang::ast::{
    common::{Ident, Span},
    expr::{Expr, ExprKind},
    items::{Function, Item, ItemKind, Module, UseDecl, UseTree, UseTreeKind},
};
use std::path::PathBuf;

// Helper functions copied from module_tests.rs for consistency
fn create_test_module(name: &str, items: Vec<Item>) -> Item {
    Item {
        kind: ItemKind::Module(Module {
            name: Ident(name.to_string()),
            items,
            span: Span { start: 0, end: 0 },
        }),
        visibility: true,
        span: Span { start: 0, end: 0 },
    }
}

fn create_test_function(name: &str) -> Item {
    Item {
        kind: ItemKind::Function(Function {
            name: Ident(name.to_string()),
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
    }
}

fn debug_print_symbols(symbol_table: &SymbolTable) {
    println!("\n--- Symbol Table Contents ---");
    
    // Instead of using get_all_scopes(), look at root scope and its descendants
    // Start with the root scope
    let root_scope = symbol_table.root_scope();
    
    // Print root scope and its symbols
    print_scope_and_symbols(symbol_table, root_scope);
    
    println!("---------------------------\n");
}

// Helper function to recursively print a scope and its symbols
fn print_scope_and_symbols(symbol_table: &SymbolTable, scope_id: ScopeId) {
    println!("Scope {:?}:", scope_id);
    
    // Get all symbols in this scope
    let all_symbols = symbol_table.get_all_symbols_in_scope(scope_id);
    
    for sym in &all_symbols {
        let sym_type = match sym {
            Symbol::Module { .. } => "Module",
            Symbol::Function { .. } => "Function",
            Symbol::Import { .. } => "Import",
            _ => "Other",
        };
        println!("  Symbol: {} ({})", sym.name().0, sym_type);
    }
}

// ISSUE 1: Test for nested module path resolution with deep nesting
#[test]
fn test_deep_nested_module_resolution() {
    // Create a deeply nested module structure: root -> a -> b -> c with a function in c
    let func_in_c = create_test_function("func");
    let module_c = create_test_module("c", vec![func_in_c]);
    let module_b = create_test_module("b", vec![module_c]);
    let module_a = create_test_module("a", vec![module_b]);
    
    // Create root module with module_a
    let root = Item {
        kind: ItemKind::Module(Module {
            name: Ident("".to_string()), // Root module has empty name
            items: vec![module_a],
            span: Span { start: 0, end: 0 },
        }),
        visibility: true,
        span: Span { start: 0, end: 0 },
    };
    
    // Create resolver and resolve the AST
    let mut resolver = Resolver::new(PathBuf::new(), "".to_string());
    let resolved = resolver.resolve(&root);
    
    // Verify resolution succeeded
    if let Err(e) = &resolved {
        println!("Resolution failed with error: {:?}", e);
        panic!("Resolution should not fail");
    }
    
    // Get the symbol table
    let symbol_table = &resolver.symbol_table;
    debug_print_symbols(symbol_table);
    
    // Find the root module
    let root_module_symbols = symbol_table.lookup("");
    assert!(!root_module_symbols.is_empty(), "Root module should exist");
    
    let root_module_id = root_module_symbols.iter().find_map(|sym| {
        if let Symbol::Module { id, .. } = sym {
            Some(*id)
        } else {
            None
        }
    }).expect("Failed to find root module ID");
    
    // Get the root scope
    let root_scopes = symbol_table.get_scopes_for_module(root_module_id);
    assert!(!root_scopes.is_empty(), "Root module should have at least one scope");
    let root_scope = root_scopes[0];
    
    // Find module a in the root scope
    let module_a_symbols = symbol_table.lookup_in_scope(root_scope, "a");
    assert!(!module_a_symbols.is_empty(), "Module 'a' should exist in root scope");
    
    // The test for the issue: navigate through the nested structure
    // This is expected to fail if there's an issue with nested module resolution
    let module_a_id = module_a_symbols.iter().find_map(|sym| {
        if let Symbol::Module { id, .. } = sym {
            Some(*id)
        } else {
            None
        }
    }).expect("Failed to find module a ID");
    
    let module_a_scopes = symbol_table.get_scopes_for_module(module_a_id);
    assert!(!module_a_scopes.is_empty(), "Module 'a' should have at least one scope");
    let module_a_scope = module_a_scopes[0];
    
    // Find module b in module a's scope
    let module_b_symbols = symbol_table.lookup_in_scope(module_a_scope, "b");
    assert!(!module_b_symbols.is_empty(), "Module 'b' should exist in module 'a' scope");
    
    let module_b_id = module_b_symbols.iter().find_map(|sym| {
        if let Symbol::Module { id, .. } = sym {
            Some(*id)
        } else {
            None
        }
    }).expect("Failed to find module b ID");
    
    let module_b_scopes = symbol_table.get_scopes_for_module(module_b_id);
    assert!(!module_b_scopes.is_empty(), "Module 'b' should have at least one scope");
    let module_b_scope = module_b_scopes[0];
    
    // Find module c in module b's scope
    let module_c_symbols = symbol_table.lookup_in_scope(module_b_scope, "c");
    assert!(!module_c_symbols.is_empty(), "Module 'c' should exist in module 'b' scope");
    
    let module_c_id = module_c_symbols.iter().find_map(|sym| {
        if let Symbol::Module { id, .. } = sym {
            Some(*id)
        } else {
            None
        }
    }).expect("Failed to find module c ID");
    
    let module_c_scopes = symbol_table.get_scopes_for_module(module_c_id);
    assert!(!module_c_scopes.is_empty(), "Module 'c' should have at least one scope");
    let module_c_scope = module_c_scopes[0];
    
    // Debug: Print all scopes for module C
    println!("Module C has {} scopes:", module_c_scopes.len());
    for (i, scope) in module_c_scopes.iter().enumerate() {
        println!("  Scope #{}: {:?}", i, scope);
        let symbols = symbol_table.get_all_symbols_in_scope(*scope);
        println!("    Contains {} symbols:", symbols.len());
        for sym in &symbols {
            println!("      Symbol: {} (type: {:?})", sym.name().0, std::mem::discriminant(sym));
            
            if let Symbol::Function { name, .. } = sym {
                println!("      Found function: {}", name.0);
            }
        }
    }
    
    // Try an exhaustive search in all module C scopes
    let mut found_func = false;
    let mut func_scope = module_c_scope; // Default to first scope
    for scope in &module_c_scopes {
        let func_symbols = symbol_table.lookup_in_scope(*scope, "func");
        if !func_symbols.is_empty() {
            println!("Found 'func' in module C scope: {:?}", scope);
            found_func = true;
            func_scope = *scope; // Update to the scope where func was found
        }
    }
    
    if !found_func {
        println!("ERROR: 'func' not found in any module C scope");
    }
    
    // Find the function in the scope where it was found
    let func_symbols = symbol_table.lookup_in_scope(func_scope, "func");
    assert!(!func_symbols.is_empty(), "Function 'func' should exist in module 'c' scope");
}

// ISSUE 2: Test for importing from deeply nested modules
#[test]
fn test_import_from_nested_modules() {
    // Create a nested module structure: root -> a -> b with a function in b
    let func_in_b = create_test_function("deep_func");
    let module_b = create_test_module("b", vec![func_in_b]);
    let module_a = create_test_module("a", vec![module_b]);
    
    // Create a use declaration to import a::b::deep_func
    let use_tree = UseTree {
        kind: UseTreeKind::Path {
            segment: Ident("a".to_string()),
            alias: None,
            sub_tree: Some(Box::new(UseTree {
                kind: UseTreeKind::Path {
                    segment: Ident("b".to_string()),
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
    
    let use_decl = Item {
        kind: ItemKind::Use(UseDecl {
            tree: use_tree,
            span: Span { start: 0, end: 0 },
        }),
        visibility: true,
        span: Span { start: 0, end: 0 },
    };
    
    // Create root module with module_a and the use declaration
    let root = Item {
        kind: ItemKind::Module(Module {
            name: Ident("".to_string()), // Root module has empty name
            items: vec![module_a, use_decl],
            span: Span { start: 0, end: 0 },
        }),
        visibility: true,
        span: Span { start: 0, end: 0 },
    };
    
    // Create resolver and resolve the AST
    let mut resolver = Resolver::new(PathBuf::new(), "".to_string());
    let resolved = resolver.resolve(&root);
    
    // This is expected to fail if there's an issue with imports from nested modules
    if let Err(e) = &resolved {
        println!("Resolution failed with error: {:?}", e);
        panic!("Resolution should not fail");
    }
    
    // Get the symbol table
    let symbol_table = &resolver.symbol_table;
    debug_print_symbols(symbol_table);
    
    // Find the root module
    let root_module_symbols = symbol_table.lookup("");
    assert!(!root_module_symbols.is_empty(), "Root module should exist");
    
    let root_module_id = root_module_symbols.iter().find_map(|sym| {
        if let Symbol::Module { id, .. } = sym {
            Some(*id)
        } else {
            None
        }
    }).expect("Failed to find root module ID");
    
    // Get the root scope
    let root_scopes = symbol_table.get_scopes_for_module(root_module_id);
    assert!(!root_scopes.is_empty(), "Root module should have at least one scope");
    let root_scope = root_scopes[0];
    
    // Find the imported function in the root scope
    let imported_func = symbol_table.lookup_in_scope(root_scope, "deep_func");
    
    // The test for the issue: verify the import exists in the root scope
    assert!(!imported_func.is_empty(), "Function 'deep_func' should be imported into root scope");
    
    // Verify that the imported symbol is an Import pointing to a Function
    match &imported_func[0] {
        Symbol::Import { name, target, .. } => {
            assert_eq!(name.0, "deep_func", "Import name should be 'deep_func'");
            
            // Verify that the target is a function
            match &**target {
                Symbol::Function { name, .. } => {
                    assert_eq!(name.0, "deep_func", "Target function name should be 'deep_func'");
                },
                _ => panic!("Import target should be a function"),
            }
        },
        _ => panic!("Expected an Import symbol"),
    }
}

// ISSUE 3: Test for glob imports from nested modules
#[test]
fn test_glob_import_from_nested_module() {
    // Create functions for a nested module
    let func1 = create_test_function("nested_func1");
    let func2 = create_test_function("nested_func2");
    
    // Create a nested module structure: root -> utils -> helpers with functions
    let helpers_module = create_test_module("helpers", vec![func1, func2]);
    let utils_module = create_test_module("utils", vec![helpers_module]);
    
    // Create a use declaration to glob import from utils::helpers
    let use_tree = UseTree {
        kind: UseTreeKind::Path {
            segment: Ident("utils".to_string()),
            alias: None,
            sub_tree: Some(Box::new(UseTree {
                kind: UseTreeKind::Path {
                    segment: Ident("helpers".to_string()),
                    alias: None,
                    sub_tree: Some(Box::new(UseTree {
                        kind: UseTreeKind::Glob,
                        span: Span { start: 0, end: 0 },
                    })),
                },
                span: Span { start: 0, end: 0 },
            })),
        },
        span: Span { start: 0, end: 0 },
    };
    
    let use_decl = Item {
        kind: ItemKind::Use(UseDecl {
            tree: use_tree,
            span: Span { start: 0, end: 0 },
        }),
        visibility: true,
        span: Span { start: 0, end: 0 },
    };
    
    // Create root module with utils_module and the use declaration
    let root = Item {
        kind: ItemKind::Module(Module {
            name: Ident("".to_string()), // Root module has empty name
            items: vec![utils_module, use_decl],
            span: Span { start: 0, end: 0 },
        }),
        visibility: true,
        span: Span { start: 0, end: 0 },
    };
    
    // Create resolver and resolve the AST
    let mut resolver = Resolver::new(PathBuf::new(), "".to_string());
    let resolved = resolver.resolve(&root);
    
    // This is expected to fail if there's an issue with glob imports from nested modules
    if let Err(e) = &resolved {
        println!("Resolution failed with error: {:?}", e);
        panic!("Resolution should not fail");
    }
    
    // Get the symbol table
    let symbol_table = &resolver.symbol_table;
    debug_print_symbols(symbol_table);
    
    // Find the root module
    let root_module_symbols = symbol_table.lookup("");
    assert!(!root_module_symbols.is_empty(), "Root module should exist");
    
    let root_module_id = root_module_symbols.iter().find_map(|sym| {
        if let Symbol::Module { id, .. } = sym {
            Some(*id)
        } else {
            None
        }
    }).expect("Failed to find root module ID");
    
    // Get the root scope
    let root_scopes = symbol_table.get_scopes_for_module(root_module_id);
    assert!(!root_scopes.is_empty(), "Root module should have at least one scope");
    let root_scope = root_scopes[0];
    
    // Find the imported functions in the root scope
    let imported_func1 = symbol_table.lookup_in_scope(root_scope, "nested_func1");
    let imported_func2 = symbol_table.lookup_in_scope(root_scope, "nested_func2");
    
    // The test for the issue: verify both imports exist in the root scope
    assert!(!imported_func1.is_empty(), "Function 'nested_func1' should be imported into root scope");
    assert!(!imported_func2.is_empty(), "Function 'nested_func2' should be imported into root scope");
} 