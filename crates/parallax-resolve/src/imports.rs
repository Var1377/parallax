//! Import resolution for name resolution.
//!
//! This module handles the resolution of `use` declarations, managing the logic
//! for resolving paths, glob imports, nested imports, and import renaming.

use std::path::PathBuf;
use crate::{
    error::ResolveError,
    symbol::{Symbol, SymbolTable, ScopeId, ModuleId},
    Result,
};
use parallax_lang::ast::{
    common::{Ident, Span},
    items::{UseDecl, UseTree, UseTreeKind},
};

/// Import resolver for handling use declarations
pub struct ImportResolver {
    /// Path to the current crate root
    pub crate_root: PathBuf,
    /// The name of the current crate
    pub crate_name: String,
    /// Source code content for error reporting
    source_code: String,
}

impl ImportResolver {
    /// Create a new import resolver
    pub fn new(crate_root: PathBuf, crate_name: String) -> Self {
        Self {
            crate_root,
            crate_name,
            source_code: String::new(),
        }
    }
    
    /// Create a new import resolver with source code for better error reporting
    pub fn new_with_source(crate_root: PathBuf, crate_name: String, source_code: String) -> Self {
        Self {
            crate_root,
            crate_name,
            source_code,
        }
    }
    
    /// Helper function to create import errors
    fn create_import_error(&self, name: &str, reason: &str, span: Span) -> ResolveError {
        ResolveError::import_error(
            name.to_string(),
            reason.to_string(),
            span,
            self.crate_root.clone(),
            self.source_code.clone(),
        )
    }
    
    /// Resolve a use declaration
    pub fn resolve_use_decl(
        &self,
        use_decl: &UseDecl,
        in_scope: ScopeId,
        symbol_table: &mut SymbolTable,
    ) -> Result<()> {
        println!("Resolving use declaration");
        match self.resolve_use_tree(&use_decl.tree, in_scope, symbol_table) {
            Ok(_) => Ok(()),
            Err(e) => {
                println!("Use declaration resolution failed: {:?}", e);
                Err(e)
            }
        }
    }
    
    /// Resolve a use tree
    pub fn resolve_use_tree(
        &self,
        tree: &UseTree,
        in_scope: ScopeId,
        symbol_table: &mut SymbolTable,
    ) -> Result<()> {
        println!("\n==== Resolving use tree ====");
        match &tree.kind {
            UseTreeKind::Path { segment, alias, sub_tree } => {
                println!("Resolving path import: {}::{}", segment.0, match sub_tree {
                    Some(_) => "<subtree>",
                    None => "<no subtree>"
                });
                
                // Special handling for 'super' keyword in imports
                if segment.0 == "super" {
                    return self.resolve_super_import(tree, in_scope, symbol_table);
                }
                
                // Debug: Show all symbols in the current scope
                let all_symbols = symbol_table.get_all_symbols_in_scope(in_scope);
                println!("Current scope has {} symbols:", all_symbols.len());
                for sym in &all_symbols {
                    println!("  Symbol: {} (type: {})", sym.name().0, 
                        match sym {
                            Symbol::Module { .. } => "Module",
                            Symbol::Function { .. } => "Function",
                            Symbol::Struct { .. } => "Struct",
                            Symbol::Enum { .. } => "Enum",
                            Symbol::Variable { .. } => "Variable",
                            Symbol::Import { .. } => "Import",
                            Symbol::Trait { .. } => "Trait",
                            _ => "Other"
                        });
                }
                
                // Look up the first segment
                let segment_name = &segment.0;
                println!("Looking up '{}' in scope: ", segment_name);
                
                // If there's a sub_tree, we need to handle differently based on type
                if let Some(sub_tree) = sub_tree {
                    match &sub_tree.kind {
                        UseTreeKind::Group(group_trees) => {
                            // Handle group import under a path node (e.g. a::{b, c})
                            println!("Resolving path with group: {} with {} items", segment_name, group_trees.len());
                            
                            // Lookup the parent module in the current scope
                            let module_symbols = symbol_table.lookup_in_scope(in_scope, segment_name);
                            if module_symbols.is_empty() {
                                println!("Module '{}' not found in scope {:?}", segment_name, in_scope);
                                return Err(self.create_import_error(segment_name, "Module not found", tree.span));
                            }
                            
                            println!("Found {} symbols for '{}'", module_symbols.len(), segment_name);
                            
                            // Find the module symbol
                            let module_id = module_symbols.iter().find_map(|sym| {
                                println!("  Symbol: {} (type: {})", sym.name().0, 
                                        match sym {
                                            Symbol::Module { .. } => "Module",
                                            Symbol::Function { .. } => "Function",
                                            Symbol::Struct { .. } => "Struct",
                                            Symbol::Enum { .. } => "Enum",
                                            Symbol::Variable { .. } => "Variable",
                                            Symbol::Import { .. } => "Import",
                                            Symbol::Trait { .. } => "Trait",
                                            _ => "Other"
                                        });
                                
                                if let Symbol::Module { id, .. } = sym {
                                    Some(*id)
                                } else {
                                    None
                                }
                            });
                            
                            if let Some(module_id) = module_id {
                                println!("Found module with ID: {:?}", module_id);
                                
                                // Get the scopes for the module
                                let module_scopes = symbol_table.get_scopes_for_module(module_id);
                                println!("Module scope: {:?}", module_scopes[0]);
                                
                                if module_scopes.is_empty() {
                                    println!("Module '{}' has no scopes", segment_name);
                                    return Err(self.create_import_error(segment_name, "Module has no scope", tree.span));
                                }
                                
                                let module_scope = module_scopes[0];
                                
                                // Now resolve each item in the group
                                for group_item in group_trees {
                                    match &group_item.kind {
                                        UseTreeKind::Path { segment: item_segment, alias: item_alias, .. } => {
                                            let item_name = &item_segment.0;
                                            println!("Looking for item '{}' in module scope {:?}", item_name, module_scope);
                                            
                                            // Look up the item in the module's scope
                                            let item_symbols = symbol_table.lookup_in_scope(module_scope, item_name);
                                            
                                            if item_symbols.is_empty() {
                                                println!("Item '{}' not found in module scope {:?}", item_name, module_scope);
                                                return Err(self.create_import_error(item_name, "Item not found in module", group_item.span));
                                            }
                                            
                                            // Check if the symbol is public
                                            let symbol = item_symbols[0];
                                            println!("Found symbol: {} (public: {})", symbol.name().0, symbol.is_public());
                                            
                                            if !symbol.is_public() {
                                                println!("Symbol '{}' is not public", item_name);
                                                return Err(self.create_import_error(item_name, "Cannot import private symbol", group_item.span));
                                            }
                                            
                                            // Use the provided alias or the original name
                                            let import_name = item_alias.clone().unwrap_or_else(|| item_segment.clone());
                                            
                                            println!("Creating import symbol with name: {}", import_name.0);
                                            
                                            // Create the import symbol
                                            let import_symbol = Symbol::Import {
                                                name: import_name.clone(),
                                                target: Box::new(symbol.clone()),
                                                span: group_item.span,
                                                defined_in: in_scope,
                                            };
                                            
                                            // Add the import to the target scope
                                            symbol_table.add_symbol_to_scope(in_scope, import_symbol);
                                            println!("Added import symbol: {}", import_name.0);
                                        },
                                        _ => {
                                            // Handle other kinds of tree nodes (unlikely in a group)
                                            println!("Non-path item in group import");
                                            return Err(self.create_import_error("group item", "Unexpected item kind in group", group_item.span));
                                        }
                                    }
                                }
                                
                                return Ok(());
                            } else {
                                println!("Symbol '{}' is not a module", segment_name);
                                return Err(self.create_import_error(segment_name, "Not a module", tree.span));
                            }
                        },
                        // ... existing code for other subtree types ...
                        _ => {
                            // This is a path with a subtree (e.g., a::b::c)
                            
                            // First, try to handle this as a deep nested import
                            if let UseTreeKind::Path { .. } = &sub_tree.kind {
                                // For complex nested paths (more than one level), use our specialized handler
                                if let Ok(()) = self.resolve_deep_nested_import(tree, in_scope, symbol_table) {
                                    return Ok(());
                                }
                                // If that fails, fall back to normal handling
                            }
                            
                            if sub_tree.kind == UseTreeKind::Glob {
                                // Handle glob import (e.g., a::b::*)
                                println!("Resolving glob import: {}::*", segment.0);
                                self.resolve_glob_import(&[segment.clone()], in_scope, symbol_table)
                            } else {
                                // Build the full path for nested imports
                                let mut path = vec![];
                                let mut current_tree = tree;
                                
                                // Collect all segments from the path
                                loop {
                                    if let UseTreeKind::Path { segment, sub_tree, .. } = &current_tree.kind {
                                        path.push(segment.0.clone());
                                        
                                        if let Some(sub_tree_inner) = sub_tree {
                                            if let UseTreeKind::Path { .. } = &sub_tree_inner.kind {
                                                current_tree = sub_tree_inner;
                                                continue;
                                            } else if let UseTreeKind::Glob = &sub_tree_inner.kind {
                                                // Handle glob imports
                                                println!("Resolving nested glob import: {}::*", path.join("::"));
                                                
                                                // Convert path to Ident vector
                                                let ident_path = path.iter().map(|s| Ident(s.clone())).collect::<Vec<_>>();
                                                return self.resolve_glob_import(&ident_path, in_scope, symbol_table);
                                            }
                                            
                                            // If we get here, we're at a leaf - it's a regular import
                                            if let UseTreeKind::Path { segment: leaf_segment, .. } = &sub_tree_inner.kind {
                                                println!("Resolving nested path import: {}::{}", path.join("::"), leaf_segment.0);
                                                
                                                // Find the module for the path
                                                let module_result = symbol_table.find_module_by_path(in_scope, &path);
                                                
                                                if let Some((_, module_scope)) = module_result {
                                                    // Find the symbol to import in the module's scope
                                                    let symbols = symbol_table.lookup_in_scope(module_scope, &leaf_segment.0);
                                                    
                                                    if symbols.is_empty() {
                                                        return Err(self.create_import_error(&leaf_segment.0, "Symbol not found", sub_tree_inner.span));
                                                    }
                                                    
                                                    // Check if the symbol is public
                                                    let symbol = symbols[0];
                                                    if !symbol.is_public() {
                                                        return Err(self.create_import_error(&leaf_segment.0, "Cannot import private symbol", sub_tree_inner.span));
                                                    }
                                                    
                                                    // Create the import
                                                    let import_name = alias.clone().unwrap_or_else(|| leaf_segment.clone());
                                                    let symbol = symbol.clone();
                                                    
                                                    println!("Adding import for {} as {}", leaf_segment.0, import_name.0);
                                                    
                                                    let import_symbol = Symbol::Import {
                                                        name: import_name,
                                                        target: Box::new(symbol.clone()),
                                                        span: sub_tree_inner.span,
                                                        defined_in: in_scope,
                                                    };
                                                    
                                                    // Add the import to the scope
                                                    symbol_table.add_symbol_to_scope(in_scope, import_symbol);
                                                    
                                                    return Ok(());
                                                } else {
                                                    return Err(self.create_import_error(&path.join("::"), "Module path not found", tree.span));
                                                }
                                            }
                                        }
                                        
                                        break;
                                    } else {
                                        break;
                                    }
                                }
                                
                                // Handle regular path import if the loop didn't return
                                println!("Resolving path import: {}::<subtree>", segment.0);
                                
                                // First, find the module for the first segment
                                let first_segment_symbols = symbol_table.lookup_in_scope(in_scope, &segment.0);
                                
                                println!("Looking up '{}' in scope: found {} symbols", 
                                       segment.0, first_segment_symbols.len());
                                
                                if first_segment_symbols.is_empty() {
                                    return Err(self.create_import_error(&segment.0, "Module not found", tree.span));
                                }
                                
                                // Find the module symbol
                                let module_id = first_segment_symbols.iter().find_map(|sym| {
                                    if let Symbol::Module { id, .. } = sym {
                                        Some(*id)
                                    } else {
                                        None
                                    }
                                });
                                
                                if let Some(module_id) = module_id {
                                    println!("Found module with ID: {:?}", module_id);
                                    
                                    // Get the scope for the module
                                    let module_scopes = symbol_table.get_scopes_for_module(module_id);
                                    
                                    if module_scopes.is_empty() {
                                        return Err(self.create_import_error(&segment.0, "Module has no scope", tree.span));
                                    }
                                    
                                    let module_scope = module_scopes[0];
                                    println!("Module scope: {:?}", module_scope);
                                    
                                    // Resolve the subtree in the module's scope
                                    self.resolve_use_tree(sub_tree, module_scope, symbol_table)
                                } else {
                                    Err(self.create_import_error(&segment.0, "Not a module", tree.span))
                                }
                            }
                        }
                    }
                } else {
                    // This is a single item import (e.g., a)
                    println!("Resolving simple import: {}", segment.0);
                    
                    // Look up the symbol in the current scope
                    let symbols = symbol_table.lookup_in_scope(in_scope, &segment.0);
                    
                    if symbols.is_empty() {
                        return Err(self.create_import_error(&segment.0, "Symbol not found", tree.span));
                    }
                    
                    // Check if the symbol is public
                    let symbol = symbols[0];
                    if !symbol.is_public() {
                        return Err(self.create_import_error(&segment.0, "Cannot import private symbol", tree.span));
                    }
                    
                    // Create an import for the symbol - use the alias if provided
                    let import_name = alias.clone().unwrap_or_else(|| segment.clone());
                    let name_str = import_name.0.clone(); // Save the name before moving
                    let symbol = symbol.clone();
                    
                    let import_symbol = Symbol::Import {
                        name: import_name,
                        target: Box::new(symbol.clone()),
                        span: tree.span,
                        defined_in: in_scope,
                    };
                    
                    // Add the import to the current scope
                    symbol_table.add_symbol_to_scope(in_scope, import_symbol);
                    
                    println!("Added import symbol: {}", name_str);
                    Ok(())
                }
            }
            UseTreeKind::Glob => {
                // This is a glob import at the root (e.g., *)
                println!("Resolving root-level glob import");
                
                // Import all symbols from the current scope to the current scope
                self.glob_import_from_scope(in_scope, in_scope, symbol_table)
            }
            UseTreeKind::Group(trees) => {
                // Process a group of import trees
                println!("Resolving group import with {} items", trees.len());
                
                // Group imports at the top level (without a parent path) should not happen
                // This is the error case that was triggered in the test
                println!("Group import without parent path - this should not happen");
                return Err(self.create_import_error("group import", "Group import without parent path", tree.span));
            }
        }
    }
    
    /// Resolve a glob import (e.g., use a::b::*)
    pub fn resolve_glob_import(
        &self,
        path: &[Ident],
        in_scope: ScopeId,
        symbol_table: &mut SymbolTable,
    ) -> Result<()> {
        println!("Resolving glob import: {:?}", path);
        
        if path.is_empty() {
            // This is a direct glob import from the current scope
            // Just copy all symbols from the current scope to the target scope
            // This is a no-op since we're already in the scope
            return Ok(());
        }
        
        // Convert path to string slices for symbol table lookup
        let path_strings: Vec<String> = path.iter().map(|ident| ident.0.clone()).collect();
        
        // Look up the module by path
        let module_result = symbol_table.find_module_by_path(in_scope, &path_strings);
        
        if let Some((module_id, module_scope)) = module_result {
            println!("Found module for glob import, importing all symbols from module_id: {:?}", module_id);
            // Import all symbols from the module scope
            self.glob_import_from_scope(module_scope, in_scope, symbol_table)?;
            return Ok(());
        }
        
        // If we reach here, the module was not found
        Err(self.create_import_error(
            &path.iter().map(|ident| ident.0.clone()).collect::<Vec<_>>().join("::"),
            "module not found for glob import",
            Span { start: 0, end: 0 }, // We don't have a span here, but that's fine for tests
        ))
    }
    
    /// Helper to glob import all symbols from one scope into another
    fn glob_import_from_scope(
        &self,
        from_scope: ScopeId,
        to_scope: ScopeId,
        symbol_table: &mut SymbolTable,
    ) -> Result<()> {
        // Now import all symbols from the target module
        let all_symbols = symbol_table.get_all_symbols_in_scope(from_scope);
        let mut count = 0;
        
        println!("Glob importing from scope {:?} to scope {:?}", from_scope, to_scope);
        println!("Found {} symbols to potentially import", all_symbols.len());
        
        // Debug: Print all symbols being imported
        for sym in &all_symbols {
            println!("  Symbol available for import: {} (public: {})", sym.name().0, sym.is_public());
        }
        
        for symbol in all_symbols {
            // Skip internal or private symbols
            if !symbol.is_public() || symbol.name().0.starts_with('_') {
                println!("Skipping non-public symbol: {}", symbol.name().0);
                continue;
            }
            
            println!("Importing symbol: {}", symbol.name().0);
            
            // Create an import for the symbol
            let import_symbol = Symbol::Import {
                name: symbol.name().clone(),
                target: Box::new(symbol.clone()),
                span: Span { start: 0, end: 0 }, // Default span
                defined_in: to_scope,
            };
            
            // Add the import to the target scope
            symbol_table.add_symbol_to_scope(to_scope, import_symbol);
            count += 1;
            
            // Verify that the symbol was added correctly
            let imported = symbol_table.lookup_in_scope(to_scope, &symbol.name().0);
            if imported.is_empty() {
                println!("Warning: Failed to add import for {} to scope {:?}", symbol.name().0, to_scope);
            } else {
                println!("Successfully added import for {} to scope {:?}", symbol.name().0, to_scope);
                
                // Debug: Print details about the imported symbol
                for (i, sym) in imported.iter().enumerate() {
                    match sym {
                        Symbol::Import { name, target, .. } => {
                            println!("  Import {} - name: {}, target type: {:?}", 
                                    i, name.0, std::mem::discriminant(&**target));
                        },
                        _ => println!("  Non-import symbol: {:?}", sym),
                    }
                }
            }
        }
        
        // After all imports, verify what's in the target scope
        let target_scope_symbols = symbol_table.get_all_symbols_in_scope(to_scope);
        println!("After import, target scope has {} symbols", target_scope_symbols.len());
        
        println!("Added {} import symbols via glob import", count);
        Ok(())
    }

    /// Helper function to extract a full nested path from a use tree
    fn extract_nested_path(&self, tree: &UseTree) -> (Vec<String>, Option<(String, Option<Ident>)>) {
        // This function extracts a full path from a nested use tree structure
        // Returns the path segments and the final segment with optional alias
        
        let mut path = Vec::new();
        let mut current_tree = tree;
        let mut final_segment = None;
        
        loop {
            if let UseTreeKind::Path { segment, sub_tree, alias } = &current_tree.kind {
                path.push(segment.0.clone());
                
                if let Some(sub_tree) = sub_tree {
                    if let UseTreeKind::Path { .. } = &sub_tree.kind {
                        current_tree = sub_tree;
                    } else if let UseTreeKind::Glob = &sub_tree.kind {
                        // If it's a glob import, we're done but no final segment
                        return (path, None);
                    } else {
                        // For other kinds, we've reached the end
                        if let UseTreeKind::Path { segment: leaf_segment, alias: leaf_alias, .. } = &sub_tree.kind {
                            final_segment = Some((leaf_segment.0.clone(), leaf_alias.clone()));
                        }
                        break;
                    }
                } else {
                    // Final segment in the path with possible alias
                    final_segment = Some((segment.0.clone(), alias.clone()));
                    break;
                }
            } else {
                break;
            }
        }
        
        (path, final_segment)
    }
    
    /// Helper function to handle deep nested imports (e.g., a::b::c::name)
    fn resolve_deep_nested_import(
        &self,
        tree: &UseTree,
        in_scope: ScopeId,
        symbol_table: &mut SymbolTable
    ) -> Result<()> {
        let (mut path_segments, final_item) = self.extract_nested_path(tree);
        
        if path_segments.is_empty() {
            return Ok(());
        }
        
        println!("Resolving deep nested import: {:?} with final item: {:?}", 
                path_segments, final_item);
        
        // Special test mode handling - keep the variable for compatibility but don't
        // use it for special module naming conventions anymore
        let _is_test = cfg!(test) || self.source_code.contains("// Test mode") || 
                      self.crate_root.to_string_lossy().contains("<test>");
        
        // Handle 'super' in the path
        let mut current_scope = in_scope;
        let mut start_idx = 0;
        
        // Check if the path starts with 'super'
        if !path_segments.is_empty() && path_segments[0] == "super" {
            // Get the module for the current scope
            let current_module = symbol_table.get_module_for_scope(current_scope);
            
            // Get the scope where the current module is defined
            let module_scopes = symbol_table.get_scopes_for_module(current_module);
            if module_scopes.is_empty() {
                return Err(self.create_import_error("super", 
                    "Module has no scope", tree.span));
            }
            
            let module_scope = module_scopes[0];
            
            // Get the parent scope
            let parent_scope = symbol_table.get_parent_scope(module_scope);
            if parent_scope.is_none() {
                return Err(self.create_import_error("super", 
                    "No parent module exists", tree.span));
            }
            
            let parent_scope = parent_scope.unwrap();
            
            // Get the parent module ID
            let parent_module = symbol_table.get_module_for_scope(parent_scope);
            
            // Get the parent module's scope
            let parent_module_scopes = symbol_table.get_scopes_for_module(parent_module);
            if parent_module_scopes.is_empty() {
                return Err(self.create_import_error("super", 
                    "Parent module has no scope", tree.span));
            }
            
            // Start from the parent module's scope
            current_scope = parent_module_scopes[0];
            
            // Skip the 'super' segment
            start_idx = 1;
            
            println!("Deep import: Found 'super', using parent module scope: {:?}", 
                    current_scope);
        }
        
        // Create a path slice starting from after any 'super'
        let path_slice = &path_segments[start_idx..];
        
        // Check if this is a glob import (no final item)
        if final_item.is_none() {
            println!("Glob import from nested path: {:?}", path_slice);
            
            // Convert the segments to a list of Idents for glob_import
            let idents = path_slice.iter()
                .map(|s| Ident(s.clone()))
                .collect::<Vec<_>>();
            
            // Use the glob import method
            return self.resolve_glob_import(&idents, in_scope, symbol_table);
        }
        
        // If we have a final item (not a glob import), handle it
        if let Some((final_name, alias)) = final_item {
            // In case of a::b::deep_func, we need to extract 'deep_func' from the path segments
            // and process a::b as the module path
            if path_segments.len() > 1 && final_name == *path_segments.last().unwrap() {
                // Remove the last segment, which is the name of the entity to import
                path_segments.pop();
            }
            
            println!("Looking up module path starting from segment {}: {}", 
                    start_idx, path_segments[start_idx..].join("::"));
            
            // Print the current scope
            let all_symbols = symbol_table.get_all_symbols_in_scope(current_scope);
            println!("Symbols in current scope: {}", all_symbols.len());
            for sym in &all_symbols {
                println!("  Symbol in current scope: {}", sym.name().0);
            }
            
            // Create a path slice starting from the segment after 'super'
            let path_slice = &path_segments[start_idx..];
            
            // If there are no remaining segments, resolve the item directly from the current scope
            if path_slice.is_empty() {
                println!("Resolving item '{}' directly from scope {:?}", final_name, current_scope);
                
                // Find the symbol in the current scope
                let symbols = symbol_table.lookup_in_scope(current_scope, &final_name);
                
                if symbols.is_empty() {
                    return Err(self.create_import_error(&final_name, 
                        "Symbol not found", tree.span));
                }
                
                // Check if the symbol is public
                let symbol = symbols[0];
                if !symbol.is_public() {
                    return Err(self.create_import_error(&final_name, 
                        "Cannot import private symbol", tree.span));
                }
                
                // Create the import symbol with proper alias if provided
                let import_name = if let Some(alias_ident) = alias {
                    alias_ident
                } else {
                    symbol.name().clone()
                };
                
                let import_symbol = Symbol::Import {
                    name: import_name,
                    target: Box::new(symbol.clone()),
                    span: tree.span,
                    defined_in: in_scope,
                };
                
                // Add the import to the target scope
                symbol_table.add_symbol_to_scope(in_scope, import_symbol);
                return Ok(());
            }
            
            // Find the module by path
            let module_result = symbol_table.find_module_by_path(current_scope, 
                             &path_slice.iter().map(|s| s.clone()).collect::<Vec<_>>());
            
            if module_result.is_none() {
                return Err(self.create_import_error(
                    &path_segments.join("::"),
                    "module not found",
                    tree.span,
                ));
            }
            
            return self.resolve_import_from_module_path(
                module_result.unwrap(), final_name, alias, in_scope, tree.span, symbol_table);
        }
        
        Ok(())
    }

    /// Helper function to handle 'super' import
    fn resolve_super_import(
        &self,
        tree: &UseTree,
        in_scope: ScopeId,
        symbol_table: &mut SymbolTable,
    ) -> Result<()> {
        println!("Handling 'super' import");
        
        // Get the module for the current scope
        let current_module = symbol_table.get_module_for_scope(in_scope);
        println!("Current module ID: {:?}", current_module);
        
        // Get the scope where the current module is defined
        let module_scopes = symbol_table.get_scopes_for_module(current_module);
        if module_scopes.is_empty() {
            return Err(self.create_import_error("super", "Module has no scope", tree.span));
        }
        
        let current_module_scope = module_scopes[0];
        println!("Current module primary scope: {:?}", current_module_scope);
        
        // Get the parent scope
        let parent_scope = symbol_table.get_parent_scope(current_module_scope);
        if parent_scope.is_none() {
            return Err(self.create_import_error("super", "No parent module exists", tree.span));
        }
        
        let parent_scope = parent_scope.unwrap();
        println!("Parent scope: {:?}", parent_scope);
        
        // Get the parent module ID
        let parent_module = symbol_table.get_module_for_scope(parent_scope);
        println!("Parent module ID: {:?}", parent_module);
        
        // Get the parent module's scope
        let parent_module_scopes = symbol_table.get_scopes_for_module(parent_module);
        if parent_module_scopes.is_empty() {
            return Err(self.create_import_error("super", "Parent module has no scope", tree.span));
        }
        
        let parent_module_scope = parent_module_scopes[0];
        println!("Parent module primary scope: {:?}", parent_module_scope);
        
        // Now resolve the subtree from the parent module's scope
        if let UseTreeKind::Path { sub_tree, .. } = &tree.kind {
            if let Some(sub_tree) = sub_tree {
                println!("Resolving subtree from parent module scope");
                return self.resolve_use_tree(sub_tree, parent_module_scope, symbol_table);
            }
        }
        
        // If there's no subtree, import the parent module itself (unusual case)
        Err(self.create_import_error("super", "Super import without path not supported", tree.span))
    }

    /// Helper method to resolve an import from a given module path result
    fn resolve_import_from_module_path(
        &self,
        module_path_result: (ModuleId, ScopeId),
        item_name: String,
        alias: Option<Ident>,
        in_scope: ScopeId,
        span: Span,
        symbol_table: &mut SymbolTable
    ) -> Result<()> {
        let (module_id, module_scope) = module_path_result;
        return self.resolve_import_from_module(
            module_id, module_scope, item_name, alias, in_scope, span, symbol_table);
    }

    /// Helper method to resolve an import from a given module
    fn resolve_import_from_module(
        &self,
        module_id: ModuleId,
        module_scope: ScopeId,
        item_name: String,
        alias: Option<Ident>,
        in_scope: ScopeId,
        span: Span,
        symbol_table: &mut SymbolTable
    ) -> Result<()> {
        println!("Found module ID: {:?} with scope: {:?}", module_id, module_scope);
        
        // List all scopes for this module to help debugging
        let all_scopes = symbol_table.get_scopes_for_module(module_id);
        println!("All scopes for module {:?}: {:?}", module_id, all_scopes);
        
        // Find the symbol in the module's scope
        let symbols = symbol_table.lookup_in_scope(module_scope, &item_name);
        
        if symbols.is_empty() {
            // Try looking in all scopes for this module
            for scope in &all_scopes {
                let scope_symbols = symbol_table.lookup_in_scope(*scope, &item_name);
                if !scope_symbols.is_empty() {
                    println!("Found symbol '{}' in alternate scope {:?}", item_name, scope);
                    
                    // Check if the symbol is public
                    let symbol = scope_symbols[0];
                    if !symbol.is_public() {
                        return Err(self.create_import_error(&item_name, 
                            "Cannot import private symbol", span));
                    }
                    
                    // Create the import with the given alias or original name
                    let import_name = alias.unwrap_or_else(|| symbol.name().clone());
                    let symbol = symbol.clone();
                    
                    println!("Adding deep import for {} as {}", item_name, import_name.0);
                    
                    let import_symbol = Symbol::Import {
                        name: import_name.clone(),
                        target: Box::new(symbol),
                        span,
                        defined_in: in_scope,
                    };
                    
                    // Add the import to the target scope
                    symbol_table.add_symbol_to_scope(in_scope, import_symbol);
                    
                    // Verify the symbol was added
                    let imported = symbol_table.lookup_in_scope(in_scope, &import_name.0);
                    if imported.is_empty() {
                        println!("Warning: Failed to add import for {} to scope {:?}", import_name.0, in_scope);
                    } else {
                        println!("Successfully added deep import for {} to scope {:?}", import_name.0, in_scope);
                    }
                    
                    return Ok(());
                }
            }
            
            println!("Symbol '{}' not found in module scope {:?}", item_name, module_scope);
            return Err(self.create_import_error(&item_name, 
                "Symbol not found in module", span));
        }
        
        // Check if the symbol is public
        let symbol = symbols[0];
        if !symbol.is_public() {
            return Err(self.create_import_error(&item_name, 
                "Cannot import private symbol", span));
        }
        
        // Create the import with the given alias or original name
        let import_name = alias.unwrap_or_else(|| symbol.name().clone());
        let symbol = symbol.clone();
        
        println!("Adding deep import for {} as {}", item_name, import_name.0);
        
        let import_symbol = Symbol::Import {
            name: import_name.clone(),
            target: Box::new(symbol),
            span,
            defined_in: in_scope,
        };
        
        // Add the import to the target scope
        symbol_table.add_symbol_to_scope(in_scope, import_symbol);
        
        // Verify the symbol was added
        let imported = symbol_table.lookup_in_scope(in_scope, &import_name.0);
        if imported.is_empty() {
            println!("Warning: Failed to add import for {} to scope {:?}", import_name.0, in_scope);
        } else {
            println!("Successfully added deep import for {} to scope {:?}", import_name.0, in_scope);
        }
        
        Ok(())
    }
} 