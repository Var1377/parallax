//! Import resolution for name resolution.
//!
//! This module handles the resolution of `use` declarations, managing the logic
//! for resolving paths, glob imports, nested imports, and import renaming.

use std::{path::PathBuf, sync::Arc};
use crate::{
    error::ResolveError,
    symbol::{Symbol, SymbolTable, ScopeId, ModuleId},
    Result,
};
use parallax_lang::ast::{
    common::{Ident, Span},
    items::{UseDecl, UseTree, UseTreeKind},
};

/// A resolver for import declarations.
pub struct ImportResolver<'a> {
    /// The symbol table to resolve imports against
    symbol_table: &'a mut SymbolTable,
    /// The current file's path
    file_path: PathBuf,
    /// The current file's source code
    source_code: String,
    /// Accumulated errors during resolution
    errors: Vec<ResolveError>,
}

impl<'a> ImportResolver<'a> {
    /// Create a new import resolver.
    pub fn new(
        symbol_table: &'a mut SymbolTable,
        file_path: PathBuf,
        source_code: String,
    ) -> Self {
        Self {
            symbol_table,
            file_path,
            source_code,
            errors: Vec::new(),
        }
    }

    /// Resolve an import declaration.
    pub fn resolve_import(&mut self, import: &UseDecl) -> Result<()> {
        self.resolve_use_tree(&import.tree, None)?;
        
        // Return an error if any were accumulated during resolution
        if let Some(err) = self.errors.pop() {
            return Err(err);
        }
        
        Ok(())
    }

    /// Resolve a use tree.
    fn resolve_use_tree(&mut self, tree: &UseTree, prefix: Option<Vec<Ident>>) -> Result<()> {
        match &tree.kind {
            UseTreeKind::Path {
                segment,
                alias,
                sub_tree,
            } => {
                let mut new_prefix = prefix.unwrap_or_default();
                new_prefix.push(segment.clone());
                
                if let Some(sub_tree) = sub_tree {
                    self.resolve_use_tree(sub_tree, Some(new_prefix))?;
                } else {
                    // This is a leaf node, so we need to resolve the path and import the symbol
                    let target_name = segment.0.clone();
                    let import_name = alias.as_ref().map(|i| i.0.clone()).unwrap_or(target_name);
                    
                    self.resolve_path_import(&new_prefix, import_name, tree.span)?;
                }
            }
            UseTreeKind::Group(use_trees) => {
                for subtree in use_trees {
                    self.resolve_use_tree(subtree, prefix.clone())?;
                }
            }
            UseTreeKind::Glob => {
                self.resolve_glob_import(prefix.unwrap_or_default(), tree.span)?;
            }
        }
        
        Ok(())
    }

    /// Resolve a simple path import (e.g., `use foo::bar` or `use foo::bar as baz`).
    fn resolve_path_import(
        &mut self,
        path: &[Ident],
        import_as: String,
        span: Span,
    ) -> Result<()> {
        // First, find the module
        let mut current_scope = self.symbol_table.root_scope();
        let mut path_iter = path.iter().peekable();
        
        // Skip the first segment if it's "crate" (direct reference to root)
        if let Some(first) = path_iter.peek() {
            if first.0 == "crate" {
                path_iter.next();
                current_scope = self.symbol_table.root_scope();
            }
        }
        
        // Navigate the path to find the target module
        while let Some(segment) = path_iter.next() {
            if path_iter.peek().is_none() {
                // This is the last segment, which is the item to import
                break;
            }
            
            // Look for a module with this name
            let module_symbols = self.symbol_table.lookup_in_scope(current_scope, &segment.0);
            let module = module_symbols.iter().find_map(|sym| {
                if let Symbol::Module { id, .. } = sym {
                    Some(*id)
                } else {
                    None
                }
            });
            
            if let Some(module_id) = module {
                // Find the scope for this module
                let module_scopes = self.symbol_table.get_scopes_for_module(module_id);
                
                if let Some(&scope_id) = module_scopes.first() {
                    current_scope = scope_id;
                } else {
                    // Module found but no scope for it
                    self.errors.push(ResolveError::import_error(
                        segment.0.clone(),
                        "Module exists but scope not found".to_string(),
                        span,
                        self.file_path.clone(),
                        self.source_code.clone(),
                    ));
                    return Ok(());
                }
            } else {
                // Module not found
                self.errors.push(ResolveError::import_error(
                    segment.0.clone(),
                    "Module not found".to_string(),
                    span,
                    self.file_path.clone(),
                    self.source_code.clone(),
                ));
                return Ok(());
            }
        }
        
        // Now find the actual item to import
        let last_segment = path.last().unwrap();
        let symbols = self.symbol_table.lookup_in_scope(current_scope, &last_segment.0);
        
        if symbols.is_empty() {
            self.errors.push(ResolveError::import_error(
                last_segment.0.clone(),
                "Symbol not found".to_string(),
                span,
                self.file_path.clone(),
                self.source_code.clone(),
            ));
            return Ok(());
        }
        
        // Process each symbol to add to the current scope
        // We'll first collect the symbols to import to avoid borrowing issues
        let mut symbols_to_import = Vec::new();
        
        for symbol in symbols {
            // Only import public symbols
            if !symbol.is_public() {
                self.errors.push(ResolveError::visibility_violation(
                    last_segment.0.clone(),
                    span,
                    self.file_path.clone(),
                    self.source_code.clone(),
                ));
                continue;
            }
            
            // Create an import symbol
            let import_symbol = Symbol::Import {
                name: Ident(import_as.clone()),
                target: Box::new(symbol.clone()),
                span,
                defined_in: self.symbol_table.current_scope(),
            };
            
            symbols_to_import.push(import_symbol);
        }
        
        // Now add all the symbols to the current scope
        for symbol in symbols_to_import {
            self.symbol_table.add_symbol(symbol);
        }
        
        Ok(())
    }

    /// Resolve a glob import (e.g., `use foo::*`).
    fn resolve_glob_import(&mut self, path: Vec<Ident>, span: Span) -> Result<()> {
        // First, find the module
        let mut current_scope = self.symbol_table.root_scope();
        let mut path_iter = path.iter();
        
        while let Some(segment) = path_iter.next() {
            // Look for a module with this name
            let module_symbols = self.symbol_table.lookup_in_scope(current_scope, &segment.0);
            let module = module_symbols.iter().find_map(|sym| {
                if let Symbol::Module { id, .. } = sym {
                    Some(*id)
                } else {
                    None
                }
            });
            
            if let Some(module_id) = module {
                // Find the scope for this module
                let module_scopes = self.symbol_table.get_scopes_for_module(module_id);
                
                if let Some(&scope_id) = module_scopes.first() {
                    current_scope = scope_id;
                } else {
                    // Module found but no scope for it
                    self.errors.push(ResolveError::import_error(
                        segment.0.clone(),
                        "Module exists but scope not found".to_string(),
                        span,
                        self.file_path.clone(),
                        self.source_code.clone(),
                    ));
                    return Ok(());
                }
            } else {
                // Module not found
                self.errors.push(ResolveError::import_error(
                    segment.0.clone(),
                    "Module not found".to_string(),
                    span,
                    self.file_path.clone(),
                    self.source_code.clone(),
                ));
                return Ok(());
            }
        }
        
        // Current scope is now the scope of the module we want to glob import from
        // Get all public symbols from this scope
        let symbols = self.symbol_table.get_all_symbols_in_scope(current_scope);
        
        // Collect symbols to import to avoid borrowing issues
        let mut symbols_to_import = Vec::new();
        
        for symbol in symbols {
            if symbol.is_public() {
                // Create an import symbol
                let import_symbol = Symbol::Import {
                    name: symbol.name().clone(),
                    target: Box::new(symbol.clone()),
                    span,
                    defined_in: self.symbol_table.current_scope(),
                };
                
                symbols_to_import.push(import_symbol);
            }
        }
        
        // Now add all the symbols to the current scope
        for symbol in symbols_to_import {
            self.symbol_table.add_symbol(symbol);
        }
        
        Ok(())
    }

    /// Get any errors accumulated during resolution.
    pub fn take_errors(&mut self) -> Vec<ResolveError> {
        std::mem::take(&mut self.errors)
    }
} 