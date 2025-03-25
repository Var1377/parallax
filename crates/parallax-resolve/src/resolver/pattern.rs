//! Pattern resolution implementation

use std::path::PathBuf;
use crate::{
    error::ResolveError,
    namespace::Namespace,
    symbol::{Symbol, SymbolTable, ScopeId},
    Result,
};
use parallax_lang::ast::pattern::{Pattern, PatternKind};

use super::path::PathResolver;

/// Resolver for patterns
pub struct PatternResolver {
    /// Path to the source file
    path: PathBuf,
    /// Source code content
    source: String,
}

impl PatternResolver {
    /// Create a new pattern resolver
    pub fn new() -> Self {
        Self {
            path: PathBuf::new(),
            source: String::new(),
        }
    }
    
    /// Initialize with source information
    pub fn init(&mut self, path: &PathBuf, source: &String) {
        self.path = path.clone();
        self.source = source.clone();
    }
    
    /// Resolve a pattern, adding any bindings to the symbol table
    pub fn resolve_pattern(
        &self,
        pattern: &Pattern,
        symbol_table: &mut SymbolTable,
        diagnostics: &mut Vec<ResolveError>,
        current_scope: ScopeId,
    ) -> Result<()> {
        match &pattern.kind {
            PatternKind::Identifier(ident) => {
                // Simple identifier binding
                if ident.0 != "_" {  // Skip wildcard identifiers
                    let var_symbol = Symbol::Variable {
                        name: ident.clone(),
                        ty: None,
                        span: pattern.span,
                        defined_in: current_scope,
                    };
                    
                    // Add the variable to the current scope
                    symbol_table.add_symbol_to_scope(current_scope, var_symbol);
                }
            }
            PatternKind::Literal(_) => {
                // Literal patterns don't introduce bindings
            }
            PatternKind::Constructor { path, args } => {
                // First, verify the constructor path exists
                let mut path_resolver = PathResolver::new();
                path_resolver.init(&self.path, &self.source);
                
                let _ = path_resolver.resolve_path(
                    path,
                    pattern.span,
                    symbol_table,
                    current_scope,
                    diagnostics,
                    Namespace::Types,
                )?;
                
                // Then resolve the argument pattern
                self.resolve_pattern(args, symbol_table, diagnostics, current_scope)?;
            }
            PatternKind::Tuple(elements) => {
                // Process each element of the tuple pattern
                for elem in elements {
                    self.resolve_pattern(elem, symbol_table, diagnostics, current_scope)?;
                }
            }
            PatternKind::Array(elements) => {
                // Process each element of the array pattern
                for elem in elements {
                    self.resolve_pattern(elem, symbol_table, diagnostics, current_scope)?;
                }
            }
            PatternKind::Struct { path, fields } => {
                // First, verify the struct type exists
                let mut path_resolver = PathResolver::new();
                path_resolver.init(&self.path, &self.source);
                
                let _ = path_resolver.resolve_path(
                    path,
                    pattern.span,
                    symbol_table,
                    current_scope,
                    diagnostics,
                    Namespace::Types,
                )?;
                
                // Process each field
                for field in fields {
                    if let Some(pat) = &field.pattern {
                        // Process the pattern
                        self.resolve_pattern(pat, symbol_table, diagnostics, current_scope)?;
                    } else {
                        // Shorthand field (e.g. { x } instead of { x: x })
                        // Add the field name as a binding
                        let var_symbol = Symbol::Variable {
                            name: field.name.clone(),
                            ty: None,
                            span: field.span,
                            defined_in: current_scope,
                        };
                        
                        symbol_table.add_symbol_to_scope(current_scope, var_symbol);
                    }
                }
            }
            PatternKind::Or(left, right) => {
                // Both branches in an or-pattern must bind the same variables
                // We'll collect bindings from both sides and ensure they match
                
                // Create temporary scopes to collect bindings
                let left_scope = symbol_table.push_scope(None);
                let right_scope = symbol_table.push_scope(None);
                
                // Resolve both sides
                self.resolve_pattern(left, symbol_table, diagnostics, left_scope)?;
                self.resolve_pattern(right, symbol_table, diagnostics, right_scope)?;
                
                // Get bindings from both sides
                let left_bindings = symbol_table.get_all_symbols_in_scope(left_scope);
                let right_bindings = symbol_table.get_all_symbols_in_scope(right_scope);
                
                // Check that both sides bind the same set of variables
                let left_names: Vec<String> = left_bindings.iter().map(|s| s.name().0.clone()).collect();
                let right_names: Vec<String> = right_bindings.iter().map(|s| s.name().0.clone()).collect();
                
                for name in &left_names {
                    if !right_names.contains(name) {
                        diagnostics.push(ResolveError::generic_error(
                            format!("Variable '{}' is bound in one branch of an or-pattern but not the other", name),
                            pattern.span,
                            self.path.clone(),
                            self.source.clone(),
                        ));
                    }
                }
                
                // Add the bindings to the current scope
                for binding in left_bindings {
                    if let Symbol::Variable { name, ty: _, span, .. } = binding {
                        let var_symbol = Symbol::Variable {
                            name: name.clone(),
                            ty: None, // Type would be determined during type checking
                            span,
                            defined_in: current_scope,
                        };
                        
                        symbol_table.add_symbol_to_scope(current_scope, var_symbol);
                    }
                }
                
                // Clean up the temporary scopes
                symbol_table.pop_scope();
                symbol_table.pop_scope();
            }
            PatternKind::Rest | PatternKind::Wildcard => {
                // Rest and wildcard patterns don't introduce bindings
            }
        }
        
        Ok(())
    }
} 