//! Type resolution implementation

use std::path::PathBuf;
use parallax_lang::ast::{
    types::{Type, TypeKind},
    items::WhereClause,
    common::{Ident, Span},
};
use crate::{
    Result,
    symbol::{SymbolTable, Symbol},
    error::ResolveError,
    namespace::Namespace,
};

use super::path::PathResolver;

/// Resolver for types
pub struct TypeResolver {
    /// Path to the source file
    path: PathBuf,
    /// Source code content
    source: String,
    /// Path resolver for resolving path segments
    path_resolver: PathResolver,
}

impl Default for TypeResolver {
    fn default() -> Self {
        Self {
            path: PathBuf::new(),
            source: String::new(),
            path_resolver: PathResolver::new(),
        }
    }
}

impl TypeResolver {
    /// Create a new type resolver
    pub fn new() -> Self {
        Self {
            path: PathBuf::new(),
            source: String::new(),
            path_resolver: PathResolver::new(),
        }
    }
    
    /// Initialize with source information
    pub fn init(&mut self, path: &PathBuf, source: &String) {
        self.path = path.clone();
        self.source = source.clone();
        self.path_resolver.init(path, source);
    }
    
    /// Resolve a type, ensuring it exists in the symbol table
    pub fn resolve_type(&self, ty: &Type, symbol_table: &SymbolTable) -> Result<()> {
        match &ty.kind {
            TypeKind::Path(segments) => {
                // For path types, we need to verify each segment exists and is accessible
                self.resolve_path_type(segments, ty.span, symbol_table)?;
                Ok(())
            }
            TypeKind::Function(param_type, return_type) => {
                // Recursively resolve parameter and return types
                self.resolve_type(param_type, symbol_table)?;
                self.resolve_type(return_type, symbol_table)?;
                Ok(())
            }
            TypeKind::Tuple(element_types) => {
                // Recursively resolve each element type
                for element_type in element_types {
                    self.resolve_type(element_type, symbol_table)?;
                }
                Ok(())
            }
            TypeKind::Array(element_type, _) => {
                // Resolve the element type
                self.resolve_type(element_type, symbol_table)?;
                Ok(())
            }
            TypeKind::KindApp(base_type, type_args) => {
                // Resolve the base type
                self.resolve_type(base_type, symbol_table)?;
                
                // Resolve each type argument
                for type_arg in type_args {
                    self.resolve_type(type_arg, symbol_table)?;
                }
                Ok(())
            }
        }
    }
    
    /// Process a where clause, resolving all types and bounds
    pub fn process_where_clause(&self, where_clause: &Option<WhereClause>, symbol_table: &SymbolTable) -> Result<()> {
        if let Some(where_clause) = where_clause {
            for predicate in &where_clause.predicates {
                // Resolve the type being constrained
                self.resolve_type(&predicate.ty, symbol_table)?;
                
                // Resolve each trait bound
                for bound in &predicate.bounds {
                    self.resolve_type(bound, symbol_table)?;
                }
            }
        }
        Ok(())
    }
    
    /// Resolve a path type (e.g., module::Type)
    fn resolve_path_type(&self, segments: &[Ident], span: Span, symbol_table: &SymbolTable) -> Result<()> {
        if segments.is_empty() {
            return Ok(());
        }
        
        // Use the path resolver to resolve the path, passing type namespace
        let mut diagnostics = Vec::new();
        let current_scope = symbol_table.current_scope();
        
        match self.path_resolver.resolve_path(
            segments,
            span,
            symbol_table,
            current_scope,
            &mut diagnostics,
            Namespace::Types, // Specify that we're looking for types
        ) {
            Ok(Some(symbol)) => {
                // Verify that the resolved symbol is a type
                if !self.is_type_symbol(&symbol) {
                    return Err(ResolveError::generic_error(
                        format!("'{}' is not a type", segments.last().unwrap().0),
                        span,
                        self.path.clone(),
                        self.source.clone(),
                    ));
                }
                Ok(())
            },
            Ok(None) => {
                // If we didn't get an explicit error but failed to resolve, report as undefined
                if diagnostics.is_empty() {
                    Err(ResolveError::undefined_name(
                        segments.last().unwrap().0.clone(),
                        span,
                        self.path.clone(),
                        self.source.clone(),
                    ))
                } else {
                    // Use the first diagnostic as our error
                    Err(diagnostics.remove(0))
                }
            },
            Err(err) => Err(err),
        }
    }
    
    /// Check if a symbol represents a type
    fn is_type_symbol(&self, symbol: &Symbol) -> bool {
        match symbol {
            Symbol::Struct { .. } | 
            Symbol::Enum { .. } | 
            Symbol::TypeParam { .. } |
            Symbol::Trait { .. } => true,
            Symbol::Import { target, .. } => {
                // For imports, check the target
                matches!(&**target,
                    Symbol::Struct { .. } | 
                    Symbol::Enum { .. } | 
                    Symbol::TypeParam { .. } |
                    Symbol::Trait { .. }
                )
            },
            _ => false,
        }
    }
} 