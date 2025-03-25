//! Symbol resolver implementation that bridges the resolver and type checker.

use crate::{
    context::Ty,
    db::{SymbolResolver, TypeCheckingDatabase},
    error::TypeError,
    resolve::TypeResolver,
};
use parallax_lang::ast::{self, common::Ident};
use parallax_resolve::symbol::{Symbol, SymbolTable};
use std::sync::Arc;

/// A symbol resolver that uses the symbol table from name resolution.
pub struct TableBasedSymbolResolver<'a, DB: TypeCheckingDatabase + ?Sized> {
    /// The database for querying resolved information
    db: &'a DB,
    /// The symbol table
    symbol_table: &'a SymbolTable,
    /// Type resolver for converting AST types to Ty
    type_resolver: TypeResolver<'a>,
}

impl<'a, DB: TypeCheckingDatabase + ?Sized> TableBasedSymbolResolver<'a, DB> {
    /// Create a new symbol resolver
    pub fn new(db: &'a DB, symbol_table: &'a SymbolTable) -> Self {
        Self {
            db,
            symbol_table,
            type_resolver: TypeResolver::new(symbol_table),
        }
    }
    
    /// Get the full name for a path
    fn path_to_string(path: &[Ident]) -> String {
        if path.is_empty() {
            return String::new();
        }
        
        path.iter()
            .map(|ident| ident.0.clone())
            .collect::<Vec<_>>()
            .join("::")
    }
}

impl<'a, DB: TypeCheckingDatabase + ?Sized> SymbolResolver for TableBasedSymbolResolver<'a, DB> {
    fn resolve_type(&self, name: &str) -> Option<Ty> {
        // Look up the type in the symbol table
        let symbols = self.symbol_table.lookup(name);
        
        // Filter to type symbols (structs, enums, etc.)
        let type_symbols = symbols.iter().filter(|s| {
            matches!(s, 
                Symbol::Struct { .. } | 
                Symbol::Enum { .. } |
                Symbol::TypeParam { .. } |
                Symbol::Trait { .. }
            )
        }).collect::<Vec<_>>();
        
        if !type_symbols.is_empty() {
            // Found the type in the symbol table
            // For now, just create a simple named type
            // In the future, this would handle generic parameters, etc.
            
            // Create a fake AST type for now
            let fake_type = ast::Type {
                kind: ast::TypeKind::Path(vec![Ident(name.to_string())]),
                span: ast::Span { start: 0, end: 0 },
            };
            
            // Resolve it using the type resolver
            return self.type_resolver.resolve_type(&fake_type).ok();
        }
        
        None
    }
    
    fn resolve_variable(&self, name: &str) -> Option<Ty> {
        // Look up the variable in the symbol table
        let symbols = self.symbol_table.lookup(name);
        
        for symbol in symbols {
            if let Symbol::Variable { ty, .. } = symbol {
                if let Some(ty) = ty {
                    // Resolve the AST type to our Ty
                    return self.type_resolver.resolve_type(ty).ok();
                }
            }
        }
        
        None
    }
    
    fn resolve_function(&self, name: &str) -> Option<(Vec<Ty>, Ty)> {
        // Look up the function in the symbol table
        let symbols = self.symbol_table.lookup(name);
        
        for symbol in symbols {
            if let Symbol::Function { sig, .. } = symbol {
                // Resolve parameter types
                let mut param_tys = Vec::new();
                for param in &sig.params {
                    if let Some(ty) = &param.ty {
                        if let Ok(resolved) = self.type_resolver.resolve_type(ty) {
                            param_tys.push(resolved);
                        } else {
                            return None; // Error resolving a parameter type
                        }
                    } else {
                        return None; // Parameter without a type
                    }
                }
                
                // Resolve return type
                let ret_ty = if let Some(ret_ty) = &sig.return_type {
                    if let Ok(resolved) = self.type_resolver.resolve_type(ret_ty) {
                        resolved
                    } else {
                        return None; // Error resolving return type
                    }
                } else {
                    // No return type, use unit
                    crate::context::Ty::Concrete(crate::context::ConcreteTy::Unit)
                };
                
                return Some((param_tys, ret_ty));
            }
        }
        
        None
    }
    
    fn source_text(&self, _span: ast::Span) -> Arc<String> {
        // Get the source text from the database
        // In a real implementation, this would use the file_id from the span
        // For now, just return the source code from the database
        self.db.source_text(0).unwrap_or_else(|| Arc::new(String::new()))
    }
} 