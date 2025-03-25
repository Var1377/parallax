use parallax_lang::ast::{Span, common::Ident, Type};
use parallax_resolve::error::ResolveError;
use std::sync::Arc;
use parallax_lang::ast;
use crate::context::{Ty, Constraint};
use crate::error::TypeError;
use crate::CheckedCrate;

// Re-export for convenience
pub use parallax_resolve::resolver::ResolvedCrate;

/// A trait for databases that provide type checking information
pub trait TypeCheckingDatabase {
    /// Get the resolved AST for the crate
    fn resolved_ast(&self) -> Result<Arc<ast::Item>, Vec<ResolveError>>;
    
    /// Get the resolved crate
    fn resolved_crate(&self) -> Result<ResolvedCrate, ResolveError>;
    
    /// Get the source text for a file
    fn source_text(&self, file_id: usize) -> Option<Arc<String>>;
    
    /// Type check the crate
    fn type_check_crate(&self) -> Result<crate::CheckedCrate, Vec<TypeError>>;
}

/// A trait for operations on a type context
pub trait TypeContextOps {
    /// Create a new type variable
    fn new_ty_var(&mut self) -> Ty;
    
    /// Add a constraint to the solver
    fn add_constraint(&mut self, constraint: Constraint);
    
    /// Record an error
    fn error(&mut self, error: TypeError);
    
    /// Record a type for a span
    fn record_type(&mut self, span: Span, ty: Ty);
    
    /// Method for symbol lookup - returns None if not found
    fn lookup_symbol(&self, path: &[Ident]) -> Option<Ty> {
        None // Default implementation
    }
    
    /// Method for type resolution - default falls back to simple path-based resolution
    fn resolve_type_from_ast(&self, ty: &Type) -> Result<Ty, TypeError>;
    
    /// Get the symbol resolver
    fn symbol_resolver(&self) -> &dyn SymbolResolver;
}

/// A trait for resolving symbols in the context of type checking
pub trait SymbolResolver {
    /// Resolve a type name to a type
    fn resolve_type(&self, name: &str) -> Option<Ty>;
    
    /// Resolve a variable name to its type
    fn resolve_variable(&self, name: &str) -> Option<Ty>;
    
    /// Resolve a function name to its parameter types and return type
    fn resolve_function(&self, name: &str) -> Option<(Vec<Ty>, Ty)>;
    
    /// Get the source text for a span
    fn source_text(&self, span: Span) -> Arc<String>;
}

/// Test implementation of TypeCheckingDatabase
#[cfg(test)]
pub struct TestDb {
    // Mock data and implementations for testing
}

#[cfg(test)]
impl TypeCheckingDatabase for TestDb {
    fn resolved_ast(&self) -> Result<Arc<ast::Item>, Vec<ResolveError>> {
        // Mock implementation for tests
        unimplemented!()
    }
    
    fn resolved_crate(&self) -> Result<ResolvedCrate, ResolveError> {
        // Mock implementation for tests
        unimplemented!()
    }
    
    fn source_text(&self, _file_id: usize) -> Option<Arc<String>> {
        // Mock implementation for tests
        None
    }
    
    fn type_check_crate(&self) -> Result<CheckedCrate, Vec<TypeError>> {
        // Mock implementation for tests
        unimplemented!()
    }
} 