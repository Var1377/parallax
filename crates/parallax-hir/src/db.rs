//! Database interface for HIR

use std::sync::Arc;
use crate::HirResult;
use parallax_lang::ast;
use parallax_resolve::{
    error::ResolveError,
    resolver::ResolvedCrate,
    db::ResolverDatabase,
};
use parallax_typeck::{
    error::TypeError,
    db::TypeCheckingDatabase,
};

/// Database trait for HIR-related queries
#[salsa::query_group(HirStorage)]
pub trait HirDatabase: salsa::Database + ResolverDatabase + TypeCheckingDatabase {
    /// Get the HIR representation of the crate
    #[salsa::invoke(crate::lower::lower_ast_to_hir_impl)]
    fn hir(&self) -> Result<HirResult, HirError>;
}

/// Errors that can occur during HIR lowering
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirError {
    /// An error occurred during name resolution
    ResolveError(ResolveError),
    /// An error occurred during type checking
    TypeCheckError(Vec<TypeError>),
    /// An internal error occurred (e.g., unexpected AST structure)
    InternalError(String),
}

impl From<ResolveError> for HirError {
    fn from(err: ResolveError) -> Self {
        HirError::ResolveError(err)
    }
}

impl From<Vec<TypeError>> for HirError {
    fn from(errors: Vec<TypeError>) -> Self {
        HirError::TypeCheckError(errors)
    }
}

impl From<TypeError> for HirError {
    fn from(err: TypeError) -> Self {
        HirError::TypeCheckError(vec![err])
    }
}

/// A database implementation for testing
#[cfg(test)]
#[salsa::database(HirStorage, parallax_resolve::db::ResolverStorage, parallax_typeck::TypeCheckStorage)]
pub struct TestDatabase {
    storage: salsa::Storage<TestDatabase>,
}

#[cfg(test)]
impl salsa::Database for TestDatabase {
    fn salsa_event(&self, _event: salsa::Event) {}
}

#[cfg(test)]
impl TestDatabase {
    /// Create a new test database with the given AST
    pub fn new(ast: Arc<ast::Item>) -> Self {
        let mut db = TestDatabase {
            storage: salsa::Storage::default(),
        };
        
        // Set up the inputs for the resolver
        db.set_ast(ast);
        db.set_source_path("<test>".to_string());
        db.set_source_code("".to_string());
        
        db
    }
} 