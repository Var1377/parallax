use std::sync::Arc;
use parallax_lang::ast;
use parallax_resolve::ResolverDatabase;
use crate::{CheckedCrate, error::TypeError};

/// Database trait for type checking operations
pub trait TypeCheckingDatabase: ResolverDatabase {
    /// Get the source text for a file
    fn source_text(&self, file_id: u32) -> Arc<String>;
    
    /// Type check the entire crate
    fn type_check_crate(&self) -> Result<CheckedCrate, Vec<TypeError>>;
    
    /// Get the resolved AST
    fn resolved_ast(&self) -> Result<Arc<ast::Item>, Vec<TypeError>>;
}

impl<DB: ResolverDatabase> TypeCheckingDatabase for DB {
    fn source_text(&self, _file_id: u32) -> Arc<String> {
        // This is a dummy implementation
        Arc::new(String::new())
    }

    fn type_check_crate(&self) -> Result<CheckedCrate, Vec<TypeError>> {
        // Call the actual implementation with explicit casting
        let db_ref: &dyn TypeCheckingDatabase = self;
        crate::type_check_crate(db_ref)
    }

    fn resolved_ast(&self) -> Result<Arc<ast::Item>, Vec<TypeError>> {
        // Get the AST from the resolver, using the correct method name
        match self.resolved_crate() {
            Ok(resolved) => Ok(resolved.resolved_ast.clone()),
            Err(e) => Err(vec![TypeError::ResolveError(e)]),
        }
    }
} 