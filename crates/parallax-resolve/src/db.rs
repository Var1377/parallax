//! Salsa database integration for incremental resolution.

use crate::{
    resolver::{ResolvedCrate, resolve_crate},
    symbol::SymbolTable,
    Result,
};
use parallax_lang::ast::Item;
use std::sync::Arc;

/// The Salsa query group for resolution.
#[salsa::query_group(ResolverStorage)]
pub trait ResolverDatabase: salsa::Database {
    /// Input: The AST to resolve.
    #[salsa::input]
    fn ast(&self) -> Arc<Item>;
    
    /// Input: The source path.
    #[salsa::input]
    fn source_path(&self) -> String;
    
    /// Input: The source code.
    #[salsa::input]
    fn source_code(&self) -> String;
    
    /// Query: Resolve the crate.
    #[salsa::invoke(resolve_crate)]
    fn resolved_crate(&self) -> Result<ResolvedCrate>;
    
    /// Query: Get the symbol table.
    #[salsa::invoke(crate::symbol::create_symbol_table)]
    fn symbol_table(&self) -> Arc<SymbolTable>;
}

/// Create a test database for testing resolution.
#[cfg(test)]
#[salsa::database(ResolverStorage)]
pub struct TestDatabase {
    storage: salsa::Storage<TestDatabase>,
}

#[cfg(test)]
impl salsa::Database for TestDatabase {
    fn salsa_event(&self, _event: salsa::Event) { }
}

#[cfg(test)]
impl TestDatabase {
    /// Create a new test database.
    pub fn new(ast: Arc<Item>, source_path: String, source_code: String) -> Self {
        let mut db = TestDatabase {
            storage: salsa::Storage::default(),
        };
        
        db.set_ast(ast);
        db.set_source_path(source_path);
        db.set_source_code(source_code);
        
        db
    }
    
    /// Create a new test database with default source path and code.
    pub fn with_ast(ast: Arc<Item>) -> Self {
        Self::new(
            ast,
            "<test>".to_string(),
            "".to_string(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parallax_lang::ast::{Item, ItemKind};
    use parallax_lang::ast::common::Span;

    #[test]
    fn test_database_works() {
        // Create a simple test item (an empty module)
        let test_item = Item {
            kind: ItemKind::Module(
                parallax_lang::ast::items::Module {
                    name: parallax_lang::ast::common::Ident("test_module".to_string()),
                    items: vec![],
                    span: Span { start: 0, end: 0 },
                }
            ),
            visibility: true,
            span: Span { start: 0, end: 0 },
        };
        
        // Create the database
        let db = TestDatabase::new(
            Arc::new(test_item),
            "test.plx".to_string(),
            "// Test source code".to_string()
        );
        
        // Verify inputs were set correctly
        assert_eq!(db.source_path(), "test.plx");
        assert_eq!(db.source_code(), "// Test source code");
        
        // Get the symbol table
        let symbol_table = db.symbol_table();
        
        // A new symbol table should be empty but have a root scope
        assert!(symbol_table.lookup("non_existent").is_empty());
    }
} 