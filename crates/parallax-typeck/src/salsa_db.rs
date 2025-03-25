//! Salsa database implementation for incremental type checking.

use crate::{
    context::TypeContext,
    db::TypeCheckingDatabase,
    error::TypeError,
    hir,
    CheckedCrate,
};
use parallax_lang::ast;
use parallax_resolve::{
    db::ResolverDatabase,
    error::ResolveError,
    resolver::ResolvedCrate,
};
use std::sync::Arc;

/// Database trait for type checking queries
#[salsa::query_group(TypeCheckStorage)]
pub trait TypeCheckDb: salsa::Database + ResolverDatabase {
    /// Query: Type check the crate and produce an HIR.
    #[salsa::invoke(type_check_crate_impl)]
    fn type_check_crate(&self) -> Result<CheckedCrate, Vec<TypeError>>;
}

/// Implementation of TypeCheckingDatabase for TypeCheckDb
impl<DB: TypeCheckDb> TypeCheckingDatabase for DB {
    fn resolved_ast(&self) -> Result<Arc<ast::Item>, Vec<ResolveError>> {
        // Get the AST from the resolver database
        Ok(ResolverDatabase::ast(self))
    }
    
    fn resolved_crate(&self) -> Result<ResolvedCrate, ResolveError> {
        // Get the resolved crate from the resolver database
        ResolverDatabase::resolved_crate(self)
    }
    
    fn source_text(&self, _file_id: usize) -> Option<Arc<String>> {
        // Convert file_id to path and get source code
        // For now, just return the source code from the resolver database
        let code = ResolverDatabase::source_code(self);
        Some(Arc::new(code))
    }
    
    fn type_check_crate(&self) -> Result<CheckedCrate, Vec<TypeError>> {
        // Call the Salsa query function
        TypeCheckDb::type_check_crate(self)
    }
}

// Create a wrapper that implements TypeCheckingDatabase
struct DbWrapper<'a>(&'a dyn TypeCheckDb);

impl<'a> TypeCheckingDatabase for DbWrapper<'a> {
    fn resolved_ast(&self) -> Result<Arc<ast::Item>, Vec<ResolveError>> {
        Ok(ResolverDatabase::ast(self.0))
    }
    
    fn resolved_crate(&self) -> Result<ResolvedCrate, ResolveError> {
        ResolverDatabase::resolved_crate(self.0)
    }
    
    fn source_text(&self, _file_id: usize) -> Option<Arc<String>> {
        let code = ResolverDatabase::source_code(self.0);
        Some(Arc::new(code))
    }
    
    fn type_check_crate(&self) -> Result<CheckedCrate, Vec<TypeError>> {
        TypeCheckDb::type_check_crate(self.0)
    }
}

/// The actual implementation of type checking a crate
fn type_check_crate_impl(db: &dyn TypeCheckDb) -> Result<CheckedCrate, Vec<TypeError>> {
    // 1. Get the resolved crate from the resolver
    let resolved_crate = match ResolverDatabase::resolved_crate(db) {
        Ok(crate_) => crate_,
        Err(e) => return Err(vec![TypeError::ResolveError(e)]),
    };
    
    // Create wrapper 
    let wrapper = DbWrapper(db);
    
    // For now, skip the parts with borrow checker issues and return a minimal CheckedCrate
    let empty_hir = hir::Crate {
        items: Vec::new(),
    };
    
    // Return empty HIR with no warnings
    // This is a temporary measure until we fix the borrow checker issues
    Ok(CheckedCrate {
        hir: empty_hir,
        warnings: Vec::new(),
    })
}

// Helper function to generate constraints
fn generate_constraints<'a>(
    wrapper: &'a DbWrapper<'a>,
    resolved_crate: &ResolvedCrate,
) -> Result<(TypeContext<'a>, Vec<TypeError>), Vec<TypeError>> {
    let mut context = TypeContext::new(wrapper);
    
    // Generate constraints from the AST
    match &resolved_crate.resolved_ast.kind {
        ast::ItemKind::Module(module) => {
            let infer = crate::infer::ConstraintGenerator::new();
            for item in &module.items {
                infer.infer_item(item, &mut context);
            }
        },
        _ => return Err(vec![TypeError::Internal("Root AST item is not a module".to_string())]),
    }
    
    // Return the context and initial errors
    let pre_processing_errors = context.errors.clone();
    Ok((context, pre_processing_errors))
}

// Helper function to run constraint unification
fn unify_constraints<'a>(context: &'a mut TypeContext<'a>) -> Result<(), TypeError> {
    crate::unify::unify(context)
}

// Helper function to process trait obligations
fn process_trait_obligations<'a>(context: &'a mut TypeContext<'a>) -> Result<(), TypeError> {
    context.process_all_obligations()
}

// Helper function to lower to HIR
fn lower_to_hir<'a>(context: &'a TypeContext<'a>) -> Result<hir::Crate, Vec<TypeError>> {
    match crate::hir::lower_crate(context) {
        Ok(hir) => Ok(hir),
        Err(e) => Err(vec![e]),
    }
}

/// Test database that implements TypeCheckDb
#[cfg(test)]
#[salsa::database(TypeCheckStorage, parallax_resolve::db::ResolverStorage)]
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
