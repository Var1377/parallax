// src/tests/mocks.rs
//! Shared mock implementations for testing.

use parallax_lang::ast::{self, Span, Ident};
use crate::{context::{Ty, ConcreteTy, Constraint, TyVid}, db::{TypeContextOps, TypeCheckingDatabase, SymbolResolver, ResolvedCrate}, error::TypeError};
use parallax_resolve::error::ResolveError;
use rustc_hash::FxHashMap;
use std::{collections::HashMap, sync::Arc, path::PathBuf};

// --- Mock Database --- 

#[derive(Default)]
pub struct MockDb {
    pub symbol_table: HashMap<Vec<Ident>, Ty>, // Simple symbol table for testing
    pub sources: HashMap<usize, Arc<String>>, // Mock source files
}

impl MockDb {
    pub fn new() -> Self {
        Self::default()
    }
}

impl TypeCheckingDatabase for MockDb {
    fn resolved_ast(&self) -> Result<Arc<ast::Item>, Vec<ResolveError>> {
        // Return a dummy AST item or error for tests that need it
        let mock_error = ResolveError::undefined_name(
            "Mock AST not implemented".to_string(),
            Span { start: 0, end: 0 },
            PathBuf::new(),
            "mock source".to_string()
        );
        Err(vec![mock_error])
    }

    fn resolved_crate(&self) -> Result<ResolvedCrate, ResolveError> {
        // Create a minimal ResolvedCrate for testing
        // Create a dummy expression for the function body
        let dummy_expr = Box::new(ast::Expr {
            span: Span { start: 0, end: 0 },
            kind: ast::ExprKind::Literal(ast::Literal::Int(0)),
        });
        
        // Create a dummy function Item
        let dummy_ast = Arc::new(ast::Item {
            kind: ast::ItemKind::Function(ast::items::Function {
                name: Ident("test_fn".to_string()),
                generic_params: None,
                params: Vec::new(),
                return_type: None,
                where_clause: None,
                body: dummy_expr,
                span: Span { start: 0, end: 0 },
            }),
            visibility: true, // public visibility
            span: Span { start: 0, end: 0 },
        });

        let symbol_table = Arc::new(parallax_resolve::symbol::SymbolTable::default());
        
        Ok(ResolvedCrate {
            symbol_table,
            resolved_ast: dummy_ast,
            diagnostics: Vec::new(),
        })
    }

    fn source_text(&self, file_id: usize) -> Option<Arc<String>> {
        self.sources.get(&file_id).cloned()
    }

    fn type_check_crate(&self) -> Result<crate::CheckedCrate, Vec<TypeError>> {
        unimplemented!("type_check_crate not mocked")
    }
}

// --- Mock Symbol Resolver --- 

#[derive(Default)]
pub struct MockSymResolver {
    variables: HashMap<String, Ty>,
    functions: HashMap<String, (Vec<Ty>, Ty)>,
    types: HashMap<String, Ty>,
}

impl MockSymResolver {
    pub fn register_variable(&mut self, name: &str, ty: Ty) {
        self.variables.insert(name.to_string(), ty);
    }
    pub fn register_function(&mut self, name: &str, params: Vec<Ty>, ret: Ty) {
        self.functions.insert(name.to_string(), (params, ret));
    }
    pub fn register_type(&mut self, name: &str, ty: Ty) {
        self.types.insert(name.to_string(), ty);
    }
}

impl SymbolResolver for MockSymResolver {
    fn resolve_type(&self, name: &str) -> Option<Ty> {
         match name { // Add primitives
            "Int" => Some(Ty::Concrete(ConcreteTy::Int)),
            "Bool" => Some(Ty::Concrete(ConcreteTy::Bool)),
            "String" => Some(Ty::Concrete(ConcreteTy::String)),
            "Char" => Some(Ty::Concrete(ConcreteTy::Char)),
            "Float" => Some(Ty::Concrete(ConcreteTy::Float)),
            "Unit" => Some(Ty::Concrete(ConcreteTy::Unit)),
            _ => self.types.get(name).cloned(),
        }
    }

    fn resolve_variable(&self, name: &str) -> Option<Ty> {
        self.variables.get(name).cloned()
    }

    fn resolve_function(&self, name: &str) -> Option<(Vec<Ty>, Ty)> {
        self.functions.get(name).cloned()
    }

    fn source_text(&self, _span: Span) -> Arc<String> {
        // Return dummy source text
        Arc::new("mock source".to_string())
    }
}


// --- Mock Type Context --- 

pub struct MockContext {
    next_ty_vid: u32,
    pub constraints: Vec<Constraint>,
    pub errors: Vec<TypeError>,
    pub type_map: FxHashMap<Span, Ty>,
    pub symbol_resolver: MockSymResolver,
}

impl MockContext {
    pub fn new() -> Self {
        Self {
            next_ty_vid: 0,
            constraints: Vec::new(),
            errors: Vec::new(),
            type_map: FxHashMap::default(),
            symbol_resolver: MockSymResolver::default(),
        }
    }
    
    pub fn register_symbol(&mut self, name: &str, ty: Ty) {
        // For simplicity, assume symbols are variables for now
        self.symbol_resolver.register_variable(name, ty);
    }

    pub fn get_type(&self, span: Span) -> Option<&Ty> {
        self.type_map.get(&span)
    }
}

impl TypeContextOps for MockContext {
    fn new_ty_var(&mut self) -> Ty {
        let vid = TyVid(self.next_ty_vid);
        self.next_ty_vid += 1;
        Ty::Var(vid)
    }

    fn add_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }

    fn error(&mut self, error: TypeError) {
        self.errors.push(error);
    }

    fn record_type(&mut self, span: Span, ty: Ty) {
        self.type_map.insert(span, ty);
    }

    fn lookup_symbol(&self, path: &[Ident]) -> Option<Ty> {
        if path.len() == 1 {
            self.symbol_resolver.resolve_variable(&path[0].0)
        } else {
            None // Mock doesn't support multi-part paths yet
        }
    }

    fn resolve_type_from_ast(&self, ty: &ast::Type) -> Result<Ty, TypeError> {
        // Simple path resolution based on the mock symbol resolver
        match &ty.kind {
            ast::TypeKind::Path(path) if path.len() == 1 => {
                self.symbol_resolver.resolve_type(&path[0].0).ok_or_else(|| TypeError::UnknownType {
                    name: path[0].0.clone(),
                    span: ty.span,
                })
            },
            _ => Err(TypeError::UnresolvedType {
                ty: format!("{:?}", ty.kind),
                span: ty.span,
            }),
        }
    }

    fn symbol_resolver(&self) -> &dyn SymbolResolver {
        &self.symbol_resolver
    }
} 