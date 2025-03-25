//! AST to HIR lowering
//!
//! This module handles the conversion of the Parallax AST to HIR,
//! using the results of name resolution and type checking.

use parallax_lang::ast::{self, items};
use parallax_resolve::{
    resolver::ResolvedCrate,
    db::ResolverDatabase,
};
use parallax_typeck::{
    context::{Ty, ConcreteTy},
    db::TypeCheckingDatabase,
    CheckedCrate, 
};
use crate::{
    hir::*,
    db::HirError,
    HirResult,
};
use std::sync::Arc;

/// Public interface for lowering an AST to HIR
pub fn lower_ast_to_hir(
    resolved_crate: &ResolvedCrate,
) -> Result<HirResult, HirError> {
    let lowerer = SimpleLowerer::new(resolved_crate);
    lowerer.lower()
}

/// Implementation of AST to HIR lowering as a Salsa query
pub(crate) fn lower_ast_to_hir_impl(db: &dyn crate::db::HirDatabase) -> Result<HirResult, HirError> {
    // 1. Get the resolved crate from name resolution
    let resolved_crate = ResolverDatabase::resolved_crate(db)?;
    
    // 2. Get the type checked crate
    let checked_crate = TypeCheckingDatabase::type_check_crate(db)?;
    
    // Create a lowerer with both name resolution and type checking results
    let mut lowerer = SimpleLowerer::new(&resolved_crate);
    
    // Perform the lowering, taking type information from the checked crate
    lowerer.lower()
}

/// A simplified lowerer that creates a basic HIR representation
struct SimpleLowerer<'a> {
    /// The resolved crate from name resolution
    resolved_crate: &'a ResolvedCrate,
    /// Warnings accumulated during lowering
    warnings: Vec<String>,
}

impl<'a> SimpleLowerer<'a> {
    /// Create a new lowerer
    fn new(resolved_crate: &'a ResolvedCrate) -> Self {
        Self {
            resolved_crate,
            warnings: Vec::new(),
        }
    }
    
    /// Lower the root module to our HIR
    fn lower_root(&mut self) -> Result<Option<Item>, HirError> {
        // Get the root AST item from the resolved crate
        let ast = &self.resolved_crate.resolved_ast;
        
        // Create a unit type for cases where we don't have type information
        let unit_ty = Ty::Concrete(ConcreteTy::Unit);
        
        // Convert the root item to HIR
        match &ast.kind {
            ast::ItemKind::Module(module) => {
                let mut items = Vec::new();
                
                // Process each item in the module
                for item in &module.items {
                    match &item.kind {
                        ast::ItemKind::Function(func) => {
                            items.push(self.lower_function(func, item.span));
                        },
                        ast::ItemKind::Struct(struct_def) => {
                            items.push(self.lower_struct(struct_def, item.span));
                        },
                        ast::ItemKind::Module(module) => {
                            items.push(self.lower_module(module, item.span));
                        },
                        ast::ItemKind::Enum(enum_def) => {
                            items.push(self.lower_enum(enum_def, item.span));
                        },
                        _ => {
                            // Skip other item kinds
                            self.warnings.push(format!("Skipping unsupported item kind: {:?}", item.kind));
                        }
                    }
                }
                
                let module_item = Module {
                    name: module.name.0.clone(),
                    items,
                };
                
                Ok(Some(Item {
                    kind: ItemKind::Module(module_item),
                    span: ast.span,
                    ty: unit_ty,
                }))
            },
            _ => {
                self.warnings.push(format!("Root AST item is not a module: {:?}", ast.kind));
                Ok(None)
            }
        }
    }
    
    // Lower a function definition
    fn lower_function(&self, func: &items::Function, span: ast::Span) -> Item {
        // Create a unit type for now
        let unit_ty = Ty::Concrete(ConcreteTy::Unit);
        
        // Create a basic function representation
        let function = Function {
            name: func.name.0.clone(),
            params: Vec::new(), // Simplified
            return_ty: unit_ty.clone(),
            body: Expr {
                kind: ExprKind::Block(Vec::new()),
                ty: unit_ty.clone(),
                span: func.span,
            },
            generic_params: Vec::new(),
            where_clause: Vec::new(),
        };
        
        Item {
            kind: ItemKind::Function(function),
            span,
            ty: unit_ty,
        }
    }
    
    // Lower a struct definition
    fn lower_struct(&self, struct_def: &items::StructDef, span: ast::Span) -> Item {
        // Create a unit type for now
        let unit_ty = Ty::Concrete(ConcreteTy::Unit);
        
        // Create a basic struct representation
        let struct_item = Struct {
            name: struct_def.name.0.clone(),
            fields: Vec::new(), // Simplified
            generic_params: Vec::new(),
            where_clause: Vec::new(),
        };
        
        Item {
            kind: ItemKind::Struct(struct_item),
            span,
            ty: unit_ty,
        }
    }
    
    // Lower a module definition
    fn lower_module(&self, module: &items::Module, span: ast::Span) -> Item {
        // Create a unit type for now
        let unit_ty = Ty::Concrete(ConcreteTy::Unit);
        
        // Create a basic module representation
        let module_item = Module {
            name: module.name.0.clone(),
            items: Vec::new(), // Simplified
        };
        
        Item {
            kind: ItemKind::Module(module_item),
            span,
            ty: unit_ty,
        }
    }
    
    // Lower an enum definition
    fn lower_enum(&self, enum_def: &items::EnumDef, span: ast::Span) -> Item {
        // Create a unit type for now
        let unit_ty = Ty::Concrete(ConcreteTy::Unit);
        
        // Create a basic enum representation
        let enum_item = Enum {
            name: enum_def.name.0.clone(),
            variants: Vec::new(), // Simplified
            generic_params: Vec::new(),
            where_clause: Vec::new(),
        };
        
        Item {
            kind: ItemKind::Enum(enum_item),
            span,
            ty: unit_ty,
        }
    }
    
    /// Convert the resolved AST to HIR
    fn lower(&self) -> Result<HirResult, HirError> {
        // Create a copy of the lowerer to use with a mutable reference
        let mut lowerer = SimpleLowerer::new(self.resolved_crate);
        
        let mut items = Vec::new();
        
        if let Some(root_item) = lowerer.lower_root()? {
            items.push(root_item);
        }
        
        Ok(HirResult {
            hir: Crate { items },
            warnings: lowerer.warnings,
        })
    }
    
    /// Helper method to convert AST type to HIR type
    fn ast_type_to_ty(&self, ty: &ast::Type) -> Ty {
        // Convert AST type to HIR type (simplified)
        match &ty.kind {
            ast::TypeKind::Path(segments) => {
                let name = segments.last().map(|seg| seg.0.clone()).unwrap_or_default();
                // Handle primitive types
                match name.as_str() {
                    "i32" => Ty::Concrete(ConcreteTy::Int),
                    "f32" => Ty::Concrete(ConcreteTy::Float),
                    "bool" => Ty::Concrete(ConcreteTy::Bool),
                    "String" => Ty::Concrete(ConcreteTy::String),
                    "char" => Ty::Concrete(ConcreteTy::Char),
                    "()" => Ty::Concrete(ConcreteTy::Unit),
                    _ => Ty::Concrete(ConcreteTy::Named {
                        name,
                        args: Vec::new(),
                    }),
                }
            },
            ast::TypeKind::Function(param, ret) => {
                Ty::Function {
                    params: vec![self.ast_type_to_ty(param)],
                    ret: Box::new(self.ast_type_to_ty(ret)),
                }
            },
            ast::TypeKind::Tuple(types) => {
                Ty::Tuple(types.iter().map(|ty| self.ast_type_to_ty(ty)).collect())
            },
            // Add other type conversions as needed
            _ => Ty::Concrete(ConcreteTy::Unit), // Fallback
        }
    }
} 