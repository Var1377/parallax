//! Type resolver integration with the symbol system

use parallax_lang::ast::{self, Type, Ident};
use parallax_resolve::symbol::SymbolTable;
use crate::{
    context::{Ty, ConcreteTy},
    error::TypeError,
};

/// Responsible for resolving Type AST nodes to concrete types.
pub struct TypeResolver<'a> {
    symbol_table: &'a SymbolTable,
}

impl<'a> TypeResolver<'a> {
    /// Create a new type resolver
    pub fn new(symbol_table: &'a SymbolTable) -> Self {
        Self { symbol_table }
    }

    /// Resolve a Type AST node to a Ty
    pub fn resolve_type(&self, ty: &ast::Type) -> Result<Ty, TypeError> {
        match &ty.kind {
            ast::TypeKind::Path(path) => self.resolve_path_type(path),
            ast::TypeKind::Tuple(elements) => {
                let mut resolved_elements = Vec::with_capacity(elements.len());
                
                for elem in elements {
                    let resolved = self.resolve_type(elem)?;
                    resolved_elements.push(resolved);
                }
                
                Ok(Ty::Tuple(resolved_elements))
            },
            ast::TypeKind::Array(elem_ty, size) => {
                // Get the element type
                let element_ty = self.resolve_type(elem_ty)?;
                
                // We represent arrays as a named type with a generic parameter
                Ok(Ty::Concrete(ConcreteTy::Named {
                    name: "Array".to_string(),
                    args: vec![element_ty],
                }))
            },
            ast::TypeKind::Function(param_ty, ret_ty) => {
                // Resolve parameter type
                let resolved_param = self.resolve_type(param_ty)?;
                
                // Resolve return type
                let resolved_ret = self.resolve_type(ret_ty)?;
                
                Ok(Ty::Function {
                    params: vec![resolved_param],
                    ret: Box::new(resolved_ret),
                })
            },
            ast::TypeKind::KindApp(base_ty, args) => {
                // Resolve the base type
                let base_ty = self.resolve_type(base_ty)?;
                
                // Resolve each argument
                let mut resolved_args = Vec::with_capacity(args.len());
                for arg in args {
                    resolved_args.push(self.resolve_type(arg)?);
                }
                
                // Apply the arguments to the base type
                match base_ty {
                    Ty::Concrete(ConcreteTy::Named { name, .. }) => {
                        Ok(Ty::Concrete(ConcreteTy::Named {
                            name,
                            args: resolved_args,
                        }))
                    },
                    _ => Err(TypeError::UnresolvedType {
                        ty: format!("Cannot apply type arguments to {:?}", base_ty),
                        span: ty.span,
                    }),
                }
            },
        }
    }

    /// Resolve a path type to a Ty
    fn resolve_path_type(&self, path: &[Ident]) -> Result<Ty, TypeError> {
        if path.is_empty() {
            return Err(TypeError::UnresolvedType {
                ty: "empty path".to_string(),
                span: ast::Span { start: 0, end: 0 },
            });
        }

        // Handle primitive types directly
        if path.len() == 1 {
            match path[0].0.as_str() {
                "Int" => return Ok(Ty::Concrete(ConcreteTy::Int)),
                "Bool" => return Ok(Ty::Concrete(ConcreteTy::Bool)),
                "Char" => return Ok(Ty::Concrete(ConcreteTy::Char)),
                "String" => return Ok(Ty::Concrete(ConcreteTy::String)),
                "Unit" => return Ok(Ty::Concrete(ConcreteTy::Unit)),
                _ => {} // Fall through to symbol lookup
            }
        }

        // Look up the type in the symbol table
        let symbols = self.symbol_table.lookup(&path.last().unwrap().0);
        
        // Filter to type symbols (structs, enums, etc.)
        let type_symbols = symbols.iter().filter(|s| {
            matches!(s, 
                parallax_resolve::symbol::Symbol::Struct { .. } | 
                parallax_resolve::symbol::Symbol::Enum { .. } |
                parallax_resolve::symbol::Symbol::TypeParam { .. } |
                parallax_resolve::symbol::Symbol::Trait { .. }
            )
        }).collect::<Vec<_>>();
        
        if !type_symbols.is_empty() {
            // Found the type in the symbol table
            return Ok(Ty::Concrete(ConcreteTy::Named {
                name: path.last().unwrap().0.clone(),
                args: vec![],
            }));
        }
        
        // For longer paths, we need to do scope-based lookup
        // This would be implemented in a real resolver
        // For now, just return a named type as a placeholder
        Ok(Ty::Concrete(ConcreteTy::Named {
            name: path.last().unwrap().0.clone(),
            args: vec![],
        }))
    }

    /// Lookup a symbol from a path and get its type
    pub fn lookup_symbol(&self, path: &[Ident]) -> Result<Ty, TypeError> {
        // Lookup the symbol in the symbol table
        if path.is_empty() {
            return Err(TypeError::UnresolvedType {
                ty: "empty path".to_string(),
                span: ast::Span { start: 0, end: 0 },
            });
        }
        
        let symbols = self.symbol_table.lookup(&path.last().unwrap().0);
        
        if symbols.is_empty() {
            return Err(TypeError::UnknownType {
                name: path.last().unwrap().0.clone(),
                span: ast::Span { start: 0, end: 0 }, // We don't have the span here
            });
        }
        
        // For variables, get their type
        for symbol in &symbols {
            if let parallax_resolve::symbol::Symbol::Variable { ty, .. } = symbol {
                if let Some(ty) = ty {
                    // Resolve the AST type to our Ty
                    return self.resolve_type(ty);
                }
            }
        }
        
        // For functions, construct a function type
        for symbol in &symbols {
            if let parallax_resolve::symbol::Symbol::Function { sig, .. } = symbol {
                // This would resolve the function signature to a function type
                // For now, return a placeholder
                return Ok(Ty::Function {
                    params: vec![],
                    ret: Box::new(Ty::Concrete(ConcreteTy::Unit)),
                });
            }
        }
        
        // Just return a placeholder type for now
        Ok(Ty::Concrete(ConcreteTy::Named {
            name: path.last().unwrap().0.clone(),
            args: vec![],
        }))
    }
} 