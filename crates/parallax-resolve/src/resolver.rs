//! Core name resolution logic for the Parallax compiler.
//!
//! This module contains the main resolver that processes the AST and links
//! all identifiers to their declarations.

use std::{collections::HashMap, path::PathBuf, sync::Arc};
use crate::{
    db::ResolverDatabase,
    error::ResolveError,
    imports::ImportResolver,
    namespace::{Namespace, NamespaceManager},
    symbol::{Symbol, SymbolTable, ScopeId, ModuleId},
    Result,
};
use parallax_lang::ast::{
    common::{Ident, Span},
    expr::{Expr, ExprKind},
    items::{Item, ItemKind, Function, StructDef, EnumDef, Module, UseDecl, Parameter},
    pattern::{Pattern, PatternKind},
};

/// The result of name resolution for a crate.
#[derive(Debug, Clone)]
pub struct ResolvedCrate {
    /// The symbol table containing all resolved symbols.
    pub symbol_table: Arc<SymbolTable>,
    /// The resolved AST.
    pub resolved_ast: Arc<Item>,
    /// Any diagnostics generated during resolution.
    pub diagnostics: Vec<ResolveError>,
}

impl PartialEq for ResolvedCrate {
    fn eq(&self, other: &Self) -> bool {
        // We consider ResolvedCrates equal if their symbol tables and ASTs are equal
        // For Arc<Item>, we need to compare the Items themselves
        self.symbol_table == other.symbol_table && 
        Arc::ptr_eq(&self.resolved_ast, &other.resolved_ast)
    }
}

impl Eq for ResolvedCrate {}

/// The resolver that processes an AST and resolves all names.
pub struct Resolver {
    /// The symbol table being built.
    symbol_table: SymbolTable,
    /// The current module being processed.
    current_module: ModuleId,
    /// The source path.
    source_path: PathBuf,
    /// The source code.
    source_code: String,
    /// Accumulated errors.
    errors: Vec<ResolveError>,
}

impl Resolver {
    /// Create a new resolver.
    pub fn new(source_path: PathBuf, source_code: String) -> Self {
        let symbol_table = SymbolTable::new();
        let current_module = symbol_table.root_module();
        
        Self {
            symbol_table,
            current_module,
            source_path,
            source_code,
            errors: Vec::new(),
        }
    }
    
    /// Resolve a crate.
    pub fn resolve(&mut self, item: &Item) -> Result<ResolvedCrate> {
        // First pass: collect all declarations
        self.collect_declarations(item)?;
        
        // Second pass: resolve all identifiers
        self.resolve_item(item)?;
        
        // Check for errors
        if let Some(err) = self.errors.pop() {
            return Err(err);
        }
        
        // Create the resolved crate
        let resolved_crate = ResolvedCrate {
            symbol_table: Arc::new(self.symbol_table.clone()),
            resolved_ast: Arc::new(item.clone()),
            diagnostics: std::mem::take(&mut self.errors),
        };
        
        Ok(resolved_crate)
    }
    
    /// First pass: collect all declarations in the crate.
    fn collect_declarations(&mut self, item: &Item) -> Result<()> {
        match &item.kind {
            ItemKind::Module(module) => {
                // Create a new module scope
                let (scope_id, module_id) = self.symbol_table.push_module();
                
                // Add the module to the current scope
                let module_symbol = Symbol::Module {
                    name: module.name.clone(),
                    id: module_id,
                    span: module.span,
                    is_public: item.visibility,
                    defined_in: self.symbol_table.current_scope(),
                };
                self.symbol_table.add_symbol(module_symbol);
                
                // Process items in the module
                let prev_module = self.current_module;
                self.current_module = module_id;
                
                for item in &module.items {
                    self.collect_declarations(item)?;
                }
                
                // Restore previous module
                self.current_module = prev_module;
                self.symbol_table.pop_scope();
            }
            ItemKind::Function(function) => {
                // Add the function to the current scope
                let function_symbol = Symbol::Function {
                    name: function.name.clone(),
                    sig: Arc::new(function.clone()),
                    span: function.span,
                    is_public: item.visibility,
                    defined_in: self.symbol_table.current_scope(),
                };
                self.symbol_table.add_symbol(function_symbol);
                
                // Process function body later
            }
            ItemKind::Struct(struct_def) => {
                // Add the struct to the current scope
                let struct_symbol = Symbol::Struct {
                    name: struct_def.name.clone(),
                    def: Arc::new(struct_def.clone()),
                    span: struct_def.span,
                    is_public: item.visibility,
                    defined_in: self.symbol_table.current_scope(),
                };
                self.symbol_table.add_symbol(struct_symbol);
                
                // Process struct fields later
            }
            ItemKind::Enum(enum_def) => {
                // Add the enum to the current scope
                let enum_symbol = Symbol::Enum {
                    name: enum_def.name.clone(),
                    def: Arc::new(enum_def.clone()),
                    span: enum_def.span,
                    is_public: item.visibility,
                    defined_in: self.symbol_table.current_scope(),
                };
                self.symbol_table.add_symbol(enum_symbol);
                
                // Process enum variants later
            }
            ItemKind::Use(use_decl) => {
                // Process use declarations
                let mut import_resolver = ImportResolver::new(
                    &mut self.symbol_table,
                    self.source_path.clone(),
                    self.source_code.clone(),
                );
                
                import_resolver.resolve_import(use_decl)?;
                
                // Collect any errors
                self.errors.extend(import_resolver.take_errors());
            }
            // Handle other item types (traits, impls, etc.)
            _ => {
                // Not implemented yet
            }
        }
        
        Ok(())
    }
    
    /// Second pass: resolve all identifiers.
    fn resolve_item(&mut self, item: &Item) -> Result<()> {
        match &item.kind {
            ItemKind::Function(function) => {
                // Push a new scope for the function
                self.symbol_table.push_scope(None);
                
                // Process parameters
                for param in &function.params {
                    self.resolve_pattern(&param.pattern)?;
                }
                
                // Process body
                self.resolve_expr(&function.body)?;
                
                // Pop the function scope
                self.symbol_table.pop_scope();
            }
            ItemKind::Module(module) => {
                // Find the module's scope
                let module_symbols = self.symbol_table.lookup(&module.name.0);
                let module_id = module_symbols.iter().find_map(|sym| {
                    if let Symbol::Module { id, .. } = sym {
                        Some(*id)
                    } else {
                        None
                    }
                });
                
                if let Some(module_id) = module_id {
                    // Process items in the module
                    let prev_module = self.current_module;
                    self.current_module = module_id;
                    
                    // We need to find the scope for this module
                    // This is a simplification since we don't have a direct way to find the scope
                    // In a real implementation, the symbol table would have a method for this
                    
                    for child_item in &module.items {
                        self.resolve_item(child_item)?;
                    }
                    
                    // Restore previous module
                    self.current_module = prev_module;
                }
            }
            // Handle other item types
            _ => {
                // Not implemented yet
            }
        }
        
        Ok(())
    }
    
    /// Resolve an expression.
    fn resolve_expr(&mut self, expr: &Expr) -> Result<()> {
        match &expr.kind {
            ExprKind::Block(exprs) => {
                // Push a new scope for the block
                self.symbol_table.push_scope(None);
                
                // Process each expression
                for expr in exprs {
                    self.resolve_expr(expr)?;
                }
                
                // Pop the block scope
                self.symbol_table.pop_scope();
            }
            ExprKind::If { condition, then_branch, else_branch } => {
                // Resolve the condition
                self.resolve_expr(condition)?;
                
                // Resolve the then branch
                self.resolve_expr(then_branch)?;
                
                // Resolve the else branch if it exists
                if let Some(else_branch) = else_branch {
                    self.resolve_expr(else_branch)?;
                }
            }
            ExprKind::Path(segments) => {
                // Resolve a path expression (e.g., foo::bar)
                self.resolve_path(segments, expr.span)?;
            }
            ExprKind::Let { pattern, type_ann: _, value } => {
                // Resolve the value first
                self.resolve_expr(value)?;
                
                // Now resolve the pattern, which will add any bindings to the current scope
                self.resolve_pattern(pattern)?;
            }
            // Handle other expression types
            _ => {
                // Not implemented yet
            }
        }
        
        Ok(())
    }
    
    /// Resolve a pattern.
    fn resolve_pattern(&mut self, pattern: &Pattern) -> Result<()> {
        match &pattern.kind {
            PatternKind::Identifier(ident) => {
                // Add the variable to the current scope
                let var_symbol = Symbol::Variable {
                    name: ident.clone(),
                    ty: None, // We don't know the type yet
                    span: pattern.span,
                    defined_in: self.symbol_table.current_scope(),
                };
                self.symbol_table.add_symbol(var_symbol);
            }
            PatternKind::Struct { path, fields } => {
                // Resolve the struct type
                self.resolve_path(path, pattern.span)?;
                
                // Resolve each field pattern
                for field in fields {
                    if let Some(pat) = &field.pattern {
                        self.resolve_pattern(pat)?;
                    } else {
                        // If there's no pattern, this is a shorthand (e.g., { x })
                        // which is equivalent to { x: x }
                        let var_symbol = Symbol::Variable {
                            name: field.name.clone(),
                            ty: None,
                            span: field.span,
                            defined_in: self.symbol_table.current_scope(),
                        };
                        self.symbol_table.add_symbol(var_symbol);
                    }
                }
            }
            PatternKind::Tuple(patterns) => {
                // Resolve each pattern in the tuple
                for pat in patterns {
                    self.resolve_pattern(pat)?;
                }
            }
            // Handle other pattern types
            _ => {
                // Not implemented yet
            }
        }
        
        Ok(())
    }
    
    /// Resolve a path expression.
    fn resolve_path(&mut self, segments: &[Ident], span: Span) -> Result<()> {
        if segments.is_empty() {
            return Ok(());
        }
        
        // Try to resolve the first segment
        let first_segment = &segments[0].0;
        let symbols = self.symbol_table.lookup(first_segment);
        
        if symbols.is_empty() {
            // Symbol not found
            self.errors.push(ResolveError::undefined_name(
                first_segment.clone(),
                span,
                self.source_path.clone(),
                self.source_code.clone(),
            ));
            return Ok(());
        }
        
        if segments.len() == 1 {
            // This is a simple reference, no further path segments
            // If there are multiple symbols with this name, we need to disambiguate
            if symbols.len() > 1 {
                // For now, just choose the first one
                // In a real implementation, we would use type information to disambiguate
            }
            
            // The symbol was found
            return Ok(());
        }
        
        // This is a path with multiple segments (e.g., foo::bar::baz)
        // We need to make sure the first segment is a module or another namespace
        let module = symbols.iter().find_map(|sym| {
            if let Symbol::Module { id, .. } = sym {
                Some(*id)
            } else {
                None
            }
        });
        
        if let Some(module_id) = module {
            // Continue resolving in the module
            // This is a simplification since we don't have a direct way to get a module's scope
            // In a real implementation, the symbol table would have a method for this
        } else {
            // The first segment is not a module
            self.errors.push(ResolveError::generic_error(
                format!("'{}' is not a module", first_segment),
                span,
                self.source_path.clone(),
                self.source_code.clone(),
            ));
        }
        
        Ok(())
    }
}

/// Entry point for the resolver.
pub fn resolve_crate(db: &dyn ResolverDatabase) -> Result<ResolvedCrate> {
    let ast = db.ast();
    let source_path = PathBuf::from(db.source_path());
    let source_code = db.source_code();
    
    let mut resolver = Resolver::new(source_path, source_code);
    resolver.resolve(&ast)
} 