//! Symbol table implementation for name resolution.

use fxhash::FxHashMap;
use parallax_lang::ast::{
    common::{Ident, Span},
    items::{Function, StructDef, EnumDef, TraitDef},
    types::Type,
};
use std::sync::Arc;

/// A unique identifier for a scope in the symbol table.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub usize);

/// A unique identifier for a module in the codebase.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(pub usize);

/// A symbol in the symbol table.
#[derive(Debug, Clone)]
pub enum Symbol {
    /// A function definition
    Function {
        /// The name of the function
        name: Ident,
        /// The signature of the function
        sig: Arc<Function>,
        /// The span of the function definition
        span: Span,
        /// Whether the function is public
        is_public: bool,
        /// The defining scope
        defined_in: ScopeId,
    },
    /// A struct definition
    Struct {
        /// The name of the struct
        name: Ident,
        /// The struct definition
        def: Arc<StructDef>,
        /// The span of the struct definition
        span: Span,
        /// Whether the struct is public
        is_public: bool,
        /// The defining scope
        defined_in: ScopeId,
    },
    /// An enum definition
    Enum {
        /// The name of the enum
        name: Ident,
        /// The enum definition
        def: Arc<EnumDef>,
        /// The span of the enum definition
        span: Span,
        /// Whether the enum is public
        is_public: bool,
        /// The defining scope
        defined_in: ScopeId,
    },
    /// A variable definition
    Variable {
        /// The name of the variable
        name: Ident,
        /// The type of the variable, if known
        ty: Option<Type>,
        /// The span of the variable definition
        span: Span,
        /// The defining scope
        defined_in: ScopeId,
    },
    /// A module definition
    Module {
        /// The name of the module
        name: Ident,
        /// The module ID
        id: ModuleId,
        /// The span of the module definition
        span: Span,
        /// Whether the module is public
        is_public: bool,
        /// The defining scope
        defined_in: ScopeId,
    },
    /// A trait definition
    Trait {
        /// The name of the trait
        name: Ident,
        /// The trait definition
        def: Arc<TraitDef>,
        /// The span of the trait definition
        span: Span,
        /// Whether the trait is public
        is_public: bool,
        /// The defining scope
        defined_in: ScopeId,
    },
    /// A type parameter
    TypeParam {
        /// The name of the type parameter
        name: Ident,
        /// The span of the type parameter definition
        span: Span,
        /// The defining scope
        defined_in: ScopeId,
    },
    /// An imported symbol
    Import {
        /// The name of the imported symbol
        name: Ident,
        /// The target symbol
        target: Box<Symbol>,
        /// The span of the import
        span: Span,
        /// The defining scope
        defined_in: ScopeId,
    },
}

// Manual implementation of PartialEq for Symbol
impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Symbol::Function { name: n1, span: s1, is_public: p1, defined_in: d1, .. },
             Symbol::Function { name: n2, span: s2, is_public: p2, defined_in: d2, .. }) => {
                n1 == n2 && s1 == s2 && p1 == p2 && d1 == d2
            }
            (Symbol::Struct { name: n1, span: s1, is_public: p1, defined_in: d1, .. },
             Symbol::Struct { name: n2, span: s2, is_public: p2, defined_in: d2, .. }) => {
                n1 == n2 && s1 == s2 && p1 == p2 && d1 == d2
            }
            (Symbol::Enum { name: n1, span: s1, is_public: p1, defined_in: d1, .. },
             Symbol::Enum { name: n2, span: s2, is_public: p2, defined_in: d2, .. }) => {
                n1 == n2 && s1 == s2 && p1 == p2 && d1 == d2
            }
            (Symbol::Variable { name: n1, ty: t1, span: s1, defined_in: d1 },
             Symbol::Variable { name: n2, ty: t2, span: s2, defined_in: d2 }) => {
                n1 == n2 && t1 == t2 && s1 == s2 && d1 == d2
            }
            (Symbol::Module { name: n1, id: i1, span: s1, is_public: p1, defined_in: d1 },
             Symbol::Module { name: n2, id: i2, span: s2, is_public: p2, defined_in: d2 }) => {
                n1 == n2 && i1 == i2 && s1 == s2 && p1 == p2 && d1 == d2
            }
            (Symbol::Trait { name: n1, span: s1, is_public: p1, defined_in: d1, .. },
             Symbol::Trait { name: n2, span: s2, is_public: p2, defined_in: d2, .. }) => {
                n1 == n2 && s1 == s2 && p1 == p2 && d1 == d2
            }
            (Symbol::TypeParam { name: n1, span: s1, defined_in: d1 },
             Symbol::TypeParam { name: n2, span: s2, defined_in: d2 }) => {
                n1 == n2 && s1 == s2 && d1 == d2
            }
            (Symbol::Import { name: n1, target: t1, span: s1, defined_in: d1 },
             Symbol::Import { name: n2, target: t2, span: s2, defined_in: d2 }) => {
                n1 == n2 && t1 == t2 && s1 == s2 && d1 == d2
            }
            _ => false,
        }
    }
}

// Implement Eq for Symbol
impl Eq for Symbol {}

impl Symbol {
    /// Get the name of the symbol.
    pub fn name(&self) -> &Ident {
        match self {
            Symbol::Function { name, .. } => name,
            Symbol::Struct { name, .. } => name,
            Symbol::Enum { name, .. } => name,
            Symbol::Variable { name, .. } => name,
            Symbol::Module { name, .. } => name,
            Symbol::Trait { name, .. } => name,
            Symbol::TypeParam { name, .. } => name,
            Symbol::Import { name, .. } => name,
        }
    }

    /// Get the span of the symbol definition.
    pub fn span(&self) -> Span {
        match self {
            Symbol::Function { span, .. } => *span,
            Symbol::Struct { span, .. } => *span,
            Symbol::Enum { span, .. } => *span,
            Symbol::Variable { span, .. } => *span,
            Symbol::Module { span, .. } => *span,
            Symbol::Trait { span, .. } => *span,
            Symbol::TypeParam { span, .. } => *span,
            Symbol::Import { span, .. } => *span,
        }
    }

    /// Check if the symbol is public.
    pub fn is_public(&self) -> bool {
        match self {
            Symbol::Function { is_public, .. } => *is_public,
            Symbol::Struct { is_public, .. } => *is_public,
            Symbol::Enum { is_public, .. } => *is_public,
            Symbol::Variable { .. } => false, // Variables are always private
            Symbol::Module { is_public, .. } => *is_public,
            Symbol::Trait { is_public, .. } => *is_public,
            Symbol::TypeParam { .. } => false, // Type parameters aren't public
            Symbol::Import { target, .. } => target.is_public(),
        }
    }

    /// Get the scope that defined this symbol.
    pub fn defined_in(&self) -> ScopeId {
        match self {
            Symbol::Function { defined_in, .. } => *defined_in,
            Symbol::Struct { defined_in, .. } => *defined_in,
            Symbol::Enum { defined_in, .. } => *defined_in,
            Symbol::Variable { defined_in, .. } => *defined_in,
            Symbol::Module { defined_in, .. } => *defined_in,
            Symbol::Trait { defined_in, .. } => *defined_in,
            Symbol::TypeParam { defined_in, .. } => *defined_in,
            Symbol::Import { defined_in, .. } => *defined_in,
        }
    }
}

/// A scope in the symbol table.
#[derive(Debug, Clone)]
pub struct Scope {
    /// The parent scope, if any
    parent: Option<ScopeId>,
    /// The symbols defined in this scope
    symbols: FxHashMap<String, Vec<Symbol>>,
    /// The child scopes
    children: Vec<ScopeId>,
    /// The module this scope is part of
    module: ModuleId,
}

// Manual implementation of PartialEq for Scope
impl PartialEq for Scope {
    fn eq(&self, other: &Self) -> bool {
        self.parent == other.parent && 
        self.children == other.children && 
        self.module == other.module &&
        // Compare symbols map by key
        self.symbols.len() == other.symbols.len() &&
        self.symbols.keys().all(|k| other.symbols.contains_key(k))
    }
}

// Implement Eq for Scope
impl Eq for Scope {}

/// The symbol table for name resolution.
#[derive(Debug, Clone)]
pub struct SymbolTable {
    /// The scopes in the symbol table
    scopes: FxHashMap<ScopeId, Scope>,
    /// The current scope
    current_scope: ScopeId,
    /// The root scope
    root_scope: ScopeId,
    /// The next scope ID to assign
    next_scope_id: usize,
    /// The next module ID to assign
    next_module_id: usize,
    /// The root module
    root_module: ModuleId,
}

// Manual implementation of PartialEq for SymbolTable
impl PartialEq for SymbolTable {
    fn eq(&self, other: &Self) -> bool {
        self.current_scope == other.current_scope &&
        self.root_scope == other.root_scope &&
        self.next_scope_id == other.next_scope_id &&
        self.next_module_id == other.next_module_id &&
        self.root_module == other.root_module &&
        // Compare scopes map by key
        self.scopes.len() == other.scopes.len() &&
        self.scopes.keys().all(|k| other.scopes.contains_key(k))
    }
}

// Implement Eq for SymbolTable
impl Eq for SymbolTable {}

impl SymbolTable {
    /// Create a new symbol table.
    pub fn new() -> Self {
        let root_scope_id = ScopeId(0);
        let root_module_id = ModuleId(0);
        
        let mut scopes = FxHashMap::default();
        scopes.insert(
            root_scope_id,
            Scope {
                parent: None,
                symbols: FxHashMap::default(),
                children: Vec::new(),
                module: root_module_id,
            },
        );
        
        SymbolTable {
            scopes,
            current_scope: root_scope_id,
            root_scope: root_scope_id,
            next_scope_id: 1,
            next_module_id: 1,
            root_module: root_module_id,
        }
    }

    /// Get the root scope ID.
    pub fn root_scope(&self) -> ScopeId {
        self.root_scope
    }

    /// Get the root module ID.
    pub fn root_module(&self) -> ModuleId {
        self.root_module
    }

    /// Get the current scope ID.
    pub fn current_scope(&self) -> ScopeId {
        self.current_scope
    }

    /// Push a new scope and make it the current scope.
    pub fn push_scope(&mut self, module: Option<ModuleId>) -> ScopeId {
        let new_scope_id = ScopeId(self.next_scope_id);
        self.next_scope_id += 1;
        
        let module_id = module.unwrap_or_else(|| self.get_module_for_scope(self.current_scope));
        
        let scope = Scope {
            parent: Some(self.current_scope),
            symbols: FxHashMap::default(),
            children: Vec::new(),
            module: module_id,
        };
        
        // Add the new scope as a child of the current scope
        if let Some(parent_scope) = self.scopes.get_mut(&self.current_scope) {
            parent_scope.children.push(new_scope_id);
        }
        
        self.scopes.insert(new_scope_id, scope);
        let prev_scope = self.current_scope;
        self.current_scope = new_scope_id;
        
        new_scope_id
    }

    /// Pop the current scope and return to the parent scope.
    pub fn pop_scope(&mut self) -> Option<ScopeId> {
        if let Some(scope) = self.scopes.get(&self.current_scope) {
            if let Some(parent) = scope.parent {
                let old_scope = self.current_scope;
                self.current_scope = parent;
                return Some(old_scope);
            }
        }
        None
    }

    /// Push a new module scope.
    pub fn push_module(&mut self) -> (ScopeId, ModuleId) {
        let module_id = ModuleId(self.next_module_id);
        self.next_module_id += 1;
        
        let scope_id = self.push_scope(Some(module_id));
        
        (scope_id, module_id)
    }

    /// Get the module for a given scope.
    pub fn get_module_for_scope(&self, scope_id: ScopeId) -> ModuleId {
        if let Some(scope) = self.scopes.get(&scope_id) {
            return scope.module;
        }
        self.root_module
    }

    /// Get the current module.
    pub fn current_module(&self) -> ModuleId {
        self.get_module_for_scope(self.current_scope)
    }

    /// Add a symbol to the current scope.
    pub fn add_symbol(&mut self, symbol: Symbol) {
        let name = symbol.name().0.clone();
        
        if let Some(scope) = self.scopes.get_mut(&self.current_scope) {
            scope.symbols.entry(name).or_insert_with(Vec::new).push(symbol);
        }
    }

    /// Lookup a symbol by name in the current scope and parent scopes.
    pub fn lookup(&self, name: &str) -> Vec<&Symbol> {
        let mut result = Vec::new();
        let mut current = Some(self.current_scope);
        
        // First search in all parent scopes
        while let Some(scope_id) = current {
            if let Some(scope) = self.scopes.get(&scope_id) {
                if let Some(symbols) = scope.symbols.get(name) {
                    result.extend(symbols.iter());
                }
                current = scope.parent;
            } else {
                break;
            }
        }
        
        result
    }

    /// Lookup a symbol by name in a specific scope.
    pub fn lookup_in_scope(&self, scope_id: ScopeId, name: &str) -> Vec<&Symbol> {
        let mut result = Vec::new();
        
        if let Some(scope) = self.scopes.get(&scope_id) {
            if let Some(symbols) = scope.symbols.get(name) {
                result.extend(symbols.iter());
            }
        }
        
        result
    }

    /// Check if a symbol exists in the current scope only (not in parent scopes).
    pub fn exists_in_current_scope(&self, name: &str) -> bool {
        if let Some(scope) = self.scopes.get(&self.current_scope) {
            return scope.symbols.contains_key(name);
        }
        false
    }
    
    /// Set the current scope.
    pub fn set_current_scope(&mut self, scope_id: ScopeId) {
        self.current_scope = scope_id;
    }
    
    /// Get a reference to a scope by ID.
    pub fn get_scope(&self, scope_id: ScopeId) -> Option<&Scope> {
        self.scopes.get(&scope_id)
    }
    
    /// Get all scopes associated with a module.
    pub fn get_scopes_for_module(&self, module_id: ModuleId) -> Vec<ScopeId> {
        let mut result = Vec::new();
        
        // Scan all scopes and add those with matching module ID
        for (scope_id, scope) in &self.scopes {
            if scope.module == module_id {
                result.push(*scope_id);
            }
        }
        
        result
    }
    
    /// Get all symbols defined in a scope.
    pub fn get_all_symbols_in_scope(&self, scope_id: ScopeId) -> Vec<Symbol> {
        let mut result = Vec::new();
        
        if let Some(scope) = self.scopes.get(&scope_id) {
            for symbols in scope.symbols.values() {
                for symbol in symbols {
                    result.push(symbol.clone());
                }
            }
        }
        
        result
    }

    /// Get the parent scope of a given scope
    pub fn get_parent_scope(&self, scope_id: ScopeId) -> Option<ScopeId> {
        self.get_scope(scope_id).and_then(|scope| scope.parent)
    }

    /// Check if the given scope is a module scope (i.e., the primary scope for a module)
    pub fn is_module_scope(&self, scope_id: ScopeId) -> bool {
        if let Some(scope) = self.get_scope(scope_id) {
            // Get all scopes for this module
            let module_scopes = self.get_scopes_for_module(scope.module);
            
            // If this is the first/primary scope for the module
            if !module_scopes.is_empty() && module_scopes[0] == scope_id {
                return true;
            }
        }
        false
    }

    /// Add a symbol to a specific scope.
    pub fn add_symbol_to_scope(&mut self, scope_id: ScopeId, symbol: Symbol) {
        let name = symbol.name().0.clone();
        
        if let Some(scope) = self.scopes.get_mut(&scope_id) {
            scope.symbols.entry(name).or_insert_with(Vec::new).push(symbol);
        }
    }

    /// Find a module by path, starting from the given scope.
    /// The path is a list of module names to traverse.
    /// Returns the module ID and scope ID if found.
    pub fn find_module_by_path(&self, starting_scope: ScopeId, path: &[String]) -> Option<(ModuleId, ScopeId)> {
        if path.is_empty() {
            return None;
        }
        
        let first_segment = &path[0];
        
        // Look for the first segment in the starting scope and its ancestors
        let mut current_scope = Some(starting_scope);
        let mut found_module_id = None;
        let mut found_scope_id = None;
        
        while let Some(scope_id) = current_scope {
            let segment_symbols = self.lookup_in_scope(scope_id, first_segment);
            
            for symbol in segment_symbols {
                match symbol {
                    Symbol::Module { id, .. } => {
                        found_module_id = Some(*id);
                        found_scope_id = self.get_scopes_for_module(*id).first().cloned();
                        break;
                    },
                    Symbol::Import { target, .. } => {
                        if let Symbol::Module { id, .. } = &**target {
                            found_module_id = Some(*id);
                            found_scope_id = self.get_scopes_for_module(*id).first().cloned();
                            break;
                        }
                    },
                    _ => continue,
                }
            }
            
            if found_module_id.is_some() {
                break;
            }
            
            // Try parent scope
            current_scope = self.get_parent_scope(scope_id);
        }
        
        // If first segment not found, return None
        if found_module_id.is_none() || found_scope_id.is_none() {
            return None;
        }
        
        // Handle single segment path
        if path.len() == 1 {
            return Some((found_module_id.unwrap(), found_scope_id.unwrap()));
        }
        
        // For multi-segment paths, continue resolving from the found module
        let mut current_module_id = found_module_id.unwrap();
        let mut current_scope_id = found_scope_id.unwrap();
        
        for segment in &path[1..] {
            let segment_symbols = self.lookup_in_scope(current_scope_id, segment);
            let mut found = false;
            
            for symbol in segment_symbols {
                match symbol {
                    Symbol::Module { id, .. } => {
                        current_module_id = *id;
                        current_scope_id = match self.get_scopes_for_module(*id).first() {
                            Some(scope) => *scope,
                            None => return None,
                        };
                        found = true;
                        break;
                    },
                    Symbol::Import { target, .. } => {
                        if let Symbol::Module { id, .. } = &**target {
                            current_module_id = *id;
                            current_scope_id = match self.get_scopes_for_module(*id).first() {
                                Some(scope) => *scope,
                                None => return None,
                            };
                            found = true;
                            break;
                        }
                    },
                    _ => continue,
                }
            }
            
            if !found {
                return None;
            }
        }
        
        Some((current_module_id, current_scope_id))
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

/// Create a new symbol table for the Salsa database.
pub fn create_symbol_table(db: &dyn crate::db::ResolverDatabase) -> std::sync::Arc<SymbolTable> {
    // This function integrates with Salsa for incremental computation
    // It creates a symbol table based on the resolved crate
    if let Ok(resolved_crate) = db.resolved_crate() {
        resolved_crate.symbol_table
    } else {
        // If resolution failed, return an empty symbol table
        std::sync::Arc::new(SymbolTable::new())
    }
} 