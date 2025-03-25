//! Namespace management for name resolution.
//!
//! Parallax has three separate namespaces:
//! - Types: structs, enums, type aliases, traits, etc.
//! - Values: functions, variables, etc.
//! - Modules: modules and imports.

use crate::symbol::{Symbol, SymbolTable};

/// The namespaces in Parallax.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Namespace {
    /// The type namespace (structs, enums, type aliases, traits, etc.)
    Types,
    /// The value namespace (functions, variables, etc.)
    Values,
    /// The module namespace (modules and imports)
    Modules,
}

/// Helper functions for working with namespaces.
#[derive(Debug, Clone, Default)]
pub struct NamespaceManager;

impl NamespaceManager {
    /// Create a new namespace manager.
    pub fn new() -> Self {
        Self {}
    }
    
    /// Get the namespace of a symbol.
    pub fn get_namespace(symbol: &Symbol) -> Namespace {
        match symbol {
            Symbol::Function { .. } => Namespace::Values,
            Symbol::Struct { .. } => Namespace::Types,
            Symbol::Enum { .. } => Namespace::Types,
            Symbol::Variable { .. } => Namespace::Values,
            Symbol::Module { .. } => Namespace::Modules,
            Symbol::Trait { .. } => Namespace::Types,
            Symbol::TypeParam { .. } => Namespace::Types,
            Symbol::Import { target, .. } => Self::get_namespace(target),
        }
    }

    /// Lookup a symbol in a specific namespace.
    pub fn lookup_in_namespace<'a>(
        symbol_table: &'a SymbolTable,
        name: &str,
        namespace: Namespace,
    ) -> Vec<&'a Symbol> {
        symbol_table
            .lookup(name)
            .into_iter()
            .filter(|symbol| Self::get_namespace(symbol) == namespace)
            .collect()
    }

    /// Check if a symbol belongs to a namespace.
    pub fn is_in_namespace(symbol: &Symbol, namespace: Namespace) -> bool {
        Self::get_namespace(symbol) == namespace
    }

    /// Find a symbol in a specific namespace.
    pub fn find_in_namespace<'a>(
        symbols: impl IntoIterator<Item = &'a Symbol>,
        namespace: Namespace,
    ) -> Vec<&'a Symbol> {
        symbols
            .into_iter()
            .filter(|symbol| Self::is_in_namespace(symbol, namespace))
            .collect()
    }

    /// Check if types and values with the same name can coexist.
    pub fn can_share_name(symbol1: &Symbol, symbol2: &Symbol) -> bool {
        let ns1 = Self::get_namespace(symbol1);
        let ns2 = Self::get_namespace(symbol2);
        
        // Same namespace -> can't share name
        if ns1 == ns2 {
            return false;
        }
        
        // Different namespaces -> can share name only in specific cases
        match (ns1, ns2) {
            // Types and values can share names
            (Namespace::Types, Namespace::Values) | (Namespace::Values, Namespace::Types) => true,
            
            // Modules can't share names with anything else
            (Namespace::Modules, _) | (_, Namespace::Modules) => false,
            
            // Default case (should be unreachable)
            _ => false,
        }
    }
} 