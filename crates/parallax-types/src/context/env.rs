// src/context/env.rs
//! Defines the TypeEnvironment for managing variable bindings and scopes.

use std::collections::HashMap;
use std::sync::Arc;
use crate::types::Ty; // Use crate path
use crate::context::inference::Substitution; // Use crate path
use parallax_resolve::types::Symbol; // Import Symbol
use std::cell::RefCell; // Added for apply_substitution

/// A binding in the type environment, storing type and symbol.
#[derive(Debug, Clone)]
struct Binding {
    ty: Ty,
    symbol: Symbol, // Symbol of the definition/binding site
}

/// A store for variable types during type checking, supporting nested scopes.
#[derive(Debug, Clone)]
pub struct TypeEnvironment {
    /// Map from variable names to their binding info (type + symbol) in the current scope.
    env: RefCell<HashMap<String, Binding>>,
    /// Parent environment for lexical scoping.
    pub(crate) parent: Option<Arc<TypeEnvironment>>,
}

impl Default for TypeEnvironment { // Add Default impl
    fn default() -> Self {
        Self::new()
    }
}

impl TypeEnvironment {
    /// Create a new empty root environment.
    ///
    /// Preconditions: None.
    /// Postconditions: Returns a new `TypeEnvironment` with no bindings and no parent.
    pub fn new() -> Self {
        Self {
            env: RefCell::new(HashMap::new()),
            parent: None,
        }
    }

    /// Create a new environment with a specified parent scope.
    ///
    /// Preconditions: `parent` is a valid `Arc<TypeEnvironment>`.
    /// Postconditions: Returns a new `TypeEnvironment` with the given parent and no local bindings.
    pub fn with_parent(parent: Arc<TypeEnvironment>) -> Self {
        Self {
            env: RefCell::new(HashMap::new()),
            parent: Some(parent),
        }
    }

    /// Looks up a variable name in the environment chain.
    /// Starts in the current scope and walks up the parent chain.
    ///
    /// Preconditions: `name` is the variable name to look up.
    /// Postconditions: Returns `Some((Ty, Symbol))` (owned) if found, `None` otherwise.
    pub fn get(&self, name: &str) -> Option<(Ty, Symbol)> {
        let binding_ref = self.env.borrow();
        if let Some(binding) = binding_ref.get(name) {
            // Return owned copies
            Some((binding.ty.clone(), binding.symbol))
        } else if let Some(p) = &self.parent {
            p.get(name) // Recursively search parent
        } else {
            None // Not found in any scope
        }
    }

    /// Add a binding for a variable name in the current scope.
    ///
    /// Preconditions: `name` is the variable identifier to bind.
    ///                `symbol` is the resolver symbol for this binding.
    ///                `ty` is the type to bind it to.
    /// Postconditions: The binding is added to the *current* scope (not parents).
    pub fn add(&self, name: String, symbol: Symbol, ty: Ty) {
        self.env.borrow_mut().insert(name, Binding { ty, symbol });
    }

    /// Apply a substitution to all types *within the current scope only*.
    /// Does not modify parent scopes. Returns a *new* environment.
    ///
    /// Preconditions: `subst` contains the substitutions to apply.
    /// Postconditions: Returns a new `TypeEnvironment` with the same parent,
    /// where all types in the local `env` have had the substitution applied.
    pub fn apply_substitution(&self, subst: &Substitution) -> Self {
        let new_env = self.env.borrow().iter()
            .map(|(name, binding)| {
                (name.clone(), Binding {
                    ty: binding.ty.apply_subst(subst),
                    symbol: binding.symbol,
                })
            })
            .collect::<HashMap<_, _>>();

        let new_parent = self.parent.as_ref().map(|p| Arc::new(p.apply_substitution(subst)));

        Self {
            env: RefCell::new(new_env),
            parent: new_parent,
        }
    }

    /// Get a map of bindings defined *directly* in the current scope (excluding parents).
    /// Primarily useful for debugging or specific scope analysis.
    ///
    /// Preconditions: None.
    /// Postconditions: Returns a clone of the local `env` map with type and symbol.
    #[allow(dead_code)]
    pub(crate) fn get_current_scope_bindings(&self) -> HashMap<String, (Ty, Symbol)> {
        self.env.borrow()
            .iter()
            .map(|(name, binding)| (name.clone(), (binding.ty.clone(), binding.symbol)))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{Ty, TyKind, PrimitiveType};
    use crate::context::inference::TypeId;
    use std::collections::BTreeMap;
    use std::sync::Arc;
    use parallax_resolve::types::Symbol;

    fn i32_ty() -> Ty { Ty::new(TyKind::Primitive(PrimitiveType::I32)) }
    fn bool_ty() -> Ty { Ty::new(TyKind::Primitive(PrimitiveType::Bool)) }
    fn var_ty(id: u32) -> Ty { Ty::new(TyKind::Var(TypeId(id))) }

    #[test]
    fn env_new_empty() {
        let env = TypeEnvironment::new();
        assert!(env.env.borrow().is_empty());
        assert!(env.parent.is_none());
    }

    #[test]
    fn env_add_and_get_single_scope() {
        let mut env = TypeEnvironment::new();
        let sym1 = Symbol::fresh();
        env.add("x".to_string(), sym1, i32_ty());

        let (ty, symbol) = env.get("x").expect("variable x not found");
        assert_eq!(ty, i32_ty());
        assert_eq!(symbol, sym1);
        assert!(env.get("y").is_none());
    }

    #[test]
    fn env_nested_scopes_get_parent() {
        let mut parent_env = TypeEnvironment::new();
        let sym_parent = Symbol::fresh();
        parent_env.add("a".to_string(), sym_parent, bool_ty());
        let parent_arc = Arc::new(parent_env);

        let mut child_env = TypeEnvironment::with_parent(parent_arc.clone());
        let sym_child = Symbol::fresh();
        child_env.add("b".to_string(), sym_child, i32_ty());

        // Get from child scope
        let (ty_b, sym_b) = child_env.get("b").expect("variable b not found");
        assert_eq!(ty_b, i32_ty());
        assert_eq!(sym_b, sym_child);

        // Get from parent scope
        let (ty_a, sym_a) = child_env.get("a").expect("variable a not found in parent");
        assert_eq!(ty_a, bool_ty());
        assert_eq!(sym_a, sym_parent);

        // Check parent is untouched
        assert!(parent_arc.get("b").is_none());
    }

    #[test]
    fn env_nested_scopes_shadowing() {
        let mut parent_env = TypeEnvironment::new();
        let sym_parent = Symbol::fresh();
        parent_env.add("x".to_string(), sym_parent, bool_ty());
        let parent_arc = Arc::new(parent_env);

        let mut child_env = TypeEnvironment::with_parent(parent_arc);
        let sym_child = Symbol::fresh();
        child_env.add("x".to_string(), sym_child, i32_ty()); // Shadow x

        // Get shadowed variable (should be child's version)
        let (ty_x, sym_x) = child_env.get("x").expect("variable x not found in child");
        assert_eq!(ty_x, i32_ty());
        assert_eq!(sym_x, sym_child);
    }

    #[test]
    fn env_apply_substitution_current_scope_only() {
        let mut parent_env = TypeEnvironment::new();
        let sym_parent = Symbol::fresh();
        parent_env.add("a".to_string(), sym_parent, var_ty(0)); // Parent has t0
        let parent_arc = Arc::new(parent_env);

        let mut child_env = TypeEnvironment::with_parent(parent_arc.clone());
        let sym_child_b = Symbol::fresh();
        let sym_child_c = Symbol::fresh();
        child_env.add("b".to_string(), sym_child_b, var_ty(1)); // Child has t1
        child_env.add("c".to_string(), sym_child_c, bool_ty()); // Child has bool

        let mut subst = Substitution::new();
        subst.insert(TypeId(0), i32_ty()); // t0 -> i32
        subst.insert(TypeId(1), bool_ty()); // t1 -> bool

        let substituted_child = child_env.apply_substitution(&subst);

        // Check substituted child scope
        let (ty_b, _) = substituted_child.get("b").unwrap();
        assert_eq!(ty_b, bool_ty()); // b (was t1) should now be bool
        let (ty_c, _) = substituted_child.get("c").unwrap();
        assert_eq!(ty_c, bool_ty()); // c should remain bool

        // Check parent scope *through the substituted child* (should not be changed)
        let (ty_a, _) = substituted_child.get("a").unwrap();
        assert_eq!(ty_a, var_ty(0)); // Parent's a (t0) should remain t0

        // Check original parent scope directly (unaffected)
        let (ty_a_orig, _) = parent_arc.get("a").unwrap();
        assert_eq!(ty_a_orig, var_ty(0));
    }
} 