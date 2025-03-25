use crate::context::{Ty, ConcreteTy};
use parallax_lang::ast;
use std::collections::HashSet;

/// A trait predicate that needs to be proved
#[derive(Debug, Clone)]
pub struct PredicateObligation {
    pub trait_ref: TraitRef,
    pub span: parallax_lang::ast::Span,
    // Track the generic environment for this obligation
    pub generic_env: Option<GenericEnv>,
}

/// A reference to a trait with its type parameters
#[derive(Debug, Clone)]
pub struct TraitRef {
    pub trait_id: String, // TODO: Use proper trait ID type
    pub self_ty: Ty,
    pub args: Vec<Ty>,
    pub span: Option<parallax_lang::ast::Span>,
}

/// Environment for generic type parameters
#[derive(Debug, Clone, Default)]
pub struct GenericEnv {
    // Maps type parameter names to their bounds
    pub bounds: Vec<(String, Vec<TraitRef>)>,
    // Cache of previously checked implementations
    impl_cache: HashSet<(String, String)>, // (trait_name, type_name)
}

impl GenericEnv {
    /// Create a new generic environment
    pub fn new() -> Self {
        Self {
            bounds: Vec::new(),
            impl_cache: HashSet::new(),
        }
    }

    /// Add a bound to a type parameter
    pub fn add_bound(&mut self, param_name: String, trait_ref: TraitRef) {
        // Check if we already have this parameter
        for (name, bounds) in &mut self.bounds {
            if name == &param_name {
                bounds.push(trait_ref);
                return;
            }
        }

        // If not, add a new entry
        self.bounds.push((param_name, vec![trait_ref]));
    }

    /// Check if a type parameter satisfies a trait bound
    pub fn satisfies_bound(&self, param_name: &str, trait_id: &str) -> bool {
        for (name, bounds) in &self.bounds {
            if name == param_name {
                for bound in bounds {
                    if bound.trait_id == trait_id {
                        return true;
                    }
                }
            }
        }
        false
    }

    /// Cache an implementation check result
    pub fn cache_impl(&mut self, trait_name: &str, type_name: &str) {
        self.impl_cache.insert((trait_name.to_string(), type_name.to_string()));
    }

    /// Check if an implementation has been cached
    pub fn is_impl_cached(&self, trait_name: &str, type_name: &str) -> bool {
        self.impl_cache.contains(&(trait_name.to_string(), type_name.to_string()))
    }
}

// Declare the solver submodule
pub mod solver;

// Re-export the solver under the traits module
pub use solver::TraitSolver; 