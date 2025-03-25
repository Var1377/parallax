use crate::{
    context::Ty,
    db::TypeCheckingDatabase,
    error::TypeError,
};

/// A trait predicate that needs to be proved
#[derive(Debug, Clone)]
pub struct PredicateObligation {
    pub trait_ref: TraitRef,
    pub span: parallax_lang::ast::Span,
}

/// A reference to a trait with its type parameters
#[derive(Debug, Clone)]
pub struct TraitRef {
    pub trait_id: String, // TODO: Use proper trait ID type
    pub self_ty: Ty,
    pub args: Vec<Ty>,
}

/// The trait solver, responsible for proving trait implementations
pub struct TraitSolver<'db> {
    db: &'db dyn TypeCheckingDatabase,
}

impl<'db> TraitSolver<'db> {
    pub fn new(db: &'db dyn TypeCheckingDatabase) -> Self {
        Self { db }
    }

    /// Solve all trait obligations
    pub fn solve_obligations(
        &mut self,
        obligations: &[PredicateObligation],
    ) -> Result<(), TypeError> {
        for obligation in obligations {
            self.solve_obligation(obligation)?;
        }
        Ok(())
    }

    /// Solve a single trait obligation
    fn solve_obligation(&mut self, obligation: &PredicateObligation) -> Result<(), TypeError> {
        // For now, just check if we have a direct impl
        // In the future, this would use Chalk for more sophisticated trait solving
        if !self.has_impl(&obligation.trait_ref) {
            return Err(TypeError::TraitNotImplemented {
                trait_: obligation.trait_ref.trait_id.clone(),
                ty: obligation.trait_ref.self_ty.clone(),
                span: obligation.span,
            });
        }
        Ok(())
    }

    /// Check if we have a direct implementation of a trait for a type
    fn has_impl(&self, trait_ref: &TraitRef) -> bool {
        // TODO: Actually look up impls in the database
        // For now, just return true for some built-in impls
        match (&trait_ref.trait_id[..], &trait_ref.self_ty) {
            ("Add", Ty::Concrete(ref c)) => matches!(
                c,
                crate::context::ConcreteTy::Int | crate::context::ConcreteTy::Float
            ),
            ("Display", _) => true, // Assume everything implements Display for now
            _ => false,
        }
    }
} 