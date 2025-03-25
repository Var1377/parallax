use crate::{
    context::{Ty, TyVid, Constraint, TypeContext, ConcreteTy},
    error::TypeError,
    traits::TraitRef,
};
use parallax_lang::ast::Span;
use std::collections::HashMap;

/// Unification context for tracking substitutions and solving constraints
pub struct Unifier<'a> {
    ctx: &'a mut TypeContext<'a>,
    substitutions: HashMap<TyVid, Ty>,
}

impl<'a> Unifier<'a> {
    /// Create a new unifier
    pub fn new(ctx: &'a mut TypeContext<'a>) -> Self {
        Self {
            ctx,
            substitutions: HashMap::new(),
        }
    }
    
    /// Unify all constraints in the context
    pub fn unify_all(&mut self) -> Result<(), TypeError> {
        let constraints = std::mem::take(&mut self.ctx.constraints);
        
        for constraint in constraints {
            match constraint {
                Constraint::Eq(a, b) => self.unify(&a, &b)?,
                Constraint::SubType(sub, sup) => self.unify_subtype(&sub, &sup)?,
                Constraint::HasTrait { ty, trait_id, args } => {
                    self.check_trait_bound(&ty, &trait_id, &args)?
                },
            }
        }
        
        // Apply substitutions to types in the context
        self.apply_substitutions();
        
        Ok(())
    }
    
    /// Unify two types for equality
    pub fn unify(&mut self, t1: &Ty, t2: &Ty) -> Result<(), TypeError> {
        match (t1, t2) {
            // Type variables
            (Ty::Var(v1), Ty::Var(v2)) if v1 == v2 => Ok(()),
            (Ty::Var(v), t) | (t, Ty::Var(v)) => {
                // Check for recursive types
                if self.contains_var(*v, t) {
                    return Err(TypeError::RecursiveType {
                        ty: t.clone(),
                        span: self.ctx.traits_span.unwrap_or(Span { start: 0, end: 0 }),
                    });
                }
                
                // Add substitution
                self.substitutions.insert(*v, t.clone());
                Ok(())
            }
            
            // Generic types
            (Ty::Generic { name: n1, .. }, Ty::Generic { name: n2, .. }) if n1 == n2 => {
                // Same generic parameter is equal to itself
                Ok(())
            }
            
            // Concrete types
            (Ty::Concrete(c1), Ty::Concrete(c2)) => self.unify_concrete(c1, c2),
            
            // Function types
            (Ty::Function { params: p1, ret: r1 }, Ty::Function { params: p2, ret: r2 }) => {
                // Check parameter count
                if p1.len() != p2.len() {
                    return Err(TypeError::Mismatch {
                        expected: t1.clone(),
                        found: t2.clone(),
                        span: self.ctx.traits_span.unwrap_or(Span { start: 0, end: 0 }),
                    });
                }
                
                // Unify parameters
                for (param1, param2) in p1.iter().zip(p2.iter()) {
                    self.unify(param1, param2)?;
                }
                
                // Unify return types
                self.unify(r1, r2)
            }
            
            // Tuple types
            (Ty::Tuple(elems1), Ty::Tuple(elems2)) => {
                // Check element count
                if elems1.len() != elems2.len() {
                    return Err(TypeError::Mismatch {
                        expected: t1.clone(),
                        found: t2.clone(),
                        span: self.ctx.traits_span.unwrap_or(Span { start: 0, end: 0 }),
                    });
                }
                
                // Unify elements
                for (elem1, elem2) in elems1.iter().zip(elems2.iter()) {
                    self.unify(elem1, elem2)?;
                }
                
                Ok(())
            }
            
            // Error type unifies with anything
            (Ty::Error, _) | (_, Ty::Error) => Ok(()),
            
            // Anything else is a mismatch
            _ => Err(TypeError::Mismatch {
                expected: t1.clone(),
                found: t2.clone(),
                span: self.ctx.traits_span.unwrap_or(Span { start: 0, end: 0 }),
            }),
        }
    }
    
    /// Unify two concrete types
    fn unify_concrete(&mut self, c1: &ConcreteTy, c2: &ConcreteTy) -> Result<(), TypeError> {
        match (c1, c2) {
            // Same variant is equal
            (ConcreteTy::Int, ConcreteTy::Int) |
            (ConcreteTy::Float, ConcreteTy::Float) |
            (ConcreteTy::Bool, ConcreteTy::Bool) |
            (ConcreteTy::String, ConcreteTy::String) |
            (ConcreteTy::Char, ConcreteTy::Char) |
            (ConcreteTy::Unit, ConcreteTy::Unit) => Ok(()),
            
            // Named types
            (ConcreteTy::Named { name: n1, args: a1 }, ConcreteTy::Named { name: n2, args: a2 }) => {
                // Check name
                if n1 != n2 {
                    return Err(TypeError::Mismatch {
                        expected: Ty::Concrete(c1.clone()),
                        found: Ty::Concrete(c2.clone()),
                        span: self.ctx.traits_span.unwrap_or(Span { start: 0, end: 0 }),
                    });
                }
                
                // Check argument count
                if a1.len() != a2.len() {
                    return Err(TypeError::Mismatch {
                        expected: Ty::Concrete(c1.clone()),
                        found: Ty::Concrete(c2.clone()),
                        span: self.ctx.traits_span.unwrap_or(Span { start: 0, end: 0 }),
                    });
                }
                
                // Unify arguments
                for (arg1, arg2) in a1.iter().zip(a2.iter()) {
                    self.unify(arg1, arg2)?;
                }
                
                Ok(())
            }
            
            // Anything else is a mismatch
            _ => Err(TypeError::Mismatch {
                expected: Ty::Concrete(c1.clone()),
                found: Ty::Concrete(c2.clone()),
                span: self.ctx.traits_span.unwrap_or(Span { start: 0, end: 0 }),
            }),
        }
    }
    
    /// Unify a subtype with a supertype
    fn unify_subtype(&mut self, sub: &Ty, sup: &Ty) -> Result<(), TypeError> {
        // For now, just use equality unification
        // In a complete implementation, we would implement proper subtyping rules
        self.unify(sub, sup)
    }
    
    /// Check if a type satisfies a trait bound
    fn check_trait_bound(&mut self, ty: &Ty, trait_id: &str, args: &[Ty]) -> Result<(), TypeError> {
        // Create a TraitRef from the type and trait name
        let trait_ref = TraitRef {
            trait_id: trait_id.to_string(),
            self_ty: ty.clone(),
            args: args.to_vec(),
            span: None,
        };
        
        // Use the TraitSolver through the TypeContext
        self.ctx.check_trait_implementation(&trait_ref)
    }
    
    /// Check if a type variable appears in a type
    fn contains_var(&self, vid: TyVid, ty: &Ty) -> bool {
        match ty {
            Ty::Var(v) => {
                if *v == vid {
                    return true;
                }
                // Check for indirect references through substitutions
                if let Some(subst_ty) = self.substitutions.get(v) {
                    return self.contains_var(vid, subst_ty);
                }
                false
            }
            Ty::Concrete(concrete_ty) => match concrete_ty {
                ConcreteTy::Named { args, .. } => {
                    args.iter().any(|arg| self.contains_var(vid, arg))
                }
                _ => false,
            },
            Ty::Function { params, ret } => {
                params.iter().any(|param| self.contains_var(vid, param)) ||
                self.contains_var(vid, ret)
            }
            Ty::Tuple(elems) => {
                elems.iter().any(|elem| self.contains_var(vid, elem))
            }
            Ty::Generic { .. } | Ty::Error => false,
        }
    }
    
    /// Apply substitutions to all recorded types in the context
    fn apply_substitutions(&mut self) {
        // To avoid borrow checker issues, first collect all spans and types
        let type_map_entries: Vec<_> = self.ctx.type_map.iter().map(|(span, ty)| (*span, ty.clone())).collect();
        
        // Then update each entry with substituted types
        for (span, ty) in type_map_entries {
            let new_ty = self.substitute_vars(&ty);
            if let Some(entry) = self.ctx.type_map.get_mut(&span) {
                *entry = new_ty;
            }
        }
    }
    
    /// Substitute type variables in a type
    fn substitute_vars(&self, ty: &Ty) -> Ty {
        match ty {
            Ty::Var(vid) => {
                if let Some(substituted) = self.substitutions.get(vid) {
                    // Recursively substitute in the result
                    self.substitute_vars(substituted)
                } else {
                    ty.clone()
                }
            }
            Ty::Concrete(concrete_ty) => match concrete_ty {
                ConcreteTy::Named { name, args } => {
                    let new_args = args.iter()
                        .map(|arg| self.substitute_vars(arg))
                        .collect();
                    
                    Ty::Concrete(ConcreteTy::Named {
                        name: name.clone(),
                        args: new_args,
                    })
                }
                _ => ty.clone(),
            },
            Ty::Function { params, ret } => {
                let new_params = params.iter()
                    .map(|param| self.substitute_vars(param))
                    .collect();
                
                let new_ret = Box::new(self.substitute_vars(ret));
                
                Ty::Function {
                    params: new_params,
                    ret: new_ret,
                }
            }
            Ty::Tuple(elems) => {
                let new_elems = elems.iter()
                    .map(|elem| self.substitute_vars(elem))
                    .collect();
                
                Ty::Tuple(new_elems)
            }
            // Generic types and error types are left as is
            Ty::Generic { .. } | Ty::Error => ty.clone(),
        }
    }
}

/// Unify types and constraints in the context, returning errors if unification fails
pub fn unify<'a>(ctx: &'a mut TypeContext<'a>) -> Result<(), TypeError> {
    let mut unifier = Unifier::new(ctx);
    unifier.unify_all()?;
    
    // Apply the substitutions to the type map
    unifier.apply_substitutions();
    
    Ok(())
} 