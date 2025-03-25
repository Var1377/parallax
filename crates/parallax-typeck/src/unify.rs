use std::collections::HashMap;
use crate::{
    context::{Ty, TyVid, Constraint},
    error::TypeError,
};
use parallax_lang::ast::Span;

/// A unification table for solving type constraints
pub struct UnificationTable {
    /// Maps type variables to their representative type
    substitutions: HashMap<TyVid, Ty>,
}

impl UnificationTable {
    pub fn new() -> Self {
        Self {
            substitutions: HashMap::new(),
        }
    }

    /// Solve all constraints in the given list
    pub fn unify_all(&mut self, constraints: &[Constraint]) -> Result<(), TypeError> {
        for constraint in constraints {
            match constraint {
                Constraint::Eq(a, b) => self.unify(a, b)?,
                Constraint::SubType(sub, sup) => self.unify_subtype(sub, sup)?,
            }
        }
        Ok(())
    }

    /// Unify two types, ensuring they are equal
    pub fn unify(&mut self, a: &Ty, b: &Ty) -> Result<(), TypeError> {
        let a = self.resolve(a);
        let b = self.resolve(b);

        match (a, b) {
            // Two type variables - make them equal
            (Ty::Var(a_vid), Ty::Var(b_vid)) => {
                self.substitutions.insert(a_vid, Ty::Var(b_vid));
                Ok(())
            }

            // Type variable and concrete type - substitute
            (Ty::Var(vid), ty) | (ty, Ty::Var(vid)) => {
                // Check for recursive types
                if self.occurs_check(&vid, &ty) {
                    return Err(TypeError::RecursiveType {
                        ty: ty.clone(),
                        span: Span { start: 0, end: 0 }, // Dummy span
                    });
                }
                self.substitutions.insert(vid, ty);
                Ok(())
            }

            // Two function types
            (
                Ty::Function {
                    params: params1,
                    ret: ret1,
                },
                Ty::Function {
                    params: params2,
                    ret: ret2,
                },
            ) => {
                if params1.len() != params2.len() {
                    return Err(TypeError::UnificationError {
                        first: Ty::Function {
                            params: params1,
                            ret: ret1,
                        },
                        second: Ty::Function {
                            params: params2,
                            ret: ret2,
                        },
                        span: Span { start: 0, end: 0 }, // Dummy span
                    });
                }

                for (p1, p2) in params1.iter().zip(params2.iter()) {
                    self.unify(p1, p2)?;
                }
                self.unify(&ret1, &ret2)?;
                Ok(())
            }

            // Two tuple types
            (Ty::Tuple(tys1), Ty::Tuple(tys2)) => {
                if tys1.len() != tys2.len() {
                    return Err(TypeError::UnificationError {
                        first: Ty::Tuple(tys1),
                        second: Ty::Tuple(tys2),
                        span: Span { start: 0, end: 0 }, // Dummy span
                    });
                }

                for (t1, t2) in tys1.iter().zip(tys2.iter()) {
                    self.unify(t1, t2)?;
                }
                Ok(())
            }

            // Two concrete types
            (Ty::Concrete(c1), Ty::Concrete(c2)) if c1 == c2 => Ok(()),

            // Everything else fails
            (t1, t2) => Err(TypeError::UnificationError {
                first: t1,
                second: t2,
                span: Span { start: 0, end: 0 }, // Dummy span
            }),
        }
    }

    /// Unify a subtype with a supertype
    fn unify_subtype(&mut self, sub: &Ty, sup: &Ty) -> Result<(), TypeError> {
        // For now, just require equality. In the future, this would handle
        // proper subtyping relationships
        self.unify(sub, sup)
    }

    /// Resolve a type by following substitutions
    fn resolve(&self, ty: &Ty) -> Ty {
        match ty {
            Ty::Var(vid) => match self.substitutions.get(vid) {
                Some(ty) => self.resolve(ty),
                None => ty.clone(),
            },
            Ty::Function { params, ret } => Ty::Function {
                params: params.iter().map(|t| self.resolve(t)).collect(),
                ret: Box::new(self.resolve(ret)),
            },
            Ty::Tuple(tys) => Ty::Tuple(tys.iter().map(|t| self.resolve(t)).collect()),
            ty => ty.clone(),
        }
    }

    /// Check if a type variable occurs in a type (to prevent recursive types)
    fn occurs_check(&self, vid: &TyVid, ty: &Ty) -> bool {
        match ty {
            Ty::Var(other_vid) => {
                if vid == other_vid {
                    return true;
                }
                match self.substitutions.get(other_vid) {
                    Some(ty) => self.occurs_check(vid, ty),
                    None => false,
                }
            }
            Ty::Function { params, ret } => {
                params.iter().any(|t| self.occurs_check(vid, t))
                    || self.occurs_check(vid, ret)
            }
            Ty::Tuple(tys) => tys.iter().any(|t| self.occurs_check(vid, t)),
            Ty::Concrete(_) => false,
        }
    }
} 