// src/context/inference.rs
//! Manages type inference variables, substitutions, and the unification process.

use std::collections::{HashSet, BTreeMap, HashMap};
use std::fmt;
use miette::SourceSpan;
use std::sync::Arc;

use crate::core::SELF_TYPE_ID;
// Use crate paths
use crate::types::*;
use crate::error::{TypeError, TypeResult, display_type};
use crate::context::TraitRepository; // Import other context types

/// A unique identifier for a type variable during inference.
/// Represented as a simple integer index.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeId(pub u32);

/// A mapping from type variable IDs (`TypeId`) to inferred types (`Ty`).
/// Used to track the results of unification.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Substitution {
    map: BTreeMap<TypeId, Ty>,
}

impl Substitution {
    /// Create an empty substitution.
    pub fn new() -> Self {
        Self::default()
    }

    /// Retrieve the type associated with a `TypeId`.
    /// Handles transitive substitutions (e.g., t0 -> t1, t1 -> i32 will return i32 for t0).
    ///
    /// Preconditions: `var_id` is the ID to look up.
    /// Postconditions: Returns `Some(&Ty)` if the ID is mapped (possibly transitively),
    /// otherwise returns `None`.
    pub fn get(&self, var_id: &TypeId) -> Option<&Ty> {
        // Recursively resolve the substitution until a concrete type or unmapped ID is found.
        let mut current_id = *var_id;
        loop {
            match self.map.get(&current_id) {
                Some(ty) => {
                    match &ty.kind {
                        // If the substitution maps to another variable, follow the chain.
                        TyKind::Var(next_id) => {
                            // Avoid infinite loops for simple cycles (t0 -> t0)
                            if next_id == var_id {
                                return self.map.get(var_id);
                            }
                            current_id = *next_id;
                        }
                        // Found a non-variable type, return it.
                        _ => return Some(ty),
                    }
                }
                // ID is not present in the map, no substitution available.
                None => return None,
            }
        }
    }

    /// Add a substitution mapping a `TypeId` to a `Ty`.
    /// Precondition: `var_id` must not be `SELF_TYPE_ID`.
    /// Precondition: `ty` must not be `TyKind::Error`.
    /// Precondition: `ty` should not contain `var_id` (occurs check needed before calling).
    ///
    /// Preconditions: `var_id != SELF_TYPE_ID`, `ty` is not Error, `ty` does not contain `var_id`.
    /// Postconditions: The map contains the binding `var_id` -> `ty`.
    /// Assertions: Enforces preconditions.
    pub fn insert(&mut self, var_id: TypeId, ty: Ty) {
        // Assertion: Prevent adding a mapping for the special 'Self' placeholder.
        assert!(var_id != SELF_TYPE_ID, "Attempted to substitute SelfType placeholder");
        // Assertion: Don't store Error types directly in substitutions.
        assert!(!matches!(ty.kind, TyKind::Error), "Attempted to substitute with Error type");
        // Assertion: Occurs check should ideally happen *before* calling insert.
        // This assertion adds a safety net.
        assert!(!ty.free_vars().contains(&var_id), "Occurs check failed: Attempted to insert recursive type {:?} -> {}", var_id, display_type(&ty));

        self.map.insert(var_id, ty);
    }

    /// Get the set of all `TypeId`s that are keys in this substitution map.
    ///
    /// Preconditions: None.
    /// Postconditions: Returns a `HashSet` containing all `TypeId`s present as keys.
    pub fn domain(&self) -> HashSet<TypeId> {
        self.map.keys().cloned().collect()
    }

    /// Apply this substitution to a given type.
    /// Convenience method that delegates to `ty.apply_subst(self)`.
    ///
    /// Preconditions: `ty` is the type to apply the substitution to.
    /// Postconditions: Returns the new type after applying the substitution.
    pub fn apply_to_ty(&self, ty: &Ty) -> Ty {
        ty.apply_subst(self)
    }

    /// Combine two substitutions. If there are conflicting bindings for the same `TypeId`,
    /// the bindings from `other` take precedence.
    /// It applies `self` to the types in `other` before merging.
    ///
    /// Preconditions: `self` and `other` are valid substitutions.
    /// Postconditions: Returns a new `Substitution` containing the merged bindings.
    /// Assertions: Checks for unification errors during composition.
    pub fn compose(&self, other: &Substitution, _trait_repo: &TraitRepository) -> TypeResult<Substitution> {
        let mut new_subst = self.map.clone(); // Start with our current substitutions
        for (var_id, ty) in &other.map {
            // Apply our substitution to the type being inserted from `other`
            let substituted_ty = self.apply_to_ty(ty);

            if let Some(existing_ty) = new_subst.get(var_id) {
                // Variable already exists. Unify the existing type with the new substituted type.
                // We need a dummy span here.
                let dummy_span = SourceSpan::from(0..0);
                let mut temp_inf_ctx = InferenceContext::new(); // Need a temporary context
                // TODO: Pass the actual trait_repo here. This is problematic.
                // We need access to the TraitRepository to solve constraints arising from composition.
                // For now, assume an empty repo for the temp solve, which is incorrect but avoids API breakage.
                let dummy_repo = TraitRepository::new(); 
                temp_inf_ctx.unify(existing_ty, &substituted_ty, dummy_span)?; // Propagate unification error
                // If unification succeeded, the result is implicitly handled by the existing binding
                // in new_subst and the recursive application in `apply_to_ty`.
                // We need to apply the *result* of the unification (potentially updating existing bindings).
                let unification_subst = temp_inf_ctx.solve(&dummy_repo)?; // Pass dummy repo here too
                for (k, v) in unification_subst.map {
                    // Apply the composed substitution *so far* to the new value before inserting
                    let final_v = self.apply_to_ty(&other.apply_to_ty(&v)); // Apply both
                    new_subst.insert(k, final_v);
                }
                
            } else {
                // Variable doesn't exist, insert the substituted type directly.
                new_subst.insert(*var_id, substituted_ty);
            }
        }
        Ok(Substitution { map: new_subst })
    }

    /// Returns true if the substitution map is empty.
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
}

/// Indicates the kind of defaulting required for an unresolved type variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefaultKind {
    Integer, // Defaults to i32
    Float,   // Defaults to f64
}

/// Manages the state of type inference, including variable generation and constraints.
#[derive(Debug)] // Removed Clone
pub struct InferenceContext {
    /// The current substitution map, updated during unification.
    subst: Substitution,
    /// Counter for generating fresh type variable IDs.
    next_var_id: u32,
    /// Constraints accumulated during unification (e.g., trait bounds).
    /// Vec<(Ty, TraitRef)> signifies "Ty must implement TraitRef".
    constraints: Vec<(Ty, TraitRef)>,
    /// Tracks type variables created for unsuffixed literals that need defaulting.
    defaulting_needed: HashMap<TypeId, DefaultKind>,
    // TODO: Consider adding recursion depth tracking for occurs check/limit.
    // recursion_depth: u32,
}

/// Represents a snapshot of the inference context, allowing backtracking.
/// Used for speculative type checking (e.g., trying different method candidates).
#[derive(Debug, Clone)]
pub struct InferenceSnapshot {
    /// The substitution map at the time of the snapshot.
    subst: Substitution,
    /// The next variable ID at the time of the snapshot.
    next_var_id: u32,
    /// The number of constraints at the time of the snapshot.
    constraints_len: usize,
    /// The defaulting map at the time of the snapshot.
    defaulting_needed: HashMap<TypeId, DefaultKind>,
}

impl InferenceContext {
    /// Creates a new, empty inference context.
    pub fn new() -> Self {
        Self {
            subst: Substitution::new(),
            next_var_id: 0,
            constraints: Vec::new(),
            defaulting_needed: HashMap::new(), // Initialize empty map
        }
    }

    /// Generates a fresh type variable (`TyKind::Var`) with a unique ID.
    /// Optionally marks the variable as needing integer or float defaulting.
    ///
    /// Preconditions: `default_kind` specifies if this variable needs defaulting.
    /// Postconditions: Returns a new `Ty` of kind `TyKind::Var` with a unique `TypeId`.
    ///                 Increments the internal `next_var_id` counter.
    ///                 If `default_kind` is Some, adds an entry to `self.defaulting_needed`.
    pub fn fresh_var_with_defaulting(&mut self, default_kind: Option<DefaultKind>) -> Ty {
        let id = TypeId(self.next_var_id);
        self.next_var_id += 1;
        // Assertion: Check for potential overflow, though unlikely.
        assert!(self.next_var_id < u32::MAX - 1, "TypeId counter overflow");

        if let Some(kind) = default_kind {
            self.defaulting_needed.insert(id, kind);
        }

        Ty::new(TyKind::Var(id))
    }

    // Keep the old fresh_var for cases where no defaulting is needed.
    pub fn fresh_var(&mut self) -> Ty {
        self.fresh_var_with_defaulting(None)
    }

    /// Attempts to unify two types, `ty1` and `ty2`.
    /// Modifies the internal substitution map (`self.subst`) if unification is possible.
    /// Adds trait constraints to `self.constraints` if necessary.
    ///
    /// Preconditions: `ty1` and `ty2` are the types to unify. `span` is for error reporting.
    /// Postconditions: If successful, `self.subst` is updated to reflect the unification.
    ///                 If traits are involved, constraints might be added to `self.constraints`.
    ///                 Returns `Ok(())` on success, `Err(TypeError)` on failure.
    /// Assertions: Performs occurs check.
    pub fn unify(&mut self, ty1: &Ty, ty2: &Ty, span: SourceSpan) -> TypeResult<()> {
        // Apply current substitution to both types before attempting unification.
        let t1 = self.subst.apply_to_ty(ty1);
        let t2 = self.subst.apply_to_ty(ty2);

        // --- Base Cases ---
        // If types are already equal, unification succeeds trivially.
        if t1 == t2 { return Ok(()); }

        // If either type is an error type, unification succeeds but propagates the error state.
        if matches!(t1.kind, TyKind::Error) || matches!(t2.kind, TyKind::Error) { return Ok(()); }

        // Handle unification with the Never type `!`.
        if matches!(t1.kind, TyKind::Never) { return Ok(()); } // `!` unifies with anything (flows into it).
        if matches!(t2.kind, TyKind::Never) { return Ok(()); } // Symmetrically.

        // --- Variable Cases ---
        // If t1 is a variable, try to substitute it with t2.
        if let TyKind::Var(id1) = t1.kind {
            return self.unify_variable(id1, &t2, span);
        }
        // If t2 is a variable, try to substitute it with t1.
        if let TyKind::Var(id2) = t2.kind {
            return self.unify_variable(id2, &t1, span);
        }

        // --- Recursive Cases --- Handle unification of compound types.
        match (&t1.kind, &t2.kind) {
            // --- Primitives --- (Must match exactly)
            (TyKind::Primitive(p1), TyKind::Primitive(p2)) => {
                if p1 == p2 { Ok(()) }
                 else {
                    Err(TypeError::TypeMismatch {
                        expected: display_type(&t1), found: display_type(&t2), span,
                    })
                }
            }

            // --- InferInt --- 
            // InferInt(id) with Primitive(p) where p is integer => unify Var(id) with Primitive(p)
            (TyKind::InferInt(id1), TyKind::Primitive(p2)) if p2.is_integer() => {
                self.unify_variable(*id1, &t2, span)
            }
            (TyKind::Primitive(p1), TyKind::InferInt(id2)) if p1.is_integer() => {
                self.unify_variable(*id2, &t1, span)
            }
            // InferInt(id1) with InferInt(id2) => unify Var(id1) with Var(id2)
            (TyKind::InferInt(id1), TyKind::InferInt(id2)) => {
                // Unify the underlying variables
                self.unify_variable(*id1, &Ty::new(TyKind::Var(*id2)), span)
            }
            // InferInt(id1) with Var(id2) => unify Var(id1) with Var(id2)
            (TyKind::InferInt(id1), TyKind::Var(id2)) => {
                self.unify_variable(*id1, &t2, span)
            }
            (TyKind::Var(id1), TyKind::InferInt(id2)) => {
                self.unify_variable(*id1, &Ty::new(TyKind::Var(*id2)), span) // Unify var1 with var2
            }

            // --- InferFloat --- (Similar logic as InferInt)
            (TyKind::InferFloat(id1), TyKind::Primitive(p2)) if p2.is_float() => {
                self.unify_variable(*id1, &t2, span)
            }
            (TyKind::Primitive(p1), TyKind::InferFloat(id2)) if p1.is_float() => {
                self.unify_variable(*id2, &t1, span)
            }
            (TyKind::InferFloat(id1), TyKind::InferFloat(id2)) => {
                self.unify_variable(*id1, &Ty::new(TyKind::Var(*id2)), span)
            }
            (TyKind::InferFloat(id1), TyKind::Var(id2)) => {
                self.unify_variable(*id1, &t2, span)
            }
            (TyKind::Var(id1), TyKind::InferFloat(id2)) => {
                self.unify_variable(*id1, &Ty::new(TyKind::Var(*id2)), span)
            }

            // --- Named types (structs/enums) ---
            (TyKind::Named { symbol: sym1, args: args1, .. }, TyKind::Named { symbol: sym2, args: args2, .. }) => {
                // Symbols must match for the types to be potentially compatible.
                if sym1 != sym2 {
                    return Err(TypeError::TypeMismatch {
                        expected: display_type(&t1), found: display_type(&t2), span,
                    });
                }
                // Argument counts must match.
                if args1.len() != args2.len() {
                    return Err(TypeError::GenericArgCountMismatch {
                        kind: "Type".to_string(), // TODO: Be more specific (Struct/Enum)?
                        name: display_type(&t1), // Display the base type
                        expected: args1.len(),
                        found: args2.len(),
                        span,
                    });
                }
                // Recursively unify arguments.
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    self.unify(arg1, arg2, span)?;
                }
                Ok(())
            }
            // Arrays must have the same size and unify element types.
            (TyKind::Array(elem1, size1), TyKind::Array(elem2, size2)) => {
                if size1 != size2 {
                    return Err(TypeError::TypeMismatch {
                        expected: format!("array of size {:?}", size1),
                        found: format!("array of size {:?}", size2),
                        span,
                    });
                }
                self.unify(elem1, elem2, span)
            }
            // Tuples must have the same arity and unify element types.
            (TyKind::Tuple(elems1), TyKind::Tuple(elems2)) => {
                if elems1.len() != elems2.len() {
                    return Err(TypeError::TypeMismatch {
                        expected: format!("tuple of size {}", elems1.len()),
                        found: format!("tuple of size {}", elems2.len()),
                        span,
                    });
                }
                for (elem1, elem2) in elems1.iter().zip(elems2.iter()) {
                    self.unify(elem1, elem2, span)?;
                }
                Ok(())
            }
            // Functions must unify parameter counts, parameter types (contravariantly), and return types (covariantly).
            (TyKind::Function(params1, ret1), TyKind::Function(params2, ret2)) => {
                if params1.len() != params2.len() {
                    return Err(TypeError::ParamCountMismatch {
                        name: "(function type)".to_string(),
                        expected: params1.len(),
                        found: params2.len(),
                        span,
                    });
                }
                // Unify parameters (note: strict invariance for now, matching Rust).
                for (p1, p2) in params1.iter().zip(params2.iter()) {
                    self.unify(p1, p2, span)?;
                    // For full contravariance: self.unify(p2, p1, span)?;
                }
                // Unify return types.
                self.unify(ret1, ret2, span)
            }
             // Maps must unify key and value types.
             (TyKind::Map(k1, v1), TyKind::Map(k2, v2)) => {
                self.unify(k1, k2, span)?;
                self.unify(v1, v2, span)
            }
            // Sets must unify element types.
            (TyKind::Set(e1), TyKind::Set(e2)) => {
                self.unify(e1, e2, span)
            }
            // Unification with SelfType should generally be handled by substitution *before* unify is called.
            // If it occurs here, it might indicate an error or unresolved state.
            (TyKind::SelfType, _) | (_, TyKind::SelfType) => {
                 Err(TypeError::InternalError {
                     message: "Attempted to unify unresolved SelfType".to_string(),
                     span: Some(span),
                 })
             }
            // All other combinations are type mismatches.
            _ => Err(TypeError::TypeMismatch {
                expected: display_type(&t1),
                found: display_type(&t2),
                span,
            }),
        }
    }

    /// Helper function to unify a variable (`var_id`) with another type (`other_ty`).
    /// Performs the occurs check and updates the substitution map.
    fn unify_variable(&mut self, var_id: TypeId, other_ty: &Ty, span: SourceSpan) -> TypeResult<()> {
        // If the other type is the *same* variable, do nothing.
        if let TyKind::Var(other_id) = other_ty.kind {
            if var_id == other_id { return Ok(()); }
        }

        // Occurs Check: Ensure `var_id` does not appear within `other_ty`.
        // This prevents infinite types like `t = List<t>`.
        if other_ty.free_vars().contains(&var_id) {
            // TODO: Improve error span? It might be better to report where the cycle is introduced.
            return Err(TypeError::InferenceRecursionLimit { span }); // Use a specific error?
        }

        // <<< START DEFAULTING PROPAGATION >>>
        let var_default_kind = self.defaulting_needed.get(&var_id).copied();
        let mut other_default_kind = None;

        // Check if other_ty is a variable that needs defaulting
        if let TyKind::Var(other_id) = other_ty.kind {
            other_default_kind = self.defaulting_needed.get(&other_id).copied();
        }
        // Check if other_ty is an InferInt/InferFloat itself
        match other_ty.kind {
            TyKind::InferInt(infer_id) => other_default_kind = self.defaulting_needed.get(&infer_id).copied(),
            TyKind::InferFloat(infer_id) => other_default_kind = self.defaulting_needed.get(&infer_id).copied(),
            _ => {}
        }

        // Check for conflicting defaults
        if let (Some(d1), Some(d2)) = (var_default_kind, other_default_kind) {
            if d1 != d2 {
                return Err(TypeError::LiteralDefaultConflict {
                    ty1: display_type(&Ty::new(TyKind::Var(var_id))),
                    ty2: display_type(other_ty),
                    span,
                });
            }
        }

        // Propagate defaulting requirement
        // If one needs defaulting and the other doesn't, mark the other.
        // If other_ty is Var(other_id), mark other_id.
        // If other_ty is InferInt(infer_id), mark infer_id.
        // If other_ty is InferFloat(infer_id), mark infer_id.
        let target_id_for_default = match other_ty.kind {
            TyKind::Var(id) => Some(id),
            TyKind::InferInt(id) => Some(id),
            TyKind::InferFloat(id) => Some(id),
            _ => None,
        };

        if let Some(target_id) = target_id_for_default {
            if let Some(kind_to_propagate) = var_default_kind.or(other_default_kind) {
                if self.defaulting_needed.get(&target_id) != Some(&kind_to_propagate) {
                    println!("    [UnifyVar Default] Propagating default {:?} from {:?} to {:?}", kind_to_propagate, var_id, target_id);
                    self.defaulting_needed.insert(target_id, kind_to_propagate);
                }
                if self.defaulting_needed.get(&var_id) != Some(&kind_to_propagate) {
                     println!("    [UnifyVar Default] Propagating default {:?} from {:?} to {:?}", kind_to_propagate, target_id, var_id);
                    self.defaulting_needed.insert(var_id, kind_to_propagate);
                }
            }
        }
        // <<< END DEFAULTING PROPAGATION >>>

        // If unification is valid, add the substitution.
        self.subst.insert(var_id, other_ty.clone());
        Ok(())
    }

    /// Add a trait constraint: `ty` must implement `trait_ref`.
    /// These constraints are solved later by `solve`.
    ///
    /// Preconditions: `ty` and `trait_ref` are the constraint components.
    /// Postconditions: The constraint `(ty, trait_ref)` is added to `self.constraints`.
    pub fn add_constraint(&mut self, ty: Ty, trait_ref: TraitRef) {
        // Apply current substitution before adding constraint
        let concrete_ty = self.subst.apply_to_ty(&ty);
        let concrete_trait_ref = TraitRef {
             trait_id: trait_ref.trait_id,
             // Apply substitution to trait arguments as well
             type_arguments: trait_ref.type_arguments.iter().map(|arg| self.subst.apply_to_ty(arg)).collect(),
             span: trait_ref.span,
         };
        self.constraints.push((concrete_ty, concrete_trait_ref));
    }

    /// Solves the accumulated constraints and returns the final substitution.
    /// This typically involves querying the `TraitRepository`.
    ///
    /// Preconditions: `trait_repo` provides access to trait/impl definitions.
    /// Postconditions: Returns `Ok(Substitution)` containing the complete substitution
    ///                 if all constraints are satisfied. Returns `Err(TypeError)` if
    ///                 any constraint cannot be satisfied.
    pub fn solve(&mut self, trait_repo: &TraitRepository) -> TypeResult<Substitution> {
        let mut progress = true;
        let mut errors: Vec<TypeError> = Vec::new();

        while progress {
            progress = false;
            let mut remaining_constraints = Vec::new();
            let constraints_to_check = std::mem::take(&mut self.constraints);

            // Apply current substitution before checking this round
            let subst_snapshot = self.subst.clone(); 
            let current_constraints: Vec<_> = constraints_to_check.into_iter()
                .map(|(ty, tr)| (subst_snapshot.apply_to_ty(&ty), substitute_trait_ref(&tr, &subst_snapshot)))
                .collect();

            for (ty, required_trait_ref) in current_constraints {
                let snapshot = self.snapshot(); // Snapshot before attempting resolution

                match trait_repo.resolve_trait_implementation(&ty, &required_trait_ref, self, required_trait_ref.span) {
                    Ok(Some(_impl_id)) => {
                        // Constraint satisfied!
                        // Check if the resolution generated new substitutions.
                        if self.subst != snapshot.subst { // Direct comparison now possible
                             progress = true;
                        }
                        // Constraint is removed by not adding it to remaining_constraints.
                        // Rollback is not needed as the unification was successful.
                        println!("Constraint satisfied: {} implements {}", display_type(&ty), trait_repo.get_trait(required_trait_ref.trait_id).map_or("???", |t|&t.name));
                    }
                    Ok(None) => {
                        // Check for specific built-in traits before giving up
                        let trait_name = trait_repo.get_trait(required_trait_ref.trait_id).map(|t| t.name.as_str());

                        let mut built_in_satisfied = false;
                        match trait_name {
                            Some("Copy") => {
                                // TODO: Implement check_is_copyable(target_ty)
                                if ty.is_primitive() { // Basic check
                                    println!("  -> Satisfied built-in Copy for primitive {}", display_type(&ty));
                                    built_in_satisfied = true;
                                    progress = true; // Found a solution
                                }
                            }
                            Some("Sized") => {
                                // For now, assume all types are Sized. This needs refinement later
                                // when unsized types (like slices or trait objects) are introduced.
                                println!("  -> Satisfied built-in Sized for {}", display_type(&ty));
                                built_in_satisfied = true;
                                progress = true;
                            }
                            Some("Add") | Some("Sub") | Some("Mul") | Some("Div") | Some("Rem") => {
                                if ty.is_primitive() && ty.kind.expect_primitive().is_numeric() {
                                    println!("  -> Satisfied built-in {} for numeric primitive {}", trait_name.unwrap(), display_type(&ty));
                                    built_in_satisfied = true;
                                    progress = true;
                                }
                            }
                            // TODO: Add cases for other built-ins like Sized, Add, etc.
                            _ => {}
                        }

                        if built_in_satisfied {
                            // Built-in trait was satisfied, don't rollback or keep constraint
                            // Rollback is still needed because the *direct* impl wasn't found
                            self.rollback_to(snapshot);
                            continue; // Go to the next constraint
                        }

                        // If not a recognized built-in or satisfied by a direct impl,
                        // and resolve_trait_implementation already checked supertraits,
                        // then keep the constraint *only if* the type contains inference vars.
                        // Otherwise, it's an error.
                        if !ty.free_vars().is_empty() {
                            // Type still has inference variables, constraint might become solvable later.
                            println!("Constraint pending (type has vars): {} implements {}", display_type(&ty), trait_repo.get_trait(required_trait_ref.trait_id).map_or("???", |t|&t.name));
                            remaining_constraints.push((ty.clone(), required_trait_ref.clone()));
                            self.rollback_to(snapshot); // Rollback the failed attempt
                        } else {
                            // Type is concrete, no direct impl, no supertrait impl, no built-in impl.
                            println!("Constraint failed (concrete type): {} implements {}", display_type(&ty), trait_repo.get_trait(required_trait_ref.trait_id).map_or("???", |t|&t.name));
                            errors.push(TypeError::RequiredTraitNotImplemented {
                                ty: display_type(&ty),
                                trait_name: trait_repo.get_trait(required_trait_ref.trait_id).map_or(format!("TraitId({})", required_trait_ref.trait_id.0), |t| t.name.clone()),
                                span: required_trait_ref.span,
                            });
                             // Rollback the failed attempt before adding error
                            self.rollback_to(snapshot);
                            progress = true; // Error occurred, stop looping
                        }
                    }
                    Err(e) => {
                        // Resolution failed for this constraint.
                        println!("Constraint failed: {} implements {}: {}", display_type(&ty), trait_repo.get_trait(required_trait_ref.trait_id).map_or("???", |t|&t.name), e);
                        errors.push(e);
                        // Let's remove it, as it cannot be satisfied.
                        self.rollback_to(snapshot); // Rollback unification attempt
                        progress = true; // Error occurred, consider it progress to stop looping potentially
                    }
                }
            }

            // Put back the constraints that weren't solved in this iteration.
            self.constraints = remaining_constraints;
            
            // If errors occurred, stop solving and return the first error.
            if !errors.is_empty() {
                return Err(errors.remove(0));
            }
        }

        // After the loop, if constraints remain, they couldn't be solved.
        if !self.constraints.is_empty() {
            let (ty, tr) = &self.constraints[0];
             println!("Unsolved constraints remain: e.g., {} implements {}", display_type(ty), trait_repo.get_trait(tr.trait_id).map_or("???", |t|&t.name));
            return Err(TypeError::RequiredTraitNotImplemented {
                 ty: display_type(ty),
                 trait_name: trait_repo.get_trait(tr.trait_id).map_or(format!("TraitId({})", tr.trait_id.0), |t| t.name.clone()),
                 span: tr.span,
             });
        }

        // --- Defaulting Step ---
        println!("  [Solve Defaulting] Subst BEFORE defaulting: {}", self.subst);
        let subst_before_defaulting = self.subst.clone();
        let mut needs_update = false;
        let mut updated_subst_map = subst_before_defaulting.map.clone();

        // Iterate over variables marked as needing defaulting
        for (&var_id, &default_kind) in self.defaulting_needed.iter() {
            // Resolve the variable using the substitution *before* this defaulting phase
            let resolved_ty = subst_before_defaulting.apply_to_ty(&Ty::new(TyKind::Var(var_id)));

            // Check if the variable is still essentially unresolved
            // (i.e., resolves to itself or another variable/infer type)
            let is_unresolved = match resolved_ty.kind {
                TyKind::Var(_) => true, 
                TyKind::InferInt(_) => true,
                TyKind::InferFloat(_) => true,
                _ => false, // Resolved to a concrete type
            };

            if is_unresolved {
                // Apply the required default
                let default_ty = match default_kind {
                    DefaultKind::Integer => Ty::new(TyKind::Primitive(PrimitiveType::I32)),
                    DefaultKind::Float => Ty::new(TyKind::Primitive(PrimitiveType::F64)),
                };
                println!("      [Defaulting] Applying default for Var {:?} ({:?}): {}", var_id, default_kind, display_type(&default_ty));
                updated_subst_map.insert(var_id, default_ty);
                needs_update = true;
            } else {
                println!("      [Defaulting] Var {:?} needed {:?} default, but already resolved to: {}", var_id, default_kind, display_type(&resolved_ty));
            }
        }

        if needs_update {
            println!("  [Solve Defaulting] Subst UPDATED after defaulting: {:?}", updated_subst_map);
            self.subst = Substitution { map: updated_subst_map };
        }
        println!("  [Solve Defaulting] Subst AFTER defaulting: {}", self.subst);
        // --- END Defaulting Step ---

        // --- Final Resolution Step ---
        let mut fully_resolved_subst = Substitution::new();
        let subst_to_resolve = self.subst.clone(); // Clone the defaulted subst

        println!("  [Solve FinalResolution] Starting final resolution. Initial subst: {}", subst_to_resolve);

        // Iterate over ALL potential variable IDs up to the next generated ID
        for i in 0..self.next_var_id {
            let var_id_to_check = TypeId(i);
            // Resolve this variable using the substitution *after* defaulting
            let resolved_ty = subst_to_resolve.apply_to_ty(&Ty::new(TyKind::Var(var_id_to_check)));
            // Only insert if the resolved type is different from the original variable
            // or if the original variable was explicitly mapped (though apply_to_ty handles cycles)
            if resolved_ty.kind != TyKind::Var(var_id_to_check) || subst_to_resolve.map.contains_key(&var_id_to_check) {
                 println!("    [FinalResolution] Var {:?} -> Resolved Ty {}", var_id_to_check, display_type(&resolved_ty));
                 fully_resolved_subst.insert(var_id_to_check, resolved_ty);
            }
        }

        println!("  [Solve FinalResolution] Final resolved subst: {}", fully_resolved_subst);
        Ok(fully_resolved_subst) // Return the fully resolved substitution
    }

    /// Creates a snapshot of the current inference state.
    ///
    /// Preconditions: None.
    /// Postconditions: Returns an `InferenceSnapshot` capturing the current state.
    pub fn snapshot(&self) -> InferenceSnapshot {
        InferenceSnapshot {
            subst: self.subst.clone(),
            next_var_id: self.next_var_id,
            constraints_len: self.constraints.len(),
            defaulting_needed: self.defaulting_needed.clone(), // Clone the map
        }
    }

    /// Restores the inference context to a previous snapshot.
    ///
    /// Preconditions: `snapshot` is a valid snapshot created earlier.
    /// Postconditions: `self` is reset to the state captured in `snapshot`.
    pub fn rollback_to(&mut self, snapshot: InferenceSnapshot) {
        self.subst = snapshot.subst;
        self.next_var_id = snapshot.next_var_id;
        // Truncate constraints vector back to its size at the time of the snapshot.
        self.constraints.truncate(snapshot.constraints_len);
        self.defaulting_needed = snapshot.defaulting_needed; // Restore the map
    }

    /// Applies the current substitution to a type.
    /// This is a public interface to access subst.apply_to_ty.
    ///
    /// Preconditions: `ty` is the type to apply substitution to.
    /// Postconditions: Returns a new type with all type variables substituted according to the current substitution.
    pub fn apply_substitution(&self, ty: &Ty) -> Ty {
        self.subst.apply_to_ty(ty)
    }
}

// Helper needed by solve() -> needs to be defined in trait_repo.rs or here
// Moved from trait_repo.rs to avoid circular dependency if trait_repo needs inference
/// Performs substitution on TraitRef arguments.
pub(crate) fn substitute_trait_ref(trait_ref: &TraitRef, subst: &Substitution) -> TraitRef {
    TraitRef {
        trait_id: trait_ref.trait_id,
        type_arguments: trait_ref.type_arguments.iter()
            .map(|ty| ty.apply_subst(subst))
            .collect(),
        span: trait_ref.span, // Keep original span
    }
}

// --- Display Implementation for Debugging ---

impl fmt::Display for Substitution {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut entries: Vec<String> = self.map.iter()
            .map(|(id, ty)| format!("{}: {}", id, display_type(ty)))
            .collect();
        entries.sort(); // Sort for consistent output
        write!(f, "{{{}}}", entries.join(", "))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{Ty, TyKind, PrimitiveType};
    use crate::context::inference::TypeId;
    use std::sync::Arc;

    // Helper types
    fn i32_ty() -> Ty { Ty::new(TyKind::Primitive(PrimitiveType::I32)) }
    fn bool_ty() -> Ty { Ty::new(TyKind::Primitive(PrimitiveType::Bool)) }
    fn var_ty(id: u32) -> Ty { Ty::new(TyKind::Var(TypeId(id))) }
    fn tuple_ty(tys: Vec<Ty>) -> Ty { Ty::new(TyKind::Tuple(tys)) }

    // --- Substitution Tests ---
    #[test]
    fn subst_new_empty() {
        let subst = Substitution::new();
        assert!(subst.is_empty());
    }

    #[test]
    fn subst_insert_and_get() {
        let mut subst = Substitution::new();
        subst.insert(TypeId(0), i32_ty());
        assert_eq!(subst.get(&TypeId(0)), Some(&i32_ty()));
        assert!(subst.get(&TypeId(1)).is_none());
    }

    #[test]
    fn subst_get_transitive() {
        let mut subst = Substitution::new();
        subst.insert(TypeId(0), var_ty(1));
        subst.insert(TypeId(1), bool_ty());
        assert_eq!(subst.get(&TypeId(0)), Some(&bool_ty()));
    }

    #[test]
    fn subst_domain() {
        let mut subst = Substitution::new();
        subst.insert(TypeId(0), i32_ty());
        subst.insert(TypeId(2), bool_ty());
        let domain = subst.domain();
        assert!(domain.contains(&TypeId(0)));
        assert!(domain.contains(&TypeId(2)));
        assert!(!domain.contains(&TypeId(1)));
        assert_eq!(domain.len(), 2);
    }

    #[test]
    fn subst_apply_to_ty() {
        let mut subst = Substitution::new();
        subst.insert(TypeId(0), i32_ty());
        let ty_t0 = var_ty(0);
        let result = subst.apply_to_ty(&ty_t0);
        assert_eq!(result, i32_ty());

        let ty_bool = bool_ty();
        let result2 = subst.apply_to_ty(&ty_bool);
        assert_eq!(result2, ty_bool);
    }

    // Note: Skipping `compose` test due to TraitRepository dependency

    // --- InferenceContext Tests ---
    #[test]
    fn infctx_fresh_var() {
        let mut ctx = InferenceContext::new();
        let v0 = ctx.fresh_var();
        let v1 = ctx.fresh_var();
        assert_eq!(v0.kind, TyKind::Var(TypeId(0)));
        assert_eq!(v1.kind, TyKind::Var(TypeId(1)));
    }

    #[test]
    fn infctx_unify_same_type() {
        let mut ctx = InferenceContext::new();
        let dummy_span = SourceSpan::from(0..0);
        assert!(ctx.unify(&i32_ty(), &i32_ty(), dummy_span).is_ok());
        assert!(ctx.subst.is_empty()); // No substitution needed
    }

    #[test]
    fn infctx_unify_var_with_concrete() {
        let mut ctx = InferenceContext::new();
        let dummy_span = SourceSpan::from(0..0);
        let t0 = ctx.fresh_var(); // t0
        assert!(ctx.unify(&t0, &i32_ty(), dummy_span).is_ok());
        assert_eq!(ctx.subst.get(&TypeId(0)), Some(&i32_ty()));
    }

    #[test]
    fn infctx_unify_concrete_with_var() {
        let mut ctx = InferenceContext::new();
        let dummy_span = SourceSpan::from(0..0);
        let t0 = ctx.fresh_var(); // t0
        assert!(ctx.unify(&bool_ty(), &t0, dummy_span).is_ok());
        assert_eq!(ctx.subst.get(&TypeId(0)), Some(&bool_ty()));
    }

    #[test]
    fn infctx_unify_two_vars() {
        let mut ctx = InferenceContext::new();
        let dummy_span = SourceSpan::from(0..0);
        let t0 = ctx.fresh_var(); // t0
        let t1 = ctx.fresh_var(); // t1
        assert!(ctx.unify(&t0, &t1, dummy_span).is_ok());
        // One variable should map to the other, e.g., t0 -> t1
        let t0_subst = ctx.subst.get(&TypeId(0));
        assert!(matches!(t0_subst, Some(ty) if ty.kind == TyKind::Var(TypeId(1))));
    }

    #[test]
    fn infctx_unify_recursive() {
        let mut ctx = InferenceContext::new();
        let dummy_span = SourceSpan::from(0..0);
        let t0 = ctx.fresh_var(); // t0
        let t1 = ctx.fresh_var(); // t1
        let tuple = tuple_ty(vec![t0.clone(), bool_ty()]); // (t0, bool)
        // Unify t1 with (t0, bool)
        assert!(ctx.unify(&t1, &tuple, dummy_span).is_ok());
        // Unify t0 with i32
        assert!(ctx.unify(&t0, &i32_ty(), dummy_span).is_ok());
        // Now t1 should resolve to (i32, bool)
        let resolved_t1 = ctx.apply_substitution(&var_ty(1));
        assert_eq!(resolved_t1, tuple_ty(vec![i32_ty(), bool_ty()]));
    }

    #[test]
    fn infctx_unify_mismatch() {
        let mut ctx = InferenceContext::new();
        let dummy_span = SourceSpan::from(0..0);
        let result = ctx.unify(&i32_ty(), &bool_ty(), dummy_span);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), TypeError::TypeMismatch { .. }));
    }

    #[test]
    fn infctx_unify_occurs_check() {
        let mut ctx = InferenceContext::new();
        let dummy_span = SourceSpan::from(0..0);
        let t0 = ctx.fresh_var(); // t0
        let tuple_with_t0 = tuple_ty(vec![t0.clone()]); // (t0)
        // Try to unify t0 = (t0)
        let result = ctx.unify(&t0, &tuple_with_t0, dummy_span);
        assert!(result.is_err());
        // Currently maps to InferenceRecursionLimit, adjust if specific OccursError added
        assert!(matches!(result.unwrap_err(), TypeError::InferenceRecursionLimit { .. }));
    }

    // Basic snapshot/rollback test
    #[test]
    fn infctx_snapshot_rollback() {
        let mut ctx = InferenceContext::new();
        let dummy_span = SourceSpan::from(0..0);
        let t0 = ctx.fresh_var();
        let t1 = ctx.fresh_var();

        let snapshot = ctx.snapshot();

        // Perform unification
        ctx.unify(&t0, &i32_ty(), dummy_span).unwrap();
        assert_eq!(ctx.subst.get(&TypeId(0)), Some(&i32_ty()));
        assert_eq!(ctx.next_var_id, 2);

        // Rollback
        ctx.rollback_to(snapshot);

        // Check state reset
        assert!(ctx.subst.is_empty());
        assert_eq!(ctx.next_var_id, 2); // next_var_id might not reset depending on intent, let's assume it doesn't for now
        assert_eq!(ctx.constraints.len(), 0);

        // Unify differently after rollback
        ctx.unify(&t1, &bool_ty(), dummy_span).unwrap();
        assert!(ctx.subst.get(&TypeId(0)).is_none());
        assert_eq!(ctx.subst.get(&TypeId(1)), Some(&bool_ty()));
    }

    // Note: Skipping `solve` tests due to TraitRepository dependency
} 