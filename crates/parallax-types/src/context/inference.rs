use std::collections::HashMap;
use std::sync::Arc;

use miette::SourceSpan;

// TODO: Update these imports after moving types.rs
use crate::error::{TypeError, TypeResult};
use crate::types::{Ty, TyKind, TypeId, PrimitiveType};

/// Represents a snapshot of the inference context state for rollback.
#[derive(Debug, Clone)]
pub struct InferenceSnapshot {
    next_var_id: u32,
    substitution: Substitution,
    // We might need to snapshot other things later, like constraints
}

/// A context for type inference
#[derive(Debug, Clone)]
pub struct InferenceContext {
    /// Counter for generating unique type variable IDs
    next_var_id: u32,
    /// Current substitution for type variables
    substitution: Substitution,
    /// Maximum recursion depth for unification
    max_recursion_depth: u32,
}

impl InferenceContext {
    /// Create a new inference context
    pub fn new() -> Self {
        Self {
            next_var_id: 0,
            substitution: Substitution::default(),
            max_recursion_depth: 100, // Default recursion limit
        }
    }
    
    /// Create a snapshot of the current inference state.
    pub fn snapshot(&self) -> InferenceSnapshot {
        InferenceSnapshot {
            next_var_id: self.next_var_id,
            substitution: self.substitution.clone(),
        }
    }

    /// Rollback the inference context to a previous state.
    pub fn rollback(&mut self, snapshot: InferenceSnapshot) {
        self.next_var_id = snapshot.next_var_id;
        self.substitution = snapshot.substitution;
    }
    
    /// Create a fresh type variable
    pub fn fresh_var(&mut self) -> Ty {
        let id = TypeId(self.next_var_id);
        self.next_var_id += 1;
        Ty::new(TyKind::Var(id))
    }
    
    /// Create a fresh type variable with a span
    pub fn fresh_var_with_span(&mut self, span: SourceSpan) -> Ty {
        let id = TypeId(self.next_var_id);
        self.next_var_id += 1;
        Ty::with_span(TyKind::Var(id), span)
    }
    
    /// Create a primitive type
    pub fn primitive(&self, prim: PrimitiveType) -> Ty {
        Ty::new(TyKind::Primitive(prim))
    }
    
    /// Create a primitive type with a span
    pub fn primitive_with_span(&self, prim: PrimitiveType, span: SourceSpan) -> Ty {
        Ty::with_span(TyKind::Primitive(prim), span)
    }
    
    /// Create a named type
    pub fn named(&self, name: String, args: Vec<Ty>) -> Ty {
        Ty::new(TyKind::Named { name, symbol: None, args })
    }
    
    /// Create a named type with a span
    pub fn named_with_span(&self, name: String, args: Vec<Ty>, span: SourceSpan) -> Ty {
        Ty::with_span(TyKind::Named { name, symbol: None, args }, span)
    }
    
    /// Create an array type
    pub fn array(&self, elem_ty: Ty, size: usize) -> Ty {
        Ty::new(TyKind::Array(Arc::new(elem_ty), size))
    }
    
    /// Create an array type with a span
    pub fn array_with_span(&self, elem_ty: Ty, size: usize, span: SourceSpan) -> Ty {
        Ty::with_span(TyKind::Array(Arc::new(elem_ty), size), span)
    }
    
    /// Create a tuple type
    pub fn tuple(&self, tys: Vec<Ty>) -> Ty {
        Ty::new(TyKind::Tuple(tys))
    }
    
    /// Create a tuple type with a span
    pub fn tuple_with_span(&self, tys: Vec<Ty>, span: SourceSpan) -> Ty {
        Ty::with_span(TyKind::Tuple(tys), span)
    }
    
    /// Create a function type
    pub fn function(&self, params: Vec<Ty>, ret: Ty) -> Ty {
        Ty::new(TyKind::Function(params, Arc::new(ret)))
    }
    
    /// Create a function type with a span
    pub fn function_with_span(&self, params: Vec<Ty>, ret: Ty, span: SourceSpan) -> Ty {
        Ty::with_span(TyKind::Function(params, Arc::new(ret)), span)
    }
    
    /// Apply the current substitution to a type
    pub fn resolve_type(&self, ty: &Ty) -> Ty {
        ty.apply_subst(&self.substitution)
    }
    
    /// Unify two types, returning the substitution needed to make them equal.
    pub fn unify(&mut self, ty1: &Ty, ty2: &Ty) -> TypeResult<Substitution> {
        // Max depth check removed here, handled in unify_with_depth
        
        // --- Promotion Check BEFORE resolving fully ---
        // Check if unifying a variable with a concrete type promotes a previous literal binding
        match (&ty1.kind, &ty2.kind) {
            // Case: var1 unifying with concrete Integer P2
            (TyKind::Var(id1), TyKind::Primitive(p2)) if p2.is_integer() && p2 != &PrimitiveType::IntegerLiteral => {
                if let Some(bound_ty) = self.substitution.get(id1) {
                    if matches!(bound_ty.kind, TyKind::Primitive(PrimitiveType::IntegerLiteral)) {
                        let mut subst_delta = Substitution::new();
                        subst_delta.insert(*id1, ty2.clone()); // Bind var1 to concrete P2
                        self.substitution = self.substitution.compose(&subst_delta);
                        return Ok(subst_delta);
                    }
                }
            }
            // Case: concrete Integer P1 unifying with var2
            (TyKind::Primitive(p1), TyKind::Var(id2)) if p1.is_integer() && p1 != &PrimitiveType::IntegerLiteral => {
                if let Some(bound_ty) = self.substitution.get(id2) {
                    if matches!(bound_ty.kind, TyKind::Primitive(PrimitiveType::IntegerLiteral)) {
                        let mut subst_delta = Substitution::new();
                        subst_delta.insert(*id2, ty1.clone()); // Bind var2 to concrete P1
                        self.substitution = self.substitution.compose(&subst_delta);
                        return Ok(subst_delta);
                    }
                }
            }
            // Case: var1 unifying with concrete Float P2
            (TyKind::Var(id1), TyKind::Primitive(p2)) if p2.is_float() && p2 != &PrimitiveType::FloatLiteral => {
                 if let Some(bound_ty) = self.substitution.get(id1) {
                    if matches!(bound_ty.kind, TyKind::Primitive(PrimitiveType::FloatLiteral)) {
                        let mut subst_delta = Substitution::new();
                        subst_delta.insert(*id1, ty2.clone()); // Bind var1 to concrete P2
                        self.substitution = self.substitution.compose(&subst_delta);
                        return Ok(subst_delta);
                    }
                }
            }
             // Case: concrete Float P1 unifying with var2
             (TyKind::Primitive(p1), TyKind::Var(id2)) if p1.is_float() && p1 != &PrimitiveType::FloatLiteral => {
                 if let Some(bound_ty) = self.substitution.get(id2) {
                    if matches!(bound_ty.kind, TyKind::Primitive(PrimitiveType::FloatLiteral)) {
                        let mut subst_delta = Substitution::new();
                        subst_delta.insert(*id2, ty1.clone()); // Bind var2 to concrete P1
                        self.substitution = self.substitution.compose(&subst_delta);
                        return Ok(subst_delta);
                    }
                }
            }
            _ => {} // Not a promotion case, continue to full resolution and unify_with_depth
        }

        // Resolve types *after* promotion check
        let resolved1 = self.resolve_type(ty1);
        let resolved2 = self.resolve_type(ty2);

        // --- DEBUG PRINT --- 
        println!("[Unify] Comparing resolved: ({}) vs ({})", crate::error::display_type(&resolved1), crate::error::display_type(&resolved2));

        // Proceed with unification using the resolved types (handles all other cases)
        self.unify_with_depth(&resolved1, &resolved2, 0)
    }
    
    /// Unify two types with a recursion depth counter
    /// Returns the substitution delta applied during this unification.
    fn unify_with_depth(&mut self, ty1: &Ty, ty2: &Ty, depth: u32) -> TypeResult<Substitution> {
        // Check for recursion limit
        if depth > self.max_recursion_depth {
            return Err(TypeError::InferenceRecursionLimit {
                span: ty1.span.or(ty2.span).unwrap_or_else(|| SourceSpan::from((0, 0))),
            });
        }

        // Apply current substitution to the types
        let ty1 = self.resolve_type(ty1);
        let ty2 = self.resolve_type(ty2);

        match (&ty1.kind, &ty2.kind) {
            // If either side is a type variable, bind it and return the binding
            (TyKind::Var(id1), _) => self.bind_var(*id1, &ty2),
            (_, TyKind::Var(id2)) => self.bind_var(*id2, &ty1),

            // --- Handle Never and Error types explicitly ---
            (TyKind::Never, TyKind::Never) => Ok(Substitution::new()), // Never unifies with Never
            // TODO: Should Never unify with anything else?
            // TODO: How should Error propagate? For now, let it unify with anything to avoid cascades.
            (TyKind::Error, _) | (_, TyKind::Error) => Ok(Substitution::new()),
            // --- End Never/Error Handling ---

            // --- Handle SelfType --- 
            (TyKind::SelfType, TyKind::SelfType) => Ok(Substitution::new()), // Self unifies with Self
            // Note: Unifying SelfType with a concrete type is handled by `bind_var` or by substituting Self before unify is called.

            // --- Handle unification involving IntegerLiteral or FloatLiteral FIRST ---
            (TyKind::Primitive(PrimitiveType::IntegerLiteral), TyKind::Primitive(p)) |
            (TyKind::Primitive(p), TyKind::Primitive(PrimitiveType::IntegerLiteral)) => {
                 // Allow IntegerLiteral to unify with any concrete integer type OR itself.
                 if p.is_integer() {
                     Ok(Substitution::new())
                 } else {
                     Err(TypeError::TypeMismatch {
                         expected: "any integer type".to_string(),
                         found: format!("{:?}", p),
                         span: if matches!(ty1.kind, TyKind::Primitive(PrimitiveType::IntegerLiteral)) { ty2.span } else { ty1.span }.unwrap_or(SourceSpan::from((0,0))),
                     })
                 }
             }
            (TyKind::Primitive(PrimitiveType::FloatLiteral), TyKind::Primitive(p)) |
            (TyKind::Primitive(p), TyKind::Primitive(PrimitiveType::FloatLiteral)) => {
                 // Allow FloatLiteral to unify with F32, F64, OR itself.
                 if matches!(p, PrimitiveType::F32 | PrimitiveType::F64 | PrimitiveType::FloatLiteral) {
                     Ok(Substitution::new())
                 } else {
                     Err(TypeError::TypeMismatch {
                         expected: "f32, f64, or {{float}}".to_string(),
                         found: format!("{:?}", p),
                          span: if matches!(ty1.kind, TyKind::Primitive(PrimitiveType::FloatLiteral)) { ty2.span } else { ty1.span }.unwrap_or(SourceSpan::from((0,0))),
                     })
                 }
            }
            // IntegerLiteral vs FloatLiteral case is handled below in the general primitive check
            // if we allow coercion between them. Currently `can_coerce_primitive` doesn't allow this.

            // --- End Literal Unification ---

            // General Primitive types must be the same or coercible (AFTER literal checks)
            (TyKind::Primitive(prim1), TyKind::Primitive(prim2)) => {
                if prim1 == prim2 {
                    Ok(Substitution::new()) // No change needed
                } else if can_coerce_primitive(prim1, prim2) {
                    // Coercion allowed (e.g., I32 -> I64). Unification succeeds with no binding needed.
                    // The type checker might later enforce the "wider" type based on context.
                    Ok(Substitution::new())
                } else if can_coerce_primitive(prim2, prim1) {
                    // Coercion allowed in the other direction.
                     Ok(Substitution::new())
                }
                 else {
                    Err(TypeError::TypeMismatch {
                        expected: format!("{:?}", prim1),
                        found: format!("{:?}", prim2),
                        span: ty2.span.unwrap_or_else(|| SourceSpan::from((0, 0))),
                    })
                }
            },

            // Named types must have the same name and unifiable arguments
            (TyKind::Named { name: name1, args: args1, .. }, TyKind::Named { name: name2, args: args2, .. }) => {
                if name1 != name2 {
                    return Err(TypeError::TypeMismatch {
                        expected: name1.clone(),
                        found: name2.clone(),
                        span: ty2.span.unwrap_or_else(|| SourceSpan::from((0, 0))),
                    });
                }

                if args1.len() != args2.len() {
                    return Err(TypeError::TypeMismatch {
                        expected: format!("{}<{} args>", name1, args1.len()),
                        found: format!("{}<{} args>", name2, args2.len()),
                        span: ty2.span.unwrap_or_else(|| SourceSpan::from((0, 0))),
                    });
                }

                let mut combined_subst = Substitution::new();
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    let subst_delta = self.unify_with_depth(arg1, arg2, depth + 1)?;
                    combined_subst = combined_subst.compose(&subst_delta);
                }

                Ok(combined_subst)
            },

            // Array types must have unifiable element types and the same size
            (TyKind::Array(elem_ty1, size1), TyKind::Array(elem_ty2, size2)) => {
                if size1 != size2 {
                    return Err(TypeError::TypeMismatch {
                        expected: format!("array of size {}", size1),
                        found: format!("array of size {}", size2),
                        span: ty2.span.unwrap_or_else(|| SourceSpan::from((0, 0))),
                    });
                }

                self.unify_with_depth(elem_ty1, elem_ty2, depth + 1)
            },

            // Tuple types must have the same length and unifiable elements
            (TyKind::Tuple(tys1), TyKind::Tuple(tys2)) => {
                if tys1.len() != tys2.len() {
                    return Err(TypeError::TypeMismatch {
                        expected: format!("tuple with {} elements", tys1.len()),
                        found: format!("tuple with {} elements", tys2.len()),
                        span: ty2.span.unwrap_or_else(|| SourceSpan::from((0, 0))),
                    });
                }

                let mut combined_subst = Substitution::new();
                for (ty1, ty2) in tys1.iter().zip(tys2.iter()) {
                    let subst_delta = self.unify_with_depth(ty1, ty2, depth + 1)?;
                    combined_subst = combined_subst.compose(&subst_delta);
                }

                Ok(combined_subst)
            },

            // Function types must have unifiable parameter and return types
            (TyKind::Function(params1, ret1), TyKind::Function(params2, ret2)) => {
                if params1.len() != params2.len() {
                    return Err(TypeError::TypeMismatch {
                        expected: format!("function with {} parameters", params1.len()),
                        found: format!("function with {} parameters", params2.len()),
                        span: ty2.span.unwrap_or_else(|| SourceSpan::from((0, 0))),
                    });
                }

                let mut combined_subst = Substitution::new();
                for (param1, param2) in params1.iter().zip(params2.iter()) {
                    let subst_delta = self.unify_with_depth(param1, param2, depth + 1)?;
                    combined_subst = combined_subst.compose(&subst_delta);
                }

                let ret_subst = self.unify_with_depth(ret1, ret2, depth + 1)?;
                Ok(combined_subst.compose(&ret_subst))
            },

            // Everything else is a type mismatch
            _ => Err(TypeError::TypeMismatch {
                expected: crate::error::display_type(&ty1),
                found: crate::error::display_type(&ty2),
                span: ty2.span.unwrap_or_else(|| SourceSpan::from((0, 0))),
            }),
        }
    }
    
    /// Bind a type variable to a type
    /// Returns the substitution representing this binding.
    fn bind_var(&mut self, var_id: TypeId, ty: &Ty) -> TypeResult<Substitution> {
        // If binding to itself, do nothing
        if let TyKind::Var(id) = &ty.kind {
            if *id == var_id {
                return Ok(Substitution::new());
            }
        }
        
        // Check for infinite types
        if self.occurs_check(&var_id, ty) {
            return Err(TypeError::InternalError {
                message: format!("Infinite type detected: t{} occurs in {:?}", var_id.0, ty),
                span: ty.span,
            });
        }
        
        // Create the substitution delta for this single binding
        let mut subst = Substitution::new();
        subst.insert(var_id, ty.clone());
        
        // Apply the delta to the main context substitution
        self.substitution = self.substitution.compose(&subst);
        
        Ok(subst) // Return the delta
    }
    
    /// Check if a type variable occurs in a type
    fn occurs_check(&self, var_id: &TypeId, ty: &Ty) -> bool {
        match &ty.kind {
            TyKind::Var(id) => id == var_id,
            TyKind::Primitive(_) => false,
            TyKind::Named { args, .. } => args.iter().any(|arg| self.occurs_check(var_id, arg)),
            TyKind::Array(elem_ty, _) => self.occurs_check(var_id, elem_ty),
            TyKind::Tuple(tys) => tys.iter().any(|ty| self.occurs_check(var_id, ty)),
            TyKind::Function(params, ret) => {
                params.iter().any(|param| self.occurs_check(var_id, param)) 
                    || self.occurs_check(var_id, ret)
            }
            TyKind::Map(key_ty, value_ty) => {
                self.occurs_check(var_id, key_ty) || self.occurs_check(var_id, value_ty)
            }
            TyKind::Set(elem_ty) => self.occurs_check(var_id, elem_ty),
            TyKind::Error => false,
            TyKind::Never => false,
            TyKind::SelfType => false, // SelfType does not contain the variable unless substituted
        }
    }
    
    /// Get the final substitution
    pub fn get_substitution(&self) -> Substitution {
        self.substitution.clone()
    }

    /// Apply a substitution delta to the context's main substitution.
    pub fn apply_substitution(&mut self, subst_delta: &Substitution) {
        self.substitution = self.substitution.compose(subst_delta);
    }

    /// Applies the current substitution to a type.
    /// This is similar to `Ty::apply_subst` but uses the context's substitution.
    fn apply_substitution_to_ty(&self, ty: &Ty) -> Ty {
        match &ty.kind {
            TyKind::Var(id) => {
                if let Some(subst_ty) = self.substitution.get(id) {
                    // Recursively apply substitution to the result
                    self.apply_substitution_to_ty(subst_ty)
                } else {
                    ty.clone() // Variable not in substitution
                }
            }
            TyKind::Named { name, symbol, args } => {
                let new_args = args
                    .iter()
                    .map(|arg| self.apply_substitution_to_ty(arg))
                    .collect();
                Ty { kind: TyKind::Named { name: name.clone(), symbol: *symbol, args: new_args }, span: ty.span }
            }
            TyKind::Array(elem_ty, size) => {
                let new_elem = self.apply_substitution_to_ty(elem_ty);
                Ty::new(TyKind::Array(Arc::new(new_elem), *size))
            }
            TyKind::Tuple(tys) => {
                let new_tys = tys.iter().map(|t| self.apply_substitution_to_ty(t)).collect();
                Ty::new(TyKind::Tuple(new_tys))
            }
            TyKind::Function(params, ret) => {
                let new_params = params.iter().map(|p| self.apply_substitution_to_ty(p)).collect();
                let new_ret = self.apply_substitution_to_ty(ret);
                Ty::new(TyKind::Function(new_params, Arc::new(new_ret)))
            }
            TyKind::Map(key_ty, value_ty) => {
                let new_key = self.apply_substitution_to_ty(key_ty);
                let new_value = self.apply_substitution_to_ty(value_ty);
                Ty::new(TyKind::Map(Arc::new(new_key), Arc::new(new_value)))
            }
            TyKind::Set(elem_ty) => {
                let new_elem = self.apply_substitution_to_ty(elem_ty);
                Ty::new(TyKind::Set(Arc::new(new_elem)))
            }
            _ => ty.clone(), // No substitution needed for other types
        }
    }
}

/// Helper function to check if primitive `a` can coerce to primitive `b`.
/// This defines implicit widening rules.
fn can_coerce_primitive(a: &PrimitiveType, b: &PrimitiveType) -> bool {
    use PrimitiveType::*;
    match (a, b) {
        // Integer widening
        (I8, I16 | I32 | I64 | I128) => true,
        (I16, I32 | I64 | I128) => true,
        (I32, I64 | I128) => true,
        (I64, I128) => true,
        (U8, U16 | U32 | U64 | U128) => true,
        (U16, U32 | U64 | U128) => true,
        (U32, U64 | U128) => true,
        (U64, U128) => true,
        // Float widening
        (F32, F64) => true,
        // Integer to Float
        (I8 | I16 | I32 | I64 | I128, F32 | F64) => true,
        (U8 | U16 | U32 | U64 | U128, F32 | F64) => true,
        _ => false,
    }
}

/// A store for variable types during type checking
#[derive(Debug, Clone)]
pub struct TypeEnvironment {
    /// Map from variable names to their types
    env: HashMap<String, Ty>,
    /// Parent environment, if any
    parent: Option<Arc<TypeEnvironment>>,
}

impl TypeEnvironment {
    /// Create a new empty environment
    pub fn new() -> Self {
        Self {
            env: HashMap::new(),
            parent: None,
        }
    }
    
    /// Create a new environment with a parent
    pub fn with_parent(parent: Arc<TypeEnvironment>) -> Self {
        Self {
            env: HashMap::new(),
            parent: Some(parent),
        }
    }
    
    /// Get the type of a variable
    pub fn get(&self, name: &str) -> Option<&Ty> {
        self.env.get(name).or_else(|| {
            // If not found in current scope, look in parent
            self.parent.as_ref().and_then(|parent| parent.get(name))
        })
    }
    
    /// Add a binding to the environment
    pub fn add(&mut self, name: String, ty: Ty) {
        self.env.insert(name, ty);
    }
    
    /// Apply a substitution to all types in the environment
    pub fn apply_substitution(&self, subst: &Substitution) -> Self {
        let mut new_env = HashMap::new();
        
        // Apply substitution to all bindings
        for (name, ty) in &self.env {
            new_env.insert(name.clone(), ty.apply_subst(subst));
        }
        
        // Create a new environment with the same parent
        Self {
            env: new_env,
            parent: self.parent.clone(),
        }
    }

    /// Get a map of bindings defined directly in the current scope (excluding parents).
    #[allow(dead_code)]
    pub(crate) fn get_current_scope_bindings(&self) -> HashMap<String, Ty> {
        self.env.clone()
    }
}

// A mapping from type variables to concrete types
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Substitution {
    mappings: HashMap<TypeId, Ty>,
}

// Add impl block for Substitution in inference.rs
impl Substitution {
    // Add new method
    pub fn new() -> Self {
        Self {
            mappings: HashMap::new()
        }
    }

    // Add insert method
    pub fn insert(&mut self, id: TypeId, ty: Ty) {
        self.mappings.insert(id, ty);
    }

    // Add insert_self method that takes a reference
    pub fn insert_self(&mut self, concrete_self_ty: &Ty) {
        fn substitute_self(ty: &Ty, concrete_self: &Ty) -> Ty {
            match &ty.kind {
                TyKind::SelfType => concrete_self.clone(),
                TyKind::Var(_) | TyKind::Primitive(_) | TyKind::Error | TyKind::Never => ty.clone(),
                TyKind::Named { name, symbol, args } => {
                    let new_args = args.iter().map(|arg| substitute_self(arg, concrete_self)).collect();
                    Ty { kind: TyKind::Named { name: name.clone(), symbol: *symbol, args: new_args }, span: ty.span }
                }
                TyKind::Array(elem_ty, size) => {
                    let new_elem = substitute_self(elem_ty, concrete_self);
                    Ty { kind: TyKind::Array(Arc::new(new_elem), *size), span: ty.span }
                }
                TyKind::Tuple(tys) => {
                    let new_tys = tys.iter().map(|t| substitute_self(t, concrete_self)).collect();
                    Ty { kind: TyKind::Tuple(new_tys), span: ty.span }
                }
                TyKind::Function(params, ret) => {
                    let new_params = params.iter().map(|p| substitute_self(p, concrete_self)).collect();
                    let new_ret = substitute_self(ret, concrete_self);
                    Ty { kind: TyKind::Function(new_params, Arc::new(new_ret)), span: ty.span }
                }
                TyKind::Map(key_ty, value_ty) => {
                    let new_key = substitute_self(key_ty, concrete_self);
                    let new_value = substitute_self(value_ty, concrete_self);
                    Ty { kind: TyKind::Map(Arc::new(new_key), Arc::new(new_value)), span: ty.span }
                }
                TyKind::Set(elem_ty) => {
                    let new_elem = substitute_self(elem_ty, concrete_self);
                    Ty { kind: TyKind::Set(Arc::new(new_elem)), span: ty.span }
                }
            }
        }
        let mut new_mappings = HashMap::with_capacity(self.mappings.len());
        for (id, ty) in &self.mappings {
            new_mappings.insert(*id, substitute_self(ty, concrete_self_ty));
        }
        self.mappings = new_mappings;
    }

    // Add is_empty method
    pub fn is_empty(&self) -> bool {
        self.mappings.is_empty()
    }

    // Add get method here
    pub fn get(&self, id: &TypeId) -> Option<&Ty> {
        self.mappings.get(id)
    }

    // Add compose method here
    pub fn compose(&self, other: &Substitution) -> Substitution {
        let mut result = self.clone(); // Start with self's mappings
        // Apply other's substitutions to the types already in self
        for (id, ty) in &self.mappings {
            // Ensure Ty::apply_subst exists and accepts &Substitution
            result.mappings.insert(*id, ty.apply_subst(other)); 
        }
        // Add/Overwrite mappings from other
        for (id, ty) in &other.mappings {
             // Use insert which overwrites existing keys
             result.mappings.insert(*id, ty.clone()); 
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*; // Import items from parent module
    use crate::types::{Ty, TyKind, TypeId, PrimitiveType}; // Add types import
    use miette::SourceSpan;
    use std::sync::Arc;

    // Helper function to create a type variable
    fn ty_var(id: u32) -> Ty {
        Ty::new(TyKind::Var(TypeId(id)))
    }

    // Helper function to create a primitive type
    fn ty_prim(prim: PrimitiveType) -> Ty {
        Ty::new(TyKind::Primitive(prim))
    }

    #[test]
    fn test_fresh_var() {
        let mut ctx = InferenceContext::new();
        let var1 = ctx.fresh_var();
        let var2 = ctx.fresh_var();
        assert_ne!(var1, var2);
        if let TyKind::Var(id1) = var1.kind {
            assert_eq!(id1.0, 0);
        } else {
            panic!("Expected var1 to be a type variable");
        }
        if let TyKind::Var(id2) = var2.kind {
            assert_eq!(id2.0, 1);
        } else {
            panic!("Expected var2 to be a type variable");
        }
    }

    #[test]
    fn test_unify_simple() {
        let mut ctx = InferenceContext::new();
        let var0 = ctx.fresh_var();
        let i32_ty = ctx.primitive(PrimitiveType::I32);

        // Unify t0 with i32
        let subst = ctx.unify(&var0, &i32_ty).expect("Unification failed");
        assert!(!subst.is_empty());
        assert_eq!(subst.get(&TypeId(0)), Some(&i32_ty));

        // Check context substitution
        assert_eq!(ctx.resolve_type(&var0), i32_ty);

        // Unify i32 with i32 (should succeed, no change)
        let subst2 = ctx.unify(&i32_ty, &i32_ty).expect("Unification failed");
        assert!(subst2.is_empty());
        assert_eq!(ctx.resolve_type(&var0), i32_ty); // var0 should still resolve to i32
    }

    #[test]
    fn test_unify_vars() {
        let mut ctx = InferenceContext::new();
        let var0 = ctx.fresh_var(); // t0
        let var1 = ctx.fresh_var(); // t1
        let i32_ty = ctx.primitive(PrimitiveType::I32);

        // Unify t0 = t1
        ctx.unify(&var0, &var1).expect("t0 = t1 failed");
        assert_eq!(ctx.resolve_type(&var0).kind, TyKind::Var(TypeId(1))); // t0 resolves to t1

        // Unify t1 = i32
        ctx.unify(&var1, &i32_ty).expect("t1 = i32 failed");

        // Check resolution after transitive unification
        assert_eq!(ctx.resolve_type(&var1), i32_ty); // t1 resolves to i32
        assert_eq!(ctx.resolve_type(&var0), i32_ty); // t0 should also resolve to i32
    }

    #[test]
    fn test_unify_mismatch() {
        let mut ctx = InferenceContext::new();
        let i32_ty = ctx.primitive(PrimitiveType::I32);
        let bool_ty = ctx.primitive(PrimitiveType::Bool);

        let result = ctx.unify(&i32_ty, &bool_ty);
        assert!(result.is_err());
        // Optionally check the error type
        if let Err(TypeError::TypeMismatch { expected, found, .. }) = result {
            assert!(expected.contains("I32") || found.contains("I32"));
            assert!(expected.contains("Bool") || found.contains("Bool"));
        } else {
            panic!("Expected TypeMismatch error");
        }
    }

    #[test]
    fn test_unify_occurs_check() {
        let mut ctx = InferenceContext::new();
        let var0 = ctx.fresh_var(); // t0
        let vec_var0 = Ty::new(TyKind::Named {
            name: "Vec".to_string(),
            symbol: None,
            args: vec![var0.clone()],
        });

        // Try to unify t0 = Vec<t0>
        let result = ctx.unify(&var0, &vec_var0);
        assert!(result.is_err());
        if let Err(TypeError::InternalError { message, .. }) = result {
            assert!(message.contains("Infinite type detected"));
        } else {
            panic!("Expected InternalError (occurs check)");
        }
    }
    
    #[test]
    fn test_substitution_compose() {
        let mut ctx = InferenceContext::new();
        let var0 = ctx.fresh_var(); // t0
        let var1 = ctx.fresh_var(); // t1
        let i32_ty = ctx.primitive(PrimitiveType::I32);
        let vec_i32 = Ty::new(TyKind::Named {
            name: "Vec".to_string(),
            symbol: None,
            args: vec![i32_ty.clone()]
        });
        let vec_var0 = Ty::new(TyKind::Named {
            name: "Vec".to_string(),
            symbol: None,
            args: vec![var0.clone()]
        });

        let mut subst1 = Substitution::new();
        subst1.insert(TypeId(0), i32_ty);
        subst1.insert(TypeId(1), var0);
        
        let mut subst2 = Substitution::new();
        subst2.insert(TypeId(0), vec_i32);
        subst2.insert(TypeId(1), vec_var0);

        // Re-declare expected types for clarity and to avoid borrow issues
        let expected_ty0 = Ty::new(TyKind::Named {
            name: "Vec".to_string(),
            symbol: None,
            args: vec![Ty::new(TyKind::Primitive(PrimitiveType::I32))]
        });
        let expected_ty1 = Ty::new(TyKind::Named {
            name: "Vec".to_string(),
            symbol: None,
            args: vec![Ty::new(TyKind::Var(TypeId(0)))]
        });

        // Compose subst1 then subst2 (apply subst2 to the results of subst1)
        let composed = subst1.compose(&subst2);
        
        // Check composed substitution results
        assert_eq!(composed.get(&TypeId(0)), Some(&expected_ty0));
        assert_eq!(composed.get(&TypeId(1)), Some(&expected_ty1));
    }

    #[test]
    fn test_unify_tuple() {
        let mut ctx = InferenceContext::new();
        let var0 = ctx.fresh_var(); // t0
        let var1 = ctx.fresh_var(); // t1
        let i32_ty = ctx.primitive(PrimitiveType::I32);
        let bool_ty = ctx.primitive(PrimitiveType::Bool);

        let tuple1 = ctx.tuple(vec![var0.clone(), bool_ty.clone()]); // (t0, bool)
        let tuple2 = ctx.tuple(vec![i32_ty.clone(), var1.clone()]);  // (i32, t1)

        let subst = ctx.unify(&tuple1, &tuple2).expect("Tuple unification failed");

        // Check the delta substitution
        assert_eq!(subst.get(&TypeId(0)), Some(&i32_ty)); // t0 -> i32
        assert_eq!(subst.get(&TypeId(1)), Some(&bool_ty));  // t1 -> bool

        // Check context resolution
        assert_eq!(ctx.resolve_type(&var0), i32_ty);
        assert_eq!(ctx.resolve_type(&var1), bool_ty);
    }

    #[test]
    fn test_unify_function() {
        let mut ctx = InferenceContext::new();
        let var0 = ctx.fresh_var(); // t0
        let var1 = ctx.fresh_var(); // t1
        let i32_ty = ctx.primitive(PrimitiveType::I32);
        let bool_ty = ctx.primitive(PrimitiveType::Bool);

        let func1 = ctx.function(vec![var0.clone()], bool_ty.clone()); // fn(t0) -> bool
        let func2 = ctx.function(vec![i32_ty.clone()], var1.clone());  // fn(i32) -> t1

        let subst = ctx.unify(&func1, &func2).expect("Function unification failed");

        // Check the delta substitution
        assert_eq!(subst.get(&TypeId(0)), Some(&i32_ty)); // t0 -> i32
        assert_eq!(subst.get(&TypeId(1)), Some(&bool_ty));  // t1 -> bool

        // Check context resolution
        assert_eq!(ctx.resolve_type(&var0), i32_ty);
        assert_eq!(ctx.resolve_type(&var1), bool_ty);
    }

    #[test]
    fn test_unify_named() {
        let mut ctx = InferenceContext::new();
        let var0 = ctx.fresh_var(); // t0
        let var1 = ctx.fresh_var(); // t1
        let i32_ty = ctx.primitive(PrimitiveType::I32);
        let bool_ty = ctx.primitive(PrimitiveType::Bool);

        let named1 = ctx.named("Option".to_string(), vec![var0.clone()]); // Option<t0>
        let named2 = ctx.named("Option".to_string(), vec![i32_ty.clone()]); // Option<i32>
        let named3 = ctx.named("Result".to_string(), vec![var1.clone(), bool_ty.clone()]); // Result<t1, bool>
        let named4 = ctx.named("Option".to_string(), vec![i32_ty.clone()]); // Option<i32> (same as named2)

        // Unify Option<t0> = Option<i32>
        let subst1 = ctx.unify(&named1, &named2).expect("Named type unification 1 failed");
        assert_eq!(subst1.get(&TypeId(0)), Some(&i32_ty)); // t0 -> i32
        assert_eq!(ctx.resolve_type(&var0), i32_ty);

        // Unify Option<i32> = Option<i32> (should succeed, no change)
        let subst2 = ctx.unify(&named2, &named4).expect("Named type unification 2 failed");
        assert!(subst2.is_empty());

        // Unify Option<t0> = Result<t1, bool> (should fail due to name mismatch)
        let result3 = ctx.unify(&named1, &named3);
        assert!(result3.is_err());
        if let Err(TypeError::TypeMismatch { expected, found, .. }) = result3 {
            assert_eq!(expected, "Option");
            assert_eq!(found, "Result");
        } else {
            panic!("Expected TypeMismatch error for named types");
        }
    }

    #[test]
    fn test_unify_literal_integer() {
        let mut ctx = InferenceContext::new();
        let int_literal = ctx.primitive(PrimitiveType::IntegerLiteral);
        let float_literal = ctx.primitive(PrimitiveType::FloatLiteral);
        let i32_ty = ctx.primitive(PrimitiveType::I32);
        let u64_ty = ctx.primitive(PrimitiveType::U64);
        let f32_ty = ctx.primitive(PrimitiveType::F32);
        let bool_ty = ctx.primitive(PrimitiveType::Bool);
        let var0 = ctx.fresh_var();

        // IntegerLiteral unifies with concrete integer
        let subst1 = ctx.unify(&int_literal, &i32_ty).expect("Unify IntLit, I32 failed");
        assert!(subst1.is_empty()); // No substitution created
        let subst2 = ctx.unify(&u64_ty, &int_literal).expect("Unify U64, IntLit failed");
        assert!(subst2.is_empty());

        // IntegerLiteral does NOT unify with float or bool
        assert!(ctx.unify(&int_literal, &f32_ty).is_err());
        assert!(ctx.unify(&bool_ty, &int_literal).is_err());
        // REMOVED: IntegerLiteral vs FloatLiteral is now handled by coercion/later checks, not direct unification
        // assert!(ctx.unify(&int_literal, &float_literal).is_ok()); // IntLit can unify with FloatLit

        // Unify Var = IntLit, then Var = I32
        let subst3 = ctx.unify(&var0, &int_literal).expect("Unify t0, IntLit failed");
        assert_eq!(subst3.get(&TypeId(0)), Some(&int_literal));
        let subst4 = ctx.unify(&var0, &i32_ty).expect("Unify t0, I32 failed");
        // Unifying with a concrete type after a literal should work.
        // The resulting substitution might bind t0->i32 or be empty depending on impl.
        // Let's check the resolved type.
        assert_eq!(ctx.resolve_type(&var0), i32_ty);
    }

    #[test]
    fn test_unify_literal_float() {
        let mut ctx = InferenceContext::new();
        let float_literal = ctx.primitive(PrimitiveType::FloatLiteral);
        let f32_ty = ctx.primitive(PrimitiveType::F32);
        let f64_ty = ctx.primitive(PrimitiveType::F64);
        let i32_ty = ctx.primitive(PrimitiveType::I32);
        let var0 = ctx.fresh_var();

        // FloatLiteral unifies with concrete float
        let subst1 = ctx.unify(&float_literal, &f32_ty).expect("Unify FloatLit, F32 failed");
        assert!(subst1.is_empty());
        let subst2 = ctx.unify(&f64_ty, &float_literal).expect("Unify F64, FloatLit failed");
        assert!(subst2.is_empty());

        // FloatLiteral does NOT unify with integer (handled by coercion rules later)
        assert!(ctx.unify(&float_literal, &i32_ty).is_err());

        // Unify Var = FloatLit, then Var = F64
        let subst3 = ctx.unify(&var0, &float_literal).expect("Unify t0, FloatLit failed");
        assert_eq!(subst3.get(&TypeId(0)), Some(&float_literal));
        let subst4 = ctx.unify(&var0, &f64_ty).expect("Unify t0, F64 failed");
        assert_eq!(ctx.resolve_type(&var0), f64_ty);
    }

    #[test]
    fn test_snapshot_rollback() {
        let mut ctx = InferenceContext::new();
        let var1 = ctx.fresh_var();
        let var2 = ctx.fresh_var();
        let i32_ty = ty_prim(PrimitiveType::I32);

        let snapshot1 = ctx.snapshot();
        assert!(ctx.unify(&var1, &i32_ty).is_ok());
        assert_eq!(ctx.resolve_type(&var1), i32_ty);

        let snapshot2 = ctx.snapshot();
        assert!(ctx.unify(&var2, &var1).is_ok());
        assert_eq!(ctx.resolve_type(&var2), i32_ty);

        // Rollback to snapshot 2
        ctx.rollback(snapshot2);
        assert_eq!(ctx.resolve_type(&var1), i32_ty); // var1 still i32
        assert_ne!(ctx.resolve_type(&var2), i32_ty); // var2 is var again
        assert!(matches!(ctx.resolve_type(&var2).kind, TyKind::Var(_)));

        // Rollback to snapshot 1
        ctx.rollback(snapshot1);
        assert_ne!(ctx.resolve_type(&var1), i32_ty); // var1 is var again
        assert_ne!(ctx.resolve_type(&var2), i32_ty); // var2 is var again
        assert!(matches!(ctx.resolve_type(&var1).kind, TyKind::Var(_)));
        assert!(matches!(ctx.resolve_type(&var2).kind, TyKind::Var(_)));
        assert_ne!(ctx.resolve_type(&var1), ctx.resolve_type(&var2)); // They are different vars
    }

    // --- TypeEnvironment Tests ---
    #[test]
    fn test_type_environment_basic() {
        let mut env = TypeEnvironment::new();
        let i32_ty = ty_prim(PrimitiveType::I32);
        let bool_ty = ty_prim(PrimitiveType::Bool);

        assert!(env.get("x").is_none());

        env.add("x".to_string(), i32_ty.clone());
        assert_eq!(env.get("x"), Some(&i32_ty));

        env.add("y".to_string(), bool_ty.clone());
        assert_eq!(env.get("x"), Some(&i32_ty));
        assert_eq!(env.get("y"), Some(&bool_ty));

        // Shadowing
        env.add("x".to_string(), bool_ty.clone());
        assert_eq!(env.get("x"), Some(&bool_ty));
    }

    #[test]
    fn test_type_environment_parent() {
        let mut parent_env = TypeEnvironment::new();
        let i32_ty = ty_prim(PrimitiveType::I32);
        let bool_ty = ty_prim(PrimitiveType::Bool);
        parent_env.add("x".to_string(), i32_ty.clone());
        parent_env.add("global".to_string(), ty_prim(PrimitiveType::String));
        let parent_arc = Arc::new(parent_env);

        let mut child_env = TypeEnvironment::with_parent(parent_arc);
        child_env.add("y".to_string(), bool_ty.clone());
        child_env.add("x".to_string(), ty_prim(PrimitiveType::F32)); // Shadow parent

        // Access child
        assert_eq!(child_env.get("y"), Some(&bool_ty));
        // Access shadowed parent
        assert_eq!(child_env.get("x"), Some(&ty_prim(PrimitiveType::F32)));
        // Access unshadowed parent
        assert_eq!(child_env.get("global"), Some(&ty_prim(PrimitiveType::String)));
        // Access non-existent
        assert!(child_env.get("z").is_none());
    }

    #[test]
    fn test_type_environment_apply_substitution() {
        let mut env = TypeEnvironment::new();
        let var0 = ty_var(0);
        let var1 = ty_var(1);
        let i32_ty = ty_prim(PrimitiveType::I32);

        env.add("a".to_string(), var0.clone());
        env.add("b".to_string(), var1.clone());
        env.add("c".to_string(), ty_prim(PrimitiveType::Bool)); // Concrete type

        let mut subst = Substitution::new();
        subst.insert(TypeId(0), i32_ty.clone());

        let new_env = env.apply_substitution(&subst);

        assert_eq!(new_env.get("a"), Some(&i32_ty));
        assert_eq!(new_env.get("b"), Some(&var1)); // Unchanged var
        assert_eq!(new_env.get("c"), Some(&ty_prim(PrimitiveType::Bool))); // Unchanged concrete
    }

     #[test]
     fn test_type_environment_get_bindings() {
         let mut env = TypeEnvironment::new();
         let i32_ty = ty_prim(PrimitiveType::I32);
         let bool_ty = ty_prim(PrimitiveType::Bool);
         env.add("x".to_string(), i32_ty.clone());
         env.add("y".to_string(), bool_ty.clone());

         let bindings = env.get_current_scope_bindings();
         assert_eq!(bindings.len(), 2);
         assert_eq!(bindings.get("x"), Some(&i32_ty));
         assert_eq!(bindings.get("y"), Some(&bool_ty));

         // Test with parent - should only get current scope
         let parent_arc = Arc::new(env);
         let mut child_env = TypeEnvironment::with_parent(parent_arc);
         child_env.add("z".to_string(), ty_prim(PrimitiveType::Char));
         let child_bindings = child_env.get_current_scope_bindings();
         assert_eq!(child_bindings.len(), 1);
         assert_eq!(child_bindings.get("z"), Some(&ty_prim(PrimitiveType::Char)));
         assert!(child_bindings.get("x").is_none()); 
     }

    // --- Substitution Tests ---
    // Tests for compose are already present

    // Helper for SelfType
    fn ty_self() -> Ty {
        Ty::new(TyKind::SelfType)
    }

    #[test]
    fn test_substitution_insert_self() {
        let mut subst = Substitution::new();
        let i32_ty = ty_prim(PrimitiveType::I32);
        let bool_ty = ty_prim(PrimitiveType::Bool);
        let self_ty = ty_self();
        let vec_self = Ty::new(TyKind::Named { name: "Vec".to_string(), symbol: None, args: vec![self_ty.clone()] });

        // Insert Self -> i32
        subst.insert_self(&i32_ty);

        // Check substitution of Self itself
        assert_eq!(self_ty.apply_subst(&subst), i32_ty);
        // Check substitution within a structure
        let expected_vec_i32 = Ty::new(TyKind::Named { name: "Vec".to_string(), symbol: None, args: vec![i32_ty.clone()] });
        assert_eq!(vec_self.apply_subst(&subst), expected_vec_i32);
        // Check substitution of a non-Self type (should be no-op)
        assert_eq!(bool_ty.apply_subst(&subst), bool_ty);

        // Test inserting Self -> Bool
        let mut subst2 = Substitution::new();
        subst2.insert_self(&bool_ty);
        assert_eq!(self_ty.apply_subst(&subst2), bool_ty);
        let expected_vec_bool = Ty::new(TyKind::Named { name: "Vec".to_string(), symbol: None, args: vec![bool_ty.clone()] });
        assert_eq!(vec_self.apply_subst(&subst2), expected_vec_bool);
    }

    // --- More Unification Tests ---

    // Helper for Map/Set
    fn ty_map(key: Ty, value: Ty) -> Ty {
        Ty::new(TyKind::Map(Arc::new(key), Arc::new(value)))
    }
    fn ty_set(elem: Ty) -> Ty {
        Ty::new(TyKind::Set(Arc::new(elem)))
    }

    #[test]
    fn test_unify_map() {
        let mut ctx = InferenceContext::new();
        let map1 = ty_map(ty_prim(PrimitiveType::String), ty_prim(PrimitiveType::I32));
        let map2 = ty_map(ty_prim(PrimitiveType::String), ty_prim(PrimitiveType::I32));
        let map3 = ty_map(ty_prim(PrimitiveType::String), ty_prim(PrimitiveType::Bool));
        let map4 = ty_map(ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32));
        let var_map = ty_map(ctx.fresh_var(), ctx.fresh_var());
        let var_key = ty_map(ctx.fresh_var(), ty_prim(PrimitiveType::I32));
        let var_val = ty_map(ty_prim(PrimitiveType::String), ctx.fresh_var());

        // Exact match
        assert!(ctx.unify(&map1, &map2).is_ok());
        // Value mismatch
        assert!(ctx.unify(&map1, &map3).is_err());
        // Key mismatch
        assert!(ctx.unify(&map1, &map4).is_err());
        // Unify concrete with vars
        assert!(ctx.unify(&map1, &var_map).is_ok());
        assert_eq!(ctx.resolve_type(&var_map), map1);
        // Unify concrete with partial vars
        assert!(ctx.unify(&map1, &var_key).is_ok());
        assert_eq!(ctx.resolve_type(&var_key), map1);
        assert!(ctx.unify(&map1, &var_val).is_ok());
        assert_eq!(ctx.resolve_type(&var_val), map1);
    }

    #[test]
    fn test_unify_set() {
        let mut ctx = InferenceContext::new();
        let set1 = ty_set(ty_prim(PrimitiveType::F64));
        let set2 = ty_set(ty_prim(PrimitiveType::F64));
        let set3 = ty_set(ty_prim(PrimitiveType::Char));
        let var_set = ty_set(ctx.fresh_var());

        // Exact match
        assert!(ctx.unify(&set1, &set2).is_ok());
        // Element mismatch
        assert!(ctx.unify(&set1, &set3).is_err());
        // Unify concrete with var
        assert!(ctx.unify(&set1, &var_set).is_ok());
        assert_eq!(ctx.resolve_type(&var_set), set1);
    }

    #[test]
    fn test_unify_occurs_check_composite() {
        let mut ctx = InferenceContext::new();
        let var = ctx.fresh_var(); // t0
        // Type: (t0, i32)
        let tuple_with_var = Ty::new(TyKind::Tuple(vec![var.clone(), ty_prim(PrimitiveType::I32)]));

        // Try to unify: t0 = (t0, i32) -> should fail occurs check
        let result = ctx.unify(&var, &tuple_with_var);
        assert!(result.is_err());
        assert!(matches!(result.err().unwrap(), TypeError::InternalError { message, .. } if message.contains("occurs check failed")));
    }

     #[test]
     fn test_unify_self_type() {
        let mut ctx = InferenceContext::new();
        let self1 = ty_self();
        let self2 = ty_self();
        let i32_ty = ty_prim(PrimitiveType::I32);
        let var = ctx.fresh_var();

        // Self == Self
        assert!(ctx.unify(&self1, &self2).is_ok());

        // Self == i32 (bind_var handles this)
        assert!(ctx.unify(&self1, &i32_ty).is_err()); // Should not unify directly, needs substitution context

        // Self == var (bind_var handles this)
        assert!(ctx.unify(&self1, &var).is_ok());
        // After unification, resolving the var should yield SelfType
        assert_eq!(ctx.resolve_type(&var).kind, TyKind::SelfType);

        // var == Self
        let var2 = ctx.fresh_var();
        assert!(ctx.unify(&var2, &self1).is_ok());
        assert_eq!(ctx.resolve_type(&var2).kind, TyKind::SelfType);
    }
}