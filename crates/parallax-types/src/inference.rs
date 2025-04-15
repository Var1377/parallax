use std::collections::HashMap;
use std::sync::Arc;

use miette::SourceSpan;

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
        Ty::new(TyKind::Named { name, args })
    }
    
    /// Create a named type with a span
    pub fn named_with_span(&self, name: String, args: Vec<Ty>, span: SourceSpan) -> Ty {
        Ty::with_span(TyKind::Named { name, args }, span)
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
    
    /// Unify two types, updating the substitution
    /// Returns the substitution delta applied.
    pub fn unify(&mut self, ty1: &Ty, ty2: &Ty) -> TypeResult<Substitution> {
        self.unify_with_depth(ty1, ty2, 0)
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

            // --- Handle unification involving IntegerLiteral or FloatLiteral FIRST ---
            (TyKind::Primitive(PrimitiveType::IntegerLiteral), TyKind::Primitive(p)) |
            (TyKind::Primitive(p), TyKind::Primitive(PrimitiveType::IntegerLiteral)) => {
                 if p.is_integer() {
                     // Allow IntegerLiteral to unify with any concrete integer type.
                     // No substitution needed here; the concrete type wins.
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
                 if matches!(p, PrimitiveType::F32 | PrimitiveType::F64) {
                     // Allow FloatLiteral to unify with F32 or F64.
                     Ok(Substitution::new())
                 } else {
                     Err(TypeError::TypeMismatch {
                         expected: "f32 or f64".to_string(),
                         found: format!("{:?}", p),
                          span: if matches!(ty1.kind, TyKind::Primitive(PrimitiveType::FloatLiteral)) { ty2.span } else { ty1.span }.unwrap_or(SourceSpan::from((0,0))),
                     })
                 }
            }
            // IntegerLiteral can unify with FloatLiteral (resulting type becomes float)
            /* // REMOVED UNREACHABLE PATTERN
            (TyKind::Primitive(PrimitiveType::IntegerLiteral), TyKind::Primitive(PrimitiveType::FloatLiteral)) |
            (TyKind::Primitive(PrimitiveType::FloatLiteral), TyKind::Primitive(PrimitiveType::IntegerLiteral)) => {
                Ok(Substitution::new()) // Allow unification, context determines concrete float type
            }
            */
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
            (TyKind::Named { name: name1, args: args1 }, TyKind::Named { name: name2, args: args2 }) => {
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
                TyKind::Named { name, args } => {
                    let new_args = args.iter().map(|arg| substitute_self(arg, concrete_self)).collect();
                    Ty { kind: TyKind::Named { name: name.clone(), args: new_args }, span: ty.span }
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
        let mut result = self.clone();
        for (id, ty) in &self.mappings {
            // Ensure Ty::apply_subst exists and accepts &Substitution
            result.mappings.insert(*id, ty.apply_subst(other)); 
        }
        for (id, ty) in &other.mappings {
            result.mappings.entry(*id).or_insert_with(|| ty.clone());
        }
        result
    }
} 