// src/types/core.rs
use parallax_resolve::{types::Symbol}; // Import Symbol directly
use std::collections::HashSet;
use std::fmt;
use std::sync::Arc;
use miette::SourceSpan;
use serde::{Deserialize, Serialize};

use crate::{context::inference::{Substitution, TypeId}, error::display_type}; // Import TypeId

/// A sentinel TypeId used to represent the 'Self' type keyword placeholder
/// before substitution in impl blocks or trait method resolution.
/// It should *never* appear in a final, fully resolved type.
pub const SELF_TYPE_ID: TypeId = TypeId(u32::MAX); // Use MAX as a sentinel

impl fmt::Display for TypeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "t{}", self.0)
    }
}

/// A type in the Parallax language after type checking.
/// This structure represents the resolved type information.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ty {
    /// The kind of type (e.g., primitive, named, variable, function).
    pub kind: TyKind,
    /// Optional source span pointing to the original type annotation or expression
    /// from which this type was derived.
    pub span: Option<SourceSpan>,
}

impl Ty {
    /// Create a new type with no source span.
    /// Used internally when span information is not readily available.
    ///
    /// Preconditions: `kind` is a valid `TyKind`.
    /// Postconditions: Returns a `Ty` with the given `kind` and `span = None`.
    pub fn new(kind: TyKind) -> Self {
        Ty { kind, span: None }
    }

    /// Create a new type with the given source span.
    ///
    /// Preconditions: `kind` is valid, `span` corresponds to the source location.
    /// Postconditions: Returns a `Ty` with the given `kind` and `span`.
    pub fn with_span(kind: TyKind, span: SourceSpan) -> Self {
        Ty { kind, span: Some(span) }
    }

    /// Returns `true` if this type is a type variable (`TyKind::Var`).
    ///
    /// Preconditions: None.
    /// Postconditions: Returns `true` iff `self.kind` is `TyKind::Var`.
    pub fn is_var(&self) -> bool {
        matches!(self.kind, TyKind::Var(_))
    }

    /// Returns `true` if this type contains no inference variables (`TyKind::Var`)
    /// and no `TyKind::SelfType` placeholders.
    /// Note: Does not guarantee that named types (structs/enums) are fully defined,
    /// only that the type *structure* itself is concrete.
    ///
    /// Preconditions: None.
    /// Postconditions: Returns `true` if `free_vars()` is empty and `contains_self_type()` is false.
    pub fn is_concrete(&self) -> bool {
        // A concrete type has no free inference variables and does not contain SelfType.
        self.free_vars().is_empty() && !self.contains_self_type()
    }

    /// Applies a substitution recursively to produce a new type.
    /// Resolves variables and inference literals.
    ///
    /// Preconditions: `subst` is the substitution map to apply.
    /// Postconditions: Returns a new `Ty` with substitutions applied.
    pub fn apply_subst(&self, subst: &Substitution) -> Ty {
        // <<< DEBUG PRINT >>>
        println!("  [apply_subst] Applying to: {} with Subst: {}", display_type(self), subst);

        // --- Step 1: Resolve the top-level type if it's a variable OR an inference literal ---
        let mut resolved_ty = self.clone(); // Start with a clone

        // Keep resolving as long as we have a variable/infer type and a substitution for it
        loop {
             match resolved_ty.kind {
                TyKind::Var(id) => {
                    if let Some(replacement) = subst.get(&id) {
                        resolved_ty = replacement.clone();
                        if resolved_ty.span.is_none() { resolved_ty.span = self.span; }
                        // Continue loop in case replacement is another Var/Infer type
                    } else {
                        break; // Variable not found, stop resolving
                    }
                }
                TyKind::InferInt(id) => {
                    // <<< DEBUG PRINT >>>
                    println!("    [apply_subst Step 1] Trying to resolve InferInt({:?})", id);
                    if let Some(replacement) = subst.get(&id) {
                         // <<< DEBUG PRINT >>>
                        println!("      -> Found substitution: {}", display_type(replacement));
                        resolved_ty = replacement.clone();
                        if resolved_ty.span.is_none() { resolved_ty.span = self.span; }
                         // Continue loop
                    } else {
                         // <<< DEBUG PRINT >>>
                        println!("      -> No substitution found for InferInt({:?})", id);
                        // Should ideally be defaulted already, but return self if not found
                        break;
                    }
                }
                TyKind::InferFloat(id) => {
                     // <<< DEBUG PRINT >>>
                    println!("    [apply_subst Step 1] Trying to resolve InferFloat({:?})", id);
                    if let Some(replacement) = subst.get(&id) {
                        // <<< DEBUG PRINT >>>
                        println!("      -> Found substitution: {}", display_type(replacement));
                        resolved_ty = replacement.clone();
                        if resolved_ty.span.is_none() { resolved_ty.span = self.span; }
                         // Continue loop
                    } else {
                         // <<< DEBUG PRINT >>>
                        println!("      -> No substitution found for InferFloat({:?})", id);
                         // Should ideally be defaulted already, but return self if not found
                        break;
                    }
                }
                _ => {
                    // Not a resolvable type at the top level, break the loop
                    break;
                }
            }
        }
        // At this point, resolved_ty holds the most resolved version based on top-level variables/infer types.
        // <<< DEBUG PRINT >>>
        println!("    [apply_subst Step 1 Result] Resolved top-level to: {}", display_type(&resolved_ty));

        // --- Step 2: Recurse into the components of the resolved type ---
        let new_kind = match &resolved_ty.kind {
            // Var should be resolved by the loop above, return clone defensively.
            // InferInt/InferFloat are also handled by the loop above. If they persist,
            // they'll be handled by the default arm below.
            TyKind::Var(_) => resolved_ty.kind.clone(),

            // Recursive cases: Apply substitution to inner types
            TyKind::Named { name, symbol, args } => TyKind::Named {
                name: name.clone(),
                symbol: *symbol,
                args: args.iter().map(|arg| arg.apply_subst(subst)).collect(),
            },
            TyKind::Function(params, ret) => TyKind::Function(
                params.iter().map(|p| p.apply_subst(subst)).collect(),
                ret.apply_subst(subst).into(),
            ),
            TyKind::Tuple(elements) => TyKind::Tuple(
                elements.iter().map(|e| e.apply_subst(subst)).collect(),
            ),
            TyKind::Array(element_ty, size) => {
                TyKind::Array(Arc::new(element_ty.apply_subst(subst)), *size)
            }
            TyKind::Pointer(inner, ptr_type) => {
                TyKind::Pointer(Arc::new(inner.apply_subst(subst)), *ptr_type)
            }
            TyKind::Map(key, val) => {
                 TyKind::Map(Arc::new(key.apply_subst(subst)), Arc::new(val.apply_subst(subst)))
            }
            TyKind::Set(elem) => {
                TyKind::Set(Arc::new(elem.apply_subst(subst)))
            }

            // Non-recursive kinds: These don't contain nested types that need substitution.
            // This also handles InferInt/InferFloat if they weren't resolved by the loop in Step 1.
            kind => kind.clone(), // Primitive, Error, Never, SelfType, GenericParam, unresolved InferInt/Float
        };

        // Return the type with potentially substituted inner components, using the span from resolved_ty
        // <<< DEBUG PRINT >>>
        let final_ty = Ty {
            kind: new_kind,
            span: resolved_ty.span, // Use span from resolved_ty (which might have been updated)
        };
        println!("  [apply_subst] Result: {}", display_type(&final_ty));
        final_ty
    }

    /// Applies a substitution in place recursively.
    /// NOTE: This mutates the Ty. Use Ty::apply_subst for an immutable version.
    pub fn apply_subst_mut(&mut self, subst: &Substitution) {
        match &mut self.kind {
            TyKind::Var(id) => {
                if let Some(new_ty) = subst.get(id) {
                    // Recursively apply substitution to the *result* before assigning
                    let mut substituted_new_ty = new_ty.clone();
                    substituted_new_ty.apply_subst_mut(subst); // Apply recursively

                    // If the substituted type is the same as the current var, avoid replacing.
                    if let TyKind::Var(new_id) = substituted_new_ty.kind {
                        if new_id == *id {
                            return; // No change needed
                        }
                    }
                    println!("  [apply_subst_mut] Substituting Var({}) -> {}", id, display_type(&substituted_new_ty));
                    *self = substituted_new_ty; // Replace self with the fully substituted type
                }
            }
            TyKind::Named { args, .. } => {
                for arg in args {
                    arg.apply_subst_mut(subst);
                }
            }
            TyKind::Function(params, ret) => {
                for param in params {
                    param.apply_subst_mut(subst);
                }
                Arc::make_mut(ret).apply_subst_mut(subst);
            }
            TyKind::Tuple(elements) => {
                for elem in elements {
                    elem.apply_subst_mut(subst);
                }
            }
            TyKind::Array(element_ty, _) => {
                Arc::make_mut(element_ty).apply_subst_mut(subst);
            }
            TyKind::Pointer(inner, _) => {
                Arc::make_mut(inner).apply_subst_mut(subst);
            }
             TyKind::Map(key, val) => { 
                 Arc::make_mut(key).apply_subst_mut(subst);
                 Arc::make_mut(val).apply_subst_mut(subst);
             }
            TyKind::Set(elem) => { 
                Arc::make_mut(elem).apply_subst_mut(subst);
            }
            // Non-recursive/non-substitutable kinds: Primitive, SelfType, GenericParam, Never, Error
            _ => {}
        }
    }

    /// Find all free inference variables (`TyKind::Var`) in the type.
    ///
    /// Preconditions: None.
    /// Postconditions: Returns a `HashSet` containing the names of free inference variables.
    pub fn free_vars(&self) -> HashSet<TypeId> { // Return HashSet<TypeId>
        let mut vars = HashSet::new();
        self.collect_free_vars(&mut vars);
        vars
    }

    fn collect_free_vars(&self, vars: &mut HashSet<TypeId>) { // Collect TypeId
        match &self.kind {
            TyKind::Var(id) => { // Use Var(id)
                vars.insert(*id); // Insert TypeId
            }
            TyKind::Named { args, .. } => {
                for arg in args {
                    arg.collect_free_vars(vars);
                }
            }
            TyKind::Function(params, ret) => {
                ret.collect_free_vars(vars);
                for param in params {
                    param.collect_free_vars(vars);
                }
            }
            TyKind::Tuple(elements) => {
                for elem in elements {
                    elem.collect_free_vars(vars);
                }
            }
            TyKind::Array(element_ty, _) => {
                element_ty.collect_free_vars(vars);
            }
            TyKind::Pointer(inner, _) => { 
                inner.collect_free_vars(vars);
            }
             TyKind::Map(key, val) => { 
                 key.collect_free_vars(vars);
                 val.collect_free_vars(vars);
             }
            TyKind::Set(elem) => { 
                elem.collect_free_vars(vars);
            }
            // Primitive, SelfType, GenericParam, Never, Error don't contain Var vars
            _ => {}
        }
    }

    /// Recursively checks if the type contains `TyKind::SelfType`.
    pub fn contains_self_type(&self) -> bool {
        match &self.kind {
            TyKind::SelfType => true,
            TyKind::Primitive(_) => false,
            TyKind::InferInt(_) => false,
            TyKind::InferFloat(_) => false,
            TyKind::Named { args, .. } => args.iter().any(Ty::contains_self_type),
            TyKind::Function(params, ret) => {
                ret.contains_self_type() || params.iter().any(Ty::contains_self_type)
            }
            TyKind::Tuple(elements) => elements.iter().any(Ty::contains_self_type),
            TyKind::Array(element_ty, _) => element_ty.contains_self_type(),
            TyKind::Pointer(inner, _) => inner.contains_self_type(), 
            TyKind::Map(key, val) => key.contains_self_type() || val.contains_self_type(),
            TyKind::Set(elem) => elem.contains_self_type(),
            TyKind::GenericParam(_) => false, 
            TyKind::Var(_) => false, // Var is not SelfType 
            TyKind::Never => false,
            TyKind::Error => false,
        }
    }

    /// Returns `true` if this type is a primitive type (`TyKind::Primitive`).
    pub fn is_primitive(&self) -> bool {
        matches!(self.kind, TyKind::Primitive(_))
    }

    pub fn is_error(&self) -> bool {
        matches!(self.kind, TyKind::Error)
    }
}

/// Represents the different kinds of types in the Parallax type system.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind {
    /// A type variable (`t0`, `t1`, ...) used during type inference.
    /// Should not persist in the final typed HIR outside of generic function signatures.
    Var(TypeId),

    /// A built-in primitive type (e.g., `i32`, `bool`, `string`).
    Primitive(PrimitiveType),

    /// Represents an integer literal whose concrete type (e.g., `i32`, `u64`) has not
    /// yet been determined by inference or suffix. It holds the TypeId of the
    /// inference variable associated with it.
    InferInt(TypeId),

    /// Represents a float literal whose concrete type (`f32` or `f64`) has not
    /// yet been determined. It holds the TypeId of the
    /// inference variable associated with it.
    InferFloat(TypeId),

    /// A user-defined named type (struct, enum, potentially type alias later).
    Named {
        /// The source name of the type (e.g., "List", "Option").
        name: String,
        /// The unique symbol identifying the type *definition* (struct or enum).
        /// This allows distinguishing between types with the same name in different modules.
        symbol: Option<Symbol>,
        /// Resolved type arguments if the type is generic (e.g., `i32` in `List<i32>`).
        args: Vec<Ty>,
    },

    /// An array type with a known element type and size (e.g., `[i32; 5]`).
    Array(Arc<Ty>, Option<usize>),

    /// A tuple type (e.g., `(i32, bool)`).
    Tuple(Vec<Ty>),

    /// A function type (e.g., `fn(i32, bool) -> string`).
    Function(Vec<Ty>, Arc<Ty>), // Parameters, Return Type

    /// An error type, representing a failure during type checking.
    /// Unifies with any type but signals that an error occurred upstream.
    Error,

    /// The never type `!`, indicating divergence (e.g., a function that exits).
    /// Unifies with any type.
    Never,

    /// Placeholder for the 'Self' type keyword within traits and impls.
    /// Should be substituted with the concrete implementing type during checking.
    /// It is an error for this to appear in the final HIR outside of trait/impl signatures.
    SelfType,

    /// A map type (e.g., `Map<string, i32>`).
    Map(Arc<Ty>, Arc<Ty>), // Key Type, Value Type

    /// A set type (e.g., `Set<string>`).
    Set(Arc<Ty>), // Element Type

    /// A pointer type (e.g., `*const i32` or `*mut i32`).
    Pointer(Arc<Ty>, PointerType),

    /// A generic parameter type (e.g., `T` in `struct Vec<T>`). Not an inference variable.
    GenericParam(String),
}

/// Represents built-in primitive types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PrimitiveType {
    // Signed Integers
    I8, I16, I32, I64, I128,
    // Unsigned Integers
    U8, U16, U32, U64, U128,
    // Floating Point
    F32, F64,
    // Other Primitives
    Bool,
    Char,
    String,
    /// The unit type `()`, typically the type of expressions used as statements.
    Unit,
}

impl fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrimitiveType::I8 => write!(f, "i8"),
            PrimitiveType::I16 => write!(f, "i16"),
            PrimitiveType::I32 => write!(f, "i32"),
            PrimitiveType::I64 => write!(f, "i64"),
            PrimitiveType::I128 => write!(f, "i128"),
            PrimitiveType::U8 => write!(f, "u8"),
            PrimitiveType::U16 => write!(f, "u16"),
            PrimitiveType::U32 => write!(f, "u32"),
            PrimitiveType::U64 => write!(f, "u64"),
            PrimitiveType::U128 => write!(f, "u128"),
            PrimitiveType::F32 => write!(f, "f32"),
            PrimitiveType::F64 => write!(f, "f64"),
            PrimitiveType::Bool => write!(f, "bool"),
            PrimitiveType::Char => write!(f, "char"),
            PrimitiveType::String => write!(f, "string"),
            PrimitiveType::Unit => write!(f, "()"),
        }
    }
}

impl PrimitiveType {
    /// Returns `true` if this type is a float type (`f32`, `f64`).
    pub fn is_float(&self) -> bool {
        matches!(self, PrimitiveType::F32 | PrimitiveType::F64)
    }

    /// Returns `true` if this type is an integer type (signed, unsigned).
    pub fn is_integer(&self) -> bool {
        matches!(self,
            PrimitiveType::I8 | PrimitiveType::I16 | PrimitiveType::I32 | PrimitiveType::I64 | PrimitiveType::I128 |
            PrimitiveType::U8 | PrimitiveType::U16 | PrimitiveType::U32 | PrimitiveType::U64 | PrimitiveType::U128
        )
    }

    /// Returns `true` if this type is numeric (integer or float).
    pub fn is_numeric(&self) -> bool {
        self.is_integer() || self.is_float()
    }
}

impl TyKind {
    /// Panics if the kind is not Primitive, otherwise returns the PrimitiveType.
    pub fn expect_primitive(&self) -> PrimitiveType {
        match self {
            TyKind::Primitive(p) => *p,
            _ => panic!("Expected TyKind::Primitive, found {:?}", self),
        }
    }
}

/// Represents the kind of a pointer (*mut or *const).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PointerType {
    Const,
    Mutable,
}

impl fmt::Display for TyKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TyKind::Var(id) => write!(f, "t{}", id.0),
            TyKind::Primitive(prim) => write!(f, "{}", prim),
            TyKind::InferInt(id) => write!(f, "{{integer(t{})}}", id.0),
            TyKind::InferFloat(id) => write!(f, "{{float(t{})}}", id.0),
            TyKind::Named { name, symbol: _, args } => {
                write!(f, "{}", name)?;
                if !args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", arg)?;
                    }
                    write!(f, ">")?;
                }
                Ok(())
            },
            TyKind::Array(element_ty, size) => {
                write!(f, "{}", element_ty)?;
                if let Some(size) = size {
                    write!(f, "[{}]", size)?;
                }
                Ok(())
            },
            TyKind::Tuple(elements) => {
                write!(f, "(")?;
                for (i, element) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", element)?;
                }
                write!(f, ")")
            },
            TyKind::Function(params, ret) => {
                write!(f, "fn(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, ") -> ")?;
                write!(f, "{}", ret)
            },
            TyKind::Error => write!(f, "Error"),
            TyKind::Never => write!(f, "Never"),
            TyKind::SelfType => write!(f, "Self"),
            TyKind::Map(key, val) => write!(f, "Map<{} -> {}>", key, val),
            TyKind::Set(elem) => write!(f, "Set<{}>", elem),
            TyKind::Pointer(inner, ptr_type) => {
                write!(f, "{}", inner)?;
                match ptr_type {
                    PointerType::Const => write!(f, "*const"),
                    PointerType::Mutable => write!(f, "*mut"),
                }
            },
            TyKind::GenericParam(name) => write!(f, "{}", name),
        }
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::inference::{Substitution, TypeId}; // Import TypeId
    use std::collections::{HashSet, BTreeMap};
    use std::sync::Arc;

    fn i32_ty() -> Ty { Ty::new(TyKind::Primitive(PrimitiveType::I32)) }
    fn bool_ty() -> Ty { Ty::new(TyKind::Primitive(PrimitiveType::Bool)) }
    fn var_ty(id: u32) -> Ty { Ty::new(TyKind::Var(TypeId(id))) }
    fn self_ty() -> Ty { Ty::new(TyKind::SelfType) }

    #[test]
    fn ty_apply_subst_simple() {
        let mut subst = Substitution::new();
        subst.insert(TypeId(0), i32_ty());
        let ty_t0 = var_ty(0);
        let result = ty_t0.apply_subst(&subst);
        assert_eq!(result, i32_ty());
    }

    #[test]
    fn ty_apply_subst_chained() {
        let mut subst = Substitution::new();
        subst.insert(TypeId(0), var_ty(1)); // t0 -> t1
        subst.insert(TypeId(1), bool_ty()); // t1 -> bool
        let ty_t0 = var_ty(0);
        let result = ty_t0.apply_subst(&subst);
        assert_eq!(result, bool_ty());
    }

    #[test]
    fn ty_apply_subst_nested() {
        let mut subst = Substitution::new();
        subst.insert(TypeId(0), i32_ty());
        let ty_vec_t0 = Ty::new(TyKind::Named {
            name: "Vec".to_string(),
            symbol: None,
            args: vec![var_ty(0)],
        });
        let expected = Ty::new(TyKind::Named {
            name: "Vec".to_string(),
            symbol: None,
            args: vec![i32_ty()],
        });
        let result = ty_vec_t0.apply_subst(&subst);
        assert_eq!(result, expected);
    }

    #[test]
    fn ty_apply_subst_no_change() {
        let mut subst = Substitution::new();
        subst.insert(TypeId(0), i32_ty());
        let ty_bool = bool_ty();
        let result = ty_bool.apply_subst(&subst);
        assert_eq!(result, ty_bool);
    }

    #[test]
    fn ty_free_vars_simple() {
        let ty = var_ty(0);
        let fv = ty.free_vars();
        assert!(fv.contains(&TypeId(0)));
        assert_eq!(fv.len(), 1);
    }

    #[test]
    fn ty_free_vars_nested() {
        let ty = Ty::new(TyKind::Tuple(vec![var_ty(0), var_ty(1), i32_ty()]));
        let fv = ty.free_vars();
        assert!(fv.contains(&TypeId(0)));
        assert!(fv.contains(&TypeId(1)));
        assert_eq!(fv.len(), 2);
    }

    #[test]
    fn ty_free_vars_bound() {
        let mut subst = Substitution::new();
        subst.insert(TypeId(0), i32_ty()); // t0 is bound
        let ty = Ty::new(TyKind::Tuple(vec![var_ty(0), var_ty(1)]));
        let substituted_ty = ty.apply_subst(&subst);
        let fv = substituted_ty.free_vars();
        assert!(!fv.contains(&TypeId(0))); // t0 should no longer be free
        assert!(fv.contains(&TypeId(1)));
        assert_eq!(fv.len(), 1);
    }

    #[test]
    fn ty_free_vars_no_vars() {
        let ty = i32_ty();
        let fv = ty.free_vars();
        assert!(fv.is_empty());
    }

    #[test]
    fn ty_contains_self_type_basic() {
        assert!(self_ty().contains_self_type());
        assert!(!i32_ty().contains_self_type());
    }

    #[test]
    fn ty_contains_self_type_nested() {
        let ty_tuple = Ty::new(TyKind::Tuple(vec![i32_ty(), self_ty()]));
        assert!(ty_tuple.contains_self_type());

        let ty_func = Ty::new(TyKind::Function(vec![bool_ty()], Arc::new(self_ty())));
        assert!(ty_func.contains_self_type());

        let ty_named = Ty::new(TyKind::Named { name: "X".into(), symbol: None, args: vec![i32_ty(), self_ty()]});
        assert!(ty_named.contains_self_type());
    }

    #[test]
    fn ty_contains_self_type_negative() {
        let ty_tuple = Ty::new(TyKind::Tuple(vec![i32_ty(), bool_ty()]));
        assert!(!ty_tuple.contains_self_type());

        let ty_named = Ty::new(TyKind::Named { name: "X".into(), symbol: None, args: vec![i32_ty()]});
        assert!(!ty_named.contains_self_type());
    }

    fn test_contains_self_type() {
        let i32_ty = Ty::new(TyKind::Primitive(PrimitiveType::I32));
        let self_ty = Ty::new(TyKind::SelfType);
        let gen_ty = Ty::new(TyKind::GenericParam("T".into()));
        let tuple_ty = Ty::new(TyKind::Tuple(vec![i32_ty.clone(), self_ty.clone()]));
        let func_ty = Ty::new(TyKind::Function(vec![self_ty.clone()], Arc::new(i32_ty.clone())));
        let ty_named = Ty::new(TyKind::Named { name: "X".into(), symbol: None, args: vec![i32_ty.clone()]});

        assert!(!i32_ty.contains_self_type());
        assert!(self_ty.contains_self_type());
        assert!(!gen_ty.contains_self_type());
        assert!(tuple_ty.contains_self_type());
        assert!(func_ty.contains_self_type());
        assert!(!ty_named.contains_self_type());
    }
}