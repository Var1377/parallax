use std::collections::HashSet;
use std::fmt;
use std::sync::Arc;

use miette::SourceSpan;
use serde::{Deserialize, Serialize};

// Removed outdated TODO comment
use crate::context::inference::Substitution;

/// A unique identifier for a type variable
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TypeId(pub u32);

impl fmt::Display for TypeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "t{}", self.0)
    }
}

/// A type in the Parallax language
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ty {
    /// The kind of type
    pub kind: TyKind,
    /// Source span of the type in the original code
    pub span: Option<SourceSpan>,
}

impl Ty {
    /// Create a new type with no source span
    pub fn new(kind: TyKind) -> Self {
        Self { kind, span: None }
    }

    /// Create a new type with the given source span
    pub fn with_span(kind: TyKind, span: SourceSpan) -> Self {
        Self { kind, span: Some(span) }
    }
    
    /// Returns true if this type is a type variable
    pub fn is_var(&self) -> bool {
        matches!(self.kind, TyKind::Var(_))
    }
    
    /// Returns true if this type contains no type variables
    pub fn is_concrete(&self) -> bool {
        self.free_vars().is_empty()
    }

    /// Apply a substitution to this type, replacing type variables
    pub fn apply_subst(&self, subst: &Substitution) -> Self {
        match &self.kind {
            TyKind::Var(id) => {
                if let Some(ty) = subst.get(id) {
                    let mut res = ty.apply_subst(subst);
                    if res.span.is_none() {
                        res.span = self.span;
                    }
                    res
                } else {
                    self.clone()
                }
            }
            TyKind::Primitive(_) => self.clone(),
            TyKind::Named { name, args, .. } => {
                let new_args = args.iter().map(|arg| arg.apply_subst(subst)).collect();
                Ty {
                    kind: TyKind::Named { name: name.clone(), symbol: None, args: new_args },
                    span: self.span,
                }
            }
            TyKind::Array(elem_ty, size) => {
                let new_elem = elem_ty.apply_subst(subst);
                Ty {
                    kind: TyKind::Array(Arc::new(new_elem), *size),
                    span: self.span,
                }
            }
            TyKind::Tuple(tys) => {
                let new_tys = tys.iter().map(|ty| ty.apply_subst(subst)).collect();
                Ty {
                    kind: TyKind::Tuple(new_tys),
                    span: self.span,
                }
            }
            TyKind::Function(params, ret) => {
                let new_params = params.iter().map(|param| param.apply_subst(subst)).collect();
                let new_ret = ret.apply_subst(subst);
                Ty {
                    kind: TyKind::Function(new_params, Arc::new(new_ret)),
                    span: self.span,
                }
            }
            TyKind::Error => self.clone(),
            TyKind::Never => self.clone(),
            TyKind::SelfType => self.clone(),
            TyKind::Map(key_ty, value_ty) => {
                let new_key_ty = key_ty.apply_subst(subst);
                let new_value_ty = value_ty.apply_subst(subst);
                Ty {
                    kind: TyKind::Map(Arc::new(new_key_ty), Arc::new(new_value_ty)),
                    span: self.span,
                }
            }
            TyKind::Set(elem_ty) => {
                let new_elem_ty = elem_ty.apply_subst(subst);
                Ty {
                    kind: TyKind::Set(Arc::new(new_elem_ty)),
                    span: self.span,
                }
            }
        }
    }

    /// Get the free type variables in this type
    pub fn free_vars(&self) -> HashSet<TypeId> {
        let mut vars = HashSet::new();
        self.collect_free_vars(&mut vars);
        vars
    }

    // Helper for free_vars to avoid redundant allocations
    fn collect_free_vars(&self, vars: &mut HashSet<TypeId>) {
        match &self.kind {
            TyKind::Var(id) => { vars.insert(*id); }
            TyKind::Primitive(_) | TyKind::Error | TyKind::Never | TyKind::SelfType => {} // Skip SelfType
            TyKind::Named { args, .. } => {
                for arg in args {
                    arg.collect_free_vars(vars);
                }
            }
            TyKind::Array(elem_ty, _) => elem_ty.collect_free_vars(vars),
            TyKind::Tuple(tys) => {
                for ty in tys {
                    ty.collect_free_vars(vars);
                }
            }
            TyKind::Function(params, ret) => {
                for param in params {
                    param.collect_free_vars(vars);
                }
                ret.collect_free_vars(vars);
            }
            TyKind::Map(key_ty, value_ty) => {
                key_ty.collect_free_vars(vars);
                value_ty.collect_free_vars(vars);
            }
            TyKind::Set(elem_ty) => elem_ty.collect_free_vars(vars),
        }
    }
}

/// The kind of a type
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind {
    /// A type variable used during type inference
    Var(TypeId),
    
    /// A primitive type (int, float, etc.)
    Primitive(PrimitiveType),
    
    /// A named type (struct, enum, type alias, etc.)
    Named {
        /// The name of the type
        name: String,
        /// The original symbol, if available (useful for traits/types resolved directly)
        symbol: Option<parallax_resolve::types::Symbol>,
        /// Type arguments for generic types
        args: Vec<Ty>,
    },
    
    /// An array type with a known size
    Array(Arc<Ty>, usize),
    
    /// A tuple type
    Tuple(Vec<Ty>),
    
    /// A function type
    Function(Vec<Ty>, Arc<Ty>),
    
    /// An error type, used when type checking fails
    Error,

    /// The never type `!`, indicating divergence
    Never,

    /// Placeholder for the 'Self' type keyword within traits and impls.
    SelfType,

    /// A map type (key, value)
    Map(Arc<Ty>, Arc<Ty>),

    /// A set type
    Set(Arc<Ty>),
}

/// Primitive types in the Parallax language
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PrimitiveType {
    /// Signed 8-bit integer
    I8,
    /// Signed 16-bit integer
    I16,
    /// Signed 32-bit integer
    I32, 
    /// Signed 64-bit integer
    I64,
    /// Signed 128-bit integer
    I128,
    /// Unsigned 8-bit integer
    U8,
    /// Unsigned 16-bit integer
    U16,
    /// Unsigned 32-bit integer
    U32,
    /// Unsigned 64-bit integer
    U64,
    /// Unsigned 128-bit integer
    U128,
    /// 32-bit floating point
    F32,
    /// 64-bit floating point
    F64,
    /// Boolean type
    Bool,
    /// Character type
    Char,
    /// String type
    String,
    /// The unit type `()`, representing the absence of a value.
    Unit,
    /// Integer literal type (can be narrowed to any integer type)
    IntegerLiteral,
    /// Float literal type (can be narrowed to f32 or f64)
    FloatLiteral,
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
            PrimitiveType::IntegerLiteral => write!(f, "{{integer}}"),
            PrimitiveType::FloatLiteral => write!(f, "{{float}}"),
        }
    }
}

// Add impl block for PrimitiveType helper methods
impl PrimitiveType {
    pub fn is_numeric(&self) -> bool {
        match self {
            PrimitiveType::I8
            | PrimitiveType::I16
            | PrimitiveType::I32
            | PrimitiveType::I64
            | PrimitiveType::I128
            | PrimitiveType::U8
            | PrimitiveType::U16
            | PrimitiveType::U32
            | PrimitiveType::U64
            | PrimitiveType::U128
            | PrimitiveType::F32
            | PrimitiveType::F64
            | PrimitiveType::IntegerLiteral
            | PrimitiveType::FloatLiteral => true,
            _ => false,
        }
    }

    /// Returns true if this type is a float type (f32, f64, or FloatLiteral).
    pub fn is_float(&self) -> bool {
        matches!(self, PrimitiveType::F32 | PrimitiveType::F64 | PrimitiveType::FloatLiteral)
    }
    
    /// Returns true if this type is an integer type (i*/u* or IntegerLiteral).
    pub fn is_integer(&self) -> bool {
        match self {
            PrimitiveType::I8
            | PrimitiveType::I16
            | PrimitiveType::I32
            | PrimitiveType::I64
            | PrimitiveType::I128
            | PrimitiveType::U8
            | PrimitiveType::U16
            | PrimitiveType::U32
            | PrimitiveType::U64
            | PrimitiveType::U128
            | PrimitiveType::IntegerLiteral => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*; // Import items from parent module
    use crate::context::inference::Substitution;
    use std::sync::Arc;

    // Helper function to create a type variable
    fn ty_var(id: u32) -> Ty {
        Ty::new(TyKind::Var(TypeId(id)))
    }

    // Helper function to create a primitive type
    fn ty_prim(prim: PrimitiveType) -> Ty {
        Ty::new(TyKind::Primitive(prim))
    }

    // Helper function to create a map type
    fn ty_map(key: Ty, value: Ty) -> Ty {
        Ty::new(TyKind::Map(Arc::new(key), Arc::new(value)))
    }

    // Helper function to create a set type
    fn ty_set(elem: Ty) -> Ty {
        Ty::new(TyKind::Set(Arc::new(elem)))
    }

    #[test]
    fn test_is_var_and_concrete() {
        let ty_v = ty_var(0);
        let ty_p = ty_prim(PrimitiveType::I32);
        let ty_tuple_v = Ty::new(TyKind::Tuple(vec![ty_prim(PrimitiveType::Bool), ty_var(1)]));
        let ty_tuple_p = Ty::new(TyKind::Tuple(vec![ty_prim(PrimitiveType::Bool), ty_prim(PrimitiveType::String)]));

        assert!(ty_v.is_var());
        assert!(!ty_p.is_var());
        assert!(!ty_tuple_v.is_var());
        assert!(!ty_tuple_p.is_var());

        assert!(!ty_v.is_concrete());
        assert!(ty_p.is_concrete());
        assert!(!ty_tuple_v.is_concrete());
        assert!(ty_tuple_p.is_concrete());
    }

    #[test]
    fn test_free_vars() {
        let ty1 = ty_var(0);
        let ty2 = ty_prim(PrimitiveType::I32);
        let ty3 = Ty::new(TyKind::Tuple(vec![ty_var(1), ty_var(2)]));
        let ty4 = Ty::new(TyKind::Named {
            name: "Vec".to_string(),
            symbol: None,
            args: vec![ty_var(3)],
        });
        let ty5 = Ty::new(TyKind::Function(
            vec![ty_var(4)],
            Arc::new(ty_prim(PrimitiveType::Bool)),
        ));
        let ty6 = Ty::new(TyKind::Array(Arc::new(ty_var(5)), 10));
        let ty7 = Ty::new(TyKind::Named {
            name: "MyStruct".to_string(),
            symbol: None,
            args: vec![ty_var(6), ty_var(6)],
        });
        let ty8 = Ty::new(TyKind::Tuple(vec![ty_var(7), ty_var(8), ty1.clone()]));
        let ty9 = ty_map(ty_var(9), ty_prim(PrimitiveType::String));
        let ty10 = ty_set(ty_var(10));
        let ty11 = ty_map(ty_var(11), ty_set(ty_var(12))); // Nested map/set

        assert_eq!(ty1.free_vars(), HashSet::from([TypeId(0)]));
        assert!(ty2.free_vars().is_empty());
        assert_eq!(ty3.free_vars(), HashSet::from([TypeId(1), TypeId(2)]));
        assert_eq!(ty4.free_vars(), HashSet::from([TypeId(3)]));
        assert_eq!(ty5.free_vars(), HashSet::from([TypeId(4)]));
        assert_eq!(ty6.free_vars(), HashSet::from([TypeId(5)]));
        assert_eq!(ty7.free_vars(), HashSet::from([TypeId(6)]));
        assert_eq!(ty8.free_vars(), HashSet::from([TypeId(0), TypeId(7), TypeId(8)]));
        assert_eq!(ty9.free_vars(), HashSet::from([TypeId(9)]));
        assert_eq!(ty10.free_vars(), HashSet::from([TypeId(10)]));
        assert_eq!(ty11.free_vars(), HashSet::from([TypeId(11), TypeId(12)]));
    }

    #[test]
    fn test_apply_subst() {
        let var0 = ty_var(0);
        let var1 = ty_var(1);
        let i32_ty = ty_prim(PrimitiveType::I32);
        let bool_ty = ty_prim(PrimitiveType::Bool);
        let tuple_ty = Ty::new(TyKind::Tuple(vec![var0.clone(), var1.clone()]));
        let map_ty = ty_map(var0.clone(), var1.clone());
        let set_ty = ty_set(var0.clone());

        let mut subst1 = Substitution::new();
        subst1.insert(TypeId(0), i32_ty.clone());

        let mut subst2 = Substitution::new();
        subst2.insert(TypeId(1), bool_ty.clone());

        let mut subst3 = Substitution::new();
        subst3.insert(TypeId(0), var1.clone()); // Replace t0 with t1
        subst3.insert(TypeId(1), i32_ty.clone()); // Replace t1 with i32

        // Simple substitution
        assert_eq!(var0.apply_subst(&subst1), i32_ty);
        assert_eq!(var1.apply_subst(&subst1), var1); // No change

        // Substitution in tuple
        let expected_tuple1 = Ty::new(TyKind::Tuple(vec![i32_ty.clone(), var1.clone()]));
        assert_eq!(tuple_ty.apply_subst(&subst1), expected_tuple1);

        // Substitution in map/set
        let expected_map1 = ty_map(i32_ty.clone(), var1.clone());
        assert_eq!(map_ty.apply_subst(&subst1), expected_map1);
        let expected_set1 = ty_set(i32_ty.clone());
        assert_eq!(set_ty.apply_subst(&subst1), expected_set1);

        // Compose substitutions manually before applying
        let subst1_then_2 = subst1.compose(&subst2);
        let expected_tuple12 = Ty::new(TyKind::Tuple(vec![i32_ty.clone(), bool_ty.clone()]));
        assert_eq!(tuple_ty.apply_subst(&subst1_then_2), expected_tuple12);

        // Compose on map/set
        let expected_map12 = ty_map(i32_ty.clone(), bool_ty.clone());
        assert_eq!(map_ty.apply_subst(&subst1_then_2), expected_map12);
        // Set only depends on var0, so subst2 has no effect after subst1
        assert_eq!(set_ty.apply_subst(&subst1_then_2), expected_set1);

        // Recursive substitution (t0 -> t1 -> i32)
        // Note: apply_subst should handle the recursion. The test validates the Ty::apply_subst behavior.
        // Whether the Substitution itself correctly handles chained/recursive gets is tested elsewhere (context::inference tests).
        let expected_tuple3 = Ty::new(TyKind::Tuple(vec![i32_ty.clone(), i32_ty.clone()]));
        assert_eq!(tuple_ty.apply_subst(&subst3), expected_tuple3);

        // Substitution on concrete type
        assert_eq!(i32_ty.apply_subst(&subst1), i32_ty);
    }

    #[test]
    fn test_primitive_helpers() {
        assert!(PrimitiveType::I32.is_numeric());
        assert!(PrimitiveType::F64.is_numeric());
        assert!(PrimitiveType::IntegerLiteral.is_numeric());
        assert!(!PrimitiveType::Bool.is_numeric());
        assert!(!PrimitiveType::String.is_numeric());

        assert!(PrimitiveType::U64.is_integer());
        assert!(PrimitiveType::IntegerLiteral.is_integer());
        assert!(!PrimitiveType::F32.is_integer());
        assert!(!PrimitiveType::Bool.is_integer());
    }
}