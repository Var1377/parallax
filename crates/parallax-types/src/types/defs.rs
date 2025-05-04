// src/types/defs.rs
//! Core type definitions used by the type checker.

use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use miette::SourceSpan;
use parallax_resolve::{types::{Symbol, ResolvedExpr}};

use crate::context::inference::TypeId;
// Use crate paths for Ty, TypeId, TraitId, ImplId
use crate::types::core::{Ty};
use crate::types::hir::TypedExpr; // Required for checked_default_bodies
use crate::context::{ImplId, Substitution, TraitId};

// --- Common Definitions ---

/// Definition of a generic parameter including its bounds.
/// Stored in `StructDef`, `EnumDef`, `FunctionSignature`, `TraitDef`, `ImplDef`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GenericParamDef {
    /// Original name from source (e.g., "T").
    pub name: String,
    /// Symbol from resolver (unique ID for this param def within its scope).
    pub symbol: Symbol,
    /// TypeId assigned by typechecker (usually maps to a `TyKind::Var` initially).
    /// This ID is used for substitution during instantiation.
    pub id: TypeId,
    /// Trait bounds required for this parameter (e.g., `T: Display + Debug`).
    pub bounds: Vec<TraitRef>,
    /// Source span of the generic parameter definition.
    pub span: SourceSpan,
}

/// A field in a struct or struct-like enum variant.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field {
    /// Field name.
    pub name: String,
    /// The resolved symbol identifying this specific field definition.
    pub symbol: Symbol,
    /// Type of the field (potentially containing generic parameters from the parent struct/enum).
    pub ty: Ty,
    /// Source span of the field definition.
    pub span: SourceSpan,
}

/// A parameter in a function signature (excluding `self`).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParamType {
    /// Parameter name (might be `_` if unused in source).
    pub name: String,
    /// Type of the parameter (potentially containing generic parameters from the function).
    pub ty: Ty,
    /// Source span of the parameter definition.
    pub span: SourceSpan,
    // Consider adding `symbol: Symbol` if needed to link back to the parameter binding.
}

/// Defines different kinds of the `self` parameter for associated functions (methods).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SelfParamKind {
    /// Represents `self`, consuming the value.
    Value,
}

// --- Item Definitions (Used by Checker Context) ---
// These represent the information stored *during* checking, before bodies are processed.

/// A struct type definition (stored in TypeContext/TraitRepository).
#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: String,
    pub symbol: Symbol,
    pub generic_params: Vec<GenericParamDef>,
    pub fields: Vec<Field>,
    pub span: SourceSpan,
}

/// A variant within an enum definition.
#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: String,
    pub symbol: Symbol,
    /// Fields of the variant. Empty for unit variants. Contains indexed fields (name="0", "1",...) for tuple variants.
    pub fields: Vec<Field>,
    pub span: SourceSpan,
}

/// An enum type definition (stored in TypeContext/TraitRepository).
#[derive(Debug, Clone)]
pub struct EnumDef {
    pub name: String,
    pub symbol: Symbol,
    pub generic_params: Vec<GenericParamDef>,
    pub variants: Vec<EnumVariant>,
    pub span: SourceSpan,
}

/// A function signature (stored in TypeContext/TraitRepository).
#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: String,
    /// Defines the kind of `self` parameter, if this is a method.
    pub self_param: Option<SelfParamKind>,
    /// Generic parameters defined *on the function itself*.
    pub generic_params: Vec<GenericParamDef>,
    /// Regular parameters (excluding `self`).
    pub params: Vec<ParamType>,
    pub return_type: Ty,
    pub span: SourceSpan,
}

impl FunctionSignature {
    /// Checks if any parameter or the return type contains SelfType recursively.
    pub fn contains_self_type(&self) -> bool {
        self.return_type.contains_self_type() || self.params.iter().any(|p| p.ty.contains_self_type())
        // We don't need to check self_param presence, just the types.
        // We also don't check generic param definitions themselves for SelfType.
    }

    /// Applies a substitution to parameter types and the return type in place.
    pub fn apply_subst_mut(&mut self, subst: &Substitution) {
        self.return_type.apply_subst_mut(subst);
        for param in &mut self.params {
            param.ty.apply_subst_mut(subst);
        }
        // We don't substitute within generic param bounds here.
    }
}

/// Represents a resolved trait reference, potentially with type arguments.
/// Used for trait bounds, supertraits, and impl targets.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitRef {
    /// The ID of the referenced trait definition in the `TraitRepository`.
    pub trait_id: TraitId,
    /// Concrete types for the trait's generic parameters (e.g., `String` in `MyTrait<String>`).
    /// Empty if the trait is not generic.
    pub type_arguments: Vec<Ty>,
    /// Source span where this trait reference occurs.
    pub span: SourceSpan,
}

/// Definition of an associated type within a trait signature (stored in TraitDef).
#[derive(Debug, Clone)]
pub struct AssociatedTypeDef {
    pub name: String,
    /// Symbol for the associated type definition itself (e.g., for `Item` in `trait Iterator { type Item; }`).
    pub symbol: Symbol,
    /// Trait bounds required for this associated type (e.g., `Item: Display`).
    pub bounds: Vec<TraitRef>,
    /// Default type if provided in the trait definition (e.g., `type Item = i32;`).
    pub default: Option<Ty>,
    pub span: SourceSpan,
}

/// Definition of a trait (stored in TraitRepository).
#[derive(Debug, Clone)]
pub struct TraitDef {
    /// Unique ID assigned by the `TraitRepository`.
    pub id: TraitId,
    /// Original symbol from the resolver identifying this trait definition.
    pub trait_symbol: Symbol,
    pub name: String,
    pub generic_params: Vec<GenericParamDef>,
    /// Bounds required by the trait itself (supertraits).
    pub bounds: Vec<TraitRef>,
    /// Methods defined in the trait signature. Keyed by the method's definition symbol.
    pub methods: HashMap<Symbol, TraitMethod>,
    /// Associated types defined in the trait signature. Keyed by the associated type's symbol.
    pub associated_types: HashMap<Symbol, AssociatedTypeDef>,
    pub span: SourceSpan,
}

/// Definition of a method signature within a trait definition (stored in TraitDef).
#[derive(Debug, Clone)]
pub struct TraitMethod {
    pub name: String,
    /// Original symbol from the resolver identifying this method definition.
    pub method_symbol: Symbol,
    /// The signature of the method as defined in the trait.
    pub signature: FunctionSignature,
    /// The *resolved* AST node for the default method body, if provided.
    pub default_body: Option<ResolvedExpr>,
}

/// Definition of a trait implementation (`impl Trait for Type` or `impl Type`) (stored in TraitRepository).
#[derive(Debug, Clone)]
pub struct ImplDef {
    /// Unique ID assigned by the `TraitRepository`.
    pub id: ImplId,
    /// Original symbol from the resolver identifying this specific impl block.
    pub impl_symbol: Symbol,
    /// The symbol of the trait being implemented, if this is a trait impl.
    pub trait_symbol: Option<Symbol>,
    /// The concrete type implementing the trait or receiving inherent methods.
    pub implementing_type: Ty,
    /// Generic parameters defined *on the impl block itself* (e.g., `impl<T: Debug>`).
    pub generic_params: Vec<GenericParamDef>,
    /// Signatures for methods explicitly defined in this impl block.
    /// Keyed by the *implementing function's symbol*.
    pub method_signatures: HashMap<Symbol, FunctionSignature>,
    /// Mapping from the trait method symbol to the symbol of the function implementing it.
    /// Needed to check if defaults are overridden and potentially for other lookups.
    pub methods: HashMap<Symbol, Symbol>,
    /// Bindings for associated types defined in this impl block (e.g., `type Item = i32;`).
    pub associated_type_bindings: HashMap<Symbol, Ty>,
    /// Mapping from a trait method symbol (that has a default implementation)
    /// to the symbol of the synthesized `TypedFunction` generated by type-checking
    /// the default body in the context of this specific `ImplDef`.
    pub default_method_impl_symbols: HashMap<Symbol, Symbol>,
    /// The type-checked default method bodies used by this impl block.
    /// Keyed by the *trait method symbol*. Stored here because the check happens
    /// in the context of the specific impl.
    pub checked_default_bodies: HashMap<Symbol, TypedExpr>,
    pub span: SourceSpan,
}

/// Enum distinguishing the different kinds of definitions stored in `TypeContext`.
#[derive(Debug, Clone)]
pub enum TypeDef {
    /// A struct definition (signature only).
    Struct(StructDef),
    /// An enum definition (signature only).
    Enum(EnumDef),
    /// A function signature (standalone or concrete impl method).
    Function(FunctionSignature),
    // Note: Trait definitions are stored separately in TraitRepository using TraitDef.
} 