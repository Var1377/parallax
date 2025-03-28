use super::common::{Span, Ident};
use super::types::Type;
use super::expr::GenericParam;
use super::pattern::Pattern;

/// A crate in the AST
#[derive(Debug, Clone)]
pub struct Crate {
    pub items: Vec<Item>,
}

/// An item in the AST (e.g., function, type definition, etc.)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Item {
    pub kind: ItemKind,
    pub visibility: bool,
    pub span: Span,
}

/// The kind of an item
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ItemKind {
    TypeDef(TypeDef),
    Function(Function),
    Enum(EnumDef),
    Struct(StructDef),
    Trait(TraitDef),
    Impl(ImplDef),
    Module(Module),
    Use(UseDecl),
}

/// A type definition item (e.g., `type T = i32;`)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeDef {
    pub name: Ident,
    pub generic_params: Option<Vec<GenericParam>>,
    pub ty: Type,
    pub where_clause: Option<WhereClause>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub name: Ident,
    pub generic_params: Option<Vec<GenericParam>>,
    pub params: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub where_clause: Option<WhereClause>,
    pub body: Box<super::expr::Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Parameter {
    pub pattern: Pattern,
    pub ty: Option<Type>,
    pub default_value: Option<super::expr::Expr>,
    pub is_variadic: bool,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumDef {
    pub name: Ident,
    pub generic_params: Option<Vec<GenericParam>>,
    pub where_clause: Option<WhereClause>,
    pub variants: Vec<EnumVariant>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumVariant {
    pub name: Ident,
    pub kind: EnumVariantKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EnumVariantKind {
    Unit,
    Tuple(Vec<Type>),
    Struct(Vec<StructField>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructDef {
    pub name: Ident,
    pub generic_params: Option<Vec<GenericParam>>,
    pub where_clause: Option<WhereClause>,
    pub fields: Vec<StructField>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    pub name: Ident,
    pub ty: Type,
    pub visibility: bool,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitDef {
    pub name: Ident,
    pub generic_params: Option<Vec<GenericParam>>,
    pub where_clause: Option<WhereClause>,
    pub supertraits: Vec<Type>,
    pub items: Vec<TraitItem>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TraitItem {
    Method {
        function: Function,
        default_impl: Option<super::expr::Expr>,
        span: Span,
    },
    AssociatedType {
        name: Ident,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImplDef {
    pub generic_params: Option<Vec<GenericParam>>,
    pub trait_type: Option<Type>,
    pub self_type: Type,
    pub where_clause: Option<WhereClause>,
    pub items: Vec<ImplItem>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ImplItem {
    Method(Function),
    AssociatedType {
        name: Ident,
        ty: Type,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Module {
    pub name: Ident,
    pub items: Vec<Item>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UseDecl {
    pub tree: UseTree,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UseTree {
    pub kind: UseTreeKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UseTreeKind {
    Path {
        segment: Ident,
        alias: Option<Ident>,
        sub_tree: Option<Box<UseTree>>,
    },
    Group(Vec<UseTree>),
    Glob,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WhereClause {
    pub predicates: Vec<WherePredicate>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WherePredicate {
    pub ty: Type,
    pub bounds: Vec<Type>,
    pub span: Span,
} 