use super::common::Ident;
use super::types::Type;
use super::expr::GenericParam;
use super::pattern::Pattern;
use super::Expr;
use miette::SourceSpan;

/// Represents a trait bound, e.g., `Display` or `Iterator<Item=String>`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitBound {
    pub path: Vec<Ident>, // The trait path (e.g., std::fmt::Display)
    // Note: generic_args currently parses Type, which might need refinement 
    // if specific argument kinds (like lifetimes or consts) are needed here.
    pub generic_args: Option<Vec<Type>>, // Optional args like <Item=String>
    pub span: SourceSpan,
}

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
    pub span: SourceSpan,
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
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub name: Ident,
    pub generic_params: Option<Vec<GenericParam>>,
    pub params: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub where_clause: Option<WhereClause>,
    pub body: Option<Box<Expr>>,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Parameter {
    pub pattern: Pattern,
    pub ty: Option<Type>,
    pub default_value: Option<Expr>,
    pub is_variadic: bool,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumDef {
    pub name: Ident,
    pub generic_params: Option<Vec<GenericParam>>,
    pub where_clause: Option<WhereClause>,
    pub variants: Vec<EnumVariant>,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumVariant {
    pub name: Ident,
    pub kind: EnumVariantKind,
    pub span: SourceSpan,
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
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    pub name: Ident,
    pub ty: Type,
    pub visibility: bool,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitDef {
    pub name: Ident,
    pub generic_params: Option<Vec<GenericParam>>,
    pub where_clause: Option<WhereClause>,
    pub supertraits: Vec<TraitBound>,
    pub items: Vec<TraitItem>,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TraitItem {
    Method {
        function: Function,
        span: SourceSpan,
    },
    AssociatedType {
        name: Ident,
        span: SourceSpan,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImplDef {
    pub generic_params: Option<Vec<GenericParam>>,
    pub trait_type: Option<Type>,
    pub self_type: Type,
    pub where_clause: Option<WhereClause>,
    pub items: Vec<ImplItem>,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ImplItem {
    Method(Function),
    AssociatedType {
        name: Ident,
        ty: Type,
        span: SourceSpan,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Module {
    pub name: Ident,
    pub items: Vec<Item>,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UseDecl {
    pub tree: UseTree,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UseTree {
    pub kind: UseTreeKind,
    pub span: SourceSpan,
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
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WherePredicate {
    pub ty: Type,
    pub bounds: Vec<TraitBound>,
    pub span: SourceSpan,
} 