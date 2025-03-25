//! HIR data structures for the Parallax compiler
//!
//! This module defines the High-level Intermediate Representation (HIR)
//! data structures used by the Parallax compiler.

use std::sync::Arc;
use parallax_lang::ast;
use parallax_typeck::context::{Ty, ConcreteTy};

/// The root HIR node representing a whole crate
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Crate {
    /// The items defined in the crate
    pub items: Vec<Item>,
}

/// An item in the HIR
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Item {
    /// The kind of item
    pub kind: ItemKind,
    /// The source span of the item
    pub span: ast::Span,
    /// The type of the item
    pub ty: Ty,
}

/// The different kinds of items in the HIR
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ItemKind {
    /// A function definition
    Function(Function),
    /// A struct definition
    Struct(Struct),
    /// A module definition
    Module(Module),
    /// An enum definition
    Enum(Enum),
    /// A trait definition
    Trait(Trait),
    /// A type alias
    TypeAlias(TypeAlias),
    /// A constant declaration
    Const(Const),
}

/// A function definition
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Function {
    /// The name of the function
    pub name: String,
    /// The parameters of the function
    pub params: Vec<Param>,
    /// The return type of the function
    pub return_ty: Ty,
    /// The body of the function
    pub body: Expr,
    /// Generic parameters (if any)
    pub generic_params: Vec<GenericParam>,
    /// Where clause constraints (if any)
    pub where_clause: Vec<WhereClause>,
}

/// A struct definition
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Struct {
    /// The name of the struct
    pub name: String,
    /// The fields of the struct
    pub fields: Vec<Field>,
    /// Generic parameters (if any)
    pub generic_params: Vec<GenericParam>,
    /// Where clause constraints (if any)
    pub where_clause: Vec<WhereClause>,
}

/// A module definition
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Module {
    /// The name of the module
    pub name: String,
    /// The items defined in the module
    pub items: Vec<Item>,
}

/// An enum definition
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Enum {
    /// The name of the enum
    pub name: String,
    /// The variants of the enum
    pub variants: Vec<EnumVariant>,
    /// Generic parameters (if any)
    pub generic_params: Vec<GenericParam>,
    /// Where clause constraints (if any)
    pub where_clause: Vec<WhereClause>,
}

/// An enum variant
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct EnumVariant {
    /// The name of the variant
    pub name: String,
    /// The fields of the variant (if any)
    pub fields: Option<Vec<Field>>,
}

/// A trait definition
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Trait {
    /// The name of the trait
    pub name: String,
    /// The items defined in the trait
    pub items: Vec<Item>,
    /// Generic parameters (if any)
    pub generic_params: Vec<GenericParam>,
    /// Where clause constraints (if any)
    pub where_clause: Vec<WhereClause>,
}

/// A type alias
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypeAlias {
    /// The name of the type alias
    pub name: String,
    /// The type being aliased
    pub ty: Ty,
    /// Generic parameters (if any)
    pub generic_params: Vec<GenericParam>,
    /// Where clause constraints (if any)
    pub where_clause: Vec<WhereClause>,
}

/// A constant declaration
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Const {
    /// The name of the constant
    pub name: String,
    /// The type of the constant
    pub ty: Ty,
    /// The initializer expression
    pub initializer: Expr,
}

/// A struct or enum field
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Field {
    /// The name of the field
    pub name: String,
    /// The type of the field
    pub ty: Ty,
}

/// A function parameter
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Param {
    /// The name of the parameter
    pub name: String,
    /// The type of the parameter
    pub ty: Ty,
}

/// A generic type parameter
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct GenericParam {
    /// The name of the parameter
    pub name: String,
    /// The bounds on the parameter (trait constraints)
    pub bounds: Vec<String>,
}

/// A where clause constraint
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct WhereClause {
    /// The type being constrained
    pub ty: Ty,
    /// The trait bounds
    pub bounds: Vec<String>,
}

/// An expression in the HIR
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Expr {
    /// The kind of expression
    pub kind: ExprKind,
    /// The type of the expression
    pub ty: Ty,
    /// The source span of the expression
    pub span: ast::Span,
}

/// The different kinds of expressions in the HIR
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ExprKind {
    /// A literal value
    Literal(ast::Literal),
    /// A function call
    Call { 
        func: Box<Expr>, 
        args: Vec<Expr> 
    },
    /// A binary operation
    Binary { 
        left: Box<Expr>, 
        op: ast::BinaryOp, 
        right: Box<Expr> 
    },
    /// A unary operation
    Unary { 
        op: ast::expr::UnaryOp, 
        expr: Box<Expr> 
    },
    /// A field access
    Field { 
        object: Box<Expr>, 
        name: String 
    },
    /// A path reference (variable, function, etc.)
    Path(String),
    /// A block of expressions
    Block(Vec<Expr>),
    /// An if expression
    If { 
        condition: Box<Expr>, 
        then_branch: Box<Expr>, 
        else_branch: Option<Box<Expr>> 
    },
    /// A match expression
    Match { 
        scrutinee: Box<Expr>, 
        arms: Vec<MatchArm> 
    },
    /// A struct construction expression
    Struct { 
        name: String, 
        fields: Vec<(String, Expr)>, 
        base: Option<Box<Expr>> 
    },
    /// A tuple construction expression
    Tuple(Vec<Expr>),
    /// An array construction expression
    Array(Vec<Expr>),
    /// A lambda expression
    Lambda { 
        params: Vec<Param>, 
        body: Box<Expr> 
    },
    /// A let binding expression
    Let { 
        pattern: Pattern, 
        init: Box<Expr> 
    },
    /// An assignment expression
    Assign { 
        target: Box<Expr>, 
        value: Box<Expr> 
    },
}

/// A match arm
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MatchArm {
    /// The pattern to match against
    pub pattern: Pattern,
    /// The expression to evaluate if the pattern matches
    pub expr: Expr,
}

/// A pattern in the HIR
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Pattern {
    /// The kind of pattern
    pub kind: PatternKind,
    /// The type of the pattern
    pub ty: Ty,
    /// The source span of the pattern
    pub span: ast::Span,
}

/// The different kinds of patterns in the HIR
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PatternKind {
    /// A wildcard pattern (_)
    Wildcard,
    /// An identifier pattern (binding)
    Identifier(String),
    /// A literal pattern
    Literal(ast::Literal),
    /// A struct pattern
    Struct { 
        name: String, 
        fields: Vec<(String, Pattern)>, 
        rest: bool 
    },
    /// A tuple pattern
    Tuple(Vec<Pattern>),
    /// An array pattern
    Array(Vec<Pattern>),
    /// An enum variant pattern
    Variant { 
        name: String, 
        fields: Vec<Pattern> 
    },
} 