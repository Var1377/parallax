use super::common::{Ident, Span, Literal};
use super::pattern::Pattern;
use super::types::Type;
use super::items::Parameter;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind {
    Block(Vec<Expr>),
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
    },
    Match {
        scrutinee: Box<Expr>,
        arms: Vec<(Pattern, Expr)>,
    },
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Call {
        func: Box<Expr>,
        args: Vec<Argument>,
    },
    Lambda {
        generic_params: Option<Vec<GenericParam>>,
        params: Vec<Parameter>,
        body: Box<Expr>,
    },
    Literal(Literal),
    Path(Vec<Ident>),
    Field {
        object: Box<Expr>,
        name: Ident,
    },
    Array(Vec<Expr>),
    Tuple(Vec<Expr>),
    Map(Vec<(Expr, Expr)>),
    HashSet(Vec<Expr>),
    Let {
        pattern: Pattern,
        type_ann: Option<Type>,
        value: Box<Expr>,
    },
    Struct {
        path: Vec<Ident>,
        fields: Vec<(Ident, Expr)>,
        base: Option<Box<Expr>>,
    },
    Paren(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GenericParam {
    pub is_phantom: bool,
    pub name: Ident,
    pub kind: Option<Kind>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Kind {
    Star,
    Function(Box<Kind>, Box<Kind>),
    Tuple(Vec<Kind>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Lt,
    Gt,
    And,
    Or,
    Arrow,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Neg,  // -
    Not,  // !
    Ref,  // &
    Deref, // *
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Argument {
    pub name: Option<Ident>,
    pub value: Expr,
    pub span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn is_literal(&self) -> bool {
        matches!(self.kind, ExprKind::Literal(_))
    }

    pub fn is_path(&self) -> bool {
        matches!(self.kind, ExprKind::Path(_))
    }
}