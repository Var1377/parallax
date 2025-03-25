use super::common::{Ident, Span};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Path(Vec<Ident>),
    Function(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Array(Box<Type>, usize),
    KindApp(Box<Type>, Vec<Type>),
}

impl Type {
    pub fn new(kind: TypeKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn is_function(&self) -> bool {
        matches!(self.kind, TypeKind::Function(_, _))
    }

    pub fn is_tuple(&self) -> bool {
        matches!(self.kind, TypeKind::Tuple(_))
    }
}