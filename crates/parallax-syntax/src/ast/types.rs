use super::common::Ident;
use miette::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub kind: TypeKind,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Path(Vec<Ident>),
    Function(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Array(Box<Type>, usize),
    KindApp(Box<Type>, Vec<Type>),
    Never,
}

impl Type {
    pub fn new(kind: TypeKind, span: SourceSpan) -> Self {
        Self { kind, span }
    }

    pub fn is_function(&self) -> bool {
        matches!(self.kind, TypeKind::Function(_, _))
    }

    pub fn is_tuple(&self) -> bool {
        matches!(self.kind, TypeKind::Tuple(_))
    }

    pub fn is_array(&self) -> bool {
        matches!(self.kind, TypeKind::Array(_, _))
    }

    pub fn is_kind_app(&self) -> bool {
        matches!(self.kind, TypeKind::KindApp(_, _))
    }
}