use super::common::{Ident, Literal};
use miette::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Pattern {
    pub kind: PatternKind,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PatternKind {
    Identifier(Ident),
    Literal(Literal),
    ConstructorWithArgs {
        path: Vec<Ident>,
        args: Box<Pattern>,
    },
    PathPattern {
        path: Vec<Ident>,
    },
    Array(Vec<Pattern>),
    Tuple(Vec<Pattern>),
    Struct {
        path: Vec<Ident>,
        fields: Vec<PatternField>,
    },
    Rest,
    Or(Box<Pattern>, Box<Pattern>),
    Wildcard,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PatternField {
    pub name: Ident,
    pub pattern: Option<Pattern>,
    pub span: SourceSpan,
}

impl Pattern {
    pub fn new(kind: PatternKind, span: SourceSpan) -> Self {
        Self { kind, span }
    }

    pub fn is_identifier(&self) -> bool {
        matches!(self.kind, PatternKind::Identifier(_))
    }

    pub fn is_literal(&self) -> bool {
        matches!(self.kind, PatternKind::Literal(_))
    }

    pub fn is_tuple(&self) -> bool {
        matches!(self.kind, PatternKind::Tuple(_))
    }

    pub fn is_struct(&self) -> bool {
        matches!(self.kind, PatternKind::Struct { .. })
    }

    pub fn is_rest(&self) -> bool {
        matches!(self.kind, PatternKind::Rest)
    }

    pub fn is_wildcard(&self) -> bool {
        matches!(self.kind, PatternKind::Wildcard)
    }
}
