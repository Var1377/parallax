use miette::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    pub name: String,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int {
        value: i128,
        suffix: Option<String>,
    },
    Float {
        value: f64,
        suffix: Option<String>,
    },
    String(String),
    Char(char), 
    Bool(bool),
}

impl Eq for Literal {}
impl std::hash::Hash for Literal {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Literal::Int { value, suffix } => {
                value.hash(state);
                suffix.hash(state);
            }
            Literal::Float { value, suffix } => {
                value.to_bits().hash(state);
                suffix.hash(state);
            }
            Literal::String(s) => s.hash(state),
            Literal::Char(c) => c.hash(state),
            Literal::Bool(b) => b.hash(state),
        }
    }
}