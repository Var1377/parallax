#[derive(Debug)]
pub struct Book<'a> {
    // A list of definitions: @<name> = <Net>
    pub named_nets: Vec<NamedNetwork<'a>>,
}

#[derive(Debug)]
pub struct NamedNetwork<'a> {
    pub name: &'a str,
    pub net: Network<'a>,
}

#[derive(Debug)]
pub struct Network<'a> {
    pub tree: Tree<'a>,
    pub redexes: Vec<Redex<'a>>,
}

#[derive(Debug)]
pub struct Redex<'a> {
    pub safe: bool,  // true if the redex is marked with !
    pub left: Tree<'a>,
    pub right: Tree<'a>,
}

#[derive(Debug, Clone)]
pub enum Tree<'a> {
    // <alphanumeric>
    Variable(&'a str),
    // "*" (ERASER)
    Eraser,
    // "@" <alphanumeric>
    Reference(&'a str),
    // <Numeric>
    Numeric(Numeric<'a>),
    // "(" <Tree> <Tree> ")"
    Constructor(Box<Tree<'a>>, Box<Tree<'a>>),
    // "{" <Tree> <Tree> "}"
    Duplicator(Box<Tree<'a>>, Box<Tree<'a>>),
    // "$(" <Tree> <Tree> ")"
    Operator(Box<Tree<'a>>, Box<Tree<'a>>),
    // "?(" <Tree> <Tree> ")"
    Switch(Box<Tree<'a>>, Box<Tree<'a>>),
}

#[derive(Debug, Clone)]
pub enum Numeric<'a> {
    Number(&'a str),            // e.g., "123", "1.23"
    Operator(Operation<'a>),     // e.g., "[+]", "[-2]", etc.
}

impl<'a> Numeric<'a> {
    pub fn get_str(&'a self) -> Option<&'a str> {
        match self {
            Numeric::Number(num) => Some(num),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    // Arithmetic
    Add,    // "+"
    Sub,    // "-"
    FlippedSub, // ":-"
    Mul,    // "*"
    Div,    // "/"
    FlippedDiv, // ":/"
    Mod,    // "%"
    FlippedMod, // ":%"
    
    // Comparison
    Eq,     // "="
    Ne,     // "!"
    Lt,     // "<"
    Gt,     // ">"
    Le,     // "<="
    Ge,     // ">="
    
    // Bitwise
    And,    // "&"
    Or,     // "|"
    Xor,    // "^"
    Shl,    // "<<"
    FlippedShl, // ":<<"
    Shr,    // ">>"
    FlippedShr, // ":>>"
}


#[derive(Debug, Clone)]
pub enum Operation<'a> {
    // Unapplied: "[" <Operation> "]"
    Unapplied(Operator),
    // Partially applied: "[" <Operation> <Number> "]"
    PartiallyApplied(Operator, &'a str),
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Operator::*;
        match self {
            Add => write!(f, "+"),
            Sub => write!(f, "-"),
            FlippedSub => write!(f, ":-"),
            Mul => write!(f, "*"),
            Div => write!(f, "/"),
            FlippedDiv => write!(f, ":/"),
            Mod => write!(f, "%"),
            FlippedMod => write!(f, ":%"),
            Eq => write!(f, "="),
            Ne => write!(f, "!"),
            Lt => write!(f, "<"),
            Gt => write!(f, ">"),
            Le => write!(f, "<="),
            Ge => write!(f, ">="),
            And => write!(f, "&"),
            Or => write!(f, "|"),
            Xor => write!(f, "^"),
            Shl => write!(f, "<<"),
            FlippedShl => write!(f, ":<<"),
            Shr => write!(f, ">>"),
            FlippedShr => write!(f, ":>>"),
        }
    }
}