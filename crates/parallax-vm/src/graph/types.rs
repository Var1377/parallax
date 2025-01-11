use super::strings::IString;

/// The type of a node, determining its behavior during reduction
#[derive(Debug, Clone, PartialEq)]
pub enum NodeType {
    /// A variable that can be substituted
    Variable(IString),
    /// A reference to a global definition
    Reference(IString),
    /// An eraser that deletes what it connects to
    Eraser,
    /// A constructor for building data
    Constructor(IString),
    /// A duplicator for sharing subgraphs
    Duplicator,
    /// A numeric value
    Number(i64),
    /// A binary operator
    Operator(Operator),
    /// A switch for pattern matching
    Switch,
}

/// Available binary operators
#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    // Arithmetic
    Add,
    Sub,
    FlippedSub,  // For n - m, represents m - n
    Mul,
    Div,
    FlippedDiv,  // For n / m, represents m / n
    Mod,
    FlippedMod,  // For n % m, represents m % n
    // Comparison
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    // Bitwise
    And,
    Or,
    Xor,
    Shl,
    FlippedShl,  // For n << m, represents m << n
    Shr,
    FlippedShr,  // For n >> m, represents m >> n
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Add => write!(f, "+"),
            Operator::Sub => write!(f, "-"),
            Operator::FlippedSub => write!(f, "-"),
            Operator::Mul => write!(f, "*"),
            Operator::Div => write!(f, "/"),
            Operator::FlippedDiv => write!(f, "/"),
            Operator::Mod => write!(f, "%"),
            Operator::FlippedMod => write!(f, "%"),
            Operator::And => write!(f, "&"),
            Operator::Or => write!(f, "|"),
            Operator::Xor => write!(f, "^"),
            Operator::Shl => write!(f, "<<"),
            Operator::FlippedShl => write!(f, "<<"),
            Operator::Shr => write!(f, ">>"),
            Operator::FlippedShr => write!(f, ">>"),
            Operator::Eq => write!(f, "=="),
            Operator::Ne => write!(f, "!="),
            Operator::Lt => write!(f, "<"),
            Operator::Gt => write!(f, ">"),
            Operator::Le => write!(f, "<="),
            Operator::Ge => write!(f, ">="),
        }
    }
}