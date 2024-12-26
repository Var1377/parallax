#[derive(Debug, Clone)]
pub struct Book<'s>(pub Vec<NamedNetwork<'s>>);

#[derive(Debug, Clone)]
pub struct NamedNetwork<'s>(pub &'s str, pub Network<'s>);

#[derive(Debug, Clone)]
pub struct Network<'s>(pub Tree<'s>, pub Vec<Redex<'s>>);

#[derive(Debug, Clone)]
pub struct Redex<'s>(pub bool, pub Tree<'s>, pub Tree<'s>);

#[derive(Debug, Clone)]
pub enum Tree<'s> {
    Variable(&'s str),
    Reference(&'s str),
    Eraser,
    Constructor(Box<Tree<'s>>, Box<Tree<'s>>),
    Duplicator(Box<Tree<'s>>, Box<Tree<'s>>),
    Operator(Box<Tree<'s>>, Box<Tree<'s>>),
    Switch(Box<Tree<'s>>, Box<Tree<'s>>),
    Numeric(Numeric),
}

#[derive(Debug, Clone)]
pub enum Numeric {
    Number(Number),
    Operator(Operation, Option<Number>),
}

#[derive(Debug, Clone)]
pub enum Number {
    Int(i64),
    Float(f64),
}

#[derive(Debug, Clone)]
pub enum Operation {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
    Xor,
    Shr,
    Shl,
    // FpSub,
    // FpDiv,
    // FpShr,
    // FpShl,
}