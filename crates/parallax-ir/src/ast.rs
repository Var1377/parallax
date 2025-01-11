/// The grammar references:
/// <Book>, <Net>, <Redex>, <Tree>, <Node>, <Numeric>, <Number>, <Operator>, <Operation>, etc.

#[derive(Debug, PartialEq)]
pub struct Book<'a> {
    // A list of definitions: @<name> = <Net>
    pub definitions: Vec<Definition<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Definition<'a> {
    pub name: &'a str,
    pub net: Net<'a>,
}

#[derive(Debug, PartialEq)]
pub struct Net<'a> {
    pub tree: Tree<'a>,
    pub redexes: Vec<Redex<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Redex<'a> {
    pub strict: bool,  // true if the redex is marked with !
    pub left: Tree<'a>,
    pub right: Tree<'a>,
}

#[derive(Debug, PartialEq)]
pub enum Tree<'a> {
    // <alphanumeric>
    Var(&'a str),

    // <Node>
    Node(Node<'a>),
}

#[derive(Debug, PartialEq)]
pub enum Node<'a> {
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

#[derive(Debug, PartialEq)]
pub enum Numeric<'a> {
    Number(&'a str),            // e.g., "123", "1.23"
    Operator(Operator<'a>),     // e.g., "[+]", "[-2]", etc.
}

#[derive(Debug, PartialEq)]
pub enum Operator<'a> {
    // Unapplied: "[" <Operation> "]"
    Unapplied(&'a str),
    // Partially applied: "[" <Operation> <Number> "]"
    PartiallyApplied(&'a str, &'a str),
}
