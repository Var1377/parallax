//! MIR statement definitions
//!
//! This module contains data structures for statements in the MIR.

use super::function::{LocalId, MirType};
use super::place::Place;
use std::fmt;

/// A statement in the MIR
#[derive(Debug, Clone)]
pub enum MirStatement {
    /// Assignment of a value to a place
    Assign {
        /// Destination place
        destination: Place,
        /// Source value
        source: Rvalue,
    },
    /// Function call that does not return a value
    CallVoid {
        /// Function to call
        func: Box<Operand>,
        /// Arguments to the function
        args: Vec<Operand>,
    },
    /// Memory allocation (for a variable)
    Alloc {
        /// Place to allocate
        place: Place,
        /// Type of the allocation
        ty: MirType,
        /// Initial value (if any)
        init: Option<Operand>,
    },
    /// No operation (used for padding or placeholders)
    Nop,
}

/// Right-hand side of an assignment
#[derive(Debug, Clone)]
pub enum Rvalue {
    /// Use of a value
    Use(Operand),
    /// Reference to a place
    Ref(Place),
    /// Binary operation
    BinaryOp {
        /// Operation kind
        op: BinOp,
        /// Left operand
        left: Operand,
        /// Right operand
        right: Operand,
    },
    /// Unary operation
    UnaryOp {
        /// Operation kind
        op: UnOp,
        /// Operand
        operand: Operand,
    },
    /// Function call with return value
    Call {
        /// Function to call
        func: Box<Operand>,
        /// Arguments to the function
        args: Vec<Operand>,
    },
    /// Aggregate construction
    Aggregate {
        /// Type of the aggregate
        kind: AggregateKind,
        /// Elements of the aggregate
        elements: Vec<Operand>,
    },
}

/// Binary operation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    /// Addition
    Add,
    /// Subtraction
    Sub,
    /// Multiplication
    Mul,
    /// Division
    Div,
    /// Remainder
    Rem,
    /// Bitwise AND
    BitAnd,
    /// Bitwise OR
    BitOr,
    /// Bitwise XOR
    BitXor,
    /// Left shift
    Shl,
    /// Right shift
    Shr,
    /// Equality comparison
    Eq,
    /// Inequality comparison
    Ne,
    /// Less than comparison
    Lt,
    /// Less than or equal comparison
    Le,
    /// Greater than comparison
    Gt,
    /// Greater than or equal comparison
    Ge,
}

/// Unary operation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    /// Negation
    Neg,
    /// Bitwise NOT
    Not,
}

/// Aggregate kind
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AggregateKind {
    /// Tuple
    Tuple,
    /// Array
    Array,
    /// Structure
    Struct {
        /// Structure name
        name: String,
    },
    /// Enumeration variant
    Enum {
        /// Enumeration name
        name: String,
        /// Variant name
        variant: String,
    },
}

/// An operand in the MIR
#[derive(Debug, Clone)]
pub enum Operand {
    /// Copy of a place
    Copy(Place),
    /// Move of a place
    Move(Place),
    /// Constant value
    Constant(Constant),
}

/// A constant value
#[derive(Debug, Clone)]
pub enum Constant {
    /// Integer constant
    Int(i64),
    /// Floating-point constant
    Float(f64),
    /// Boolean constant
    Bool(bool),
    /// Character constant
    Char(char),
    /// String constant
    String(String),
    /// Unit value
    Unit,
    /// Function reference
    Function(String),
    /// Named constant
    Named(String),
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Rem => write!(f, "%"),
            BinOp::BitAnd => write!(f, "&"),
            BinOp::BitOr => write!(f, "|"),
            BinOp::BitXor => write!(f, "^"),
            BinOp::Shl => write!(f, "<<"),
            BinOp::Shr => write!(f, ">>"),
            BinOp::Eq => write!(f, "=="),
            BinOp::Ne => write!(f, "!="),
            BinOp::Lt => write!(f, "<"),
            BinOp::Le => write!(f, "<="),
            BinOp::Gt => write!(f, ">"),
            BinOp::Ge => write!(f, ">="),
        }
    }
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnOp::Neg => write!(f, "-"),
            UnOp::Not => write!(f, "!"),
        }
    }
} 