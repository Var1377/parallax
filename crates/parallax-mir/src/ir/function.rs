//! MIR function definition
//!
//! This module contains data structures for functions and their control flow graphs.

use rustc_hash::FxHashMap;
use std::fmt;
use std::hash::{Hash, Hasher};

// Forward declarations for circular references
use super::statement::MirStatement;
use super::terminator::MirTerminator;

/// A function in the MIR
#[derive(Debug, Clone)]
pub struct MirFunction {
    /// Unique identifier for the function
    pub id: FunctionId,
    /// Function name (for debugging)
    pub name: String,
    /// Function signature
    pub signature: MirFunctionSignature,
    /// Control flow graph (function body)
    pub body: ControlFlowGraph,
    /// Local variable declarations
    pub locals: Vec<MirLocalDecl>,
    /// Function attributes
    pub attributes: Vec<FunctionAttribute>,
}

/// Function signature
#[derive(Debug, Clone)]
pub struct MirFunctionSignature {
    /// Parameter types
    pub params: Vec<MirType>,
    /// Return type
    pub return_type: MirType,
}

/// Control flow graph
#[derive(Debug, Clone)]
pub struct ControlFlowGraph {
    /// Entry block ID
    pub entry_block: BlockId,
    /// Blocks in the CFG
    pub blocks: FxHashMap<BlockId, BasicBlock>,
}

/// A basic block in the CFG
#[derive(Debug, Clone)]
pub struct BasicBlock {
    /// Block ID
    pub id: BlockId,
    /// Statements in the block
    pub statements: Vec<MirStatement>,
    /// Terminator (control flow at the end of the block)
    pub terminator: MirTerminator,
}

/// Local variable declaration
#[derive(Debug, Clone)]
pub struct MirLocalDecl {
    /// Local ID
    pub id: LocalId,
    /// Variable type
    pub ty: MirType,
    /// Original name (for debugging)
    pub name: Option<String>,
}

/// Function attribute
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionAttribute {
    /// Inline hint
    Inline,
    /// Never inline
    NoInline,
    /// Compiler intrinsic
    Intrinsic(String),
    /// Cold (rarely executed)
    Cold,
}

/// MIR type representation
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MirType {
    /// Integer type
    Int,
    /// Floating-point type
    Float,
    /// Boolean type
    Bool,
    /// Character type
    Char,
    /// String type
    String,
    /// Unit type
    Unit,
    /// Named type (struct, enum, etc.)
    Named {
        /// Type name
        name: String,
        /// Type arguments
        args: Vec<MirType>,
    },
    /// Tuple type
    Tuple(Vec<MirType>),
    /// Array type
    Array(Box<MirType>),
    /// Function type
    Function {
        /// Parameter types
        params: Vec<MirType>,
        /// Return type
        ret: Box<MirType>,
    },
    /// Generic parameter
    Generic(String),
    /// Error type (for error recovery)
    Error,
}

// Type aliases for IDs
pub type FunctionId = u32;
pub type BlockId = u32;
pub type LocalId = u32;

// Display implementation for MirType
impl fmt::Display for MirType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MirType::Int => write!(f, "int"),
            MirType::Float => write!(f, "float"),
            MirType::Bool => write!(f, "bool"),
            MirType::Char => write!(f, "char"),
            MirType::String => write!(f, "string"),
            MirType::Unit => write!(f, "()"),
            MirType::Named { name, args } => {
                write!(f, "{}", name)?;
                if !args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", arg)?;
                    }
                    write!(f, ">")?;
                }
                Ok(())
            }
            MirType::Tuple(types) => {
                write!(f, "(")?;
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", ty)?;
                }
                write!(f, ")")
            }
            MirType::Array(elem) => write!(f, "[{}]", elem),
            MirType::Function { params, ret } => {
                write!(f, "(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, ") -> {}", ret)
            }
            MirType::Generic(name) => write!(f, "{}", name),
            MirType::Error => write!(f, "<error>"),
        }
    }
}

impl PartialEq for MirFunction {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.name == other.name
    }
}

impl Eq for MirFunction {}

impl std::hash::Hash for MirFunction {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.name.hash(state);
    }
} 