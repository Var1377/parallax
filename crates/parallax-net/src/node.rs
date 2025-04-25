use std::sync::atomic::{AtomicU64, Ordering};

// Re-export Port from the port module
pub use crate::port::Port;

/// A redex represents a potential interaction between two ports
/// 
/// A redex is a pair of ports that can potentially interact according to
/// the rules of the interaction net. The first port is the active port
/// that initiates the interaction, and the second port is the passive port
/// that receives it.
#[derive(Debug, Clone, Copy)]
pub struct Redex(pub Port, pub Port);

/// The different types of nodes in the interaction net
/// 
/// Each node type has specific reduction rules and port configurations:
/// - Eraser: Removes nodes and connections
/// - Constructor: Creates compound data structures
/// - Duplicator: Copies data
/// - Static: Represents global functions, constants, or predefined operations (e.g., projection).
///           Its `data` field encodes the specific entity or operation.
/// - Number: Stores numeric values
/// - Switch: Implements control flow
/// - Async: Handles asynchronous operations
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeType {
    Eraser = 0,
    Constructor = 1,
    Duplicator = 2,
    Static = 3,
    Number = 4,
    Switch = 5,
    Async = 6,
    Pointer = 7,
}

impl NodeType {
    /// Attempts to convert a u8 to a NodeType.
    #[inline(always)]
    pub fn from_u8(value: u8) -> Option<Self> {
        match value {
            0 => Some(NodeType::Eraser),
            1 => Some(NodeType::Constructor),
            2 => Some(NodeType::Duplicator),
            3 => Some(NodeType::Static),
            4 => Some(NodeType::Number),
            5 => Some(NodeType::Switch),
            6 => Some(NodeType::Async),
            7 => Some(NodeType::Pointer),
            _ => None,
        }
    }
}

/// An eraser node simply annihilates pairs it interacts with.
/// It has one principal port.
#[derive(Debug, Clone)]
pub struct Eraser {
    pub principle: Port,
}

impl Eraser {
    #[inline(always)]
    pub fn new_null() -> Self {
        Self { principle: Port::NULL }
    }
}

/// A constructor node that creates compound data structures
/// 
/// A constructor has three ports:
/// - principle: The main port for interaction
/// - left: The left auxiliary port
/// - right: The right auxiliary port
#[derive(Debug, Clone)]
pub struct Constructor {
    pub principle: Port,
    pub left: Port,
    pub right: Port,
}

impl Constructor {
    #[inline(always)]
    pub fn new_null() -> Self {
        Self { principle: Port::NULL, left: Port::NULL, right: Port::NULL }
    }
}

/// A duplicator node that copies data
/// 
/// A duplicator has three ports:
/// - principle: The main port for interaction
/// - left: The left auxiliary port
/// - right: The right auxiliary port
#[derive(Debug, Clone)]
pub struct Duplicator {
    pub principle: Port,
    pub left: Port,
    pub right: Port,
}

impl Duplicator {
    #[inline(always)]
    pub fn new_null() -> Self {
        Self { principle: Port::NULL, left: Port::NULL, right: Port::NULL }
    }
}

/// A static node representing a global function, constant, or predefined operation.
///
/// Its behavior is determined by the interaction rule corresponding to the entity/operation
/// encoded in its `data` field.
///
/// - principle: The main port for interaction
/// - data: An atomic value encoding the FunctionId, GlobalId, or OperationType + associated data.
#[derive(Debug)]
pub struct Static {
    pub principle: Port,
    pub data: AtomicU64,
}

impl Static {
    #[inline(always)]
    pub fn new_null() -> Self {
        Self { principle: Port::NULL, data: AtomicU64::new(0) } // Default data to 0
    }
}

/// A number node that stores numeric values
/// 
/// A number has two ports:
/// - principle: The main port for interaction
/// - data: A 128-bit value that can be read and written.
///       Note: Not atomic, reductions must ensure exclusive access via partition ownership.
#[derive(Debug, Clone, Copy)]
pub struct Number {
    pub principle: Port,
    pub data: u128,
}

impl Number {
    #[inline(always)]
    pub fn new_null() -> Self {
        Self { principle: Port::NULL, data: 0 }
    }
}

/// A switch node that implements control flow
/// 
/// A switch has three ports:
/// - principle: The main port for interaction
/// - left: The left branch port
/// - right: The right branch port
#[derive(Debug, Clone)]
pub struct Switch {
    pub principle: Port,
    pub left: Port,
    pub right: Port,
}

impl Switch {
    #[inline(always)]
    pub fn new_null() -> Self {
        Self { principle: Port::NULL, left: Port::NULL, right: Port::NULL }
    }
}

/// An async node that handles asynchronous operations
/// 
/// An async has two ports:
/// - principle: The main port for interaction
/// - id: The id of the async operation
#[derive(Debug, Clone)]
pub struct Async {
    pub principle: Port,
    pub id: usize,
}

impl Async {
    #[inline(always)]
    pub fn new_null() -> Self {
        Self { principle: Port::NULL, id: 0 } // Default id to 0
    }
}

/// A pointer node that points to some memory location
#[derive(Debug, Clone)]
pub struct Pointer {
    pub principle: Port,
    pub data: (), // TODO: Add data type
}


