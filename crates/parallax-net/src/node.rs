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
/// - Ref: Provides mutable references
/// - Number: Stores numeric values
/// - Switch: Implements control flow
/// - Async: Handles asynchronous operations
#[repr(u8)]
pub enum NodeType {
    Eraser = 0,
    Constructor = 1,
    Duplicator = 2,
    Ref = 3,
    Number = 4,
    Switch = 5,
    Async = 6,
}

/// A constructor node that creates compound data structures
/// 
/// A constructor has three ports:
/// - principle: The main port for interaction
/// - left: The left auxiliary port
/// - right: The right auxiliary port
pub struct Constructor {
    pub principle: Port,
    pub left: Port,
    pub right: Port,
}

/// A duplicator node that copies data
/// 
/// A duplicator has three ports:
/// - principle: The main port for interaction
/// - left: The left auxiliary port
/// - right: The right auxiliary port
pub struct Duplicator {
    pub principle: Port,
    pub left: Port,
    pub right: Port,
}

/// A reference node that provides mutable storage
/// 
/// A ref has two ports:
/// - principle: The main port for interaction
/// - data: An atomic value that can be read and written
pub struct Ref {
    pub principle: Port,
    pub data: AtomicU64,
}

/// A number node that stores numeric values
/// 
/// A number has two ports:
/// - principle: The main port for interaction
/// - data: An atomic value that can be read and written
pub struct Number {
    pub principle: Port,
    pub data: AtomicU64,
}

/// A switch node that implements control flow
/// 
/// A switch has three ports:
/// - principle: The main port for interaction
/// - left: The left branch port
/// - right: The right branch port
pub struct Switch {
    pub principle: Port,
    pub left: Port,
    pub right: Port,
}

/// An async node that handles asynchronous operations
/// 
/// An async has two ports:
/// - principle: The main port for interaction
/// - data: Additional data needed for the async operation
pub struct Async {
    pub principle: Port,
    pub data: usize,
}

impl Constructor {
    /// Creates a new constructor node
    pub fn new(principle: Port, left: Port, right: Port) -> Self {
        Self { principle, left, right }
    }
}

impl Duplicator {
    /// Creates a new duplicator node
    pub fn new(principle: Port, left: Port, right: Port) -> Self {
        Self { principle, left, right }
    }
}

impl Ref {
    /// Creates a new ref node
    pub fn new(principle: Port, data: u64) -> Self {
        Self { 
            principle, 
            data: AtomicU64::new(data),
        }
    }

    /// Gets the current value of the ref
    pub fn get_data(&self) -> u64 {
        self.data.load(Ordering::Relaxed)
    }

    /// Sets the value of the ref
    pub fn set_data(&self, value: u64) {
        self.data.store(value, Ordering::Relaxed);
    }
}

impl Number {
    /// Creates a new number node
    pub fn new(principle: Port, data: u64) -> Self {
        Self { 
            principle, 
            data: AtomicU64::new(data),
        }
    }

    /// Gets the current value of the number
    pub fn get_data(&self) -> u64 {
        self.data.load(Ordering::Relaxed)
    }

    /// Sets the value of the number
    pub fn set_data(&self, value: u64) {
        self.data.store(value, Ordering::Relaxed);
    }
}

impl Switch {
    /// Creates a new switch node
    pub fn new(principle: Port, left: Port, right: Port) -> Self {
        Self { principle, left, right }
    }
}

impl Async {
    /// Creates a new async node
    pub fn new(principle: Port, data: usize) -> Self {
        Self { 
            principle, 
            data,
        }
    }
}
