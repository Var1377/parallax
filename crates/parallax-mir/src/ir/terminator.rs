//! MIR terminator definitions
//!
//! This module contains data structures for terminators in the MIR.
//! Terminators control the flow between basic blocks.

use super::function::BlockId;
use super::statement::Operand;
use super::place::Place;
use std::fmt;

/// A terminator in the MIR
#[derive(Debug, Clone)]
pub enum MirTerminator {
    /// Jump to a target block
    Goto {
        /// Target block
        target: BlockId,
    },
    /// Conditional branch
    Switch {
        /// Discriminant
        discriminant: Operand,
        /// Target blocks (value -> block)
        targets: Vec<(SwitchValue, BlockId)>,
        /// Default target
        default: BlockId,
    },
    /// Return from function
    Return {
        /// Return value (if any)
        value: Option<Operand>,
    },
    /// Unreachable code
    Unreachable,
    /// Function call that may diverge
    Call {
        /// Function to call
        func: Operand,
        /// Arguments to the function
        args: Vec<Operand>,
        /// Destination for return value
        destination: Option<Place>,
        /// Target block for normal return
        target: BlockId,
        /// Target block for unwind
        unwind: Option<BlockId>,
    },
    /// Match statement terminator
    Match {
        /// Scrutinee value
        scrutinee: Operand,
        /// Arms of the match
        arms: Vec<MatchArm>,
        /// Default arm (if no patterns match)
        default_arm: Option<BlockId>,
    },
}

/// A switch value
#[derive(Debug, Clone, PartialEq)]
pub enum SwitchValue {
    /// Integer value
    Int(i64),
    /// Boolean value
    Bool(bool),
    /// String value
    String(String),
    /// Enumeration variant
    Variant {
        /// Enumeration name
        enum_name: String,
        /// Variant name
        variant_name: String,
    },
}

/// A match arm
#[derive(Debug, Clone)]
pub struct MatchArm {
    /// Pattern to match
    pub pattern: MatchPattern,
    /// Target block if pattern matches
    pub target: BlockId,
    /// Guard condition (if any)
    pub guard: Option<Operand>,
}

/// A match pattern
#[derive(Debug, Clone)]
pub enum MatchPattern {
    /// Match a constant value
    Constant(SwitchValue),
    /// Match a tuple structure
    Tuple(Vec<MatchPattern>),
    /// Match a struct
    Struct {
        /// Struct name
        name: String,
        /// Field patterns
        fields: Vec<(String, MatchPattern)>,
    },
    /// Match an enumeration variant
    Variant {
        /// Enumeration name
        enum_name: String,
        /// Variant name
        variant_name: String,
        /// Subpatterns for variant fields
        fields: Vec<MatchPattern>,
    },
    /// Match a value and bind it to a name
    Binding {
        /// Name to bind to
        name: String,
        /// Subpattern (if any)
        subpattern: Option<Box<MatchPattern>>,
    },
    /// Match any value
    Wildcard,
    /// Match a range of values
    Range {
        /// Start of the range (inclusive)
        start: Box<SwitchValue>,
        /// End of the range (inclusive)
        end: Box<SwitchValue>,
    },
    /// Match an or-pattern
    Or(Vec<MatchPattern>),
}

impl fmt::Display for SwitchValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SwitchValue::Int(value) => write!(f, "{}", value),
            SwitchValue::Bool(value) => write!(f, "{}", value),
            SwitchValue::String(value) => write!(f, "\"{}\"", value),
            SwitchValue::Variant { enum_name, variant_name } => 
                write!(f, "{}::{}", enum_name, variant_name),
        }
    }
} 