//! IR data structures for MIR
//!
//! This module contains the core data structures for the MIR.

// Re-export main types from submodules
pub use self::function::{
    BasicBlock, BlockId, ControlFlowGraph, FunctionAttribute, FunctionId, LocalId, MirFunction,
    MirFunctionSignature, MirLocalDecl, MirType,
};
pub use self::place::{Place, ProjectionElem};
pub use self::statement::{
    AggregateKind, BinOp, Constant, MirStatement, Operand, Rvalue, UnOp,
};
pub use self::terminator::{
    MatchArm, MatchPattern, MirTerminator, SwitchValue,
};
pub use self::visit::{MirCounter, MirVisitor};

// Public submodules
pub mod function;
pub mod place;
pub mod statement;
pub mod terminator;
pub mod visit; 