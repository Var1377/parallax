use parallax_mir::mir::{MirEdge, MirNode, MirType, NodeId};
use thiserror::Error;
use crate::port::Port;

/// Error type for MIR lowering process.
#[derive(Debug, Error)]
pub enum LoweringError {
    #[error("Unsupported MIR node type: {0:?}")]
    UnsupportedNode(MirNode),
    #[error("Unsupported MIR type: {0:?}")]
    UnsupportedType(MirType),
    #[error("Invalid MIR structure: {0}")]
    InvalidMir(String),
    #[error("Failed to find source/target port for MIR edge: {0:?}")]
    PortNotFound(MirEdge),
    #[error("Failed to connect net ports {0:?} and {1:?}")]
    EdgeConnectionFailed(Port, Port),
    #[error("Missing node definition during lowering for node ID: {0:?}")]
    NodeNotFound(NodeId),
    #[error("Lowering for {0} not implemented yet")]
    NotImplemented(&'static str),
    #[error("Internal error during lowering: {0}")]
    Internal(String),
} 