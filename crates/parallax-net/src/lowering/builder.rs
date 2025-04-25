use std::collections::HashMap;

use crate::node::{Async, Constructor, Duplicator, Eraser, NodeType, Number, Static, Switch, Pointer};
use crate::port::Port;
use parallax_mir::mir::{MirGraph, NodeId, PortIndex, MirNode};
use parallax_resolve::types::Symbol;

use super::config::InitialNetConfig;
use super::error::LoweringError;
// use parallax_mir::mir::{LiteralValue, OpKind, SsaVarId}; // Commented out unresolved imports

/// Helper struct to manage the state during the lowering of a single MirGraph.
pub struct NetBuilder<'a> {
    /// Reference to the MIR graph being lowered.
    pub graph: &'a MirGraph,
    /// The interaction net configuration being built.
    pub config: super::InitialNetConfig,
    /// Mapping from (MirNodeId, MirPortIndex) to the corresponding interaction net Port.
    pub port_map: HashMap<(NodeId, PortIndex), Port>,
    /// Mapping for nodes that don't have a direct output port mapped (e.g., OpApply constructor)
    pub internal_node_map: HashMap<NodeId, Port>,
    pub partition_id: u16,
    /// Mapping from intrinsic Symbol to its operation code.
    pub intrinsic_op_map: &'a HashMap<Symbol, u64>,
    /// The runtime FunctionId of the function being lowered.
    pub current_function_id: usize,
}

impl<'a> NetBuilder<'a> {
    pub fn new(graph: &'a MirGraph, partition_id: u16, intrinsic_op_map: &'a HashMap<Symbol, u64>, function_id: usize) -> Self {
        NetBuilder {
            graph,
            config: InitialNetConfig::default(),
            port_map: HashMap::new(),
            internal_node_map: HashMap::new(),
            partition_id,
            intrinsic_op_map,
            current_function_id: function_id, // Store function_id
        }
    }

    /// Allocates a new node in the appropriate slab and returns its index and principal port.
    pub fn alloc_constructor(&mut self, node: Constructor) -> (usize, Port) {
        let index = self.config.constructors.insert(node);
        let port = Port::principal(NodeType::Constructor, self.partition_id, index as u64);
        (index, port)
    }

    pub fn alloc_duplicator(&mut self, node: Duplicator) -> (usize, Port) {
        let index = self.config.duplicators.insert(node);
        let port = Port::principal(NodeType::Duplicator, self.partition_id, index as u64);
        (index, port)
    }

    pub fn alloc_static(&mut self, node: Static) -> (usize, Port) {
        let index = self.config.statics.insert(node);
        let port = Port::principal(NodeType::Static, self.partition_id, index as u64);
        (index, port)
    }

    pub fn alloc_number(&mut self, node: Number) -> (usize, Port) {
        let index = self.config.numbers.insert(node);
        let port = Port::principal(NodeType::Number, self.partition_id, index as u64);
        (index, port)
    }

    pub fn alloc_switch(&mut self, node: Switch) -> (usize, Port) {
        let index = self.config.switches.insert(node);
        let port = Port::principal(NodeType::Switch, self.partition_id, index as u64);
        (index, port)
    }

    pub fn alloc_async(&mut self, node: Async) -> (usize, Port) {
        let index = self.config.asyncs.insert(node);
        let port = Port::principal(NodeType::Async, self.partition_id, index as u64);
        (index, port)
    }

    pub fn alloc_eraser(&mut self, node: Eraser) -> (usize, Port) {
        let index = self.config.erasers.insert(node);
        let port = Port::principal(NodeType::Eraser, self.partition_id, index as u64);
        (index, port)
    }

    pub fn alloc_pointer(&mut self, node: Pointer) -> (usize, Port) {
        let index = self.config.pointers.insert(node);
        let port = Port::principal(NodeType::Pointer, self.partition_id, index as u64);
        (index, port)
    }

    /// Registers the mapping from an MIR port to a net port.
    pub fn map_output_port(&mut self, mir_node_id: NodeId, mir_port_index: PortIndex, net_port: Port) {
        self.port_map.insert((mir_node_id, mir_port_index), net_port);
    }

    pub fn map_internal_node_port(&mut self, mir_node_id: NodeId, net_port: Port) {
        self.internal_node_map.insert(mir_node_id, net_port);
    }

    /// Retrieves the net port corresponding to an MIR port.
    pub fn get_output_net_port(&self, mir_node_id: NodeId, mir_port_index: PortIndex) -> Result<Port, LoweringError> {
        self.port_map.get(&(mir_node_id, mir_port_index)).cloned().ok_or_else(|| {
            let edge_context = self.graph.edges.iter().find(|e| (e.from_node == mir_node_id && e.from_port == mir_port_index) || (e.to_node == mir_node_id));
            LoweringError::Internal(format!(
                "Could not find mapped output net port for MIR port ({:?}, {:?}). Originating node: {:?}. Edge context: {:?}",
                mir_node_id, mir_port_index, self.graph.nodes.get(&mir_node_id), edge_context
            ))
        })
    }

    pub fn get_internal_node_port(&self, mir_node_id: NodeId) -> Result<Port, LoweringError> {
        self.internal_node_map.get(&mir_node_id).cloned().ok_or_else(|| {
            LoweringError::Internal(format!("Could not find internal mapped port for MIR node {:?}", mir_node_id))
        })
    }

    /// Retrieves the MIR node definition.
    pub fn get_mir_node(&self, node_id: NodeId) -> Result<&MirNode, LoweringError> {
        self.graph.nodes.get(&node_id).ok_or(LoweringError::NodeNotFound(node_id))
    }

    /// Helper to lower nested structures (like tuples, arrays, >2-arity constructors)
    /// Returns the principal port of the head constructor.
    pub fn lower_nested_constructor(&mut self, element_ports: &[Port]) -> Result<Port, LoweringError> {
        if element_ports.is_empty() {
            // How to represent empty tuple/nil? Maybe a specific Ref/Number tag?
            // For now, create a constructor with NULL ports - needs reduction rule.
            let (idx, principal_port) = self.alloc_constructor(Constructor {
                principle: Port::NULL, left: Port::NULL, right: Port::NULL
            });
            self.config.constructors[idx].principle = principal_port;
            // TODO: Need a way to tag this as NIL or use a specific encoding.
            Ok(principal_port)
        } else if element_ports.len() == 1 {
            // Single element, just return its port (no constructor needed?)
            // Or wrap in a unary constructor? Let's wrap for consistency.
            let (idx, principal_port) = self.alloc_constructor(Constructor {
                principle: Port::NULL, left: element_ports[0], right: Port::NULL // Use NULL for second element
            });
            self.config.constructors[idx].principle = principal_port;
            Ok(principal_port)
        } else {
            // Recursively build nested pairs: Cons(a, Cons(b, Cons(c, ...)))
            let tail_port = self.lower_nested_constructor(&element_ports[1..])?;
            let (idx, principal_port) = self.alloc_constructor(Constructor {
                principle: Port::NULL, left: element_ports[0], right: tail_port
            });
            self.config.constructors[idx].principle = principal_port;
            Ok(principal_port)
        }
    }

    /// Consumes the builder and returns the finalized configuration.
    pub(super) fn build(self) -> InitialNetConfig {
        self.config
    }
} 