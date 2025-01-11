use super::{Port, NodeType};
use super::strings::IString;

/// A node in the interaction net
#[derive(Debug, Clone)]
pub struct Node {
    /// The unique identifier of this node
    id: u32,
    /// The type of this node
    node_type: NodeType,
    /// The ports of this node
    pub(crate) ports: Vec<Port>,
}

impl Node {
    /// Creates a new node with the given type
    pub fn new(id: u32, node_type: NodeType) -> Self {
        let port_count = match &node_type {
            NodeType::Variable(_) => 1,
            NodeType::Reference(_) => 1,
            NodeType::Eraser => 1,
            NodeType::Constructor(_) => 3,
            NodeType::Duplicator => 3,
            NodeType::Number(_) => 1,
            NodeType::Operator(_) => 3,
            NodeType::Switch => 3,
        };

        Self {
            id,
            node_type,
            ports: vec![Port::new(); port_count],
        }
    }

    /// Gets the unique identifier of this node
    pub fn id(&self) -> u32 {
        self.id
    }

    /// Gets the type of this node
    pub fn node_type(&self) -> &NodeType {
        &self.node_type
    }

    /// Gets the number of ports on this node
    pub fn port_count(&self) -> usize {
        match &self.node_type {
            NodeType::Number(_) => 1,
            NodeType::Operator(_) => 3,
            NodeType::Constructor(_) => 3,
            NodeType::Eraser => 1,
            NodeType::Duplicator => 3,
            NodeType::Reference(_) => 1,
            NodeType::Variable(_) => 1,
            NodeType::Switch => 3,
        }
    }

    /// Gets a reference to a port
    pub fn port(&self, idx: usize) -> Option<&Port> {
        self.ports.get(idx)
    }

    /// Gets a mutable reference to a port
    pub fn port_mut(&mut self, idx: usize) -> Option<&mut Port> {
        self.ports.get_mut(idx)
    }

    pub fn duplicate(&self, name: IString) -> (Node, Node) {
        let original = Node {
            id: self.id,
            node_type: NodeType::Constructor(name.clone()),
            ports: vec![Port::default(); 3],
        };
        let copy = Node {
            id: self.id + 1,
            node_type: NodeType::Constructor(name),
            ports: vec![Port::default(); 3],
        };
        (original, copy)
    }
}