use std::collections::HashMap;
use super::{Node, NodeType, IString, StringPool};

/// A redex (reducible expression) in the graph
#[derive(Debug)]
pub struct Redex {
    /// The left node of the redex
    pub left: u32,
    /// The port on the left node
    pub left_port: usize,
    /// The right node of the redex
    pub right: u32,
    /// The port on the right node
    pub right_port: usize,
}

/// A definition in the book
#[derive(Debug, Clone)]
pub struct Definition {
    /// The root node of the definition's network
    pub root_id: u32,
    /// The nodes in the definition's network
    pub nodes: Vec<u32>,
}

/// The state of the interaction net machine
#[derive(Debug)]
pub struct GlobalNetwork {
    /// All nodes in the graph
    pub(crate) nodes: HashMap<u32, Node>,
    /// Pending redexes to process
    pub(crate) redexes: Vec<Redex>,
    /// Next available node ID
    next_id: u32,
    /// String pool for interning
    strings: StringPool,
    /// Global definitions from the book
    pub(crate) definitions: HashMap<IString, Definition>,
}

impl GlobalNetwork {
    /// Creates a new empty network
    pub fn new() -> Self {
        Self {
            nodes: HashMap::new(),
            redexes: Vec::new(),
            next_id: 0,
            strings: StringPool::new(),
            definitions: HashMap::new(),
        }
    }

    /// Creates a new node and returns its ID
    pub fn add_node(&mut self, node_type: NodeType) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        self.nodes.insert(id, Node::new(id, node_type));
        id
    }

    /// Gets a reference to a node
    pub fn get_node(&self, id: u32) -> Option<&Node> {
        self.nodes.get(&id)
    }

    /// Gets a mutable reference to a node
    pub fn get_node_mut(&mut self, id: u32) -> Option<&mut Node> {
        self.nodes.get_mut(&id)
    }

    /// Removes a node from the graph
    pub fn remove_node(&mut self, id: u32) -> Option<Node> {
        self.nodes.remove(&id)
    }

    /// Interns a string and returns its unique ID
    pub fn intern(&mut self, s: &str) -> IString {
        self.strings.intern(s)
    }

    /// Gets the string for a given ID
    pub fn resolve(&self, id: IString) -> Option<&str> {
        self.strings.resolve(id)
    }

    /// Connects two ports and potentially creates a redex
    pub fn connect(&mut self, from: (u32, usize), to: (u32, usize)) {
        // Connect the ports
        if let Some(node) = self.get_node_mut(from.0) {
            if let Some(port) = node.port_mut(from.1) {
                port.connect(to.0);
            }
        }
        if let Some(node) = self.get_node_mut(to.0) {
            if let Some(port) = node.port_mut(to.1) {
                port.connect(from.0);
            }
        }

        // Check if this creates a redex
        if let (Some(n1), Some(n2)) = (self.get_node(from.0), self.get_node(to.0)) {
            if !matches!(n1.node_type(), NodeType::Variable(_)) || 
               !matches!(n2.node_type(), NodeType::Variable(_)) {
                self.redexes.push(Redex {
                    left: from.0,
                    left_port: from.1,
                    right: to.0,
                    right_port: to.1,
                });
            }
        }
    }

    /// Gets the next redex to process
    pub fn next_redex(&mut self) -> Option<Redex> {
        self.redexes.pop()
    }

    /// Adds a redex to be processed
    pub fn add_redex(&mut self, redex: Redex) {
        self.redexes.push(redex);
    }

    /// Checks if there are any redexes left to process
    pub fn has_redexes(&self) -> bool {
        !self.redexes.is_empty()
    }

    /// Gets the number of nodes in the graph
    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }

    /// Gets the number of pending redexes
    pub fn redex_count(&self) -> usize {
        self.redexes.len()
    }

    /// Adds a definition to the network
    pub fn add_definition(&mut self, name: IString, root_id: u32, nodes: Vec<u32>) {
        let def = Definition { root_id, nodes };
        self.definitions.insert(name, def);
    }

    /// Gets a definition by name
    pub fn get_definition(&self, name: IString) -> Option<&Definition> {
        self.definitions.get(&name)
    }

    /// Safely removes a list of nodes, ensuring all connections are properly cleaned up
    pub fn safely_remove_nodes(&mut self, nodes: Vec<u32>) {
        // Check if each node can be safely removed
        let nodes_to_remove: Vec<u32> = nodes.into_iter()
            .filter(|&node_id| {
                if let Some(node) = self.get_node(node_id) {
                    // A node can be removed if it has no remaining connections
                    !node.ports.iter()
                        .any(|port| port.connected_to().is_some())
                } else {
                    false
                }
            })
            .collect();

        // First disconnect all ports of nodes that will be removed
        for &node_id in &nodes_to_remove {
            if let Some(node) = self.get_node_mut(node_id) {
                for port_idx in 0..node.port_count() {
                    if let Some(port) = node.port_mut(port_idx) {
                        port.disconnect();
                    }
                }
            }
        }

        // Now remove the nodes that are safe to remove
        for &node_id in &nodes_to_remove {
            self.remove_node(node_id);
        }
    }
}