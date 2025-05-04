use std::collections::HashMap;

use parallax_net::port::Connection;
use parallax_resolve::types::Symbol;
use super::variable_disconnection::{DisconnectedVariables, Node as DisconnectedNode, NamedNet as DisconnectedNamedNet, Numeric, Operation};

pub struct RemovedVariables {
    pub named_nets: Vec<NamedNet>,
}

pub struct NamedNet {
    pub name: Symbol,
    pub nodes: Vec<Node>,
    pub redexes: Vec<(usize, usize)>,
    pub entry_point: (usize, usize),
}

#[derive(Debug)]
pub enum Node {
    /// An Eraser node with a single main port.
    Eraser {
        /// [main_port]
        ports: [Option<Connection>; 1],
    },
    
    /// A Reference node that points to a named network.
    Reference {
        /// The name of the referenced network.
        name: Symbol,
        
        /// [main_port]
        ports: [Option<Connection>; 1],
    },
    
    /// A Numeric node representing a number or an operator.
    Numeric {
        /// The numeric value or operator.
        value: Numeric,
        
        /// [main_port]
        ports: [Option<Connection>; 1],
    },
    
    /// A Constructor node with one main port and two auxiliary ports.
    Constructor {
        /// [main_port, auxiliary_port_1, auxiliary_port_2]
        ports: [Option<Connection>; 3],
    },
    
    /// A Duplicator node with one main port and two auxiliary ports.
    Duplicator {
        /// [main_port, auxiliary_port_1, auxiliary_port_2]
        ports: [Option<Connection>; 3],
    },
    
    /// An Operator node with one main port and two auxiliary ports.
    Operator {
        /// The specific operation this node represents.
        operation: Operation,
        
        /// [main_port, auxiliary_port_1, auxiliary_port_2]
        ports: [Option<Connection>; 3],
    },
    
    /// A Switch node with one main port and two auxiliary ports.
    Switch {
        /// [main_port, auxiliary_port_1, auxiliary_port_2]
        ports: [Option<Connection>; 3],
    },
}

impl Node {
    pub fn ports(&self) -> &[Option<Connection>] {
        match self {
            Node::Eraser { ports } => ports,
            Node::Reference { ports, .. } => ports,
            Node::Numeric { ports, .. } => ports,
            Node::Constructor { ports } => ports,
            Node::Duplicator { ports } => ports,
            Node::Operator { ports, .. } => ports,
            Node::Switch { ports } => ports,
        }
    }

    pub fn ports_mut(&mut self) -> &mut [Option<Connection>] {
        match self {
            Node::Eraser { ports } => ports,
            Node::Reference { ports, .. } => ports,
            Node::Numeric { ports, .. } => ports,
            Node::Constructor { ports } => ports,
            Node::Duplicator { ports } => ports,
            Node::Operator { ports, .. } => ports,
            Node::Switch { ports } => ports,
        }
    }

    pub fn set_port(&mut self, port: usize, connection: Connection) {
        self.ports_mut()[port] = Some(connection);
    }
}

impl From<DisconnectedVariables> for RemovedVariables {
    fn from(disconnected: DisconnectedVariables) -> Self {
        RemovedVariables {
            named_nets: disconnected.named_nets.into_iter()
                .map(NamedNet::from)
                .collect()
        }
    }
}

impl From<DisconnectedNamedNet> for NamedNet {
    fn from(net: DisconnectedNamedNet) -> Self {
        let mut index_map = HashMap::new();
        let mut new_nodes = Vec::new();
        
        // First pass: Create new nodes and build index mapping
        for (old_idx, node) in net.nodes.iter().enumerate() {
            match node {
                DisconnectedNode::Variable { .. } => {
                    continue;
                }
                _ => {
                    index_map.insert(old_idx, new_nodes.len());
                    
                    // Create the new node, copying Symbol for Reference
                    new_nodes.push(match node {
                        DisconnectedNode::Eraser { .. } => {
                            Node::Eraser { ports: [None] }
                        }
                        DisconnectedNode::Reference { name, .. } => {
                            Node::Reference { name: *name, ports: [None] }
                        }
                        DisconnectedNode::Numeric { value, .. } => {
                            Node::Numeric { value: value.clone(), ports: [None] }
                        }
                        DisconnectedNode::Constructor { .. } => {
                            Node::Constructor { ports: [None, None, None] }
                        }
                        DisconnectedNode::Duplicator { .. } => {
                            Node::Duplicator { ports: [None, None, None] }
                        }
                        DisconnectedNode::Operator { operation, .. } => {
                            Node::Operator { operation: operation.clone(), ports: [None, None, None] }
                        }
                        DisconnectedNode::Switch { .. } => {
                            Node::Switch { ports: [None, None, None] }
                        }
                        DisconnectedNode::Variable { .. } => unreachable!(),
                    });
                }
            }
        }
        
        // Second pass: Update connections using the index mapping
        for (old_idx, node) in net.nodes.iter().enumerate() {
            if let Some(&new_idx) = index_map.get(&old_idx) {
                if new_idx >= new_nodes.len() { continue; }
                let new_node = &mut new_nodes[new_idx];
                
                let old_ports = match node {
                    DisconnectedNode::Variable { .. } => continue,
                    DisconnectedNode::Eraser { ports } => ports.as_slice(),
                    DisconnectedNode::Reference { ports, .. } => ports.as_slice(),
                    DisconnectedNode::Numeric { ports, .. } => ports.as_slice(),
                    DisconnectedNode::Constructor { ports } => ports.as_slice(),
                    DisconnectedNode::Duplicator { ports } => ports.as_slice(),
                    DisconnectedNode::Operator { ports, .. } => ports.as_slice(),
                    DisconnectedNode::Switch { ports } => ports.as_slice(),
                };
                
                for (port_idx, old_conn) in old_ports.iter().enumerate() {
                    if let Some(conn) = old_conn {
                        if let Some(&new_dest) = index_map.get(&conn.destination) {
                            if port_idx < new_node.ports_mut().len() {
                                new_node.set_port(port_idx, Connection {
                                    destination: new_dest,
                                    port: conn.port,
                                });
                            }
                        }
                    }
                }
            }
        }
        
        // Update redexes with new indices
        let new_redexes = net.redexes.iter()
            .filter_map(|&(left, right)| {
                Some((
                    *index_map.get(&left)?,
                    *index_map.get(&right)?
                ))
            })
            .collect();
        
        // Update entry point with new index
        let new_entry_point = if let Some(&new_idx) = index_map.get(&net.entry_point.0) {
            (new_idx, net.entry_point.1)
        } else {
            panic!("Entry point index {} not found in node mapping after variable removal. Original entry point: {:?}", net.entry_point.0, net.entry_point);
        };
        
        NamedNet {
            name: net.name,
            nodes: new_nodes,
            redexes: new_redexes,
            entry_point: new_entry_point,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::variable_disconnection::{Numeric, Operation};
    use parallax_net::port::Connection;
    use crate::ast::Operator;
    use parallax_resolve::types::Symbol;

    fn create_disconnected_node(node: DisconnectedNode) -> DisconnectedNode {
        node
    }

    fn set_disconnected_port(node: &mut DisconnectedNode, port: usize, conn: Connection) {
        match node {
            DisconnectedNode::Variable { ports, .. } => ports[port] = Some(conn),
            DisconnectedNode::Eraser { ports } => ports[port] = Some(conn),
            DisconnectedNode::Reference { ports, .. } => ports[port] = Some(conn),
            DisconnectedNode::Numeric { ports, .. } => ports[port] = Some(conn),
            DisconnectedNode::Constructor { ports } => ports[port] = Some(conn),
            DisconnectedNode::Duplicator { ports } => ports[port] = Some(conn),
            DisconnectedNode::Operator { ports, .. } => ports[port] = Some(conn),
            DisconnectedNode::Switch { ports } => ports[port] = Some(conn),
        }
    }

    fn create_test_disconnected_net(
        name_symbol: Symbol,
        nodes: Vec<DisconnectedNode>,
        redexes: Vec<(usize, usize)>,
        entry_point: (usize, usize)
    ) -> DisconnectedNamedNet {
        DisconnectedNamedNet {
            name: name_symbol,
            nodes,
            redexes,
            entry_point,
        }
    }

    #[test]
    fn test_basic_conversion() {
        let test_symbol = Symbol::fresh();
        let var_symbol = Symbol::fresh();
        let mut nodes = vec![
            create_disconnected_node(DisconnectedNode::Constructor { ports: [None, None, None] }),
            create_disconnected_node(DisconnectedNode::Variable { name: var_symbol, ports: [None] }),
            create_disconnected_node(DisconnectedNode::Numeric { value: Numeric::Integer(42), ports: [None] }),
        ];
        
        set_disconnected_port(&mut nodes[0], 1, Connection { destination: 1, port: 0 });
        set_disconnected_port(&mut nodes[0], 2, Connection { destination: 2, port: 0 });
        set_disconnected_port(&mut nodes[2], 0, Connection { destination: 0, port: 2 });

        let disconnected_net = create_test_disconnected_net(
            test_symbol,
            nodes,
            vec![],
            (0, 0)
        );

        let removed_net = NamedNet::from(disconnected_net);

        assert_eq!(removed_net.name, test_symbol);
        assert_eq!(removed_net.nodes.len(), 2);
        assert!(matches!(removed_net.nodes[0], Node::Constructor { .. }));
        assert!(matches!(removed_net.nodes[1], Node::Numeric { .. }));

        if let Node::Constructor { ports } = &removed_net.nodes[0] {
            assert!(ports[1].is_none(), "Port 1 (connected to removed var) should be None");
            assert!(ports[2].is_some(), "Port 2 should be connected");
            let conn = ports[2].unwrap();
            assert_eq!(conn.destination, 1, "C0.p2 destination should be remapped to index 1");
            assert_eq!(conn.port, 0, "C0.p2 port should remain 0");
        } else {
            panic!("Node 0 should be a constructor");
        }
        
        if let Node::Numeric { ports, .. } = &removed_net.nodes[1] {
            assert!(ports[0].is_some(), "Numeric port 0 should be connected");
            let conn = ports[0].unwrap();
            assert_eq!(conn.destination, 0, "Num.p0 destination should be remapped to index 0");
            assert_eq!(conn.port, 2, "Num.p0 port should remain 2");
        } else {
            panic!("Node 1 should be numeric");
        }
    }

    #[test]
    fn test_redex_preservation() {
        let test_symbol = Symbol::fresh();
        let var_symbol = Symbol::fresh();
        let mut nodes = vec![
            create_disconnected_node(DisconnectedNode::Constructor { ports: [None, None, None] }),
            create_disconnected_node(DisconnectedNode::Constructor { ports: [None, None, None] }),
            create_disconnected_node(DisconnectedNode::Variable { name: var_symbol, ports: [None] }),
        ];
        
        set_disconnected_port(&mut nodes[0], 0, Connection { destination: 1, port: 0 });
        set_disconnected_port(&mut nodes[1], 0, Connection { destination: 0, port: 0 });

        let disconnected_net = create_test_disconnected_net(
            test_symbol,
            nodes,
            vec![(0, 1)],
            (0, 0)
        );

        let removed_net = NamedNet::from(disconnected_net);

        assert_eq!(removed_net.nodes.len(), 2);
        assert_eq!(removed_net.redexes.len(), 1);
        assert_eq!(removed_net.redexes[0], (0, 1));
        
        if let Node::Constructor { ports, .. } = &removed_net.nodes[0] {
            let conn = ports[0].expect("Redex port 0 missing");
            assert_eq!(conn.destination, 1);
            assert_eq!(conn.port, 0);
        }
        
        if let Node::Constructor { ports, .. } = &removed_net.nodes[1] {
            let conn = ports[0].expect("Redex port 0 missing");
            assert_eq!(conn.destination, 0);
            assert_eq!(conn.port, 0);
        }
    }

    #[test]
    fn test_entry_point_update() {
        let test_symbol = Symbol::fresh();
        let entry_symbol = Symbol::fresh();
        let mut nodes = vec![
            create_disconnected_node(DisconnectedNode::Variable { name: entry_symbol, ports: [None] }),
            create_disconnected_node(DisconnectedNode::Constructor { ports: [None, None, None] }),
            create_disconnected_node(DisconnectedNode::Numeric { value: Numeric::Integer(5), ports: [None] }),
        ];
        
        set_disconnected_port(&mut nodes[0], 0, Connection { destination: 1, port: 1 });
        set_disconnected_port(&mut nodes[1], 1, Connection { destination: 0, port: 0 });
        set_disconnected_port(&mut nodes[1], 2, Connection { destination: 2, port: 0 });
        set_disconnected_port(&mut nodes[2], 0, Connection { destination: 1, port: 2 });

        let disconnected_net = create_test_disconnected_net(
            test_symbol,
            nodes,
            vec![],
            (1, 1)
        );

        let removed_net = NamedNet::from(disconnected_net);

        assert_eq!(removed_net.nodes.len(), 2);
        assert_eq!(removed_net.entry_point, (0, 1));
    }

    #[test]
    fn test_reference_node_conversion() {
        let test_symbol = Symbol::fresh();
        let ref_target_symbol = Symbol::fresh();
        let var_symbol = Symbol::fresh();
        let mut nodes = vec![
            create_disconnected_node(DisconnectedNode::Reference { name: ref_target_symbol, ports: [None] }),
            create_disconnected_node(DisconnectedNode::Variable { name: var_symbol, ports: [None] }),
        ];
        set_disconnected_port(&mut nodes[0], 0, Connection { destination: 1, port: 0 });

        let disconnected_net = create_test_disconnected_net(
            test_symbol,
            nodes,
            vec![],
            (0, 0)
        );

        let removed_net = NamedNet::from(disconnected_net);

        assert_eq!(removed_net.nodes.len(), 1);
        if let Node::Reference { name, ports } = &removed_net.nodes[0] {
            assert_eq!(*name, ref_target_symbol, "Reference symbol should be preserved");
            assert!(ports[0].is_none(), "Reference port should be disconnected (was connected to removed var)");
        } else {
            panic!("Expected Reference node");
        }
    }
}
