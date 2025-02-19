

use std::collections::HashMap;

use crate::ast::{self, Operator};

pub struct DisconnectedVariables<'a> {
    pub named_nets: Vec<NamedNet<'a>>,
}

pub struct NamedNet<'a> {
    pub name: &'a str,
    pub nodes: Vec<Node<'a>>,
    pub redexes: Vec<(usize, usize)>,
    pub entry_point: (usize, usize),  // (node_index, port)
}

#[derive(Debug)]
pub enum Node<'a> {
    Variable {
        name: &'a str,
        ports: [Option<Connection>; 1],
    },
    /// An Eraser node with a single main port.
    Eraser {
        /// [main_port]
        ports: [Option<Connection>; 1],
    },
    
    /// A Reference node that points to a named network.
    Reference {
        /// The name of the referenced network.
        name: &'a str,
        
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

#[derive(Debug, Clone)]
pub enum Numeric {
    Integer(i64),
    Float(f64),
    Operator(Operation),
}

#[derive(Debug, Clone)]
pub enum Operation {
    Unapplied(Operator),
    PartiallyApplied(Operator, f64),
}

impl<'a> Node<'a> {
    fn ports(&self) -> &[Option<Connection>] {
        match self {
            Node::Variable { ports, .. } => ports,
            Node::Eraser { ports, .. } => ports,
            Node::Reference { ports, .. } => ports,
            Node::Numeric { ports, .. } => ports,
            Node::Constructor { ports, .. } => ports,
            Node::Duplicator { ports, .. } => ports,
            Node::Operator { ports, .. } => ports,
            Node::Switch { ports, .. } => ports,
        }
    }

    fn ports_mut(&mut self) -> &mut [Option<Connection>] {
        match self {
            Node::Variable { ports, .. } => ports,
            Node::Eraser { ports, .. } => ports,
            Node::Reference { ports, .. } => ports,
            Node::Numeric { ports, .. } => ports,
            Node::Constructor { ports, .. } => ports,
            Node::Duplicator { ports, .. } => ports,
            Node::Operator { ports, .. } => ports,
            Node::Switch { ports, .. } => ports,
        }
    }

    fn set_port(&mut self, port: usize, connection: Connection) {
        let ports = self.ports_mut();
        ports[port] = Some(connection);
    }
}

struct Ctx<'a, 'b> {
    variable_locations: HashMap<&'a str, Vec<usize>>,
    nodes: &'b mut Vec<Node<'a>>,
}

fn disconnect_variable_nodes(nodes: &mut Vec<Node<'_>>) -> Option<(usize, usize)> {
    // First, collect all variables by name
    let mut var_groups: HashMap<&str, Vec<(usize, usize)>> = HashMap::new();
    let mut entry_point_update = None;
    
    // Group variables by name
    for (node_id, node) in nodes.iter().enumerate() {
        if let Node::Variable { name, .. } = node {
            var_groups.entry(name).or_default().push((node_id, 0));
        }
    }
    
    // For each group of variables, connect all nodes that were connected to them
    for var_nodes in var_groups.values() {
        let mut connected_nodes = Vec::new();
        
        // Collect all non-variable nodes connected to these variables
        for &(var_id, _) in var_nodes {
            if let Some(conn) = nodes[var_id].ports()[0] {
                let dest_id = conn.destination;
                let dest_port = conn.port;
                if !matches!(nodes[dest_id], Node::Variable { .. }) {
                    connected_nodes.push((dest_id, dest_port));
                }
            }
        }
        
        // If this variable group contains only one non-variable connection,
        // it might be the new entry point
        if connected_nodes.len() == 1 {
            entry_point_update = Some(connected_nodes[0]);
        }
        
        // Connect all collected nodes to each other
        for i in 0..connected_nodes.len() {
            let (from_id, from_port) = connected_nodes[i];
            for j in (i+1)..connected_nodes.len() {
                let (to_id, to_port) = connected_nodes[j];
                nodes[from_id].set_port(from_port, Connection { destination: to_id, port: to_port });
                nodes[to_id].set_port(to_port, Connection { destination: from_id, port: from_port });
            }
        }
    }
    
    // Clear all variable node connections
    for node in nodes.iter_mut() {
        if let Node::Variable { ports, .. } = node {
            ports[0] = None;
        }
    }

    entry_point_update
}

impl<'a> From<ast::NamedNetwork<'a>> for NamedNet<'a> {
    fn from(ast: ast::NamedNetwork<'a>) -> Self {
        let mut nodes = Vec::new();
        let mut redexes = Vec::new();

        let mut ctx = Ctx {
            variable_locations: HashMap::new(),
            nodes: &mut nodes,
        };  

        let node_id = traverse(ast.net.tree, &mut ctx);
        let mut entry_point = (node_id, 0);  // Default to port 0

        for redex in ast.net.redexes {
            let left_id = traverse(redex.left, &mut ctx);
            let right_id = traverse(redex.right, &mut ctx);
            ctx.nodes[left_id].set_port(0, Connection { destination: right_id, port: 0 });
            ctx.nodes[right_id].set_port(0, Connection { destination: left_id, port: 0 });

            redexes.push((left_id, right_id));
        }

        // Remove variable nodes by reconfiguring connections
        if let Some(new_entry) = disconnect_variable_nodes(&mut nodes) {
            entry_point = new_entry;
        }

        NamedNet { name: ast.name, nodes, entry_point, redexes }
    }
}

impl<'a> From<ast::Book<'a>> for DisconnectedVariables<'a> {
    fn from(ast: ast::Book<'a>) -> Self {
        DisconnectedVariables { named_nets: ast.named_nets.into_iter().map(|net| net.into()).collect() }
    }
}


fn traverse<'a, 'b>(tree: ast::Tree<'a>, ctx: &mut Ctx<'a, 'b>) -> usize {
    let id = ctx.nodes.len();
    match tree {
        ast::Tree::Variable(name) => {
            // Simply create the variable node without any connections
            ctx.nodes.push(Node::Variable { name, ports: [None; 1] });
            
            // Just track the location
            ctx.variable_locations.entry(name)
                .or_insert_with(Vec::new)
                .push(id);
        }
        ast::Tree::Eraser => {
            ctx.nodes.push(Node::Eraser { ports: [None; 1] });
        }
        ast::Tree::Reference(name) => {
            ctx.nodes.push(Node::Reference { name, ports: [None; 1] });
        },
        ast::Tree::Numeric(value) => {
            let value = match value {
                ast::Numeric::Number(num) => num.parse().map(Numeric::Integer).unwrap_or_else(|_| Numeric::Float(num.parse().expect("Invalid number"))),
                ast::Numeric::Operator(op) => Numeric::Operator(match op {
                    ast::Operation::Unapplied(op) => Operation::Unapplied(op),
                    ast::Operation::PartiallyApplied(op, num) => Operation::PartiallyApplied(op, num.parse().expect("Invalid number")),
                })
            };
            ctx.nodes.push(Node::Numeric { value, ports: [None; 1] });
        },
        ast::Tree::Constructor(left, right) => {
            ctx.nodes.push(Node::Constructor { ports: [None; 3] });
            let left_id = traverse(*left, ctx);
            let right_id = traverse(*right, ctx);
            ctx.nodes[left_id].set_port(0, Connection { destination: id, port: 1 });
            ctx.nodes[right_id].set_port(0, Connection { destination: id, port: 2 });
            ctx.nodes[id].set_port(1, Connection { destination: left_id, port: 0 });
            ctx.nodes[id].set_port(2, Connection { destination: right_id, port: 0 });
        },
        ast::Tree::Duplicator(left, right) => {
            ctx.nodes.push(Node::Duplicator { ports: [None; 3] });
            let left_id = traverse(*left, ctx);
            let right_id = traverse(*right, ctx);
            ctx.nodes[left_id].set_port(0, Connection { destination: id, port: 1 });
            ctx.nodes[right_id].set_port(0, Connection { destination: id, port: 2 });
            ctx.nodes[id].set_port(1, Connection { destination: left_id, port: 0 });
        },
        ast::Tree::Operator(left, right) => {
            ctx.nodes.push(Node::Operator { operation: Operation::Unapplied(Operator::Add), ports: [None; 3] });
            let left_id = traverse(*left, ctx);
            let right_id = traverse(*right, ctx);
            ctx.nodes[left_id].set_port(0, Connection { destination: id, port: 1 });
            ctx.nodes[right_id].set_port(0, Connection { destination: id, port: 2 });
            ctx.nodes[id].set_port(1, Connection { destination: left_id, port: 0 });
            ctx.nodes[id].set_port(2, Connection { destination: right_id, port: 0 });
        },
        ast::Tree::Switch(left, right) => {
            ctx.nodes.push(Node::Switch { ports: [None; 3] });
            let left_id = traverse(*left, ctx);
            let right_id = traverse(*right, ctx);
            ctx.nodes[left_id].set_port(0, Connection { destination: id, port: 1 });
            ctx.nodes[right_id].set_port(0, Connection { destination: id, port: 2 });
            ctx.nodes[id].set_port(1, Connection { destination: left_id, port: 0 });
            ctx.nodes[id].set_port(2, Connection { destination: right_id, port: 0 });
        },
    }
    id
}

#[cfg(test)]
mod tests {
    use super::*;
    use parallax_hvm::ast::{self, Book, NamedNetwork, Network, Tree, Numeric as AstNumeric, Operation as AstOperation};

    fn create_test_book<'a>(nets: Vec<(&'a str, Tree<'a>)>) -> Book<'a> {
        Book {
            named_nets: nets.into_iter()
                .map(|(name, tree)| NamedNetwork {
                    name,
                    net: Network {
                        tree,
                        redexes: vec![],
                    },
                })
                .collect()
        }
    }

    fn assert_connected(nodes: &[Node], from_id: usize, from_port: usize, to_id: usize, to_port: usize) {
        if let Some(conn) = nodes[from_id].ports()[from_port] {
            assert_eq!(conn.destination, to_id);
            assert_eq!(conn.port, to_port);
        } else {
            panic!("Expected connection from node {} port {} to node {} port {}, but found none",
                from_id, from_port, to_id, to_port);
        }
    }

    fn count_node_types(nodes: &[Node]) -> (usize, usize) {
        let mut var_count = 0;
        let mut constructor_count = 0;
        for node in nodes {
            match node {
                Node::Variable { .. } => var_count += 1,
                Node::Constructor { .. } => constructor_count += 1,
                _ => {}
            }
        }
        (var_count, constructor_count)
    }

    fn print_node_structure(nodes: &[Node]) {
        println!("\nDetailed node structure:");
        for (i, node) in nodes.iter().enumerate() {
            println!("\nNode {}: {:?}", i, node);
            match node {
                Node::Constructor { ports } => {
                    println!("  Constructor ports:");
                    for (j, port) in ports.iter().enumerate() {
                        if let Some(conn) = port {
                            println!("    Port {} -> Node {} Port {}", j, conn.destination, conn.port);
                        } else {
                            println!("    Port {} -> None", j);
                        }
                    }
                }
                Node::Variable { name, ports } => {
                    println!("  Variable '{}' ports:", name);
                    for (j, port) in ports.iter().enumerate() {
                        if let Some(conn) = port {
                            println!("    Port {} -> Node {} Port {}", j, conn.destination, conn.port);
                        } else {
                            println!("    Port {} -> None", j);
                        }
                    }
                }
                _ => {}
            }
        }
    }

    #[test]
    fn test_basic_numeric_conversion() {
        let book = create_test_book(vec![
            ("test", Tree::Numeric(AstNumeric::Number("42")))
        ]);
        
        let ir_book: DisconnectedVariables = book.into();
        assert_eq!(ir_book.named_nets.len(), 1);
        
        let net = &ir_book.named_nets[0];
        assert_eq!(net.name, "test");
        assert_eq!(net.nodes.len(), 1);
        
        match &net.nodes[0] {
            Node::Numeric { value, .. } => {
                match value {
                    Numeric::Integer(n) => assert_eq!(*n, 42),
                    _ => panic!("Expected integer numeric"),
                }
            },
            _ => panic!("Expected numeric node"),
        }
    }

    #[test]
    fn test_constructor_connections() {
        let tree = Tree::Constructor(
            Box::new(Tree::Numeric(AstNumeric::Number("1"))),
            Box::new(Tree::Numeric(AstNumeric::Number("2")))
        );
        let book = create_test_book(vec![("test", tree)]);
        
        let ir_book: DisconnectedVariables = book.into();
        let net = &ir_book.named_nets[0];
        
        // Should have 3 nodes: Constructor and two Numerics
        assert_eq!(net.nodes.len(), 3);
        
        // Check node types
        assert!(matches!(net.nodes[0], Node::Constructor { .. }));
        assert!(matches!(net.nodes[1], Node::Numeric { .. }));
        assert!(matches!(net.nodes[2], Node::Numeric { .. }));
        
        // Check connections
        assert_connected(&net.nodes, 0, 1, 1, 0);
        assert_connected(&net.nodes, 0, 2, 2, 0);
        assert_connected(&net.nodes, 1, 0, 0, 1);
        assert_connected(&net.nodes, 2, 0, 0, 2);
    }

    #[test]
    fn test_variable_node_removal_simple() {
        // Test case: Two variables connected directly
        let tree = Tree::Constructor(
            Box::new(Tree::Variable("x")),
            Box::new(Tree::Variable("x"))
        );
        let book = create_test_book(vec![("test", tree)]);
        let ir_book: DisconnectedVariables = book.into();
        let net = &ir_book.named_nets[0];

        // Print debug info
        println!("Nodes after conversion:");
        for (i, node) in net.nodes.iter().enumerate() {
            println!("Node {}: {:?}", i, node);
        }
        
        // Basic structure checks
        assert_eq!(net.nodes.len(), 3);
        assert!(matches!(net.nodes[0], Node::Constructor { .. }));
        
        // Check variable nodes are disconnected
        let mut var_count = 0;
        for node in &net.nodes {
            if let Node::Variable { ports, .. } = node {
                assert!(ports[0].is_none(), "Variable node should be disconnected");
                var_count += 1;
            }
        }
        assert_eq!(var_count, 2, "Should have exactly two variable nodes");
        
        // Check constructor ports are connected to each other
        let constructor = &net.nodes[0];
        if let Node::Constructor { ports } = constructor {
            assert!(ports[1].is_some(), "Constructor port 1 should be connected");
            assert!(ports[2].is_some(), "Constructor port 2 should be connected");
            
            let port1 = ports[1].unwrap();
            let port2 = ports[2].unwrap();
            assert_eq!(port1.destination, port2.destination, "Constructor ports should connect to same destination");
        }
    }

    #[test]
    fn test_variable_node_removal_chain() {
        // Test case: Chain of variables x -> y -> x
        let tree = Tree::Constructor(
            Box::new(Tree::Variable("x")),
            Box::new(Tree::Constructor(
                Box::new(Tree::Variable("y")),
                Box::new(Tree::Variable("x"))
            ))
        );
        let book = create_test_book(vec![("test", tree)]);
        let ir_book: DisconnectedVariables = book.into();
        let net = &ir_book.named_nets[0];

        println!("Nodes in chain test:");
        for (i, node) in net.nodes.iter().enumerate() {
            println!("Node {}: {:?}", i, node);
        }

        // Should have 5 nodes: 2 constructors and 3 variables
        assert_eq!(net.nodes.len(), 5);
        
        // Check all variable nodes are disconnected
        for node in &net.nodes {
            if let Node::Variable { ports, .. } = node {
                assert!(ports[0].is_none(), "Variable node should be disconnected");
            }
        }
        
        // Check constructors are properly connected
        let mut constructor_count = 0;
        for node in &net.nodes {
            if let Node::Constructor { ports } = node {
                assert!(ports[1].is_some() && ports[2].is_some(), "Constructor ports should be connected");
                constructor_count += 1;
            }
        }
        assert_eq!(constructor_count, 2, "Should have exactly two constructors");
    }

    #[test]
    fn test_variable_node_removal_cycle() {
        // Test case: Cycle of variables x -> y -> z -> x
        let tree = Tree::Constructor(
            Box::new(Tree::Variable("x")),
            Box::new(Tree::Constructor(
                Box::new(Tree::Variable("y")),
                Box::new(Tree::Variable("z"))
            ))
        );
        let book = create_test_book(vec![("test", tree)]);
        let ir_book: DisconnectedVariables = book.into();
        let net = &ir_book.named_nets[0];

        println!("Nodes in cycle test:");
        for (i, node) in net.nodes.iter().enumerate() {
            println!("Node {}: {:?}", i, node);
        }

        // Check all variable nodes are disconnected
        for node in &net.nodes {
            if let Node::Variable { ports, .. } = node {
                assert!(ports[0].is_none(), "Variable node should be disconnected");
            }
        }
        
        // Check constructors maintain their structure
        let mut constructor_count = 0;
        for node in &net.nodes {
            if let Node::Constructor { ports } = node {
                assert!(ports[1].is_some() && ports[2].is_some(), "Constructor ports should be connected");
                constructor_count += 1;
            }
        }
        assert_eq!(constructor_count, 2, "Should have exactly two constructors");
    }

    #[test]
    fn test_variable_node_removal_multiple_references() {
        // Test case: Two pairs of variables
        let tree = Tree::Constructor(
            Box::new(Tree::Variable("y")),  // First occurrence of y
            Box::new(Tree::Constructor(
                Box::new(Tree::Variable("x")),  // First occurrence of x
                Box::new(Tree::Constructor(
                    Box::new(Tree::Variable("y")),  // Second occurrence of y
                    Box::new(Tree::Variable("x"))   // Second occurrence of x
                ))
            ))
        );
        let book = create_test_book(vec![("test", tree)]);
        let ir_book: DisconnectedVariables = book.into();
        let net = &ir_book.named_nets[0];

        println!("Nodes in multiple references test:");
        for (i, node) in net.nodes.iter().enumerate() {
            println!("Node {}: {:?}", i, node);
        }

        // Count nodes by type
        let (var_count, constructor_count) = count_node_types(&net.nodes);
        println!("\nFound {} variables and {} constructors", var_count, constructor_count);
        
        // Should have 3 constructors and 4 variables (2 pairs of 2)
        assert_eq!(constructor_count, 3, "Should have exactly three constructors");
        assert_eq!(var_count, 4, "Should have exactly four variables (two pairs)");
        assert_eq!(net.nodes.len(), 7, "Should have exactly seven nodes total");
        
        // Check all variable nodes are disconnected
        let mut var_names = std::collections::HashMap::new();
        for node in &net.nodes {
            if let Node::Variable { name, ports, .. } = node {
                assert!(ports[0].is_none(), "Variable node should be disconnected");
                *var_names.entry(*name).or_insert(0) += 1;
            }
        }
        
        // Each variable should appear exactly twice
        assert_eq!(var_names["x"], 2, "Should have exactly two 'x' variables");
        assert_eq!(var_names["y"], 2, "Should have exactly two 'y' variables");
        
        // Check constructors are properly connected
        let mut constructor_count = 0;
        for node in &net.nodes {
            if let Node::Constructor { ports } = node {
                assert!(ports[1].is_some() && ports[2].is_some(), "Constructor ports should be connected");
                constructor_count += 1;
            }
        }
        assert_eq!(constructor_count, 3, "Should have exactly three constructors");
    }

    #[test]
    fn test_operator_conversion() {
        let tree = Tree::Numeric(AstNumeric::Operator(AstOperation::Unapplied(ast::Operator::Add)));
        let book = create_test_book(vec![("test", tree)]);
        
        let ir_book: DisconnectedVariables = book.into();
        let net = &ir_book.named_nets[0];
        
        match &net.nodes[0] {
            Node::Numeric { value, .. } => {
                match value {
                    Numeric::Operator(Operation::Unapplied(op)) => {
                        assert!(matches!(op, Operator::Add));
                    },
                    _ => panic!("Expected unapplied operator"),
                }
            },
            _ => panic!("Expected numeric node"),
        }
    }

    #[test]
    fn test_redex_creation() {
        let net = Network {
            tree: Tree::Constructor(Box::new(Tree::Variable("x")), Box::new(Tree::Variable("y"))),
            redexes: vec![
                ast::Redex {
                    safe: false,
                    left: Tree::Numeric(AstNumeric::Number("1")),
                    right: Tree::Numeric(AstNumeric::Number("2")),
                }
            ],
        };
        let book = Book {
            named_nets: vec![NamedNetwork { name: "test", net }],
        };
        
        let ir_book: DisconnectedVariables = book.into();
        let ir_net = &ir_book.named_nets[0];
        
        // Check redex connections
        assert_eq!(ir_net.redexes.len(), 1);
        let (left_id, right_id) = ir_net.redexes[0];
        assert_connected(&ir_net.nodes, left_id, 0, right_id, 0);
        assert_connected(&ir_net.nodes, right_id, 0, left_id, 0);
    }

    #[test]
    fn test_variable_node_nested_structure() {
        // Test case: Nested structure with variables
        // Note: Each variable name can only appear twice in a network
        let inner_tree = Tree::Constructor(
            Box::new(Tree::Variable("x")),  // First occurrence of x
            Box::new(Tree::Constructor(
                Box::new(Tree::Variable("y")),  // First occurrence of y
                Box::new(Tree::Variable("y"))   // Second occurrence of y
            ))
        );
        let tree = Tree::Constructor(
            Box::new(Tree::Variable("x")),  // Second occurrence of x
            Box::new(inner_tree)
        );
        let book = create_test_book(vec![("test", tree)]);
        let ir_book: DisconnectedVariables = book.into();
        let net = &ir_book.named_nets[0];

        print_node_structure(&net.nodes);

        let (var_count, constructor_count) = count_node_types(&net.nodes);
        println!("\nFound {} variables and {} constructors", var_count, constructor_count);
        
        // Should have 3 constructors and 4 variables (2 pairs of 2)
        assert_eq!(constructor_count, 3, "Should have exactly three constructors");
        assert_eq!(var_count, 4, "Should have exactly four variables (two pairs)");
        
        // Count variables by name - each should appear exactly twice
        let mut var_names = std::collections::HashMap::new();
        for node in &net.nodes {
            if let Node::Variable { name, .. } = node {
                *var_names.entry(*name).or_insert(0) += 1;
            }
        }
        assert_eq!(var_names["x"], 2, "Should have exactly two 'x' variables");
        assert_eq!(var_names["y"], 2, "Should have exactly two 'y' variables");
    }

    #[test]
    fn test_variable_node_different_names() {
        // Test case: Multiple variables with different names
        let tree = Tree::Constructor(
            Box::new(Tree::Variable("x")),
            Box::new(Tree::Constructor(
                Box::new(Tree::Variable("y")),
                Box::new(Tree::Variable("x"))
            ))
        );
        let book = create_test_book(vec![("test", tree)]);
        let ir_book: DisconnectedVariables = book.into();
        let net = &ir_book.named_nets[0];

        print_node_structure(&net.nodes);

        // Count variables by name
        let mut var_names = std::collections::HashMap::new();
        for node in &net.nodes {
            if let Node::Variable { name, .. } = node {
                *var_names.entry(*name).or_insert(0) += 1;
            }
        }
        println!("\nVariable counts by name:");
        for (name, count) in &var_names {
            println!("  '{}': {}", name, count);
        }

        assert_eq!(var_names["x"], 2, "Should have exactly two 'x' variables");
        assert_eq!(var_names["y"], 1, "Should have exactly one 'y' variable");
    }

    #[test]
    fn test_variable_node_deep_nesting() {
        // Test case: Deeply nested structure with two variables
        let mut current = Tree::Variable("x");  // First occurrence of x
        let mut depth = 0;
        for _ in 0..3 {
            current = Tree::Constructor(
                Box::new(if depth == 0 { 
                    Tree::Variable("x")  // Second occurrence of x
                } else { 
                    Tree::Variable("y")  // First occurrence of y
                }),
                Box::new(current)
            );
            depth += 1;
        }
        let book = create_test_book(vec![("test", current)]);
        let ir_book: DisconnectedVariables = book.into();
        let net = &ir_book.named_nets[0];

        print_node_structure(&net.nodes);

        // Count variables by name
        let mut var_names = std::collections::HashMap::new();
        for node in &net.nodes {
            if let Node::Variable { name, .. } = node {
                *var_names.entry(*name).or_insert(0) += 1;
            }
        }
        
        // Each variable should appear exactly twice
        assert_eq!(var_names["x"], 2, "Should have exactly two 'x' variables");
        assert!(var_names["y"] <= 2, "Should have at most two 'y' variables");
    }

    #[test]
    fn test_variable_node_connections_before_removal() {
        // Test the state of connections before variable removal
        let tree = Tree::Constructor(
            Box::new(Tree::Variable("x")),
            Box::new(Tree::Variable("x"))
        );
        
        // Create nodes but don't remove variables yet
        let mut nodes = Vec::new();
        let mut ctx = Ctx {
            variable_locations: HashMap::new(),
            nodes: &mut nodes,
        };
        
        traverse(tree, &mut ctx);
        
        println!("\nConnections before variable removal:");
        print_node_structure(&nodes);
        
        // Now remove variables and check again
        disconnect_variable_nodes(&mut nodes);
        
        println!("\nConnections after variable removal:");
        print_node_structure(&nodes);
    }

    #[test]
    fn test_variable_node_removal_connections() {
        // Create nodes manually to test remove_variable_nodes directly
        let mut nodes = vec![
            Node::Constructor { ports: [None, None, None] },  // id: 0
            Node::Variable { name: "x", ports: [None] },     // id: 1
            Node::Constructor { ports: [None, None, None] },  // id: 2
            Node::Variable { name: "x", ports: [None] }      // id: 3
        ];

        // Set up initial connections:
        // Constructor0.port1 -> Variable1.port0
        // Variable1.port0 -> Constructor0.port1
        // Constructor2.port1 -> Variable3.port0
        // Variable3.port0 -> Constructor2.port1
        nodes[0].set_port(1, Connection { destination: 1, port: 0 });
        nodes[1].set_port(0, Connection { destination: 0, port: 1 });
        nodes[2].set_port(1, Connection { destination: 3, port: 0 });
        nodes[3].set_port(0, Connection { destination: 2, port: 1 });

        println!("\nBefore variable removal:");
        print_node_structure(&nodes);

        // Remove variables
        disconnect_variable_nodes(&mut nodes);

        println!("\nAfter variable removal:");
        print_node_structure(&nodes);

        // Check that constructors are now connected to each other
        if let Node::Constructor { ports: ports1 } = &nodes[0] {
            if let Node::Constructor { ports: ports2 } = &nodes[2] {
                // The ports that were connected to the same variable should now be connected to each other
                let conn1 = ports1[1].expect("Constructor 1 port should be connected");
                let conn2 = ports2[1].expect("Constructor 2 port should be connected");
                
                assert_eq!(conn1.destination, 2, "First constructor should connect to second");
                assert_eq!(conn1.port, 1, "Should connect to the matching port");
                assert_eq!(conn2.destination, 0, "Second constructor should connect to first");
                assert_eq!(conn2.port, 1, "Should connect to the matching port");
            }
        }

        // Check that variables are disconnected
        for node in &nodes {
            if let Node::Variable { ports, .. } = node {
                assert!(ports[0].is_none(), "Variables should be disconnected");
            }
        }
    }

    #[test]
    fn test_variable_node_entry_point() {
        // Create a network where the entry point is a variable connected to a constructor
        let tree = Tree::Variable("x");  // Entry point variable
        let book = create_test_book(vec![("test", tree)]);
        
        // Create nodes manually to test the exact scenario
        let mut nodes = vec![
            Node::Variable { name: "x", ports: [None] },     // id: 0 (entry point)
            Node::Constructor { ports: [None, None, None] },  // id: 1
        ];

        // Connect variable to constructor's port 2
        nodes[0].set_port(0, Connection { destination: 1, port: 2 });
        nodes[1].set_port(2, Connection { destination: 0, port: 0 });

        println!("\nBefore variable removal:");
        print_node_structure(&nodes);

        // Remove variables and get new entry point
        let new_entry = disconnect_variable_nodes(&mut nodes);
        
        println!("\nAfter variable removal:");
        print_node_structure(&nodes);

        // The entry point should be redirected to the constructor's port 2
        assert_eq!(new_entry, Some((1, 2)), "Entry point should be redirected to constructor port 2");

        // All variables should be disconnected
        for node in &nodes {
            if let Node::Variable { ports, .. } = node {
                assert!(ports[0].is_none(), "Variables should be disconnected");
            }
        }
    }
}
