use std::collections::HashMap;
use crate::ast::{self, Operator};
use parallax_resolve::types::Symbol;
use parallax_net::port::Connection;

pub struct DisconnectedVariables {
    pub named_nets: Vec<NamedNet>,
}

pub struct NamedNet {
    pub name: Symbol,
    pub nodes: Vec<Node>,
    pub redexes: Vec<(usize, usize)>,
    pub entry_point: (usize, usize),  // (node_index, port)
}

#[derive(Debug)]
pub enum Node {
    Variable {
        name: Symbol,
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

impl Node {
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

struct Ctx<'b> {
    variable_symbols: HashMap<String, Symbol>,
    variable_locations: HashMap<Symbol, Vec<usize>>,
    nodes: &'b mut Vec<Node>,
}

fn disconnect_variable_nodes(nodes: &mut Vec<Node>) -> Option<(usize, usize)> {
    let mut var_groups: HashMap<Symbol, Vec<(usize, usize)>> = HashMap::new();
    let mut entry_point_update = None;
    
    for (node_id, node) in nodes.iter().enumerate() {
        if let Node::Variable { name: symbol, .. } = node {
            var_groups.entry(*symbol).or_default().push((node_id, 0));
        }
    }
    
    for var_nodes in var_groups.values() {
        let mut connected_nodes = Vec::new();
        
        for &(var_id, _) in var_nodes {
            if let Some(conn) = nodes[var_id].ports()[0] {
                let dest_id = conn.destination;
                let dest_port = conn.port;
                if !matches!(nodes[dest_id], Node::Variable { .. }) {
                    connected_nodes.push((dest_id, dest_port));
                }
            }
        }
        
        if connected_nodes.len() == 1 {
            entry_point_update = Some(connected_nodes[0]);
        }
        
        for i in 0..connected_nodes.len() {
            let (from_id, from_port) = connected_nodes[i];
            for j in (i+1)..connected_nodes.len() {
                let (to_id, to_port) = connected_nodes[j];
                nodes[from_id].set_port(from_port, Connection { destination: to_id, port: to_port });
                nodes[to_id].set_port(to_port, Connection { destination: from_id, port: from_port });
            }
        }
    }
    
    for node in nodes.iter_mut() {
        if let Node::Variable { ports, .. } = node {
            ports[0] = None;
        }
    }

    entry_point_update
}

impl<'a> From<ast::NamedNetwork<'a>> for NamedNet {
    fn from(ast: ast::NamedNetwork<'a>) -> Self {
        let mut nodes = Vec::new();
        let mut redexes = Vec::new();

        let mut ctx = Ctx {
            variable_symbols: HashMap::new(),
            variable_locations: HashMap::new(),
            nodes: &mut nodes,
        };  

        let node_id = traverse(ast.net.tree, &mut ctx);
        let mut entry_point = (node_id, 0);  // Default to port 0

        for redex in ast.net.redexes {
            let left_id = traverse(redex.left, &mut ctx);
            let right_id = traverse(redex.right, &mut ctx);
            if left_id < ctx.nodes.len() && right_id < ctx.nodes.len() {
                 ctx.nodes[left_id].set_port(0, Connection { destination: right_id, port: 0 });
                 ctx.nodes[right_id].set_port(0, Connection { destination: left_id, port: 0 });
                 redexes.push((left_id, right_id));
             } else {
                 eprintln!("Warning: Invalid node indices {} or {} for redex in net {}", left_id, right_id, ast.name);
             }
        }

        if let Some(new_entry) = disconnect_variable_nodes(&mut nodes) {
            entry_point = new_entry;
        }

        NamedNet { name: Symbol::fresh(), nodes, entry_point, redexes }
    }
}

impl<'a> From<ast::Book<'a>> for DisconnectedVariables {
    fn from(ast: ast::Book<'a>) -> Self {
        DisconnectedVariables { named_nets: ast.named_nets.into_iter().map(|net| net.into()).collect() }
    }
}

fn traverse<'a, 'b>(tree: ast::Tree<'a>, ctx: &mut Ctx<'b>) -> usize {
    let id = ctx.nodes.len();
    match tree {
        ast::Tree::Variable(name_str) => {
            let var_symbol = *ctx.variable_symbols.entry(name_str.to_string())
                .or_insert_with(Symbol::fresh);

            ctx.nodes.push(Node::Variable { name: var_symbol, ports: [None; 1] });
            
            ctx.variable_locations.entry(var_symbol)
                .or_default()
                .push(id);
        }
        ast::Tree::Eraser => {
            ctx.nodes.push(Node::Eraser { ports: [None; 1] });
        }
        ast::Tree::Reference(name_str) => {
            let ref_symbol = Symbol::fresh();
            ctx.nodes.push(Node::Reference { name: ref_symbol, ports: [None; 1] });
        },
        ast::Tree::Numeric(value) => {
            let parsed_value = match value {
                ast::Numeric::Number(num) => num.parse().map(Numeric::Integer).or_else(|_| num.parse().map(Numeric::Float)).expect("Invalid number"),
                ast::Numeric::Operator(op) => Numeric::Operator(match op {
                    ast::Operation::Unapplied(op) => Operation::Unapplied(op),
                    ast::Operation::PartiallyApplied(op, num) => Operation::PartiallyApplied(op, num.parse().expect("Invalid number")),
                })
            };
            ctx.nodes.push(Node::Numeric { value: parsed_value, ports: [None; 1] });
        },
        ast::Tree::Constructor(left, right) => {
            ctx.nodes.push(Node::Constructor { ports: [None; 3] });
            let left_id = traverse(*left, ctx);
            let right_id = traverse(*right, ctx);
            if left_id < ctx.nodes.len() { ctx.nodes[left_id].set_port(0, Connection { destination: id, port: 1 }); }
            if right_id < ctx.nodes.len() { ctx.nodes[right_id].set_port(0, Connection { destination: id, port: 2 }); }
            if id < ctx.nodes.len() {
                ctx.nodes[id].set_port(1, Connection { destination: left_id, port: 0 });
                ctx.nodes[id].set_port(2, Connection { destination: right_id, port: 0 });
            }
        },
        ast::Tree::Duplicator(left, right) => {
            ctx.nodes.push(Node::Duplicator { ports: [None; 3] });
            let left_id = traverse(*left, ctx);
            let right_id = traverse(*right, ctx);
            if left_id < ctx.nodes.len() { ctx.nodes[left_id].set_port(0, Connection { destination: id, port: 1 }); }
            if right_id < ctx.nodes.len() { ctx.nodes[right_id].set_port(0, Connection { destination: id, port: 2 }); }
            if id < ctx.nodes.len() {
                ctx.nodes[id].set_port(1, Connection { destination: left_id, port: 0 });
                ctx.nodes[id].set_port(2, Connection { destination: right_id, port: 0 });
            }
        },
        ast::Tree::Operator(left, right) => {
            ctx.nodes.push(Node::Operator { operation: Operation::Unapplied(Operator::Add), ports: [None; 3] });
            let left_id = traverse(*left, ctx);
            let right_id = traverse(*right, ctx);
            if left_id < ctx.nodes.len() { ctx.nodes[left_id].set_port(0, Connection { destination: id, port: 1 }); }
            if right_id < ctx.nodes.len() { ctx.nodes[right_id].set_port(0, Connection { destination: id, port: 2 }); }
            if id < ctx.nodes.len() {
                ctx.nodes[id].set_port(1, Connection { destination: left_id, port: 0 });
                ctx.nodes[id].set_port(2, Connection { destination: right_id, port: 0 });
            }
        },
        ast::Tree::Switch(left, right) => {
            ctx.nodes.push(Node::Switch { ports: [None; 3] });
            let left_id = traverse(*left, ctx);
            let right_id = traverse(*right, ctx);
            if left_id < ctx.nodes.len() { ctx.nodes[left_id].set_port(0, Connection { destination: id, port: 1 }); }
            if right_id < ctx.nodes.len() { ctx.nodes[right_id].set_port(0, Connection { destination: id, port: 2 }); }
            if id < ctx.nodes.len() {
                ctx.nodes[id].set_port(1, Connection { destination: left_id, port: 0 });
                ctx.nodes[id].set_port(2, Connection { destination: right_id, port: 0 });
            }
        },
    }
    id
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{self, Book, NamedNetwork, Network, Tree, Numeric as AstNumeric, Operation as AstOperation};
    use parallax_resolve::types::Symbol;

    fn create_test_book<'a>(nets: Vec<(&'a str, Tree<'a>)>) -> Book<'a> {
        Book {
            named_nets: nets.into_iter()
                .map(|(name_str, tree)| NamedNetwork {
                    name: name_str,
                    net: Network {
                        tree,
                        redexes: vec![],
                    },
                })
                .collect()
        }
    }

    fn assert_connected(nodes: &[Node], from_id: usize, from_port: usize, to_id: usize, to_port: usize) {
        if from_id >= nodes.len() || to_id >= nodes.len() {
             panic!("Invalid node index in assert_connected: from={}, to={}, len={}", from_id, to_id, nodes.len());
        }
        if let Some(conn) = nodes[from_id].ports().get(from_port).copied().flatten() {
             assert_eq!(conn.destination, to_id, "Connection destination mismatch from node {} port {}", from_id, from_port);
             assert_eq!(conn.port, to_port, "Connection port mismatch from node {} port {}", from_id, from_port);
        } else {
             panic!("Expected connection from node {} port {} to node {} port {}, but found none or port index invalid",
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
                Node::Constructor { ports } | Node::Duplicator { ports } | Node::Operator { ports, .. } | Node::Switch { ports } => {
                     println!("  Ports:");
                     for (j, port) in ports.iter().enumerate() {
                         if let Some(conn) = port {
                             println!("    Port {} -> Node {} Port {}", j, conn.destination, conn.port);
                         } else {
                             println!("    Port {} -> None", j);
                         }
                     }
                }
                Node::Variable { name: symbol, ports } => {
                    println!("  Variable Symbol({}) ports:", symbol.id());
                    for (j, port) in ports.iter().enumerate() {
                        if let Some(conn) = port {
                            println!("    Port {} -> Node {} Port {}", j, conn.destination, conn.port);
                        } else {
                            println!("    Port {} -> None", j);
                        }
                    }
                }
                 Node::Reference { name: symbol, ports } => {
                     println!("  Reference Symbol({}) ports:", symbol.id());
                     if let Some(conn) = ports[0] {
                         println!("    Port 0 -> Node {} Port {}", conn.destination, conn.port);
                     } else {
                         println!("    Port 0 -> None");
                     }
                 }
                 Node::Numeric { value, ports } => {
                     println!("  Numeric {:?} ports:", value);
                     if let Some(conn) = ports[0] {
                           println!("    Port 0 -> Node {} Port {}", conn.destination, conn.port);
                       } else {
                           println!("    Port 0 -> None");
                       }
                 }
                 Node::Eraser { ports } => {
                     println!("  Eraser ports:");
                     if let Some(conn) = ports[0] {
                          println!("    Port 0 -> Node {} Port {}", conn.destination, conn.port);
                     } else {
                         println!("    Port 0 -> None");
                     }
                 }
            }
        }
    }

    #[test]
    fn test_basic_numeric_conversion() {
        let book = create_test_book(vec![
            ("test_net", Tree::Numeric(AstNumeric::Number("42")))
        ]);
        let ir_book: DisconnectedVariables = book.into();
        assert_eq!(ir_book.named_nets.len(), 1);
        let net = &ir_book.named_nets[0];
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

        assert_eq!(net.nodes.len(), 3, "Expected 3 nodes");
        print_node_structure(&net.nodes);

        assert!(matches!(net.nodes[0], Node::Constructor { .. }));
        assert!(matches!(net.nodes[1], Node::Numeric { value: Numeric::Integer(1), .. }));
        assert!(matches!(net.nodes[2], Node::Numeric { value: Numeric::Integer(2), .. }));

        assert_connected(&net.nodes, 0, 1, 1, 0);
        assert_connected(&net.nodes, 0, 2, 2, 0);
        assert_connected(&net.nodes, 1, 0, 0, 1);
        assert_connected(&net.nodes, 2, 0, 0, 2);
    }

    #[test]
    fn test_variable_node_removal_simple() {
        let tree = Tree::Constructor(
            Box::new(Tree::Variable("x")),
            Box::new(Tree::Variable("x"))
        );
        let book = create_test_book(vec![("test", tree)]);
        let ir_book: DisconnectedVariables = book.into();
        let net = &ir_book.named_nets[0];

        println!("Nodes after conversion (simple variable):");
        print_node_structure(&net.nodes);

        assert_eq!(net.nodes.len(), 3);
        assert!(matches!(net.nodes[0], Node::Constructor { .. }));

        let mut var_count = 0;
        let mut var_symbol = None;
        for node in &net.nodes {
            if let Node::Variable { name: symbol, ports, .. } = node {
                assert!(ports[0].is_none(), "Variable node should be disconnected after disconnect_variable_nodes");
                var_count += 1;
                if var_symbol.is_none() { var_symbol = Some(symbol); }
                else { assert_eq!(var_symbol.unwrap(), symbol, "Both variables 'x' should have the same generated symbol"); }
            }
        }
        assert_eq!(var_count, 2, "Should have exactly two variable nodes");

        if let Node::Constructor { ports } = &net.nodes[0] {
            assert!(ports[1].is_some(), "Constructor port 1 should be connected");
            assert!(ports[2].is_some(), "Constructor port 2 should be connected");
            let conn1 = ports[1].unwrap();
            let conn2 = ports[2].unwrap();
            assert_eq!(conn1.destination, 0, "Port 1 should connect back to constructor");
            assert_eq!(conn1.port, 2, "Port 1 should connect back to port 2");
            assert_eq!(conn2.destination, 0, "Port 2 should connect back to constructor");
            assert_eq!(conn2.port, 1, "Port 2 should connect back to port 1");
        } else {
            panic!("Node 0 should be a constructor");
        }
    }

    #[test]
    fn test_variable_node_removal_chain() {
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
        print_node_structure(&net.nodes);

        assert_eq!(net.nodes.len(), 5);
        let (var_count, constructor_count) = count_node_types(&net.nodes);
        assert_eq!(constructor_count, 2, "Should have 2 constructors");
        assert_eq!(var_count, 3, "Should have 3 variables");

        let mut x_symbol = None;
        let mut y_symbol = None;
        let mut x_count = 0;
        let mut y_count = 0;

        for node in &net.nodes {
            if let Node::Variable { name: symbol, ports, .. } = node {
                assert!(ports[0].is_none(), "Variable node port 0 should be None after disconnect");
                if matches!(node, Node::Variable{ name: _, ports: _ }) {
                     let node_id = net.nodes.iter().position(|n| n as *const _ == node as *const _).unwrap();
                     if node_id == 1 || node_id == 4 {
                         if x_symbol.is_none() { x_symbol = Some(symbol); }
                         else { assert_eq!(x_symbol.unwrap(), symbol); }
                         x_count += 1;
                     } else if node_id == 3 {
                          if y_symbol.is_none() { y_symbol = Some(symbol); }
                          else { assert_eq!(y_symbol.unwrap(), symbol); }
                          y_count += 1;
                     }
                }
            }
        }
        assert_eq!(x_count, 2);
        assert_eq!(y_count, 1);
        assert_ne!(x_symbol.unwrap(), y_symbol.unwrap(), "Symbols for 'x' and 'y' should differ");

        assert_connected(&net.nodes, 0, 1, 2, 2);
        assert_connected(&net.nodes, 0, 2, 2, 0);
        assert!(net.nodes[2].ports()[1].is_none(), "C1.p1 should be disconnected");
        assert_connected(&net.nodes, 2, 2, 0, 1);
    }

    #[test]
    fn test_variable_node_removal_multiple_references() {
        let tree = Tree::Constructor(
            Box::new(Tree::Variable("y")),
            Box::new(Tree::Constructor(
                Box::new(Tree::Variable("x")),
                Box::new(Tree::Constructor(
                    Box::new(Tree::Variable("y")),
                    Box::new(Tree::Variable("x"))
                ))
            ))
        );
        let book = create_test_book(vec![("test", tree)]);
        let ir_book: DisconnectedVariables = book.into();
        let net = &ir_book.named_nets[0];

        println!("Nodes in multiple references test:");
        print_node_structure(&net.nodes);

        let (var_count, constructor_count) = count_node_types(&net.nodes);
        assert_eq!(constructor_count, 3, "Should have 3 constructors");
        assert_eq!(var_count, 4, "Should have 4 variables");
        assert_eq!(net.nodes.len(), 7, "Should have 7 nodes total");

        let mut symbols = HashMap::new();
        for node in &net.nodes {
            if let Node::Variable { name: symbol, ports, .. } = node {
                assert!(ports[0].is_none(), "Variable node should be disconnected");
                *symbols.entry(symbol).or_insert(0) += 1;
            }
        }
        assert_eq!(symbols.len(), 2, "Should only be 2 unique symbols (for x and y)");
        for count in symbols.values() {
             assert_eq!(*count, 2, "Each unique symbol should appear twice");
        }

        assert_connected(&net.nodes, 0, 1, 4, 1);
        assert_connected(&net.nodes, 0, 2, 2, 0);
        assert_connected(&net.nodes, 2, 1, 4, 2);
        assert_connected(&net.nodes, 2, 2, 4, 0);
        assert_connected(&net.nodes, 4, 1, 0, 1);
        assert_connected(&net.nodes, 4, 2, 2, 1);
    }

    #[test]
    fn test_operator_conversion() {
        let tree = Tree::Numeric(AstNumeric::Operator(AstOperation::Unapplied(ast::Operator::Add)));
        let book = create_test_book(vec![("test", tree)]);
        let ir_book: DisconnectedVariables = book.into();
        let net = &ir_book.named_nets[0];
        assert_eq!(net.nodes.len(), 1);
        match &net.nodes[0] {
            Node::Numeric { value, .. } => {
                match value {
                    Numeric::Operator(Operation::Unapplied(op)) => {
                        assert_eq!(*op, Operator::Add);
                    },
                    _ => panic!("Expected unapplied operator"),
                }
            },
            _ => panic!("Expected numeric node"),
        }
    }

    #[test]
    fn test_redex_creation() {
        let net_ast = Network {
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
            named_nets: vec![NamedNetwork { name: "test", net: net_ast }],
        };

        let ir_book: DisconnectedVariables = book.into();
        let ir_net = &ir_book.named_nets[0];

        println!("Nodes in redex test:");
        print_node_structure(&ir_net.nodes);

        assert_eq!(ir_net.nodes.len(), 5);
        assert_eq!(ir_net.redexes.len(), 1);

        let (left_id, right_id) = ir_net.redexes[0];
        assert!((left_id == 3 && right_id == 4) || (left_id == 4 && right_id == 3), "Redex should connect nodes 3 and 4");
        assert_connected(&ir_net.nodes, left_id, 0, right_id, 0);
        assert_connected(&ir_net.nodes, right_id, 0, left_id, 0);
    }

    #[test]
    fn test_variable_node_entry_point() {
        let tree = Tree::Variable("x");
        let book = create_test_book(vec![("test", tree.clone())]);

        let mut nodes = vec![];
        let mut ctx = Ctx {
             variable_symbols: HashMap::new(),
             variable_locations: HashMap::new(),
             nodes: &mut nodes,
        };
        let _ = traverse(tree, &mut ctx);
        nodes.push(Node::Constructor { ports: [None, None, None] });
        nodes[0].set_port(0, Connection { destination: 1, port: 2 });
        nodes[1].set_port(2, Connection { destination: 0, port: 0 });

        println!("Nodes before disconnect (entry point test):");
        print_node_structure(&nodes);

        let new_entry = disconnect_variable_nodes(&mut nodes);

        println!("\nNodes after disconnect (entry point test):");
        print_node_structure(&nodes);

        assert_eq!(new_entry, Some((1, 2)), "Entry point should be redirected to constructor port 2");
        assert!(nodes[0].ports()[0].is_none(), "Variable should be disconnected");
    }
}
