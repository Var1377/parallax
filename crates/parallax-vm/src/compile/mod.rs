use crate::VMResult;
use parallax_ir::ast;

use crate::graph::{GlobalNetwork, IString};

mod book;
mod net;
mod node;
mod numeric;

pub use book::compile_book;
pub use net::compile_net;
pub use node::compile_node;
pub use numeric::compile_numeric;

/// Context for compilation
pub struct Context<'a> {
    /// The network being built
    pub(crate) net: &'a mut GlobalNetwork,
    /// The current definition being compiled, if any
    pub(crate) current_def: Option<IString>,
}

impl<'a> Context<'a> {
    /// Creates a new compilation context
    pub fn new(net: &'a mut GlobalNetwork) -> Self {
        Self {
            net,
            current_def: None,
        }
    }
    
    /// Creates a new compilation context with a current definition
    pub fn with_definition(net: &'a mut GlobalNetwork, def_name: IString) -> Self {
        Self {
            net,
            current_def: Some(def_name),
        }
    }
}

/// Compiles an AST into a graph
pub fn compile(ast: &ast::Book) -> VMResult<GlobalNetwork> {
    let mut net = GlobalNetwork::new();
    let mut ctx = Context::new(&mut net);
    compile_book(&mut ctx, ast)?;
    Ok(net)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_empty_book() {
        let book = ast::Book {
            definitions: vec![],
        };
        
        let net = compile(&book).unwrap();
        assert_eq!(net.definitions.len(), 0);
    }

    #[test]
    fn test_compile_single_definition() {
        let book = ast::Book {
            definitions: vec![
                ast::Definition {
                    name: "main",
                    net: ast::Net {
                        tree: ast::Tree::Node(ast::Node::Numeric(ast::Numeric::Number("42"))),
                        redexes: vec![],
                    },
                },
            ],
        };
        
        let mut net = compile(&book).unwrap();
        
        // Check that the definition was added
        assert_eq!(net.definitions.len(), 1);
        
        // Get the definition
        let def_name = net.intern("main");
        let def = net.definitions.get(&def_name).unwrap();
        
        // Check the root node
        if let Some(root) = net.get_node(def.root_id) {
            assert!(matches!(root.node_type(), crate::graph::NodeType::Number(42)));
        } else {
            panic!("Root node should exist");
        }
        
        // Check that all nodes are tracked
        assert_eq!(def.nodes.len(), 1);
        assert!(def.nodes.contains(&def.root_id));
    }

    #[test]
    fn test_compile_multiple_definitions() {
        let book = ast::Book {
            definitions: vec![
                ast::Definition {
                    name: "first",
                    net: ast::Net {
                        tree: ast::Tree::Node(ast::Node::Numeric(ast::Numeric::Number("1"))),
                        redexes: vec![],
                    },
                },
                ast::Definition {
                    name: "second",
                    net: ast::Net {
                        tree: ast::Tree::Node(ast::Node::Numeric(ast::Numeric::Number("2"))),
                        redexes: vec![],
                    },
                },
            ],
        };
        
        let mut net = compile(&book).unwrap();
        
        // Check that both definitions were added
        assert_eq!(net.definitions.len(), 2);
        
        // Check first definition
        let first_name = net.intern("first");
        let first_def = net.definitions.get(&first_name).unwrap();
        
        if let Some(root) = net.get_node(first_def.root_id) {
            assert!(matches!(root.node_type(), crate::graph::NodeType::Number(1)));
        } else {
            panic!("First root node should exist");
        }
        
        assert_eq!(first_def.nodes.len(), 1);
        assert!(first_def.nodes.contains(&first_def.root_id));
        
        // Check second definition
        let second_name = net.intern("second");
        let second_def = net.definitions.get(&second_name).unwrap();
        
        if let Some(root) = net.get_node(second_def.root_id) {
            assert!(matches!(root.node_type(), crate::graph::NodeType::Number(2)));
        } else {
            panic!("Second root node should exist");
        }
        
        assert_eq!(second_def.nodes.len(), 1);
        assert!(second_def.nodes.contains(&second_def.root_id));
    }

    #[test]
    fn test_compile_definition_with_redex() {
        let book = ast::Book {
            definitions: vec![
                ast::Definition {
                    name: "main",
                    net: ast::Net {
                        tree: ast::Tree::Node(ast::Node::Numeric(ast::Numeric::Number("1"))),
                        redexes: vec![
                            ast::Redex {
                                strict: false,
                                left: ast::Tree::Node(ast::Node::Numeric(ast::Numeric::Number("2"))),
                                right: ast::Tree::Node(ast::Node::Numeric(ast::Numeric::Number("3"))),
                            },
                        ],
                    },
                },
            ],
        };
        
        let mut net = compile(&book).unwrap();
        
        // Check that the definition was added
        assert_eq!(net.definitions.len(), 1);
        
        // Get the definition
        let def_name = net.intern("main");
        let def = net.definitions.get(&def_name).unwrap();
        
        // Check the root node
        if let Some(root) = net.get_node(def.root_id) {
            assert!(matches!(root.node_type(), crate::graph::NodeType::Number(1)));
        } else {
            panic!("Root node should exist");
        }
        
        // Check that all nodes are tracked (root + 2 redex nodes)
        assert_eq!(def.nodes.len(), 3);
        
        // Find the redex nodes
        let mut found_left = false;
        let mut found_right = false;
        
        for &node_id in &def.nodes {
            if let Some(node) = net.get_node(node_id) {
                match node.node_type() {
                    crate::graph::NodeType::Number(2) => {
                        found_left = true;
                        // Check connection
                        if let Some(port) = node.port(0) {
                            if let Some(other_id) = port.connected_to() {
                                if let Some(other) = net.get_node(other_id) {
                                    assert!(matches!(other.node_type(), crate::graph::NodeType::Number(3)));
                                }
                            }
                        }
                    }
                    crate::graph::NodeType::Number(3) => {
                        found_right = true;
                        // Check connection
                        if let Some(port) = node.port(0) {
                            if let Some(other_id) = port.connected_to() {
                                if let Some(other) = net.get_node(other_id) {
                                    assert!(matches!(other.node_type(), crate::graph::NodeType::Number(2)));
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        
        assert!(found_left, "Left redex node not found");
        assert!(found_right, "Right redex node not found");
    }
} 