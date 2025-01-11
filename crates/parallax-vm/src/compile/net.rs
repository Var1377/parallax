use crate::VMResult;
use parallax_ir::ast;

use super::{Context, compile_node};

/// Compiles a net into a graph
pub fn compile_net(ctx: &mut Context, net: &ast::Net) -> VMResult<u32> {
    // Compile the main tree
    let root_id = compile_node(ctx, &net.tree)?;
    
    // Compile and connect all redexes
    for redex in &net.redexes {
        // Compile left and right sides
        let left_id = compile_node(ctx, &redex.left)?;
        let right_id = compile_node(ctx, &redex.right)?;
        
        // Connect them
        ctx.net.connect((left_id, 0), (right_id, 0));
        
        // Add to redexes list if strict
        if redex.strict {
            // Clear any existing redexes
            ctx.net.redexes.clear();
            
            // Add this redex
            ctx.net.redexes.push(crate::graph::Redex {
                left: left_id,
                left_port: 0,
                right: right_id,
                right_port: 0,
            });
        }
    }
    
    Ok(root_id)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::{GlobalNetwork, NodeType};

    #[test]
    fn test_compile_simple_net() {
        let mut net = GlobalNetwork::new();
        let mut ctx = Context::new(&mut net);
        
        let ast_net = ast::Net {
            tree: ast::Tree::Node(ast::Node::Numeric(ast::Numeric::Number("42"))),
            redexes: vec![],
        };
        
        let id = compile_net(&mut ctx, &ast_net).unwrap();
        
        if let Some(node) = net.get_node(id) {
            assert!(matches!(node.node_type(), NodeType::Number(42)));
        } else {
            panic!("Node should exist");
        }
    }

    #[test]
    fn test_compile_net_with_redex() {
        let mut net = GlobalNetwork::new();
        let mut ctx = Context::new(&mut net);
        
        let ast_net = ast::Net {
            tree: ast::Tree::Node(ast::Node::Numeric(ast::Numeric::Number("1"))),
            redexes: vec![
                ast::Redex {
                    strict: false,
                    left: ast::Tree::Node(ast::Node::Numeric(ast::Numeric::Number("2"))),
                    right: ast::Tree::Node(ast::Node::Numeric(ast::Numeric::Number("3"))),
                }
            ],
        };
        
        let id = compile_net(&mut ctx, &ast_net).unwrap();
        
        // Check root node
        if let Some(node) = net.get_node(id) {
            assert!(matches!(node.node_type(), NodeType::Number(1)));
        } else {
            panic!("Root node should exist");
        }
        
        // Find redex nodes by checking all nodes
        let mut found_left = false;
        let mut found_right = false;
        
        for node in net.nodes.values() {
            match node.node_type() {
                NodeType::Number(2) => {
                    found_left = true;
                    // Check connection
                    if let Some(port) = node.port(0) {
                        if let Some(other_id) = port.connected_to() {
                            if let Some(other) = net.get_node(other_id) {
                                assert!(matches!(other.node_type(), NodeType::Number(3)));
                            }
                        }
                    }
                }
                NodeType::Number(3) => {
                    found_right = true;
                    // Check connection
                    if let Some(port) = node.port(0) {
                        if let Some(other_id) = port.connected_to() {
                            if let Some(other) = net.get_node(other_id) {
                                assert!(matches!(other.node_type(), NodeType::Number(2)));
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        
        assert!(found_left, "Left redex node not found");
        assert!(found_right, "Right redex node not found");
    }

    #[test]
    fn test_compile_net_with_strict_redex() {
        let mut net = GlobalNetwork::new();
        let mut ctx = Context::new(&mut net);
        
        let ast_net = ast::Net {
            tree: ast::Tree::Node(ast::Node::Numeric(ast::Numeric::Number("1"))),
            redexes: vec![
                ast::Redex {
                    strict: true,
                    left: ast::Tree::Node(ast::Node::Numeric(ast::Numeric::Number("2"))),
                    right: ast::Tree::Node(ast::Node::Numeric(ast::Numeric::Number("3"))),
                }
            ],
        };
        
        let id = compile_net(&mut ctx, &ast_net).unwrap();
        
        // Check root node
        if let Some(node) = net.get_node(id) {
            assert!(matches!(node.node_type(), NodeType::Number(1)));
        } else {
            panic!("Root node should exist");
        }
        
        // Find redex nodes and check strictness
        let mut found_left = false;
        let mut found_right = false;
        
        for node in net.nodes.values() {
            match node.node_type() {
                NodeType::Number(2) => {
                    found_left = true;
                    // Check connection
                    if let Some(port) = node.port(0) {
                        if let Some(other_id) = port.connected_to() {
                            if let Some(other) = net.get_node(other_id) {
                                assert!(matches!(other.node_type(), NodeType::Number(3)));
                            }
                        }
                    }
                }
                NodeType::Number(3) => {
                    found_right = true;
                    // Check connection
                    if let Some(port) = node.port(0) {
                        if let Some(other_id) = port.connected_to() {
                            if let Some(other) = net.get_node(other_id) {
                                assert!(matches!(other.node_type(), NodeType::Number(2)));
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        
        assert!(found_left, "Left redex node not found");
        assert!(found_right, "Right redex node not found");
        
        // Check that the redex was added to the list
        assert_eq!(net.redexes.len(), 1);
        let redex = &net.redexes[0];
        assert_eq!(redex.left_port, 0);
        assert_eq!(redex.right_port, 0);
        
        // Find the node IDs
        let mut left_id = None;
        let mut right_id = None;
        
        for (id, node) in net.nodes.iter() {
            match node.node_type() {
                NodeType::Number(2) => left_id = Some(*id),
                NodeType::Number(3) => right_id = Some(*id),
                _ => {}
            }
        }
        
        assert_eq!(redex.left, left_id.unwrap());
        assert_eq!(redex.right, right_id.unwrap());
    }
} 