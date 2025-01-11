use crate::{VMError, VMResult};
use parallax_ir::ast;

use super::{Context, compile_numeric};
use crate::graph::NodeType;

/// Compiles a tree node into a graph node, returning its ID
pub fn compile_node(ctx: &mut Context, tree: &ast::Tree) -> VMResult<u32> {
    match tree {
        ast::Tree::Var(name) => {
            let id = ctx.net.intern(name);
            Ok(ctx.net.add_node(NodeType::Variable(id)))
        }
        ast::Tree::Node(node) => compile_node_type(ctx, node),
    }
}

/// Compiles a node type into a graph node
fn compile_node_type(ctx: &mut Context, node: &ast::Node) -> VMResult<u32> {
    match node {
        ast::Node::Eraser => Ok(ctx.net.add_node(NodeType::Eraser)),
        
        ast::Node::Reference(name) => {
            let id = ctx.net.intern(name);
            Ok(ctx.net.add_node(NodeType::Reference(id)))
        }
        
        ast::Node::Numeric(num) => compile_numeric(ctx, num),
        
        ast::Node::Constructor(left, right) => {
            // Create constructor node
            let cons_id = ctx.net.add_node(NodeType::Constructor(ctx.current_def.ok_or_else(|| {
                VMError::Runtime {
                    message: "Constructor outside definition".into()
                }
            })?));
            
            // Compile and connect children
            let left_id = compile_node(ctx, left)?;
            let right_id = compile_node(ctx, right)?;
            
            ctx.net.connect((cons_id, 1), (left_id, 0));
            ctx.net.connect((cons_id, 2), (right_id, 0));
            
            Ok(cons_id)
        }
        
        ast::Node::Duplicator(left, right) => {
            // Create duplicator node
            let dup_id = ctx.net.add_node(NodeType::Duplicator);
            
            // Compile and connect children
            let left_id = compile_node(ctx, left)?;
            let right_id = compile_node(ctx, right)?;
            
            ctx.net.connect((dup_id, 1), (left_id, 0));
            ctx.net.connect((dup_id, 2), (right_id, 0));
            
            Ok(dup_id)
        }
        
        ast::Node::Operator(left, right) => {
            // Create operator node (default to Add, numeric compilation will handle actual operator)
            let op_id = ctx.net.add_node(NodeType::Operator(crate::graph::Operator::Add));
            
            // Compile and connect children
            let left_id = compile_node(ctx, left)?;
            let right_id = compile_node(ctx, right)?;
            
            ctx.net.connect((op_id, 1), (left_id, 0));
            ctx.net.connect((op_id, 2), (right_id, 0));
            
            Ok(op_id)
        }
        
        ast::Node::Switch(left, right) => {
            // Create switch node
            let switch_id = ctx.net.add_node(NodeType::Switch);
            
            // Compile and connect children
            let left_id = compile_node(ctx, left)?;
            let right_id = compile_node(ctx, right)?;
            
            ctx.net.connect((switch_id, 1), (left_id, 0));
            ctx.net.connect((switch_id, 2), (right_id, 0));
            
            Ok(switch_id)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::{GlobalNetwork, Operator};

    #[test]
    fn test_compile_variable() {
        let mut net = GlobalNetwork::new();
        let mut ctx = Context::new(&mut net);
        
        let tree = ast::Tree::Var("x");
        let id = compile_node(&mut ctx, &tree).unwrap();
        
        if let Some(node) = net.get_node(id) {
            match node.node_type() {
                NodeType::Variable(name) => {
                    assert_eq!(net.resolve(*name).unwrap(), "x");
                }
                _ => panic!("Expected variable node"),
            }
        } else {
            panic!("Node should exist");
        }
    }

    #[test]
    fn test_compile_eraser() {
        let mut net = GlobalNetwork::new();
        let mut ctx = Context::new(&mut net);
        
        let tree = ast::Tree::Node(ast::Node::Eraser);
        let id = compile_node(&mut ctx, &tree).unwrap();
        
        if let Some(node) = net.get_node(id) {
            assert!(matches!(node.node_type(), NodeType::Eraser));
        } else {
            panic!("Node should exist");
        }
    }

    #[test]
    fn test_compile_reference() {
        let mut net = GlobalNetwork::new();
        let mut ctx = Context::new(&mut net);
        
        let tree = ast::Tree::Node(ast::Node::Reference("main"));
        let id = compile_node(&mut ctx, &tree).unwrap();
        
        if let Some(node) = net.get_node(id) {
            match node.node_type() {
                NodeType::Reference(name) => {
                    assert_eq!(net.resolve(*name).unwrap(), "main");
                }
                _ => panic!("Expected reference node"),
            }
        } else {
            panic!("Node should exist");
        }
    }

    #[test]
    fn test_compile_constructor() {
        let mut net = GlobalNetwork::new();
        let def_name = net.intern("test_def");
        let mut ctx = Context::with_definition(&mut net, def_name);
        
        let tree = ast::Tree::Node(ast::Node::Constructor(
            Box::new(ast::Tree::Node(ast::Node::Numeric(ast::Numeric::Number("1")))),
            Box::new(ast::Tree::Node(ast::Node::Numeric(ast::Numeric::Number("2")))),
        ));
        
        let id = compile_node(&mut ctx, &tree).unwrap();
        
        if let Some(node) = net.get_node(id) {
            assert!(matches!(node.node_type(), NodeType::Constructor(_)));
            
            // Verify connections to children
            if let Some(port1) = node.port(1) {
                if let Some(child_id) = port1.connected_to() {
                    if let Some(child) = net.get_node(child_id) {
                        assert!(matches!(child.node_type(), NodeType::Number(1)));
                    }
                }
            }
            
            if let Some(port2) = node.port(2) {
                if let Some(child_id) = port2.connected_to() {
                    if let Some(child) = net.get_node(child_id) {
                        assert!(matches!(child.node_type(), NodeType::Number(2)));
                    }
                }
            }
        } else {
            panic!("Node should exist");
        }
    }

    #[test]
    fn test_compile_duplicator() {
        let mut net = GlobalNetwork::new();
        let mut ctx = Context::new(&mut net);
        
        let tree = ast::Tree::Node(ast::Node::Duplicator(
            Box::new(ast::Tree::Node(ast::Node::Numeric(ast::Numeric::Number("1")))),
            Box::new(ast::Tree::Node(ast::Node::Numeric(ast::Numeric::Number("2")))),
        ));
        
        let id = compile_node(&mut ctx, &tree).unwrap();
        
        if let Some(node) = net.get_node(id) {
            assert!(matches!(node.node_type(), NodeType::Duplicator));
            
            // Verify connections to children
            if let Some(port1) = node.port(1) {
                if let Some(child_id) = port1.connected_to() {
                    if let Some(child) = net.get_node(child_id) {
                        assert!(matches!(child.node_type(), NodeType::Number(1)));
                    }
                }
            }
            
            if let Some(port2) = node.port(2) {
                if let Some(child_id) = port2.connected_to() {
                    if let Some(child) = net.get_node(child_id) {
                        assert!(matches!(child.node_type(), NodeType::Number(2)));
                    }
                }
            }
        } else {
            panic!("Node should exist");
        }
    }

    #[test]
    fn test_compile_operator() {
        let mut net = GlobalNetwork::new();
        let mut ctx = Context::new(&mut net);
        
        let tree = ast::Tree::Node(ast::Node::Operator(
            Box::new(ast::Tree::Node(ast::Node::Numeric(ast::Numeric::Number("1")))),
            Box::new(ast::Tree::Node(ast::Node::Numeric(ast::Numeric::Number("2")))),
        ));
        
        let id = compile_node(&mut ctx, &tree).unwrap();
        
        if let Some(node) = net.get_node(id) {
            assert!(matches!(node.node_type(), NodeType::Operator(Operator::Add)));
            
            // Verify connections to children
            if let Some(port1) = node.port(1) {
                if let Some(child_id) = port1.connected_to() {
                    if let Some(child) = net.get_node(child_id) {
                        assert!(matches!(child.node_type(), NodeType::Number(1)));
                    }
                }
            }
            
            if let Some(port2) = node.port(2) {
                if let Some(child_id) = port2.connected_to() {
                    if let Some(child) = net.get_node(child_id) {
                        assert!(matches!(child.node_type(), NodeType::Number(2)));
                    }
                }
            }
        } else {
            panic!("Node should exist");
        }
    }

    #[test]
    fn test_compile_switch() {
        let mut net = GlobalNetwork::new();
        let mut ctx = Context::new(&mut net);
        
        let tree = ast::Tree::Node(ast::Node::Switch(
            Box::new(ast::Tree::Node(ast::Node::Numeric(ast::Numeric::Number("1")))),
            Box::new(ast::Tree::Node(ast::Node::Numeric(ast::Numeric::Number("2")))),
        ));
        
        let id = compile_node(&mut ctx, &tree).unwrap();
        
        if let Some(node) = net.get_node(id) {
            assert!(matches!(node.node_type(), NodeType::Switch));
            
            // Verify connections to children
            if let Some(port1) = node.port(1) {
                if let Some(child_id) = port1.connected_to() {
                    if let Some(child) = net.get_node(child_id) {
                        assert!(matches!(child.node_type(), NodeType::Number(1)));
                    }
                }
            }
            
            if let Some(port2) = node.port(2) {
                if let Some(child_id) = port2.connected_to() {
                    if let Some(child) = net.get_node(child_id) {
                        assert!(matches!(child.node_type(), NodeType::Number(2)));
                    }
                }
            }
        } else {
            panic!("Node should exist");
        }
    }

    #[test]
    fn test_constructor_without_definition() {
        let mut net = GlobalNetwork::new();
        let mut ctx = Context::new(&mut net);
        
        let tree = ast::Tree::Node(ast::Node::Constructor(
            Box::new(ast::Tree::Node(ast::Node::Numeric(ast::Numeric::Number("1")))),
            Box::new(ast::Tree::Node(ast::Node::Numeric(ast::Numeric::Number("2")))),
        ));
        
        assert!(compile_node(&mut ctx, &tree).is_err());
    }
} 