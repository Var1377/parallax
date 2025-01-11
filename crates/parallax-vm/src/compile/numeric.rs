use crate::{VMResult, VMError};
use parallax_ir::ast;

use super::Context;
use crate::graph::{NodeType, Operator};

/// Compiles a numeric node into a graph node
pub fn compile_numeric(ctx: &mut Context, num: &ast::Numeric) -> VMResult<u32> {
    match num {
        ast::Numeric::Number(n) => {
            // Parse the number string
            let value = n.parse::<i64>().map_err(|_| {
                VMError::Runtime {
                    message: format!("Invalid number: {}", n)
                }
            })?;
            
            Ok(ctx.net.add_node(NodeType::Number(value)))
        }
        
        ast::Numeric::Operator(op) => {
            match op {
                ast::Operator::Unapplied(op_str) => {
                    // Convert operation string to operator type
                    let operator = match *op_str {
                        "+" => Operator::Add,
                        "-" => Operator::Sub,
                        "*" => Operator::Mul,
                        "/" => Operator::Div,
                        "%" => Operator::Mod,
                        "==" => Operator::Eq,
                        "!=" => Operator::Ne,
                        "<" => Operator::Lt,
                        "<=" => Operator::Le,
                        ">" => Operator::Gt,
                        ">=" => Operator::Ge,
                        "&" => Operator::And,
                        "|" => Operator::Or,
                        "^" => Operator::Xor,
                        _ => return Err(VMError::Runtime {
                            message: format!("Unknown operator: {}", op_str)
                        }),
                    };
                    
                    Ok(ctx.net.add_node(NodeType::Operator(operator)))
                }
                
                ast::Operator::PartiallyApplied(op_str, value) => {
                    // Parse the value
                    let num = value.parse::<i64>().map_err(|_| {
                        VMError::Runtime {
                            message: format!("Invalid number in operator: {}", value)
                        }
                    })?;
                    
                    // Create operator node
                    let operator = match *op_str {
                        "+" => Operator::Add,
                        "-" => Operator::Sub,
                        "*" => Operator::Mul,
                        "/" => Operator::Div,
                        "%" => Operator::Mod,
                        "==" => Operator::Eq,
                        "!=" => Operator::Ne,
                        "<" => Operator::Lt,
                        "<=" => Operator::Le,
                        ">" => Operator::Gt,
                        ">=" => Operator::Ge,
                        "&" => Operator::And,
                        "|" => Operator::Or,
                        "^" => Operator::Xor,
                        _ => return Err(VMError::Runtime {
                            message: format!("Unknown operator: {}", op_str)
                        }),
                    };
                    
                    let op_id = ctx.net.add_node(NodeType::Operator(operator));
                    let num_id = ctx.net.add_node(NodeType::Number(num));
                    
                    // Connect the number to port 2 (right operand)
                    ctx.net.connect((op_id, 2), (num_id, 0));
                    
                    Ok(op_id)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::GlobalNetwork;

    #[test]
    fn test_compile_number() {
        let mut net = GlobalNetwork::new();
        let mut ctx = Context::new(&mut net);
        
        let num = ast::Numeric::Number("42");
        let id = compile_numeric(&mut ctx, &num).unwrap();
        
        if let Some(node) = net.get_node(id) {
            assert!(matches!(node.node_type(), NodeType::Number(42)));
        } else {
            panic!("Node should exist");
        }
    }

    #[test]
    fn test_compile_invalid_number() {
        let mut net = GlobalNetwork::new();
        let mut ctx = Context::new(&mut net);
        
        let num = ast::Numeric::Number("not_a_number");
        assert!(compile_numeric(&mut ctx, &num).is_err());
    }

    #[test]
    fn test_compile_unapplied_operator() {
        let mut net = GlobalNetwork::new();
        let mut ctx = Context::new(&mut net);
        
        let num = ast::Numeric::Operator(ast::Operator::Unapplied("+"));
        let id = compile_numeric(&mut ctx, &num).unwrap();
        
        if let Some(node) = net.get_node(id) {
            assert!(matches!(node.node_type(), NodeType::Operator(Operator::Add)));
        } else {
            panic!("Node should exist");
        }
    }

    #[test]
    fn test_compile_partially_applied_operator() {
        let mut net = GlobalNetwork::new();
        let mut ctx = Context::new(&mut net);
        
        let num = ast::Numeric::Operator(ast::Operator::PartiallyApplied("+", "42"));
        let id = compile_numeric(&mut ctx, &num).unwrap();
        
        if let Some(node) = net.get_node(id) {
            assert!(matches!(node.node_type(), NodeType::Operator(Operator::Add)));
            
            // Verify connection to number
            if let Some(port2) = node.port(2) {
                if let Some(child_id) = port2.connected_to() {
                    if let Some(child) = net.get_node(child_id) {
                        assert!(matches!(child.node_type(), NodeType::Number(42)));
                    }
                }
            }
        } else {
            panic!("Node should exist");
        }
    }

    #[test]
    fn test_compile_invalid_operator() {
        let mut net = GlobalNetwork::new();
        let mut ctx = Context::new(&mut net);
        
        let num = ast::Numeric::Operator(ast::Operator::Unapplied("invalid"));
        assert!(compile_numeric(&mut ctx, &num).is_err());
    }

    #[test]
    fn test_compile_invalid_partial_operator() {
        let mut net = GlobalNetwork::new();
        let mut ctx = Context::new(&mut net);
        
        let num = ast::Numeric::Operator(ast::Operator::PartiallyApplied("+", "not_a_number"));
        assert!(compile_numeric(&mut ctx, &num).is_err());
    }
} 