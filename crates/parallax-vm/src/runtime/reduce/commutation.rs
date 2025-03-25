use parallax_error::VMError;
use crate::graph::{Net, Redex};

use super::ReductionResult;

pub fn reduce_commutation(net: &mut Net, redex: Redex) -> ReductionResult {
    // Get the nodes involved in the reduction
    let left_node = net.get_node(redex.left)
        .ok_or_else(|| VMError::Runtime { message: "Left node not found".into() })?;
    let right_node = net.get_node(redex.right)
        .ok_or_else(|| VMError::Runtime { message: "Right node not found".into() })?;

    // Store node types before creating new nodes
    let left_type = left_node.node_type().clone();
    let right_type = right_node.node_type().clone();

    // Store port connections before modifying the network
    let mut left_connections = Vec::new();
    let mut right_connections = Vec::new();

    for i in 1..left_node.port_count() {
        if let Some(port) = left_node.port(i) {
            if let Some(connected_to) = port.connected_to() {
                left_connections.push((i, connected_to));
            }
        }
    }

    for i in 1..right_node.port_count() {
        if let Some(port) = right_node.port(i) {
            if let Some(connected_to) = port.connected_to() {
                right_connections.push((i, connected_to));
            }
        }
    }

    // Create new nodes with swapped types
    let new_left = net.add_node(right_type);
    let new_right = net.add_node(left_type);

    // Connect the main ports
    net.connect((new_left, 0), (new_right, 0));

    // Connect auxiliary ports using stored connections
    for (i, connected_to) in right_connections {
        net.connect((new_left, i), (connected_to, 0));
    }

    for (i, connected_to) in left_connections {
        net.connect((new_right, i), (connected_to, 0));
    }

    // Remove the original nodes
    net.remove_node(redex.left);
    net.remove_node(redex.right);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::{NodeType, Operator};

    #[test]
    fn test_commutation() {
        let mut net = Net::new();

        let cons_str = net.intern("Cons");
        
        // Create an operator node and a constructor node
        let op = net.add_node(NodeType::Operator(Operator::Add));
        let cons = net.add_node(NodeType::Constructor(cons_str));
        
        // Connect them
        net.connect((op, 0), (cons, 0));
        
        // Create a redex for reduction
        let redex = Redex {
            left: op,
            right: cons,
            left_port: 0,
            right_port: 0,
        };
        
        // Reduce the redex
        let result = reduce_commutation(&mut net, redex);
        assert!(result.is_ok());
        
        // Verify that the original nodes are removed
        assert!(net.get_node(op).is_none());
        assert!(net.get_node(cons).is_none());
        
        // Find the new nodes
        let mut found_op = false;
        let mut found_cons = false;
        
        for i in 0..net.node_count() as u32 {
            if let Some(node) = net.get_node(i) {
                match node.node_type() {
                    NodeType::Operator(Operator::Add) => found_op = true,
                    NodeType::Constructor(_) => found_cons = true,
                    _ => {}
                }
            }
        }
        
        assert!(found_op, "New operator node should be created");
        assert!(found_cons, "New constructor node should be created");
    }
} 