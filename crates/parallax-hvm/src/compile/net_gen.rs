use super::variable_removal::{RemovedVariables, NamedNet as RemovedNamedNet, Node as RemovedNode};
// Import Numeric and Operation from the variable_disconnection module which is private
use super::variable_disconnection;
use crate::{IRResult, IRError};
use parallax_resolve::types::Symbol;
use parallax_net::{
    CompiledNet, InitialNetConfig, Wire, 
    node::{NodeType, Eraser, Constructor, Duplicator, Static, Number, Switch, Async, Pointer}, // Import from node module
    port::{Port, PortType}, // Import from port module
    encoding::{self, encode_static_data, encode_intrinsic_op, TAG_FUNCTION, TAG_INTRINSIC_OP, TAG_NIL, OPCAT_ARITHMETIC, TYPECODE_I64, TYPECODE_F64, OP_ARITH_ADD, OP_ARITH_SUB, OP_ARITH_MUL, OP_ARITH_DIV, OP_ARITH_REM, OP_ARITH_EQ, OP_ARITH_NE, OP_ARITH_LT, OP_ARITH_LE, OP_ARITH_GT, OP_ARITH_GE, OP_ARITH_AND, OP_ARITH_OR, OP_ARITH_XOR, OP_ARITH_SHL, OP_ARITH_SHR} // Encoding helpers and constants
};
use crate::ast::Operator as AstOperator; // Import HVM AST Operator
use std::collections::{HashMap, HashSet};
use std::sync::atomic::AtomicU64;
use std::str::FromStr; // For parsing strings to numbers

pub fn generate_compiled_net(
    removed_vars: RemovedVariables,
    // Removed symbol_map parameter and TODO
) -> IRResult<CompiledNet> {
    let mut networks = HashMap::new();

    for removed_net in removed_vars.named_nets {
        // Use the Symbol directly from removed_net
        let symbol = removed_net.name;
        // Removed placeholder generation

        let config = convert_named_net(&removed_net)?;
        networks.insert(symbol, config);
    }

    Ok(CompiledNet { networks })
}

// Remove 'a lifetime from function signature
fn convert_named_net(removed_net: &RemovedNamedNet) -> IRResult<InitialNetConfig> {
    let mut constructors = Vec::new(); // Node storage
    let mut duplicators = Vec::new();
    let mut statics = Vec::new();
    let mut numbers = Vec::new();
    let mut switches = Vec::new();
    let mut asyncs = Vec::new();
    let mut erasers = Vec::new();
    let mut pointers = Vec::new();
    
    let mut initial_wires = Vec::new();
    let mut initial_redexes = Vec::new();
    let mut seen_wires: HashSet<Wire> = HashSet::new();
    let mut node_type_map = Vec::new();

    // Partition ID is always 0 as requested
    let partition_id: u16 = 0;

    // --- Pass 1: Create nodes ---
    for (node_index, removed_node) in removed_net.nodes.iter().enumerate() {
        let node_index_u64 = node_index as u64;

        match removed_node {
            RemovedNode::Eraser { .. } => {
                let eraser = Eraser::new_null();
                erasers.push(eraser);
                node_type_map.push(NodeType::Eraser);
            }
            RemovedNode::Reference { name: ref_symbol, .. } => {
                // Use the Symbol directly from the RemovedNode
                let referenced_symbol_id: u64 = ref_symbol.id() as u64;
                // Removed placeholder generation and comments

                let static_data = encode_static_data(TAG_FUNCTION, referenced_symbol_id);
                let mut static_node = Static::new_null();
                static_node.data = AtomicU64::new(static_data);
                statics.push(static_node);
                node_type_map.push(NodeType::Static);
            }
            RemovedNode::Numeric { value, .. } => {
                match value {
                    variable_disconnection::Numeric::Integer(i_val) => {
                        let mut number = Number::new_null();
                        number.data = *i_val as u128;
                        numbers.push(number);
                        node_type_map.push(NodeType::Number);
                    }
                    variable_disconnection::Numeric::Float(f_val) => {
                        let mut number = Number::new_null();
                        number.data = f_val.to_bits() as u128;
                        numbers.push(number);
                        node_type_map.push(NodeType::Number);
                    }
                    variable_disconnection::Numeric::Operator(op) => {
                        let hvm_op = match op {
                            variable_disconnection::Operation::Unapplied(op) => *op,
                            variable_disconnection::Operation::PartiallyApplied(op, val) => {
                                return Err(IRError::Compilation {
                                    message: format!("Partially applied operators '[{} {}]' not yet supported during net generation.", op, val)
                                });
                            }
                        };
                        let encoded_op = map_hvm_operator_to_static(hvm_op)?;
                        let mut static_node = Static::new_null();
                        static_node.data = AtomicU64::new(encoded_op);
                        statics.push(static_node);
                        node_type_map.push(NodeType::Static);
                    }
                }
            }
            RemovedNode::Constructor { .. } => {
                let constructor = Constructor::new_null();
                constructors.push(constructor);
                node_type_map.push(NodeType::Constructor);
            }
            RemovedNode::Duplicator { .. } => {
                let duplicator = Duplicator::new_null();
                duplicators.push(duplicator);
                node_type_map.push(NodeType::Duplicator);
            }
            RemovedNode::Operator { .. } => {
                panic!("Encountered HVM Operator node (e.g., $(a b)) during net generation. This node type is not supported for direct conversion.");
            }
            RemovedNode::Switch { .. } => {
                let switch = Switch::new_null();
                switches.push(switch);
                node_type_map.push(NodeType::Switch);
            }
        }
    }

    // --- Pass 2: Create wires --- (remains the same logic)
    for (node_index, removed_node) in removed_net.nodes.iter().enumerate() {
        let node_index_u64 = node_index as u64;
        let current_node_type_enum = node_type_map[node_index];

        for (port_idx, connection_opt) in removed_node.ports().iter().enumerate() {
            if let Some(conn) = connection_opt {
                let dest_node_index = conn.destination;
                if dest_node_index >= node_type_map.len() { continue; }
                let dest_node_index_u64 = dest_node_index as u64;
                let dest_node_type_enum = node_type_map[dest_node_index];
                let dest_port_idx = conn.port;

                let current_port_type = hvm_port_idx_to_net_port_type(port_idx, current_node_type_enum);
                let dest_port_type = hvm_port_idx_to_net_port_type(dest_port_idx, dest_node_type_enum);

                if current_port_type == PortType::Null || dest_port_type == PortType::Null {
                    continue;
                }

                let port1 = Port::new(current_port_type, current_node_type_enum as u8, partition_id, node_index_u64);
                let port2 = Port::new(dest_port_type, dest_node_type_enum as u8, partition_id, dest_node_index_u64);

                let wire = if port1.as_u64() < port2.as_u64() { Wire(port1, port2) } else { Wire(port2, port1) };

                // Use Eq implementation directly rather than relying on HashSet's Hash trait for Wire
                if seen_wires.insert(wire) {
                    initial_wires.push(wire);
                }
            }
        }
    }

    // --- Map Redexes --- 
    for &(hvm_left_idx, hvm_right_idx) in &removed_net.redexes {
        if hvm_left_idx >= node_type_map.len() || hvm_right_idx >= node_type_map.len() { continue; }

        let left_node_type = node_type_map[hvm_left_idx];
        let right_node_type = node_type_map[hvm_right_idx];

        let port1 = Port::principal(left_node_type, partition_id, hvm_left_idx as u64);
        let port2 = Port::principal(right_node_type, partition_id, hvm_right_idx as u64);

        let wire = if port1.as_u64() < port2.as_u64() { Wire(port1, port2) } else { Wire(port2, port1) };
        initial_redexes.push(wire);
    }

    // --- Determine Entry Point --- 
    let (entry_node_idx, entry_port_idx) = removed_net.entry_point;
    if entry_node_idx >= node_type_map.len() {
         return Err(IRError::Compilation {
            message: format!("Entry point node (index {}) was removed or could not be generated.", entry_node_idx)
        });
    }
    let entry_node_type = node_type_map[entry_node_idx];
    let entry_port_type = hvm_port_idx_to_net_port_type(entry_port_idx, entry_node_type);
    let principal_port = Port::new(entry_port_type, entry_node_type as u8, partition_id, entry_node_idx as u64);

    // Add initial redexes to initial wires
    for redex in initial_redexes {
        if !initial_wires.contains(&redex) {
            initial_wires.push(redex);
        }
    }

    // Convert Vec<T> into the Slab/Map format expected by InitialNetConfig
    let mut config = InitialNetConfig::default();
    
    for constructor in constructors {
        config.constructors.insert(constructor);
    }
    
    for duplicator in duplicators {
        config.duplicators.insert(duplicator);
    }
    
    for static_node in statics {
        config.statics.insert(static_node);
    }
    
    for number in numbers {
        config.numbers.insert(number);
    }
    
    for switch in switches {
        config.switches.insert(switch);
    }
    
    for async_node in asyncs {
        config.asyncs.insert(async_node);
    }
    
    for eraser in erasers {
        config.erasers.insert(eraser);
    }
    
    for pointer in pointers {
        config.pointers.insert(pointer);
    }
    
    config.initial_wires = initial_wires;
    config.root = principal_port;
    
    Ok(config)
}

/// Maps an HVM AST Operator to a parallax-net encoded static value (u64).
fn map_hvm_operator_to_static(op: AstOperator) -> IRResult<u64> {
    // TODO: Determine correct source/target types based on context or default?
    let src_type = TYPECODE_I64; // Defaulting to i64 for now
    let tgt_type = TYPECODE_I64;

    let (op_category, op_code) = match op {
        AstOperator::Add => (OPCAT_ARITHMETIC, OP_ARITH_ADD),
        AstOperator::Sub => (OPCAT_ARITHMETIC, OP_ARITH_SUB),
        AstOperator::Mul => (OPCAT_ARITHMETIC, OP_ARITH_MUL),
        AstOperator::Div => (OPCAT_ARITHMETIC, OP_ARITH_DIV),
        AstOperator::Mod => (OPCAT_ARITHMETIC, OP_ARITH_REM),
        AstOperator::Eq  => (OPCAT_ARITHMETIC, OP_ARITH_EQ),
        AstOperator::Ne  => (OPCAT_ARITHMETIC, OP_ARITH_NE),
        AstOperator::Lt  => (OPCAT_ARITHMETIC, OP_ARITH_LT),
        AstOperator::Gt  => (OPCAT_ARITHMETIC, OP_ARITH_GT),
        AstOperator::Le  => (OPCAT_ARITHMETIC, OP_ARITH_LE),
        AstOperator::Ge  => (OPCAT_ARITHMETIC, OP_ARITH_GE),
        AstOperator::And => (OPCAT_ARITHMETIC, OP_ARITH_AND), // Assuming bitwise
        AstOperator::Or  => (OPCAT_ARITHMETIC, OP_ARITH_OR),  // Assuming bitwise
        AstOperator::Xor => (OPCAT_ARITHMETIC, OP_ARITH_XOR), // Assuming bitwise
        AstOperator::Shl => (OPCAT_ARITHMETIC, OP_ARITH_SHL),
        AstOperator::Shr => (OPCAT_ARITHMETIC, OP_ARITH_SHR),
        // Flipped operators: Map to non-flipped version.
        // The runtime interaction rules or wiring should handle operand order.
        AstOperator::FlippedSub => (OPCAT_ARITHMETIC, OP_ARITH_SUB),
        AstOperator::FlippedDiv => (OPCAT_ARITHMETIC, OP_ARITH_DIV),
        AstOperator::FlippedMod => (OPCAT_ARITHMETIC, OP_ARITH_REM),
        AstOperator::FlippedShl => (OPCAT_ARITHMETIC, OP_ARITH_SHL),
        AstOperator::FlippedShr => (OPCAT_ARITHMETIC, OP_ARITH_SHR),
    };

    Ok(encode_intrinsic_op(op_category, op_code, src_type, tgt_type))
}

/// Converts HVM port indices (0, 1, 2) to parallax-net PortType based on the parallax-net NodeType.
fn hvm_port_idx_to_net_port_type(hvm_port_idx: usize, net_node_type: NodeType) -> PortType {
    match net_node_type {
        // Nodes with 1 port (principal only)
        NodeType::Eraser | NodeType::Static | NodeType::Number | NodeType::Async | NodeType::Pointer => {
            match hvm_port_idx {
                0 => PortType::Principal,
                _ => PortType::Null, // Invalid port index for these types
            }
        }
        // Nodes with 3 ports (principal, left, right)
        NodeType::Constructor | NodeType::Duplicator | NodeType::Switch => {
            match hvm_port_idx {
                0 => PortType::Principal,
                1 => PortType::Left,
                2 => PortType::Right,
                _ => PortType::Null, // Invalid port index
            }
        }
    }
}

// Helper extension trait to get NodeType enum from parallax_net::NodeType
trait NodeTypeExt {
    fn node_type_enum(&self) -> NodeType;
}

impl NodeTypeExt for parallax_net::node::NodeType {
    fn node_type_enum(&self) -> NodeType {
        *self
    }
} 