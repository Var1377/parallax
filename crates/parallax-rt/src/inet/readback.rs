use super::{manager::AllPartitions, PartitionIdx};
use super::reductions::{get_partition_ptr_mut, get_port_ptr_mut}; // NOTE: Using mut ptr getter for now, read-only preferred if possible
use parallax_net::{node::*, Port, NodeType, port::PortType};
use parking_lot::{RwLock, RwLockReadGuard};
use std::collections::HashSet;
use std::sync::atomic::Ordering; // Added missing import
use thiserror::Error; // Using thiserror for cleaner error definition

/// Represents the structure read back from the interaction net.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Constructor {
        // We might need a tag/id here later if constructors have different types
        // For now, assume a generic constructor
        left: Box<Term>,
        right: Box<Term>,
    },
    Number(u128),
    Variable(String), // Placeholder for variables/cycles/unreadable ports
    Eraser, // Represents an erased value
    // Add other term types as needed (e.g., Ref(Symbol), Lambda, App)
}

/// Errors that can occur during the readback process.
#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum ReadbackError {
    #[error("Cycle detected during readback at port {0:?}")]
    CycleDetected(Port),
    #[error("Unsupported node type encountered during readback: {0:?}")]
    UnsupportedNodeType(NodeType),
    #[error("Invalid port encountered during readback: {0:?}")]
    InvalidPort(Port),
    #[error("Partition not found for port {0:?}")]
    PartitionNotFound(Port),
    #[error("Node not found within partition for port {0:?}")]
    NodeNotFound(Port),
    #[error("Attempted to read back from a NULL port")]
    NullPort,
}

/// Reads back a term representation from the net starting at the given root port.
/// Acquires a read lock on the partitions.
pub fn readback(
    root_port: Port,
    all_partitions: &RwLock<AllPartitions>,
) -> Result<Term, ReadbackError> {
    if root_port == Port::NULL {
        return Err(ReadbackError::NullPort);
    }
    let read_guard = all_partitions.read();
    let mut visited = HashSet::new();
    // Safety: readback_recursive assumes the read_guard is held and ports are valid
    // within the locked state. It uses unsafe to dereference pointers obtained
    // via get_port_ptr_mut, which is necessary to follow connections.
    unsafe { readback_recursive(root_port, &read_guard, &mut visited) }
}

/// Recursive helper function for readback.
unsafe fn readback_recursive(
    port: Port,
    read_guard: &RwLockReadGuard<AllPartitions>,
    visited: &mut HashSet<Port>,
) -> Result<Term, ReadbackError> {
    if port == Port::NULL {
        // Represent a connection to NULL as an Eraser term? Or maybe a specific Null term?
        // Let's use Eraser for now, as it often signifies termination/deletion.
        return Ok(Term::Eraser);
    }

    // --- Cycle Detection ---
    if !visited.insert(port) {
        // If the port was already in the set, we have a cycle.
        // Represent cycles as a generic Variable for now.
        log::warn!("Cycle detected during readback at port {:?}. Representing as Variable.", port);
        // TODO: Could potentially use a more sophisticated cycle representation
        return Ok(Term::Variable(format!("CYCLE_AT_{:?}", port.as_u64())));
        // Or return Err(ReadbackError::CycleDetected(port));
    }

    // Follow the connection to see where this port actually points
    // Safety: Dereferencing pointers obtained from get_port_ptr_mut is unsafe.
    // Assumes the read_guard is held and the port is valid.
    let target_port_ptr = get_port_ptr_mut(read_guard, port)
        .ok_or_else(|| {
            visited.remove(&port);
            ReadbackError::InvalidPort(port)
        })?;
    let target_port = *target_port_ptr; // Dereference the pointer to get the connected port

    // Check if the *target* port is NULL after following the connection
    if target_port == Port::NULL {
        visited.remove(&port); // Backtrack visited set
        return Ok(Term::Eraser);
    }

    // Now, analyze the *target* port we are connected to
    let target_node_type = NodeType::from_u8(target_port.node_type())
        .ok_or_else(|| {
             visited.remove(&port); 
             ReadbackError::InvalidPort(target_port) // Target port has invalid node type
         })?; 

    // Based on the target node type, decide how to read back
    // Need to store result before removing from visited set
    let result = match target_node_type {
        NodeType::Constructor => {
            // If we land on a principal port of a constructor, read its aux ports
            if target_port.port_type() == PortType::Principal {
                let p_id = target_port.partition_id();
                let node_idx = target_port.node_index() as usize;
                // Safety: Dereferencing partition_ptr is unsafe.
                let partition_ptr = get_partition_ptr_mut(read_guard, p_id);
                let constructor_node = (*partition_ptr).constructors.get(node_idx)
                    .ok_or_else(|| { visited.remove(&port); ReadbackError::NodeNotFound(target_port)})?;
                
                // Clone ports before recursive call to avoid borrow checker issues with visited
                let left_port = constructor_node.left;
                let right_port = constructor_node.right;

                // Recursively read back aux ports
                let left_term = readback_recursive(left_port, read_guard, visited)?;
                let right_term = readback_recursive(right_port, read_guard, visited)?;

                Ok(Term::Constructor {
                    left: Box::new(left_term),
                    right: Box::new(right_term),
                })
            } else {
                // If we land on an aux port, treat it as a variable/boundary
                Ok(Term::Variable(format!("AUX_{:?}", target_port.as_u64())))
            }
        }
        NodeType::Number => {
            // If we land on a principal port of a number, read its value
            if target_port.port_type() == PortType::Principal {
                let p_id = target_port.partition_id();
                let node_idx = target_port.node_index() as usize;
                // Safety: Dereferencing partition_ptr is unsafe.
                let partition_ptr = get_partition_ptr_mut(read_guard, p_id);
                let number_node = (*partition_ptr).numbers.get(node_idx)
                    .ok_or_else(|| { visited.remove(&port); ReadbackError::NodeNotFound(target_port)})?;
                
                Ok(Term::Number(number_node.data))
            } else {
                // Aux ports shouldn't exist for numbers
                Err(ReadbackError::InvalidPort(target_port))
            }
        }
        NodeType::Eraser => {
            // Landing on an Eraser node means the value was erased
            Ok(Term::Eraser)
        }
        // --- Unsupported Node Types ---
        NodeType::Duplicator | NodeType::Switch | NodeType::Async | NodeType::Pointer | NodeType::Static => {
            // Duplicators represent sharing, difficult to read back linearly
            // Switches represent control flow, not simple terms
            // Async/Pointer are underspecified
            // Static needs symbol mapping (TODO)
            log::warn!("Unsupported node type {:?} encountered during readback at port {:?}. Representing as Variable.", target_node_type, target_port);
            Ok(Term::Variable(format!("UNSUPPORTED_{:?}_{:?}", target_node_type, target_port.as_u64())))
            // Or return Err(ReadbackError::UnsupportedNodeType(target_node_type));
        }
    };

    // --- Backtrack visited set before returning ---
    // Ensure removal happens even if an error occurred mid-match
    visited.remove(&port);

    result // Return the computed result
}

// TODO: Implement mapping for Static nodes back to Symbols if needed.
// TODO: Refine representation of cycles, variables, and unsupported nodes.
// TODO: Consider a read-only port getter if `get_port_ptr_mut` is too strong permission-wise.
