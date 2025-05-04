mod eraser;
mod constructor;
mod duplicator;
mod r#static;
mod number;
mod switch;
mod r#async;
mod pointer;

pub use eraser::*;
pub use constructor::*;
pub use duplicator::*;
pub use r#static::*;
pub use number::*;
pub use switch::*;
pub use r#async::*;
pub use pointer::*;

use super::{manager::AllPartitions, Partition, PartitionIdx};
use parallax_net::{node::Eraser, port::PortType, NodeType, Port, Wire};
use parking_lot::RwLockReadGuard;
use log; // Import log


/// Connects two ports p1 and p2.
/// Updates the corresponding Port fields in the nodes they point to using the provided read_guard.
/// # Safety
/// Relies on the caller (Worker) ensuring exclusive logical access via the read_guard.
/// The read_guard allows interior mutability via unsafe code.
#[inline] // Make connect pub(crate)
pub(crate) unsafe fn connect(
    p1: Port,
    p2: Port,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    // Use the provided read_guard directly
    let ptr1 = get_port_ptr_mut(read_guard, p1).expect("connect: Port p1 not found");
    let ptr2 = get_port_ptr_mut(read_guard, p2).expect("connect: Port p2 not found");

    #[cfg(debug_assertions)]
    assert!(ptr1 != ptr2, "connect: Attempted to connect port {:?} to itself!", p1);

    // Dereference raw pointers to write the connection
    *ptr1 = p2;
    *ptr2 = p1;
}

/// Gets a raw pointer to the mutable Partition data for a given ID.
/// Assumes the read lock is held and the ID is valid.
#[inline]
pub(crate) unsafe fn get_partition_ptr_mut<'a>(
    partitions_guard: &'a RwLockReadGuard<'a, AllPartitions>,
    p_id: PartitionIdx
) -> *mut Partition {
    let partition_state = partitions_guard.get(p_id as usize)
        .expect("get_partition_ptr_mut: Invalid partition ID");
    let (partition_cell, _) = partition_state;
    partition_cell.get() // Returns *mut Partition
}

/// Gets a raw pointer to the mutable Port field within a specific partition.
/// Assumes the read lock is held and the port is valid within its partition.
#[inline]
pub(crate) unsafe fn get_port_ptr_mut<'a>(
    partitions_guard: &'a RwLockReadGuard<'a, AllPartitions>,
    port: Port
) -> Option<*mut Port> {
    let p_id = port.partition_id();
    let node_idx = port.node_index() as usize;
    let port_type = port.port_type();
    
    let partition_ptr = get_partition_ptr_mut(partitions_guard, p_id);
    // Dereferencing the raw pointer is unsafe
    let partition = &mut *partition_ptr;

    match NodeType::from_u8(port.node_type()) {
        Some(NodeType::Constructor) => partition.constructors.get_mut(node_idx).map(|node| {
            match port_type {
                PortType::Principal => &mut node.principle as *mut Port,
                PortType::Left => &mut node.left as *mut Port,
                PortType::Right => &mut node.right as *mut Port,
                _ => panic!("Invalid port type for Constructor"),
            }
        }),
        Some(NodeType::Duplicator) => partition.duplicators.get_mut(node_idx).map(|node| {
            match port_type {
                PortType::Principal => &mut node.principle as *mut Port,
                PortType::Left => &mut node.left as *mut Port,
                PortType::Right => &mut node.right as *mut Port,
                _ => panic!("Invalid port type for Duplicator"),
            }
        }),
        Some(NodeType::Static) => partition.statics.get_mut(node_idx).map(|node| {
            if port_type == PortType::Principal { &mut node.principle as *mut Port } else { panic!("Invalid port type for Static") }
        }),
        Some(NodeType::Number) => partition.numbers.get_mut(node_idx).map(|node| {
            if port_type == PortType::Principal { &mut node.principle as *mut Port } else { panic!("Invalid port type for Number") }
        }),
        Some(NodeType::Switch) => partition.switches.get_mut(node_idx).map(|node| {
            match port_type {
                PortType::Principal => &mut node.principle as *mut Port,
                PortType::Left => &mut node.left as *mut Port,
                PortType::Right => &mut node.right as *mut Port,
                _ => panic!("Invalid port type for Switch"),
            }
        }),
        Some(NodeType::Async) => partition.asyncs.get_mut(node_idx).map(|node| {
            if port_type == PortType::Principal { &mut node.principle as *mut Port } else { panic!("Invalid port type for Async") }
        }),
        Some(NodeType::Eraser) => partition.erasers.get_mut(node_idx).map(|node| {
            if port_type == PortType::Principal { &mut node.principle as *mut Port } else { panic!("Invalid port type for Eraser") }
        }),
        Some(NodeType::Pointer) => partition.pointers.get_mut(node_idx).map(|node| {
            if port_type == PortType::Principal { &mut node.principle as *mut Port } else { panic!("Invalid port type for Pointer") }
        }),
        None => {
            log::error!("get_port_ptr_mut: Unknown node type in port {:?}", port);
            None
        }
    }
}

/// Allocates a new Eraser node in the target port's partition and connects its principal port
/// to the target port.
/// # Safety
/// Relies on the caller (Worker) ensuring exclusive logical access via the read_guard.
/// Uses the provided read_guard for all operations.
#[inline]
pub(crate) unsafe fn alloc_and_connect_eraser(
    target_port: Port,
    read_guard: &RwLockReadGuard<AllPartitions>,
) -> Port {
    if target_port == Port::NULL {
        return Port::NULL;
    }
    let partition_id = target_port.partition_id();
    
    // Allocate the eraser using the read_guard
    let new_eraser_port = {
        let partition_ptr = get_partition_ptr_mut(read_guard, partition_id);
        let partition = &mut *partition_ptr; // Unsafe dereference for allocation

        let new_eraser = Eraser { principle: Port::NULL };
        let new_eraser_idx = partition.alloc_eraser(new_eraser);
        Port::principal(NodeType::Eraser, partition_id, new_eraser_idx as u64)
    };
    
    // Connect ports using the same read_guard
    connect(new_eraser_port, target_port, read_guard);
    new_eraser_port
}

/// Adds a potential active pair (a wire connecting two principal ports)
/// to the next queue of the specified partition if both ports are principal.
/// # Safety
/// Assumes the partition_id is valid and the caller has appropriate access rights.
/// Takes a read guard to access partition pointers.
#[inline]
pub(crate) unsafe fn add_active_pair_to_partition(
    partition_id: PartitionIdx,
    wire: Wire,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    // Only add the wire if both ports are principal ports
    if wire.0.port_type() == PortType::Principal && wire.1.port_type() == PortType::Principal {
        if wire.0 == Port::NULL || wire.1 == Port::NULL {
            log::trace!("Skipping NULL wire for active pair: {:?}", wire);
            return; // Don't add wires involving NULL ports
        }
        let part_ptr = get_partition_ptr_mut(read_guard, partition_id);
        (*part_ptr).add_active_pair(wire);
        log::trace!("Added active pair {:?} to partition {}", wire, partition_id);
    } else {
         log::trace!("Skipping non-principal wire {:?} for active pair queue.", wire);
    }
}

/// Retrieves the auxiliary ports (left, right) for a given principal port.
/// Panics if the node type doesn't have auxiliary ports or if the node is not found.
/// # Safety
/// Assumes the port and partition_id are valid.
/// Takes a read guard.
#[inline]
pub(crate) unsafe fn get_aux_ports(port: Port, read_guard: &RwLockReadGuard<AllPartitions>) -> (Port, Port) {
    let p_id = port.partition_id();
    let node_idx = port.node_index() as usize;
    let node_type = NodeType::from_u8(port.node_type())
        .expect("get_aux_ports: Invalid node type");

    let partition_ptr = get_partition_ptr_mut(read_guard, p_id);
    let partition = &*partition_ptr; // Read-only deref is sufficient

    match node_type {
        NodeType::Constructor => {
            let node = partition.constructors.get(node_idx)
                .expect("get_aux_ports: Constructor node not found");
            (node.left, node.right)
        }
        NodeType::Duplicator => {
            let node = partition.duplicators.get(node_idx)
                .expect("get_aux_ports: Duplicator node not found");
            (node.left, node.right)
        }
        NodeType::Switch => {
            let node = partition.switches.get(node_idx)
                .expect("get_aux_ports: Switch node not found");
            (node.left, node.right)
        }
        _ => panic!("get_aux_ports: Node type {:?} does not have auxiliary ports", node_type),
    }
}

/// Removes the node associated with the given principal port from its partition's slab.
/// Panics if the node is not found.
/// # Safety
/// Assumes the port and partition_id are valid.
/// Takes a read guard.
#[inline]
pub(crate) unsafe fn remove_node(port: Port, read_guard: &RwLockReadGuard<AllPartitions>) {
    let p_id = port.partition_id();
    let node_idx = port.node_index() as usize;
    let node_type = NodeType::from_u8(port.node_type()); // Allow None here

    // Check if node type is valid before proceeding
    let node_type = match node_type {
        Some(nt) => nt,
        None => {
            log::error!("remove_node: Invalid node type for port {:?}", port);
            return; // Exit if node type is invalid
        }
    };
    
    log::trace!("Removing node {:?} ({:?}) index {}", port, node_type, node_idx); // Use log trace

    let partition_ptr = get_partition_ptr_mut(read_guard, p_id);
    let partition = &mut *partition_ptr; // Mutable deref needed for remove

    let removed = match node_type {
        NodeType::Constructor => partition.constructors.try_remove(node_idx).is_some(),
        NodeType::Duplicator => partition.duplicators.try_remove(node_idx).is_some(),
        NodeType::Static => partition.statics.try_remove(node_idx).is_some(),
        NodeType::Number => partition.numbers.try_remove(node_idx).is_some(),
        NodeType::Switch => partition.switches.try_remove(node_idx).is_some(),
        NodeType::Async => partition.asyncs.try_remove(node_idx).is_some(),
        NodeType::Eraser => partition.erasers.try_remove(node_idx).is_some(),
        NodeType::Pointer => partition.pointers.try_remove(node_idx).is_some(),
    };

    if !removed {
        // This should ideally not happen if the reduction logic is correct
        log::warn!("remove_node: Node {:?} ({:?}) not found in partition {} or already removed.", port, node_type, p_id);
    }
}

/// Annihilates any node type by connecting its ports to newly allocated erasers.
/// Also removes the node itself.
/// # Safety
/// Assumes the read lock is held and the port is valid.
#[inline]
pub(crate) unsafe fn annihilate_any(port: Port, read_guard: &RwLockReadGuard<AllPartitions>) {
    if port == Port::NULL { return; }
    let node_type = NodeType::from_u8(port.node_type());
    log::trace!("Annihilating node at port {:?} ({:?})", port, node_type);

    match node_type {
        Some(NodeType::Constructor) | Some(NodeType::Duplicator) | Some(NodeType::Switch) => {
            let (left, right) = get_aux_ports(port, read_guard);
            alloc_and_connect_eraser(left, read_guard);
            alloc_and_connect_eraser(right, read_guard);
            // Principal port is connected to something else, let that rule handle it or just remove the node.
            remove_node(port, read_guard);
        }
        Some(NodeType::Static) | Some(NodeType::Number) | Some(NodeType::Async) | Some(NodeType::Eraser) | Some(NodeType::Pointer) => {
             // These only have a principal port connected externally.
             // The reduction rule using annihilate should handle the other side.
             remove_node(port, read_guard);
        }
        None => {
            log::error!("annihilate_any: Unknown node type for port {:?}", port);
        }
    }
}