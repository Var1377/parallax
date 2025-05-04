use super::constants::*;
use super::*;
use crate::inet::*;
use crate::inet::manager::AllPartitions;
use crate::inet::reductions::{remove_node, get_aux_ports, get_partition_ptr_mut};
use crate::inet::worker::Worker;
use crate::inet::CompiledDefs;
use parallax_net::{node::{Static, Constructor, Duplicator, Number, NodeType}, port::{Port, PortType}, Wire};
use parallax_net::encoding::encode_static_data;
use parking_lot::RwLockReadGuard;
use std::sync::atomic::{AtomicU64, Ordering};


// Helper to allocate a Static node representing True or False
#[inline]
pub(crate) unsafe fn alloc_static_bool(value: bool, target_partition_id: u16, read_guard: &RwLockReadGuard<AllPartitions>) -> Port {
    // TODO: Could pre-allocate global TRUE/FALSE static nodes.
    let bool_data = encode_static_data(TAG_NIL, value as u64);
    let target_partition = &mut *get_partition_ptr_mut(read_guard, target_partition_id);
    let bool_idx = target_partition.alloc_static(Static {
        principle: Port::NULL, 
        data: AtomicU64::new(bool_data)
    });
    let bool_port = Port::principal(NodeType::Static, target_partition_id, bool_idx as u64);
    target_partition.statics[bool_idx].principle = bool_port;
    bool_port
}

// Helper to get data from a static node port
#[inline]
pub(crate) unsafe fn get_static_data(port: Port, read_guard: &RwLockReadGuard<AllPartitions>) -> u64 {
     let p_id = port.partition_id();
     let idx = port.node_index() as usize;
     let partition_ptr = get_partition_ptr_mut(read_guard, p_id);
     (*partition_ptr).statics.get(idx)
        .map(|node| node.data.load(Ordering::Relaxed))
        .expect("Static node not found for get_static_data")
}

// Helper to get a specific aux port
#[inline]
pub(super) unsafe fn get_aux_port(port: Port, read_guard: &RwLockReadGuard<AllPartitions>, which: PortType) -> Port {
    debug_assert!(which == PortType::Left || which == PortType::Right, "get_aux_port called with non-aux port type");
    let (left, right) = get_aux_ports(port, read_guard);
    if which == PortType::Left { left } else { right }
}

// Helper to annihilate two static nodes (no aux ports)
#[inline]
pub(super) unsafe fn annihilate_no_aux(p1: Port, p2: Port, read_guard: &RwLockReadGuard<AllPartitions>) {
    remove_node(p1, read_guard); 
    remove_node(p2, read_guard);
}

// Helper to get data from a number node port
#[inline]
pub(crate) unsafe fn get_number_data(port: Port, read_guard: &RwLockReadGuard<AllPartitions>) -> u128 {
    debug_assert!(NodeType::from_u8(port.node_type()) == Some(NodeType::Number));
    let p_id = port.partition_id();
    let node_idx = port.node_index() as usize;
    let partition_ptr = get_partition_ptr_mut(read_guard, p_id);
    let partition = &*partition_ptr; 
    partition.numbers.get(node_idx)
        .map(|node| node.data)
        .expect("get_number_data: Number node not found")
}

// Helper to allocate a Number node
#[inline]
pub(super) unsafe fn alloc_number(
    data: u128,
    partition_id: u16,
    read_guard: &RwLockReadGuard<AllPartitions>,
) -> Port {
    let partition_ptr = get_partition_ptr_mut(read_guard, partition_id);
    let partition = &mut *partition_ptr; 

    let new_node = Number { principle: Port::NULL, data };
    let new_idx = partition.alloc_number(new_node);
    let new_port = Port::principal(NodeType::Number, partition_id, new_idx as u64);
    
    let partition_ptr_again = get_partition_ptr_mut(read_guard, partition_id);
    let partition_again = &mut *partition_ptr_again;
    partition_again.numbers[new_idx].principle = new_port;

    new_port
} 