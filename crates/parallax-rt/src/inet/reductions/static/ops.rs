use super::constants::*;
use super::helpers::*;
use super::intrinsics::dispatch_intrinsic_op; // Needed for NIL handling
use crate::inet::*;
use crate::inet::manager::AllPartitions;
use crate::inet::worker::Worker;
use crate::inet::CompiledDefs;
use crate::inet::{load_function_into_state, rewrite_port};
use crate::inet::reductions::{remove_node, get_partition_ptr_mut, add_redex_to_partition, connect, alloc_and_connect_eraser, annihilate_any};
use parallax_net::{node::{Static, Constructor, Duplicator, Number, NodeType}, port::{Port, PortType}, Redex};
use parking_lot::RwLockReadGuard;
use std::sync::Arc;


/// Interaction rule for Static ~ Static.
#[inline]
pub(super) unsafe fn static_static(
    s1_port: Port, 
    s2_port: Port, 
    worker: &Worker, 
    read_guard: &RwLockReadGuard<AllPartitions>,
    compiled_defs: &Arc<CompiledDefs>
) {
    let s1_data = get_static_data(s1_port, read_guard);
    let s1_tag = decode_tag(s1_data);
    let s1_payload = decode_data(s1_data);

    let s2_data = get_static_data(s2_port, read_guard);
    let s2_tag = decode_tag(s2_data);
    let s2_payload = decode_data(s2_data);
    
    log::trace!("Rule: Static-Static (Tag1: {}, Tag2: {})", s1_tag, s2_tag);

    match (s1_tag, s2_tag) {
        (TAG_FUNCTION, TAG_FUNCTION) => {
             log::warn!("Static ~ Static interaction between two Function pointers ({}, {}). Annihilating.", s1_payload, s2_payload);
             annihilate_no_aux(s1_port, s2_port, read_guard);
        }
        (TAG_NIL, TAG_NIL) => {
             annihilate_no_aux(s1_port, s2_port, read_guard);
        }
        // Handle NIL ~ X interaction where X is Static
        (TAG_NIL, _) | (_, TAG_NIL) => {
             log::trace!("Rule: Static(NIL) ~ Static: Annihilating both");
             remove_node(s1_port, read_guard);
             remove_node(s2_port, read_guard);
        }
        _ => {
             log::warn!("Unhandled Static ~ Static interaction (Tag1: {}, Tag2: {}). Annihilating.", s1_tag, s2_tag);
             annihilate_no_aux(s1_port, s2_port, read_guard);
        }
    }
}

/// Helper function implementing the function CALL logic for Static ~ X interactions.
#[inline]
pub(super) unsafe fn static_call_generic(
    s_port: Port,            // The Static node representing the function
    func_symbol_id: u64,   // Decoded Function Symbol ID
    caller_port: Port,       // The node calling the function (e.g., AppCON)
    worker: &Worker, 
    read_guard: &RwLockReadGuard<AllPartitions>,
    compiled_defs: &Arc<CompiledDefs>
) {
    let func_id = func_symbol_id as usize;
    log::trace!("Rule: Static-X CALL (FunctionId: {})", func_id);

    let func_net_arc = compiled_defs.get(func_id)
        .expect("FunctionId out of bounds for compiled_defs");
    let func_net = func_net_arc.as_ref();

    let target_partition_id = caller_port.partition_id();
    let target_partition_ptr = get_partition_ptr_mut(read_guard, target_partition_id);
    let target_partition = &mut *target_partition_ptr;

    let node_maps = load_function_into_state(func_net, target_partition);

    for local_redex in &func_net.initial_redexes {
        let port_a = rewrite_port(local_redex.0, &node_maps, target_partition_id);
        let port_b = rewrite_port(local_redex.1, &node_maps, target_partition_id);
        if port_a != Port::NULL && port_b != Port::NULL { 
            add_redex_to_partition(target_partition_id, Redex(port_a, port_b), read_guard);
        }
    }

    let rewritten_root_port = rewrite_port(func_net.root, &node_maps, target_partition_id);

    if rewritten_root_port != Port::NULL {
        connect(caller_port, rewritten_root_port, read_guard);
    } else {
        log::warn!("CALL Rule: Rewritten root port is NULL for func_id {}. Connecting caller ({:?}) to Eraser.", func_id, caller_port);
        alloc_and_connect_eraser(caller_port, read_guard);
    }

    remove_node(s_port, read_guard);
}

/// Interaction rule for Static(IsVariant) ~ Constructor.
/// Produces a Number(0) for false or Number(1) for true.
#[inline]
pub(super) unsafe fn static_is_variant_op(
    s_port: Port,            // The Static(IsVariant | VariantSymID) node
    s_data: u64,           // The raw data from the Static node
    con_port: Port,          // The Constructor node being checked
    caller_port: Port,       // The port waiting for the boolean result (e.g., Switch input)
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let target_variant_sym_id = decode_data(s_data);
    log::trace!(
        "Rule: Static(IsVariant | {}) ~ Constructor({:?}) -> Caller({:?})",
        target_variant_sym_id,
        con_port,
        caller_port
    );

    // Get the tag port from the constructor
    // Assumes Constructor layout: Right Aux = Tag Port
    let tag_port = get_aux_port(con_port, read_guard, PortType::Right);

    let mut matches = false;
    if tag_port.node_type() == NodeType::Static as u8 {
        let tag_static_data = get_static_data(tag_port, read_guard);
        let tag_type = decode_tag(tag_static_data);
        let constructor_tag_id = decode_data(tag_static_data);

        // Assume constructor tags use TAG_FUNCTION, and tuples use TAG_NIL (id 0)
        if (tag_type == TAG_FUNCTION || tag_type == TAG_NIL) && constructor_tag_id == target_variant_sym_id {
            matches = true;
        }
    } else {
        log::warn!(
            "IsVariant check failed: Constructor {:?} has non-Static tag port {:?}",
            con_port,
            tag_port
        );
    }

    // Allocate Number(0) or Number(1)
    let result_val: u128 = if matches { 1 } else { 0 };
    let target_partition_id = caller_port.partition_id(); // Allocate result in caller's partition
    let result_num_port = alloc_number(result_val, target_partition_id, read_guard);

    // Connect the caller to the result
    connect(caller_port, result_num_port, read_guard);
    if caller_port.port_type() == PortType::Principal {
         add_redex_to_partition(target_partition_id, Redex(caller_port, result_num_port), read_guard);
    }

    // Annihilate the Static(IsVariant) node and the Constructor node.
    // The constructor's payload might still be needed if the check is true,
    // but it should be connected to the other branch of the original Switch/If.
    // For simplicity here, we annihilate the whole structure. A more advanced
    // scheme might keep the payload alive via duplication if needed.
    remove_node(s_port, read_guard);
    annihilate_any(con_port, read_guard); // Annihilates Constructor and its connected Tag/Payload
}

// Removed static_project_op (now handled by gadget)
// REMOVED DOWNCAST OP (now handled by Cons/Era gadget) 