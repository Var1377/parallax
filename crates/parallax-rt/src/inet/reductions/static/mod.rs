pub(crate) mod constants;
pub mod helpers;
mod intrinsics;
mod ops;

use crate::inet::*;
use crate::inet::worker::Worker;
use crate::inet::CompiledDefs;
use crate::inet::manager::AllPartitions;
use crate::inet::reductions::get_port_ptr_mut;
use crate::inet::reductions::{remove_node, annihilate_any, get_partition_ptr_mut, get_aux_ports, connect, add_active_pair_to_partition};
use parallax_net::{node::{NodeType, Static}, port::{Port, PortType}, Wire};
use parallax_net::encoding::{decode_static_tag, decode_static_payload};
use parking_lot::RwLockReadGuard;
use std::sync::Arc;

use self::constants::*;
use self::helpers::*;
use self::intrinsics::dispatch_intrinsic_op;
use self::ops::*;


/// Main dispatch function for reducing interactions involving a Static node.
#[inline]
pub(crate) unsafe fn reduce_static(
    wire: Wire,
    worker: &Worker, 
    read_guard: &RwLockReadGuard<AllPartitions>,
    compiled_defs: &Arc<CompiledDefs>
) {
    let port_a = wire.0;
    let port_b = wire.1;

    let (s_port, other_port) = if NodeType::from_u8(port_a.node_type()) == Some(NodeType::Static) {
        (port_a, port_b)
    } else if NodeType::from_u8(port_b.node_type()) == Some(NodeType::Static) {
        (port_b, port_a)
    } else {
        // This shouldn't happen if called correctly, but handle defensively.
        log::error!("reduce_static called with no Static node in redex: {:?}", wire);
        return;
    };

    let s_data = get_static_data(s_port, read_guard);
    let s_tag = decode_static_tag(s_data);
    let s_payload = decode_static_payload(s_data);

    let other_type = NodeType::from_u8(other_port.node_type());

    log::trace!("Rule: Static ~ {:?} (Tag: {}, Payload: {})", other_type, s_tag, s_payload);

    match other_type {
        Some(NodeType::Static) => {
            static_static(s_port, other_port, worker, read_guard, compiled_defs);
        }
        Some(NodeType::Constructor) => {
            // Constructor usually represents App(F, X) or variant data structure
            match s_tag {
                TAG_FUNCTION => {
                    // Static(Function|ID) ~ Constructor(AppHead, Arg)
                    // This handles both function calls
                    static_call_generic(s_port, s_payload, other_port, worker, read_guard, compiled_defs);
                }
                TAG_INTRINSIC_OP => {
                    // Static(Intrinsic|EncodedOp) ~ Constructor(AppHead, Arg)
                    // Pass the full encoded op (s_payload) to dispatch
                    dispatch_intrinsic_op(wire, s_port, s_payload, other_port, worker, read_guard);
                }
                TAG_IS_VARIANT => {
                    // get_port_ptr_mut returns Option, use expect for now in unsafe code
                    let caller_port_ptr = get_port_ptr_mut(read_guard, s_port)
                        .expect(&format!("reduce_static: IsVariant port {:?} not found", s_port));
                    static_is_variant_op(s_port, s_data, other_port, *caller_port_ptr, read_guard);
                }
                TAG_NIL => {
                    // Static(NIL) ~ Constructor -> Annihilate Both
                    log::trace!("Rule: Static(NIL) ~ Constructor: Annihilating both");
                    remove_node(s_port, read_guard);
                    annihilate_any(other_port, read_guard);
                }
                _ => {
                     log::warn!("Unhandled Static(Tag:{}) ~ Constructor interaction. Annihilating.", s_tag);
                     annihilate_any(s_port, read_guard);
                     annihilate_any(other_port, read_guard);
                }
            }
        }
        Some(NodeType::Duplicator) => {
            log::trace!("Rule: Static ~ Duplicator");
            let target_partition_id = s_port.partition_id();
            // Directly get and dereference the pointer, assuming it's never null.
            let target_partition_ptr = get_partition_ptr_mut(read_guard, target_partition_id);
            let target_partition = &mut *target_partition_ptr;
            
            let new_static_idx = target_partition.alloc_static(Static {
                principle: Port::NULL,
                data: s_data.into(),
            });
            let new_static_port = Port::principal(NodeType::Static, target_partition_id, new_static_idx as u64);
            
            // Re-get and dereference pointer after potential reallocation, assuming non-null.
            let tp_ptr_again = get_partition_ptr_mut(read_guard, target_partition_id);
            let part_again = &mut *tp_ptr_again;
            part_again.statics[new_static_idx].principle = new_static_port;

            let (dup_left, dup_right) = get_aux_ports(other_port, read_guard);
            connect(dup_left, s_port, read_guard);
            connect(dup_right, new_static_port, read_guard);
            add_active_pair_to_partition(target_partition_id, Wire(dup_left, s_port), read_guard);
            add_active_pair_to_partition(target_partition_id, Wire(dup_right, new_static_port), read_guard);
            remove_node(other_port, read_guard);
        }
        Some(NodeType::Number) => {
             // Static ~ Number -> Should generally not happen unless it's NIL
             if s_tag == TAG_NIL {
                 log::trace!("Rule: Static(NIL) ~ Number: Annihilating both");
                 remove_node(s_port, read_guard);
                 remove_node(other_port, read_guard); // Remove the Number node
             } else {
                 log::warn!("Unhandled Static(Tag:{}) ~ Number interaction. Annihilating.", s_tag);
                 annihilate_any(s_port, read_guard);
                 annihilate_any(other_port, read_guard);
             }
        }
        Some(NodeType::Eraser) => {
            // Static ~ Eraser -> Annihilate Static
            log::trace!("Rule: Static ~ Eraser");
            remove_node(s_port, read_guard);
            // Eraser self-destructs implicitly or handled by other rules
        }
        Some(NodeType::Switch) => {
            log::warn!("Unhandled Static(Tag:{}) ~ Switch interaction. Annihilating.", s_tag);
            annihilate_any(s_port, read_guard);
            annihilate_any(other_port, read_guard);
        }
        Some(NodeType::Async) => {
            log::warn!("Unhandled Static(Tag:{}) ~ Async interaction. Annihilating.", s_tag);
            annihilate_any(s_port, read_guard);
            annihilate_any(other_port, read_guard);
        }
        Some(NodeType::Pointer) => {
            log::warn!("Unhandled Static(Tag:{}) ~ Pointer interaction. Annihilating.", s_tag);
            annihilate_any(s_port, read_guard);
            annihilate_any(other_port, read_guard);
        }
        None => {
            // Interaction with NULL or invalid node type
             log::warn!("Static node interacting with NULL or invalid port {:?}. Annihilating Static.", other_port);
             annihilate_any(s_port, read_guard);
        }
    }
} 