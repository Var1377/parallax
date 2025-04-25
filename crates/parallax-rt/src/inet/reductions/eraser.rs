use parallax_net::port::PortType;

use super::*;

/// Helper function for Eraser-X interactions where X has auxiliary ports.
/// Implements the optimized erasure logic from HVM's ERAS rule for CON/DUP/SWI.
///
/// The non-eraser node `port_x` is removed. The Eraser `port_e` is kept
/// and connected to the left auxiliary port of the removed node (`port_l`).
/// A *new* Eraser is allocated and connected to the right auxiliary port (`port_r`).
///
/// ```text
/// Before:
///   port_e  port_x
///      [E]~p--p[X]
///             l-...
///             r-...
///
/// After:
///   port_e  port_l
///      [E]-p--? ...
///
///            port_r
///      [E']-p--? ...
/// ```
/// # Safety
/// Caller must ensure `port_e` is an Eraser and `port_x` is a node with auxiliary ports.
/// Relies on the caller providing a valid `read_guard`.
#[inline]
unsafe fn annihilate_aux(port_e: Port, port_x: Port, read_guard: &parking_lot::RwLockReadGuard<AllPartitions>) {
    // port_e is guaranteed to be the Eraser port by the caller (reduce function)
    // port_x is the other node
    log::trace!("Rule: E-{:?} Annihilation (Optimized Aux)", NodeType::from_u8(port_x.node_type()).unwrap());
    
    // Get auxiliary ports of X BEFORE removing X
    let (port_l, port_r) = get_aux_ports(port_x, read_guard);
    
    // Remove only node X
    remove_node(port_x, read_guard);
    
    // Keep the Eraser node E, but reconnect it to port_l
    // (port_e's original connection to port_x is now severed because port_x was removed)
    connect(port_e, port_l, read_guard);

    // Allocate and connect only one new eraser for port_r
    let e_r = alloc_and_connect_eraser(port_r, read_guard);
    
    // Add new redexes ONLY if the auxiliary port connected is a principal port.
    if port_l.port_type() == PortType::Principal {
        add_redex_to_partition(port_l.partition_id(), Redex(port_e, port_l), read_guard);
    }
    if port_r.port_type() == PortType::Principal {
        // Note: e_r is the principal port of the new eraser
        add_redex_to_partition(port_r.partition_id(), Redex(e_r, port_r), read_guard); 
    }
}

/// Helper function for Eraser-X interactions where X has no auxiliary ports.
/// Implements simple annihilation: both nodes are removed.
///
/// ```text
/// Before:
///   port_e  port_x
///      [E]~p--p[X]
///
/// After:
///      (nodes removed)
/// ```
/// # Safety
/// Caller must ensure `port_e` is an Eraser and `port_x` has no auxiliary ports.
/// Relies on the caller providing a valid `read_guard`.
#[inline]
pub unsafe fn annihilate_no_aux(port_e: Port, port_x: Port, read_guard: &parking_lot::RwLockReadGuard<AllPartitions>) {
     // port_e is guaranteed to be the Eraser port by the caller (reduce function)
     // port_x is the other node
     log::trace!("Rule: E-{:?} Annihilation (No Aux)", NodeType::from_u8(port_x.node_type()).unwrap());
    // Remove node X
    remove_node(port_x, read_guard);
    // Remove the Eraser node E
    remove_node(port_e, read_guard);
    // No auxiliary ports, so no new redexes generated
}

/// Interaction rule for Eraser ~ Eraser.
/// Both nodes are simply removed.
#[inline]
pub unsafe fn eraser_eraser(e1: Port, e2: Port, read_guard: &parking_lot::RwLockReadGuard<AllPartitions>) {
    annihilate_aux(e1, e2, read_guard);
}

/// Interaction rule for Eraser ~ Constructor.
/// Uses `annihilate_aux` logic.
pub unsafe fn eraser_constructor(port_e: Port, port_constructor: Port, read_guard: &RwLockReadGuard<AllPartitions>) { 
    // Assume port_e is Eraser, port_constructor is Constructor
    annihilate_aux(port_e, port_constructor, read_guard);
}

/// Interaction rule for Eraser ~ Duplicator.
/// Uses `annihilate_aux` logic.
pub unsafe fn eraser_duplicator(port_e: Port, port_duplicator: Port, read_guard: &RwLockReadGuard<AllPartitions>) { 
    // Assume port_e is Eraser, port_duplicator is Duplicator
    annihilate_aux(port_e, port_duplicator, read_guard);
}

/// Interaction rule for Eraser ~ Static (Ref).
/// Uses `annihilate_no_aux` logic.
pub unsafe fn eraser_static(port_e: Port, port_static: Port, read_guard: &RwLockReadGuard<AllPartitions>) { 
    // Assume port_e is Eraser, port_static is Static
    annihilate_no_aux(port_e, port_static, read_guard);
}

/// Interaction rule for Eraser ~ Number.
/// Uses `annihilate_no_aux` logic.
pub unsafe fn eraser_number(port_e: Port, port_number: Port, read_guard: &RwLockReadGuard<AllPartitions>) { 
    // Assume port_e is Eraser, port_number is Number
    annihilate_no_aux(port_e, port_number, read_guard);
}

/// Interaction rule for Eraser ~ Switch.
/// Uses `annihilate_aux` logic.
pub unsafe fn eraser_switch(port_e: Port, port_switch: Port, read_guard: &RwLockReadGuard<AllPartitions>) { 
    // Assume port_e is Eraser, port_switch is Switch
    annihilate_aux(port_e, port_switch, read_guard);
}

/// Interaction rule for Eraser ~ Async.
/// Uses `annihilate_no_aux` logic.
pub unsafe fn eraser_async(port_e: Port, port_async: Port, read_guard: &RwLockReadGuard<AllPartitions>) { 
    // Assume port_e is Eraser, port_async is Async
    annihilate_no_aux(port_e, port_async, read_guard);
}

/// Interaction rule for Eraser ~ Pointer.
/// Uses `annihilate_no_aux` logic.
pub unsafe fn eraser_pointer(port_e: Port, port_pointer: Port, read_guard: &RwLockReadGuard<AllPartitions>) { 
    // Assume port_e is Eraser, port_pointer is Pointer
    annihilate_no_aux(port_e, port_pointer, read_guard);
}
