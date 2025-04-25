use super::*;
use parallax_net::{node::{Switch, Constructor, Number}, port::PortType};

/// Switch ~ Switch (ANNI Rule)
/// ```text
///         Left Ports  Right Ports
///           |            |
///   sw1_l <-+-- SWI -----+-> sw1_r
///               | p1
///            (Interaction)
///               | p2
///   sw2_l <-+-- SWI -----+-> sw2_r
///           |            |
///         Left Ports  Right Ports
///
/// After Reduction:
///
///      sw1_l <---> sw2_l
///      sw1_r <---> sw2_r
/// ```
#[inline]
pub unsafe fn switch_switch(sw1: Port, sw2: Port, read_guard: &parking_lot::RwLockReadGuard<AllPartitions>) {
    log::trace!("Rule: SWI-SWI Annihilation");
    let (sw1_l, sw1_r) = get_aux_ports(sw1, read_guard);
    let (sw2_l, sw2_r) = get_aux_ports(sw2, read_guard);

    remove_node(sw1, read_guard);
    remove_node(sw2, read_guard);

    connect(sw1_l, sw2_l, read_guard);
    connect(sw1_r, sw2_r, read_guard);

    if sw1_l.port_type() == PortType::Principal && sw2_l.port_type() == PortType::Principal {
        add_redex_to_partition(sw1_l.partition_id(), Redex(sw1_l, sw2_l), read_guard);
    }
    if sw1_r.port_type() == PortType::Principal && sw2_r.port_type() == PortType::Principal {
        add_redex_to_partition(sw1_r.partition_id(), Redex(sw1_r, sw2_r), read_guard);
    }
}

/// Switch ~ Constructor (COMM Rule)
/// ```text
///         Left Port  Right Port
///          |           |
///   sw_l <-+-- SWI -----+-> sw_r
///              | p_sw
///           (Interaction)
///              | p_con
///   con_l <-+-- CON -----+-> con_r
///           |           |
///         Left Port  Right Port
///
/// After Reduction:
///
///                   sw_l
///                    |
///   +------------- CON (new_c1) -------+
///   | p_c1         /   \             | p_c1
///   |             v0    v2            |
///   | p_sw1      / \  / \          | p_sw2
///   SWI (new_sw1)   X   SWI (new_sw2) |
///   |    \ /  \ /    |
///   |     v1    v3     |
///   |      \   /      |
///   +------- CON (new_c2) -------+
///           |           |
///         sw_r        con_r
///           ^           ^
///           |___________| (connected via redexes)
///
/// Redexes Added:
///   (new_sw1.left(v0), new_c1.left(v0))
///   (new_sw1.right(v1), new_c2.left(v1))
///   (new_sw2.left(v2), new_c1.right(v2))
///   (new_sw2.right(v3), new_c2.right(v3))
/// ```
#[inline]
pub unsafe fn switch_constructor(sw: Port, con: Port, read_guard: &parking_lot::RwLockReadGuard<AllPartitions>) {
    log::trace!("Rule: SWI-CON Commutation");
    let sw_p_id = sw.partition_id();
    let con_p_id = con.partition_id();

    let (sw_l, sw_r) = get_aux_ports(sw, read_guard);
    let (con_l, con_r) = get_aux_ports(con, read_guard);

    // Assume allocation happens in the first port's partition for simplicity.
    // In a multi-partition setup, this needs careful handling.
    let target_p_id = sw_p_id;
    let partition_ptr = get_partition_ptr_mut(read_guard, target_p_id);
    let partition = &mut *partition_ptr;

    // Allocate new nodes
    let new_sw1_idx = partition.alloc_switch(Switch::new_null());
    let new_sw2_idx = partition.alloc_switch(Switch::new_null());
    let new_c1_idx = partition.alloc_constructor(Constructor::new_null());
    let new_c2_idx = partition.alloc_constructor(Constructor::new_null());

    let new_sw1 = Port::principal(NodeType::Switch, target_p_id, new_sw1_idx as u64);
    let new_sw1_l = Port::left(NodeType::Switch, target_p_id, new_sw1_idx as u64);
    let new_sw1_r = Port::right(NodeType::Switch, target_p_id, new_sw1_idx as u64);
    let new_sw2 = Port::principal(NodeType::Switch, target_p_id, new_sw2_idx as u64);
    let new_sw2_l = Port::left(NodeType::Switch, target_p_id, new_sw2_idx as u64);
    let new_sw2_r = Port::right(NodeType::Switch, target_p_id, new_sw2_idx as u64);
    let new_c1 = Port::principal(NodeType::Constructor, target_p_id, new_c1_idx as u64);
    let new_c1_l = Port::left(NodeType::Constructor, target_p_id, new_c1_idx as u64);
    let new_c1_r = Port::right(NodeType::Constructor, target_p_id, new_c1_idx as u64);
    let new_c2 = Port::principal(NodeType::Constructor, target_p_id, new_c2_idx as u64);
    let new_c2_l = Port::left(NodeType::Constructor, target_p_id, new_c2_idx as u64);
    let new_c2_r = Port::right(NodeType::Constructor, target_p_id, new_c2_idx as u64);

    // Wire the new structure
    connect(new_sw1, con_l, read_guard);
    connect(new_sw2, con_r, read_guard);
    connect(new_c1, sw_l, read_guard);
    connect(new_c2, sw_r, read_guard);

    connect(new_sw1_l, new_c1_l, read_guard);
    connect(new_sw1_r, new_c2_l, read_guard);
    connect(new_sw2_l, new_c1_r, read_guard);
    connect(new_sw2_r, new_c2_r, read_guard);

    // Remove original nodes
    remove_node(sw, read_guard);
    remove_node(con, read_guard);

    // Add new redexes
    add_redex_to_partition(target_p_id, Redex(new_sw1_l, new_c1_l), read_guard);
    add_redex_to_partition(target_p_id, Redex(new_sw1_r, new_c2_l), read_guard);
    add_redex_to_partition(target_p_id, Redex(new_sw2_l, new_c1_r), read_guard);
    add_redex_to_partition(target_p_id, Redex(new_sw2_r, new_c2_r), read_guard);
}

/// Switch ~ Number (Simplified Rule for Boolean-like Input)
/// Treats 0 as false and non-zero as true.
/// Routes the principal port partner to the corresponding aux port and erases the other.
#[inline]
pub unsafe fn switch_number(sw: Port, num: Port, read_guard: &parking_lot::RwLockReadGuard<AllPartitions>) {
    log::trace!("Rule: SWI-NUM Routing (0=False, NonZero=True)");
    
    // Get the port connected to the Switch's principal port (the "caller")
    let caller_port = *get_port_ptr_mut(read_guard, sw).expect("Switch port not found");
    
    // Get Switch aux ports and Number value
    let (sw_l, sw_r) = get_aux_ports(sw, read_guard);
    let num_val = helpers::get_number_data(num, read_guard);

    // Remove original Switch and Number nodes
    remove_node(sw, read_guard);
    remove_node(num, read_guard);

    if num_val == 0 { // False path
        log::trace!("SWI-NUM: Value is 0 (False). Routing caller {:?} to Right Aux {:?}", caller_port, sw_r);
        connect(caller_port, sw_r, read_guard); // Connect caller to Right Aux (False branch)
        alloc_and_connect_eraser(sw_l, read_guard); // Erase Left Aux (True branch)
        // Add redex if necessary
        if caller_port.port_type() == PortType::Principal && sw_r.port_type() == PortType::Principal {
             add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, sw_r), read_guard);
        }
    } else { // True path (non-zero)
        log::trace!("SWI-NUM: Value is non-zero (True). Routing caller {:?} to Left Aux {:?}", caller_port, sw_l);
        connect(caller_port, sw_l, read_guard); // Connect caller to Left Aux (True branch)
        alloc_and_connect_eraser(sw_r, read_guard); // Erase Right Aux (False branch)
        // Add redex if necessary
        if caller_port.port_type() == PortType::Principal && sw_l.port_type() == PortType::Principal {
             add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, sw_l), read_guard);
        }
    }
}