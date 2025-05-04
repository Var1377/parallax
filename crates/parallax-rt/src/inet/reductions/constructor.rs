use super::*;

/// Constructor ~ Constructor (ANNI Rule)
/// ```text
///         Left Ports  Right Ports
///           |            |
///   c1_l <-+-- CON -----+-> c1_r
///              | p1
///           (Interaction)
///              | p2
///   c2_l <-+-- CON -----+-> c2_r
///           |            |
///         Left Ports  Right Ports
///
/// After Reduction:
///
///      c1_l <---> c2_l
///      c1_r <---> c2_r
/// ```
#[inline]
pub unsafe fn constructor_constructor(c1: Port, c2: Port, read_guard: &parking_lot::RwLockReadGuard<AllPartitions>) { 
    log::trace!("Rule: CON-CON Annihilation");
    let (c1_l, c1_r) = get_aux_ports(c1, read_guard);
    let (c2_l, c2_r) = get_aux_ports(c2, read_guard);

    remove_node(c1, read_guard);
    remove_node(c2, read_guard);

    connect(c1_l, c2_l, read_guard);
    connect(c1_r, c2_r, read_guard);

    if c1_l.port_type() == PortType::Principal && c2_l.port_type() == PortType::Principal {
         add_active_pair_to_partition(c1_l.partition_id(), Wire(c1_l, c2_l), read_guard);
    }
     if c1_r.port_type() == PortType::Principal && c2_r.port_type() == PortType::Principal {
         add_active_pair_to_partition(c1_r.partition_id(), Wire(c1_r, c2_r), read_guard);
    }

    // Add potential active pairs (previously redexes)
    add_active_pair_to_partition(c1_l.partition_id(), Wire(c1_l, c2_l), read_guard);
    add_active_pair_to_partition(c1_r.partition_id(), Wire(c1_r, c2_r), read_guard);
}

/// Constructor ~ Number (ERAS Rule)
/// ```text
///         Left Port  Right Port
///          |           |
///   con_l <-+-- CON -----+-> con_r
///              | p_con
///           (Interaction)
///              | p_num
///             NUM
///
/// After Reduction:
///
///      con_l <- ERA (new1)
///                  |
///                 NUM
///                  |
///      con_r <- ERA (new2)
/// ```
#[inline]
pub unsafe fn constructor_number(con: Port, num: Port, read_guard: &parking_lot::RwLockReadGuard<AllPartitions>) {
    log::trace!("Rule: CON-NUM Erasure");
    let (con_l, con_r) = get_aux_ports(con, read_guard);

    // Important: Remove the constructor node FIRST to avoid dangling pointers if allocation fails
    remove_node(con, read_guard);

    // Allocate erasers and connect them
    let eraser1 = alloc_and_connect_eraser(con_l, read_guard);
    let eraser2 = alloc_and_connect_eraser(con_r, read_guard);

    // Connect the number to the erasers
    if eraser1 != Port::NULL { connect(num, eraser1, read_guard); }
    if eraser2 != Port::NULL { connect(num, eraser2, read_guard); }

    // Note: We don't add redexes here because ERASER interactions are handled implicitly
    // when the eraser later encounters another principal port.
    // Also, the number node 'num' persists and now points potentially to two erasers.
    // If 'num' was already connected, the original connection is overwritten.
}