use super::*;
use parking_lot::RwLock;
use parallax_net::{node::{Static, Async, Pointer}, Constructor, Duplicator, Switch, Number, port::PortType};
use std::sync::atomic::{AtomicU64, Ordering};

/// Interaction rule for Duplicator ~ Duplicator.
/// Implements Annihilation (HVM ANNI rule).
/// The auxiliary ports of the two duplicators are connected pairwise.
///
/// ```text
/// Before:
///   d1_l -l\     /l- d2_l
///          p~[D]~p
///   d1_r -r/     \r- d2_r
///
/// After:
///   d1_l ----- d2_l
///   d1_r ----- d2_r
/// (Original [D] nodes removed)
/// ```
#[inline]
pub unsafe fn duplicator_duplicator(d1: Port, d2: Port, read_guard: &parking_lot::RwLockReadGuard<AllPartitions>) { 
    log::trace!("Rule: D-D Annihilation");
    let (d1_l, d1_r) = get_aux_ports(d1, read_guard);
    let (d2_l, d2_r) = get_aux_ports(d2, read_guard);

    remove_node(d1, read_guard);
    remove_node(d2, read_guard);

    connect(d1_l, d2_l, read_guard);
    connect(d1_r, d2_r, read_guard);

    if d1_l.port_type() == PortType::Principal && d2_l.port_type() == PortType::Principal {
         add_redex_to_partition(d1_l.partition_id(), Redex(d1_l, d2_l), read_guard);
    }
    if d1_r.port_type() == PortType::Principal && d2_r.port_type() == PortType::Principal {
         add_redex_to_partition(d1_r.partition_id(), Redex(d1_r, d2_r), read_guard);
    }
}

/// Interaction rule for Duplicator ~ Number.
/// Implements standard duplication for interaction nets.
/// The number node is cloned, and each copy is connected to one auxiliary
/// port of the original duplicator.
///
/// ```text
/// Before:
///         d_l -l\
///   ... -p[N]~p[D]-r - d_r
///
/// After:
///         d_l --p[N']
///                
///         d_r --p[N'']
/// (Original [D] and [N] nodes removed)
/// ```
#[inline]
pub unsafe fn duplicator_number(d_port: Port, n_port: Port, read_guard: &parking_lot::RwLockReadGuard<AllPartitions>) { 
    log::trace!("Rule: D-N Duplication");
    
    // Get aux ports of the duplicator
    let (d_l, d_r) = get_aux_ports(d_port, read_guard);
    
    // Get the partition and data of the number node
    let n_p_id = n_port.partition_id();
    let n_idx = n_port.node_index() as usize;
    let n_partition_ptr = get_partition_ptr_mut(read_guard, n_p_id);
    let number_value = (*n_partition_ptr).numbers.get(n_idx)
        .map(|n_node| n_node.data)
        .expect("Original Number node not found");

    // Determine target partitions for new number nodes based on duplicator aux ports
    let p_id_dl = d_l.partition_id();
    let p_id_dr = d_r.partition_id();
    
    // Get mutable pointers to target partitions
    let partition_dl_ptr = get_partition_ptr_mut(read_guard, p_id_dl);
    let partition_dr_ptr = get_partition_ptr_mut(read_guard, p_id_dr);
    
    // Allocate two new Number nodes, copying the data
    let n1_idx = (*partition_dl_ptr).alloc_number(Number { 
        principle: Port::NULL, 
        data: number_value 
    });
    let n2_idx = (*partition_dr_ptr).alloc_number(Number { 
        principle: Port::NULL, 
        data: number_value 
    });
    
    // Create ports for the new number nodes
    let n1_p = Port::principal(NodeType::Number, p_id_dl, n1_idx as u64);
    let n2_p = Port::principal(NodeType::Number, p_id_dr, n2_idx as u64);
    
    // Update the principal ports within the newly allocated number nodes
    (*partition_dl_ptr).numbers.get_mut(n1_idx).map(|n| { n.principle = n1_p; });
    (*partition_dr_ptr).numbers.get_mut(n2_idx).map(|n| { n.principle = n2_p; });

    // Connect duplicator aux ports to new number principal ports
    connect(d_l, n1_p, read_guard);
    connect(d_r, n2_p, read_guard);

    // Remove original interacting nodes
    remove_node(d_port, read_guard);
    remove_node(n_port, read_guard);

    // Add new redexes if duplicator aux ports were principal
    if d_l.port_type() == PortType::Principal {
        add_redex_to_partition(p_id_dl, Redex(d_l, n1_p), read_guard);
    }
    if d_r.port_type() == PortType::Principal {
        add_redex_to_partition(p_id_dr, Redex(d_r, n2_p), read_guard);
    }
}

/// Interaction rule for Duplicator ~ Switch.
/// Implements Commutation (HVM COMM rule).
/// Allocates 4 new nodes (2 Duplicators, 2 Switches) and connects them.
///
/// ```text
/// Before:
///   d_l -l\         /l- sw_l
///          p~[D]~p[Sw]-r - sw_r
///   d_r -r/         
///
/// After:                     
///        d_l --p[Sw']l--l[D']p-- sw_l
///                 r      r/   
///                 |      /     
///                 |     /      
///                 |    /       
///                 r   l        
///        d_r --p[Sw'']l--l[D'']p-- sw_r
///                 r------r      
/// (Original [D] and [Sw] nodes removed)
/// ```
pub unsafe fn duplicator_switch(d_port: Port, sw_port: Port, read_guard: &parking_lot::RwLockReadGuard<AllPartitions>) {
    log::trace!("Rule: D-SW Commutation");
    
    // Get aux ports
    let (sw_l, sw_r) = get_aux_ports(sw_port, read_guard);
    let (d_l, d_r) = get_aux_ports(d_port, read_guard);

    // Determine target partitions for new nodes based on switch aux ports
    let p_id_sw_l = sw_l.partition_id();
    let p_id_sw_r = sw_r.partition_id();
    let p_id_dl = d_l.partition_id(); // Needed for allocation
    let p_id_dr = d_r.partition_id(); // Needed for allocation
    
    // Get mutable pointers to target partitions
    let partition_swl_ptr = get_partition_ptr_mut(read_guard, p_id_sw_l);
    let partition_swr_ptr = get_partition_ptr_mut(read_guard, p_id_sw_r);
    let partition_dl_ptr = get_partition_ptr_mut(read_guard, p_id_dl);
    let partition_dr_ptr = get_partition_ptr_mut(read_guard, p_id_dr);
    
    // Allocate 4 new nodes: 2 Duplicators (d1, d2), 2 Switches (sw1, sw2)
    // Target partition based on where the *original* aux port came from
    let d1_idx = (*partition_swl_ptr).alloc_duplicator(Duplicator::new_null());
    let d2_idx = (*partition_swr_ptr).alloc_duplicator(Duplicator::new_null());
    let sw1_idx = (*partition_dl_ptr).alloc_switch(Switch::new_null());
    let sw2_idx = (*partition_dr_ptr).alloc_switch(Switch::new_null());
    
    // Create ports for the new nodes
    let d1_p = Port::principal(NodeType::Duplicator, p_id_sw_l, d1_idx as u64);
    let d1_l = Port::left(NodeType::Duplicator, p_id_sw_l, d1_idx as u64);
    let d1_r = Port::right(NodeType::Duplicator, p_id_sw_l, d1_idx as u64);
    let d2_p = Port::principal(NodeType::Duplicator, p_id_sw_r, d2_idx as u64);
    let d2_l = Port::left(NodeType::Duplicator, p_id_sw_r, d2_idx as u64);
    let d2_r = Port::right(NodeType::Duplicator, p_id_sw_r, d2_idx as u64);
    
    let sw1_p = Port::principal(NodeType::Switch, p_id_dl, sw1_idx as u64);
    let sw1_l = Port::left(NodeType::Switch, p_id_dl, sw1_idx as u64);
    let sw1_r = Port::right(NodeType::Switch, p_id_dl, sw1_idx as u64);
    let sw2_p = Port::principal(NodeType::Switch, p_id_dr, sw2_idx as u64);
    let sw2_l = Port::left(NodeType::Switch, p_id_dr, sw2_idx as u64);
    let sw2_r = Port::right(NodeType::Switch, p_id_dr, sw2_idx as u64);

    // Update the ports within the newly allocated nodes
    (*partition_swl_ptr).duplicators.get_mut(d1_idx).map(|n| { n.principle = d1_p; n.left = d1_l; n.right = d1_r; });
    (*partition_swr_ptr).duplicators.get_mut(d2_idx).map(|n| { n.principle = d2_p; n.left = d2_l; n.right = d2_r; });
    (*partition_dl_ptr).switches.get_mut(sw1_idx).map(|n| { n.principle = sw1_p; n.left = sw1_l; n.right = sw1_r; });
    (*partition_dr_ptr).switches.get_mut(sw2_idx).map(|n| { n.principle = sw2_p; n.left = sw2_l; n.right = sw2_r; });

    // Connect according to HVM COMM rule diagram:
    // Original Aux Ports <-> New Principal Ports
    connect(sw_l, d1_p, read_guard); // sw_l uses d1 (in sw_l's partition)
    connect(sw_r, d2_p, read_guard); // sw_r uses d2 (in sw_r's partition)
    connect(d_l, sw1_p, read_guard);  // d_l uses sw1 (in d_l's partition)
    connect(d_r, sw2_p, read_guard);  // d_r uses sw2 (in d_r's partition)
    // Interconnect New Aux Ports
    connect(d1_l, sw1_l, read_guard);
    connect(d1_r, sw2_l, read_guard);
    connect(d2_l, sw1_r, read_guard);
    connect(d2_r, sw2_r, read_guard);

    // Remove original interacting nodes
    remove_node(sw_port, read_guard);
    remove_node(d_port, read_guard);

    // Add new redexes if original aux ports were principal
    if sw_l.port_type() == PortType::Principal {
        add_redex_to_partition(p_id_sw_l, Redex(d1_p, sw_l), read_guard);
    }
    if sw_r.port_type() == PortType::Principal {
        add_redex_to_partition(p_id_sw_r, Redex(d2_p, sw_r), read_guard);
    }
    if d_l.port_type() == PortType::Principal {
        add_redex_to_partition(p_id_dl, Redex(sw1_p, d_l), read_guard);
    }
    if d_r.port_type() == PortType::Principal {
        add_redex_to_partition(p_id_dr, Redex(sw2_p, d_r), read_guard);
    }
}

/// Interaction rule for Duplicator ~ Constructor.
/// Implements Commutation
/// Allocates 4 new nodes (2 Duplicators, 2 Constructors) and connects them.
///
/// ```text
/// Before:
///   d_l -l\         /l- c_l
///          p~[D]~p[C]-r - c_r
///   d_r -r/         
///
/// After:                     
///        d_l --p[C']l--l[D']p-- c_l
///                 r      r/   
///                 |      /     
///                 |     /      
///                 |    /       
///                 r   l        
///        d_r --p[C'']l--l[D'']p-- c_r
///                 r------r      
/// (Original [D] and [C] nodes removed)
/// ```
#[inline]
pub unsafe fn duplicator_constructor(d_port: Port, c_port: Port, read_guard: &parking_lot::RwLockReadGuard<AllPartitions>) {
    log::trace!("Rule: D-C Commutation");
    
    // Get aux ports
    let (c_l, c_r) = get_aux_ports(c_port, read_guard);
    let (d_l, d_r) = get_aux_ports(d_port, read_guard);

    // Determine target partitions for new nodes based on original aux ports
    let p_id_cl = c_l.partition_id();
    let p_id_cr = c_r.partition_id();
    let p_id_dl = d_l.partition_id();
    let p_id_dr = d_r.partition_id();
    
    // Get mutable pointers to target partitions
    let partition_cl_ptr = get_partition_ptr_mut(read_guard, p_id_cl);
    let partition_cr_ptr = get_partition_ptr_mut(read_guard, p_id_cr);
    let partition_dl_ptr = get_partition_ptr_mut(read_guard, p_id_dl);
    let partition_dr_ptr = get_partition_ptr_mut(read_guard, p_id_dr);
    
    // Allocate 4 new nodes: 2 Duplicators (d1, d2), 2 Constructors (c1, c2)
    let d1_idx = (*partition_cl_ptr).alloc_duplicator(Duplicator::new_null());
    let d2_idx = (*partition_cr_ptr).alloc_duplicator(Duplicator::new_null());
    let c1_idx = (*partition_dl_ptr).alloc_constructor(Constructor::new_null());
    let c2_idx = (*partition_dr_ptr).alloc_constructor(Constructor::new_null());
    
    // Create ports for the new nodes
    let d1_p = Port::principal(NodeType::Duplicator, p_id_cl, d1_idx as u64);
    let d1_l = Port::left(NodeType::Duplicator, p_id_cl, d1_idx as u64);
    let d1_r = Port::right(NodeType::Duplicator, p_id_cl, d1_idx as u64);
    let d2_p = Port::principal(NodeType::Duplicator, p_id_cr, d2_idx as u64);
    let d2_l = Port::left(NodeType::Duplicator, p_id_cr, d2_idx as u64);
    let d2_r = Port::right(NodeType::Duplicator, p_id_cr, d2_idx as u64);
    
    let c1_p = Port::principal(NodeType::Constructor, p_id_dl, c1_idx as u64);
    let c1_l = Port::left(NodeType::Constructor, p_id_dl, c1_idx as u64);
    let c1_r = Port::right(NodeType::Constructor, p_id_dl, c1_idx as u64);
    let c2_p = Port::principal(NodeType::Constructor, p_id_dr, c2_idx as u64);
    let c2_l = Port::left(NodeType::Constructor, p_id_dr, c2_idx as u64);
    let c2_r = Port::right(NodeType::Constructor, p_id_dr, c2_idx as u64);

    // Update the ports within the newly allocated nodes
    (*partition_cl_ptr).duplicators.get_mut(d1_idx).map(|n| { n.principle = d1_p; n.left = d1_l; n.right = d1_r; });
    (*partition_cr_ptr).duplicators.get_mut(d2_idx).map(|n| { n.principle = d2_p; n.left = d2_l; n.right = d2_r; });
    (*partition_dl_ptr).constructors.get_mut(c1_idx).map(|n| { n.principle = c1_p; n.left = c1_l; n.right = c1_r; });
    (*partition_dr_ptr).constructors.get_mut(c2_idx).map(|n| { n.principle = c2_p; n.left = c2_l; n.right = c2_r; });

    // Connect according to HVM COMM rule diagram:
    connect(c_l, d1_p, read_guard);
    connect(c_r, d2_p, read_guard);
    connect(d_l, c1_p, read_guard);
    connect(d_r, c2_p, read_guard);
    connect(d1_l, c1_l, read_guard);
    connect(d1_r, c2_l, read_guard);
    connect(d2_l, c1_r, read_guard);
    connect(d2_r, c2_r, read_guard);

    // Remove original interacting nodes
    remove_node(c_port, read_guard);
    remove_node(d_port, read_guard);

    // Add new redexes if original aux ports were principal
    if c_l.port_type() == PortType::Principal { add_redex_to_partition(p_id_cl, Redex(d1_p, c_l), read_guard); }
    if c_r.port_type() == PortType::Principal { add_redex_to_partition(p_id_cr, Redex(d2_p, c_r), read_guard); }
    if d_l.port_type() == PortType::Principal { add_redex_to_partition(p_id_dl, Redex(c1_p, d_l), read_guard); }
    if d_r.port_type() == PortType::Principal { add_redex_to_partition(p_id_dr, Redex(c2_p, d_r), read_guard); }
}

/// Interaction rule for Duplicator ~ Static (Ref).
/// Implements standard duplication for interaction nets.
/// The Ref node is cloned (value is copied), and each copy is connected
/// to one auxiliary port of the original duplicator.
///
/// ```text
/// Before:
///         d_l -l\
///   ... -p[S]~p[D]-r - d_r
///
/// After:
///         d_l --p[S']
///                
///         d_r --p[S'']
/// (Original [D] and [S] nodes removed)
/// ```
#[inline]
pub unsafe fn duplicator_static(d_port: Port, s_port: Port, read_guard: &parking_lot::RwLockReadGuard<AllPartitions>) { 
    log::trace!("Rule: D-Static Duplication");
    let (d_l, d_r) = get_aux_ports(d_port, read_guard);
    let s_p_id = s_port.partition_id();
    let s_idx = s_port.node_index() as usize;
    let s_partition_ptr = get_partition_ptr_mut(read_guard, s_p_id);
    let static_value = (*s_partition_ptr).statics.get(s_idx)
        .map(|s_node| s_node.data.load(Ordering::Relaxed))
        .expect("Original Static node not found");

    let p_id_dl = d_l.partition_id();
    let p_id_dr = d_r.partition_id();
    let partition_dl_ptr = get_partition_ptr_mut(read_guard, p_id_dl);
    let partition_dr_ptr = get_partition_ptr_mut(read_guard, p_id_dr);
    
    let s1_idx = (*partition_dl_ptr).alloc_static(Static { principle: Port::NULL, data: AtomicU64::new(static_value) });
    let s2_idx = (*partition_dr_ptr).alloc_static(Static { principle: Port::NULL, data: AtomicU64::new(static_value) });
    
    let s1_p = Port::principal(NodeType::Static, p_id_dl, s1_idx as u64);
    let s2_p = Port::principal(NodeType::Static, p_id_dr, s2_idx as u64);
    
    (*partition_dl_ptr).statics.get_mut(s1_idx).map(|n| { n.principle = s1_p; });
    (*partition_dr_ptr).statics.get_mut(s2_idx).map(|n| { n.principle = s2_p; });

    connect(d_l, s1_p, read_guard);
    connect(d_r, s2_p, read_guard);
    remove_node(d_port, read_guard);
    remove_node(s_port, read_guard);

    if d_l.port_type() == PortType::Principal { add_redex_to_partition(p_id_dl, Redex(d_l, s1_p), read_guard); }
    if d_r.port_type() == PortType::Principal { add_redex_to_partition(p_id_dr, Redex(d_r, s2_p), read_guard); }
}

/// Interaction rule for Duplicator ~ Async.
/// Implements standard duplication for interaction nets.
/// The Async node is cloned (id is copied), and each copy is connected
/// to one auxiliary port of the original duplicator.
///
/// ```text
/// Before:
///         d_l -l\
///   ... -p[A]~p[D]-r - d_r
///
/// After:
///         d_l --p[A']
///                
///         d_r --p[A'']
/// (Original [D] and [A] nodes removed)
/// ```
#[inline]
pub unsafe fn duplicator_async(d_port: Port, a_port: Port, read_guard: &parking_lot::RwLockReadGuard<AllPartitions>) { 
    log::trace!("Rule: D-Async Duplication");
    let (d_l, d_r) = get_aux_ports(d_port, read_guard);
    let a_p_id = a_port.partition_id();
    let a_idx = a_port.node_index() as usize;
    let a_partition_ptr = get_partition_ptr_mut(read_guard, a_p_id);
    let async_id = (*a_partition_ptr).asyncs.get(a_idx)
        .map(|a_node| a_node.id)
        .expect("Original Async node not found");

    let p_id_dl = d_l.partition_id();
    let p_id_dr = d_r.partition_id();
    let partition_dl_ptr = get_partition_ptr_mut(read_guard, p_id_dl);
    let partition_dr_ptr = get_partition_ptr_mut(read_guard, p_id_dr);
    
    let a1_idx = (*partition_dl_ptr).alloc_async(Async { principle: Port::NULL, id: async_id });
    let a2_idx = (*partition_dr_ptr).alloc_async(Async { principle: Port::NULL, id: async_id });
    
    let a1_p = Port::principal(NodeType::Async, p_id_dl, a1_idx as u64);
    let a2_p = Port::principal(NodeType::Async, p_id_dr, a2_idx as u64);
    
    (*partition_dl_ptr).asyncs.get_mut(a1_idx).map(|n| { n.principle = a1_p; });
    (*partition_dr_ptr).asyncs.get_mut(a2_idx).map(|n| { n.principle = a2_p; });

    connect(d_l, a1_p, read_guard);
    connect(d_r, a2_p, read_guard);
    remove_node(d_port, read_guard);
    remove_node(a_port, read_guard);

    if d_l.port_type() == PortType::Principal { add_redex_to_partition(p_id_dl, Redex(d_l, a1_p), read_guard); }
    if d_r.port_type() == PortType::Principal { add_redex_to_partition(p_id_dr, Redex(d_r, a2_p), read_guard); }
}

/// Interaction rule for Duplicator ~ Pointer.
/// Implements standard duplication for interaction nets.
/// The Pointer node is cloned (data is cloned), and each copy is connected
/// to one auxiliary port of the original duplicator.
///
/// ```text
/// Before:
///         d_l -l\
///   ... -p[P]~p[D]-r - d_r
///
/// After:
///         d_l --p[P']
///                
///         d_r --p[P'']
/// (Original [D] and [P] nodes removed)
/// ```
#[inline]
pub unsafe fn duplicator_pointer(d_port: Port, p_port: Port, read_guard: &parking_lot::RwLockReadGuard<AllPartitions>) { 
    log::trace!("Rule: D-Pointer Duplication");
    let (d_l, d_r) = get_aux_ports(d_port, read_guard);
    let p_p_id = p_port.partition_id();
    let p_idx = p_port.node_index() as usize;
    let p_partition_ptr = get_partition_ptr_mut(read_guard, p_p_id);
    // Clone the data (currently `()`) - need .clone() for future data types
    let pointer_data = (*p_partition_ptr).pointers.get(p_idx)
        .map(|p_node| p_node.data.clone())
        .expect("Original Pointer node not found");

    let p_id_dl = d_l.partition_id();
    let p_id_dr = d_r.partition_id();
    let partition_dl_ptr = get_partition_ptr_mut(read_guard, p_id_dl);
    let partition_dr_ptr = get_partition_ptr_mut(read_guard, p_id_dr);
    
    let p1_idx = (*partition_dl_ptr).alloc_pointer(Pointer { principle: Port::NULL, data: pointer_data.clone() });
    let p2_idx = (*partition_dr_ptr).alloc_pointer(Pointer { principle: Port::NULL, data: pointer_data });
    
    let p1_p = Port::principal(NodeType::Pointer, p_id_dl, p1_idx as u64);
    let p2_p = Port::principal(NodeType::Pointer, p_id_dr, p2_idx as u64);
    
    (*partition_dl_ptr).pointers.get_mut(p1_idx).map(|n| { n.principle = p1_p; });
    (*partition_dr_ptr).pointers.get_mut(p2_idx).map(|n| { n.principle = p2_p; });

    connect(d_l, p1_p, read_guard);
    connect(d_r, p2_p, read_guard);
    remove_node(d_port, read_guard);
    remove_node(p_port, read_guard);

    if d_l.port_type() == PortType::Principal { add_redex_to_partition(p_id_dl, Redex(d_l, p1_p), read_guard); }
    if d_r.port_type() == PortType::Principal { add_redex_to_partition(p_id_dr, Redex(d_r, p2_p), read_guard); }
}