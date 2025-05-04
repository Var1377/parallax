use crate::node::{Constructor, Duplicator, Static, Number, Switch, Async, Eraser, Wire, Pointer};
use crate::port::Port;
use slab::Slab;
use std::collections::HashMap;

/// Represents the initial state of an interaction net for a single function.
/// This includes the nodes and the initial redexes that kick off computation.
///
/// Contains the interaction net nodes allocated for the function, stored in type-specific slabs.
/// Also includes the initial redexes needed to start the computation and the root port
/// representing the function's interface.
#[derive(Debug)]
pub struct InitialNetConfig {
    /// Storage for Constructor nodes.
    pub constructors: Slab<Constructor>,
    /// Storage for Duplicator nodes.
    pub duplicators: Slab<Duplicator>,
    /// Storage for Static nodes.
    pub statics: Slab<Static>,
    /// Storage for Number nodes.
    pub numbers: Slab<Number>,
    /// Storage for Switch nodes.
    pub switches: Slab<Switch>,
    /// Storage for Async nodes (if used).
    pub asyncs: Slab<Async>,
    /// Storage for Eraser nodes.
    pub erasers: Slab<Eraser>,
    /// Storage for Pointer nodes.
    pub pointers: Slab<Pointer>,
    /// The initial pairs of ports (wires) that require interaction to begin computation.
    pub initial_wires: Vec<Wire>,
    /// The root port (principal of the RootCON constructor) representing the function's interface.
    /// This port is used by the caller (e.g., an AppCON) to interact with the function.
    pub root: Port,
    // We might need a way to map back from net node index/Port to original MIR node for debugging
    // pub debug_info: HashMap<Port, (NodeId, PortIndex)>,
}

impl Default for InitialNetConfig {
    fn default() -> Self {
        InitialNetConfig {
            constructors: Slab::new(),
            duplicators: Slab::new(),
            statics: Slab::new(),
            numbers: Slab::new(),
            switches: Slab::new(),
            asyncs: Slab::new(),
            erasers: Slab::new(),
            pointers: Slab::new(),
            initial_wires: Vec::new(),
            root: Port::NULL, // Initialize with a dummy value
        }
    }
} 