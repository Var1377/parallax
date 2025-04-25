use parallax_net::{NodeType, Redex, Port, node::*}; // Import Port and node types
use log;
use parking_lot::{RwLock, RwLockReadGuard}; // Use parking_lot RwLock
use super::worker::Worker;
use crate::inet::manager::AllPartitions; // Import the type alias
use crate::inet::CompiledDefs; // Import CompiledDefs
use crate::inet::reductions::*;
use crate::inet::reductions::reduce_static; // Import reduce_static specifically
use std::sync::Arc; // Import Arc

impl Worker {
    /// Performs a single reduction step between two ports. We must own all partitions involved in the reduction.
///
/// # Safety
    /// This function assumes the worker owns both partitions involved in the redex.
    /// The read_guard guarantees access to the AllPartitions structure without structural changes.
    pub unsafe fn reduce(
        &self, 
        redex: Redex, 
        read_guard: &parking_lot::RwLockReadGuard<AllPartitions>,
        compiled_defs: &Arc<CompiledDefs>, // Added compiled_defs parameter
    ) {
    let port_a = redex.0;
    let port_b = redex.1;

    let p_id_a = port_a.partition_id();
    let p_id_b = port_b.partition_id();

        // Assume partitions exist in a single-partition environment
        let type_a = NodeType::from_u8(port_a.node_type())
            .expect("Invalid node type in port A");
        let type_b = NodeType::from_u8(port_b.node_type())
            .expect("Invalid node type in port B");

        log::debug!("Worker {}: Reducing redex {:?} ({:?}) in p{} ~ {:?} ({:?}) in p{}",
                self.id, port_a, type_a, p_id_a, port_b, type_b, p_id_b);

        match (type_a, type_b) {
            // 1. Eraser rules (Eraser is always first arg)
            (NodeType::Eraser, NodeType::Eraser)      => eraser_eraser(port_a, port_b, read_guard),
            (NodeType::Eraser, NodeType::Constructor) => eraser_constructor(port_a, port_b, read_guard),
            (NodeType::Eraser, NodeType::Duplicator)  => eraser_duplicator(port_a, port_b, read_guard),
            (NodeType::Eraser, NodeType::Static)      => eraser_static(port_a, port_b, read_guard),
            (NodeType::Eraser, NodeType::Number)      => eraser_number(port_a, port_b, read_guard),
            (NodeType::Eraser, NodeType::Switch)      => eraser_switch(port_a, port_b, read_guard),
            (NodeType::Eraser, NodeType::Async)       => eraser_async(port_a, port_b, read_guard),
            (NodeType::Eraser, NodeType::Pointer)     => eraser_pointer(port_a, port_b, read_guard),
            // Symmetric cases (call with Eraser first)
            (NodeType::Constructor, NodeType::Eraser) => eraser_constructor(port_b, port_a, read_guard),
            (NodeType::Duplicator,  NodeType::Eraser) => eraser_duplicator(port_b, port_a, read_guard),
            (NodeType::Static,      NodeType::Eraser) => eraser_static(port_b, port_a, read_guard),
            (NodeType::Number,      NodeType::Eraser) => eraser_number(port_b, port_a, read_guard),
            (NodeType::Switch,      NodeType::Eraser) => eraser_switch(port_b, port_a, read_guard),
            (NodeType::Async,       NodeType::Eraser) => eraser_async(port_b, port_a, read_guard),
            (NodeType::Pointer,     NodeType::Eraser) => eraser_pointer(port_b, port_a, read_guard),
            
            // 2. Duplicator rules (excluding Eraser)
            (NodeType::Duplicator, NodeType::Duplicator)  => duplicator_duplicator(port_a, port_b, read_guard), // unimplemented
            (NodeType::Duplicator, NodeType::Constructor) => duplicator_constructor(port_a, port_b, read_guard), // Renamed - now implemented correctly
            (NodeType::Duplicator, NodeType::Static)      => duplicator_static(port_a, port_b, read_guard), // unimplemented
            (NodeType::Duplicator, NodeType::Number)      => duplicator_number(port_a, port_b, read_guard), // unimplemented
            (NodeType::Duplicator, NodeType::Switch)      => duplicator_switch(port_a, port_b, read_guard), // unimplemented
            (NodeType::Duplicator, NodeType::Async)       => duplicator_async(port_a, port_b, read_guard), // unimplemented
            (NodeType::Duplicator, NodeType::Pointer)     => duplicator_pointer(port_a, port_b, read_guard), // unimplemented
            // Symmetric cases (handled by Eraser block or here)
            (NodeType::Constructor, NodeType::Duplicator) => duplicator_constructor(port_b, port_a, read_guard), // Renamed
            (NodeType::Static,      NodeType::Duplicator) => duplicator_static(port_b, port_a, read_guard), // unimplemented
            (NodeType::Number,      NodeType::Duplicator) => duplicator_number(port_b, port_a, read_guard), // unimplemented
            (NodeType::Switch,      NodeType::Duplicator) => duplicator_switch(port_b, port_a, read_guard), // unimplemented
            (NodeType::Async,       NodeType::Duplicator) => duplicator_async(port_b, port_a, read_guard), // unimplemented
            (NodeType::Pointer,     NodeType::Duplicator) => duplicator_pointer(port_b, port_a, read_guard), // unimplemented
            
            // 3. Async rules (excluding Eraser, Duplicator)
            (NodeType::Async, NodeType::Async)       => async_async(port_a, port_b, read_guard), // unimplemented
            (NodeType::Async, NodeType::Constructor) => async_constructor(port_a, port_b, read_guard), // unimplemented
            (NodeType::Async, NodeType::Static)      => async_static(port_a, port_b, read_guard), // unimplemented
            (NodeType::Async, NodeType::Number)      => async_number(port_a, port_b, read_guard), // unimplemented
            (NodeType::Async, NodeType::Switch)      => async_switch(port_a, port_b, read_guard), // unimplemented
            (NodeType::Async, NodeType::Pointer)     => async_pointer(port_a, port_b, read_guard), // unimplemented
            // Symmetric cases (handled by Eraser/Duplicator or here)
            (NodeType::Constructor, NodeType::Async) => async_constructor(port_b, port_a, read_guard), // unimplemented
            (NodeType::Number,      NodeType::Async) => async_number(port_b, port_a, read_guard), // unimplemented
            (NodeType::Switch,      NodeType::Async) => async_switch(port_b, port_a, read_guard), // unimplemented
            (NodeType::Pointer,     NodeType::Async) => async_pointer(port_b, port_a, read_guard), // unimplemented

            // 4. Static rules (call the dispatcher)
            (NodeType::Static, _) => reduce_static(redex, self, read_guard, compiled_defs),
            (_, NodeType::Static) => reduce_static(redex, self, read_guard, compiled_defs),
            
            // 5. Pointer rules (excluding Eraser, Duplicator, Async, Static)
            (NodeType::Pointer, NodeType::Pointer)     => pointer_pointer(port_a, port_b, read_guard), // unimplemented
            (NodeType::Pointer, NodeType::Constructor) => pointer_constructor(port_a, port_b, read_guard), // unimplemented
            (NodeType::Pointer, NodeType::Number)      => pointer_number(port_a, port_b, read_guard), // renamed
            (NodeType::Pointer, NodeType::Switch)      => pointer_switch(port_a, port_b, read_guard), // renamed
            // Symmetric cases (handled by other blocks or here)
            (NodeType::Constructor, NodeType::Pointer) => pointer_constructor(port_b, port_a, read_guard), // unimplemented
            (NodeType::Number,      NodeType::Pointer) => pointer_number(port_b, port_a, read_guard), // renamed
            (NodeType::Switch,      NodeType::Pointer) => pointer_switch(port_b, port_a, read_guard), // renamed
            
            // 6. Switch rules (excluding Eraser, Duplicator, Async, Static, Pointer)
            (NodeType::Switch, NodeType::Switch)      => switch_switch(port_a, port_b, read_guard), // unimplemented
            (NodeType::Switch, NodeType::Constructor) => switch_constructor(port_a, port_b, read_guard), // renamed
            (NodeType::Switch, NodeType::Number)      => switch_number(port_a, port_b, read_guard), // renamed
            // Symmetric cases (handled by other blocks or here)
            (NodeType::Constructor, NodeType::Switch) => switch_constructor(port_b, port_a, read_guard), // renamed
            (NodeType::Number,      NodeType::Switch) => switch_number(port_b, port_a, read_guard), // renamed

            // 7. Constructor rules (remaining)
            (NodeType::Constructor, NodeType::Constructor) => constructor_constructor(port_a, port_b, read_guard), // Implemented (placeholder)
            (NodeType::Constructor, NodeType::Number)      => constructor_number(port_a, port_b, read_guard), // unimplemented
            // Symmetric cases handled by other blocks or here
            (NodeType::Number,     NodeType::Constructor)  => constructor_number(port_b, port_a, read_guard), // unimplemented
            
            // 8. Number rules (remaining)
            (NodeType::Number, NodeType::Number)      => number_number(port_a, port_b, read_guard), // unimplemented
        }
    }
}

