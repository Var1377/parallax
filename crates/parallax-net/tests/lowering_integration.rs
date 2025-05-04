use parallax_net::{lower_module, CompiledNet, LoweringError};
use parallax_net::encoding::*;
use parallax_net::node::{NodeType}; // Removed unused Redex for now
use parallax_net::port::{Port, PortType}; // Removed unused PortType for now
use parallax_mir::mir::*;
use parallax_hir::hir::{PrimitiveType as HirPrimitiveType, HirLiteral};
use parallax_resolve::types::{PrimitiveType as ResolvePrimitiveType, Symbol};
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::atomic::Ordering;

// --- Helpers copied from src/lowering/tests.rs ---

// Helper function to create a simple MIR graph for testing
// Note: NodeIds here are temporary for test definition; they get remapped.
fn create_test_graph(symbol: Symbol, nodes: Vec<(NodeId, MirNode)>, edges: Vec<MirEdge>, param_node: Option<NodeId>, return_port: Option<(NodeId, PortIndex)>) -> MirGraph {
    let mut graph = MirGraph::new(symbol);
    // Use add_node to manage next_node_id internally
    let mut id_map = HashMap::new(); // Map temporary test IDs to actual assigned IDs
    for (temp_id, node) in nodes {
        let actual_id = graph.add_node(node);
        id_map.insert(temp_id, actual_id);
    }
    // Remap edge IDs
    graph.edges = edges.into_iter().map(|mut edge| {
        edge.from_node = *id_map.get(&edge.from_node).expect("From node ID not found in map");
        edge.to_node = *id_map.get(&edge.to_node).expect("To node ID not found in map");
        edge
    }).collect();
    // Remap param and return IDs
    graph.parameter_node = param_node.map(|id| *id_map.get(&id).expect("Param node ID not found in map"));
    graph.return_port = return_port.map(|(id, port)| (*id_map.get(&id).expect("Return node ID not found in map"), port));
    graph
}

// --- Integration Test ---

#[test]
fn test_lower_module_integration_call() -> Result<(), LoweringError> {
    // Create func_b: fn func_b(x: i64) -> i64 { x } // Identity for simplicity
    let func_b_symbol = Symbol::new(100);
    let i64_ty = MirType::Primitive(ResolvePrimitiveType::I64);
    let param_b_id = NodeId(0);
    let graph_b = create_test_graph(
        func_b_symbol,
        vec![
            // Param node type should be the aggregate tuple type matching the caller's expectation
            (param_b_id, MirNode::Parameter { index: 0, ty: MirType::Tuple(vec![i64_ty.clone()]) })
        ],
        vec![],
        Some(param_b_id),
        Some((param_b_id, PortIndex(0))) // Return param directly
    );

    // Create func_a: fn func_a(y: i64) -> i64 { func_b(y) }
    let func_a_symbol = Symbol::new(101);
    let param_a_id = NodeId(0); // Temp ID for param_a
    let static_addr_a_id = NodeId(1); // Temp ID for static addr of func_b
    let call_a_id = NodeId(2); // Temp ID for call node
    let project_a_id = NodeId(3); // Temp ID for project node (NEW)
    let param_a_ty = MirType::Tuple(vec![i64_ty.clone()]); // Explicit type for clarity
    let func_b_ptr_ty = MirType::FunctionPointer(vec![i64_ty.clone()], Arc::new(i64_ty.clone()));

    let graph_a = create_test_graph(
        func_a_symbol,
        vec![
             // Param node type should be the aggregate tuple type
            (param_a_id, MirNode::Parameter { index: 0, ty: param_a_ty.clone() }),
            // Project node to extract the i64 from the tuple parameter (NEW)
            (project_a_id, MirNode::Project {
                field_index: 0,
                aggregate_ty: param_a_ty.clone(),
                field_ty: i64_ty.clone(),
            }),
            (static_addr_a_id, MirNode::StaticAddr { symbol: func_b_symbol, ty: func_b_ptr_ty.clone() }),
            (call_a_id, MirNode::FunctionCall { func_ty: func_b_ptr_ty.clone() }),
        ],
        vec![
            // Edge: ParamA -> Project Input (UPDATED)
            MirEdge { from_node: param_a_id, from_port: PortIndex(0), to_node: project_a_id, to_port: PortIndex(0) },
            // Edge: Project Output -> Call Arg 1 (UPDATED)
            MirEdge { from_node: project_a_id, from_port: PortIndex(0), to_node: call_a_id, to_port: PortIndex(1) },
            // Edge: StaticAddrB -> Call Func Input 0 (Unchanged)
            MirEdge { from_node: static_addr_a_id, from_port: PortIndex(0), to_node: call_a_id, to_port: PortIndex(0) },
        ],
        Some(param_a_id),
        Some((call_a_id, PortIndex(0))) // Return result of calling func_b
    );

    // Create MirModule
    let mut functions = HashMap::new();
    functions.insert(func_a_symbol, graph_a);
    functions.insert(func_b_symbol, graph_b);

    let mir_module = MirModule {
        name: "integration_test_module".to_string(),
        functions,
        structs: vec![],
        enums: vec![],
        statics: vec![],
        entry_point: Some(func_a_symbol), // Entry point is func_a
        intrinsics: vec![],
        // Provide a default descriptor store and empty maps for this test
        // Box the descriptor store
        descriptor_store: Box::new(parallax_layout::DescriptorStore { descriptors: vec![parallax_layout::LayoutDescriptor::Handle] }),
        // Add empty maps
        adt_index_map: HashMap::new(),
        primitive_index_map: HashMap::new(),
        tuple_index_map: HashMap::new(),
        array_index_map: HashMap::new(),
    };

    // Lower the whole module (pass descriptor_store and adt_index_map)
    let compiled_net = lower_module(&mir_module)?;

    // Assertions
    assert_eq!(compiled_net.networks.len(), 2, "Expected lowered networks for both functions");

    // Check func_a's lowered config
    let config_a = compiled_net.networks.get(&func_a_symbol).expect("func_a config not found");
    // Expected Nodes: RootCON(1), Project(1 DUP, 1 ERA), Static(func_b)(1), AppCON(Call)(1), ParamEra(1)
    // The implementation creates two duplicators for projection handling
    assert_eq!(config_a.constructors.len(), 2, "func_a: Expected RootCON + AppCON");
    assert_eq!(config_a.duplicators.len(), 2, "func_a: Expected 2 duplicators for projection handling");
    assert_eq!(config_a.erasers.len(), 2, "func_a: Expected Proj ERA + Param ERA");
    assert_eq!(config_a.statics.len(), 1, "func_a: Expected Static for func_b addr");
    let static_a_data = config_a.statics[0].data.load(Ordering::Relaxed);
    assert_eq!(decode_static_tag(static_a_data), TAG_FUNCTION);
    assert_eq!(decode_static_payload(static_a_data), func_b_symbol.id() as u64, "func_a: Static Addr does not point to func_b");
    assert!(!config_a.initial_wires.is_empty(), "func_a: Expected initial wires");

    // Check func_b's lowered config (identity function)
    let config_b = compiled_net.networks.get(&func_b_symbol).expect("func_b config not found");
    // Expected Nodes: RootCON(1), Project(1 DUP, 1 ERA)
    // Param DUP/ERA is optimized away because parameter is used exactly once for return.
    assert_eq!(config_b.constructors.len(), 1, "func_b: Expected RootCON");
    assert_eq!(config_b.duplicators.len(), 1, "func_b: Expected 1 Proj DUP");
    assert_eq!(config_b.erasers.len(), 1, "func_b: Expected 1 Proj ERA");
    assert_eq!(config_b.statics.len(), 0, "func_b: Expected no Statics");
    assert!(!config_b.initial_wires.is_empty(), "func_b: Expected initial wires");

    Ok(())
} 