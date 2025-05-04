// Integration tests specifically for lowering involving MirNode::StaticAddr

use parallax_net::{lower_module, CompiledNet, LoweringError};
use parallax_net::encoding::*;
use parallax_net::node::{NodeType, Wire};
use parallax_net::port::{Port, PortType};
use parallax_mir::mir::{MirEdge, MirGraph, MirModule, MirNode, NodeId, PortIndex, MirType};
use parallax_hir::hir::{PrimitiveType as HirPrimitiveType, HirLiteral};
use parallax_resolve::types::{PrimitiveType as ResolvePrimitiveType, Symbol};
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::atomic::Ordering;

// --- Helpers --- //
fn create_test_graph(symbol: Symbol, nodes: Vec<(NodeId, MirNode)>, edges: Vec<MirEdge>, param_node: Option<NodeId>, return_port: Option<(NodeId, PortIndex)>) -> MirGraph {
    let mut graph = MirGraph::new(symbol);
    let mut id_map = HashMap::new();
    for (temp_id, node) in nodes {
        let actual_id = graph.add_node(node);
        id_map.insert(temp_id, actual_id);
    }
    graph.edges = edges.into_iter().map(|mut edge| {
        edge.from_node = *id_map.get(&edge.from_node).expect("From node ID not found in map");
        edge.to_node = *id_map.get(&edge.to_node).expect("To node ID not found in map");
        edge
    }).collect();
    graph.parameter_node = param_node.map(|id| *id_map.get(&id).expect("Param node ID not found in map"));
    graph.return_port = return_port.map(|(id, port)| (*id_map.get(&id).expect("Return node ID not found in map"), port));
    graph
}

// --- Static Address Test --- //

#[test]
fn test_lower_module_with_static_addr() -> Result<(), LoweringError> {
    // Create a function that returns the address of `target_func`
    let main_func_symbol = Symbol::new(500);
    let target_func_symbol = Symbol::new(501);
    // Define dummy type for the target function pointer
    let target_func_ty = MirType::FunctionPointer(vec![], Arc::new(MirType::Primitive(ResolvePrimitiveType::Unit)));

    // Temp Node IDs
    let param_id = NodeId(0);
    let static_addr_id = NodeId(1);

    let graph = create_test_graph(
        main_func_symbol,
        vec![
            (param_id, MirNode::Parameter { index: 0, ty: MirType::Tuple(vec![]) }),
            (static_addr_id, MirNode::StaticAddr { symbol: target_func_symbol, ty: target_func_ty.clone() }),
        ],
        vec![], // No edges needed
        Some(param_id),
        Some((static_addr_id, PortIndex(0))) // Return the static address
    );

    // Create MirModule (include a dummy graph for target_func, even if empty)
    let mut functions = HashMap::new();
    functions.insert(main_func_symbol, graph);
    functions.insert(target_func_symbol, MirGraph::new(target_func_symbol)); // Add dummy target func

    let mir_module = MirModule {
        name: "static_addr_test_module".to_string(),
        functions,
        structs: vec![],
        enums: vec![],
        statics: vec![],
        entry_point: Some(main_func_symbol),
        intrinsics: vec![],
        descriptor_store: Box::new(parallax_layout::DescriptorStore { descriptors: vec![parallax_layout::LayoutDescriptor::Handle] }),
        adt_index_map: HashMap::new(),
        primitive_index_map: HashMap::new(),
        tuple_index_map: HashMap::new(),
        array_index_map: HashMap::new(),
    };

    // Lower the module
    let compiled_net = lower_module(&mir_module)?;

    // Assertions for main_func
    assert_eq!(compiled_net.networks.len(), 2); // Should lower both functions
    let config = compiled_net.networks.get(&main_func_symbol).expect("Config not found");

    // Expected Nodes: RootCON(1), ParamEraser(1), Static(func addr)(1)
    assert_eq!(config.constructors.len(), 1, "Expected 1 RootCON");
    assert_eq!(config.duplicators.len(), 0, "Expected 0 Param DUP (unused param)");
    assert_eq!(config.erasers.len(), 1, "Expected 1 Param Eraser");
    assert_eq!(config.statics.len(), 1, "Expected 1 Static node for func addr");
    assert_eq!(config.numbers.len(), 0);

    // Check Static node details
    let (static_idx, static_node) = config.statics.iter().next().expect("Static node not found");
    let static_data = static_node.data.load(Ordering::Relaxed);
    let decoded_tag = decode_static_tag(static_data);
    let decoded_payload = decode_static_payload(static_data);
    assert_eq!(decoded_tag, TAG_FUNCTION, "Static node tag should be FUNCTION");
    assert_eq!(decoded_payload, target_func_symbol.id() as u64, "Static node payload should be target func symbol ID");
    let static_port = static_node.principle;

    // Check RootCON details
    let root_con_idx = config.root.node_index() as usize;
    let root_con = &config.constructors[root_con_idx];
    let root_port = config.root;

    let (eraser_idx, _eraser_node) = config.erasers.iter().next().expect("Eraser node not found");
    let eraser_port = Port::principal(NodeType::Eraser, root_port.partition_id(), eraser_idx as u64);

    // Check RootCON connections
    assert_eq!(root_con.left, eraser_port, "Root.left should connect to Param Eraser");
    assert_eq!(root_con.right, static_port, "Root.right should connect to Static node");

    // Check Wires
    assert_eq!(config.initial_wires.len(), 2);
    assert!(config.initial_wires.contains(&Wire(Port::left(NodeType::Constructor, root_port.partition_id(), root_con_idx as u64), eraser_port)), "Missing wire: Root.L -> Eraser.P");
    assert!(config.initial_wires.contains(&Wire(Port::right(NodeType::Constructor, root_port.partition_id(), root_con_idx as u64), static_port)), "Missing wire: Root.R -> Static.P");

    Ok(())
} 