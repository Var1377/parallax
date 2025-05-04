// Integration tests specifically for lowering involving MirNode::Constant

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

// --- Helpers copied from lowering_integration.rs --- // TODO: Maybe factor out to a common test util?

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

// --- Constant Test --- //

#[test]
fn test_lower_module_with_constant() -> Result<(), LoweringError> {
    // Create a function that returns a constant i64
    let func_symbol = Symbol::new(200);
    let i64_ty = MirType::Primitive(ResolvePrimitiveType::I64);
    let param_id = NodeId(0);
    let const_id = NodeId(1);

    let graph = create_test_graph(
        func_symbol,
        vec![
            // Parameter node (aggregate tuple, empty in this case)
            (param_id, MirNode::Parameter { index: 0, ty: MirType::Tuple(vec![]) }),
            // Constant node
            (const_id, MirNode::Constant { value: HirLiteral::IntLiteral { value: 123, ty: HirPrimitiveType::I64 }, ty: i64_ty.clone() }),
        ],
        vec![], // No explicit edges needed between param and const
        Some(param_id),
        Some((const_id, PortIndex(0))) // Return the constant
    );

    // Create MirModule
    let mut functions = HashMap::new();
    functions.insert(func_symbol, graph);

    let mir_module = MirModule {
        name: "constant_test_module".to_string(),
        functions,
        structs: vec![],
        enums: vec![],
        statics: vec![],
        entry_point: Some(func_symbol),
        intrinsics: vec![],
        descriptor_store: Box::new(parallax_layout::DescriptorStore { descriptors: vec![parallax_layout::LayoutDescriptor::Handle] }),
        adt_index_map: HashMap::new(),
        primitive_index_map: HashMap::new(),
        tuple_index_map: HashMap::new(),
        array_index_map: HashMap::new(),
    };

    // Lower the module
    let compiled_net = lower_module(&mir_module)?;

    // Assertions
    assert_eq!(compiled_net.networks.len(), 1, "Expected lowered network for the function");
    let config = compiled_net.networks.get(&func_symbol).expect("Function config not found");

    // Expected Nodes: RootCON(1), ParamEraser(1), Number(1)
    assert_eq!(config.constructors.len(), 1, "Expected 1 RootCON");
    assert_eq!(config.duplicators.len(), 0, "Expected 0 Param DUPs (param unused)");
    assert_eq!(config.erasers.len(), 1, "Expected 1 Param Eraser (for unused param)");
    assert_eq!(config.numbers.len(), 1, "Expected 1 Number node");
    assert_eq!(config.statics.len(), 0, "Expected no Static nodes");

    // Check Number node details
    let number_node = &config.numbers[0]; // Assuming index 0
    assert_eq!(number_node.data, 123u128);
    let number_port = Port::principal(NodeType::Number, 0, 0); // Assuming partition 0, index 0
    assert_eq!(number_node.principle, number_port);

    // Check RootCON details
    let root_con_idx = 0;
    let root_con = &config.constructors[root_con_idx];
    let root_port = Port::principal(NodeType::Constructor, 0, root_con_idx as u64);
    assert_eq!(config.root, root_port);

    // Check Param setup (Eraser directly connected to RootCON.left)
    let eraser_idx = 0;
    let eraser_port = Port::principal(NodeType::Eraser, 0, eraser_idx as u64);

    // Check RootCON connections
    assert_eq!(root_con.left, eraser_port, "Root.left should connect to Param Eraser");
    assert_eq!(root_con.right, number_port, "Root.right should connect to Number node");

    // Check Redexes
    // Root.L -> Eraser
    // Root.R -> Number.P
    assert_eq!(config.initial_wires.len(), 2, "Expected 2 initial wires");
    assert!(config.initial_wires.contains(&Wire(Port::left(NodeType::Constructor, 0, root_con_idx as u64), eraser_port)));
    assert!(config.initial_wires.contains(&Wire(Port::right(NodeType::Constructor, 0, root_con_idx as u64), number_port)));

    Ok(())
} 