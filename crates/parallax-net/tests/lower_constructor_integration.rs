// Integration tests specifically for lowering involving MirNode::Constructor

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

// --- Constructor Test --- //

#[test]
fn test_lower_module_with_constructor() -> Result<(), LoweringError> {
    // Create a function that returns a tuple (42i64, true)
    let func_symbol = Symbol::new(300);
    let i64_ty = MirType::Primitive(ResolvePrimitiveType::I64);
    let bool_ty = MirType::Primitive(ResolvePrimitiveType::Bool);
    let tuple_ty = MirType::Tuple(vec![i64_ty.clone(), bool_ty.clone()]);

    // Temp Node IDs
    let param_id = NodeId(0);
    let const_i64_id = NodeId(1);
    let const_bool_id = NodeId(2);
    let constructor_id = NodeId(3);

    let graph = create_test_graph(
        func_symbol,
        vec![
            (param_id, MirNode::Parameter { index: 0, ty: MirType::Tuple(vec![]) }),
            (const_i64_id, MirNode::Constant { value: HirLiteral::IntLiteral { value: 42, ty: HirPrimitiveType::I64 }, ty: i64_ty.clone() }),
            (const_bool_id, MirNode::Constant { value: HirLiteral::BoolLiteral(true), ty: bool_ty.clone() }),
            (constructor_id, MirNode::Constructor {
                tag: Symbol::new(0), // Tuple tag
                field_types: vec![i64_ty.clone(), bool_ty.clone()],
                ty: tuple_ty.clone(),
            }),
        ],
        vec![
            // Connect constants to constructor inputs
            MirEdge { from_node: const_i64_id, from_port: PortIndex(0), to_node: constructor_id, to_port: PortIndex(0) },
            MirEdge { from_node: const_bool_id, from_port: PortIndex(0), to_node: constructor_id, to_port: PortIndex(1) },
        ],
        Some(param_id),
        Some((constructor_id, PortIndex(0))) // Return the constructed tuple
    );

    // Create MirModule
    let mut functions = HashMap::new();
    functions.insert(func_symbol, graph);
    let mir_module = MirModule {
        name: "constructor_test_module".to_string(),
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
    assert_eq!(compiled_net.networks.len(), 1);
    let config = compiled_net.networks.get(&func_symbol).expect("Config not found");

    // Expected Net Structure for Tuple (42, true) -> Cons(42, Cons(true, NIL))
    // Expected Nodes: RootCON(1), ParamEraser(1), Number(42)(1), Number(true)(1), Static(NIL)(1), Constructor(Cons_true)(1), Constructor(Cons_42)(1)
    assert_eq!(config.constructors.len(), 3, "Expected RootCON + 2 Cons nodes");
    assert_eq!(config.duplicators.len(), 0, "Expected 0 Param DUPs (param unused)");
    assert_eq!(config.erasers.len(), 1, "Expected 1 Param Eraser (for unused param)");
    assert_eq!(config.numbers.len(), 2, "Expected 2 Number nodes (42, true)");
    assert_eq!(config.statics.len(), 1, "Expected 1 Static NIL node");

    // Find nodes (indices might vary slightly)
    // Assuming RootCON is index 0 for simplicity now that DUP is gone?
    // Need a more robust way if RootCON isn't guaranteed index 0.
    // Let's find RootCON by checking config.root
    let root_port_from_config = config.root;
    assert_eq!(root_port_from_config.node_type(), NodeType::Constructor as u8);
    let root_con_idx = root_port_from_config.node_index() as usize;

    let num_42_idx = config.numbers.iter().position(|(_, n)| n.data == 42).unwrap_or_else(|| panic!("Number node 42 not found"));
    let num_true_idx = config.numbers.iter().position(|(_, n)| n.data == 1).unwrap_or_else(|| panic!("Number node true(1) not found"));
    let nil_static_idx = config.statics.iter().position(|(_, n)| decode_static_tag(n.data.load(Ordering::Relaxed)) == TAG_NIL).unwrap_or_else(|| panic!("NIL Static node not found"));
    // Cons_true links the 'true' number and NIL
    let num_true_port_principal = Port::principal(NodeType::Number, 0, num_true_idx as u64);
    let nil_port_principal = Port::principal(NodeType::Static, 0, nil_static_idx as u64);
    let cons_true_idx = config.constructors.iter().position(|(idx, n)| {
        idx != root_con_idx &&
        // Check if the node's left input wire originates from num_true
        config.initial_wires.contains(&Wire(num_true_port_principal, Port::left(NodeType::Constructor, 0, idx as u64))) &&
        n.right == nil_port_principal // Check right connects to NIL principal
    }).unwrap_or_else(|| panic!("Cons_true node not found or incorrectly linked"));

    // Cons_42 links the '42' number and Cons_true
    let num_42_port_principal = Port::principal(NodeType::Number, 0, num_42_idx as u64);
    let cons_true_port_principal = Port::principal(NodeType::Constructor, 0, cons_true_idx as u64);
    let cons_42_idx = config.constructors.iter().position(|(idx, n)| {
        idx != root_con_idx &&
        // Check if the node's left input wire originates from num_42
        config.initial_wires.contains(&Wire(num_42_port_principal, Port::left(NodeType::Constructor, 0, idx as u64))) &&
        n.right == cons_true_port_principal // Check right connects to Cons_true principal
    }).unwrap_or_else(|| panic!("Cons_42 node not found or incorrectly linked"));

    // let param_dup_idx = 0; // No DUP
    let eraser_idx = 0; // Assuming the single eraser has index 0

    let root_port = Port::principal(NodeType::Constructor, 0, root_con_idx as u64);
    let cons_42_port = Port::principal(NodeType::Constructor, 0, cons_42_idx as u64);
    let cons_true_port = Port::principal(NodeType::Constructor, 0, cons_true_idx as u64);
    let num_42_port = Port::principal(NodeType::Number, 0, num_42_idx as u64);
    let num_true_port = Port::principal(NodeType::Number, 0, num_true_idx as u64);
    let nil_port = Port::principal(NodeType::Static, 0, nil_static_idx as u64);
    // let param_dup_port = Port::principal(NodeType::Duplicator, 0, param_dup_idx as u64); // No DUP
    let eraser_port = Port::principal(NodeType::Eraser, 0, eraser_idx as u64);

    // Check RootCON connections
    let root_con = &config.constructors[root_con_idx];
    assert_eq!(config.root, root_port);
    assert_eq!(root_con.left, eraser_port); // Corrected expectation: Root.left -> Eraser
    assert_eq!(root_con.right, cons_42_port); // Root connects to the head of the tuple structure

    // Check Wires:
    // Root.L -> Eraser
    // Root.R -> Cons_42.P
    // Cons_42.L <- Num_42.P (Lowered MIR Edge 1)
    // Cons_42.R -> Cons_true.P
    // Cons_true.L <- Num_true.P (Lowered MIR Edge 2)
    // Cons_true.R -> NIL.P
    assert_eq!(config.initial_wires.len(), 6, "Expected 6 wires"); // Wire count remains the same
    assert!(config.initial_wires.contains(&Wire(Port::left(NodeType::Constructor, 0, root_con_idx as u64), eraser_port))); // Corrected wire check
    assert!(config.initial_wires.contains(&Wire(Port::right(NodeType::Constructor, 0, root_con_idx as u64), cons_42_port)));
    // Check lowered edges (source -> dest)
    assert!(config.initial_wires.contains(&Wire(num_42_port, Port::left(NodeType::Constructor, 0, cons_42_idx as u64)))); // Corrected: Num -> Cons.L
    assert!(config.initial_wires.contains(&Wire(num_true_port, Port::left(NodeType::Constructor, 0, cons_true_idx as u64)))); // Corrected: Num -> Cons.L
    // Check internal constructor links
    assert!(config.initial_wires.contains(&Wire(Port::right(NodeType::Constructor, 0, cons_42_idx as u64), cons_true_port)));
    assert!(config.initial_wires.contains(&Wire(Port::right(NodeType::Constructor, 0, cons_true_idx as u64), nil_port)));

    Ok(())
} 