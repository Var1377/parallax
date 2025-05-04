// Integration tests specifically for lowering involving MirNode::Project

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

// --- Projection Test --- //

#[test]
fn test_lower_module_with_projection() -> Result<(), LoweringError> {
    // Create a function: fn proj_first(t: (i64, bool)) -> i64 { t.0 }
    let func_symbol = Symbol::new(400);
    let i64_ty = MirType::Primitive(ResolvePrimitiveType::I64);
    let bool_ty = MirType::Primitive(ResolvePrimitiveType::Bool);
    let tuple_ty = MirType::Tuple(vec![i64_ty.clone(), bool_ty.clone()]);

    // Temp Node IDs
    let param_id = NodeId(0); // Aggregate parameter node for the tuple
    let project_id = NodeId(1); // Project node

    let graph = create_test_graph(
        func_symbol,
        vec![
            // Parameter node takes the aggregate tuple
            (param_id, MirNode::Parameter { index: 0, ty: tuple_ty.clone() }),
            // Project node extracts the first field (index 0)
            (project_id, MirNode::Project {
                field_index: 0,
                aggregate_ty: tuple_ty.clone(),
                field_ty: i64_ty.clone(),
            }),
        ],
        vec![
            // Connect Param output to Project input
            MirEdge { from_node: param_id, from_port: PortIndex(0), to_node: project_id, to_port: PortIndex(0) },
        ],
        Some(param_id),
        Some((project_id, PortIndex(0))) // Return the projected value
    );

    // Create MirModule
    let mut functions = HashMap::new();
    functions.insert(func_symbol, graph);
    let mir_module = MirModule {
        name: "projection_test_module".to_string(),
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

    // Expected Nodes: RootCON(1), ParamDUP(1), ProjGadget(1D+1E), ParamEraser(1)
    assert_eq!(config.constructors.len(), 1, "Expected 1 RootCON");
    assert_eq!(config.duplicators.len(), 2, "Expected Param DUP + Proj DUP");
    assert_eq!(config.erasers.len(), 2, "Expected Proj ERA + Param ERA");
    assert_eq!(config.numbers.len(), 0, "Expected no Number nodes");
    assert_eq!(config.switches.len(), 0, "Expected no Switch nodes");
    assert_eq!(config.statics.len(), 0, "Expected no Static nodes");

    // --- Find nodes robustly --- 
    let root_con = config.constructors.get(0).expect("RootCON node not found");
    let root_port = config.root;
    assert_eq!(root_port.node_type(), NodeType::Constructor as u8);
    let root_con_idx = root_port.node_index() as usize; // Get index from actual root port

    // Find Param DUP from RootCON.left
    let param_dup_port = root_con.left;
    assert_eq!(param_dup_port.node_type(), NodeType::Duplicator as u8, "RootCON.left should connect to Param DUP");
    let param_dup_idx = param_dup_port.node_index() as usize;

    // Find Gadget Output from RootCON.right
    let gadget_output_port = root_con.right;
    assert_eq!(gadget_output_port.node_type(), NodeType::Duplicator as u8, "RootCON.right should connect to Proj DUP");
    assert_eq!(gadget_output_port.port_type(), PortType::Left, "RootCON.right should connect to Proj DUP Aux1");
    let proj_dup_idx = gadget_output_port.node_index() as usize;
    let proj_dup = config.duplicators.get(proj_dup_idx).expect("Proj DUP node not found");
    let gadget_input_port = proj_dup.principle; // Get input port from the found DUP node

    // Find Gadget ERA from Proj DUP aux2 connection
    let proj_dup_aux2 = Port::right(NodeType::Duplicator, 0, proj_dup_idx as u64);
    let proj_era_port = config.initial_wires.iter().find_map(|wire| {
        if wire.0 == proj_dup_aux2 && wire.1.node_type() == NodeType::Eraser as u8 {
            Some(wire.1)
        } else if wire.1 == proj_dup_aux2 && wire.0.node_type() == NodeType::Eraser as u8 {
            Some(wire.0)
        } else {
            None
        }
    }).expect("Proj ERA port not found");
    let proj_era_idx = proj_era_port.node_index() as usize;

    // Find Param ERA from Param DUP aux2 connection
    let param_dup_aux2 = Port::right(NodeType::Duplicator, 0, param_dup_idx as u64);
     let param_era_port = config.initial_wires.iter().find_map(|wire| {
        if wire.0 == param_dup_aux2 && wire.1.node_type() == NodeType::Eraser as u8 {
            Some(wire.1)
        } else if wire.1 == param_dup_aux2 && wire.0.node_type() == NodeType::Eraser as u8 {
            Some(wire.0)
        } else {
            None
        }
    }).expect("Param Eraser port not found");
    let param_era_idx = param_era_port.node_index() as usize;

    // --- Check RootCON connections using found ports --- 
    // Already checked when finding the ports
    // assert_eq!(root_con.left, param_dup_port, "Root.left should connect to Param DUP");
    // assert_eq!(root_con.right, gadget_output_port, "Root.right should connect to Project gadget output");

    // Check Redexes
    // 1. Root.L -> ParamDup.P
    // 2. Root.R -> Gadget.Output (ProjDup.L)
    // 3. ParamDup.Aux1 -> Gadget.Input (ProjDup.P) <-- Lowered MIR Edge (Corrected)
    // 4. Gadget Internal: ProjDup.Aux2 -> ProjEra.P
    // 5. ParamDup.Aux2 -> ParamEra.P (Added for used-once param)
    assert_eq!(config.initial_wires.len(), 5, "Expected 5 wires"); // Updated wire count
    let root_con_left_aux = Port::left(NodeType::Constructor, 0, root_con_idx as u64);
    let root_con_right_aux = Port::right(NodeType::Constructor, 0, root_con_idx as u64);
    let param_dup_aux1 = Port::left(NodeType::Duplicator, 0, param_dup_idx as u64);
    
    assert!(config.initial_wires.contains(&Wire(root_con_left_aux, param_dup_port)), "Missing: Root.L -> ParamDup.P");
    assert!(config.initial_wires.contains(&Wire(root_con_right_aux, gadget_output_port)), "Missing: Root.R -> Gadget.Output");
    // assert!(config.initial_wires.contains(&Wire(param_dup_port, gadget_input_port)), "Missing lowered MIR edge: Param -> Project"); // Incorrect: Param.P -> Gadget.P
    assert!(config.initial_wires.contains(&Wire(param_dup_aux1, gadget_input_port)), "Missing lowered MIR edge: ParamDup.Aux1 -> Gadget.Input"); // Corrected
    assert!(config.initial_wires.contains(&Wire(proj_dup_aux2, proj_era_port)), "Missing internal gadget wiring: ProjDup.Aux2 -> ProjEra.P"); // Corrected var names
    assert!(config.initial_wires.contains(&Wire(param_dup_aux2, param_era_port)), "Missing ParamDup.Aux2 -> ParamEra wire"); // Added check

    Ok(())
} 