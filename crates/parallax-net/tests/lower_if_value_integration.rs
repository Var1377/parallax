// Integration tests specifically for lowering involving MirNode::IfValue

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

// --- IfValue Test --- //

#[test]
fn test_lower_module_with_if_value() -> Result<(), LoweringError> {
    // Create a function: fn select(cond: bool) -> i64 { if cond { 10 } else { 20 } }
    let func_symbol = Symbol::new(600);
    let bool_ty = MirType::Primitive(ResolvePrimitiveType::Bool);
    let i64_ty = MirType::Primitive(ResolvePrimitiveType::I64);

    // Temp Node IDs
    let param_id = NodeId(0); // Aggregate parameter (contains bool)
    let const_true_val_id = NodeId(1);
    let const_false_val_id = NodeId(2);
    let if_node_id = NodeId(3);
    // Note: Need a Project node to extract the bool from the aggregate param
    let project_cond_id = NodeId(4);

    let graph = create_test_graph(
        func_symbol,
        vec![
            (param_id, MirNode::Parameter { index: 0, ty: MirType::Tuple(vec![bool_ty.clone()]) }),
            (project_cond_id, MirNode::Project {
                field_index: 0,
                aggregate_ty: MirType::Tuple(vec![bool_ty.clone()]),
                field_ty: bool_ty.clone(),
            }),
            (const_true_val_id, MirNode::Constant { value: HirLiteral::IntLiteral { value: 10, ty: HirPrimitiveType::I64 }, ty: i64_ty.clone() }),
            (const_false_val_id, MirNode::Constant { value: HirLiteral::IntLiteral { value: 20, ty: HirPrimitiveType::I64 }, ty: i64_ty.clone() }),
            (if_node_id, MirNode::IfValue {
                condition_ty: bool_ty.clone(),
                ty: i64_ty.clone(),
            }),
        ],
        vec![
            // Connect Param -> ProjectCond
            MirEdge { from_node: param_id, from_port: PortIndex(0), to_node: project_cond_id, to_port: PortIndex(0) },
            // Connect ProjectCond output -> IfValue condition input (port 0)
            MirEdge { from_node: project_cond_id, from_port: PortIndex(0), to_node: if_node_id, to_port: PortIndex(0) },
            // Connect constants -> IfValue value inputs (port 1 and 2)
            MirEdge { from_node: const_true_val_id, from_port: PortIndex(0), to_node: if_node_id, to_port: PortIndex(1) },
            MirEdge { from_node: const_false_val_id, from_port: PortIndex(0), to_node: if_node_id, to_port: PortIndex(2) },
        ],
        Some(param_id),
        Some((if_node_id, PortIndex(0))) // Return the result of the IfValue
    );

    // Create MirModule
    let mut functions = HashMap::new();
    functions.insert(func_symbol, graph);
    let mir_module = MirModule {
        name: "if_value_test_module".to_string(),
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

    // Expected Nodes: RootCON(1), ParamDUP(1), ProjCondGadget(1D+1E), Num(10), Num(20), Switch(1), ParamEraser(1)
    assert_eq!(config.constructors.len(), 1, "Expected 1 RootCON");
    assert_eq!(config.duplicators.len(), 2, "Expected Param DUP + Proj DUP");
    assert_eq!(config.erasers.len(), 2, "Expected Proj ERA + Param ERA");
    assert_eq!(config.numbers.len(), 2, "Expected Number nodes for 10 and 20");
    assert_eq!(config.switches.len(), 1, "Expected 1 Switch node");
    assert_eq!(config.statics.len(), 0);

    // --- Find nodes robustly based on connections --- 
    // Find RootCON by checking config.root
    let root_port_from_config = config.root;
    assert_eq!(root_port_from_config.node_type(), NodeType::Constructor as u8);
    let root_con_idx = root_port_from_config.node_index() as usize;
    let root_port = Port::principal(NodeType::Constructor, 0, root_con_idx as u64); // Reinstate calculation
    let root_con = config.constructors.get(root_con_idx).expect("RootCON not found");
    assert_eq!(config.root, root_port, "Config root port mismatch");

    // Find Param DUP (connected to RootCON.left)
    let param_dup_port = root_con.left;
    assert_eq!(param_dup_port.node_type(), NodeType::Duplicator as u8, "RootCON.left should connect to Param DUP");
    let param_dup_idx = param_dup_port.node_index() as usize;

    // Find Switch node (connected to RootCON.right)
    let switch_port = root_con.right;
    assert_eq!(switch_port.node_type(), NodeType::Switch as u8, "RootCON.right should connect to Switch");
    let switch_idx = switch_port.node_index() as usize;

    // Find Number nodes (connected to Switch aux ports)
    let num_10_port = config.initial_wires.iter().find_map(|wire| {
        if wire.1 == Port::left(NodeType::Switch, 0, switch_idx as u64) && wire.0.node_type() == NodeType::Number as u8 {
            Some(wire.0)
        } else if wire.0 == Port::left(NodeType::Switch, 0, switch_idx as u64) && wire.1.node_type() == NodeType::Number as u8 {
            Some(wire.1)
        } else {
            None
        }
    }).expect("Number(10) port not found");
    let num_10_idx = num_10_port.node_index() as usize;
    assert_eq!(config.numbers[num_10_idx].data, 10);

    let num_20_port = config.initial_wires.iter().find_map(|wire| {
        if wire.1 == Port::right(NodeType::Switch, 0, switch_idx as u64) && wire.0.node_type() == NodeType::Number as u8 {
            Some(wire.0)
        } else if wire.0 == Port::right(NodeType::Switch, 0, switch_idx as u64) && wire.1.node_type() == NodeType::Number as u8 {
            Some(wire.1)
        } else {
            None
        }
    }).expect("Number(20) port not found");
    let num_20_idx = num_20_port.node_index() as usize;
    assert_eq!(config.numbers[num_20_idx].data, 20);

    // Find Projection DUP (its output aux1 connects to Switch principal)
    let proj_cond_gadget_output = config.initial_wires.iter().find_map(|wire| {
        if wire.1 == switch_port && wire.0.node_type() == NodeType::Duplicator as u8 && wire.0.port_type() == PortType::Left {
            Some(wire.0)
        } else if wire.0 == switch_port && wire.1.node_type() == NodeType::Duplicator as u8 && wire.1.port_type() == PortType::Left {
            Some(wire.1)
        } else {
            None
        }
    }).expect("Proj DUP output port not found");
    let proj_cond_dup_idx = proj_cond_gadget_output.node_index() as usize;
    let proj_cond_dup = config.duplicators.get(proj_cond_dup_idx).expect("Proj DUP node not found");
    let proj_cond_gadget_input = proj_cond_dup.principle; // Get input port from the found DUP node

    // Find Projection ERA (connected to Proj DUP aux2)
    let proj_cond_dup_aux2 = Port::right(NodeType::Duplicator, 0, proj_cond_dup_idx as u64);
    let proj_cond_era_port = config.initial_wires.iter().find_map(|wire| {
        if wire.0 == proj_cond_dup_aux2 && wire.1.node_type() == NodeType::Eraser as u8 {
            Some(wire.1)
        } else if wire.1 == proj_cond_dup_aux2 && wire.0.node_type() == NodeType::Eraser as u8 {
            Some(wire.0)
        } else {
            None
        }
    }).expect("Proj ERA port not found");
    let proj_cond_era_idx = proj_cond_era_port.node_index() as usize;

    // --- Ports calculated using found indices/connections ---
    let root_port = Port::principal(NodeType::Constructor, 0, 0 as u64);
    let param_dup_port = Port::principal(NodeType::Duplicator, 0, param_dup_idx as u64);
    let proj_cond_gadget_input = Port::principal(NodeType::Duplicator, 0, proj_cond_dup_idx as u64);
    let proj_cond_gadget_output = Port::left(NodeType::Duplicator, 0, proj_cond_dup_idx as u64);
    let num_10_port = Port::principal(NodeType::Number, 0, num_10_idx as u64);
    let num_20_port = Port::principal(NodeType::Number, 0, num_20_idx as u64);
    let switch_port = Port::principal(NodeType::Switch, 0, switch_idx as u64);

    // --- Check RootCON connections using found ports ---
    assert_eq!(root_con.left, param_dup_port); // Input param tuple
    assert_eq!(root_con.right, switch_port); // Result comes from switch interaction

    // Check Redexes
    // 1. Root.L -> ParamDup.P
    // 2. Root.R -> Switch.P
    // 3. ParamDup.Aux1 -> ProjCondGadget.Input (Corrected from ParamDup.P)
    // 4. ProjCondGadget.Output -> Switch.P (MIR Edge 1)
    // 5. Num(10).P -> Switch.L (MIR Edge 2)
    // 6. Num(20).P -> Switch.R (MIR Edge 3)
    // 7. ProjCondGadget Internal Wiring (ProjDup.Aux2 -> ProjEra.P)
    // 8. ParamDup.Aux2 -> ParamEra.P (Added for used-once param)
    assert_eq!(config.initial_wires.len(), 8, "Expected 8 initial wires"); // Updated count

    let root_con_left_aux = Port::left(NodeType::Constructor, 0, 0 as u64);
    let root_con_right_aux = Port::right(NodeType::Constructor, 0, 0 as u64);
    let param_dup_aux1 = Port::left(NodeType::Duplicator, 0, param_dup_idx as u64);
    let switch_aux_l = Port::left(NodeType::Switch, 0, switch_idx as u64);
    let switch_aux_r = Port::right(NodeType::Switch, 0, switch_idx as u64);

    assert!(config.initial_wires.contains(&Wire(root_con_left_aux, param_dup_port)), "Missing: Root.L -> ParamDup.P");
    assert!(config.initial_wires.contains(&Wire(root_con_right_aux, switch_port)), "Missing: Root.R -> Switch.P");
    assert!(config.initial_wires.contains(&Wire(param_dup_aux1, proj_cond_gadget_input)), "Missing: ParamDup.Aux1 -> ProjCondGadget.Input"); // Corrected
    // Check MIR Edges -> Switch Inputs
    assert!(config.initial_wires.contains(&Wire(proj_cond_gadget_output, switch_port)), "Missing: ProjCond -> Switch.P (Cond Input)");
    assert!(config.initial_wires.contains(&Wire(num_10_port, switch_aux_l)), "Missing: Num(10) -> Switch.L (True Val)");
    assert!(config.initial_wires.contains(&Wire(num_20_port, switch_aux_r)), "Missing: Num(20) -> Switch.R (False Val)");
    // Check Proj gadget internal wiring
    assert!(config.initial_wires.contains(&Wire(proj_cond_dup_aux2, proj_cond_era_port)), "Missing ProjCond internal wiring");
    // Check Param DUP unused aux wiring
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
    assert!(config.initial_wires.contains(&Wire(param_dup_aux2, param_era_port)), "Missing ParamDup.Aux2 -> ParamEra wire");

    Ok(())
} 