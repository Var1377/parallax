use parallax_mir::{lower_module, MirGraph, MirModule, MirNode, MirType, PortIndex, NodeId, LoweringError};
use parallax_hir::hir::{
    HirModule, HirFunction, HirFunctionSignature, HirExpr, HirExprKind, HirTailExpr,
    HirLiteral, HirType, PrimitiveType as HirPrimitiveType, Operand, HirVar, HirValue, HirStructDef
};
use parallax_resolve::types::{Symbol, PrimitiveType as ResolvePrimitiveType};
use miette::{SourceOffset, SourceSpan};
use std::sync::Arc;
use std::collections::HashMap;

// Use helpers from the common module
mod common;
use common::{dummy_span, create_test_hir_module_single_func, prepare_for_lower_module};

// --- Tests ---

#[test]
fn test_lower_add_intrinsic() -> Result<(), LoweringError> {
    // Define HIR for: fn add_one_one() -> i32 { let tmp = add(1, 1); tmp }
    let func_symbol = Symbol::fresh();
    let add_intrinsic_symbol = Symbol::fresh();
    let add_intrinsic_path = "std::num::__intrinsic_i32_add__".to_string();
    let tmp_var = parallax_hir::hir::HirVar(0); // Variable to hold the result

    let func = HirFunction {
        span: dummy_span(),
        symbol: func_symbol,
        name: "add_one_one".to_string(),
        signature: HirFunctionSignature {
            params: vec![],
            return_type: HirType::Primitive(HirPrimitiveType::I32),
            is_effectful: false,
        },
        body: Some(
            HirExpr {
                span: dummy_span(),
                kind: HirExprKind::Let {
                    var: tmp_var,
                    var_ty: HirType::Primitive(HirPrimitiveType::I32),
                    value: Box::new(HirValue::Call {
                        func: Operand::Global(add_intrinsic_symbol),
                        args: vec![
                            Operand::Const(HirLiteral::IntLiteral { value: 1, ty: HirPrimitiveType::I32 }),
                            Operand::Const(HirLiteral::IntLiteral { value: 1, ty: HirPrimitiveType::I32 }),
                        ],
                    }),
                    rest: Box::new(HirExpr {
                        span: dummy_span(),
                        kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(tmp_var))),
                        ty: HirType::Primitive(HirPrimitiveType::I32),
                    }),
                },
                ty: HirType::Primitive(HirPrimitiveType::I32),
            }
        ),
    };

    let hir_module = create_test_hir_module_single_func(
        func,
        vec![(add_intrinsic_path.clone(), add_intrinsic_symbol)]
    );
    let (descriptor_store_box, adt_map, prim_map, tuple_map, array_map) = prepare_for_lower_module(&hir_module).expect("Layout setup failed");
    let mir_module = lower_module(&hir_module, descriptor_store_box, adt_map, prim_map, tuple_map, array_map)?;

    // --- Assertions ---
    let graph = mir_module.functions.get(&func_symbol).unwrap();

    // 1. Find the FunctionCall node
    let mut call_node_id = None;
    let expected_func_ty = MirType::FunctionPointer(
        vec![
            MirType::Primitive(ResolvePrimitiveType::I32),
            MirType::Primitive(ResolvePrimitiveType::I32),
        ],
        Arc::new(MirType::Primitive(ResolvePrimitiveType::I32)),
    );
    for (id, node) in &graph.nodes {
        if let MirNode::FunctionCall { func_ty } = node {
            if func_ty == &expected_func_ty {
                 call_node_id = Some(*id);
                 break;
            }
        }
    }
    assert!(call_node_id.is_some(), "FunctionCall node not found");
    let call_node_id = call_node_id.unwrap();

    // 2. Find the StaticAddr node for the add intrinsic
    let mut static_addr_node_id = None;
    for (id, node) in &graph.nodes {
        if let MirNode::StaticAddr { symbol, ty } = node {
            if *symbol == add_intrinsic_symbol && ty == &expected_func_ty {
                static_addr_node_id = Some(*id);
                break;
            }
        }
    }
    assert!(static_addr_node_id.is_some(), "StaticAddr node for add intrinsic not found");
    let static_addr_node_id = static_addr_node_id.unwrap();

    // 3. Find the two Constant(1) nodes
    let mut const_node_ids = Vec::new();
    for (id, node) in &graph.nodes {
        if let MirNode::Constant { value, ty } = node {
             if let HirLiteral::IntLiteral { value: val, .. } = value {
                 if *val == 1 && ty == &MirType::Primitive(ResolvePrimitiveType::I32) {
                     const_node_ids.push(*id);
                 }
             }
        }
    }
    assert_eq!(const_node_ids.len(), 2, "Expected two Constant(1) nodes");

    // 4. Check edges connect StaticAddr and Constants to FunctionCall
    let mut found_edge_static = false;
    let mut found_edge_const1 = false;
    let mut found_edge_const2 = false;
    for edge in &graph.edges {
        if edge.to_node == call_node_id {
            match edge.to_port {
                PortIndex(0) if edge.from_node == static_addr_node_id => found_edge_static = true,
                PortIndex(1) if const_node_ids.contains(&edge.from_node) => found_edge_const1 = true,
                PortIndex(2) if const_node_ids.contains(&edge.from_node) => found_edge_const2 = true,
                 _ => {}
            }
        }
    }
    assert!(found_edge_static, "Edge from StaticAddr to FunctionCall:0 not found");
    assert!(found_edge_const1, "Edge from Constant(1) to FunctionCall:1 not found");
    assert!(found_edge_const2, "Edge from Constant(1) to FunctionCall:2 not found");
    // Use find to check uniqueness - find first ID, then check if second edge uses the other ID
    let first_const_id = graph.edges.iter().find(|e| e.to_node == call_node_id && e.to_port == PortIndex(1)).map(|e| e.from_node);
    let second_const_id = graph.edges.iter().find(|e| e.to_node == call_node_id && e.to_port == PortIndex(2)).map(|e| e.from_node);
    assert!(first_const_id.is_some() && second_const_id.is_some());
    assert_ne!(first_const_id.unwrap(), second_const_id.unwrap(), "Constants should connect to different nodes or ports");

    // 5. Check return port points to the FunctionCall output
    let (return_node_id, return_port_index) = graph.return_port.expect("Return port missing");
    assert_eq!(return_node_id, call_node_id, "Return node should be FunctionCall");
    assert_eq!(return_port_index, PortIndex(0));

    // 6. Check total nodes (Param, StaticAddr, Const, Const, FuncCall)
    assert_eq!(graph.nodes.len(), 5, "Expected 5 nodes");
    // 7. Check total edges (StaticAddr->Call, Const->Call, Const->Call)
    assert_eq!(graph.edges.len(), 3, "Expected 3 edges");

    Ok(())
}

#[test]
fn test_lower_lt_intrinsic() -> Result<(), LoweringError> {
    // Define HIR for: fn check_lt() -> bool { let tmp = lt(5, 10); tmp }
    let func_symbol = Symbol::fresh();
    let lt_intrinsic_symbol = Symbol::fresh();
    let lt_intrinsic_path = "std::num::__intrinsic_i32_lt__".to_string(); // Less than for i32
    let tmp_var = parallax_hir::hir::HirVar(0);

    let func = HirFunction {
        span: dummy_span(),
        symbol: func_symbol,
        name: "check_lt".to_string(),
        signature: HirFunctionSignature {
            params: vec![],
            return_type: HirType::Primitive(HirPrimitiveType::Bool),
            is_effectful: false,
        },
        body: Some(
            HirExpr {
                span: dummy_span(),
                kind: HirExprKind::Let {
                    var: tmp_var,
                    var_ty: HirType::Primitive(HirPrimitiveType::Bool),
                    value: Box::new(HirValue::Call {
                        func: Operand::Global(lt_intrinsic_symbol),
                        args: vec![
                            Operand::Const(HirLiteral::IntLiteral { value: 5, ty: HirPrimitiveType::I32 }),
                            Operand::Const(HirLiteral::IntLiteral { value: 10, ty: HirPrimitiveType::I32 }),
                        ],
                    }),
                    rest: Box::new(HirExpr {
                        span: dummy_span(),
                        kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(tmp_var))),
                        ty: HirType::Primitive(HirPrimitiveType::Bool),
                    }),
                },
                ty: HirType::Primitive(HirPrimitiveType::Bool),
            }
        ),
    };

    let hir_module = create_test_hir_module_single_func(
        func,
        vec![(lt_intrinsic_path.clone(), lt_intrinsic_symbol)]
    );
    let (descriptor_store_box, adt_map, prim_map, tuple_map, array_map) = prepare_for_lower_module(&hir_module).expect("Layout setup failed");
    let mir_module = lower_module(&hir_module, descriptor_store_box, adt_map, prim_map, tuple_map, array_map)?;

    // --- Assertions ---
    let graph = mir_module.functions.get(&func_symbol).unwrap();

    // 1. Find the FunctionCall node
    let mut call_node_id_opt = None;
    let expected_func_ty = MirType::FunctionPointer(
        vec![
            MirType::Primitive(ResolvePrimitiveType::I32),
            MirType::Primitive(ResolvePrimitiveType::I32),
        ],
        Arc::new(MirType::Primitive(ResolvePrimitiveType::Bool)), // Returns bool
    );
    for (id, node) in &graph.nodes {
        if let MirNode::FunctionCall { func_ty } = node {
            if func_ty == &expected_func_ty {
                 call_node_id_opt = Some(*id);
                 break;
            }
        }
    }
    let call_node_id = call_node_id_opt.expect("FunctionCall node not found");

    // 2. Find the StaticAddr node for the lt intrinsic
    let mut static_addr_node_id_opt = None;
    for (id, node) in &graph.nodes {
        if let MirNode::StaticAddr { symbol, ty } = node {
            if *symbol == lt_intrinsic_symbol && ty == &expected_func_ty {
                static_addr_node_id_opt = Some(*id);
                break;
            }
        }
    }
    let static_addr_node_id = static_addr_node_id_opt.expect("StaticAddr for lt not found");

    // 3. Find the Constant(5) and Constant(10) nodes
    let mut const_5_id_opt = None;
    let mut const_10_id_opt = None;
    for (id, node) in &graph.nodes {
        if let MirNode::Constant { value: HirLiteral::IntLiteral { value, .. }, ty } = node {
            if ty == &MirType::Primitive(ResolvePrimitiveType::I32) {
                if *value == 5 { const_5_id_opt = Some(*id); }
                if *value == 10 { const_10_id_opt = Some(*id); }
            }
        }
    }
    assert!(const_5_id_opt.is_some(), "Constant(5) not found");
    assert!(const_10_id_opt.is_some(), "Constant(10) not found");

    // 4. Check edges connect StaticAddr and Constants to FunctionCall
    assert!(graph.edges.iter().any(|e| e.from_node == static_addr_node_id && e.to_node == call_node_id && e.to_port == PortIndex(0)), "Edge StaticAddr->Call:0 missing");
    assert!(graph.edges.iter().any(|e| e.from_node == const_5_id_opt.unwrap() && e.to_node == call_node_id && (e.to_port == PortIndex(1) || e.to_port == PortIndex(2))), "Edge Const(5)->Call:1/2 missing");
    assert!(graph.edges.iter().any(|e| e.from_node == const_10_id_opt.unwrap() && e.to_node == call_node_id && (e.to_port == PortIndex(1) || e.to_port == PortIndex(2))), "Edge Const(10)->Call:1/2 missing");

    // 5. Check return port points to the FunctionCall output
    let (return_node_id, return_port_index) = graph.return_port.expect("Return port missing");
    assert_eq!(return_node_id, call_node_id, "Return node should be FunctionCall");
    assert_eq!(return_port_index, PortIndex(0));

    Ok(())
}

// TODO: Add tests for other intrinsics (sub, mul, cmp, etc.), different types. 