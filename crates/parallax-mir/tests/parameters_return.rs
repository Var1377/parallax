use parallax_mir::{lower_module, MirGraph, MirModule, MirNode, MirType, PortIndex, NodeId, LoweringError};
use parallax_hir::hir::{
    HirModule, HirFunction, HirFunctionSignature, HirExpr, HirExprKind, HirTailExpr,
    HirLiteral, HirType, PrimitiveType as HirPrimitiveType, Operand, HirVar, HirStructDef
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
fn test_lower_identity_function() -> Result<(), LoweringError> {
    // Define HIR for: fn identity(a: i32) -> i32 = a;
    let func_symbol = Symbol::fresh();
    let param_a_var = parallax_hir::hir::HirVar(0); // Use ID 0 for HirVar

    let func = HirFunction {
        span: dummy_span(),
        symbol: func_symbol,
        name: "identity".to_string(),
        signature: HirFunctionSignature {
            params: vec![(param_a_var, HirType::Primitive(HirPrimitiveType::I32))],
            return_type: HirType::Primitive(HirPrimitiveType::I32),
            is_effectful: false,
        },
        body: Some(
            HirExpr {
                span: dummy_span(),
                kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(param_a_var))),
                ty: HirType::Primitive(HirPrimitiveType::I32),
            }
        ),
    };

    let hir_module = create_test_hir_module_single_func(func, vec![]);
    let (descriptor_store_box, adt_map, prim_map, tuple_map, array_map) = prepare_for_lower_module(&hir_module).expect("Layout setup failed");
    let mir_module = lower_module(&hir_module, descriptor_store_box, adt_map, prim_map, tuple_map, array_map)?;

    // --- Assertions ---
    let graph = mir_module.functions.get(&func_symbol).unwrap();

    // 1. Check parameter node
    let param_node_id = graph.parameter_node.expect("Parameter node should exist");
    let param_node = graph.nodes.get(&param_node_id).unwrap();
    let expected_param_ty = MirType::Tuple(vec![MirType::Primitive(ResolvePrimitiveType::I32)]);
    match param_node {
        MirNode::Parameter { ty, index } => {
            assert_eq!(*index, 0);
            assert_eq!(ty, &expected_param_ty);
        }
        _ => panic!("Expected parameter node type"),
    }

    // 2. Find the projection node for parameter 'a'
    let mut project_node_id = None;
    for edge in &graph.edges {
        if edge.from_node == param_node_id && edge.from_port == PortIndex(0) {
            let potential_project_node = graph.nodes.get(&edge.to_node).unwrap();
            if let MirNode::Project { field_index, aggregate_ty, field_ty } = potential_project_node {
                if *field_index == 0 && aggregate_ty == &expected_param_ty && field_ty == &MirType::Primitive(ResolvePrimitiveType::I32) {
                    project_node_id = Some(edge.to_node);
                    break;
                }
            }
        }
    }
    assert!(project_node_id.is_some(), "Projection node for parameter 'a' not found");
    let project_node_id = project_node_id.unwrap();

    // 3. Check return port points to the projection node
    let (return_node_id, return_port_index) = graph.return_port.expect("Return port should be set");
    assert_eq!(return_node_id, project_node_id, "Return node should be the projection node");
    assert_eq!(return_port_index, PortIndex(0));

    // 4. Check total number of nodes (Param, Project)
    assert_eq!(graph.nodes.len(), 2, "Expected 2 nodes (Param, Project)");

    // 5. Check number of edges (Param -> Project)
    assert_eq!(graph.edges.len(), 1, "Expected 1 edge (Param -> Project)");

    Ok(())
}

#[test]
fn test_lower_multiple_params() -> Result<(), LoweringError> {
    // Define HIR for: fn second(a: i32, b: bool, c: f64) -> bool = b;
    let func_symbol = Symbol::fresh();
    let param_a_var = parallax_hir::hir::HirVar(0);
    let param_b_var = parallax_hir::hir::HirVar(1);
    let param_c_var = parallax_hir::hir::HirVar(2);

    let i32_ty = HirType::Primitive(HirPrimitiveType::I32);
    let bool_ty = HirType::Primitive(HirPrimitiveType::Bool);
    let f64_ty = HirType::Primitive(HirPrimitiveType::F64);
    let mir_i32_ty = MirType::Primitive(ResolvePrimitiveType::I32);
    let mir_bool_ty = MirType::Primitive(ResolvePrimitiveType::Bool);
    let mir_f64_ty = MirType::Primitive(ResolvePrimitiveType::F64);

    let func = HirFunction {
        span: dummy_span(),
        symbol: func_symbol,
        name: "second".to_string(),
        signature: HirFunctionSignature {
            params: vec![
                (param_a_var, i32_ty.clone()),
                (param_b_var, bool_ty.clone()),
                (param_c_var, f64_ty.clone()),
            ],
            return_type: bool_ty.clone(),
            is_effectful: false,
        },
        body: Some(
            HirExpr {
                span: dummy_span(),
                kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(param_b_var))),
                ty: bool_ty.clone(),
            }
        ),
    };

    let hir_module = create_test_hir_module_single_func(func, vec![]);
    let (descriptor_store_box, adt_map, prim_map, tuple_map, array_map) = prepare_for_lower_module(&hir_module).expect("Layout setup failed");
    let mir_module = lower_module(&hir_module, descriptor_store_box, adt_map, prim_map, tuple_map, array_map)?;

    // --- Assertions ---
    let graph = mir_module.functions.get(&func_symbol).unwrap();

    // 1. Check parameter node
    let param_node_id = graph.parameter_node.expect("Parameter node missing");
    let param_node = graph.nodes.get(&param_node_id).unwrap();
    let expected_param_ty = MirType::Tuple(vec![mir_i32_ty, mir_bool_ty.clone(), mir_f64_ty]);
    match param_node {
        MirNode::Parameter { ty, .. } => assert_eq!(ty, &expected_param_ty),
        _ => panic!("Expected Parameter node"),
    }

    // 2. Find the projection nodes for parameters a, b, c
    let mut proj_a_id_opt = None;
    let mut proj_b_id_opt = None;
    let mut proj_c_id_opt = None;
    for edge in &graph.edges {
        if edge.from_node == param_node_id {
            if let Some(MirNode::Project { field_index, aggregate_ty, field_ty }) = graph.nodes.get(&edge.to_node) {
                if aggregate_ty == &expected_param_ty {
                    match *field_index {
                        0 if field_ty == &MirType::Primitive(ResolvePrimitiveType::I32) => proj_a_id_opt = Some(edge.to_node),
                        1 if field_ty == &MirType::Primitive(ResolvePrimitiveType::Bool) => proj_b_id_opt = Some(edge.to_node),
                        2 if field_ty == &MirType::Primitive(ResolvePrimitiveType::F64) => proj_c_id_opt = Some(edge.to_node),
                        _ => {}
                    }
                }
            }
        }
    }
    assert!(proj_a_id_opt.is_some(), "Projection for 'a' not found");
    assert!(proj_b_id_opt.is_some(), "Projection for 'b' not found");
    assert!(proj_c_id_opt.is_some(), "Projection for 'c' not found");
    let proj_b_id = proj_b_id_opt.unwrap();

    // 3. Check return port points to the projection node for 'b'
    let (return_node_id, return_port_index) = graph.return_port.expect("Return port missing");
    assert_eq!(return_node_id, proj_b_id, "Return should be projection for 'b'");
    assert_eq!(return_port_index, PortIndex(0));

    // 4. Check total number of nodes (Param, ProjA, ProjB, ProjC)
    assert_eq!(graph.nodes.len(), 4, "Expected 4 nodes");

    // 5. Check number of edges (Param -> Proj * 3)
    assert_eq!(graph.edges.len(), 3, "Expected 3 edges");

    Ok(())
} 