use parallax_mir::{lower_module, MirGraph, MirModule, MirNode, MirType, PortIndex, NodeId, LoweringError};
use parallax_hir::hir::{
    HirModule, HirFunction, HirFunctionSignature, HirExpr, HirExprKind, HirTailExpr,
    HirLiteral, HirType, PrimitiveType as HirPrimitiveType, Operand, HirVar, HirValue, HirStructDef
};
use parallax_resolve::types::{Symbol, PrimitiveType as ResolvePrimitiveType};
use miette::{SourceOffset, SourceSpan};
use std::sync::Arc;
use std::collections::HashMap;

// Import helper from the module level
mod common;
use common::{dummy_span, create_test_hir_module_single_func, prepare_for_lower_module};

// --- Test Helpers (Copied) ---

fn create_test_hir_module(func: HirFunction, intrinsics: Vec<(String, Symbol)>) -> HirModule {
    let entry_symbol = func.symbol;
    create_test_hir_module_multi(vec![func], vec![], Some(entry_symbol), intrinsics)
}

fn create_test_hir_module_multi(
    funcs: Vec<HirFunction>,
    structs: Vec<HirStructDef>,
    entry: Option<Symbol>,
    intrinsics: Vec<(String, Symbol)>
) -> HirModule {
    HirModule {
        name: "test_module".to_string(),
        functions: funcs,
        structs,
        enums: vec![],
        statics: vec![],
        entry_point: entry,
        intrinsics,
        next_var_id: 100,
    }
}

// --- Tests ---

#[test]
fn test_lower_if_value() -> Result<(), LoweringError> {
    // Define HIR for: fn select(cond: bool, a: i32, b: i32) -> i32 = if cond then a else b;
    let func_symbol = Symbol::fresh();
    let param_cond_var = parallax_hir::hir::HirVar(0);
    let param_a_var = parallax_hir::hir::HirVar(1);
    let param_b_var = parallax_hir::hir::HirVar(2);

    let bool_ty = HirType::Primitive(HirPrimitiveType::Bool);
    let int_ty = HirType::Primitive(HirPrimitiveType::I32);
    let mir_bool_ty = MirType::Primitive(ResolvePrimitiveType::Bool);
    let mir_int_ty = MirType::Primitive(ResolvePrimitiveType::I32);

    let func = HirFunction {
        span: dummy_span(),
        symbol: func_symbol,
        name: "select".to_string(),
        signature: HirFunctionSignature {
            params: vec![
                (param_cond_var, bool_ty.clone()),
                (param_a_var, int_ty.clone()),
                (param_b_var, int_ty.clone()),
            ],
            return_type: int_ty.clone(),
            is_effectful: false,
        },
        body: Some(
            HirExpr {
                span: dummy_span(),
                kind: HirExprKind::Tail(HirTailExpr::If {
                    condition: Operand::Var(param_cond_var),
                    then_branch: Box::new(HirExpr {
                        span: dummy_span(),
                        kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(param_a_var))),
                        ty: int_ty.clone(),
                    }),
                    else_branch: Box::new(HirExpr {
                        span: dummy_span(),
                        kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(param_b_var))),
                        ty: int_ty.clone(),
                    }),
                }),
                ty: int_ty.clone(),
            }
        ),
    };

    let hir_module = create_test_hir_module_single_func(func, vec![]);
    let (descriptor_store_box, adt_map, prim_map, tuple_map, array_map) = prepare_for_lower_module(&hir_module).expect("Layout setup failed");
    let mir_module = lower_module(&hir_module, descriptor_store_box, adt_map, prim_map, tuple_map, array_map)?;

    // --- Assertions ---
    let graph = mir_module.functions.get(&func_symbol).unwrap();

    // 1. Check parameter node and projections
    let param_node_id = graph.parameter_node.expect("Param node missing");
    let expected_agg_param_ty = MirType::Tuple(vec![mir_bool_ty.clone(), mir_int_ty.clone(), mir_int_ty.clone()]);
    let mut proj_cond_id_opt = None;
    let mut proj_a_id_opt = None;
    let mut proj_b_id_opt = None;
    for edge in &graph.edges {
        if edge.from_node == param_node_id {
             if let Some(MirNode::Project { field_index, aggregate_ty, .. }) = graph.nodes.get(&edge.to_node) {
                 if aggregate_ty == &expected_agg_param_ty {
                     match field_index {
                         0 => proj_cond_id_opt = Some(edge.to_node),
                         1 => proj_a_id_opt = Some(edge.to_node),
                         2 => proj_b_id_opt = Some(edge.to_node),
                         _ => {}
                     }
                 }
             }
        }
    }
    let proj_cond_id = proj_cond_id_opt.expect("Projection for cond not found");
    let proj_a_id = proj_a_id_opt.expect("Projection for a not found");
    let proj_b_id = proj_b_id_opt.expect("Projection for b not found");

    // 2. Find the IfValue node
    let mut if_node_id_opt = None;
    for (id, node) in &graph.nodes {
        if let MirNode::IfValue { condition_ty, ty } = node {
            if condition_ty == &mir_bool_ty && ty == &mir_int_ty {
                if_node_id_opt = Some(*id);
                break;
            }
        }
    }
    let if_node_id = if_node_id_opt.expect("IfValue node not found");

    // 3. Check edges from projections to IfValue inputs
    let mut found_cond_edge = false;
    let mut found_a_edge = false;
    let mut found_b_edge = false;
    for edge in &graph.edges {
        if edge.to_node == if_node_id {
            match edge.to_port {
                PortIndex(0) if edge.from_node == proj_cond_id => found_cond_edge = true,
                PortIndex(1) if edge.from_node == proj_a_id => found_a_edge = true,
                PortIndex(2) if edge.from_node == proj_b_id => found_b_edge = true,
                _ => {}
            }
        }
    }
    assert!(found_cond_edge, "Edge from cond projection to IfValue input 0 not found");
    assert!(found_a_edge, "Edge from a projection to IfValue input 1 not found");
    assert!(found_b_edge, "Edge from b projection to IfValue input 2 not found");

    // 4. Check return port points to IfValue output
    let (return_node_id, return_port_index) = graph.return_port.expect("Return port missing");
    assert_eq!(return_node_id, if_node_id, "Return node should be the IfValue node");
    assert_eq!(return_port_index, PortIndex(0));

    // 5. Node count: Param, ProjCond, ProjA, ProjB, IfValue = 5
    assert_eq!(graph.nodes.len(), 5, "Expected 5 nodes");
    // 6. Edge count: Param->Proj * 3, Proj->If * 3 = 6
    assert_eq!(graph.edges.len(), 6, "Expected 6 edges");

    Ok(())
}

// TODO: Add tests for MatchDispatch. 