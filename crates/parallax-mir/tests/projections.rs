use parallax_mir::{lower_module, MirGraph, MirModule, MirNode, MirType, PortIndex, NodeId, LoweringError};
use parallax_hir::hir::{
    HirModule, HirFunction, HirFunctionSignature, HirExpr, HirExprKind, HirTailExpr,
    HirLiteral, HirType, PrimitiveType as HirPrimitiveType, Operand, HirVar, HirValue,
    ProjectionKind, HirStructDef
};
use parallax_resolve::types::{Symbol, PrimitiveType as ResolvePrimitiveType};
use miette::{SourceOffset, SourceSpan};
use std::sync::Arc;
use std::collections::HashMap;

// Use helpers from the common module
mod common;
use common::{dummy_span, create_test_hir_module_single_func, create_test_hir_module_full, define_point_struct, prepare_for_lower_module};

// --- Tests ---

#[test]
fn test_lower_tuple_projection() -> Result<(), LoweringError> {
    // Define HIR for: fn get_first(pair: (i32, bool)) -> i32 { let tmp = pair.0; tmp }
    let func_symbol = Symbol::fresh();
    let param_pair_var = parallax_hir::hir::HirVar(0);
    let tmp_var = parallax_hir::hir::HirVar(1); // Variable for the projected field
    let tuple_ty = HirType::Tuple(vec![
        HirType::Primitive(HirPrimitiveType::I32),
        HirType::Primitive(HirPrimitiveType::Bool),
    ]);
    let int_ty = HirType::Primitive(HirPrimitiveType::I32);
    let mir_tuple_ty = MirType::Tuple(vec![
        MirType::Primitive(ResolvePrimitiveType::I32),
        MirType::Primitive(ResolvePrimitiveType::Bool),
    ]);
    let mir_int_ty = MirType::Primitive(ResolvePrimitiveType::I32);

    let func = HirFunction {
        span: dummy_span(),
        symbol: func_symbol,
        name: "get_first".to_string(),
        signature: HirFunctionSignature {
            params: vec![(param_pair_var, tuple_ty.clone())],
            return_type: int_ty.clone(),
            is_effectful: false,
        },
        body: Some(
            HirExpr {
                span: dummy_span(),
                kind: HirExprKind::Let {
                    var: tmp_var,
                    var_ty: int_ty.clone(),
                    value: Box::new(HirValue::Project {
                        base: Operand::Var(param_pair_var),
                        projection: ProjectionKind::TupleIndex(0),
                    }),
                    rest: Box::new(HirExpr {
                        span: dummy_span(),
                        kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(tmp_var))),
                        ty: int_ty.clone(),
                    }),
                },
                ty: int_ty.clone(),
            }
        ),
    };

    let hir_module = create_test_hir_module_single_func(func, vec![]);
    let (descriptor_store_box, adt_index_map, primitive_index_map, tuple_index_map, array_index_map) = prepare_for_lower_module(&hir_module).expect("Layout setup failed");
    let mir_module = lower_module(&hir_module, descriptor_store_box, adt_index_map, primitive_index_map, tuple_index_map, array_index_map)?;

    // --- Assertions ---
    let graph = mir_module.functions.get(&func_symbol).unwrap();

    // 1. Find the Parameter node and its projection for the input tuple
    let param_node_id = graph.parameter_node.expect("Parameter node missing");
    let expected_agg_param_ty = MirType::Tuple(vec![mir_tuple_ty.clone()]); // Input is tuple containing the tuple
    let mut tuple_project_node_id = None;
    for edge in &graph.edges {
        if edge.from_node == param_node_id {
            if let Some(MirNode::Project { field_index: 0, aggregate_ty, field_ty }) = graph.nodes.get(&edge.to_node) {
                if aggregate_ty == &expected_agg_param_ty && field_ty == &mir_tuple_ty {
                    tuple_project_node_id = Some(edge.to_node);
                    break;
                }
            }
        }
    }
    assert!(tuple_project_node_id.is_some(), "Projection for input tuple not found");
    let tuple_project_node_id = tuple_project_node_id.unwrap();

    // 2. Find the second Projection node (extracting field 0 from the tuple)
    let mut field_project_node_id = None;
    for edge in &graph.edges {
        if edge.from_node == tuple_project_node_id {
             if let Some(MirNode::Project { field_index: 0, aggregate_ty, field_ty }) = graph.nodes.get(&edge.to_node) {
                 if aggregate_ty == &mir_tuple_ty && field_ty == &mir_int_ty {
                    field_project_node_id = Some(edge.to_node);
                    break;
                 }
             }
        }
    }
    assert!(field_project_node_id.is_some(), "Projection for tuple field 0 not found");
    let field_project_node_id = field_project_node_id.unwrap();

    // 3. Check return port points to the second Projection node output
    let (return_node_id, return_port_index) = graph.return_port.expect("Return port missing");
    assert_eq!(return_node_id, field_project_node_id, "Return should be field projection");
    assert_eq!(return_port_index, PortIndex(0));

    // 4. Check total nodes (Param, ProjectTuple, ProjectField)
    assert_eq!(graph.nodes.len(), 3, "Expected 3 nodes");
    // 5. Check total edges (Param->ProjTuple, ProjTuple->ProjField)
    assert_eq!(graph.edges.len(), 2, "Expected 2 edges");

    Ok(())
}

#[test]
fn test_lower_struct_projection() -> Result<(), LoweringError> {
     // Define HIR for: fn get_y(p: Point) -> i64 = p.y;
    let func_symbol = Symbol::fresh();
    let (point_struct, struct_symbol, _field_x_symbol, field_y_symbol) = define_point_struct();
    let param_p_var = parallax_hir::hir::HirVar(0);
    let tmp_var = parallax_hir::hir::HirVar(1);
    let struct_ty = HirType::Adt(struct_symbol);
    let mir_struct_ty = MirType::Adt(struct_symbol);
    let i64_ty = HirType::Primitive(HirPrimitiveType::I64);
    let mir_i64_ty = MirType::Primitive(ResolvePrimitiveType::I64);

    let func = HirFunction {
        span: dummy_span(),
        symbol: func_symbol,
        name: "get_y".to_string(),
        signature: HirFunctionSignature {
            params: vec![(param_p_var, struct_ty.clone())],
            return_type: i64_ty.clone(),
            is_effectful: false,
        },
        body: Some(
            HirExpr {
                span: dummy_span(),
                kind: HirExprKind::Let {
                    var: tmp_var,
                    var_ty: i64_ty.clone(),
                    value: Box::new(HirValue::Project {
                        base: Operand::Var(param_p_var),
                        projection: ProjectionKind::Field(field_y_symbol), // Project by field symbol
                    }),
                     rest: Box::new(HirExpr {
                         span: dummy_span(),
                         kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(tmp_var))),
                         ty: i64_ty.clone(),
                     }),
                },
                ty: i64_ty.clone(),
            }
        ),
    };

    let hir_module = create_test_hir_module_full(vec![func], vec![point_struct], vec![], vec![], Some(func_symbol), vec![]);
    let (descriptor_store_box, adt_index_map, primitive_index_map, tuple_index_map, array_index_map) = prepare_for_lower_module(&hir_module).expect("Layout setup failed");
    let mir_module = lower_module(&hir_module, descriptor_store_box, adt_index_map, primitive_index_map, tuple_index_map, array_index_map)?;

    // --- Assertions ---
    let graph = mir_module.functions.get(&func_symbol).unwrap();

    // 1. Find the Parameter node and its projection for the input struct
    let param_node_id = graph.parameter_node.expect("Parameter node missing");
    let expected_agg_param_ty = MirType::Tuple(vec![mir_struct_ty.clone()]);
    let struct_project_node_id = graph.edges.iter().find_map(|e| {
        if e.from_node == param_node_id {
            if let Some(MirNode::Project { field_index: 0, aggregate_ty, field_ty }) = graph.nodes.get(&e.to_node) {
                if aggregate_ty == &expected_agg_param_ty && field_ty == &mir_struct_ty {
                    return Some(e.to_node)
                }
            }
        }
        None
    }).expect("Projection for input struct not found");

    // 2. Find the second Projection node (extracting field 'y' (index 1) from the struct)
    let field_project_node_id = graph.edges.iter().find_map(|e| {
        if e.from_node == struct_project_node_id {
             if let Some(MirNode::Project { field_index: 1, aggregate_ty, field_ty }) = graph.nodes.get(&e.to_node) {
                 if aggregate_ty == &mir_struct_ty && field_ty == &mir_i64_ty {
                    return Some(e.to_node)
                 }
             }
        }
        None
    }).expect("Projection for struct field 'y' (index 1) not found");

    // 3. Check return port points to the second Projection node output
    let (return_node_id, return_port_index) = graph.return_port.expect("Return port missing");
    assert_eq!(return_node_id, field_project_node_id, "Return should be field projection");
    assert_eq!(return_port_index, PortIndex(0));

    // 4. Check total nodes (Param, ProjectStruct, ProjectField)
    assert_eq!(graph.nodes.len(), 3);
    // 5. Check total edges (Param->ProjStruct, ProjStruct->ProjField)
    assert_eq!(graph.edges.len(), 2);

    Ok(())
}

// TODO: Add tests for array projection, downcast? 