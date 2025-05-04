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
use common::{dummy_span, create_test_hir_module_single_func, create_test_hir_module_full, prepare_for_lower_module};

// --- Test Helpers (Copied) ---

// ... existing code ...

// --- Tests ---

#[test]
fn test_lower_regular_function_call() -> Result<(), LoweringError> {
    // Define HIR for:
    // fn callee() -> i32 = 99;
    // fn caller() -> i32 { let tmp = callee(); tmp }

    let callee_symbol = Symbol::fresh();
    let caller_symbol = Symbol::fresh();
    let tmp_var = parallax_hir::hir::HirVar(0);
    let int_ty = HirType::Primitive(HirPrimitiveType::I32);
    let mir_int_ty = MirType::Primitive(ResolvePrimitiveType::I32);

    let callee_func = HirFunction {
        span: dummy_span(),
        symbol: callee_symbol,
        name: "callee".to_string(),
        signature: HirFunctionSignature {
            params: vec![],
            return_type: int_ty.clone(),
            is_effectful: false,
        },
        body: Some(
            HirExpr {
                span: dummy_span(),
                kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Const(
                    HirLiteral::IntLiteral { value: 99, ty: HirPrimitiveType::I32 }
                ))),
                ty: int_ty.clone(),
            }
        ),
    };

    let caller_func = HirFunction {
        span: dummy_span(),
        symbol: caller_symbol,
        name: "caller".to_string(),
        signature: HirFunctionSignature {
            params: vec![],
            return_type: int_ty.clone(),
            is_effectful: false,
        },
        body: Some(
            HirExpr {
                span: dummy_span(),
                kind: HirExprKind::Let {
                    var: tmp_var,
                    var_ty: int_ty.clone(),
                    value: Box::new(HirValue::Call {
                        func: Operand::Global(callee_symbol),
                        args: vec![],
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

    let hir_module = create_test_hir_module_full(vec![callee_func, caller_func], vec![], vec![], vec![], Some(caller_symbol), vec![]);
    let (descriptor_store_box, adt_map, prim_map, tuple_map, array_map) = prepare_for_lower_module(&hir_module).expect("Layout setup failed");
    let mir_module = lower_module(&hir_module, descriptor_store_box, adt_map, prim_map, tuple_map, array_map)?;

    // --- Assertions (Focus on Caller) ---
    let graph = mir_module.functions.get(&caller_symbol).unwrap();

    // 1. Find the StaticAddr node for the callee function
    let mut static_addr_node_id = None;
    let callee_func_ty = MirType::FunctionPointer(vec![], Arc::new(mir_int_ty.clone()));
    for (id, node) in &graph.nodes {
        if let MirNode::StaticAddr { symbol, ty } = node {
            if *symbol == callee_symbol && ty == &callee_func_ty {
                static_addr_node_id = Some(*id);
                break;
            }
        }
    }
    assert!(static_addr_node_id.is_some(), "StaticAddr node for callee not found");
    let static_addr_node_id = static_addr_node_id.unwrap();

    // 2. Find the FunctionCall node
    let mut call_node_id = None;
    for (id, node) in &graph.nodes {
        if let MirNode::FunctionCall { func_ty } = node {
            if func_ty == &callee_func_ty {
                 call_node_id = Some(*id);
                 break;
            }
        }
    }
    assert!(call_node_id.is_some(), "FunctionCall node for callee not found");
    let call_node_id = call_node_id.unwrap();

    // 3. Check edge from StaticAddr to FunctionCall input 0
    let mut found_edge_static = false;
    for edge in &graph.edges {
         if edge.to_node == call_node_id && edge.to_port == PortIndex(0) &&
            edge.from_node == static_addr_node_id && edge.from_port == PortIndex(0) {
             found_edge_static = true;
             break;
         }
    }
    assert!(found_edge_static, "Edge from StaticAddr to FunctionCall not found");

    // 4. Check return port points to the FunctionCall output 0
    let (return_node_id, return_port_index) = graph.return_port.expect("Return port missing");
    assert_eq!(return_node_id, call_node_id, "Return node should be FunctionCall");
    assert_eq!(return_port_index, PortIndex(0));

    // 5. Check total nodes (Param, StaticAddr, FuncCall)
    assert_eq!(graph.nodes.len(), 3, "Expected 3 nodes");
    // 6. Check total edges (StaticAddr->Call)
    assert_eq!(graph.edges.len(), 1, "Expected 1 edge");

    Ok(())
}

#[test]
fn test_lower_function_call_with_args() -> Result<(), LoweringError> {
    // Define HIR for:
    // fn callee(a: i32, b: bool) -> i32 = if b then a else 0;
    // fn caller() -> i32 { let tmp = callee(55, true); tmp }

    let callee_symbol = Symbol::fresh();
    let caller_symbol = Symbol::fresh();
    let tmp_var = parallax_hir::hir::HirVar(0);
    let callee_param_a_var = parallax_hir::hir::HirVar(1);
    let callee_param_b_var = parallax_hir::hir::HirVar(2);

    let int_ty = HirType::Primitive(HirPrimitiveType::I32);
    let bool_ty = HirType::Primitive(HirPrimitiveType::Bool);
    let mir_int_ty = MirType::Primitive(ResolvePrimitiveType::I32);
    let mir_bool_ty = MirType::Primitive(ResolvePrimitiveType::Bool);

    let callee_func = HirFunction {
        span: dummy_span(),
        symbol: callee_symbol,
        name: "callee".to_string(),
        signature: HirFunctionSignature {
            params: vec![(callee_param_a_var, int_ty.clone()), (callee_param_b_var, bool_ty.clone())],
            return_type: int_ty.clone(),
            is_effectful: false,
        },
        body: Some(HirExpr {
            span: dummy_span(),
            kind: HirExprKind::Tail(HirTailExpr::If {
                condition: Operand::Var(callee_param_b_var),
                then_branch: Box::new(HirExpr { span: dummy_span(), kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(callee_param_a_var))), ty: int_ty.clone() }),
                else_branch: Box::new(HirExpr { span: dummy_span(), kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Const(HirLiteral::IntLiteral{value: 0, ty: HirPrimitiveType::I32}))), ty: int_ty.clone() }),
            }),
            ty: int_ty.clone(),
        }),
    };

    let caller_func = HirFunction {
        span: dummy_span(),
        symbol: caller_symbol,
        name: "caller".to_string(),
        signature: HirFunctionSignature { params: vec![], return_type: int_ty.clone(), is_effectful: false },
        body: Some(HirExpr {
            span: dummy_span(),
            kind: HirExprKind::Let {
                var: tmp_var,
                var_ty: int_ty.clone(),
                value: Box::new(HirValue::Call {
                    func: Operand::Global(callee_symbol),
                    args: vec![
                        Operand::Const(HirLiteral::IntLiteral { value: 55, ty: HirPrimitiveType::I32 }),
                        Operand::Const(HirLiteral::BoolLiteral(true)),
                    ],
                }),
                rest: Box::new(HirExpr { span: dummy_span(), kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(tmp_var))), ty: int_ty.clone() }),
            },
            ty: int_ty.clone(),
        }),
    };

    let hir_module = create_test_hir_module_full(vec![callee_func, caller_func], vec![], vec![], vec![], Some(caller_symbol), vec![]);
    let (descriptor_store_box, adt_map, prim_map, tuple_map, array_map) = prepare_for_lower_module(&hir_module).expect("Layout setup failed");
    let mir_module = lower_module(&hir_module, descriptor_store_box, adt_map, prim_map, tuple_map, array_map)?;

    // --- Assertions (Focus on Caller) ---
    let graph = mir_module.functions.get(&caller_symbol).unwrap();

    // 1. Find StaticAddr, Constants, and FunctionCall nodes
    let callee_func_ty = MirType::FunctionPointer(vec![mir_int_ty.clone(), mir_bool_ty.clone()], Arc::new(mir_int_ty.clone()));
    let static_addr_id = graph.nodes.iter().find_map(|(id, n)| if let MirNode::StaticAddr { symbol, ty } = n { if *symbol == callee_symbol && ty == &callee_func_ty { Some(*id) } else { None }} else {None}).expect("StaticAddr missing");
    let const_55_id = graph.nodes.iter().find_map(|(id, n)| if let MirNode::Constant { value: HirLiteral::IntLiteral{value: 55, ..}, ..} = n { Some(*id) } else { None }).expect("Const 55 missing");
    let const_true_id = graph.nodes.iter().find_map(|(id, n)| if let MirNode::Constant { value: HirLiteral::BoolLiteral(true), ..} = n { Some(*id) } else { None }).expect("Const true missing");
    let call_id = graph.nodes.iter().find_map(|(id, n)| if let MirNode::FunctionCall { func_ty } = n { if func_ty == &callee_func_ty { Some(*id) } else { None }} else {None}).expect("FunctionCall missing");

    // 2. Check edges to FunctionCall
    assert!(graph.edges.iter().any(|e| e.from_node == static_addr_id && e.to_node == call_id && e.to_port == PortIndex(0)), "Edge StaticAddr->Call:0 missing");
    assert!(graph.edges.iter().any(|e| e.from_node == const_55_id && e.to_node == call_id && e.to_port == PortIndex(1)), "Edge Const(55)->Call:1 missing");
    assert!(graph.edges.iter().any(|e| e.from_node == const_true_id && e.to_node == call_id && e.to_port == PortIndex(2)), "Edge Const(true)->Call:2 missing");

    // 3. Check return port points to the FunctionCall output 0
    let (return_node_id, return_port_index) = graph.return_port.expect("Return port missing");
    assert_eq!(return_node_id, call_id, "Return node should be FunctionCall");
    assert_eq!(return_port_index, PortIndex(0));

    // 4. Node count (Param, StaticAddr, ConstInt, ConstBool, FuncCall)
    assert_eq!(graph.nodes.len(), 5);
    // 5. Edge count (StaticAddr->Call, Const->Call, Const->Call)
    assert_eq!(graph.edges.len(), 3);

    Ok(())
}

// TODO: Add tests for calls returning aggregates/non-primitives. 