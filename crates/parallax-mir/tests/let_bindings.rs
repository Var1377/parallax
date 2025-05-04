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

// --- Tests ---

#[test]
fn test_lower_let_const() -> Result<(), LoweringError> {
    // Define HIR for: fn main() -> i32 { let x = 42; x }
    let main_symbol = Symbol::fresh();
    let var_x = parallax_hir::hir::HirVar(0); // Use ID 0 for HirVar

    let main_func = HirFunction {
        span: dummy_span(),
        symbol: main_symbol,
        name: "main".to_string(),
        signature: HirFunctionSignature {
            params: vec![],
            return_type: HirType::Primitive(HirPrimitiveType::I32),
            is_effectful: false,
        },
        body: Some(
            HirExpr {
                span: dummy_span(),
                kind: HirExprKind::Let {
                    var: var_x,
                    var_ty: HirType::Primitive(HirPrimitiveType::I32),
                    value: Box::new(HirValue::Use(Operand::Const(HirLiteral::IntLiteral {
                        value: 42,
                        ty: HirPrimitiveType::I32,
                    }))),
                    rest: Box::new(HirExpr {
                        span: dummy_span(),
                        kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(var_x))),
                        ty: HirType::Primitive(HirPrimitiveType::I32),
                    }),
                },
                ty: HirType::Primitive(HirPrimitiveType::I32),
            }
        ),
    };

    let hir_module = create_test_hir_module_single_func(main_func, vec![]);
    let (descriptor_store_box, adt_map, prim_map, tuple_map, array_map) = prepare_for_lower_module(&hir_module).expect("Layout setup failed");
    let mir_module = lower_module(&hir_module, descriptor_store_box, adt_map, prim_map, tuple_map, array_map)?;

    // --- Assertions ---
    let graph = mir_module.functions.get(&main_symbol).unwrap();

    // 1. Check parameter node (empty tuple)
    let param_node_id = graph.parameter_node.expect("Parameter node missing");
    let param_node = graph.nodes.get(&param_node_id).unwrap();
     match param_node {
         MirNode::Parameter { ty, .. } => {
              match ty {
                  MirType::Tuple(elements) => assert!(elements.is_empty()),
                  _ => panic!("Expected Tuple type for param node")
              }
         }
          _ => panic!("Expected Parameter node type")
     }

    // 2. Find the Constant node for 42
    let mut const_node_id = None;
    for (id, node) in &graph.nodes {
        if let MirNode::Constant { value, ty } = node {
             if let HirLiteral::IntLiteral { value: val, .. } = value {
                 if *val == 42 && ty == &MirType::Primitive(ResolvePrimitiveType::I32) {
                     const_node_id = Some(*id);
                     break;
                 }
             }
        }
    }
    assert!(const_node_id.is_some(), "Constant node for 42 not found");
    let const_node_id = const_node_id.unwrap();

    // 3. Check return port points to the Constant node
    let (return_node_id, return_port_index) = graph.return_port.expect("Return port missing");
    assert_eq!(return_node_id, const_node_id, "Return should be Constant node");
    assert_eq!(return_port_index, PortIndex(0));

    // 4. Check total number of nodes (Param, Const)
    assert_eq!(graph.nodes.len(), 2, "Expected 2 nodes (Param, Const)");

    // 5. Check number of edges (should be 0)
    assert_eq!(graph.edges.len(), 0, "Expected 0 edges");

    Ok(())
}

#[test]
fn test_lower_let_call() -> Result<(), LoweringError> {
    // Define HIR for: fn caller() -> i32 { let x = callee(); x }
    //                fn callee() -> i32 { 42 }
    let caller_symbol = Symbol::fresh();
    let callee_symbol = Symbol::fresh();
    let var_x = parallax_hir::hir::HirVar(0);
    let int_ty = HirType::Primitive(HirPrimitiveType::I32);
    let mir_int_ty = MirType::Primitive(ResolvePrimitiveType::I32);
    let mir_fn_ty = MirType::FunctionPointer(vec![], Arc::new(mir_int_ty.clone()));

    let callee_func = HirFunction {
        span: dummy_span(),
        symbol: callee_symbol,
        name: "callee".to_string(),
        signature: HirFunctionSignature { params: vec![], return_type: int_ty.clone(), is_effectful: false },
        body: Some(HirExpr {
            span: dummy_span(),
            kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Const(HirLiteral::IntLiteral { value: 42, ty: HirPrimitiveType::I32 }))),
            ty: int_ty.clone(),
        }),
    };

    let caller_func = HirFunction {
        span: dummy_span(),
        symbol: caller_symbol,
        name: "caller".to_string(),
        signature: HirFunctionSignature { params: vec![], return_type: int_ty.clone(), is_effectful: false },
        body: Some(
            HirExpr {
                span: dummy_span(),
                kind: HirExprKind::Let {
                    var: var_x,
                    var_ty: int_ty.clone(),
                    value: Box::new(HirValue::Call {
                        func: Operand::Global(callee_symbol),
                        args: vec![],
                    }),
                    rest: Box::new(HirExpr {
                        span: dummy_span(),
                        kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(var_x))),
                        ty: int_ty.clone(),
                    }),
                },
                ty: int_ty.clone(),
            }
        ),
    };
    // Use the full helper directly if it's confirmed public in common.rs
    // For now, assuming it might not be, construct manually or use restricted helper
    let hir_module = common::create_test_hir_module_full(vec![caller_func, callee_func], vec![], vec![], vec![], Some(caller_symbol), vec![]);
    let (descriptor_store_box, adt_map, prim_map, tuple_map, array_map) = prepare_for_lower_module(&hir_module).expect("Layout setup failed");
    let mir_module = lower_module(&hir_module, descriptor_store_box, adt_map, prim_map, tuple_map, array_map)?;

    // Assertions (focus on caller graph)
    let graph = mir_module.functions.get(&caller_symbol).unwrap();

    // 1. Find StaticAddr for callee
    let static_addr_id = graph.nodes.iter().find_map(|(id, node)| {
        if let MirNode::StaticAddr { symbol, ty } = node { if *symbol == callee_symbol { Some(*id) } else { None } } else { None }
    }).expect("StaticAddr not found");

    // 2. Find FunctionCall node
    let call_id = graph.nodes.iter().find_map(|(id, node)| {
        if let MirNode::FunctionCall { func_ty } = node { if func_ty == &mir_fn_ty { Some(*id) } else { None } } else { None }
    }).expect("FunctionCall not found");

    // 3. Check edge StaticAddr -> Call
    assert!(graph.edges.iter().any(|e| e.from_node == static_addr_id && e.to_node == call_id && e.to_port == PortIndex(0)), "Edge StaticAddr->Call missing");

    // 4. Check return port points to the FunctionCall result
    let (ret_id, ret_port) = graph.return_port.expect("Return port missing");
    assert_eq!(ret_id, call_id, "Return should be FunctionCall node");
    assert_eq!(ret_port, PortIndex(0));

    // 5. Node count (Param, StaticAddr, Call)
    assert_eq!(graph.nodes.len(), 3);
    // 6. Edge count (StaticAddr->Call)
    assert_eq!(graph.edges.len(), 1);

    Ok(())
}

// TODO: Add tests for let binding other HirValue types (calls, aggregates, etc.) 