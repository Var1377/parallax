//! Tests for basic MIR lowering (constants, let).

use super::helpers::*;
// Add necessary imports used directly in tests
use crate::lowering::LoweringError;
use crate::mir::{MirNode, MirType, MirEdge, PortIndex};
use parallax_hir::hir::{ HirFunction, HirFunctionSignature, HirExpr, HirExprKind, HirTailExpr, HirValue, Operand, HirLiteral, HirVar, AggregateKind, HirType, PrimitiveType};
use parallax_resolve::types::{PrimitiveType as ResolvePrimitiveType, Symbol};

#[test]
fn test_lower_return_constant() -> Result<(), LoweringError> {
    let func_symbol = Symbol::new(0);
    let hir_i64_ty = PrimitiveType::I64;
    let return_value = HirLiteral::IntLiteral { value: 42, ty: hir_i64_ty };
    let return_type = HirType::Primitive(hir_i64_ty);

    let func_def = HirFunction {
        symbol: func_symbol,
        name: "test_func".to_string(),
        signature: HirFunctionSignature {
            params: vec![],
            return_type: return_type.clone(),
            is_effectful: false, // Assuming default false
        },
        body: Some(HirExpr {
            kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Const(return_value.clone()))),
            ty: return_type,
            span: dummy_span(),
        }),
        span: dummy_span(),
    };

    let hir_module = create_test_module(func_def.clone());
    let mir_graph = test_lower_function(&hir_module, &func_def)?;

    // --- Assertions ---
    assert_eq!(mir_graph.symbol, func_symbol);
    assert!(mir_graph.parameter_node.is_some(), "Parameter node should exist");
    assert!(mir_graph.return_port.is_some(), "Return port should be set");
    assert_eq!(mir_graph.nodes.len(), 2, "Expected Param and Constant nodes");

    let return_node_id = mir_graph.return_port.unwrap().0;
    let return_node = mir_graph.nodes.get(&return_node_id).expect("Return node not found");

    match return_node {
        MirNode::Constant { value, ty } => {
            assert_eq!(*value, return_value);
            assert_eq!(*ty, MirType::Primitive(ResolvePrimitiveType::I64));
        }
        _ => panic!("Return node is not a Constant node, but {:?}", return_node),
    }

    let param_node_id = mir_graph.parameter_node.unwrap();
    let param_node = mir_graph.nodes.get(&param_node_id).expect("Param node not found");
    match param_node {
        MirNode::Parameter { index, ty } => {
            assert_eq!(*index, 0);
            assert_eq!(*ty, MirType::Tuple(vec![]));
        }
        _ => panic!("Expected Parameter node, found {:?}", param_node),
    }

    assert!(mir_graph.edges.is_empty(), "Expected no explicit edges");

    Ok(())
}

#[test]
fn test_lower_let_binding() -> Result<(), LoweringError> {
    let func_symbol = Symbol::new(0);
    let hir_i64_ty = PrimitiveType::I64;
    let i64_ty = HirType::Primitive(hir_i64_ty);
    let var_x = HirVar(0);
    let const_val = HirLiteral::IntLiteral { value: 10, ty: hir_i64_ty };

    // let x = 10; return x;
    let func_def = HirFunction {
        symbol: func_symbol,
        name: "test_let".to_string(),
        signature: HirFunctionSignature {
            params: vec![],
            return_type: i64_ty.clone(),
            is_effectful: false,
        },
        body: Some(HirExpr {
            kind: HirExprKind::Let {
                var: var_x,
                var_ty: i64_ty.clone(),
                value: Box::new(HirValue::Use(Operand::Const(const_val.clone()))),
                rest: Box::new(HirExpr {
                    kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(var_x))),
                    ty: i64_ty.clone(),
                    span: dummy_span(),
                }),
            },
            ty: i64_ty.clone(),
            span: dummy_span(),
        }),
        span: dummy_span(),
    };

    let hir_module = create_test_module(func_def.clone());
    let mir_graph = test_lower_function(&hir_module, &func_def)?;

    // --- Assertions ---
    assert_eq!(mir_graph.symbol, func_symbol);
    assert!(mir_graph.parameter_node.is_some());
    assert!(mir_graph.return_port.is_some());
    assert_eq!(mir_graph.nodes.len(), 2, "Expected Param and Constant nodes");

    let return_node_id = mir_graph.return_port.unwrap().0;
    let return_node = mir_graph.nodes.get(&return_node_id).expect("Return node not found");

    match return_node {
        MirNode::Constant { value, ty } => {
            assert_eq!(*value, const_val);
            assert_eq!(*ty, MirType::Primitive(ResolvePrimitiveType::I64));
        }
        _ => panic!("Return node is not a Constant node, but {:?}", return_node),
    }

    assert!(mir_graph.edges.is_empty(), "Expected no explicit edges");

    Ok(())
} 