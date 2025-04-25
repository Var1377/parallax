//! Tests for control flow lowering (if, match).

use super::helpers::*;
// Add necessary imports used directly in tests
use crate::lowering::LoweringError;
use crate::mir::{MirNode, MirType, MirEdge, PortIndex};
use parallax_hir::hir::{ HirFunction, HirFunctionSignature, HirExpr, HirExprKind, HirTailExpr, HirValue, Operand, HirLiteral, HirVar, AggregateKind, HirType, PrimitiveType, HirPattern};
use parallax_resolve::types::{PrimitiveType as ResolvePrimitiveType, Symbol};
use std::collections::HashMap; // Needed for MatchDispatch arms

#[test]
fn test_lower_if_expr() -> Result<(), LoweringError> {
    let func_symbol = Symbol::new(0);
    let hir_bool_ty = PrimitiveType::Bool;
    let hir_i64_ty = PrimitiveType::I64;
    let bool_ty = HirType::Primitive(hir_bool_ty);
    let param_cond_var = HirVar(0);
    let i64_ty = HirType::Primitive(hir_i64_ty);
    let const_10 = HirLiteral::IntLiteral { value: 10, ty: hir_i64_ty };
    let const_20 = HirLiteral::IntLiteral { value: 20, ty: hir_i64_ty };

    // fn test_if(cond: bool) -> i64 { if cond { 10 } else { 20 } }
    let func_def = HirFunction {
        symbol: func_symbol,
        name: "test_if".to_string(),
        signature: HirFunctionSignature {
            params: vec![(param_cond_var, bool_ty.clone())],
            return_type: i64_ty.clone(),
            is_effectful: false,
        },
        body: Some(HirExpr {
            kind: HirExprKind::Tail(HirTailExpr::If {
                condition: Operand::Var(param_cond_var),
                then_branch: Box::new(HirExpr {
                    kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Const(const_10.clone()))),
                    ty: i64_ty.clone(),
                    span: dummy_span(),
                }),
                else_branch: Box::new(HirExpr {
                    kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Const(const_20.clone()))),
                    ty: i64_ty.clone(),
                    span: dummy_span(),
                }),
            }),
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

    // Expecting: Param (tuple), Project (bool), Constant (10), Constant (20), IfValue
    assert_eq!(mir_graph.nodes.len(), 5);

    // Find the nodes
    let param_node_id = mir_graph.parameter_node.unwrap();
    let mut proj_node_id = None;
    let mut const_10_node_id = None;
    let mut const_20_node_id = None;
    let mut if_node_id = None;

    for (id, node) in &mir_graph.nodes {
        match node {
            MirNode::Project { field_index: 0, aggregate_ty, field_ty }
                if *aggregate_ty == MirType::Tuple(vec![MirType::Primitive(ResolvePrimitiveType::Bool)]) &&
                   *field_ty == MirType::Primitive(ResolvePrimitiveType::Bool) => proj_node_id = Some(*id),
            MirNode::Constant { value: HirLiteral::IntLiteral { value: 10, .. }, .. } => const_10_node_id = Some(*id),
            MirNode::Constant { value: HirLiteral::IntLiteral { value: 20, .. }, .. } => const_20_node_id = Some(*id),
            MirNode::IfValue { condition_ty, ty }
                if *condition_ty == MirType::Primitive(ResolvePrimitiveType::Bool) &&
                   *ty == MirType::Primitive(ResolvePrimitiveType::I64) => if_node_id = Some(*id),
            MirNode::Parameter { .. } => {}, // Already found param via graph.parameter_node
            _ => panic!("Unexpected node found: {:?}", node),
        }
    }

    let proj_node_id = proj_node_id.expect("Projection node not found");
    let const_10_node_id = const_10_node_id.expect("Constant 10 node not found");
    let const_20_node_id = const_20_node_id.expect("Constant 20 node not found");
    let if_node_id = if_node_id.expect("IfValue node not found");

    // Check return port points to IfValue
    assert_eq!(mir_graph.return_port.unwrap().0, if_node_id);

    // Check edges
    assert_eq!(mir_graph.edges.len(), 4, "Expected 4 edges");
    // Param -> Project
    assert!(mir_graph.edges.contains(&MirEdge { from_node: param_node_id, from_port: PortIndex(0), to_node: proj_node_id, to_port: PortIndex(0) }));
    // Project (cond) -> IfValue (input 0)
    assert!(mir_graph.edges.contains(&MirEdge { from_node: proj_node_id, from_port: PortIndex(0), to_node: if_node_id, to_port: PortIndex(0) }));
    // Const 10 -> IfValue (input 1 - then)
    assert!(mir_graph.edges.contains(&MirEdge { from_node: const_10_node_id, from_port: PortIndex(0), to_node: if_node_id, to_port: PortIndex(1) }));
    // Const 20 -> IfValue (input 2 - else)
    assert!(mir_graph.edges.contains(&MirEdge { from_node: const_20_node_id, from_port: PortIndex(0), to_node: if_node_id, to_port: PortIndex(2) }));

    Ok(())
}

#[test]
fn test_lower_enum_match() -> Result<(), LoweringError> {
    let func_symbol = Symbol::new(0);
    let option_enum = create_option_enum_def();
    let option_sym = option_enum.symbol;
    let none_sym = option_enum.variants[0].symbol;
    let some_sym = option_enum.variants[1].symbol;
    let option_ty = HirType::Adt(option_sym);
    let hir_i32_ty = PrimitiveType::I32;
    let i32_ty = HirType::Primitive(hir_i32_ty);

    let param_opt = HirVar(0);
    let var_val = HirVar(1); // For binding inside Some
    let const_0 = HirLiteral::IntLiteral { value: 0, ty: hir_i32_ty };
    let const_1 = HirLiteral::IntLiteral { value: 1, ty: hir_i32_ty };

    // fn test_match(opt: Option<i32>) -> i32 {
    //   match opt {
    //     None => 0,
    //     Some(val) => 1, // Simplified, ignore val for now
    //   }
    // }
    let func_def = HirFunction {
        symbol: func_symbol,
        name: "test_match".to_string(),
        signature: HirFunctionSignature {
            params: vec![(param_opt, option_ty.clone())],
            return_type: i32_ty.clone(),
            is_effectful: false,
        },
        body: Some(HirExpr {
            kind: HirExprKind::Tail(HirTailExpr::Match {
                scrutinee: Operand::Var(param_opt),
                arms: vec![
                    // Arm 0: None => 0
                    (
                        HirPattern::Variant { variant_symbol: none_sym, bindings: vec![] },
                        HirExpr {
                            kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Const(const_0.clone()))),
                            ty: i32_ty.clone(),
                            span: dummy_span(),
                        }
                    ),
                    // Arm 1: Some(val) => 1
                    (
                        // Note: Bindings not fully handled by MatchDispatch MIR node yet
                        HirPattern::Variant { variant_symbol: some_sym, bindings: vec![(var_val, i32_ty.clone())] },
                        HirExpr {
                            kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Const(const_1.clone()))),
                            ty: i32_ty.clone(),
                            span: dummy_span(),
                        }
                    ),
                ],
                otherwise: None, // Match should be exhaustive
            }),
            ty: i32_ty.clone(),
            span: dummy_span(),
        }),
        span: dummy_span(),
    };

    let mut hir_module = create_test_module(func_def.clone());
    hir_module.enums.push(option_enum.clone()); // Add enum def to module

    let mir_graph = test_lower_function(&hir_module, &func_def)?;

    // --- Assertions ---
    assert!(mir_graph.parameter_node.is_some());
    assert!(mir_graph.return_port.is_some());

    // Expecting: Param(Option<i32>), Project(Option<i32>), Const(0), Const(1), MatchDispatch
    assert_eq!(mir_graph.nodes.len(), 5);

    let mut param_id = None;
    let mut project_id = None;
    let mut const_0_id = None;
    let mut const_1_id = None;
    let mut match_id = None;

    for (id, node) in &mir_graph.nodes {
        match node {
            MirNode::Parameter { ty, .. } if *ty == MirType::Tuple(vec![MirType::Adt(option_sym)]) => param_id = Some(*id),
            MirNode::Project { field_index: 0, aggregate_ty, field_ty }
                 if *aggregate_ty == MirType::Tuple(vec![MirType::Adt(option_sym)]) &&
                    *field_ty == MirType::Adt(option_sym) => project_id = Some(*id),
            MirNode::Constant { value: HirLiteral::IntLiteral { value: 0, .. }, .. } => const_0_id = Some(*id),
            MirNode::Constant { value: HirLiteral::IntLiteral { value: 1, .. }, .. } => const_1_id = Some(*id),
            MirNode::MatchDispatch { enum_symbol, arms, otherwise } if *enum_symbol == option_sym => {
                assert_eq!(arms.len(), 2);
                assert!(arms.contains_key(&none_sym));
                assert!(arms.contains_key(&some_sym));
                assert!(otherwise.is_none()); // Explicitly None in HIR
                match_id = Some(*id);
            }
            _ => panic!("Unexpected node: {:?}", node),
        }
    }

    let param_id = param_id.expect("Param node not found");
    let project_id = project_id.expect("Project node for param not found");
    let const_0_id = const_0_id.expect("Const 0 node not found");
    let const_1_id = const_1_id.expect("Const 1 node not found");
    let match_id = match_id.expect("MatchDispatch node not found");

    // Get the target ports for the arms from the MatchDispatch node
    let match_node = mir_graph.nodes.get(&match_id).unwrap();
    let (none_target_node, none_target_port) = match match_node {
        MirNode::MatchDispatch { arms, .. } => arms.get(&none_sym).cloned().unwrap(),
        _ => panic!("Match node is not MatchDispatch"),
    };
    let (some_target_node, some_target_port) = match match_node {
        MirNode::MatchDispatch { arms, .. } => arms.get(&some_sym).cloned().unwrap(),
        _ => panic!("Match node is not MatchDispatch"),
    };

    // Check that the arm targets point to the correct constant nodes
    assert_eq!(none_target_node, const_0_id);
    assert_eq!(none_target_port, PortIndex(0));
    assert_eq!(some_target_node, const_1_id); // Note: Bindings ignored for now
    assert_eq!(some_target_port, PortIndex(0));

    // Check return port points to MatchDispatch node
    assert_eq!(mir_graph.return_port.unwrap().0, match_id);

    // Check edges: Param -> Project -> MatchDispatch
    // Note: Edges from Const nodes to Match arms are implicit via the `arms` map
    assert_eq!(mir_graph.edges.len(), 2, "Expected 2 edges");
    // Param -> Project
    assert!(mir_graph.edges.contains(&MirEdge { from_node: param_id, from_port: PortIndex(0), to_node: project_id, to_port: PortIndex(0) }));
    // Project (scrutinee) -> MatchDispatch (input 0)
    assert!(mir_graph.edges.contains(&MirEdge { from_node: project_id, from_port: PortIndex(0), to_node: match_id, to_port: PortIndex(0) }));

    Ok(())
} 