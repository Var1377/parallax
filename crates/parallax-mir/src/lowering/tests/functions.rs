//! Tests for function call lowering.

use super::helpers::*;
// Add necessary imports used directly in tests
use crate::lowering::LoweringError;
use crate::mir::{MirNode, MirType, MirEdge, PortIndex};
use parallax_hir::hir::{ HirFunction, HirFunctionSignature, HirExpr, HirExprKind, HirTailExpr, HirValue, Operand, HirLiteral, HirVar, AggregateKind, HirType, PrimitiveType};
use parallax_resolve::types::{PrimitiveType as ResolvePrimitiveType, Symbol};
use std::sync::Arc;

#[test]
fn test_lower_function_call() -> Result<(), LoweringError> {
    let main_symbol = Symbol::new(0);
    let add_symbol = Symbol::new(1);
    let hir_i32_ty = PrimitiveType::I32;
    let i32_ty = HirType::Primitive(hir_i32_ty);
    let param_x = HirVar(0);
    let param_y = HirVar(1);
    let var_res = HirVar(2);
    let const_5 = HirLiteral::IntLiteral { value: 5, ty: hir_i32_ty };
    let const_7 = HirLiteral::IntLiteral { value: 7, ty: hir_i32_ty };

    // fn add(x: i32, y: i32) -> i32 { x } // Dummy body, just need signature correct
    let add_func = HirFunction {
        symbol: add_symbol,
        name: "add".to_string(),
        signature: HirFunctionSignature {
            params: vec![(param_x, i32_ty.clone()), (param_y, i32_ty.clone())],
            return_type: i32_ty.clone(),
            is_effectful: false,
        },
        body: Some(HirExpr { // Need some body for lowering
             kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(param_x))),
             ty: i32_ty.clone(),
             span: dummy_span(),
        }),
        span: dummy_span(),
    };

    // fn main() -> i32 { let res = add(5, 7); res }
    let main_func = HirFunction {
        symbol: main_symbol,
        name: "main".to_string(),
        signature: HirFunctionSignature {
            params: vec![],
            return_type: i32_ty.clone(),
            is_effectful: false,
        },
        body: Some(HirExpr {
            kind: HirExprKind::Let {
                var: var_res,
                var_ty: i32_ty.clone(),
                value: Box::new(HirValue::Call {
                    func: Operand::Global(add_symbol), // Calling global function `add`
                    args: vec![
                        Operand::Const(const_5.clone()),
                        Operand::Const(const_7.clone()),
                    ],
                }),
                rest: Box::new(HirExpr {
                    kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(var_res))),
                    ty: i32_ty.clone(),
                    span: dummy_span(),
                }),
            },
            ty: i32_ty.clone(),
            span: dummy_span(),
        }),
        span: dummy_span(),
    };

    // Create module containing BOTH functions
    let mut hir_module = create_test_module(main_func.clone());
    hir_module.functions.push(add_func); // Add the 'add' function

    // Test lowering of the 'main' function
    let mir_graph = test_lower_function(&hir_module, &main_func)?;

    // --- Assertions (for main graph) ---
    assert_eq!(mir_graph.symbol, main_symbol);
    assert!(mir_graph.parameter_node.is_some());
    assert!(mir_graph.return_port.is_some());

    // Expecting: Param, Const(5), Const(7), StaticAddr(add), FunctionCall
    assert_eq!(mir_graph.nodes.len(), 5);

    let mut const_5_id = None;
    let mut const_7_id = None;
    let mut static_addr_id = None;
    let mut call_id = None;
    let mut param_id = None;
    let expected_func_ty = MirType::FunctionPointer(
        vec![MirType::Primitive(ResolvePrimitiveType::I32), MirType::Primitive(ResolvePrimitiveType::I32)],
        Arc::new(MirType::Primitive(ResolvePrimitiveType::I32)),
    );

    for (id, node) in &mir_graph.nodes {
        match node {
            MirNode::Constant { value: HirLiteral::IntLiteral { value: 5, .. }, .. } => const_5_id = Some(*id),
            MirNode::Constant { value: HirLiteral::IntLiteral { value: 7, .. }, .. } => const_7_id = Some(*id),
            MirNode::StaticAddr { symbol, ty }
                 if *symbol == add_symbol && *ty == expected_func_ty => static_addr_id = Some(*id),
            MirNode::FunctionCall { func_ty } if *func_ty == expected_func_ty => call_id = Some(*id),
            MirNode::Parameter { .. } => param_id = Some(*id),
            _ => panic!("Unexpected node in main graph: {:?}", node),
        }
    }

    let const_5_id = const_5_id.expect("Const 5 node not found");
    let const_7_id = const_7_id.expect("Const 7 node not found");
    let static_addr_id = static_addr_id.expect("StaticAddr node for 'add' not found");
    let call_id = call_id.expect("FunctionCall node not found");
    let _param_id = param_id.expect("Parameter node not found");

    // Check return port points to FunctionCall node
    assert_eq!(mir_graph.return_port.unwrap().0, call_id);

    // Check edges
    assert_eq!(mir_graph.edges.len(), 3, "Expected 3 edges");
    // StaticAddr(add) -> FunctionCall (input 0 - func ptr)
    assert!(mir_graph.edges.contains(&MirEdge { from_node: static_addr_id, from_port: PortIndex(0), to_node: call_id, to_port: PortIndex(0) }));
    // Const(5) -> FunctionCall (input 1 - arg 0)
    assert!(mir_graph.edges.contains(&MirEdge { from_node: const_5_id, from_port: PortIndex(0), to_node: call_id, to_port: PortIndex(1) }));
    // Const(7) -> FunctionCall (input 2 - arg 1)
    assert!(mir_graph.edges.contains(&MirEdge { from_node: const_7_id, from_port: PortIndex(0), to_node: call_id, to_port: PortIndex(2) }));

    Ok(())
}

#[test]
fn test_lower_intrinsic_call() -> Result<(), LoweringError> {
    let main_symbol = Symbol::new(0);
    let panic_symbol = Symbol::new(749); // Match symbol from user output
    let hir_string_ty = PrimitiveType::String;
    let string_ty = HirType::Primitive(hir_string_ty);
    // Panic typically returns Never, but let's use Unit for simplicity if Never causes issues
    let never_ty = HirType::Never;
    let unit_ty = HirType::Tuple(vec![]); // () type

    let var_call = HirVar(0);
    let const_msg = HirLiteral::StringLiteral("panic message".to_string());

    // Assume panic signature: fn panic(msg: String) -> Never
    // We need the actual function definition for panic in the module to lower its type correctly.
    // Let's create a dummy definition for the test setup.
    let panic_func_def = HirFunction {
        symbol: panic_symbol,
        name: "panic".to_string(), // Name doesn't strictly matter here
        signature: HirFunctionSignature {
            params: vec![(HirVar(100), string_ty.clone())], // Dummy param var
            return_type: never_ty.clone(),
            is_effectful: true, // Panic is effectful
        },
        body: None, // Intrinsics/Extern functions have no body
        span: dummy_span(),
    };

    // fn main() -> () { let _ = panic("panic message"); () }
    let main_func = HirFunction {
        symbol: main_symbol,
        name: "main".to_string(),
        signature: HirFunctionSignature {
            params: vec![],
            return_type: unit_ty.clone(), // Main returns unit
            is_effectful: false,
        },
        body: Some(HirExpr {
            kind: HirExprKind::Let {
                var: var_call, // Result of panic is Never, bind it
                var_ty: never_ty.clone(),
                value: Box::new(HirValue::Call {
                    func: Operand::Global(panic_symbol), // Calling intrinsic via Global
                    args: vec![
                        Operand::Const(const_msg.clone()),
                    ],
                }),
                // Since the call returns Never, the rest of the expression is unreachable
                // A robust HIR might represent this better, but for this test,
                // we just return unit after the call attempt.
                rest: Box::new(HirExpr {
                    kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Const(HirLiteral::Unit))),
                    ty: unit_ty.clone(),
                    span: dummy_span(),
                }),
            },
            ty: unit_ty.clone(), // Overall type of let is the type of rest
            span: dummy_span(),
        }),
        span: dummy_span(),
    };

    let mut hir_module = create_test_module(main_func.clone());
    hir_module.functions.push(panic_func_def); // Add panic definition
    hir_module.intrinsics.push(("std::panic::panic".to_string(), panic_symbol)); // Mark as intrinsic

    let mir_graph = test_lower_function(&hir_module, &main_func)?;

    // --- Assertions (for main graph) ---
    assert_eq!(mir_graph.symbol, main_symbol);
    assert!(mir_graph.parameter_node.is_some());
    assert!(mir_graph.return_port.is_some());

    // Expecting: Param, Const(msg), StaticAddr(panic), FunctionCall(panic), Const(Unit)
    // The FunctionCall returns Never, so the Const(Unit) might technically be dead code,
    // but lowering likely still generates it before realizing reachability.
    assert_eq!(mir_graph.nodes.len(), 5);

    let mut const_msg_id = None;
    let mut static_addr_id = None;
    let mut call_id = None;
    let mut param_id = None;
    let mut const_unit_id = None;

    let expected_panic_func_ty = MirType::FunctionPointer(
        vec![MirType::Primitive(ResolvePrimitiveType::String)],
        Arc::new(MirType::Never), // Panic returns Never
    );

    for (id, node) in &mir_graph.nodes {
        match node {
            MirNode::Constant { value: HirLiteral::StringLiteral(s), .. } if s == "panic message" => const_msg_id = Some(*id),
            MirNode::Constant { value: HirLiteral::Unit, .. } => const_unit_id = Some(*id),
            MirNode::StaticAddr { symbol, ty }
                 if *symbol == panic_symbol && *ty == expected_panic_func_ty => static_addr_id = Some(*id),
            MirNode::FunctionCall { func_ty } if *func_ty == expected_panic_func_ty => call_id = Some(*id),
            MirNode::Parameter { .. } => param_id = Some(*id),
            _ => panic!("Unexpected node in main graph: {:?}", node),
        }
    }

    let const_msg_id = const_msg_id.expect("Const msg node not found");
    let static_addr_id = static_addr_id.expect("StaticAddr node for panic not found");
    let call_id = call_id.expect("FunctionCall node for panic not found");
    let _param_id = param_id.expect("Parameter node not found");
    let const_unit_id = const_unit_id.expect("Const Unit node not found");

    // Check return port points to Const(Unit) node (since panic returns Never, rest executes)
    assert_eq!(mir_graph.return_port.unwrap().0, const_unit_id);

    // Check edges
    // Param -> (none)
    // Const(msg) -> FunctionCall (input 1 - arg 0)
    // StaticAddr(panic) -> FunctionCall (input 0 - func ptr)
    // FunctionCall -> (none, its output is Never and not used by Const(Unit))
    assert_eq!(mir_graph.edges.len(), 2);
    assert!(mir_graph.edges.contains(&MirEdge { from_node: static_addr_id, from_port: PortIndex(0), to_node: call_id, to_port: PortIndex(0) }));
    assert!(mir_graph.edges.contains(&MirEdge { from_node: const_msg_id, from_port: PortIndex(0), to_node: call_id, to_port: PortIndex(1) }));

    Ok(())
} 