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

// --- Test Helpers ---

// Helper to create a simple HIR module for testing
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
        next_var_id: 100, // Start var IDs high to avoid clashes in manual tests
    }
}

// --- Tests ---

#[test]
fn test_lower_return_i32_literal() -> Result<(), LoweringError> {
    // Define HIR for: fn main() = 42;
    let main_symbol = Symbol::fresh();
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
                kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Const(
                    HirLiteral::IntLiteral {
                        value: 42,
                        ty: HirPrimitiveType::I32,
                    }
                ))),
                ty: HirType::Primitive(HirPrimitiveType::I32),
            }
        ),
    };

    let hir_module = create_test_hir_module_single_func(main_func, vec![]);
    let (descriptor_store_box, adt_map, prim_map, tuple_map, array_map) = prepare_for_lower_module(&hir_module).expect("Layout setup failed");
    let mir_module = lower_module(&hir_module, descriptor_store_box, adt_map, prim_map, tuple_map, array_map)?;

    // Assertions
    let main_graph = mir_module.functions.get(&main_symbol).expect("Main graph missing");

    let (return_node_id, return_port_index) = main_graph.return_port.expect("Return port missing");
    assert_eq!(return_port_index, PortIndex(0));

    let return_node = main_graph.nodes.get(&return_node_id).expect("Return node missing");
    match return_node {
        MirNode::Constant { value, ty } => {
            if let HirLiteral::IntLiteral { value: ret_val, .. } = value {
                 assert_eq!(*ret_val, 42);
            } else {
                panic!("Expected IntLiteral");
            }
            assert_eq!(ty, &MirType::Primitive(ResolvePrimitiveType::I32));
        }
        _ => panic!("Expected Constant node, found {:?}", return_node),
    }

    let param_node_id = main_graph.parameter_node.expect("Parameter node missing");
    let param_node = main_graph.nodes.get(&param_node_id).unwrap();
     match param_node {
         MirNode::Parameter { ty, .. } => {
              match ty {
                  MirType::Tuple(elements) => assert!(elements.is_empty()),
                  _ => panic!("Expected Tuple type for param node")
              }
         }
          _ => panic!("Expected Parameter node type")
     }
    assert_ne!(return_node_id, param_node_id);
    assert!(main_graph.edges.is_empty()); // No edges needed

    Ok(())
}

// TODO: Add tests for other literal types (f64, bool, char, unit, string?)

#[test]
fn test_lower_return_f64_literal() -> Result<(), LoweringError> {
    let main_symbol = Symbol::fresh();
    let main_func = HirFunction {
        span: dummy_span(),
        symbol: main_symbol,
        name: "main".to_string(),
        signature: HirFunctionSignature {
            params: vec![],
            return_type: HirType::Primitive(HirPrimitiveType::F64),
            is_effectful: false,
        },
        body: Some(
            HirExpr {
                span: dummy_span(),
                kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Const(
                    HirLiteral::FloatLiteral {
                        value: 3.14,
                        ty: HirPrimitiveType::F64,
                    }
                ))),
                ty: HirType::Primitive(HirPrimitiveType::F64),
            }
        ),
    };
    let hir_module = create_test_hir_module_single_func(main_func, vec![]);
    let (descriptor_store_box, adt_map, prim_map, tuple_map, array_map) = prepare_for_lower_module(&hir_module).expect("Layout setup failed");
    let mir_module = lower_module(&hir_module, descriptor_store_box, adt_map, prim_map, tuple_map, array_map)?;
    let main_graph = mir_module.functions.get(&main_symbol).expect("Graph missing");
    let (ret_id, ret_port) = main_graph.return_port.expect("Return port missing");
    assert_eq!(ret_port, PortIndex(0));
    match main_graph.nodes.get(&ret_id) {
        Some(MirNode::Constant { value: HirLiteral::FloatLiteral { value: v, ..}, ty }) => {
            assert_eq!(*v, 3.14);
            assert_eq!(ty, &MirType::Primitive(ResolvePrimitiveType::F64));
        }
        other => panic!("Expected F64 Constant node, got {:?}", other)
    }
    Ok(())
}

#[test]
fn test_lower_return_bool_literal() -> Result<(), LoweringError> {
    let main_symbol = Symbol::fresh();
    let main_func = HirFunction {
        span: dummy_span(),
        symbol: main_symbol,
        name: "main".to_string(),
        signature: HirFunctionSignature {
            params: vec![],
            return_type: HirType::Primitive(HirPrimitiveType::Bool),
            is_effectful: false,
        },
        body: Some(
            HirExpr {
                span: dummy_span(),
                kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Const(
                    HirLiteral::BoolLiteral(true)
                ))),
                ty: HirType::Primitive(HirPrimitiveType::Bool),
            }
        ),
    };
    let hir_module = create_test_hir_module_single_func(main_func, vec![]);
    let (descriptor_store_box, adt_map, prim_map, tuple_map, array_map) = prepare_for_lower_module(&hir_module).expect("Layout setup failed");
    let mir_module = lower_module(&hir_module, descriptor_store_box, adt_map, prim_map, tuple_map, array_map)?;
    let main_graph = mir_module.functions.get(&main_symbol).expect("Graph missing");
    let (ret_id, _) = main_graph.return_port.expect("Return port missing");
    match main_graph.nodes.get(&ret_id) {
        Some(MirNode::Constant { value: HirLiteral::BoolLiteral(v), ty }) => {
            assert_eq!(*v, true);
            assert_eq!(ty, &MirType::Primitive(ResolvePrimitiveType::Bool));
        }
        other => panic!("Expected Bool Constant node, got {:?}", other)
    }
    Ok(())
}

#[test]
fn test_lower_return_char_literal() -> Result<(), LoweringError> {
    let main_symbol = Symbol::fresh();
    let main_func = HirFunction {
        span: dummy_span(),
        symbol: main_symbol,
        name: "main".to_string(),
        signature: HirFunctionSignature {
            params: vec![],
            return_type: HirType::Primitive(HirPrimitiveType::Char),
            is_effectful: false,
        },
        body: Some(
            HirExpr {
                span: dummy_span(),
                kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Const(
                    HirLiteral::CharLiteral('ü')
                ))),
                ty: HirType::Primitive(HirPrimitiveType::Char),
            }
        ),
    };
    let hir_module = create_test_hir_module_single_func(main_func, vec![]);
    let (descriptor_store_box, adt_map, prim_map, tuple_map, array_map) = prepare_for_lower_module(&hir_module).expect("Layout setup failed");
    let mir_module = lower_module(&hir_module, descriptor_store_box, adt_map, prim_map, tuple_map, array_map)?;
    let main_graph = mir_module.functions.get(&main_symbol).expect("Graph missing");
    let (ret_id, _) = main_graph.return_port.expect("Return port missing");
    match main_graph.nodes.get(&ret_id) {
        Some(MirNode::Constant { value: HirLiteral::CharLiteral(v), ty }) => {
            assert_eq!(*v, 'ü');
            assert_eq!(ty, &MirType::Primitive(ResolvePrimitiveType::Char));
        }
        other => panic!("Expected Char Constant node, got {:?}", other)
    }
    Ok(())
}

#[test]
fn test_lower_return_unit_literal() -> Result<(), LoweringError> {
    let main_symbol = Symbol::fresh();
    let main_func = HirFunction {
        span: dummy_span(),
        symbol: main_symbol,
        name: "main".to_string(),
        signature: HirFunctionSignature {
            params: vec![],
            return_type: HirType::Tuple(vec![]), // Unit represented as empty tuple type
            is_effectful: false,
        },
        body: Some(
            HirExpr {
                span: dummy_span(),
                kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Const(
                    HirLiteral::Unit
                ))),
                ty: HirType::Tuple(vec![]),
            }
        ),
    };
    let hir_module = create_test_hir_module_single_func(main_func, vec![]);
    let (descriptor_store_box, adt_map, prim_map, tuple_map, array_map) = prepare_for_lower_module(&hir_module).expect("Layout setup failed");
    let mir_module = lower_module(&hir_module, descriptor_store_box, adt_map, prim_map, tuple_map, array_map)?;
    let main_graph = mir_module.functions.get(&main_symbol).expect("Graph missing");
    let (ret_id, _) = main_graph.return_port.expect("Return port missing");
    match main_graph.nodes.get(&ret_id) {
        // Unit literal lowers to a Constant node holding the Unit literal
        Some(MirNode::Constant { value: HirLiteral::Unit, ty }) => {
            assert_eq!(ty, &MirType::Tuple(vec![]));
        }
        other => panic!("Expected Unit Constant node, got {:?}", other)
    }
    Ok(())
} 