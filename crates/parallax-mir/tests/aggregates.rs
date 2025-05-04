use parallax_mir::{lower_module, MirGraph, MirModule, MirNode, MirType, PortIndex, NodeId, LoweringError};
use parallax_hir::hir::{
    HirModule, HirFunction, HirFunctionSignature, HirExpr, HirExprKind, HirTailExpr,
    HirLiteral, HirType, PrimitiveType as HirPrimitiveType, Operand, HirVar, HirValue,
    AggregateKind, HirStructDef, HirEnumDef
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
fn test_lower_tuple_construction() -> Result<(), LoweringError> {
    // Define HIR for: fn make_pair() -> (i32, bool) { let tmp = (10, true); tmp }
    let func_symbol = Symbol::fresh();
    let tmp_var = parallax_hir::hir::HirVar(0); // Variable for the tuple
    let tuple_ty = HirType::Tuple(vec![
        HirType::Primitive(HirPrimitiveType::I32),
        HirType::Primitive(HirPrimitiveType::Bool),
    ]);
    let mir_tuple_ty = MirType::Tuple(vec![
        MirType::Primitive(ResolvePrimitiveType::I32),
        MirType::Primitive(ResolvePrimitiveType::Bool),
    ]);

    let func = HirFunction {
        span: dummy_span(),
        symbol: func_symbol,
        name: "make_pair".to_string(),
        signature: HirFunctionSignature {
            params: vec![],
            return_type: tuple_ty.clone(),
            is_effectful: false,
        },
        body: Some(
            HirExpr {
                span: dummy_span(),
                kind: HirExprKind::Let {
                    var: tmp_var,
                    var_ty: tuple_ty.clone(),
                    value: Box::new(HirValue::Aggregate {
                        kind: AggregateKind::Tuple,
                        fields: vec![
                            Operand::Const(HirLiteral::IntLiteral { value: 10, ty: HirPrimitiveType::I32 }),
                            Operand::Const(HirLiteral::BoolLiteral(true)),
                        ],
                    }),
                    rest: Box::new(HirExpr {
                        span: dummy_span(),
                        kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(tmp_var))),
                        ty: tuple_ty.clone(),
                    }),
                },
                ty: tuple_ty.clone(),
            }
        ),
    };

    let hir_module = create_test_hir_module_single_func(func, vec![]);
    let (descriptor_store_box, adt_map, prim_map, tuple_map, array_map) = prepare_for_lower_module(&hir_module).expect("Layout setup failed");
    let mir_module = lower_module(&hir_module, descriptor_store_box, adt_map, prim_map, tuple_map, array_map)?;

    // --- Assertions ---
    let graph = mir_module.functions.get(&func_symbol).unwrap();

    // 1. Find the two Constant nodes
    let mut const_int_id = None;
    let mut const_bool_id = None;
    for (id, node) in &graph.nodes {
        match node {
            MirNode::Constant { value: HirLiteral::IntLiteral { value: v, .. }, ty } if *v == 10 && ty == &MirType::Primitive(ResolvePrimitiveType::I32) => {
                const_int_id = Some(*id);
            }
            MirNode::Constant { value: HirLiteral::BoolLiteral(b), ty } if *b && ty == &MirType::Primitive(ResolvePrimitiveType::Bool) => {
                const_bool_id = Some(*id);
            }
            _ => {}
        }
    }
    assert!(const_int_id.is_some(), "Constant(10) not found");
    assert!(const_bool_id.is_some(), "Constant(true) not found");
    let const_int_id = const_int_id.unwrap();
    let const_bool_id = const_bool_id.unwrap();

    // 2. Find the Constructor node for the tuple
    let mut constructor_node_id = None;
    for (id, node) in &graph.nodes {
        if let MirNode::Constructor { tag, field_types, ty } = node {
            if tag.id() == 0 && field_types.len() == 2 && ty == &mir_tuple_ty {
                constructor_node_id = Some(*id);
                break;
            }
        }
    }
    assert!(constructor_node_id.is_some(), "Tuple Constructor node not found");
    let constructor_node_id = constructor_node_id.unwrap();

    // 3. Check edges from Constants to Constructor
    let mut found_edge_int = false;
    let mut found_edge_bool = false;
    for edge in &graph.edges {
        if edge.to_node == constructor_node_id {
            if edge.from_node == const_int_id && edge.to_port == PortIndex(0) { found_edge_int = true; }
            if edge.from_node == const_bool_id && edge.to_port == PortIndex(1) { found_edge_bool = true; }
        }
    }
    assert!(found_edge_int, "Edge from Constant(10) to Constructor:0 not found");
    assert!(found_edge_bool, "Edge from Constant(true) to Constructor:1 not found");

    // 4. Check return port points to Constructor output
    let (return_node_id, return_port_index) = graph.return_port.expect("Return port missing");
    assert_eq!(return_node_id, constructor_node_id, "Return node should be Constructor");
    assert_eq!(return_port_index, PortIndex(0));

    // 5. Check total nodes (Param, Const, Const, Constructor)
    assert_eq!(graph.nodes.len(), 4, "Expected 4 nodes");
    // 6. Check total edges (Const->Ctor, Const->Ctor)
    assert_eq!(graph.edges.len(), 2, "Expected 2 edges");

    Ok(())
}

#[test]
fn test_lower_struct_construction() -> Result<(), LoweringError> {
    // Define HIR for: fn make_point() -> Point = Point { x: 5, y: -5 };
    let func_symbol = Symbol::fresh();
    let (point_struct, struct_symbol, _field_x_symbol, _field_y_symbol) = define_point_struct();
    let struct_ty = HirType::Adt(struct_symbol);
    let mir_struct_ty = MirType::Adt(struct_symbol);
    let mir_i64_ty = MirType::Primitive(ResolvePrimitiveType::I64);

    let tmp_var = parallax_hir::hir::HirVar(0);

    let func = HirFunction {
        span: dummy_span(),
        symbol: func_symbol,
        name: "make_point".to_string(),
        signature: HirFunctionSignature {
            params: vec![],
            return_type: struct_ty.clone(),
            is_effectful: false,
        },
        body: Some(
            HirExpr {
                span: dummy_span(),
                kind: HirExprKind::Let {
                     var: tmp_var,
                     var_ty: struct_ty.clone(),
                     value: Box::new(HirValue::Aggregate {
                        kind: AggregateKind::Struct(struct_symbol),
                        fields: vec![
                            Operand::Const(HirLiteral::IntLiteral { value: 5, ty: HirPrimitiveType::I64 }),
                            Operand::Const(HirLiteral::IntLiteral { value: -5, ty: HirPrimitiveType::I64 }),
                        ],
                     }),
                     rest: Box::new(HirExpr {
                         span: dummy_span(),
                         kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(tmp_var))),
                         ty: struct_ty.clone(),
                     }),
                },
                ty: struct_ty.clone(),
            }
        ),
    };

    let hir_module = create_test_hir_module_full(vec![func], vec![point_struct], vec![], vec![], Some(func_symbol), vec![]);
    let (descriptor_store_box, adt_map, prim_map, tuple_map, array_map) = prepare_for_lower_module(&hir_module).expect("Layout setup failed");
    let mir_module = lower_module(&hir_module, descriptor_store_box, adt_map, prim_map, tuple_map, array_map)?;

    // --- Assertions ---
    let graph = mir_module.functions.get(&func_symbol).unwrap();

    // 1. Check MIR struct def exists
    assert!(mir_module.structs.iter().any(|s| s.symbol == struct_symbol), "Point struct def missing");

    // 2. Find Constant nodes
    let const_5_id = graph.nodes.iter().find_map(|(id, n)| if let MirNode::Constant { value: HirLiteral::IntLiteral { value: 5, .. }, ..} = n { Some(*id) } else { None }).expect("Const 5 missing");
    let const_neg_5_id = graph.nodes.iter().find_map(|(id, n)| if let MirNode::Constant { value: HirLiteral::IntLiteral { value: -5, .. }, ..} = n { Some(*id) } else { None }).expect("Const -5 missing");

    // 3. Find Constructor node for the struct
    let constructor_id = graph.nodes.iter().find_map(|(id, n)| {
        if let MirNode::Constructor { tag, field_types, ty } = n {
            if *tag == struct_symbol && field_types.len() == 2 && ty == &mir_struct_ty {
                 assert_eq!(field_types[0], mir_i64_ty);
                 assert_eq!(field_types[1], mir_i64_ty);
                 Some(*id)
            } else { None }
        } else { None }
    }).expect("Struct Constructor missing");

    // 4. Check edges from Constants to Constructor
    assert!(graph.edges.iter().any(|e| e.from_node == const_5_id && e.to_node == constructor_id && e.to_port == PortIndex(0)), "Edge Const(5)->Ctor:0 missing");
    assert!(graph.edges.iter().any(|e| e.from_node == const_neg_5_id && e.to_node == constructor_id && e.to_port == PortIndex(1)), "Edge Const(-5)->Ctor:1 missing");

    // 5. Check return port points to Constructor output
    let (ret_id, ret_port) = graph.return_port.expect("Return port missing");
    assert_eq!(ret_id, constructor_id, "Return node should be Constructor");
    assert_eq!(ret_port, PortIndex(0));

    // 6. Node count (Param, Const, Const, Constructor)
    assert_eq!(graph.nodes.len(), 4);
    // 7. Edge count (Const->Ctor, Const->Ctor)
    assert_eq!(graph.edges.len(), 2);

    Ok(())
}

// TODO: Add tests for enum variant construction. 