//! Tests for aggregate type (tuple, struct, array) lowering.

use super::helpers::*;
// Add necessary imports used directly in tests
use crate::lowering::LoweringError;
use crate::mir::{MirNode, MirType, MirEdge, PortIndex};
use parallax_hir::hir::{ HirFunction, HirFunctionSignature, HirExpr, HirExprKind, HirTailExpr, HirValue, Operand, HirLiteral, HirVar, AggregateKind, ProjectionKind, HirType, PrimitiveType};
use parallax_resolve::types::{PrimitiveType as ResolvePrimitiveType, Symbol};
use std::sync::Arc; // Needed for Arc in MirType::FunctionPointer/Array

#[test]
fn test_lower_tuple_construct_and_project() -> Result<(), LoweringError> {
    let func_symbol = Symbol::new(0);
    let hir_i32_ty = PrimitiveType::I32;
    let hir_bool_ty = PrimitiveType::Bool;
    let i32_ty = HirType::Primitive(hir_i32_ty);
    let bool_ty = HirType::Primitive(hir_bool_ty);
    let tuple_ty = HirType::Tuple(vec![i32_ty.clone(), bool_ty.clone()]);

    let const_42 = HirLiteral::IntLiteral { value: 42, ty: hir_i32_ty };
    let const_true = HirLiteral::BoolLiteral(true);
    let var_tup = HirVar(0);
    let var_proj = HirVar(1);

    // fn test_tuple() -> bool {
    //   let tup = (42, true);
    //   let proj = tup.1;
    //   proj
    // }
    let func_def = HirFunction {
        symbol: func_symbol,
        name: "test_tuple".to_string(),
        signature: HirFunctionSignature {
            params: vec![],
            return_type: bool_ty.clone(),
            is_effectful: false,
        },
        body: Some(HirExpr {
            kind: HirExprKind::Let {
                var: var_tup,
                var_ty: tuple_ty.clone(),
                value: Box::new(HirValue::Aggregate {
                    kind: AggregateKind::Tuple,
                    fields: vec![
                        Operand::Const(const_42.clone()),
                        Operand::Const(const_true),
                    ],
                }),
                rest: Box::new(HirExpr {
                    kind: HirExprKind::Let {
                        var: var_proj,
                        var_ty: bool_ty.clone(),
                        value: Box::new(HirValue::Project {
                            base: Operand::Var(var_tup),
                            projection: ProjectionKind::TupleIndex(1),
                        }),
                        rest: Box::new(HirExpr {
                            kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(var_proj))),
                            ty: bool_ty.clone(),
                            span: dummy_span(),
                        }),
                    },
                    ty: bool_ty.clone(),
                    span: dummy_span(),
                }),
            },
            ty: bool_ty.clone(),
            span: dummy_span(),
        }),
        span: dummy_span(),
    };

    let hir_module = create_test_module(func_def.clone());
    let mir_graph = test_lower_function(&hir_module, &func_def)?;

    // --- Assertions ---
    assert!(mir_graph.parameter_node.is_some());
    assert!(mir_graph.return_port.is_some());

    // Expecting: Param, Const(42), Const(true), Constructor(tuple), Project(bool)
    assert_eq!(mir_graph.nodes.len(), 5);

    let mut const_42_id = None;
    let mut const_true_id = None;
    let mut constructor_id = None;
    let mut project_id = None;
    let mut param_id = None;

    for (id, node) in &mir_graph.nodes {
        match node {
            MirNode::Constant { value: HirLiteral::IntLiteral { value: 42, .. }, .. } => const_42_id = Some(*id),
            MirNode::Constant { value: HirLiteral::BoolLiteral(true), .. } => const_true_id = Some(*id),
            MirNode::Constructor { tag: Symbol(0), field_types, ty } // Tuple tag is 0
                if field_types.len() == 2 &&
                   field_types[0] == MirType::Primitive(ResolvePrimitiveType::I32) &&
                   field_types[1] == MirType::Primitive(ResolvePrimitiveType::Bool) &&
                   *ty == MirType::Tuple(vec![MirType::Primitive(ResolvePrimitiveType::I32), MirType::Primitive(ResolvePrimitiveType::Bool)]) => constructor_id = Some(*id),
            MirNode::Project { field_index: 1, aggregate_ty, field_ty }
                if *aggregate_ty == MirType::Tuple(vec![MirType::Primitive(ResolvePrimitiveType::I32), MirType::Primitive(ResolvePrimitiveType::Bool)]) &&
                   *field_ty == MirType::Primitive(ResolvePrimitiveType::Bool) => project_id = Some(*id),
            MirNode::Parameter { .. } => param_id = Some(*id),
            _ => panic!("Unexpected node: {:?}", node),
        }
    }

    let const_42_id = const_42_id.expect("Const 42 node not found");
    let const_true_id = const_true_id.expect("Const true node not found");
    let constructor_id = constructor_id.expect("Constructor node not found");
    let project_id = project_id.expect("Project node not found");
    let _param_id = param_id.expect("Parameter node not found"); // Use if needed

    // Check return port points to Project node
    assert_eq!(mir_graph.return_port.unwrap().0, project_id);

    // Check edges
    assert_eq!(mir_graph.edges.len(), 3, "Expected 3 edges");
    // Const(42) -> Constructor (input 0)
    assert!(mir_graph.edges.contains(&MirEdge { from_node: const_42_id, from_port: PortIndex(0), to_node: constructor_id, to_port: PortIndex(0) }));
    // Const(true) -> Constructor (input 1)
    assert!(mir_graph.edges.contains(&MirEdge { from_node: const_true_id, from_port: PortIndex(0), to_node: constructor_id, to_port: PortIndex(1) }));
    // Constructor -> Project (input 0)
    assert!(mir_graph.edges.contains(&MirEdge { from_node: constructor_id, from_port: PortIndex(0), to_node: project_id, to_port: PortIndex(0) }));

    Ok(())
}

#[test]
fn test_lower_struct_construct_and_project() -> Result<(), LoweringError> {
    let func_symbol = Symbol::new(0);
    let point_struct = create_point_struct_def();
    let point_sym = point_struct.symbol;
    let x_sym = point_struct.fields[0].0;
    let _y_sym = point_struct.fields[1].0;
    let point_ty = HirType::Adt(point_sym);
    let hir_i32_ty = PrimitiveType::I32;
    let i32_ty = HirType::Primitive(hir_i32_ty);

    let const_5 = HirLiteral::IntLiteral { value: 5, ty: hir_i32_ty };
    let const_neg_2 = HirLiteral::IntLiteral { value: -2, ty: hir_i32_ty };
    let var_p = HirVar(0);
    let var_x_val = HirVar(1);

    // fn test_struct() -> i32 {
    //   let p = Point { x: 5, y: -2 };
    //   let x_val = p.x;
    //   x_val
    // }
    let func_def = HirFunction {
        symbol: func_symbol,
        name: "test_struct".to_string(),
        signature: HirFunctionSignature {
            params: vec![],
            return_type: i32_ty.clone(),
            is_effectful: false,
        },
        body: Some(HirExpr {
            kind: HirExprKind::Let {
                var: var_p,
                var_ty: point_ty.clone(),
                value: Box::new(HirValue::Aggregate {
                    kind: AggregateKind::Struct(point_sym),
                    fields: vec![
                        Operand::Const(const_5.clone()),
                        Operand::Const(const_neg_2.clone()),
                    ],
                }),
                rest: Box::new(HirExpr {
                    kind: HirExprKind::Let {
                        var: var_x_val,
                        var_ty: i32_ty.clone(),
                        value: Box::new(HirValue::Project {
                            base: Operand::Var(var_p),
                            projection: ProjectionKind::Field(x_sym),
                        }),
                        rest: Box::new(HirExpr {
                            kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(var_x_val))),
                            ty: i32_ty.clone(),
                            span: dummy_span(),
                        }),
                    },
                    ty: i32_ty.clone(),
                    span: dummy_span(),
                }),
            },
            ty: i32_ty.clone(),
            span: dummy_span(),
        }),
        span: dummy_span(),
    };

    let mut hir_module = create_test_module(func_def.clone());
    hir_module.structs.push(point_struct); // Add struct def to module

    let mir_graph = test_lower_function(&hir_module, &func_def)?;

    // --- Assertions ---
    assert!(mir_graph.parameter_node.is_some());
    assert!(mir_graph.return_port.is_some());

    // Expecting: Param, Const(5), Const(-2), Constructor(Point), Project(x)
    assert_eq!(mir_graph.nodes.len(), 5);

    let mut const_5_id = None;
    let mut const_neg_2_id = None;
    let mut constructor_id = None;
    let mut project_id = None;
    let mut param_id = None;

    for (id, node) in &mir_graph.nodes {
        match node {
            MirNode::Constant { value: HirLiteral::IntLiteral { value: 5, .. }, .. } => const_5_id = Some(*id),
            MirNode::Constant { value: HirLiteral::IntLiteral { value: -2, .. }, .. } => const_neg_2_id = Some(*id),
            MirNode::Constructor { tag, field_types, ty } if *tag == point_sym => {
                assert_eq!(field_types.len(), 2);
                assert_eq!(field_types[0], MirType::Primitive(ResolvePrimitiveType::I32));
                assert_eq!(field_types[1], MirType::Primitive(ResolvePrimitiveType::I32));
                assert_eq!(*ty, MirType::Adt(point_sym));
                constructor_id = Some(*id);
            }
            MirNode::Project { field_index: 0, aggregate_ty, field_ty } // x is field 0
                if *aggregate_ty == MirType::Adt(point_sym) &&
                   *field_ty == MirType::Primitive(ResolvePrimitiveType::I32) => project_id = Some(*id),
            MirNode::Parameter { .. } => param_id = Some(*id),
            _ => panic!("Unexpected node: {:?}", node),
        }
    }

    let const_5_id = const_5_id.expect("Const 5 node not found");
    let const_neg_2_id = const_neg_2_id.expect("Const -2 node not found");
    let constructor_id = constructor_id.expect("Constructor node not found");
    let project_id = project_id.expect("Project node not found");
    let _param_id = param_id.expect("Parameter node not found");

    // Check return port points to Project node
    assert_eq!(mir_graph.return_port.unwrap().0, project_id);

    // Check edges
    assert_eq!(mir_graph.edges.len(), 3, "Expected 3 edges");
    // Const(5) -> Constructor (input 0 - field x)
    assert!(mir_graph.edges.contains(&MirEdge { from_node: const_5_id, from_port: PortIndex(0), to_node: constructor_id, to_port: PortIndex(0) }));
    // Const(-2) -> Constructor (input 1 - field y)
    assert!(mir_graph.edges.contains(&MirEdge { from_node: const_neg_2_id, from_port: PortIndex(0), to_node: constructor_id, to_port: PortIndex(1) }));
    // Constructor -> Project (input 0)
    assert!(mir_graph.edges.contains(&MirEdge { from_node: constructor_id, from_port: PortIndex(0), to_node: project_id, to_port: PortIndex(0) }));

    Ok(())
}

#[test]
fn test_lower_array_construct_and_project() -> Result<(), LoweringError> {
    let func_symbol = Symbol::new(0);
    let hir_i32_ty = PrimitiveType::I32;
    let i32_ty = HirType::Primitive(hir_i32_ty);
    let array_ty = HirType::Array(Arc::new(i32_ty.clone()), 3); // Array of 3 i32s

    let const_1 = HirLiteral::IntLiteral { value: 1, ty: hir_i32_ty };
    let const_2 = HirLiteral::IntLiteral { value: 2, ty: hir_i32_ty };
    let const_3 = HirLiteral::IntLiteral { value: 3, ty: hir_i32_ty };
    let const_idx_1 = HirLiteral::IntLiteral { value: 1, ty: hir_i32_ty }; // Index to project

    let var_arr = HirVar(0);
    let var_proj = HirVar(1);

    // fn test_array() -> i32 {
    //   let arr = [1, 2, 3];
    //   let proj = arr[1]; // Should be 2
    //   proj
    // }
    let func_def = HirFunction {
        symbol: func_symbol,
        name: "test_array".to_string(),
        signature: HirFunctionSignature {
            params: vec![],
            return_type: i32_ty.clone(),
            is_effectful: false,
        },
        body: Some(HirExpr {
            kind: HirExprKind::Let {
                var: var_arr,
                var_ty: array_ty.clone(),
                value: Box::new(HirValue::Aggregate {
                    kind: AggregateKind::Array,
                    fields: vec![
                        Operand::Const(const_1.clone()),
                        Operand::Const(const_2.clone()),
                        Operand::Const(const_3.clone()),
                    ],
                }),
                rest: Box::new(HirExpr {
                    kind: HirExprKind::Let {
                        var: var_proj,
                        var_ty: i32_ty.clone(),
                        value: Box::new(HirValue::Project {
                            base: Operand::Var(var_arr),
                            projection: ProjectionKind::ArrayIndex(Operand::Const(const_idx_1.clone())),
                        }),
                        rest: Box::new(HirExpr {
                            kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(var_proj))),
                            ty: i32_ty.clone(),
                            span: dummy_span(),
                        }),
                    },
                    ty: i32_ty.clone(),
                    span: dummy_span(),
                }),
            },
            ty: i32_ty.clone(),
            span: dummy_span(),
        }),
        span: dummy_span(),
    };

    let hir_module = create_test_module(func_def.clone());
    let mir_graph = test_lower_function(&hir_module, &func_def)?;

    // --- Assertions ---
    assert!(mir_graph.parameter_node.is_some());
    assert!(mir_graph.return_port.is_some());

    // Expecting: Param, Const(1), Const(2), Const(3), Const(idx=1),
    //            ArrayConstruct, ArrayProject
    assert_eq!(mir_graph.nodes.len(), 7);

    let mut const_1_id = None;
    let mut const_2_id = None;
    let mut const_3_id = None;
    let mut const_idx_1_id = None;
    let mut array_construct_id = None;
    let mut array_project_id = None;
    let mut param_id = None;

    let expected_mir_array_ty = MirType::Array(Arc::new(MirType::Primitive(ResolvePrimitiveType::I32)), 3);
    let expected_mir_i32_ty = MirType::Primitive(ResolvePrimitiveType::I32);

    for (id, node) in &mir_graph.nodes {
        match node {
            MirNode::Constant { value: HirLiteral::IntLiteral { value: 1, .. }, ty } if *ty == expected_mir_i32_ty => {
                 // Check if it's the element or the index
                 // This relies on node creation order, which isn't ideal for robust tests
                 // but sufficient for now. A better way might be to check edge destinations.
                 if const_1_id.is_none() { const_1_id = Some(*id); } else { const_idx_1_id = Some(*id); }
            }
            MirNode::Constant { value: HirLiteral::IntLiteral { value: 2, .. }, .. } => const_2_id = Some(*id),
            MirNode::Constant { value: HirLiteral::IntLiteral { value: 3, .. }, .. } => const_3_id = Some(*id),
            MirNode::ArrayConstruct { element_ty, size } if *element_ty == expected_mir_i32_ty && *size == 3 => array_construct_id = Some(*id),
            MirNode::ArrayProject { array_ty, index_ty, element_ty } if *array_ty == expected_mir_array_ty && matches!(index_ty, MirType::Primitive(ResolvePrimitiveType::I32)) && *element_ty == expected_mir_i32_ty => array_project_id = Some(*id),
            MirNode::Parameter { .. } => param_id = Some(*id),
            _ => panic!("Unexpected node: {:?}", node),
        }
    }

    let const_1_id = const_1_id.expect("Const 1 node not found");
    let const_2_id = const_2_id.expect("Const 2 node not found");
    let const_3_id = const_3_id.expect("Const 3 node not found");
    let const_idx_1_id = const_idx_1_id.expect("Const index 1 node not found");
    let array_construct_id = array_construct_id.expect("ArrayConstruct node not found");
    let array_project_id = array_project_id.expect("ArrayProject node not found");
    let _param_id = param_id.expect("Parameter node not found");

    // Check return port points to ArrayProject node
    assert_eq!(mir_graph.return_port.unwrap().0, array_project_id);

    // Check edges
    assert_eq!(mir_graph.edges.len(), 5, "Expected 5 edges");
    // Const elements -> ArrayConstruct
    assert!(mir_graph.edges.contains(&MirEdge { from_node: const_1_id, from_port: PortIndex(0), to_node: array_construct_id, to_port: PortIndex(0) }));
    assert!(mir_graph.edges.contains(&MirEdge { from_node: const_2_id, from_port: PortIndex(0), to_node: array_construct_id, to_port: PortIndex(1) }));
    assert!(mir_graph.edges.contains(&MirEdge { from_node: const_3_id, from_port: PortIndex(0), to_node: array_construct_id, to_port: PortIndex(2) }));
    // ArrayConstruct -> ArrayProject (input 0 - array)
    assert!(mir_graph.edges.contains(&MirEdge { from_node: array_construct_id, from_port: PortIndex(0), to_node: array_project_id, to_port: PortIndex(0) }));
    // Const(idx=1) -> ArrayProject (input 1 - index)
    assert!(mir_graph.edges.contains(&MirEdge { from_node: const_idx_1_id, from_port: PortIndex(0), to_node: array_project_id, to_port: PortIndex(1) }));

    Ok(())
} 