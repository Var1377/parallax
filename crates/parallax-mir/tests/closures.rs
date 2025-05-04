use parallax_mir::{lower_module, MirGraph, MirModule, MirNode, MirType, PortIndex, NodeId, LoweringError};
use parallax_hir::hir::{
    HirModule, HirFunction, HirFunctionSignature, HirExpr, HirExprKind, HirTailExpr,
    HirLiteral, HirType, PrimitiveType as HirPrimitiveType, Operand, HirVar, HirValue,
    AggregateKind, HirStructDef
};
use parallax_resolve::types::{Symbol, PrimitiveType as ResolvePrimitiveType};
use miette::{SourceOffset, SourceSpan};
use std::sync::Arc;
use std::collections::HashMap;

// Import helper from the module level
mod common;
use common::prepare_for_lower_module;

// --- Test Helpers (Copied) ---

fn dummy_span() -> SourceSpan {
    SourceSpan::new(SourceOffset::from(0), 0)
}

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
fn test_lower_captureless_closure() -> Result<(), LoweringError> {
    // Define HIR for:
    // fn lambda_body(a: i32, b: i32) -> i32 = 99;
    // fn make_closure() -> fn(i32, i32)->i32 { let f = |a,b| lambda_body(a,b); f }

    let lambda_body_symbol = Symbol::fresh();
    let make_closure_symbol = Symbol::fresh();
    let closure_var = parallax_hir::hir::HirVar(0);
    let param_a_var = parallax_hir::hir::HirVar(1); // Var for lambda param a
    let param_b_var = parallax_hir::hir::HirVar(2); // Var for lambda param b

    let int_ty = HirType::Primitive(HirPrimitiveType::I32);
    let mir_int_ty = MirType::Primitive(ResolvePrimitiveType::I32);
    let fn_ptr_ty = HirType::FunctionPointer(
        vec![int_ty.clone(), int_ty.clone()],
        Arc::new(int_ty.clone()),
    );
    let mir_empty_tuple_ty = MirType::Tuple(vec![]);

    // 1. Define the lambda body function (just returns 99)
    let lambda_body_func = HirFunction {
        span: dummy_span(),
        symbol: lambda_body_symbol,
        name: "lambda_body".to_string(),
        signature: HirFunctionSignature {
            params: vec![(param_a_var, int_ty.clone()), (param_b_var, int_ty.clone())],
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

    // 2. Define the function that creates the closure
    let make_closure_func = HirFunction {
        span: dummy_span(),
        symbol: make_closure_symbol,
        name: "make_closure".to_string(),
        signature: HirFunctionSignature {
            params: vec![],
            return_type: fn_ptr_ty.clone(),
            is_effectful: false,
        },
        body: Some(
            HirExpr {
                span: dummy_span(),
                kind: HirExprKind::Let {
                    var: closure_var,
                    var_ty: fn_ptr_ty.clone(),
                    value: Box::new(HirValue::Closure {
                        function_symbol: lambda_body_symbol,
                        captures: vec![], // No captures
                    }),
                    rest: Box::new(HirExpr {
                        span: dummy_span(),
                        kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(closure_var))),
                        ty: fn_ptr_ty.clone(),
                    }),
                },
                ty: fn_ptr_ty.clone(),
            }
        ),
    };

    let hir_module = create_test_hir_module_multi(
        vec![lambda_body_func, make_closure_func],
        vec![],
        Some(make_closure_symbol),
        vec![]
    );
    // Destructure all maps
    let (descriptor_store_box, adt_map, prim_map, tuple_map, array_map) = prepare_for_lower_module(&hir_module).expect("Layout setup failed");
    // Pass references
    let mir_module = lower_module(&hir_module, descriptor_store_box, adt_map, prim_map, tuple_map, array_map)?;

    // --- Assertions ---

    // A. Check that two function graphs were created
    assert_eq!(mir_module.functions.len(), 2, "Expected two MIR function graphs");

    // B. Assertions on the make_closure graph
    let make_closure_graph = mir_module.functions.get(&make_closure_symbol).unwrap();
    let mut specialized_lambda_symbol_opt = None;
    let mut closure_node_id_opt = None;
    let mut closure_env_ty_opt = None;
    let mut closure_fptr_ty_opt = None;

    // B.1 Find the Closure node and the specialized symbol
    for (id, node) in &make_closure_graph.nodes {
        if let MirNode::Closure {
            original_lambda_symbol,
            specialized_function_symbol,
            capture_types,
            env_ty,
            func_ptr_ty,
        } = node {
            if *original_lambda_symbol == lambda_body_symbol {
                closure_node_id_opt = Some(*id);
                specialized_lambda_symbol_opt = Some(*specialized_function_symbol);
                assert!(capture_types.is_empty(), "Capture types should be empty");
                closure_env_ty_opt = Some(env_ty.clone());
                closure_fptr_ty_opt = Some(func_ptr_ty.clone());
                break;
            }
        }
    }
    assert!(closure_node_id_opt.is_some(), "Closure node not found");
    let closure_node_id = closure_node_id_opt.unwrap();
    let specialized_lambda_symbol = specialized_lambda_symbol_opt.unwrap();

    // B.2 Check the types produced by the Closure node
    assert_eq!(closure_env_ty_opt.unwrap(), mir_empty_tuple_ty, "Closure env ty mismatch");
    // Expected fptr: points to specialized graph, takes env + original params
    let expected_specialized_fptr_ty = MirType::FunctionPointer(
        vec![mir_empty_tuple_ty.clone(), mir_int_ty.clone(), mir_int_ty.clone()],
        Arc::new(mir_int_ty.clone()),
    );
    assert_eq!(closure_fptr_ty_opt.unwrap(), expected_specialized_fptr_ty, "Closure fptr ty mismatch");

    // B.3 Check return port points to the Closure node's function pointer output (Port 1)
    let (return_node_id, return_port_index) = make_closure_graph.return_port.expect("Return port missing");
    assert_eq!(return_node_id, closure_node_id, "Return node should be Closure");
    assert_eq!(return_port_index, PortIndex(1), "Return port index should be 1 (func ptr)");

    // C. Check the specialized lambda graph exists and is correct
    assert!(mir_module.functions.contains_key(&specialized_lambda_symbol), "Specialized lambda graph not found");
    let specialized_graph = mir_module.functions.get(&specialized_lambda_symbol).unwrap();

    // C.1 Check specialized graph's parameters (should include env, then original a, b)
    let spec_param_node_id = specialized_graph.parameter_node.expect("Specialized param node missing");
    let spec_param_node = specialized_graph.nodes.get(&spec_param_node_id).unwrap();
    let expected_spec_agg_param_ty = MirType::Tuple(vec![mir_empty_tuple_ty, mir_int_ty.clone(), mir_int_ty.clone()]);
     match spec_param_node {
         MirNode::Parameter { ty, .. } => assert_eq!(ty, &expected_spec_agg_param_ty),
         _ => panic!("Expected parameter node type")
     }

    // C.2 Check specialized graph's body (should just return constant 99)
    let mut const_99_id = None;
     for (id, node) in &specialized_graph.nodes {
         if let MirNode::Constant { value: HirLiteral::IntLiteral { value: 99, .. }, .. } = node {
             const_99_id = Some(*id);
             break;
         }
     }
    assert!(const_99_id.is_some(), "Constant(99) not found in specialized graph");
    let (spec_ret_id, spec_ret_port) = specialized_graph.return_port.expect("Specialized return port missing");
    assert_eq!(spec_ret_id, const_99_id.unwrap(), "Specialized graph should return Constant(99)");
    assert_eq!(spec_ret_port, PortIndex(0));

    Ok(())
}

#[test]
fn test_lower_capturing_closure() -> Result<(), LoweringError> {
    // Define HIR for:
    // fn lambda_body(x_capture: i64, y_param: i64) -> i64 { let res = add(x_capture, y_param); res }
    // fn make_adder(x: i64) -> fn(i64) -> i64 { let f = |y| lambda_body(x, y); f } // Captures x

    let lambda_body_symbol = Symbol::fresh();
    let make_adder_symbol = Symbol::fresh();
    let add_intrinsic_symbol = Symbol::fresh(); // Symbol for the add operation
    let add_intrinsic_path = "std::num::__intrinsic_i64_add__".to_string();

    // Vars defined in the scope of the test function
    let make_adder_param_x_var = parallax_hir::hir::HirVar(1);
    let closure_var = parallax_hir::hir::HirVar(0);

    // Vars local to the definition of lambda_body_func's HIR body
    let lambda_param_y_var = parallax_hir::hir::HirVar(2); // Different ID from make_adder_param_x_var
    let lambda_add_result_var = parallax_hir::hir::HirVar(3);

    let i64_ty = HirType::Primitive(HirPrimitiveType::I64);
    let mir_i64_ty = MirType::Primitive(ResolvePrimitiveType::I64);
    let fn_i64_to_i64_ty = HirType::FunctionPointer(vec![i64_ty.clone()], Arc::new(i64_ty.clone()));
    let mir_env_ty = MirType::Tuple(vec![mir_i64_ty.clone()]); // Env holds one i64

    // 1. Define the lambda body function (adds capture and param)
    let lambda_body_func = HirFunction {
        span: dummy_span(),
        symbol: lambda_body_symbol,
        name: "lambda_body_add".to_string(),
        signature: HirFunctionSignature { // Signature has only the explicit param 'y'
            params: vec![(lambda_param_y_var, i64_ty.clone())],
            return_type: i64_ty.clone(),
            is_effectful: false,
        },
        body: Some(
            HirExpr {
                span: dummy_span(),
                kind: HirExprKind::Let {
                    var: lambda_add_result_var,
                    var_ty: i64_ty.clone(),
                    value: Box::new(HirValue::Call {
                        func: Operand::Global(add_intrinsic_symbol),
                        args: vec![
                            // IMPORTANT: The HIR body refers to the *capture source* (make_adder_param_x_var)
                            // and the lambda's *own parameter* (lambda_param_y_var).
                            // Lowering context maps these correctly during specialization.
                            Operand::Var(make_adder_param_x_var),
                            Operand::Var(lambda_param_y_var),
                        ],
                    }),
                    rest: Box::new(HirExpr {
                        span: dummy_span(),
                        kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(lambda_add_result_var))),
                        ty: i64_ty.clone(),
                    }),
                },
                ty: i64_ty.clone(),
            }
        ),
    };

    // 2. Define the function that creates the closure
    let make_adder_func = HirFunction {
        span: dummy_span(),
        symbol: make_adder_symbol,
        name: "make_adder".to_string(),
        signature: HirFunctionSignature {
            params: vec![(make_adder_param_x_var, i64_ty.clone())], // Takes x
            return_type: fn_i64_to_i64_ty.clone(), // Returns fn(i64) -> i64
            is_effectful: false,
        },
        body: Some(
            HirExpr {
                span: dummy_span(),
                kind: HirExprKind::Let {
                    var: closure_var,
                    var_ty: fn_i64_to_i64_ty.clone(),
                    value: Box::new(HirValue::Closure {
                        function_symbol: lambda_body_symbol,
                        captures: vec![Operand::Var(make_adder_param_x_var)], // Captures x
                    }),
                    rest: Box::new(HirExpr {
                        span: dummy_span(),
                        kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(closure_var))),
                        ty: fn_i64_to_i64_ty.clone(),
                    }),
                },
                ty: fn_i64_to_i64_ty.clone(),
            }
        ),
    };

    let hir_module = create_test_hir_module_multi(
        vec![lambda_body_func, make_adder_func],
        vec![],
        Some(make_adder_symbol),
        vec![(add_intrinsic_path.clone(), add_intrinsic_symbol)]
    );
    // Destructure all maps
    let (descriptor_store_box, adt_map, prim_map, tuple_map, array_map) = prepare_for_lower_module(&hir_module).expect("Layout setup failed");
    // Pass references
    let mir_module = lower_module(&hir_module, descriptor_store_box, adt_map, prim_map, tuple_map, array_map)?;

    // --- Assertions ---

    // A. Check that two function graphs were created
    assert_eq!(mir_module.functions.len(), 2, "Expected two MIR function graphs");

    // B. Assertions on the make_adder graph
    let make_adder_graph = mir_module.functions.get(&make_adder_symbol).unwrap();
    let mut specialized_lambda_symbol_opt = None;
    let mut closure_node_id_opt = None;

    // B.1 Find the Closure node and the specialized symbol
    for (id, node) in &make_adder_graph.nodes {
        if let MirNode::Closure {
            original_lambda_symbol,
            specialized_function_symbol,
            capture_types,
            env_ty,
            func_ptr_ty,
        } = node {
            if *original_lambda_symbol == lambda_body_symbol {
                closure_node_id_opt = Some(*id);
                specialized_lambda_symbol_opt = Some(*specialized_function_symbol);
                assert_eq!(capture_types.len(), 1, "Expected 1 capture type");
                assert_eq!(capture_types[0], mir_i64_ty, "Capture type mismatch");
                assert_eq!(*env_ty, mir_env_ty, "Closure env ty mismatch");
                let expected_fptr_ty = MirType::FunctionPointer(
                    vec![mir_env_ty.clone(), mir_i64_ty.clone()], // Env + param y
                    Arc::new(mir_i64_ty.clone()),
                );
                assert_eq!(*func_ptr_ty, expected_fptr_ty, "Closure fptr ty mismatch");
                break;
            }
        }
    }
    let closure_node_id = closure_node_id_opt.expect("Closure node not found");
    let specialized_lambda_symbol = specialized_lambda_symbol_opt.expect("Specialized symbol not found");

    // B.2 Check edge connecting input 'x' to the Closure node's capture input (Port 0)
    let make_adder_param_proj_id = make_adder_graph.edges.iter()
        .find(|e| e.from_node == make_adder_graph.parameter_node.unwrap())
        .map(|e| e.to_node)
        .expect("Projection for make_adder param 'x' not found");

    let found_capture_edge = make_adder_graph.edges.iter().any(|edge|
        edge.to_node == closure_node_id && edge.to_port == PortIndex(0) && // Closure input 0 is first capture
        edge.from_node == make_adder_param_proj_id
    );
    assert!(found_capture_edge, "Edge from parameter 'x' to Closure capture input not found");

    // C. Check the specialized lambda graph
    assert!(mir_module.functions.contains_key(&specialized_lambda_symbol), "Specialized lambda graph not found");
    let specialized_graph = mir_module.functions.get(&specialized_lambda_symbol).unwrap();

    // C.1 Check specialized graph's parameters (capture env, original param y)
    let spec_param_node_id = specialized_graph.parameter_node.expect("Specialized graph parameter node missing");
    let spec_param_node = specialized_graph.nodes.get(&spec_param_node_id).unwrap();
    let expected_spec_agg_param_ty = MirType::Tuple(vec![mir_env_ty.clone(), mir_i64_ty.clone()]);
     match spec_param_node {
         MirNode::Parameter { ty, .. } => assert_eq!(ty, &expected_spec_agg_param_ty),
         _ => panic!("Expected parameter node type")
     }

    // C.2 Find projections for environment (index 0) and param y (index 1)
    let mut env_proj_id_opt = None;
    let mut y_proj_id_opt = None;
    for edge in &specialized_graph.edges {
        if edge.from_node == spec_param_node_id {
             if let Some(MirNode::Project { field_index, .. }) = specialized_graph.nodes.get(&edge.to_node) {
                 if *field_index == 0 { env_proj_id_opt = Some(edge.to_node); }
                 if *field_index == 1 { y_proj_id_opt = Some(edge.to_node); }
             }
        }
    }
    let env_proj_id = env_proj_id_opt.expect("Env projection not found");
    let y_proj_id = y_proj_id_opt.expect("Param y projection not found");

    // C.3 Find projection for capture 'x' (index 0) from the environment tuple
    let mut x_proj_id_opt = None;
    for edge in &specialized_graph.edges {
        if edge.from_node == env_proj_id {
             if let Some(MirNode::Project { field_index: 0, field_ty, .. }) = specialized_graph.nodes.get(&edge.to_node) {
                 if field_ty == &mir_i64_ty {
                     x_proj_id_opt = Some(edge.to_node);
                     break;
                 }
             }
        }
    }
    let x_proj_id = x_proj_id_opt.expect("Capture x projection not found");

    // C.4 Find the Add intrinsic call (StaticAddr + FunctionCall)
    let mut add_call_id_opt = None;
    let mut add_static_addr_id_opt = None;
    let add_func_ty = MirType::FunctionPointer(vec![mir_i64_ty.clone(), mir_i64_ty.clone()], Arc::new(mir_i64_ty.clone()));
    for (id, node) in &specialized_graph.nodes {
         if let MirNode::StaticAddr{ symbol, ty } = node { if *symbol == add_intrinsic_symbol && ty == &add_func_ty { add_static_addr_id_opt = Some(*id); } }
         if let MirNode::FunctionCall{ func_ty } = node { if func_ty == &add_func_ty { add_call_id_opt = Some(*id); } }
    }
    let add_static_addr_id = add_static_addr_id_opt.expect("Add StaticAddr not found");
    let add_call_id = add_call_id_opt.expect("Add FunctionCall not found");

    // C.5 Check edges connect projected x, projected y, and static addr to the add call
    let mut found_x_edge = false;
    let mut found_y_edge = false;
    let mut found_op_edge = false;
    for edge in &specialized_graph.edges {
         if edge.to_node == add_call_id {
             if edge.from_node == x_proj_id && (edge.to_port == PortIndex(1) || edge.to_port == PortIndex(2)) { found_x_edge = true; }
             if edge.from_node == y_proj_id && (edge.to_port == PortIndex(1) || edge.to_port == PortIndex(2)) { found_y_edge = true; }
             if edge.from_node == add_static_addr_id && edge.to_port == PortIndex(0) { found_op_edge = true; }
         }
    }
    assert!(found_x_edge, "Edge from projected x to add call not found");
    assert!(found_y_edge, "Edge from projected y to add call not found");
    assert!(found_op_edge, "Edge from static addr to add call not found");

    // C.6 Check specialized graph return port points to the add call
    let (spec_ret_id, spec_ret_port) = specialized_graph.return_port.expect("Specialized return port missing");
    assert_eq!(spec_ret_id, add_call_id, "Specialized graph should return result of add call");
    assert_eq!(spec_ret_port, PortIndex(0));

    Ok(())
} 