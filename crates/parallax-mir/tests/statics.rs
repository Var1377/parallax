use parallax_mir::{lower_module, MirGraph, MirModule, MirNode, MirType, PortIndex, NodeId, LoweringError};
use parallax_hir::hir::{
    HirModule, HirFunction, HirFunctionSignature, HirExpr, HirExprKind, HirTailExpr,
    HirLiteral, HirType, PrimitiveType as HirPrimitiveType, Operand, HirVar, HirValue,
    HirGlobalStatic, HirStructDef
};
use parallax_resolve::types::{Symbol, PrimitiveType as ResolvePrimitiveType};
use miette::{SourceOffset, SourceSpan};
use std::sync::Arc;
use std::collections::HashMap;

// --- Test Helpers (Copied) ---

mod common; // Add mod declaration
use common::prepare_for_lower_module; // Add import for helper

fn dummy_span() -> SourceSpan {
    SourceSpan::new(SourceOffset::from(0), 0)
}

// Helper to create a simple HIR module for testing
fn create_test_hir_module(func: HirFunction, intrinsics: Vec<(String, Symbol)>) -> HirModule {
    let entry_symbol = func.symbol;
    create_test_hir_module_multi(vec![func], vec![], vec![], Some(entry_symbol), intrinsics)
}

// Helper to accept multiple functions, structs, and statics
fn create_test_hir_module_multi(
    funcs: Vec<HirFunction>,
    structs: Vec<HirStructDef>,
    statics: Vec<HirGlobalStatic>,
    entry: Option<Symbol>,
    intrinsics: Vec<(String, Symbol)>
) -> HirModule {
    HirModule {
        name: "test_module".to_string(),
        functions: funcs,
        structs,
        enums: vec![],
        statics,
        entry_point: entry,
        intrinsics,
        next_var_id: 100,
    }
}

// --- Tests ---

#[test]
fn test_lower_static_addr() -> Result<(), LoweringError> {
    // Define HIR for:
    // static MY_CONST: i32 = 100;
    // fn main() -> i32 { MY_CONST }

    let static_symbol = Symbol::fresh();
    let main_symbol = Symbol::fresh();
    let int_ty = HirType::Primitive(HirPrimitiveType::I32);
    let mir_int_ty = MirType::Primitive(ResolvePrimitiveType::I32);

    let my_const = HirGlobalStatic {
        symbol: static_symbol,
        name: "MY_CONST".to_string(),
        ty: int_ty.clone(),
        initializer: Some(HirValue::Use(Operand::Const(HirLiteral::IntLiteral { value: 100, ty: HirPrimitiveType::I32 }))),
        is_mutable: false,
        span: dummy_span(),
    };

    let main_func = HirFunction {
        span: dummy_span(),
        symbol: main_symbol,
        name: "main".to_string(),
        signature: HirFunctionSignature {
            params: vec![],
            return_type: int_ty.clone(),
            is_effectful: false,
        },
        body: Some(
            HirExpr {
                span: dummy_span(),
                kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Global(static_symbol))),
                ty: int_ty.clone(),
            }
        ),
    };

    let hir_module = create_test_hir_module_multi(
        vec![main_func],
        vec![],
        vec![my_const],
        Some(main_symbol),
        vec![]
    );
    let (descriptor_store_box, adt_index_map, primitive_index_map, tuple_index_map, array_index_map) = prepare_for_lower_module(&hir_module).expect("Layout setup failed");
    let mir_module = lower_module(&hir_module, descriptor_store_box, adt_index_map, primitive_index_map, tuple_index_map, array_index_map)?;

    // --- Assertions ---
    let graph = mir_module.functions.get(&main_symbol).unwrap();

    // 1. Check parameter node (empty)
    let _param_node_id = graph.parameter_node.expect("Param node missing");

    // 2. Find the StaticAddr node for the global constant
    let mut static_addr_node_id_opt = None;
    for (id, node) in &graph.nodes {
        if let MirNode::StaticAddr { symbol, ty } = node {
            if *symbol == static_symbol && ty == &mir_int_ty { // StaticAddr should have the type of the static itself
                static_addr_node_id_opt = Some(*id);
                break;
            }
        }
    }
    let static_addr_node_id = static_addr_node_id_opt.expect("StaticAddr node for MY_CONST not found");

    // 3. Check return port points to the StaticAddr node output
    let (return_node_id, return_port_index) = graph.return_port.expect("Return port missing");
    assert_eq!(return_node_id, static_addr_node_id, "Return should be StaticAddr node");
    assert_eq!(return_port_index, PortIndex(0));

    // 4. Check total nodes (Param, StaticAddr)
    assert_eq!(graph.nodes.len(), 2, "Expected 2 nodes");
    // 5. Check total edges (0)
    assert_eq!(graph.edges.len(), 0, "Expected 0 edges");

    Ok(())
}

// TODO: Add tests for static functions, mutable statics?

#[test]
fn test_lower_static_function_addr() -> Result<(), LoweringError> {
    // Define HIR for:
    // fn my_func() -> i32 = 0;
    // fn main() -> fn() -> i32 { my_func } // Returns the function pointer itself

    let my_func_symbol = Symbol::fresh();
    let main_symbol = Symbol::fresh();
    let int_ty = HirType::Primitive(HirPrimitiveType::I32);
    let fn_ty = HirType::FunctionPointer(vec![], Arc::new(int_ty.clone()));
    let mir_int_ty = MirType::Primitive(ResolvePrimitiveType::I32);
    let mir_fn_ty = MirType::FunctionPointer(vec![], Arc::new(mir_int_ty.clone()));

    let my_func = HirFunction {
        span: dummy_span(),
        symbol: my_func_symbol,
        name: "my_func".to_string(),
        signature: HirFunctionSignature { params: vec![], return_type: int_ty.clone(), is_effectful: false },
        body: Some(HirExpr {
            span: dummy_span(),
            kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Const(HirLiteral::IntLiteral { value: 0, ty: HirPrimitiveType::I32 }))),
            ty: int_ty.clone(),
        }),
    };

    let main_func = HirFunction {
        span: dummy_span(),
        symbol: main_symbol,
        name: "main".to_string(),
        signature: HirFunctionSignature {
            params: vec![],
            return_type: fn_ty.clone(), // Returns the function type
            is_effectful: false,
        },
        body: Some(
            HirExpr {
                span: dummy_span(),
                // Directly return the global symbol for the function
                kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Global(my_func_symbol))),
                ty: fn_ty.clone(),
            }
        ),
    };

    let hir_module = create_test_hir_module_multi(
        vec![main_func, my_func],
        vec![],
        vec![], // No statics needed here
        Some(main_symbol),
        vec![]
    );
    let (descriptor_store_box, adt_index_map, primitive_index_map, tuple_index_map, array_index_map) = prepare_for_lower_module(&hir_module).expect("Layout setup failed");
    let mir_module = lower_module(&hir_module, descriptor_store_box, adt_index_map, primitive_index_map, tuple_index_map, array_index_map)?;

    // --- Assertions ---
    let graph = mir_module.functions.get(&main_symbol).unwrap();

    // 1. Find the StaticAddr node for my_func
    let mut static_addr_node_id_opt = None;
    for (id, node) in &graph.nodes {
        if let MirNode::StaticAddr { symbol, ty } = node {
            if *symbol == my_func_symbol && ty == &mir_fn_ty { // Type is the function pointer type
                static_addr_node_id_opt = Some(*id);
                break;
            }
        }
    }
    let static_addr_node_id = static_addr_node_id_opt.expect("StaticAddr node for my_func not found");

    // 2. Check return port points to the StaticAddr node output
    let (return_node_id, return_port_index) = graph.return_port.expect("Return port missing");
    assert_eq!(return_node_id, static_addr_node_id, "Return should be StaticAddr node");
    assert_eq!(return_port_index, PortIndex(0));

    // 3. Node count (Param, StaticAddr)
    assert_eq!(graph.nodes.len(), 2);
    // 4. Edge count (0)
    assert_eq!(graph.edges.len(), 0);

    Ok(())
} 