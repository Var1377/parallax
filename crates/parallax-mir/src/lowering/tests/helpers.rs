//! Common helper functions for MIR lowering tests.

// Re-export items needed by multiple test modules
use crate::lowering::*;
use crate::mir::*;
use parallax_layout::DescriptorStore;
use parallax_layout::{LayoutComputer, LayoutDescriptor};
use parallax_hir::hir::{self as hir, HirModule, HirFunction, HirFunctionSignature, HirExpr, HirExprKind, HirTailExpr, HirValue, Operand, HirLiteral, HirVar, AggregateKind, HirType, PrimitiveType};
use parallax_resolve::types::{PrimitiveType as ResolvePrimitiveType, Symbol};
use std::collections::HashMap;
use miette::SourceSpan;
use std::sync::Arc;

// Helper to create a dummy span
pub(crate) fn dummy_span() -> SourceSpan {
    SourceSpan::from((0, 0))
}

// Helper function to create a minimal HirModule for testing a single function
// Can be expanded to include structs/enums by the caller
pub(crate) fn create_test_module(func_def: HirFunction) -> HirModule {
    hir::HirModule {
        name: "test_module".to_string(),
        functions: vec![func_def],
        structs: vec![],
        enums: vec![],
        statics: vec![],
        entry_point: None,
        next_var_id: 0, // Start var IDs from 0 for simplicity in tests
        intrinsics: vec![],
    }
}

// Helper function to create a basic lowering context for a single function test
pub(crate) fn test_lower_function(
    hir_module: &HirModule,
    func_def: &HirFunction,
) -> Result<MirGraph, LoweringError> {
    // Remove manual descriptor management
    // let mut descriptors = Vec::new();
    // let handle_desc_index = descriptors.len();
    // descriptors.push(LayoutDescriptor::Handle);

    // Convert Vec<HirStructDef> and Vec<HirEnumDef> to HashMap for LayoutComputer
    let struct_defs_map = hir_module
        .structs
        .iter()
        .map(|s| (s.symbol, s.clone()))
        .collect::<HashMap<_,_>>();
    let enum_defs_map = hir_module
        .enums
        .iter()
        .map(|e| (e.symbol, e.clone()))
        .collect::<HashMap<_,_>>();

    // Instantiate LayoutComputer with ADT maps only
    let mut layout_computer = LayoutComputer::new(
        struct_defs_map,
        enum_defs_map,
    );

    // Pre-calculate layouts for ADTs in the test module (if any)
    // This mimics the behavior in lower_module where layouts are pre-computed
    for struct_def in &hir_module.structs {
        let hir_type = HirType::Adt(struct_def.symbol);
        layout_computer.get_or_create_descriptor_index(&hir_type).expect("Layout computation error");
    }
    for enum_def in &hir_module.enums {
        let hir_type = HirType::Adt(enum_def.symbol);
        layout_computer.get_or_create_descriptor_index(&hir_type).expect("Layout computation error");
    }

    // Finalize to get the store and maps. Destructure all maps.
    let (descriptors_vec, adt_map, primitive_map, tuple_map, array_map) = layout_computer.finalize();

    // Create a DescriptorStore from the returned Vec
    let descriptor_store = DescriptorStore { descriptors: descriptors_vec };

    let mut closure_spec_map = HashMap::new(); // Empty for simple tests

    // Run prepass first (even if empty, it's part of the process)
    if let Some(body) = &func_def.body {
        prepass::find_closures_in_expr(body, hir_module, &mut closure_spec_map)?;
    }

    // Call the internal lower_function directly, passing the finalized descriptor store and map references
    module::lower_function(
        hir_module,
        func_def,
        func_def.symbol, // Use function's own symbol as target
        &descriptor_store, // Pass immutable store reference
        &adt_map, // Pass map references
        &primitive_map,
        &tuple_map,
        &array_map,
        &mut closure_spec_map,
    )
}

// Helper to define a simple struct for tests
pub(crate) fn create_point_struct_def() -> hir::HirStructDef {
    let point_sym = Symbol::new(100);
    let x_sym = Symbol::new(101);
    let y_sym = Symbol::new(102);
    let hir_i32_ty = PrimitiveType::I32;
    let i32_ty = HirType::Primitive(hir_i32_ty);

    hir::HirStructDef {
        symbol: point_sym,
        name: "Point".to_string(),
        fields: vec![
            (x_sym, "x".to_string(), i32_ty.clone()),
            (y_sym, "y".to_string(), i32_ty.clone()),
        ],
        span: dummy_span(),
    }
}

// Helper to define a simple enum for tests
pub(crate) fn create_option_enum_def() -> hir::HirEnumDef {
    let option_sym = Symbol::new(200);
    let none_sym = Symbol::new(201);
    let some_sym = Symbol::new(202);
    let hir_i32_ty = PrimitiveType::I32;
    let i32_ty = HirType::Primitive(hir_i32_ty);

    hir::HirEnumDef {
        symbol: option_sym,
        name: "Option".to_string(),
        variants: vec![
            hir::HirEnumVariant {
                symbol: none_sym,
                name: "None".to_string(),
                fields: vec![],
                span: dummy_span(),
            },
            hir::HirEnumVariant {
                symbol: some_sym,
                name: "Some".to_string(),
                fields: vec![i32_ty],
                span: dummy_span(),
            },
        ],
        span: dummy_span(),
    }
} 