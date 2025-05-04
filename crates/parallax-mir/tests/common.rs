// Common helper functions for MIR lowering integration tests

use parallax_mir::{LoweringError}; // Bring LoweringError into scope if needed by tests
use parallax_hir::hir::{
    HirModule, HirFunction, HirFunctionSignature, HirExpr, HirExprKind, HirTailExpr,
    HirLiteral, HirType, PrimitiveType, Operand, HirVar, HirValue,
    HirGlobalStatic, HirStructDef, HirEnumDef, HirEnumVariant
};
use parallax_resolve::types::{Symbol, PrimitiveType as ResolvePrimitiveType};
use miette::{SourceOffset, SourceSpan};
use std::sync::Arc;
use std::collections::HashMap;
use parallax_layout::{LayoutComputer, DescriptorStore, LayoutDescriptor, DescriptorIndex, LayoutError};

pub fn dummy_span() -> SourceSpan {
    SourceSpan::new(SourceOffset::from(0), 0)
}

// Helper to create a simple HIR module for testing a single function
pub fn create_test_hir_module_single_func(func: HirFunction, intrinsics: Vec<(String, Symbol)>) -> HirModule {
    let entry_symbol = func.symbol;
    create_test_hir_module_full(vec![func], vec![], vec![], vec![], Some(entry_symbol), intrinsics)
}

// Fully configurable helper
pub fn create_test_hir_module_full(
    funcs: Vec<HirFunction>,
    structs: Vec<HirStructDef>,
    enums: Vec<HirEnumDef>,
    statics: Vec<HirGlobalStatic>,
    entry: Option<Symbol>,
    intrinsics: Vec<(String, Symbol)>
) -> HirModule {
    HirModule {
        name: "test_module".to_string(),
        functions: funcs,
        structs,
        enums,
        statics,
        entry_point: entry,
        intrinsics,
        next_var_id: 100, // Start var IDs high
    }
}

// Helper to perform layout computation and prepare args for lower_module
pub fn prepare_for_lower_module(hir_module: &HirModule) -> Result<(
    Box<DescriptorStore>,
    HashMap<Symbol, DescriptorIndex>,
    HashMap<PrimitiveType, DescriptorIndex>,
    HashMap<Vec<HirType>, DescriptorIndex>,
    HashMap<(HirType, usize), DescriptorIndex>
), LayoutError> {
    let struct_defs_map = hir_module.structs.iter().map(|s| (s.symbol, s.clone())).collect::<HashMap<_, _>>();
    let enum_defs_map = hir_module.enums.iter().map(|e| (e.symbol, e.clone())).collect::<HashMap<_, _>>();

    let mut layout_computer = LayoutComputer::new(
        struct_defs_map.clone(),
        enum_defs_map.clone(),
    );

    // Pre-calculate layouts for common types used in tests
    let types_to_precompute = vec![
        HirType::Primitive(PrimitiveType::I8),
        HirType::Primitive(PrimitiveType::I16),
        HirType::Primitive(PrimitiveType::I32),
        HirType::Primitive(PrimitiveType::I64),
        HirType::Primitive(PrimitiveType::I128),
        HirType::Primitive(PrimitiveType::U8),
        HirType::Primitive(PrimitiveType::U16),
        HirType::Primitive(PrimitiveType::U32),
        HirType::Primitive(PrimitiveType::U64),
        HirType::Primitive(PrimitiveType::U128),
        HirType::Primitive(PrimitiveType::F32),
        HirType::Primitive(PrimitiveType::F64),
        HirType::Primitive(PrimitiveType::Bool),
        HirType::Primitive(PrimitiveType::Char),
        HirType::Primitive(PrimitiveType::String),
        HirType::Primitive(PrimitiveType::Unit),
        HirType::Tuple(vec![HirType::Primitive(PrimitiveType::I32), HirType::Primitive(PrimitiveType::Bool)]),
        // Add array types if used in tests, e.g.:
        // HirType::Array(Arc::new(HirType::Primitive(PrimitiveType::I32)), 3),
    ];

    for hir_type in types_to_precompute {
        layout_computer.get_or_create_descriptor_index(&hir_type)?;
    }

    for struct_def in &hir_module.structs {
        let hir_type = HirType::Adt(struct_def.symbol);
        layout_computer.get_or_create_descriptor_index(&hir_type)?;
    }
    for enum_def in &hir_module.enums {
        let hir_type = HirType::Adt(enum_def.symbol);
        layout_computer.get_or_create_descriptor_index(&hir_type)?;
    }

    // Finalize returns Vec and maps
    let (descriptors_vec, adt_map, primitive_map, tuple_map, array_map) = layout_computer.finalize();
    // Create DescriptorStore, Box it, and return the correct tuple
    let descriptor_store = Box::new(DescriptorStore { descriptors: descriptors_vec });
    Ok((descriptor_store, adt_map, primitive_map, tuple_map, array_map))
}

// --- Struct Definition Helper ---
pub fn define_point_struct() -> (HirStructDef, Symbol, Symbol, Symbol) {
    let struct_symbol = Symbol::fresh();
    let field_x_symbol = Symbol::fresh();
    let field_y_symbol = Symbol::fresh();
    let point_struct = HirStructDef {
        span: dummy_span(),
        symbol: struct_symbol,
        name: "Point".to_string(),
        fields: vec![
            (field_x_symbol, "x".to_string(), HirType::Primitive(PrimitiveType::I64)),
            (field_y_symbol, "y".to_string(), HirType::Primitive(PrimitiveType::I64)),
        ],
    };
    (point_struct, struct_symbol, field_x_symbol, field_y_symbol)
}

// --- Enum Definition Helper ---
pub fn define_option_enum() -> (HirEnumDef, Symbol, Symbol, Symbol) {
    let enum_symbol = Symbol::fresh();
    let some_variant_symbol = Symbol::fresh();
    let none_variant_symbol = Symbol::fresh();
    let option_enum = HirEnumDef {
        span: dummy_span(),
        symbol: enum_symbol,
        name: "Option".to_string(),
        variants: vec![
            HirEnumVariant {
                symbol: some_variant_symbol,
                name: "Some".to_string(),
                fields: vec![HirType::Primitive(PrimitiveType::I32)], // Option<i32>
                span: dummy_span(),
            },
            HirEnumVariant {
                symbol: none_variant_symbol,
                name: "None".to_string(),
                fields: vec![],
                span: dummy_span(),
            },
        ],
    };
    (option_enum, enum_symbol, some_variant_symbol, none_variant_symbol)
} 