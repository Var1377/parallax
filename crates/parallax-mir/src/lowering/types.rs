//! # HIR Type to MIR Type Conversion (`lowering::types`)
//!
//! Provides the logic for converting High-Level Intermediate Representation (HIR) types
//! ([`HirType`]) into their corresponding Mid-Level Intermediate Representation (MIR) types
//! ([`MirType`]). This is typically a straightforward mapping, preserving the structure
//! of the type (primitive, ADT, tuple, array, function pointer, never).

use std::sync::Arc; // For Arc in Array and FunctionPointer types
use parallax_hir::hir::HirType;
use crate::mir::MirType;
// Import both PrimitiveType enums with aliases
use parallax_hir::hir::PrimitiveType as HirPrimitiveType;
use parallax_resolve::types::PrimitiveType as ResolvePrimitiveType;

/// Maps an HIR primitive type to its corresponding Resolve primitive type.
/// This is needed because MirType::Primitive uses ResolvePrimitiveType.
fn lower_hir_primitive_to_resolve_primitive(hir_prim: HirPrimitiveType) -> ResolvePrimitiveType {
    match hir_prim {
        HirPrimitiveType::I8 => ResolvePrimitiveType::I8,
        HirPrimitiveType::I16 => ResolvePrimitiveType::I16,
        HirPrimitiveType::I32 => ResolvePrimitiveType::I32,
        HirPrimitiveType::I64 => ResolvePrimitiveType::I64,
        HirPrimitiveType::I128 => ResolvePrimitiveType::I128,
        HirPrimitiveType::U8 => ResolvePrimitiveType::U8,
        HirPrimitiveType::U16 => ResolvePrimitiveType::U16,
        HirPrimitiveType::U32 => ResolvePrimitiveType::U32,
        HirPrimitiveType::U64 => ResolvePrimitiveType::U64,
        HirPrimitiveType::U128 => ResolvePrimitiveType::U128,
        HirPrimitiveType::F32 => ResolvePrimitiveType::F32,
        HirPrimitiveType::F64 => ResolvePrimitiveType::F64,
        HirPrimitiveType::Bool => ResolvePrimitiveType::Bool,
        HirPrimitiveType::Char => ResolvePrimitiveType::Char,
        HirPrimitiveType::String => ResolvePrimitiveType::String,
        HirPrimitiveType::Unit => ResolvePrimitiveType::Unit,
    }
}

/// Lowers a single [`HirType`] to its corresponding [`MirType`] representation.
///
/// This function performs a structural conversion:
/// *   `HirType::Primitive` maps to `MirType::Primitive`.
/// *   `HirType::Adt` maps to `MirType::Adt` (retaining the symbol).
/// *   `HirType::Tuple` maps to `MirType::Tuple`, recursively lowering element types.
/// *   `HirType::Array` maps to `MirType::Array`, recursively lowering the element type.
/// *   `HirType::FunctionPointer` maps to `MirType::FunctionPointer`, recursively lowering argument and return types.
/// *   `HirType::Never` maps to `MirType::Never`.
///
/// # Arguments
/// * `hir_type`: A reference to the HIR type to be lowered.
///
/// # Returns
/// The corresponding MIR type.
pub(super) fn lower_hir_type_to_mir_type(hir_type: &HirType) -> MirType {
    match hir_type {
        HirType::Primitive(p) => MirType::Primitive(lower_hir_primitive_to_resolve_primitive(*p)),
        HirType::Adt(s) => MirType::Adt(*s), // Keep the same symbol
        HirType::Tuple(hir_element_types) => {
            // Recursively lower each element type in the tuple.
            let mir_element_types = hir_element_types.iter().map(lower_hir_type_to_mir_type).collect();
            MirType::Tuple(mir_element_types)
        }
        HirType::Array(hir_element_type, size) => {
            // Recursively lower the element type and wrap it in an Arc.
            let mir_element_type = Arc::new(lower_hir_type_to_mir_type(hir_element_type));
            MirType::Array(mir_element_type, *size)
        }
        HirType::FunctionPointer(hir_arg_types, hir_ret_type) => {
            // Recursively lower argument types.
            let mir_arg_types = hir_arg_types.iter().map(lower_hir_type_to_mir_type).collect();
            // Recursively lower the return type and wrap it in an Arc.
            let mir_ret_type = Arc::new(lower_hir_type_to_mir_type(hir_ret_type));
            MirType::FunctionPointer(mir_arg_types, mir_ret_type)
        }
        HirType::Never => MirType::Never, // Direct mapping for the bottom type
    }
} 