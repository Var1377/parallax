//!
//! Contains the logic for converting HIR types to MIR types.

use std::sync::Arc;
use parallax_common::{hir::HirType, mir::MirType, resolve::types::PrimitiveType as ResolvePrimitiveType};

/// Lowers a single HIR type to its corresponding MIR type representation.
///
/// This function handles primitive types, ADTs (structs/enums),
/// tuples, arrays, and function pointers.
pub(super) fn lower_hir_type_to_mir_type(hir_type: &HirType) -> MirType {
    match hir_type {
        HirType::Primitive(p) => MirType::Primitive(*p),
        HirType::Adt(s) => MirType::Adt(*s),
        HirType::Tuple(ts) => MirType::Tuple(ts.iter().map(lower_hir_type_to_mir_type).collect()),
        HirType::Array(t, size) => MirType::Array(Arc::new(lower_hir_type_to_mir_type(t)), *size),
        HirType::FunctionPointer(args, ret) => MirType::FunctionPointer(
            args.iter().map(lower_hir_type_to_mir_type).collect(),
            Arc::new(lower_hir_type_to_mir_type(ret)),
        ),
        HirType::Never => MirType::Never,
    }
} 