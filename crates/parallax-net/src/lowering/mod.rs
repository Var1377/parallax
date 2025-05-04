mod builder;
mod config;
mod edges;
pub mod error;
mod nodes;
mod tests;

pub use config::InitialNetConfig;
pub use error::LoweringError;

// Use the new encoding module
use crate::encoding::*; 
use crate::node::{NodeType, Wire, Constructor, Duplicator, Eraser, Number, Static, Switch, Async};
use crate::port::Port;
use builder::NetBuilder;
use nodes::lower_mir_node;

use parallax_mir::{
    MirEdge, MirGraph, MirModule, MirNode, NodeId, PortIndex, MirType, // Removed MirNodeMetadata, renamed MirNodeId -> NodeId based on definition
};
use parallax_resolve::types::Symbol;
use std::collections::HashMap;
use crate::CompiledNet;
use std::collections::HashSet;

/// Lowers a full MIR module into a compiled interaction net artifact.
///
/// Each function graph is lowered independently into an `InitialNetConfig`,
/// which contains the necessary interaction net nodes and initial wires
/// to represent the function's computation according to the structural encoding rules.
/// The function accesses the `DescriptorStore` within the `mir_module` if needed
/// for layout-dependent lowering decisions (though currently, layout primarily
/// affects runtime GC interpretation).
///
/// # Arguments
/// * `mir_module` - A reference to the MIR module containing functions and the descriptor store.
///
/// # Returns
/// A `Result` containing the compiled interaction net artifact.
pub fn lower_module(
    mir_module: &MirModule, // Changed: Accept the whole module
) -> Result<CompiledNet, LoweringError> {
    let mut networks = HashMap::new();
    let mut partition_counter: u16 = 0;
    let mut func_id_counter: usize = 0;

    // --- Build Intrinsic Op Map --- 
    // Maps intrinsic Symbol to its fully encoded 64-bit Static node data
    let mut intrinsic_op_map: HashMap<Symbol, u64> = HashMap::new();
    for (path, symbol) in &mir_module.intrinsics {
        // Use encoding functions from the encoding module
        let encoded_static_data: Option<u64> = match path.as_str() {
            
            // --- Intrinsics (Arithmetic, Comparison, Conversions grouped by Source Type) ---
            // Order: u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, f32, f64, bool, runtime

            // --- u8 --- 
            "std::num::__intrinsic_u8_add__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_ADD, TYPECODE_U8, TYPECODE_NONE)),
            "std::num::__intrinsic_u8_sub__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SUB, TYPECODE_U8, TYPECODE_NONE)),
            "std::num::__intrinsic_u8_mul__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_MUL, TYPECODE_U8, TYPECODE_NONE)),
            "std::num::__intrinsic_u8_div__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_DIV, TYPECODE_U8, TYPECODE_NONE)),
            "std::num::__intrinsic_u8_rem__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_REM, TYPECODE_U8, TYPECODE_NONE)),
            "std::num::__intrinsic_u8_and__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_AND, TYPECODE_U8, TYPECODE_NONE)),
            "std::num::__intrinsic_u8_or__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_OR, TYPECODE_U8, TYPECODE_NONE)),
            "std::num::__intrinsic_u8_xor__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_XOR, TYPECODE_U8, TYPECODE_NONE)),
            "std::num::__intrinsic_u8_shl__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SHL, TYPECODE_U8, TYPECODE_NONE)),
            "std::num::__intrinsic_u8_shr__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SHR, TYPECODE_U8, TYPECODE_NONE)),
            "std::num::__intrinsic_u8_not__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NOT, TYPECODE_U8, TYPECODE_NONE)),
            "std::num::__intrinsic_u8_pow__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_POW, TYPECODE_U8, TYPECODE_NONE)),
            "std::num::__intrinsic_u8_eq__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_EQ, TYPECODE_U8, TYPECODE_BOOL)),
            "std::num::__intrinsic_u8_ne__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NE, TYPECODE_U8, TYPECODE_BOOL)),
            "std::num::__intrinsic_u8_lt__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_LT, TYPECODE_U8, TYPECODE_BOOL)),
            "std::num::__intrinsic_u8_le__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_LE, TYPECODE_U8, TYPECODE_BOOL)),
            "std::num::__intrinsic_u8_gt__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_GT, TYPECODE_U8, TYPECODE_BOOL)),
            "std::num::__intrinsic_u8_ge__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_GE, TYPECODE_U8, TYPECODE_BOOL)),
            "std::num::__intrinsic_u8_abs__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_ABS, TYPECODE_U8, TYPECODE_NONE)),
            // U8 Conversions
            "std::num::__intrinsic_u8_to_i8__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U8, TYPECODE_I8)),
            "std::num::__intrinsic_u8_to_u16__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U8, TYPECODE_U16)),
            "std::num::__intrinsic_u8_to_i16__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U8, TYPECODE_I16)),
            "std::num::__intrinsic_u8_to_u32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U8, TYPECODE_U32)),
            "std::num::__intrinsic_u8_to_i32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U8, TYPECODE_I32)),
            "std::num::__intrinsic_u8_to_u64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U8, TYPECODE_U64)),
            "std::num::__intrinsic_u8_to_i64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U8, TYPECODE_I64)),
            "std::num::__intrinsic_u8_to_u128__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U8, TYPECODE_U128)),
            "std::num::__intrinsic_u8_to_i128__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U8, TYPECODE_I128)),
            "std::num::__intrinsic_u8_to_f32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U8, TYPECODE_F32)),
            "std::num::__intrinsic_u8_to_f64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U8, TYPECODE_F64)),

             // --- i8 ---
            "std::num::__intrinsic_i8_add__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_ADD, TYPECODE_I8, TYPECODE_NONE)),
            "std::num::__intrinsic_i8_sub__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SUB, TYPECODE_I8, TYPECODE_NONE)),
            "std::num::__intrinsic_i8_mul__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_MUL, TYPECODE_I8, TYPECODE_NONE)),
            "std::num::__intrinsic_i8_div__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_DIV, TYPECODE_I8, TYPECODE_NONE)),
            "std::num::__intrinsic_i8_rem__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_REM, TYPECODE_I8, TYPECODE_NONE)),
            "std::num::__intrinsic_i8_and__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_AND, TYPECODE_I8, TYPECODE_NONE)),
            "std::num::__intrinsic_i8_or__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_OR, TYPECODE_I8, TYPECODE_NONE)),
            "std::num::__intrinsic_i8_xor__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_XOR, TYPECODE_I8, TYPECODE_NONE)),
            "std::num::__intrinsic_i8_shl__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SHL, TYPECODE_I8, TYPECODE_NONE)),
            "std::num::__intrinsic_i8_shr__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SHR, TYPECODE_I8, TYPECODE_NONE)),
            "std::num::__intrinsic_i8_neg__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NEG, TYPECODE_I8, TYPECODE_NONE)),
            "std::num::__intrinsic_i8_not__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NOT, TYPECODE_I8, TYPECODE_NONE)),
            "std::num::__intrinsic_i8_abs__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_ABS, TYPECODE_I8, TYPECODE_NONE)),
            "std::num::__intrinsic_i8_pow__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_POW, TYPECODE_I8, TYPECODE_NONE)),
            "std::num::__intrinsic_i8_eq__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_EQ, TYPECODE_I8, TYPECODE_BOOL)),
            "std::num::__intrinsic_i8_ne__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NE, TYPECODE_I8, TYPECODE_BOOL)),
            "std::num::__intrinsic_i8_lt__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_LT, TYPECODE_I8, TYPECODE_BOOL)),
            "std::num::__intrinsic_i8_le__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_LE, TYPECODE_I8, TYPECODE_BOOL)),
            "std::num::__intrinsic_i8_gt__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_GT, TYPECODE_I8, TYPECODE_BOOL)),
            "std::num::__intrinsic_i8_ge__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_GE, TYPECODE_I8, TYPECODE_BOOL)),
            // I8 Conversions
            "std::num::__intrinsic_i8_to_u8__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I8, TYPECODE_U8)),
            "std::num::__intrinsic_i8_to_u16__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I8, TYPECODE_U16)),
            "std::num::__intrinsic_i8_to_i16__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I8, TYPECODE_I16)),
            "std::num::__intrinsic_i8_to_u32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I8, TYPECODE_U32)),
            "std::num::__intrinsic_i8_to_i32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I8, TYPECODE_I32)),
            "std::num::__intrinsic_i8_to_u64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I8, TYPECODE_U64)),
            "std::num::__intrinsic_i8_to_i64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I8, TYPECODE_I64)),
            "std::num::__intrinsic_i8_to_u128__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I8, TYPECODE_U128)),
            "std::num::__intrinsic_i8_to_i128__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I8, TYPECODE_I128)),
            "std::num::__intrinsic_i8_to_f32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I8, TYPECODE_F32)),
            "std::num::__intrinsic_i8_to_f64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I8, TYPECODE_F64)),

            // --- u16 --- 
            "std::num::__intrinsic_u16_add__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_ADD, TYPECODE_U16, TYPECODE_NONE)),
            "std::num::__intrinsic_u16_sub__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SUB, TYPECODE_U16, TYPECODE_NONE)),
            "std::num::__intrinsic_u16_mul__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_MUL, TYPECODE_U16, TYPECODE_NONE)),
            "std::num::__intrinsic_u16_div__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_DIV, TYPECODE_U16, TYPECODE_NONE)),
            "std::num::__intrinsic_u16_rem__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_REM, TYPECODE_U16, TYPECODE_NONE)),
            "std::num::__intrinsic_u16_and__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_AND, TYPECODE_U16, TYPECODE_NONE)),
            "std::num::__intrinsic_u16_or__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_OR, TYPECODE_U16, TYPECODE_NONE)),
            "std::num::__intrinsic_u16_xor__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_XOR, TYPECODE_U16, TYPECODE_NONE)),
            "std::num::__intrinsic_u16_shl__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SHL, TYPECODE_U16, TYPECODE_NONE)),
            "std::num::__intrinsic_u16_shr__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SHR, TYPECODE_U16, TYPECODE_NONE)),
            "std::num::__intrinsic_u16_not__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NOT, TYPECODE_U16, TYPECODE_NONE)),
            "std::num::__intrinsic_u16_pow__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_POW, TYPECODE_U16, TYPECODE_NONE)),
            "std::num::__intrinsic_u16_eq__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_EQ, TYPECODE_U16, TYPECODE_BOOL)),
            "std::num::__intrinsic_u16_ne__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NE, TYPECODE_U16, TYPECODE_BOOL)),
            "std::num::__intrinsic_u16_lt__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_LT, TYPECODE_U16, TYPECODE_BOOL)),
            "std::num::__intrinsic_u16_le__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_LE, TYPECODE_U16, TYPECODE_BOOL)),
            "std::num::__intrinsic_u16_gt__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_GT, TYPECODE_U16, TYPECODE_BOOL)),
            "std::num::__intrinsic_u16_ge__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_GE, TYPECODE_U16, TYPECODE_BOOL)),
            "std::num::__intrinsic_u16_abs__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_ABS, TYPECODE_U16, TYPECODE_NONE)),
            // U16 Conversions
            "std::num::__intrinsic_u16_to_u8__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U16, TYPECODE_U8)),
            "std::num::__intrinsic_u16_to_i8__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U16, TYPECODE_I8)),
            "std::num::__intrinsic_u16_to_i16__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U16, TYPECODE_I16)),
            "std::num::__intrinsic_u16_to_u32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U16, TYPECODE_U32)),
            "std::num::__intrinsic_u16_to_i32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U16, TYPECODE_I32)),
            "std::num::__intrinsic_u16_to_u64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U16, TYPECODE_U64)),
            "std::num::__intrinsic_u16_to_i64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U16, TYPECODE_I64)),
            "std::num::__intrinsic_u16_to_u128__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U16, TYPECODE_U128)),
            "std::num::__intrinsic_u16_to_i128__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U16, TYPECODE_I128)),
            "std::num::__intrinsic_u16_to_f32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U16, TYPECODE_F32)),
            "std::num::__intrinsic_u16_to_f64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U16, TYPECODE_F64)),

            // --- i16 --- 
            "std::num::__intrinsic_i16_add__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_ADD, TYPECODE_I16, TYPECODE_NONE)),
            "std::num::__intrinsic_i16_sub__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SUB, TYPECODE_I16, TYPECODE_NONE)),
            "std::num::__intrinsic_i16_mul__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_MUL, TYPECODE_I16, TYPECODE_NONE)),
            "std::num::__intrinsic_i16_div__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_DIV, TYPECODE_I16, TYPECODE_NONE)),
            "std::num::__intrinsic_i16_rem__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_REM, TYPECODE_I16, TYPECODE_NONE)),
            "std::num::__intrinsic_i16_and__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_AND, TYPECODE_I16, TYPECODE_NONE)),
            "std::num::__intrinsic_i16_or__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_OR, TYPECODE_I16, TYPECODE_NONE)),
            "std::num::__intrinsic_i16_xor__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_XOR, TYPECODE_I16, TYPECODE_NONE)),
            "std::num::__intrinsic_i16_shl__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SHL, TYPECODE_I16, TYPECODE_NONE)),
            "std::num::__intrinsic_i16_shr__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SHR, TYPECODE_I16, TYPECODE_NONE)),
            "std::num::__intrinsic_i16_neg__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NEG, TYPECODE_I16, TYPECODE_NONE)),
            "std::num::__intrinsic_i16_not__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NOT, TYPECODE_I16, TYPECODE_NONE)),
            "std::num::__intrinsic_i16_abs__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_ABS, TYPECODE_I16, TYPECODE_NONE)),
            "std::num::__intrinsic_i16_pow__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_POW, TYPECODE_I16, TYPECODE_NONE)),
            "std::num::__intrinsic_i16_eq__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_EQ, TYPECODE_I16, TYPECODE_BOOL)),
            "std::num::__intrinsic_i16_ne__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NE, TYPECODE_I16, TYPECODE_BOOL)),
            "std::num::__intrinsic_i16_lt__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_LT, TYPECODE_I16, TYPECODE_BOOL)),
            "std::num::__intrinsic_i16_le__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_LE, TYPECODE_I16, TYPECODE_BOOL)),
            "std::num::__intrinsic_i16_gt__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_GT, TYPECODE_I16, TYPECODE_BOOL)),
            "std::num::__intrinsic_i16_ge__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_GE, TYPECODE_I16, TYPECODE_BOOL)),
            // I16 Conversions
            "std::num::__intrinsic_i16_to_u8__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I16, TYPECODE_U8)),
            "std::num::__intrinsic_i16_to_i8__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I16, TYPECODE_I8)),
            "std::num::__intrinsic_i16_to_u16__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I16, TYPECODE_U16)),
            "std::num::__intrinsic_i16_to_u32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I16, TYPECODE_U32)),
            "std::num::__intrinsic_i16_to_i32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I16, TYPECODE_I32)),
            "std::num::__intrinsic_i16_to_u64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I16, TYPECODE_U64)),
            "std::num::__intrinsic_i16_to_i64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I16, TYPECODE_I64)),
            "std::num::__intrinsic_i16_to_u128__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I16, TYPECODE_U128)),
            "std::num::__intrinsic_i16_to_i128__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I16, TYPECODE_I128)),
            "std::num::__intrinsic_i16_to_f32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I16, TYPECODE_F32)),
            "std::num::__intrinsic_i16_to_f64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I16, TYPECODE_F64)),

            // --- u32 --- 
            "std::num::__intrinsic_u32_add__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_ADD, TYPECODE_U32, TYPECODE_NONE)),
            "std::num::__intrinsic_u32_sub__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SUB, TYPECODE_U32, TYPECODE_NONE)),
            "std::num::__intrinsic_u32_mul__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_MUL, TYPECODE_U32, TYPECODE_NONE)),
            "std::num::__intrinsic_u32_div__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_DIV, TYPECODE_U32, TYPECODE_NONE)),
            "std::num::__intrinsic_u32_rem__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_REM, TYPECODE_U32, TYPECODE_NONE)),
            "std::num::__intrinsic_u32_and__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_AND, TYPECODE_U32, TYPECODE_NONE)),
            "std::num::__intrinsic_u32_or__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_OR, TYPECODE_U32, TYPECODE_NONE)),
            "std::num::__intrinsic_u32_xor__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_XOR, TYPECODE_U32, TYPECODE_NONE)),
            "std::num::__intrinsic_u32_shl__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SHL, TYPECODE_U32, TYPECODE_NONE)),
            "std::num::__intrinsic_u32_shr__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SHR, TYPECODE_U32, TYPECODE_NONE)),
            "std::num::__intrinsic_u32_not__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NOT, TYPECODE_U32, TYPECODE_NONE)),
            "std::num::__intrinsic_u32_pow__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_POW, TYPECODE_U32, TYPECODE_NONE)),
            "std::num::__intrinsic_u32_eq__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_EQ, TYPECODE_U32, TYPECODE_BOOL)),
            "std::num::__intrinsic_u32_ne__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NE, TYPECODE_U32, TYPECODE_BOOL)),
            "std::num::__intrinsic_u32_lt__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_LT, TYPECODE_U32, TYPECODE_BOOL)),
            "std::num::__intrinsic_u32_le__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_LE, TYPECODE_U32, TYPECODE_BOOL)),
            "std::num::__intrinsic_u32_gt__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_GT, TYPECODE_U32, TYPECODE_BOOL)),
            "std::num::__intrinsic_u32_ge__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_GE, TYPECODE_U32, TYPECODE_BOOL)),
            "std::num::__intrinsic_u32_abs__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_ABS, TYPECODE_U32, TYPECODE_NONE)),
            // U32 Conversions
            "std::num::__intrinsic_u32_to_u8__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U32, TYPECODE_U8)),
            "std::num::__intrinsic_u32_to_i8__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U32, TYPECODE_I8)),
            "std::num::__intrinsic_u32_to_u16__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U32, TYPECODE_U16)),
            "std::num::__intrinsic_u32_to_i16__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U32, TYPECODE_I16)),
            "std::num::__intrinsic_u32_to_i32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U32, TYPECODE_I32)),
            "std::num::__intrinsic_u32_to_u64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U32, TYPECODE_U64)),
            "std::num::__intrinsic_u32_to_i64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U32, TYPECODE_I64)),
            "std::num::__intrinsic_u32_to_u128__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U32, TYPECODE_U128)),
            "std::num::__intrinsic_u32_to_i128__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U32, TYPECODE_I128)),
            "std::num::__intrinsic_u32_to_f32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U32, TYPECODE_F32)),
            "std::num::__intrinsic_u32_to_f64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U32, TYPECODE_F64)),

            // --- i32 --- 
            "std::num::__intrinsic_i32_add__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_ADD, TYPECODE_I32, TYPECODE_NONE)),
            "std::num::__intrinsic_i32_sub__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SUB, TYPECODE_I32, TYPECODE_NONE)),
            "std::num::__intrinsic_i32_mul__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_MUL, TYPECODE_I32, TYPECODE_NONE)),
            "std::num::__intrinsic_i32_div__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_DIV, TYPECODE_I32, TYPECODE_NONE)),
            "std::num::__intrinsic_i32_rem__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_REM, TYPECODE_I32, TYPECODE_NONE)),
            "std::num::__intrinsic_i32_and__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_AND, TYPECODE_I32, TYPECODE_NONE)),
            "std::num::__intrinsic_i32_or__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_OR, TYPECODE_I32, TYPECODE_NONE)),
            "std::num::__intrinsic_i32_xor__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_XOR, TYPECODE_I32, TYPECODE_NONE)),
            "std::num::__intrinsic_i32_shl__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SHL, TYPECODE_I32, TYPECODE_NONE)),
            "std::num::__intrinsic_i32_shr__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SHR, TYPECODE_I32, TYPECODE_NONE)),
            "std::num::__intrinsic_i32_neg__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NEG, TYPECODE_I32, TYPECODE_NONE)),
            "std::num::__intrinsic_i32_not__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NOT, TYPECODE_I32, TYPECODE_NONE)),
            "std::num::__intrinsic_i32_abs__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_ABS, TYPECODE_I32, TYPECODE_NONE)),
            "std::num::__intrinsic_i32_pow__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_POW, TYPECODE_I32, TYPECODE_NONE)),
            "std::num::__intrinsic_i32_eq__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_EQ, TYPECODE_I32, TYPECODE_BOOL)),
            "std::num::__intrinsic_i32_ne__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NE, TYPECODE_I32, TYPECODE_BOOL)),
            "std::num::__intrinsic_i32_lt__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_LT, TYPECODE_I32, TYPECODE_BOOL)),
            "std::num::__intrinsic_i32_le__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_LE, TYPECODE_I32, TYPECODE_BOOL)),
            "std::num::__intrinsic_i32_gt__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_GT, TYPECODE_I32, TYPECODE_BOOL)),
            "std::num::__intrinsic_i32_ge__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_GE, TYPECODE_I32, TYPECODE_BOOL)),
            // I32 Conversions
            "std::num::__intrinsic_i32_to_u8__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I32, TYPECODE_U8)),
            "std::num::__intrinsic_i32_to_i8__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I32, TYPECODE_I8)),
            "std::num::__intrinsic_i32_to_u16__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I32, TYPECODE_U16)),
            "std::num::__intrinsic_i32_to_i16__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I32, TYPECODE_I16)),
            "std::num::__intrinsic_i32_to_u32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I32, TYPECODE_U32)),
            "std::num::__intrinsic_i32_to_u64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I32, TYPECODE_U64)),
            "std::num::__intrinsic_i32_to_i64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I32, TYPECODE_I64)),
            "std::num::__intrinsic_i32_to_u128__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I32, TYPECODE_U128)),
            "std::num::__intrinsic_i32_to_i128__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I32, TYPECODE_I128)),
            "std::num::__intrinsic_i32_to_f32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I32, TYPECODE_F32)),
            "std::num::__intrinsic_i32_to_f64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I32, TYPECODE_F64)),

            // --- u64 --- 
            "std::num::__intrinsic_u64_add__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_ADD, TYPECODE_U64, TYPECODE_NONE)),
            "std::num::__intrinsic_u64_sub__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SUB, TYPECODE_U64, TYPECODE_NONE)),
            "std::num::__intrinsic_u64_mul__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_MUL, TYPECODE_U64, TYPECODE_NONE)),
            "std::num::__intrinsic_u64_div__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_DIV, TYPECODE_U64, TYPECODE_NONE)),
            "std::num::__intrinsic_u64_rem__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_REM, TYPECODE_U64, TYPECODE_NONE)),
            "std::num::__intrinsic_u64_and__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_AND, TYPECODE_U64, TYPECODE_NONE)),
            "std::num::__intrinsic_u64_or__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_OR, TYPECODE_U64, TYPECODE_NONE)),
            "std::num::__intrinsic_u64_xor__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_XOR, TYPECODE_U64, TYPECODE_NONE)),
            "std::num::__intrinsic_u64_shl__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SHL, TYPECODE_U64, TYPECODE_NONE)),
            "std::num::__intrinsic_u64_shr__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SHR, TYPECODE_U64, TYPECODE_NONE)),
            "std::num::__intrinsic_u64_not__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NOT, TYPECODE_U64, TYPECODE_NONE)),
            "std::num::__intrinsic_u64_pow__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_POW, TYPECODE_U64, TYPECODE_NONE)),
            "std::num::__intrinsic_u64_eq__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_EQ, TYPECODE_U64, TYPECODE_BOOL)),
            "std::num::__intrinsic_u64_ne__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NE, TYPECODE_U64, TYPECODE_BOOL)),
            "std::num::__intrinsic_u64_lt__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_LT, TYPECODE_U64, TYPECODE_BOOL)),
            "std::num::__intrinsic_u64_le__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_LE, TYPECODE_U64, TYPECODE_BOOL)),
            "std::num::__intrinsic_u64_gt__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_GT, TYPECODE_U64, TYPECODE_BOOL)),
            "std::num::__intrinsic_u64_ge__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_GE, TYPECODE_U64, TYPECODE_BOOL)),
            "std::num::__intrinsic_u64_abs__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_ABS, TYPECODE_U64, TYPECODE_NONE)),
            // U64 Conversions
            "std::num::__intrinsic_u64_to_u8__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U64, TYPECODE_U8)),
            "std::num::__intrinsic_u64_to_i8__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U64, TYPECODE_I8)),
            "std::num::__intrinsic_u64_to_u16__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U64, TYPECODE_U16)),
            "std::num::__intrinsic_u64_to_i16__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U64, TYPECODE_I16)),
            "std::num::__intrinsic_u64_to_u32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U64, TYPECODE_U32)),
            "std::num::__intrinsic_u64_to_i32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U64, TYPECODE_I32)),
            "std::num::__intrinsic_u64_to_i64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U64, TYPECODE_I64)),
            "std::num::__intrinsic_u64_to_u128__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U64, TYPECODE_U128)),
            "std::num::__intrinsic_u64_to_i128__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U64, TYPECODE_I128)),
            "std::num::__intrinsic_u64_to_f32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U64, TYPECODE_F32)),
            "std::num::__intrinsic_u64_to_f64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U64, TYPECODE_F64)),

            // --- i64 --- 
            "std::num::__intrinsic_i64_add__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_ADD, TYPECODE_I64, TYPECODE_NONE)),
            "std::num::__intrinsic_i64_sub__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SUB, TYPECODE_I64, TYPECODE_NONE)),
            "std::num::__intrinsic_i64_mul__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_MUL, TYPECODE_I64, TYPECODE_NONE)),
            "std::num::__intrinsic_i64_div__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_DIV, TYPECODE_I64, TYPECODE_NONE)),
            "std::num::__intrinsic_i64_rem__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_REM, TYPECODE_I64, TYPECODE_NONE)),
            "std::num::__intrinsic_i64_and__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_AND, TYPECODE_I64, TYPECODE_NONE)),
            "std::num::__intrinsic_i64_or__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_OR, TYPECODE_I64, TYPECODE_NONE)),
            "std::num::__intrinsic_i64_xor__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_XOR, TYPECODE_I64, TYPECODE_NONE)),
            "std::num::__intrinsic_i64_shl__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SHL, TYPECODE_I64, TYPECODE_NONE)),
            "std::num::__intrinsic_i64_shr__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SHR, TYPECODE_I64, TYPECODE_NONE)),
            "std::num::__intrinsic_i64_neg__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NEG, TYPECODE_I64, TYPECODE_NONE)),
            "std::num::__intrinsic_i64_not__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NOT, TYPECODE_I64, TYPECODE_NONE)),
            "std::num::__intrinsic_i64_abs__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_ABS, TYPECODE_I64, TYPECODE_NONE)),
            "std::num::__intrinsic_i64_pow__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_POW, TYPECODE_I64, TYPECODE_NONE)),
            "std::num::__intrinsic_i64_eq__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_EQ, TYPECODE_I64, TYPECODE_BOOL)),
            "std::num::__intrinsic_i64_ne__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NE, TYPECODE_I64, TYPECODE_BOOL)),
            "std::num::__intrinsic_i64_lt__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_LT, TYPECODE_I64, TYPECODE_BOOL)),
            "std::num::__intrinsic_i64_le__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_LE, TYPECODE_I64, TYPECODE_BOOL)),
            "std::num::__intrinsic_i64_gt__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_GT, TYPECODE_I64, TYPECODE_BOOL)),
            "std::num::__intrinsic_i64_ge__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_GE, TYPECODE_I64, TYPECODE_BOOL)),
            // I64 Conversions
            "std::num::__intrinsic_i64_to_u8__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I64, TYPECODE_U8)),
            "std::num::__intrinsic_i64_to_i8__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I64, TYPECODE_I8)),
            "std::num::__intrinsic_i64_to_u16__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I64, TYPECODE_U16)),
            "std::num::__intrinsic_i64_to_i16__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I64, TYPECODE_I16)),
            "std::num::__intrinsic_i64_to_u32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I64, TYPECODE_U32)),
            "std::num::__intrinsic_i64_to_i32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I64, TYPECODE_I32)),
            "std::num::__intrinsic_i64_to_u64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I64, TYPECODE_U64)),
            "std::num::__intrinsic_i64_to_u128__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I64, TYPECODE_U128)),
            "std::num::__intrinsic_i64_to_i128__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I64, TYPECODE_I128)),
            "std::num::__intrinsic_i64_to_f32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I64, TYPECODE_F32)),
            "std::num::__intrinsic_i64_to_f64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I64, TYPECODE_F64)),

            // --- u128 --- 
            "std::num::__intrinsic_u128_add__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_ADD, TYPECODE_U128, TYPECODE_NONE)),
            "std::num::__intrinsic_u128_sub__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SUB, TYPECODE_U128, TYPECODE_NONE)),
            "std::num::__intrinsic_u128_mul__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_MUL, TYPECODE_U128, TYPECODE_NONE)),
            "std::num::__intrinsic_u128_div__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_DIV, TYPECODE_U128, TYPECODE_NONE)),
            "std::num::__intrinsic_u128_rem__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_REM, TYPECODE_U128, TYPECODE_NONE)),
            "std::num::__intrinsic_u128_and__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_AND, TYPECODE_U128, TYPECODE_NONE)),
            "std::num::__intrinsic_u128_or__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_OR, TYPECODE_U128, TYPECODE_NONE)),
            "std::num::__intrinsic_u128_xor__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_XOR, TYPECODE_U128, TYPECODE_NONE)),
            "std::num::__intrinsic_u128_shl__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SHL, TYPECODE_U128, TYPECODE_NONE)),
            "std::num::__intrinsic_u128_shr__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SHR, TYPECODE_U128, TYPECODE_NONE)),
            "std::num::__intrinsic_u128_not__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NOT, TYPECODE_U128, TYPECODE_NONE)),
            "std::num::__intrinsic_u128_pow__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_POW, TYPECODE_U128, TYPECODE_NONE)),
            "std::num::__intrinsic_u128_eq__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_EQ, TYPECODE_U128, TYPECODE_BOOL)),
            "std::num::__intrinsic_u128_ne__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NE, TYPECODE_U128, TYPECODE_BOOL)),
            "std::num::__intrinsic_u128_lt__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_LT, TYPECODE_U128, TYPECODE_BOOL)),
            "std::num::__intrinsic_u128_le__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_LE, TYPECODE_U128, TYPECODE_BOOL)),
            "std::num::__intrinsic_u128_gt__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_GT, TYPECODE_U128, TYPECODE_BOOL)),
            "std::num::__intrinsic_u128_ge__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_GE, TYPECODE_U128, TYPECODE_BOOL)),
            "std::num::__intrinsic_u128_abs__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_ABS, TYPECODE_U128, TYPECODE_NONE)),
            // U128 Conversions
            "std::num::__intrinsic_u128_to_u8__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U128, TYPECODE_U8)),
            "std::num::__intrinsic_u128_to_i8__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U128, TYPECODE_I8)),
            "std::num::__intrinsic_u128_to_u16__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U128, TYPECODE_U16)),
            "std::num::__intrinsic_u128_to_i16__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U128, TYPECODE_I16)),
            "std::num::__intrinsic_u128_to_u32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U128, TYPECODE_U32)),
            "std::num::__intrinsic_u128_to_i32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U128, TYPECODE_I32)),
            "std::num::__intrinsic_u128_to_u64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U128, TYPECODE_U64)),
            "std::num::__intrinsic_u128_to_i64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U128, TYPECODE_I64)),
            "std::num::__intrinsic_u128_to_i128__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U128, TYPECODE_I128)),
            "std::num::__intrinsic_u128_to_f32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U128, TYPECODE_F32)),
            "std::num::__intrinsic_u128_to_f64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_U128, TYPECODE_F64)),

            // --- i128 --- 
            "std::num::__intrinsic_i128_add__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_ADD, TYPECODE_I128, TYPECODE_NONE)),
            "std::num::__intrinsic_i128_sub__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SUB, TYPECODE_I128, TYPECODE_NONE)),
            "std::num::__intrinsic_i128_mul__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_MUL, TYPECODE_I128, TYPECODE_NONE)),
            "std::num::__intrinsic_i128_div__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_DIV, TYPECODE_I128, TYPECODE_NONE)),
            "std::num::__intrinsic_i128_rem__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_REM, TYPECODE_I128, TYPECODE_NONE)),
            "std::num::__intrinsic_i128_and__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_AND, TYPECODE_I128, TYPECODE_NONE)),
            "std::num::__intrinsic_i128_or__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_OR, TYPECODE_I128, TYPECODE_NONE)),
            "std::num::__intrinsic_i128_xor__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_XOR, TYPECODE_I128, TYPECODE_NONE)),
            "std::num::__intrinsic_i128_shl__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SHL, TYPECODE_I128, TYPECODE_NONE)),
            "std::num::__intrinsic_i128_shr__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SHR, TYPECODE_I128, TYPECODE_NONE)),
            "std::num::__intrinsic_i128_neg__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NEG, TYPECODE_I128, TYPECODE_NONE)),
            "std::num::__intrinsic_i128_not__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NOT, TYPECODE_I128, TYPECODE_NONE)),
            "std::num::__intrinsic_i128_abs__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_ABS, TYPECODE_I128, TYPECODE_NONE)),
            "std::num::__intrinsic_i128_pow__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_POW, TYPECODE_I128, TYPECODE_NONE)),
            "std::num::__intrinsic_i128_eq__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_EQ, TYPECODE_I128, TYPECODE_BOOL)),
            "std::num::__intrinsic_i128_ne__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NE, TYPECODE_I128, TYPECODE_BOOL)),
            "std::num::__intrinsic_i128_lt__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_LT, TYPECODE_I128, TYPECODE_BOOL)),
            "std::num::__intrinsic_i128_le__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_LE, TYPECODE_I128, TYPECODE_BOOL)),
            "std::num::__intrinsic_i128_gt__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_GT, TYPECODE_I128, TYPECODE_BOOL)),
            "std::num::__intrinsic_i128_ge__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_GE, TYPECODE_I128, TYPECODE_BOOL)),
            // I128 Conversions
            "std::num::__intrinsic_i128_to_u8__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I128, TYPECODE_U8)),
            "std::num::__intrinsic_i128_to_i8__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I128, TYPECODE_I8)),
            "std::num::__intrinsic_i128_to_u16__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I128, TYPECODE_U16)),
            "std::num::__intrinsic_i128_to_i16__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I128, TYPECODE_I16)),
            "std::num::__intrinsic_i128_to_u32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I128, TYPECODE_U32)),
            "std::num::__intrinsic_i128_to_i32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I128, TYPECODE_I32)),
            "std::num::__intrinsic_i128_to_u64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I128, TYPECODE_U64)),
            "std::num::__intrinsic_i128_to_i64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I128, TYPECODE_I64)),
            "std::num::__intrinsic_i128_to_u128__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I128, TYPECODE_U128)),
            "std::num::__intrinsic_i128_to_f32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I128, TYPECODE_F32)),
            "std::num::__intrinsic_i128_to_f64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_I128, TYPECODE_F64)),

            // --- f32 --- 
            "std::num::__intrinsic_f32_add__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_ADD, TYPECODE_F32, TYPECODE_NONE)),
            "std::num::__intrinsic_f32_sub__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SUB, TYPECODE_F32, TYPECODE_NONE)),
            "std::num::__intrinsic_f32_mul__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_MUL, TYPECODE_F32, TYPECODE_NONE)),
            "std::num::__intrinsic_f32_div__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_DIV, TYPECODE_F32, TYPECODE_NONE)),
            "std::num::__intrinsic_f32_rem__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_REM, TYPECODE_F32, TYPECODE_NONE)),
            "std::num::__intrinsic_f32_neg__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NEG, TYPECODE_F32, TYPECODE_NONE)),
            "std::num::__intrinsic_f32_abs__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_ABS, TYPECODE_F32, TYPECODE_NONE)),
            "std::num::__intrinsic_f32_pow__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_POW, TYPECODE_F32, TYPECODE_NONE)),
            "std::num::__intrinsic_f32_eq__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_EQ, TYPECODE_F32, TYPECODE_BOOL)),
            "std::num::__intrinsic_f32_ne__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NE, TYPECODE_F32, TYPECODE_BOOL)),
            "std::num::__intrinsic_f32_lt__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_LT, TYPECODE_F32, TYPECODE_BOOL)),
            "std::num::__intrinsic_f32_le__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_LE, TYPECODE_F32, TYPECODE_BOOL)),
            "std::num::__intrinsic_f32_gt__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_GT, TYPECODE_F32, TYPECODE_BOOL)),
            "std::num::__intrinsic_f32_ge__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_GE, TYPECODE_F32, TYPECODE_BOOL)),
            // F32 Conversions
            "std::num::__intrinsic_f32_to_u8__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_F32, TYPECODE_U8)),
            "std::num::__intrinsic_f32_to_i8__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_F32, TYPECODE_I8)),
            "std::num::__intrinsic_f32_to_u16__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_F32, TYPECODE_U16)),
            "std::num::__intrinsic_f32_to_i16__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_F32, TYPECODE_I16)),
            "std::num::__intrinsic_f32_to_u32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_F32, TYPECODE_U32)),
            "std::num::__intrinsic_f32_to_i32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_F32, TYPECODE_I32)),
            "std::num::__intrinsic_f32_to_u64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_F32, TYPECODE_U64)),
            "std::num::__intrinsic_f32_to_i64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_F32, TYPECODE_I64)),
            "std::num::__intrinsic_f32_to_u128__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_F32, TYPECODE_U128)),
            "std::num::__intrinsic_f32_to_i128__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_F32, TYPECODE_I128)),
            "std::num::__intrinsic_f32_to_f64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_F32, TYPECODE_F64)),

            // --- f64 --- 
            "std::num::__intrinsic_f64_add__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_ADD, TYPECODE_F64, TYPECODE_NONE)),
            "std::num::__intrinsic_f64_sub__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_SUB, TYPECODE_F64, TYPECODE_NONE)),
            "std::num::__intrinsic_f64_mul__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_MUL, TYPECODE_F64, TYPECODE_NONE)),
            "std::num::__intrinsic_f64_div__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_DIV, TYPECODE_F64, TYPECODE_NONE)),
            "std::num::__intrinsic_f64_rem__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_REM, TYPECODE_F64, TYPECODE_NONE)),
            "std::num::__intrinsic_f64_neg__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NEG, TYPECODE_F64, TYPECODE_NONE)),
            "std::num::__intrinsic_f64_abs__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_ABS, TYPECODE_F64, TYPECODE_NONE)),
            "std::num::__intrinsic_f64_pow__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_POW, TYPECODE_F64, TYPECODE_NONE)),
            "std::num::__intrinsic_f64_eq__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_EQ, TYPECODE_F64, TYPECODE_BOOL)),
            "std::num::__intrinsic_f64_ne__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NE, TYPECODE_F64, TYPECODE_BOOL)),
            "std::num::__intrinsic_f64_lt__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_LT, TYPECODE_F64, TYPECODE_BOOL)),
            "std::num::__intrinsic_f64_le__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_LE, TYPECODE_F64, TYPECODE_BOOL)),
            "std::num::__intrinsic_f64_gt__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_GT, TYPECODE_F64, TYPECODE_BOOL)),
            "std::num::__intrinsic_f64_ge__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_GE, TYPECODE_F64, TYPECODE_BOOL)),
            // F64 Conversions
            "std::num::__intrinsic_f64_to_u8__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_F64, TYPECODE_U8)),
            "std::num::__intrinsic_f64_to_i8__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_F64, TYPECODE_I8)),
            "std::num::__intrinsic_f64_to_u16__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_F64, TYPECODE_U16)),
            "std::num::__intrinsic_f64_to_i16__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_F64, TYPECODE_I16)),
            "std::num::__intrinsic_f64_to_u32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_F64, TYPECODE_U32)),
            "std::num::__intrinsic_f64_to_i32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_F64, TYPECODE_I32)),
            "std::num::__intrinsic_f64_to_u64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_F64, TYPECODE_U64)),
            "std::num::__intrinsic_f64_to_i64__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_F64, TYPECODE_I64)),
            "std::num::__intrinsic_f64_to_u128__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_F64, TYPECODE_U128)),
            "std::num::__intrinsic_f64_to_i128__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_F64, TYPECODE_I128)),
            "std::num::__intrinsic_f64_to_f32__" => Some(encode_intrinsic_op(OPCAT_CONVERSION, OP_CONVERT, TYPECODE_F64, TYPECODE_F32)),

            // --- Boolean --- 
            // Use specific BOOL category and opcodes
            "crate::bool::__intrinsic_bool_and__" | "std::num::__intrinsic_bool_and__" => Some(encode_intrinsic_op(OPCAT_BOOLEAN, OP_BOOL_AND, TYPECODE_BOOL, TYPECODE_BOOL)),
            "crate::bool::__intrinsic_bool_or__" | "std::num::__intrinsic_bool_or__" => Some(encode_intrinsic_op(OPCAT_BOOLEAN, OP_BOOL_OR, TYPECODE_BOOL, TYPECODE_BOOL)),
            "crate::bool::__intrinsic_bool_xor__" | "std::num::__intrinsic_bool_xor__" => Some(encode_intrinsic_op(OPCAT_BOOLEAN, OP_BOOL_XOR, TYPECODE_BOOL, TYPECODE_BOOL)),
            "crate::bool::__intrinsic_bool_not__" | "std::num::__intrinsic_bool_not__" => Some(encode_intrinsic_op(OPCAT_BOOLEAN, OP_BOOL_NOT, TYPECODE_BOOL, TYPECODE_BOOL)),
            // Equality/Inequality still uses ARITH category with BOOL types
            "crate::bool::__intrinsic_bool_eq__" | "std::num::__intrinsic_bool_eq__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_EQ, TYPECODE_BOOL, TYPECODE_BOOL)),
            "crate::bool::__intrinsic_bool_ne__" | "std::num::__intrinsic_bool_ne__" => Some(encode_intrinsic_op(OPCAT_ARITHMETIC, OP_ARITH_NE, TYPECODE_BOOL, TYPECODE_BOOL)),

            // --- Runtime --- 
            "crate::panic::__intrinsic_panic__" | "std::panic::__intrinsic_panic__" => Some(encode_intrinsic_op(OPCAT_RUNTIME, OP_RUNTIME_PANIC, TYPECODE_NONE, TYPECODE_NONE)),
            "std::io::__intrinsic_println__" => Some(encode_intrinsic_op(OPCAT_RUNTIME, OP_RUNTIME_PRINTLN, TYPECODE_NONE, TYPECODE_NONE)),
            "std::io::__intrinsic_readln__" => Some(encode_intrinsic_op(OPCAT_RUNTIME, OP_RUNTIME_READLN, TYPECODE_NONE, TYPECODE_NONE)),

            // Default
            _ => {
                println!("Warning: Unhandled intrinsic path during encoding: {}", path);
                None
            }
        };

        // Store the fully encoded 64-bit data in the map
        if let Some(data) = encoded_static_data {
            intrinsic_op_map.insert(*symbol, data); 
        }
    }
    
    // Iterate and assign FunctionIds (simple counter for now)
    let mut func_ids = HashMap::new();
    for symbol in mir_module.functions.keys() {
        func_ids.insert(*symbol, func_id_counter);
        func_id_counter += 1;
    }

    // Lower each function
    for (symbol, graph) in &mir_module.functions { // Iterate over functions in the module
        let function_id = *func_ids.get(symbol).expect("Function ID should exist");
        let partition_id = partition_counter;
        println!(
            "Lowering function {:?} (ID: {}) to net partition {}",
            symbol,
            function_id,
            partition_id
        );
        let initial_config = lower_function(graph, partition_id, &intrinsic_op_map, function_id)?;
        partition_counter += 1;
        networks.insert(*symbol, initial_config);
    }

    Ok(CompiledNet {
        networks,
    })
}

/// Lowers a single MIR function graph into an interaction net configuration.
/// Creates the RootCON and connects parameter/return ports.
fn lower_function(
    graph: &MirGraph,
    partition_id: u16,
    intrinsic_op_map: &HashMap<Symbol, u64>,
    function_id: usize,
) -> Result<InitialNetConfig, LoweringError> {
    println!("[LOWER FUNC START] {:?}", graph.symbol);
    let mut builder = NetBuilder::new(graph, partition_id, intrinsic_op_map, function_id);

    println!("[LOWER FUNC] Before node loop. config.root = {:?}", builder.config.root);
    // Lower all nodes first (creates initial port mappings)
    for (node_id, node) in &graph.nodes {
        let _ = lower_mir_node(&mut builder, *node_id, node)?;
    }
    println!("[LOWER FUNC] After node loop. config.root = {:?}", builder.config.root);

    // --- RootCON Allocation ---
    let (root_con_idx, root_con_port) = builder.alloc_constructor(Constructor::new_null());
    builder.config.constructors[root_con_idx].principle = root_con_port;
    builder.config.root = root_con_port; // Set the root port for the config
    println!("[LOWER FUNC] After RootCON alloc. root_con_idx = {}, root_con_port = {:?}, config.root = {:?}", root_con_idx, root_con_port, builder.config.root);


    // --- Handle Parameter Connection & Mapping FIRST ---
    println!("[LOWER FUNC] Before Param handling. config.root = {:?}", builder.config.root);
    let mut param_final_output_port = Port::NULL; // Port where the parameter value emerges (e.g., DUP aux1)
    if let Some(param_node_id) = graph.parameter_node {
        let used_source_nodes: HashSet<NodeId> = graph.edges.iter().map(|e| e.from_node).collect();
        let is_param_return_value = match graph.return_port {
            Some((return_node_id, _)) => return_node_id == param_node_id,
            None => false,
        };
        let is_param_used = used_source_nodes.contains(&param_node_id) || is_param_return_value;

        if is_param_used {
            // --- Parameter is USED ---
            let (dup_idx, dup_principal) = builder.alloc_duplicator(Duplicator::new_null());
            let dup_aux1 = Port::left(NodeType::Duplicator, partition_id, dup_idx as u64);
            let dup_aux2 = Port::right(NodeType::Duplicator, partition_id, dup_idx as u64);
            builder.config.duplicators[dup_idx].principle = dup_principal;

            // Connect RootCON.left to Duplicator principal
            builder.config.constructors[root_con_idx].left = dup_principal;
            // *** ADD Wire for RootCON.left connection ***
            let wire_to_add = Wire(
                Port::left(NodeType::Constructor, partition_id, root_con_idx as u64),
                dup_principal,
            );
            builder.config.initial_wires.push(wire_to_add);
            println!("[LOWER FUNC DEBUG] Added RootCON.left wire: {:?}. Count: {}", wire_to_add, builder.config.initial_wires.len());
            println!("[LOWER FUNC] Param USED - Set RootCON(idx={}).left = {:?}", root_con_idx, dup_principal);

            // *** CRITICAL: Update the map NOW. The parameter value comes from dup_aux1. ***
            builder.map_output_port(param_node_id, PortIndex(0), dup_aux1);
            param_final_output_port = dup_aux1; // Store for potential return connection

            // Connect dangling aux port (dup_aux2) based on usage count
            let edge_usages = graph.edges.iter().filter(|e| e.from_node == param_node_id).count();
            let total_usages = edge_usages + if is_param_return_value { 1 } else { 0 };

            if total_usages == 0 {
                 return Err(LoweringError::Internal(format!("Internal Error: Parameter {:?} marked as used but has 0 total usages.", param_node_id)));
            } else if total_usages == 1 {
                 let (eraser_idx, eraser_port) = builder.alloc_eraser(Eraser::new_null());
                 builder.config.erasers[eraser_idx].principle = eraser_port;
                 let wire_to_add = Wire(dup_aux2, eraser_port);
                 builder.config.initial_wires.push(wire_to_add);
                 println!("[LOWER FUNC] Param USED ONCE. Added wire {:?}", wire_to_add);
            } else {
                 println!("[LOWER FUNC] Param USED >= 2. No eraser needed for aux2.");
            }
        } else {
            // --- Parameter is UNUSED ---
            let (eraser_idx, eraser_port) = builder.alloc_eraser(Eraser::new_null());
            builder.config.erasers[eraser_idx].principle = eraser_port;
            builder.config.constructors[root_con_idx].left = eraser_port;
            // *** ADD Wire for RootCON.left connection ***
            let wire_to_add = Wire(
                Port::left(NodeType::Constructor, partition_id, root_con_idx as u64),
                eraser_port,
            );
            builder.config.initial_wires.push(wire_to_add);
            println!("[LOWER FUNC DEBUG] Added RootCON.left wire: {:?}. Count: {}", wire_to_add, builder.config.initial_wires.len());
            println!("[LOWER FUNC] Param UNUSED - Set RootCON(idx={}).left = {:?}", root_con_idx, eraser_port);
            // Update map to point to the eraser? Or just leave it NULL/DEAD? Let's map it.
            // Although no edge should read from it, this might prevent errors if get_output is called.
            builder.map_output_port(param_node_id, PortIndex(0), eraser_port); // Map unused param output to eraser
            param_final_output_port = eraser_port; // Technically the 'value' comes from the eraser now
        }
    } else {
        // --- No Parameters ---
        let (left_eraser_idx, left_eraser_port) = builder.alloc_eraser(Eraser::new_null());
        builder.config.erasers[left_eraser_idx].principle = left_eraser_port;
        builder.config.constructors[root_con_idx].left = left_eraser_port;
        // *** ADD Wire for RootCON.left connection ***
        let wire_to_add = Wire(
            Port::left(NodeType::Constructor, partition_id, root_con_idx as u64),
            left_eraser_port,
        );
        builder.config.initial_wires.push(wire_to_add);
        println!("[LOWER FUNC DEBUG] Added RootCON.left wire: {:?}. Count: {}", wire_to_add, builder.config.initial_wires.len());
        println!("[LOWER FUNC] NO Param - Set RootCON(idx={}).left = {:?}", root_con_idx, left_eraser_port);
    }
    println!("[LOWER FUNC] After Param handling. config.root = {:?}. RootCON(idx={}).left = {:?}", builder.config.root, root_con_idx, builder.config.constructors[root_con_idx].left);

    // --- Handle Return Connection ---
    println!("[LOWER FUNC] Before Return handling. config.root = {:?}", builder.config.root);
    let return_target_port = match graph.return_port {
        Some((return_node_id, return_port_index)) => {
             builder.get_output_net_port(return_node_id, return_port_index)?
        }
        None => {
            let (eraser_idx, eraser_port) = builder.alloc_eraser(Eraser::new_null());
            builder.config.erasers[eraser_idx].principle = eraser_port;
            eraser_port
        }
    };
    builder.config.constructors[root_con_idx].right = return_target_port;
    // *** ADD Wire for RootCON.right connection ***
    let wire_to_add = Wire(
        Port::right(NodeType::Constructor, partition_id, root_con_idx as u64),
        return_target_port,
    );
    builder.config.initial_wires.push(wire_to_add);
    println!("[LOWER FUNC DEBUG] Added RootCON.right wire: {:?}. Count: {}", wire_to_add, builder.config.initial_wires.len());
    println!("[LOWER FUNC] After Return handling. Set RootCON(idx={}).right = {:?}. config.root = {:?}", root_con_idx, return_target_port, builder.config.root);


    // --- Lower all edges (create initial wires) ---
    println!("[LOWER FUNC] Before Edge lowering. config.root = {:?}", builder.config.root);
    // Iterate using graph's edge vector directly
    for edge_data in &graph.edges { // Reverted loop
        // ADDED DEBUG PRINT
        println!("[LOWER FUNC DEBUG] Processing edge: src={:?}({:?}) -> tgt={:?}({:?})",
                 edge_data.from_node, edge_data.from_port, // Corrected field names
                 edge_data.to_node, edge_data.to_port);     // Corrected field names

        // Removed the complex and erroneous parameter usage check from here.
        // Parameter connection logic is handled earlier.
        // The edge should always be lowered if it exists in graph.edges.

        lower_mir_edge(&mut builder, edge_data)?; // Re-borrow builder mutably
    }
    println!("[LOWER FUNC] After Edge lowering. config.root = {:?}", builder.config.root);

    // --- Erase unused outputs --- 
    println!("[LOWER FUNC] Starting unused output erasure pass.");
    let mut used_ports = HashSet::new();
    // Collect all ports used in wires
    for wire in &builder.config.initial_wires {
        used_ports.insert(wire.0.as_u64());
        used_ports.insert(wire.1.as_u64());
    }
    // The final return port (connected to RootCON.right) is also used
    used_ports.insert(builder.config.constructors[root_con_idx].right.as_u64());
    // Note: RootCON.left is handled implicitly by the parameter usage logic or explicit erasure.
    
    let mut new_erasers = Vec::new();

    // Check Constructor outputs
    for (idx, node) in builder.config.constructors.iter() {
        let principal_port = Port::principal(NodeType::Constructor, partition_id, idx as u64);
        let left_port = Port::left(NodeType::Constructor, partition_id, idx as u64);
        let right_port = Port::right(NodeType::Constructor, partition_id, idx as u64);

        // Check Principal Port
        let pp_key = principal_port.as_u64();
        let is_root_or_target = idx == root_con_idx || principal_port == builder.config.constructors[root_con_idx].right;
        if !is_root_or_target && principal_port != Port::NULL && !used_ports.contains(&pp_key) {
            println!("[ERASURE CHECK] Constructor P {:?} (Key: {}) - UNUSED", principal_port, pp_key);
            new_erasers.push(principal_port);
        } else if principal_port != Port::NULL {
            println!("[ERASURE CHECK] Constructor P {:?} (Key: {}) - USED (IsRootOrTarget: {})", principal_port, pp_key, is_root_or_target);
        }

        // Check Left Aux Port (from struct field)
        let left_aux_key = node.left.as_u64();
        if node.left != Port::NULL && !used_ports.contains(&left_aux_key) {
            println!("[ERASURE CHECK] Constructor L (struct) {:?} (Key: {}) - UNUSED", node.left, left_aux_key);
            new_erasers.push(node.left);
        } else if node.left != Port::NULL {
             println!("[ERASURE CHECK] Constructor L (struct) {:?} (Key: {}) - USED", node.left, left_aux_key);
        }
        // Check Left Aux Port (calculated)
        let left_aux_calc_key = left_port.as_u64();
        if left_port != Port::NULL && !used_ports.contains(&left_aux_calc_key) {
            println!("[ERASURE CHECK] Constructor L (calc) {:?} (Key: {}) - UNUSED", left_port, left_aux_calc_key);
            new_erasers.push(left_port);
        } else if left_port != Port::NULL {
             println!("[ERASURE CHECK] Constructor L (calc) {:?} (Key: {}) - USED", left_port, left_aux_calc_key);
        }

        // Check Right Aux Port (from struct field)
        let right_aux_key = node.right.as_u64();
        if node.right != Port::NULL && !used_ports.contains(&right_aux_key) {
            println!("[ERASURE CHECK] Constructor R (struct) {:?} (Key: {}) - UNUSED", node.right, right_aux_key);
            new_erasers.push(node.right);
        } else if node.right != Port::NULL {
             println!("[ERASURE CHECK] Constructor R (struct) {:?} (Key: {}) - USED", node.right, right_aux_key);
        }
         // Check Right Aux Port (calculated)
        let right_aux_calc_key = right_port.as_u64();
        if right_port != Port::NULL && !used_ports.contains(&right_aux_calc_key) {
             println!("[ERASURE CHECK] Constructor R (calc) {:?} (Key: {}) - UNUSED", right_port, right_aux_calc_key);
            new_erasers.push(right_port);
        } else if right_port != Port::NULL {
             println!("[ERASURE CHECK] Constructor R (calc) {:?} (Key: {}) - USED", right_port, right_aux_calc_key);
        }
    }
    // Check Duplicator outputs
    for (idx, node) in builder.config.duplicators.iter() {
        let left_port = Port::left(NodeType::Duplicator, partition_id, idx as u64);
        let right_port = Port::right(NodeType::Duplicator, partition_id, idx as u64);
        // Check Left Aux Port (from struct field)
        let left_aux_key = node.left.as_u64();
        if node.left != Port::NULL && !used_ports.contains(&left_aux_key) {
             println!("[ERASURE CHECK] Duplicator L (struct) {:?} (Key: {}) - UNUSED", node.left, left_aux_key);
            new_erasers.push(node.left);
        } else if node.left != Port::NULL {
             println!("[ERASURE CHECK] Duplicator L (struct) {:?} (Key: {}) - USED", node.left, left_aux_key);
        }
        // Check Left Aux Port (calculated)
        let left_aux_calc_key = left_port.as_u64();
        if left_port != Port::NULL && !used_ports.contains(&left_aux_calc_key) {
             println!("[ERASURE CHECK] Duplicator L (calc) {:?} (Key: {}) - UNUSED", left_port, left_aux_calc_key);
            new_erasers.push(left_port);
        } else if left_port != Port::NULL {
             println!("[ERASURE CHECK] Duplicator L (calc) {:?} (Key: {}) - USED", left_port, left_aux_calc_key);
        }

        // Check Right Aux Port (from struct field)
        let right_aux_key = node.right.as_u64();
        if node.right != Port::NULL && !used_ports.contains(&right_aux_key) {
             println!("[ERASURE CHECK] Duplicator R (struct) {:?} (Key: {}) - UNUSED", node.right, right_aux_key);
            new_erasers.push(node.right);
        } else if node.right != Port::NULL {
             println!("[ERASURE CHECK] Duplicator R (struct) {:?} (Key: {}) - USED", node.right, right_aux_key);
        }
         // Check Right Aux Port (calculated)
        let right_aux_calc_key = right_port.as_u64();
        if right_port != Port::NULL && !used_ports.contains(&right_aux_calc_key) {
             println!("[ERASURE CHECK] Duplicator R (calc) {:?} (Key: {}) - UNUSED", right_port, right_aux_calc_key);
            new_erasers.push(right_port);
        } else if right_port != Port::NULL {
             println!("[ERASURE CHECK] Duplicator R (calc) {:?} (Key: {}) - USED", right_port, right_aux_calc_key);
        }
    }
    // Check Static outputs (only principal)
    for (idx, _node) in builder.config.statics.iter() {
        let principal_port = Port::principal(NodeType::Static, partition_id, idx as u64);
        let pp_key = principal_port.as_u64();
        if principal_port != Port::NULL && !used_ports.contains(&pp_key) {
            println!("[ERASURE CHECK] Static P {:?} (Key: {}) - UNUSED", principal_port, pp_key);
            new_erasers.push(principal_port);
        } else if principal_port != Port::NULL {
             println!("[ERASURE CHECK] Static P {:?} (Key: {}) - USED", principal_port, pp_key);
        }
    }
    // Check Number outputs (only principal)
    for (idx, _node) in builder.config.numbers.iter() {
        let principal_port = Port::principal(NodeType::Number, partition_id, idx as u64);
        let pp_key = principal_port.as_u64();
        if principal_port != Port::NULL && !used_ports.contains(&pp_key) {
            println!("[ERASURE CHECK] Number P {:?} (Key: {}) - UNUSED", principal_port, pp_key);
            new_erasers.push(principal_port);
        } else if principal_port != Port::NULL {
             println!("[ERASURE CHECK] Number P {:?} (Key: {}) - USED", principal_port, pp_key);
        }
    }
     // Check Switch outputs (principal is output, left/right are inputs essentially)
     for (idx, _node) in builder.config.switches.iter() {
        let principal_port = Port::principal(NodeType::Switch, partition_id, idx as u64);
        let pp_key = principal_port.as_u64();
        if principal_port != Port::NULL && !used_ports.contains(&pp_key) {
            println!("[ERASURE CHECK] Switch P {:?} (Key: {}) - UNUSED", principal_port, pp_key);
            new_erasers.push(principal_port);
        } else if principal_port != Port::NULL {
             println!("[ERASURE CHECK] Switch P {:?} (Key: {}) - USED", principal_port, pp_key);
        }
    }
    // Add checks for Async, Pointer etc. if they have outputs

    // Remove duplicates before creating erasers
    new_erasers.sort_unstable_by_key(|p| p.as_u64());
    new_erasers.dedup();

    // Create erasers and wires for unused ports
    let added_eraser_count = new_erasers.len(); // Get length before iterating
    for unused_port in &new_erasers { // Iterate over slice
        println!("[LOWER FUNC] Erasing unused output port: {:?}", unused_port);
        let (eraser_idx, eraser_port) = builder.alloc_eraser(Eraser::new_null());
        builder.config.erasers[eraser_idx].principle = eraser_port; // Set eraser principal
        // Connect the unused port to the new eraser
        // Ensure we use the value from the slice (*unused_port)
        builder.config.initial_wires.push(Wire(*unused_port, eraser_port));
    }
    println!("[LOWER FUNC] Finished unused output erasure pass. Added {} erasers.", added_eraser_count);

    // Final check print (already added)
    println!(
        "[LOWER FUNC FINAL CHECK] {:?} config.root = {:?}, RootCON(idx={}).left = {:?}, RootCON(idx={}).right = {:?}", 
        graph.symbol, 
        builder.config.root, 
        root_con_idx, 
        builder.config.constructors[root_con_idx].left, 
        root_con_idx,
        builder.config.constructors[root_con_idx].right
    );

    Ok(builder.build())
}

/// Lowers a single MIR edge into an initial interaction net Wire.
fn lower_mir_edge(
    builder: &mut NetBuilder,
    edge: &MirEdge,
) -> Result<(), LoweringError> {
    let src_port = builder.get_output_net_port(edge.from_node, edge.from_port)?;
    let dst_port = builder.get_input_port(edge.to_node, edge.to_port)?;

    let wire_to_add = Wire(src_port, dst_port);
    // Log the edge being lowered, especially for the function call test
    println!(
        "Lowering edge: {:?} -> {:?} => Wire {:?}",
        edge.from_node,
        edge.to_node,
        wire_to_add
    );

    // Create the wire and add it to the initial configuration
    builder.config.initial_wires.push(wire_to_add);
    Ok(())
} 