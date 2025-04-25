mod builder;
mod config;
mod edges;
mod error;
mod nodes;
mod tests;

pub use config::InitialNetConfig;
pub use error::LoweringError;

use crate::node::{NodeType, Redex, Constructor, Duplicator, Eraser, Number, Static, Switch, Async};
use crate::port::Port;
use builder::NetBuilder;
use edges::create_redex_for_edge;
use nodes::{lower_mir_node, 
    OP_I64_ADD, OP_I64_SUB, OP_I64_MUL, OP_I64_DIV, OP_I64_REM, OP_I64_AND, OP_I64_OR, OP_I64_XOR, OP_I64_SHL, OP_I64_SHR, OP_I64_NEG, OP_I64_NOT, OP_I64_ABS, OP_I64_POW, OP_I64_EQ, OP_I64_NE, OP_I64_LT, OP_I64_LE, OP_I64_GT, OP_I64_GE,
    OP_U64_ADD, OP_U64_SUB, OP_U64_MUL, OP_U64_DIV, OP_U64_REM, OP_U64_AND, OP_U64_OR, OP_U64_XOR, OP_U64_SHL, OP_U64_SHR, OP_U64_NOT, OP_U64_POW, OP_U64_EQ, OP_U64_NE, OP_U64_LT, OP_U64_LE, OP_U64_GT, OP_U64_GE,
    OP_F64_ADD, OP_F64_SUB, OP_F64_MUL, OP_F64_DIV, OP_F64_REM, OP_F64_NEG, OP_F64_ABS, OP_F64_POW, OP_F64_EQ, OP_F64_NE, OP_F64_LT, OP_F64_LE, OP_F64_GT, OP_F64_GE,
    OP_I32_ADD, OP_I32_SUB, OP_I32_MUL, OP_I32_DIV, OP_I32_REM, OP_I32_AND, OP_I32_OR, OP_I32_XOR, OP_I32_SHL, OP_I32_SHR, OP_I32_NEG, OP_I32_NOT, OP_I32_ABS, OP_I32_POW, OP_I32_EQ, OP_I32_NE, OP_I32_LT, OP_I32_LE, OP_I32_GT, OP_I32_GE,
    OP_U32_ADD, OP_U32_SUB, OP_U32_MUL, OP_U32_DIV, OP_U32_REM, OP_U32_AND, OP_U32_OR, OP_U32_XOR, OP_U32_SHL, OP_U32_SHR, OP_U32_NOT, OP_U32_POW, OP_U32_EQ, OP_U32_NE, OP_U32_LT, OP_U32_LE, OP_U32_GT, OP_U32_GE,
    OP_F32_ADD, OP_F32_SUB, OP_F32_MUL, OP_F32_DIV, OP_F32_REM, OP_F32_NEG, OP_F32_ABS, OP_F32_POW, OP_F32_EQ, OP_F32_NE, OP_F32_LT, OP_F32_LE, OP_F32_GT, OP_F32_GE,
    OP_BOOL_AND, OP_BOOL_OR, OP_BOOL_XOR, OP_BOOL_NOT, OP_BOOL_EQ, OP_BOOL_NE,
    OP_I16_ADD, OP_I16_SUB, OP_I16_MUL, OP_I16_DIV, OP_I16_REM, OP_I16_AND, OP_I16_OR, OP_I16_XOR, OP_I16_SHL, OP_I16_SHR, OP_I16_NEG, OP_I16_NOT, OP_I16_ABS, OP_I16_POW, OP_I16_EQ, OP_I16_NE, OP_I16_LT, OP_I16_LE, OP_I16_GT, OP_I16_GE,
    OP_U16_ADD, OP_U16_SUB, OP_U16_MUL, OP_U16_DIV, OP_U16_REM, OP_U16_AND, OP_U16_OR, OP_U16_XOR, OP_U16_SHL, OP_U16_SHR, OP_U16_NOT, OP_U16_POW, OP_U16_EQ, OP_U16_NE, OP_U16_LT, OP_U16_LE, OP_U16_GT, OP_U16_GE,
    OP_I8_ADD, OP_I8_SUB, OP_I8_MUL, OP_I8_DIV, OP_I8_REM, OP_I8_AND, OP_I8_OR, OP_I8_XOR, OP_I8_SHL, OP_I8_SHR, OP_I8_NEG, OP_I8_NOT, OP_I8_ABS, OP_I8_POW, OP_I8_EQ, OP_I8_NE, OP_I8_LT, OP_I8_LE, OP_I8_GT, OP_I8_GE,
    OP_U8_ADD, OP_U8_SUB, OP_U8_MUL, OP_U8_DIV, OP_U8_REM, OP_U8_AND, OP_U8_OR, OP_U8_XOR, OP_U8_SHL, OP_U8_SHR, OP_U8_NOT, OP_U8_POW, OP_U8_EQ, OP_U8_NE, OP_U8_LT, OP_U8_LE, OP_U8_GT, OP_U8_GE,
    OP_I128_ADD, OP_I128_SUB, OP_I128_MUL, OP_I128_DIV, OP_I128_REM, OP_I128_AND, OP_I128_OR, OP_I128_XOR, OP_I128_SHL, OP_I128_SHR, OP_I128_NEG, OP_I128_NOT, OP_I128_ABS, OP_I128_POW, OP_I128_EQ, OP_I128_NE, OP_I128_LT, OP_I128_LE, OP_I128_GT, OP_I128_GE,
    OP_U128_ADD, OP_U128_SUB, OP_U128_MUL, OP_U128_DIV, OP_U128_REM, OP_U128_AND, OP_U128_OR, OP_U128_XOR, OP_U128_SHL, OP_U128_SHR, OP_U128_NOT, OP_U128_POW, OP_U128_EQ, OP_U128_NE, OP_U128_LT, OP_U128_LE, OP_U128_GT, OP_U128_GE,
    OP_PANIC
};
use parallax_mir::mir::{MirGraph, MirModule, NodeId, MirNode, PortIndex, MirEdge};
use parallax_resolve::types::Symbol;
use std::collections::HashMap;
use crate::CompiledNet;

/// Lowers a full MIR module into a compiled interaction net artifact.
///
/// Each function graph is lowered independently into an `InitialNetConfig`,
/// which contains the necessary interaction net nodes and initial redexes
/// to represent the function's computation according to the structural encoding rules.
///
/// # Arguments
/// * `mir_module` - A reference to the MIR module containing functions to lower.
///
/// # Returns
/// A `Result` containing the compiled interaction net artifact.
pub fn lower_module(
    mir_module: &MirModule,
) -> Result<CompiledNet, LoweringError> {
    let mut networks = HashMap::new();
    let mut partition_counter: u16 = 0;
    let mut func_id_counter: usize = 0;

    // --- Build Intrinsic Op Map --- 
    let mut intrinsic_op_map = HashMap::new();
    for (path, symbol) in &mir_module.intrinsics {
        let op_code = match path.as_str() {
            // i64
            "std::num::__intrinsic_i64_add__" => Some(OP_I64_ADD),
            "std::num::__intrinsic_i64_sub__" => Some(OP_I64_SUB),
            "std::num::__intrinsic_i64_mul__" => Some(OP_I64_MUL),
            "std::num::__intrinsic_i64_div__" => Some(OP_I64_DIV),
            "std::num::__intrinsic_i64_rem__" => Some(OP_I64_REM),
            "std::num::__intrinsic_i64_and__" => Some(OP_I64_AND),
            "std::num::__intrinsic_i64_or__" => Some(OP_I64_OR),
            "std::num::__intrinsic_i64_xor__" => Some(OP_I64_XOR),
            "std::num::__intrinsic_i64_shl__" => Some(OP_I64_SHL),
            "std::num::__intrinsic_i64_shr__" => Some(OP_I64_SHR),
            "std::num::__intrinsic_i64_neg__" => Some(OP_I64_NEG),
            "std::num::__intrinsic_i64_not__" => Some(OP_I64_NOT),
            "std::num::__intrinsic_i64_abs__" => Some(OP_I64_ABS),
            "std::num::__intrinsic_i64_pow__" => Some(OP_I64_POW),
            "std::num::__intrinsic_i64_eq__" => Some(OP_I64_EQ),
            "std::num::__intrinsic_i64_ne__" => Some(OP_I64_NE),
            "std::num::__intrinsic_i64_lt__" => Some(OP_I64_LT),
            "std::num::__intrinsic_i64_le__" => Some(OP_I64_LE),
            "std::num::__intrinsic_i64_gt__" => Some(OP_I64_GT),
            "std::num::__intrinsic_i64_ge__" => Some(OP_I64_GE),
            // u64
            "std::num::__intrinsic_u64_add__" => Some(OP_U64_ADD),
            "std::num::__intrinsic_u64_sub__" => Some(OP_U64_SUB),
            "std::num::__intrinsic_u64_mul__" => Some(OP_U64_MUL),
            "std::num::__intrinsic_u64_div__" => Some(OP_U64_DIV),
            "std::num::__intrinsic_u64_rem__" => Some(OP_U64_REM),
            "std::num::__intrinsic_u64_and__" => Some(OP_U64_AND),
            "std::num::__intrinsic_u64_or__" => Some(OP_U64_OR),
            "std::num::__intrinsic_u64_xor__" => Some(OP_U64_XOR),
            "std::num::__intrinsic_u64_shl__" => Some(OP_U64_SHL),
            "std::num::__intrinsic_u64_shr__" => Some(OP_U64_SHR),
            "std::num::__intrinsic_u64_not__" => Some(OP_U64_NOT),
            "std::num::__intrinsic_u64_pow__" => Some(OP_U64_POW),
            "std::num::__intrinsic_u64_eq__" => Some(OP_U64_EQ),
            "std::num::__intrinsic_u64_ne__" => Some(OP_U64_NE),
            "std::num::__intrinsic_u64_lt__" => Some(OP_U64_LT),
            "std::num::__intrinsic_u64_le__" => Some(OP_U64_LE),
            "std::num::__intrinsic_u64_gt__" => Some(OP_U64_GT),
            "std::num::__intrinsic_u64_ge__" => Some(OP_U64_GE),
            // f64
            "std::num::__intrinsic_f64_add__" => Some(OP_F64_ADD),
            "std::num::__intrinsic_f64_sub__" => Some(OP_F64_SUB),
            "std::num::__intrinsic_f64_mul__" => Some(OP_F64_MUL),
            "std::num::__intrinsic_f64_div__" => Some(OP_F64_DIV),
            "std::num::__intrinsic_f64_rem__" => Some(OP_F64_REM),
            "std::num::__intrinsic_f64_neg__" => Some(OP_F64_NEG),
            "std::num::__intrinsic_f64_abs__" => Some(OP_F64_ABS),
            "std::num::__intrinsic_f64_pow__" => Some(OP_F64_POW),
            "std::num::__intrinsic_f64_eq__" => Some(OP_F64_EQ),
            "std::num::__intrinsic_f64_ne__" => Some(OP_F64_NE),
            "std::num::__intrinsic_f64_lt__" => Some(OP_F64_LT),
            "std::num::__intrinsic_f64_le__" => Some(OP_F64_LE),
            "std::num::__intrinsic_f64_gt__" => Some(OP_F64_GT),
            "std::num::__intrinsic_f64_ge__" => Some(OP_F64_GE),
            // i32
            "std::num::__intrinsic_i32_add__" => Some(OP_I32_ADD),
            "std::num::__intrinsic_i32_sub__" => Some(OP_I32_SUB),
            "std::num::__intrinsic_i32_mul__" => Some(OP_I32_MUL),
            "std::num::__intrinsic_i32_div__" => Some(OP_I32_DIV),
            "std::num::__intrinsic_i32_rem__" => Some(OP_I32_REM),
            "std::num::__intrinsic_i32_and__" => Some(OP_I32_AND),
            "std::num::__intrinsic_i32_or__" => Some(OP_I32_OR),
            "std::num::__intrinsic_i32_xor__" => Some(OP_I32_XOR),
            "std::num::__intrinsic_i32_shl__" => Some(OP_I32_SHL),
            "std::num::__intrinsic_i32_shr__" => Some(OP_I32_SHR),
            "std::num::__intrinsic_i32_neg__" => Some(OP_I32_NEG),
            "std::num::__intrinsic_i32_not__" => Some(OP_I32_NOT),
            "std::num::__intrinsic_i32_abs__" => Some(OP_I32_ABS),
            "std::num::__intrinsic_i32_pow__" => Some(OP_I32_POW),
            "std::num::__intrinsic_i32_eq__" => Some(OP_I32_EQ),
            "std::num::__intrinsic_i32_ne__" => Some(OP_I32_NE),
            "std::num::__intrinsic_i32_lt__" => Some(OP_I32_LT),
            "std::num::__intrinsic_i32_le__" => Some(OP_I32_LE),
            "std::num::__intrinsic_i32_gt__" => Some(OP_I32_GT),
            "std::num::__intrinsic_i32_ge__" => Some(OP_I32_GE),
            // u32
            "std::num::__intrinsic_u32_add__" => Some(OP_U32_ADD),
            "std::num::__intrinsic_u32_sub__" => Some(OP_U32_SUB),
            "std::num::__intrinsic_u32_mul__" => Some(OP_U32_MUL),
            "std::num::__intrinsic_u32_div__" => Some(OP_U32_DIV),
            "std::num::__intrinsic_u32_rem__" => Some(OP_U32_REM),
            "std::num::__intrinsic_u32_and__" => Some(OP_U32_AND),
            "std::num::__intrinsic_u32_or__" => Some(OP_U32_OR),
            "std::num::__intrinsic_u32_xor__" => Some(OP_U32_XOR),
            "std::num::__intrinsic_u32_shl__" => Some(OP_U32_SHL),
            "std::num::__intrinsic_u32_shr__" => Some(OP_U32_SHR),
            "std::num::__intrinsic_u32_not__" => Some(OP_U32_NOT),
            "std::num::__intrinsic_u32_pow__" => Some(OP_U32_POW),
            "std::num::__intrinsic_u32_eq__" => Some(OP_U32_EQ),
            "std::num::__intrinsic_u32_ne__" => Some(OP_U32_NE),
            "std::num::__intrinsic_u32_lt__" => Some(OP_U32_LT),
            "std::num::__intrinsic_u32_le__" => Some(OP_U32_LE),
            "std::num::__intrinsic_u32_gt__" => Some(OP_U32_GT),
            "std::num::__intrinsic_u32_ge__" => Some(OP_U32_GE),
            // f32
            "std::num::__intrinsic_f32_add__" => Some(OP_F32_ADD),
            "std::num::__intrinsic_f32_sub__" => Some(OP_F32_SUB),
            "std::num::__intrinsic_f32_mul__" => Some(OP_F32_MUL),
            "std::num::__intrinsic_f32_div__" => Some(OP_F32_DIV),
            "std::num::__intrinsic_f32_rem__" => Some(OP_F32_REM),
            "std::num::__intrinsic_f32_neg__" => Some(OP_F32_NEG),
            "std::num::__intrinsic_f32_abs__" => Some(OP_F32_ABS),
            "std::num::__intrinsic_f32_pow__" => Some(OP_F32_POW),
            "std::num::__intrinsic_f32_eq__" => Some(OP_F32_EQ),
            "std::num::__intrinsic_f32_ne__" => Some(OP_F32_NE),
            "std::num::__intrinsic_f32_lt__" => Some(OP_F32_LT),
            "std::num::__intrinsic_f32_le__" => Some(OP_F32_LE),
            "std::num::__intrinsic_f32_gt__" => Some(OP_F32_GT),
            "std::num::__intrinsic_f32_ge__" => Some(OP_F32_GE),
            // bool
            "crate::bool::__intrinsic_bool_and__" => Some(OP_BOOL_AND),
            "crate::bool::__intrinsic_bool_or__" => Some(OP_BOOL_OR),
            "crate::bool::__intrinsic_bool_xor__" => Some(OP_BOOL_XOR),
            "crate::bool::__intrinsic_bool_not__" => Some(OP_BOOL_NOT),
            "crate::bool::__intrinsic_bool_eq__" => Some(OP_BOOL_EQ),
            "crate::bool::__intrinsic_bool_ne__" => Some(OP_BOOL_NE),
            // i16
            "std::num::__intrinsic_i16_add__" => Some(OP_I16_ADD),
            "std::num::__intrinsic_i16_sub__" => Some(OP_I16_SUB),
            "std::num::__intrinsic_i16_mul__" => Some(OP_I16_MUL),
            "std::num::__intrinsic_i16_div__" => Some(OP_I16_DIV),
            "std::num::__intrinsic_i16_rem__" => Some(OP_I16_REM),
            "std::num::__intrinsic_i16_and__" => Some(OP_I16_AND),
            "std::num::__intrinsic_i16_or__" => Some(OP_I16_OR),
            "std::num::__intrinsic_i16_xor__" => Some(OP_I16_XOR),
            "std::num::__intrinsic_i16_shl__" => Some(OP_I16_SHL),
            "std::num::__intrinsic_i16_shr__" => Some(OP_I16_SHR),
            "std::num::__intrinsic_i16_neg__" => Some(OP_I16_NEG),
            "std::num::__intrinsic_i16_not__" => Some(OP_I16_NOT),
            "std::num::__intrinsic_i16_abs__" => Some(OP_I16_ABS),
            "std::num::__intrinsic_i16_pow__" => Some(OP_I16_POW),
            "std::num::__intrinsic_i16_eq__" => Some(OP_I16_EQ),
            "std::num::__intrinsic_i16_ne__" => Some(OP_I16_NE),
            "std::num::__intrinsic_i16_lt__" => Some(OP_I16_LT),
            "std::num::__intrinsic_i16_le__" => Some(OP_I16_LE),
            "std::num::__intrinsic_i16_gt__" => Some(OP_I16_GT),
            "std::num::__intrinsic_i16_ge__" => Some(OP_I16_GE),
            // u16
            "std::num::__intrinsic_u16_add__" => Some(OP_U16_ADD),
            "std::num::__intrinsic_u16_sub__" => Some(OP_U16_SUB),
            "std::num::__intrinsic_u16_mul__" => Some(OP_U16_MUL),
            "std::num::__intrinsic_u16_div__" => Some(OP_U16_DIV),
            "std::num::__intrinsic_u16_rem__" => Some(OP_U16_REM),
            "std::num::__intrinsic_u16_and__" => Some(OP_U16_AND),
            "std::num::__intrinsic_u16_or__" => Some(OP_U16_OR),
            "std::num::__intrinsic_u16_xor__" => Some(OP_U16_XOR),
            "std::num::__intrinsic_u16_shl__" => Some(OP_U16_SHL),
            "std::num::__intrinsic_u16_shr__" => Some(OP_U16_SHR),
            "std::num::__intrinsic_u16_not__" => Some(OP_U16_NOT),
            "std::num::__intrinsic_u16_pow__" => Some(OP_U16_POW),
            "std::num::__intrinsic_u16_eq__" => Some(OP_U16_EQ),
            "std::num::__intrinsic_u16_ne__" => Some(OP_U16_NE),
            "std::num::__intrinsic_u16_lt__" => Some(OP_U16_LT),
            "std::num::__intrinsic_u16_le__" => Some(OP_U16_LE),
            "std::num::__intrinsic_u16_gt__" => Some(OP_U16_GT),
            "std::num::__intrinsic_u16_ge__" => Some(OP_U16_GE),
            // i8
            "std::num::__intrinsic_i8_add__" => Some(OP_I8_ADD),
            "std::num::__intrinsic_i8_sub__" => Some(OP_I8_SUB),
            "std::num::__intrinsic_i8_mul__" => Some(OP_I8_MUL),
            "std::num::__intrinsic_i8_div__" => Some(OP_I8_DIV),
            "std::num::__intrinsic_i8_rem__" => Some(OP_I8_REM),
            "std::num::__intrinsic_i8_and__" => Some(OP_I8_AND),
            "std::num::__intrinsic_i8_or__" => Some(OP_I8_OR),
            "std::num::__intrinsic_i8_xor__" => Some(OP_I8_XOR),
            "std::num::__intrinsic_i8_shl__" => Some(OP_I8_SHL),
            "std::num::__intrinsic_i8_shr__" => Some(OP_I8_SHR),
            "std::num::__intrinsic_i8_neg__" => Some(OP_I8_NEG),
            "std::num::__intrinsic_i8_not__" => Some(OP_I8_NOT),
            "std::num::__intrinsic_i8_abs__" => Some(OP_I8_ABS),
            "std::num::__intrinsic_i8_pow__" => Some(OP_I8_POW),
            "std::num::__intrinsic_i8_eq__" => Some(OP_I8_EQ),
            "std::num::__intrinsic_i8_ne__" => Some(OP_I8_NE),
            "std::num::__intrinsic_i8_lt__" => Some(OP_I8_LT),
            "std::num::__intrinsic_i8_le__" => Some(OP_I8_LE),
            "std::num::__intrinsic_i8_gt__" => Some(OP_I8_GT),
            "std::num::__intrinsic_i8_ge__" => Some(OP_I8_GE),
            // u8
            "std::num::__intrinsic_u8_add__" => Some(OP_U8_ADD),
            "std::num::__intrinsic_u8_sub__" => Some(OP_U8_SUB),
            "std::num::__intrinsic_u8_mul__" => Some(OP_U8_MUL),
            "std::num::__intrinsic_u8_div__" => Some(OP_U8_DIV),
            "std::num::__intrinsic_u8_rem__" => Some(OP_U8_REM),
            "std::num::__intrinsic_u8_and__" => Some(OP_U8_AND),
            "std::num::__intrinsic_u8_or__" => Some(OP_U8_OR),
            "std::num::__intrinsic_u8_xor__" => Some(OP_U8_XOR),
            "std::num::__intrinsic_u8_shl__" => Some(OP_U8_SHL),
            "std::num::__intrinsic_u8_shr__" => Some(OP_U8_SHR),
            "std::num::__intrinsic_u8_not__" => Some(OP_U8_NOT),
            "std::num::__intrinsic_u8_pow__" => Some(OP_U8_POW),
            "std::num::__intrinsic_u8_eq__" => Some(OP_U8_EQ),
            "std::num::__intrinsic_u8_ne__" => Some(OP_U8_NE),
            "std::num::__intrinsic_u8_lt__" => Some(OP_U8_LT),
            "std::num::__intrinsic_u8_le__" => Some(OP_U8_LE),
            "std::num::__intrinsic_u8_gt__" => Some(OP_U8_GT),
            "std::num::__intrinsic_u8_ge__" => Some(OP_U8_GE),
            // i128
            "std::num::__intrinsic_i128_add__" => Some(OP_I128_ADD),
            "std::num::__intrinsic_i128_sub__" => Some(OP_I128_SUB),
            "std::num::__intrinsic_i128_mul__" => Some(OP_I128_MUL),
            "std::num::__intrinsic_i128_div__" => Some(OP_I128_DIV),
            "std::num::__intrinsic_i128_rem__" => Some(OP_I128_REM),
            "std::num::__intrinsic_i128_and__" => Some(OP_I128_AND),
            "std::num::__intrinsic_i128_or__" => Some(OP_I128_OR),
            "std::num::__intrinsic_i128_xor__" => Some(OP_I128_XOR),
            "std::num::__intrinsic_i128_shl__" => Some(OP_I128_SHL),
            "std::num::__intrinsic_i128_shr__" => Some(OP_I128_SHR),
            "std::num::__intrinsic_i128_neg__" => Some(OP_I128_NEG),
            "std::num::__intrinsic_i128_not__" => Some(OP_I128_NOT),
            "std::num::__intrinsic_i128_abs__" => Some(OP_I128_ABS),
            "std::num::__intrinsic_i128_pow__" => Some(OP_I128_POW),
            "std::num::__intrinsic_i128_eq__" => Some(OP_I128_EQ),
            "std::num::__intrinsic_i128_ne__" => Some(OP_I128_NE),
            "std::num::__intrinsic_i128_lt__" => Some(OP_I128_LT),
            "std::num::__intrinsic_i128_le__" => Some(OP_I128_LE),
            "std::num::__intrinsic_i128_gt__" => Some(OP_I128_GT),
            "std::num::__intrinsic_i128_ge__" => Some(OP_I128_GE),
            // u128
            "std::num::__intrinsic_u128_add__" => Some(OP_U128_ADD),
            "std::num::__intrinsic_u128_sub__" => Some(OP_U128_SUB),
            "std::num::__intrinsic_u128_mul__" => Some(OP_U128_MUL),
            "std::num::__intrinsic_u128_div__" => Some(OP_U128_DIV),
            "std::num::__intrinsic_u128_rem__" => Some(OP_U128_REM),
            "std::num::__intrinsic_u128_and__" => Some(OP_U128_AND),
            "std::num::__intrinsic_u128_or__" => Some(OP_U128_OR),
            "std::num::__intrinsic_u128_xor__" => Some(OP_U128_XOR),
            "std::num::__intrinsic_u128_shl__" => Some(OP_U128_SHL),
            "std::num::__intrinsic_u128_shr__" => Some(OP_U128_SHR),
            "std::num::__intrinsic_u128_not__" => Some(OP_U128_NOT),
            "std::num::__intrinsic_u128_pow__" => Some(OP_U128_POW),
            "std::num::__intrinsic_u128_eq__" => Some(OP_U128_EQ),
            "std::num::__intrinsic_u128_ne__" => Some(OP_U128_NE),
            "std::num::__intrinsic_u128_lt__" => Some(OP_U128_LT),
            "std::num::__intrinsic_u128_le__" => Some(OP_U128_LE),
            "std::num::__intrinsic_u128_gt__" => Some(OP_U128_GT),
            "std::num::__intrinsic_u128_ge__" => Some(OP_U128_GE),
            // Panic
            "crate::panic::__intrinsic_panic__" => Some(OP_PANIC),
            _ => {
                println!("Warning: Unhandled intrinsic path: {}", path);
                None
            }
        };
        if let Some(code) = op_code {
            intrinsic_op_map.insert(*symbol, code);
        }
    }
    
    // Iterate and assign FunctionIds (simple counter for now)
    let mut func_ids = HashMap::new();
    for symbol in mir_module.functions.keys() {
        func_ids.insert(*symbol, func_id_counter);
        func_id_counter += 1;
    }

    // Lower each function
    for (symbol, graph) in &mir_module.functions {
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
    let mut builder = NetBuilder::new(graph, partition_id, intrinsic_op_map, function_id);

    // Lower all nodes first
    for (node_id, node) in &graph.nodes {
        lower_mir_node(&mut builder, *node_id, node)?;
    }

    // Lower all edges (create initial redexes)
    for edge in &graph.edges {
        lower_mir_edge(&mut builder, edge)?;
    }

    // Handle Root connection (Parameters and Return)
    if let Some(param_node_id) = graph.parameter_node {
        // Get the net port corresponding to the *single* output of the parameter node
        // (which was likely lowered to a Duplicator)
        let param_source_port = builder.get_output_net_port(param_node_id, PortIndex(0))?;

        // Allocate the RootCON
        let (root_con_idx, root_con_port) = builder.alloc_constructor(Constructor::new_null());
        builder.config.constructors[root_con_idx].principle = root_con_port;
        builder.config.root = root_con_port; // Set the root port for the config

        // Connect RootCON.left (arg input) to the parameter source port
        // Connect RootCON.right (return output) based on graph.return_port
        let return_target_port = match graph.return_port {
            Some((return_node_id, return_port_index)) => {
                builder.get_output_net_port(return_node_id, return_port_index)?
            }
            None => {
                // Function might not return (e.g., ends in Unreachable).
                // Connect RootCON.right to an Eraser.
                let (eraser_idx, eraser_port) = builder.alloc_eraser(Eraser::new_null());
                builder.config.erasers[eraser_idx].principle = eraser_port;
                eraser_port // Target the eraser
            }
        };

        // Update RootCON ports
        builder.config.constructors[root_con_idx].left = param_source_port;
        builder.config.constructors[root_con_idx].right = return_target_port;

        // Add initial redex: (RootCON.left, ParamSource)
        builder.config.initial_redexes.push(Redex(
            Port::left(NodeType::Constructor, partition_id, root_con_idx as u64),
            param_source_port,
        ));
        // Add initial redex: (RootCON.right, ReturnTarget)
        builder.config.initial_redexes.push(Redex(
            Port::right(NodeType::Constructor, partition_id, root_con_idx as u64),
            return_target_port,
        ));

    } else {
        // No parameters, potentially just returning a constant or diverging.
        // We still need a RootCON for the function interface.
        let (root_con_idx, root_con_port) = builder.alloc_constructor(Constructor::new_null());
        builder.config.constructors[root_con_idx].principle = root_con_port;
        builder.config.root = root_con_port; // Set the root port

        // Connect RootCON.left to an Eraser (no parameters)
        let (left_eraser_idx, left_eraser_port) = builder.alloc_eraser(Eraser::new_null());
        builder.config.erasers[left_eraser_idx].principle = left_eraser_port;
        builder.config.constructors[root_con_idx].left = left_eraser_port;

        // Connect RootCON.right to return value or eraser
        let return_target_port = match graph.return_port {
            Some((return_node_id, return_port_index)) => {
                builder.get_output_net_port(return_node_id, return_port_index)?
            }
            None => {
                let (right_eraser_idx, right_eraser_port) = builder.alloc_eraser(Eraser::new_null());
                builder.config.erasers[right_eraser_idx].principle = right_eraser_port;
                right_eraser_port
            }
        };
        builder.config.constructors[root_con_idx].right = return_target_port;

        // Add initial redexes for the erasers/return value connected to RootCON
        builder.config.initial_redexes.push(Redex(
            Port::left(NodeType::Constructor, partition_id, root_con_idx as u64),
            left_eraser_port,
        ));
        builder.config.initial_redexes.push(Redex(
            Port::right(NodeType::Constructor, partition_id, root_con_idx as u64),
            return_target_port,
        ));
    }

    Ok(builder.build())
}

/// Lowers a single MIR edge into an initial interaction net Redex.
fn lower_mir_edge(
    builder: &mut NetBuilder,
    edge: &MirEdge,
) -> Result<(), LoweringError> {
    create_redex_for_edge(builder, edge)
} 