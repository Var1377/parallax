use super::constants::*;
use super::helpers::{get_aux_port, alloc_static_bool, get_number_data, alloc_number, get_static_data, decode_tag, decode_data};
use crate::inet::*;
use crate::inet::manager::AllPartitions;
use crate::inet::worker::Worker;
use crate::inet::CompiledDefs;
use crate::inet::reductions::{remove_node, annihilate_any, connect, add_redex_to_partition};
use parallax_net::{node::{Static, Constructor, Duplicator, Number, NodeType}, port::{Port, PortType}, Redex};
use parking_lot::RwLockReadGuard;
use log;


/// Dispatches Static(IntrinsicOp) ~ X interactions based on the op_code.
#[inline]
pub(super) unsafe fn dispatch_intrinsic_op(
    redex: Redex,
    s_port: Port,
    op_code: u64,
    other_port: Port,
    worker: &Worker,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
     log::trace!("Rule: Static(Intrinsic|Op={}) ~ {:?}", op_code, other_port);

     // Determine the caller port (which receives the result or is annihilated)
     let caller_port = if redex.0 == s_port { redex.1 } else { redex.0 };

    // Ensure the other port is a Constructor (expected AppCON head)
    if NodeType::from_u8(other_port.node_type()) != Some(NodeType::Constructor) {
        log::error!("Intrinsic Op {} expected Constructor argument, got {:?}. Annihilating.", op_code, other_port);
        remove_node(s_port, read_guard);
        annihilate_any(other_port, read_guard); // Annihilate whatever it was
        if caller_port != other_port { // Caller might be the same if something went very wrong
             annihilate_any(caller_port, read_guard);
        }
        return;
    }

    match op_code {
        // --- i64 Ops ---
        OP_I64_ADD => intrinsic_i64_binop(s_port, other_port, caller_port, |a, b| a.wrapping_add(b), read_guard),
        OP_I64_SUB => intrinsic_i64_binop(s_port, other_port, caller_port, |a, b| a.wrapping_sub(b), read_guard),
        OP_I64_MUL => intrinsic_i64_binop(s_port, other_port, caller_port, |a, b| a.wrapping_mul(b), read_guard),
        OP_I64_DIV => intrinsic_i64_binop(s_port, other_port, caller_port, |a, b| if b == 0 { 0 } else { a.wrapping_div(b) }, read_guard),
        OP_I64_REM => intrinsic_i64_binop(s_port, other_port, caller_port, |a, b| if b == 0 { 0 } else { a.wrapping_rem(b) }, read_guard),
        OP_I64_AND => intrinsic_i64_binop(s_port, other_port, caller_port, |a, b| a & b, read_guard),
        OP_I64_OR  => intrinsic_i64_binop(s_port, other_port, caller_port, |a, b| a | b, read_guard),
        OP_I64_XOR => intrinsic_i64_binop(s_port, other_port, caller_port, |a, b| a ^ b, read_guard),
        OP_I64_SHL => intrinsic_i64_binop(s_port, other_port, caller_port, |a, b| a.wrapping_shl(b as u32), read_guard),
        OP_I64_SHR => intrinsic_i64_binop(s_port, other_port, caller_port, |a, b| a.wrapping_shr(b as u32), read_guard),
        OP_I64_POW => intrinsic_i64_binop(s_port, other_port, caller_port, |a, b| a.wrapping_pow(b as u32), read_guard),
        OP_I64_NEG => intrinsic_i64_unop(s_port, other_port, caller_port, |a| a.wrapping_neg(), read_guard),
        OP_I64_NOT => intrinsic_i64_unop(s_port, other_port, caller_port, |a| !a, read_guard),
        OP_I64_ABS => intrinsic_i64_unop(s_port, other_port, caller_port, |a| a.abs(), read_guard),
        OP_I64_EQ  => intrinsic_i64_cmpop(s_port, other_port, caller_port, |a, b| a == b, read_guard),
        OP_I64_NE  => intrinsic_i64_cmpop(s_port, other_port, caller_port, |a, b| a != b, read_guard),
        OP_I64_LT  => intrinsic_i64_cmpop(s_port, other_port, caller_port, |a, b| a < b, read_guard),
        OP_I64_LE  => intrinsic_i64_cmpop(s_port, other_port, caller_port, |a, b| a <= b, read_guard),
        OP_I64_GT  => intrinsic_i64_cmpop(s_port, other_port, caller_port, |a, b| a > b, read_guard),
        OP_I64_GE  => intrinsic_i64_cmpop(s_port, other_port, caller_port, |a, b| a >= b, read_guard),

        // --- u64 Ops ---
        OP_U64_ADD => intrinsic_u64_binop(s_port, other_port, caller_port, |a, b| a.wrapping_add(b), read_guard),
        OP_U64_SUB => intrinsic_u64_binop(s_port, other_port, caller_port, |a, b| a.wrapping_sub(b), read_guard),
        OP_U64_MUL => intrinsic_u64_binop(s_port, other_port, caller_port, |a, b| a.wrapping_mul(b), read_guard),
        OP_U64_DIV => intrinsic_u64_binop(s_port, other_port, caller_port, |a, b| if b == 0 { 0 } else { a.wrapping_div(b) }, read_guard),
        OP_U64_REM => intrinsic_u64_binop(s_port, other_port, caller_port, |a, b| if b == 0 { 0 } else { a.wrapping_rem(b) }, read_guard),
        OP_U64_AND => intrinsic_u64_binop(s_port, other_port, caller_port, |a, b| a & b, read_guard),
        OP_U64_OR  => intrinsic_u64_binop(s_port, other_port, caller_port, |a, b| a | b, read_guard),
        OP_U64_XOR => intrinsic_u64_binop(s_port, other_port, caller_port, |a, b| a ^ b, read_guard),
        OP_U64_SHL => intrinsic_u64_binop(s_port, other_port, caller_port, |a, b| a.wrapping_shl(b as u32), read_guard),
        OP_U64_SHR => intrinsic_u64_binop(s_port, other_port, caller_port, |a, b| a.wrapping_shr(b as u32), read_guard),
        OP_U64_POW => intrinsic_u64_binop(s_port, other_port, caller_port, |a, b| a.wrapping_pow(b as u32), read_guard),
        OP_U64_NOT => intrinsic_u64_unop(s_port, other_port, caller_port, |a| !a, read_guard),
        OP_U64_EQ  => intrinsic_u64_cmpop(s_port, other_port, caller_port, |a, b| a == b, read_guard),
        OP_U64_NE  => intrinsic_u64_cmpop(s_port, other_port, caller_port, |a, b| a != b, read_guard),
        OP_U64_LT  => intrinsic_u64_cmpop(s_port, other_port, caller_port, |a, b| a < b, read_guard),
        OP_U64_LE  => intrinsic_u64_cmpop(s_port, other_port, caller_port, |a, b| a <= b, read_guard),
        OP_U64_GT  => intrinsic_u64_cmpop(s_port, other_port, caller_port, |a, b| a > b, read_guard),
        OP_U64_GE  => intrinsic_u64_cmpop(s_port, other_port, caller_port, |a, b| a >= b, read_guard),

        // --- f64 Ops ---
        OP_F64_ADD => intrinsic_f64_binop(s_port, other_port, caller_port, |a, b| a + b, read_guard),
        OP_F64_SUB => intrinsic_f64_binop(s_port, other_port, caller_port, |a, b| a - b, read_guard),
        OP_F64_MUL => intrinsic_f64_binop(s_port, other_port, caller_port, |a, b| a * b, read_guard),
        OP_F64_DIV => intrinsic_f64_binop(s_port, other_port, caller_port, |a, b| a / b, read_guard),
        OP_F64_REM => intrinsic_f64_binop(s_port, other_port, caller_port, |a, b| a % b, read_guard),
        OP_F64_POW => intrinsic_f64_binop(s_port, other_port, caller_port, |a, b| a.powf(b), read_guard),
        OP_F64_NEG => intrinsic_f64_unop(s_port, other_port, caller_port, |a| -a, read_guard),
        OP_F64_ABS => intrinsic_f64_unop(s_port, other_port, caller_port, |a| a.abs(), read_guard),
        OP_F64_EQ  => intrinsic_f64_cmpop(s_port, other_port, caller_port, |a, b| a == b, read_guard),
        OP_F64_NE  => intrinsic_f64_cmpop(s_port, other_port, caller_port, |a, b| a != b, read_guard),
        OP_F64_LT  => intrinsic_f64_cmpop(s_port, other_port, caller_port, |a, b| a < b, read_guard),
        OP_F64_LE  => intrinsic_f64_cmpop(s_port, other_port, caller_port, |a, b| a <= b, read_guard),
        OP_F64_GT  => intrinsic_f64_cmpop(s_port, other_port, caller_port, |a, b| a > b, read_guard),
        OP_F64_GE  => intrinsic_f64_cmpop(s_port, other_port, caller_port, |a, b| a >= b, read_guard),
        
        // --- i32 Ops ---
        OP_I32_ADD => intrinsic_i32_binop(s_port, other_port, caller_port, |a, b| a.wrapping_add(b), read_guard),
        OP_I32_SUB => intrinsic_i32_binop(s_port, other_port, caller_port, |a, b| a.wrapping_sub(b), read_guard),
        OP_I32_MUL => intrinsic_i32_binop(s_port, other_port, caller_port, |a, b| a.wrapping_mul(b), read_guard),
        OP_I32_DIV => intrinsic_i32_binop(s_port, other_port, caller_port, |a, b| if b == 0 { 0 } else { a.wrapping_div(b) }, read_guard),
        OP_I32_REM => intrinsic_i32_binop(s_port, other_port, caller_port, |a, b| if b == 0 { 0 } else { a.wrapping_rem(b) }, read_guard),
        OP_I32_AND => intrinsic_i32_binop(s_port, other_port, caller_port, |a, b| a & b, read_guard),
        OP_I32_OR  => intrinsic_i32_binop(s_port, other_port, caller_port, |a, b| a | b, read_guard),
        OP_I32_XOR => intrinsic_i32_binop(s_port, other_port, caller_port, |a, b| a ^ b, read_guard),
        OP_I32_SHL => intrinsic_i32_binop(s_port, other_port, caller_port, |a, b| a.wrapping_shl(b as u32), read_guard),
        OP_I32_SHR => intrinsic_i32_binop(s_port, other_port, caller_port, |a, b| a.wrapping_shr(b as u32), read_guard),
        OP_I32_POW => intrinsic_i32_binop(s_port, other_port, caller_port, |a, b| a.wrapping_pow(b as u32), read_guard),
        OP_I32_NEG => intrinsic_i32_unop(s_port, other_port, caller_port, |a| a.wrapping_neg(), read_guard),
        OP_I32_NOT => intrinsic_i32_unop(s_port, other_port, caller_port, |a| !a, read_guard),
        OP_I32_ABS => intrinsic_i32_unop(s_port, other_port, caller_port, |a| a.abs(), read_guard),
        OP_I32_EQ  => intrinsic_i32_cmpop(s_port, other_port, caller_port, |a, b| a == b, read_guard),
        OP_I32_NE  => intrinsic_i32_cmpop(s_port, other_port, caller_port, |a, b| a != b, read_guard),
        OP_I32_LT  => intrinsic_i32_cmpop(s_port, other_port, caller_port, |a, b| a < b, read_guard),
        OP_I32_LE  => intrinsic_i32_cmpop(s_port, other_port, caller_port, |a, b| a <= b, read_guard),
        OP_I32_GT  => intrinsic_i32_cmpop(s_port, other_port, caller_port, |a, b| a > b, read_guard),
        OP_I32_GE  => intrinsic_i32_cmpop(s_port, other_port, caller_port, |a, b| a >= b, read_guard),

        // --- u32 Ops ---
        OP_U32_ADD => intrinsic_u32_binop(s_port, other_port, caller_port, |a, b| a.wrapping_add(b), read_guard),
        OP_U32_SUB => intrinsic_u32_binop(s_port, other_port, caller_port, |a, b| a.wrapping_sub(b), read_guard),
        OP_U32_MUL => intrinsic_u32_binop(s_port, other_port, caller_port, |a, b| a.wrapping_mul(b), read_guard),
        OP_U32_DIV => intrinsic_u32_binop(s_port, other_port, caller_port, |a, b| if b == 0 { 0 } else { a.wrapping_div(b) }, read_guard),
        OP_U32_REM => intrinsic_u32_binop(s_port, other_port, caller_port, |a, b| if b == 0 { 0 } else { a.wrapping_rem(b) }, read_guard),
        OP_U32_AND => intrinsic_u32_binop(s_port, other_port, caller_port, |a, b| a & b, read_guard),
        OP_U32_OR  => intrinsic_u32_binop(s_port, other_port, caller_port, |a, b| a | b, read_guard),
        OP_U32_XOR => intrinsic_u32_binop(s_port, other_port, caller_port, |a, b| a ^ b, read_guard),
        OP_U32_SHL => intrinsic_u32_binop(s_port, other_port, caller_port, |a, b| a.wrapping_shl(b), read_guard),
        OP_U32_SHR => intrinsic_u32_binop(s_port, other_port, caller_port, |a, b| a.wrapping_shr(b), read_guard),
        OP_U32_POW => intrinsic_u32_binop(s_port, other_port, caller_port, |a, b| a.wrapping_pow(b), read_guard),
        OP_U32_NOT => intrinsic_u32_unop(s_port, other_port, caller_port, |a| !a, read_guard),
        OP_U32_EQ  => intrinsic_u32_cmpop(s_port, other_port, caller_port, |a, b| a == b, read_guard),
        OP_U32_NE  => intrinsic_u32_cmpop(s_port, other_port, caller_port, |a, b| a != b, read_guard),
        OP_U32_LT  => intrinsic_u32_cmpop(s_port, other_port, caller_port, |a, b| a < b, read_guard),
        OP_U32_LE  => intrinsic_u32_cmpop(s_port, other_port, caller_port, |a, b| a <= b, read_guard),
        OP_U32_GT  => intrinsic_u32_cmpop(s_port, other_port, caller_port, |a, b| a > b, read_guard),
        OP_U32_GE  => intrinsic_u32_cmpop(s_port, other_port, caller_port, |a, b| a >= b, read_guard),

        // --- f32 Ops ---
        OP_F32_ADD => intrinsic_f32_binop(s_port, other_port, caller_port, |a, b| a + b, read_guard),
        OP_F32_SUB => intrinsic_f32_binop(s_port, other_port, caller_port, |a, b| a - b, read_guard),
        OP_F32_MUL => intrinsic_f32_binop(s_port, other_port, caller_port, |a, b| a * b, read_guard),
        OP_F32_DIV => intrinsic_f32_binop(s_port, other_port, caller_port, |a, b| a / b, read_guard),
        OP_F32_REM => intrinsic_f32_binop(s_port, other_port, caller_port, |a, b| a % b, read_guard),
        OP_F32_POW => intrinsic_f32_binop(s_port, other_port, caller_port, |a, b| a.powf(b), read_guard),
        OP_F32_NEG => intrinsic_f32_unop(s_port, other_port, caller_port, |a| -a, read_guard),
        OP_F32_ABS => intrinsic_f32_unop(s_port, other_port, caller_port, |a| a.abs(), read_guard),
        OP_F32_EQ  => intrinsic_f32_cmpop(s_port, other_port, caller_port, |a, b| a == b, read_guard),
        OP_F32_NE  => intrinsic_f32_cmpop(s_port, other_port, caller_port, |a, b| a != b, read_guard),
        OP_F32_LT  => intrinsic_f32_cmpop(s_port, other_port, caller_port, |a, b| a < b, read_guard),
        OP_F32_LE  => intrinsic_f32_cmpop(s_port, other_port, caller_port, |a, b| a <= b, read_guard),
        OP_F32_GT  => intrinsic_f32_cmpop(s_port, other_port, caller_port, |a, b| a > b, read_guard),
        OP_F32_GE  => intrinsic_f32_cmpop(s_port, other_port, caller_port, |a, b| a >= b, read_guard),

        // --- bool Ops ---
        OP_BOOL_AND => intrinsic_bool_binop(s_port, other_port, caller_port, |a, b| a && b, read_guard),
        OP_BOOL_OR  => intrinsic_bool_binop(s_port, other_port, caller_port, |a, b| a || b, read_guard),
        OP_BOOL_XOR => intrinsic_bool_binop(s_port, other_port, caller_port, |a, b| a ^ b, read_guard),
        OP_BOOL_NOT => intrinsic_bool_unop(s_port, other_port, caller_port, |a| !a, read_guard),
        OP_BOOL_EQ  => intrinsic_bool_binop(s_port, other_port, caller_port, |a, b| a == b, read_guard),
        OP_BOOL_NE  => intrinsic_bool_binop(s_port, other_port, caller_port, |a, b| a != b, read_guard),

        // --- i16 Ops ---
        OP_I16_ADD => intrinsic_i16_binop(s_port, other_port, caller_port, |a, b| a.wrapping_add(b), read_guard),
        OP_I16_SUB => intrinsic_i16_binop(s_port, other_port, caller_port, |a, b| a.wrapping_sub(b), read_guard),
        OP_I16_MUL => intrinsic_i16_binop(s_port, other_port, caller_port, |a, b| a.wrapping_mul(b), read_guard),
        OP_I16_DIV => intrinsic_i16_binop(s_port, other_port, caller_port, |a, b| if b == 0 { 0 } else { a.wrapping_div(b) }, read_guard),
        OP_I16_REM => intrinsic_i16_binop(s_port, other_port, caller_port, |a, b| if b == 0 { 0 } else { a.wrapping_rem(b) }, read_guard),
        OP_I16_AND => intrinsic_i16_binop(s_port, other_port, caller_port, |a, b| a & b, read_guard),
        OP_I16_OR  => intrinsic_i16_binop(s_port, other_port, caller_port, |a, b| a | b, read_guard),
        OP_I16_XOR => intrinsic_i16_binop(s_port, other_port, caller_port, |a, b| a ^ b, read_guard),
        OP_I16_SHL => intrinsic_i16_binop(s_port, other_port, caller_port, |a, b| a.wrapping_shl(b as u32), read_guard),
        OP_I16_SHR => intrinsic_i16_binop(s_port, other_port, caller_port, |a, b| a.wrapping_shr(b as u32), read_guard),
        OP_I16_POW => intrinsic_i16_binop(s_port, other_port, caller_port, |a, b| a.wrapping_pow(b as u32), read_guard),
        OP_I16_NEG => intrinsic_i16_unop(s_port, other_port, caller_port, |a| a.wrapping_neg(), read_guard),
        OP_I16_NOT => intrinsic_i16_unop(s_port, other_port, caller_port, |a| !a, read_guard),
        OP_I16_ABS => intrinsic_i16_unop(s_port, other_port, caller_port, |a| a.abs(), read_guard),
        OP_I16_EQ  => intrinsic_i16_cmpop(s_port, other_port, caller_port, |a, b| a == b, read_guard),
        OP_I16_NE  => intrinsic_i16_cmpop(s_port, other_port, caller_port, |a, b| a != b, read_guard),
        OP_I16_LT  => intrinsic_i16_cmpop(s_port, other_port, caller_port, |a, b| a < b, read_guard),
        OP_I16_LE  => intrinsic_i16_cmpop(s_port, other_port, caller_port, |a, b| a <= b, read_guard),
        OP_I16_GT  => intrinsic_i16_cmpop(s_port, other_port, caller_port, |a, b| a > b, read_guard),
        OP_I16_GE  => intrinsic_i16_cmpop(s_port, other_port, caller_port, |a, b| a >= b, read_guard),

        // --- u16 Ops ---
        OP_U16_ADD => intrinsic_u16_binop(s_port, other_port, caller_port, |a, b| a.wrapping_add(b), read_guard),
        OP_U16_SUB => intrinsic_u16_binop(s_port, other_port, caller_port, |a, b| a.wrapping_sub(b), read_guard),
        OP_U16_MUL => intrinsic_u16_binop(s_port, other_port, caller_port, |a, b| a.wrapping_mul(b), read_guard),
        OP_U16_DIV => intrinsic_u16_binop(s_port, other_port, caller_port, |a, b| if b == 0 { 0 } else { a.wrapping_div(b) }, read_guard),
        OP_U16_REM => intrinsic_u16_binop(s_port, other_port, caller_port, |a, b| if b == 0 { 0 } else { a.wrapping_rem(b) }, read_guard),
        OP_U16_AND => intrinsic_u16_binop(s_port, other_port, caller_port, |a, b| a & b, read_guard),
        OP_U16_OR  => intrinsic_u16_binop(s_port, other_port, caller_port, |a, b| a | b, read_guard),
        OP_U16_XOR => intrinsic_u16_binop(s_port, other_port, caller_port, |a, b| a ^ b, read_guard),
        OP_U16_SHL => intrinsic_u16_binop(s_port, other_port, caller_port, |a, b| a.wrapping_shl(b as u32), read_guard),
        OP_U16_SHR => intrinsic_u16_binop(s_port, other_port, caller_port, |a, b| a.wrapping_shr(b as u32), read_guard),
        OP_U16_POW => intrinsic_u16_binop(s_port, other_port, caller_port, |a, b| a.wrapping_pow(b as u32), read_guard),
        OP_U16_NOT => intrinsic_u16_unop(s_port, other_port, caller_port, |a| !a, read_guard),
        OP_U16_EQ  => intrinsic_u16_cmpop(s_port, other_port, caller_port, |a, b| a == b, read_guard),
        OP_U16_NE  => intrinsic_u16_cmpop(s_port, other_port, caller_port, |a, b| a != b, read_guard),
        OP_U16_LT  => intrinsic_u16_cmpop(s_port, other_port, caller_port, |a, b| a < b, read_guard),
        OP_U16_LE  => intrinsic_u16_cmpop(s_port, other_port, caller_port, |a, b| a <= b, read_guard),
        OP_U16_GT  => intrinsic_u16_cmpop(s_port, other_port, caller_port, |a, b| a > b, read_guard),
        OP_U16_GE  => intrinsic_u16_cmpop(s_port, other_port, caller_port, |a, b| a >= b, read_guard),

        // --- i8 Ops ---
        OP_I8_ADD => intrinsic_i8_binop(s_port, other_port, caller_port, |a, b| a.wrapping_add(b), read_guard),
        OP_I8_SUB => intrinsic_i8_binop(s_port, other_port, caller_port, |a, b| a.wrapping_sub(b), read_guard),
        OP_I8_MUL => intrinsic_i8_binop(s_port, other_port, caller_port, |a, b| a.wrapping_mul(b), read_guard),
        OP_I8_DIV => intrinsic_i8_binop(s_port, other_port, caller_port, |a, b| if b == 0 { 0 } else { a.wrapping_div(b) }, read_guard),
        OP_I8_REM => intrinsic_i8_binop(s_port, other_port, caller_port, |a, b| if b == 0 { 0 } else { a.wrapping_rem(b) }, read_guard),
        OP_I8_AND => intrinsic_i8_binop(s_port, other_port, caller_port, |a, b| a & b, read_guard),
        OP_I8_OR  => intrinsic_i8_binop(s_port, other_port, caller_port, |a, b| a | b, read_guard),
        OP_I8_XOR => intrinsic_i8_binop(s_port, other_port, caller_port, |a, b| a ^ b, read_guard),
        OP_I8_SHL => intrinsic_i8_binop(s_port, other_port, caller_port, |a, b| a.wrapping_shl(b as u32), read_guard),
        OP_I8_SHR => intrinsic_i8_binop(s_port, other_port, caller_port, |a, b| a.wrapping_shr(b as u32), read_guard),
        OP_I8_POW => intrinsic_i8_binop(s_port, other_port, caller_port, |a, b| a.wrapping_pow(b as u32), read_guard),
        OP_I8_NEG => intrinsic_i8_unop(s_port, other_port, caller_port, |a| a.wrapping_neg(), read_guard),
        OP_I8_NOT => intrinsic_i8_unop(s_port, other_port, caller_port, |a| !a, read_guard),
        OP_I8_ABS => intrinsic_i8_unop(s_port, other_port, caller_port, |a| a.abs(), read_guard),
        OP_I8_EQ  => intrinsic_i8_cmpop(s_port, other_port, caller_port, |a, b| a == b, read_guard),
        OP_I8_NE  => intrinsic_i8_cmpop(s_port, other_port, caller_port, |a, b| a != b, read_guard),
        OP_I8_LT  => intrinsic_i8_cmpop(s_port, other_port, caller_port, |a, b| a < b, read_guard),
        OP_I8_LE  => intrinsic_i8_cmpop(s_port, other_port, caller_port, |a, b| a <= b, read_guard),
        OP_I8_GT  => intrinsic_i8_cmpop(s_port, other_port, caller_port, |a, b| a > b, read_guard),
        OP_I8_GE  => intrinsic_i8_cmpop(s_port, other_port, caller_port, |a, b| a >= b, read_guard),

        // --- u8 Ops ---
        OP_U8_ADD => intrinsic_u8_binop(s_port, other_port, caller_port, |a, b| a.wrapping_add(b), read_guard),
        OP_U8_SUB => intrinsic_u8_binop(s_port, other_port, caller_port, |a, b| a.wrapping_sub(b), read_guard),
        OP_U8_MUL => intrinsic_u8_binop(s_port, other_port, caller_port, |a, b| a.wrapping_mul(b), read_guard),
        OP_U8_DIV => intrinsic_u8_binop(s_port, other_port, caller_port, |a, b| if b == 0 { 0 } else { a.wrapping_div(b) }, read_guard),
        OP_U8_REM => intrinsic_u8_binop(s_port, other_port, caller_port, |a, b| if b == 0 { 0 } else { a.wrapping_rem(b) }, read_guard),
        OP_U8_AND => intrinsic_u8_binop(s_port, other_port, caller_port, |a, b| a & b, read_guard),
        OP_U8_OR  => intrinsic_u8_binop(s_port, other_port, caller_port, |a, b| a | b, read_guard),
        OP_U8_XOR => intrinsic_u8_binop(s_port, other_port, caller_port, |a, b| a ^ b, read_guard),
        OP_U8_SHL => intrinsic_u8_binop(s_port, other_port, caller_port, |a, b| a.wrapping_shl(b as u32), read_guard),
        OP_U8_SHR => intrinsic_u8_binop(s_port, other_port, caller_port, |a, b| a.wrapping_shr(b as u32), read_guard),
        OP_U8_POW => intrinsic_u8_binop(s_port, other_port, caller_port, |a, b| a.wrapping_pow(b as u32), read_guard),
        OP_U8_NOT => intrinsic_u8_unop(s_port, other_port, caller_port, |a| !a, read_guard),
        OP_U8_EQ  => intrinsic_u8_cmpop(s_port, other_port, caller_port, |a, b| a == b, read_guard),
        OP_U8_NE  => intrinsic_u8_cmpop(s_port, other_port, caller_port, |a, b| a != b, read_guard),
        OP_U8_LT  => intrinsic_u8_cmpop(s_port, other_port, caller_port, |a, b| a < b, read_guard),
        OP_U8_LE  => intrinsic_u8_cmpop(s_port, other_port, caller_port, |a, b| a <= b, read_guard),
        OP_U8_GT  => intrinsic_u8_cmpop(s_port, other_port, caller_port, |a, b| a > b, read_guard),
        OP_U8_GE  => intrinsic_u8_cmpop(s_port, other_port, caller_port, |a, b| a >= b, read_guard),

        // --- i128 Ops ---
        OP_I128_ADD => intrinsic_i128_binop(s_port, other_port, caller_port, |a, b| a.wrapping_add(b), read_guard),
        OP_I128_SUB => intrinsic_i128_binop(s_port, other_port, caller_port, |a, b| a.wrapping_sub(b), read_guard),
        OP_I128_MUL => intrinsic_i128_binop(s_port, other_port, caller_port, |a, b| a.wrapping_mul(b), read_guard),
        OP_I128_DIV => intrinsic_i128_binop(s_port, other_port, caller_port, |a, b| if b == 0 { 0 } else { a.wrapping_div(b) }, read_guard),
        OP_I128_REM => intrinsic_i128_binop(s_port, other_port, caller_port, |a, b| if b == 0 { 0 } else { a.wrapping_rem(b) }, read_guard),
        OP_I128_AND => intrinsic_i128_binop(s_port, other_port, caller_port, |a, b| a & b, read_guard),
        OP_I128_OR  => intrinsic_i128_binop(s_port, other_port, caller_port, |a, b| a | b, read_guard),
        OP_I128_XOR => intrinsic_i128_binop(s_port, other_port, caller_port, |a, b| a ^ b, read_guard),
        OP_I128_SHL => intrinsic_i128_binop(s_port, other_port, caller_port, |a, b| a.wrapping_shl(b as u32), read_guard),
        OP_I128_SHR => intrinsic_i128_binop(s_port, other_port, caller_port, |a, b| a.wrapping_shr(b as u32), read_guard),
        OP_I128_POW => intrinsic_i128_binop(s_port, other_port, caller_port, |a, b| a.wrapping_pow(b as u32), read_guard),
        OP_I128_NEG => intrinsic_i128_unop(s_port, other_port, caller_port, |a| a.wrapping_neg(), read_guard),
        OP_I128_NOT => intrinsic_i128_unop(s_port, other_port, caller_port, |a| !a, read_guard),
        OP_I128_ABS => intrinsic_i128_unop(s_port, other_port, caller_port, |a| a.abs(), read_guard),
        OP_I128_EQ  => intrinsic_i128_cmpop(s_port, other_port, caller_port, |a, b| a == b, read_guard),
        OP_I128_NE  => intrinsic_i128_cmpop(s_port, other_port, caller_port, |a, b| a != b, read_guard),
        OP_I128_LT  => intrinsic_i128_cmpop(s_port, other_port, caller_port, |a, b| a < b, read_guard),
        OP_I128_LE  => intrinsic_i128_cmpop(s_port, other_port, caller_port, |a, b| a <= b, read_guard),
        OP_I128_GT  => intrinsic_i128_cmpop(s_port, other_port, caller_port, |a, b| a > b, read_guard),
        OP_I128_GE  => intrinsic_i128_cmpop(s_port, other_port, caller_port, |a, b| a >= b, read_guard),

        // --- u128 Ops ---
        OP_U128_ADD => intrinsic_u128_binop(s_port, other_port, caller_port, |a, b| a.wrapping_add(b), read_guard),
        OP_U128_SUB => intrinsic_u128_binop(s_port, other_port, caller_port, |a, b| a.wrapping_sub(b), read_guard),
        OP_U128_MUL => intrinsic_u128_binop(s_port, other_port, caller_port, |a, b| a.wrapping_mul(b), read_guard),
        OP_U128_DIV => intrinsic_u128_binop(s_port, other_port, caller_port, |a, b| if b == 0 { 0 } else { a.wrapping_div(b) }, read_guard),
        OP_U128_REM => intrinsic_u128_binop(s_port, other_port, caller_port, |a, b| if b == 0 { 0 } else { a.wrapping_rem(b) }, read_guard),
        OP_U128_AND => intrinsic_u128_binop(s_port, other_port, caller_port, |a, b| a & b, read_guard),
        OP_U128_OR  => intrinsic_u128_binop(s_port, other_port, caller_port, |a, b| a | b, read_guard),
        OP_U128_XOR => intrinsic_u128_binop(s_port, other_port, caller_port, |a, b| a ^ b, read_guard),
        OP_U128_SHL => intrinsic_u128_binop(s_port, other_port, caller_port, |a, b| a.wrapping_shl(b as u32), read_guard),
        OP_U128_SHR => intrinsic_u128_binop(s_port, other_port, caller_port, |a, b| a.wrapping_shr(b as u32), read_guard),
        OP_U128_POW => intrinsic_u128_binop(s_port, other_port, caller_port, |a, b| a.wrapping_pow(b as u32), read_guard),
        OP_U128_NOT => intrinsic_u128_unop(s_port, other_port, caller_port, |a| !a, read_guard),
        OP_U128_EQ  => intrinsic_u128_cmpop(s_port, other_port, caller_port, |a, b| a == b, read_guard),
        OP_U128_NE  => intrinsic_u128_cmpop(s_port, other_port, caller_port, |a, b| a != b, read_guard),
        OP_U128_LT  => intrinsic_u128_cmpop(s_port, other_port, caller_port, |a, b| a < b, read_guard),
        OP_U128_LE  => intrinsic_u128_cmpop(s_port, other_port, caller_port, |a, b| a <= b, read_guard),
        OP_U128_GT  => intrinsic_u128_cmpop(s_port, other_port, caller_port, |a, b| a > b, read_guard),
        OP_U128_GE  => intrinsic_u128_cmpop(s_port, other_port, caller_port, |a, b| a >= b, read_guard),

        // --- Misc Ops ---
        OP_PANIC => intrinsic_panic(s_port, other_port, caller_port, read_guard),
        _ => {
            log::error!("dispatch_intrinsic_op: Unhandled OpCode: {}", op_code);
            remove_node(s_port, read_guard);
            annihilate_any(other_port, read_guard); 
            if caller_port != other_port { annihilate_any(caller_port, read_guard); }
         }
    }
}

// --- Concrete Intrinsic Handlers ---

// --- i64 Handlers ---
#[inline]
unsafe fn intrinsic_i64_unop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(i64) -> i64,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let arg0_port = get_aux_port(app_head_port, read_guard, PortType::Left);
    if arg0_port == Port::NULL || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) {
        // ... error handling & annihilate ...
        return;
    }
    let arg0_val = get_number_data(arg0_port, read_guard) as i64;
    let result_val = op_fn(arg0_val);
    let result_port = alloc_number(result_val as u128, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_i64_binop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(i64, i64) -> i64,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* error */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) { /* error */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as i64;
    let arg1_val = get_number_data(arg1_port, read_guard) as i64;
    let result_val = op_fn(arg0_val, arg1_val);
    let result_port = alloc_number(result_val as u128, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_i64_cmpop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(i64, i64) -> bool,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* error */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) { /* error */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as i64;
    let arg1_val = get_number_data(arg1_port, read_guard) as i64;
    let result_bool = op_fn(arg0_val, arg1_val);
    let result_port = alloc_static_bool(result_bool, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

// --- u64 Handlers ---
#[inline]
unsafe fn intrinsic_u64_unop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(u64) -> u64,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let arg0_port = get_aux_port(app_head_port, read_guard, PortType::Left);
    if arg0_port == Port::NULL || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as u64;
    let result_val = op_fn(arg0_val);
    let result_port = alloc_number(result_val as u128, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_u64_binop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(u64, u64) -> u64,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* ... */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as u64;
    let arg1_val = get_number_data(arg1_port, read_guard) as u64;
    let result_val = op_fn(arg0_val, arg1_val);
    let result_port = alloc_number(result_val as u128, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_u64_cmpop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(u64, u64) -> bool,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* ... */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as u64;
    let arg1_val = get_number_data(arg1_port, read_guard) as u64;
    let result_bool = op_fn(arg0_val, arg1_val);
    let result_port = alloc_static_bool(result_bool, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

// --- f64 Handlers ---
#[inline]
unsafe fn intrinsic_f64_unop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(f64) -> f64,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let arg0_port = get_aux_port(app_head_port, read_guard, PortType::Left);
    if arg0_port == Port::NULL || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = f64::from_bits(get_number_data(arg0_port, read_guard) as u64);
    let result_val = op_fn(arg0_val);
    let result_port = alloc_number(result_val.to_bits() as u128, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_f64_binop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(f64, f64) -> f64,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* ... */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = f64::from_bits(get_number_data(arg0_port, read_guard) as u64);
    let arg1_val = f64::from_bits(get_number_data(arg1_port, read_guard) as u64);
    let result_val = op_fn(arg0_val, arg1_val);
    let result_port = alloc_number(result_val.to_bits() as u128, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_f64_cmpop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(f64, f64) -> bool,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* ... */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = f64::from_bits(get_number_data(arg0_port, read_guard) as u64);
    let arg1_val = f64::from_bits(get_number_data(arg1_port, read_guard) as u64);
    let result_bool = op_fn(arg0_val, arg1_val);
    let result_port = alloc_static_bool(result_bool, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

// --- i32 Handlers ---
#[inline]
unsafe fn intrinsic_i32_unop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(i32) -> i32,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let arg0_port = get_aux_port(app_head_port, read_guard, PortType::Left);
    if arg0_port == Port::NULL || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as i32;
    let result_val = op_fn(arg0_val);
    let result_port = alloc_number(result_val as u128, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_i32_binop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(i32, i32) -> i32,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* ... */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as i32;
    let arg1_val = get_number_data(arg1_port, read_guard) as i32;
    let result_val = op_fn(arg0_val, arg1_val);
    let result_port = alloc_number(result_val as u128, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_i32_cmpop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(i32, i32) -> bool,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* ... */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as i32;
    let arg1_val = get_number_data(arg1_port, read_guard) as i32;
    let result_bool = op_fn(arg0_val, arg1_val);
    let result_port = alloc_static_bool(result_bool, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

// --- u32 Handlers ---
#[inline]
unsafe fn intrinsic_u32_unop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(u32) -> u32,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let arg0_port = get_aux_port(app_head_port, read_guard, PortType::Left);
    if arg0_port == Port::NULL || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as u32;
    let result_val = op_fn(arg0_val);
    let result_port = alloc_number(result_val as u128, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_u32_binop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(u32, u32) -> u32,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* ... */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as u32;
    let arg1_val = get_number_data(arg1_port, read_guard) as u32;
    let result_val = op_fn(arg0_val, arg1_val);
    let result_port = alloc_number(result_val as u128, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_u32_cmpop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(u32, u32) -> bool,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* ... */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as u32;
    let arg1_val = get_number_data(arg1_port, read_guard) as u32;
    let result_bool = op_fn(arg0_val, arg1_val);
    let result_port = alloc_static_bool(result_bool, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

// --- f32 Handlers ---
#[inline]
unsafe fn intrinsic_f32_unop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(f32) -> f32,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let arg0_port = get_aux_port(app_head_port, read_guard, PortType::Left);
    if arg0_port == Port::NULL || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = f32::from_bits(get_number_data(arg0_port, read_guard) as u32);
    let result_val = op_fn(arg0_val);
    let result_port = alloc_number(result_val.to_bits() as u128, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_f32_binop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(f32, f32) -> f32,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* ... */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = f32::from_bits(get_number_data(arg0_port, read_guard) as u32);
    let arg1_val = f32::from_bits(get_number_data(arg1_port, read_guard) as u32);
    let result_val = op_fn(arg0_val, arg1_val);
    let result_port = alloc_number(result_val.to_bits() as u128, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_f32_cmpop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(f32, f32) -> bool,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* ... */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = f32::from_bits(get_number_data(arg0_port, read_guard) as u32);
    let arg1_val = f32::from_bits(get_number_data(arg1_port, read_guard) as u32);
    let result_bool = op_fn(arg0_val, arg1_val);
    let result_port = alloc_static_bool(result_bool, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

// --- bool Handlers ---
// Bool ops take Static bools as input, not Numbers
#[inline]
unsafe fn intrinsic_bool_unop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(bool) -> bool,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let arg0_port = get_aux_port(app_head_port, read_guard, PortType::Left);
    if arg0_port == Port::NULL || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Static) { /* ... */ return; }
    let arg0_val_data = get_static_data(arg0_port, read_guard);
    if decode_tag(arg0_val_data) != TAG_NIL { /* Error: Expected Bool encoded as Static(NIL|0_or_1) */ return; }
    let arg0_val = decode_data(arg0_val_data) != 0;

    let result_val = op_fn(arg0_val);
    let result_port = alloc_static_bool(result_val, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_bool_binop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(bool, bool) -> bool,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* ... */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Static) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Static) { /* ... */ return; }
        
    let arg0_val_data = get_static_data(arg0_port, read_guard);
    let arg1_val_data = get_static_data(arg1_port, read_guard);
    if decode_tag(arg0_val_data) != TAG_NIL || decode_tag(arg1_val_data) != TAG_NIL { /* Error */ return; }
    let arg0_val = decode_data(arg0_val_data) != 0;
    let arg1_val = decode_data(arg1_val_data) != 0;

    let result_val = op_fn(arg0_val, arg1_val);
    let result_port = alloc_static_bool(result_val, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

// --- i16 Handlers ---
#[inline]
unsafe fn intrinsic_i16_unop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(i16) -> i16,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let arg0_port = get_aux_port(app_head_port, read_guard, PortType::Left);
    if arg0_port == Port::NULL || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as i16;
    let result_val = op_fn(arg0_val);
    let result_port = alloc_number(result_val as u128, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_i16_binop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(i16, i16) -> i16,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* ... */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as i16;
    let arg1_val = get_number_data(arg1_port, read_guard) as i16;
    let result_val = op_fn(arg0_val, arg1_val);
    let result_port = alloc_number(result_val as u128, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_i16_cmpop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(i16, i16) -> bool,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* ... */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as i16;
    let arg1_val = get_number_data(arg1_port, read_guard) as i16;
    let result_bool = op_fn(arg0_val, arg1_val);
    let result_port = alloc_static_bool(result_bool, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

// --- u16 Handlers ---
#[inline]
unsafe fn intrinsic_u16_unop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(u16) -> u16,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let arg0_port = get_aux_port(app_head_port, read_guard, PortType::Left);
    if arg0_port == Port::NULL || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as u16;
    let result_val = op_fn(arg0_val);
    let result_port = alloc_number(result_val as u128, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_u16_binop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(u16, u16) -> u16,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* ... */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as u16;
    let arg1_val = get_number_data(arg1_port, read_guard) as u16;
    let result_val = op_fn(arg0_val, arg1_val);
    let result_port = alloc_number(result_val as u128, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_u16_cmpop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(u16, u16) -> bool,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* ... */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as u16;
    let arg1_val = get_number_data(arg1_port, read_guard) as u16;
    let result_bool = op_fn(arg0_val, arg1_val);
    let result_port = alloc_static_bool(result_bool, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

// --- i8 Handlers ---
#[inline]
unsafe fn intrinsic_i8_unop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(i8) -> i8,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let arg0_port = get_aux_port(app_head_port, read_guard, PortType::Left);
    if arg0_port == Port::NULL || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as i8;
    let result_val = op_fn(arg0_val);
    let result_port = alloc_number(result_val as u128, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_i8_binop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(i8, i8) -> i8,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* ... */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as i8;
    let arg1_val = get_number_data(arg1_port, read_guard) as i8;
    let result_val = op_fn(arg0_val, arg1_val);
    let result_port = alloc_number(result_val as u128, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_i8_cmpop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(i8, i8) -> bool,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* ... */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as i8;
    let arg1_val = get_number_data(arg1_port, read_guard) as i8;
    let result_bool = op_fn(arg0_val, arg1_val);
    let result_port = alloc_static_bool(result_bool, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

// --- u8 Handlers ---
#[inline]
unsafe fn intrinsic_u8_unop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(u8) -> u8,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let arg0_port = get_aux_port(app_head_port, read_guard, PortType::Left);
    if arg0_port == Port::NULL || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as u8;
    let result_val = op_fn(arg0_val);
    let result_port = alloc_number(result_val as u128, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_u8_binop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(u8, u8) -> u8,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* ... */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as u8;
    let arg1_val = get_number_data(arg1_port, read_guard) as u8;
    let result_val = op_fn(arg0_val, arg1_val);
    let result_port = alloc_number(result_val as u128, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_u8_cmpop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(u8, u8) -> bool,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* ... */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as u8;
    let arg1_val = get_number_data(arg1_port, read_guard) as u8;
    let result_bool = op_fn(arg0_val, arg1_val);
    let result_port = alloc_static_bool(result_bool, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

// --- i128 Handlers ---
#[inline]
unsafe fn intrinsic_i128_unop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(i128) -> i128,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let arg0_port = get_aux_port(app_head_port, read_guard, PortType::Left);
    if arg0_port == Port::NULL || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as i128;
    let result_val = op_fn(arg0_val);
    let result_port = alloc_number(result_val as u128, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_i128_binop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(i128, i128) -> i128,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* ... */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as i128;
    let arg1_val = get_number_data(arg1_port, read_guard) as i128;
    let result_val = op_fn(arg0_val, arg1_val);
    let result_port = alloc_number(result_val as u128, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_i128_cmpop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(i128, i128) -> bool,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* ... */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard) as i128;
    let arg1_val = get_number_data(arg1_port, read_guard) as i128;
    let result_bool = op_fn(arg0_val, arg1_val);
    let result_port = alloc_static_bool(result_bool, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

// --- u128 Handlers ---
#[inline]
unsafe fn intrinsic_u128_unop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(u128) -> u128,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let arg0_port = get_aux_port(app_head_port, read_guard, PortType::Left);
    if arg0_port == Port::NULL || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard); // No cast needed
    let result_val = op_fn(arg0_val);
    let result_port = alloc_number(result_val, s_port.partition_id(), read_guard); // No cast needed
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_u128_binop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(u128, u128) -> u128,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* ... */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard);
    let arg1_val = get_number_data(arg1_port, read_guard);
    let result_val = op_fn(arg0_val, arg1_val);
    let result_port = alloc_number(result_val, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

#[inline]
unsafe fn intrinsic_u128_cmpop(
    s_port: Port, app_head_port: Port, caller_port: Port,
    op_fn: fn(u128, u128) -> bool,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
    if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) { /* ... */ return; }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL 
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) 
        || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) { /* ... */ return; }
    let arg0_val = get_number_data(arg0_port, read_guard);
    let arg1_val = get_number_data(arg1_port, read_guard);
    let result_bool = op_fn(arg0_val, arg1_val);
    let result_port = alloc_static_bool(result_bool, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
    if caller_port.port_type() == PortType::Principal { add_redex_to_partition(caller_port.partition_id(), Redex(caller_port, result_port), read_guard); }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}

/// Placeholder handler for panic intrinsic
#[inline]
unsafe fn intrinsic_panic(
    s_port: Port,            // The Static(Intrinsic) node
    app_head_port: Port,     // The AppCON: App(Op, Msg)
    caller_port: Port,       // The port that originally called the intrinsic
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    unimplemented!("panic intrinsic");
    // TODO:
    // 1. Extract Msg port from AppCON.
    // 2. Expect Msg to be connected to the representation of a string (e.g., list of char Numbers).
    // 3. Decode the string message.
    // 4. Call std::panic! with the message.
} 