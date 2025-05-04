use super::constants::*;
use super::helpers::{get_aux_port, alloc_static_bool, get_number_data, alloc_number, get_static_data};
use super::get_partition_ptr_mut;
use parallax_net::encoding::{
    decode_static_tag, decode_static_payload,
    decode_intrinsic_category, decode_intrinsic_opcode,
    decode_intrinsic_src_type, decode_intrinsic_tgt_type,
    OPCAT_ARITHMETIC, OPCAT_BOOLEAN, OPCAT_CONVERSION, OPCAT_RUNTIME,
    OP_ARITH_ADD, OP_ARITH_SUB, OP_ARITH_MUL, OP_ARITH_DIV, OP_ARITH_REM,
    OP_ARITH_AND, OP_ARITH_OR, OP_ARITH_XOR, OP_ARITH_SHL, OP_ARITH_SHR,
    OP_ARITH_NEG, OP_ARITH_NOT, OP_ARITH_ABS, OP_ARITH_POW, // Note: ABS/POW are not implemented yet
    OP_ARITH_EQ, OP_ARITH_NE, OP_ARITH_LT, OP_ARITH_LE, OP_ARITH_GT, OP_ARITH_GE,
    OP_BOOL_AND, OP_BOOL_OR, OP_BOOL_XOR, OP_BOOL_NOT,
    OP_CONVERT,
    OP_RUNTIME_PANIC, OP_RUNTIME_PRINTLN, OP_RUNTIME_READLN,
    TYPECODE_BOOL,
    TYPECODE_U8, TYPECODE_I8, TYPECODE_U16, TYPECODE_I16,
    TYPECODE_U32, TYPECODE_I32, TYPECODE_U64, TYPECODE_I64,
    TYPECODE_U128, TYPECODE_I128, TYPECODE_F32, TYPECODE_F64,
    TAG_NIL, encode_static_data // Added encode_static_data
};
use crate::inet::*;
use crate::inet::manager::AllPartitions;
use crate::inet::worker::Worker;
use crate::inet::reductions::{remove_node, annihilate_any, connect, add_active_pair_to_partition, get_port_ptr_mut};
use parallax_net::{node::{Static, Constructor, Duplicator, Number, NodeType}, port::{Port, PortType}, Wire};
use parking_lot::RwLockReadGuard;
use log;
use std::ops::{Add, Sub, Mul, Div, Rem, BitAnd, BitOr, BitXor, Shl, Shr, Not, Neg};
use std::sync::atomic::AtomicU64;


// --- Helper Functions for Encoding/Decoding ---

#[inline(always)] fn decode_i64(d: u128) -> i64 { d as i64 }
#[inline(always)] fn encode_i64(r: i64) -> u128 { r as u128 }
#[inline(always)] fn decode_u64(d: u128) -> u64 { d as u64 }
#[inline(always)] fn encode_u64(r: u64) -> u128 { r as u128 }
#[inline(always)] fn decode_f64(d: u128) -> f64 { f64::from_bits(d as u64) }
#[inline(always)] fn encode_f64(r: f64) -> u128 { r.to_bits() as u128 }
#[inline(always)] fn decode_f32(d: u128) -> f32 { f32::from_bits(d as u32) }
#[inline(always)] fn encode_f32(r: f32) -> u128 { r.to_bits() as u128 }
#[inline(always)] fn decode_i32(d: u128) -> i32 { d as i32 }
#[inline(always)] fn encode_i32(r: i32) -> u128 { r as u128 }
#[inline(always)] fn decode_u32(d: u128) -> u32 { d as u32 }
#[inline(always)] fn encode_u32(r: u32) -> u128 { r as u128 }
#[inline(always)] fn decode_i16(d: u128) -> i16 { d as i16 }
#[inline(always)] fn encode_i16(r: i16) -> u128 { r as u128 }
#[inline(always)] fn decode_u16(d: u128) -> u16 { d as u16 }
#[inline(always)] fn encode_u16(r: u16) -> u128 { r as u128 }
#[inline(always)] fn decode_i8(d: u128) -> i8 { d as i8 }
#[inline(always)] fn encode_i8(r: i8) -> u128 { r as u128 }
#[inline(always)] fn decode_u8(d: u128) -> u8 { d as u8 }
#[inline(always)] fn encode_u8(r: u8) -> u128 { r as u128 }
#[inline(always)] fn decode_i128(d: u128) -> i128 { d as i128 }
#[inline(always)] fn encode_i128(r: i128) -> u128 { r as u128 }
#[inline(always)] fn decode_u128(d: u128) -> u128 { d }
#[inline(always)] fn encode_u128(r: u128) -> u128 { r }

// --- Helper Macro for Annihilation on Error ---
macro_rules! handle_intrinsic_error {
    ($fn_name:expr, $err_msg:expr, $s_port:expr, $app_head_port:expr, $caller_port:expr, $read_guard:expr $(, $args:expr)*) => {
        {
            log::error!(concat!("{}: ", $err_msg), $fn_name $(, $args)*);
            remove_node($s_port, $read_guard);
            annihilate_any($app_head_port, $read_guard); // Annihilates args implicitly
            if $caller_port != Port::NULL { annihilate_any($caller_port, $read_guard); }
            return; // Ensure the function exits after handling the error
        }
    };
}

// --- Intrinsic Handler Macros ---

// Macro for Numeric Unary Operations (Number -> Number)
macro_rules! intrinsic_numeric_unop {
    (
        $fn_name:ident,
        $type:ty,
        $op:expr,
        $decode:expr,
        $encode:expr
    ) => {
#[inline]
        unsafe fn $fn_name(
    s_port: Port, app_head_port: Port, caller_port: Port,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let arg0_port = get_aux_port(app_head_port, read_guard, PortType::Left);
    if arg0_port == Port::NULL || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) {
                handle_intrinsic_error!(stringify!($fn_name), "Expected Number arg, found {:?}", s_port, app_head_port, caller_port, read_guard, arg0_port);
            }

            let arg0_data = get_number_data(arg0_port, read_guard);
            let decode_fn: fn(u128) -> $type = $decode;
            let arg0_val = decode_fn(arg0_data);

            let operation: fn($type) -> $type = $op;
            let result_val = operation(arg0_val);

            let encode_fn: fn($type) -> u128 = $encode;
            let result_data = encode_fn(result_val);
            let result_port = alloc_number(result_data, s_port.partition_id(), read_guard);

    connect(caller_port, result_port, read_guard);
            if caller_port.port_type() == PortType::Principal {
                add_active_pair_to_partition(caller_port.partition_id(), Wire(caller_port, result_port), read_guard);
            }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}
    };
}


// Macro for Numeric Binary Operations (Number, Number -> Number)
macro_rules! intrinsic_numeric_binop {
    (
        $fn_name:ident,
        $type:ty,
        $op:expr,
        $decode:expr,
        $encode:expr
    ) => {
#[inline]
        unsafe fn $fn_name(
    s_port: Port, app_head_port: Port, caller_port: Port,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
            if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) {
                 handle_intrinsic_error!(stringify!($fn_name), "Expected inner AppCON, found {:?}", s_port, app_head_port, caller_port, read_guard, app_inner_port);
            }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);

     if arg0_port == Port::NULL || arg1_port == Port::NULL
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number)
                || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) {
                 handle_intrinsic_error!(stringify!($fn_name), "Invalid arg ports/types ({:?}, {:?})", s_port, app_head_port, caller_port, read_guard, arg0_port, arg1_port);
             }

            let arg0_data = get_number_data(arg0_port, read_guard);
            let arg1_data = get_number_data(arg1_port, read_guard);
            let decode_fn: fn(u128) -> $type = $decode;
            let arg0_val = decode_fn(arg0_data);
            let arg1_val = decode_fn(arg1_data);

            let operation: fn($type, $type) -> $type = $op;
            let result_val = operation(arg0_val, arg1_val);

            let encode_fn: fn($type) -> u128 = $encode;
            let result_data = encode_fn(result_val);
            let result_port = alloc_number(result_data, s_port.partition_id(), read_guard);

    connect(caller_port, result_port, read_guard);
            if caller_port.port_type() == PortType::Principal {
                add_active_pair_to_partition(caller_port.partition_id(), Wire(caller_port, result_port), read_guard);
            }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
        }
    };
}

// Macro for Numeric Comparison Operations (Number, Number -> Bool)
macro_rules! intrinsic_numeric_cmpop {
     (
        $fn_name:ident,
        $type:ty,
        $op:expr,
        $decode:expr
    ) => {
#[inline]
         unsafe fn $fn_name(
    s_port: Port, app_head_port: Port, caller_port: Port,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
             if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) {
                 handle_intrinsic_error!(stringify!($fn_name), "Expected inner AppCON, found {:?}", s_port, app_head_port, caller_port, read_guard, app_inner_port);
             }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number)
                 || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Number) {
                  handle_intrinsic_error!(stringify!($fn_name), "Invalid arg ports/types ({:?}, {:?})", s_port, app_head_port, caller_port, read_guard, arg0_port, arg1_port);
             }

             let arg0_data = get_number_data(arg0_port, read_guard);
             let arg1_data = get_number_data(arg1_port, read_guard);
             let decode_fn: fn(u128) -> $type = $decode;
             let arg0_val = decode_fn(arg0_data);
             let arg1_val = decode_fn(arg1_data);

             let operation: fn($type, $type) -> bool = $op;
             let result_bool = operation(arg0_val, arg1_val);

    let result_port = alloc_static_bool(result_bool, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
             if caller_port.port_type() == PortType::Principal {
                 add_active_pair_to_partition(caller_port.partition_id(), Wire(caller_port, result_port), read_guard);
             }

    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}
     };
 }

 // Macro for Boolean Unary Operations (Static(Bool) -> Static(Bool))
 macro_rules! intrinsic_bool_unop {
     (
         $fn_name:ident,
         $op:expr
     ) => {
#[inline]
         unsafe fn $fn_name(
    s_port: Port, app_head_port: Port, caller_port: Port,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
             let arg0_port = get_aux_port(app_head_port, read_guard, PortType::Left);
             if arg0_port == Port::NULL || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Static) {
                 handle_intrinsic_error!(stringify!($fn_name), "Expected Static arg, found {:?}", s_port, app_head_port, caller_port, read_guard, arg0_port);
             }
             let arg0_data = get_static_data(arg0_port, read_guard);
             if decode_static_tag(arg0_data) != TAG_NIL {
                 handle_intrinsic_error!(stringify!($fn_name), "Expected Static(NIL | Bool) arg, found tag {}", s_port, app_head_port, caller_port, read_guard, decode_static_tag(arg0_data));
             }
             let arg0_val = decode_static_payload(arg0_data) != 0;

             let operation: fn(bool) -> bool = $op;
             let result_val = operation(arg0_val);

    let result_port = alloc_static_bool(result_val, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
             if caller_port.port_type() == PortType::Principal {
                 add_active_pair_to_partition(caller_port.partition_id(), Wire(caller_port, result_port), read_guard);
             }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
         }
     };
 }

 // Macro for Boolean Binary Operations (Static(Bool), Static(Bool) -> Static(Bool))
 macro_rules! intrinsic_bool_binop {
     (
         $fn_name:ident,
         $op:expr
     ) => {
#[inline]
         unsafe fn $fn_name(
    s_port: Port, app_head_port: Port, caller_port: Port,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
             if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) {
                 handle_intrinsic_error!(stringify!($fn_name), "Expected inner AppCON, found {:?}", s_port, app_head_port, caller_port, read_guard, app_inner_port);
             }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL
        || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Static)
                 || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Static) {
                  handle_intrinsic_error!(stringify!($fn_name), "Invalid arg ports/types ({:?}, {:?})", s_port, app_head_port, caller_port, read_guard, arg0_port, arg1_port);
              }

             let arg0_data = get_static_data(arg0_port, read_guard);
             let arg1_data = get_static_data(arg1_port, read_guard);
             if decode_static_tag(arg0_data) != TAG_NIL || decode_static_tag(arg1_data) != TAG_NIL {
                 handle_intrinsic_error!(stringify!($fn_name), "Expected Static(NIL | Bool) args, found tags {} {}", s_port, app_head_port, caller_port, read_guard, decode_static_tag(arg0_data), decode_static_tag(arg1_data));
             }
             let arg0_val = decode_static_payload(arg0_data) != 0;
             let arg1_val = decode_static_payload(arg1_data) != 0;

             let operation: fn(bool, bool) -> bool = $op;
             let result_val = operation(arg0_val, arg1_val);

    let result_port = alloc_static_bool(result_val, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
             if caller_port.port_type() == PortType::Principal {
                 add_active_pair_to_partition(caller_port.partition_id(), Wire(caller_port, result_port), read_guard);
             }

    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}
     };
 }

 // Macro for Boolean Comparison Operations (Static(Bool), Static(Bool) -> Bool)
 macro_rules! intrinsic_bool_cmpop {
     (
         $fn_name:ident,
         $op:expr
     ) => {
#[inline]
         unsafe fn $fn_name(
    s_port: Port, app_head_port: Port, caller_port: Port,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let app_inner_port = get_aux_port(app_head_port, read_guard, PortType::Right);
             if app_inner_port == Port::NULL || NodeType::from_u8(app_inner_port.node_type()) != Some(NodeType::Constructor) {
                 handle_intrinsic_error!(stringify!($fn_name), "Expected inner AppCON, found {:?}", s_port, app_head_port, caller_port, read_guard, app_inner_port);
             }
    let arg0_port = get_aux_port(app_inner_port, read_guard, PortType::Left);
    let arg1_port = get_aux_port(app_head_port, read_guard, PortType::Left);
     if arg0_port == Port::NULL || arg1_port == Port::NULL
                 || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Static)
                 || NodeType::from_u8(arg1_port.node_type()) != Some(NodeType::Static) {
                  handle_intrinsic_error!(stringify!($fn_name), "Invalid arg ports/types ({:?}, {:?})", s_port, app_head_port, caller_port, read_guard, arg0_port, arg1_port);
             }

             let arg0_data = get_static_data(arg0_port, read_guard);
             let arg1_data = get_static_data(arg1_port, read_guard);
             if decode_static_tag(arg0_data) != TAG_NIL || decode_static_tag(arg1_data) != TAG_NIL {
                 handle_intrinsic_error!(stringify!($fn_name), "Expected Static(NIL | Bool) args, found tags {} {}", s_port, app_head_port, caller_port, read_guard, decode_static_tag(arg0_data), decode_static_tag(arg1_data));
             }
             let arg0_val = decode_static_payload(arg0_data) != 0;
             let arg1_val = decode_static_payload(arg1_data) != 0;

             let operation: fn(bool, bool) -> bool = $op;
             let result_bool = operation(arg0_val, arg1_val);

    let result_port = alloc_static_bool(result_bool, s_port.partition_id(), read_guard);
    connect(caller_port, result_port, read_guard);
             if caller_port.port_type() == PortType::Principal {
                 add_active_pair_to_partition(caller_port.partition_id(), Wire(caller_port, result_port), read_guard);
             }

    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
}
     };
 }

 // Macro for Numeric Type Conversions (Number -> Number)
 macro_rules! intrinsic_numeric_convop {
     (
         $fn_name:ident,
         $in_type:ty,
         $out_type:ty,
         $op:expr,
         $in_decode:expr,
         $out_encode:expr
     ) => {
#[inline]
         unsafe fn $fn_name(
    s_port: Port, app_head_port: Port, caller_port: Port,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let arg0_port = get_aux_port(app_head_port, read_guard, PortType::Left);
             if arg0_port == Port::NULL || NodeType::from_u8(arg0_port.node_type()) != Some(NodeType::Number) {
                 handle_intrinsic_error!(stringify!($fn_name), "Expected Number arg, found {:?}", s_port, app_head_port, caller_port, read_guard, arg0_port);
             }

             let arg0_data = get_number_data(arg0_port, read_guard);
             let decode_fn: fn(u128) -> $in_type = $in_decode;
             let arg0_val = decode_fn(arg0_data);

             // Use `as` for standard conversions
             let result_val = arg0_val as $out_type;

             let encode_fn: fn($out_type) -> u128 = $out_encode;
             let result_data = encode_fn(result_val);
             let result_port = alloc_number(result_data, s_port.partition_id(), read_guard);

    connect(caller_port, result_port, read_guard);
             if caller_port.port_type() == PortType::Principal {
                 add_active_pair_to_partition(caller_port.partition_id(), Wire(caller_port, result_port), read_guard);
             }
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
         }
     };
 }

// --- Concrete Intrinsic Handlers Generated by Macros ---

// --- u8 Handlers ---
intrinsic_numeric_binop!(intrinsic_u8_add, u8, |a, b| a.wrapping_add(b), decode_u8, encode_u8);
intrinsic_numeric_binop!(intrinsic_u8_sub, u8, |a, b| a.wrapping_sub(b), decode_u8, encode_u8);
intrinsic_numeric_binop!(intrinsic_u8_mul, u8, |a, b| a.wrapping_mul(b), decode_u8, encode_u8);
intrinsic_numeric_binop!(intrinsic_u8_div, u8, |a, b| if b == 0 { 0 } else { a.wrapping_div(b) }, decode_u8, encode_u8);
intrinsic_numeric_binop!(intrinsic_u8_rem, u8, |a, b| if b == 0 { 0 } else { a.wrapping_rem(b) }, decode_u8, encode_u8);
intrinsic_numeric_binop!(intrinsic_u8_and, u8, |a, b| a.bitand(b), decode_u8, encode_u8);
intrinsic_numeric_binop!(intrinsic_u8_or,  u8, |a, b| a.bitor(b), decode_u8, encode_u8);
intrinsic_numeric_binop!(intrinsic_u8_xor, u8, |a, b| a.bitxor(b), decode_u8, encode_u8);
intrinsic_numeric_binop!(intrinsic_u8_shl, u8, |a, b| a.wrapping_shl(b as u32), decode_u8, encode_u8);
intrinsic_numeric_binop!(intrinsic_u8_shr, u8, |a, b| a.wrapping_shr(b as u32), decode_u8, encode_u8);
intrinsic_numeric_unop!(intrinsic_u8_not, u8, |a| a.not(), decode_u8, encode_u8);
intrinsic_numeric_cmpop!(intrinsic_u8_eq, u8, |a, b| a == b, decode_u8);
intrinsic_numeric_cmpop!(intrinsic_u8_ne, u8, |a, b| a != b, decode_u8);
intrinsic_numeric_cmpop!(intrinsic_u8_lt, u8, |a, b| a < b, decode_u8);
intrinsic_numeric_cmpop!(intrinsic_u8_le, u8, |a, b| a <= b, decode_u8);
intrinsic_numeric_cmpop!(intrinsic_u8_gt, u8, |a, b| a > b, decode_u8);
intrinsic_numeric_cmpop!(intrinsic_u8_ge, u8, |a, b| a >= b, decode_u8);

// --- i8 Handlers ---
intrinsic_numeric_binop!(intrinsic_i8_add, i8, |a, b| a.wrapping_add(b), decode_i8, encode_i8);
intrinsic_numeric_binop!(intrinsic_i8_sub, i8, |a, b| a.wrapping_sub(b), decode_i8, encode_i8);
intrinsic_numeric_binop!(intrinsic_i8_mul, i8, |a, b| a.wrapping_mul(b), decode_i8, encode_i8);
intrinsic_numeric_binop!(intrinsic_i8_div, i8, |a, b| if b == 0 { 0 } else { a.wrapping_div(b) }, decode_i8, encode_i8);
intrinsic_numeric_binop!(intrinsic_i8_rem, i8, |a, b| if b == 0 { 0 } else { a.wrapping_rem(b) }, decode_i8, encode_i8);
intrinsic_numeric_binop!(intrinsic_i8_and, i8, |a, b| a.bitand(b), decode_i8, encode_i8);
intrinsic_numeric_binop!(intrinsic_i8_or,  i8, |a, b| a.bitor(b), decode_i8, encode_i8);
intrinsic_numeric_binop!(intrinsic_i8_xor, i8, |a, b| a.bitxor(b), decode_i8, encode_i8);
intrinsic_numeric_binop!(intrinsic_i8_shl, i8, |a, b| a.wrapping_shl(b as u32), decode_i8, encode_i8);
intrinsic_numeric_binop!(intrinsic_i8_shr, i8, |a, b| a.wrapping_shr(b as u32), decode_i8, encode_i8);
intrinsic_numeric_unop!(intrinsic_i8_neg, i8, |a| a.wrapping_neg(), decode_i8, encode_i8);
intrinsic_numeric_unop!(intrinsic_i8_not, i8, |a| a.not(), decode_i8, encode_i8);
intrinsic_numeric_cmpop!(intrinsic_i8_eq, i8, |a, b| a == b, decode_i8);
intrinsic_numeric_cmpop!(intrinsic_i8_ne, i8, |a, b| a != b, decode_i8);
intrinsic_numeric_cmpop!(intrinsic_i8_lt, i8, |a, b| a < b, decode_i8);
intrinsic_numeric_cmpop!(intrinsic_i8_le, i8, |a, b| a <= b, decode_i8);
intrinsic_numeric_cmpop!(intrinsic_i8_gt, i8, |a, b| a > b, decode_i8);
intrinsic_numeric_cmpop!(intrinsic_i8_ge, i8, |a, b| a >= b, decode_i8);

// --- u16 Handlers ---
intrinsic_numeric_binop!(intrinsic_u16_add, u16, |a, b| a.wrapping_add(b), decode_u16, encode_u16);
intrinsic_numeric_binop!(intrinsic_u16_sub, u16, |a, b| a.wrapping_sub(b), decode_u16, encode_u16);
intrinsic_numeric_binop!(intrinsic_u16_mul, u16, |a, b| a.wrapping_mul(b), decode_u16, encode_u16);
intrinsic_numeric_binop!(intrinsic_u16_div, u16, |a, b| if b == 0 { 0 } else { a.wrapping_div(b) }, decode_u16, encode_u16);
intrinsic_numeric_binop!(intrinsic_u16_rem, u16, |a, b| if b == 0 { 0 } else { a.wrapping_rem(b) }, decode_u16, encode_u16);
intrinsic_numeric_binop!(intrinsic_u16_and, u16, |a, b| a.bitand(b), decode_u16, encode_u16);
intrinsic_numeric_binop!(intrinsic_u16_or,  u16, |a, b| a.bitor(b), decode_u16, encode_u16);
intrinsic_numeric_binop!(intrinsic_u16_xor, u16, |a, b| a.bitxor(b), decode_u16, encode_u16);
intrinsic_numeric_binop!(intrinsic_u16_shl, u16, |a, b| a.wrapping_shl(b as u32), decode_u16, encode_u16);
intrinsic_numeric_binop!(intrinsic_u16_shr, u16, |a, b| a.wrapping_shr(b as u32), decode_u16, encode_u16);
intrinsic_numeric_unop!(intrinsic_u16_not, u16, |a| a.not(), decode_u16, encode_u16);
intrinsic_numeric_cmpop!(intrinsic_u16_eq, u16, |a, b| a == b, decode_u16);
intrinsic_numeric_cmpop!(intrinsic_u16_ne, u16, |a, b| a != b, decode_u16);
intrinsic_numeric_cmpop!(intrinsic_u16_lt, u16, |a, b| a < b, decode_u16);
intrinsic_numeric_cmpop!(intrinsic_u16_le, u16, |a, b| a <= b, decode_u16);
intrinsic_numeric_cmpop!(intrinsic_u16_gt, u16, |a, b| a > b, decode_u16);
intrinsic_numeric_cmpop!(intrinsic_u16_ge, u16, |a, b| a >= b, decode_u16);

// --- i16 Handlers ---
intrinsic_numeric_binop!(intrinsic_i16_add, i16, |a, b| a.wrapping_add(b), decode_i16, encode_i16);
intrinsic_numeric_binop!(intrinsic_i16_sub, i16, |a, b| a.wrapping_sub(b), decode_i16, encode_i16);
intrinsic_numeric_binop!(intrinsic_i16_mul, i16, |a, b| a.wrapping_mul(b), decode_i16, encode_i16);
intrinsic_numeric_binop!(intrinsic_i16_div, i16, |a, b| if b == 0 { 0 } else { a.wrapping_div(b) }, decode_i16, encode_i16);
intrinsic_numeric_binop!(intrinsic_i16_rem, i16, |a, b| if b == 0 { 0 } else { a.wrapping_rem(b) }, decode_i16, encode_i16);
intrinsic_numeric_binop!(intrinsic_i16_and, i16, |a, b| a.bitand(b), decode_i16, encode_i16);
intrinsic_numeric_binop!(intrinsic_i16_or,  i16, |a, b| a.bitor(b), decode_i16, encode_i16);
intrinsic_numeric_binop!(intrinsic_i16_xor, i16, |a, b| a.bitxor(b), decode_i16, encode_i16);
intrinsic_numeric_binop!(intrinsic_i16_shl, i16, |a, b| a.wrapping_shl(b as u32), decode_i16, encode_i16);
intrinsic_numeric_binop!(intrinsic_i16_shr, i16, |a, b| a.wrapping_shr(b as u32), decode_i16, encode_i16);
intrinsic_numeric_unop!(intrinsic_i16_neg, i16, |a| a.wrapping_neg(), decode_i16, encode_i16);
intrinsic_numeric_unop!(intrinsic_i16_not, i16, |a| a.not(), decode_i16, encode_i16);
intrinsic_numeric_cmpop!(intrinsic_i16_eq, i16, |a, b| a == b, decode_i16);
intrinsic_numeric_cmpop!(intrinsic_i16_ne, i16, |a, b| a != b, decode_i16);
intrinsic_numeric_cmpop!(intrinsic_i16_lt, i16, |a, b| a < b, decode_i16);
intrinsic_numeric_cmpop!(intrinsic_i16_le, i16, |a, b| a <= b, decode_i16);
intrinsic_numeric_cmpop!(intrinsic_i16_gt, i16, |a, b| a > b, decode_i16);
intrinsic_numeric_cmpop!(intrinsic_i16_ge, i16, |a, b| a >= b, decode_i16);

// --- u32 Handlers ---
intrinsic_numeric_binop!(intrinsic_u32_add, u32, |a, b| a.wrapping_add(b), decode_u32, encode_u32);
intrinsic_numeric_binop!(intrinsic_u32_sub, u32, |a, b| a.wrapping_sub(b), decode_u32, encode_u32);
intrinsic_numeric_binop!(intrinsic_u32_mul, u32, |a, b| a.wrapping_mul(b), decode_u32, encode_u32);
intrinsic_numeric_binop!(intrinsic_u32_div, u32, |a, b| if b == 0 { 0 } else { a.wrapping_div(b) }, decode_u32, encode_u32);
intrinsic_numeric_binop!(intrinsic_u32_rem, u32, |a, b| if b == 0 { 0 } else { a.wrapping_rem(b) }, decode_u32, encode_u32);
intrinsic_numeric_binop!(intrinsic_u32_and, u32, |a, b| a.bitand(b), decode_u32, encode_u32);
intrinsic_numeric_binop!(intrinsic_u32_or,  u32, |a, b| a.bitor(b), decode_u32, encode_u32);
intrinsic_numeric_binop!(intrinsic_u32_xor, u32, |a, b| a.bitxor(b), decode_u32, encode_u32);
intrinsic_numeric_binop!(intrinsic_u32_shl, u32, |a, b| a.wrapping_shl(b), decode_u32, encode_u32);
intrinsic_numeric_binop!(intrinsic_u32_shr, u32, |a, b| a.wrapping_shr(b), decode_u32, encode_u32);
intrinsic_numeric_unop!(intrinsic_u32_not, u32, |a| a.not(), decode_u32, encode_u32);
intrinsic_numeric_cmpop!(intrinsic_u32_eq, u32, |a, b| a == b, decode_u32);
intrinsic_numeric_cmpop!(intrinsic_u32_ne, u32, |a, b| a != b, decode_u32);
intrinsic_numeric_cmpop!(intrinsic_u32_lt, u32, |a, b| a < b, decode_u32);
intrinsic_numeric_cmpop!(intrinsic_u32_le, u32, |a, b| a <= b, decode_u32);
intrinsic_numeric_cmpop!(intrinsic_u32_gt, u32, |a, b| a > b, decode_u32);
intrinsic_numeric_cmpop!(intrinsic_u32_ge, u32, |a, b| a >= b, decode_u32);

// --- i32 Handlers ---
intrinsic_numeric_binop!(intrinsic_i32_add, i32, |a, b| a.wrapping_add(b), decode_i32, encode_i32);
intrinsic_numeric_binop!(intrinsic_i32_sub, i32, |a, b| a.wrapping_sub(b), decode_i32, encode_i32);
intrinsic_numeric_binop!(intrinsic_i32_mul, i32, |a, b| a.wrapping_mul(b), decode_i32, encode_i32);
intrinsic_numeric_binop!(intrinsic_i32_div, i32, |a, b| if b == 0 { 0 } else { a.wrapping_div(b) }, decode_i32, encode_i32);
intrinsic_numeric_binop!(intrinsic_i32_rem, i32, |a, b| if b == 0 { 0 } else { a.wrapping_rem(b) }, decode_i32, encode_i32);
intrinsic_numeric_binop!(intrinsic_i32_and, i32, |a, b| a.bitand(b), decode_i32, encode_i32);
intrinsic_numeric_binop!(intrinsic_i32_or,  i32, |a, b| a.bitor(b), decode_i32, encode_i32);
intrinsic_numeric_binop!(intrinsic_i32_xor, i32, |a, b| a.bitxor(b), decode_i32, encode_i32);
intrinsic_numeric_binop!(intrinsic_i32_shl, i32, |a, b| a.wrapping_shl(b as u32), decode_i32, encode_i32);
intrinsic_numeric_binop!(intrinsic_i32_shr, i32, |a, b| a.wrapping_shr(b as u32), decode_i32, encode_i32);
intrinsic_numeric_unop!(intrinsic_i32_neg, i32, |a| a.wrapping_neg(), decode_i32, encode_i32);
intrinsic_numeric_unop!(intrinsic_i32_not, i32, |a| a.not(), decode_i32, encode_i32);
intrinsic_numeric_cmpop!(intrinsic_i32_eq, i32, |a, b| a == b, decode_i32);
intrinsic_numeric_cmpop!(intrinsic_i32_ne, i32, |a, b| a != b, decode_i32);
intrinsic_numeric_cmpop!(intrinsic_i32_lt, i32, |a, b| a < b, decode_i32);
intrinsic_numeric_cmpop!(intrinsic_i32_le, i32, |a, b| a <= b, decode_i32);
intrinsic_numeric_cmpop!(intrinsic_i32_gt, i32, |a, b| a > b, decode_i32);
intrinsic_numeric_cmpop!(intrinsic_i32_ge, i32, |a, b| a >= b, decode_i32);

// --- u64 Handlers ---
intrinsic_numeric_binop!(intrinsic_u64_add, u64, |a, b| a.wrapping_add(b), decode_u64, encode_u64);
intrinsic_numeric_binop!(intrinsic_u64_sub, u64, |a, b| a.wrapping_sub(b), decode_u64, encode_u64);
intrinsic_numeric_binop!(intrinsic_u64_mul, u64, |a, b| a.wrapping_mul(b), decode_u64, encode_u64);
intrinsic_numeric_binop!(intrinsic_u64_div, u64, |a, b| if b == 0 { 0 } else { a.wrapping_div(b) }, decode_u64, encode_u64);
intrinsic_numeric_binop!(intrinsic_u64_rem, u64, |a, b| if b == 0 { 0 } else { a.wrapping_rem(b) }, decode_u64, encode_u64);
intrinsic_numeric_binop!(intrinsic_u64_and, u64, |a, b| a.bitand(b), decode_u64, encode_u64);
intrinsic_numeric_binop!(intrinsic_u64_or,  u64, |a, b| a.bitor(b), decode_u64, encode_u64);
intrinsic_numeric_binop!(intrinsic_u64_xor, u64, |a, b| a.bitxor(b), decode_u64, encode_u64);
intrinsic_numeric_binop!(intrinsic_u64_shl, u64, |a, b| a.wrapping_shl(b as u32), decode_u64, encode_u64);
intrinsic_numeric_binop!(intrinsic_u64_shr, u64, |a, b| a.wrapping_shr(b as u32), decode_u64, encode_u64);
intrinsic_numeric_unop!(intrinsic_u64_not, u64, |a| a.not(), decode_u64, encode_u64);
intrinsic_numeric_cmpop!(intrinsic_u64_eq, u64, |a, b| a == b, decode_u64);
intrinsic_numeric_cmpop!(intrinsic_u64_ne, u64, |a, b| a != b, decode_u64);
intrinsic_numeric_cmpop!(intrinsic_u64_lt, u64, |a, b| a < b, decode_u64);
intrinsic_numeric_cmpop!(intrinsic_u64_le, u64, |a, b| a <= b, decode_u64);
intrinsic_numeric_cmpop!(intrinsic_u64_gt, u64, |a, b| a > b, decode_u64);
intrinsic_numeric_cmpop!(intrinsic_u64_ge, u64, |a, b| a >= b, decode_u64);

// --- i64 Handlers ---
intrinsic_numeric_binop!(intrinsic_i64_add, i64, |a, b| a.wrapping_add(b), decode_i64, encode_i64);
intrinsic_numeric_binop!(intrinsic_i64_sub, i64, |a, b| a.wrapping_sub(b), decode_i64, encode_i64);
intrinsic_numeric_binop!(intrinsic_i64_mul, i64, |a, b| a.wrapping_mul(b), decode_i64, encode_i64);
intrinsic_numeric_binop!(intrinsic_i64_div, i64, |a, b| if b == 0 { 0 } else { a.wrapping_div(b) }, decode_i64, encode_i64);
intrinsic_numeric_binop!(intrinsic_i64_rem, i64, |a, b| if b == 0 { 0 } else { a.wrapping_rem(b) }, decode_i64, encode_i64);
intrinsic_numeric_binop!(intrinsic_i64_and, i64, |a, b| a.bitand(b), decode_i64, encode_i64);
intrinsic_numeric_binop!(intrinsic_i64_or,  i64, |a, b| a.bitor(b), decode_i64, encode_i64);
intrinsic_numeric_binop!(intrinsic_i64_xor, i64, |a, b| a.bitxor(b), decode_i64, encode_i64);
intrinsic_numeric_binop!(intrinsic_i64_shl, i64, |a, b| a.wrapping_shl(b as u32), decode_i64, encode_i64);
intrinsic_numeric_binop!(intrinsic_i64_shr, i64, |a, b| a.wrapping_shr(b as u32), decode_i64, encode_i64);
intrinsic_numeric_unop!(intrinsic_i64_neg, i64, |a| a.wrapping_neg(), decode_i64, encode_i64);
intrinsic_numeric_unop!(intrinsic_i64_not, i64, |a| a.not(), decode_i64, encode_i64);
intrinsic_numeric_cmpop!(intrinsic_i64_eq, i64, |a, b| a == b, decode_i64);
intrinsic_numeric_cmpop!(intrinsic_i64_ne, i64, |a, b| a != b, decode_i64);
intrinsic_numeric_cmpop!(intrinsic_i64_lt, i64, |a, b| a < b, decode_i64);
intrinsic_numeric_cmpop!(intrinsic_i64_le, i64, |a, b| a <= b, decode_i64);
intrinsic_numeric_cmpop!(intrinsic_i64_gt, i64, |a, b| a > b, decode_i64);
intrinsic_numeric_cmpop!(intrinsic_i64_ge, i64, |a, b| a >= b, decode_i64);

// --- u128 Handlers ---
intrinsic_numeric_binop!(intrinsic_u128_add, u128, |a, b| a.wrapping_add(b), decode_u128, encode_u128);
intrinsic_numeric_binop!(intrinsic_u128_sub, u128, |a, b| a.wrapping_sub(b), decode_u128, encode_u128);
intrinsic_numeric_binop!(intrinsic_u128_mul, u128, |a, b| a.wrapping_mul(b), decode_u128, encode_u128);
intrinsic_numeric_binop!(intrinsic_u128_div, u128, |a, b| if b == 0 { 0 } else { a.wrapping_div(b) }, decode_u128, encode_u128);
intrinsic_numeric_binop!(intrinsic_u128_rem, u128, |a, b| if b == 0 { 0 } else { a.wrapping_rem(b) }, decode_u128, encode_u128);
intrinsic_numeric_binop!(intrinsic_u128_and, u128, |a, b| a.bitand(b), decode_u128, encode_u128);
intrinsic_numeric_binop!(intrinsic_u128_or,  u128, |a, b| a.bitor(b), decode_u128, encode_u128);
intrinsic_numeric_binop!(intrinsic_u128_xor, u128, |a, b| a.bitxor(b), decode_u128, encode_u128);
intrinsic_numeric_binop!(intrinsic_u128_shl, u128, |a, b| a.wrapping_shl(b as u32), decode_u128, encode_u128);
intrinsic_numeric_binop!(intrinsic_u128_shr, u128, |a, b| a.wrapping_shr(b as u32), decode_u128, encode_u128);
intrinsic_numeric_unop!(intrinsic_u128_not, u128, |a| a.not(), decode_u128, encode_u128);
intrinsic_numeric_cmpop!(intrinsic_u128_eq, u128, |a, b| a == b, decode_u128);
intrinsic_numeric_cmpop!(intrinsic_u128_ne, u128, |a, b| a != b, decode_u128);
intrinsic_numeric_cmpop!(intrinsic_u128_lt, u128, |a, b| a < b, decode_u128);
intrinsic_numeric_cmpop!(intrinsic_u128_le, u128, |a, b| a <= b, decode_u128);
intrinsic_numeric_cmpop!(intrinsic_u128_gt, u128, |a, b| a > b, decode_u128);
intrinsic_numeric_cmpop!(intrinsic_u128_ge, u128, |a, b| a >= b, decode_u128);

// --- i128 Handlers ---
intrinsic_numeric_binop!(intrinsic_i128_add, i128, |a, b| a.wrapping_add(b), decode_i128, encode_i128);
intrinsic_numeric_binop!(intrinsic_i128_sub, i128, |a, b| a.wrapping_sub(b), decode_i128, encode_i128);
intrinsic_numeric_binop!(intrinsic_i128_mul, i128, |a, b| a.wrapping_mul(b), decode_i128, encode_i128);
intrinsic_numeric_binop!(intrinsic_i128_div, i128, |a, b| if b == 0 { 0 } else { a.wrapping_div(b) }, decode_i128, encode_i128);
intrinsic_numeric_binop!(intrinsic_i128_rem, i128, |a, b| if b == 0 { 0 } else { a.wrapping_rem(b) }, decode_i128, encode_i128);
intrinsic_numeric_binop!(intrinsic_i128_and, i128, |a, b| a.bitand(b), decode_i128, encode_i128);
intrinsic_numeric_binop!(intrinsic_i128_or,  i128, |a, b| a.bitor(b), decode_i128, encode_i128);
intrinsic_numeric_binop!(intrinsic_i128_xor, i128, |a, b| a.bitxor(b), decode_i128, encode_i128);
intrinsic_numeric_binop!(intrinsic_i128_shl, i128, |a, b| a.wrapping_shl(b as u32), decode_i128, encode_i128);
intrinsic_numeric_binop!(intrinsic_i128_shr, i128, |a, b| a.wrapping_shr(b as u32), decode_i128, encode_i128);
intrinsic_numeric_unop!(intrinsic_i128_neg, i128, |a| a.wrapping_neg(), decode_i128, encode_i128);
intrinsic_numeric_unop!(intrinsic_i128_not, i128, |a| a.not(), decode_i128, encode_i128);
intrinsic_numeric_cmpop!(intrinsic_i128_eq, i128, |a, b| a == b, decode_i128);
intrinsic_numeric_cmpop!(intrinsic_i128_ne, i128, |a, b| a != b, decode_i128);
intrinsic_numeric_cmpop!(intrinsic_i128_lt, i128, |a, b| a < b, decode_i128);
intrinsic_numeric_cmpop!(intrinsic_i128_le, i128, |a, b| a <= b, decode_i128);
intrinsic_numeric_cmpop!(intrinsic_i128_gt, i128, |a, b| a > b, decode_i128);
intrinsic_numeric_cmpop!(intrinsic_i128_ge, i128, |a, b| a >= b, decode_i128);

// --- f32 Handlers ---
intrinsic_numeric_binop!(intrinsic_f32_add, f32, |a, b| a + b, decode_f32, encode_f32);
intrinsic_numeric_binop!(intrinsic_f32_sub, f32, |a, b| a - b, decode_f32, encode_f32);
intrinsic_numeric_binop!(intrinsic_f32_mul, f32, |a, b| a * b, decode_f32, encode_f32);
intrinsic_numeric_binop!(intrinsic_f32_div, f32, |a, b| a / b, decode_f32, encode_f32);
intrinsic_numeric_binop!(intrinsic_f32_rem, f32, |a, b| a % b, decode_f32, encode_f32);
intrinsic_numeric_unop!(intrinsic_f32_neg, f32, |a: f32| -a, decode_f32, encode_f32);
intrinsic_numeric_cmpop!(intrinsic_f32_eq, f32, |a, b| a == b, decode_f32);
intrinsic_numeric_cmpop!(intrinsic_f32_ne, f32, |a, b| a != b, decode_f32);
intrinsic_numeric_cmpop!(intrinsic_f32_lt, f32, |a, b| a < b, decode_f32);
intrinsic_numeric_cmpop!(intrinsic_f32_le, f32, |a, b| a <= b, decode_f32);
intrinsic_numeric_cmpop!(intrinsic_f32_gt, f32, |a, b| a > b, decode_f32);
intrinsic_numeric_cmpop!(intrinsic_f32_ge, f32, |a, b| a >= b, decode_f32);

// --- f64 Handlers ---
intrinsic_numeric_binop!(intrinsic_f64_add, f64, |a, b| a + b, decode_f64, encode_f64);
intrinsic_numeric_binop!(intrinsic_f64_sub, f64, |a, b| a - b, decode_f64, encode_f64);
intrinsic_numeric_binop!(intrinsic_f64_mul, f64, |a, b| a * b, decode_f64, encode_f64);
intrinsic_numeric_binop!(intrinsic_f64_div, f64, |a, b| a / b, decode_f64, encode_f64);
intrinsic_numeric_binop!(intrinsic_f64_rem, f64, |a, b| a % b, decode_f64, encode_f64);
intrinsic_numeric_unop!(intrinsic_f64_neg, f64, |a: f64| -a, decode_f64, encode_f64);
intrinsic_numeric_cmpop!(intrinsic_f64_eq, f64, |a, b| a == b, decode_f64);
intrinsic_numeric_cmpop!(intrinsic_f64_ne, f64, |a, b| a != b, decode_f64);
intrinsic_numeric_cmpop!(intrinsic_f64_lt, f64, |a, b| a < b, decode_f64);
intrinsic_numeric_cmpop!(intrinsic_f64_le, f64, |a, b| a <= b, decode_f64);
intrinsic_numeric_cmpop!(intrinsic_f64_gt, f64, |a, b| a > b, decode_f64);
intrinsic_numeric_cmpop!(intrinsic_f64_ge, f64, |a, b| a >= b, decode_f64);

// --- bool Handlers ---
intrinsic_bool_binop!(intrinsic_bool_and, |a, b| a && b);
intrinsic_bool_binop!(intrinsic_bool_or, |a, b| a || b);
intrinsic_bool_binop!(intrinsic_bool_xor, |a, b| a ^ b);
intrinsic_bool_unop!(intrinsic_bool_not, |a| !a);
intrinsic_bool_cmpop!(intrinsic_bool_eq, |a, b| a == b);
intrinsic_bool_cmpop!(intrinsic_bool_ne, |a, b| a != b);

// --- ABS Handlers ---
intrinsic_numeric_unop!(intrinsic_u8_abs,   u8,   |a| a,                decode_u8, encode_u8);
intrinsic_numeric_unop!(intrinsic_i8_abs,   i8,   |a| a.wrapping_abs(), decode_i8, encode_i8);
intrinsic_numeric_unop!(intrinsic_u16_abs,  u16,  |a| a,                decode_u16, encode_u16);
intrinsic_numeric_unop!(intrinsic_i16_abs,  i16,  |a| a.wrapping_abs(), decode_i16, encode_i16);
intrinsic_numeric_unop!(intrinsic_u32_abs,  u32,  |a| a,                decode_u32, encode_u32);
intrinsic_numeric_unop!(intrinsic_i32_abs,  i32,  |a| a.wrapping_abs(), decode_i32, encode_i32);
intrinsic_numeric_unop!(intrinsic_u64_abs,  u64,  |a| a,                decode_u64, encode_u64);
intrinsic_numeric_unop!(intrinsic_i64_abs,  i64,  |a| a.wrapping_abs(), decode_i64, encode_i64);
intrinsic_numeric_unop!(intrinsic_u128_abs, u128, |a| a,                decode_u128, encode_u128);
intrinsic_numeric_unop!(intrinsic_i128_abs, i128, |a| a.wrapping_abs(), decode_i128, encode_i128);
intrinsic_numeric_unop!(intrinsic_f32_abs,  f32,  |a| a.abs(),          decode_f32, encode_f32);
intrinsic_numeric_unop!(intrinsic_f64_abs,  f64,  |a| a.abs(),          decode_f64, encode_f64);

// --- POW Handlers ---
intrinsic_numeric_binop!(intrinsic_u8_pow,   u8,   |a, b| a.wrapping_pow(b as u32), decode_u8,   encode_u8);
intrinsic_numeric_binop!(intrinsic_i8_pow,   i8,   |a, b| a.wrapping_pow(b as u32), decode_i8,   encode_i8);
intrinsic_numeric_binop!(intrinsic_u16_pow,  u16,  |a, b| a.wrapping_pow(b as u32), decode_u16,  encode_u16);
intrinsic_numeric_binop!(intrinsic_i16_pow,  i16,  |a, b| a.wrapping_pow(b as u32), decode_i16,  encode_i16);
intrinsic_numeric_binop!(intrinsic_u32_pow,  u32,  |a, b| a.wrapping_pow(b as u32), decode_u32,  encode_u32);
intrinsic_numeric_binop!(intrinsic_i32_pow,  i32,  |a, b| a.wrapping_pow(b as u32), decode_i32,  encode_i32);
intrinsic_numeric_binop!(intrinsic_u64_pow,  u64,  |a, b| a.wrapping_pow(b as u32), decode_u64,  encode_u64);
intrinsic_numeric_binop!(intrinsic_i64_pow,  i64,  |a, b| a.wrapping_pow(b as u32), decode_i64,  encode_i64);
intrinsic_numeric_binop!(intrinsic_u128_pow, u128, |a, b| a.wrapping_pow(b as u32), decode_u128, encode_u128);
intrinsic_numeric_binop!(intrinsic_i128_pow, i128, |a, b| a.wrapping_pow(b as u32), decode_i128, encode_i128);
intrinsic_numeric_binop!(intrinsic_f32_pow,  f32,  |a, b| a.powf(b),              decode_f32,  encode_f32);
intrinsic_numeric_binop!(intrinsic_f64_pow,  f64,  |a, b| a.powf(b),              decode_f64,  encode_f64);

// --- Conversion Handlers ---

// From u8
intrinsic_numeric_convop!(intrinsic_conv_u8_i8,   u8, i8,   |a| a as i8,   decode_u8, encode_i8);
intrinsic_numeric_convop!(intrinsic_conv_u8_u16,  u8, u16,  |a| a as u16,  decode_u8, encode_u16);
intrinsic_numeric_convop!(intrinsic_conv_u8_i16,  u8, i16,  |a| a as i16,  decode_u8, encode_i16);
intrinsic_numeric_convop!(intrinsic_conv_u8_u32,  u8, u32,  |a| a as u32,  decode_u8, encode_u32);
intrinsic_numeric_convop!(intrinsic_conv_u8_i32,  u8, i32,  |a| a as i32,  decode_u8, encode_i32);
intrinsic_numeric_convop!(intrinsic_conv_u8_u64,  u8, u64,  |a| a as u64,  decode_u8, encode_u64);
intrinsic_numeric_convop!(intrinsic_conv_u8_i64,  u8, i64,  |a| a as i64,  decode_u8, encode_i64);
intrinsic_numeric_convop!(intrinsic_conv_u8_u128, u8, u128, |a| a as u128, decode_u8, encode_u128);
intrinsic_numeric_convop!(intrinsic_conv_u8_i128, u8, i128, |a| a as i128, decode_u8, encode_i128);
intrinsic_numeric_convop!(intrinsic_conv_u8_f32,  u8, f32,  |a| a as f32,  decode_u8, encode_f32);
intrinsic_numeric_convop!(intrinsic_conv_u8_f64,  u8, f64,  |a| a as f64,  decode_u8, encode_f64);

// From i8
intrinsic_numeric_convop!(intrinsic_conv_i8_u8,   i8, u8,   |a| a as u8,   decode_i8, encode_u8);
intrinsic_numeric_convop!(intrinsic_conv_i8_u16,  i8, u16,  |a| a as u16,  decode_i8, encode_u16);
intrinsic_numeric_convop!(intrinsic_conv_i8_i16,  i8, i16,  |a| a as i16,  decode_i8, encode_i16);
intrinsic_numeric_convop!(intrinsic_conv_i8_u32,  i8, u32,  |a| a as u32,  decode_i8, encode_u32);
intrinsic_numeric_convop!(intrinsic_conv_i8_i32,  i8, i32,  |a| a as i32,  decode_i8, encode_i32);
intrinsic_numeric_convop!(intrinsic_conv_i8_u64,  i8, u64,  |a| a as u64,  decode_i8, encode_u64);
intrinsic_numeric_convop!(intrinsic_conv_i8_i64,  i8, i64,  |a| a as i64,  decode_i8, encode_i64);
intrinsic_numeric_convop!(intrinsic_conv_i8_u128, i8, u128, |a| a as u128, decode_i8, encode_u128);
intrinsic_numeric_convop!(intrinsic_conv_i8_i128, i8, i128, |a| a as i128, decode_i8, encode_i128);
intrinsic_numeric_convop!(intrinsic_conv_i8_f32,  i8, f32,  |a| a as f32,  decode_i8, encode_f32);
intrinsic_numeric_convop!(intrinsic_conv_i8_f64,  i8, f64,  |a| a as f64,  decode_i8, encode_f64);

// From u16
intrinsic_numeric_convop!(intrinsic_conv_u16_u8,   u16, u8,   |a| a as u8,   decode_u16, encode_u8);
intrinsic_numeric_convop!(intrinsic_conv_u16_i8,   u16, i8,   |a| a as i8,   decode_u16, encode_i8);
intrinsic_numeric_convop!(intrinsic_conv_u16_i16,  u16, i16,  |a| a as i16,  decode_u16, encode_i16);
intrinsic_numeric_convop!(intrinsic_conv_u16_u32,  u16, u32,  |a| a as u32,  decode_u16, encode_u32);
intrinsic_numeric_convop!(intrinsic_conv_u16_i32,  u16, i32,  |a| a as i32,  decode_u16, encode_i32);
intrinsic_numeric_convop!(intrinsic_conv_u16_u64,  u16, u64,  |a| a as u64,  decode_u16, encode_u64);
intrinsic_numeric_convop!(intrinsic_conv_u16_i64,  u16, i64,  |a| a as i64,  decode_u16, encode_i64);
intrinsic_numeric_convop!(intrinsic_conv_u16_u128, u16, u128, |a| a as u128, decode_u16, encode_u128);
intrinsic_numeric_convop!(intrinsic_conv_u16_i128, u16, i128, |a| a as i128, decode_u16, encode_i128);
intrinsic_numeric_convop!(intrinsic_conv_u16_f32,  u16, f32,  |a| a as f32,  decode_u16, encode_f32);
intrinsic_numeric_convop!(intrinsic_conv_u16_f64,  u16, f64,  |a| a as f64,  decode_u16, encode_f64);

// From i16
intrinsic_numeric_convop!(intrinsic_conv_i16_u8,   i16, u8,   |a| a as u8,   decode_i16, encode_u8);
intrinsic_numeric_convop!(intrinsic_conv_i16_i8,   i16, i8,   |a| a as i8,   decode_i16, encode_i8);
intrinsic_numeric_convop!(intrinsic_conv_i16_u16,  i16, u16,  |a| a as u16,  decode_i16, encode_u16);
intrinsic_numeric_convop!(intrinsic_conv_i16_u32,  i16, u32,  |a| a as u32,  decode_i16, encode_u32);
intrinsic_numeric_convop!(intrinsic_conv_i16_i32,  i16, i32,  |a| a as i32,  decode_i16, encode_i32);
intrinsic_numeric_convop!(intrinsic_conv_i16_u64,  i16, u64,  |a| a as u64,  decode_i16, encode_u64);
intrinsic_numeric_convop!(intrinsic_conv_i16_i64,  i16, i64,  |a| a as i64,  decode_i16, encode_i64);
intrinsic_numeric_convop!(intrinsic_conv_i16_u128, i16, u128, |a| a as u128, decode_i16, encode_u128);
intrinsic_numeric_convop!(intrinsic_conv_i16_i128, i16, i128, |a| a as i128, decode_i16, encode_i128);
intrinsic_numeric_convop!(intrinsic_conv_i16_f32,  i16, f32,  |a| a as f32,  decode_i16, encode_f32);
intrinsic_numeric_convop!(intrinsic_conv_i16_f64,  i16, f64,  |a| a as f64,  decode_i16, encode_f64);

// From u32
intrinsic_numeric_convop!(intrinsic_conv_u32_u8,   u32, u8,   |a| a as u8,   decode_u32, encode_u8);
intrinsic_numeric_convop!(intrinsic_conv_u32_i8,   u32, i8,   |a| a as i8,   decode_u32, encode_i8);
intrinsic_numeric_convop!(intrinsic_conv_u32_u16,  u32, u16,  |a| a as u16,  decode_u32, encode_u16);
intrinsic_numeric_convop!(intrinsic_conv_u32_i16,  u32, i16,  |a| a as i16,  decode_u32, encode_i16);
intrinsic_numeric_convop!(intrinsic_conv_u32_i32,  u32, i32,  |a| a as i32,  decode_u32, encode_i32);
intrinsic_numeric_convop!(intrinsic_conv_u32_u64,  u32, u64,  |a| a as u64,  decode_u32, encode_u64);
intrinsic_numeric_convop!(intrinsic_conv_u32_i64,  u32, i64,  |a| a as i64,  decode_u32, encode_i64);
intrinsic_numeric_convop!(intrinsic_conv_u32_u128, u32, u128, |a| a as u128, decode_u32, encode_u128);
intrinsic_numeric_convop!(intrinsic_conv_u32_i128, u32, i128, |a| a as i128, decode_u32, encode_i128);
intrinsic_numeric_convop!(intrinsic_conv_u32_f32,  u32, f32,  |a| a as f32,  decode_u32, encode_f32);
intrinsic_numeric_convop!(intrinsic_conv_u32_f64,  u32, f64,  |a| a as f64,  decode_u32, encode_f64);

// From i32
intrinsic_numeric_convop!(intrinsic_conv_i32_u8,   i32, u8,   |a| a as u8,   decode_i32, encode_u8);
intrinsic_numeric_convop!(intrinsic_conv_i32_i8,   i32, i8,   |a| a as i8,   decode_i32, encode_i8);
intrinsic_numeric_convop!(intrinsic_conv_i32_u16,  i32, u16,  |a| a as u16,  decode_i32, encode_u16);
intrinsic_numeric_convop!(intrinsic_conv_i32_i16,  i32, i16,  |a| a as i16,  decode_i32, encode_i16);
intrinsic_numeric_convop!(intrinsic_conv_i32_u32,  i32, u32,  |a| a as u32,  decode_i32, encode_u32);
intrinsic_numeric_convop!(intrinsic_conv_i32_u64,  i32, u64,  |a| a as u64,  decode_i32, encode_u64);
intrinsic_numeric_convop!(intrinsic_conv_i32_i64,  i32, i64,  |a| a as i64,  decode_i32, encode_i64);
intrinsic_numeric_convop!(intrinsic_conv_i32_u128, i32, u128, |a| a as u128, decode_i32, encode_u128);
intrinsic_numeric_convop!(intrinsic_conv_i32_i128, i32, i128, |a| a as i128, decode_i32, encode_i128);
intrinsic_numeric_convop!(intrinsic_conv_i32_f32,  i32, f32,  |a| a as f32,  decode_i32, encode_f32);
intrinsic_numeric_convop!(intrinsic_conv_i32_f64,  i32, f64,  |a| a as f64,  decode_i32, encode_f64);

// From u64
intrinsic_numeric_convop!(intrinsic_conv_u64_u8,   u64, u8,   |a| a as u8,   decode_u64, encode_u8);
intrinsic_numeric_convop!(intrinsic_conv_u64_i8,   u64, i8,   |a| a as i8,   decode_u64, encode_i8);
intrinsic_numeric_convop!(intrinsic_conv_u64_u16,  u64, u16,  |a| a as u16,  decode_u64, encode_u16);
intrinsic_numeric_convop!(intrinsic_conv_u64_i16,  u64, i16,  |a| a as i16,  decode_u64, encode_i16);
intrinsic_numeric_convop!(intrinsic_conv_u64_u32,  u64, u32,  |a| a as u32,  decode_u64, encode_u32);
intrinsic_numeric_convop!(intrinsic_conv_u64_i32,  u64, i32,  |a| a as i32,  decode_u64, encode_i32);
intrinsic_numeric_convop!(intrinsic_conv_u64_i64,  u64, i64,  |a| a as i64,  decode_u64, encode_i64);
intrinsic_numeric_convop!(intrinsic_conv_u64_u128, u64, u128, |a| a as u128, decode_u64, encode_u128);
intrinsic_numeric_convop!(intrinsic_conv_u64_i128, u64, i128, |a| a as i128, decode_u64, encode_i128);
intrinsic_numeric_convop!(intrinsic_conv_u64_f32,  u64, f32,  |a| a as f32,  decode_u64, encode_f32);
intrinsic_numeric_convop!(intrinsic_conv_u64_f64,  u64, f64,  |a| a as f64,  decode_u64, encode_f64);

// From i64
intrinsic_numeric_convop!(intrinsic_conv_i64_u8,   i64, u8,   |a| a as u8,   decode_i64, encode_u8);
intrinsic_numeric_convop!(intrinsic_conv_i64_i8,   i64, i8,   |a| a as i8,   decode_i64, encode_i8);
intrinsic_numeric_convop!(intrinsic_conv_i64_u16,  i64, u16,  |a| a as u16,  decode_i64, encode_u16);
intrinsic_numeric_convop!(intrinsic_conv_i64_i16,  i64, i16,  |a| a as i16,  decode_i64, encode_i16);
intrinsic_numeric_convop!(intrinsic_conv_i64_u32,  i64, u32,  |a| a as u32,  decode_i64, encode_u32);
intrinsic_numeric_convop!(intrinsic_conv_i64_i32,  i64, i32,  |a| a as i32,  decode_i64, encode_i32);
intrinsic_numeric_convop!(intrinsic_conv_i64_u64,  i64, u64,  |a| a as u64,  decode_i64, encode_u64);
intrinsic_numeric_convop!(intrinsic_conv_i64_u128, i64, u128, |a| a as u128, decode_i64, encode_u128);
intrinsic_numeric_convop!(intrinsic_conv_i64_i128, i64, i128, |a| a as i128, decode_i64, encode_i128);
intrinsic_numeric_convop!(intrinsic_conv_i64_f32,  i64, f32,  |a| a as f32,  decode_i64, encode_f32);
intrinsic_numeric_convop!(intrinsic_conv_i64_f64,  i64, f64,  |a| a as f64,  decode_i64, encode_f64);

// From u128
intrinsic_numeric_convop!(intrinsic_conv_u128_u8,   u128, u8,   |a| a as u8,   decode_u128, encode_u8);
intrinsic_numeric_convop!(intrinsic_conv_u128_i8,   u128, i8,   |a| a as i8,   decode_u128, encode_i8);
intrinsic_numeric_convop!(intrinsic_conv_u128_u16,  u128, u16,  |a| a as u16,  decode_u128, encode_u16);
intrinsic_numeric_convop!(intrinsic_conv_u128_i16,  u128, i16,  |a| a as i16,  decode_u128, encode_i16);
intrinsic_numeric_convop!(intrinsic_conv_u128_u32,  u128, u32,  |a| a as u32,  decode_u128, encode_u32);
intrinsic_numeric_convop!(intrinsic_conv_u128_i32,  u128, i32,  |a| a as i32,  decode_u128, encode_i32);
intrinsic_numeric_convop!(intrinsic_conv_u128_u64,  u128, u64,  |a| a as u64,  decode_u128, encode_u64);
intrinsic_numeric_convop!(intrinsic_conv_u128_i64,  u128, i64,  |a| a as i64,  decode_u128, encode_i64);
intrinsic_numeric_convop!(intrinsic_conv_u128_i128, u128, i128, |a| a as i128, decode_u128, encode_i128);
intrinsic_numeric_convop!(intrinsic_conv_u128_f32,  u128, f32,  |a| a as f32,  decode_u128, encode_f32); // May truncate
intrinsic_numeric_convop!(intrinsic_conv_u128_f64,  u128, f64,  |a| a as f64,  decode_u128, encode_f64); // May truncate

// From i128
intrinsic_numeric_convop!(intrinsic_conv_i128_u8,   i128, u8,   |a| a as u8,   decode_i128, encode_u8);
intrinsic_numeric_convop!(intrinsic_conv_i128_i8,   i128, i8,   |a| a as i8,   decode_i128, encode_i8);
intrinsic_numeric_convop!(intrinsic_conv_i128_u16,  i128, u16,  |a| a as u16,  decode_i128, encode_u16);
intrinsic_numeric_convop!(intrinsic_conv_i128_i16,  i128, i16,  |a| a as i16,  decode_i128, encode_i16);
intrinsic_numeric_convop!(intrinsic_conv_i128_u32,  i128, u32,  |a| a as u32,  decode_i128, encode_u32);
intrinsic_numeric_convop!(intrinsic_conv_i128_i32,  i128, i32,  |a| a as i32,  decode_i128, encode_i32);
intrinsic_numeric_convop!(intrinsic_conv_i128_u64,  i128, u64,  |a| a as u64,  decode_i128, encode_u64);
intrinsic_numeric_convop!(intrinsic_conv_i128_i64,  i128, i64,  |a| a as i64,  decode_i128, encode_i64);
intrinsic_numeric_convop!(intrinsic_conv_i128_u128, i128, u128, |a| a as u128, decode_i128, encode_u128);
intrinsic_numeric_convop!(intrinsic_conv_i128_f32,  i128, f32,  |a| a as f32,  decode_i128, encode_f32); // May truncate
intrinsic_numeric_convop!(intrinsic_conv_i128_f64,  i128, f64,  |a| a as f64,  decode_i128, encode_f64); // May truncate

// From f32
intrinsic_numeric_convop!(intrinsic_conv_f32_u8,   f32, u8,   |a| a as u8,   decode_f32, encode_u8);
intrinsic_numeric_convop!(intrinsic_conv_f32_i8,   f32, i8,   |a| a as i8,   decode_f32, encode_i8);
intrinsic_numeric_convop!(intrinsic_conv_f32_u16,  f32, u16,  |a| a as u16,  decode_f32, encode_u16);
intrinsic_numeric_convop!(intrinsic_conv_f32_i16,  f32, i16,  |a| a as i16,  decode_f32, encode_i16);
intrinsic_numeric_convop!(intrinsic_conv_f32_u32,  f32, u32,  |a| a as u32,  decode_f32, encode_u32);
intrinsic_numeric_convop!(intrinsic_conv_f32_i32,  f32, i32,  |a| a as i32,  decode_f32, encode_i32);
intrinsic_numeric_convop!(intrinsic_conv_f32_u64,  f32, u64,  |a| a as u64,  decode_f32, encode_u64);
intrinsic_numeric_convop!(intrinsic_conv_f32_i64,  f32, i64,  |a| a as i64,  decode_f32, encode_i64);
intrinsic_numeric_convop!(intrinsic_conv_f32_u128, f32, u128, |a| a as u128, decode_f32, encode_u128);
intrinsic_numeric_convop!(intrinsic_conv_f32_i128, f32, i128, |a| a as i128, decode_f32, encode_i128);
intrinsic_numeric_convop!(intrinsic_conv_f32_f64,  f32, f64,  |a| a as f64,  decode_f32, encode_f64);

// From f64
intrinsic_numeric_convop!(intrinsic_conv_f64_u8,   f64, u8,   |a| a as u8,   decode_f64, encode_u8);
intrinsic_numeric_convop!(intrinsic_conv_f64_i8,   f64, i8,   |a| a as i8,   decode_f64, encode_i8);
intrinsic_numeric_convop!(intrinsic_conv_f64_u16,  f64, u16,  |a| a as u16,  decode_f64, encode_u16);
intrinsic_numeric_convop!(intrinsic_conv_f64_i16,  f64, i16,  |a| a as i16,  decode_f64, encode_i16);
intrinsic_numeric_convop!(intrinsic_conv_f64_u32,  f64, u32,  |a| a as u32,  decode_f64, encode_u32);
intrinsic_numeric_convop!(intrinsic_conv_f64_i32,  f64, i32,  |a| a as i32,  decode_f64, encode_i32);
intrinsic_numeric_convop!(intrinsic_conv_f64_u64,  f64, u64,  |a| a as u64,  decode_f64, encode_u64);
intrinsic_numeric_convop!(intrinsic_conv_f64_i64,  f64, i64,  |a| a as i64,  decode_f64, encode_i64);
intrinsic_numeric_convop!(intrinsic_conv_f64_u128, f64, u128, |a| a as u128, decode_f64, encode_u128);
intrinsic_numeric_convop!(intrinsic_conv_f64_i128, f64, i128, |a| a as i128, decode_f64, encode_i128);
intrinsic_numeric_convop!(intrinsic_conv_f64_f32,  f64, f32,  |a| a as f32,  decode_f64, encode_f32);


// --- Runtime Handlers ---
/// Placeholder handler for panic intrinsic
#[inline]
unsafe fn intrinsic_runtime_panic(
    s_port: Port,
    app_head_port: Port,
    caller_port: Port,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let message = "Panic intrinsic called (message decoding not implemented)";
    log::error!("{}", message);
    remove_node(s_port, read_guard);
    annihilate_any(app_head_port, read_guard);
    if caller_port != Port::NULL { annihilate_any(caller_port, read_guard); }
    panic!("{}", message);
}

/// Placeholder handler for println intrinsic
#[inline]
unsafe fn intrinsic_runtime_println(
    s_port: Port,
    app_head_port: Port,
    caller_port: Port,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    // TODO: Implement actual printing logic, potentially decoding the argument list
    log::info!("println intrinsic called (not implemented)");
    remove_node(s_port, read_guard);
    let partition_ptr = get_partition_ptr_mut(read_guard, s_port.partition_id());
    let partition = &mut *partition_ptr;
    let nil_data = encode_static_data(TAG_NIL, 0);
    let nil_idx = partition.alloc_static(Static {
        principle: Port::NULL,
        data: AtomicU64::new(nil_data),
    });
    let nil_port = Port::principal(NodeType::Static, s_port.partition_id(), nil_idx as u64);
    partition.statics[nil_idx].principle = nil_port; // Set principle after allocation
    connect(caller_port, nil_port, read_guard);
    if caller_port.port_type() == PortType::Principal {
        add_active_pair_to_partition(caller_port.partition_id(), Wire(caller_port, nil_port), read_guard);
    }
}

/// Placeholder handler for readln intrinsic
#[inline]
unsafe fn intrinsic_runtime_readln(
    s_port: Port,
    app_head_port: Port, // This port likely represents the continuation/callback
    caller_port: Port,   // Should be NULL if called directly? Check convention.
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    // TODO: Implement actual reading logic, likely involving async suspension/worker state
    log::info!("readln intrinsic called (not implemented) - returning NIL");
    remove_node(s_port, read_guard);
    // For readln, the app_head_port is the continuation. We connect it to the result (NIL for now).
    let partition_ptr = get_partition_ptr_mut(read_guard, s_port.partition_id());
    let partition = &mut *partition_ptr;
    let nil_data = encode_static_data(TAG_NIL, 0);
    let nil_idx = partition.alloc_static(Static {
        principle: Port::NULL,
        data: AtomicU64::new(nil_data),
    });
    let nil_port = Port::principal(NodeType::Static, s_port.partition_id(), nil_idx as u64);
    partition.statics[nil_idx].principle = nil_port; // Set principle after allocation
    connect(app_head_port, nil_port, read_guard);
     if app_head_port.port_type() == PortType::Principal {
        add_active_pair_to_partition(app_head_port.partition_id(), Wire(app_head_port, nil_port), read_guard);
     }
    // Annihilate the original caller if present (might not be if top-level call?)
    if caller_port != Port::NULL { annihilate_any(caller_port, read_guard); }
}

/// Dispatches Static(IntrinsicOp) ~ X interactions based on the encoded operation.
#[inline]
pub(super) unsafe fn dispatch_intrinsic_op(
    wire: Wire,
    s_port: Port,
    encoded_op: u64,
    other_port: Port,
    _worker: &Worker,
    read_guard: &RwLockReadGuard<AllPartitions>,
) {
    let payload = decode_static_payload(encoded_op);
    let op_category = decode_intrinsic_category(payload);
    let op_code = decode_intrinsic_opcode(payload);
    let op_source_type = decode_intrinsic_src_type(payload);
    let op_target_type = decode_intrinsic_tgt_type(payload);

    log::trace!(
        "Rule: Static(Intrinsic|Cat:{},Op:{},SrcT:{},TgtT:{}) ~ {:?}",
        op_category, op_code, op_source_type, op_target_type, other_port
    );

    // Get the principal port of the *caller* of the intrinsic application
    // `other_port` represents the principal port of the outermost AppCON node
    // The caller is connected to this principal port.
    let caller_port = match get_port_ptr_mut(read_guard, other_port) {
        Some(ptr) => *ptr, // Dereference the pointer to get the Port value
        None => {
            log::error!("Intrinsic Op dispatch failed: AppCON head port {:?} not found or invalid.", other_port);
            remove_node(s_port, read_guard); // Clean up the static intrinsic node
            // Attempt to annihilate the other side too if possible
            if other_port != Port::NULL { annihilate_any(other_port, read_guard); }
            return;
        }
    };

    // Check if the `other_port` is actually a Constructor (AppCON)
    if NodeType::from_u8(other_port.node_type()) != Some(NodeType::Constructor) {
        log::error!("Intrinsic Op expected Constructor argument (AppCON), got {:?}. Annihilating.", other_port);
        remove_node(s_port, read_guard);
        annihilate_any(other_port, read_guard);
        // Also annihilate the caller connected to it if it exists
        if caller_port != Port::NULL { annihilate_any(caller_port, read_guard); }
        return;
    }

    // --- Dispatch Logic ---
    match op_category {
        OPCAT_ARITHMETIC => match (op_source_type, op_code) {
            // Bool (Comparisons only - using ARITH codes EQ/NE)
            (TYPECODE_BOOL, OP_ARITH_EQ) => intrinsic_bool_eq(s_port, other_port, caller_port, read_guard),
            (TYPECODE_BOOL, OP_ARITH_NE) => intrinsic_bool_ne(s_port, other_port, caller_port, read_guard),

            // U8
            (TYPECODE_U8, OP_ARITH_ADD) => intrinsic_u8_add(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, OP_ARITH_SUB) => intrinsic_u8_sub(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, OP_ARITH_MUL) => intrinsic_u8_mul(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, OP_ARITH_DIV) => intrinsic_u8_div(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, OP_ARITH_REM) => intrinsic_u8_rem(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, OP_ARITH_AND) => intrinsic_u8_and(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, OP_ARITH_OR)  => intrinsic_u8_or(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, OP_ARITH_XOR) => intrinsic_u8_xor(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, OP_ARITH_SHL) => intrinsic_u8_shl(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, OP_ARITH_SHR) => intrinsic_u8_shr(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, OP_ARITH_NOT) => intrinsic_u8_not(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, OP_ARITH_EQ) => intrinsic_u8_eq(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, OP_ARITH_NE) => intrinsic_u8_ne(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, OP_ARITH_LT) => intrinsic_u8_lt(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, OP_ARITH_LE) => intrinsic_u8_le(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, OP_ARITH_GT) => intrinsic_u8_gt(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, OP_ARITH_GE) => intrinsic_u8_ge(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, OP_ARITH_ABS) => intrinsic_u8_abs(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, OP_ARITH_POW) => intrinsic_u8_pow(s_port, other_port, caller_port, read_guard),

            // I8
            (TYPECODE_I8, OP_ARITH_ADD) => intrinsic_i8_add(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, OP_ARITH_SUB) => intrinsic_i8_sub(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, OP_ARITH_MUL) => intrinsic_i8_mul(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, OP_ARITH_DIV) => intrinsic_i8_div(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, OP_ARITH_REM) => intrinsic_i8_rem(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, OP_ARITH_AND) => intrinsic_i8_and(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, OP_ARITH_OR)  => intrinsic_i8_or(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, OP_ARITH_XOR) => intrinsic_i8_xor(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, OP_ARITH_SHL) => intrinsic_i8_shl(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, OP_ARITH_SHR) => intrinsic_i8_shr(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, OP_ARITH_NEG) => intrinsic_i8_neg(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, OP_ARITH_NOT) => intrinsic_i8_not(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, OP_ARITH_EQ) => intrinsic_i8_eq(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, OP_ARITH_NE) => intrinsic_i8_ne(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, OP_ARITH_LT) => intrinsic_i8_lt(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, OP_ARITH_LE) => intrinsic_i8_le(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, OP_ARITH_GT) => intrinsic_i8_gt(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, OP_ARITH_GE) => intrinsic_i8_ge(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, OP_ARITH_ABS) => intrinsic_i8_abs(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, OP_ARITH_POW) => intrinsic_i8_pow(s_port, other_port, caller_port, read_guard),

            // U16
            (TYPECODE_U16, OP_ARITH_ADD) => intrinsic_u16_add(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, OP_ARITH_SUB) => intrinsic_u16_sub(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, OP_ARITH_MUL) => intrinsic_u16_mul(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, OP_ARITH_DIV) => intrinsic_u16_div(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, OP_ARITH_REM) => intrinsic_u16_rem(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, OP_ARITH_AND) => intrinsic_u16_and(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, OP_ARITH_OR)  => intrinsic_u16_or(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, OP_ARITH_XOR) => intrinsic_u16_xor(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, OP_ARITH_SHL) => intrinsic_u16_shl(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, OP_ARITH_SHR) => intrinsic_u16_shr(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, OP_ARITH_NOT) => intrinsic_u16_not(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, OP_ARITH_EQ) => intrinsic_u16_eq(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, OP_ARITH_NE) => intrinsic_u16_ne(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, OP_ARITH_LT) => intrinsic_u16_lt(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, OP_ARITH_LE) => intrinsic_u16_le(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, OP_ARITH_GT) => intrinsic_u16_gt(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, OP_ARITH_GE) => intrinsic_u16_ge(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, OP_ARITH_ABS) => intrinsic_u16_abs(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, OP_ARITH_POW) => intrinsic_u16_pow(s_port, other_port, caller_port, read_guard),

            // I16
            (TYPECODE_I16, OP_ARITH_ADD) => intrinsic_i16_add(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, OP_ARITH_SUB) => intrinsic_i16_sub(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, OP_ARITH_MUL) => intrinsic_i16_mul(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, OP_ARITH_DIV) => intrinsic_i16_div(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, OP_ARITH_REM) => intrinsic_i16_rem(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, OP_ARITH_AND) => intrinsic_i16_and(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, OP_ARITH_OR)  => intrinsic_i16_or(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, OP_ARITH_XOR) => intrinsic_i16_xor(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, OP_ARITH_SHL) => intrinsic_i16_shl(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, OP_ARITH_SHR) => intrinsic_i16_shr(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, OP_ARITH_NEG) => intrinsic_i16_neg(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, OP_ARITH_NOT) => intrinsic_i16_not(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, OP_ARITH_EQ) => intrinsic_i16_eq(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, OP_ARITH_NE) => intrinsic_i16_ne(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, OP_ARITH_LT) => intrinsic_i16_lt(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, OP_ARITH_LE) => intrinsic_i16_le(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, OP_ARITH_GT) => intrinsic_i16_gt(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, OP_ARITH_GE) => intrinsic_i16_ge(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, OP_ARITH_ABS) => intrinsic_i16_abs(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, OP_ARITH_POW) => intrinsic_i16_pow(s_port, other_port, caller_port, read_guard),

            // U32
            (TYPECODE_U32, OP_ARITH_ADD) => intrinsic_u32_add(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, OP_ARITH_SUB) => intrinsic_u32_sub(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, OP_ARITH_MUL) => intrinsic_u32_mul(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, OP_ARITH_DIV) => intrinsic_u32_div(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, OP_ARITH_REM) => intrinsic_u32_rem(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, OP_ARITH_AND) => intrinsic_u32_and(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, OP_ARITH_OR)  => intrinsic_u32_or(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, OP_ARITH_XOR) => intrinsic_u32_xor(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, OP_ARITH_SHL) => intrinsic_u32_shl(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, OP_ARITH_SHR) => intrinsic_u32_shr(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, OP_ARITH_NOT) => intrinsic_u32_not(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, OP_ARITH_EQ) => intrinsic_u32_eq(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, OP_ARITH_NE) => intrinsic_u32_ne(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, OP_ARITH_LT) => intrinsic_u32_lt(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, OP_ARITH_LE) => intrinsic_u32_le(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, OP_ARITH_GT) => intrinsic_u32_gt(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, OP_ARITH_GE) => intrinsic_u32_ge(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, OP_ARITH_ABS) => intrinsic_u32_abs(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, OP_ARITH_POW) => intrinsic_u32_pow(s_port, other_port, caller_port, read_guard),

            // I32
            (TYPECODE_I32, OP_ARITH_ADD) => intrinsic_i32_add(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, OP_ARITH_SUB) => intrinsic_i32_sub(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, OP_ARITH_MUL) => intrinsic_i32_mul(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, OP_ARITH_DIV) => intrinsic_i32_div(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, OP_ARITH_REM) => intrinsic_i32_rem(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, OP_ARITH_AND) => intrinsic_i32_and(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, OP_ARITH_OR)  => intrinsic_i32_or(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, OP_ARITH_XOR) => intrinsic_i32_xor(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, OP_ARITH_SHL) => intrinsic_i32_shl(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, OP_ARITH_SHR) => intrinsic_i32_shr(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, OP_ARITH_NEG) => intrinsic_i32_neg(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, OP_ARITH_NOT) => intrinsic_i32_not(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, OP_ARITH_EQ) => intrinsic_i32_eq(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, OP_ARITH_NE) => intrinsic_i32_ne(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, OP_ARITH_LT) => intrinsic_i32_lt(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, OP_ARITH_LE) => intrinsic_i32_le(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, OP_ARITH_GT) => intrinsic_i32_gt(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, OP_ARITH_GE) => intrinsic_i32_ge(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, OP_ARITH_ABS) => intrinsic_i32_abs(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, OP_ARITH_POW) => intrinsic_i32_pow(s_port, other_port, caller_port, read_guard),

            // U64
            (TYPECODE_U64, OP_ARITH_ADD) => intrinsic_u64_add(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, OP_ARITH_SUB) => intrinsic_u64_sub(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, OP_ARITH_MUL) => intrinsic_u64_mul(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, OP_ARITH_DIV) => intrinsic_u64_div(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, OP_ARITH_REM) => intrinsic_u64_rem(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, OP_ARITH_AND) => intrinsic_u64_and(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, OP_ARITH_OR)  => intrinsic_u64_or(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, OP_ARITH_XOR) => intrinsic_u64_xor(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, OP_ARITH_SHL) => intrinsic_u64_shl(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, OP_ARITH_SHR) => intrinsic_u64_shr(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, OP_ARITH_NOT) => intrinsic_u64_not(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, OP_ARITH_EQ) => intrinsic_u64_eq(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, OP_ARITH_NE) => intrinsic_u64_ne(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, OP_ARITH_LT) => intrinsic_u64_lt(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, OP_ARITH_LE) => intrinsic_u64_le(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, OP_ARITH_GT) => intrinsic_u64_gt(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, OP_ARITH_GE) => intrinsic_u64_ge(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, OP_ARITH_ABS) => intrinsic_u64_abs(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, OP_ARITH_POW) => intrinsic_u64_pow(s_port, other_port, caller_port, read_guard),

            // I64
            (TYPECODE_I64, OP_ARITH_ADD) => intrinsic_i64_add(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, OP_ARITH_SUB) => intrinsic_i64_sub(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, OP_ARITH_MUL) => intrinsic_i64_mul(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, OP_ARITH_DIV) => intrinsic_i64_div(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, OP_ARITH_REM) => intrinsic_i64_rem(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, OP_ARITH_AND) => intrinsic_i64_and(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, OP_ARITH_OR)  => intrinsic_i64_or(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, OP_ARITH_XOR) => intrinsic_i64_xor(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, OP_ARITH_SHL) => intrinsic_i64_shl(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, OP_ARITH_SHR) => intrinsic_i64_shr(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, OP_ARITH_NEG) => intrinsic_i64_neg(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, OP_ARITH_NOT) => intrinsic_i64_not(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, OP_ARITH_EQ) => intrinsic_i64_eq(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, OP_ARITH_NE) => intrinsic_i64_ne(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, OP_ARITH_LT) => intrinsic_i64_lt(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, OP_ARITH_LE) => intrinsic_i64_le(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, OP_ARITH_GT) => intrinsic_i64_gt(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, OP_ARITH_GE) => intrinsic_i64_ge(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, OP_ARITH_ABS) => intrinsic_i64_abs(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, OP_ARITH_POW) => intrinsic_i64_pow(s_port, other_port, caller_port, read_guard),

            // U128
            (TYPECODE_U128, OP_ARITH_ADD) => intrinsic_u128_add(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, OP_ARITH_SUB) => intrinsic_u128_sub(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, OP_ARITH_MUL) => intrinsic_u128_mul(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, OP_ARITH_DIV) => intrinsic_u128_div(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, OP_ARITH_REM) => intrinsic_u128_rem(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, OP_ARITH_AND) => intrinsic_u128_and(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, OP_ARITH_OR)  => intrinsic_u128_or(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, OP_ARITH_XOR) => intrinsic_u128_xor(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, OP_ARITH_SHL) => intrinsic_u128_shl(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, OP_ARITH_SHR) => intrinsic_u128_shr(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, OP_ARITH_NOT) => intrinsic_u128_not(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, OP_ARITH_EQ) => intrinsic_u128_eq(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, OP_ARITH_NE) => intrinsic_u128_ne(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, OP_ARITH_LT) => intrinsic_u128_lt(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, OP_ARITH_LE) => intrinsic_u128_le(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, OP_ARITH_GT) => intrinsic_u128_gt(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, OP_ARITH_GE) => intrinsic_u128_ge(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, OP_ARITH_ABS) => intrinsic_u128_abs(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, OP_ARITH_POW) => intrinsic_u128_pow(s_port, other_port, caller_port, read_guard),

            // I128
            (TYPECODE_I128, OP_ARITH_ADD) => intrinsic_i128_add(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, OP_ARITH_SUB) => intrinsic_i128_sub(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, OP_ARITH_MUL) => intrinsic_i128_mul(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, OP_ARITH_DIV) => intrinsic_i128_div(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, OP_ARITH_REM) => intrinsic_i128_rem(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, OP_ARITH_AND) => intrinsic_i128_and(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, OP_ARITH_OR)  => intrinsic_i128_or(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, OP_ARITH_XOR) => intrinsic_i128_xor(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, OP_ARITH_SHL) => intrinsic_i128_shl(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, OP_ARITH_SHR) => intrinsic_i128_shr(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, OP_ARITH_NEG) => intrinsic_i128_neg(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, OP_ARITH_NOT) => intrinsic_i128_not(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, OP_ARITH_EQ) => intrinsic_i128_eq(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, OP_ARITH_NE) => intrinsic_i128_ne(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, OP_ARITH_LT) => intrinsic_i128_lt(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, OP_ARITH_LE) => intrinsic_i128_le(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, OP_ARITH_GT) => intrinsic_i128_gt(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, OP_ARITH_GE) => intrinsic_i128_ge(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, OP_ARITH_ABS) => intrinsic_i128_abs(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, OP_ARITH_POW) => intrinsic_i128_pow(s_port, other_port, caller_port, read_guard),

            // F32
            (TYPECODE_F32, OP_ARITH_ADD) => intrinsic_f32_add(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F32, OP_ARITH_SUB) => intrinsic_f32_sub(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F32, OP_ARITH_MUL) => intrinsic_f32_mul(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F32, OP_ARITH_DIV) => intrinsic_f32_div(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F32, OP_ARITH_REM) => intrinsic_f32_rem(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F32, OP_ARITH_NEG) => intrinsic_f32_neg(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F32, OP_ARITH_EQ) => intrinsic_f32_eq(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F32, OP_ARITH_NE) => intrinsic_f32_ne(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F32, OP_ARITH_LT) => intrinsic_f32_lt(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F32, OP_ARITH_LE) => intrinsic_f32_le(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F32, OP_ARITH_GT) => intrinsic_f32_gt(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F32, OP_ARITH_GE) => intrinsic_f32_ge(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F32, OP_ARITH_ABS) => intrinsic_f32_abs(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F32, OP_ARITH_POW) => intrinsic_f32_pow(s_port, other_port, caller_port, read_guard),

            // F64
            (TYPECODE_F64, OP_ARITH_ADD) => intrinsic_f64_add(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F64, OP_ARITH_SUB) => intrinsic_f64_sub(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F64, OP_ARITH_MUL) => intrinsic_f64_mul(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F64, OP_ARITH_DIV) => intrinsic_f64_div(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F64, OP_ARITH_REM) => intrinsic_f64_rem(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F64, OP_ARITH_NEG) => intrinsic_f64_neg(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F64, OP_ARITH_EQ) => intrinsic_f64_eq(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F64, OP_ARITH_NE) => intrinsic_f64_ne(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F64, OP_ARITH_LT) => intrinsic_f64_lt(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F64, OP_ARITH_LE) => intrinsic_f64_le(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F64, OP_ARITH_GT) => intrinsic_f64_gt(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F64, OP_ARITH_GE) => intrinsic_f64_ge(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F64, OP_ARITH_ABS) => intrinsic_f64_abs(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F64, OP_ARITH_POW) => intrinsic_f64_pow(s_port, other_port, caller_port, read_guard),

            // Fallback
            _ => handle_intrinsic_error!("dispatch_intrinsic_op", "Unsupported ARITHMETIC/COMPARISON intrinsic: SrcT={}, OpCode={}", s_port, other_port, caller_port, read_guard, op_source_type, op_code),
        },
        OPCAT_BOOLEAN => match (op_source_type, op_code) {
            (TYPECODE_BOOL, OP_BOOL_AND) => intrinsic_bool_and(s_port, other_port, caller_port, read_guard),
            (TYPECODE_BOOL, OP_BOOL_OR)  => intrinsic_bool_or(s_port, other_port, caller_port, read_guard),
            (TYPECODE_BOOL, OP_BOOL_XOR) => intrinsic_bool_xor(s_port, other_port, caller_port, read_guard),
            (TYPECODE_BOOL, OP_BOOL_NOT) => intrinsic_bool_not(s_port, other_port, caller_port, read_guard),
            _ => handle_intrinsic_error!("dispatch_intrinsic_op", "Unsupported BOOLEAN intrinsic: SrcT={}, OpCode={}", s_port, other_port, caller_port, read_guard, op_source_type, op_code),
        },
        OPCAT_CONVERSION => match (op_source_type, op_target_type, op_code) {
            // From U8
            (TYPECODE_U8, TYPECODE_I8, OP_CONVERT) => intrinsic_conv_u8_i8(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, TYPECODE_U16, OP_CONVERT) => intrinsic_conv_u8_u16(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, TYPECODE_I16, OP_CONVERT) => intrinsic_conv_u8_i16(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, TYPECODE_U32, OP_CONVERT) => intrinsic_conv_u8_u32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, TYPECODE_I32, OP_CONVERT) => intrinsic_conv_u8_i32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, TYPECODE_U64, OP_CONVERT) => intrinsic_conv_u8_u64(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, TYPECODE_I64, OP_CONVERT) => intrinsic_conv_u8_i64(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, TYPECODE_U128, OP_CONVERT) => intrinsic_conv_u8_u128(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, TYPECODE_I128, OP_CONVERT) => intrinsic_conv_u8_i128(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, TYPECODE_F32, OP_CONVERT) => intrinsic_conv_u8_f32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U8, TYPECODE_F64, OP_CONVERT) => intrinsic_conv_u8_f64(s_port, other_port, caller_port, read_guard),
            // From I8
            (TYPECODE_I8, TYPECODE_U8, OP_CONVERT) => intrinsic_conv_i8_u8(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, TYPECODE_U16, OP_CONVERT) => intrinsic_conv_i8_u16(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, TYPECODE_I16, OP_CONVERT) => intrinsic_conv_i8_i16(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, TYPECODE_U32, OP_CONVERT) => intrinsic_conv_i8_u32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, TYPECODE_I32, OP_CONVERT) => intrinsic_conv_i8_i32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, TYPECODE_U64, OP_CONVERT) => intrinsic_conv_i8_u64(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, TYPECODE_I64, OP_CONVERT) => intrinsic_conv_i8_i64(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, TYPECODE_U128, OP_CONVERT) => intrinsic_conv_i8_u128(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, TYPECODE_I128, OP_CONVERT) => intrinsic_conv_i8_i128(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, TYPECODE_F32, OP_CONVERT) => intrinsic_conv_i8_f32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I8, TYPECODE_F64, OP_CONVERT) => intrinsic_conv_i8_f64(s_port, other_port, caller_port, read_guard),
            // From U16
            (TYPECODE_U16, TYPECODE_U8, OP_CONVERT) => intrinsic_conv_u16_u8(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, TYPECODE_I8, OP_CONVERT) => intrinsic_conv_u16_i8(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, TYPECODE_I16, OP_CONVERT) => intrinsic_conv_u16_i16(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, TYPECODE_U32, OP_CONVERT) => intrinsic_conv_u16_u32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, TYPECODE_I32, OP_CONVERT) => intrinsic_conv_u16_i32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, TYPECODE_U64, OP_CONVERT) => intrinsic_conv_u16_u64(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, TYPECODE_I64, OP_CONVERT) => intrinsic_conv_u16_i64(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, TYPECODE_U128, OP_CONVERT) => intrinsic_conv_u16_u128(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, TYPECODE_I128, OP_CONVERT) => intrinsic_conv_u16_i128(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, TYPECODE_F32, OP_CONVERT) => intrinsic_conv_u16_f32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U16, TYPECODE_F64, OP_CONVERT) => intrinsic_conv_u16_f64(s_port, other_port, caller_port, read_guard),
            // From I16
            (TYPECODE_I16, TYPECODE_U8, OP_CONVERT) => intrinsic_conv_i16_u8(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, TYPECODE_I8, OP_CONVERT) => intrinsic_conv_i16_i8(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, TYPECODE_U16, OP_CONVERT) => intrinsic_conv_i16_u16(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, TYPECODE_U32, OP_CONVERT) => intrinsic_conv_i16_u32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, TYPECODE_I32, OP_CONVERT) => intrinsic_conv_i16_i32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, TYPECODE_U64, OP_CONVERT) => intrinsic_conv_i16_u64(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, TYPECODE_I64, OP_CONVERT) => intrinsic_conv_i16_i64(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, TYPECODE_U128, OP_CONVERT) => intrinsic_conv_i16_u128(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, TYPECODE_I128, OP_CONVERT) => intrinsic_conv_i16_i128(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, TYPECODE_F32, OP_CONVERT) => intrinsic_conv_i16_f32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I16, TYPECODE_F64, OP_CONVERT) => intrinsic_conv_i16_f64(s_port, other_port, caller_port, read_guard),
            // From U32
            (TYPECODE_U32, TYPECODE_U8, OP_CONVERT) => intrinsic_conv_u32_u8(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, TYPECODE_I8, OP_CONVERT) => intrinsic_conv_u32_i8(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, TYPECODE_U16, OP_CONVERT) => intrinsic_conv_u32_u16(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, TYPECODE_I16, OP_CONVERT) => intrinsic_conv_u32_i16(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, TYPECODE_I32, OP_CONVERT) => intrinsic_conv_u32_i32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, TYPECODE_U64, OP_CONVERT) => intrinsic_conv_u32_u64(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, TYPECODE_I64, OP_CONVERT) => intrinsic_conv_u32_i64(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, TYPECODE_U128, OP_CONVERT) => intrinsic_conv_u32_u128(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, TYPECODE_I128, OP_CONVERT) => intrinsic_conv_u32_i128(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, TYPECODE_F32, OP_CONVERT) => intrinsic_conv_u32_f32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U32, TYPECODE_F64, OP_CONVERT) => intrinsic_conv_u32_f64(s_port, other_port, caller_port, read_guard),
            // From I32
            (TYPECODE_I32, TYPECODE_U8, OP_CONVERT) => intrinsic_conv_i32_u8(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, TYPECODE_I8, OP_CONVERT) => intrinsic_conv_i32_i8(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, TYPECODE_U16, OP_CONVERT) => intrinsic_conv_i32_u16(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, TYPECODE_I16, OP_CONVERT) => intrinsic_conv_i32_i16(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, TYPECODE_U32, OP_CONVERT) => intrinsic_conv_i32_u32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, TYPECODE_U64, OP_CONVERT) => intrinsic_conv_i32_u64(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, TYPECODE_I64, OP_CONVERT) => intrinsic_conv_i32_i64(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, TYPECODE_U128, OP_CONVERT) => intrinsic_conv_i32_u128(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, TYPECODE_I128, OP_CONVERT) => intrinsic_conv_i32_i128(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, TYPECODE_F32, OP_CONVERT) => intrinsic_conv_i32_f32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I32, TYPECODE_F64, OP_CONVERT) => intrinsic_conv_i32_f64(s_port, other_port, caller_port, read_guard),
            // From U64
            (TYPECODE_U64, TYPECODE_U8, OP_CONVERT) => intrinsic_conv_u64_u8(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, TYPECODE_I8, OP_CONVERT) => intrinsic_conv_u64_i8(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, TYPECODE_U16, OP_CONVERT) => intrinsic_conv_u64_u16(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, TYPECODE_I16, OP_CONVERT) => intrinsic_conv_u64_i16(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, TYPECODE_U32, OP_CONVERT) => intrinsic_conv_u64_u32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, TYPECODE_I32, OP_CONVERT) => intrinsic_conv_u64_i32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, TYPECODE_I64, OP_CONVERT) => intrinsic_conv_u64_i64(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, TYPECODE_U128, OP_CONVERT) => intrinsic_conv_u64_u128(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, TYPECODE_I128, OP_CONVERT) => intrinsic_conv_u64_i128(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, TYPECODE_F32, OP_CONVERT) => intrinsic_conv_u64_f32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U64, TYPECODE_F64, OP_CONVERT) => intrinsic_conv_u64_f64(s_port, other_port, caller_port, read_guard),
            // From I64
            (TYPECODE_I64, TYPECODE_U8, OP_CONVERT) => intrinsic_conv_i64_u8(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, TYPECODE_I8, OP_CONVERT) => intrinsic_conv_i64_i8(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, TYPECODE_U16, OP_CONVERT) => intrinsic_conv_i64_u16(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, TYPECODE_I16, OP_CONVERT) => intrinsic_conv_i64_i16(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, TYPECODE_U32, OP_CONVERT) => intrinsic_conv_i64_u32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, TYPECODE_I32, OP_CONVERT) => intrinsic_conv_i64_i32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, TYPECODE_U64, OP_CONVERT) => intrinsic_conv_i64_u64(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, TYPECODE_U128, OP_CONVERT) => intrinsic_conv_i64_u128(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, TYPECODE_I128, OP_CONVERT) => intrinsic_conv_i64_i128(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, TYPECODE_F32, OP_CONVERT) => intrinsic_conv_i64_f32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I64, TYPECODE_F64, OP_CONVERT) => intrinsic_conv_i64_f64(s_port, other_port, caller_port, read_guard),
             // From U128
            (TYPECODE_U128, TYPECODE_U8, OP_CONVERT) => intrinsic_conv_u128_u8(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, TYPECODE_I8, OP_CONVERT) => intrinsic_conv_u128_i8(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, TYPECODE_U16, OP_CONVERT) => intrinsic_conv_u128_u16(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, TYPECODE_I16, OP_CONVERT) => intrinsic_conv_u128_i16(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, TYPECODE_U32, OP_CONVERT) => intrinsic_conv_u128_u32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, TYPECODE_I32, OP_CONVERT) => intrinsic_conv_u128_i32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, TYPECODE_U64, OP_CONVERT) => intrinsic_conv_u128_u64(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, TYPECODE_I64, OP_CONVERT) => intrinsic_conv_u128_i64(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, TYPECODE_I128, OP_CONVERT) => intrinsic_conv_u128_i128(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, TYPECODE_F32, OP_CONVERT) => intrinsic_conv_u128_f32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_U128, TYPECODE_F64, OP_CONVERT) => intrinsic_conv_u128_f64(s_port, other_port, caller_port, read_guard),
            // From I128
            (TYPECODE_I128, TYPECODE_U8, OP_CONVERT) => intrinsic_conv_i128_u8(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, TYPECODE_I8, OP_CONVERT) => intrinsic_conv_i128_i8(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, TYPECODE_U16, OP_CONVERT) => intrinsic_conv_i128_u16(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, TYPECODE_I16, OP_CONVERT) => intrinsic_conv_i128_i16(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, TYPECODE_U32, OP_CONVERT) => intrinsic_conv_i128_u32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, TYPECODE_I32, OP_CONVERT) => intrinsic_conv_i128_i32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, TYPECODE_U64, OP_CONVERT) => intrinsic_conv_i128_u64(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, TYPECODE_I64, OP_CONVERT) => intrinsic_conv_i128_i64(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, TYPECODE_U128, OP_CONVERT) => intrinsic_conv_i128_u128(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, TYPECODE_F32, OP_CONVERT) => intrinsic_conv_i128_f32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_I128, TYPECODE_F64, OP_CONVERT) => intrinsic_conv_i128_f64(s_port, other_port, caller_port, read_guard),
            // From F32
            (TYPECODE_F32, TYPECODE_U8, OP_CONVERT) => intrinsic_conv_f32_u8(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F32, TYPECODE_I8, OP_CONVERT) => intrinsic_conv_f32_i8(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F32, TYPECODE_U16, OP_CONVERT) => intrinsic_conv_f32_u16(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F32, TYPECODE_I16, OP_CONVERT) => intrinsic_conv_f32_i16(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F32, TYPECODE_U32, OP_CONVERT) => intrinsic_conv_f32_u32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F32, TYPECODE_I32, OP_CONVERT) => intrinsic_conv_f32_i32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F32, TYPECODE_U64, OP_CONVERT) => intrinsic_conv_f32_u64(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F32, TYPECODE_I64, OP_CONVERT) => intrinsic_conv_f32_i64(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F32, TYPECODE_U128, OP_CONVERT) => intrinsic_conv_f32_u128(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F32, TYPECODE_I128, OP_CONVERT) => intrinsic_conv_f32_i128(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F32, TYPECODE_F64, OP_CONVERT) => intrinsic_conv_f32_f64(s_port, other_port, caller_port, read_guard),
            // From F64
            (TYPECODE_F64, TYPECODE_U8, OP_CONVERT) => intrinsic_conv_f64_u8(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F64, TYPECODE_I8, OP_CONVERT) => intrinsic_conv_f64_i8(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F64, TYPECODE_U16, OP_CONVERT) => intrinsic_conv_f64_u16(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F64, TYPECODE_I16, OP_CONVERT) => intrinsic_conv_f64_i16(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F64, TYPECODE_U32, OP_CONVERT) => intrinsic_conv_f64_u32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F64, TYPECODE_I32, OP_CONVERT) => intrinsic_conv_f64_i32(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F64, TYPECODE_U64, OP_CONVERT) => intrinsic_conv_f64_u64(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F64, TYPECODE_I64, OP_CONVERT) => intrinsic_conv_f64_i64(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F64, TYPECODE_U128, OP_CONVERT) => intrinsic_conv_f64_u128(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F64, TYPECODE_I128, OP_CONVERT) => intrinsic_conv_f64_i128(s_port, other_port, caller_port, read_guard),
            (TYPECODE_F64, TYPECODE_F32, OP_CONVERT) => intrinsic_conv_f64_f32(s_port, other_port, caller_port, read_guard),

            // Fallback
            _ => handle_intrinsic_error!("dispatch_intrinsic_op", "Unsupported CONVERSION intrinsic: SrcT={}, TgtT={}, OpCode={}", s_port, other_port, caller_port, read_guard, op_source_type, op_target_type, op_code),
        },
        OPCAT_RUNTIME => match op_code {
            OP_RUNTIME_PANIC => intrinsic_runtime_panic(s_port, other_port, caller_port, read_guard),
            OP_RUNTIME_PRINTLN => intrinsic_runtime_println(s_port, other_port, caller_port, read_guard),
            OP_RUNTIME_READLN => intrinsic_runtime_readln(s_port, other_port, caller_port, read_guard),
            _ => handle_intrinsic_error!("dispatch_intrinsic_op", "Unsupported RUNTIME intrinsic: OpCode={}", s_port, other_port, caller_port, read_guard, op_code),
        },
        _ => handle_intrinsic_error!("dispatch_intrinsic_op", "Unknown Intrinsic Category: {}", s_port, other_port, caller_port, read_guard, op_category),
    }
}