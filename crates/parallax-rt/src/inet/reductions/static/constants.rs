// --- Static Node Data Encoding Constants (mirrored from lowering/nodes.rs) ---
#![allow(dead_code)] // Allow unused constants for now

pub(super) const TAG_SHIFT: u32 = 60;
pub(super) const DATA_MASK: u64 = (1u64 << TAG_SHIFT) - 1;

pub(crate) const TAG_NIL: u64 = 0;
pub(crate) const TAG_FUNCTION: u64 = 1;
// pub(super) const TAG_GLOBAL_VAR: u64 = 2; // Unused for now
pub(crate) const TAG_INTRINSIC_OP: u64 = 6;
pub(crate) const TAG_IS_VARIANT: u64 = 8;

// --- Intrinsic Operation Codes (mirrored from lowering/nodes.rs) ---
// i64: 0-127
pub(super) const OP_I64_ADD: u64 = 0;
pub(super) const OP_I64_SUB: u64 = 1;
pub(super) const OP_I64_MUL: u64 = 2;
pub(super) const OP_I64_DIV: u64 = 3;
pub(super) const OP_I64_REM: u64 = 4;
pub(super) const OP_I64_AND: u64 = 5;
pub(super) const OP_I64_OR:  u64 = 6;
pub(super) const OP_I64_XOR: u64 = 7;
pub(super) const OP_I64_SHL: u64 = 8;
pub(super) const OP_I64_SHR: u64 = 9;
pub(super) const OP_I64_NEG: u64 = 10;
pub(super) const OP_I64_NOT: u64 = 11;
pub(super) const OP_I64_ABS: u64 = 12;
pub(super) const OP_I64_POW: u64 = 13;
pub(super) const OP_I64_EQ:  u64 = 14;
pub(super) const OP_I64_NE:  u64 = 15;
pub(super) const OP_I64_LT:  u64 = 16;
pub(super) const OP_I64_LE:  u64 = 17;
pub(super) const OP_I64_GT:  u64 = 18;
pub(super) const OP_I64_GE:  u64 = 19;

// u64: 128-255
pub(super) const OP_U64_ADD: u64 = 128 + 0;
pub(super) const OP_U64_SUB: u64 = 128 + 1;
pub(super) const OP_U64_MUL: u64 = 128 + 2;
pub(super) const OP_U64_DIV: u64 = 128 + 3;
pub(super) const OP_U64_REM: u64 = 128 + 4;
pub(super) const OP_U64_AND: u64 = 128 + 5;
pub(super) const OP_U64_OR:  u64 = 128 + 6;
pub(super) const OP_U64_XOR: u64 = 128 + 7;
pub(super) const OP_U64_SHL: u64 = 128 + 8;
pub(super) const OP_U64_SHR: u64 = 128 + 9;
pub(super) const OP_U64_NOT: u64 = 128 + 11;
pub(super) const OP_U64_POW: u64 = 128 + 13;
pub(super) const OP_U64_EQ:  u64 = 128 + 14;
pub(super) const OP_U64_NE:  u64 = 128 + 15;
pub(super) const OP_U64_LT:  u64 = 128 + 16;
pub(super) const OP_U64_LE:  u64 = 128 + 17;
pub(super) const OP_U64_GT:  u64 = 128 + 18;
pub(super) const OP_U64_GE:  u64 = 128 + 19;

// f64: 256-383
pub(super) const OP_F64_ADD: u64 = 256 + 0;
pub(super) const OP_F64_SUB: u64 = 256 + 1;
pub(super) const OP_F64_MUL: u64 = 256 + 2;
pub(super) const OP_F64_DIV: u64 = 256 + 3;
pub(super) const OP_F64_REM: u64 = 256 + 4;
pub(super) const OP_F64_NEG: u64 = 256 + 10;
pub(super) const OP_F64_ABS: u64 = 256 + 12;
pub(super) const OP_F64_POW: u64 = 256 + 13;
pub(super) const OP_F64_EQ:  u64 = 256 + 14;
pub(super) const OP_F64_NE:  u64 = 256 + 15;
pub(super) const OP_F64_LT:  u64 = 256 + 16;
pub(super) const OP_F64_LE:  u64 = 256 + 17;
pub(super) const OP_F64_GT:  u64 = 256 + 18;
pub(super) const OP_F64_GE:  u64 = 256 + 19;

// i32: 384-511
pub(super) const OP_I32_ADD: u64 = 384 + 0;
pub(super) const OP_I32_SUB: u64 = 384 + 1;
pub(super) const OP_I32_MUL: u64 = 384 + 2;
pub(super) const OP_I32_DIV: u64 = 384 + 3;
pub(super) const OP_I32_REM: u64 = 384 + 4;
pub(super) const OP_I32_AND: u64 = 384 + 5;
pub(super) const OP_I32_OR:  u64 = 384 + 6;
pub(super) const OP_I32_XOR: u64 = 384 + 7;
pub(super) const OP_I32_SHL: u64 = 384 + 8;
pub(super) const OP_I32_SHR: u64 = 384 + 9;
pub(super) const OP_I32_NEG: u64 = 384 + 10;
pub(super) const OP_I32_NOT: u64 = 384 + 11;
pub(super) const OP_I32_ABS: u64 = 384 + 12;
pub(super) const OP_I32_POW: u64 = 384 + 13;
pub(super) const OP_I32_EQ:  u64 = 384 + 14;
pub(super) const OP_I32_NE:  u64 = 384 + 15;
pub(super) const OP_I32_LT:  u64 = 384 + 16;
pub(super) const OP_I32_LE:  u64 = 384 + 17;
pub(super) const OP_I32_GT:  u64 = 384 + 18;
pub(super) const OP_I32_GE:  u64 = 384 + 19;

// u32: 512-639
pub(super) const OP_U32_ADD: u64 = 512 + 0;
pub(super) const OP_U32_SUB: u64 = 512 + 1;
pub(super) const OP_U32_MUL: u64 = 512 + 2;
pub(super) const OP_U32_DIV: u64 = 512 + 3;
pub(super) const OP_U32_REM: u64 = 512 + 4;
pub(super) const OP_U32_AND: u64 = 512 + 5;
pub(super) const OP_U32_OR:  u64 = 512 + 6;
pub(super) const OP_U32_XOR: u64 = 512 + 7;
pub(super) const OP_U32_SHL: u64 = 512 + 8;
pub(super) const OP_U32_SHR: u64 = 512 + 9;
pub(super) const OP_U32_NOT: u64 = 512 + 11;
pub(super) const OP_U32_POW: u64 = 512 + 13;
pub(super) const OP_U32_EQ:  u64 = 512 + 14;
pub(super) const OP_U32_NE:  u64 = 512 + 15;
pub(super) const OP_U32_LT:  u64 = 512 + 16;
pub(super) const OP_U32_LE:  u64 = 512 + 17;
pub(super) const OP_U32_GT:  u64 = 512 + 18;
pub(super) const OP_U32_GE:  u64 = 512 + 19;

// f32: 640-767
pub(super) const OP_F32_ADD: u64 = 640 + 0;
pub(super) const OP_F32_SUB: u64 = 640 + 1;
pub(super) const OP_F32_MUL: u64 = 640 + 2;
pub(super) const OP_F32_DIV: u64 = 640 + 3;
pub(super) const OP_F32_REM: u64 = 640 + 4;
pub(super) const OP_F32_NEG: u64 = 640 + 10;
pub(super) const OP_F32_ABS: u64 = 640 + 12;
pub(super) const OP_F32_POW: u64 = 640 + 13;
pub(super) const OP_F32_EQ:  u64 = 640 + 14;
pub(super) const OP_F32_NE:  u64 = 640 + 15;
pub(super) const OP_F32_LT:  u64 = 640 + 16;
pub(super) const OP_F32_LE:  u64 = 640 + 17;
pub(super) const OP_F32_GT:  u64 = 640 + 18;
pub(super) const OP_F32_GE:  u64 = 640 + 19;

// bool: 768-895
pub(super) const OP_BOOL_AND: u64 = 768 + 5;
pub(super) const OP_BOOL_OR:  u64 = 768 + 6;
pub(super) const OP_BOOL_XOR: u64 = 768 + 7;
pub(super) const OP_BOOL_NOT: u64 = 768 + 11;
pub(super) const OP_BOOL_EQ:  u64 = 768 + 14;
pub(super) const OP_BOOL_NE:  u64 = 768 + 15;

// i16: 896-1023
pub(super) const OP_I16_ADD: u64 = 896 + 0;
pub(super) const OP_I16_SUB: u64 = 896 + 1;
pub(super) const OP_I16_MUL: u64 = 896 + 2;
pub(super) const OP_I16_DIV: u64 = 896 + 3;
pub(super) const OP_I16_REM: u64 = 896 + 4;
pub(super) const OP_I16_AND: u64 = 896 + 5;
pub(super) const OP_I16_OR:  u64 = 896 + 6;
pub(super) const OP_I16_XOR: u64 = 896 + 7;
pub(super) const OP_I16_SHL: u64 = 896 + 8;
pub(super) const OP_I16_SHR: u64 = 896 + 9;
pub(super) const OP_I16_NEG: u64 = 896 + 10;
pub(super) const OP_I16_NOT: u64 = 896 + 11;
pub(super) const OP_I16_ABS: u64 = 896 + 12;
pub(super) const OP_I16_POW: u64 = 896 + 13;
pub(super) const OP_I16_EQ:  u64 = 896 + 14;
pub(super) const OP_I16_NE:  u64 = 896 + 15;
pub(super) const OP_I16_LT:  u64 = 896 + 16;
pub(super) const OP_I16_LE:  u64 = 896 + 17;
pub(super) const OP_I16_GT:  u64 = 896 + 18;
pub(super) const OP_I16_GE:  u64 = 896 + 19;

// u16: 1152-1279
pub(super) const OP_U16_ADD: u64 = 1152 + 0;
pub(super) const OP_U16_SUB: u64 = 1152 + 1;
pub(super) const OP_U16_MUL: u64 = 1152 + 2;
pub(super) const OP_U16_DIV: u64 = 1152 + 3;
pub(super) const OP_U16_REM: u64 = 1152 + 4;
pub(super) const OP_U16_AND: u64 = 1152 + 5;
pub(super) const OP_U16_OR:  u64 = 1152 + 6;
pub(super) const OP_U16_XOR: u64 = 1152 + 7;
pub(super) const OP_U16_SHL: u64 = 1152 + 8;
pub(super) const OP_U16_SHR: u64 = 1152 + 9;
pub(super) const OP_U16_NOT: u64 = 1152 + 11;
pub(super) const OP_U16_POW: u64 = 1152 + 13;
pub(super) const OP_U16_EQ:  u64 = 1152 + 14;
pub(super) const OP_U16_NE:  u64 = 1152 + 15;
pub(super) const OP_U16_LT:  u64 = 1152 + 16;
pub(super) const OP_U16_LE:  u64 = 1152 + 17;
pub(super) const OP_U16_GT:  u64 = 1152 + 18;
pub(super) const OP_U16_GE:  u64 = 1152 + 19;

// i8: 1280-1407
pub(super) const OP_I8_ADD: u64 = 1280 + 0;
pub(super) const OP_I8_SUB: u64 = 1280 + 1;
pub(super) const OP_I8_MUL: u64 = 1280 + 2;
pub(super) const OP_I8_DIV: u64 = 1280 + 3;
pub(super) const OP_I8_REM: u64 = 1280 + 4;
pub(super) const OP_I8_AND: u64 = 1280 + 5;
pub(super) const OP_I8_OR:  u64 = 1280 + 6;
pub(super) const OP_I8_XOR: u64 = 1280 + 7;
pub(super) const OP_I8_SHL: u64 = 1280 + 8;
pub(super) const OP_I8_SHR: u64 = 1280 + 9;
pub(super) const OP_I8_NEG: u64 = 1280 + 10;
pub(super) const OP_I8_NOT: u64 = 1280 + 11;
pub(super) const OP_I8_ABS: u64 = 1280 + 12;
pub(super) const OP_I8_POW: u64 = 1280 + 13;
pub(super) const OP_I8_EQ:  u64 = 1280 + 14;
pub(super) const OP_I8_NE:  u64 = 1280 + 15;
pub(super) const OP_I8_LT:  u64 = 1280 + 16;
pub(super) const OP_I8_LE:  u64 = 1280 + 17;
pub(super) const OP_I8_GT:  u64 = 1280 + 18;
pub(super) const OP_I8_GE:  u64 = 1280 + 19;

// u8: 1408-1535
pub(super) const OP_U8_ADD: u64 = 1408 + 0;
pub(super) const OP_U8_SUB: u64 = 1408 + 1;
pub(super) const OP_U8_MUL: u64 = 1408 + 2;
pub(super) const OP_U8_DIV: u64 = 1408 + 3;
pub(super) const OP_U8_REM: u64 = 1408 + 4;
pub(super) const OP_U8_AND: u64 = 1408 + 5;
pub(super) const OP_U8_OR:  u64 = 1408 + 6;
pub(super) const OP_U8_XOR: u64 = 1408 + 7;
pub(super) const OP_U8_SHL: u64 = 1408 + 8;
pub(super) const OP_U8_SHR: u64 = 1408 + 9;
pub(super) const OP_U8_NOT: u64 = 1408 + 11;
pub(super) const OP_U8_POW: u64 = 1408 + 13;
pub(super) const OP_U8_EQ:  u64 = 1408 + 14;
pub(super) const OP_U8_NE:  u64 = 1408 + 15;
pub(super) const OP_U8_LT:  u64 = 1408 + 16;
pub(super) const OP_U8_LE:  u64 = 1408 + 17;
pub(super) const OP_U8_GT:  u64 = 1408 + 18;
pub(super) const OP_U8_GE:  u64 = 1408 + 19;

// i128: 1536-1663
pub(super) const OP_I128_ADD: u64 = 1536 + 0;
pub(super) const OP_I128_SUB: u64 = 1536 + 1;
pub(super) const OP_I128_MUL: u64 = 1536 + 2;
pub(super) const OP_I128_DIV: u64 = 1536 + 3;
pub(super) const OP_I128_REM: u64 = 1536 + 4;
pub(super) const OP_I128_AND: u64 = 1536 + 5;
pub(super) const OP_I128_OR:  u64 = 1536 + 6;
pub(super) const OP_I128_XOR: u64 = 1536 + 7;
pub(super) const OP_I128_SHL: u64 = 1536 + 8;
pub(super) const OP_I128_SHR: u64 = 1536 + 9;
pub(super) const OP_I128_NEG: u64 = 1536 + 10;
pub(super) const OP_I128_NOT: u64 = 1536 + 11;
pub(super) const OP_I128_ABS: u64 = 1536 + 12;
pub(super) const OP_I128_POW: u64 = 1536 + 13;
pub(super) const OP_I128_EQ:  u64 = 1536 + 14;
pub(super) const OP_I128_NE:  u64 = 1536 + 15;
pub(super) const OP_I128_LT:  u64 = 1536 + 16;
pub(super) const OP_I128_LE:  u64 = 1536 + 17;
pub(super) const OP_I128_GT:  u64 = 1536 + 18;
pub(super) const OP_I128_GE:  u64 = 1536 + 19;

// u128: 1664-1791
pub(super) const OP_U128_ADD: u64 = 1664 + 0;
pub(super) const OP_U128_SUB: u64 = 1664 + 1;
pub(super) const OP_U128_MUL: u64 = 1664 + 2;
pub(super) const OP_U128_DIV: u64 = 1664 + 3;
pub(super) const OP_U128_REM: u64 = 1664 + 4;
pub(super) const OP_U128_AND: u64 = 1664 + 5;
pub(super) const OP_U128_OR:  u64 = 1664 + 6;
pub(super) const OP_U128_XOR: u64 = 1664 + 7;
pub(super) const OP_U128_SHL: u64 = 1664 + 8;
pub(super) const OP_U128_SHR: u64 = 1664 + 9;
pub(super) const OP_U128_NOT: u64 = 1664 + 11;
pub(super) const OP_U128_POW: u64 = 1664 + 13;
pub(super) const OP_U128_EQ:  u64 = 1664 + 14;
pub(super) const OP_U128_NE:  u64 = 1664 + 15;
pub(super) const OP_U128_LT:  u64 = 1664 + 16;
pub(super) const OP_U128_LE:  u64 = 1664 + 17;
pub(super) const OP_U128_GT:  u64 = 1664 + 18;
pub(super) const OP_U128_GE:  u64 = 1664 + 19;

// Misc Ops: Use range starting after the last numeric type (e.g., 2048+)
pub(super) const OP_PANIC: u64 = 2048; 