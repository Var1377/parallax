use parallax_hir::Symbol; // Assuming Symbol is needed for encode_static_symbol

// --- Static Node Data Encoding Constants ---
pub const TAG_SHIFT: u32 = 60;
pub const DATA_MASK: u64 = (1u64 << TAG_SHIFT) - 1;

// --- Static Node Tags (Top 4 bits) ---
pub const TAG_NIL: u64 = 0;         // Represents the end of a list or Unit/Tuple tag
pub const TAG_FUNCTION: u64 = 1;    // Data = Function Symbol ID
pub const TAG_GLOBAL_VAR: u64 = 2;  // Data = Global Static Symbol ID
// TAG 3-5 Reserved
pub const TAG_INTRINSIC_OP: u64 = 6; // Data = Encoded Intrinsic Op Code
// TAG 7 Reserved
pub const TAG_IS_VARIANT: u64 = 8;   // Data = Variant Symbol ID (Used by MIR lowering for match)
// Add more tags up to 15 if needed

// --- Intrinsic Operation Encoding (Lower 60 bits when TAG is TAG_INTRINSIC_OP) ---
// Layout of the 60 bits:
// | Bits    | Field        | Purpose                                     |
// | :------ | :----------- | :------------------------------------------ |
// | 56-59   | Category     | Arithmetic, Conversion, Boolean, Runtime IO |
// | 48-55   | OpCode       | Specific operation within the category      |
// | 44-47   | Source Type  | Type code (e.g., i64, f32) if applicable    |
// | 40-43   | Target Type  | Type code (e.g., i32, u8) if applicable     |
// | 0-39    | (Reserved)   | Unused for now                              |

// Field Shifts and Masks (Relative to the 60-bit payload)
pub const OPCAT_SHIFT: u32 = 56;
pub const OPCODE_SHIFT: u32 = 48;
pub const SRC_TYPE_SHIFT: u32 = 44;
pub const TGT_TYPE_SHIFT: u32 = 40;

// Use masks that cover the bit range *after* shifting
pub const OPCAT_MASK: u64 = 0xF;   // 4 bits
pub const OPCODE_MASK: u64 = 0xFF; // 8 bits
pub const TYPE_MASK: u64 = 0xF;    // 4 bits

// Operation Categories (Using bits 56-59 of payload)
pub const OPCAT_ARITHMETIC: u64 = 0;
pub const OPCAT_BOOLEAN: u64    = 1; // Keeping separate for clarity, though could reuse ARITH
pub const OPCAT_CONVERSION: u64 = 2;
pub const OPCAT_RUNTIME: u64    = 3;
// Add more categories if needed (e.g., Memory, Control Flow?)

// Type Codes (For Source/Target Type fields, bits 44-47 and 40-43 of payload)
pub const TYPECODE_NONE: u64 = 0; // Placeholder for ops not needing type info in these fields
pub const TYPECODE_U8:   u64 = 1;
pub const TYPECODE_I8:   u64 = 2;
pub const TYPECODE_U16:  u64 = 3;
pub const TYPECODE_I16:  u64 = 4;
pub const TYPECODE_U32:  u64 = 5;
pub const TYPECODE_I32:  u64 = 6;
pub const TYPECODE_U64:  u64 = 7;
pub const TYPECODE_I64:  u64 = 8;
pub const TYPECODE_U128: u64 = 9; // Note: Runtime handles actual 128-bit ops, this is just encoding
pub const TYPECODE_I128: u64 = 10;// Note: Runtime handles actual 128-bit ops, this is just encoding
pub const TYPECODE_F32:  u64 = 11;
pub const TYPECODE_F64:  u64 = 12;
pub const TYPECODE_BOOL: u64 = 13;
// Max 15 types with 4 bits

// Specific OpCodes (Within Category, bits 48-55 of payload)
// Arithmetic (Use with OPCAT_ARITHMETIC, SrcType indicates operand type)
pub const OP_ARITH_ADD: u64 = 0;
pub const OP_ARITH_SUB: u64 = 1;
pub const OP_ARITH_MUL: u64 = 2;
pub const OP_ARITH_DIV: u64 = 3;
pub const OP_ARITH_REM: u64 = 4;
pub const OP_ARITH_AND: u64 = 5; // Bitwise (Use BOOL category for logical)
pub const OP_ARITH_OR:  u64 = 6; // Bitwise (Use BOOL category for logical)
pub const OP_ARITH_XOR: u64 = 7; // Bitwise (Use BOOL category for logical)
pub const OP_ARITH_SHL: u64 = 8;
pub const OP_ARITH_SHR: u64 = 9; // Arithmetic/Logical determined by SrcType at runtime
pub const OP_ARITH_NEG: u64 = 10;
pub const OP_ARITH_NOT: u64 = 11; // Bitwise (Use BOOL category for logical)
pub const OP_ARITH_ABS: u64 = 12;
pub const OP_ARITH_POW: u64 = 13;
pub const OP_ARITH_EQ:  u64 = 14; // Comparison
pub const OP_ARITH_NE:  u64 = 15; // Comparison
pub const OP_ARITH_LT:  u64 = 16; // Comparison
pub const OP_ARITH_LE:  u64 = 17; // Comparison
pub const OP_ARITH_GT:  u64 = 18; // Comparison
pub const OP_ARITH_GE:  u64 = 19; // Comparison

// Boolean (Use with OPCAT_BOOLEAN, SrcType=TYPECODE_BOOL, TgtType=TYPECODE_BOOL)
// Logical operations specifically for boolean values.
pub const OP_BOOL_AND: u64 = 0;
pub const OP_BOOL_OR:  u64 = 1;
pub const OP_BOOL_XOR: u64 = 2;
pub const OP_BOOL_NOT: u64 = 3;
// Comparisons (EQ, NE) can reuse OP_ARITH_EQ/NE with OPCAT_ARITHMETIC and TYPECODE_BOOL

// Conversion (Use with OPCAT_CONVERSION, SrcType and TgtType are significant)
pub const OP_CONVERT: u64 = 0; // The only opcode needed in this category

// Runtime (Use with OPCAT_RUNTIME)
pub const OP_RUNTIME_PANIC:    u64 = 0;
pub const OP_RUNTIME_PRINTLN: u64 = 1;
pub const OP_RUNTIME_READLN:  u64 = 2;
// Add opcode for array get
pub const OP_RUNTIME_ARRAY_GET: u64 = 3;
// Add other runtime ops like memory allocation, thread spawn, etc.

// --- Encoding Functions ---

/// Encodes the full 64-bit value for a Static node.
#[inline(always)]
pub const fn encode_static_data(tag: u64, data_payload: u64) -> u64 {
    // Basic assertion, could be more robust if not const fn
    // debug_assert!(data_payload <= DATA_MASK, "Static data payload exceeds 60 bits");
    // debug_assert!(tag <= 0xF, "Static tag exceeds 4 bits");
    (tag << TAG_SHIFT) | (data_payload & DATA_MASK)
}

/// Encodes the 60-bit data payload for an arithmetic or comparison operation.
#[inline(always)]
pub const fn encode_arithmetic_payload(op_code: u64, src_type: u64) -> u64 {
    (OPCAT_ARITHMETIC << OPCAT_SHIFT)
        | ((op_code & OPCODE_MASK) << OPCODE_SHIFT)
        | ((src_type & TYPE_MASK) << SRC_TYPE_SHIFT)
        // Target type is not usually relevant for arithmetic encoding itself
}

/// Encodes the 60-bit data payload for a boolean operation.
#[inline(always)]
pub const fn encode_boolean_payload(op_code: u64) -> u64 {
    (OPCAT_BOOLEAN << OPCAT_SHIFT)
        | ((op_code & OPCODE_MASK) << OPCODE_SHIFT)
        | (TYPECODE_BOOL << SRC_TYPE_SHIFT) // Source is always bool
        // Target type is not usually relevant for boolean encoding itself
}

/// Encodes the 60-bit data payload for a type conversion operation.
#[inline(always)]
pub const fn encode_conversion_payload(src_type: u64, tgt_type: u64) -> u64 {
    (OPCAT_CONVERSION << OPCAT_SHIFT)
        | ((OP_CONVERT & OPCODE_MASK) << OPCODE_SHIFT) // Opcode is always OP_CONVERT
        | ((src_type & TYPE_MASK) << SRC_TYPE_SHIFT)
        | ((tgt_type & TYPE_MASK) << TGT_TYPE_SHIFT)
}

/// Encodes the 60-bit data payload for a runtime operation (like IO or panic).
#[inline(always)]
pub const fn encode_runtime_payload(op_code: u64) -> u64 {
    (OPCAT_RUNTIME << OPCAT_SHIFT)
        | ((op_code & OPCODE_MASK) << OPCODE_SHIFT)
        // Type fields are not usually relevant for runtime ops
}

/// Convenience function to encode a full Static node value for an intrinsic operation.
#[inline(always)]
pub const fn encode_intrinsic_op(op_category: u64, op_code: u64, src_type: u64, tgt_type: u64) -> u64 {
     let payload = (op_category << OPCAT_SHIFT)
                | ((op_code & OPCODE_MASK) << OPCODE_SHIFT)
                | ((src_type & TYPE_MASK) << SRC_TYPE_SHIFT)
                | ((tgt_type & TYPE_MASK) << TGT_TYPE_SHIFT);
     encode_static_data(TAG_INTRINSIC_OP, payload)
}

/// Encodes a Static node representing a function pointer.
#[inline(always)]
pub fn encode_static_function(symbol: Symbol) -> u64 {
    encode_static_data(TAG_FUNCTION, symbol.id() as u64) // Assumes Symbol::id() fits
}

/// Encodes a Static node representing a global variable address/reference.
#[inline(always)]
pub fn encode_static_global_var(symbol: Symbol) -> u64 {
    encode_static_data(TAG_GLOBAL_VAR, symbol.id() as u64) // Assumes Symbol::id() fits
}

/// Encodes a Static node representing a variant tag for matching.
#[inline(always)]
pub fn encode_static_variant(symbol: Symbol) -> u64 {
    encode_static_data(TAG_IS_VARIANT, symbol.id() as u64) // Assumes Symbol::id() fits
}

/// Encodes a Static node representing NIL.
#[inline(always)]
pub const fn encode_static_nil() -> u64 {
    encode_static_data(TAG_NIL, 0)
}

// --- Decoding Functions ---

/// Decodes the 4-bit tag from a Static node's 64-bit data.
#[inline(always)]
pub const fn decode_static_tag(static_data: u64) -> u64 {
    static_data >> TAG_SHIFT
}

/// Decodes the 60-bit data payload from a Static node's 64-bit data.
#[inline(always)]
pub const fn decode_static_payload(static_data: u64) -> u64 {
    static_data & DATA_MASK
}

/// Decodes the operation category from an intrinsic op's 60-bit payload.
#[inline(always)]
pub const fn decode_intrinsic_category(payload: u64) -> u64 {
    (payload >> OPCAT_SHIFT) & OPCAT_MASK
}

/// Decodes the specific opcode from an intrinsic op's 60-bit payload.
#[inline(always)]
pub const fn decode_intrinsic_opcode(payload: u64) -> u64 {
    (payload >> OPCODE_SHIFT) & OPCODE_MASK
}

/// Decodes the source type code from an intrinsic op's 60-bit payload.
#[inline(always)]
pub const fn decode_intrinsic_src_type(payload: u64) -> u64 {
    (payload >> SRC_TYPE_SHIFT) & TYPE_MASK
}

/// Decodes the target type code from an intrinsic op's 60-bit payload.
#[inline(always)]
pub const fn decode_intrinsic_tgt_type(payload: u64) -> u64 {
    (payload >> TGT_TYPE_SHIFT) & TYPE_MASK
}

/// Decodes the Function Symbol ID from a TAG_FUNCTION static node's payload.
#[inline(always)]
pub const fn decode_function_symbol_id(payload: u64) -> u64 {
    payload // The payload *is* the ID
}

/// Decodes the Variant Symbol ID from a TAG_IS_VARIANT static node's payload.
#[inline(always)]
pub const fn decode_variant_symbol_id(payload: u64) -> u64 {
    payload // The payload *is* the ID
} 

// --- Literal Encoding Helper ---

/// Encodes an HIR Literal into a u128 for storage in a Number node.
/// Returns an error if the literal type is not directly encodable (e.g., String).
pub fn encode_literal_to_u128(literal: &parallax_hir::hir::HirLiteral) -> Result<u128, crate::lowering::error::LoweringError> {
    use parallax_hir::hir::HirLiteral;
    match literal {
        HirLiteral::IntLiteral { value: i, .. } => Ok(*i as u128),
        HirLiteral::FloatLiteral { value: f, .. } => Ok(f.to_bits() as u128),
        HirLiteral::BoolLiteral(b) => Ok(*b as u128),
        HirLiteral::CharLiteral(c) => Ok(*c as u128),
        HirLiteral::Unit => Ok(0),
        HirLiteral::StringLiteral(_) => Err(crate::lowering::error::LoweringError::NotImplemented(
            "String Constant Lowering to Number Node"
        )),
    }
} 