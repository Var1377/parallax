use crate::num::*;

// Type conversion traits
pub trait Into<T> {
    fn into(self) -> T;
}

pub trait TryInto<T> {
    fn try_into(self) -> Option<T>;
}


// ---------- i8 Conversions ----------
// Widening conversions to signed types (always safe)
impl Into<i16> for i8 {
    fn into(self) -> i16 {
        __intrinsic_i8_to_i16__(self)
    }
}

impl Into<i32> for i8 {
    fn into(self) -> i32 {
        __intrinsic_i8_to_i32__(self)
    }
}

impl Into<i64> for i8 {
    fn into(self) -> i64 {
        __intrinsic_i8_to_i64__(self)
    }
}

impl Into<i128> for i8 {
    fn into(self) -> i128 {
        __intrinsic_i8_to_i128__(self)
    }
}

// Conversion to unsigned types (potentially unsafe if i8 < 0)
impl TryInto<u8> for i8 {
    fn try_into(self) -> Option<u8> {
        __intrinsic_i8_to_u8__(self)
    }
}

impl TryInto<u16> for i8 {
    fn try_into(self) -> Option<u16> {
        __intrinsic_i8_to_u16__(self)
    }
}

impl TryInto<u32> for i8 {
    fn try_into(self) -> Option<u32> {
        __intrinsic_i8_to_u32__(self)
    }
}

impl TryInto<u64> for i8 {
    fn try_into(self) -> Option<u64> {
        __intrinsic_i8_to_u64__(self)
    }
}

impl TryInto<u128> for i8 {
    fn try_into(self) -> Option<u128> {
        __intrinsic_i8_to_u128__(self)
    }
}

// Conversions to floating point types (always safe)
impl Into<f32> for i8 {
    fn into(self) -> f32 {
        __intrinsic_i8_to_f32__(self)
    }
}

impl Into<f64> for i8 {
    fn into(self) -> f64 {
        __intrinsic_i8_to_f64__(self)
    }
}

// ---------- u8 Conversions ----------
// Widening conversions to larger unsigned types (always safe)
impl Into<u16> for u8 {
    fn into(self) -> u16 {
        __intrinsic_u8_to_u16__(self)
    }
}

impl Into<u32> for u8 {
    fn into(self) -> u32 {
        __intrinsic_u8_to_u32__(self)
    }
}

impl Into<u64> for u8 {
    fn into(self) -> u64 {
        __intrinsic_u8_to_u64__(self)
    }
}

impl Into<u128> for u8 {
    fn into(self) -> u128 {
        __intrinsic_u8_to_u128__(self)
    }
}

// Widening conversions to signed types that can hold all u8 values (always safe)
impl Into<i16> for u8 {
    fn into(self) -> i16 {
        __intrinsic_u8_to_i16__(self)
    }
}

impl Into<i32> for u8 {
    fn into(self) -> i32 {
        __intrinsic_u8_to_i32__(self)
    }
}

impl Into<i64> for u8 {
    fn into(self) -> i64 {
        __intrinsic_u8_to_i64__(self)
    }
}

impl Into<i128> for u8 {
    fn into(self) -> i128 {
        __intrinsic_u8_to_i128__(self)
    }
}

// Conversion to i8 (potentially unsafe if u8 > 127)
impl TryInto<i8> for u8 {
    fn try_into(self) -> Option<i8> {
        __intrinsic_u8_to_i8__(self)
    }
}

// Conversions to floating point types (always safe, may lose precision)
impl Into<f32> for u8 {
    fn into(self) -> f32 {
        __intrinsic_u8_to_f32__(self)
    }
}

impl Into<f64> for u8 {
    fn into(self) -> f64 {
        __intrinsic_u8_to_f64__(self)
    }
}

// ---------- u16 Conversions ----------
// Widening conversions to larger unsigned types (always safe)
impl Into<u32> for u16 {
    fn into(self) -> u32 {
        __intrinsic_u16_to_u32__(self)
    }
}

impl Into<u64> for u16 {
    fn into(self) -> u64 {
        __intrinsic_u16_to_u64__(self)
    }
}

impl Into<u128> for u16 {
    fn into(self) -> u128 {
        __intrinsic_u16_to_u128__(self)
    }
}

// Widening conversions to signed types that can hold all u16 values (always safe)
impl Into<i32> for u16 {
    fn into(self) -> i32 {
        __intrinsic_u16_to_i32__(self)
    }
}

impl Into<i64> for u16 {
    fn into(self) -> i64 {
        __intrinsic_u16_to_i64__(self)
    }
}

impl Into<i128> for u16 {
    fn into(self) -> i128 {
        __intrinsic_u16_to_i128__(self)
    }
}

// Narrowing conversions to smaller types (potentially unsafe)
impl TryInto<u8> for u16 {
    fn try_into(self) -> Option<u8> {
        __intrinsic_u16_to_u8__(self)
    }
}

impl TryInto<i8> for u16 {
    fn try_into(self) -> Option<i8> {
        __intrinsic_u16_to_i8__(self)
    }
}

impl TryInto<i16> for u16 {
    fn try_into(self) -> Option<i16> {
        __intrinsic_u16_to_i16__(self)
    }
}

// Conversions to floating point types (always safe, may lose precision)
impl Into<f32> for u16 {
    fn into(self) -> f32 {
        __intrinsic_u16_to_f32__(self)
    }
}

impl Into<f64> for u16 {
    fn into(self) -> f64 {
        __intrinsic_u16_to_f64__(self)
    }
}

// ---------- u32 Conversions ----------
// Widening conversions to larger unsigned types (always safe)
impl Into<u64> for u32 {
    fn into(self) -> u64 {
        __intrinsic_u32_to_u64__(self)
    }
}

impl Into<u128> for u32 {
    fn into(self) -> u128 {
        __intrinsic_u32_to_u128__(self)
    }
}

// Widening conversions to signed types that can hold all u32 values (always safe)
impl Into<i64> for u32 {
    fn into(self) -> i64 {
        __intrinsic_u32_to_i64__(self)
    }
}

impl Into<i128> for u32 {
    fn into(self) -> i128 {
        __intrinsic_u32_to_i128__(self)
    }
}

// Narrowing conversions to smaller types (potentially unsafe)
impl TryInto<u8> for u32 {
    fn try_into(self) -> Option<u8> {
        __intrinsic_u32_to_u8__(self)
    }
}

impl TryInto<u16> for u32 {
    fn try_into(self) -> Option<u16> {
        __intrinsic_u32_to_u16__(self)
    }
}

impl TryInto<i8> for u32 {
    fn try_into(self) -> Option<i8> {
        __intrinsic_u32_to_i8__(self)
    }
}

impl TryInto<i16> for u32 {
    fn try_into(self) -> Option<i16> {
        __intrinsic_u32_to_i16__(self)
    }
}

impl TryInto<i32> for u32 {
    fn try_into(self) -> Option<i32> {
        __intrinsic_u32_to_i32__(self)
    }
}

// Conversions to floating point types (always safe, may lose precision)
impl Into<f32> for u32 {
    fn into(self) -> f32 {
        __intrinsic_u32_to_f32__(self)
    }
}

impl Into<f64> for u32 {
    fn into(self) -> f64 {
        __intrinsic_u32_to_f64__(self)
    }
}

// ---------- u64 Conversions ----------
// Widening conversions to larger unsigned types (always safe)
impl Into<u128> for u64 {
    fn into(self) -> u128 {
        __intrinsic_u64_to_u128__(self)
    }
}

// Widening conversions to signed types that can hold all u64 values (always safe)
impl Into<i128> for u64 {
    fn into(self) -> i128 {
        __intrinsic_u64_to_i128__(self)
    }
}

// Narrowing conversions to smaller types (potentially unsafe)
impl TryInto<u8> for u64 {
    fn try_into(self) -> Option<u8> {
        __intrinsic_u64_to_u8__(self)
    }
}

impl TryInto<u16> for u64 {
    fn try_into(self) -> Option<u16> {
        __intrinsic_u64_to_u16__(self)
    }
}

impl TryInto<u32> for u64 {
    fn try_into(self) -> Option<u32> {
        __intrinsic_u64_to_u32__(self)
    }
}

impl TryInto<i8> for u64 {
    fn try_into(self) -> Option<i8> {
        __intrinsic_u64_to_i8__(self)
    }
}

impl TryInto<i16> for u64 {
    fn try_into(self) -> Option<i16> {
        __intrinsic_u64_to_i16__(self)
    }
}

impl TryInto<i32> for u64 {
    fn try_into(self) -> Option<i32> {
        __intrinsic_u64_to_i32__(self)
    }
}

impl TryInto<i64> for u64 {
    fn try_into(self) -> Option<i64> {
        __intrinsic_u64_to_i64__(self)
    }
}

// Conversions to floating point types (always safe, may lose precision)
impl Into<f32> for u64 {
    fn into(self) -> f32 {
        __intrinsic_u64_to_f32__(self)
    }
}

impl Into<f64> for u64 {
    fn into(self) -> f64 {
        __intrinsic_u64_to_f64__(self)
    }
}

// ---------- u128 Conversions ----------
// Narrowing conversions to smaller types (potentially unsafe)
impl TryInto<u8> for u128 {
    fn try_into(self) -> Option<u8> {
        __intrinsic_u128_to_u8__(self)
    }
}

impl TryInto<u16> for u128 {
    fn try_into(self) -> Option<u16> {
        __intrinsic_u128_to_u16__(self)
    }
}

impl TryInto<u32> for u128 {
    fn try_into(self) -> Option<u32> {
        __intrinsic_u128_to_u32__(self)
    }
}

impl TryInto<u64> for u128 {
    fn try_into(self) -> Option<u64> {
        __intrinsic_u128_to_u64__(self)
    }
}

impl TryInto<i8> for u128 {
    fn try_into(self) -> Option<i8> {
        __intrinsic_u128_to_i8__(self)
    }
}

impl TryInto<i16> for u128 {
    fn try_into(self) -> Option<i16> {
        __intrinsic_u128_to_i16__(self)
    }
}

impl TryInto<i32> for u128 {
    fn try_into(self) -> Option<i32> {
        __intrinsic_u128_to_i32__(self)
    }
}

impl TryInto<i64> for u128 {
    fn try_into(self) -> Option<i64> {
        __intrinsic_u128_to_i64__(self)
    }
}

impl TryInto<i128> for u128 {
    fn try_into(self) -> Option<i128> {
        __intrinsic_u128_to_i128__(self)
    }
}

// Conversions to floating point types (always safe, may lose precision for very large values)
impl Into<f32> for u128 {
    fn into(self) -> f32 {
        __intrinsic_u128_to_f32__(self)
    }
}

impl Into<f64> for u128 {
    fn into(self) -> f64 {
        __intrinsic_u128_to_f64__(self)
    }
}

// ---------- f32 Conversions ----------
// Widening conversions to f64 (always safe)
impl Into<f64> for f32 {
    fn into(self) -> f64 {
        __intrinsic_f32_to_f64__(self)
    }
}

// Conversions to integer types (potentially unsafe if out of range)
impl TryInto<i8> for f32 {
    fn try_into(self) -> Option<i8> {
        __intrinsic_f32_to_i8__(self)
    }
}

impl TryInto<i16> for f32 {
    fn try_into(self) -> Option<i16> {
        __intrinsic_f32_to_i16__(self)
    }
}

impl TryInto<i32> for f32 {
    fn try_into(self) -> Option<i32> {
        __intrinsic_f32_to_i32__(self)
    }
}

impl TryInto<i64> for f32 {
    fn try_into(self) -> Option<i64> {
        __intrinsic_f32_to_i64__(self)
    }
}

impl TryInto<i128> for f32 {
    fn try_into(self) -> Option<i128> {
        __intrinsic_f32_to_i128__(self)
    }
}

impl TryInto<u8> for f32 {
    fn try_into(self) -> Option<u8> {
        __intrinsic_f32_to_u8__(self)
    }
}

impl TryInto<u16> for f32 {
    fn try_into(self) -> Option<u16> {
        __intrinsic_f32_to_u16__(self)
    }
}

impl TryInto<u32> for f32 {
    fn try_into(self) -> Option<u32> {
        __intrinsic_f32_to_u32__(self)
    }
}

impl TryInto<u64> for f32 {
    fn try_into(self) -> Option<u64> {
        __intrinsic_f32_to_u64__(self)
    }
}

impl TryInto<u128> for f32 {
    fn try_into(self) -> Option<u128> {
        __intrinsic_f32_to_u128__(self)
    }
}

// ---------- f64 Conversions ----------
// Narrowing conversion to f32 (potentially loses precision)
impl TryInto<f32> for f64 {
    fn try_into(self) -> Option<f32> {
        __intrinsic_f64_to_f32__(self)
    }
}

// Conversions to integer types (potentially unsafe if out of range)
impl TryInto<i8> for f64 {
    fn try_into(self) -> Option<i8> {
        __intrinsic_f64_to_i8__(self)
    }
}

impl TryInto<i16> for f64 {
    fn try_into(self) -> Option<i16> {
        __intrinsic_f64_to_i16__(self)
    }
}

impl TryInto<i32> for f64 {
    fn try_into(self) -> Option<i32> {
        __intrinsic_f64_to_i32__(self)
    }
}

impl TryInto<i64> for f64 {
    fn try_into(self) -> Option<i64> {
        __intrinsic_f64_to_i64__(self)
    }
}

impl TryInto<i128> for f64 {
    fn try_into(self) -> Option<i128> {
        __intrinsic_f64_to_i128__(self)
    }
}

impl TryInto<u8> for f64 {
    fn try_into(self) -> Option<u8> {
        __intrinsic_f64_to_u8__(self)
    }
}

impl TryInto<u16> for f64 {
    fn try_into(self) -> Option<u16> {
        __intrinsic_f64_to_u16__(self)
    }
}

impl TryInto<u32> for f64 {
    fn try_into(self) -> Option<u32> {
        __intrinsic_f64_to_u32__(self)
    }
}

impl TryInto<u64> for f64 {
    fn try_into(self) -> Option<u64> {
        __intrinsic_f64_to_u64__(self)
    }
}

impl TryInto<u128> for f64 {
    fn try_into(self) -> Option<u128> {
        __intrinsic_f64_to_u128__(self)
    }
} 