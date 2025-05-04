use crate::ops::*;
use crate::option::Option;
use crate::panic::panic;
use crate::cmp::*;

// --------- Core Numeric Traits ---------

pub trait Abs {
    fn abs(self) -> Self;
}

pub trait Pow {
    fn pow(self, exponent: Self) -> Self;
}

// --------- Unsigned Integer Intrinsics & Trait Implementations ---------

// --- u8 ---
// Intrinsics
// pub fn __intrinsic_u8_add__(lhs: u8, rhs: u8) -> u8;
// pub fn __intrinsic_u8_sub__(lhs: u8, rhs: u8) -> u8;
// pub fn __intrinsic_u8_mul__(lhs: u8, rhs: u8) -> u8;
// pub fn __intrinsic_u8_div__(lhs: u8, rhs: u8) -> u8;
// pub fn __intrinsic_u8_rem__(lhs: u8, rhs: u8) -> u8;
// pub fn __intrinsic_u8_and__(lhs: u8, rhs: u8) -> u8;
// pub fn __intrinsic_u8_or__(lhs: u8, rhs: u8) -> u8;
// pub fn __intrinsic_u8_xor__(lhs: u8, rhs: u8) -> u8;
// pub fn __intrinsic_u8_shl__(lhs: u8, rhs: u8) -> u8;
// pub fn __intrinsic_u8_shr__(lhs: u8, rhs: u8) -> u8;
// pub fn __intrinsic_u8_not__(val: u8) -> u8;
// pub fn __intrinsic_u8_abs__(val: u8) -> u8;
// pub fn __intrinsic_u8_pow__(base: u8, exp: u8) -> u8;
// pub fn __intrinsic_u8_eq__(lhs: u8, rhs: u8) -> bool;
// pub fn __intrinsic_u8_ne__(lhs: u8, rhs: u8) -> bool;
// pub fn __intrinsic_u8_lt__(lhs: u8, rhs: u8) -> bool;
// pub fn __intrinsic_u8_le__(lhs: u8, rhs: u8) -> bool;
// pub fn __intrinsic_u8_gt__(lhs: u8, rhs: u8) -> bool;
// pub fn __intrinsic_u8_ge__(lhs: u8, rhs: u8) -> bool;
// 
// // Trait Implementations
// impl Add for u8 { fn add(self, other: Self) -> Self = __intrinsic_u8_add__(self, other); }
// impl Sub for u8 { fn sub(self, other: Self) -> Self = __intrinsic_u8_sub__(self, other); }
// impl Mul for u8 { fn mul(self, other: Self) -> Self = __intrinsic_u8_mul__(self, other); }
// impl Div for u8 { fn div(self, other: Self) -> Self = __intrinsic_u8_div__(self, other); }
// impl Rem for u8 { fn rem(self, other: Self) -> Self = __intrinsic_u8_rem__(self, other); }
// impl Abs for u8 { fn abs(self) -> Self = __intrinsic_u8_abs__(self); }
// impl Pow for u8 { fn pow(self, exponent: Self) -> Self = __intrinsic_u8_pow__(self, exponent); }
// impl And for u8 { fn and(self, other: Self) -> Self = __intrinsic_u8_and__(self, other); }
// impl Or for u8 { fn or(self, other: Self) -> Self = __intrinsic_u8_or__(self, other); }
// impl Xor for u8 { fn xor(self, other: Self) -> Self = __intrinsic_u8_xor__(self, other); }
// impl Not for u8 { fn not(self) -> Self = __intrinsic_u8_not__(self); }
// impl ShiftLeft for u8 { fn shl(self, other: Self) -> Self = __intrinsic_u8_shl__(self, other); }
// impl ShiftRight for u8 { fn shr(self, other: Self) -> Self = __intrinsic_u8_shr__(self, other); }
// 
// // --- u16 ---
// // Intrinsics
// pub fn __intrinsic_u16_add__(lhs: u16, rhs: u16) -> u16;
// pub fn __intrinsic_u16_sub__(lhs: u16, rhs: u16) -> u16;
// pub fn __intrinsic_u16_mul__(lhs: u16, rhs: u16) -> u16;
// pub fn __intrinsic_u16_div__(lhs: u16, rhs: u16) -> u16;
// pub fn __intrinsic_u16_rem__(lhs: u16, rhs: u16) -> u16;
// pub fn __intrinsic_u16_and__(lhs: u16, rhs: u16) -> u16;
// pub fn __intrinsic_u16_or__(lhs: u16, rhs: u16) -> u16;
// pub fn __intrinsic_u16_xor__(lhs: u16, rhs: u16) -> u16;
// pub fn __intrinsic_u16_shl__(lhs: u16, rhs: u16) -> u16;
// pub fn __intrinsic_u16_shr__(lhs: u16, rhs: u16) -> u16;
// pub fn __intrinsic_u16_not__(val: u16) -> u16;
// pub fn __intrinsic_u16_abs__(val: u16) -> u16;
// pub fn __intrinsic_u16_pow__(base: u16, exp: u16) -> u16;
// pub fn __intrinsic_u16_eq__(lhs: u16, rhs: u16) -> bool;
// pub fn __intrinsic_u16_ne__(lhs: u16, rhs: u16) -> bool;
// pub fn __intrinsic_u16_lt__(lhs: u16, rhs: u16) -> bool;
// pub fn __intrinsic_u16_le__(lhs: u16, rhs: u16) -> bool;
// pub fn __intrinsic_u16_gt__(lhs: u16, rhs: u16) -> bool;
// pub fn __intrinsic_u16_ge__(lhs: u16, rhs: u16) -> bool;
// 
// // Trait Implementations
// impl Add for u16 { fn add(self, other: Self) -> Self = __intrinsic_u16_add__(self, other); }
// impl Sub for u16 { fn sub(self, other: Self) -> Self = __intrinsic_u16_sub__(self, other); }
// impl Mul for u16 { fn mul(self, other: Self) -> Self = __intrinsic_u16_mul__(self, other); }
// impl Div for u16 { fn div(self, other: Self) -> Self = __intrinsic_u16_div__(self, other); }
// impl Rem for u16 { fn rem(self, other: Self) -> Self = __intrinsic_u16_rem__(self, other); }
// impl Abs for u16 { fn abs(self) -> Self = __intrinsic_u16_abs__(self); }
// impl Pow for u16 { fn pow(self, exponent: Self) -> Self = __intrinsic_u16_pow__(self, exponent); }
// impl And for u16 { fn and(self, other: Self) -> Self = __intrinsic_u16_and__(self, other); }
// impl Or for u16 { fn or(self, other: Self) -> Self = __intrinsic_u16_or__(self, other); }
// impl Xor for u16 { fn xor(self, other: Self) -> Self = __intrinsic_u16_xor__(self, other); }
// impl Not for u16 { fn not(self) -> Self = __intrinsic_u16_not__(self); }
// impl ShiftLeft for u16 { fn shl(self, other: Self) -> Self = __intrinsic_u16_shl__(self, other); }
// impl ShiftRight for u16 { fn shr(self, other: Self) -> Self = __intrinsic_u16_shr__(self, other); }
// 
// // --- u32 ---
// // Intrinsics
// pub fn __intrinsic_u32_add__(lhs: u32, rhs: u32) -> u32;
// pub fn __intrinsic_u32_sub__(lhs: u32, rhs: u32) -> u32;
// pub fn __intrinsic_u32_mul__(lhs: u32, rhs: u32) -> u32;
// pub fn __intrinsic_u32_div__(lhs: u32, rhs: u32) -> u32;
// pub fn __intrinsic_u32_rem__(lhs: u32, rhs: u32) -> u32;
// pub fn __intrinsic_u32_and__(lhs: u32, rhs: u32) -> u32;
// pub fn __intrinsic_u32_or__(lhs: u32, rhs: u32) -> u32;
// pub fn __intrinsic_u32_xor__(lhs: u32, rhs: u32) -> u32;
// pub fn __intrinsic_u32_shl__(lhs: u32, rhs: u32) -> u32;
// pub fn __intrinsic_u32_shr__(lhs: u32, rhs: u32) -> u32;
// pub fn __intrinsic_u32_not__(val: u32) -> u32;
// pub fn __intrinsic_u32_abs__(val: u32) -> u32;
// pub fn __intrinsic_u32_pow__(base: u32, exp: u32) -> u32;
// pub fn __intrinsic_u32_eq__(lhs: u32, rhs: u32) -> bool;
// pub fn __intrinsic_u32_ne__(lhs: u32, rhs: u32) -> bool;
// pub fn __intrinsic_u32_lt__(lhs: u32, rhs: u32) -> bool;
// pub fn __intrinsic_u32_le__(lhs: u32, rhs: u32) -> bool;
// pub fn __intrinsic_u32_gt__(lhs: u32, rhs: u32) -> bool;
// pub fn __intrinsic_u32_ge__(lhs: u32, rhs: u32) -> bool;
// 
// // Trait Implementations
// impl Add for u32 { fn add(self, other: Self) -> Self = __intrinsic_u32_add__(self, other); }
// impl Sub for u32 { fn sub(self, other: Self) -> Self = __intrinsic_u32_sub__(self, other); }
// impl Mul for u32 { fn mul(self, other: Self) -> Self = __intrinsic_u32_mul__(self, other); }
// impl Div for u32 { fn div(self, other: Self) -> Self = __intrinsic_u32_div__(self, other); }
// impl Rem for u32 { fn rem(self, other: Self) -> Self = __intrinsic_u32_rem__(self, other); }
// impl Abs for u32 { fn abs(self) -> Self = __intrinsic_u32_abs__(self); }
// impl Pow for u32 { fn pow(self, exponent: Self) -> Self = __intrinsic_u32_pow__(self, exponent); }
// impl And for u32 { fn and(self, other: Self) -> Self = __intrinsic_u32_and__(self, other); }
// impl Or for u32 { fn or(self, other: Self) -> Self = __intrinsic_u32_or__(self, other); }
// impl Xor for u32 { fn xor(self, other: Self) -> Self = __intrinsic_u32_xor__(self, other); }
// impl Not for u32 { fn not(self) -> Self = __intrinsic_u32_not__(self); }
// impl ShiftLeft for u32 { fn shl(self, other: Self) -> Self = __intrinsic_u32_shl__(self, other); }
// impl ShiftRight for u32 { fn shr(self, other: Self) -> Self = __intrinsic_u32_shr__(self, other); }
// 
// // --- u64 ---
// // Intrinsics
// pub fn __intrinsic_u64_add__(lhs: u64, rhs: u64) -> u64;
// pub fn __intrinsic_u64_sub__(lhs: u64, rhs: u64) -> u64;
// pub fn __intrinsic_u64_mul__(lhs: u64, rhs: u64) -> u64;
// pub fn __intrinsic_u64_div__(lhs: u64, rhs: u64) -> u64;
// pub fn __intrinsic_u64_rem__(lhs: u64, rhs: u64) -> u64;
// pub fn __intrinsic_u64_and__(lhs: u64, rhs: u64) -> u64;
// pub fn __intrinsic_u64_or__(lhs: u64, rhs: u64) -> u64;
// pub fn __intrinsic_u64_xor__(lhs: u64, rhs: u64) -> u64;
// pub fn __intrinsic_u64_shl__(lhs: u64, rhs: u64) -> u64;
// pub fn __intrinsic_u64_shr__(lhs: u64, rhs: u64) -> u64;
// pub fn __intrinsic_u64_not__(val: u64) -> u64;
// pub fn __intrinsic_u64_abs__(val: u64) -> u64;
// pub fn __intrinsic_u64_pow__(base: u64, exp: u64) -> u64;
// pub fn __intrinsic_u64_eq__(lhs: u64, rhs: u64) -> bool;
// pub fn __intrinsic_u64_ne__(lhs: u64, rhs: u64) -> bool;
// pub fn __intrinsic_u64_lt__(lhs: u64, rhs: u64) -> bool;
// pub fn __intrinsic_u64_le__(lhs: u64, rhs: u64) -> bool;
// pub fn __intrinsic_u64_gt__(lhs: u64, rhs: u64) -> bool;
// pub fn __intrinsic_u64_ge__(lhs: u64, rhs: u64) -> bool;
// 
// // Trait Implementations
// impl Add for u64 { fn add(self, other: Self) -> Self = __intrinsic_u64_add__(self, other); }
// impl Sub for u64 { fn sub(self, other: Self) -> Self = __intrinsic_u64_sub__(self, other); }
// impl Mul for u64 { fn mul(self, other: Self) -> Self = __intrinsic_u64_mul__(self, other); }
// impl Div for u64 { fn div(self, other: Self) -> Self = __intrinsic_u64_div__(self, other); }
// impl Rem for u64 { fn rem(self, other: Self) -> Self = __intrinsic_u64_rem__(self, other); }
// impl Abs for u64 { fn abs(self) -> Self = __intrinsic_u64_abs__(self); }
// impl Pow for u64 { fn pow(self, exponent: Self) -> Self = __intrinsic_u64_pow__(self, exponent); }
// impl And for u64 { fn and(self, other: Self) -> Self = __intrinsic_u64_and__(self, other); }
// impl Or for u64 { fn or(self, other: Self) -> Self = __intrinsic_u64_or__(self, other); }
// impl Xor for u64 { fn xor(self, other: Self) -> Self = __intrinsic_u64_xor__(self, other); }
// impl Not for u64 { fn not(self) -> Self = __intrinsic_u64_not__(self); }
// impl ShiftLeft for u64 { fn shl(self, other: Self) -> Self = __intrinsic_u64_shl__(self, other); }
// impl ShiftRight for u64 { fn shr(self, other: Self) -> Self = __intrinsic_u64_shr__(self, other); }
// 
// // --- u128 ---
// // Intrinsics
// pub fn __intrinsic_u128_add__(lhs: u128, rhs: u128) -> u128;
// pub fn __intrinsic_u128_sub__(lhs: u128, rhs: u128) -> u128;
// pub fn __intrinsic_u128_mul__(lhs: u128, rhs: u128) -> u128;
// pub fn __intrinsic_u128_div__(lhs: u128, rhs: u128) -> u128;
// pub fn __intrinsic_u128_rem__(lhs: u128, rhs: u128) -> u128;
// pub fn __intrinsic_u128_and__(lhs: u128, rhs: u128) -> u128;
// pub fn __intrinsic_u128_or__(lhs: u128, rhs: u128) -> u128;
// pub fn __intrinsic_u128_xor__(lhs: u128, rhs: u128) -> u128;
// pub fn __intrinsic_u128_shl__(lhs: u128, rhs: u128) -> u128;
// pub fn __intrinsic_u128_shr__(lhs: u128, rhs: u128) -> u128;
// pub fn __intrinsic_u128_not__(val: u128) -> u128;
// pub fn __intrinsic_u128_abs__(val: u128) -> u128;
// pub fn __intrinsic_u128_pow__(base: u128, exp: u128) -> u128;
// pub fn __intrinsic_u128_eq__(lhs: u128, rhs: u128) -> bool;
// pub fn __intrinsic_u128_ne__(lhs: u128, rhs: u128) -> bool;
// pub fn __intrinsic_u128_lt__(lhs: u128, rhs: u128) -> bool;
// pub fn __intrinsic_u128_le__(lhs: u128, rhs: u128) -> bool;
// pub fn __intrinsic_u128_gt__(lhs: u128, rhs: u128) -> bool;
// pub fn __intrinsic_u128_ge__(lhs: u128, rhs: u128) -> bool;
// 
// // Trait Implementations
// impl Add for u128 { fn add(self, other: Self) -> Self = __intrinsic_u128_add__(self, other); }
// impl Sub for u128 { fn sub(self, other: Self) -> Self = __intrinsic_u128_sub__(self, other); }
// impl Mul for u128 { fn mul(self, other: Self) -> Self = __intrinsic_u128_mul__(self, other); }
// impl Div for u128 { fn div(self, other: Self) -> Self = __intrinsic_u128_div__(self, other); }
// impl Rem for u128 { fn rem(self, other: Self) -> Self = __intrinsic_u128_rem__(self, other); }
// impl Abs for u128 { fn abs(self) -> Self = __intrinsic_u128_abs__(self); }
// impl Pow for u128 { fn pow(self, exponent: Self) -> Self = __intrinsic_u128_pow__(self, exponent); }
// impl And for u128 { fn and(self, other: Self) -> Self = __intrinsic_u128_and__(self, other); }
// impl Or for u128 { fn or(self, other: Self) -> Self = __intrinsic_u128_or__(self, other); }
// impl Xor for u128 { fn xor(self, other: Self) -> Self = __intrinsic_u128_xor__(self, other); }
// impl Not for u128 { fn not(self) -> Self = __intrinsic_u128_not__(self); }
// impl ShiftLeft for u128 { fn shl(self, other: Self) -> Self = __intrinsic_u128_shl__(self, other); }
// impl ShiftRight for u128 { fn shr(self, other: Self) -> Self = __intrinsic_u128_shr__(self, other); }
// 
// // --------- Signed Integer Intrinsics & Trait Implementations ---------
// 
// // --- i8 ---
// // Intrinsics
// pub fn __intrinsic_i8_add__(lhs: i8, rhs: i8) -> i8;
// pub fn __intrinsic_i8_sub__(lhs: i8, rhs: i8) -> i8;
// pub fn __intrinsic_i8_mul__(lhs: i8, rhs: i8) -> i8;
// pub fn __intrinsic_i8_div__(lhs: i8, rhs: i8) -> i8;
// pub fn __intrinsic_i8_rem__(lhs: i8, rhs: i8) -> i8;
// pub fn __intrinsic_i8_and__(lhs: i8, rhs: i8) -> i8;
// pub fn __intrinsic_i8_or__(lhs: i8, rhs: i8) -> i8;
// pub fn __intrinsic_i8_xor__(lhs: i8, rhs: i8) -> i8;
// pub fn __intrinsic_i8_shl__(lhs: i8, rhs: i8) -> i8;
// pub fn __intrinsic_i8_shr__(lhs: i8, rhs: i8) -> i8; // Arithmetic shift
// pub fn __intrinsic_i8_neg__(val: i8) -> i8;
// pub fn __intrinsic_i8_not__(val: i8) -> i8;
// pub fn __intrinsic_i8_abs__(val: i8) -> i8;
// pub fn __intrinsic_i8_pow__(base: i8, exp: i8) -> i8;
// pub fn __intrinsic_i8_eq__(lhs: i8, rhs: i8) -> bool;
// pub fn __intrinsic_i8_ne__(lhs: i8, rhs: i8) -> bool;
// pub fn __intrinsic_i8_lt__(lhs: i8, rhs: i8) -> bool;
// pub fn __intrinsic_i8_le__(lhs: i8, rhs: i8) -> bool;
// pub fn __intrinsic_i8_gt__(lhs: i8, rhs: i8) -> bool;
// pub fn __intrinsic_i8_ge__(lhs: i8, rhs: i8) -> bool;
// 
// // Trait Implementations
// impl Add for i8 { fn add(self, other: Self) -> Self = __intrinsic_i8_add__(self, other); }
// impl Sub for i8 { fn sub(self, other: Self) -> Self = __intrinsic_i8_sub__(self, other); }
// impl Mul for i8 { fn mul(self, other: Self) -> Self = __intrinsic_i8_mul__(self, other); }
// impl Div for i8 { fn div(self, other: Self) -> Self = __intrinsic_i8_div__(self, other); }
// impl Rem for i8 { fn rem(self, other: Self) -> Self = __intrinsic_i8_rem__(self, other); }
// impl Neg for i8 { fn neg(self) -> Self = __intrinsic_i8_neg__(self); }
// impl Abs for i8 { fn abs(self) -> Self = __intrinsic_i8_abs__(self); }
// impl Pow for i8 { fn pow(self, exponent: Self) -> Self = __intrinsic_i8_pow__(self, exponent); }
// impl And for i8 { fn and(self, other: Self) -> Self = __intrinsic_i8_and__(self, other); }
// impl Or for i8 { fn or(self, other: Self) -> Self = __intrinsic_i8_or__(self, other); }
// impl Xor for i8 { fn xor(self, other: Self) -> Self = __intrinsic_i8_xor__(self, other); }
// impl Not for i8 { fn not(self) -> Self = __intrinsic_i8_not__(self); }
// impl ShiftLeft for i8 { fn shl(self, other: Self) -> Self = __intrinsic_i8_shl__(self, other); }
// impl ShiftRight for i8 { fn shr(self, other: Self) -> Self = __intrinsic_i8_shr__(self, other); }
// 
// // --- i16 ---
// // Intrinsics
// pub fn __intrinsic_i16_add__(lhs: i16, rhs: i16) -> i16;
// pub fn __intrinsic_i16_sub__(lhs: i16, rhs: i16) -> i16;
// pub fn __intrinsic_i16_mul__(lhs: i16, rhs: i16) -> i16;
// pub fn __intrinsic_i16_div__(lhs: i16, rhs: i16) -> i16;
// pub fn __intrinsic_i16_rem__(lhs: i16, rhs: i16) -> i16;
// pub fn __intrinsic_i16_and__(lhs: i16, rhs: i16) -> i16;
// pub fn __intrinsic_i16_or__(lhs: i16, rhs: i16) -> i16;
// pub fn __intrinsic_i16_xor__(lhs: i16, rhs: i16) -> i16;
// pub fn __intrinsic_i16_shl__(lhs: i16, rhs: i16) -> i16;
// pub fn __intrinsic_i16_shr__(lhs: i16, rhs: i16) -> i16;
// pub fn __intrinsic_i16_neg__(val: i16) -> i16;
// pub fn __intrinsic_i16_not__(val: i16) -> i16;
// pub fn __intrinsic_i16_abs__(val: i16) -> i16;
// pub fn __intrinsic_i16_pow__(base: i16, exp: i16) -> i16;
// pub fn __intrinsic_i16_eq__(lhs: i16, rhs: i16) -> bool;
// pub fn __intrinsic_i16_ne__(lhs: i16, rhs: i16) -> bool;
// pub fn __intrinsic_i16_lt__(lhs: i16, rhs: i16) -> bool;
// pub fn __intrinsic_i16_le__(lhs: i16, rhs: i16) -> bool;
// pub fn __intrinsic_i16_gt__(lhs: i16, rhs: i16) -> bool;
// pub fn __intrinsic_i16_ge__(lhs: i16, rhs: i16) -> bool;
// 
// // Trait Implementations
// impl Add for i16 { fn add(self, other: Self) -> Self = __intrinsic_i16_add__(self, other); }
// impl Sub for i16 { fn sub(self, other: Self) -> Self = __intrinsic_i16_sub__(self, other); }
// impl Mul for i16 { fn mul(self, other: Self) -> Self = __intrinsic_i16_mul__(self, other); }
// impl Div for i16 { fn div(self, other: Self) -> Self = __intrinsic_i16_div__(self, other); }
// impl Rem for i16 { fn rem(self, other: Self) -> Self = __intrinsic_i16_rem__(self, other); }
// impl Neg for i16 { fn neg(self) -> Self = __intrinsic_i16_neg__(self); }
// impl Abs for i16 { fn abs(self) -> Self = __intrinsic_i16_abs__(self); }
// impl Pow for i16 { fn pow(self, exponent: Self) -> Self = __intrinsic_i16_pow__(self, exponent); }
// impl And for i16 { fn and(self, other: Self) -> Self = __intrinsic_i16_and__(self, other); }
// impl Or for i16 { fn or(self, other: Self) -> Self = __intrinsic_i16_or__(self, other); }
// impl Xor for i16 { fn xor(self, other: Self) -> Self = __intrinsic_i16_xor__(self, other); }
// impl Not for i16 { fn not(self) -> Self = __intrinsic_i16_not__(self); }
// impl ShiftLeft for i16 { fn shl(self, other: Self) -> Self = __intrinsic_i16_shl__(self, other); }
// impl ShiftRight for i16 { fn shr(self, other: Self) -> Self = __intrinsic_i16_shr__(self, other); }
// 
// // --- i32 ---
pub fn __intrinsic_i32_add__(lhs: i32, rhs: i32) -> i32;
// pub fn __intrinsic_i32_sub__(lhs: i32, rhs: i32) -> i32;
// pub fn __intrinsic_i32_mul__(lhs: i32, rhs: i32) -> i32;
// pub fn __intrinsic_i32_div__(lhs: i32, rhs: i32) -> i32;
// pub fn __intrinsic_i32_rem__(lhs: i32, rhs: i32) -> i32;
// pub fn __intrinsic_i32_and__(lhs: i32, rhs: i32) -> i32;
// pub fn __intrinsic_i32_or__(lhs: i32, rhs: i32) -> i32;
// pub fn __intrinsic_i32_xor__(lhs: i32, rhs: i32) -> i32;
// pub fn __intrinsic_i32_shl__(lhs: i32, rhs: i32) -> i32;
// pub fn __intrinsic_i32_shr__(lhs: i32, rhs: i32) -> i32;
// pub fn __intrinsic_i32_neg__(val: i32) -> i32;
// pub fn __intrinsic_i32_not__(val: i32) -> i32;
// pub fn __intrinsic_i32_abs__(val: i32) -> i32;
// pub fn __intrinsic_i32_pow__(base: i32, exp: i32) -> i32;
pub fn __intrinsic_i32_eq__(lhs: i32, rhs: i32) -> bool;
pub fn __intrinsic_i32_ne__(lhs: i32, rhs: i32) -> bool;
pub fn __intrinsic_i32_lt__(lhs: i32, rhs: i32) -> bool;
pub fn __intrinsic_i32_le__(lhs: i32, rhs: i32) -> bool;
pub fn __intrinsic_i32_gt__(lhs: i32, rhs: i32) -> bool;
pub fn __intrinsic_i32_ge__(lhs: i32, rhs: i32) -> bool;
// 
// // Trait Implementations
impl Add for i32 { fn add(self, other: Self) -> Self = __intrinsic_i32_add__(self, other); }
// impl Sub for i32 { fn sub(self, other: Self) -> Self = __intrinsic_i32_sub__(self, other); }
// impl Mul for i32 { fn mul(self, other: Self) -> Self = __intrinsic_i32_mul__(self, other); }
// impl Div for i32 { fn div(self, other: Self) -> Self = __intrinsic_i32_div__(self, other); }
// impl Rem for i32 { fn rem(self, other: Self) -> Self = __intrinsic_i32_rem__(self, other); }
// impl Neg for i32 { fn neg(self) -> Self = __intrinsic_i32_neg__(self); }
// impl Abs for i32 { fn abs(self) -> Self = __intrinsic_i32_abs__(self); }
// impl Pow for i32 { fn pow(self, exponent: Self) -> Self = __intrinsic_i32_pow__(self, exponent); }
// impl And for i32 { fn and(self, other: Self) -> Self = __intrinsic_i32_and__(self, other); }
// impl Or for i32 { fn or(self, other: Self) -> Self = __intrinsic_i32_or__(self, other); }
// impl Xor for i32 { fn xor(self, other: Self) -> Self = __intrinsic_i32_xor__(self, other); }
// impl Not for i32 { fn not(self) -> Self = __intrinsic_i32_not__(self); }
// impl ShiftLeft for i32 { fn shl(self, other: Self) -> Self = __intrinsic_i32_shl__(self, other); }
// impl ShiftRight for i32 { fn shr(self, other: Self) -> Self = __intrinsic_i32_shr__(self, other); }
// 
// // --- i64 ---
// // Intrinsics
// pub fn __intrinsic_i64_add__(lhs: i64, rhs: i64) -> i64;
// pub fn __intrinsic_i64_sub__(lhs: i64, rhs: i64) -> i64;
// pub fn __intrinsic_i64_mul__(lhs: i64, rhs: i64) -> i64;
// pub fn __intrinsic_i64_div__(lhs: i64, rhs: i64) -> i64;
// pub fn __intrinsic_i64_rem__(lhs: i64, rhs: i64) -> i64;
// pub fn __intrinsic_i64_and__(lhs: i64, rhs: i64) -> i64;
// pub fn __intrinsic_i64_or__(lhs: i64, rhs: i64) -> i64;
// pub fn __intrinsic_i64_xor__(lhs: i64, rhs: i64) -> i64;
// pub fn __intrinsic_i64_shl__(lhs: i64, rhs: i64) -> i64;
// pub fn __intrinsic_i64_shr__(lhs: i64, rhs: i64) -> i64;
// pub fn __intrinsic_i64_neg__(val: i64) -> i64;
// pub fn __intrinsic_i64_not__(val: i64) -> i64;
// pub fn __intrinsic_i64_abs__(val: i64) -> i64;
// pub fn __intrinsic_i64_pow__(base: i64, exp: i64) -> i64;
// pub fn __intrinsic_i64_eq__(lhs: i64, rhs: i64) -> bool;
// pub fn __intrinsic_i64_ne__(lhs: i64, rhs: i64) -> bool;
// pub fn __intrinsic_i64_lt__(lhs: i64, rhs: i64) -> bool;
// pub fn __intrinsic_i64_le__(lhs: i64, rhs: i64) -> bool;
// pub fn __intrinsic_i64_gt__(lhs: i64, rhs: i64) -> bool;
// pub fn __intrinsic_i64_ge__(lhs: i64, rhs: i64) -> bool;
// 
// // Trait Implementations
// impl Add for i64 { fn add(self, other: Self) -> Self = __intrinsic_i64_add__(self, other); }
// impl Sub for i64 { fn sub(self, other: Self) -> Self = __intrinsic_i64_sub__(self, other); }
// impl Mul for i64 { fn mul(self, other: Self) -> Self = __intrinsic_i64_mul__(self, other); }
// impl Div for i64 { fn div(self, other: Self) -> Self = __intrinsic_i64_div__(self, other); }
// impl Rem for i64 { fn rem(self, other: Self) -> Self = __intrinsic_i64_rem__(self, other); }
// impl Neg for i64 { fn neg(self) -> Self = __intrinsic_i64_neg__(self); }
// impl Abs for i64 { fn abs(self) -> Self = __intrinsic_i64_abs__(self); }
// impl Pow for i64 { fn pow(self, exponent: Self) -> Self = __intrinsic_i64_pow__(self, exponent); }
// impl And for i64 { fn and(self, other: Self) -> Self = __intrinsic_i64_and__(self, other); }
// impl Or for i64 { fn or(self, other: Self) -> Self = __intrinsic_i64_or__(self, other); }
// impl Xor for i64 { fn xor(self, other: Self) -> Self = __intrinsic_i64_xor__(self, other); }
// impl Not for i64 { fn not(self) -> Self = __intrinsic_i64_not__(self); }
// impl ShiftLeft for i64 { fn shl(self, other: Self) -> Self = __intrinsic_i64_shl__(self, other); }
// impl ShiftRight for i64 { fn shr(self, other: Self) -> Self = __intrinsic_i64_shr__(self, other); }
// 
// // --- i128 ---
// // Intrinsics
// pub fn __intrinsic_i128_add__(lhs: i128, rhs: i128) -> i128;
// pub fn __intrinsic_i128_sub__(lhs: i128, rhs: i128) -> i128;
// pub fn __intrinsic_i128_mul__(lhs: i128, rhs: i128) -> i128;
// pub fn __intrinsic_i128_div__(lhs: i128, rhs: i128) -> i128;
// pub fn __intrinsic_i128_rem__(lhs: i128, rhs: i128) -> i128;
// pub fn __intrinsic_i128_and__(lhs: i128, rhs: i128) -> i128;
// pub fn __intrinsic_i128_or__(lhs: i128, rhs: i128) -> i128;
// pub fn __intrinsic_i128_xor__(lhs: i128, rhs: i128) -> i128;
// pub fn __intrinsic_i128_shl__(lhs: i128, rhs: i128) -> i128;
// pub fn __intrinsic_i128_shr__(lhs: i128, rhs: i128) -> i128;
// pub fn __intrinsic_i128_neg__(val: i128) -> i128;
// pub fn __intrinsic_i128_not__(val: i128) -> i128;
// pub fn __intrinsic_i128_abs__(val: i128) -> i128;
// pub fn __intrinsic_i128_pow__(base: i128, exp: i128) -> i128;
// pub fn __intrinsic_i128_eq__(lhs: i128, rhs: i128) -> bool;
// pub fn __intrinsic_i128_ne__(lhs: i128, rhs: i128) -> bool;
// pub fn __intrinsic_i128_lt__(lhs: i128, rhs: i128) -> bool;
// pub fn __intrinsic_i128_le__(lhs: i128, rhs: i128) -> bool;
// pub fn __intrinsic_i128_gt__(lhs: i128, rhs: i128) -> bool;
// pub fn __intrinsic_i128_ge__(lhs: i128, rhs: i128) -> bool;
// 
// // Trait Implementations
// impl Add for i128 { fn add(self, other: Self) -> Self = __intrinsic_i128_add__(self, other); }
// impl Sub for i128 { fn sub(self, other: Self) -> Self = __intrinsic_i128_sub__(self, other); }
// impl Mul for i128 { fn mul(self, other: Self) -> Self = __intrinsic_i128_mul__(self, other); }
// impl Div for i128 { fn div(self, other: Self) -> Self = __intrinsic_i128_div__(self, other); }
// impl Rem for i128 { fn rem(self, other: Self) -> Self = __intrinsic_i128_rem__(self, other); }
// impl Neg for i128 { fn neg(self) -> Self = __intrinsic_i128_neg__(self); }
// impl Abs for i128 { fn abs(self) -> Self = __intrinsic_i128_abs__(self); }
// impl Pow for i128 { fn pow(self, exponent: Self) -> Self = __intrinsic_i128_pow__(self, exponent); }
// impl And for i128 { fn and(self, other: Self) -> Self = __intrinsic_i128_and__(self, other); }
// impl Or for i128 { fn or(self, other: Self) -> Self = __intrinsic_i128_or__(self, other); }
// impl Xor for i128 { fn xor(self, other: Self) -> Self = __intrinsic_i128_xor__(self, other); }
// impl Not for i128 { fn not(self) -> Self = __intrinsic_i128_not__(self); }
// impl ShiftLeft for i128 { fn shl(self, other: Self) -> Self = __intrinsic_i128_shl__(self, other); }
// impl ShiftRight for i128 { fn shr(self, other: Self) -> Self = __intrinsic_i128_shr__(self, other); }
// 
// // --------- Floating Point Intrinsics & Trait Implementations ---------
// 
// // --- f32 ---
// // Intrinsics
// pub fn __intrinsic_f32_add__(lhs: f32, rhs: f32) -> f32;
// pub fn __intrinsic_f32_sub__(lhs: f32, rhs: f32) -> f32;
// pub fn __intrinsic_f32_mul__(lhs: f32, rhs: f32) -> f32;
// pub fn __intrinsic_f32_div__(lhs: f32, rhs: f32) -> f32;
// pub fn __intrinsic_f32_rem__(lhs: f32, rhs: f32) -> f32;
// pub fn __intrinsic_f32_neg__(val: f32) -> f32;
// pub fn __intrinsic_f32_abs__(val: f32) -> f32;
// pub fn __intrinsic_f32_pow__(base: f32, exp: f32) -> f32;
// pub fn __intrinsic_f32_eq__(lhs: f32, rhs: f32) -> bool;
// pub fn __intrinsic_f32_ne__(lhs: f32, rhs: f32) -> bool;
// pub fn __intrinsic_f32_lt__(lhs: f32, rhs: f32) -> bool;
// pub fn __intrinsic_f32_le__(lhs: f32, rhs: f32) -> bool;
// pub fn __intrinsic_f32_gt__(lhs: f32, rhs: f32) -> bool;
// pub fn __intrinsic_f32_ge__(lhs: f32, rhs: f32) -> bool;
// 
// // Trait Implementations
// impl Add for f32 { fn add(self, other: Self) -> Self = __intrinsic_f32_add__(self, other); }
// impl Sub for f32 { fn sub(self, other: Self) -> Self = __intrinsic_f32_sub__(self, other); }
// impl Mul for f32 { fn mul(self, other: Self) -> Self = __intrinsic_f32_mul__(self, other); }
// impl Div for f32 { fn div(self, other: Self) -> Self = __intrinsic_f32_div__(self, other); }
// impl Rem for f32 { fn rem(self, other: Self) -> Self = __intrinsic_f32_rem__(self, other); }
// impl Neg for f32 { fn neg(self) -> Self = __intrinsic_f32_neg__(self); }
// impl Abs for f32 { fn abs(self) -> Self = __intrinsic_f32_abs__(self); }
// impl Pow for f32 { fn pow(self, exponent: Self) -> Self = __intrinsic_f32_pow__(self, exponent); }
// 
// // --- f64 ---
// // Intrinsics
// pub fn __intrinsic_f64_add__(lhs: f64, rhs: f64) -> f64;
// pub fn __intrinsic_f64_sub__(lhs: f64, rhs: f64) -> f64;
// pub fn __intrinsic_f64_mul__(lhs: f64, rhs: f64) -> f64;
// pub fn __intrinsic_f64_div__(lhs: f64, rhs: f64) -> f64;
// pub fn __intrinsic_f64_rem__(lhs: f64, rhs: f64) -> f64;
// pub fn __intrinsic_f64_neg__(val: f64) -> f64;
// pub fn __intrinsic_f64_abs__(val: f64) -> f64;
// pub fn __intrinsic_f64_pow__(base: f64, exp: f64) -> f64;
// pub fn __intrinsic_f64_eq__(lhs: f64, rhs: f64) -> bool;
// pub fn __intrinsic_f64_ne__(lhs: f64, rhs: f64) -> bool;
// pub fn __intrinsic_f64_lt__(lhs: f64, rhs: f64) -> bool;
// pub fn __intrinsic_f64_le__(lhs: f64, rhs: f64) -> bool;
// pub fn __intrinsic_f64_gt__(lhs: f64, rhs: f64) -> bool;
// pub fn __intrinsic_f64_ge__(lhs: f64, rhs: f64) -> bool;
// 
// // Trait Implementations
// impl Add for f64 { fn add(self, other: Self) -> Self = __intrinsic_f64_add__(self, other); }
// impl Sub for f64 { fn sub(self, other: Self) -> Self = __intrinsic_f64_sub__(self, other); }
// impl Mul for f64 { fn mul(self, other: Self) -> Self = __intrinsic_f64_mul__(self, other); }
// impl Div for f64 { fn div(self, other: Self) -> Self = __intrinsic_f64_div__(self, other); }
// impl Rem for f64 { fn rem(self, other: Self) -> Self = __intrinsic_f64_rem__(self, other); }
// impl Neg for f64 { fn neg(self) -> Self = __intrinsic_f64_neg__(self); }
// impl Abs for f64 { fn abs(self) -> Self = __intrinsic_f64_abs__(self); }
// impl Pow for f64 { fn pow(self, exponent: Self) -> Self = __intrinsic_f64_pow__(self, exponent); }
// 
// // --------- Boolean Intrinsics & Operations ---------
// // Note: Logical ops (&&, ||, !) are usually handled directly by the compiler,
// // but defining intrinsics can be useful for consistency or potential function calls.
// 
pub fn __intrinsic_bool_and__(lhs: bool, rhs: bool) -> bool;
pub fn __intrinsic_bool_or__(lhs: bool, rhs: bool) -> bool;
pub fn __intrinsic_bool_xor__(lhs: bool, rhs: bool) -> bool;
pub fn __intrinsic_bool_not__(val: bool) -> bool;
pub fn __intrinsic_bool_eq__(lhs: bool, rhs: bool) -> bool;

impl And for bool {
    fn and(self, other: Self) -> Self = __intrinsic_bool_and__(self, other);
}

impl Or for bool {
    fn or(self, other: Self) -> Self = __intrinsic_bool_or__(self, other);
}

impl Xor for bool {
    fn xor(self, other: Self) -> Self = __intrinsic_bool_xor__(self, other);
}

impl Not for bool {
    fn not(self) -> Self = __intrinsic_bool_not__(self);
}

impl Eq for bool {
    fn eq(self, other: Self) -> bool = __intrinsic_bool_eq__(self, other);
}

// --- i32 --- (Restore Add impl)
impl Add for i32 { fn add(self, other: Self) -> Self = __intrinsic_i32_add__(self, other); }

// 
// // No trait implementations for Add, Sub etc. on bool typically
// 
// // --------- Conversion Intrinsics ---------
// // These intrinsics are used by the Into/TryInto implementations in into.plx
// 
// // Signed to Signed
// pub fn __intrinsic_i8_to_i16__(val: i8) -> i16;
// pub fn __intrinsic_i8_to_i32__(val: i8) -> i32;
// pub fn __intrinsic_i8_to_i64__(val: i8) -> i64;
// pub fn __intrinsic_i8_to_i128__(val: i8) -> i128;
// pub fn __intrinsic_i16_to_i8__(val: i16) -> Option<i8>;
// pub fn __intrinsic_i16_to_i32__(val: i16) -> i32;
// pub fn __intrinsic_i16_to_i64__(val: i16) -> i64;
// pub fn __intrinsic_i16_to_i128__(val: i16) -> i128;
// pub fn __intrinsic_i32_to_i8__(val: i32) -> Option<i8>;
// pub fn __intrinsic_i32_to_i16__(val: i32) -> Option<i16>;
// pub fn __intrinsic_i32_to_i64__(val: i32) -> i64;
// pub fn __intrinsic_i32_to_i128__(val: i32) -> i128;
// pub fn __intrinsic_i64_to_i8__(val: i64) -> Option<i8>;
// pub fn __intrinsic_i64_to_i16__(val: i64) -> Option<i16>;
// pub fn __intrinsic_i64_to_i32__(val: i64) -> Option<i32>;
// pub fn __intrinsic_i64_to_i128__(val: i64) -> i128;
// pub fn __intrinsic_i128_to_i8__(val: i128) -> Option<i8>;
// pub fn __intrinsic_i128_to_i16__(val: i128) -> Option<i16>;
// pub fn __intrinsic_i128_to_i32__(val: i128) -> Option<i32>;
// pub fn __intrinsic_i128_to_i64__(val: i128) -> Option<i64>;
// pub fn __intrinsic_i128_to_i128__(val: i128) -> Option<i128>;
// 
// // Unsigned to Unsigned
// pub fn __intrinsic_u8_to_u16__(val: u8) -> u16;
// pub fn __intrinsic_u8_to_u32__(val: u8) -> u32;
// pub fn __intrinsic_u8_to_u64__(val: u8) -> u64;
// pub fn __intrinsic_u8_to_u128__(val: u8) -> u128;
// pub fn __intrinsic_u16_to_u8__(val: u16) -> Option<u8>;
// pub fn __intrinsic_u16_to_u32__(val: u16) -> u32;
// pub fn __intrinsic_u16_to_u64__(val: u16) -> u64;
// pub fn __intrinsic_u16_to_u128__(val: u16) -> u128;
// pub fn __intrinsic_u32_to_u8__(val: u32) -> Option<u8>;
// pub fn __intrinsic_u32_to_u16__(val: u32) -> Option<u16>;
// pub fn __intrinsic_u32_to_u64__(val: u32) -> u64;
// pub fn __intrinsic_u32_to_u128__(val: u32) -> u128;
// pub fn __intrinsic_u64_to_u8__(val: u64) -> Option<u8>;
// pub fn __intrinsic_u64_to_u16__(val: u64) -> Option<u16>;
// pub fn __intrinsic_u64_to_u32__(val: u64) -> Option<u32>;
// pub fn __intrinsic_u64_to_u128__(val: u64) -> u128;
// pub fn __intrinsic_u128_to_u8__(val: u128) -> Option<u8>;
// pub fn __intrinsic_u128_to_u16__(val: u128) -> Option<u16>;
// pub fn __intrinsic_u128_to_u32__(val: u128) -> Option<u32>;
// pub fn __intrinsic_u128_to_u64__(val: u128) -> Option<u64>;
// 
// // Signed to Unsigned
// pub fn __intrinsic_i8_to_u8__(val: i8) -> Option<u8>;
// pub fn __intrinsic_i8_to_u16__(val: i8) -> Option<u16>;
// pub fn __intrinsic_i8_to_u32__(val: i8) -> Option<u32>;
// pub fn __intrinsic_i8_to_u64__(val: i8) -> Option<u64>;
// pub fn __intrinsic_i8_to_u128__(val: i8) -> Option<u128>;
// pub fn __intrinsic_i16_to_u8__(val: i16) -> Option<u8>;
// pub fn __intrinsic_i16_to_u16__(val: i16) -> Option<u16>;
// pub fn __intrinsic_i16_to_u32__(val: i16) -> Option<u32>;
// pub fn __intrinsic_i16_to_u64__(val: i16) -> Option<u64>;
// pub fn __intrinsic_i16_to_u128__(val: i16) -> Option<u128>;
// pub fn __intrinsic_i32_to_u8__(val: i32) -> Option<u8>;
// pub fn __intrinsic_i32_to_u16__(val: i32) -> Option<u16>;
// pub fn __intrinsic_i32_to_u32__(val: i32) -> Option<u32>;
// pub fn __intrinsic_i32_to_u64__(val: i32) -> Option<u64>;
// pub fn __intrinsic_i32_to_u128__(val: i32) -> Option<u128>;
// pub fn __intrinsic_i64_to_u8__(val: i64) -> Option<u8>;
// pub fn __intrinsic_i64_to_u16__(val: i64) -> Option<u16>;
// pub fn __intrinsic_i64_to_u32__(val: i64) -> Option<u32>;
// pub fn __intrinsic_i64_to_u64__(val: i64) -> Option<u64>;
// pub fn __intrinsic_i64_to_u128__(val: i64) -> Option<u128>;
// pub fn __intrinsic_i128_to_u8__(val: i128) -> Option<u8>;
// pub fn __intrinsic_i128_to_u16__(val: i128) -> Option<u16>;
// pub fn __intrinsic_i128_to_u32__(val: i128) -> Option<u32>;
// pub fn __intrinsic_i128_to_u64__(val: i128) -> Option<i64>;
// pub fn __intrinsic_i128_to_u128__(val: i128) -> Option<i128>;
// 
// // Float to Float
// pub fn __intrinsic_f32_to_f64__(val: f32) -> f64;
// pub fn __intrinsic_f64_to_f32__(val: f64) -> Option<f32>;
// 
// // Float to Integer
// pub fn __intrinsic_f32_to_i8__(val: f32) -> Option<i8>;
// pub fn __intrinsic_f32_to_i16__(val: f32) -> Option<i16>;
// pub fn __intrinsic_f32_to_i32__(val: f32) -> Option<i32>;
// pub fn __intrinsic_f32_to_i64__(val: f32) -> Option<i64>;
// pub fn __intrinsic_f32_to_i128__(val: f32) -> Option<i128>;
// pub fn __intrinsic_f64_to_i8__(val: f64) -> Option<i8>;
// pub fn __intrinsic_f64_to_i16__(val: f64) -> Option<i16>;
// pub fn __intrinsic_f64_to_i32__(val: f64) -> Option<i32>;
// pub fn __intrinsic_f64_to_i64__(val: f64) -> Option<i64>;
// pub fn __intrinsic_f64_to_i128__(val: f64) -> Option<i128>;
// pub fn __intrinsic_f32_to_u8__(val: f32) -> Option<u8>;
// pub fn __intrinsic_f32_to_u16__(val: f32) -> Option<u16>;
// pub fn __intrinsic_f32_to_u32__(val: f32) -> Option<u32>;
// pub fn __intrinsic_f32_to_u64__(val: f32) -> Option<u64>;
// pub fn __intrinsic_f32_to_u128__(val: f32) -> Option<u128>;
// pub fn __intrinsic_f64_to_u8__(val: f64) -> Option<u8>;
// pub fn __intrinsic_f64_to_u16__(val: f64) -> Option<u16>;
// pub fn __intrinsic_f64_to_u32__(val: f64) -> Option<u32>;
// pub fn __intrinsic_f64_to_u64__(val: f64) -> Option<u64>;
// pub fn __intrinsic_f64_to_u128__(val: f64) -> Option<u128>;
// 
// // Integer to Float
// pub fn __intrinsic_i8_to_f32__(val: i8) -> f32;
// pub fn __intrinsic_i16_to_f32__(val: i16) -> f32;
// pub fn __intrinsic_i32_to_f32__(val: i32) -> f32;
// pub fn __intrinsic_i64_to_f32__(val: i64) -> f32;
// pub fn __intrinsic_i128_to_f32__(val: i128) -> f32;
// pub fn __intrinsic_i8_to_f64__(val: i8) -> f64;
// pub fn __intrinsic_i16_to_f64__(val: i16) -> f64;
// pub fn __intrinsic_i32_to_f64__(val: i32) -> f64;
// pub fn __intrinsic_i64_to_f64__(val: i64) -> f64;
// pub fn __intrinsic_i128_to_f64__(val: i128) -> f64;
// pub fn __intrinsic_u8_to_f32__(val: u8) -> f32;
// pub fn __intrinsic_u16_to_f32__(val: u16) -> f32;
// pub fn __intrinsic_u32_to_f32__(val: u32) -> f32;
// pub fn __intrinsic_u64_to_f32__(val: u64) -> f32;
// pub fn __intrinsic_u128_to_f32__(val: u128) -> f32;
// pub fn __intrinsic_u8_to_f64__(val: u8) -> f64;
// pub fn __intrinsic_u16_to_f64__(val: u16) -> f64;
// pub fn __intrinsic_u32_to_f64__(val: u32) -> f64;
// pub fn __intrinsic_u64_to_f64__(val: u64) -> f64;
// pub fn __intrinsic_u128_to_f64__(val: u128) -> f64;