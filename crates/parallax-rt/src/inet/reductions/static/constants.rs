// Import encoding constants and functions from parallax-net
// This replaces the manually mirrored constants below.
pub(crate) use parallax_net::encoding::*;

// --- REMOVE OLD CONSTANTS --- 
// // --- Static Node Data Encoding Constants (mirrored from lowering/nodes.rs) --- 
// #![allow(dead_code)] // Allow unused constants for now
// 
// pub(super) const TAG_SHIFT: u32 = 60;
// pub(super) const DATA_MASK: u64 = (1u64 << TAG_SHIFT) - 1;
// 
// pub(crate) const TAG_NIL: u64 = 0;
// pub(crate) const TAG_FUNCTION: u64 = 1;
// // pub(super) const TAG_GLOBAL_VAR: u64 = 2; // Unused for now
// pub(crate) const TAG_INTRINSIC_OP: u64 = 6;
// pub(crate) const TAG_IS_VARIANT: u64 = 8;
// 
// // --- Intrinsic Operation Codes (mirrored from lowering/nodes.rs) ---
// // i64: 0-127
// pub(super) const OP_I64_ADD: u64 = 0;
// pub(super) const OP_I64_SUB: u64 = 1;
// // ... rest of old OP codes ...
// pub(super) const OP_PANIC: u64 = 2048; 