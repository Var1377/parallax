use crate::NativeError;
use crate::translator::context::{TranslationContext, KnownFunction};
use cranelift_codegen::ir::{Value, InstBuilder, types};
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::isa::TargetIsa;
use cranelift_frontend::FunctionBuilder;
use cranelift_jit::JITModule;
use std::sync::Arc;

/// Translates a call to an intrinsic function directly into Cranelift IR.
pub fn translate_intrinsic_call<'ctx>(
    builder: &mut FunctionBuilder,
    ctx: &TranslationContext<'ctx>,
    intrinsic_info: &KnownFunction, // Contains name, param_types, return_type
    arg_vals: &[Value],
    jit_module: &mut JITModule,
    isa: &Arc<dyn TargetIsa>,
) -> Result<Value, NativeError> {

    match intrinsic_info.name.as_str() {
        // Boolean intrinsics
        "__intrinsic_bool_and__" => {
            if arg_vals.len() != 2 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 2 arguments, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            
            // Type check arguments
            let lhs = arg_vals[0];
            let rhs = arg_vals[1];
            let lhs_ty = builder.func.dfg.value_type(lhs);
            let rhs_ty = builder.func.dfg.value_type(rhs);
            
            // Bool values in Cranelift are represented as i8 with 0=false, 1=true
            if lhs_ty != types::I8 || rhs_ty != types::I8 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected bool arguments, got {:?} and {:?}",
                    intrinsic_info.name, lhs_ty, rhs_ty
                )));
            }
            
            // Implement boolean AND using band (bitwise AND)
            let result = builder.ins().band(lhs, rhs);
            Ok(result)
        },
        
        "__intrinsic_bool_or__" => {
            if arg_vals.len() != 2 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 2 arguments, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            
            // Type check arguments
            let lhs = arg_vals[0];
            let rhs = arg_vals[1];
            let lhs_ty = builder.func.dfg.value_type(lhs);
            let rhs_ty = builder.func.dfg.value_type(rhs);
            
            if lhs_ty != types::I8 || rhs_ty != types::I8 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected bool arguments, got {:?} and {:?}",
                    intrinsic_info.name, lhs_ty, rhs_ty
                )));
            }
            
            // Implement boolean OR using bor (bitwise OR)
            let result = builder.ins().bor(lhs, rhs);
            Ok(result)
        },
        
        "__intrinsic_bool_xor__" => {
            if arg_vals.len() != 2 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 2 arguments, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            
            // Type check arguments
            let lhs = arg_vals[0];
            let rhs = arg_vals[1];
            let lhs_ty = builder.func.dfg.value_type(lhs);
            let rhs_ty = builder.func.dfg.value_type(rhs);
            
            if lhs_ty != types::I8 || rhs_ty != types::I8 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected bool arguments, got {:?} and {:?}",
                    intrinsic_info.name, lhs_ty, rhs_ty
                )));
            }
            
            // Implement boolean XOR using bxor (bitwise XOR)
            let result = builder.ins().bxor(lhs, rhs);
            Ok(result)
        },
        
        "__intrinsic_bool_not__" => {
            if arg_vals.len() != 1 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 1 argument, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            
            // Type check argument
            let val = arg_vals[0];
            let val_ty = builder.func.dfg.value_type(val);
            
            if val_ty != types::I8 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected bool argument, got {:?}",
                    intrinsic_info.name, val_ty
                )));
            }
            
            // Implement boolean NOT using integer 1 and subtraction
            // NOT x = 1 - x (for boolean x that's 0 or 1)
            let one = builder.ins().iconst(types::I8, 1);
            let result = builder.ins().isub(one, val);
            Ok(result)
        },
        
        "__intrinsic_bool_eq__" => {
            if arg_vals.len() != 2 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 2 arguments, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            
            // Type check arguments
            let lhs = arg_vals[0];
            let rhs = arg_vals[1];
            let lhs_ty = builder.func.dfg.value_type(lhs);
            let rhs_ty = builder.func.dfg.value_type(rhs);
            
            if lhs_ty != types::I8 || rhs_ty != types::I8 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected bool arguments, got {:?} and {:?}",
                    intrinsic_info.name, lhs_ty, rhs_ty
                )));
            }
            
            // Compare for equality using integer comparison
            let cmp_result = builder.ins().icmp(IntCC::Equal, lhs, rhs);
            let true_val = builder.ins().iconst(types::I8, 1);
            let false_val = builder.ins().iconst(types::I8, 0);
            let result = builder.ins().select(cmp_result, true_val, false_val);
            Ok(result)
        },
        
        "__intrinsic_bool_ne__" => {
            if arg_vals.len() != 2 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 2 arguments, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            
            // Type check arguments
            let lhs = arg_vals[0];
            let rhs = arg_vals[1];
            let lhs_ty = builder.func.dfg.value_type(lhs);
            let rhs_ty = builder.func.dfg.value_type(rhs);
            
            if lhs_ty != types::I8 || rhs_ty != types::I8 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected bool arguments, got {:?} and {:?}",
                    intrinsic_info.name, lhs_ty, rhs_ty
                )));
            }
            
            // Compare for inequality using integer comparison
            let cmp_result = builder.ins().icmp(IntCC::NotEqual, lhs, rhs);
            let true_val = builder.ins().iconst(types::I8, 1);
            let false_val = builder.ins().iconst(types::I8, 0);
            let result = builder.ins().select(cmp_result, true_val, false_val);
            Ok(result)
        },

        // i32 intrinsics
        "__intrinsic_i32_add__" => {
            if arg_vals.len() != 2 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 2 arguments, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            // Type check arguments
            let lhs = arg_vals[0];
            let rhs = arg_vals[1];
            let lhs_ty = builder.func.dfg.value_type(lhs);
            let rhs_ty = builder.func.dfg.value_type(rhs);
            if lhs_ty != types::I32 || rhs_ty != types::I32 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected i32 arguments, got {:?} and {:?}",
                    intrinsic_info.name, lhs_ty, rhs_ty
                )));
            }
            let result = builder.ins().iadd(lhs, rhs);
            Ok(result)
        },
        "__intrinsic_i32_sub__" => {
            if arg_vals.len() != 2 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 2 arguments, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            // Type check arguments
            let lhs = arg_vals[0];
            let rhs = arg_vals[1];
            let lhs_ty = builder.func.dfg.value_type(lhs);
            let rhs_ty = builder.func.dfg.value_type(rhs);
            if lhs_ty != types::I32 || rhs_ty != types::I32 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected i32 arguments, got {:?} and {:?}",
                    intrinsic_info.name, lhs_ty, rhs_ty
                )));
            }
            let result = builder.ins().isub(lhs, rhs);
            Ok(result)
        },
        "__intrinsic_i32_mul__" => {
            if arg_vals.len() != 2 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 2 arguments, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            // Type check arguments
            let lhs = arg_vals[0];
            let rhs = arg_vals[1];
            let lhs_ty = builder.func.dfg.value_type(lhs);
            let rhs_ty = builder.func.dfg.value_type(rhs);
            if lhs_ty != types::I32 || rhs_ty != types::I32 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected i32 arguments, got {:?} and {:?}",
                    intrinsic_info.name, lhs_ty, rhs_ty
                )));
            }
            let result = builder.ins().imul(lhs, rhs);
            Ok(result)
        },
        "__intrinsic_i32_div__" => {
            if arg_vals.len() != 2 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 2 arguments, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            // Type check arguments
            let lhs = arg_vals[0];
            let rhs = arg_vals[1];
            let lhs_ty = builder.func.dfg.value_type(lhs);
            let rhs_ty = builder.func.dfg.value_type(rhs);
            if lhs_ty != types::I32 || rhs_ty != types::I32 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected i32 arguments, got {:?} and {:?}",
                    intrinsic_info.name, lhs_ty, rhs_ty
                )));
            }
            // Note: This performs a signed division (sdiv)
            let result = builder.ins().sdiv(lhs, rhs);
            Ok(result)
        },
        "__intrinsic_i32_rem__" => {
            if arg_vals.len() != 2 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 2 arguments, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            // Type check arguments
            let lhs = arg_vals[0];
            let rhs = arg_vals[1];
            let lhs_ty = builder.func.dfg.value_type(lhs);
            let rhs_ty = builder.func.dfg.value_type(rhs);
            if lhs_ty != types::I32 || rhs_ty != types::I32 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected i32 arguments, got {:?} and {:?}",
                    intrinsic_info.name, lhs_ty, rhs_ty
                )));
            }
            // Note: This performs a signed remainder (srem)
            let result = builder.ins().srem(lhs, rhs);
            Ok(result)
        },
        "__intrinsic_i32_and__" => {
            if arg_vals.len() != 2 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 2 arguments, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            // Type check arguments
            let lhs = arg_vals[0];
            let rhs = arg_vals[1];
            let lhs_ty = builder.func.dfg.value_type(lhs);
            let rhs_ty = builder.func.dfg.value_type(rhs);
            if lhs_ty != types::I32 || rhs_ty != types::I32 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected i32 arguments, got {:?} and {:?}",
                    intrinsic_info.name, lhs_ty, rhs_ty
                )));
            }
            let result = builder.ins().band(lhs, rhs);
            Ok(result)
        },
        "__intrinsic_i32_or__" => {
            if arg_vals.len() != 2 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 2 arguments, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            // Type check arguments
            let lhs = arg_vals[0];
            let rhs = arg_vals[1];
            let lhs_ty = builder.func.dfg.value_type(lhs);
            let rhs_ty = builder.func.dfg.value_type(rhs);
            if lhs_ty != types::I32 || rhs_ty != types::I32 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected i32 arguments, got {:?} and {:?}",
                    intrinsic_info.name, lhs_ty, rhs_ty
                )));
            }
            let result = builder.ins().bor(lhs, rhs);
            Ok(result)
        },
        "__intrinsic_i32_xor__" => {
            if arg_vals.len() != 2 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 2 arguments, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            // Type check arguments
            let lhs = arg_vals[0];
            let rhs = arg_vals[1];
            let lhs_ty = builder.func.dfg.value_type(lhs);
            let rhs_ty = builder.func.dfg.value_type(rhs);
            if lhs_ty != types::I32 || rhs_ty != types::I32 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected i32 arguments, got {:?} and {:?}",
                    intrinsic_info.name, lhs_ty, rhs_ty
                )));
            }
            let result = builder.ins().bxor(lhs, rhs);
            Ok(result)
        },
        "__intrinsic_i32_shl__" => {
            if arg_vals.len() != 2 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 2 arguments, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            // Type check arguments
            let lhs = arg_vals[0];
            let rhs = arg_vals[1];
            let lhs_ty = builder.func.dfg.value_type(lhs);
            let rhs_ty = builder.func.dfg.value_type(rhs);
            if lhs_ty != types::I32 || rhs_ty != types::I32 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected i32 arguments, got {:?} and {:?}",
                    intrinsic_info.name, lhs_ty, rhs_ty
                )));
            }
            let result = builder.ins().ishl(lhs, rhs);
            Ok(result)
        },
        "__intrinsic_i32_shr__" => {
            if arg_vals.len() != 2 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 2 arguments, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            // Type check arguments
            let lhs = arg_vals[0];
            let rhs = arg_vals[1];
            let lhs_ty = builder.func.dfg.value_type(lhs);
            let rhs_ty = builder.func.dfg.value_type(rhs);
            if lhs_ty != types::I32 || rhs_ty != types::I32 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected i32 arguments, got {:?} and {:?}",
                    intrinsic_info.name, lhs_ty, rhs_ty
                )));
            }
            // For signed integers, use arithmetic shift right (sshr)
            let result = builder.ins().sshr(lhs, rhs);
            Ok(result)
        },
        "__intrinsic_i32_neg__" => {
            if arg_vals.len() != 1 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 1 argument, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            // Type check argument
            let val = arg_vals[0];
            let val_ty = builder.func.dfg.value_type(val);
            if val_ty != types::I32 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected i32 argument, got {:?}",
                    intrinsic_info.name, val_ty
                )));
            }
            let result = builder.ins().ineg(val);
            Ok(result)
        },
        "__intrinsic_i32_not__" => {
            if arg_vals.len() != 1 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 1 argument, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            // Type check argument
            let val = arg_vals[0];
            let val_ty = builder.func.dfg.value_type(val);
            if val_ty != types::I32 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected i32 argument, got {:?}",
                    intrinsic_info.name, val_ty
                )));
            }
            let result = builder.ins().bnot(val);
            Ok(result)
        },
        "__intrinsic_i32_abs__" => {
            if arg_vals.len() != 1 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 1 argument, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            // Type check argument
            let val = arg_vals[0];
            let val_ty = builder.func.dfg.value_type(val);
            if val_ty != types::I32 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected i32 argument, got {:?}",
                    intrinsic_info.name, val_ty
                )));
            }
            
            // Create abs implementation: return val < 0 ? -val : val
            let zero = builder.ins().iconst(types::I32, 0);
            let is_negative = builder.ins().icmp(IntCC::SignedLessThan, val, zero);
            let neg_val = builder.ins().ineg(val);
            let result = builder.ins().select(is_negative, neg_val, val);
            Ok(result)
        },
        "__intrinsic_i32_pow__" => {
            if arg_vals.len() != 2 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 2 arguments, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            // Type check arguments
            let base = arg_vals[0];
            let exp = arg_vals[1];
            let base_ty = builder.func.dfg.value_type(base);
            let exp_ty = builder.func.dfg.value_type(exp);
            if base_ty != types::I32 || exp_ty != types::I32 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected i32 arguments, got {:?} and {:?}",
                    intrinsic_info.name, base_ty, exp_ty
                )));
            }
            
            // For pow, we would typically implement this as a loop or function call
            // Since Cranelift doesn't have a direct pow instruction, we'll return an unimplemented error for now
            // A proper implementation would require creating a loop or function call
            return Err(NativeError::Unimplemented(format!(
                "Intrinsic function {} requires custom implementation with a loop",
                intrinsic_info.name
            )));
        },
        "__intrinsic_i32_eq__" => {
            if arg_vals.len() != 2 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 2 arguments, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            // Type check arguments
            let lhs = arg_vals[0];
            let rhs = arg_vals[1];
            let lhs_ty = builder.func.dfg.value_type(lhs);
            let rhs_ty = builder.func.dfg.value_type(rhs);
            if lhs_ty != types::I32 || rhs_ty != types::I32 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected i32 arguments, got {:?} and {:?}",
                    intrinsic_info.name, lhs_ty, rhs_ty
                )));
            }
            // Compare for equality and return a bool (as i8)
            let cmp_result = builder.ins().icmp(IntCC::Equal, lhs, rhs);
            let true_val = builder.ins().iconst(types::I8, 1);
            let false_val = builder.ins().iconst(types::I8, 0);
            let result = builder.ins().select(cmp_result, true_val, false_val);
            Ok(result)
        },
        "__intrinsic_i32_ne__" => {
            if arg_vals.len() != 2 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 2 arguments, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            // Type check arguments
            let lhs = arg_vals[0];
            let rhs = arg_vals[1];
            let lhs_ty = builder.func.dfg.value_type(lhs);
            let rhs_ty = builder.func.dfg.value_type(rhs);
            if lhs_ty != types::I32 || rhs_ty != types::I32 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected i32 arguments, got {:?} and {:?}",
                    intrinsic_info.name, lhs_ty, rhs_ty
                )));
            }
            // Compare for inequality and return a bool (as i8)
            let cmp_result = builder.ins().icmp(IntCC::NotEqual, lhs, rhs);
            let true_val = builder.ins().iconst(types::I8, 1);
            let false_val = builder.ins().iconst(types::I8, 0);
            let result = builder.ins().select(cmp_result, true_val, false_val);
            Ok(result)
        },
        "__intrinsic_i32_lt__" => {
            if arg_vals.len() != 2 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 2 arguments, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            // Type check arguments
            let lhs = arg_vals[0];
            let rhs = arg_vals[1];
            let lhs_ty = builder.func.dfg.value_type(lhs);
            let rhs_ty = builder.func.dfg.value_type(rhs);
            if lhs_ty != types::I32 || rhs_ty != types::I32 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected i32 arguments, got {:?} and {:?}",
                    intrinsic_info.name, lhs_ty, rhs_ty
                )));
            }
            // Compare for less than and return a bool (as i8)
            let cmp_result = builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs);
            let true_val = builder.ins().iconst(types::I8, 1);
            let false_val = builder.ins().iconst(types::I8, 0);
            let result = builder.ins().select(cmp_result, true_val, false_val);
            Ok(result)
        },
        "__intrinsic_i32_le__" => {
            if arg_vals.len() != 2 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 2 arguments, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            // Type check arguments
            let lhs = arg_vals[0];
            let rhs = arg_vals[1];
            let lhs_ty = builder.func.dfg.value_type(lhs);
            let rhs_ty = builder.func.dfg.value_type(rhs);
            if lhs_ty != types::I32 || rhs_ty != types::I32 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected i32 arguments, got {:?} and {:?}",
                    intrinsic_info.name, lhs_ty, rhs_ty
                )));
            }
            // Compare for less than or equal and return a bool (as i8)
            let cmp_result = builder.ins().icmp(IntCC::SignedLessThanOrEqual, lhs, rhs);
            let true_val = builder.ins().iconst(types::I8, 1);
            let false_val = builder.ins().iconst(types::I8, 0);
            let result = builder.ins().select(cmp_result, true_val, false_val);
            Ok(result)
        },
        "__intrinsic_i32_gt__" => {
            if arg_vals.len() != 2 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 2 arguments, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            // Type check arguments
            let lhs = arg_vals[0];
            let rhs = arg_vals[1];
            let lhs_ty = builder.func.dfg.value_type(lhs);
            let rhs_ty = builder.func.dfg.value_type(rhs);
            if lhs_ty != types::I32 || rhs_ty != types::I32 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected i32 arguments, got {:?} and {:?}",
                    intrinsic_info.name, lhs_ty, rhs_ty
                )));
            }
            // Compare for greater than and return a bool (as i8)
            let cmp_result = builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs);
            let true_val = builder.ins().iconst(types::I8, 1);
            let false_val = builder.ins().iconst(types::I8, 0);
            let result = builder.ins().select(cmp_result, true_val, false_val);
            Ok(result)
        },
        "__intrinsic_i32_ge__" => {
            if arg_vals.len() != 2 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected 2 arguments, got {}",
                    intrinsic_info.name, arg_vals.len()
                )));
            }
            // Type check arguments
            let lhs = arg_vals[0];
            let rhs = arg_vals[1];
            let lhs_ty = builder.func.dfg.value_type(lhs);
            let rhs_ty = builder.func.dfg.value_type(rhs);
            if lhs_ty != types::I32 || rhs_ty != types::I32 {
                return Err(NativeError::TypeError(format!(
                    "Intrinsic {} expected i32 arguments, got {:?} and {:?}",
                    intrinsic_info.name, lhs_ty, rhs_ty
                )));
            }
            // Compare for greater than or equal and return a bool (as i8)
            let cmp_result = builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs);
            let true_val = builder.ins().iconst(types::I8, 1);
            let false_val = builder.ins().iconst(types::I8, 0);
            let result = builder.ins().select(cmp_result, true_val, false_val);
            Ok(result)
        },

        // Default: Unimplemented
        _ => Err(NativeError::Unimplemented(format!(
            "Intrinsic function {} is not yet implemented",
            intrinsic_info.name
        ))),
    }
} 