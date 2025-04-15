// Handles translation of HIR pattern matching.
use crate::NativeError;
use crate::translator::context::TranslationContext;
use crate::translator::operand::translate_literal;
use crate::translator::types::translate_type;
use crate::translator::layout::{LayoutComputer, get_enum_discriminant_type, get_enum_discriminant_offset_bytes};
use cranelift_codegen::ir::{Value, Block, InstBuilder, types, MemFlags};
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::isa::TargetIsa;
use cranelift_frontend::FunctionBuilder;
use cranelift_jit::JITModule;
use parallax_hir::hir::{HirPattern, HirType};
use std::sync::Arc;

/// Translates a pattern check.
/// Emits a conditional branch to `match_block` or `no_match_block`.
/// Returns the condition value (i1) indicating if the pattern matches.
/// Binds variables in the context if the pattern involves binding.
/// 
/// NOTE: This currently only handles simple patterns (Const, Wildcard, Bind).
/// Variant patterns only perform discriminant check.
pub fn translate_pattern_check<'ctx>(
    builder: &mut FunctionBuilder,
    ctx: &mut TranslationContext<'ctx>,
    pattern: &HirPattern,
    scrutinee_val: Value,
    _scrutinee_hir_ty: &HirType,
    jit_module: &mut JITModule,
    isa: &Arc<dyn TargetIsa>,
    layout_computer: &mut LayoutComputer,
    match_block: Block,
    no_match_block: Block,
) -> Result<Value, NativeError> {

    match pattern {
        HirPattern::Const(literal) => {
            let const_val = translate_literal(builder, literal, isa, jit_module, ctx)?;
            let scrutinee_cl_ty = builder.func.dfg.value_type(scrutinee_val);
            // Ensure types match for comparison
            if builder.func.dfg.value_type(const_val) != scrutinee_cl_ty {
                 return Err(NativeError::TypeError(format!("Type mismatch between scrutinee ({:?}) and const pattern ({:?})", scrutinee_cl_ty, literal)));
            }
            let condition = builder.ins().icmp(IntCC::Equal, scrutinee_val, const_val);
            builder.ins().brif(condition, match_block, &[], no_match_block, &[]);
            Ok(condition)
        }
        HirPattern::Wildcard => {
            // Wildcard always matches
            builder.ins().jump(match_block, &[]);
            let true_cond = builder.ins().iconst(types::I8, 1); // Condition is always true
            Ok(true_cond)
        }
        HirPattern::Bind { var, var_ty } => {
            let scrutinee_cl_ty = builder.func.dfg.value_type(scrutinee_val);
            // Bind always matches, add the variable binding
            // Ensure type matches
            if let Some(cl_ty) = translate_type(var_ty, isa.as_ref(), ctx, layout_computer)? {
                 if cl_ty != scrutinee_cl_ty {
                     return Err(NativeError::TypeError(format!("Type mismatch between scrutinee ({:?}) and bind pattern var {:?} ({:?})", scrutinee_cl_ty, var, var_ty)));
                 }
                ctx.add_var_binding(*var, scrutinee_val, var_ty.clone());
            } else {
                 // Binding a ZST
                 ctx.add_var_type(*var, var_ty.clone());
            }
            builder.ins().jump(match_block, &[]);
            let true_cond = builder.ins().iconst(types::I8, 1); // Condition is always true
            Ok(true_cond)
        }
        HirPattern::Variant { variant_symbol, bindings: _ } => {
            // --- Discriminant check --- 
             let (enum_def, _variant_def) = ctx.get_enum_and_variant_def(*variant_symbol)
                 .ok_or_else(|| NativeError::TypeError(format!("Variant symbol {:?} not found for match pattern", variant_symbol)))?;
             let enum_symbol = enum_def.symbol;
             let expected_discriminant = enum_def.variants.iter().position(|v| v.symbol == *variant_symbol)
                 .ok_or_else(|| NativeError::TypeError("Variant symbol not found within its own enum definition?".to_string()))?;
             let discr_cl_ty = get_enum_discriminant_type(enum_symbol, layout_computer, ctx)?;
             let discr_offset = get_enum_discriminant_offset_bytes(enum_symbol, layout_computer, ctx)?;
             // --- IMPLEMENT TODO: Memflags for discriminant load --- 
             let loaded_discr = builder.ins().load(discr_cl_ty, MemFlags::trusted(), scrutinee_val, discr_offset as i32);
             // --- End IMPLEMENT TODO --- 
             let expected_discr_val = builder.ins().iconst(discr_cl_ty, expected_discriminant as i64);
             let condition = builder.ins().icmp(IntCC::Equal, loaded_discr, expected_discr_val);
             builder.ins().brif(condition, match_block, &[], no_match_block, &[]);
            // --- End Discriminant check --- 

            // TODO: Implement binding of fields within the variant in the match_block if condition is true.
            // This requires loading fields from scrutinee_val + payload_offset + field_offset.

            Ok(condition)
         }
    }
}

// Original translate_pattern function - can be removed or adapted
/*
pub fn translate_pattern(
    builder: &mut FunctionBuilder,
    ctx: &mut TranslationContext,
    pattern: &HirPattern,
    scrutinee_val: Value,
    jit_module: &mut JITModule,
    isa: &Arc<dyn TargetIsa>,
    layout_computer: &mut LayoutComputer,
) -> Result<(), NativeError> {
    // This function's purpose is less clear now. 
    // The logic is mostly in translate_pattern_check.
    // It might be used *after* a check succeeds to perform bindings?
    Err(NativeError::Unimplemented("translate_pattern role needs review after refactor".to_string()))
}
*/ 