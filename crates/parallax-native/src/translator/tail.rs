// Handles translation of HIR tail expressions (Return, If, Match, Never).
use crate::NativeError;
use crate::translator::context::TranslationContext;
use crate::translator::operand::translate_operand;
use crate::translator::expr::translate_expr; // Need this for recursive calls in arms/branches
use crate::translator::pattern::translate_pattern_check; // Use the new check function
use crate::translator::types::translate_type;
use crate::translator::helpers::declare_shadow_stack_pop_fn;
use cranelift_codegen::ir::{Value, InstBuilder, types, MemFlags, Block, TrapCode};
use cranelift_codegen::isa::TargetIsa;
use cranelift_frontend::FunctionBuilder;
use cranelift_jit::JITModule;
use parallax_hir::hir::{HirTailExpr, HirExpr, Operand, HirType, PrimitiveType}; // Use PrimitiveType
use parallax_hir::Symbol; // Import Symbol
use std::sync::Arc;
use std::collections::HashSet;
use cranelift_codegen::ir::condcodes::IntCC;

pub fn translate_tail_expr<'ctx>(
    builder: &mut FunctionBuilder,
    ctx: &mut TranslationContext<'ctx>,
    tail_expr: &HirTailExpr,
    jit_module: &mut JITModule,
    isa: &Arc<dyn TargetIsa>,
) -> Result<Value, NativeError> {
    match tail_expr {
        HirTailExpr::Value(operand) => {
            // Translate the operand; this is the resulting value of the Tail expression.
            let hir_type = ctx.get_operand_type(operand);
            // Pass descriptor state if needed
            translate_operand(builder, ctx, operand, hir_type.as_ref(), jit_module, isa)
        }
        HirTailExpr::If { condition, then_branch, else_branch } => {
            // Delegate to the dedicated if-tail translator, passing state
            translate_if_tail(builder, ctx, condition, then_branch, else_branch, jit_module, isa)
        }
        HirTailExpr::Match { scrutinee, arms, otherwise } => {
            // Get scrutinee value and type using translate_operand
            let scrutinee_type = ctx.get_operand_type(scrutinee)
                .ok_or_else(|| NativeError::TypeError("Cannot determine type of match scrutinee".to_string()))?;
            let scrutinee_val = translate_operand(builder, ctx, scrutinee, Some(&scrutinee_type), jit_module, isa)?;

            // 2. Setup Blocks
            let merge_block = builder.create_block();
            let otherwise_block = builder.create_block();
            
            // Track if we've seen a pattern that always succeeds (wildcard or binding)
            let mut has_exhaustive_pattern = false;
            
            // Variable to keep track of which blocks need sealing
            let mut blocks_to_seal = Vec::new();

            // 3. Map result_type to Cranelift type for merge block's parameter
            // Assume the type of the first arm's expression is the result type
            let result_hir_ty = arms.first().map(|(_, expr)| &expr.ty).unwrap_or_else(|| {
                // If no arms, use the otherwise type, or Unit if no otherwise
                otherwise.as_ref().map_or(&HirType::Primitive(PrimitiveType::Unit), |expr| &expr.ty)
            });
            
            let result_cl_ty = translate_type(result_hir_ty.clone(), isa.as_ref(), ctx)?.unwrap_or(types::I8); // Default to I8 for ZST
            let merge_param = builder.append_block_param(merge_block, result_cl_ty);

            // Keep track of blocks needing sealing
            blocks_to_seal.push(merge_block);
            blocks_to_seal.push(otherwise_block);

            // 4. Process each match arm
            for (i, (pattern, expr)) in arms.iter().enumerate() {
                let arm_block = builder.create_block();
                blocks_to_seal.push(arm_block);

                // 4.1 Emit pattern test
                let is_last_arm = i == arms.len() - 1;
                let next_test_block = if is_last_arm { otherwise_block } else { builder.create_block() };
                
                if !is_last_arm {
                    blocks_to_seal.push(next_test_block);
                }

                // Generate branch based on pattern match
                translate_pattern_check(
                    builder,
                    ctx,
                    pattern,
                    scrutinee_val,
                    &scrutinee_type,
                    jit_module,
                    isa,
                    arm_block,
                    next_test_block,
                )?;

                // Check if this pattern would always match (wildcard or binding)
                has_exhaustive_pattern = has_exhaustive_pattern || matches!(pattern, &parallax_hir::hir::HirPattern::Wildcard | &parallax_hir::hir::HirPattern::Bind { .. });

                // 4.2 Emit arm block (the code that runs when pattern matches)
                builder.switch_to_block(arm_block);
                
                // Translate the arm's expression and jump to merge with the result
                let arm_result = translate_expr(builder, ctx, expr, jit_module, isa)?;
                
                // --- Pop Shadow Stack Before Non-Tail Return (Jump to Merge) --- 
                let pointer_type = isa.pointer_type();
                let count = ctx.shadow_stack_push_count();
                if count > 0 {
                    let pop_fn_ref = declare_shadow_stack_pop_fn(jit_module, builder.func, pointer_type)?;
                    let count_val = builder.ins().iconst(pointer_type, count as i64);
                    builder.ins().call(pop_fn_ref, &[count_val]);
                    // Since this jump simulates a return, reset the counter conceptually for this path.
                    // Although ctx is function-wide, this prevents miscounting if merge leads elsewhere.
                    // Consider if a more robust stack management per block is needed later.
                    // ctx.reset_shadow_stack_push_count(); 
                    // ^^^ Let's not reset here, func.rs handles final return. 
                }
                // ------------------------------------------------------------- //
                
                builder.ins().jump(merge_block, &[arm_result]);

                // Switch to the next test block for the next pattern (unless this was the last pattern)
                if !is_last_arm {
                    builder.switch_to_block(next_test_block);
                }
            }

            // 5. Handle otherwise block (pattern match failure) - should only happen for non-exhaustive matches
            builder.switch_to_block(otherwise_block);
            
            if let Some(otherwise_expr) = otherwise {
                // Translate the otherwise expression
                let otherwise_result = translate_expr(builder, ctx, otherwise_expr, jit_module, isa)?;
                
                // --- Pop Shadow Stack Before Non-Tail Return (Jump to Merge) --- 
                let pointer_type = isa.pointer_type();
                let count = ctx.shadow_stack_push_count();
                if count > 0 {
                    let pop_fn_ref = declare_shadow_stack_pop_fn(jit_module, builder.func, pointer_type)?;
                    let count_val = builder.ins().iconst(pointer_type, count as i64);
                    builder.ins().call(pop_fn_ref, &[count_val]);
                    // ctx.reset_shadow_stack_push_count(); // See comment in Match arm
                }
                // ------------------------------------------------------------- //
                
                builder.ins().jump(merge_block, &[otherwise_result]);
            } else if !has_exhaustive_pattern {
                // Emit trap (this will be hit if no patterns match)
                builder.ins().trap(TrapCode::unwrap_user(0));
            } else {
                // Emit unreachable path (shouldn't happen, but avoid unterminated block)
                // This path is a safeguard for the translator, but should never execute at runtime
                let zero_val = builder.ins().iconst(result_cl_ty, 0);
                builder.ins().jump(merge_block, &[zero_val]);
            }

            // 6. Seal all blocks and finalize
            builder.switch_to_block(merge_block);
            
            for block in blocks_to_seal {
                builder.seal_block(block);
            }

            // Return the merge block parameter as the result of the match
            Ok(merge_param)
        }
        HirTailExpr::Never => {
            // --- Pop Shadow Stack Before Unreachable --- //
            let pointer_type = isa.pointer_type();
            let pop_fn_ref = declare_shadow_stack_pop_fn(jit_module, builder.func, pointer_type)?;
            let count = ctx.shadow_stack_push_count();
            if count > 0 {
                let count_val = builder.ins().iconst(pointer_type, count as i64);
                builder.ins().call(pop_fn_ref, &[count_val]);
            }
            // ------------------------------------- //

            // Mark block as unreachable
            builder.ins().trap(TrapCode::unwrap_user(0)); // User trap 0 for Never
            Ok(builder.ins().iconst(types::I8, 0)) // Return dummy
        }
    }
}

pub fn translate_if_tail<'ctx>(
    builder: &mut FunctionBuilder,
    ctx: &mut TranslationContext<'ctx>,
    condition: &Operand,
    then_expr: &HirExpr,
    else_expr: &HirExpr,
    jit_module: &mut JITModule,
    isa: &Arc<dyn TargetIsa>,
) -> Result<Value, NativeError> {
    let cond_val = translate_operand(
        builder,
        ctx,
        condition,
        Some(&HirType::Primitive(PrimitiveType::Bool)),
        jit_module,
        isa,
    )?;

    let then_block = builder.create_block();
    let else_block = builder.create_block();
    let merge_block = builder.create_block();

    // Create conditional branch
    builder.ins().brif(cond_val, then_block, &[], else_block, &[]);

    // Translate the then expression
    builder.switch_to_block(then_block);
    
    // --- Pop Shadow Stack Before Non-Tail Return (Jump to Merge) --- 
    let pointer_type = isa.pointer_type();
    let count_then = ctx.shadow_stack_push_count();
    if count_then > 0 {
        let pop_fn_ref = declare_shadow_stack_pop_fn(jit_module, builder.func, pointer_type)?;
        let count_val = builder.ins().iconst(pointer_type, count_then as i64);
        builder.ins().call(pop_fn_ref, &[count_val]);
        // ctx.reset_shadow_stack_push_count(); // See comment in Match arm
    }
    // ------------------------------------------------------------- //

    // Translate the then expression
    builder.switch_to_block(then_block);
    
    let then_result = translate_expr(builder, ctx, then_expr, jit_module, isa)?;

    // Create a jump to the merge block with the then result
    builder.ins().jump(merge_block, &[then_result]);

    // Translate the else expression
    builder.switch_to_block(else_block);
    
    // --- Pop Shadow Stack Before Non-Tail Return (Jump to Merge) --- 
    let pointer_type_else = isa.pointer_type(); // Redefine for borrow checker maybe?
    let count_else = ctx.shadow_stack_push_count();
    if count_else > 0 {
        let pop_fn_ref = declare_shadow_stack_pop_fn(jit_module, builder.func, pointer_type_else)?;
        let count_val = builder.ins().iconst(pointer_type_else, count_else as i64);
        builder.ins().call(pop_fn_ref, &[count_val]);
        // ctx.reset_shadow_stack_push_count(); // See comment in Match arm
    }
    // ------------------------------------------------------------- //

    // Translate the else expression
    builder.switch_to_block(else_block);
    
    let else_result = translate_expr(builder, ctx, else_expr, jit_module, isa)?;

    // Create a jump to the merge block with the else result
    builder.ins().jump(merge_block, &[else_result]);

    // Switch to the merge block and get the result
    builder.switch_to_block(merge_block);
    
    // Add a block parameter to the merge block for the result
    let result_type = builder.func.dfg.value_type(then_result);
    // We can use the same type for both results since typechecking ensures
    // that both branches return the same type
    let result = builder.append_block_param(merge_block, result_type);
    
    // Seal and finalize
    builder.seal_block(then_block);
    builder.seal_block(else_block);
    builder.seal_block(merge_block);
    
    Ok(result)
}

// Potential function for tail call logic (TCO or regular call)
// NOTE: This is the old stub from expr.rs, kept for reference but unused.
/*
fn translate_tail_call<'ctx>(
    builder: &mut FunctionBuilder,
    ctx: &mut TranslationContext<'ctx>,
    callee: &Operand,
    args: &[Operand], // Changed from &[HirExpr]
    param_types: &Vec<HirType>, // Added: Static param types
    ret_type: &HirType,       // Added: Static return type
    jit_module: &mut JITModule,
    isa: &Arc<dyn TargetIsa>,
    layout_computer: &mut LayoutComputer,
) -> Result<Value, NativeError> {
    // TODO: If this function is repurposed for TCO, replace call_indirect with return_call_indirect
    // For now, just do a regular call and return its result.

    // 1. Translate Arguments (Operands)
    let mut arg_vals = Vec::with_capacity(args.len());
    for (arg_op, arg_hir_ty) in args.iter().zip(param_types.iter()) { // Assuming args len == param_types len
        let arg_val = translate_operand(builder, ctx, arg_op, Some(arg_hir_ty), jit_module, isa, layout_computer)?;
        // Skip ZSTs
        if let Some(_) = translate_type(arg_hir_ty, isa.as_ref(), ctx, layout_computer)? {
             arg_vals.push(arg_val);
        }
    }
    
    // Translate Callee
    let callee_val = translate_operand(builder, ctx, callee, Some(&HirType::FunctionPointer(param_types.clone(), Arc::new(ret_type.clone()))), jit_module, isa, layout_computer)?;

    // --- Check for Tagged Pointer (Closure Call) ---
    let pointer_type = isa.pointer_type();
    let tag_mask = builder.ins().iconst(pointer_type, 1); // LSB tag
    let tag = builder.ins().band(callee_val, tag_mask);
    let is_tagged = builder.ins().icmp_imm(cranelift_codegen::ir::condcodes::IntCC::NotEqual, tag, 0);

    let closure_call_block = builder.create_block();
    let direct_call_block = builder.create_block();
    let merge_call_block = builder.create_block();

    // Determine result type for merge block parameter using static ret_type
    let result_cl_ty = translate_type(ret_type, isa.as_ref(), ctx, layout_computer)?
        .unwrap_or(pointer_type); // Placeholder for ZST
    builder.append_block_param(merge_call_block, result_cl_ty);

    builder.ins().brif(is_tagged, closure_call_block, &[], direct_call_block, &[]);

    // --- Closure Call Path ---
    builder.switch_to_block(closure_call_block);
    {
        // Untag the pointer
        let untagged_callee = builder.ins().iadd_imm(callee_val, -1); // Subtract 1 to untag
        // Load func_ptr and env_handle
        let env_handle_ptr = untagged_callee; 
        let env_handle = builder.ins().load(pointer_type, MemFlags::trusted(), env_handle_ptr, 0); 
        let fn_ptr_offset = builder.ins().iconst(pointer_type, pointer_type.bytes() as i64);
        let fn_ptr_addr = builder.ins().iadd(untagged_callee, fn_ptr_offset);
        let fn_ptr_val = builder.ins().load(pointer_type, MemFlags::trusted(), fn_ptr_addr, 0); 

        // Prepare arguments for closure call convention (env + original non-ZST args)
        let mut closure_call_args = vec![env_handle]; // First arg is environment
        closure_call_args.extend_from_slice(&arg_vals); // Use already filtered arg_vals

        // Create signature for the indirect call using static HIR types
        let mut sig_param_abis = Vec::with_capacity(param_types.len() + 1); 
        sig_param_abis.push(AbiParam::new(pointer_type)); // Environment handle
        for hir_ty in param_types { // Use passed-in param_types
            if let Some(cl_ty) = translate_type(hir_ty, isa.as_ref(), ctx, layout_computer)? {
                 sig_param_abis.push(AbiParam::new(cl_ty));
            }
        }
        let sig_return_abis = if ret_type.is_never() { vec![] } else { vec![AbiParam::new(result_cl_ty)] };

        let signature = Signature {
            params: sig_param_abis, 
            returns: sig_return_abis, 
            call_conv: isa.default_call_conv(),
        };
        let closure_sig_ref = builder.import_signature(signature);

        // Perform the indirect call (regular call for now)
        let call = builder.ins().call_indirect(closure_sig_ref, fn_ptr_val, &closure_call_args); 
        let results = builder.inst_results(call);
        let result_val = results.get(0).copied().ok_or(NativeError::TypeError("Closure call produced no return value".to_string()))?;
        builder.ins().jump(merge_call_block, &[result_val]);
    }

    // --- Direct Function Call Path ---
    builder.switch_to_block(direct_call_block);
    {
        let func_ptr_val = callee_val;
        // Create signature for the direct call using static HIR types
        let mut sig_param_abis = Vec::with_capacity(param_types.len()); 
        for hir_ty in param_types { // Use passed-in param_types
            if let Some(cl_ty) = translate_type(hir_ty, isa.as_ref(), ctx, layout_computer)? {
                 sig_param_abis.push(AbiParam::new(cl_ty));
            }
        }
        let sig_return_abis = if ret_type.is_never() { vec![] } else { vec![AbiParam::new(result_cl_ty)] };

        let signature = Signature {
            params: sig_param_abis, 
            returns: sig_return_abis, 
            call_conv: isa.default_call_conv(),
        };
        let direct_sig_ref = builder.import_signature(signature);

        // Perform the direct call (regular call for now)
        let direct_call_inst = builder.ins().call_indirect(direct_sig_ref, func_ptr_val, &arg_vals);
        let results = builder.inst_results(direct_call_inst);
        let result_val = results.get(0).copied().ok_or(NativeError::TypeError("Indirect regular function call produced no return value".to_string()))?;
        builder.ins().jump(merge_call_block, &[result_val]);
    }

    // --- Merge Call Path ---
    builder.switch_to_block(merge_call_block);
    builder.seal_block(closure_call_block);
    builder.seal_block(direct_call_block);
    builder.seal_block(merge_call_block);

    let call_result = builder.block_params(merge_call_block)[0];

    // --- Pop Shadow Stack Before Return ---
    let pop_fn_ref = declare_shadow_stack_pop_fn(jit_module, builder.func, pointer_type)?;
    let count = ctx.shadow_stack_push_count();
    if count > 0 {
        let count_val = builder.ins().iconst(pointer_type, count as i64);
        builder.ins().call(pop_fn_ref, &[count_val]);
    }
    // ------------------------------------

    builder.ins().return_(&[call_result]);
    Ok(call_result) // Result is technically ignored due to return, but needed for type check
}
*/ 