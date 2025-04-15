// Handles translation of HIR tail expressions (Return, If, Match, Never).
use crate::NativeError;
use crate::translator::context::TranslationContext;
use crate::translator::operand::translate_operand;
use crate::translator::expr::translate_expr; // Need this for recursive calls in arms/branches
use crate::translator::pattern::translate_pattern_check; // Use the new check function
use crate::translator::types::translate_type;
use crate::translator::helpers::declare_shadow_stack_pop_fn;
use crate::translator::layout::LayoutComputer;
use cranelift_codegen::ir::{Value, InstBuilder, types};
use cranelift_codegen::isa::TargetIsa;
use cranelift_frontend::FunctionBuilder;
use cranelift_jit::JITModule;
use parallax_hir::hir::{HirTailExpr, HirExpr, Operand, HirType, ResolvePrimitiveType};
use std::sync::Arc;

pub fn translate_tail_expr<'ctx>(
    builder: &mut FunctionBuilder,
    ctx: &mut TranslationContext<'ctx>,
    tail_expr: &HirTailExpr,
    jit_module: &mut JITModule,
    isa: &Arc<dyn TargetIsa>,
    layout_computer: &mut LayoutComputer,
) -> Result<Value, NativeError> {
    match tail_expr {
        HirTailExpr::Return(operand) => {
            // Translate the operand to get the return value
            let hir_type = ctx.get_operand_type(operand);
            let return_val = translate_operand(builder, ctx, operand, hir_type.as_ref(), jit_module, isa, layout_computer)?;

            // --- Pop Shadow Stack Before Return --- 
            let pointer_type = isa.pointer_type();
            let pop_fn_ref = declare_shadow_stack_pop_fn(jit_module, builder.func, pointer_type)?;
            let count = ctx.shadow_stack_push_count();
            if count > 0 {
                let count_val = builder.ins().iconst(pointer_type, count as i64);
                builder.ins().call(pop_fn_ref, &[count_val]);
            }
            // -------------------------------------

            // Emit the return instruction
            builder.ins().return_(&[return_val]);

            // Return the value for consistency, though it won't be used after a return
            Ok(return_val)
        }
        HirTailExpr::If { condition, then_branch, else_branch } => {
            // Delegate to the dedicated if-tail translator
            translate_if_tail(builder, ctx, condition, then_branch, else_branch, jit_module, isa, layout_computer)
        }
        HirTailExpr::Match { scrutinee, arms, otherwise } => {
            // 1. Translate Scrutinee
            let scrutinee_type = ctx.get_operand_type(scrutinee)
                .ok_or_else(|| NativeError::TypeError("Cannot determine type of match scrutinee".to_string()))?;
            let scrutinee_val = translate_operand(builder, ctx, scrutinee, Some(&scrutinee_type), jit_module, isa, layout_computer)?;
            //let scrutinee_cl_ty = builder.func.dfg.value_type(scrutinee_val); // Use this if needed below

            // 2. Setup Blocks
            let merge_block = builder.create_block();
            let otherwise_block = builder.create_block(); // Block for the default/otherwise case

            // 3. Determine Result Type and add param to merge block
            // Assume the type of the first arm's expression is the result type
            let result_hir_ty = arms.first().map(|(_, expr)| &expr.ty).unwrap_or_else(|| {
                // If no arms, use the otherwise type, or Never if no otherwise
                otherwise.as_ref().map_or(&HirType::Never, |expr| &expr.ty)
            });
            let result_cl_ty = translate_type(result_hir_ty, isa.as_ref(), ctx, layout_computer)?.unwrap_or(types::I8); // Default to I8 for ZST
            let merge_param = builder.append_block_param(merge_block, result_cl_ty);

            // Keep track of blocks needing sealing
            let mut blocks_to_seal = vec![merge_block, otherwise_block];

            // 4. Iterate Arms - Chain pattern checks
            let mut first_check_block = builder.current_block().unwrap(); // Block before the first check

            for (i, (pattern, arm_expr)) in arms.iter().enumerate() {
                let arm_body_block = builder.create_block();
                let next_check_block = builder.create_block(); // Block for the next pattern check if this one fails
                blocks_to_seal.push(arm_body_block);
                blocks_to_seal.push(next_check_block);
                
                builder.switch_to_block(first_check_block);
                
                // Perform the pattern check, branching to arm_body or next_check
                let _condition: Value = translate_pattern_check(
                    builder,
                    ctx,
                    pattern,
                    scrutinee_val,
                    &scrutinee_type,
                    jit_module,
                    isa,
                    layout_computer,
                    arm_body_block,   // Block if match
                    next_check_block  // Block if no match
                )?;
                
                // Translate Arm Body (in arm_body_block)
                builder.switch_to_block(arm_body_block);
                // TODO: Handle bindings created by translate_pattern_check (especially for Variant fields)
                let arm_result = translate_expr(builder, ctx, arm_expr, jit_module, isa, layout_computer)?;
                let jump_arg = if builder.func.dfg.value_type(arm_result) == result_cl_ty {
                     arm_result
                } else {
                     builder.ins().iconst(result_cl_ty, 0)
                };
                builder.ins().jump(merge_block, &[jump_arg]);

                // The block for the next check is now the current `first_check_block`
                first_check_block = next_check_block;
            }
            
            // After the loop, the last `next_check_block` should jump to `otherwise`
            builder.switch_to_block(first_check_block);
            builder.ins().jump(otherwise_block, &[]);

            // 5. Otherwise Branch
            builder.switch_to_block(otherwise_block);
            if let Some(otherwise_expr) = otherwise {
                let otherwise_result = translate_expr(builder, ctx, otherwise_expr, jit_module, isa, layout_computer)?;
                 let jump_arg = if builder.func.dfg.value_type(otherwise_result) == result_cl_ty {
                     otherwise_result
                 } else {
                     builder.ins().iconst(result_cl_ty, 0)
                 };
                builder.ins().jump(merge_block, &[jump_arg]);
            } else {
                // No otherwise branch - implies the match should be exhaustive.
                // Trap if this block is reached.
                builder.ins().trap(cranelift_codegen::ir::TrapCode::unwrap_user(2));
            }

            // 6. Merge
            builder.switch_to_block(merge_block);

            // Seal all the blocks we created
            for block in blocks_to_seal {
                // Avoid sealing the current block if it's the merge block already
                if builder.current_block() != Some(block) {
                     builder.seal_block(block);
                }
            }
            // Seal the final merge block explicitly if it's reachable
            if builder.current_block() == Some(merge_block) {
                 builder.seal_block(merge_block);
            }

            // Return the merge parameter
            Ok(merge_param)
        }
        HirTailExpr::Never => {
            // --- Pop Shadow Stack Before Unreachable ---
            let pointer_type = isa.pointer_type();
            let pop_fn_ref = declare_shadow_stack_pop_fn(jit_module, builder.func, pointer_type)?;
            let count = ctx.shadow_stack_push_count();
            if count > 0 {
                let count_val = builder.ins().iconst(pointer_type, count as i64);
                builder.ins().call(pop_fn_ref, &[count_val]);
            }
            // -------------------------------------
            
            // Mark block as unreachable
            builder.ins().trap(cranelift_codegen::ir::TrapCode::unwrap_user(2));
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
    layout_computer: &mut LayoutComputer,
) -> Result<Value, NativeError> {
    let cond_val = translate_operand(builder, ctx, condition, Some(&HirType::Primitive(ResolvePrimitiveType::Bool)), jit_module, isa, layout_computer)?;

    let then_block = builder.create_block();
    let else_block = builder.create_block();

    // Since this is a tail-position if, the branches will terminate with `return`.
    // We don't need a merge block.

    builder.ins().brif(cond_val, then_block, &[], else_block, &[]);

    // Generate code for the then branch (ends with return)
    builder.switch_to_block(then_block);
    let current_then_block = builder.current_block().unwrap(); // Get handle before translating
    let _then_val = translate_expr(builder, ctx, then_expr, jit_module, isa, layout_computer)?; // Translate, likely ends in return
    
    // Check if the builder is still in the same block AND if that block lacks a terminator
    let still_in_then_block = builder.current_block() == Some(current_then_block);
    let then_block_terminated = builder.func.layout.last_inst(current_then_block)
        .map_or(false, |inst| builder.func.dfg.insts[inst].opcode().is_terminator());

    if still_in_then_block && !then_block_terminated {
         builder.ins().trap(cranelift_codegen::ir::TrapCode::unwrap_user(3));
    }

    // Generate code for the else branch (ends with return)
    builder.switch_to_block(else_block);
    let current_else_block = builder.current_block().unwrap(); // Get handle before translating
    let _else_val = translate_expr(builder, ctx, else_expr, jit_module, isa, layout_computer)?; // Translate, likely ends in return
    
    // Check if the builder is still in the same block AND if that block lacks a terminator
    let still_in_else_block = builder.current_block() == Some(current_else_block);
    let else_block_terminated = builder.func.layout.last_inst(current_else_block)
        .map_or(false, |inst| builder.func.dfg.insts[inst].opcode().is_terminator());
        
    if still_in_else_block && !else_block_terminated {
        builder.ins().trap(cranelift_codegen::ir::TrapCode::unwrap_user(3));
    }

    // Seal the blocks
    builder.seal_block(then_block);
    builder.seal_block(else_block);

    // Since both branches must return, the code after the branch is unreachable.
    // We need to return *something* to satisfy the Rust type checker here, but it won't
    // actually be used if the branches correctly return.
    // Return a dummy value of the function's expected return type.
    let expected_ret_ty = builder.func.signature.returns.first().map(|p| p.value_type).unwrap_or(types::I8);
    Ok(builder.ins().iconst(expected_ret_ty, 0))
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