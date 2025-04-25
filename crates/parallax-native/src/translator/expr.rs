use crate::NativeError;
use crate::translator::context::{TranslationContext, declare_variable};
// Import functions from the new modules
use crate::translator::value::translate_value;
use crate::translator::tail::translate_tail_expr;
use crate::translator::operand::translate_operand;
use crate::translator::helpers::{declare_shadow_stack_pop_fn}; // Import necessary helpers

use cranelift_codegen::ir::{Value, InstBuilder, types, MemFlags, AbiParam, Signature};
use cranelift_codegen::isa::TargetIsa;
use cranelift_frontend::FunctionBuilder;
use cranelift_jit::JITModule;
use cranelift_module::Module;
use parallax_hir::hir::{HirExpr, HirExprKind, HirValue, Operand, HirTailExpr, HirType};
use parallax_hir::Symbol; // Import Symbol
use std::sync::Arc;
use std::collections::HashMap; // Import HashMap
use crate::translator::types::translate_type; // Keep this for let binding type check
use parallax_gc::{LayoutDescriptor, DescriptorIndex}; // Import new GC types

/// Translate an HIR expression to Cranelift IR
/// 
/// This is the main entry point for translating expressions.
pub fn translate_expr<'ctx>(
    builder: &mut FunctionBuilder,
    ctx: &mut TranslationContext<'ctx>,
    expr: &HirExpr,
    jit_module: &mut JITModule,
    isa: &Arc<dyn TargetIsa>,
) -> Result<Value, NativeError> {
    match &expr.kind {
        HirExprKind::Let { var, var_ty, value, rest } => {
            // --- TCO Pattern Detection --- 
            if let HirValue::Call { func: callee_op, args: arg_ops } = &**value {
                if let HirExprKind::Tail(HirTailExpr::Value(Operand::Var(ret_var))) = &rest.kind {
                    if *ret_var == *var {
                        println!("TCO Pattern Matched for var {:?}", var); // Debug print
                        
                        let mut arg_vals = Vec::with_capacity(arg_ops.len());
                        for arg_op in arg_ops {
                            let arg_hir_ty = ctx.get_operand_type(arg_op);
                            let arg_val = translate_operand(builder, ctx, arg_op, arg_hir_ty.as_ref(), jit_module, isa)?;
                            if let Some(_) = translate_type(arg_hir_ty.unwrap_or(HirType::Never), isa.as_ref(), ctx)? {
                                 arg_vals.push(arg_val);
                            }
                        }
                        
                        let callee_hir_ty = ctx.get_operand_type(callee_op)
                            .ok_or_else(|| NativeError::TypeError("Cannot determine type of tail call callee operand".to_string()))?;
                        let callee_val = translate_operand(builder, ctx, callee_op, Some(&callee_hir_ty), jit_module, isa)?;

                        let pointer_type = isa.pointer_type();
                        let pop_fn_ref = declare_shadow_stack_pop_fn(jit_module, builder.func, pointer_type)?;
                        let count = ctx.shadow_stack_push_count();
                        if count > 0 {
                            let count_val = builder.ins().iconst(pointer_type, count as i64);
                            builder.ins().call(pop_fn_ref, &[count_val]);
                        }

                        match callee_op {
                            Operand::Global(func_sym) => {
                                let known_func = ctx.get_function_like_info(*func_sym)
                                     .ok_or_else(|| NativeError::Unimplemented(format!("Function info for {:?} not found in context during TCO", func_sym)))?;
                                let func_ref = jit_module.get_name(&known_func.name)
                                     .ok_or_else(|| NativeError::Unimplemented(format!("Function name '{}' not found in JIT module during TCO", known_func.name)))?;
                                let func_id = match func_ref { 
                                    cranelift_module::FuncOrDataId::Func(id) => id, 
                                    _ => return Err(NativeError::TypeError("Expected function symbol for direct TCO".to_string())) 
                                };
                                let local_func_ref = jit_module.declare_func_in_func(func_id, builder.func);
                                builder.ins().return_call(local_func_ref, &arg_vals);
                            }
                            Operand::Var(_) | Operand::Const(_) => {
                                if let HirType::FunctionPointer(param_types, ret_type) = callee_hir_ty {
                                    let tag_mask = builder.ins().iconst(pointer_type, 1);
                                    let tag = builder.ins().band(callee_val, tag_mask);
                                    let is_closure = builder.ins().icmp_imm(cranelift_codegen::ir::condcodes::IntCC::Equal, tag, 1);

                                    let closure_tco_block = builder.create_block();
                                    let regular_tco_block = builder.create_block();
                                    builder.ins().brif(is_closure, closure_tco_block, &[], regular_tco_block, &[]);

                                    // --- Closure TCO Path --- 
                                    builder.switch_to_block(closure_tco_block);
                                    {
                                        let one = builder.ins().iconst(pointer_type, 1);
                                        let closure_ref_ptr = builder.ins().bxor(callee_val, one);
                                        let func_ptr_val = builder.ins().load(pointer_type, MemFlags::trusted(), closure_ref_ptr, 0);
                                        let env_offset = i32::try_from(pointer_type.bytes()).expect("Pointer size should fit in i32 for load offset");
                                        let env_handle_val = builder.ins().load(pointer_type, MemFlags::trusted(), closure_ref_ptr, env_offset);

                                        let mut final_call_args = Vec::with_capacity(arg_vals.len() + 1);
                                        final_call_args.push(env_handle_val);
                                        final_call_args.extend_from_slice(&arg_vals);

                                        let result_cl_ty = translate_type((*ret_type).clone(), isa.as_ref(), ctx)?.unwrap_or(types::I8); // ZST default
                                        let mut sig_param_abis = Vec::with_capacity(param_types.len() + 1);
                                        sig_param_abis.push(AbiParam::new(pointer_type)); // Env
                                        for hir_ty in &param_types {
                                            if let Some(cl_ty) = translate_type(hir_ty.clone(), isa.as_ref(), ctx)? {
                                                sig_param_abis.push(AbiParam::new(cl_ty));
                                            }
                                        }
                                        let sig_return_abis = if ret_type.is_never() { vec![] } else { vec![AbiParam::new(result_cl_ty)] };
                                        let signature = Signature { params: sig_param_abis, returns: sig_return_abis, call_conv: isa.default_call_conv() };
                                        let sig_ref = builder.import_signature(signature);

                                        builder.ins().return_call_indirect(sig_ref, func_ptr_val, &final_call_args);
                                    }

                                    // --- Regular TCO Path --- 
                                    builder.switch_to_block(regular_tco_block);
                                    {
                                        let func_ptr_val = callee_val;
                                        let result_cl_ty = translate_type((*ret_type).clone(), isa.as_ref(), ctx)?.unwrap_or(types::I8);
                                        let mut sig_param_abis = Vec::with_capacity(param_types.len());
                                        for hir_ty in &param_types {
                                            if let Some(cl_ty) = translate_type(hir_ty.clone(), isa.as_ref(), ctx)? {
                                                sig_param_abis.push(AbiParam::new(cl_ty));
                                            }
                                        }
                                        let sig_return_abis = if ret_type.is_never() { vec![] } else { vec![AbiParam::new(result_cl_ty)] };
                                        let signature = Signature { params: sig_param_abis, returns: sig_return_abis, call_conv: isa.default_call_conv() };
                                        let sig_ref = builder.import_signature(signature);

                                        builder.ins().return_call_indirect(sig_ref, func_ptr_val, &arg_vals);
                                    }
                                    // Sealing happens implicitly by return_call_indirect?
                                    // builder.seal_block(closure_tco_block); // Seal after switch?
                                    // builder.seal_block(regular_tco_block);
                                } else {
                                    return Err(NativeError::TypeError(format!("Operand used as indirect tail call callee is not a function pointer type, found {:?}", callee_hir_ty)));
                                }
                            }
                        }
                        return Ok(builder.ins().iconst(types::I8, 0)); // Dummy value for TCO path
                    }
                }
            }
            // --- End TCO Pattern Detection --- 

            // --- Original Let Logic --- 
            let cl_type = if let Some(ty) = translate_type(var_ty.clone(), isa.as_ref(), ctx)? {
                ty
            } else {
                ctx.add_var_type(*var, var_ty.clone());
                return translate_expr(builder, ctx, rest, jit_module, isa);
            };
            
            // Use the simplified translate_value signature without layoutComputer
            let value_val = translate_value(builder, ctx, value, jit_module, isa)?;
            
            declare_variable(builder, ctx, *var, var_ty.clone(), cl_type)?;
            ctx.add_var_binding(*var, value_val, var_ty.clone());
            translate_expr(builder, ctx, rest, jit_module, isa)
            // --- End Original Let Logic --- 
        }
        HirExprKind::Tail(tail_expr) => {
            translate_tail_expr(builder, ctx, tail_expr, jit_module, isa)
        }
    }
} 