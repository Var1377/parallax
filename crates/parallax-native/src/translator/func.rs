use crate::NativeError;
use crate::translator::context::{TranslationContext, KnownFunction};
use crate::translator::expr::translate_expr;
use crate::translator::types::{translate_type, translate_signature};
use crate::translator::helpers::declare_shadow_stack_pop_fn;
use cranelift_codegen::Context;
use cranelift_codegen::isa::TargetIsa;
use cranelift_codegen::ir::{Function, InstBuilder, TrapCode, UserFuncName, Opcode};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::JITModule;
use cranelift_module::{Linkage, Module};
use parallax_hir::hir::{HirFunction, HirExpr, PrimitiveType};
use parallax_hir::hir::HirModule;
use std::sync::Arc;
use std::collections::HashMap;
use parallax_layout::{DescriptorIndex, DescriptorStore, LayoutDescriptor};
use parallax_hir::Symbol;
use parallax_hir::hir::{HirStructDef, HirEnumDef, HirType};
use std::collections::HashSet;

/// Translates the body of an HIR function into Cranelift IR.
/// Returns the populated Function object.
pub fn translate_function_body<'ctx>(
    _hir_module: &'ctx HirModule,
    hir_function: &'ctx HirFunction,
    _body: &'ctx HirExpr,
    func_id: cranelift_module::FuncId,
    func_builder_ctx: &mut FunctionBuilderContext,
    jit_module: &mut JITModule,
    isa: &Arc<dyn TargetIsa>,
    known_functions: &'ctx HashMap<Symbol, KnownFunction>,
    struct_defs: &'ctx HashMap<Symbol, HirStructDef>,
    enum_defs: &'ctx HashMap<Symbol, HirEnumDef>,
    descriptor_store: &'ctx DescriptorStore,
    adt_index_map: &'ctx HashMap<Symbol, DescriptorIndex>,
    primitive_index_map: &'ctx HashMap<PrimitiveType, DescriptorIndex>,
    tuple_index_map: &'ctx HashMap<Vec<HirType>, DescriptorIndex>,
    array_index_map: &'ctx HashMap<(HirType, usize), DescriptorIndex>,
    handle_descriptor_index: Option<DescriptorIndex>,
    intrinsic_symbols: &'ctx HashSet<Symbol>,
) -> Result<Function, NativeError> {
    let mut translation_ctx = TranslationContext::new(
        struct_defs,
        enum_defs,
        descriptor_store,
        adt_index_map,
        primitive_index_map,
        tuple_index_map,
        array_index_map,
        handle_descriptor_index,
        intrinsic_symbols,
    );
    // Add known functions to the context
    for (symbol, info) in known_functions {
        translation_ctx.add_function_info(*symbol, info.clone());
    }

    // Translate signature locally
    let signature = translate_signature(
        &hir_function.signature,
        isa,
        &mut translation_ctx,
    )?;

    // Create the Function object INSIDE this function
    let mut func = Function::with_name_signature(UserFuncName::user(0, func_id.as_u32()), signature.clone());

    // Create the builder using the function object we just made
    let mut builder = FunctionBuilder::new(&mut func, func_builder_ctx);
    
    // Create an entry block and switch to it
    let entry_block = builder.create_block();
    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);
    
    // Process function parameters and add them to the context
    let entry_params = builder.block_params(entry_block).to_vec();

    // Process parameters
    // Iterate over the HIR parameters and the Cranelift entry parameters simultaneously
    for ((hir_var, hir_ty), cl_val) in hir_function // Corrected iteration pattern
        .signature
        .params
        .iter()
        .zip(entry_params.iter()) // Zip directly with Cranelift values
    {
        // Translate the HIR type to check if it's a ZST
        if let Some(cl_ty) = translate_type(hir_ty.clone(), isa.as_ref(), &mut translation_ctx)? {
             // Verify Cranelift value type matches translated type (optional but good sanity check)
             let val_ty = builder.func.dfg.value_type(*cl_val);
             if val_ty != cl_ty {
                 return Err(NativeError::TypeError(format!(
                    "Type mismatch for parameter {:?}: expected {:?}, got {:?}",
                    hir_var, cl_ty, val_ty
                )));
             }
            // Add binding using the Cranelift Value (*cl_val)
            translation_ctx.add_var_binding(*hir_var, *cl_val, hir_ty.clone());
        } else {
             // It's a ZST, just add the type info
             translation_ctx.add_var_type(*hir_var, hir_ty.clone());
        }
    }

    builder.seal_block(entry_block);

    let pointer_type = isa.pointer_type(); // Get pointer type early

    // Translate the function body expression
    if let Some(body_expr) = &hir_function.body {
        match translate_expr(
            &mut builder,
            &mut translation_ctx,
            body_expr,
            jit_module,
            isa,
        ) {
            Ok(final_value) => {
                if let Some(current_block) = builder.current_block() {
                    let is_terminated = builder.func.layout.last_inst(current_block)
                        .map_or(false, |inst| builder.func.dfg.insts[inst].opcode().is_terminator());

                    if !is_terminated {
                        // --- Pop Shadow Stack Before Return --- 
                        let count = translation_ctx.shadow_stack_push_count();
                        if count > 0 {
                            let pop_fn_ref = declare_shadow_stack_pop_fn(jit_module, builder.func, pointer_type)?;
                            let count_val = builder.ins().iconst(pointer_type, count as i64);
                            builder.ins().call(pop_fn_ref, &[count_val]);
                        }
                        // -------------------------------------
                        if !hir_function.signature.return_type.is_never() {
                            if let Some(_) = translate_type(hir_function.signature.return_type.clone(), isa.as_ref(), &mut translation_ctx)? {
                                builder.ins().return_(&[final_value]);
                            } else {
                                builder.ins().return_(&[]);
                            }
                        } else {
                            builder.ins().trap(TrapCode::unwrap_user(0));
                        }
                    }
                }
            }
            Err(e) => {
                println!("Error translating function body for {}: {}", hir_function.name, e);
                if let Some(current_block) = builder.current_block() {
                    let is_terminated = builder.func.layout.last_inst(current_block)
                        .map_or(false, |inst| builder.func.dfg.insts[inst].opcode().is_terminator());

                    if !is_terminated {
                         // --- Pop Shadow Stack Before Return --- 
                        let count = translation_ctx.shadow_stack_push_count();
                        if count > 0 {
                            let pop_fn_ref = declare_shadow_stack_pop_fn(jit_module, builder.func, pointer_type)?;
                            let count_val = builder.ins().iconst(pointer_type, count as i64);
                            builder.ins().call(pop_fn_ref, &[count_val]);
                        }
                        // -------------------------------------
                        if let Some(ret_ty) = translate_type(hir_function.signature.return_type.clone(), isa.as_ref(), &mut translation_ctx)? {
                            let zero = builder.ins().iconst(ret_ty, 0);
                            builder.ins().return_(&[zero]);
                        } else {
                            builder.ins().return_(&[]);
                        }
                    }
                }
                return Err(e);
            }
        }
    } else {
        let current_block_reachable = builder.current_block().is_some() && !builder.is_unreachable();
        if current_block_reachable {
            if signature.returns.is_empty() {
                 // --- Pop Shadow Stack Before Return --- 
                let count = translation_ctx.shadow_stack_push_count();
                if count > 0 {
                    let pop_fn_ref = declare_shadow_stack_pop_fn(jit_module, builder.func, pointer_type)?;
                    let count_val = builder.ins().iconst(pointer_type, count as i64);
                    builder.ins().call(pop_fn_ref, &[count_val]);
                }
                // -------------------------------------
                builder.ins().return_(&[]);
            } else {
                // --- Pop Shadow Stack Before Trap --- 
                let count = translation_ctx.shadow_stack_push_count();
                if count > 0 {
                    let pop_fn_ref = declare_shadow_stack_pop_fn(jit_module, builder.func, pointer_type)?;
                    let count_val = builder.ins().iconst(pointer_type, count as i64);
                    builder.ins().call(pop_fn_ref, &[count_val]);
                }
                // -------------------------------------
                builder.ins().trap(TrapCode::unwrap_user(1));
            }
        }
    }
    
    // --- Debug Print IR --- 
    println!("--- Cranelift IR for {}: ---", hir_function.name);
    println!("{}", builder.func);
    println!("--- End IR for {} ---", hir_function.name);
    // -----------------------

    // Seal all blocks. This requires all blocks to be terminated.
    // The logic above (or within translate_expr) should ensure this.
    builder.seal_all_blocks(); // Seal all blocks now
    
    // Finalize the function building process
    builder.finalize();

    // Return the populated function object
    Ok(func)
} 