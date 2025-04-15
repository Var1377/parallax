use crate::NativeError;
use crate::translator::context::{TranslationContext, KnownFunction};
use crate::translator::expr::translate_expr;
use crate::translator::types::{translate_type, translate_signature};
use cranelift_codegen::Context;
use cranelift_codegen::isa::TargetIsa;
use cranelift_codegen::ir::{Function, InstBuilder, TrapCode};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::JITModule;
use cranelift_module::{Linkage, Module};
use parallax_hir::hir::{HirFunction, HirExpr};
use parallax_hir::hir::HirModule;
use std::sync::Arc;
use std::collections::HashMap;
use crate::translator::layout::{LayoutComputer, new_layout_computer};

/// Translates the body of an HIR function into Cranelift IR.
pub fn translate_function_body<'ctx, 'hir>(
    _hir_module: &HirModule,
    hir_function: &HirFunction,
    _body: &HirExpr,
    func_builder_ctx: &mut FunctionBuilderContext,
    ctx: &mut Context,
    jit_module: &mut JITModule,
    isa: &Arc<dyn TargetIsa>,
    known_functions: &HashMap<parallax_hir::Symbol, KnownFunction>,
    struct_defs: &'ctx [parallax_hir::hir::HirStructDef],
    enum_defs: &'ctx [parallax_hir::hir::HirEnumDef],
) -> Result<(), NativeError> {
    let mut translation_ctx = TranslationContext::new(struct_defs, enum_defs);
    // Add known functions to the context
    for (symbol, info) in known_functions {
        translation_ctx.add_function_info(*symbol, info.clone());
    }

    // Create the LayoutComputer without context reference
    let mut layout_computer = new_layout_computer();

    // Pass translation_ctx explicitly to translate_signature
    let signature = translate_signature(
        &hir_function.signature,
        isa,
        &translation_ctx,
        &mut layout_computer,
    )?;

    // Create a new Cranelift function
    let func_id = jit_module
        .declare_function(&hir_function.name, Linkage::Export, &signature)
        .map_err(|e| NativeError::CraneliftModule(e))?;

    // Clone signature here because Function::with_name_signature takes ownership
    ctx.func = Function::with_name_signature(cranelift_codegen::ir::UserFuncName::user(0, func_id.as_u32()), signature.clone());

    let mut builder = FunctionBuilder::new(&mut ctx.func, func_builder_ctx);
    
    // Create an entry block and switch to it
    let entry_block = builder.create_block();
    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);
    
    // Process function parameters and add them to the context
    let entry_params = builder.block_params(entry_block).to_vec(); // Collect params to avoid borrow issues

    // Create a scope to limit the borrow needed for layout computation
    let param_cl_types: Vec<Option<cranelift_codegen::ir::Type>> = {
        // Create LayoutComputer *inside* this scope, without context reference
        let mut layout_computer = new_layout_computer();

        // Pre-compute Cranelift types for parameters within this scope, passing translation_ctx
        hir_function
            .signature
            .params
            .iter()
            .map(|(_, hir_ty)| translate_type(hir_ty, isa.as_ref(), &translation_ctx, &mut layout_computer))
            .collect::<Result<Vec<_>, _>>()?
    }; // layout_computer goes out of scope here

    // Now iterate and bind variables. layout_computer's borrow has ended.
    for (((hir_var, hir_ty), cl_val), opt_cl_ty) in hir_function
        .signature
        .params
        .iter()
        .zip(entry_params.iter()) // Use the collected entry_params
        .zip(param_cl_types.iter()) 
    {
        if let Some(cl_ty) = opt_cl_ty {
             // Check if the Cranelift value type matches the translated type
             let val_ty = builder.func.dfg.value_type(*cl_val);
             if val_ty != *cl_ty {
                 // This indicates a mismatch between signature translation and function definition
                 return Err(NativeError::TypeError(format!( // Changed error type
                    "Type mismatch for parameter {:?}: expected {:?}, got {:?}",
                    hir_var, cl_ty, val_ty
                )));
             }
            // This mutable borrow should now be fine
            translation_ctx.add_var_binding(*hir_var, *cl_val, hir_ty.clone());
        } else {
             // This mutable borrow should also be fine
             translation_ctx.add_var_type(*hir_var, hir_ty.clone());
        }
    }

    // Translate the function body expression, passing LayoutComputer
    if let Some(body_expr) = &hir_function.body {
        // Re-create LayoutComputer specifically for body translation, without context reference
        let mut layout_computer = new_layout_computer();
        // Pass translation_ctx explicitly to translate_expr (which will pass it down)
        match translate_expr(&mut builder, &mut translation_ctx, body_expr, jit_module, isa, &mut layout_computer) {
            Ok(final_value) => {
                 // If the function is supposed to return a value and the current block is reachable
                 let current_block_reachable = builder.current_block().is_some() && !builder.is_unreachable();
                if current_block_reachable && !hir_function.signature.return_type.is_never() {
                    // Pass translation_ctx explicitly to translate_type
                    if let Some(_) = translate_type(&hir_function.signature.return_type, isa.as_ref(), &translation_ctx, &mut layout_computer)? {
                         builder.ins().return_(&[final_value]);
                    }
                }
            }
            Err(e) => {
                eprintln!("Error translating function body for {}: {}", hir_function.name, e);
                // Ensure block is terminated if error occurred mid-translation
                let current_block_reachable = builder.current_block().is_some() && !builder.is_unreachable();
                if current_block_reachable {
                    // Use LayoutComputer for return type translation
                    // Re-create layout_computer again for this scope, without context reference
                    let mut layout_computer = new_layout_computer();
                     // Pass translation_ctx explicitly to translate_type
                     if let Some(ret_ty) = translate_type(&hir_function.signature.return_type, isa.as_ref(), &translation_ctx, &mut layout_computer)? {
                         let zero = builder.ins().iconst(ret_ty, 0);
                         builder.ins().return_(&[zero]);
                     } else {
                         builder.ins().return_(&[]);
                     }
                }
                return Err(e);
            }
        }
    } else {
        // Handle extern functions or functions without bodies
        let current_block_reachable = builder.current_block().is_some() && !builder.is_unreachable();
        if current_block_reachable {
            // Check the original signature (before cloning)
            if signature.returns.is_empty() {
                builder.ins().return_(&[]);
            } else {
                // Use a standard trap code instead of User(0)
                // User code 1 signifies an unexpected execution path (extern function with return type but no body reached).
                builder.ins().trap(TrapCode::user(1).unwrap());
            }
        }
    }
    
    // Seal all blocks. This requires all blocks to be terminated.
    // The logic above (or within translate_expr) should ensure this.
    builder.seal_all_blocks(); // Seal all blocks now
    
    // Finalize the function
    builder.finalize();
    
    // Define the function in the JIT module
    jit_module
        .define_function(func_id, ctx)
        .map_err(NativeError::CraneliftModule)?; // Use a specific error variant

    // Clear the context for the next function
    jit_module.clear_context(ctx);

    Ok(())
} 