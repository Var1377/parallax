// Handles translation of HIR values (Use, Call, Aggregate, Project, Closure).
use crate::NativeError;
use crate::translator::context::TranslationContext;
use crate::translator::operand::translate_operand;
use crate::translator::types::translate_type;
use crate::translator::helpers::{declare_shadow_stack_push_fn, declare_alloc_closure_fn};
use crate::translator::layout::{LayoutComputer, get_layout, get_size_bytes, get_repc_type_with_layout, get_field_offset_bytes, get_enum_discriminant_offset_bytes, get_enum_discriminant_type, get_variant_payload_offset_bytes};
use cranelift_codegen::ir::{Value, InstBuilder, types, MemFlags, TrapCode, Signature, AbiParam};
use cranelift_codegen::isa::TargetIsa;
use cranelift_frontend::FunctionBuilder;
use cranelift_jit::JITModule;
use cranelift_module::Module;
use parallax_hir::hir::{HirValue, Operand, HirType, AggregateKind, ProjectionKind, ResolvePrimitiveType};
use std::sync::Arc;
use parallax_gc::{CaptureType, CaptureItem};
use cranelift_codegen::ir::StackSlotData;
use cranelift_codegen::ir::StackSlotKind;
use std::mem::size_of;

/// Helper to check if a type is zero-sized based on layout.
fn is_zst(hir_type: &HirType, layout_computer: &mut LayoutComputer, ctx: &TranslationContext) -> bool {
    match get_layout(hir_type, layout_computer, ctx) {
        Ok(layout) => get_size_bytes(&layout) == 0,
        Err(_) => false, // Assume not ZST if layout fails
    }
}

/// Translate an HIR value to Cranelift IR
pub fn translate_value<'ctx>(
    builder: &mut FunctionBuilder,
    ctx: &mut TranslationContext<'ctx>,
    value: &HirValue,
    jit_module: &mut JITModule,
    isa: &Arc<dyn TargetIsa>,
    layout_computer: &mut LayoutComputer,
) -> Result<Value, NativeError> {
    match value {
        HirValue::Use(operand) => {
            let expected_hir_ty = ctx.get_operand_type(operand);
            translate_operand(builder, ctx, operand, expected_hir_ty.as_ref(), jit_module, isa, layout_computer)
        }
        HirValue::Call { func, args } => {
            // Translate arguments first
            let mut arg_vals = Vec::with_capacity(args.len());
            for arg in args {
                let arg_type = ctx.get_operand_type(arg);
                let arg_val = translate_operand(builder, ctx, arg, arg_type.as_ref(), jit_module, isa, layout_computer)?;
                // Only add non-ZST arguments to the call list
                if arg_type.map_or(false, |t| !is_zst(&t, layout_computer, ctx)) { // Use free function
                    arg_vals.push(arg_val);
                }
            }

            let callee_hir_ty = ctx.get_operand_type(func)
                .ok_or_else(|| NativeError::TypeError("Cannot determine type of callee operand".to_string()))?;

            match func {
                Operand::Global(func_sym) => {
                    // Direct call logic
                    let known_func = ctx.get_function_like_info(*func_sym)
                        .ok_or_else(|| NativeError::Unimplemented(format!("Function info for {:?} not found in context", func_sym)))?;
                    let func_ref = jit_module
                        .get_name(&known_func.name)
                        .ok_or_else(|| NativeError::Unimplemented(format!("Function symbol {:?} (name: '{}') not found in JIT module", func_sym, known_func.name)))?;
                    let func_id = match func_ref {
                         cranelift_module::FuncOrDataId::Func(id) => id,
                         cranelift_module::FuncOrDataId::Data(_) => return Err(NativeError::TypeError("Expected function symbol, found data".to_string())),
                    };
                    let local_func_ref = jit_module.declare_func_in_func(func_id, builder.func);
                    let call = builder.ins().call(local_func_ref, &arg_vals);
                    let results = builder.inst_results(call);
                    // Handle void return
                    if results.is_empty() {
                        // Return dummy I8 value for void calls used in expressions
                        Ok(builder.ins().iconst(types::I8, 0))
                    } else {
                        Ok(results[0])
                    }
                }
                Operand::Var(_) | Operand::Const(_) => {
                    // Indirect call (function pointer or closure)
                    let callee_val = translate_operand(builder, ctx, func, Some(&callee_hir_ty), jit_module, isa, layout_computer)?;

                    // Check if the HIR type is a function pointer
                    if let HirType::FunctionPointer(param_types, ret_type) = callee_hir_ty {
                        // --- Indirect Call Logic ---
                        let merge_block = builder.create_block();
                        let closure_block = builder.create_block();
                        let regular_block = builder.create_block();
                        
                        let pointer_type = isa.pointer_type();
                        
                        // Determine result type for merge block parameter
                        let result_cl_ty = translate_type(&*ret_type, isa.as_ref(), ctx, layout_computer)?
                            .unwrap_or(types::I8); // Use I8 dummy for ZST return
                        // Only add merge param if function actually returns a value
                        if result_cl_ty != types::I8 || !ret_type.is_never() { 
                             builder.append_block_param(merge_block, result_cl_ty);
                        }

                        // --- Check Tag on callee_val --- 
                        let tag_mask = builder.ins().iconst(pointer_type, 1);
                        let tag = builder.ins().band(callee_val, tag_mask);
                        let is_closure = builder.ins().icmp_imm(cranelift_codegen::ir::condcodes::IntCC::Equal, tag, 1);

                        builder.ins().brif(is_closure, closure_block, &[], regular_block, &[]);

                        // --- Closure Call Block --- 
                        builder.switch_to_block(closure_block);
                        {
                            let one = builder.ins().iconst(pointer_type, 1);
                            let closure_ref_ptr = builder.ins().bxor(callee_val, one);
                            let func_ptr_val = builder.ins().load(pointer_type, MemFlags::trusted(), closure_ref_ptr, 0);
                            let env_offset = i32::try_from(pointer_type.bytes()).expect("Pointer size should fit in i32 for load offset");
                            let env_handle_val = builder.ins().load(pointer_type, MemFlags::trusted(), closure_ref_ptr, env_offset);
                            
                            let mut final_call_args = Vec::with_capacity(arg_vals.len() + 1);
                            final_call_args.push(env_handle_val);
                            final_call_args.extend_from_slice(&arg_vals);

                            let mut sig_param_abis = Vec::with_capacity(param_types.len() + 1);
                            sig_param_abis.push(AbiParam::new(pointer_type));
                            for hir_ty in &param_types {
                                if let Some(cl_ty) = translate_type(hir_ty, isa.as_ref(), ctx, layout_computer)? {
                                     sig_param_abis.push(AbiParam::new(cl_ty));
                                }
                            }
                            
                            let sig_return_abis = if result_cl_ty != types::I8 || !ret_type.is_never() {
                                vec![AbiParam::new(result_cl_ty)]
                            } else {
                                vec![]
                            };
                            
                            let signature = Signature { params: sig_param_abis, returns: sig_return_abis, call_conv: isa.default_call_conv() };
                            let sig_ref = builder.import_signature(signature);
                            
                            let call = builder.ins().call_indirect(sig_ref, func_ptr_val, &final_call_args);
                            let results = builder.inst_results(call);
                            if results.is_empty() {
                                builder.ins().jump(merge_block, &[]); // Jump with no args for void return
                            } else {
                                let result = results[0];
                                builder.ins().jump(merge_block, &[result]);
                            }
                        }

                        // --- Regular Function Pointer Call Block --- 
                        builder.switch_to_block(regular_block);
                        {
                            let func_ptr_val = callee_val;
                            let mut sig_param_abis = Vec::with_capacity(param_types.len());
                            for hir_ty in &param_types {
                                if let Some(cl_ty) = translate_type(hir_ty, isa.as_ref(), ctx, layout_computer)? {
                                     sig_param_abis.push(AbiParam::new(cl_ty));
                                }
                            }
                            
                             let sig_return_abis = if result_cl_ty != types::I8 || !ret_type.is_never() {
                                 vec![AbiParam::new(result_cl_ty)]
                             } else {
                                 vec![]
                             };
                             
                             let signature = Signature { params: sig_param_abis, returns: sig_return_abis, call_conv: isa.default_call_conv() };
                             let sig_ref = builder.import_signature(signature);
                            
                            let call = builder.ins().call_indirect(sig_ref, func_ptr_val, &arg_vals);
                            let results = builder.inst_results(call);
                            if results.is_empty() {
                                builder.ins().jump(merge_block, &[]);
                            } else {
                                let result = results[0];
                                builder.ins().jump(merge_block, &[result]);
                            }
                        }

                        // --- Merge Block --- 
                        builder.switch_to_block(merge_block);
                        builder.seal_block(closure_block);
                        builder.seal_block(regular_block);
                        builder.seal_block(merge_block);

                        if result_cl_ty != types::I8 || !ret_type.is_never() {
                             Ok(builder.block_params(merge_block)[0])
                        } else {
                             // Return dummy for void
                             Ok(builder.ins().iconst(types::I8, 0))
                        }

                    } else {
                        Err(NativeError::TypeError(format!("Operand used as callee is not a function pointer type, found {:?}", callee_hir_ty)))
                    }
                }
            }
        }
        HirValue::Aggregate { kind, fields } => {
            let aggregate_hir_ty = match kind {
                AggregateKind::Tuple => HirType::Tuple(fields.iter().map(|op| ctx.get_operand_type(op).unwrap_or(HirType::Never)).collect()),
                AggregateKind::Struct(s_sym) => HirType::Adt(*s_sym),
                AggregateKind::Array => {
                     if fields.is_empty() {
                         // Represent empty array like Unit (ZST)
                         return Ok(builder.ins().iconst(types::I8, 0)); // Return dummy ZST value
                     } else {
                         let elem_ty = ctx.get_operand_type(&fields[0]).unwrap_or(HirType::Never);
                         HirType::Array(Arc::new(elem_ty), fields.len())
                     }
                 }
                AggregateKind::EnumVariant(v_sym) => {
                     let (enum_def, _) = ctx.get_enum_and_variant_def(*v_sym)
                         .ok_or_else(|| NativeError::TypeError(format!("Variant symbol {:?} not found", v_sym)))?;
                     HirType::Adt(enum_def.symbol)
                 }
            };

            // Handle ZST cases (empty tuple/array)
            if is_zst(&aggregate_hir_ty, layout_computer, ctx) { // Use free function
                 return Ok(builder.ins().iconst(types::I8, 0));
            }

            let layout = get_layout(&aggregate_hir_ty, layout_computer, ctx)?;
            let size_bytes = get_size_bytes(&layout);
            let stack_slot = builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, size_bytes as u32, 0));
            let base_ptr = builder.ins().stack_addr(isa.pointer_type(), stack_slot, 0);

            match kind {
                 AggregateKind::EnumVariant(variant_symbol) => {
                     let (enum_def, variant_def) = ctx.get_enum_and_variant_def(*variant_symbol)
                         .ok_or_else(|| NativeError::TypeError(format!("Variant symbol {:?} not found", variant_symbol)))?;
                     let enum_symbol = enum_def.symbol;

                     let discriminant_value = enum_def.variants.iter().position(|v| v.symbol == *variant_symbol)
                         .ok_or_else(|| NativeError::TypeError("Variant symbol not found within its own enum definition?".to_string()))?;
                     let discriminant_cl_type = get_enum_discriminant_type(enum_symbol, layout_computer, ctx)?;
                     let discriminant_offset = get_enum_discriminant_offset_bytes(enum_symbol, layout_computer, ctx)?;
                     let discriminant_val = builder.ins().iconst(discriminant_cl_type, discriminant_value as i64);
                     builder.ins().store(MemFlags::trusted(), discriminant_val, base_ptr, discriminant_offset as i32);

                     let payload_offset_bytes = get_variant_payload_offset_bytes(enum_symbol, *variant_symbol, layout_computer, ctx)?;
                     let payload_hir_type = HirType::Tuple(variant_def.fields.clone());
                     let repc_payload_type_with_layout = get_repc_type_with_layout(&payload_hir_type, layout_computer, ctx)?;

                     for (i, field_operand) in fields.iter().enumerate() {
                         let field_val = translate_operand(builder, ctx, field_operand, None, jit_module, isa, layout_computer)?;
                         // Skip storing ZST fields
                         if ctx.get_operand_type(field_operand).map_or(false, |t| is_zst(&t, layout_computer, ctx)) { continue; } // Use free function

                         let field_offset_in_payload = get_field_offset_bytes(&repc_payload_type_with_layout, i)?;
                         let final_field_offset = payload_offset_bytes + field_offset_in_payload;
                         builder.ins().store(MemFlags::trusted(), field_val, base_ptr, final_field_offset as i32);
                     }
                 }
                 _ => { // Handle Tuple, Struct, Array
                     let computed_type_with_layout = get_repc_type_with_layout(&aggregate_hir_ty, layout_computer, ctx)?;

                     for (i, field_operand) in fields.iter().enumerate() {
                          let field_val = translate_operand(builder, ctx, field_operand, None, jit_module, isa, layout_computer)?;
                          // Skip storing ZST fields
                          if ctx.get_operand_type(field_operand).map_or(false, |t| is_zst(&t, layout_computer, ctx)) { continue; } // Use free function

                          let field_offset_bytes = match kind {
                               AggregateKind::Array => {
                                   if let HirType::Array(elem_ty, _) = &aggregate_hir_ty {
                                        let elem_layout = get_layout(elem_ty, layout_computer, ctx)?;
                                        let elem_size = get_size_bytes(&elem_layout);
                                        (i as u64) * elem_size
                                   } else {
                                        return Err(NativeError::TypeError("Mismatched type for Array aggregate".to_string()));
                                   }
                               }
                               _ => { // Struct or Tuple: Use repc field offset
                                   get_field_offset_bytes(&computed_type_with_layout, i)?
                               }
                          };
                          builder.ins().store(MemFlags::trusted(), field_val, base_ptr, field_offset_bytes as i32);
                      }
                  }
            }
            Ok(base_ptr)
        }
        HirValue::Project { base, projection } => {
            let base_hir_ty = ctx.get_operand_type(base).ok_or_else(|| NativeError::TypeError("Cannot determine type of base for projection".to_string()))?;
            // If base is ZST, projection result is also ZST
            if is_zst(&base_hir_ty, layout_computer, ctx) { // Use free function
                return Ok(builder.ins().iconst(types::I8, 0));
            }
            let base_ptr = translate_operand(builder, ctx, base, Some(&base_hir_ty), jit_module, isa, layout_computer)?;

            match projection {
                ProjectionKind::Field(field_symbol) => {
                    let struct_symbol = match &base_hir_ty {
                        HirType::Adt(s) => *s,
                        _ => return Err(NativeError::TypeError("Projection base is not an ADT".to_string()))
                    };
                    let struct_def = ctx.get_struct_def(struct_symbol).ok_or_else(|| NativeError::TypeError("Struct definition not found".to_string()))?;
                    let field_index = struct_def.fields.iter().position(|(sym, _, _)| *sym == *field_symbol)
                        .ok_or_else(|| NativeError::TypeError("Field symbol not found in struct def".to_string()))?;
                    
                    let field_hir_type = &struct_def.fields[field_index].2;
                    // If projecting a ZST field, return dummy value
                    if is_zst(field_hir_type, layout_computer, ctx) { // Use free function
                        return Ok(builder.ins().iconst(types::I8, 0));
                    }

                    let computed_type = get_repc_type_with_layout(&base_hir_ty, layout_computer, ctx)?;
                    let field_offset_bytes = get_field_offset_bytes(&computed_type, field_index)?;
                    let field_cl_type = translate_type(field_hir_type, isa.as_ref(), ctx, layout_computer)?.unwrap(); // Should not be None here

                    let value = builder.ins().load(field_cl_type, MemFlags::trusted(), base_ptr, field_offset_bytes as i32);
                    Ok(value)
                }
                ProjectionKind::TupleIndex(index) => {
                     let field_hir_type = match &base_hir_ty {
                         HirType::Tuple(elements) => elements.get(*index as usize).ok_or_else(|| NativeError::TypeError("Tuple index out of bounds".to_string()))?,
                         _ => return Err(NativeError::TypeError("Projection base is not a Tuple".to_string()))
                     };
                    // If projecting a ZST field, return dummy value
                    if is_zst(field_hir_type, layout_computer, ctx) { // Use free function
                        return Ok(builder.ins().iconst(types::I8, 0));
                    }

                     let computed_type = get_repc_type_with_layout(&base_hir_ty, layout_computer, ctx)?;
                     let field_offset_bytes = get_field_offset_bytes(&computed_type, *index as usize)?;
                     let field_cl_type = translate_type(field_hir_type, isa.as_ref(), ctx, layout_computer)?.unwrap(); // Should not be None here

                    let value = builder.ins().load(field_cl_type, MemFlags::trusted(), base_ptr, field_offset_bytes as i32);
                    Ok(value)
                }
                ProjectionKind::ArrayIndex(index_operand) => {
                    let elem_hir_ty = match &base_hir_ty {
                        HirType::Array(elem_ty, _) => elem_ty,
                        _ => return Err(NativeError::TypeError("Projection base is not an Array".to_string()))
                    };
                    // If projecting a ZST element, return dummy value
                    if is_zst(elem_hir_ty, layout_computer, ctx) { // Use free function
                        return Ok(builder.ins().iconst(types::I8, 0));
                    }

                    let elem_layout = get_layout(elem_hir_ty, layout_computer, ctx)?;
                    let elem_size_bytes = get_size_bytes(&elem_layout);
                    let index_val = translate_operand(builder, ctx, index_operand, Some(&HirType::Primitive(ResolvePrimitiveType::U64)), jit_module, isa, layout_computer)?;

                    // Bounds check (TODO: Make this optional)
                    if let HirType::Array(_, size) = &base_hir_ty {
                        let array_len_val = builder.ins().iconst(types::I64, *size as i64);
                        let is_in_bounds = builder.ins().icmp(cranelift_codegen::ir::condcodes::IntCC::UnsignedLessThan, index_val, array_len_val);
                        builder.ins().trapz(is_in_bounds, TrapCode::HEAP_OUT_OF_BOUNDS);
                    }

                    let elem_size_val = builder.ins().iconst(isa.pointer_type(), elem_size_bytes as i64);
                    let offset_val_from_index = builder.ins().imul(index_val, elem_size_val);
                    let elem_cl_type = translate_type(elem_hir_ty, isa.as_ref(), ctx, layout_computer)?.unwrap(); // Should not be None here
                    let field_addr = builder.ins().iadd(base_ptr, offset_val_from_index);

                    let value = builder.ins().load(elem_cl_type, MemFlags::trusted(), field_addr, 0);
                    return Ok(value);
                }
                 ProjectionKind::Downcast(variant_symbol) => {
                     let enum_symbol = match &base_hir_ty {
                         HirType::Adt(s) => *s,
                         _ => return Err(NativeError::TypeError("Downcast projection base is not an ADT (enum)".to_string()))
                     };
                     let (enum_def, _variant_def) = ctx.get_enum_and_variant_def(*variant_symbol)
                         .ok_or_else(|| NativeError::TypeError(format!("Variant symbol {:?} not found for downcast", variant_symbol)))?;
                     let expected_discriminant = enum_def.variants.iter().position(|v| v.symbol == *variant_symbol)
                         .ok_or_else(|| NativeError::TypeError("Variant symbol not found within its own enum definition?".to_string()))?;

                     let discr_cl_ty = get_enum_discriminant_type(enum_symbol, layout_computer, ctx)?;
                     let discr_offset = get_enum_discriminant_offset_bytes(enum_symbol, layout_computer, ctx)?;
                     let loaded_discr = builder.ins().load(discr_cl_ty, MemFlags::trusted(), base_ptr, discr_offset as i32);

                     let expected_discr_val = builder.ins().iconst(discr_cl_ty, expected_discriminant as i64);
                     let comparison_result = builder.ins().icmp(cranelift_codegen::ir::condcodes::IntCC::Equal, loaded_discr, expected_discr_val);
                     // Use HEAP_OUT_OF_BOUNDS (all caps) as it is valid here.
                     builder.ins().trapz(comparison_result, TrapCode::HEAP_OUT_OF_BOUNDS); 
                     return Ok(base_ptr); // Return the original base pointer after successful check
                 }
            }
        }
        HirValue::Closure { function_symbol, captures } => {
            let func_ptr_val = translate_operand(builder, ctx, &Operand::Global(*function_symbol), None, jit_module, isa, layout_computer)?;
            let pointer_type = isa.pointer_type();
            let mut capture_items_on_stack : Vec<(CaptureType, Value)> = Vec::with_capacity(captures.len());

            for captured_operand in captures {
                let hir_ty = ctx.get_operand_type(captured_operand).ok_or_else(||
                    NativeError::TypeError(format!("Cannot determine type of captured operand: {:?}", captured_operand))
                )?;
                let operand_val = translate_operand(builder, ctx, captured_operand, Some(&hir_ty), jit_module, isa, layout_computer)?;
                let operand_cl_type = builder.func.dfg.value_type(operand_val);

                let (capture_type, data_val_usize) = match hir_ty {
                    HirType::Primitive(prim) => match prim {
                        ResolvePrimitiveType::I64 | ResolvePrimitiveType::U64 => {
                            let val_usize = if operand_cl_type == pointer_type { operand_val }
                                            else if operand_cl_type.bits() > pointer_type.bits() { builder.ins().ireduce(pointer_type, operand_val) }
                                            else { builder.ins().uextend(pointer_type, operand_val) };
                            (CaptureType::U64, val_usize)
                        }
                        ResolvePrimitiveType::F64 => {
                            let val_bits = builder.ins().bitcast(types::I64, MemFlags::trusted(), operand_val);
                            let val_usize = if pointer_type.bits() < 64 { builder.ins().ireduce(pointer_type, val_bits) }
                                            else if pointer_type.bits() > 64 { builder.ins().uextend(pointer_type, val_bits) }
                                            else { val_bits };
                            (CaptureType::F64, val_usize)
                        }
                        // Other primitives need explicit handling if capture is needed (e.g., Bool, Char, smaller ints)
                        _ => return Err(NativeError::Unimplemented(format!("Capturing primitive type {:?} not yet supported", prim)))
                    },
                    // These types are expected to be GC Handles (pointers)
                    HirType::Adt(_) | HirType::Tuple(_) | HirType::Array(_, _) | HirType::FunctionPointer(_, _) |
                    HirType::Primitive(ResolvePrimitiveType::String) // String is handled as a pointer (Handle<StringRef>)
                     => {
                        if operand_cl_type != pointer_type { return Err(NativeError::TypeError(format!("Expected pointer type for captured handle, got {:?}", operand_cl_type))); }
                        (CaptureType::Handle, operand_val)
                    }
                    HirType::Never => return Err(NativeError::TypeError("Cannot capture value of type Never".to_string())),
                };
                capture_items_on_stack.push((capture_type, data_val_usize));
            }

            let capture_item_size = size_of::<CaptureItem>() as u32;
            let total_size = capture_item_size * (captures.len() as u32);
            let stack_slot = builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, total_size, 0));
            let capture_array_ptr = builder.ins().stack_addr(pointer_type, stack_slot, 0);
            let capture_type_cl_type = types::I8; // CaptureType is u8
            let data_offset_within_item = size_of::<CaptureType>() as i32;

            for (i, (cap_ty, data_val)) in capture_items_on_stack.iter().enumerate() {
                let current_offset = (i as i32) * (capture_item_size as i32);
                let ty_val = builder.ins().iconst(capture_type_cl_type, *cap_ty as i64);
                // Store CaptureType
                builder.ins().store(MemFlags::trusted(), ty_val, capture_array_ptr, current_offset);
                // Store data field (already pointer_type)
                builder.ins().store(MemFlags::trusted(), *data_val, capture_array_ptr, current_offset + data_offset_within_item);
            }

            let local_alloc_func_ref = declare_alloc_closure_fn(jit_module, builder.func, pointer_type)?;
            let captures_len_val = builder.ins().iconst(pointer_type, captures.len() as i64);
            let call_args = &[func_ptr_val, capture_array_ptr, captures_len_val];
            let call_inst = builder.ins().call(local_alloc_func_ref, call_args);
            let results = builder.inst_results(call_inst);

            if results.is_empty() {
                return Err(NativeError::CompilationError("Closure allocation call returned no value".to_string()));
            }
            let result_handle_val = results[0]; // This is the tagged Handle<ClosureRef>

            // Push the resulting Handle onto the shadow stack
            let push_fn_ref = declare_shadow_stack_push_fn(jit_module, builder.func, pointer_type)?;
            builder.ins().call(push_fn_ref, &[result_handle_val]);
            ctx.increment_shadow_stack_push_count();

            Ok(result_handle_val)
        }
    }
}