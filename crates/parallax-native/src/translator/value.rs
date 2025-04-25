// Handles translation of HIR values (Use, Call, Aggregate, Project, Closure).
use crate::NativeError;
use crate::translator::context::TranslationContext;
use crate::translator::operand::translate_operand;
use crate::translator::types::translate_type;
use crate::translator::helpers::{declare_shadow_stack_push_fn, declare_alloc_object_fn, declare_alloc_closure_fn};
use parallax_gc::layout::helpers::{get_size_bytes, get_alignment_bytes, get_struct_field_offset_bytes, get_enum_discriminant_info, get_discriminant_cl_type, get_enum_variant_info, get_array_info};
use cranelift_codegen::ir::{Value, InstBuilder, types, MemFlags, TrapCode, Signature, AbiParam};
use cranelift_codegen::isa::TargetIsa;
use cranelift_frontend::FunctionBuilder;
use cranelift_jit::JITModule;
use cranelift_module::Module;
use parallax_gc::layout::LayoutError;
use parallax_hir::hir::{HirValue, Operand, HirType, AggregateKind, ProjectionKind, PrimitiveType};
use parallax_hir::Symbol; // Import Symbol
use std::sync::Arc;
use parallax_gc::{LayoutDescriptor, DescriptorIndex};
use cranelift_codegen::ir::StackSlotData;
use cranelift_codegen::ir::StackSlotKind;
use std::mem; // Add missing import
use std::collections::{HashSet, HashMap};
use memoffset::offset_of; // Import offset_of

/// Helper to check if a type is zero-sized based on descriptor.
/// Takes an immutable context to avoid borrow checker issues when called inside loops
/// where the context might be mutably borrowed elsewhere (e.g., by translate_operand).
/// If the descriptor index isn't cached, it conservatively assumes the type is *not* ZST.
fn is_zst(hir_type: &HirType, ctx: &TranslationContext) -> bool {
    // Try to get descriptor index immutably from cache
    let maybe_idx = ctx.find_descriptor_index_in_cache(hir_type);

    match maybe_idx {
        Some(idx) => {
            // If index found, get descriptor (immutable borrow)
            if let Some(desc) = ctx.get_descriptor_by_index(idx) {
                get_size_bytes(desc) == 0
            } else {
                false // Descriptor not found for cached index? Should be rare.
            }
        },
        None => false, // Not cached, assume not ZST to be safe without mutable borrow
    }
}

/// Translate an HIR value to Cranelift IR
pub fn translate_value<'ctx>(
    builder: &mut FunctionBuilder,
    ctx: &mut TranslationContext<'ctx>,
    value: &HirValue,
    jit_module: &mut JITModule,
    isa: &Arc<dyn TargetIsa>,
) -> Result<Value, NativeError> {
    let pointer_type = isa.pointer_type();
    match value {
        HirValue::Use(operand) => {
            let expected_hir_ty = ctx.get_operand_type(operand);
            translate_operand(builder, ctx, operand, expected_hir_ty.as_ref(), jit_module, isa)
        }
        HirValue::Call { func, args } => {
            // Translate arguments first
            let mut arg_vals = Vec::with_capacity(args.len());
            for arg in args {
                let arg_type = ctx.get_operand_type(arg);
                let arg_val = translate_operand(builder, ctx, arg, arg_type.as_ref(), jit_module, isa)?;
                // Only add non-ZST arguments to the call list
                if arg_type.map_or(false, |t| !is_zst(&t, ctx)) {
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
                    let callee_val = translate_operand(builder, ctx, func, Some(&callee_hir_ty), jit_module, isa)?;

                    // Check if the HIR type is a function pointer
                    if let HirType::FunctionPointer(param_types, ret_type) = callee_hir_ty {
                        // --- Indirect Call Logic ---
                        let merge_block = builder.create_block();
                        let closure_block = builder.create_block();
                        let regular_block = builder.create_block();
                        
                        let result_cl_ty = translate_type((*ret_type).clone(), isa.as_ref(), ctx)?
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
                                if let Some(cl_ty) = translate_type(hir_ty.clone(), isa.as_ref(), ctx)? {
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
                                if let Some(cl_ty) = translate_type(hir_ty.clone(), isa.as_ref(), ctx)? {
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
                         HirType::Primitive(PrimitiveType::Unit) // Treat empty array as Unit for type calculation
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
            let is_zst_agg = is_zst(&aggregate_hir_ty, ctx);
            if is_zst_agg {
                return Ok(builder.ins().iconst(types::I8, 0));
            }

            // --- Determine Allocation Strategy --- //
            let needs_heap = { // Create a scope for the visiting set
                let mut visiting = HashSet::new();
                ctx.type_needs_heap_allocation(&aggregate_hir_ty, &mut visiting)
            };

            // 1. Pre-calculate field types to avoid borrow issues later
            let field_types: Vec<_> = fields.iter()
                .map(|op| ctx.get_operand_type(op).unwrap_or(HirType::Never))
                .collect();

            // 2. Get Descriptor Index
            let descriptor_index = ctx.get_or_create_descriptor_index(&aggregate_hir_ty)?;

            // 3. Get Size/Alignment from descriptor
            let aggregate_descriptor = ctx.get_descriptor_by_index(descriptor_index).cloned()
                .ok_or_else(|| NativeError::LayoutError(LayoutError::Other(format!("Descriptor index {} not found after creation for type {:?}", descriptor_index, aggregate_hir_ty))))?;
            let size_bytes = get_size_bytes(&aggregate_descriptor);
            let align_bytes = get_alignment_bytes(&aggregate_descriptor);

            if needs_heap {
                // --- Heap Allocation Path --- //
                // 4. Create Temporary Stack Buffer for Initial Data
                let init_data_slot = builder.create_sized_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot, size_bytes as u32, 0 // Use calculated size
                ));
                let init_data_ptr_val = builder.ins().stack_addr(pointer_type, init_data_slot, 0);

                // 5. Translate Fields and Store into Stack Buffer
                match kind {
                    AggregateKind::EnumVariant(variant_symbol) => {
                        let (enum_def, variant_def) = ctx.get_enum_and_variant_def(*variant_symbol).unwrap();
                        let variant_index = enum_def.variants.iter().position(|v| v.symbol == *variant_symbol)
                            .ok_or_else(|| NativeError::TypeError(format!("Variant symbol {:?} not found in its own enum definition", variant_symbol)))?;

                        // Use the descriptor already fetched for the aggregate
                        let enum_descriptor = aggregate_descriptor;
                        let discriminant_cl_type = ctx.get_discriminant_cl_type(descriptor_index)?;
                        let (_discr_size, discriminant_offset) = ctx.get_enum_discriminant_info(descriptor_index)?;
                        let discriminant_val = builder.ins().iconst(discriminant_cl_type, variant_index as i64);
                        builder.ins().store(MemFlags::trusted(), discriminant_val, init_data_ptr_val, discriminant_offset as i32);

                        if !variant_def.fields.is_empty() {
                            // Pass variant_index directly (usize), ignore first element (discriminant) in return tuple
                            let (_, payload_base_offset, payload_desc_idx) = ctx.get_enum_variant_info(descriptor_index, variant_index)?;
                            let payload_descriptor = ctx.get_descriptor_by_index(payload_desc_idx).cloned()
                                .ok_or_else(|| NativeError::LayoutError(LayoutError::Other(format!("Payload descriptor index {} not found for variant {}", payload_desc_idx, variant_index))))?;

                            for (field_index, (field_operand, field_hir_type)) in fields.iter().zip(field_types.iter()).enumerate() {
                                // Skip ZST fields (using pre-calculated type)
                                if !is_zst(field_hir_type, ctx) {
                                    let field_val = translate_operand(builder, ctx, field_operand, Some(field_hir_type), jit_module, isa)?;
                                    // Extract offset before adding
                                    let (field_offset_in_payload_offset, _) = ctx.get_aggregate_field_info(payload_desc_idx, field_index)?;
                                    let final_field_offset = payload_base_offset + field_offset_in_payload_offset;
                                    builder.ins().store(MemFlags::trusted(), field_val, init_data_ptr_val, final_field_offset as i32);
                                }
                            }
                        }
                    }
                    _ => { // Struct, Tuple, Array
                         let struct_descriptor = aggregate_descriptor;
                         for (i, (field_operand, field_hir_type)) in fields.iter().zip(field_types.iter()).enumerate() {
                              // Skip ZST fields (using pre-calculated type)
                              if !is_zst(field_hir_type, ctx) {
                                  let field_val = translate_operand(builder, ctx, field_operand, Some(field_hir_type), jit_module, isa)?;
                                  let field_offset_bytes = get_struct_field_offset_bytes(&struct_descriptor, i)?;
                                  builder.ins().store(MemFlags::trusted(), field_val, init_data_ptr_val, field_offset_bytes as i32);
                              }
                          }
                    }
                }

                // 6. Call parallax_alloc_object FFI
                let alloc_fn_ref = declare_alloc_object_fn(jit_module, builder.func, pointer_type)?;
                let descriptor_index_val = builder.ins().iconst(pointer_type, descriptor_index as i64);
                let call_args = &[descriptor_index_val, init_data_ptr_val];
                let call_inst = builder.ins().call(alloc_fn_ref, call_args);
                let results = builder.inst_results(call_inst);
                if results.is_empty() {
                    return Err(NativeError::CompilationError("parallax_alloc_object call returned no value".to_string()));
                }
                let result_handle = results[0];

                // 7. Push Handle onto Shadow Stack
                let push_fn_ref = declare_shadow_stack_push_fn(jit_module, builder.func, pointer_type)?;
                builder.ins().call(push_fn_ref, &[result_handle]);
                ctx.increment_shadow_stack_push_count();

                // 8. Return Handle<GcObject>
                Ok(result_handle)

            } else {
                // --- Stack Allocation Path --- //
                // 4. Create Stack Slot
                let stack_slot = builder.create_sized_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot, size_bytes as u32, align_bytes as u8
                ));
                let base_ptr = builder.ins().stack_addr(pointer_type, stack_slot, 0);

                // 5. Translate Fields and Store into Stack Slot
                match &kind {
                    AggregateKind::EnumVariant(variant_symbol) => {
                        let (enum_def, variant_def) = ctx.get_enum_and_variant_def(*variant_symbol).unwrap();
                        let variant_index = enum_def.variants.iter().position(|v| v.symbol == *variant_symbol)
                            .ok_or_else(|| NativeError::TypeError(format!("Variant symbol {:?} not found in its own enum definition", variant_symbol)))?;

                        // Use the descriptor already fetched for the aggregate
                        let enum_descriptor = aggregate_descriptor;
                        let discriminant_cl_type = ctx.get_discriminant_cl_type(descriptor_index)?;
                        let (_discr_size, discriminant_offset) = ctx.get_enum_discriminant_info(descriptor_index)?;
                        let discriminant_val = builder.ins().iconst(discriminant_cl_type, variant_index as i64);
                        builder.ins().store(MemFlags::trusted(), discriminant_val, base_ptr, discriminant_offset as i32);

                        if !variant_def.fields.is_empty() {
                            // Pass variant_index directly (usize), ignore first element (discriminant) in return tuple
                            let (_, payload_base_offset, payload_desc_idx) = ctx.get_enum_variant_info(descriptor_index, variant_index)?;
                            let payload_descriptor = ctx.get_descriptor_by_index(payload_desc_idx).cloned()
                                .ok_or_else(|| NativeError::LayoutError(LayoutError::Other(format!("Payload descriptor index {} not found for variant {}", payload_desc_idx, variant_index))))?;
                            for (field_index, (field_operand, field_hir_type)) in fields.iter().zip(field_types.iter()).enumerate() {
                                if !is_zst(field_hir_type, ctx) {
                                    let field_val = translate_operand(builder, ctx, field_operand, Some(field_hir_type), jit_module, isa)?;
                                    // Extract offset before adding
                                    let (field_offset_in_payload_offset, _) = ctx.get_aggregate_field_info(payload_desc_idx, field_index)?;
                                    let final_field_offset = payload_base_offset + field_offset_in_payload_offset;
                                    builder.ins().store(MemFlags::trusted(), field_val, base_ptr, final_field_offset as i32);
                                }
                            }
                        }
                    }
                    AggregateKind::Array => {
                        // Remove reliance on get_type_from_descriptor_index
                        // Get element descriptor index directly from array descriptor
                        let (elem_desc_idx, _array_len) = get_array_info(&aggregate_descriptor)?;
                        let elem_descriptor = ctx.get_descriptor_by_index(elem_desc_idx).cloned()
                            .ok_or_else(|| NativeError::LayoutError(LayoutError::Other(format!("Element descriptor index {} not found", elem_desc_idx))))?;

                        // Check ZST based on descriptor size and skip if fields are empty
                        if get_size_bytes(&elem_descriptor) > 0 && !fields.is_empty() {
                            // Use the aggregate_descriptor which should be LayoutDescriptor::Array
                            let elem_size = get_size_bytes(&elem_descriptor);
                            let elem_align = get_alignment_bytes(&elem_descriptor);
                            let stride = (elem_size + elem_align - 1) & !(elem_align - 1); // Align up
                            let stride_val = builder.ins().iconst(pointer_type, stride as i64);
                            for (i, field_operand) in fields.iter().enumerate() {
                                // Pass None for expected_hir_ty to translate_operand
                                let field_val = translate_operand(builder, ctx, field_operand, None, jit_module, isa)?;
                                let index_val = builder.ins().iconst(pointer_type, i as i64);
                                let offset_val = builder.ins().imul(index_val, stride_val);
                                let field_addr = builder.ins().iadd(base_ptr, offset_val);
                                builder.ins().store(MemFlags::trusted(), field_val, field_addr, 0);
                            }
                        }
                    }
                    _ => { // Struct, Tuple
                         let struct_descriptor = &aggregate_descriptor;
                         for (i, (field_operand, field_hir_type)) in fields.iter().zip(field_types.iter()).enumerate() {
                              if !is_zst(field_hir_type, ctx) {
                                  let field_val = translate_operand(builder, ctx, field_operand, Some(field_hir_type), jit_module, isa)?;
                                  let field_offset_bytes = get_struct_field_offset_bytes(struct_descriptor, i)?;
                                  builder.ins().store(MemFlags::trusted(), field_val, base_ptr, field_offset_bytes as i32);
                              }
                          }
                    }
                }

                Ok(base_ptr) // Return pointer to stack allocation
            }
        }
        HirValue::Project { base, projection } => {
            let base_hir_ty = ctx.get_operand_type(base).ok_or_else(|| NativeError::TypeError("Cannot determine type of base for projection".to_string()))?;
            // If base is ZST, projection result is also ZST
            if is_zst(&base_hir_ty, ctx) {
                return Ok(builder.ins().iconst(types::I8, 0));
            }
            let base_ptr = translate_operand(builder, ctx, base, Some(&base_hir_ty), jit_module, isa)?;

            // Get descriptor for the base type
            let base_desc_idx = ctx.get_or_create_descriptor_index(&base_hir_ty)?;
            let base_descriptor = ctx.get_descriptor_by_index(base_desc_idx)
                .ok_or_else(|| NativeError::LayoutError(LayoutError::Other(format!("Descriptor index {} not found for projection base type {:?}", base_desc_idx, base_hir_ty))))?;

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
                    if is_zst(field_hir_type, ctx) { return Ok(builder.ins().iconst(types::I8, 0)); }

                    // Use descriptor to get offset
                    let field_offset_bytes = get_struct_field_offset_bytes(base_descriptor, field_index)?;
                    let field_cl_type = translate_type(field_hir_type.clone(), isa.as_ref(), ctx)?.unwrap();

                    let value = builder.ins().load(field_cl_type, MemFlags::trusted(), base_ptr, field_offset_bytes as i32);
                    Ok(value)
                }
                ProjectionKind::TupleIndex(index) => {
                     let field_hir_type = match &base_hir_ty {
                         HirType::Tuple(elements) => elements.get(*index as usize).ok_or_else(|| NativeError::TypeError("Tuple index out of bounds".to_string()))?,
                         _ => return Err(NativeError::TypeError("Projection base is not a Tuple".to_string()))
                     };
                    if is_zst(field_hir_type, ctx) { return Ok(builder.ins().iconst(types::I8, 0)); }

                     // Tuples use LayoutDescriptor::Struct, use descriptor to get offset
                     let field_offset_bytes = get_struct_field_offset_bytes(base_descriptor, *index as usize)?;
                     let field_cl_type = translate_type(field_hir_type.clone(), isa.as_ref(), ctx)?.unwrap();

                    let value = builder.ins().load(field_cl_type, MemFlags::trusted(), base_ptr, field_offset_bytes as i32);
                    Ok(value)
                }
                ProjectionKind::ArrayIndex(index_operand) => {
                    let (elem_desc_idx, array_len) = get_array_info(base_descriptor)?;
                    let elem_descriptor = ctx.get_descriptor_by_index(elem_desc_idx)
                        .ok_or_else(|| NativeError::LayoutError(LayoutError::Other(format!("Element descriptor index {} not found", elem_desc_idx))))?;
                    let elem_hir_ty = match &base_hir_ty {
                        HirType::Array(elem_ty, _) => elem_ty.as_ref(), // Get inner type
                        _ => unreachable!(), // Should be caught by get_array_info
                    };

                    if is_zst(elem_hir_ty, ctx) { return Ok(builder.ins().iconst(types::I8, 0)); }

                    let elem_size_bytes = get_size_bytes(elem_descriptor);
                    let elem_align_bytes = get_alignment_bytes(elem_descriptor);
                    let stride = (elem_size_bytes + elem_align_bytes - 1) & !(elem_align_bytes - 1);

                    let index_val = translate_operand(builder, ctx, index_operand, Some(&HirType::Primitive(PrimitiveType::U64)), jit_module, isa)?;

                    // Bounds check (TODO: Make this optional)
                    let array_len_val = builder.ins().iconst(types::I64, array_len as i64);
                    let is_in_bounds = builder.ins().icmp(cranelift_codegen::ir::condcodes::IntCC::UnsignedLessThan, index_val, array_len_val);
                    builder.ins().trapz(is_in_bounds, TrapCode::HEAP_OUT_OF_BOUNDS);

                    let stride_val = builder.ins().iconst(pointer_type, stride as i64);
                    let index_val_ptr_sized = if builder.func.dfg.value_type(index_val) == pointer_type {
                         index_val
                     } else {
                         builder.ins().uextend(pointer_type, index_val)
                     };
                    let offset_val_from_index = builder.ins().imul(index_val_ptr_sized, stride_val);
                    let elem_cl_type = translate_type(elem_hir_ty.clone(), isa.as_ref(), ctx)?.unwrap();
                    let field_addr = builder.ins().iadd(base_ptr, offset_val_from_index);

                    let value = builder.ins().load(elem_cl_type, MemFlags::trusted(), field_addr, 0);
                    Ok(value) // Return Ok(value) instead of return Ok(value)
                }
                 ProjectionKind::Downcast(variant_symbol) => {
                     let (enum_def, _variant_def) = ctx.get_enum_and_variant_def(*variant_symbol)
                         .ok_or_else(|| NativeError::TypeError(format!("Variant symbol {:?} not found for downcast", variant_symbol)))?;
                     let expected_discriminant = enum_def.variants.iter().position(|v| v.symbol == *variant_symbol)
                         .ok_or_else(|| NativeError::TypeError("Variant symbol not found within its own enum definition?".to_string()))?;

                     // Use base_descriptor obtained earlier
                     let discr_cl_ty = get_discriminant_cl_type(base_descriptor)?;
                     let (_discr_size, discr_offset) = get_enum_discriminant_info(base_descriptor)?;
                     let loaded_discr = builder.ins().load(discr_cl_ty, MemFlags::trusted(), base_ptr, discr_offset as i32);

                     let expected_discr_val = builder.ins().iconst(discr_cl_ty, expected_discriminant as i64);
                     let comparison_result = builder.ins().icmp(cranelift_codegen::ir::condcodes::IntCC::Equal, loaded_discr, expected_discr_val);
                     builder.ins().trapz(comparison_result, TrapCode::HEAP_OUT_OF_BOUNDS);
                     Ok(base_ptr) // Return Ok(base_ptr)
                 }
            }
        }
        HirValue::Closure { function_symbol, captures } => {
            // Get pointer type for the target system
            let pointer_type = isa.pointer_type();
            
            // --- Environment Handling --- 
            
            // 1. Determine Environment Type and Layout
            let captured_hir_types: Vec<_> = captures.iter()
                .map(|op| ctx.get_operand_type(op).unwrap_or(HirType::Never)) // Get HIR types of captures
                .collect();
            
            // Represent environment as a tuple of captured types
            let env_hir_type = HirType::Tuple(captured_hir_types);

            // Skip if environment is ZST (no captures or all captures are ZSTs)
            if is_zst(&env_hir_type, ctx) {
                // Environment is zero-sized, treat env_handle as null.
                let func_ptr_val = translate_operand(builder, ctx, &Operand::Global(*function_symbol), None, jit_module, isa)?;
                let null_env_handle = builder.ins().iconst(pointer_type, 0);
                
                let alloc_closure_fn_ref = declare_alloc_closure_fn(jit_module, builder.func, pointer_type)?;
                let closure_call_args = &[func_ptr_val, null_env_handle];
                let closure_call_inst = builder.ins().call(alloc_closure_fn_ref, closure_call_args);
                let closure_results = builder.inst_results(closure_call_inst);
                if closure_results.is_empty() {
                    return Err(NativeError::CompilationError("Closure allocation call returned no value (ZST env)".to_string()));
                }
                // Result is already tagged by parallax_alloc_closure
                let tagged_closure_handle = closure_results[0];
                return Ok(tagged_closure_handle); 
            }
            
            // Get the descriptor index and descriptor for the environment struct/tuple type
            let env_descriptor_index = ctx.get_or_create_descriptor_index(&env_hir_type)?;
            let env_descriptor = ctx.get_descriptor_by_index(env_descriptor_index).cloned()
                .ok_or_else(|| NativeError::LayoutError(LayoutError::MissingDescriptor(env_descriptor_index)))?;
            
            let env_size_bytes = get_size_bytes(&env_descriptor);
            let _env_align_bytes = get_alignment_bytes(&env_descriptor); // Alignment needed for stack slot?            

            // 2. Allocate Temporary Stack Buffer for Environment Data
            // Use ExplicitSlot as we manage the layout. Size comes from descriptor.
            let init_data_slot = builder.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot, env_size_bytes as u32, 0 // Offset 0 within slot
            ));
            let init_data_ptr_val = builder.ins().stack_addr(pointer_type, init_data_slot, 0);
            
            // 3. Translate Captured Variables and Store into Stack Buffer
            match env_descriptor {
                LayoutDescriptor::Struct { fields: env_field_layouts, .. } => {
                    if env_field_layouts.len() != captures.len() {
                         return Err(NativeError::LayoutError(LayoutError::Other("Environment layout field count mismatch with captures".to_string())));
                    }
                    for (i, captured_operand) in captures.iter().enumerate() {
                        let (field_offset, field_desc_idx) = env_field_layouts[i];
                        let field_hir_type = match &env_hir_type {
                            HirType::Tuple(elements) => elements.get(i).cloned().ok_or_else(|| NativeError::TypeError("Tuple index mismatch during closure env translation".to_string()))?,
                            _ => return Err(NativeError::TypeError("Environment HIR type is not a Tuple as expected".to_string())),
                        };
                        
                        // Skip storing ZST fields
                        if !is_zst(&field_hir_type, ctx) {
                            let captured_val = translate_operand(builder, ctx, captured_operand, Some(&field_hir_type), jit_module, isa)?;
                            builder.ins().store(MemFlags::trusted(), captured_val, init_data_ptr_val, field_offset as i32);
                        }
                    }
                }
                _ => return Err(NativeError::LayoutError(LayoutError::Other("Environment descriptor is not a Struct/Tuple".to_string())))
            }

            // 4. Allocate Environment Object on Heap using parallax_alloc_object
            let alloc_obj_fn_ref = declare_alloc_object_fn(jit_module, builder.func, pointer_type)?;
            let env_descriptor_index_val = builder.ins().iconst(pointer_type, env_descriptor_index as i64);
            // Pass the pointer to the *initialized* stack buffer
            let env_call_args = &[env_descriptor_index_val, init_data_ptr_val]; 
            let env_call_inst = builder.ins().call(alloc_obj_fn_ref, env_call_args);
            let env_results = builder.inst_results(env_call_inst);
            if env_results.is_empty() {
                return Err(NativeError::CompilationError("Environment allocation call (parallax_alloc_object) returned no value".to_string()));
            }
            let env_handle_val = env_results[0]; // This is the Handle<GcObject> for the environment
            
            // 5. Push Environment Handle to Shadow Stack (already correct)
            let push_fn_ref = declare_shadow_stack_push_fn(jit_module, builder.func, pointer_type)?;
            builder.ins().call(push_fn_ref, &[env_handle_val]);
            ctx.increment_shadow_stack_push_count();
            
            // --- Closure Allocation --- 
            
            // 6. Get Function Pointer (already correct)
            let func_ptr_val = translate_operand(builder, ctx, &Operand::Global(*function_symbol), None, jit_module, isa)?;
            
            // 7. Declare parallax_alloc_closure (already correct)
            let alloc_closure_fn_ref = declare_alloc_closure_fn(jit_module, builder.func, pointer_type)?;
            
            // 8. Call parallax_alloc_closure with function pointer and environment handle (already correct)
            let closure_call_args = &[func_ptr_val, env_handle_val];
            let closure_call_inst = builder.ins().call(alloc_closure_fn_ref, closure_call_args);
            let closure_results = builder.inst_results(closure_call_inst);
            if closure_results.is_empty() {
                return Err(NativeError::CompilationError("Closure allocation call returned no value".to_string()));
            }
            
            // 9. Return Tagged Closure Handle (already correct)
            // The result is already tagged by parallax_alloc_closure
            let tagged_closure_handle = closure_results[0];
            Ok(tagged_closure_handle)
        }
    }
}