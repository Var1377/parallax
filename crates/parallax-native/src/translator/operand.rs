// Handles translation of HIR operands (Var, Const, Global) and literals.
use crate::NativeError;
use crate::translator::context::{TranslationContext, GlobalInfo};
use crate::translator::types::translate_type;
use crate::translator::helpers::{declare_alloc_string_from_buffer_fn, declare_shadow_stack_push_fn};
use std::mem::size_of;
use cranelift_codegen::settings;
use cranelift_codegen::ir::{Value, InstBuilder, types, MemFlags, AbiParam, FuncRef, Opcode, InstructionData, immediates::{Imm64, Ieee64}};
use cranelift_codegen::isa::TargetIsa;
use cranelift_jit::{JITModule, JITBuilder};
use cranelift_module::{Module, Linkage, DataDescription, DataId, Init};
use cranelift_codegen::{Context as ClifContext, isa};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use parallax_hir::hir::{Operand, HirLiteral, HirType, PrimitiveType};
use std::sync::Arc;
use std::mem;
use parallax_layout::LayoutDescriptor;

/// Translate an HIR operand to a Cranelift value
pub fn translate_operand<'ctx>(
    builder: &mut FunctionBuilder,
    ctx: &TranslationContext<'ctx>,
    operand: &Operand,
    expected_hir_ty: Option<&HirType>,
    jit_module: &mut JITModule,
    isa: &Arc<dyn TargetIsa>,
) -> Result<Value, NativeError> {
    match operand {
        Operand::Var(var) => {
            if let Some(value) = ctx.get_var(*var) {
                Ok(value)
            } else {
                if ctx.get_var_type(*var).is_some() {
                    Ok(builder.ins().iconst(types::I8, 0))
                } else {
                    Err(NativeError::Unimplemented(format!("Unknown variable: {:?}", var)))
                }
            }
        }
        Operand::Const(literal) => {
            translate_literal(builder, literal, isa, jit_module, ctx)
        }
        Operand::Global(symbol) => {
            match ctx.get_global_info(*symbol) {
                Some(GlobalInfo::Function(func_sym, _func_ty)) => {
                    let func_ref = jit_module.get_name(&ctx.get_function_like_info(func_sym).unwrap().name)
                        .ok_or_else(|| NativeError::Unimplemented(format!("Function symbol {:?} not found in JIT", func_sym)))?;
                    match func_ref {
                        cranelift_module::FuncOrDataId::Func(id) => {
                             let local_func_ref = jit_module.declare_func_in_func(id, builder.func);
                             Ok(builder.ins().func_addr(isa.pointer_type(), local_func_ref))
                        }
                        cranelift_module::FuncOrDataId::Data(_) => Err(NativeError::TypeError("Expected function symbol, found data".to_string())),
                    }
                }
                Some(GlobalInfo::Data(data_name, _data_ty)) => {
                    let data_ref = jit_module.get_name(&data_name)
                        .ok_or_else(|| NativeError::Unimplemented(format!("Global data symbol {:?} (name: '{}') not found in JIT module", symbol, data_name)))?;
                    match data_ref {
                        cranelift_module::FuncOrDataId::Data(id) => {
                            let global_val = jit_module.declare_data_in_func(id, builder.func);
                            Ok(builder.ins().global_value(isa.pointer_type(), global_val))
                        }
                        cranelift_module::FuncOrDataId::Func(_) => Err(NativeError::TypeError(format!("Expected data symbol '{}', found function", data_name))),
                    }
                }
                Some(GlobalInfo::Static(static_symbol, _, _)) => {
                    let (static_name, _, _) = ctx.get_global_statics().get(&static_symbol)
                         .ok_or_else(|| NativeError::Unimplemented(format!("Static variable info for {:?} not found in context", static_symbol)))?;

                    let data_ref = jit_module.get_name(static_name)
                        .ok_or_else(|| NativeError::Unimplemented(format!("Global static symbol {:?} (name: '{}') not found in JIT module", symbol, static_name)))?;
                    match data_ref {
                        cranelift_module::FuncOrDataId::Data(id) => {
                            let global_val = jit_module.declare_data_in_func(id, builder.func);
                            Ok(builder.ins().global_value(isa.pointer_type(), global_val))
                        }
                        cranelift_module::FuncOrDataId::Func(_) => Err(NativeError::TypeError(format!("Expected static data symbol '{}', found function", static_name))),
                    }
                }
                Some(GlobalInfo::Struct(..)) | Some(GlobalInfo::Enum(..)) => {
                    Err(NativeError::TypeError(format!("Cannot use Struct/Enum definition {:?} directly as a value", symbol)))
                }
                None => Err(NativeError::Unimplemented(format!("Unknown global symbol: {:?}", symbol))),
            }
        }
    }
}

/// Translate an HIR literal to a Cranelift value
pub fn translate_literal<'ctx>(
    builder: &mut FunctionBuilder,
    literal: &HirLiteral,
    isa: &Arc<dyn TargetIsa>,
    jit_module: &mut JITModule,
    ctx: &TranslationContext<'ctx>,
) -> Result<Value, NativeError> {
    let pointer_type = isa.pointer_type();
    match literal {
        HirLiteral::IntLiteral { value, ty } => {
            let hir_ty = HirType::Primitive(ty.clone());
            let clif_ty = translate_type(hir_ty, isa.as_ref(), ctx)?.unwrap_or(types::I64);
            // Convert potentially i128/u128 value to i64 for iconst.
            // This is lossy for values outside the i64 range.
            // Cranelift's iconst takes an Imm64.
            // TODO: Handle i128/u128 losslessness (e.g., via split or memory).
            let val_i64 = *value as i64;
            Ok(builder.ins().iconst(clif_ty, val_i64))
        }
        HirLiteral::FloatLiteral { value, ty } => {
            let hir_ty = HirType::Primitive(ty.clone());
            let clif_ty = translate_type(hir_ty, isa.as_ref(), ctx)?.unwrap_or(types::F64);
            match clif_ty {
                types::F32 => Ok(builder.ins().f32const(*value as f32)), // Cast value to f32 if needed
                types::F64 => Ok(builder.ins().f64const(*value)),        // Value is already f64
                _ => Err(NativeError::TypeError(format!(
                    "translate_literal: Unsupported float type {:?}",
                    ty
                ))),
            }
        }
        HirLiteral::StringLiteral(s) => {
            let string_bytes = s.as_bytes().to_vec();
            let string_len = string_bytes.len();
            let data_symbol_id = ctx.next_data_symbol_id();
            let data_name = format!(".L.str.{}", data_symbol_id);
            let data_id = jit_module.declare_data(
                &data_name,
                Linkage::Local,
                false,
                false
            )?;

            let mut data_desc = DataDescription::new();
            data_desc.init = Init::Bytes { contents: string_bytes.into_boxed_slice() };
            jit_module.define_data(data_id, &data_desc)?;

            let global_val = jit_module.declare_data_in_func(data_id, builder.func);
            let data_addr = builder.ins().global_value(pointer_type, global_val);

            let len_val = builder.ins().iconst(pointer_type, string_len as i64);
            let args = &[data_addr, len_val];

            let alloc_fn_ref = declare_alloc_string_from_buffer_fn(jit_module, builder.func, pointer_type)?;
            let call_inst = builder.ins().call(alloc_fn_ref, args);
            let results = builder.inst_results(call_inst);
            if results.is_empty() {
                return Err(NativeError::CompilationError("String allocation call returned no value".to_string()));
            }
            let result_handle = results[0];

            let push_fn_ref = declare_shadow_stack_push_fn(jit_module, builder.func, pointer_type)?;
            builder.ins().call(push_fn_ref, &[result_handle]);
            ctx.increment_shadow_stack_push_count();

            Ok(result_handle)
        }
        HirLiteral::BoolLiteral(b) => {
            Ok(builder.ins().iconst(types::I8, *b as i64))
        }
        HirLiteral::CharLiteral(c) => {
            Ok(builder.ins().iconst(types::I32, *c as i64))
        }
        HirLiteral::Unit => {
            Ok(builder.ins().iconst(types::I8, 0))
        }
    }
}

// Helper accepts a closure `test_body`
fn run_clif_test_context<F, R>(test_body: F) -> R
where
    F: FnOnce(
        &mut FunctionBuilder,
        &Arc<dyn isa::TargetIsa>,
        &mut JITModule,
        &crate::translator::context::TranslationContext,
    ) -> R,
{
    let flag_builder = settings::builder();
    let flags = settings::Flags::new(flag_builder);
    let isa = isa::lookup_by_name("x86_64").unwrap().finish(flags).unwrap();
    let isa: Arc<dyn isa::TargetIsa> = Arc::from(isa);
    let builder = JITBuilder::with_isa(isa.clone(), cranelift_module::default_libcall_names());
    let mut jit_module = JITModule::new(builder);

    let mut sig = jit_module.make_signature();
    sig.returns.push(AbiParam::new(isa.pointer_type())); // Generic return type
    let func_id = jit_module
        .declare_function("test", Linkage::Local, &sig)
        .expect("Failed to declare function");

    let mut clif_ctx = ClifContext::new();
    clif_ctx.func.signature = sig;
    let mut builder_ctx = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut clif_ctx.func, &mut builder_ctx);
    let entry_block = builder.create_block();
    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);
    builder.seal_block(entry_block);

    // --- Setup LayoutComputer and Precompute Test Types --- 
    let struct_defs = std::collections::HashMap::new();
    let enum_defs = std::collections::HashMap::new();
    let mut layout_computer = parallax_layout::LayoutComputer::new(
        struct_defs.clone(), // Clone the empty maps
        enum_defs.clone()    // Clone the empty maps
    );

    // Explicitly compute layouts for types used in the tests
    layout_computer.get_or_create_descriptor_index(&HirType::Primitive(PrimitiveType::I64)).expect("Layout for I64 failed");
    layout_computer.get_or_create_descriptor_index(&HirType::Primitive(PrimitiveType::I32)).expect("Layout for I32 failed");
    layout_computer.get_or_create_descriptor_index(&HirType::Primitive(PrimitiveType::I8)).expect("Layout for I8 failed");
    layout_computer.get_or_create_descriptor_index(&HirType::Primitive(PrimitiveType::F64)).expect("Layout for F64 failed");
    layout_computer.get_or_create_descriptor_index(&HirType::Primitive(PrimitiveType::F32)).expect("Layout for F32 failed");
    layout_computer.get_or_create_descriptor_index(&HirType::Primitive(PrimitiveType::Bool)).expect("Layout for Bool failed"); // Bool uses I8 layout
    layout_computer.get_or_create_descriptor_index(&HirType::Primitive(PrimitiveType::Char)).expect("Layout for Char failed"); // Char uses I32 layout
    layout_computer.get_or_create_descriptor_index(&HirType::Primitive(PrimitiveType::Unit)).expect("Layout for Unit failed");
    layout_computer.get_or_create_descriptor_index(&HirType::Primitive(PrimitiveType::String)).expect("Layout for String failed"); // String uses Handle layout

    // Finalize to get the store and maps
    let (descriptors_vec, adt_indices, primitive_indices, tuple_indices, array_indices) = layout_computer.finalize();
    // Create a DescriptorStore instance
    let descriptor_store = parallax_layout::DescriptorStore { descriptors: descriptors_vec }; // Create store
    let handle_index = Some(0); // Assuming handle index is 0 from LayoutComputer::new
    // ------------------------------------------------------- 

    // Create immutable context for tests using the *populated* maps
    let intrinsic_symbols = std::collections::HashSet::new(); // Empty set for tests
    let trans_ctx = crate::translator::context::TranslationContext::new(
        &struct_defs, // Now passing reference to original empty maps
        &enum_defs,
        &descriptor_store, // Pass reference to the created DescriptorStore
        &adt_indices,    
        &primitive_indices,
        &tuple_indices,
        &array_indices, // Add back
        handle_index,
        &intrinsic_symbols,
    );

    // Call the provided test logic closure, passing immutable context
    let result = test_body(&mut builder, &isa, &mut jit_module, &trans_ctx);

    // Ensure the block is terminated before finalizing
    // The test signature expects a pointer return, but the actual value
    // from translate_literal might not be a pointer. We'll just return a dummy
    // pointer (null) for now, as the tests primarily check the generated instructions.
    let return_type = isa.pointer_type();
    let dummy_return_val = builder.ins().iconst(return_type, 0);
    builder.ins().return_(&[dummy_return_val]);

    // Finalize after closure returns
    builder.finalize();

    result
}

#[test]
fn test_translate_literal_int_isolated() {
    run_clif_test_context(|builder, isa, jit_module, trans_ctx| {
        let test_int: i128 = 98765;
        // Use the new literal variant with type
        let literal = HirLiteral::IntLiteral { value: test_int, ty: PrimitiveType::I64 };

        let value = translate_literal(
            builder,
            &literal,
            isa,
            jit_module,
            trans_ctx,
        )
        .expect("translate_literal failed for int");

        let inst_data = builder.func.dfg.insts[builder.func.dfg.value_def(value).unwrap_inst()];
        assert_eq!(inst_data.opcode(), Opcode::Iconst);
        let const_val = match inst_data {
            InstructionData::UnaryImm { imm, .. } => imm.bits() as u128,
            _ => panic!("Expected UnaryImm instruction data for iconst"),
        };
        // Compare against the potentially truncated i64 value used in translate_literal
        assert_eq!(const_val as i64, test_int as i64);
        // Check the type based on the literal's `ty` field
        assert_eq!(builder.func.dfg.value_type(value), types::I64);

        // Test a different integer type (I32)
        let literal_i32 = HirLiteral::IntLiteral { value: 123, ty: PrimitiveType::I32 };
        let value_i32 = translate_literal(builder, &literal_i32, isa, jit_module, trans_ctx)
            .expect("translate_literal failed for i32");
        assert_eq!(builder.func.dfg.value_type(value_i32), types::I32);
        let inst_data_i32 = builder.func.dfg.insts[builder.func.dfg.value_def(value_i32).unwrap_inst()];
        assert_eq!(inst_data_i32.opcode(), Opcode::Iconst);
        let const_val_i32 = match inst_data_i32 {
             InstructionData::UnaryImm { imm, .. } => imm.bits(),
             _ => panic!("Expected UnaryImm instruction data for iconst (i32)"),
        };
        assert_eq!(const_val_i32, 123);
    });
}

#[test]
fn test_translate_literal_float_isolated() {
    run_clif_test_context(|builder, isa, jit_module, trans_ctx| {
        let test_float: f64 = 3.14159;
        // Use the new literal variant with type
        let literal = HirLiteral::FloatLiteral { value: test_float, ty: PrimitiveType::F64 };

        let value = translate_literal(
            builder,
            &literal,
            isa,
            jit_module,
            trans_ctx,
        )
        .expect("translate_literal failed for float");

        let inst_data = builder.func.dfg.insts[builder.func.dfg.value_def(value).unwrap_inst()];
        assert_eq!(inst_data.opcode(), Opcode::F64const);
        let const_val = match inst_data {
            InstructionData::UnaryIeee64 { imm, .. } => imm.bits(),
            _ => panic!("Expected UnaryIeee64 instruction data for f64const"),
        };
        assert_eq!(f64::from_bits(const_val), test_float); // Compare actual f64 values
        // Check the type based on the literal's `ty` field
        assert_eq!(builder.func.dfg.value_type(value), types::F64);

        // Test F32
        let test_float32: f64 = 1.618; // Value still f64, but ty is F32
        let literal_f32 = HirLiteral::FloatLiteral { value: test_float32, ty: PrimitiveType::F32 };
        let value_f32 = translate_literal(builder, &literal_f32, isa, jit_module, trans_ctx)
            .expect("translate_literal failed for f32");
        assert_eq!(builder.func.dfg.value_type(value_f32), types::F32);
        let inst_data_f32 = builder.func.dfg.insts[builder.func.dfg.value_def(value_f32).unwrap_inst()];
        assert_eq!(inst_data_f32.opcode(), Opcode::F32const);
        let const_val_f32 = match inst_data_f32 {
             InstructionData::UnaryIeee32 { imm, .. } => imm.bits(),
             _ => panic!("Expected UnaryIeee32 instruction data for f32const"),
        };
        // Compare f32 value
        assert_eq!(f32::from_bits(const_val_f32), test_float32 as f32);
    });
}

#[test]
fn test_translate_literal_bool_true_isolated() {
    run_clif_test_context(|builder, isa, jit_module, trans_ctx| {
        let literal = HirLiteral::BoolLiteral(true);

        let value = translate_literal(
            builder,
            &literal,
            isa,
            jit_module,
            trans_ctx,
        )
        .expect("translate_literal failed for bool(true)");

        let inst_data = builder.func.dfg.insts[builder.func.dfg.value_def(value).unwrap_inst()];
        assert_eq!(inst_data.opcode(), Opcode::Iconst);
        let const_val = match inst_data {
            InstructionData::UnaryImm { imm, .. } => imm.bits() as u128,
            _ => panic!("Expected UnaryImm instruction data for iconst"),
        };
        assert_eq!(const_val, 1);
        assert_eq!(builder.func.dfg.value_type(value), types::I8);
    });
}

#[test]
fn test_translate_literal_bool_false_isolated() {
    run_clif_test_context(|builder, isa, jit_module, trans_ctx| {
        let literal = HirLiteral::BoolLiteral(false);

        let value = translate_literal(
            builder,
            &literal,
            isa,
            jit_module,
            trans_ctx,
        )
        .expect("translate_literal failed for bool(false)");

        let inst_data = builder.func.dfg.insts[builder.func.dfg.value_def(value).unwrap_inst()];
        assert_eq!(inst_data.opcode(), Opcode::Iconst);
        let const_val = match inst_data {
            InstructionData::UnaryImm { imm, .. } => imm.bits() as u128,
            _ => panic!("Expected UnaryImm instruction data for iconst"),
        };
        assert_eq!(const_val, 0);
        assert_eq!(builder.func.dfg.value_type(value), types::I8);
    });
}

#[test]
fn test_translate_literal_char_isolated() {
    run_clif_test_context(|builder, isa, jit_module, trans_ctx| {
        let test_char = '🚀';
        let literal = HirLiteral::CharLiteral(test_char);

        let value = translate_literal(
            builder,
            &literal,
            isa,
            jit_module,
            trans_ctx,
        )
        .expect("translate_literal failed for char");

        let inst_data = builder.func.dfg.insts[builder.func.dfg.value_def(value).unwrap_inst()];
        assert_eq!(inst_data.opcode(), Opcode::Iconst);
        let const_val = match inst_data {
            InstructionData::UnaryImm { imm, .. } => imm.bits() as u128,
            _ => panic!("Expected UnaryImm instruction data for iconst"),
        };
        assert_eq!(const_val, test_char as u128);
        assert_eq!(builder.func.dfg.value_type(value), types::I32);
    });
}

#[test]
fn test_translate_literal_unit_isolated() {
    run_clif_test_context(|builder, isa, jit_module, trans_ctx| {
        let literal = HirLiteral::Unit;

        let value = translate_literal(
            builder,
            &literal,
            isa,
            jit_module,
            trans_ctx,
        )
        .expect("translate_literal failed for unit");

        // Unit is represented as a zero i8 constant
        let inst_data = builder.func.dfg.insts[builder.func.dfg.value_def(value).unwrap_inst()];
        assert_eq!(inst_data.opcode(), Opcode::Iconst);
        let const_val = match inst_data {
            InstructionData::UnaryImm { imm, .. } => imm.bits() as u128,
            _ => panic!("Expected UnaryImm instruction data for iconst"),
        };
        assert_eq!(const_val, 0);
        assert_eq!(builder.func.dfg.value_type(value), types::I8);
    });
}

#[test]
fn test_translate_literal_string_isolated() {
    run_clif_test_context(|builder, isa, jit_module, trans_ctx| {
        let test_string = "Hello, GC!";
        let literal = HirLiteral::StringLiteral(test_string.to_string());

        let _push_fn = declare_shadow_stack_push_fn(jit_module, builder.func, isa.pointer_type())
            .expect("Failed to declare push_shadow_stack");
        let _alloc_fn = declare_alloc_string_from_buffer_fn(jit_module, builder.func, isa.pointer_type())
            .expect("Failed to declare alloc_string_from_buffer");

        let value = translate_literal(
            builder,
            &literal,
            isa,
            jit_module,
            trans_ctx,
        )
        .expect("translate_literal failed for string");

        assert_eq!(builder.func.dfg.value_type(value), isa.pointer_type());

        let mut found_call = false;
        for block in builder.func.layout.blocks() {
            for inst in builder.func.layout.block_insts(block) {
                if builder.func.dfg.insts[inst].opcode() == Opcode::Call {
                    found_call = true;
                    break;
                }
            }
            if found_call { break; }
        }
        assert!(found_call, "No call instruction found for string allocation");

        let data_name = ".L.str.0";
        assert!(jit_module.get_name(data_name).is_some(), "String data symbol not found");
    });
}