// Handles translation of HIR operands (Var, Const, Global) and literals.
use crate::NativeError;
use crate::translator::context::{TranslationContext, GlobalInfo};
use cranelift_codegen::ir::{Value, InstBuilder, types, MemFlags};
use cranelift_codegen::isa::TargetIsa;
use cranelift_frontend::FunctionBuilder;
use cranelift_jit::JITModule;
use cranelift_module::{FuncOrDataId, Linkage, Module};
use parallax_hir::hir::{Operand, HirLiteral, HirType};
use std::sync::Arc;
use crate::translator::layout::LayoutComputer;
use std::mem::size_of;
use cranelift_codegen::settings;
use cranelift_codegen::Context as ClifContext;
use cranelift_frontend::{FunctionBuilderContext};
use cranelift_jit::JITBuilder;
use cranelift_codegen::ir::{Opcode, InstructionData};
use cranelift_codegen::isa;
use cranelift_codegen::ir::{AbiParam};

/// Translate an HIR operand to a Cranelift value
pub fn translate_operand<'ctx>(
    builder: &mut FunctionBuilder,
    ctx: &mut TranslationContext<'ctx>,
    operand: &Operand,
    _expected_hir_ty: Option<&HirType>,
    jit_module: &mut JITModule,
    isa: &Arc<dyn TargetIsa>,
    _layout_computer: &mut LayoutComputer,
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
    ctx: &mut TranslationContext<'ctx>,
) -> Result<Value, NativeError> {
    match literal {
        HirLiteral::Int(i) => {
            Ok(builder.ins().iconst(types::I64, *i))
        }
        HirLiteral::Float(f) => {
            let as_f64 = f64::from_bits(*f);
            Ok(builder.ins().f64const(as_f64))
        }
        HirLiteral::String(s) => {
            let pointer_type = isa.pointer_type();
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

            let mut data_desc = cranelift_module::DataDescription::new();
            data_desc.init = cranelift_module::Init::Bytes { contents: string_bytes.into_boxed_slice() };
            jit_module.define_data(data_id, &data_desc)?;

            let global_val = jit_module.declare_data_in_func(data_id, builder.func);

            let data_addr = builder.ins().global_value(pointer_type, global_val);

            let ptr_size = isa.pointer_bytes() as u32;
            let len_size = size_of::<usize>() as u32;
            let string_ref_size = ptr_size + len_size;
            let string_ref_slot = builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
                cranelift_codegen::ir::StackSlotKind::ExplicitSlot, string_ref_size, 0
            ));
            let string_ref_ptr = builder.ins().stack_addr(pointer_type, string_ref_slot, 0);

            let string_len_val = builder.ins().iconst(pointer_type, string_len as i64);
            builder.ins().store(MemFlags::trusted(), data_addr, string_ref_ptr, 0);
            builder.ins().store(MemFlags::trusted(), string_len_val, string_ref_ptr, ptr_size as i32);

            Ok(string_ref_ptr)
        }
        HirLiteral::Bool(b) => {
            Ok(builder.ins().iconst(types::I8, if *b { 1 } else { 0 }))
        }
        HirLiteral::Char(c) => {
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
        &mut crate::translator::context::TranslationContext,
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

    // Empty context for simple literal tests
    let mut trans_ctx = crate::translator::context::TranslationContext::new(&[], &[]);

    // Call the provided test logic closure
    let result = test_body(&mut builder, &isa, &mut jit_module, &mut trans_ctx);

    // Finalize after closure returns
    builder.finalize();

    result
}

#[test]
fn test_translate_literal_int_isolated() {
    run_clif_test_context(|builder, isa, jit_module, trans_ctx| {
        let test_int: i64 = 98765;
        let literal = HirLiteral::Int(test_int);

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
        assert_eq!(const_val, test_int as u128);
        assert_eq!(builder.func.dfg.value_type(value), types::I64);
    });
}

#[test]
fn test_translate_literal_float_isolated() {
    run_clif_test_context(|builder, isa, jit_module, trans_ctx| {
        let test_float: f64 = 3.14159;
        let test_float_bits = test_float.to_bits();
        let literal = HirLiteral::Float(test_float_bits);

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
        assert_eq!(const_val, test_float_bits);
        assert_eq!(builder.func.dfg.value_type(value), types::F64);
    });
}

#[test]
fn test_translate_literal_bool_true_isolated() {
    run_clif_test_context(|builder, isa, jit_module, trans_ctx| {
        let literal = HirLiteral::Bool(true);

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
        let literal = HirLiteral::Bool(false);

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
        let literal = HirLiteral::Char(test_char);

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
        let test_string = "Hello, Cranelift!";
        let literal = HirLiteral::String(test_string.to_string());

        let value = translate_literal(
            builder,
            &literal,
            isa,
            jit_module,
            trans_ctx,
        )
        .expect("translate_literal failed for string");

        let pointer_type = isa.pointer_type();

        // Verify the returned value is from stack_addr
        let stack_addr_inst = builder.func.dfg.value_def(value).unwrap_inst();
        assert_eq!(builder.func.dfg.insts[stack_addr_inst].opcode(), Opcode::StackAddr);
        assert_eq!(builder.func.dfg.value_type(value), pointer_type);

        // Remove builder.finalize() from here to avoid moving out of builder
        // Finalize is handled after the closure in run_clif_test_context if needed

        let data_name = ".L.str.0";
        let _data_id = match jit_module.get_name(data_name) {
            Some(FuncOrDataId::Data(id)) => id,
            _ => panic!("String data symbol '{}' not found or not data", data_name),
        };
    });
}