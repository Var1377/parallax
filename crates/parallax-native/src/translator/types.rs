use crate::NativeError;
use cranelift_codegen::ir::{
    types::{self, Type},
    AbiParam,
    Signature,
};
use cranelift_codegen::isa::TargetIsa;
use parallax_hir::hir::{HirType, HirFunctionSignature, ResolvePrimitiveType};
use std::sync::Arc;
use crate::translator::layout::{get_layout, LayoutComputer};
use crate::translator::context::TranslationContext;

/// Translates a HIR type into a Cranelift IR type.
///
/// Returns `None` if the type is zero-sized (like Unit) and shouldn't have a direct representation.
/// Returns `Err` if the type is not yet supported or cannot be translated.
pub fn translate_type<'ctx, 'hir>(
    hir_type: &HirType,
    isa: &dyn TargetIsa,
    ctx: &'ctx TranslationContext<'hir>,
    layout_computer: &mut LayoutComputer,
) -> Result<Option<Type>, NativeError> {
    match hir_type {
        HirType::Primitive(prim) => match prim {
            ResolvePrimitiveType::String => Ok(Some(isa.pointer_type())), // StringRef is represented by a Handle (pointer)
            _ => translate_primitive_type(*prim, isa).map(Some),
        },
        HirType::Tuple(elements) => {
            if elements.is_empty() {
                // Unit type is zero-sized
                Ok(None)
            } else {
                // Pass ctx to get_layout
                match get_layout(hir_type, layout_computer, ctx) {
                    Ok(_layout) => Ok(Some(isa.pointer_type())),
                    Err(e) => Err(e),
                }
            }
        }
        HirType::Adt(_symbol) => {
            // ADTs (Structs/Enums) are represented by pointers
            // Pass ctx to get_layout
            match get_layout(hir_type, layout_computer, ctx) {
                Ok(_layout) => Ok(Some(isa.pointer_type())),
                Err(e) => Err(e),
            }
        }
        HirType::Array(_, _) => {
            // Arrays are represented by pointers
            // Pass ctx to get_layout
            match get_layout(hir_type, layout_computer, ctx) {
                Ok(_layout) => Ok(Some(isa.pointer_type())),
                Err(e) => Err(e),
            }
        }
        HirType::FunctionPointer(_, _) => {
            // Function pointers are raw pointers
            Ok(Some(isa.pointer_type()))
        }
        HirType::Never => {
            // '!' type doesn't produce a value
            Ok(None)
        }
    }
}

/// Translates a primitive HIR type into a Cranelift IR type.
/// Note: Unit should be handled before calling this (returns Ok(None) from translate_type).
/// Note: String is handled in translate_type as it becomes a pointer.
fn translate_primitive_type(
    prim_type: ResolvePrimitiveType,
    _isa: &dyn TargetIsa,
) -> Result<Type, NativeError> {
    match prim_type {
        ResolvePrimitiveType::I8   => Ok(types::I8),
        ResolvePrimitiveType::I16  => Ok(types::I16),
        ResolvePrimitiveType::I32  => Ok(types::I32),
        ResolvePrimitiveType::I64  => Ok(types::I64),
        ResolvePrimitiveType::I128 => Ok(types::I128),
        ResolvePrimitiveType::U8   => Ok(types::I8),
        ResolvePrimitiveType::U16  => Ok(types::I16),
        ResolvePrimitiveType::U32  => Ok(types::I32),
        ResolvePrimitiveType::U64  => Ok(types::I64),
        ResolvePrimitiveType::U128 => Ok(types::I128),
        ResolvePrimitiveType::F32  => Ok(types::F32),
        ResolvePrimitiveType::F64  => Ok(types::F64),
        ResolvePrimitiveType::Bool => Ok(types::I8), // Bools represented as i8
        ResolvePrimitiveType::Char => Ok(types::I32), // Chars represented as i32 (Unicode scalar value)
        ResolvePrimitiveType::Unit => Err(NativeError::Unimplemented(
            "Unit primitive type should be handled by translate_type directly".to_string(),
        )),
        // String is handled in translate_type
        ResolvePrimitiveType::String => unreachable!("String primitive should be handled by translate_type"),
    }
}

/// Creates a Cranelift Signature based on the HIR function signature.
pub fn translate_signature<'ctx, 'hir>(
    hir_sig: &HirFunctionSignature,
    isa: &Arc<dyn TargetIsa>,
    ctx: &'ctx TranslationContext<'hir>,
    layout_computer: &mut LayoutComputer,
) -> Result<Signature, NativeError> {
    let mut sig = Signature::new(isa.default_call_conv());

    // Translate parameters
    for (_param_var, param_type) in &hir_sig.params {
        // Pass ctx to translate_type
        if let Some(cl_type) = translate_type(param_type, isa.as_ref(), ctx, layout_computer)? {
            sig.params.push(AbiParam::new(cl_type));
        } else {
            // Skip zero-sized types like Unit
        }
    }

    // Translate return type
    // Pass ctx to translate_type
    if let Some(ret_type) = translate_type(&hir_sig.return_type, isa.as_ref(), ctx, layout_computer)? {
        sig.returns.push(AbiParam::new(ret_type));
    } else {
        // No return value (e.g., returns Unit)
    }

    Ok(sig)
} 