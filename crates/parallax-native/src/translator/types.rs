use crate::NativeError;
use cranelift_codegen::ir::{
    types::{self, Type},
    AbiParam,
    Signature,
};
use cranelift_codegen::isa::TargetIsa;
use parallax_hir::hir::{HirType, HirFunctionSignature, PrimitiveType};
use std::sync::Arc;
use crate::translator::context::TranslationContext;
use parallax_gc::{layout::LayoutError, LayoutDescriptor};

/// Translates a HIR type into a Cranelift IR type.
///
/// Returns `None` if the type is zero-sized (like Unit) and shouldn't have a direct representation.
/// Returns `Err` if the type is not yet supported or cannot be translated.
pub fn translate_type<'ctx, 'hir>(
    hir_type: HirType,
    isa: &dyn TargetIsa,
    ctx: &'ctx mut TranslationContext<'hir>,
) -> Result<Option<Type>, NativeError> {
    match hir_type {
        HirType::Primitive(PrimitiveType::Unit) => return Ok(None),
        HirType::Tuple(ref elements) if elements.is_empty() => return Ok(None),
        HirType::Never => return Ok(None),
        _ => {}
    }

    let desc_idx = ctx.get_or_create_descriptor_index(&hir_type)?;
    let descriptor = ctx.get_descriptor_by_index(desc_idx)
        .ok_or_else(|| NativeError::LayoutError(LayoutError::Other(format!("Descriptor index {} not found after creation for type {:?}", desc_idx, hir_type))))?;

    match descriptor {
        LayoutDescriptor::Primitive { size_bytes, .. } => {
            if *size_bytes == 0 {
                Ok(None)
            } else {
                if let HirType::Primitive(prim) = hir_type {
                    translate_primitive_type(prim, isa).map(Some)
                } else {
                    Err(NativeError::TypeError("LayoutDescriptor::Primitive found for non-primitive HirType?".to_string()))
                }
            }
        }
        LayoutDescriptor::Handle |
        LayoutDescriptor::Struct { .. } |
        LayoutDescriptor::Enum { .. } |
        LayoutDescriptor::Array { .. } => {
            Ok(Some(isa.pointer_type()))
        }
    }
}

/// Translates a primitive HIR type into a Cranelift IR type.
/// Note: Unit should be handled before calling this (returns Ok(None) from translate_type).
/// Note: String is handled in translate_type as it becomes a pointer (Handle layout).
fn translate_primitive_type(
    prim_type: PrimitiveType,
    _isa: &dyn TargetIsa,
) -> Result<Type, NativeError> {
    match prim_type {
        PrimitiveType::I8   => Ok(types::I8),
        PrimitiveType::I16  => Ok(types::I16),
        PrimitiveType::I32  => Ok(types::I32),
        PrimitiveType::I64  => Ok(types::I64),
        PrimitiveType::I128 => Ok(types::I128),
        PrimitiveType::U8   => Ok(types::I8),
        PrimitiveType::U16  => Ok(types::I16),
        PrimitiveType::U32  => Ok(types::I32),
        PrimitiveType::U64  => Ok(types::I64),
        PrimitiveType::U128 => Ok(types::I128),
        PrimitiveType::F32  => Ok(types::F32),
        PrimitiveType::F64  => Ok(types::F64),
        PrimitiveType::Bool => Ok(types::I8), // Bools represented as i8
        PrimitiveType::Char => Ok(types::I32), // Chars represented as i32 (Unicode scalar value)
        PrimitiveType::Unit => Err(NativeError::Unimplemented(
            "Unit primitive type should be handled by translate_type directly".to_string(),
        )),
        PrimitiveType::String => Err(NativeError::Unimplemented(
            "String primitive type should be handled by translate_type directly".to_string(),
        )),
    }
}

/// Creates a Cranelift Signature based on the HIR function signature.
pub fn translate_signature<'ctx, 'hir>(
    hir_sig: &HirFunctionSignature,
    isa: &Arc<dyn TargetIsa>,
    ctx: &'ctx mut TranslationContext<'hir>,
) -> Result<Signature, NativeError> {
    let mut sig = Signature::new(isa.default_call_conv());

    for (_param_var, param_type) in &hir_sig.params {
        if let Some(cl_type) = translate_type(param_type.clone(), isa.as_ref(), ctx)? {
            sig.params.push(AbiParam::new(cl_type));
        } else {
            // Skip zero-sized types like Unit
        }
    }

    if let Some(ret_type) = translate_type(hir_sig.return_type.clone(), isa.as_ref(), ctx)? {
        sig.returns.push(AbiParam::new(ret_type));
    } else {
        // No return value (e.g., returns Unit)
    }

    Ok(sig)
} 