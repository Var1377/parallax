// parallax-hir/src/lower/items.rs
// Lowers top-level items (functions, structs, enums) from Typed AST to ANF HIR.

use super::*; // Import items from parent `mod.rs`
use crate::hir::{HirFunction, HirFunctionSignature, HirStructDef, HirEnumDef, HirEnumVariant, HirType};
use parallax_types::types::{TypedDefinitions, Ty, TyKind}; // Keep base types
use parallax_types::types::{TypedFunction, TypedStruct, TypedEnum, TypedVariant, TypedParameter, TypedField}; // Moved imports
use parallax_resolve::types::Symbol as TypeSymbol; // Use TypeSymbol alias for resolver's Symbol
use crate::lower::types::lower_type; // Keep this for lower_type usage
use crate::lower::LoweringContext;
use parallax_resolve::types::Symbol;

/// Lowers a function signature.
pub(super) fn lower_signature(ctx: &mut LoweringContext, func: &TypedFunction, defs: &TypedDefinitions) -> HirFunctionSignature {
    let params = func.params.iter().map(|param| {
        let param_symbol = param.symbol;
        let hir_var = ctx.get_or_create_hir_var(param_symbol);
        let hir_type = lower_type(&param.ty, ctx);
        (hir_var, hir_type)
    }).collect();

    let return_type = lower_type(&func.return_type, ctx);

    HirFunctionSignature {
        params,
        return_type,
        is_effectful: func.is_effectful,
    }
}

/// Lowers a typed struct definition to HIR.
pub(super) fn lower_struct_def(ctx: &mut LoweringContext, symbol: TypeSymbol, typed_struct: &TypedStruct) -> HirStructDef {
    let fields = typed_struct.fields.iter()
        .map(|field| {
            let field_symbol = TypeSymbol::new(u32::MAX);
            (field_symbol, field.name.clone(), lower_type(&field.ty, ctx))
        })
        .collect();

    HirStructDef {
        symbol: typed_struct.symbol,
        name: typed_struct.name.clone(),
        fields,
        span: typed_struct.span,
    }
}

/// Lowers a typed enum definition to HIR.
pub(super) fn lower_enum_def(ctx: &mut LoweringContext, symbol: TypeSymbol, typed_enum: &TypedEnum) -> HirEnumDef {
    let variants = typed_enum.variants.iter()
        .map(|variant| lower_enum_variant(ctx, variant))
        .collect();

    HirEnumDef {
        symbol: typed_enum.symbol,
        name: typed_enum.name.clone(),
        variants,
        span: typed_enum.span,
    }
}

/// Lowers a typed enum variant to HIR.
fn lower_enum_variant(ctx: &mut LoweringContext, typed_variant: &TypedVariant) -> HirEnumVariant {
    // Since TypedVariant is a struct, access fields directly.
    // The distinction between Unit/Tuple/Struct variants might be implicitly handled
    // by the number and names of fields in typed_variant.fields.
    // If `types` was the correct field for tuple-like variants, use that.
    // Assuming `fields` holds the relevant type information for all kinds now.
    let hir_fields = typed_variant.fields.iter()
        .map(|f| lower_type(&f.ty, ctx))
        .collect();

    HirEnumVariant {
        symbol: typed_variant.symbol,
        name: typed_variant.name.clone(),
        fields: hir_fields,
        span: typed_variant.span,
    }
}
