// parallax-hir/src/lower/items.rs
// Lowers top-level items (functions, structs, enums) from Typed AST to ANF HIR.

use super::*; // Import items from parent `mod.rs`
use crate::hir::{HirFunction, HirFunctionSignature, HirStructDef, HirEnumDef, HirEnumVariant, HirType};
use parallax_types::types::{TypedFunction, TypedStruct, TypedEnum, TypedVariant, TypedDefinitions, TypedParameter};
use parallax_resolve::types::Symbol as TypeSymbol; // Use TypeSymbol alias for resolver's Symbol
use crate::lower::types::lower_type; // Keep this for lower_type usage
use crate::lower::LoweringContext;
use parallax_resolve::types::Symbol;
use parallax_types::types::{Ty, TyKind, TypedField};

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
    match typed_variant {
        TypedVariant::Unit { name, symbol, span } => {
            HirEnumVariant {
                symbol: *symbol,
                name: name.clone(),
                fields: Vec::new(),
                span: *span,
            }
        }
        TypedVariant::Tuple { name, symbol, types, span } => {
            let fields = types.iter().map(|t| lower_type(t, ctx)).collect();
            HirEnumVariant {
                symbol: *symbol,
                name: name.clone(),
                fields,
                span: *span,
            }
        }
        TypedVariant::Struct { name, symbol, fields: struct_fields, span } => {
            let fields = struct_fields.iter().map(|f| lower_type(&f.ty, ctx)).collect();
            HirEnumVariant {
                symbol: *symbol,
                name: name.clone(),
                fields,
                span: *span,
            }
        }
    }
}
