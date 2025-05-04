// parallax-hir/src/lower/types.rs
// Lowers Typed AST types and literals to ANF HIR types and literals.

use super::*; // Import items from parent `mod.rs`
use crate::hir::{HirType, HirLiteral, PrimitiveType as HirPrimitiveType};
use parallax_syntax::ast::common::Literal as AstLiteral;
use parallax_types::types::{Ty, TyKind, TypedDefinitions, PrimitiveType as ParallaxPrimitiveType};
use parallax_types::error::display_type;
use std::sync::Arc;
use std::collections::BTreeMap;

/// Lowers a Typed AST literal to an HIR literal.
/// This function now requires the associated Type information to determine the HirPrimitiveType.
pub(super) fn lower_literal_with_type(lit: &AstLiteral, ty: &Ty) -> HirLiteral {
    println!(
        "[lower_literal_with_type] Received literal {:?} with type: {}",
        lit,
        display_type(ty) // Display the type as it enters the function
    );
    // Determine the HIR primitive type based on the final Type resolved by the type checker.
    let hir_primitive_ty = match &ty.kind {
        TyKind::Primitive(prim) => lower_primitive_type(prim),
        TyKind::InferInt(_) => panic!("Internal Compiler Error: Encountered unresolved InferInt during HIR lowering. Type: {}", display_type(ty)),
        TyKind::InferFloat(_) => panic!("Internal Compiler Error: Encountered unresolved InferFloat during HIR lowering. Type: {}", display_type(ty)),
        _ => { // Handle cases where the provided type wasn't a primitive
             panic!(
                 "Internal Compiler Error: Expected Primitive type for literal {:?}, found {}. Span: {:?}",
                 lit,
                 display_type(ty),
                 ty.span
             );
        }
    };

    // Construct the HIR literal using the determined HIR primitive type.
    match lit {
        AstLiteral::Int { value, .. } => HirLiteral::IntLiteral { value: *value as i128, ty: hir_primitive_ty },
        AstLiteral::Float { value, .. } => HirLiteral::FloatLiteral { value: *value, ty: hir_primitive_ty },
        AstLiteral::String(s) => HirLiteral::StringLiteral(s.clone()),
        AstLiteral::Bool(b) => HirLiteral::BoolLiteral(*b),
        AstLiteral::Char(c) => HirLiteral::CharLiteral(*c),
    }
}

/// Lowers a `Ty` (from parallax-types) to an `HirType`.
pub(super) fn lower_type<'def>(
    ty: &Ty,
    ctx: &LoweringContext<'def>, // Accept context to access definitions
) -> HirType {
    match &ty.kind {
        TyKind::Primitive(prim) => {
            println!(
                "[lower_type] Lowering primitive type: {:?} (from Ty: {})",
                prim,
                display_type(ty)
            );
            HirType::Primitive(lower_primitive_type(prim))
        }
        TyKind::InferInt(_) => panic!("Internal Compiler Error: Encountered unresolved InferInt during HIR lowering. Type: {}", display_type(ty)),
        TyKind::InferFloat(_) => panic!("Internal Compiler Error: Encountered unresolved InferFloat during HIR lowering. Type: {}", display_type(ty)),
        TyKind::Named { name, symbol, args: _ } => {
            match symbol {
                Some(symbol) => HirType::Adt(*symbol),
                None => {
                    // This case should ideally be prevented by the type checker.
                    // If it happens, it indicates an internal error or an unresolved type.
                    println!(
                        "Internal Error: Could not find symbol for named type '{}' during HIR lowering. Span: {:?}",
                        name,
                        ty.span
                    );
                    // Return a placeholder or panic. Returning Unit Tuple for now.
                    HirType::Tuple(vec![])
                }
            }
        }
        TyKind::Array(elem_ty, size) => {
            let hir_elem_ty = Arc::new(lower_type(elem_ty, ctx));
            HirType::Array(hir_elem_ty, *size)
        }
        TyKind::Tuple(tys) => {
            HirType::Tuple(tys.iter().map(|t| lower_type(t, ctx)).collect())
        }
        TyKind::Function(param_tys, ret_ty) => {
            let hir_params = param_tys.iter().map(|t| lower_type(t, ctx)).collect();
            let hir_ret = Arc::new(lower_type(ret_ty, ctx));
            HirType::FunctionPointer(hir_params, hir_ret)
        }
        TyKind::Never => HirType::Never,
        // These should not exist after type checking is complete and successful.
        // Type variables should be resolved, Error indicates a type error,
        // and SelfType should be replaced with the actual implementing type.
        TyKind::Var(id) => panic!("Internal Error: Encountered unresolved type variable {:?} during HIR lowering. Span: {:?}", id, ty.span),
        TyKind::Error => panic!("Internal Error: Encountered Error type during HIR lowering. Span: {:?}", ty.span),
        TyKind::SelfType => panic!("Internal Error: Encountered SelfType during HIR lowering (should be resolved). Span: {:?}", ty.span),
        // Add arms for Map and Set
        &TyKind::Map(_, _) | &TyKind::Set(_) => {
            // TODO: Decide on HIR representation for Map/Set types if needed.
            // For now, treat as unsupported or placeholder.
            println!("Warning: Lowering Map/Set types to HIR is not fully implemented. Using placeholder. Span: {:?}", ty.span);
            // Return a placeholder type, e.g., an empty tuple or a specific error marker if HIR had one.
            HirType::Tuple(vec![])
        }
        // Handle unresolved types that shouldn't reach HIR
        TyKind::Pointer(..) => panic!("Internal Error: Encountered Pointer type during HIR lowering (should be resolved?). Span: {:?}", ty.span),
        TyKind::GenericParam(id) => panic!("Internal Error: Encountered unresolved GenericParam {:?} during HIR lowering. Span: {:?}", id, ty.span),
    }
}

/// Lowers a `ParallaxPrimitiveType` to a `resolve::PrimitiveType`.
/// Assumes HirType::Primitive uses the ResolvePrimitiveType variant.
fn lower_primitive_type(prim: &ParallaxPrimitiveType) -> HirPrimitiveType {
    match prim {
        ParallaxPrimitiveType::I8 => HirPrimitiveType::I8,
        ParallaxPrimitiveType::I16 => HirPrimitiveType::I16,
        ParallaxPrimitiveType::I32 => HirPrimitiveType::I32,
        ParallaxPrimitiveType::I64 => HirPrimitiveType::I64,
        ParallaxPrimitiveType::I128 => HirPrimitiveType::I128,
        ParallaxPrimitiveType::U8 => HirPrimitiveType::U8,
        ParallaxPrimitiveType::U16 => HirPrimitiveType::U16,
        ParallaxPrimitiveType::U32 => HirPrimitiveType::U32,
        ParallaxPrimitiveType::U64 => HirPrimitiveType::U64,
        ParallaxPrimitiveType::U128 => HirPrimitiveType::U128,
        ParallaxPrimitiveType::F32 => HirPrimitiveType::F32,
        ParallaxPrimitiveType::F64 => HirPrimitiveType::F64,
        ParallaxPrimitiveType::Bool => HirPrimitiveType::Bool,
        ParallaxPrimitiveType::Char => HirPrimitiveType::Char,
        ParallaxPrimitiveType::String => HirPrimitiveType::String,
        ParallaxPrimitiveType::Unit => HirPrimitiveType::Unit,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hir::{HirLiteral, HirType};
    use crate::lower::LoweringContext;
    use parallax_resolve::types::{Symbol, PrimitiveType as ResolvePrimitive};
    use parallax_syntax::ast::common::Literal as AstLiteral;
    use parallax_types::types::{Ty, TyKind, PrimitiveType, TypedDefinitions, TypedStruct, TypedEnum};
    use miette::SourceSpan;
    use std::collections::BTreeMap;
    use std::sync::Arc;

    fn dummy_span() -> SourceSpan {
        SourceSpan::from((0, 0))
    }
    fn dummy_ty(kind: TyKind) -> Ty {
        Ty { kind, span: Some(dummy_span()) }
    }

    #[test]
    fn test_lower_literal_with_type() {
        let ty_i32 = dummy_ty(TyKind::Primitive(PrimitiveType::I32));
        let ty_f64 = dummy_ty(TyKind::Primitive(PrimitiveType::F64));
        let ty_string = dummy_ty(TyKind::Primitive(PrimitiveType::String));
        let ty_bool = dummy_ty(TyKind::Primitive(PrimitiveType::Bool));
        let ty_char = dummy_ty(TyKind::Primitive(PrimitiveType::Char));
        let ty_int_literal = dummy_ty(TyKind::Primitive(PrimitiveType::IntegerLiteral)); // Test defaulting

        assert_eq!(lower_literal_with_type(&AstLiteral::Int { value: 42, suffix: None }, &ty_i32), HirLiteral::IntLiteral { value: 42, ty: HirPrimitiveType::I32 });
        // Test defaulting for integer literal type
        assert_eq!(lower_literal_with_type(&AstLiteral::Int { value: 100, suffix: None }, &ty_int_literal), HirLiteral::IntLiteral { value: 100, ty: HirPrimitiveType::I64 });
        let f_input = 3.14f64;
        assert_eq!(lower_literal_with_type(&AstLiteral::Float { value: f_input, suffix: None }, &ty_f64), HirLiteral::FloatLiteral { value: f_input, ty: HirPrimitiveType::F64 });
        assert_eq!(lower_literal_with_type(&AstLiteral::String("hello".to_string()), &ty_string), HirLiteral::StringLiteral("hello".to_string()));
        assert_eq!(lower_literal_with_type(&AstLiteral::Bool(true), &ty_bool), HirLiteral::BoolLiteral(true));
        assert_eq!(lower_literal_with_type(&AstLiteral::Char('a'), &ty_char), HirLiteral::CharLiteral('a'));
    }

    // Helper to create a minimal LoweringContext for type tests
    fn create_test_context_with_defs(structs: BTreeMap<Symbol, TypedStruct>, enums: BTreeMap<Symbol, TypedEnum>) -> LoweringContext<'static> {
        // Leak the definitions to get a 'static lifetime - acceptable for testing.
        let defs = Box::leak(Box::new(TypedDefinitions {
            functions: BTreeMap::new(),
            structs,
            enums,
        }));
        LoweringContext::new(defs)
    }

    #[test]
    fn test_lower_primitive_type() {
        let defs = TypedDefinitions::default();
        let mut ctx = LoweringContext::new(&defs);
        let mut dummy_ty = Ty { kind: TyKind::Primitive(PrimitiveType::I32), span: Some(dummy_span()) };

        assert_eq!(lower_type(&dummy_ty, &ctx), HirType::Primitive(HirPrimitiveType::I32));
        dummy_ty.kind = TyKind::Primitive(PrimitiveType::Bool);
        assert_eq!(lower_type(&dummy_ty, &ctx), HirType::Primitive(HirPrimitiveType::Bool));
        dummy_ty.kind = TyKind::Primitive(PrimitiveType::F64);
        assert_eq!(lower_type(&dummy_ty, &ctx), HirType::Primitive(HirPrimitiveType::F64));
        dummy_ty.kind = TyKind::Primitive(PrimitiveType::String);
        assert_eq!(lower_type(&dummy_ty, &ctx), HirType::Primitive(HirPrimitiveType::String));
        dummy_ty.kind = TyKind::Primitive(PrimitiveType::Unit);
        assert_eq!(lower_type(&dummy_ty, &ctx), HirType::Primitive(HirPrimitiveType::Unit));
    }

    #[test]
    fn test_lower_never_type() {
        let defs = TypedDefinitions::default();
        let mut ctx = LoweringContext::new(&defs);
        let never_ty = Ty { kind: TyKind::Never, span: Some(dummy_span()) };
        assert_eq!(lower_type(&never_ty, &ctx), HirType::Never);
    }

    #[test]
    fn test_lower_tuple_type() {
        let defs = TypedDefinitions::default();
        let mut ctx = LoweringContext::new(&defs);
        let tuple_ty = Ty {
            kind: TyKind::Tuple(vec![
                Ty { kind: TyKind::Primitive(PrimitiveType::I32), span: Some(dummy_span()) },
                Ty { kind: TyKind::Primitive(PrimitiveType::Bool), span: Some(dummy_span()) },
            ]),
            span: Some(dummy_span()),
        };
        assert_eq!(
            lower_type(&tuple_ty, &ctx),
            HirType::Tuple(vec![
                HirType::Primitive(HirPrimitiveType::I32),
                HirType::Primitive(HirPrimitiveType::Bool),
            ])
        );
    }

    #[test]
    fn test_lower_array_type() {
        let defs = TypedDefinitions::default();
        let mut ctx = LoweringContext::new(&defs);
        let array_ty = Ty {
            kind: TyKind::Array(
                Arc::new(Ty { kind: TyKind::Primitive(PrimitiveType::F32), span: Some(dummy_span()) }),
                Some(5),
            ),
            span: Some(dummy_span()),
        };
        assert_eq!(
            lower_type(&array_ty, &ctx),
            HirType::Array(Arc::new(HirType::Primitive(HirPrimitiveType::F32)), Some(5))
        );

        // Test lowering array with unknown size (should result in None)
        let array_unknown_size_ty = Ty {
            kind: TyKind::Array(
                Arc::new(Ty { kind: TyKind::Primitive(PrimitiveType::I64), span: Some(dummy_span()) }),
                None,
            ),
            span: Some(dummy_span()),
        };
        assert_eq!(
            lower_type(&array_unknown_size_ty, &ctx),
            HirType::Array(Arc::new(HirType::Primitive(HirPrimitiveType::I64)), None)
        );
    }

    #[test]
    fn test_lower_function_pointer_type() {
        let defs = TypedDefinitions::default();
        let mut ctx = LoweringContext::new(&defs);
        let fn_ptr_ty = Ty {
            kind: TyKind::Function(
                vec![
                    Ty { kind: TyKind::Primitive(PrimitiveType::I64), span: Some(dummy_span()) },
                ],
                Arc::new(Ty { kind: TyKind::Primitive(PrimitiveType::Bool), span: Some(dummy_span()) }),
            ),
            span: Some(dummy_span()),
        };
        assert_eq!(
            lower_type(&fn_ptr_ty, &ctx),
            HirType::FunctionPointer(
                vec![HirType::Primitive(HirPrimitiveType::I64)],
                Arc::new(HirType::Primitive(HirPrimitiveType::Bool)),
            )
        );
    }

    #[test]
    fn test_lower_named_type() {
        let struct_sym = Symbol::new(1);
        let enum_sym = Symbol::new(2);

        let test_struct = TypedStruct {
            symbol: struct_sym,
            name: "MyStruct".to_string(),
            fields: vec![],
            generic_params: vec![],
            span: dummy_span(),
        };
        let test_enum = TypedEnum {
            symbol: enum_sym,
            name: "MyEnum".to_string(),
            variants: vec![],
            generic_params: vec![],
            span: dummy_span(),
        };

        let mut structs = BTreeMap::new();
        structs.insert(struct_sym, test_struct);
        let mut enums = BTreeMap::new();
        enums.insert(enum_sym, test_enum);

        let mut ctx = create_test_context_with_defs(structs, enums);

        let named_struct_ty = Ty {
            kind: TyKind::Named { name: "MyStruct".to_string(), symbol: Some(struct_sym), args: vec![] },
            span: Some(dummy_span()),
        };
        let named_enum_ty = Ty {
            kind: TyKind::Named { name: "MyEnum".to_string(), symbol: Some(enum_sym), args: vec![] },
            span: Some(dummy_span()),
        };
        let named_unknown_ty = Ty {
            kind: TyKind::Named { name: "NotFound".to_string(), symbol: None, args: vec![] },
            span: Some(dummy_span()),
        };

        assert_eq!(lower_type(&named_struct_ty, &ctx), HirType::Adt(struct_sym));
        assert_eq!(lower_type(&named_enum_ty, &ctx), HirType::Adt(enum_sym));
        assert_eq!(lower_type(&named_unknown_ty, &ctx), HirType::Tuple(vec![]));
    }
} 