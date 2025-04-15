// parallax-hir/src/lower/types.rs
// Lowers Typed AST types and literals to ANF HIR types and literals.

use super::*; // Import items from parent `mod.rs`
use crate::hir::{HirType, HirLiteral, ResolvePrimitiveType};
use parallax_syntax::ast::common::Literal as AstLiteral;
use parallax_types::types::{Ty, TyKind, TypedDefinitions, PrimitiveType as ParallaxPrimitiveType};
use std::sync::Arc;

/// Lowers a Typed AST literal to an HIR literal.
pub(super) fn lower_literal(lit: &AstLiteral) -> HirLiteral {
    match lit {
        AstLiteral::Int(i) => HirLiteral::Int(*i),
        AstLiteral::Float(f) => HirLiteral::Float(f.to_bits()),
        AstLiteral::String(s) => HirLiteral::String(s.clone()),
        AstLiteral::Bool(b) => HirLiteral::Bool(*b),
        AstLiteral::Char(c) => HirLiteral::Char(*c),
        // NOTE: parallax_syntax::ast::common::Literal doesn't have Unit, but HirLiteral does.
        // Unit literals might be handled elsewhere (e.g., empty block expr).
    }
}

/// Lowers a `Ty` (from parallax-types) to an `HirType`.
pub(super) fn lower_type<'def>(
    ty: &Ty,
    ctx: &LoweringContext<'def>, // Accept context to access definitions
) -> HirType {
    match &ty.kind {
        TyKind::Primitive(prim) => HirType::Primitive(lower_primitive_type(prim)),
        TyKind::Named { name, args: _ } => {
            // TODO: Handle generic arguments (`args`) correctly if HIR supports them later.
            // Currently, HIR's Adt(Symbol) doesn't store type arguments.

            // Find the Symbol for the named type using the definitions from the context.
            let defs = ctx.definitions();
            let symbol_opt = defs.structs.iter().find(|(_, s)| s.name == *name).map(|(s, _)| *s)
                             .or_else(|| defs.enums.iter().find(|(_, e)| e.name == *name).map(|(s, _)| *s));
            // We don't expect functions to be used as types here.

            match symbol_opt {
                Some(symbol) => HirType::Adt(symbol),
                None => {
                    // This case should ideally be prevented by the type checker.
                    // If it happens, it indicates an internal error or an unresolved type.
                    eprintln!(
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
            HirType::Array(Arc::new(lower_type(elem_ty, ctx)), *size)
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
    }
}

/// Lowers a `ParallaxPrimitiveType` to a `resolve::PrimitiveType`.
/// Assumes HirType::Primitive uses the ResolvePrimitiveType variant.
fn lower_primitive_type(prim: &ParallaxPrimitiveType) -> ResolvePrimitiveType {
    match prim {
        ParallaxPrimitiveType::I8 => ResolvePrimitiveType::I8,
        ParallaxPrimitiveType::I16 => ResolvePrimitiveType::I16,
        ParallaxPrimitiveType::I32 => ResolvePrimitiveType::I32,
        ParallaxPrimitiveType::I64 => ResolvePrimitiveType::I64,
        ParallaxPrimitiveType::I128 => ResolvePrimitiveType::I128,
        ParallaxPrimitiveType::U8 => ResolvePrimitiveType::U8,
        ParallaxPrimitiveType::U16 => ResolvePrimitiveType::U16,
        ParallaxPrimitiveType::U32 => ResolvePrimitiveType::U32,
        ParallaxPrimitiveType::U64 => ResolvePrimitiveType::U64,
        ParallaxPrimitiveType::U128 => ResolvePrimitiveType::U128,
        ParallaxPrimitiveType::IntegerLiteral => ResolvePrimitiveType::I64, // Default to I64 for literals

        ParallaxPrimitiveType::F32 => ResolvePrimitiveType::F32,
        ParallaxPrimitiveType::F64 => ResolvePrimitiveType::F64,
        ParallaxPrimitiveType::FloatLiteral => ResolvePrimitiveType::F64, // Default to F64 for literals

        ParallaxPrimitiveType::Bool => ResolvePrimitiveType::Bool,
        ParallaxPrimitiveType::Char => ResolvePrimitiveType::Char,
        ParallaxPrimitiveType::String => ResolvePrimitiveType::String,
        ParallaxPrimitiveType::Unit => ResolvePrimitiveType::Unit, // Add Unit mapping
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hir::{HirLiteral, HirType, ResolvePrimitiveType};
    use crate::lower::LoweringContext;
    use parallax_resolve::types::{Symbol, PrimitiveType as ResolvePrimitive};
    use parallax_syntax::ast::common::Literal as AstLiteral;
    use parallax_types::types::{Ty, TyKind, PrimitiveType, TypedDefinitions, TypedStruct, TypedEnum};
    use miette::SourceSpan;
    use std::collections::HashMap;
    use std::sync::Arc;

    #[test]
    fn test_lower_literal() {
        assert_eq!(lower_literal(&AstLiteral::Int(42)), HirLiteral::Int(42));
        assert_eq!(lower_literal(&AstLiteral::Int(1234567890)), HirLiteral::Int(1234567890));
        let f_input = 3.14f64;
        let f_expected_bits = f_input.to_bits();
        assert_eq!(lower_literal(&AstLiteral::Float(f_input)), HirLiteral::Float(f_expected_bits));
        assert_eq!(lower_literal(&AstLiteral::String("hello".to_string())), HirLiteral::String("hello".to_string()));
        assert_eq!(lower_literal(&AstLiteral::Bool(true)), HirLiteral::Bool(true));
        assert_eq!(lower_literal(&AstLiteral::Char('a')), HirLiteral::Char('a'));
    }

    // Helper to create a dummy span
    fn dummy_span() -> SourceSpan {
        SourceSpan::from((0, 0))
    }

    // Helper to create a minimal LoweringContext for type tests
    fn create_test_context_with_defs(structs: HashMap<Symbol, TypedStruct>, enums: HashMap<Symbol, TypedEnum>) -> LoweringContext<'static> {
        // Leak the definitions to get a 'static lifetime - acceptable for testing.
        let defs = Box::leak(Box::new(TypedDefinitions {
            functions: HashMap::new(),
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

        assert_eq!(lower_type(&dummy_ty, &ctx), HirType::Primitive(ResolvePrimitive::I32));
        dummy_ty.kind = TyKind::Primitive(PrimitiveType::Bool);
        assert_eq!(lower_type(&dummy_ty, &ctx), HirType::Primitive(ResolvePrimitive::Bool));
        dummy_ty.kind = TyKind::Primitive(PrimitiveType::F64);
        assert_eq!(lower_type(&dummy_ty, &ctx), HirType::Primitive(ResolvePrimitive::F64));
        dummy_ty.kind = TyKind::Primitive(PrimitiveType::String);
        assert_eq!(lower_type(&dummy_ty, &ctx), HirType::Primitive(ResolvePrimitive::String));
        dummy_ty.kind = TyKind::Primitive(PrimitiveType::Unit);
        assert_eq!(lower_type(&dummy_ty, &ctx), HirType::Primitive(ResolvePrimitive::Unit));
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
                HirType::Primitive(ResolvePrimitive::I32),
                HirType::Primitive(ResolvePrimitive::Bool),
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
                5,
            ),
            span: Some(dummy_span()),
        };
        assert_eq!(
            lower_type(&array_ty, &ctx),
            HirType::Array(Arc::new(HirType::Primitive(ResolvePrimitive::F32)), 5)
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
                vec![HirType::Primitive(ResolvePrimitive::I64)],
                Arc::new(HirType::Primitive(ResolvePrimitive::Bool)),
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

        let mut structs = HashMap::new();
        structs.insert(struct_sym, test_struct);
        let mut enums = HashMap::new();
        enums.insert(enum_sym, test_enum);

        let mut ctx = create_test_context_with_defs(structs, enums);

        let named_struct_ty = Ty {
            kind: TyKind::Named { name: "MyStruct".to_string(), args: vec![] },
            span: Some(dummy_span()),
        };
        let named_enum_ty = Ty {
            kind: TyKind::Named { name: "MyEnum".to_string(), args: vec![] },
            span: Some(dummy_span()),
        };
        let named_unknown_ty = Ty {
             kind: TyKind::Named { name: "NotFound".to_string(), args: vec![] },
             span: Some(dummy_span()),
         };

        assert_eq!(lower_type(&named_struct_ty, &ctx), HirType::Adt(struct_sym));
        assert_eq!(lower_type(&named_enum_ty, &ctx), HirType::Adt(enum_sym));
        assert_eq!(lower_type(&named_unknown_ty, &ctx), HirType::Tuple(vec![]));
    }
} 