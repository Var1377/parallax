// parallax-hir/src/lower/pattern.rs
// Lowers Typed AST patterns to ANF HIR patterns.

use super::*; // Import items from parent `mod.rs`
use parallax_types::types::{TypedPattern, TypedPatternKind, TypedDefinitions, TypedPatternField, Ty};
use parallax_types::types::{TypedVariant, TypedEnum, TypedStruct, TypedField};
use crate::hir::{HirPattern, HirType, HirValue, ProjectionKind, AggregateKind, HirLiteral, Operand};
use crate::hir::PrimitiveType;
use std::collections::HashMap;
// Added this alias for clarity in tests
use parallax_types::types::PrimitiveType as TypesPrimitive;

/// Lowers a TypedPattern into an HirPattern, mainly for use in Match arms.
/// Returns the lowered pattern along with a vector of symbol/variable/type tuples that
/// represent the variables introduced by this pattern.
pub(super) fn lower_pattern<'def>(
    ctx: &mut LoweringContext<'def>,
    pattern: &TypedPattern,
    defs: &'def TypedDefinitions, // Pass defs for now
) -> (HirPattern, Vec<(TypeSymbol, HirVar, Ty)>) {
    match &pattern.kind {
        TypedPatternKind::Identifier { symbol, name } => {
            // `x` or `_` in match arm -> binds the value
            // Note: HirPattern::Bind requires a type.
            let hir_var = ctx.fresh_hir_var(); // Use fresh_hir_var() directly instead of get_or_create to avoid adding to current scope
            let hir_type = lower_type(&pattern.ty, ctx);
            
            // Don't create a binding for `_` wildcard identifier
            if name == "_" {
                (HirPattern::Wildcard, vec![])
            } else {
                // Return the binding information including the original type
                (HirPattern::Bind { var: hir_var, var_ty: hir_type }, vec![(*symbol, hir_var, pattern.ty.clone())])
            }
        }
        TypedPatternKind::Wildcard => {
            (HirPattern::Wildcard, vec![])
        }
        TypedPatternKind::Literal(lit) => {
            // Lower the literal using the type information from the pattern itself.
            (HirPattern::Const(lower_literal_with_type(lit, &pattern.ty)), vec![])
        }
        TypedPatternKind::Constructor { enum_name, variant_name, args, .. } => {
             // Find the variant symbol
             let enum_def_symbol = ctx.definitions().enums.iter()
                 .find(|(_, def)| def.name == *enum_name)
                 .map(|(sym, _)| *sym)
                 .unwrap_or_else(|| panic!("Could not find enum '{}' for pattern lowering", enum_name));
             let enum_def = &defs.enums[&enum_def_symbol];
             let variant_symbol = enum_def.variants.iter()
                 .find_map(|v| match v {
                     TypedVariant::Unit { name, symbol, .. } if name == variant_name => Some(*symbol),
                     TypedVariant::Tuple { name, symbol, .. } if name == variant_name => Some(*symbol),
                     TypedVariant::Struct { name, symbol, .. } if name == variant_name => Some(*symbol),
                     _ => None,
                 })
                 .unwrap_or_else(|| panic!("Could not find variant '{}.{}' for pattern lowering", enum_name, variant_name));
             
             // Recursively lower the arguments pattern to get bindings
             let (hir_args_pattern, arg_bindings) = lower_pattern(ctx, args, defs);
             
             // Create the HirPattern::Variant. The structure of the pattern itself
             // might need refinement based on how variant matching is implemented.
             // For now, let's assume HirPattern::Variant needs the bindings structure.
             // We need HirVar and HirType for the pattern, not the original Ty.
             let hir_pattern_bindings: Vec<(HirVar, HirType)> = arg_bindings.iter()
                 .map(|(_, hir_var, ty)| (*hir_var, lower_type(ty, ctx))) // Use original Ty to get HirType
                 .collect();
             
             (HirPattern::Variant { variant_symbol, bindings: hir_pattern_bindings }, arg_bindings)
        }
        // Lowering complex patterns into simpler forms
        TypedPatternKind::Tuple(elements) => {
            // For tuple patterns, we collect bindings from each element
            let mut all_bindings = Vec::new();
            
            for element in elements {
                let (_, element_bindings) = lower_pattern(ctx, element, defs);
                all_bindings.extend(element_bindings);
            }
            
            // Use Wildcard pattern for simplicity, the bindings are the important part
            (HirPattern::Wildcard, all_bindings)
        }
        TypedPatternKind::Struct { struct_name, fields, .. } => {
            // For struct patterns, collect bindings from all field patterns
            let mut all_bindings = Vec::new();
            
            for field in fields {
                if let Some(field_pattern) = &field.pattern {
                    let (_, field_bindings) = lower_pattern(ctx, field_pattern, defs);
                    all_bindings.extend(field_bindings);
                }
            }
            
            // Use Wildcard pattern for simplicity, the bindings are the important part
            (HirPattern::Wildcard, all_bindings)
        }
        TypedPatternKind::Array(elements) => {
            // For array patterns, collect bindings from each element
            let mut all_bindings = Vec::new();
            
            for element in elements {
                let (_, element_bindings) = lower_pattern(ctx, element, defs);
                all_bindings.extend(element_bindings);
            }
            
            // Use Wildcard pattern for simplicity, the bindings are the important part  
            (HirPattern::Wildcard, all_bindings)
        }
        TypedPatternKind::Rest => {
            println!("WARN: Rest pattern unsupported in match arm. Using Wildcard.");
            (HirPattern::Wildcard, vec![])
        }
        TypedPatternKind::Or(p1, p2) => {
             // Or patterns should ideally be expanded by the frontend/typechecker into multiple arms.
             // If we encounter one here, both sides must bind the same variables for correctness.
             panic!("Or-pattern encountered during HIR lowering, should have been expanded.");
        }
    }
}

/// Recursive helper to handle pattern bindings within expressions like `let`.
/// It takes a pattern and the operand holding the value to be destructured,
/// and returns a list of bindings (hir_var, hir_type, hir_value_to_assign).
pub(super) fn lower_pattern_binding<'def>(
    ctx: &mut LoweringContext<'def>,
    pattern: &TypedPattern,
    value_operand: Operand, // The HirVar or Const holding the value to destructure
    defs: &'def TypedDefinitions, // Pass defs for now
) -> Vec<(HirVar, HirType, HirValue)>
{
    let mut bindings = Vec::new();
    let pattern_hir_type = lower_type(&pattern.ty, ctx);

    match &pattern.kind {
        TypedPatternKind::Identifier { symbol, name } => {
            // Bind the whole value_operand to a new HirVar associated with the symbol.
            if name != "_" { // Don't bind if it's explicitly a wildcard identifier
                let hir_var = ctx.fresh_hir_var(); // Create a fresh var for this specific binding instance
                // Add binding and type to the context
                ctx.add_binding_with_type(*symbol, hir_var, pattern.ty.clone());
                bindings.push((hir_var, pattern_hir_type, HirValue::Use(value_operand)));
            }
        }
        TypedPatternKind::Wildcard => {
            // No binding created, value is ignored.
        }
        TypedPatternKind::Literal(lit) => {
            // The expression lowering would likely check for equality.
            println!("WARN: Literal pattern encountered in lower_pattern_binding (unexpected?).");

            // Literal patterns don't make sense in `let` bindings as they don't bind variables.
            panic!(
                "Internal Error: Literal patterns are not supported in `let` bindings. Pattern: {:?}",
                pattern.kind
            );
        }
        TypedPatternKind::Tuple(sub_patterns) => {
            for (index, sub_pattern) in sub_patterns.iter().enumerate() {
                // Create a projection value for the element
                let projection_value = HirValue::Project {
                    base: value_operand.clone(),
                    projection: ProjectionKind::TupleIndex(index as u32),
                };
                // Create a temporary HirVar to hold the projected value
                let temp_var = ctx.fresh_hir_var();
                let temp_var_type = lower_type(&sub_pattern.ty, ctx);
                bindings.push((temp_var, temp_var_type.clone(), projection_value));

                // Recursively lower the sub-pattern against the temporary variable
                let sub_bindings = lower_pattern_binding(
                    ctx,
                    sub_pattern,
                    Operand::Var(temp_var),
                    defs,
                );
                bindings.extend(sub_bindings);
            }
        }
        TypedPatternKind::Struct { struct_name, fields, .. } => {
            // Get the struct definition to find field symbols/order if needed
            // Note: TypedPatternKind::Struct doesn't store the struct symbol directly.
            // We might need to look it up from `struct_name` or have it passed down.
            let struct_def_symbol_opt = ctx.definitions().structs.iter()
                .find(|(_, def)| def.name == *struct_name)
                .map(|(sym, _)| *sym);

            if let Some(struct_def_symbol) = struct_def_symbol_opt {
                let struct_def = &ctx.definitions().structs[&struct_def_symbol];

                for field_pattern in fields {
                    // Find the corresponding field definition in the TypedStruct definition to get the Symbol
                    let field_def = struct_def.fields.iter()
                        .find(|f| f.name == field_pattern.name)
                        .unwrap_or_else(|| panic!("Lowering Error: Field '{}' not found in struct definition '{}' during pattern binding.", field_pattern.name, struct_name));

                    let field_symbol = field_def.symbol;

                    // If the pattern exists (e.g., `field: sub_pattern`), project and recurse
                    if let Some(sub_pattern) = &field_pattern.pattern {
                        let projection_value = HirValue::Project {
                            base: value_operand.clone(),
                            projection: ProjectionKind::Field(field_symbol),
                        };
                        let temp_var = ctx.fresh_hir_var();
                        let temp_var_type = lower_type(&sub_pattern.ty, ctx);
                        bindings.push((temp_var, temp_var_type.clone(), projection_value));

                        let sub_bindings = lower_pattern_binding(
                            ctx,
                            sub_pattern,
                            Operand::Var(temp_var),
                            defs,
                        );
                        bindings.extend(sub_bindings);
                    }
                    else {
                        // Handle shorthand `{ field }`, which implies binding `field` to the projected value.
                        let projection_value = HirValue::Project {
                            base: value_operand.clone(),
                            projection: ProjectionKind::Field(field_symbol),
                        };
                        let temp_var = ctx.fresh_hir_var();
                        let temp_var_ty = lower_type(&field_def.ty, ctx); // Use type from struct field def
                        
                        // Add the binding instruction: let temp_var = base.field;
                        bindings.push((temp_var, temp_var_ty.clone(), projection_value));
                        
                        // Add the binding to the context: map the original field symbol to the new temp_var
                        ctx.add_binding_with_type(field_symbol, temp_var, field_def.ty.clone());
                    }
                    // If pattern is None (e.g., `field`), it might just be shorthand for `field: _` if pattern is None.
                }
            } else {
                panic!("Could not find struct definition for '{}' during pattern binding lowering", struct_name);
            }

        }
        TypedPatternKind::Constructor { enum_name, variant_name, args, .. } => {
            // Find the enum and variant definitions
            let enum_def_symbol_opt = ctx.definitions().enums.iter()
                .find(|(_, def)| def.name == *enum_name)
                .map(|(sym, _)| *sym);

            if let Some(enum_def_symbol) = enum_def_symbol_opt {
                let enum_def = &ctx.definitions().enums[&enum_def_symbol];
                let variant_opt = enum_def.variants.iter()
                    .find(|v| match v {
                        // Assuming TypedVariant stores name and symbol
                        TypedVariant::Unit { name, .. } => name == variant_name,
                        TypedVariant::Tuple { name, .. } => name == variant_name,
                        TypedVariant::Struct { name, .. } => name == variant_name,
                    });

                if let Some(variant) = variant_opt {
                    // Get the variant symbol (needed for ProjectionKind::Downcast)
                    let variant_symbol = match variant {
                        TypedVariant::Unit { symbol, .. } => *symbol,
                        TypedVariant::Tuple { symbol, .. } => *symbol,
                        TypedVariant::Struct { symbol, .. } => *symbol,
                    };

                    // Explicitly disallow constructor patterns in `let` for now.
                    panic!(
                        "Internal Error: Constructor patterns (e.g., Enum::Variant(x)) are not supported directly in `let` bindings. Use `match` instead. Pattern: {:?}",
                        pattern.kind
                    );

                    // If we *were* to handle it here (assuming irrefutable):
                    // 1. Could potentially generate a downcast check.
                    // 2. Project fields based on variant kind (Tuple/Struct) and recurse.
                    // This adds significant complexity to `let` lowering.

                    // Example for Tuple variant fields (if needed later):
                    // if let TypedVariant::Tuple { types, .. } = variant {
                    //     if let TypedPatternKind::Tuple(sub_patterns) = &args.kind { // Assuming args is a pattern itself
                    //         for (index, sub_pattern) in sub_patterns.iter().enumerate() {
                    //             // Project the field
                    //             let projection_value = HirValue::Project {
                    //                 base: value_operand.clone(),
                    //                 // Need a way to project *variant* fields, Hir doesn't specify this directly?
                    //                 // Maybe ProjectionKind needs VariantFieldIndex(variant_symbol, index)?
                    //                 projection: ProjectionKind::TupleIndex(index as u32), // Placeholder!
                    //             };
                    //             let temp_var = ctx.fresh_hir_var();
                    //             let temp_var_type = lower_type(&sub_pattern.ty, ctx);
                    //             bindings.push((temp_var, temp_var_type.clone(), projection_value));
                    //             bindings.extend(lower_pattern_binding(ctx, sub_pattern, Operand::Var(temp_var), defs));
                    //         }
                    //     }
                    // }
                } else {
                     panic!("Could not find variant definition for '{}.{}' during pattern binding lowering", enum_name, variant_name);
                }
            } else {
                 panic!("Could not find enum definition for '{}' during pattern binding lowering", enum_name);
            }
        }
        TypedPatternKind::Array(elements) => {
            // Handle array patterns similar to tuples
            for (index, sub_pattern) in elements.iter().enumerate() {
                // Project the element
                let index_literal = HirLiteral::IntLiteral { value: index as i128, ty: PrimitiveType::U64 }; // Assume u64 for index
                let projection_value = HirValue::Project {
                    base: value_operand.clone(),
                    projection: ProjectionKind::ArrayIndex(Operand::Const(index_literal)),
                };
                let temp_var = ctx.fresh_hir_var();
                let temp_var_type = lower_type(&sub_pattern.ty, ctx);
                bindings.push((temp_var, temp_var_type.clone(), projection_value));

                // Lower the sub-pattern
                let sub_bindings = lower_pattern_binding(
                    ctx,
                    sub_pattern,
                    Operand::Var(temp_var),
                    defs,
                );
                bindings.extend(sub_bindings);
            }
        }
        TypedPatternKind::Rest => {
            // `..` is usually for array/slice patterns, maybe struct.
            // Not typically used directly in simple `let` bindings.
            println!("WARN: Rest pattern encountered in lower_pattern_binding (unexpected?).");

            // Rest patterns don't make sense in `let` bindings.
            panic!(
                "Internal Error: Rest patterns (`..`) are not supported directly in `let` bindings. Pattern: {:?}",
                pattern.kind
            );
        }
        TypedPatternKind::Or(p1, p2) => {
            // Or patterns `p1 | p2` are inherently unsuitable for `let` bindings
            // as they don't guarantee which variables are bound.
            panic!("Internal Error: Or-pattern found in lower_pattern_binding, should be rejected by parser/typechecker.");
        }

    }
    bindings
}

#[cfg(test)]
mod tests {
    use super::{lower_pattern, lower_pattern_binding, TypesPrimitive}; // Bring alias into scope
    use crate::hir::{HirPattern, HirVar, HirType, HirLiteral, Operand, HirValue, ProjectionKind, PrimitiveType as HirPrimitiveType};
    use crate::lower::{LoweringContext, types::lower_type};
    use parallax_resolve::Symbol;
    use parallax_syntax::ast::common::Literal as AstLiteral;
    use parallax_types::types::{Ty, TyKind, TypedPattern, TypedPatternKind, TypedDefinitions, TypedPatternField};
    use parallax_types::types::{TypedVariant, TypedEnum, TypedStruct, TypedField};
    use miette::SourceSpan;
    use std::collections::HashMap;

    // --- Test Helpers ---
    fn dummy_span() -> SourceSpan {
        SourceSpan::from((0, 0))
    }

    fn dummy_ty(kind: TyKind) -> Ty {
        Ty { kind, span: Some(dummy_span()) }
    }

    fn create_test_context() -> LoweringContext<'static> {
        let defs = Box::leak(Box::new(TypedDefinitions::default()));
        LoweringContext::new(defs)
    }

    fn create_test_context_with_enums(enums: HashMap<Symbol, TypedEnum>) -> LoweringContext<'static> {
         let defs = Box::leak(Box::new(TypedDefinitions {
            functions: HashMap::new(),
            structs: HashMap::new(),
            enums,
        }));
        LoweringContext::new(defs)
    }

    fn create_test_context_with_structs(structs: HashMap<Symbol, TypedStruct>) -> LoweringContext<'static> {
        let defs = Box::leak(Box::new(TypedDefinitions {
           functions: HashMap::new(),
           structs,
           enums: HashMap::new(),
       }));
       LoweringContext::new(defs)
   }

    // --- lower_pattern Tests ---
    #[test]
    fn test_lower_pattern_wildcard() {
        let mut ctx = create_test_context();
        let pattern = TypedPattern {
            kind: TypedPatternKind::Wildcard,
            // Use the alias TypesPrimitive brought into scope
            ty: dummy_ty(TyKind::Primitive(TypesPrimitive::I32)),
            span: dummy_span(),
        };
        let defs = TypedDefinitions::default();
        let (hir_pattern, bindings) = lower_pattern(&mut ctx, &pattern, &defs);

        assert_eq!(hir_pattern, HirPattern::Wildcard);
        assert!(bindings.is_empty());
    }

    #[test]
    fn test_lower_pattern_identifier() {
        let mut ctx = create_test_context();
        let var_symbol = Symbol::new(1);
        // Use the alias TypesPrimitive brought into scope
        let ty_bool = dummy_ty(TyKind::Primitive(TypesPrimitive::Bool));
        let pattern = TypedPattern {
            kind: TypedPatternKind::Identifier { symbol: var_symbol, name: "x".to_string() },
            ty: ty_bool.clone(),
            span: dummy_span(),
        };
        let defs = TypedDefinitions::default();
        let (hir_pattern, bindings) = lower_pattern(&mut ctx, &pattern, &defs);

        // Expect HirPattern::Bind with a generated HirVar and the correct HirType
        let expected_hir_type = HirType::Primitive(HirPrimitiveType::Bool);
        match hir_pattern {
            HirPattern::Bind { var, var_ty } => {
                assert_eq!(var_ty, expected_hir_type);
                // Check bindings
                assert_eq!(bindings.len(), 1);
                assert_eq!(bindings[0].0, var_symbol); // Original TypeSymbol
                assert_eq!(bindings[0].1, var);        // Generated HirVar
                assert_eq!(bindings[0].2, pattern.ty); // Original Ty
            }
            _ => panic!("Expected HirPattern::Bind, found {:?}", hir_pattern),
        }
    }

    #[test]
    fn test_lower_pattern_identifier_underscore() {
        let mut ctx = create_test_context();
        let var_symbol = Symbol::new(1);
        // Use the alias TypesPrimitive brought into scope
        let ty_string = dummy_ty(TyKind::Primitive(TypesPrimitive::String));
        let pattern = TypedPattern {
            kind: TypedPatternKind::Identifier { symbol: var_symbol, name: "_".to_string() },
            ty: ty_string,
            span: dummy_span(),
        };
        let defs = TypedDefinitions::default();
        let (hir_pattern, bindings) = lower_pattern(&mut ctx, &pattern, &defs);

        // Underscore identifier should become Wildcard
        assert_eq!(hir_pattern, HirPattern::Wildcard);
        assert!(bindings.is_empty());
    }

    #[test]
    fn test_lower_pattern_literal() {
        let mut ctx = create_test_context();
        // Use the alias TypesPrimitive brought into scope
        let ty_i64 = dummy_ty(TyKind::Primitive(TypesPrimitive::I64));
        let pattern = TypedPattern {
            kind: TypedPatternKind::Literal(AstLiteral::Int { value: 100, suffix: None }),
            ty: ty_i64,
            span: dummy_span(),
        };
        let defs = TypedDefinitions::default();
        let (hir_pattern, bindings) = lower_pattern(&mut ctx, &pattern, &defs);

        assert_eq!(
            hir_pattern,
            HirPattern::Const(HirLiteral::IntLiteral { value: 100, ty: HirPrimitiveType::I64 })
        );
        assert!(bindings.is_empty());
    }

    #[test]
    fn test_lower_pattern_variant_unit() {
        let enum_sym = Symbol::new(10);
        let variant_sym = Symbol::new(11);
        let mut enums = HashMap::new();
        enums.insert(enum_sym, TypedEnum {
            symbol: enum_sym,
            name: "MyEnum".to_string(),
            variants: vec![TypedVariant::Unit { name: "MyUnit".to_string(), symbol: variant_sym, span: dummy_span() }],
            generic_params: vec![],
            span: dummy_span(),
        });
        let mut ctx = create_test_context_with_enums(enums);
        let defs = ctx.definitions().clone(); // Clone needed defs for passing

        let pattern = TypedPattern {
            kind: TypedPatternKind::Constructor {
                enum_name: "MyEnum".to_string(),
                variant_name: "MyUnit".to_string(),
                args: Box::new(TypedPattern {
                    kind: TypedPatternKind::Tuple(vec![]), // No args for unit
                    ty: dummy_ty(TyKind::Tuple(vec![])), // Unit tuple type
                    span: dummy_span(),
                }),
            },
            ty: dummy_ty(TyKind::Named { name: "MyEnum".to_string(), symbol: Some(enum_sym), args: vec![] }),
            span: dummy_span(),
        };

        let (hir_pattern, bindings) = lower_pattern(&mut ctx, &pattern, &defs);

        match hir_pattern {
            HirPattern::Variant { variant_symbol, bindings: hir_bindings } => {
                assert_eq!(variant_symbol, variant_sym);
                assert!(hir_bindings.is_empty()); // No fields to bind
            }
            _ => panic!("Expected HirPattern::Variant, found {:?}", hir_pattern),
        }
        assert!(bindings.is_empty()); // No variables bound
    }

    #[test]
    fn test_lower_pattern_variant_tuple() {
        let enum_sym = Symbol::new(20);
        let variant_sym = Symbol::new(21);
        let field_sym = Symbol::new(22);

        let mut enums = HashMap::new();
        enums.insert(enum_sym, TypedEnum {
            symbol: enum_sym,
            name: "MyEnum".to_string(),
            variants: vec![TypedVariant::Tuple {
                name: "MyTupleVar".to_string(),
                symbol: variant_sym,
                types: vec![dummy_ty(TyKind::Primitive(TypesPrimitive::Bool))],
                span: dummy_span(),
            }],
            generic_params: vec![],
            span: dummy_span(),
        });
        let mut ctx = create_test_context_with_enums(enums);
        let defs = ctx.definitions().clone();

        let inner_pattern = TypedPattern {
            kind: TypedPatternKind::Identifier { symbol: field_sym, name: "b".to_string() },
            // Use the alias TypesPrimitive brought into scope
            ty: dummy_ty(TyKind::Primitive(TypesPrimitive::Bool)),
            span: dummy_span(),
        };

        let pattern = TypedPattern {
            kind: TypedPatternKind::Constructor {
                enum_name: "MyEnum".to_string(),
                variant_name: "MyTupleVar".to_string(),
                args: Box::new(TypedPattern { // Wrap inner pattern in Tuple pattern
                    kind: TypedPatternKind::Tuple(vec![inner_pattern.clone()]),
                    ty: dummy_ty(TyKind::Tuple(vec![inner_pattern.ty.clone()])),
                    span: dummy_span(),
                })
            },
            ty: dummy_ty(TyKind::Named { name: "MyEnum".to_string(), symbol: Some(enum_sym), args: vec![] }),
            span: dummy_span(),
        };

        let (hir_pattern, bindings) = lower_pattern(&mut ctx, &pattern, &defs);

        match hir_pattern {
            HirPattern::Variant { variant_symbol, bindings: hir_bindings } => {
                assert_eq!(variant_symbol, variant_sym);
                assert_eq!(hir_bindings.len(), 1);
                // Check the HirVar and HirType in the pattern binding
                assert_eq!(hir_bindings[0].1, HirType::Primitive(HirPrimitiveType::Bool));
            }
            _ => panic!("Expected HirPattern::Variant, found {:?}", hir_pattern),
        }
        // Check the outer bindings Vec (Symbol, HirVar, Ty)
        assert_eq!(bindings.len(), 1);
        assert_eq!(bindings[0].0, field_sym);
        assert_eq!(bindings[0].1, HirVar(0)); // Assuming first generated HirVar
        assert_eq!(bindings[0].2.kind, TyKind::Primitive(TypesPrimitive::Bool));
    }

    // --- lower_pattern_binding Tests --- 
    #[test]
    fn test_lower_pattern_binding_identifier() {
        let mut ctx = create_test_context();
        let var_symbol = Symbol::new(1);
        let value_var = ctx.fresh_hir_var(); // Simulate the variable holding the value
        let value_operand = Operand::Var(value_var);
        // Use the alias TypesPrimitive brought into scope
        let value_ty = dummy_ty(TyKind::Primitive(TypesPrimitive::I32));
        let pattern = TypedPattern {
            kind: TypedPatternKind::Identifier { symbol: var_symbol, name: "x".to_string() },
            ty: value_ty.clone(),
            span: dummy_span(),
        };
        let defs = TypedDefinitions::default();

        let bindings = lower_pattern_binding(&mut ctx, &pattern, value_operand.clone(), &defs);

        assert_eq!(bindings.len(), 1);
        let (bound_var, bound_hir_ty, bound_hir_value) = &bindings[0];

        assert_eq!(bound_var, &HirVar(1)); // Should be the next fresh var (0 was value_var)
        assert_eq!(bound_hir_ty, &HirType::Primitive(HirPrimitiveType::I32));
        assert_eq!(bound_hir_value, &HirValue::Use(value_operand));

        // Check context was updated
        assert_eq!(ctx.get_hir_var(var_symbol), Some(*bound_var));
        assert_eq!(ctx.get_type_for_symbol(var_symbol).unwrap().kind, value_ty.kind);
    }

    #[test]
    fn test_lower_pattern_binding_wildcard() {
        let mut ctx = create_test_context();
        let value_var = ctx.fresh_hir_var();
        let value_operand = Operand::Var(value_var);
        let pattern = TypedPattern {
            kind: TypedPatternKind::Wildcard,
            // Use the alias TypesPrimitive brought into scope
            ty: dummy_ty(TyKind::Primitive(TypesPrimitive::I32)),
            span: dummy_span(),
        };
        let defs = TypedDefinitions::default();

        let bindings = lower_pattern_binding(&mut ctx, &pattern, value_operand.clone(), &defs);

        assert!(bindings.is_empty()); // Wildcard produces no bindings
    }

    #[test]
    fn test_lower_pattern_binding_tuple() {
        let mut ctx = create_test_context();
        let sym1 = Symbol::new(1);
        let sym2 = Symbol::new(2);
        let value_var = ctx.fresh_hir_var(); // var holding the tuple value
        let value_operand = Operand::Var(value_var);

        // Use the alias TypesPrimitive brought into scope
        let ty_i32 = dummy_ty(TyKind::Primitive(TypesPrimitive::I32));
        let ty_bool = dummy_ty(TyKind::Primitive(TypesPrimitive::Bool));
        let tuple_ty = dummy_ty(TyKind::Tuple(vec![ty_i32.clone(), ty_bool.clone()]));

        let pattern = TypedPattern {
            kind: TypedPatternKind::Tuple(vec![
                TypedPattern {
                    kind: TypedPatternKind::Identifier { symbol: sym1, name: "a".to_string() },
                    ty: ty_i32.clone(),
                    span: dummy_span(),
                },
                TypedPattern {
                    kind: TypedPatternKind::Identifier { symbol: sym2, name: "b".to_string() },
                    ty: ty_bool.clone(),
                    span: dummy_span(),
                },
            ]),
            ty: tuple_ty,
            span: dummy_span(),
        };
        let defs = TypedDefinitions::default();

        let bindings = lower_pattern_binding(&mut ctx, &pattern, value_operand.clone(), &defs);

        assert_eq!(bindings.len(), 4); // 2 projections + 2 final bindings

        // Binding 0: let temp0 = value.0
        let (temp_var0, temp_ty0, temp_val0) = &bindings[0];
        assert_eq!(temp_var0, &HirVar(1));
        assert_eq!(temp_ty0, &HirType::Primitive(HirPrimitiveType::I32));
        assert_eq!(temp_val0, &HirValue::Project { base: value_operand.clone(), projection: ProjectionKind::TupleIndex(0) });

        // Binding 1: let final_var1 = use temp0
        let (final_var1, final_ty1, final_val1) = &bindings[1];
        assert_eq!(final_var1, &HirVar(2));
        assert_eq!(final_ty1, &HirType::Primitive(HirPrimitiveType::I32));
        assert_eq!(final_val1, &HirValue::Use(Operand::Var(*temp_var0)));
        assert_eq!(ctx.get_hir_var(sym1), Some(*final_var1)); // Check context mapping

        // Binding 2: let temp1 = value.1
        let (temp_var1, temp_ty1, temp_val1) = &bindings[2];
        assert_eq!(temp_var1, &HirVar(3));
        assert_eq!(temp_ty1, &HirType::Primitive(HirPrimitiveType::Bool));
        assert_eq!(temp_val1, &HirValue::Project { base: value_operand.clone(), projection: ProjectionKind::TupleIndex(1) });

        // Binding 3: let final_var2 = use temp1
        let (final_var2, final_ty2, final_val2) = &bindings[3];
        assert_eq!(final_var2, &HirVar(4));
        assert_eq!(final_ty2, &HirType::Primitive(HirPrimitiveType::Bool));
        assert_eq!(final_val2, &HirValue::Use(Operand::Var(*temp_var1)));
        assert_eq!(ctx.get_hir_var(sym2), Some(*final_var2)); // Check context mapping
    }

    #[test]
    fn test_lower_pattern_binding_struct() {
        let struct_sym = Symbol::new(10);
        let field_sym_x = Symbol::new(11);
        let field_sym_y = Symbol::new(12);
        let bind_sym_y = Symbol::new(13);

        let mut structs = HashMap::new();
        structs.insert(struct_sym, TypedStruct {
            symbol: struct_sym,
            name: "Point".to_string(),
            fields: vec![
                TypedField { name: "x".to_string(), symbol: field_sym_x, ty: dummy_ty(TyKind::Primitive(TypesPrimitive::I32)), is_public: true, span: dummy_span() },
                TypedField { name: "y".to_string(), symbol: field_sym_y, ty: dummy_ty(TyKind::Primitive(TypesPrimitive::Bool)), is_public: true, span: dummy_span() },
            ],
            generic_params: vec![],
            span: dummy_span(),
        });
        let mut ctx = create_test_context_with_structs(structs);
        let defs = ctx.definitions().clone();

        let value_var = ctx.fresh_hir_var(); // var holding the struct value
        let value_operand = Operand::Var(value_var);

        let pattern = TypedPattern {
            kind: TypedPatternKind::Struct {
                struct_name: "Point".to_string(),
                fields: vec![
                    TypedPatternField { 
                        name: "x".to_string(), 
                        pattern: Some(
                            TypedPattern {
                                kind: TypedPatternKind::Wildcard,
                                ty: dummy_ty(TyKind::Primitive(TypesPrimitive::I32)),
                                span: dummy_span()
                            }
                        ), 
                        span: dummy_span()
                    },
                    TypedPatternField { 
                        name: "y".to_string(), 
                        pattern: Some(
                            TypedPattern {
                                kind: TypedPatternKind::Identifier { symbol: bind_sym_y, name: "inner_y".to_string() },
                                ty: dummy_ty(TyKind::Primitive(TypesPrimitive::Bool)),
                                span: dummy_span()
                            }
                        ),
                        span: dummy_span()
                    }
                ],
            },
            ty: dummy_ty(TyKind::Named { name: "Point".to_string(), symbol: Some(struct_sym), args: vec![] }),
            span: dummy_span(),
        };

        let bindings = lower_pattern_binding(&mut ctx, &pattern, value_operand.clone(), &defs);

        assert_eq!(bindings.len(), 3); // Proj x, Proj y, Bind final_y

        // 1. Project x: let temp_x = value.x
        let (temp_x, ty_x, val_x) = &bindings[0];
        assert_eq!(temp_x, &HirVar(1));
        assert_eq!(ty_x, &HirType::Primitive(HirPrimitiveType::I32));
        assert_eq!(val_x, &HirValue::Project { base: value_operand.clone(), projection: ProjectionKind::Field(field_sym_x) });
        // Wildcard sub-pattern generates no further bindings for temp_x

        // 2. Project y: let temp_y = value.y
        let (temp_y, ty_y, val_y) = &bindings[1];
        assert_eq!(temp_y, &HirVar(2));
        assert_eq!(ty_y, &HirType::Primitive(HirPrimitiveType::Bool));
        assert_eq!(val_y, &HirValue::Project { base: value_operand.clone(), projection: ProjectionKind::Field(field_sym_y) });

        // 3. Bind temp_y to inner_y: let final_y = use temp_y
        let (final_y, ty_final_y, val_final_y) = &bindings[2];
        assert_eq!(final_y, &HirVar(3));
        assert_eq!(ty_final_y, &HirType::Primitive(HirPrimitiveType::Bool));
        assert_eq!(val_final_y, &HirValue::Use(Operand::Var(*temp_y)));
        assert_eq!(ctx.get_hir_var(bind_sym_y), Some(*final_y)); // Check context mapping

    }

    // TODO: Add test for struct shorthand binding { field }
    // TODO: Add nested pattern binding tests

} 