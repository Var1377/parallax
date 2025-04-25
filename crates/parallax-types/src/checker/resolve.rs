use miette::SourceSpan;
use parallax_resolve::types::{Symbol, ResolvedType, ResolvedGenericParamDef, PrimitiveType as ResolverPrimitiveType};
use std::sync::Arc;

// TODO: Update imports after refactor
use crate::{
    error::{TypeError, TypeResult},
    types::{Ty, TyKind, TypeDef, GenericParamDef, PrimitiveType as ParallaxPrimitiveType, TraitRef},
    context::inference::Substitution,
};
use parallax_resolve::definitions::DefinitionKind;

use super::TypeChecker;

// Renamed from `helpers` module.
// Contains helpers for resolving types, symbols, and instantiating generics.

// --- Symbol & Name Resolution Helpers --- 

// Placeholder helper - replace with actual symbol resolution
#[allow(dead_code)]
pub(crate) fn resolve_variant_symbol_to_names(
    checker: &mut TypeChecker,
    variant_symbol: Symbol,
    span: SourceSpan // Use the span parameter
) -> TypeResult<(String, String)> {
    // Attempt to find the definition linked to the symbol
    if let Some(_type_def) = checker.type_ctx.get_type_by_symbol(&variant_symbol) {
        // We expect this symbol to resolve directly to an EnumVariant *if* the TypeContext stores them that way.
        // However, the current TypeContext stores TypeDef::Enum at the enum level symbol.
        // We need to search within enums.
        // TODO: Re-evaluate how variant symbols are stored and looked up in TypeContext.
        // For now, iterate through enums as before.
        for (_enum_name, potential_enum_def) in checker.type_ctx.all_types() {
            if let TypeDef::Enum(enum_def) = potential_enum_def {
                if let Some(variant) = enum_def.variants.iter().find(|v| v.symbol == variant_symbol) {
                    // Found the variant with the matching symbol.
                    return Ok((enum_def.name.clone(), variant.name.clone()));
                }
            }
        }
        // If not found after iterating (and symbol had a definition), it's likely not a variant symbol.
        Err(TypeError::InternalError {
            message: format!("Symbol {:?} resolved to a TypeDef, but not found as an enum variant", variant_symbol),
            span: Some(span),
        })
    } else {
        // Symbol didn't resolve to any TypeDef in the context.
        Err(TypeError::InternalError {
            message: format!("Symbol {:?} did not resolve to a type definition", variant_symbol),
            span: Some(span),
        })
    }
}

// Placeholder: determine the function type for an enum variant constructor symbol
// TODO: Move this potentially to checker/invocation.rs or checker/expr.rs?
#[allow(dead_code)]
pub(crate) fn determine_variant_constructor_type(
    checker: &mut TypeChecker,
    variant_symbol: Symbol,
    span: SourceSpan,
) -> TypeResult<Ty> {
    let (enum_name, variant_name) = resolve_variant_symbol_to_names(checker, variant_symbol, span)?;

    let enum_def = match checker.type_ctx.get_type(&enum_name) {
        Some(TypeDef::Enum(def)) => def.clone(),
        _ => return Err(TypeError::InternalError { 
            message: format!("Enum definition '{}' not found for variant constructor '{}'", enum_name, variant_name),
            span: Some(span), 
        }),
    };

    let variant_def = match enum_def.variants.iter().find(|v| v.name == variant_name) {
        Some(v_def) => v_def,
        None => return Err(TypeError::InternalError { 
            message: format!("Variant definition '{}' not found within enum '{}'", variant_name, enum_name),
            span: Some(span), 
        }),
    };

    // Get the base enum type (potentially generic)
    let base_enum_ty = Ty::with_span(
        TyKind::Named {
            name: enum_name.clone(),
            symbol: Some(enum_def.symbol),
            args: vec![],
        },
        span
    );
    // Instantiate any generic parameters on the enum itself
    let (instantiated_enum_ty, _generic_map) = crate::checker::generics::instantiate(
        checker, 
        &base_enum_ty, 
        span, 
        &enum_def.generic_params
    )?;
    let instance_args = match &instantiated_enum_ty.kind {
        TyKind::Named { args, .. } => args,
        _ => &vec![], // Should not happen after instantiate
    };

    // Build the constructor function signature
    let param_tys: Vec<Ty>;
    if variant_def.fields.is_empty() {
        // Unit variant constructor takes no arguments
        param_tys = vec![];
    } else {
        // Tuple or Struct variant constructor takes fields as arguments.
        // The current `EnumVariant` struct doesn't distinguish structurally,
        // so we just take the types from the fields.
        let mut subst = Substitution::new();
        for (gen_param, instance_arg) in std::iter::Iterator::zip(
            enum_def.generic_params.iter(),
            instance_args.iter(),
        ) {
            subst.insert(gen_param.id, instance_arg.clone());
        }
        param_tys = variant_def.fields.iter()
            .map(|f| f.ty.apply_subst(&subst))
            .collect::<Vec<_>>();
    }

    // The return type of the constructor is the instantiated enum type itself
    let return_ty = checker.resolve_type(&instantiated_enum_ty);

    Ok(Ty::with_span(
        TyKind::Function(param_tys, Arc::new(return_ty)),
        span,
    ))
}

// --- Type Resolution Helpers --- 

/// Resolves a `ResolvedType` into a `Ty` within the current checker context.
#[allow(dead_code)]
pub(crate) fn resolve_type_to_ty(
    checker: &mut TypeChecker,
    resolved_ty: &ResolvedType,
) -> Result<Ty, TypeError> {
    // Use a dummy span initially, try to get a better one from context if possible
    // TODO: Thread spans through resolution better.
    let span = TypeChecker::dummy_span(); // Corrected call

    println!("[ResolveType] Resolving: {:?}", resolved_ty);
    println!("[ResolveType] Current generic scopes: {:?}", checker.generic_scopes);

    match resolved_ty {
        ResolvedType::Primitive(prim) => {
            let parallax_prim = resolver_primitive_to_parallax(*prim)?;
            Ok(Ty::with_span(TyKind::Primitive(parallax_prim), span))
        }
        ResolvedType::IntegerLiteral => {
            Ok(Ty::with_span(TyKind::Primitive(ParallaxPrimitiveType::IntegerLiteral), span))
        }
        ResolvedType::FloatLiteral => {
            Ok(Ty::with_span(TyKind::Primitive(ParallaxPrimitiveType::FloatLiteral), span))
        }
        ResolvedType::Unknown => {
            // Represent Unknown as Error type internally
            Ok(Ty::with_span(TyKind::Error, span))
        }
        ResolvedType::Never => Ok(Ty::with_span(TyKind::Never, span)),
        ResolvedType::Tuple(types) => {
            let mut inner_tys = Vec::with_capacity(types.len());
            for rt in types {
                inner_tys.push(resolve_type_to_ty(checker, rt)?); // Recursive call
            }
            Ok(Ty::with_span(TyKind::Tuple(inner_tys), span))
        }
        ResolvedType::Function {
            param_types,
            return_type,
        } => {
            let mut param_tys = Vec::with_capacity(param_types.len());
            for rt in param_types {
                param_tys.push(resolve_type_to_ty(checker, rt)?);
            }
            let ret_ty = resolve_type_to_ty(checker, return_type)?;
            Ok(Ty::with_span(
                TyKind::Function(param_tys, Arc::new(ret_ty)),
                span,
            ))
        }
        ResolvedType::Array { element_type, size } => {
            let elem_ty = resolve_type_to_ty(checker, element_type)?;
            let array_size = size.unwrap_or(0);
            Ok(Ty::with_span(
                TyKind::Array(Arc::new(elem_ty), array_size),
                span,
            ))
        }
        ResolvedType::UserDefined { symbol, type_args } => {
            println!("[ResolveType]   UserDefined: Symbol {:?}, Args: {:?}", symbol, type_args);
            let type_symbol = *symbol;
            let type_name = match checker.get_name_for_symbol(type_symbol) {
                Ok(name) => name,
                Err(_) => {
                    checker.errors.push(TypeError::InternalError {
                        message: format!(
                            "Failed to find name for resolved user type symbol {:?}",
                            type_symbol
                        ),
                        span: Some(span),
                    });
                    return Ok(Ty::with_span(TyKind::Error, span));
                }
            };

            // Use explicit loop for type arguments
            let mut resolved_args = Vec::new();
            if let Some(args) = type_args {
                resolved_args.reserve(args.len());
                for rt in args {
                    resolved_args.push(resolve_type_to_ty(checker, rt)?);
                }
            }

            Ok(Ty::with_span(
                TyKind::Named {
                    name: type_name,
                    symbol: Some(type_symbol),
                    args: resolved_args,
                },
                span,
            ))
        }
        ResolvedType::GenericParam(name) => {
            println!("[ResolveType]   GenericParam: {}", name);
            // Search active generic scopes (innermost first)
            for scope in checker.generic_scopes.iter().rev() {
                if let Some(type_id) = scope.get(name) {
                    println!("[ResolveType]     Found in scope -> Var({:?})", type_id);
                    return Ok(Ty::with_span(TyKind::Var(*type_id), span));
                }
            }
            println!("[ResolveType]     NOT FOUND in scopes!");
            let err = TypeError::UnknownIdentifier {
                name: name.clone(),
                span,
            };
            checker.errors.push(err.clone());
            Ok(Ty::with_span(TyKind::Error, span))
        }
        ResolvedType::SelfType => {
            println!("[ResolveType]   SelfType -> TyKind::SelfType");
            // Return the dedicated SelfType kind
            Ok(Ty::with_span(TyKind::SelfType, span))
        }
        ResolvedType::Trait(symbol) => {
            // A trait itself is not a concrete type.
            // Depending on context, this might be an error or handled differently.
            // TODO: Determine correct handling for ResolvedType::Trait.
            // For now, report an error or return TyKind::Error.
            let trait_name = checker.get_name_for_symbol(*symbol).unwrap_or_else(|_| "unknown trait".to_string());
            checker.errors.push(TypeError::TraitUsedAsType { span, trait_name });
            Ok(Ty::new(TyKind::Error))
        }
    }
}

/// Helper to resolve a single generic parameter definition.
#[allow(dead_code)]
pub(crate) fn resolve_single_generic_param(
    checker: &mut TypeChecker,
    param_def: &ResolvedGenericParamDef,
    _context: Option<(DefinitionKind, Symbol)>, // Use imported DefinitionKind
) -> TypeResult<GenericParamDef> {
    let fresh_var_ty = checker.inference_ctx.fresh_var_with_span(param_def.span);
    let type_id = if let TyKind::Var(id) = fresh_var_ty.kind {
        id
    } else {
        panic!("Internal Error: Fresh var was not TyKind::Var")
    };

    let mut bounds = Vec::new();
    for bound_symbol in &param_def.bounds {
        // Look up the symbol to see if it's a known trait ID first.
        if let Ok(trait_id) = checker.lookup_trait_id_by_symbol(*bound_symbol) {
            bounds.push(TraitRef {
                trait_id,
                type_arguments: Vec::new(),
                span: param_def.span, // Use param_def span for the bound
            });
        } else {
            // If lookup_trait_id_by_symbol failed, it means the symbol either:
            // 1. Doesn't exist (handled by UnknownTrait).
            // 2. Exists but isn't a trait (handled by NotATrait).

            // Check if the symbol exists in the general definitions.
            if let Some((_kind, name, _span)) = checker.get_definition_info(*bound_symbol) {
                 // Symbol exists, but wasn't a trait ID. Report NotATrait.
                 checker.errors.push(TypeError::NotATrait {
                    found: name.clone(),
                    span: param_def.span, // Span where the bound is used
                });
                 // Add an empty TraitRef or a special marker? For now, just record the error.
            } else {
                // Symbol doesn't exist at all. Report UnknownTrait.
                 checker.errors.push(TypeError::UnknownTrait {
                    name: format!("symbol_{}", bound_symbol.id()), // Placeholder name
                    span: param_def.span,
                });
            }
        }
    }

    Ok(GenericParamDef {
        name: param_def.name.clone(),
        symbol: Symbol::fresh(), // Assign a fresh symbol here?
        id: type_id,
        bounds,
        span: param_def.span,
    })
}

#[allow(dead_code)]
fn resolver_primitive_to_parallax(prim: ResolverPrimitiveType) -> Result<ParallaxPrimitiveType, TypeError> {
    match prim {
        ResolverPrimitiveType::I8   => Ok(ParallaxPrimitiveType::I8),
        ResolverPrimitiveType::I16  => Ok(ParallaxPrimitiveType::I16),
        ResolverPrimitiveType::I32  => Ok(ParallaxPrimitiveType::I32),
        ResolverPrimitiveType::I64  => Ok(ParallaxPrimitiveType::I64),
        ResolverPrimitiveType::I128 => Ok(ParallaxPrimitiveType::I128),
        ResolverPrimitiveType::U8   => Ok(ParallaxPrimitiveType::U8),
        ResolverPrimitiveType::U16  => Ok(ParallaxPrimitiveType::U16),
        ResolverPrimitiveType::U32  => Ok(ParallaxPrimitiveType::U32),
        ResolverPrimitiveType::U64  => Ok(ParallaxPrimitiveType::U64),
        ResolverPrimitiveType::U128 => Ok(ParallaxPrimitiveType::U128),
        ResolverPrimitiveType::F32  => Ok(ParallaxPrimitiveType::F32),
        ResolverPrimitiveType::F64  => Ok(ParallaxPrimitiveType::F64),
        ResolverPrimitiveType::Bool => Ok(ParallaxPrimitiveType::Bool),
        ResolverPrimitiveType::Char => Ok(ParallaxPrimitiveType::Char),
        ResolverPrimitiveType::String => Ok(ParallaxPrimitiveType::String),
        ResolverPrimitiveType::Unit => Ok(ParallaxPrimitiveType::Unit),
        ResolverPrimitiveType::IntegerLiteral => Ok(ParallaxPrimitiveType::IntegerLiteral),
        ResolverPrimitiveType::FloatLiteral => Ok(ParallaxPrimitiveType::FloatLiteral),
    }
}

// Add a helper function (or define it elsewhere) to convert back if needed
// This might not be the best place, but for a quick fix:
#[allow(dead_code)]
pub(crate) fn convert_symbol_back(s: Symbol) -> parallax_resolve::types::Symbol {
    parallax_resolve::types::Symbol(s.id())
} 

// Note: instantiate and substitute_self helpers moved to checker/generics.rs and checker/substitute.rs respectively. 

#[cfg(test)]
mod tests {
    use super::*;
    // Use specific imports from public paths
    use crate::{
        checker::TypeChecker,
        context::{TypeEnvironment, TraitRepository, inference::InferenceContext}, 
        context::trait_repo::TraitId, // Import TraitId explicitly
        types::{Ty, TyKind, TypeId, PrimitiveType, TypeDef, StructDef, EnumDef, EnumVariant, Field, GenericParamDef, TraitRef}, // Import types directly from crate::types
        TypeDatabase,
        error::{TypeError, TypeResult},
        TypeContext, // Import TypeContext from crate root
    };
    use parallax_resolve::{ResolveDatabase, types::{Symbol, ResolvedModuleStructure, ResolvedDefinitions, ResolvedType, ResolvedGenericParamDef, PrimitiveType as ResolverPrimitiveType}, definitions::DefinitionKind};
    use parallax_syntax::SyntaxDatabase;
    use parallax_source::SourceDatabase;
    use miette::SourceSpan;
    use salsa::Database;
    use std::collections::HashMap;
    use std::sync::{Arc, Mutex};

    // --- Complete Dummy DB Setup ---
    #[salsa::db]
    #[derive(Default, Clone)]
    pub struct DummyDb {
        storage: salsa::Storage<Self>,
    }

    impl salsa::Database for DummyDb {
        fn salsa_event(&self, event: &dyn Fn() -> salsa::Event) {}
    }

    #[salsa::db]
    impl SourceDatabase for DummyDb {}
    #[salsa::db]
    impl SyntaxDatabase for DummyDb {}
    #[salsa::db]
    impl ResolveDatabase for DummyDb {}
    #[salsa::db]
    impl TypeDatabase for DummyDb {}
    // --- End Dummy DB Setup ---

    fn dummy_span() -> SourceSpan {
        SourceSpan::from((0, 0))
    }

    fn ty_prim(prim: PrimitiveType) -> Ty {
        Ty::with_span(TyKind::Primitive(prim), dummy_span())
    }

    fn ty_var(id: u32) -> Ty {
        Ty::with_span(TyKind::Var(TypeId(id)), dummy_span())
    }

     fn ty_named(name: &str, symbol: Option<Symbol>, args: Vec<Ty>) -> Ty {
        Ty::with_span(TyKind::Named { name: name.to_string(), symbol, args }, dummy_span())
    }

    fn ty_tuple(tys: Vec<Ty>) -> Ty {
        Ty::with_span(TyKind::Tuple(tys), dummy_span())
    }

     fn ty_func(params: Vec<Ty>, ret: Ty) -> Ty {
        Ty::with_span(TyKind::Function(params, Arc::new(ret)), dummy_span())
    }

    fn setup_checker() -> (DummyDb, ResolvedDefinitions, TypeChecker<'static>) {
        let db_mock = DummyDb::default();
        let db_leaked: &'static DummyDb = Box::leak(Box::new(db_mock));
        let defs_leaked: &'static ResolvedDefinitions = Box::leak(Box::new(ResolvedDefinitions::default()));

        let type_ctx = TypeContext::new();
        let trait_repo = TraitRepository::new();
        let checker = TypeChecker::new(db_leaked, defs_leaked, type_ctx, trait_repo);
        (db_leaked.clone(), defs_leaked.clone(), checker)
    }

    fn setup_checker_with_types() -> TypeChecker<'static> {
        let (_db, _defs, mut checker) = setup_checker();

        let struct_sym = Symbol::new(1);
        let enum_sym = Symbol::new(2);
        let var1_sym = Symbol::new(3);
        let var2_sym = Symbol::new(4);
        let gen_param_id = TypeId(100);

        let struct_def = StructDef {
            name: "MyStruct".to_string(),
            symbol: struct_sym,
            generic_params: vec![GenericParamDef {
                name: "T".to_string(), symbol: Symbol::new(10), id: gen_param_id, bounds: vec![], span: dummy_span()
            }],
            fields: vec![Field { name: "data".to_string(), symbol: Symbol::new(11), ty: ty_var(gen_param_id.0), span: dummy_span() }],
            span: dummy_span(),
        };
        checker.type_ctx.add_type(struct_sym, "MyStruct".to_string(), TypeDef::Struct(struct_def));
        checker.type_ctx.add_symbol_name(Symbol::new(10), "T".to_string());

        let enum_def = EnumDef {
            name: "MyEnum".to_string(),
            symbol: enum_sym,
            generic_params: vec![],
            variants: vec![
                EnumVariant { name: "Var1".to_string(), symbol: var1_sym, fields: vec![], span: dummy_span() },
                EnumVariant {
                    name: "Var2".to_string(), symbol: var2_sym,
                    fields: vec![Field { name: "_0".to_string(), symbol: Symbol::new(12), ty: ty_prim(PrimitiveType::I32), span: dummy_span() }],
                    span: dummy_span()
                },
            ],
            span: dummy_span(),
        };
        checker.type_ctx.add_type(enum_sym, "MyEnum".to_string(), TypeDef::Enum(enum_def));
        checker.type_ctx.add_symbol_name(var1_sym, "Var1".to_string());
        checker.type_ctx.add_symbol_name(var2_sym, "Var2".to_string());

        let mut current_scope = HashMap::new();
        current_scope.insert("T".to_string(), TypeId(gen_param_id.0));
        checker.generic_scopes.push(current_scope);

        checker
    }

    #[test]
    fn test_resolve_type_primitive() {
        let (_db, _defs, mut checker) = setup_checker();
        let resolved = ResolvedType::Primitive(ResolverPrimitiveType::Bool);
        let ty = resolve_type_to_ty(&mut checker, &resolved).unwrap();
        assert_eq!(ty.kind, TyKind::Primitive(PrimitiveType::Bool));
    }

    #[test]
    fn test_resolve_type_literals() {
        let (_db, _defs, mut checker) = setup_checker();
        let resolved_int = ResolvedType::IntegerLiteral;
        let ty_int = resolve_type_to_ty(&mut checker, &resolved_int).unwrap();
        assert_eq!(ty_int.kind, TyKind::Primitive(PrimitiveType::IntegerLiteral));

        let resolved_float = ResolvedType::FloatLiteral;
        let ty_float = resolve_type_to_ty(&mut checker, &resolved_float).unwrap();
        assert_eq!(ty_float.kind, TyKind::Primitive(PrimitiveType::FloatLiteral));
    }

    #[test]
    fn test_resolve_type_unknown_never() {
        let (_db, _defs, mut checker) = setup_checker();
        let resolved_unknown = ResolvedType::Unknown;
        let ty_unknown = resolve_type_to_ty(&mut checker, &resolved_unknown).unwrap();
        assert_eq!(ty_unknown.kind, TyKind::Error);

        let resolved_never = ResolvedType::Never;
        let ty_never = resolve_type_to_ty(&mut checker, &resolved_never).unwrap();
        assert_eq!(ty_never.kind, TyKind::Never);
    }

    #[test]
    fn test_resolve_type_tuple() {
        let (_db, _defs, mut checker) = setup_checker();
        let resolved = ResolvedType::Tuple(vec![
            ResolvedType::Primitive(ResolverPrimitiveType::I32),
            ResolvedType::Primitive(ResolverPrimitiveType::String),
        ]);
        let ty = resolve_type_to_ty(&mut checker, &resolved).unwrap();
        match ty.kind {
            TyKind::Tuple(elems) => {
                assert_eq!(elems.len(), 2);
                assert_eq!(elems[0].kind, TyKind::Primitive(PrimitiveType::I32));
                assert_eq!(elems[1].kind, TyKind::Primitive(PrimitiveType::String));
            }
            _ => panic!("Expected Tuple kind"),
        }
    }

    #[test]
    fn test_resolve_type_function() {
        let (_db, _defs, mut checker) = setup_checker();
        let resolved = ResolvedType::Function {
            param_types: vec![ResolvedType::Primitive(ResolverPrimitiveType::F64)],
            return_type: Box::new(ResolvedType::Primitive(ResolverPrimitiveType::Bool)),
        };
        let ty = resolve_type_to_ty(&mut checker, &resolved).unwrap();
        match ty.kind {
            TyKind::Function(params, ret) => {
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].kind, TyKind::Primitive(PrimitiveType::F64));
                assert_eq!(ret.kind, TyKind::Primitive(PrimitiveType::Bool));
            }
            _ => panic!("Expected Function kind"),
        }
    }

    #[test]
    fn test_resolve_type_array() {
        let (_db, _defs, mut checker) = setup_checker();
        let resolved = ResolvedType::Array {
            element_type: Box::new(ResolvedType::Primitive(ResolverPrimitiveType::U8)),
            size: Some(10),
        };
        let ty = resolve_type_to_ty(&mut checker, &resolved).unwrap();
        match ty.kind {
            TyKind::Array(elem, size) => {
                assert_eq!(elem.kind, TyKind::Primitive(PrimitiveType::U8));
                assert_eq!(size, 10);
            }
            _ => panic!("Expected Array kind"),
        }
    }

    #[test]
    fn test_resolve_type_user_defined_no_args() {
        let mut checker = setup_checker_with_types();
        let enum_sym = Symbol::new(2);
        let resolved = ResolvedType::UserDefined { symbol: enum_sym, type_args: None };
        let ty = resolve_type_to_ty(&mut checker, &resolved).unwrap();
        match ty.kind {
            TyKind::Named { name, symbol, args } => {
                assert_eq!(name, "MyEnum");
                assert_eq!(symbol, Some(enum_sym));
                assert!(args.is_empty());
            }
            _ => panic!("Expected Named kind"),
        }
    }

     #[test]
    fn test_resolve_type_user_defined_with_args() {
        let mut checker = setup_checker_with_types();
        let struct_sym = Symbol::new(1);
        let resolved = ResolvedType::UserDefined {
            symbol: struct_sym,
            type_args: Some(vec![ResolvedType::Primitive(ResolverPrimitiveType::I32)]),
        };
        let ty = resolve_type_to_ty(&mut checker, &resolved).unwrap();
        match ty.kind {
            TyKind::Named { name, symbol, args } => {
                assert_eq!(name, "MyStruct");
                assert_eq!(symbol, Some(struct_sym));
                assert_eq!(args.len(), 1);
                assert_eq!(args[0].kind, TyKind::Primitive(PrimitiveType::I32));
            }
            _ => panic!("Expected Named kind"),
        }
    }

    #[test]
    fn test_resolve_type_generic_param_in_scope() {
        let mut checker = setup_checker_with_types();
        let resolved = ResolvedType::GenericParam("T".to_string());
        let ty = resolve_type_to_ty(&mut checker, &resolved).unwrap();
        assert_eq!(ty.kind, TyKind::Var(TypeId(100)));
    }

    #[test]
    fn test_resolve_type_generic_param_not_in_scope() {
        let mut checker = setup_checker_with_types();
        checker.generic_scopes.pop();
        let resolved = ResolvedType::GenericParam("T".to_string());
        let ty = resolve_type_to_ty(&mut checker, &resolved).unwrap();
        assert_eq!(ty.kind, TyKind::Error);
        assert!(!checker.errors.is_empty());
        assert!(matches!(checker.errors[0], TypeError::UnknownIdentifier { ref name, .. } if name == "T"));
    }

    #[test]
    fn test_resolve_type_self() {
        let (_db, _defs, mut checker) = setup_checker();
        let resolved = ResolvedType::SelfType;
        let ty = resolve_type_to_ty(&mut checker, &resolved).unwrap();
        assert_eq!(ty.kind, TyKind::SelfType);
    }

    #[test]
    fn test_resolve_variant_names_ok() {
        let mut checker = setup_checker_with_types();
        let var1_sym = Symbol::new(3);
        let var2_sym = Symbol::new(4);

        let (enum_name1, variant_name1) = resolve_variant_symbol_to_names(&mut checker, var1_sym, dummy_span()).unwrap();
        assert_eq!(enum_name1, "MyEnum");
        assert_eq!(variant_name1, "Var1");

        let (enum_name2, variant_name2) = resolve_variant_symbol_to_names(&mut checker, var2_sym, dummy_span()).unwrap();
        assert_eq!(enum_name2, "MyEnum");
        assert_eq!(variant_name2, "Var2");
    }

    #[test]
    fn test_resolve_variant_names_fail_not_variant() {
        let mut checker = setup_checker_with_types();
        let struct_sym = Symbol::new(1);
        let result = resolve_variant_symbol_to_names(&mut checker, struct_sym, dummy_span());
        assert!(result.is_err());
        assert!(matches!(result.err().unwrap(), TypeError::InternalError { .. }));
    }

    #[test]
    fn test_resolve_variant_names_fail_unknown_symbol() {
        let (_db, _defs, mut checker) = setup_checker();
        let unknown_sym = Symbol::new(999);
        let result = resolve_variant_symbol_to_names(&mut checker, unknown_sym, dummy_span());
        assert!(result.is_err());
         assert!(matches!(result.err().unwrap(), TypeError::InternalError { .. }));
    }

    #[test]
    fn test_variant_constructor_type_unit() {
        let mut checker = setup_checker_with_types();
        let var1_sym = Symbol::new(3);
        let constructor_ty = determine_variant_constructor_type(&mut checker, var1_sym, dummy_span()).unwrap();

        match constructor_ty.kind {
            TyKind::Function(params, ret) => {
                assert!(params.is_empty());
                match ret.kind {
                    TyKind::Named { ref name, symbol, .. } => {
                        assert_eq!(name, "MyEnum");
                        assert_eq!(symbol, Some(Symbol::new(2)));
                    }
                    _ => panic!("Expected return type MyEnum"),
                }
            }
            _ => panic!("Expected Function kind"),
        }
    }

     #[test]
    fn test_variant_constructor_type_tuple() {
        let mut checker = setup_checker_with_types();
        let var2_sym = Symbol::new(4);
        let constructor_ty = determine_variant_constructor_type(&mut checker, var2_sym, dummy_span()).unwrap();

        match constructor_ty.kind {
            TyKind::Function(params, ret) => {
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].kind, TyKind::Primitive(PrimitiveType::I32));
                match ret.kind {
                    TyKind::Named { ref name, symbol, .. } => {
                        assert_eq!(name, "MyEnum");
                        assert_eq!(symbol, Some(Symbol::new(2)));
                    }
                    _ => panic!("Expected return type MyEnum"),
                }
            }
            _ => panic!("Expected Function kind"),
        }
    }

    #[test]
    fn test_resolve_generic_param_no_bounds() {
        let (_db, _defs, mut checker) = setup_checker();
        let resolved_param = ResolvedGenericParamDef {
            name: "T".to_string(),
            bounds: vec![],
            span: dummy_span(),
        };

        let generic_param_def = resolve_single_generic_param(&mut checker, &resolved_param, None).unwrap();

        assert_eq!(generic_param_def.name, "T");
        // Use checker.resolve_type (public method) again
        assert!(matches!(checker.resolve_type(&ty_var(generic_param_def.id.0)).kind, TyKind::Var(_)));
        assert!(generic_param_def.bounds.is_empty());
    }
} 