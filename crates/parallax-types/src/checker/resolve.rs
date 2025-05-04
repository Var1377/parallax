// src/checker/resolve.rs
//! Helpers for resolving AST type annotations into checker::Ty types.

use super::TypeChecker;
use crate::error::{TypeError, TypeResult};
use crate::types::{Ty, TyKind, PrimitiveType, GenericParamDef, FunctionSignature, TypeDef, TraitRef};
use crate::context::inference::TypeId;
use parallax_resolve::{
    types::{ResolvedType, PrimitiveType as ResolverPrimitive, Symbol, ResolvedGenericParamDef},
    definitions::DefinitionKind
};
use miette::SourceSpan;
use std::sync::Arc;

/// Extension trait for helper methods on ResolvedType
trait ResolvedTypeExt {
    fn get_span(&self) -> SourceSpan;
}

impl ResolvedTypeExt for ResolvedType {
    fn get_span(&self) -> SourceSpan {
        // Return a default span since ResolvedType currently doesn't store spans for all variants
        SourceSpan::from(0..0)
    }
}

/// Resolves an AST type annotation into a `Ty` within the checker's context.
/// Handles resolving names to symbols, applying generic arguments, and creating inference variables.
///
/// Preconditions: `checker` context is set up.
///                `resolved_ty` is the resolved type from the resolver.
/// Postconditions: Returns `Ok(Ty)` representing the resolved type.
///                 Returns `Err(TypeError)` if resolution fails (e.g., unknown type name).
pub(crate) fn resolve_type_to_ty(checker: &mut TypeChecker, resolved_ty: &ResolvedType) -> TypeResult<Ty> {
    match resolved_ty {
        ResolvedType::Primitive(prim) => Ok(Ty::with_span(TyKind::Primitive(map_resolver_primitive(*prim)), resolved_ty.get_span())),
        ResolvedType::UserDefined { symbol, type_args } => {
            if let Some(param_def) = checker.lookup_generic_param_by_symbol(symbol) {
                if type_args.is_some() {
                    return Err(TypeError::GenericArgCountMismatch {
                        kind: "Generic Parameter".to_string(),
                        name: param_def.name.clone(),
                        expected: 0,
                        found: type_args.as_ref().map_or(0, |v| v.len()),
                        span: resolved_ty.get_span(),
                    });
                }
                return Ok(Ty::with_span(TyKind::Var(param_def.id), resolved_ty.get_span()));
            } else if checker.type_ctx.get_definition_kind(symbol) == Some(&DefinitionKind::AssociatedType) {
                println!("Warning: Associated type resolution not fully implemented for {:?}", symbol);
                return Ok(checker.fresh_var());
            }

            // Get the type name from the definition in the type context
            let type_name = match checker.type_ctx.get_definition(symbol) {
                Some(TypeDef::Struct(sd)) => sd.name.clone(),
                Some(TypeDef::Enum(ed)) => ed.name.clone(),
                Some(TypeDef::Function(fs)) => fs.name.clone(), 
                None => format!("Unknown_{}", symbol.0), // Fallback name using symbol value
            };

            let mut resolved_args = Vec::new();
            if let Some(args) = type_args {
                for arg_ty in args {
                    match resolve_type_to_ty(checker, arg_ty) {
                        Ok(ty) => resolved_args.push(ty),
                        Err(e) => {
                            checker.report_error(e);
                            resolved_args.push(Ty::new(TyKind::Error));
                        }
                    }
                }
            }

            match checker.type_ctx.get_definition(symbol) {
                Some(TypeDef::Struct(sd)) => {
                    if sd.generic_params.len() != resolved_args.len() {
                        checker.report_error(TypeError::GenericArgCountMismatch {
                            kind: "Struct".to_string(), 
                            name: sd.name.clone(), 
                            expected: sd.generic_params.len(), 
                            found: resolved_args.len(), 
                            span: resolved_ty.get_span()
                        });
                    }
                }
                Some(TypeDef::Enum(ed)) => {
                    if ed.generic_params.len() != resolved_args.len() {
                        checker.report_error(TypeError::GenericArgCountMismatch {
                            kind: "Enum".to_string(), 
                            name: ed.name.clone(), 
                            expected: ed.generic_params.len(), 
                            found: resolved_args.len(), 
                            span: resolved_ty.get_span()
                        });
                    }
                }
                Some(TypeDef::Function(_)) => { /* This shouldn't happen for a Named type */ }
                None => { /* Resolver should guarantee symbol exists for ResolvedType::Named */ }
            }

            Ok(Ty::with_span(TyKind::Named {
                name: type_name,
                symbol: Some(*symbol),
                args: resolved_args,
            }, resolved_ty.get_span()))
        }
        ResolvedType::Tuple(elements) => {
            let resolved_elements: Vec<_> = elements.iter()
                .map(|el| resolve_type_to_ty(checker, el))
                .collect::<Result<_, _>>()?;
            Ok(Ty::with_span(TyKind::Tuple(resolved_elements), resolved_ty.get_span()))
        }
        ResolvedType::Array { element_type, size } => {
            let resolved_element = resolve_type_to_ty(checker, element_type)?;
            let concrete_size = size.ok_or_else(|| TypeError::InternalError { 
                message: "Array size not resolved".to_string(), 
                span: Some(resolved_ty.get_span()) 
            })?;
            Ok(Ty::with_span(TyKind::Array(Arc::new(resolved_element), Some(concrete_size)), resolved_ty.get_span()))
        }
        ResolvedType::Function { param_types, return_type } => {
            let resolved_params: Vec<_> = param_types.iter()
                .map(|p| resolve_type_to_ty(checker, p))
                .collect::<Result<_, _>>()?;
            let resolved_return = resolve_type_to_ty(checker, return_type)?;
            Ok(Ty::with_span(TyKind::Function(resolved_params, Arc::new(resolved_return)), resolved_ty.get_span()))
        }
        ResolvedType::SelfType => {
            // Allow conversion to TyKind::SelfType here.
            // Validation of whether Self is used correctly will happen during
            // unification or substitution later when the context is known.
            Ok(Ty::with_span(TyKind::SelfType, resolved_ty.get_span()))
        }
        ResolvedType::Unknown => Ok(checker.fresh_var()),
        ResolvedType::GenericParam(name) => {
            // Look for the generic parameter in scopes - using the name directly as String now
            for scope in checker.generic_scopes.iter().rev() {
                // Get the GenericParamDef by name directly since we're now using a name-based map
                if let Some(param_def) = scope.get(name) {
                    return Ok(Ty::with_span(TyKind::Var(param_def.id), resolved_ty.get_span()));
                }
            }
            // Not found, create a fresh type variable (could also return error)
            println!("Warning: Generic parameter {} not found in scope, using fresh var", name);
            Ok(checker.fresh_var())
        }
        // Other cases
        ResolvedType::Never => Ok(Ty::with_span(TyKind::Never, resolved_ty.get_span())),
        ResolvedType::Trait(_) => {
            // Traits as types may need special handling
            println!("Warning: Treating trait as type not fully implemented");
            Ok(checker.fresh_var())
        }
        ResolvedType::IntegerLiteral | ResolvedType::FloatLiteral => {
            // These are type inference placeholders
            Ok(checker.fresh_var())
        }
    }
}

/// Resolves a single generic parameter definition from the resolver AST.
/// Assigns a fresh `TypeId` and resolves trait bounds.
///
/// Preconditions: `checker` context is set up.
///                `resolved_param` is the generic parameter definition from the resolver.
///                `definition_context` optional info about where the param is defined (for errors).
/// Postconditions: Returns `Ok(GenericParamDef)` on success.
///                 Returns `Err(TypeError)` if bound resolution fails.
pub(crate) fn resolve_single_generic_param(
    checker: &mut TypeChecker,
    resolved_param: &ResolvedGenericParamDef,
    _definition_context: Option<(DefinitionKind, Symbol)>,
) -> TypeResult<GenericParamDef> {
    let param_name = resolved_param.name.clone();
    let param_span = resolved_param.span;

    let param_ty_id = checker.fresh_var().kind.expect_var_id();

    // Create a new Symbol for this parameter if needed
    let param_symbol = Symbol::new(param_ty_id.0);
    
    let mut checker_bounds = Vec::new();
    for trait_symbol in &resolved_param.bounds {
        let trait_def_opt = checker.trait_repo.get_trait_by_symbol(trait_symbol).cloned();
        match trait_def_opt {
            Some(trait_def) => {
                // No type arguments for the bound trait (simple bound like T: Display)
                checker_bounds.push(TraitRef {
                    trait_id: trait_def.id,
                    type_arguments: vec![],
                    span: resolved_param.span,
                });
            }
            None => {
                checker.report_error(TypeError::NotATrait {
                    found: format!("symbol_{:?}", trait_symbol),
                    span: resolved_param.span,
                });
            }
        }
    }

    Ok(GenericParamDef {
        name: param_name,
        symbol: param_symbol,
        id: param_ty_id,
        bounds: checker_bounds,
        span: param_span,
    })
}

/// Maps resolver primitive types to checker primitive types.
pub(crate) fn map_resolver_primitive(prim: ResolverPrimitive) -> PrimitiveType {
    match prim {
        ResolverPrimitive::I8 => PrimitiveType::I8,
        ResolverPrimitive::I16 => PrimitiveType::I16,
        ResolverPrimitive::I32 => PrimitiveType::I32,
        ResolverPrimitive::I64 => PrimitiveType::I64,
        ResolverPrimitive::I128 => PrimitiveType::I128,
        ResolverPrimitive::U8 => PrimitiveType::U8,
        ResolverPrimitive::U16 => PrimitiveType::U16,
        ResolverPrimitive::U32 => PrimitiveType::U32,
        ResolverPrimitive::U64 => PrimitiveType::U64,
        ResolverPrimitive::U128 => PrimitiveType::U128,
        ResolverPrimitive::F32 => PrimitiveType::F32,
        ResolverPrimitive::F64 => PrimitiveType::F64,
        ResolverPrimitive::Bool => PrimitiveType::Bool,
        ResolverPrimitive::Char => PrimitiveType::Char,
        ResolverPrimitive::String => PrimitiveType::String,
        ResolverPrimitive::Unit => PrimitiveType::Unit,
        ResolverPrimitive::IntegerLiteral => PrimitiveType::I32, // Default for literals
        ResolverPrimitive::FloatLiteral => PrimitiveType::F64, // Default for literals
    }
}

impl TyKind {
    pub(crate) fn expect_var_id(&self) -> TypeId {
        match self {
            TyKind::Var(id) => *id,
            _ => panic!("Expected TyKind::Var, found {:?}", self),
        }
    }
}

impl TypeChecker<'_> {
    pub(crate) fn lookup_generic_param_by_symbol(&self, symbol: &Symbol) -> Option<&GenericParamDef> {
        for scope in self.generic_scopes.iter().rev() {
            for param_def in scope.values() {
                if param_def.symbol == *symbol {
                    return Some(param_def);
                }
            }
        }
        None
    }
}

impl FunctionSignature {
    pub(crate) fn dummy() -> Self {
        FunctionSignature {
            name: "<dummy_generic>".to_string(),
            self_param: None,
            generic_params: vec![],
            params: vec![],
            return_type: Ty::new(TyKind::Error),
            span: SourceSpan::from(0..0),
        }
    }
} 