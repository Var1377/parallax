use std::collections::HashMap;
use miette::SourceSpan;
use parallax_resolve::types::Symbol;
use crate::{
    error::{TypeError, TypeResult},
    inference::Substitution,
    typecheck::TypeChecker,
    types::{Ty, TyKind, TypeDef, TypeId, GenericParamDef},
};
use std::sync::Arc;

// Placeholder helper - replace with actual symbol resolution
pub(crate) fn resolve_variant_symbol_to_names(
    checker: &mut TypeChecker,
    variant_symbol: Symbol,
    span: SourceSpan // Use the span parameter
) -> TypeResult<(String, String)> {
     println!("Warning: Using placeholder variant name lookup for symbol {}", variant_symbol.id());
     // Inefficient placeholder: Iterate all known enums and their variants
     for (_name, type_def) in checker.type_ctx.all_types() { // Use public method
         if let TypeDef::Enum(enum_def) = type_def {
            if let Some(variant) = enum_def.variants.iter().find(|v| v.symbol == variant_symbol) {
                 // Found the variant with the matching symbol.
                 println!(" Placeholder: Found variant name '{}' in enum '{}' for symbol {}", variant.name, enum_def.name, variant_symbol.id());
                 return Ok((enum_def.name.clone(), variant.name.clone()));
            }
         }
     }

    Err(TypeError::InternalError { 
        message: format!("Placeholder lookup failed for variant symbol ID: {}", variant_symbol.id()),
        span: Some(span), // TODO: Add span? <-- Use span here
    })
}

/// Recursively substitutes `TyKind::SelfType` with `implementing_type`.
/// Includes a recursion limit to prevent infinite loops.
pub fn substitute_self(ty: &Ty, implementing_type: &Ty, recursion_limit: u32) -> Ty {
    if recursion_limit == 0 {
        // Consider reporting an error or returning a special error type
        return Ty::new(TyKind::Error); // Return Error type if limit exceeded
    }

    let new_kind = match &ty.kind {
        TyKind::SelfType => return implementing_type.clone(), // Base case: substitute Self
        TyKind::Var(_) | TyKind::Primitive(_) | TyKind::Error | TyKind::Never => {
            // These types cannot contain SelfType
            return ty.clone();
        }
        TyKind::Named { name, args } => {
            let new_args = args
                .iter()
                .map(|arg| substitute_self(arg, implementing_type, recursion_limit - 1))
                .collect();
            TyKind::Named {
                name: name.clone(),
                args: new_args,
            }
        }
        TyKind::Array(elem_ty, size) => {
            let new_elem_ty = substitute_self(elem_ty, implementing_type, recursion_limit - 1);
            TyKind::Array(Arc::new(new_elem_ty), *size)
        }
        TyKind::Tuple(tys) => {
            let new_tys = tys
                .iter()
                .map(|t| substitute_self(t, implementing_type, recursion_limit - 1))
                .collect();
            TyKind::Tuple(new_tys)
        }
        TyKind::Function(params, ret) => {
            let new_params = params
                .iter()
                .map(|p| substitute_self(p, implementing_type, recursion_limit - 1))
                .collect();
            let new_ret = substitute_self(ret, implementing_type, recursion_limit - 1);
            TyKind::Function(new_params, Arc::new(new_ret))
        }
        TyKind::Map(key_ty, value_ty) => {
            let new_key_ty = substitute_self(key_ty, implementing_type, recursion_limit - 1);
            let new_value_ty = substitute_self(value_ty, implementing_type, recursion_limit - 1);
            TyKind::Map(Arc::new(new_key_ty), Arc::new(new_value_ty))
        }
        TyKind::Set(elem_ty) => {
            let new_elem_ty = substitute_self(elem_ty, implementing_type, recursion_limit - 1);
            TyKind::Set(Arc::new(new_elem_ty))
        }
    };

    Ty {
        kind: new_kind,
        span: ty.span, // Preserve original span if possible
    }
}

/// Helper function to check if a type contains SelfType (avoids unnecessary substitution).
pub fn contains_self(ty: &Ty) -> bool {
    match &ty.kind {
        TyKind::SelfType => true,
        TyKind::Named { args, .. } => args.iter().any(contains_self),
        TyKind::Array(elem_ty, _) => contains_self(elem_ty),
        TyKind::Tuple(tys) => tys.iter().any(contains_self),
        TyKind::Function(params, ret) => {
            params.iter().any(contains_self) || contains_self(ret)
        }
        TyKind::Map(key_ty, value_ty) => contains_self(key_ty) || contains_self(value_ty),
        TyKind::Set(elem_ty) => contains_self(elem_ty),
        TyKind::Var(_) | TyKind::Primitive(_) | TyKind::Error | TyKind::Never => false,
    }
}

/// Helper function to instantiate a polymorphic type with fresh type variables.
/// Returns the instantiated type and a map from original type IDs to fresh type IDs.
pub(crate) fn instantiate(
    checker: &mut TypeChecker,
    ty: &Ty,
    span: SourceSpan,
    generic_params: &[GenericParamDef],
) -> TypeResult<(Ty, HashMap<TypeId, TypeId>)> {
    let mut subst = Substitution::default();
    let mut generic_to_fresh_map = HashMap::new();

    // Create fresh type variables for each generic parameter
    for param in generic_params {
        let fresh_var = checker.fresh_infer_var(span);
        if let TyKind::Var(id) = fresh_var.kind {
            // Store the mapping from original type ID to fresh type ID
            generic_to_fresh_map.insert(param.id, id);
            subst.insert(param.id, fresh_var);
        } else {
            return Err(TypeError::InternalError {
                message: format!("Expected fresh var to be TyKind::Var, but got {:?}", fresh_var.kind),
                span: Some(span),
            });
        }
    }

    // Apply the substitution to create the instantiated type
    let instantiated_ty = ty.apply_subst(&subst);
    Ok((instantiated_ty, generic_to_fresh_map))
}

// Add a helper function (or define it elsewhere) to convert back if needed
// This might not be the best place, but for a quick fix:
pub(crate) fn convert_symbol_back(s: Symbol) -> parallax_resolve::types::Symbol {
    parallax_resolve::types::Symbol(s.id())
} 