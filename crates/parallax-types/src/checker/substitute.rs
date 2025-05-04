// src/checker/substitute.rs
//! Implements type substitution logic.

use crate::error::TypeError;
use crate::error::display_type;
use crate::types::{Ty, TyKind, FunctionSignature, GenericParamDef, ParamType};
use miette::SourceSpan;
use std::sync::Arc;

/// Recursively substitutes `TyKind::SelfType` with `concrete_self_ty` throughout `target_ty`.
/// Includes a recursion limit to prevent infinite loops in case of errors.
///
/// Preconditions: `target_ty` is the type to substitute within.
///                `concrete_self_ty` is the type to replace `SelfType` with.
///                `limit` is the maximum recursion depth.
/// Postconditions: Returns `Ok(Ty)` with `SelfType` replaced, or `Err(TypeError)` if recursion limit hit.
pub(crate) fn substitute_self(
    target_ty: &Ty,
    concrete_self_ty: &Ty,
    limit: u32,
) -> Result<Ty, TypeError> {
    if limit == 0 {
        return Err(TypeError::InferenceRecursionLimit { span: target_ty.span.unwrap_or_else(|| SourceSpan::from(0..0)) });
    }

    // Check the outer type kind first.
    if matches!(target_ty.kind, TyKind::SelfType) {
        // Base case: Found SelfType, replace it.
        return Ok(concrete_self_ty.clone());
    }

    // Recursively apply substitution to inner types.
    let new_kind = match &target_ty.kind {
        // SelfType handled above.
        TyKind::Var(_) | TyKind::Primitive(_) | TyKind::Error | TyKind::Never => return Ok(target_ty.clone()), // These don't contain SelfType internally.
        TyKind::InferInt(_) | TyKind::InferFloat(_) => return Ok(target_ty.clone()), // Inference literal types don't contain SelfType.

        TyKind::Named { name, symbol, args } => {
            let new_args = args.iter()
                .map(|arg| substitute_self(arg, concrete_self_ty, limit - 1))
                .collect::<Result<Vec<_>, _>>()?;
            TyKind::Named { name: name.clone(), symbol: *symbol, args: new_args }
        }
        TyKind::Array(elem_ty, size) => {
            let new_elem = substitute_self(elem_ty, concrete_self_ty, limit - 1)?;
            TyKind::Array(Arc::new(new_elem), *size)
        }
        TyKind::Tuple(tys) => {
            let new_tys = tys.iter()
                .map(|ty| substitute_self(ty, concrete_self_ty, limit - 1))
                .collect::<Result<Vec<_>, _>>()?;
            TyKind::Tuple(new_tys)
        }
        TyKind::Function(params, ret) => {
            let new_params = params.iter()
                .map(|param| substitute_self(param, concrete_self_ty, limit - 1))
                .collect::<Result<Vec<_>, _>>()?;
            let new_ret = substitute_self(ret, concrete_self_ty, limit - 1)?;
            TyKind::Function(new_params, Arc::new(new_ret))
        }
        TyKind::Map(key_ty, value_ty) => {
            let new_key = substitute_self(key_ty, concrete_self_ty, limit - 1)?;
            let new_value = substitute_self(value_ty, concrete_self_ty, limit - 1)?;
            TyKind::Map(Arc::new(new_key), Arc::new(new_value))
        }
        TyKind::Set(elem_ty) => {
            let new_elem = substitute_self(elem_ty, concrete_self_ty, limit - 1)?;
            TyKind::Set(Arc::new(new_elem))
        }
        TyKind::Pointer(inner, ptr_type) => {
            let new_inner = substitute_self(inner, concrete_self_ty, limit - 1)?;
            TyKind::Pointer(Arc::new(new_inner), *ptr_type)
        }
        TyKind::GenericParam(name) => {
            // No substitution for generic params
            return Ok(target_ty.clone());
        }
        TyKind::SelfType => unreachable!(), // Handled at the start
    };

    Ok(Ty { kind: new_kind, span: target_ty.span })
}

/// Substitutes `SelfType` within a `FunctionSignature`.
pub(crate) fn substitute_signature_self(
    sig: &FunctionSignature,
    concrete_self_ty: &Ty,
    limit: u32,
) -> Result<FunctionSignature, TypeError> {
    // Substitute Self in parameter types
    let new_params = sig.params.iter()
        .map(|p| -> Result<ParamType, TypeError> {
            Ok(ParamType {
                ty: substitute_self(&p.ty, concrete_self_ty, limit)?,
                ..p.clone()
            })
        })
        .collect::<Result<Vec<_>, _>>()?;

    // Substitute Self in return type
    let new_return_type = substitute_self(&sig.return_type, concrete_self_ty, limit)?;

    // Substitute Self in generic parameter bounds
    let new_generic_params = sig.generic_params.iter()
        .map(|gp| -> Result<GenericParamDef, TypeError> {
            Ok(GenericParamDef {
                 bounds: gp.bounds.iter()
                     .map(|tr| -> Result<crate::types::TraitRef, TypeError> {
                          Ok(crate::types::TraitRef {
                             type_arguments: tr.type_arguments.iter()
                                 .map(|arg| substitute_self(arg, concrete_self_ty, limit))
                                 .collect::<Result<Vec<_>, _>>()?,
                             ..tr.clone()
                         })
                     })
                     .collect::<Result<Vec<_>, _>>()?,
                 ..gp.clone()
             })
         })
         .collect::<Result<Vec<_>, _>>()?;

    Ok(FunctionSignature {
        params: new_params,
        return_type: new_return_type,
        generic_params: new_generic_params,
        ..sig.clone()
    })
}

/// Substitutes `SelfType` within the type arguments of a `TraitRef`.
pub(crate) fn substitute_self_in_trait_ref(
    trait_ref: &crate::types::TraitRef,
    concrete_self_ty: &Ty,
    limit: u32,
) -> Result<crate::types::TraitRef, TypeError> {
    let new_type_arguments = trait_ref.type_arguments.iter()
        .map(|arg| substitute_self(arg, concrete_self_ty, limit))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(crate::types::TraitRef {
        type_arguments: new_type_arguments,
        ..trait_ref.clone()
    })
}

/// Performs substitution on TraitRef arguments based on a general Substitution map.
/// Needed because TraitRef contains types.
pub(crate) fn substitute_trait_ref(trait_ref: &crate::types::TraitRef, subst: &crate::context::inference::Substitution) -> crate::types::TraitRef {
    crate::types::TraitRef {
        trait_id: trait_ref.trait_id,
        type_arguments: trait_ref.type_arguments.iter()
            .map(|ty| ty.apply_subst(subst)) // apply_subst handles recursion
            .collect(),
        span: trait_ref.span, // Keep original span
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{PrimitiveType, TraitRef};
    use crate::context::TraitId;
    use crate::context::inference::TypeId;
    use std::sync::Arc;

    // Helper to create a simple named type without args
    fn named_ty(name: &str) -> Ty {
        Ty::new(TyKind::Named {
            name: name.to_string(),
            symbol: None, // Symbol not needed for these tests
            args: vec![],
        })
    }

    // Helper to create SelfType
    fn self_ty() -> Ty {
        Ty::new(TyKind::SelfType)
    }

    #[test]
    fn substitute_self_basic() {
        let concrete_self = named_ty("MyStruct");
        let target = self_ty();
        let result = substitute_self(&target, &concrete_self, 10).unwrap();
        assert_eq!(result.kind, concrete_self.kind);
    }

    #[test]
    fn substitute_self_nested() {
        let concrete_self = named_ty("MyStruct");
        // Test with Tuple(Self, i32)
        let target = Ty::new(TyKind::Tuple(vec![
            self_ty(),
            Ty::new(TyKind::Primitive(PrimitiveType::I32)),
        ]));
        let expected = Ty::new(TyKind::Tuple(vec![
            concrete_self.clone(),
            Ty::new(TyKind::Primitive(PrimitiveType::I32)),
        ]));
        let result = substitute_self(&target, &concrete_self, 10).unwrap();
        assert_eq!(result.kind, expected.kind);

        // Test with Array<Self, 5>
        let target_array = Ty::new(TyKind::Array(Arc::new(self_ty()), Some(5)));
        let expected_array = Ty::new(TyKind::Array(Arc::new(concrete_self.clone()), Some(5)));
        let result_array = substitute_self(&target_array, &concrete_self, 10).unwrap();
        assert_eq!(result_array.kind, expected_array.kind);

        // Test with Fn(Self) -> Self
        let target_fn = Ty::new(TyKind::Function(vec![self_ty()], Arc::new(self_ty())));
        let expected_fn = Ty::new(TyKind::Function(vec![concrete_self.clone()], Arc::new(concrete_self.clone())));
        let result_fn = substitute_self(&target_fn, &concrete_self, 10).unwrap();
        assert_eq!(result_fn.kind, expected_fn.kind);
    }

    #[test]
    fn substitute_self_no_self() {
        let concrete_self = named_ty("MyStruct");
        let target = Ty::new(TyKind::Primitive(PrimitiveType::Bool));
        let result = substitute_self(&target, &concrete_self, 10).unwrap();
        // Should be unchanged
        assert_eq!(result.kind, target.kind);
    }

    #[test]
    fn substitute_self_limit() {
        let concrete_self = named_ty("MyStruct");
        let target = self_ty();
        let result = substitute_self(&target, &concrete_self, 0);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), TypeError::InferenceRecursionLimit { .. }));
    }

    #[test]
    fn substitute_signature_self_simple() {
        let concrete_self = named_ty("MyStruct");
        let sig = FunctionSignature {
            name: "test".to_string(),
            self_param: None,
            generic_params: vec![],
            params: vec![ParamType { name: "x".to_string(), ty: self_ty(), span: SourceSpan::from(0..0) }],
            return_type: self_ty(),
            span: SourceSpan::from(0..0),
        };
        let result = substitute_signature_self(&sig, &concrete_self, 10).unwrap();
        assert_eq!(result.params[0].ty.kind, concrete_self.kind);
        assert_eq!(result.return_type.kind, concrete_self.kind);
    }

    #[test]
    fn substitute_trait_ref_simple() {
        let concrete_self = named_ty("MyStruct");
        let trait_ref = TraitRef {
            trait_id: TraitId(1),
            type_arguments: vec![self_ty(), Ty::new(TyKind::Primitive(PrimitiveType::I32))],
            span: SourceSpan::from(0..0),
        };
        let expected_args = vec![concrete_self.clone(), Ty::new(TyKind::Primitive(PrimitiveType::I32))];
        let result = substitute_self_in_trait_ref(&trait_ref, &concrete_self, 10).unwrap();
        assert_eq!(result.type_arguments, expected_args);
        assert_eq!(result.trait_id, trait_ref.trait_id); // ID should be unchanged
    }
} 