// Contains substitution logic, e.g., for `Self` type.

use std::sync::Arc;

use crate::types::{Ty, TyKind, FunctionSignature, ParamType};

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
        TyKind::Named { name, symbol, args } => {
            let new_args = args
                .iter()
                .map(|arg| substitute_self(arg, implementing_type, recursion_limit - 1))
                .collect();
            TyKind::Named {
                name: name.clone(),
                symbol: *symbol,
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

/// Substitutes `TyKind::SelfType` within a function signature's parameters and return type.
pub fn substitute_signature_self(
    sig: &FunctionSignature,
    implementing_type: &Ty,
    recursion_limit: u32,
) -> FunctionSignature {
    // Substitute in parameters
    let new_params = sig
        .params
        .iter()
        .map(|p| ParamType {
            ty: substitute_self(&p.ty, implementing_type, recursion_limit),
            ..p.clone() // Clone other fields like name, span
        })
        .collect();

    // Substitute in return type
    let new_return_type = substitute_self(&sig.return_type, implementing_type, recursion_limit);

    // Return a new signature with substituted types
    FunctionSignature {
        params: new_params,
        return_type: new_return_type,
        ..sig.clone() // Clone other fields like name, generics, self_param, span
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        context::inference::InferenceContext,
        types::{Ty, TyKind, PrimitiveType, FunctionSignature, ParamType, GenericParamDef, TypeId},
    };
    use miette::SourceSpan;
    use std::sync::Arc;

    fn dummy_span() -> SourceSpan {
        SourceSpan::from((0, 0))
    }

    fn ty_prim(prim: PrimitiveType) -> Ty {
        Ty::with_span(TyKind::Primitive(prim), dummy_span())
    }

    fn ty_self() -> Ty {
        Ty::with_span(TyKind::SelfType, dummy_span())
    }

    fn ty_var(id: u32) -> Ty {
        Ty::with_span(TyKind::Var(TypeId(id)), dummy_span())
    }

    fn ty_named(name: &str, args: Vec<Ty>) -> Ty {
        Ty::with_span(TyKind::Named { name: name.to_string(), symbol: None, args }, dummy_span())
    }

    fn ty_tuple(tys: Vec<Ty>) -> Ty {
        Ty::with_span(TyKind::Tuple(tys), dummy_span())
    }

    fn ty_func(params: Vec<Ty>, ret: Ty) -> Ty {
        Ty::with_span(TyKind::Function(params, Arc::new(ret)), dummy_span())
    }

    #[test]
    fn test_contains_self_basic() {
        assert!(contains_self(&ty_self()));
        assert!(!contains_self(&ty_prim(PrimitiveType::I32)));
        assert!(!contains_self(&ty_var(0)));
    }

    #[test]
    fn test_contains_self_nested() {
        assert!(contains_self(&ty_named("Vec", vec![ty_self()])));
        assert!(contains_self(&ty_tuple(vec![ty_prim(PrimitiveType::Bool), ty_self()])));
        assert!(contains_self(&ty_func(vec![ty_self()], ty_prim(PrimitiveType::I32))));
        assert!(contains_self(&ty_func(vec![ty_prim(PrimitiveType::I32)], ty_self())));
        assert!(!contains_self(&ty_named("Option", vec![ty_prim(PrimitiveType::F64)])));
        assert!(!contains_self(&ty_tuple(vec![ty_var(0), ty_prim(PrimitiveType::Char)])));
    }

    #[test]
    fn test_substitute_self_simple() {
        let i32_ty = ty_prim(PrimitiveType::I32);
        let self_ty = ty_self();
        let var_ty = ty_var(0);

        assert_eq!(substitute_self(&self_ty, &i32_ty, 10), i32_ty);
        assert_eq!(substitute_self(&i32_ty, &ty_prim(PrimitiveType::Bool), 10), i32_ty); // No change
        assert_eq!(substitute_self(&var_ty, &i32_ty, 10), var_ty); // No change
    }

    #[test]
    fn test_substitute_self_nested() {
        let i32_ty = ty_prim(PrimitiveType::I32);
        let bool_ty = ty_prim(PrimitiveType::Bool);
        let self_ty = ty_self();

        let vec_self = ty_named("Vec", vec![self_ty.clone()]);
        let expected_vec_i32 = ty_named("Vec", vec![i32_ty.clone()]);
        assert_eq!(substitute_self(&vec_self, &i32_ty, 10), expected_vec_i32);

        let tuple_self_bool = ty_tuple(vec![self_ty.clone(), bool_ty.clone()]);
        let expected_tuple_i32_bool = ty_tuple(vec![i32_ty.clone(), bool_ty.clone()]);
        assert_eq!(substitute_self(&tuple_self_bool, &i32_ty, 10), expected_tuple_i32_bool);

        let func_self_to_self = ty_func(vec![self_ty.clone()], self_ty.clone());
        let expected_func_i32_to_i32 = ty_func(vec![i32_ty.clone()], i32_ty.clone());
        assert_eq!(substitute_self(&func_self_to_self, &i32_ty, 10), expected_func_i32_to_i32);
    }

    #[test]
    fn test_substitute_self_recursion_limit() {
        let self_ty = ty_self();
        let recursive_ty = ty_named("Node", vec![self_ty.clone()]); // Node<Self>
        let concrete_ty = ty_named("MyStruct", vec![]);

        // Should succeed within limit
        assert_ne!(substitute_self(&recursive_ty, &concrete_ty, 5).kind, TyKind::Error);

        // Create a deeply nested type involving Self
        let deep_self = ty_named("Box", vec![ty_named("List", vec![self_ty])]);
        assert_ne!(substitute_self(&deep_self, &concrete_ty, 5).kind, TyKind::Error);

        // Test hitting the limit - substitute Box<List<Self>> with recursion limit 1
        // substitute_self(Box<List<Self>>, _, 1) -> Box<substitute_self(List<Self>, _, 0)>
        // substitute_self(List<Self>, _, 0) -> Error
        // substitute_self(Box<Error>, _, 1) -> Box<Error> (approx)
        let result = substitute_self(&deep_self, &concrete_ty, 1);
        // Check that *some* part of the result is Error due to limit
        match result.kind {
             TyKind::Named { ref args, .. } => match args[0].kind {
                 TyKind::Named { ref args, .. } => assert!(matches!(args[0].kind, TyKind::Error)),
                 _ => panic!("Expected inner Named type"),
             },
             _ => panic!("Expected outer Named type"),
         }
    }


    #[test]
    fn test_substitute_signature_self_simple() {
        let i32_ty = ty_prim(PrimitiveType::I32);
        let bool_ty = ty_prim(PrimitiveType::Bool);
        let self_ty = ty_self();

        let sig = FunctionSignature {
            name: "method".to_string(),
            self_param: Some(crate::types::SelfParamKind::Value),
            generic_params: vec![],
            params: vec![
                ParamType { name: "x".to_string(), ty: self_ty.clone(), span: dummy_span() },
                ParamType { name: "y".to_string(), ty: bool_ty.clone(), span: dummy_span() },
            ],
            return_type: self_ty.clone(),
            span: dummy_span(),
        };

        let expected_sig = FunctionSignature {
            name: "method".to_string(),
            self_param: Some(crate::types::SelfParamKind::Value),
            generic_params: vec![],
            params: vec![
                ParamType { name: "x".to_string(), ty: i32_ty.clone(), span: dummy_span() },
                ParamType { name: "y".to_string(), ty: bool_ty.clone(), span: dummy_span() },
            ],
            return_type: i32_ty.clone(),
            span: dummy_span(),
        };

        let substituted_sig = substitute_signature_self(&sig, &i32_ty, 10);

        assert_eq!(substituted_sig.params.len(), expected_sig.params.len());
        assert_eq!(substituted_sig.params[0].ty, expected_sig.params[0].ty);
        assert_eq!(substituted_sig.params[1].ty, expected_sig.params[1].ty);
        assert_eq!(substituted_sig.return_type, expected_sig.return_type);
        assert_eq!(substituted_sig.name, expected_sig.name);
         assert_eq!(substituted_sig.self_param, expected_sig.self_param);
    }

    #[test]
    fn test_substitute_signature_self_no_self() {
         let i32_ty = ty_prim(PrimitiveType::I32);
         let bool_ty = ty_prim(PrimitiveType::Bool);

         let sig = FunctionSignature {
             name: "free_func".to_string(),
             self_param: None,
             generic_params: vec![],
             params: vec![
                 ParamType { name: "a".to_string(), ty: i32_ty.clone(), span: dummy_span() },
             ],
             return_type: bool_ty.clone(),
             span: dummy_span(),
         };

         // Substitute with String type, should have no effect
         let string_ty = ty_prim(PrimitiveType::String);
         let substituted_sig = substitute_signature_self(&sig, &string_ty, 10);

         // Use original sig for comparison as no change is expected
         assert_eq!(substituted_sig.params.len(), sig.params.len());
         assert_eq!(substituted_sig.params[0].ty, sig.params[0].ty);
         assert_eq!(substituted_sig.return_type, sig.return_type);
         assert_eq!(substituted_sig.name, sig.name);
         assert_eq!(substituted_sig.self_param, sig.self_param);
    }
} 