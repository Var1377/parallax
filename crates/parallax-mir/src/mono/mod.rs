//! Monomorphization of MIR functions.
//!
//! This module handles monomorphizing generic MIR functions with concrete types.

use std::sync::Arc;
use std::collections::HashSet;

use crate::db::MirDatabase;
use crate::error::MirError;
use crate::ir::function::{MirFunction, MirType, MirFunctionSignature, MirLocalDecl};
use crate::ir::statement::{Rvalue, Operand, Constant, MirStatement};
use crate::ir::place::Place;
use crate::ir::terminator::MirTerminator;

/// Monomorphize a function with concrete type arguments - Database-dependent entry point
pub fn monomorphize_function(
    db: &dyn MirDatabase,
    mir_function: Arc<MirFunction>,
    type_args: Vec<Arc<MirType>>,
) -> Result<Arc<MirFunction>, MirError> {
    // This function is the entry point called from the database
    // It delegates to the core logic, which can be tested separately
    
    // For now, just delegate to the core implementation
    monomorphize_function_impl(mir_function, type_args)
}

/// Core monomorphization logic that doesn't depend on database access
/// This is the function that should be fully tested
pub fn monomorphize_function_impl(
    mir_function: Arc<MirFunction>,
    type_args: Vec<Arc<MirType>>,
) -> Result<Arc<MirFunction>, MirError> {
    // Check if function actually needs monomorphization
    if !requires_monomorphization(&mir_function) {
        return Ok(mir_function);
    }
    
    // Extract the generic parameters from the function
    let generic_params = extract_generic_parameters(&mir_function);
    
    // Ensure we have the right number of type arguments
    if generic_params.len() != type_args.len() {
        return Err(MirError::monomorphization_error(
            &format!("Expected {} type arguments, got {}", 
                     generic_params.len(), type_args.len())
        ));
    }
    
    // Create substitution map
    let substitution: Vec<(String, MirType)> = generic_params.iter()
        .zip(type_args.iter())
        .map(|(param, arg)| (param.clone(), (**arg).clone()))
        .collect();
    
    // Verify trait bounds on the type arguments (if we had trait bound info in the MIR)
    // This would be the place to verify that the concrete types satisfy the bounds
    // For now, we'll assume that the typechecker has already verified this
    // But in a more complete implementation, we would:
    // 1. Extract trait bounds from the function's generic parameters
    // 2. For each bound, check if the corresponding concrete type satisfies it
    // 3. If not, return an error
    
    // Create a deep clone of the function that we'll modify
    let mut mono_function = (*mir_function).clone();
    
    // Apply substitution to function signature
    mono_function.signature = monomorphize_signature(&mono_function.signature, &substitution);
    
    // Apply substitution to local variables
    mono_function.locals = mono_function.locals.iter()
        .map(|local| monomorphize_local(local, &substitution))
        .collect();
    
    // Apply substitution to function body
    for (_, block) in &mut mono_function.body.blocks {
        // Apply to statements
        for stmt in &mut block.statements {
            monomorphize_statement(stmt, &substitution);
        }
        
        // Apply to terminator
        monomorphize_terminator(&mut block.terminator, &substitution);
    }
    
    // Update function name to include monomorphized types
    mono_function.name = format!("{}<{}>", 
        mono_function.name,
        type_args.iter()
            .map(|ty| format!("{:?}", ty))
            .collect::<Vec<_>>()
            .join(", ")
    );
    
    Ok(Arc::new(mono_function))
}

/// Check if a function requires monomorphization
pub fn requires_monomorphization(function: &MirFunction) -> bool {
    // A function needs monomorphization if it has any generic types in:
    // 1. Function signature (params or return type)
    // 2. Local variables
    
    // Check signature
    if contains_generic_type(&function.signature.return_type) {
        return true;
    }
    
    for param_ty in &function.signature.params {
        if contains_generic_type(param_ty) {
            return true;
        }
    }
    
    // Check locals
    for local in &function.locals {
        if contains_generic_type(&local.ty) {
            return true;
        }
    }
    
    false
}

/// Extract generic type parameters from a function
pub fn extract_generic_parameters(function: &MirFunction) -> Vec<String> {
    let mut params = HashSet::new();
    
    // Collect from signature
    collect_generic_params_from_type(&function.signature.return_type, &mut params);
    
    for param_ty in &function.signature.params {
        collect_generic_params_from_type(param_ty, &mut params);
    }
    
    // Collect from locals
    for local in &function.locals {
        collect_generic_params_from_type(&local.ty, &mut params);
    }
    
    // Convert to vector and sort for consistent ordering
    let mut params_vec: Vec<String> = params.into_iter().collect();
    params_vec.sort();
    params_vec
}

/// Apply a substitution to a type
pub fn substitute_type(ty: &MirType, substitution: &[(String, MirType)]) -> MirType {
    match ty {
        MirType::Generic(name) => {
            // Substitute generic parameter with concrete type
            for (param, concrete) in substitution {
                if param == name {
                    return concrete.clone();
                }
            }
            // If not found in substitution, keep as is
            ty.clone()
        },
        MirType::Function { params, ret } => {
            // Substitute in function parameters and return type
            let new_params = params.iter()
                .map(|param_ty| substitute_type(param_ty, substitution))
                .collect();
            let new_ret = Box::new(substitute_type(ret, substitution));
            MirType::Function {
                params: new_params,
                ret: new_ret,
            }
        },
        MirType::Tuple(elements) => {
            // Substitute in tuple elements
            let new_elements = elements.iter()
                .map(|elem_ty| substitute_type(elem_ty, substitution))
                .collect();
            MirType::Tuple(new_elements)
        },
        MirType::Array(elem_ty) => {
            // Substitute in array element type
            let new_elem_ty = Box::new(substitute_type(elem_ty, substitution));
            MirType::Array(new_elem_ty)
        },
        MirType::Named { name, args } => {
            // Substitute in named type arguments
            let new_args = args.iter()
                .map(|arg_ty| substitute_type(arg_ty, substitution))
                .collect();
            MirType::Named {
                name: name.clone(),
                args: new_args,
            }
        },
        // Basic types don't need substitution
        _ => ty.clone(),
    }
}

/// Test version of monomorphize_function that doesn't depend on MirDatabase
#[cfg(test)]
pub fn monomorphize_function_test(
    mir_function: Arc<MirFunction>,
    type_args: Vec<Arc<MirType>>,
) -> Result<Arc<MirFunction>, MirError> {
    // Call the actual core implementation function that will be used in production
    // This ensures we're testing the real logic, not just a test double
    monomorphize_function_impl(mir_function, type_args)
}

// Helper functions

/// Check if a type contains any generic type parameters
fn contains_generic_type(ty: &MirType) -> bool {
    match ty {
        MirType::Generic(_) => true,
        MirType::Function { params, ret } => {
            params.iter().any(|param_ty| contains_generic_type(param_ty)) ||
            contains_generic_type(ret)
        },
        MirType::Tuple(elements) => {
            elements.iter().any(|elem_ty| contains_generic_type(elem_ty))
        },
        MirType::Array(elem_ty) => {
            contains_generic_type(elem_ty)
        },
        MirType::Named { args, .. } => {
            args.iter().any(|arg_ty| contains_generic_type(arg_ty))
        },
        // Basic types don't contain generics
        _ => false,
    }
}

/// Collect generic type parameters from a type into a set
fn collect_generic_params_from_type(ty: &MirType, params: &mut HashSet<String>) {
    match ty {
        MirType::Generic(name) => {
            params.insert(name.clone());
        },
        MirType::Function { params: fn_params, ret } => {
            for param_ty in fn_params {
                collect_generic_params_from_type(param_ty, params);
            }
            collect_generic_params_from_type(ret, params);
        },
        MirType::Tuple(elements) => {
            for elem_ty in elements {
                collect_generic_params_from_type(elem_ty, params);
            }
        },
        MirType::Array(elem_ty) => {
            collect_generic_params_from_type(elem_ty, params);
        },
        MirType::Named { args, .. } => {
            for arg_ty in args {
                collect_generic_params_from_type(arg_ty, params);
            }
        },
        // Basic types don't contain generics
        _ => {},
    }
}

/// Apply type substitution to a function signature
fn monomorphize_signature(
    signature: &MirFunctionSignature,
    substitution: &[(String, MirType)],
) -> MirFunctionSignature {
    let new_params = signature.params.iter()
        .map(|param_ty| substitute_type(param_ty, substitution))
        .collect();
    
    let new_return_type = substitute_type(&signature.return_type, substitution);
    
    MirFunctionSignature {
        params: new_params,
        return_type: new_return_type,
    }
}

/// Apply type substitution to a local variable declaration
fn monomorphize_local(
    local: &MirLocalDecl,
    substitution: &[(String, MirType)],
) -> MirLocalDecl {
    MirLocalDecl {
        id: local.id,
        ty: substitute_type(&local.ty, substitution),
        name: local.name.clone(),
    }
}

/// Apply type substitution to a statement
fn monomorphize_statement(
    stmt: &mut MirStatement,
    substitution: &[(String, MirType)],
) {
    match stmt {
        MirStatement::Assign { source, .. } => {
            // Apply substitution to the right-hand side
            monomorphize_rvalue(source, substitution);
        },
        MirStatement::CallVoid { args, .. } => {
            // Apply substitution to args
            for arg in args {
                monomorphize_operand(arg, substitution);
            }
        },
        // Other statement types may need similar treatment
        _ => {},
    }
}

/// Apply type substitution to an rvalue
fn monomorphize_rvalue(
    rvalue: &mut Rvalue,
    substitution: &[(String, MirType)],
) {
    match rvalue {
        Rvalue::Use(operand) => {
            monomorphize_operand(operand, substitution);
        },
        Rvalue::Ref(_) => {
            // Place references don't need type substitution
            // The place refers to a local whose type we've already substituted
        },
        Rvalue::BinaryOp { left, right, .. } => {
            monomorphize_operand(left, substitution);
            monomorphize_operand(right, substitution);
        },
        Rvalue::UnaryOp { operand, .. } => {
            monomorphize_operand(operand, substitution);
        },
        Rvalue::Call { args, .. } => {
            for arg in args {
                monomorphize_operand(arg, substitution);
            }
        },
        Rvalue::Aggregate { elements, .. } => {
            for elem in elements {
                monomorphize_operand(elem, substitution);
            }
        },
    }
}

/// Apply type substitution to an operand
fn monomorphize_operand(
    _operand: &mut Operand,
    _substitution: &[(String, MirType)],
) {
    // Most operands don't need substitution since they're either:
    // - Constants (which don't have types that need substitution)
    // - References to places (places refer to locals whose types we've already substituted)
    // If we had complex type annotations on operands, we would handle them here
}

/// Apply type substitution to a terminator
fn monomorphize_terminator(
    terminator: &mut MirTerminator,
    substitution: &[(String, MirType)],
) {
    match terminator {
        MirTerminator::Return { value } => {
            if let Some(operand) = value {
                monomorphize_operand(operand, substitution);
            }
        },
        MirTerminator::Call { func, args, .. } => {
            monomorphize_operand(func, substitution);
            for arg in args {
                monomorphize_operand(arg, substitution);
            }
        },
        MirTerminator::Switch { discriminant, .. } => {
            monomorphize_operand(discriminant, substitution);
        },
        MirTerminator::Match { scrutinee, .. } => {
            monomorphize_operand(scrutinee, substitution);
        },
        // Other terminators if needed
        _ => {},
    }
} 