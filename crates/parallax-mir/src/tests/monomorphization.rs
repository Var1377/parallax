//! Tests for monomorphization of generic MIR functions.

use crate::ir::function::{MirFunction, MirType};
use crate::tests::{create_test_db, create_test_function, validate_mir_function};
use std::sync::Arc;

/// Test requirement: Generic parameters must be detected
/// 
/// This test verifies that generic type parameters in a function can be identified.
#[test]
fn test_detect_generic_parameters() {
    let db = create_test_db();
    let hir_func_id = create_test_function("generic_function");
    
    // Get the MIR function
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Verify the function structure
    validate_mir_function(&mir_func).expect("MIR function validation failed");
    
    // Check if the function is correctly identified as containing generic parameters
    let requires_mono = crate::mono::requires_monomorphization(&mir_func);
    assert!(requires_mono, "Function with generic parameters not detected as requiring monomorphization");
    
    // Check if we can extract the generic parameters
    let generic_params = crate::mono::extract_generic_parameters(&mir_func);
    assert!(!generic_params.is_empty(), "No generic parameters found in generic function");
    
    // Verify specific parameter names
    let has_t_param = generic_params.iter().any(|param| param == "T");
    assert!(has_t_param, "Generic parameter 'T' not found");
}

/// Test requirement: Type substitution must work correctly
/// 
/// This test checks that type substitution in MIR types works correctly:
/// - Generic type parameters are replaced with concrete types
/// - Nested generics are handled properly
#[test]
fn test_type_substitution() {
    // Create some test types
    let generic_type = MirType::Generic("T".to_string());
    let concrete_type = MirType::Int;
    
    // Create a substitution map
    let substitution = vec![("T".to_string(), concrete_type.clone())];
    
    // Perform substitution on a simple generic type
    let result = crate::mono::substitute_type(&generic_type, &substitution);
    assert!(matches!(result, MirType::Int), "Basic type substitution failed");
    
    // Test substitution in a more complex type (generic function type)
    let func_type = MirType::Function {
        params: vec![generic_type.clone()],
        ret: Box::new(generic_type),
    };
    
    let result = crate::mono::substitute_type(&func_type, &substitution);
    
    if let MirType::Function { params, ret } = result {
        assert!(matches!(params[0], MirType::Int), "Parameter type substitution failed");
        assert!(matches!(*ret, MirType::Int), "Return type substitution failed");
    } else {
        panic!("Function type substitution produced wrong type");
    }
}

/// Test requirement: Function monomorphization must produce specialized functions
/// 
/// This test checks that monomorphization creates a specialized function:
/// - Generic type parameters in the function signature are replaced
/// - Generic types in local variables are replaced
/// - Function body is updated with concrete types
#[test]
fn test_function_monomorphization() {
    let db = create_test_db();
    let hir_func_id = create_test_function("generic_function");
    
    // Get the generic MIR function
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Create concrete type arguments
    let concrete_types = vec![Arc::new(MirType::Int)];
    
    // Monomorphize the function
    let mono_func = db.monomorphized_function(mir_func.clone(), concrete_types)
        .expect("Monomorphization failed");
    
    // Validate the monomorphized function
    validate_mir_function(&mono_func).expect("Monomorphized function validation failed");
    
    // Check that the monomorphized function doesn't contain generic types
    let contains_generic = mono_func.signature.params.iter().any(|ty| matches!(ty, MirType::Generic(_))) ||
                           matches!(mono_func.signature.return_type, MirType::Generic(_)) ||
                           mono_func.locals.iter().any(|local| matches!(local.ty, MirType::Generic(_)));
    
    assert!(!contains_generic, "Monomorphized function still contains generic types");
    
    // Verify that appropriate type substitutions were made
    let has_int_type = mono_func.signature.params.iter().any(|ty| matches!(ty, MirType::Int)) ||
                      matches!(mono_func.signature.return_type, MirType::Int) ||
                      mono_func.locals.iter().any(|local| matches!(local.ty, MirType::Int));
                      
    assert!(has_int_type, "Monomorphized function lacks expected concrete Int type");
}

/// Test requirement: Monomorphization must handle complex generic types
/// 
/// This test verifies that monomorphization works correctly with:
/// - Multiple generic parameters
/// - Nested generic types (e.g., Vec<Option<T>>)
/// - Generic function types
#[test]
fn test_complex_generics_monomorphization() {
    let db = create_test_db();
    let hir_func_id = create_test_function("complex_generic_function");
    
    // Get the generic MIR function
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Create concrete type arguments (e.g., T=Int, U=Bool)
    let concrete_types = vec![
        Arc::new(MirType::Int),
        Arc::new(MirType::Bool),
    ];
    
    // Monomorphize the function
    let mono_func = db.monomorphized_function(mir_func.clone(), concrete_types)
        .expect("Monomorphization failed");
    
    // Validate the monomorphized function
    validate_mir_function(&mono_func).expect("Monomorphized function validation failed");
    
    // Check for named type with concrete type arguments (e.g., Vec<Int>)
    let has_named_with_concrete_args = mono_func.locals.iter().any(|local| {
        if let MirType::Named { args, .. } = &local.ty {
            args.iter().any(|arg| matches!(arg, MirType::Int | MirType::Bool))
        } else {
            false
        }
    });
    
    assert!(has_named_with_concrete_args, "Monomorphized function doesn't handle nested generic types correctly");
} 