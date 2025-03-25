//! Tests for type conversions between HIR and MIR.

use crate::ir::function::MirType;
use crate::tests::{create_test_db, create_test_function};
use std::sync::Arc;

/// Test requirement: Basic HIR types must be correctly converted to MIR types
/// 
/// This test checks that basic HIR types correctly convert to MIR types:
/// - Integer types to MirType::Int
/// - Float types to MirType::Float
/// - Boolean types to MirType::Bool
/// - Character types to MirType::Char
/// - String types to MirType::String
/// - Unit type to MirType::Unit
#[test]
fn test_basic_type_conversion() {
    let db = create_test_db();
    let hir_func_id = create_test_function("basic_types");
    
    // This will fail initially until we implement type conversion
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Check that the function has local variables with various types
    let mut found_int = false;
    let mut found_bool = false;
    
    for local in &mir_func.locals {
        match local.ty {
            MirType::Int => found_int = true,
            MirType::Bool => found_bool = true,
            _ => {}
        }
    }
    
    assert!(found_int || found_bool, "No basic types found in the function");
}

/// Test requirement: Function types must be correctly converted to MIR function types
/// 
/// This test checks that function types correctly convert to MIR function types:
/// - Parameter types
/// - Return type
/// - Higher-order functions
#[test]
fn test_function_type_conversion() {
    let db = create_test_db();
    let hir_func_id = create_test_function("function_types");
    
    // This will fail initially until we implement function type conversion
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Check that the function has local variables with function types
    let mut found_function_type = false;
    
    for local in &mir_func.locals {
        if let MirType::Function { .. } = local.ty {
            found_function_type = true;
            break;
        }
    }
    
    assert!(found_function_type, "No function types found in the function");
}

/// Test requirement: Named types must be correctly converted to MIR named types
/// 
/// This test checks that named types correctly convert to MIR named types:
/// - Struct types
/// - Enum types
/// - Type aliases
#[test]
fn test_named_type_conversion() {
    let db = create_test_db();
    let hir_func_id = create_test_function("named_types");
    
    // This will fail initially until we implement named type conversion
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Check that the function has local variables with named types
    let mut found_named_type = false;
    
    for local in &mir_func.locals {
        if let MirType::Named { .. } = local.ty {
            found_named_type = true;
            break;
        }
    }
    
    assert!(found_named_type, "No named types found in the function");
}

/// Test requirement: Generic types must be correctly converted to MIR types
/// 
/// This test checks that generic types correctly convert to MIR types:
/// - Generic parameters
/// - Generic type instantiations
#[test]
fn test_generic_type_conversion() {
    let db = create_test_db();
    let hir_func_id = create_test_function("generic_types");
    
    // This will fail initially until we implement generic type conversion
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Check that the function has local variables with generic types
    let mut found_generic_type = false;
    
    for local in &mir_func.locals {
        match &local.ty {
            MirType::Generic(_) => {
                found_generic_type = true;
                break;
            }
            MirType::Named { args, .. } if !args.is_empty() => {
                // Named type with type arguments
                found_generic_type = true;
                break;
            }
            _ => {}
        }
    }
    
    assert!(found_generic_type, "No generic types found in the function");
}

/// Test requirement: Compound types must be correctly converted to MIR types
/// 
/// This test checks that compound types correctly convert to MIR types:
/// - Tuple types
/// - Array types
#[test]
fn test_compound_type_conversion() {
    let db = create_test_db();
    let hir_func_id = create_test_function("compound_types");
    
    // This will fail initially until we implement compound type conversion
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Check that the function has local variables with compound types
    let mut found_tuple = false;
    let mut found_array = false;
    
    for local in &mir_func.locals {
        match &local.ty {
            MirType::Tuple(_) => found_tuple = true,
            MirType::Array(_) => found_array = true,
            _ => {}
        }
    }
    
    assert!(found_tuple || found_array, "No compound types found in the function");
} 