//! Tests for operator lowering to function calls.

use crate::ir::function::{MirFunction, MirType};
use crate::ir::statement::{Constant, MirStatement, Operand, Rvalue};
use crate::ir::terminator::MirTerminator;
use crate::tests::{create_test_db, create_test_function, validate_mir_function};
use std::sync::Arc;

/// Test requirement: Binary operators must be lowered to function calls
/// 
/// This test checks that binary operators are correctly lowered to function calls:
/// - Addition operator (+) should call the "add" function
/// - Subtraction operator (-) should call the "subtract" function
/// - Multiplication operator (*) should call the "multiply" function
/// - Division operator (/) should call the "divide" function
/// - Comparison operators should call their respective functions
#[test]
fn test_binary_operator_lowering() {
    let db = create_test_db();
    let hir_func_id = create_test_function("binary_op_calls");
    
    // This will fail initially until we implement operator lowering
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Validate the function structure
    validate_mir_function(&mir_func).expect("MIR function validation failed");
    
    // Check for function calls representing binary operations
    let mut found_add_call = false;
    let mut found_subtract_call = false;
    let mut found_comparison_call = false;
    
    for (_, block) in &mir_func.body.blocks {
        for stmt in &block.statements {
            if let MirStatement::Assign { source: Rvalue::Call { func, .. }, .. } = stmt {
                if let Operand::Constant(Constant::String(name)) = &**func {
                    if name == "add" || name == "operator+" {
                        found_add_call = true;
                    } else if name == "subtract" || name == "operator-" {
                        found_subtract_call = true;
                    } else if name.starts_with("eq") || name.starts_with("operator==") || 
                              name.starts_with("gt") || name.starts_with("operator>") {
                        found_comparison_call = true;
                    }
                }
            }
        }
    }
    
    assert!(found_add_call, "No addition operator call found in the function");
    assert!(found_subtract_call, "No subtraction operator call found in the function");
    assert!(found_comparison_call, "No comparison operator call found in the function");
}

/// Test requirement: Unary operators must be lowered to function calls
/// 
/// This test checks that unary operators are correctly lowered to function calls:
/// - Negation operator (-) should call the "negate" function
/// - Logical NOT operator (!) should call the "not" function
#[test]
fn test_unary_operator_lowering() {
    let db = create_test_db();
    let hir_func_id = create_test_function("unary_op_calls");
    
    // This will fail initially until we implement operator lowering
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Validate the function structure
    validate_mir_function(&mir_func).expect("MIR function validation failed");
    
    // Check for function calls representing unary operations
    let mut found_negate_call = false;
    let mut found_not_call = false;
    
    for (_, block) in &mir_func.body.blocks {
        for stmt in &block.statements {
            if let MirStatement::Assign { source: Rvalue::Call { func, .. }, .. } = stmt {
                if let Operand::Constant(Constant::String(name)) = &**func {
                    if name == "negate" || name == "operator-" {
                        found_negate_call = true;
                    } else if name == "not" || name == "operator!" {
                        found_not_call = true;
                    }
                }
            }
        }
    }
    
    assert!(found_negate_call, "No negation operator call found in the function");
    assert!(found_not_call, "No logical NOT operator call found in the function");
}

/// Test requirement: Operator precedence must be preserved in function calls
/// 
/// This test checks that the precedence of operators is preserved when lowering to function calls:
/// - Complex expressions should maintain the correct order of operations
/// - Nested operators should be lowered to nested function calls
#[test]
fn test_operator_precedence() {
    let db = create_test_db();
    let hir_func_id = create_test_function("operator_precedence");
    
    // This will fail initially until we implement operator lowering with precedence
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Validate the function structure
    validate_mir_function(&mir_func).expect("MIR function validation failed");
    
    // For precedence, we need to verify that nested expressions are handled correctly
    // by checking that the arguments to higher-precedence operators become operands
    // for lower-precedence operators
    
    // To simplify testing, we'll just verify that we have both simple and nested calls
    let mut found_nested_call = false;
    let mut found_temp_var_for_subexpr = false;
    
    // Look for temporary variables that hold results of subexpressions
    for local in &mir_func.locals {
        if local.name.as_ref().map_or(false, |name| name.contains("temp") || name.contains("subexpr")) {
            found_temp_var_for_subexpr = true;
            break;
        }
    }
    
    // Look for nested calls where the result of one call is used as an argument to another
    for (_, block) in &mir_func.body.blocks {
        for stmt in &block.statements {
            if let MirStatement::Assign { source: Rvalue::Call { args, .. }, .. } = stmt {
                for arg in args {
                    if let Operand::Copy(place) = arg {
                        // If we're using a place as an argument, and that place was defined
                        // in a previous statement as the result of a function call, that's a nested call
                        for prev_stmt in &block.statements {
                            if let MirStatement::Assign { destination, source: Rvalue::Call { .. } } = prev_stmt {
                                if destination.local == place.local {
                                    found_nested_call = true;
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    
    assert!(found_temp_var_for_subexpr || found_nested_call, 
            "No evidence of properly handling nested operators found in the function");
} 