//! Tests for lowering HIR functions to MIR.

use crate::db::{MirDatabase, OptimizationLevel};
use crate::ir::function::{BlockId, FunctionId, LocalId, MirFunction, MirType};
use crate::ir::statement::{Constant, MirStatement, Operand, Rvalue};
use crate::ir::terminator::MirTerminator;
use crate::tests::{create_test_db, create_test_function, validate_mir_function};
use std::sync::Arc;

/// Test requirement: Empty function must lower to valid MIR
/// 
/// This test checks that an empty function correctly lowers to MIR with:
/// - A valid function ID and name
/// - At least one basic block
/// - A valid terminator in the entry block
/// - Function signature with correct parameter and return types
#[test]
fn test_empty_function_lowering() {
    let db = create_test_db();
    let hir_func_id = create_test_function("empty");
    
    // This will fail initially until we implement function lowering
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Validate the function structure
    validate_mir_function(&mir_func).expect("MIR function validation failed");
    
    // Check that it has the correct name
    assert_eq!(mir_func.name, "empty");
    
    // Check that it has the right signature (no params, unit return)
    assert!(mir_func.signature.params.is_empty());
    assert!(matches!(mir_func.signature.return_type, MirType::Unit));
    
    // Check that it has an entry block with a return terminator
    let entry_block = mir_func.body.blocks.get(&mir_func.body.entry_block)
        .expect("Entry block not found");
    
    // The empty function should return unit
    assert!(matches!(entry_block.terminator, 
        MirTerminator::Return { value: Some(Operand::Constant(Constant::Unit)) } |
        MirTerminator::Return { value: None }
    ));
}

/// Test requirement: Function with return value must lower correctly
/// 
/// This test checks that a function with a return statement correctly lowers to MIR with:
/// - A return terminator with the correct value
/// - Constants lowered to the correct MIR representation
#[test]
fn test_function_with_return_value() {
    let db = create_test_db();
    let hir_func_id = create_test_function("return_42");
    
    // This will fail initially until we implement function lowering
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Validate the function structure
    validate_mir_function(&mir_func).expect("MIR function validation failed");
    
    // Check that it has an entry block with a return terminator
    let entry_block = mir_func.body.blocks.get(&mir_func.body.entry_block)
        .expect("Entry block not found");
    
    // The function should return an integer constant 42
    match &entry_block.terminator {
        MirTerminator::Return { value: Some(Operand::Constant(Constant::Int(val))) } => {
            assert_eq!(*val, 42);
        }
        _ => panic!("Expected return terminator with int constant 42"),
    }
}

/// Test requirement: Function with parameters must lower correctly
/// 
/// This test checks that a function with parameters correctly lowers to MIR with:
/// - Correct parameter declarations in the function signature
/// - Local variables created for each parameter
#[test]
fn test_function_with_parameters() {
    let db = create_test_db();
    let hir_func_id = create_test_function("add");
    
    // This will fail initially until we implement function lowering
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Validate the function structure
    validate_mir_function(&mir_func).expect("MIR function validation failed");
    
    // Check that it has two integer parameters
    assert_eq!(mir_func.signature.params.len(), 2);
    assert!(matches!(mir_func.signature.params[0], MirType::Int));
    assert!(matches!(mir_func.signature.params[1], MirType::Int));
    
    // Check that it has two locals for the parameters
    assert!(mir_func.locals.len() >= 2);
    
    // Check for a return terminator
    let entry_block = mir_func.body.blocks.get(&mir_func.body.entry_block)
        .expect("Entry block not found");
    
    // The function should have some terminator (we'll check specifics in a more detailed test)
    assert!(!matches!(entry_block.terminator, MirTerminator::Unreachable));
}

/// Test requirement: Basic arithmetic expressions must lower correctly
///
/// This test checks that a function with arithmetic expressions correctly lowers to MIR with:
/// - Binary operations lowered to MIR statements
/// - Correct operand usage
/// - Results assigned to temporaries
#[test]
fn test_basic_arithmetic() {
    let db = create_test_db();
    let hir_func_id = create_test_function("add");
    
    // This will fail initially until we implement expression lowering
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Validate the function structure
    validate_mir_function(&mir_func).expect("MIR function validation failed");
    
    // Look for an assignment statement with binary operation
    let mut found_binary_op = false;
    
    for (_, block) in &mir_func.body.blocks {
        for stmt in &block.statements {
            if let MirStatement::Assign { source: Rvalue::BinaryOp { .. }, .. } = stmt {
                found_binary_op = true;
                break;
            }
        }
    }
    
    assert!(found_binary_op, "No binary operation found in the function");
}

/// Test requirement: Function call lowering must work correctly
///
/// This test checks that function calls lower correctly to MIR with:
/// - Call terminators for function calls that can diverge
/// - Call rvalues for function calls that return values
#[test]
fn test_function_call() {
    let db = create_test_db();
    let hir_func_id = create_test_function("call_other");
    
    // This will fail initially until we implement function call lowering
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Validate the function structure
    validate_mir_function(&mir_func).expect("MIR function validation failed");
    
    // Look for a call statement or terminator
    let mut found_call = false;
    
    // Check statements for call
    for (_, block) in &mir_func.body.blocks {
        for stmt in &block.statements {
            if let MirStatement::CallVoid { .. } = stmt {
                found_call = true;
                break;
            }
            if let MirStatement::Assign { source: Rvalue::Call { .. }, .. } = stmt {
                found_call = true;
                break;
            }
        }
        
        // Check terminator for call
        if let MirTerminator::Call { .. } = &block.terminator {
            found_call = true;
            break;
        }
    }
    
    assert!(found_call, "No function call found in the function");
} 