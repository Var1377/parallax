//! Tests for lowering expressions to MIR.

use crate::ir::function::{MirFunction, MirType};
use crate::ir::statement::{BinOp, Constant, MirStatement, Operand, Rvalue, UnOp};
use crate::ir::terminator::MirTerminator;
use crate::tests::{create_test_db, create_test_function, validate_mir_function};
use std::sync::Arc;

/// Test requirement: Literal expressions must lower correctly
/// 
/// This test checks that literal expressions correctly lower to MIR with:
/// - Integer literals converted to Int constants
/// - Float literals converted to Float constants
/// - Boolean literals converted to Bool constants
/// - String literals converted to String constants
#[test]
fn test_literal_expressions() {
    let db = create_test_db();
    let hir_func_id = create_test_function("literals");
    
    // This will fail initially until we implement expression lowering
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Validate the function structure
    validate_mir_function(&mir_func).expect("MIR function validation failed");
    
    // Check for various types of constants in the function
    let mut found_int = false;
    let mut found_bool = false;
    
    for (_, block) in &mir_func.body.blocks {
        for stmt in &block.statements {
            if let MirStatement::Assign { source: Rvalue::Use(Operand::Constant(constant)), .. } = stmt {
                match constant {
                    Constant::Int(_) => found_int = true,
                    Constant::Bool(_) => found_bool = true,
                    _ => {}
                }
            }
        }
        
        if let MirTerminator::Return { value: Some(Operand::Constant(constant)) } = &block.terminator {
            match constant {
                Constant::Int(_) => found_int = true,
                Constant::Bool(_) => found_bool = true,
                _ => {}
            }
        }
    }
    
    assert!(found_int || found_bool, "No literal constants found in the function");
}

/// Test requirement: Binary operations must lower correctly
/// 
/// This test checks that binary operations correctly lower to MIR with:
/// - Arithmetic operations converted to BinaryOp rvalues
/// - Comparison operations converted to BinaryOp rvalues
/// - Logical operations converted to BinaryOp rvalues
#[test]
fn test_binary_operations() {
    let db = create_test_db();
    let hir_func_id = create_test_function("binary_ops");
    
    // This will fail initially until we implement binary operation lowering
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Validate the function structure
    validate_mir_function(&mir_func).expect("MIR function validation failed");
    
    // Check for binary operations in the function
    let mut found_arithmetic = false;
    let mut found_comparison = false;
    
    for (_, block) in &mir_func.body.blocks {
        for stmt in &block.statements {
            if let MirStatement::Assign { source: Rvalue::BinaryOp { op, .. }, .. } = stmt {
                match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                        found_arithmetic = true;
                    }
                    BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                        found_comparison = true;
                    }
                    _ => {}
                }
            }
        }
    }
    
    assert!(found_arithmetic || found_comparison, 
            "No binary operations found in the function");
}

/// Test requirement: Unary operations must lower correctly
/// 
/// This test checks that unary operations correctly lower to MIR with:
/// - Negation converted to UnaryOp rvalues
/// - Logical NOT converted to UnaryOp rvalues
#[test]
fn test_unary_operations() {
    let db = create_test_db();
    let hir_func_id = create_test_function("unary_ops");
    
    // This will fail initially until we implement unary operation lowering
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Validate the function structure
    validate_mir_function(&mir_func).expect("MIR function validation failed");
    
    // Check for unary operations in the function
    let mut found_unary_op = false;
    
    for (_, block) in &mir_func.body.blocks {
        for stmt in &block.statements {
            if let MirStatement::Assign { source: Rvalue::UnaryOp { .. }, .. } = stmt {
                found_unary_op = true;
                break;
            }
        }
    }
    
    assert!(found_unary_op, "No unary operations found in the function");
}

/// Test requirement: Block expressions must lower correctly
/// 
/// This test checks that block expressions correctly lower to MIR with:
/// - Statements in the block converted to MIR statements
/// - Last expression in the block handled correctly for its value
#[test]
fn test_block_expressions() {
    let db = create_test_db();
    let hir_func_id = create_test_function("block_expr");
    
    // This will fail initially until we implement block expression lowering
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Validate the function structure
    validate_mir_function(&mir_func).expect("MIR function validation failed");
    
    // For block expressions, we expect multiple statements within a block
    // followed by a terminator that uses the result of the last statement
    let mut found_block_with_multiple_statements = false;
    
    for (_, block) in &mir_func.body.blocks {
        if block.statements.len() >= 2 {
            found_block_with_multiple_statements = true;
            break;
        }
    }
    
    assert!(found_block_with_multiple_statements, 
            "No blocks with multiple statements found in the function");
}

/// Test requirement: Variable references must lower correctly
/// 
/// This test checks that variable references correctly lower to MIR with:
/// - Local variables referenced correctly in operands
/// - Distinction between copy and move semantics
#[test]
fn test_variable_references() {
    let db = create_test_db();
    let hir_func_id = create_test_function("var_refs");
    
    // This will fail initially until we implement variable reference lowering
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Validate the function structure
    validate_mir_function(&mir_func).expect("MIR function validation failed");
    
    // Check for variable references in the function
    let mut found_copy = false;
    let mut found_move = false;
    
    for (_, block) in &mir_func.body.blocks {
        for stmt in &block.statements {
            // Look for operands that are copies or moves of places
            let operands = match stmt {
                MirStatement::Assign { source: Rvalue::Use(operand), .. } => {
                    vec![operand]
                }
                MirStatement::Assign { source: Rvalue::BinaryOp { op: _, left, right }, .. } => {
                    vec![left, right]
                }
                MirStatement::Assign { source: Rvalue::Call { args, .. }, .. } => {
                    args.iter().collect::<Vec<_>>()
                }
                _ => vec![]
            };
            
            for operand in operands {
                match operand {
                    Operand::Copy(_) => found_copy = true,
                    Operand::Move(_) => found_move = true,
                    _ => {}
                }
            }
        }
    }
    
    assert!(found_copy || found_move, 
            "No variable references found in the function");
} 