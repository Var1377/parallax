//! Tests for MIR optimizations.

use crate::ir::function::MirType;
use crate::ir::statement::{BinOp, Constant, MirStatement, Operand, Rvalue};
use crate::db::OptimizationLevel;
use crate::tests::{create_test_db, create_test_function, validate_mir_function};
use std::sync::Arc;

/// Test requirement: Constant folding must evaluate constant expressions at compile time
/// 
/// This test checks that:
/// - Binary operations with constant operands are evaluated at compile time
/// - The optimized MIR replaces the expression with a constant value
#[test]
fn test_constant_folding_binary_ops() {
    let db = create_test_db();
    let hir_func_id = create_test_function("constant_folding_binary");
    
    // Get the MIR function
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Optimize with full optimization
    let optimized = db.optimized_function(mir_func.clone(), OptimizationLevel::Full)
        .expect("Optimization failed");
    
    // Validate the optimized function
    validate_mir_function(&optimized).expect("Optimized function validation failed");
    
    // Check if constants were folded
    // In the original function, we should find binary operations on constants
    // In the optimized function, these should be replaced with single constants
    
    // Count the number of binary operations in the original vs optimized function
    let mut binary_ops_original = 0;
    let mut binary_ops_optimized = 0;
    
    // Count binary operations in original function
    for (_, block) in &mir_func.body.blocks {
        for stmt in &block.statements {
            if let MirStatement::Assign { source, .. } = stmt {
                if let Rvalue::BinaryOp { .. } = source {
                    binary_ops_original += 1;
                }
            }
        }
    }
    
    // Count binary operations in optimized function
    for (_, block) in &optimized.body.blocks {
        for stmt in &block.statements {
            if let MirStatement::Assign { source, .. } = stmt {
                if let Rvalue::BinaryOp { .. } = source {
                    binary_ops_optimized += 1;
                }
            }
        }
    }
    
    // We should have fewer binary operations in the optimized version
    assert!(binary_ops_optimized < binary_ops_original, 
        "Constant folding didn't reduce binary operations: original: {}, optimized: {}", 
        binary_ops_original, binary_ops_optimized);
}

/// Test requirement: Constant folding must handle unary operations
/// 
/// This test checks that unary operations on constants are evaluated at compile time
#[test]
fn test_constant_folding_unary_ops() {
    let db = create_test_db();
    let hir_func_id = create_test_function("constant_folding_unary");
    
    // Get the MIR function
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Optimize with full optimization
    let optimized = db.optimized_function(mir_func.clone(), OptimizationLevel::Full)
        .expect("Optimization failed");
    
    // Validate the optimized function
    validate_mir_function(&optimized).expect("Optimized function validation failed");
    
    // Count unary operations in original vs optimized
    let mut unary_ops_original = 0;
    let mut unary_ops_optimized = 0;
    
    // Count in original function
    for (_, block) in &mir_func.body.blocks {
        for stmt in &block.statements {
            if let MirStatement::Assign { source, .. } = stmt {
                if let Rvalue::UnaryOp { .. } = source {
                    unary_ops_original += 1;
                }
            }
        }
    }
    
    // Count in optimized function
    for (_, block) in &optimized.body.blocks {
        for stmt in &block.statements {
            if let MirStatement::Assign { source, .. } = stmt {
                if let Rvalue::UnaryOp { .. } = source {
                    unary_ops_optimized += 1;
                }
            }
        }
    }
    
    // We should have fewer unary operations in the optimized version
    assert!(unary_ops_optimized < unary_ops_original, 
        "Constant folding didn't reduce unary operations: original: {}, optimized: {}", 
        unary_ops_original, unary_ops_optimized);
}

/// Test requirement: Constant folding must handle complex expressions
/// 
/// This test checks that complex nested expressions with constants are properly folded
#[test]
fn test_constant_folding_complex_expressions() {
    let db = create_test_db();
    let hir_func_id = create_test_function("constant_folding_complex");
    
    // Get the MIR function
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Optimize with full optimization
    let optimized = db.optimized_function(mir_func.clone(), OptimizationLevel::Full)
        .expect("Optimization failed");
    
    // Validate the optimized function
    validate_mir_function(&optimized).expect("Optimized function validation failed");
    
    // Count total operations (both binary and unary) in original vs optimized
    let mut ops_original = 0;
    let mut ops_optimized = 0;
    
    // Count in original function
    for (_, block) in &mir_func.body.blocks {
        for stmt in &block.statements {
            if let MirStatement::Assign { source, .. } = stmt {
                match source {
                    Rvalue::BinaryOp { .. } | Rvalue::UnaryOp { .. } => {
                        ops_original += 1;
                    },
                    _ => {}
                }
            }
        }
    }
    
    // Count in optimized function
    for (_, block) in &optimized.body.blocks {
        for stmt in &block.statements {
            if let MirStatement::Assign { source, .. } = stmt {
                match source {
                    Rvalue::BinaryOp { .. } | Rvalue::UnaryOp { .. } => {
                        ops_optimized += 1;
                    },
                    _ => {}
                }
            }
        }
    }
    
    // We should have fewer operations in the optimized version
    assert!(ops_optimized < ops_original, 
        "Constant folding didn't reduce operations in complex expressions: original: {}, optimized: {}", 
        ops_original, ops_optimized);
}

/// Test requirement: Dead code elimination must remove unreachable blocks
/// 
/// This test checks that basic blocks that cannot be reached are removed from the function
#[test]
fn test_dce_unreachable_blocks() {
    let db = create_test_db();
    let hir_func_id = create_test_function("dce_unreachable_blocks");
    
    // Get the MIR function
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Count the number of blocks in the original function
    let original_block_count = mir_func.body.blocks.len();
    
    // Optimize with full optimization
    let optimized = db.optimized_function(mir_func.clone(), OptimizationLevel::Full)
        .expect("Optimization failed");
    
    // Validate the optimized function
    validate_mir_function(&optimized).expect("Optimized function validation failed");
    
    // Count the number of blocks in the optimized function
    let optimized_block_count = optimized.body.blocks.len();
    
    // We should have fewer blocks in the optimized version
    assert!(optimized_block_count < original_block_count, 
        "DCE didn't remove unreachable blocks: original: {}, optimized: {}", 
        original_block_count, optimized_block_count);
}

/// Test requirement: Dead code elimination must remove unused locals
/// 
/// This test checks that local variables that are never used are removed from the function
#[test]
fn test_dce_unused_locals() {
    let db = create_test_db();
    let hir_func_id = create_test_function("dce_unused_locals");
    
    // Get the MIR function
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Count the number of locals in the original function
    let original_locals_count = mir_func.locals.len();
    
    // Optimize with full optimization
    let optimized = db.optimized_function(mir_func.clone(), OptimizationLevel::Full)
        .expect("Optimization failed");
    
    // Validate the optimized function
    validate_mir_function(&optimized).expect("Optimized function validation failed");
    
    // Count the number of locals in the optimized function
    let optimized_locals_count = optimized.locals.len();
    
    // We should have fewer locals in the optimized version
    assert!(optimized_locals_count < original_locals_count, 
        "DCE didn't remove unused locals: original: {}, optimized: {}", 
        original_locals_count, optimized_locals_count);
}

/// Test requirement: Dead code elimination must remove unused assignments
/// 
/// This test checks that assignments to variables that are never read are removed
#[test]
fn test_dce_unused_assignments() {
    let db = create_test_db();
    let hir_func_id = create_test_function("dce_unused_assignments");
    
    // Get the MIR function
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Count the number of assignments in the original function
    let mut original_assignment_count = 0;
    for (_, block) in &mir_func.body.blocks {
        for stmt in &block.statements {
            if let MirStatement::Assign { .. } = stmt {
                original_assignment_count += 1;
            }
        }
    }
    
    // Optimize with full optimization
    let optimized = db.optimized_function(mir_func.clone(), OptimizationLevel::Full)
        .expect("Optimization failed");
    
    // Validate the optimized function
    validate_mir_function(&optimized).expect("Optimized function validation failed");
    
    // Count the number of assignments in the optimized function
    let mut optimized_assignment_count = 0;
    for (_, block) in &optimized.body.blocks {
        for stmt in &block.statements {
            if let MirStatement::Assign { .. } = stmt {
                optimized_assignment_count += 1;
            }
        }
    }
    
    // We should have fewer assignments in the optimized version
    assert!(optimized_assignment_count < original_assignment_count, 
        "DCE didn't remove unused assignments: original: {}, optimized: {}", 
        original_assignment_count, optimized_assignment_count);
} 