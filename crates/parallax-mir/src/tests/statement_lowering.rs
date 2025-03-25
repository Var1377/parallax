//! Tests for lowering statements to MIR.

use crate::ir::function::{MirFunction, MirType};
use crate::ir::place::Place;
use crate::ir::statement::{Constant, MirStatement, Operand, Rvalue};
use crate::ir::terminator::MirTerminator;
use crate::tests::{create_test_db, create_test_function, validate_mir_function};
use std::sync::Arc;

/// Test requirement: Assignment statements must lower correctly
/// 
/// This test checks that assignment statements correctly lower to MIR with:
/// - Assignments to local variables
/// - Right-hand side expressions properly lowered
#[test]
fn test_assignment_statements() {
    let db = create_test_db();
    let hir_func_id = create_test_function("assignments");
    
    // This will fail initially until we implement statement lowering
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Validate the function structure
    validate_mir_function(&mir_func).expect("MIR function validation failed");
    
    // Look for assignment statements
    let mut found_assignment = false;
    
    for (_, block) in &mir_func.body.blocks {
        for stmt in &block.statements {
            if let MirStatement::Assign { .. } = stmt {
                found_assignment = true;
                break;
            }
        }
    }
    
    assert!(found_assignment, "No assignment statements found in the function");
}

/// Test requirement: Variable declarations must lower correctly
/// 
/// This test checks that variable declarations correctly lower to MIR with:
/// - Allocation for the variable
/// - Initialization with the provided value
#[test]
fn test_variable_declarations() {
    let db = create_test_db();
    let hir_func_id = create_test_function("var_decls");
    
    // This will fail initially until we implement variable declaration lowering
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Validate the function structure
    validate_mir_function(&mir_func).expect("MIR function validation failed");
    
    // Look for allocation statements
    let mut found_allocation = false;
    
    for (_, block) in &mir_func.body.blocks {
        for stmt in &block.statements {
            if let MirStatement::Alloc { .. } = stmt {
                found_allocation = true;
                break;
            }
        }
    }
    
    assert!(found_allocation, "No variable allocation statements found in the function");
}

/// Test requirement: Field access must lower correctly
/// 
/// This test checks that field access correctly lowers to MIR with:
/// - Places with field projections
/// - Both reading from and writing to fields
#[test]
fn test_field_access() {
    let db = create_test_db();
    let hir_func_id = create_test_function("field_access");
    
    // This will fail initially until we implement field access lowering
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Validate the function structure
    validate_mir_function(&mir_func).expect("MIR function validation failed");
    
    // Look for places with field projections
    let mut found_field_projection = false;
    
    for (_, block) in &mir_func.body.blocks {
        for stmt in &block.statements {
            // Check for field access in assignments
            if let MirStatement::Assign { destination, source } = stmt {
                if !destination.projection.is_empty() {
                    // Check if any projection is a field
                    if destination.projection.iter().any(|proj| 
                        matches!(proj, crate::ir::place::ProjectionElem::Field { .. })) {
                        found_field_projection = true;
                        break;
                    }
                }
                
                // Check for field access in rvalues
                if let Rvalue::Use(Operand::Copy(place) | Operand::Move(place)) = source {
                    if place.projection.iter().any(|proj| 
                        matches!(proj, crate::ir::place::ProjectionElem::Field { .. })) {
                        found_field_projection = true;
                        break;
                    }
                }
            }
        }
    }
    
    assert!(found_field_projection, "No field access projections found in the function");
}

/// Test requirement: Function call statements must lower correctly
/// 
/// This test checks that function call statements correctly lower to MIR with:
/// - Call statements for void functions
/// - Call rvalues for functions that return values
/// - Proper argument passing
#[test]
fn test_function_call_statements() {
    let db = create_test_db();
    let hir_func_id = create_test_function("call_stmts");
    
    // This will fail initially until we implement function call lowering
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Validate the function structure
    validate_mir_function(&mir_func).expect("MIR function validation failed");
    
    // Look for call statements or rvalues
    let mut found_call = false;
    
    for (_, block) in &mir_func.body.blocks {
        for stmt in &block.statements {
            match stmt {
                MirStatement::CallVoid { .. } => {
                    found_call = true;
                    break;
                }
                MirStatement::Assign { source: Rvalue::Call { .. }, .. } => {
                    found_call = true;
                    break;
                }
                _ => {}
            }
        }
    }
    
    assert!(found_call, "No function call statements found in the function");
}

/// Test requirement: Array indexing must lower correctly
/// 
/// This test checks that array indexing correctly lowers to MIR with:
/// - Places with index projections
/// - Both reading from and writing to array elements
#[test]
fn test_array_indexing() {
    let db = create_test_db();
    let hir_func_id = create_test_function("array_index");
    
    // This will fail initially until we implement array indexing lowering
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Validate the function structure
    validate_mir_function(&mir_func).expect("MIR function validation failed");
    
    // Look for places with index projections
    let mut found_index_projection = false;
    
    for (_, block) in &mir_func.body.blocks {
        for stmt in &block.statements {
            // Check for index access in assignments
            if let MirStatement::Assign { destination, source } = stmt {
                if !destination.projection.is_empty() {
                    // Check if any projection is an index
                    if destination.projection.iter().any(|proj| 
                        matches!(proj, crate::ir::place::ProjectionElem::Index(_))) {
                        found_index_projection = true;
                        break;
                    }
                }
                
                // Check for index access in rvalues
                if let Rvalue::Use(Operand::Copy(place) | Operand::Move(place)) = source {
                    if place.projection.iter().any(|proj| 
                        matches!(proj, crate::ir::place::ProjectionElem::Index(_))) {
                        found_index_projection = true;
                        break;
                    }
                }
            }
        }
    }
    
    assert!(found_index_projection, "No array index projections found in the function");
} 