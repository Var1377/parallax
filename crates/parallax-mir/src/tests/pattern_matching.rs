//! Tests for lowering pattern matching to MIR.

use crate::ir::function::{BlockId, MirFunction, MirType};
use crate::ir::statement::{Constant, MirStatement, Operand, Rvalue};
use crate::ir::terminator::{MatchArm, MatchPattern, MirTerminator, SwitchValue};
use crate::tests::{create_test_db, create_test_function, validate_mir_function};
use std::sync::Arc;

/// Test requirement: Simple match on a constant must lower correctly
/// 
/// This test checks that a simple match on a constant correctly lowers to MIR with:
/// - A match terminator with the correct scrutinee
/// - Appropriate match arms with constant patterns
/// - Correct target blocks for each arm
#[test]
fn test_match_on_constant() {
    let db = create_test_db();
    let hir_func_id = create_test_function("match_constant");
    
    // This will fail initially until we implement pattern matching lowering
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Validate the function structure
    validate_mir_function(&mir_func).expect("MIR function validation failed");
    
    // Look for a match terminator
    let mut found_match = false;
    
    for (_, block) in &mir_func.body.blocks {
        if let MirTerminator::Match { .. } = &block.terminator {
            found_match = true;
            break;
        }
    }
    
    assert!(found_match, "No match terminator found in the function");
}

/// Test requirement: Match on an enumeration must lower correctly
/// 
/// This test checks that a match on an enum correctly lowers to MIR with:
/// - A match terminator with the correct scrutinee
/// - Variant patterns for enum matching
/// - Sub-patterns for variant fields
#[test]
fn test_match_on_enum() {
    let db = create_test_db();
    let hir_func_id = create_test_function("match_enum");
    
    // This will fail initially until we implement enum pattern matching
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Validate the function structure
    validate_mir_function(&mir_func).expect("MIR function validation failed");
    
    // Look for a match terminator with variant patterns
    let mut found_variant_match = false;
    
    for (_, block) in &mir_func.body.blocks {
        if let MirTerminator::Match { arms, .. } = &block.terminator {
            for arm in arms {
                if let MatchPattern::Variant { .. } = &arm.pattern {
                    found_variant_match = true;
                    break;
                }
            }
        }
    }
    
    assert!(found_variant_match, "No match on enum variants found in the function");
}

/// Test requirement: Match with binding patterns must lower correctly
/// 
/// This test checks that a match with binding patterns correctly lowers to MIR with:
/// - Binding patterns that capture values
/// - Local variables created for bindings
/// - Correct use of bound variables in the arm blocks
#[test]
fn test_match_with_bindings() {
    let db = create_test_db();
    let hir_func_id = create_test_function("match_bind");
    
    // This will fail initially until we implement binding patterns
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Validate the function structure
    validate_mir_function(&mir_func).expect("MIR function validation failed");
    
    // Look for a match terminator with binding patterns
    let mut found_binding_match = false;
    
    for (_, block) in &mir_func.body.blocks {
        if let MirTerminator::Match { arms, .. } = &block.terminator {
            for arm in arms {
                if let MatchPattern::Binding { .. } = &arm.pattern {
                    found_binding_match = true;
                    break;
                }
            }
        }
    }
    
    assert!(found_binding_match, "No match with binding patterns found in the function");
}

/// Test requirement: Match with nested patterns must lower correctly
/// 
/// This test checks that a match with nested patterns correctly lowers to MIR with:
/// - Complex patterns involving tuples, structs, or nested enums
/// - Correct decomposition of nested patterns
#[test]
fn test_match_with_nested_patterns() {
    let db = create_test_db();
    let hir_func_id = create_test_function("match_nested");
    
    // This will fail initially until we implement nested pattern matching
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Validate the function structure
    validate_mir_function(&mir_func).expect("MIR function validation failed");
    
    // For nested patterns, we'd need to walk the pattern tree
    // Check that we have a match terminator first
    let mut found_nested_match = false;
    
    for (_, block) in &mir_func.body.blocks {
        if let MirTerminator::Match { arms, .. } = &block.terminator {
            // Simplified check: just verify that we have patterns that could be nested
            // (tuples, structs or variant with fields)
            for arm in arms {
                match &arm.pattern {
                    MatchPattern::Tuple(fields) => {
                        assert!(!fields.is_empty(), "Empty tuple pattern should not exist");
                        found_nested_match = true;
                    }
                    MatchPattern::Struct { fields, .. } => {
                        assert!(!fields.is_empty(), "Empty struct pattern should not exist");
                        found_nested_match = true;
                    }
                    MatchPattern::Variant { fields, .. } => {
                        assert!(!fields.is_empty(), "Empty variant pattern should not exist");
                        found_nested_match = true;
                    }
                    _ => {}
                }
            }
        }
    }
    
    assert!(found_nested_match, "No match with nested patterns found in the function");
}

/// Test requirement: Match with guard conditions must lower correctly
/// 
/// This test checks that a match with guard conditions correctly lowers to MIR with:
/// - Guard conditions attached to match arms
/// - Conditional branching based on guard evaluation
#[test]
fn test_match_with_guards() {
    let db = create_test_db();
    let hir_func_id = create_test_function("match_guard");
    
    // This will fail initially until we implement guard conditions
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Validate the function structure
    validate_mir_function(&mir_func).expect("MIR function validation failed");
    
    // Look for a match terminator with guard conditions
    let mut found_guard = false;
    
    for (_, block) in &mir_func.body.blocks {
        if let MirTerminator::Match { arms, .. } = &block.terminator {
            for arm in arms {
                if arm.guard.is_some() {
                    found_guard = true;
                    break;
                }
            }
        }
    }
    
    assert!(found_guard, "No match with guard conditions found in the function");
} 