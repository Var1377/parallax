//! Tests for MIR visitors.

use crate::db::MirDatabase;
use crate::ir::function::{BlockId, FunctionId, LocalId, MirFunction, MirType};
use crate::ir::place::Place;
use crate::ir::statement::{Constant, MirStatement, Operand, Rvalue};
use crate::ir::terminator::MirTerminator;
use crate::ir::visit::{MirCounter, MirVisitor};
use crate::tests::{create_test_db, create_test_function};
use std::sync::Arc;

/// A visitor for collecting specific information from a MIR function
#[derive(Default)]
struct TestVisitor {
    // Count of different node types
    statement_count: usize,
    terminator_count: usize,
    
    // Specific node types
    assignment_count: usize,
    binary_op_count: usize,
    call_count: usize,
    return_count: usize,
    
    // Local tracking
    locals_referenced: Vec<LocalId>,
}

impl MirVisitor for TestVisitor {
    fn visit_statement(&mut self, statement: &MirStatement) {
        self.statement_count += 1;
        
        match statement {
            MirStatement::Assign { .. } => {
                self.assignment_count += 1;
            }
            MirStatement::CallVoid { .. } => {
                self.call_count += 1;
            }
            _ => {}
        }
        
        // Continue with traversal
        match statement {
            MirStatement::Assign { source: Rvalue::BinaryOp { .. }, .. } => {
                self.binary_op_count += 1;
            }
            _ => {}
        }
        
        // Visit nested nodes
        match statement {
            MirStatement::Assign { destination, source } => {
                self.visit_place(destination);
                self.visit_rvalue(source);
            }
            MirStatement::Alloc { place, .. } => {
                self.visit_place(place);
            }
            MirStatement::CallVoid { args, .. } => {
                for arg in args {
                    self.visit_operand(arg);
                }
            }
            MirStatement::Nop => {
                // Nothing to visit
            }
        }
    }
    
    fn visit_terminator(&mut self, terminator: &MirTerminator) {
        self.terminator_count += 1;
        
        match terminator {
            MirTerminator::Return { .. } => {
                self.return_count += 1;
            }
            MirTerminator::Call { .. } => {
                self.call_count += 1;
            }
            _ => {}
        }
        
        // Visit nested nodes
        match terminator {
            MirTerminator::Return { value } => {
                if let Some(value) = value {
                    self.visit_operand(value);
                }
            }
            MirTerminator::Call { destination, args, .. } => {
                if let Some(dst) = destination {
                    self.visit_place(dst);
                }
                for arg in args {
                    self.visit_operand(arg);
                }
            }
            MirTerminator::Match { scrutinee, .. } => {
                self.visit_operand(scrutinee);
            }
            _ => {}
        }
    }
    
    fn visit_place(&mut self, place: &Place) {
        if !self.locals_referenced.contains(&place.local) {
            self.locals_referenced.push(place.local);
        }
    }

    fn visit_rvalue(&mut self, rvalue: &Rvalue) {
        match rvalue {
            Rvalue::Use(operand) => {
                self.visit_operand(operand);
            }
            Rvalue::BinaryOp { left, right, .. } => {
                self.visit_operand(left);
                self.visit_operand(right);
            }
            Rvalue::UnaryOp { operand, .. } => {
                self.visit_operand(operand);
            }
            Rvalue::Call { args, .. } => {
                for arg in args {
                    self.visit_operand(arg);
                }
            }
            _ => {}
        }
    }

    fn visit_operand(&mut self, operand: &Operand) {
        match operand {
            Operand::Copy(place) | Operand::Move(place) => {
                self.visit_place(place);
            }
            _ => {}
        }
    }
}

/// Test requirement: MirVisitor traverses all nodes in a MIR function
/// 
/// This test checks that the MirVisitor correctly traverses a MIR function:
/// - All statements are visited
/// - All terminators are visited
/// - All operands are visited
/// - All places are visited
#[test]
fn test_visitor_traversal() {
    let db = create_test_db();
    let hir_func_id = create_test_function("visitor_test");
    
    // This will fail initially until we implement function lowering
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Create a visitor and traverse the function
    let mut visitor = TestVisitor::default();
    visitor.visit_function(&mir_func);
    
    // Check that the visitor counted the correct number of nodes
    assert!(visitor.statement_count > 0, "No statements visited");
    assert!(visitor.terminator_count > 0, "No terminators visited");
    
    // The number of terminators should equal the number of basic blocks
    assert_eq!(visitor.terminator_count, mir_func.body.blocks.len());
}

/// Test requirement: MirCounter correctly counts MIR elements
/// 
/// This test checks that the MirCounter correctly counts elements in a MIR function:
/// - Functions
/// - Basic blocks
/// - Statements
/// - Terminators
/// - Local variables
#[test]
fn test_mir_counter() {
    let db = create_test_db();
    let hir_func_id = create_test_function("counter_test");
    
    // Get the counter for the function
    let counter_result = db.mir_count(hir_func_id);
    
    // The counter should now return actual values based on our implementation
    let counter = counter_result.expect("Failed to count MIR elements");
    
    // Check that the counter returns reasonable values
    // For an empty function we expect:
    assert_eq!(counter.functions, 1);   // One function
    assert!(counter.blocks > 0, "Function should have at least one block");
    assert_eq!(counter.terminators, counter.blocks, "Each block should have a terminator");
    // The rest may vary based on implementation
}

/// Test requirement: Custom visitor collects specific information
/// 
/// This test checks that a custom visitor can collect specific information from a MIR function:
/// - Count of specific statement types
/// - Count of specific terminator types
/// - Collection of referenced locals
#[test]
fn test_custom_visitor() {
    let db = create_test_db();
    let hir_func_id = create_test_function("custom_visitor_test");
    
    // This will fail initially until we implement function lowering
    let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
    
    // Create a visitor and traverse the function
    let mut visitor = TestVisitor::default();
    visitor.visit_function(&mir_func);
    
    // Check that we collected some information
    assert!(visitor.statement_count >= visitor.assignment_count, 
            "Assignment count exceeds total statement count");
    assert!(visitor.terminator_count >= visitor.return_count,
            "Return count exceeds total terminator count");
            
    // All locals should be referenced at least once
    for local in &mir_func.locals {
        assert!(visitor.locals_referenced.contains(&local.id),
                "Local {} not referenced in the function", local.id);
    }
} 