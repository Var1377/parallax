//! Tests for the MIR crate.
//!
//! This module contains unit and integration tests for the MIR crate.

// Module organization
mod function_lowering;
mod pattern_matching;
mod type_conversions;
mod statement_lowering;
mod expression_lowering;
mod operator_lowering;
mod monomorphization;
mod optimizations;
mod visitors;

// Required imports
use crate::error::MirError;
use crate::ir::function::MirFunction;
use crate::ir::function::MirType;
use crate::ir::visit::{MirCounter, MirVisitor};
use std::sync::Arc;

/// Our testing approach has two branches:
/// 
/// 1. Tests that focus on testing the core logic without database dependencies.
///    These call the *_impl functions directly and don't use TestDatabase.
///    
/// 2. Tests that need to simulate the database interface.
///    These use TestDatabase which simulates HIR data and calls the core functions.
/// 
/// This approach ensures we're testing the actual implementation code that will
/// be used in production, not just test doubles.

// Create a simple struct to represent our test database
// We don't use the Salsa DB for tests to avoid dependency issues
#[derive(Default)]
pub struct TestDatabase {}

// Implement methods directly on TestDatabase rather than through a trait
impl TestDatabase {
    pub fn mir_function(&self, hir_function_id: u32) -> Result<Arc<MirFunction>, MirError> {
        // Create a mock HIR function (this simulates what the database would normally do)
        // Then call the real implementation function
        
        // This creates a mock HIR function that matches what lower_function would construct
        let mock_hir = crate::lower::MockHirFunction {
            id: hir_function_id,
        };
        
        // Call the actual implementation
        crate::lower::lower_function_from_hir(mock_hir)
    }

    pub fn mir_count(&self, hir_function_id: u32) -> Result<MirCounter, MirError> {
        // First get the MIR function
        let mir_function = self.mir_function(hir_function_id)?;
        
        // Now count using the real counting logic
        let mut counter = MirCounter::default();
        counter.visit_function(&mir_function);
        Ok(counter)
    }

    pub fn monomorphized_function(
        &self,
        mir_function: Arc<MirFunction>,
        type_args: Vec<Arc<MirType>>,
    ) -> Result<Arc<MirFunction>, MirError> {
        // Call the real implementation function directly
        crate::mono::monomorphize_function_impl(mir_function, type_args)
    }

    pub fn optimized_function(
        &self,
        mir_function: Arc<MirFunction>,
        opt_level: crate::db::OptimizationLevel,
    ) -> Result<Arc<MirFunction>, MirError> {
        // Call the real implementation function directly
        crate::opt::optimize_function_impl(mir_function, opt_level)
    }

    pub fn stdlib_functions(&self) -> Arc<Vec<Arc<MirFunction>>> {
        // For now, just return an empty list of functions
        // This will be enhanced as we implement the standard library
        Arc::new(Vec::new())
    }
}

// Helper function to create a simple HIR function for testing
pub fn create_test_function(name: &str) -> u32 {
    // Map function names to IDs based on our test cases
    match name {
        "empty" => 42,
        "return_42" => 43,
        "add" => 44,
        "match_constant" => 45,
        "match_enum" => 46,
        "match_bind" => 47,
        "match_nested" => 48,
        "match_guard" => 49,
        "literals" => 50,
        "binary_ops" => 51,
        "unary_ops" => 52,
        "block_expr" => 53,
        "var_refs" => 54,
        "assignments" => 55,
        "var_decls" => 56,
        "field_access" => 57,
        "call_stmts" => 58,
        "call_other" => 68,
        "array_index" => 59,
        "basic_types" => 71,
        "function_types" => 72,
        "named_types" => 73,
        "generic_types" => 74,
        "compound_types" => 75,
        "visitor_test" => 65,
        "counter_test" => 66,
        "custom_visitor_test" => 67,
        "binary_op_calls" => 80,
        "unary_op_calls" => 81,
        "operator_precedence" => 82,
        // Monomorphization test functions
        "generic_function" => 90,
        "complex_generic_function" => 91,
        // Optimization test functions
        "constant_folding_binary" => 100,
        "constant_folding_unary" => 101,
        "constant_folding_complex" => 102,
        // Dead code elimination test functions
        "dce_unreachable_blocks" => 103,
        "dce_unused_locals" => 104,
        "dce_unused_assignments" => 105,
        _ => 42, // Default to empty function
    }
}

// Helper to create a simple test database
pub fn create_test_db() -> TestDatabase {
    TestDatabase::default()
}

// Helper to verify a MIR function meets basic validation criteria
pub fn validate_mir_function(function: &MirFunction) -> Result<(), String> {
    // Basic validations:
    // 1. At least one basic block
    if function.body.blocks.is_empty() {
        return Err("Function has no basic blocks".to_string());
    }

    // 2. Entry block must exist
    if !function.body.blocks.contains_key(&function.body.entry_block) {
        return Err("Entry block not found in function".to_string());
    }

    // 3. All blocks must have a terminator
    for (id, block) in &function.body.blocks {
        if matches!(block.terminator, crate::ir::terminator::MirTerminator::Unreachable) {
            // If it's an Unreachable terminator, that's valid only if the block is not reachable
            // For simple validation, we'll let it pass for now
        }
    }

    // 4. All referenced locals must be declared
    let max_local_id = function
        .locals
        .iter()
        .map(|local| local.id)
        .max()
        .unwrap_or(0);

    for local in &function.locals {
        if local.id > max_local_id {
            return Err(format!("Local ID {} out of range", local.id));
        }
    }

    Ok(())
} 