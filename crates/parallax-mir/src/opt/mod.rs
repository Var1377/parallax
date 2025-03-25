//! Optimizations for MIR.
//!
//! This module implements various optimizations on MIR.

use std::sync::Arc;
use std::collections::HashSet;
use std::collections::HashMap;

use crate::db::{MirDatabase, OptimizationLevel};
use crate::error::MirError;
use crate::ir::function::{BlockId, FunctionId, LocalId, MirFunction};
use crate::ir::statement::{BinOp, Constant, MirStatement, Operand, Rvalue, UnOp};
use crate::ir::place::Place;
use crate::ir::terminator::MirTerminator;

/// Optimize a MIR function - Database-dependent entry point
pub fn optimize_function(
    db: &dyn MirDatabase,
    mir_function: Arc<MirFunction>,
    opt_level: OptimizationLevel,
) -> Result<Arc<MirFunction>, MirError> {
    // This function is the entry point called from the database
    // It delegates to the core logic, which can be tested separately
    
    // Database-specific work could happen here if needed
    // For example, getting additional information from the database
    
    // For now, just delegate to the core implementation
    optimize_function_impl(mir_function, opt_level)
}

/// Core optimization logic that doesn't depend on database access
/// This is the function that should be fully tested
pub fn optimize_function_impl(
    mir_function: Arc<MirFunction>,
    opt_level: OptimizationLevel,
) -> Result<Arc<MirFunction>, MirError> {
    match opt_level {
        OptimizationLevel::None => Ok(mir_function),
        _ => {
            // Create a mutable copy of the function that we'll optimize
            let mut function = (*mir_function).clone();
            
            // Apply optimization passes based on the optimization level
            if opt_level == OptimizationLevel::Basic || opt_level == OptimizationLevel::Full {
                // Apply constant folding
                constant_folding(&mut function);
            }
            
            if opt_level == OptimizationLevel::Full {
                // Apply dead code elimination
                dead_code_elimination(&mut function);
                
                // TODO: Add more optimization passes for higher optimization levels
            }
            
            Ok(Arc::new(function))
        }
    }
}

/// Constant folding pass
///
/// This optimization evaluates constant expressions at compile time.
fn constant_folding(function: &mut MirFunction) {
    // Process each basic block
    for (_, block) in &mut function.body.blocks {
        // Process each statement in the block
        for stmt in &mut block.statements {
            if let MirStatement::Assign { source, .. } = stmt {
                // Try to fold constants in the right-hand side of assignments
                fold_rvalue(source);
            }
        }
        
        // Process terminators (e.g., for return values)
        match &mut block.terminator {
            crate::ir::terminator::MirTerminator::Return { value } => {
                if let Some(operand) = value {
                    fold_operand(operand);
                }
            }
            crate::ir::terminator::MirTerminator::Switch { discriminant, .. } => {
                fold_operand(discriminant);
            }
            crate::ir::terminator::MirTerminator::Match { scrutinee, .. } => {
                fold_operand(scrutinee);
            }
            crate::ir::terminator::MirTerminator::Call { args, .. } => {
                for arg in args {
                    fold_operand(arg);
                }
            }
            _ => {}
        }
    }
}

/// Dead code elimination pass
///
/// This optimization removes:
/// 1. Unreachable blocks
/// 2. Unused local variables
/// 3. Assignments to variables that are never read
fn dead_code_elimination(function: &mut MirFunction) {
    // Step 1: Find reachable blocks starting from the entry block
    let reachable_blocks = find_reachable_blocks(function);
    
    // Step 2: Remove unreachable blocks
    function.body.blocks.retain(|block_id, _| reachable_blocks.contains(block_id));
    
    // Step 3: Find all used locals
    let used_locals = find_used_locals(function);
    
    // Step 4: Remove unused locals
    function.locals.retain(|local| used_locals.contains(&local.id));
    
    // Step 5: Remove unused assignments
    remove_unused_assignments(function, &used_locals);
}

/// Find all reachable blocks in a function
fn find_reachable_blocks(function: &MirFunction) -> HashSet<BlockId> {
    let mut reachable = HashSet::new();
    let mut worklist = vec![function.body.entry_block];
    
    while let Some(block_id) = worklist.pop() {
        // Skip if we've already processed this block
        if !reachable.insert(block_id) {
            continue;
        }
        
        // Get the terminator of this block
        if let Some(block) = function.body.blocks.get(&block_id) {
            // Add successor blocks to the worklist
            match &block.terminator {
                crate::ir::terminator::MirTerminator::Goto { target } => {
                    worklist.push(*target);
                }
                crate::ir::terminator::MirTerminator::Switch { targets, default, .. } => {
                    worklist.push(*default);
                    for (_, target) in targets {
                        worklist.push(*target);
                    }
                }
                crate::ir::terminator::MirTerminator::Match { arms, default_arm, .. } => {
                    if let Some(default) = default_arm {
                        worklist.push(*default);
                    }
                    for arm in arms {
                        worklist.push(arm.target);
                    }
                }
                crate::ir::terminator::MirTerminator::Call { target, unwind, .. } => {
                    // Push the normal continuation target
                    worklist.push(*target);
                    
                    // Push the unwind target if it exists
                    if let Some(unwind_target) = unwind {
                        worklist.push(*unwind_target);
                    }
                }
                _ => {}
            }
        }
    }
    
    reachable
}

/// Find all used locals in a function
fn find_used_locals(function: &MirFunction) -> HashSet<LocalId> {
    let mut used_locals = HashSet::new();
    
    // Add function parameters to used locals
    for (idx, _) in function.signature.params.iter().enumerate() {
        used_locals.insert(idx as LocalId);
    }
    
    // Process each block to find used locals
    for (_, block) in &function.body.blocks {
        // Check statements for uses of locals
        for stmt in &block.statements {
            match stmt {
                MirStatement::Assign { destination, source } => {
                    // Mark the destination as used
                    used_locals.insert(destination.local);
                    
                    // Check the source for uses of locals
                    add_used_locals_from_rvalue(source, &mut used_locals);
                }
                MirStatement::CallVoid { args, .. } => {
                    // Check arguments for uses of locals
                    for arg in args {
                        add_used_locals_from_operand(arg, &mut used_locals);
                    }
                }
                MirStatement::Alloc { place, .. } => {
                    // Mark the allocated place as used
                    used_locals.insert(place.local);
                }
                _ => {}
            }
        }
        
        // Check terminator for uses of locals
        match &block.terminator {
            crate::ir::terminator::MirTerminator::Return { value } => {
                if let Some(operand) = value {
                    add_used_locals_from_operand(operand, &mut used_locals);
                }
            }
            crate::ir::terminator::MirTerminator::Switch { discriminant, .. } => {
                add_used_locals_from_operand(discriminant, &mut used_locals);
            }
            crate::ir::terminator::MirTerminator::Match { scrutinee, .. } => {
                add_used_locals_from_operand(scrutinee, &mut used_locals);
            }
            crate::ir::terminator::MirTerminator::Call { func, args, .. } => {
                add_used_locals_from_operand(func, &mut used_locals);
                for arg in args {
                    add_used_locals_from_operand(arg, &mut used_locals);
                }
            }
            _ => {}
        }
    }
    
    // Perform another pass to find locals that are used in alive blocks
    // This is a fixpoint computation to handle more complex usage patterns
    let mut changed = true;
    while changed {
        changed = false;
        
        for (_, block) in &function.body.blocks {
            for stmt in &block.statements {
                if let MirStatement::Assign { destination, source } = stmt {
                    match source {
                        Rvalue::Use(Operand::Copy(place)) | Rvalue::Use(Operand::Move(place)) => {
                            if used_locals.contains(&destination.local) && !used_locals.contains(&place.local) {
                                used_locals.insert(place.local);
                                changed = true;
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
    }
    
    used_locals
}

/// Add locals used in an rvalue to the set of used locals
fn add_used_locals_from_rvalue(rvalue: &Rvalue, used_locals: &mut HashSet<LocalId>) {
    match rvalue {
        Rvalue::Use(operand) => {
            add_used_locals_from_operand(operand, used_locals);
        }
        Rvalue::Ref(place) => {
            used_locals.insert(place.local);
        }
        Rvalue::BinaryOp { left, right, .. } => {
            add_used_locals_from_operand(left, used_locals);
            add_used_locals_from_operand(right, used_locals);
        }
        Rvalue::UnaryOp { operand, .. } => {
            add_used_locals_from_operand(operand, used_locals);
        }
        Rvalue::Call { func, args, .. } => {
            add_used_locals_from_operand(func, used_locals);
            for arg in args {
                add_used_locals_from_operand(arg, used_locals);
            }
        }
        Rvalue::Aggregate { elements, .. } => {
            for elem in elements {
                add_used_locals_from_operand(elem, used_locals);
            }
        }
    }
}

/// Add locals used in an operand to the set of used locals
fn add_used_locals_from_operand(operand: &Operand, used_locals: &mut HashSet<LocalId>) {
    match operand {
        Operand::Copy(place) | Operand::Move(place) => {
            used_locals.insert(place.local);
        }
        Operand::Constant(_) => {}
    }
}

/// Check if an rvalue has side effects
fn rvalue_has_side_effects(rvalue: &Rvalue) -> bool {
    match rvalue {
        Rvalue::Call { .. } => true, // Function calls may have side effects
        _ => false,
    }
}

/// Remove unused assignments from a function
fn remove_unused_assignments(function: &mut MirFunction, used_locals: &HashSet<LocalId>) {
    // Track the last assignment to each local in each block
    // If a local is assigned multiple times without being read in between, 
    // we can remove all but the last assignment
    for (_, block) in &mut function.body.blocks {
        let mut last_write = HashMap::new();
        let mut to_remove = HashSet::new();
        
        // First pass: identify assignments that can be removed
        for (idx, stmt) in block.statements.iter().enumerate() {
            match stmt {
                MirStatement::Assign { destination, source } => {
                    let local_id = destination.local;
                    
                    // If local was previously written to but never read, mark the previous assignment for removal
                    if let Some(prev_idx) = last_write.get(&local_id) {
                        to_remove.insert(*prev_idx);
                    }
                    
                    // Record this assignment as the last write to the local
                    last_write.insert(local_id, idx);
                }
                _ => {
                    // For other statements, check if they read any locals
                    match stmt {
                        MirStatement::Assign { source, .. } => {
                            // Check if the source reads any locals
                            for local in find_locals_read_in_rvalue(source) {
                                // If a local is read, don't remove its last assignment
                                last_write.remove(&local);
                            }
                        }
                        // No other statement types that read locals in this implementation
                        _ => {}
                    }
                }
            }
        }
        
        // Also check the terminator for reads
        match &block.terminator {
            MirTerminator::Return { value } => {
                if let Some(value) = value {
                    // If a local is read in the return value, don't remove its last assignment
                    for local in find_locals_read_in_operand(value) {
                        last_write.remove(&local);
                    }
                }
            }
            MirTerminator::Goto { .. } | MirTerminator::Unreachable => {
                // These don't read locals
            }
            MirTerminator::Switch { discriminant, .. } => {
                // Check if discriminant reads any locals
                for local in find_locals_read_in_operand(discriminant) {
                    last_write.remove(&local);
                }
            }
            MirTerminator::Call { func, args, .. } => {
                // Check if func or args read any locals
                for local in find_locals_read_in_operand(func) {
                    last_write.remove(&local);
                }
                for arg in args {
                    for local in find_locals_read_in_operand(arg) {
                        last_write.remove(&local);
                    }
                }
            }
            MirTerminator::Match { scrutinee, .. } => {
                // Check if scrutinee reads any locals
                for local in find_locals_read_in_operand(scrutinee) {
                    last_write.remove(&local);
                }
            }
        }
        
        // Add any remaining last writes to unused locals to be removed
        for (local, idx) in last_write {
            if !used_locals.contains(&local) {
                to_remove.insert(idx);
            }
        }
        
        // Second pass: filter out the statements marked for removal
        let mut new_statements = Vec::new();
        for (idx, stmt) in block.statements.iter().enumerate() {
            match stmt {
                MirStatement::Assign { destination, source } => {
                    // Keep the assignment if:
                    // 1. It's not marked for removal, OR
                    // 2. The rvalue has side effects (like a function call)
                    if !to_remove.contains(&idx) || rvalue_has_side_effects(source) {
                        new_statements.push(stmt.clone());
                    }
                }
                _ => {
                    // Keep all other statements
                    new_statements.push(stmt.clone());
                }
            }
        }
        
        // Replace the block's statements with the filtered list
        block.statements = new_statements;
    }
}

/// Find all locals that are read in an rvalue
fn find_locals_read_in_rvalue(rvalue: &Rvalue) -> HashSet<LocalId> {
    let mut result = HashSet::new();
    match rvalue {
        Rvalue::Use(operand) => {
            result.extend(find_locals_read_in_operand(operand));
        }
        Rvalue::Ref(place) => {
            result.insert(place.local);
        }
        Rvalue::BinaryOp { left, right, .. } => {
            result.extend(find_locals_read_in_operand(left));
            result.extend(find_locals_read_in_operand(right));
        }
        Rvalue::UnaryOp { operand, .. } => {
            result.extend(find_locals_read_in_operand(operand));
        }
        Rvalue::Call { func, args, .. } => {
            result.extend(find_locals_read_in_operand(func));
            for arg in args {
                result.extend(find_locals_read_in_operand(arg));
            }
        }
        Rvalue::Aggregate { elements, .. } => {
            for elem in elements {
                result.extend(find_locals_read_in_operand(elem));
            }
        }
    }
    result
}

/// Find all locals that are read in an operand
fn find_locals_read_in_operand(operand: &Operand) -> HashSet<LocalId> {
    let mut result = HashSet::new();
    match operand {
        Operand::Copy(place) | Operand::Move(place) => {
            result.insert(place.local);
        }
        Operand::Constant(_) => {}
    }
    result
}

/// Fold constants in an rvalue
fn fold_rvalue(rvalue: &mut Rvalue) {
    match rvalue {
        Rvalue::BinaryOp { op, left, right } => {
            // First fold the operands recursively
            fold_operand(left);
            fold_operand(right);
            
            // Try to fold the binary operation if both operands are constants
            if let (Operand::Constant(lhs), Operand::Constant(rhs)) = (left, right) {
                if let Some(result) = evaluate_binary_op(*op, lhs, rhs) {
                    // Replace the binary operation with the constant result
                    *rvalue = Rvalue::Use(Operand::Constant(result));
                }
            }
        }
        Rvalue::UnaryOp { op, operand } => {
            // First fold the operand recursively
            fold_operand(operand);
            
            // Try to fold the unary operation if the operand is a constant
            if let Operand::Constant(constant) = operand {
                if let Some(result) = evaluate_unary_op(*op, constant) {
                    // Replace the unary operation with the constant result
                    *rvalue = Rvalue::Use(Operand::Constant(result));
                }
            }
        }
        Rvalue::Call { args, .. } => {
            // Fold arguments to function calls
            for arg in args {
                fold_operand(arg);
            }
        }
        Rvalue::Aggregate { elements, .. } => {
            // Fold elements in aggregates
            for elem in elements {
                fold_operand(elem);
            }
        }
        _ => {}
    }
}

/// Fold constants in an operand
fn fold_operand(operand: &mut Operand) {
    // Currently, we only need to handle place references that might need folding
    // Constant operands are already folded
}

/// Evaluate a binary operation on constants at compile time
fn evaluate_binary_op(op: BinOp, lhs: &Constant, rhs: &Constant) -> Option<Constant> {
    match (lhs, rhs) {
        (Constant::Int(a), Constant::Int(b)) => {
            // Integer binary operations
            match op {
                BinOp::Add => Some(Constant::Int(a + b)),
                BinOp::Sub => Some(Constant::Int(a - b)),
                BinOp::Mul => Some(Constant::Int(a * b)),
                BinOp::Div if *b != 0 => Some(Constant::Int(a / b)),
                BinOp::Rem if *b != 0 => Some(Constant::Int(a % b)),
                BinOp::BitAnd => Some(Constant::Int(a & b)),
                BinOp::BitOr => Some(Constant::Int(a | b)),
                BinOp::BitXor => Some(Constant::Int(a ^ b)),
                BinOp::Shl => Some(Constant::Int(a << b)),
                BinOp::Shr => Some(Constant::Int(a >> b)),
                BinOp::Eq => Some(Constant::Bool(a == b)),
                BinOp::Ne => Some(Constant::Bool(a != b)),
                BinOp::Lt => Some(Constant::Bool(a < b)),
                BinOp::Le => Some(Constant::Bool(a <= b)),
                BinOp::Gt => Some(Constant::Bool(a > b)),
                BinOp::Ge => Some(Constant::Bool(a >= b)),
                _ => None,
            }
        }
        (Constant::Float(a), Constant::Float(b)) => {
            // Floating-point binary operations
            match op {
                BinOp::Add => Some(Constant::Float(a + b)),
                BinOp::Sub => Some(Constant::Float(a - b)),
                BinOp::Mul => Some(Constant::Float(a * b)),
                BinOp::Div if *b != 0.0 => Some(Constant::Float(a / b)),
                BinOp::Eq => Some(Constant::Bool(a == b)),
                BinOp::Ne => Some(Constant::Bool(a != b)),
                BinOp::Lt => Some(Constant::Bool(a < b)),
                BinOp::Le => Some(Constant::Bool(a <= b)),
                BinOp::Gt => Some(Constant::Bool(a > b)),
                BinOp::Ge => Some(Constant::Bool(a >= b)),
                _ => None,
            }
        }
        (Constant::Bool(a), Constant::Bool(b)) => {
            // Boolean binary operations
            match op {
                BinOp::BitAnd => Some(Constant::Bool(*a && *b)),
                BinOp::BitOr => Some(Constant::Bool(*a || *b)),
                BinOp::BitXor => Some(Constant::Bool(*a != *b)),
                BinOp::Eq => Some(Constant::Bool(a == b)),
                BinOp::Ne => Some(Constant::Bool(a != b)),
                _ => None,
            }
        }
        (Constant::String(a), Constant::String(b)) => {
            // String binary operations
            match op {
                BinOp::Add => Some(Constant::String(format!("{}{}", a, b))),
                BinOp::Eq => Some(Constant::Bool(a == b)),
                BinOp::Ne => Some(Constant::Bool(a != b)),
                _ => None,
            }
        }
        _ => None, // Other combinations are not supported for constant folding
    }
}

/// Evaluate a unary operation on a constant at compile time
fn evaluate_unary_op(op: UnOp, constant: &Constant) -> Option<Constant> {
    match (op, constant) {
        (UnOp::Neg, Constant::Int(i)) => Some(Constant::Int(-i)),
        (UnOp::Neg, Constant::Float(f)) => Some(Constant::Float(-f)),
        (UnOp::Not, Constant::Int(i)) => Some(Constant::Int(!i)),
        (UnOp::Not, Constant::Bool(b)) => Some(Constant::Bool(!b)),
        _ => None, // Other combinations are not supported for constant folding
    }
}

/// Determine if a function can be inlined
pub fn can_be_inlined(function: &MirFunction) -> bool {
    // This is a stub that will be implemented later
    // For now, assume no function can be inlined
    false
}

/// Check if a function has side effects
pub fn has_side_effects(function: &MirFunction) -> bool {
    // This is a stub that will be implemented later
    // For now, assume all functions have side effects
    true
}

/// Get the size of a function (for inlining decisions)
pub fn function_size(function: &MirFunction) -> usize {
    // This is a stub that will be implemented later
    // For now, return a placeholder size
    function.body.blocks.len()
}

/// Test version of optimize_function that doesn't depend on MirDatabase
#[cfg(test)]
pub fn optimize_function_test(
    mir_function: Arc<MirFunction>,
    opt_level: OptimizationLevel,
) -> Result<Arc<MirFunction>, MirError> {
    // Call the actual core implementation function that will be used in production
    // This ensures we're testing the real logic, not just a test double
    optimize_function_impl(mir_function, opt_level)
} 