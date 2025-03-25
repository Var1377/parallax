//! MIR visitor pattern
//!
//! This module provides a visitor pattern for traversing MIR structures.

use super::function::{
    BasicBlock, ControlFlowGraph, MirFunction, MirFunctionSignature, MirLocalDecl, MirType,
};
use super::place::{Place, ProjectionElem};
use super::statement::{
    AggregateKind, BinOp, Constant, MirStatement, Operand, Rvalue, UnOp,
};
use super::terminator::{MatchArm, MatchPattern, MirTerminator, SwitchValue};
use std::hash::Hash;
use std::hash::Hasher;

/// A visitor for MIR structures
pub trait MirVisitor {
    /// Visit a function
    fn visit_function(&mut self, function: &MirFunction) {
        self.visit_function_signature(&function.signature);
        self.visit_control_flow_graph(&function.body);
        for local in &function.locals {
            self.visit_local_decl(local);
        }
    }

    /// Visit a function signature
    fn visit_function_signature(&mut self, signature: &MirFunctionSignature) {
        for param_ty in &signature.params {
            self.visit_type(param_ty);
        }
        self.visit_type(&signature.return_type);
    }

    /// Visit a local declaration
    fn visit_local_decl(&mut self, local: &MirLocalDecl) {
        self.visit_type(&local.ty);
    }

    /// Visit a type
    fn visit_type(&mut self, ty: &MirType) {
        match ty {
            MirType::Named { args, .. } => {
                for arg in args {
                    self.visit_type(arg);
                }
            }
            MirType::Tuple(types) => {
                for ty in types {
                    self.visit_type(ty);
                }
            }
            MirType::Array(elem) => {
                self.visit_type(elem);
            }
            MirType::Function { params, ret } => {
                for param in params {
                    self.visit_type(param);
                }
                self.visit_type(ret);
            }
            _ => {}
        }
    }

    /// Visit a control flow graph
    fn visit_control_flow_graph(&mut self, cfg: &ControlFlowGraph) {
        for (_, block) in &cfg.blocks {
            self.visit_basic_block(block);
        }
    }

    /// Visit a basic block
    fn visit_basic_block(&mut self, block: &BasicBlock) {
        for statement in &block.statements {
            self.visit_statement(statement);
        }
        self.visit_terminator(&block.terminator);
    }

    /// Visit a statement
    fn visit_statement(&mut self, statement: &MirStatement) {
        match statement {
            MirStatement::Assign {
                destination,
                source,
            } => {
                self.visit_place(destination);
                self.visit_rvalue(source);
            }
            MirStatement::CallVoid { func, args } => {
                self.visit_operand(func);
                for arg in args {
                    self.visit_operand(arg);
                }
            }
            MirStatement::Alloc { place, ty, init } => {
                self.visit_place(place);
                self.visit_type(ty);
                if let Some(init) = init {
                    self.visit_operand(init);
                }
            }
            MirStatement::Nop => {}
        }
    }

    /// Visit a place
    fn visit_place(&mut self, place: &Place) {
        for proj in &place.projection {
            self.visit_projection_elem(proj);
        }
    }

    /// Visit a projection element
    fn visit_projection_elem(&mut self, proj: &ProjectionElem) {
        match proj {
            ProjectionElem::Index(_) => {}
            _ => {}
        }
    }

    /// Visit a right-hand side value
    fn visit_rvalue(&mut self, rvalue: &Rvalue) {
        match rvalue {
            Rvalue::Use(operand) => {
                self.visit_operand(operand);
            }
            Rvalue::Ref(place) => {
                self.visit_place(place);
            }
            Rvalue::BinaryOp { left, right, .. } => {
                self.visit_operand(left);
                self.visit_operand(right);
            }
            Rvalue::UnaryOp { operand, .. } => {
                self.visit_operand(operand);
            }
            Rvalue::Call { func, args, .. } => {
                self.visit_operand(func);
                for arg in args {
                    self.visit_operand(arg);
                }
            }
            Rvalue::Aggregate { elements, .. } => {
                for elem in elements {
                    self.visit_operand(elem);
                }
            }
        }
    }

    /// Visit an operand
    fn visit_operand(&mut self, operand: &Operand) {
        match operand {
            Operand::Copy(place) | Operand::Move(place) => {
                self.visit_place(place);
            }
            Operand::Constant(constant) => {
                self.visit_constant(constant);
            }
        }
    }

    /// Visit a constant
    fn visit_constant(&mut self, constant: &Constant) {
        match constant {
            _ => {}
        }
    }

    /// Visit a terminator
    fn visit_terminator(&mut self, terminator: &MirTerminator) {
        match terminator {
            MirTerminator::Goto { .. } => {}
            MirTerminator::Switch {
                discriminant,
                targets,
                ..
            } => {
                self.visit_operand(discriminant);
                for (value, _) in targets {
                    self.visit_switch_value(value);
                }
            }
            MirTerminator::Return { value } => {
                if let Some(value) = value {
                    self.visit_operand(value);
                }
            }
            MirTerminator::Unreachable => {}
            MirTerminator::Call {
                func,
                args,
                destination,
                ..
            } => {
                self.visit_operand(func);
                for arg in args {
                    self.visit_operand(arg);
                }
                if let Some(dest) = destination {
                    self.visit_place(dest);
                }
            }
            MirTerminator::Match {
                scrutinee, arms, ..
            } => {
                self.visit_operand(scrutinee);
                for arm in arms {
                    self.visit_match_arm(arm);
                }
            }
        }
    }

    /// Visit a switch value
    fn visit_switch_value(&mut self, value: &SwitchValue) {
        match value {
            _ => {}
        }
    }

    /// Visit a match arm
    fn visit_match_arm(&mut self, arm: &MatchArm) {
        self.visit_match_pattern(&arm.pattern);
        if let Some(guard) = &arm.guard {
            self.visit_operand(guard);
        }
    }

    /// Visit a match pattern
    fn visit_match_pattern(&mut self, pattern: &MatchPattern) {
        match pattern {
            MatchPattern::Constant(value) => {
                self.visit_switch_value(value);
            }
            MatchPattern::Tuple(patterns) => {
                for pattern in patterns {
                    self.visit_match_pattern(pattern);
                }
            }
            MatchPattern::Struct { fields, .. } => {
                for (_, pattern) in fields {
                    self.visit_match_pattern(pattern);
                }
            }
            MatchPattern::Variant { fields, .. } => {
                for pattern in fields {
                    self.visit_match_pattern(pattern);
                }
            }
            MatchPattern::Binding { subpattern, .. } => {
                if let Some(subpattern) = subpattern {
                    self.visit_match_pattern(subpattern);
                }
            }
            MatchPattern::Wildcard => {}
            MatchPattern::Range { start, end } => {
                self.visit_switch_value(start);
                self.visit_switch_value(end);
            }
            MatchPattern::Or(patterns) => {
                for pattern in patterns {
                    self.visit_match_pattern(pattern);
                }
            }
        }
    }
}

/// A visitor for counting various MIR elements
#[derive(Debug, Default, Clone)]
pub struct MirCounter {
    /// Number of functions
    pub functions: usize,
    /// Number of basic blocks
    pub blocks: usize,
    /// Number of statements
    pub statements: usize,
    /// Number of terminators
    pub terminators: usize,
    /// Number of local variables
    pub locals: usize,
}

impl MirVisitor for MirCounter {
    fn visit_function(&mut self, function: &MirFunction) {
        self.functions += 1;
        self.locals += function.locals.len();
        self.visit_control_flow_graph(&function.body);
    }

    fn visit_basic_block(&mut self, block: &BasicBlock) {
        self.blocks += 1;
        self.statements += block.statements.len();
        self.terminators += 1;
        
        // Continue visiting the block's contents
        for statement in &block.statements {
            self.visit_statement(statement);
        }
        self.visit_terminator(&block.terminator);
    }
}

impl PartialEq for MirCounter {
    fn eq(&self, other: &Self) -> bool {
        self.functions == other.functions
            && self.blocks == other.blocks
            && self.statements == other.statements
            && self.terminators == other.terminators
            && self.locals == other.locals
    }
}

impl Eq for MirCounter {}

impl std::hash::Hash for MirCounter {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.functions.hash(state);
        self.blocks.hash(state);
        self.statements.hash(state);
        self.terminators.hash(state);
        self.locals.hash(state);
    }
} 