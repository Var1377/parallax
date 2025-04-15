use miette::SourceSpan;
use std::sync::Arc;
// use parallax_resolve::types::Symbol; // Removed
use parallax_syntax::ast::expr::{BinaryOp as AstBinaryOp, UnaryOp as AstUnaryOp};

use crate::{
    // error::{TypeError, TypeResult, display_type}, // Removed display_type
    error::{TypeError, TypeResult},
    // inference::{Substitution, self}, // Removed self
    inference::Substitution,
    types::{Ty, TyKind, PrimitiveType, TypedExpr, TypedExprKind, TraitRef as CheckerTraitRef, TypedArgument, FunctionSignature},
    typecheck::TypeChecker,
    // traits::{ADD_TRAIT_ID, SUB_TRAIT_ID, MUL_TRAIT_ID, DIV_TRAIT_ID, REM_TRAIT_ID, // Removed all trait IDs
    //          BITAND_TRAIT_ID, BITOR_TRAIT_ID, BITXOR_TRAIT_ID, SHL_TRAIT_ID, SHR_TRAIT_ID,
    //          EQ_TRAIT_ID, PARTIAL_ORD_TRAIT_ID, NEG_TRAIT_ID, NOT_TRAIT_ID, 
    //          TraitId, output_assoc_type_symbol},
    traits::{TraitId, output_assoc_type_symbol},
};

// TODO: Remove if truly unused
/*
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Shl,
    Shr,
}

impl ToString for BinaryOp {
    fn to_string(&self) -> String {
        match self {
            BinaryOp::Add => "+".to_string(),
            BinaryOp::Sub => "-".to_string(),
            BinaryOp::Mul => "*".to_string(),
            BinaryOp::Div => "/".to_string(),
            BinaryOp::Mod => "%".to_string(),
            BinaryOp::Eq => "==".to_string(),
            BinaryOp::Ne => "!=".to_string(),
            BinaryOp::Lt => "<".to_string(),
            BinaryOp::Le => "<=".to_string(),
            BinaryOp::Gt => ">".to_string(),
            BinaryOp::Ge => ">=".to_string(),
            BinaryOp::LogicalAnd => "&&".to_string(),
            BinaryOp::LogicalOr => "||".to_string(),
            BinaryOp::BitwiseAnd => "&".to_string(),
            BinaryOp::BitwiseOr => "|".to_string(),
            BinaryOp::BitwiseXor => "^".to_string(),
            BinaryOp::Shl => "<<".to_string(),
            BinaryOp::Shr => ">>".to_string(),
        }
    }
}
*/

// Define UnaryOp locally instead of importing from parallax_hir
// TODO: Remove if truly unused
/*
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum UnaryOp {
    Neg,
    Not,
}

impl ToString for UnaryOp {
    fn to_string(&self) -> String {
        match self {
            UnaryOp::Neg => "-".to_string(),
            UnaryOp::Not => "!".to_string(),
        }
    }
}
*/

// Add helper methods for PrimitiveType
impl PrimitiveType {
    pub fn is_numeric(&self) -> bool {
        match self {
            PrimitiveType::I8
            | PrimitiveType::I16
            | PrimitiveType::I32
            | PrimitiveType::I64
            | PrimitiveType::I128
            | PrimitiveType::U8
            | PrimitiveType::U16
            | PrimitiveType::U32
            | PrimitiveType::U64
            | PrimitiveType::U128
            | PrimitiveType::F32
            | PrimitiveType::F64
            | PrimitiveType::IntegerLiteral
            | PrimitiveType::FloatLiteral => true,
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            PrimitiveType::I8
            | PrimitiveType::I16
            | PrimitiveType::I32
            | PrimitiveType::I64
            | PrimitiveType::I128
            | PrimitiveType::U8
            | PrimitiveType::U16
            | PrimitiveType::U32
            | PrimitiveType::U64
            | PrimitiveType::U128
            | PrimitiveType::IntegerLiteral => true,
            _ => false,
        }
    }
}

// Convert errors to a cleaner format
impl TypeError {
    pub fn binary_type_mismatch(op: String, left: String, right: String, span: SourceSpan) -> Self {
        TypeError::InternalError {
            message: format!("Binary operation type mismatch: cannot apply '{}' to types '{}' and '{}'", op, left, right),
            span: Some(span),
        }
    }

    pub fn unary_type_mismatch(op: String, operand: String, span: SourceSpan) -> Self {
        TypeError::InternalError {
            message: format!("Unary operation type mismatch: cannot apply '{}' to type '{}'", op, operand),
            span: Some(span),
        }
    }
}

impl<'a> TypeChecker<'a> {
    // Helper methods for the TypeChecker
    // TODO: Remove if truly unused
    /*
    fn fresh_numeric_var(&mut self, span: SourceSpan) -> Ty {
        self.fresh_infer_var(span)
    }

    fn fresh_integer_var(&mut self, span: SourceSpan) -> Ty {
        self.fresh_infer_var(span)
    }
    
    // Mock implementation of attempt_unification
    fn attempt_unification<F, R>(&mut self, f: F) -> TypeResult<R>
    where
        F: FnOnce(&mut Self) -> TypeResult<R>,
    {
        let snapshot = self.inference_ctx.snapshot();
        
        match f(self) {
            Ok(result) => Ok(result),
            Err(err) => {
                self.inference_ctx.rollback(snapshot);
                Err(err)
            }
        }
    }

    // Helper method to promote numeric types
    fn promote_numeric_types(&self, left: &PrimitiveType, right: &PrimitiveType) -> Option<PrimitiveType> {
        match (left, right) {
            // Float promotion
            (PrimitiveType::F64, _) | (_, PrimitiveType::F64) => Some(PrimitiveType::F64),
            (PrimitiveType::F32, _) | (_, PrimitiveType::F32) => Some(PrimitiveType::F32),
            (PrimitiveType::FloatLiteral, _) | (_, PrimitiveType::FloatLiteral) => Some(PrimitiveType::F64),
            
            // Integer literals with specific types
            (PrimitiveType::IntegerLiteral, other) if other.is_integer() => Some(*other),
            (other, PrimitiveType::IntegerLiteral) if other.is_integer() => Some(*other),
            
            // Same type case
            (a, b) if a == b => Some(*a),
            
            // Default to i32 for integer literals
            (PrimitiveType::IntegerLiteral, PrimitiveType::IntegerLiteral) => Some(PrimitiveType::I32),
            
            // For different integer types, choose the wider one (simplified)
            _ => Some(PrimitiveType::I32),
        }
    }

    // Helper method to get the trait ID for a binary operator
    fn get_trait_id_for_binary_op(&self, op: &BinaryOp) -> Option<TraitId> {
        let trait_path: &str = match op {
            BinaryOp::Add => "std::ops::Add",
            BinaryOp::Sub => "std::ops::Sub",
            BinaryOp::Mul => "std::ops::Mul",
            BinaryOp::Div => "std::ops::Div",
            BinaryOp::Mod => "std::ops::Rem",
            BinaryOp::Eq => "std::cmp::PartialEq",
            BinaryOp::Ne => "std::cmp::PartialEq",
            BinaryOp::Lt => "std::cmp::PartialOrd",
            BinaryOp::Le => "std::cmp::PartialOrd",
            BinaryOp::Gt => "std::cmp::PartialOrd",
            BinaryOp::Ge => "std::cmp::PartialOrd",
            BinaryOp::BitwiseAnd => "std::ops::BitAnd",
            BinaryOp::BitwiseOr => "std::ops::BitOr",
            BinaryOp::BitwiseXor => "std::ops::BitXor",
            BinaryOp::Shl => "std::ops::Shl",
            BinaryOp::Shr => "std::ops::Shr",
            BinaryOp::LogicalAnd | BinaryOp::LogicalOr => return None,
        };

        // Iterate through the core_traits Vec to find the Symbol
        self._core_traits.iter() // Use _core_traits
            .find(|(path, _symbol)| path == trait_path)
            .and_then(|(_, symbol)| self.trait_repo.get_trait_id_by_symbol(*symbol))
            .or_else(|| {
                // Log an error or warning if a core trait symbol wasn't found
                // Use eprintln! or a proper logging mechanism if available
                eprintln!("Warning: Core trait symbol not found for path: {}", trait_path);
                None
            })
    }

    // Helper method to get the trait ID for a unary operator
    fn get_trait_id_for_unary_op(&self, op: &UnaryOp) -> Option<TraitId> {
        let trait_path: &str = match op {
            UnaryOp::Neg => "std::ops::Neg",
            UnaryOp::Not => "std::ops::Not",
        };

        // Iterate through the core_traits Vec to find the Symbol
        self._core_traits.iter() // Use _core_traits
            .find(|(path, _symbol)| path == trait_path)
            .and_then(|(_, symbol)| self.trait_repo.get_trait_id_by_symbol(*symbol))
            .or_else(|| {
                eprintln!("Warning: Core trait symbol not found for path: {}", trait_path);
                None
            })
    }

    // Helper method to get the method name for a binary operator
    fn get_method_name_for_binary_op(&self, op: &BinaryOp) -> Option<&'static str> {
        match op {
            BinaryOp::Add => Some("add"),
            BinaryOp::Sub => Some("sub"),
            BinaryOp::Mul => Some("mul"),
            BinaryOp::Div => Some("div"),
            BinaryOp::Mod => Some("rem"),
            BinaryOp::BitwiseAnd => Some("bitand"),
            BinaryOp::BitwiseOr => Some("bitor"),
            BinaryOp::BitwiseXor => Some("bitxor"),
            BinaryOp::Shl => Some("shl"),
            BinaryOp::Shr => Some("shr"),
            BinaryOp::Eq => Some("eq"),
            BinaryOp::Ne => Some("ne"),
            BinaryOp::Lt => Some("lt"),
            BinaryOp::Le => Some("le"),
            BinaryOp::Gt => Some("gt"),
            BinaryOp::Ge => Some("ge"),
            BinaryOp::LogicalAnd | BinaryOp::LogicalOr => None,
        }
    }

    // Helper method to get the method name for a unary operator
    fn get_method_name_for_unary_op(&self, op: &UnaryOp) -> Option<&'static str> {
        match op {
            UnaryOp::Neg => Some("neg"),
            UnaryOp::Not => Some("not"),
        }
    }

    // Helper to get function type from signature (Added)
    fn get_function_type_from_sig(&self, sig: &FunctionSignature) -> Ty {
        let param_tys = sig.params.iter().map(|p| p.ty.clone()).collect();
        let ret_ty = sig.return_type.clone();
        // Assuming Ty::new exists and handles Arc internally or directly
        Ty::new(TyKind::Function(param_tys, Arc::new(ret_ty)))
    }
    */

    // Moved pub(crate) check_binary/check_unary outside impl block
}

// Add the free functions expected by expressions.rs
// TODO: Remove if truly unused
/*
pub(crate) fn check_binary(
    checker: &mut TypeChecker,
    left: &TypedExpr,
    op: &AstBinaryOp,
    right: &TypedExpr,
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    // Convert the parallax_syntax::ast::expr::BinaryOp to our internal BinaryOp
    let internal_op = match op {
        AstBinaryOp::Add => BinaryOp::Add,
        AstBinaryOp::Sub => BinaryOp::Sub,
        AstBinaryOp::Mul => BinaryOp::Mul,
        AstBinaryOp::Div => BinaryOp::Div,
        AstBinaryOp::Rem => BinaryOp::Mod, // Map Rem to Mod
        AstBinaryOp::Eq => BinaryOp::Eq,
        AstBinaryOp::Ne => BinaryOp::Ne,
        AstBinaryOp::Lt => BinaryOp::Lt,
        AstBinaryOp::Le => BinaryOp::Le,
        AstBinaryOp::Gt => BinaryOp::Gt,
        AstBinaryOp::Ge => BinaryOp::Ge,
        AstBinaryOp::And => BinaryOp::LogicalAnd, // Map And to LogicalAnd
        AstBinaryOp::Or => BinaryOp::LogicalOr,   // Map Or to LogicalOr
        AstBinaryOp::BitAnd => BinaryOp::BitwiseAnd, // Map BitAnd to BitwiseAnd
        AstBinaryOp::BitOr => BinaryOp::BitwiseOr,   // Map BitOr to BitwiseOr
        AstBinaryOp::BitXor => BinaryOp::BitwiseXor, // Map BitXor to BitwiseXor
        AstBinaryOp::Shl => BinaryOp::Shl,
        AstBinaryOp::Shr => BinaryOp::Shr,
        AstBinaryOp::Arrow => {
            // Handle arrow operator - for now, just error
            return Err(TypeError::UnsupportedOperator {
                op: "->".to_string(),
                span,
            });
        }
    };

    // Call the method implementation - passing op reference correctly
    checker.check_binary(left, op, right, span)
}
*/

// TODO: Remove if truly unused
/*
pub(crate) fn check_unary(
    checker: &mut TypeChecker,
    op: &AstUnaryOp,
    operand: &TypedExpr,
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    // Convert the parallax_syntax::ast::expr::UnaryOp to our internal UnaryOp
    let internal_op = match op {
        AstUnaryOp::Neg => UnaryOp::Neg,
        AstUnaryOp::Not => UnaryOp::Not,
    };

    // Call the method implementation - passing op reference correctly
    checker.check_unary(op, operand, span)
}
*/
