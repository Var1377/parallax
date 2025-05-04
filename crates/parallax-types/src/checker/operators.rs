// src/checker/operators.rs
//! Type checking pass 2: Checking operators (binary, unary) by desugaring to trait calls.

use super::TypeChecker;
use crate::error::{TypeError, TypeResult, display_type};
use crate::types::{Ty, TyKind, TypedExpr, PrimitiveType, TypedExprKind, TypedArgument};
use parallax_syntax::ast::{BinaryOp, UnaryOp};
use miette::SourceSpan;
use parallax_resolve::types::Symbol;

// --- Operator to Trait/Method Mapping ---

struct OpInfo {
    method_name: &'static str,
    // We might cache TraitId later if needed
}

fn get_binary_op_info(op: &BinaryOp) -> Option<OpInfo> {
    match op {
        BinaryOp::Add => Some(OpInfo { method_name: "add" }),
        BinaryOp::Sub => Some(OpInfo { method_name: "sub" }),
        BinaryOp::Mul => Some(OpInfo { method_name: "mul" }),
        BinaryOp::Div => Some(OpInfo { method_name: "div" }),
        BinaryOp::Rem => Some(OpInfo { method_name: "rem" }),
        BinaryOp::And | BinaryOp::Or => None, // Logical handled separately
        // Map bitwise ops to traits from stdlib/ops.plx
        BinaryOp::BitAnd => Some(OpInfo { method_name: "and" }),
        BinaryOp::BitOr => Some(OpInfo { method_name: "or" }),
        BinaryOp::BitXor => Some(OpInfo { method_name: "xor" }),
        BinaryOp::Shl => Some(OpInfo { method_name: "shl" }),
        BinaryOp::Shr => Some(OpInfo { method_name: "shr" }),
        // Comparison operators use Eq/Ord traits (from cmp.plx)
        BinaryOp::Eq => Some(OpInfo { method_name: "eq" }),
        BinaryOp::Ne => Some(OpInfo { method_name: "ne" }),
        BinaryOp::Lt => Some(OpInfo { method_name: "lt" }),
        BinaryOp::Le => Some(OpInfo { method_name: "le" }),
        BinaryOp::Gt => Some(OpInfo { method_name: "gt" }),
        BinaryOp::Ge => Some(OpInfo { method_name: "ge" }),
        // Arrow is likely special, not a standard overloadable operator
        BinaryOp::Arrow => None,
    }
}

fn get_unary_op_info(op: &UnaryOp) -> Option<OpInfo> {
    match op {
        UnaryOp::Neg => Some(OpInfo { method_name: "neg" }),
        UnaryOp::Not => Some(OpInfo { method_name: "not" }),
        // UnaryOp::Deref => Some(OpInfo { trait_name: "Deref", method_name: "deref" }), // Needs Deref trait
        // UnaryOp::AddrOf => None, // Address-of (&) is typically a built-in operation
        // UnaryOp::AddrOfMut => None, // Address-of (&mut) is typically a built-in operation
        // _ => None, // This is unreachable as Neg and Not are the only variants handled
    }
}

// --- Operator Checking ---

pub(crate) fn check_binary_op(
    checker: &mut TypeChecker,
    op: &BinaryOp,
    lhs: &TypedExpr,
    rhs: &TypedExpr,
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {

    match op {
        // Logical Operators (Short-circuiting, not traits)
        BinaryOp::And | BinaryOp::Or => {
            let bool_ty = Ty::new(TyKind::Primitive(PrimitiveType::Bool));
            if !checker.unify(&lhs.ty, &bool_ty, lhs.span) {
                return Ok((TypedExprKind::Error, Ty::new(TyKind::Error)));
            }
            if !checker.unify(&rhs.ty, &bool_ty, rhs.span) {
                 return Ok((TypedExprKind::Error, Ty::new(TyKind::Error)));
            }
            // Construct specific TypedExprKind for logical ops
            let kind = match op {
                BinaryOp::And => TypedExprKind::LogicalAnd {
                    left: Box::new(lhs.clone()),
                    right: Box::new(rhs.clone()),
                },
                BinaryOp::Or => TypedExprKind::LogicalOr {
                    left: Box::new(lhs.clone()),
                    right: Box::new(rhs.clone()),
                },
                _ => unreachable!(),
            };
            Ok((kind, bool_ty))
        }
        // Other Binary Operators (Desugar to Trait Calls)
        _ => {
            if let Some(op_info) = get_binary_op_info(op) {
                // Desugar `lhs op rhs` into `lhs.method_name(rhs)`
                
                // 1. Represent the method access `lhs.method_name`
                // We need a placeholder symbol here as invocation will resolve the actual method.
                let method_access_expr = TypedExpr {
                    // Simulate field access to trigger method call logic in check_invocation
                    kind: TypedExprKind::Field {
                        object: Box::new(lhs.clone()),
                        field_name: op_info.method_name.to_string(),
                        // This symbol is just a placeholder for Field kind, 
                        // check_invocation uses find_candidates based on name & object type.
                        field_symbol: Symbol::new(u32::MAX), // Placeholder symbol
                    },
                    // The type here is not strictly correct before resolution, but check_invocation 
                    // primarily uses the object's type.
                    ty: Ty::new(TyKind::Error), // Placeholder type
                    span: lhs.span, // Use lhs span as approximate location of method lookup
                };

                // 2. Prepare the argument (rhs)
                let typed_args = vec![TypedArgument {
                    name: None, // Positional argument
                    value: rhs.clone(),
                    span: rhs.span,
                }];
                let arg_tys = vec![rhs.ty.clone()];

                println!("Desugaring binary op '{}' to method call '{}.{}'", format!("{:?}", op), display_type(&lhs.ty), op_info.method_name);

                // 3. Call invocation checker with the simulated method access
                match super::invocation::check_invocation(checker, &method_access_expr, typed_args, arg_tys, span) {
                    Ok((kind, ty)) => {
                        // Return the actual Call kind produced by check_invocation.
                        Ok((kind, ty))
                    }
                    Err(e) => {
                        // Report as InternalError wrapping the source error
                        checker.report_error(TypeError::InternalError {
                             message: format!(
                                 "Operator desugaring failed for \'{}\' (method '{}') on type {}: {}",
                                 format!("{:?}", op),
                                 op_info.method_name,
                                 display_type(&lhs.ty),
                                 e // Include the source error message
                             ),
                             span: Some(span),
                         });
                        Ok((TypedExprKind::Error, Ty::new(TyKind::Error)))
                    }
                }
            } else {
                 Err(TypeError::UnsupportedOperator { op: format!("{:?}", op), span })
            }
        }
    }
}

pub(crate) fn check_unary_op(
    checker: &mut TypeChecker,
    op: &UnaryOp,
    operand: &TypedExpr,
    span: SourceSpan,
) -> TypeResult<(TypedExprKind, Ty)> {
    if let Some(op_info) = get_unary_op_info(op) {
        // Desugar `op operand` into `operand.method_name()`

        // <<< ADD DEBUG PRINT >>>
        println!(
            "[check_unary_op] operand type for '{:?}': {}",
            op,
            display_type(&operand.ty)
        );

        // 1. Represent the method access `operand.method_name`
        let method_access_expr = TypedExpr {
            kind: TypedExprKind::Field {
                object: Box::new(operand.clone()),
                field_name: op_info.method_name.to_string(),
                field_symbol: Symbol::new(u32::MAX), // Placeholder symbol
            },
            ty: Ty::new(TyKind::Error), // Placeholder type
            span: operand.span, // Use operand span
        };

        // 2. Prepare empty argument list
        let typed_args = vec![];
        let arg_tys = vec![];

        println!("Desugaring unary op '{}' to method call '{}.{}'", format!("{:?}", op), display_type(&operand.ty), op_info.method_name);

        // 3. Call invocation checker
        match super::invocation::check_invocation(checker, &method_access_expr, typed_args, arg_tys, span) {
            Ok((kind, ty)) => {
                // Return the actual Call kind produced by check_invocation.
                Ok((kind, ty))
            }
            Err(e) => {
                checker.report_error(TypeError::InternalError {
                     message: format!(
                         "Operator desugaring failed for \'{}\' (method '{}') on type {}: {}",
                         format!("{:?}", op),
                         op_info.method_name,
                         display_type(&operand.ty),
                         e
                     ),
                     span: Some(span),
                 });
                Ok((TypedExprKind::Error, Ty::new(TyKind::Error)))
            }
        }
    } else {
        Err(TypeError::UnsupportedOperator { op: format!("{:?}", op), span })
    }
} 