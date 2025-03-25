//! Tests for function type inference and symbols

use parallax_lang::ast::{self, Ident, Span, ExprKind, Type, TypeKind};
use crate::{
    context::{Ty, ConcreteTy, Constraint, TyVid},
    db::TypeContextOps,
    error::TypeError,
    tests::mocks::MockContext, // Use shared mock
};

// Helper function to create an integer literal expression
fn int_literal(value: i64) -> ast::Expr {
    ast::Expr {
        span: Span { start: 0, end: 0 },
        kind: ExprKind::Literal(ast::Literal::Int(value)),
    }
}

// Helper to create function calls
fn call_expr(func_name: &str, args: Vec<ast::Expr>) -> ast::Expr {
    let arguments = args.into_iter()
        .map(|expr| ast::expr::Argument {
            name: None,
            value: expr,
            span: ast::Span { start: 0, end: 0 },
        })
        .collect();

    ast::Expr {
        span: ast::Span { start: 0, end: 0 },
        kind: ast::ExprKind::Call {
            func: Box::new(path_expr(func_name)),
            args: arguments,
        },
    }
}

// Helper function to create a path expression
fn path_expr(name: &str) -> ast::Expr {
    ast::Expr {
        span: Span { start: 0, end: 0 },
        kind: ExprKind::Path(vec![Ident(name.to_string())]),
    }
}

// Helper function to create a simple concrete type AST node
fn concrete_type(name: &str) -> Type {
    Type {
        span: Span { start: 0, end: 0 },
        kind: TypeKind::Path(vec![Ident(name.to_string())]),
    }
}

// Helper function to infer an expression type using the MockContext
// Note: This replicates some logic from `infer.rs` for isolated testing.
fn infer_expr_type(expr: &ast::Expr, ctx: &mut MockContext) -> Ty {
    match &expr.kind {
        ExprKind::Literal(ast::Literal::Int(_)) => {
            let ty = Ty::Concrete(ConcreteTy::Int);
            ctx.record_type(expr.span, ty.clone());
            ty
        },
        ExprKind::Literal(ast::Literal::Bool(_)) => {
            let ty = Ty::Concrete(ConcreteTy::Bool);
            ctx.record_type(expr.span, ty.clone());
            ty
        },
        ExprKind::Literal(ast::Literal::String(_)) => {
            let ty = Ty::Concrete(ConcreteTy::String);
            ctx.record_type(expr.span, ty.clone());
            ty
        },
        ExprKind::Path(path) => {
            if let Some(ty) = ctx.lookup_symbol(path) {
                ctx.record_type(expr.span, ty.clone());
                ty
            } else {
                let ty_var = ctx.new_ty_var();
                ctx.error(TypeError::Internal(
                    format!("Unresolved symbol: {}",
                            if !path.is_empty() { &path[0].0 } else { "" })
                ));
                ctx.record_type(expr.span, ty_var.clone());
                ty_var
            }
        },
        ExprKind::Call { func, args } => {
            let func_ty = infer_expr_type(func, ctx);
            let arg_tys = args.iter()
                .map(|arg| infer_expr_type(&arg.value, ctx))
                .collect::<Vec<_>>();

            match func_ty {
                Ty::Function { params, ret } => {
                    if params.len() != arg_tys.len() {
                        ctx.error(TypeError::ArgumentCountMismatch {
                            expected: params.len(),
                            actual: arg_tys.len(),
                            span: expr.span,
                        });
                    } else {
                        for (param_ty, arg_ty) in params.iter().zip(arg_tys.iter()) {
                            ctx.add_constraint(Constraint::Eq(
                                arg_ty.clone(),
                                param_ty.clone(),
                            ));
                        }
                    }
                    let return_ty = (*ret).clone();
                    ctx.record_type(expr.span, return_ty.clone());
                    return_ty
                },
                _ => {
                    let ty_var = ctx.new_ty_var();
                    ctx.error(TypeError::Internal(
                        format!("Type {:?} is not callable", func_ty)
                    ));
                    ctx.record_type(expr.span, ty_var.clone());
                    ty_var
                }
            }
        },
        _ => {
            let ty_var = ctx.new_ty_var();
            ctx.error(TypeError::Internal(
                format!("Unhandled expression kind: {:?}", expr.kind),
            ));
            ctx.record_type(expr.span, ty_var.clone());
            ty_var
        }
    }
}

#[test]
fn test_function_call_inference() {
    let mut mock_ctx = MockContext::new(); // Use shared mock

    // Register a function "add" with type (Int, Int) -> Int
    let add_type = Ty::Function {
        params: vec![
            Ty::Concrete(ConcreteTy::Int),
            Ty::Concrete(ConcreteTy::Int),
        ],
        ret: Box::new(Ty::Concrete(ConcreteTy::Int)),
    };
    // Use the mock context's symbol registration
    mock_ctx.symbol_resolver.register_function("add", 
        vec![Ty::Concrete(ConcreteTy::Int), Ty::Concrete(ConcreteTy::Int)], 
        Ty::Concrete(ConcreteTy::Int)
    );
    mock_ctx.register_symbol("add", add_type);

    let call = call_expr("add", vec![
        int_literal(1),
        int_literal(2),
    ]);

    let call_ty = infer_expr_type(&call, &mut mock_ctx);

    assert!(matches!(call_ty, Ty::Concrete(ConcreteTy::Int)));
    assert!(mock_ctx.errors.is_empty());
}

#[test]
fn test_function_call_type_mismatch() {
    let mut mock_ctx = MockContext::new(); // Use shared mock

    // Register a function "max" with type (Int, Int) -> Int
    let max_type = Ty::Function {
        params: vec![
            Ty::Concrete(ConcreteTy::Int),
            Ty::Concrete(ConcreteTy::Int),
        ],
        ret: Box::new(Ty::Concrete(ConcreteTy::Int)),
    };
    mock_ctx.symbol_resolver.register_function("max", 
        vec![Ty::Concrete(ConcreteTy::Int), Ty::Concrete(ConcreteTy::Int)], 
        Ty::Concrete(ConcreteTy::Int)
    );
    mock_ctx.register_symbol("max", max_type);

    let call = call_expr("max", vec![
        int_literal(1),
        ast::Expr {
            span: Span { start: 0, end: 0 },
            kind: ExprKind::Literal(ast::Literal::Bool(true)),
        },
    ]);

    let call_ty = infer_expr_type(&call, &mut mock_ctx);

    assert!(matches!(call_ty, Ty::Concrete(ConcreteTy::Int)));
    assert!(!mock_ctx.constraints.is_empty());

    let has_constraint = mock_ctx.constraints.iter().any(|c| {
        match c {
            Constraint::Eq(left, right) => {
                matches!(left, Ty::Concrete(ConcreteTy::Bool)) &&
                matches!(right, Ty::Concrete(ConcreteTy::Int)) ||
                matches!(left, Ty::Concrete(ConcreteTy::Int)) &&
                matches!(right, Ty::Concrete(ConcreteTy::Bool))
            },
            _ => false,
        }
    });

    assert!(has_constraint, "Expected constraint between Bool and Int");
}

#[test]
fn test_variable_lookup() {
    let mut mock_ctx = MockContext::new(); // Use shared mock

    // Register a variable "counter" with type Int
    mock_ctx.register_symbol("counter", Ty::Concrete(ConcreteTy::Int));

    let path = path_expr("counter");
    let path_ty = infer_expr_type(&path, &mut mock_ctx);

    assert!(matches!(path_ty, Ty::Concrete(ConcreteTy::Int)));
}

#[test]
fn test_unresolved_symbol() {
    let mut mock_ctx = MockContext::new(); // Use shared mock

    let path = path_expr("undefined_var");
    let path_ty = infer_expr_type(&path, &mut mock_ctx);

    assert!(matches!(path_ty, Ty::Var(_)));
    assert!(!mock_ctx.errors.is_empty());

    let has_unresolved_error = mock_ctx.errors.iter().any(|err| {
        match err {
            TypeError::Internal(msg) => msg.contains("Unresolved symbol"),
            _ => false,
        }
    });

    assert!(has_unresolved_error, "Expected an error about unresolved symbol");
}

// Additional tests that would be implemented:
// - Generic function instantiation
// - Higher-order functions
// - Module resolution for imported symbols
// - Method resolution 