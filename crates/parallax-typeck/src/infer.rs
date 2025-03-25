use parallax_lang::ast::{self, items, Expr, ExprKind};
use crate::{
    context::{TypeContext, Ty, ConcreteTy, Constraint},
    error::TypeError,
};

pub struct ConstraintGenerator<'db> {
    ctx: &'db mut TypeContext<'db>,
}

impl<'db> ConstraintGenerator<'db> {
    pub fn new(ctx: &'db mut TypeContext<'db>) -> Self {
        Self { ctx }
    }

    pub fn infer_crate(&mut self, krate: &items::Crate) -> Ty {
        for item in &krate.items {
            match &item.kind {
                ast::ItemKind::Function(func) => {
                    self.infer_function(func);
                }
                _ => todo!("infer other item kinds"),
            }
        }
        self.ctx.new_ty_var() // Return a placeholder type for now
    }

    fn infer_function(&mut self, func: &items::Function) -> Ty {
        // Infer parameter types
        let param_tys = func.params.iter()
            .map(|param| {
                if let Some(ty) = &param.ty {
                    self.resolve_type(ty)
                } else {
                    self.ctx.new_ty_var()
                }
            })
            .collect::<Vec<_>>();

        // Infer return type
        let ret_ty = if let Some(ty) = &func.return_type {
            self.resolve_type(ty)
        } else {
            self.ctx.new_ty_var()
        };

        // Infer body
        let body_ty = self.infer_expr(&func.body);

        // Add constraint that body type matches return type
        self.ctx.add_constraint(Constraint::Eq(body_ty, ret_ty.clone()));

        // Create function type
        Ty::Function {
            params: param_tys,
            ret: Box::new(ret_ty),
        }
    }

    fn infer_expr(&mut self, expr: &Expr) -> Ty {
        let ty = match &expr.kind {
            ExprKind::Literal(lit) => self.infer_literal(lit),
            ExprKind::Call { func, args } => self.infer_call(func, args),
            ExprKind::Binary { left, op, right } => self.infer_binary(left, *op, right),
            // Add other expression kinds as needed
            _ => {
                self.ctx.error(TypeError::Internal(
                    "unsupported expression kind".to_string(),
                ));
                self.ctx.new_ty_var()
            }
        };

        // Record the type for this expression
        self.ctx.record_type(expr.span, ty.clone());
        ty
    }

    fn infer_literal(&mut self, lit: &ast::Literal) -> Ty {
        match lit {
            ast::Literal::Int(_) => Ty::Concrete(ConcreteTy::Int),
            ast::Literal::Float(_) => Ty::Concrete(ConcreteTy::Float),
            ast::Literal::String(_) => Ty::Concrete(ConcreteTy::String),
            ast::Literal::Char(_) => Ty::Concrete(ConcreteTy::Char),
            ast::Literal::Bool(_) => Ty::Concrete(ConcreteTy::Bool),
        }
    }

    fn infer_call(&mut self, func: &Expr, args: &[ast::Argument]) -> Ty {
        let func_ty = self.infer_expr(func);
        let arg_tys = args.iter()
            .map(|arg| self.infer_expr(&arg.value))
            .collect::<Vec<_>>();

        let ret_ty = self.ctx.new_ty_var();

        // Add constraint that func_ty is a function type with matching args
        self.ctx.add_constraint(Constraint::Eq(
            func_ty,
            Ty::Function {
                params: arg_tys,
                ret: Box::new(ret_ty.clone()),
            },
        ));

        ret_ty
    }

    fn infer_binary(&mut self, left: &Expr, op: ast::BinaryOp, right: &Expr) -> Ty {
        let left_ty = self.infer_expr(left);
        let right_ty = self.infer_expr(right);

        match op {
            ast::BinaryOp::Add | ast::BinaryOp::Sub | ast::BinaryOp::Mul | ast::BinaryOp::Div => {
                // Numeric operators require numeric types
                self.ctx.add_constraint(Constraint::Eq(left_ty.clone(), right_ty));
                left_ty
            }
            ast::BinaryOp::Eq | ast::BinaryOp::Lt | ast::BinaryOp::Gt => {
                // Comparison operators require same type and return bool
                self.ctx.add_constraint(Constraint::Eq(left_ty, right_ty));
                Ty::Concrete(ConcreteTy::Bool)
            }
            ast::BinaryOp::And | ast::BinaryOp::Or => {
                // Logical operators require bool operands and return bool
                self.ctx.add_constraint(Constraint::Eq(
                    left_ty,
                    Ty::Concrete(ConcreteTy::Bool),
                ));
                self.ctx.add_constraint(Constraint::Eq(
                    right_ty,
                    Ty::Concrete(ConcreteTy::Bool),
                ));
                Ty::Concrete(ConcreteTy::Bool)
            }
            ast::BinaryOp::Arrow => {
                // Effect sequencing operator, returns the type of the right operand
                right_ty
            }
        }
    }

    fn resolve_type(&mut self, ty: &ast::Type) -> Ty {
        match &ty.kind {
            ast::TypeKind::Path(path) => {
                // For now, just create a named type. In practice, we'd resolve this
                // through the name resolution system
                Ty::Concrete(ConcreteTy::Named {
                    name: path.last().unwrap().0.clone(),
                    args: vec![],
                })
            }
            ast::TypeKind::Function(param_ty, ret_ty) => {
                Ty::Function {
                    params: vec![self.resolve_type(param_ty)],
                    ret: Box::new(self.resolve_type(ret_ty)),
                }
            }
            ast::TypeKind::Tuple(tys) => {
                Ty::Tuple(tys.iter().map(|ty| self.resolve_type(ty)).collect())
            }
            // Add other type kinds as needed
            _ => {
                self.ctx.error(TypeError::Internal(
                    "unsupported type kind".to_string(),
                ));
                self.ctx.new_ty_var()
            }
        }
    }
} 