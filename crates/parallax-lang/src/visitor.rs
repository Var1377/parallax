use crate::ast::{
    Expr, ExprKind, Pattern, PatternKind,
    Type, TypeKind, items::{Function, Parameter, Item, ItemKind}
};
use crate::ast::expr::Argument;

/// Trait for implementing the visitor pattern over the AST
pub trait Visitor: Sized {
    type Error;

    // Expression nodes
    fn visit_expr(&mut self, expr: &Expr) -> Result<(), Self::Error> {
        walk_expr(self, expr)
    }

    fn visit_literal(&mut self, _lit: &crate::ast::common::Literal) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_path(&mut self, _path: &[crate::ast::common::Ident]) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_block(&mut self, exprs: &[Expr]) -> Result<(), Self::Error> {
        for expr in exprs {
            self.visit_expr(expr)?;
        }
        Ok(())
    }

    fn visit_call(&mut self, func: &Expr, args: &[Argument]) -> Result<(), Self::Error> {
        self.visit_expr(func)?;
        for arg in args {
            self.visit_expr(&arg.value)?;
        }
        Ok(())
    }

    fn visit_binary(&mut self, left: &Expr, _op: &crate::ast::expr::BinaryOp, right: &Expr) -> Result<(), Self::Error> {
        self.visit_expr(left)?;
        self.visit_expr(right)?;
        Ok(())
    }

    // Item nodes
    fn visit_item(&mut self, item: &Item) -> Result<(), Self::Error> {
        walk_item(self, item)
    }

    fn visit_function(&mut self, func: &Function) -> Result<(), Self::Error> {
        for param in &func.params {
            self.visit_parameter(param)?;
        }
        if let Some(ret_ty) = &func.return_type {
            self.visit_type(ret_ty)?;
        }
        self.visit_expr(&func.body)?;
        Ok(())
    }

    // Pattern nodes
    fn visit_pattern(&mut self, pattern: &Pattern) -> Result<(), Self::Error> {
        walk_pattern(self, pattern)
    }

    fn visit_identifier_pattern(&mut self, _ident: &crate::ast::common::Ident) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_struct_pattern(&mut self, _path: &[crate::ast::common::Ident], fields: &[crate::ast::pattern::PatternField]) -> Result<(), Self::Error> {
        for field in fields {
            if let Some(pat) = &field.pattern {
                self.visit_pattern(pat)?;
            }
        }
        Ok(())
    }

    // Type nodes
    fn visit_type(&mut self, ty: &Type) -> Result<(), Self::Error> {
        walk_type(self, ty)
    }

    fn visit_parameter(&mut self, param: &Parameter) -> Result<(), Self::Error> {
        self.visit_pattern(&param.pattern)?;
        if let Some(ty) = &param.ty {
            self.visit_type(ty)?;
        }
        if let Some(default) = &param.default_value {
            self.visit_expr(default)?;
        }
        Ok(())
    }
}

// Helper functions to traverse the AST
pub fn walk_expr<V: Visitor>(visitor: &mut V, expr: &Expr) -> Result<(), V::Error> {
    match &expr.kind {
        ExprKind::Literal(lit) => visitor.visit_literal(lit),
        ExprKind::Path(path) => visitor.visit_path(path),
        ExprKind::Block(exprs) => visitor.visit_block(&exprs),
        ExprKind::Call { func, args } => visitor.visit_call(func, args),
        ExprKind::Binary { left, op, right } => visitor.visit_binary(left, op, right),
        ExprKind::Unary { op: _, expr } => visitor.visit_expr(expr),
        ExprKind::If { condition, then_branch, else_branch } => {
            visitor.visit_expr(condition)?;
            visitor.visit_expr(then_branch)?;
            if let Some(else_branch) = else_branch {
                visitor.visit_expr(else_branch)?;
            }
            Ok(())
        },
        ExprKind::Field { object, name: _ } => visitor.visit_expr(object),
        ExprKind::Array(elements) => {
            for element in elements {
                visitor.visit_expr(element)?;
            }
            Ok(())
        },
        ExprKind::Tuple(elements) => {
            for element in elements {
                visitor.visit_expr(element)?;
            }
            Ok(())
        },
        ExprKind::Map(entries) => {
            for (key, value) in entries {
                visitor.visit_expr(key)?;
                visitor.visit_expr(value)?;
            }
            Ok(())
        },
        ExprKind::HashSet(elements) => {
            for element in elements {
                visitor.visit_expr(element)?;
            }
            Ok(())
        },
        ExprKind::Let { pattern, type_ann, value } => {
            visitor.visit_pattern(pattern)?;
            if let Some(ty) = type_ann {
                visitor.visit_type(ty)?;
            }
            visitor.visit_expr(value)?;
            Ok(())
        },
        ExprKind::Struct { path, fields, base } => {
            visitor.visit_path(path)?;
            for (_, value) in fields {
                visitor.visit_expr(value)?;
            }
            if let Some(base) = base {
                visitor.visit_expr(base)?;
            }
            Ok(())
        },
        ExprKind::Match { scrutinee, arms } => {
            visitor.visit_expr(scrutinee)?;
            for (pattern, expr) in arms {
                visitor.visit_pattern(pattern)?;
                visitor.visit_expr(expr)?;
            }
            Ok(())
        },
        ExprKind::Lambda { generic_params: _, params: _, body } => visitor.visit_expr(body),
        ExprKind::Paren(expr) => visitor.visit_expr(expr),
    }
}

pub fn walk_item<V: Visitor>(visitor: &mut V, item: &Item) -> Result<(), V::Error> {
    match &item.kind {
        ItemKind::Function(func) => visitor.visit_function(func),
        ItemKind::TypeDef(type_def) => visitor.visit_type(&type_def.ty),
        ItemKind::Enum(_) => Ok(()), // TODO: Implement enum visiting
        ItemKind::Struct(_) => Ok(()), // TODO: Implement struct visiting
        ItemKind::Trait(_) => Ok(()), // TODO: Implement trait visiting
        ItemKind::Impl(_) => Ok(()), // TODO: Implement impl visiting
        ItemKind::Module(_) => Ok(()), // TODO: Implement module visiting
        ItemKind::Use(_) => Ok(()), // TODO: Implement use visiting
    }
}

pub fn walk_pattern<V: Visitor>(visitor: &mut V, pattern: &Pattern) -> Result<(), V::Error> {
    match &pattern.kind {
        PatternKind::Identifier(ident) => visitor.visit_identifier_pattern(ident),
        PatternKind::Literal(lit) => visitor.visit_literal(lit),
        PatternKind::Constructor { path, args } => {
            visitor.visit_path(path)?;
            visitor.visit_pattern(args)
        },
        PatternKind::Array(patterns) => {
            for pat in patterns {
                visitor.visit_pattern(pat)?;
            }
            Ok(())
        },
        PatternKind::Tuple(patterns) => {
            for pat in patterns {
                visitor.visit_pattern(pat)?;
            }
            Ok(())
        },
        PatternKind::Struct { path, fields } => visitor.visit_struct_pattern(path, fields),
        PatternKind::Rest => Ok(()),
        PatternKind::Or(left, right) => {
            visitor.visit_pattern(left)?;
            visitor.visit_pattern(right)
        },
        PatternKind::Wildcard => Ok(()),
    }
}

pub fn walk_type<V: Visitor>(visitor: &mut V, ty: &Type) -> Result<(), V::Error> {
    match &ty.kind {
        TypeKind::Path(path) => visitor.visit_path(path),
        TypeKind::Function(param_ty, ret_ty) => {
            visitor.visit_type(param_ty)?;
            visitor.visit_type(ret_ty)?;
            Ok(())
        }
        TypeKind::Tuple(types) => {
            for ty in types {
                visitor.visit_type(ty)?;
            }
            Ok(())
        }
        TypeKind::Array(elem_ty, _) => visitor.visit_type(elem_ty),
        TypeKind::KindApp(ty, tys) => {
            visitor.visit_type(ty)?;
            tys.iter().map(|ty| visitor.visit_type(ty)).collect::<Result<(), _>>()?;
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::common::{Ident, Span, Literal};
    use crate::error::ParallaxError;

    // Example visitor that collects all function names
    struct FunctionCollector {
        functions: Vec<String>,
    }

    impl FunctionCollector {
        fn new() -> Self {
            Self {
                functions: Vec::new(),
            }
        }
    }

    impl Visitor for FunctionCollector {
        type Error = ParallaxError;

        fn visit_function(&mut self, func: &Function) -> Result<(), Self::Error> {
            self.functions.push(func.name.0.clone());
            self.visit_expr(&func.body)
        }
    }

    #[test]
    fn test_function_collector() {
        let func1 = Function {
            name: Ident("add".to_string()),
            generic_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: Box::new(Expr::new(
                ExprKind::Block(vec![]),
                Span { start: 0, end: 0 },
            )),
            span: Span { start: 0, end: 0 },
        };

        let func2 = Function {
            name: Ident("subtract".to_string()),
            generic_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: Box::new(Expr::new(
                ExprKind::Block(vec![]),
                Span { start: 0, end: 0 },
            )),
            span: Span { start: 0, end: 0 },
        };

        let items = vec![
            Item {
                kind: ItemKind::Function(func1),
                visibility: false,
                span: Span { start: 0, end: 0 },
            },
            Item {
                kind: ItemKind::Function(func2),
                visibility: false,
                span: Span { start: 0, end: 0 },
            },
        ];

        let mut collector = FunctionCollector::new();
        for item in &items {
            collector.visit_item(item).unwrap();
        }

        assert_eq!(collector.functions, vec!["add", "subtract"]);
    }

    // Example visitor that counts expressions
    struct ExpressionCounter {
        count: usize,
    }

    impl ExpressionCounter {
        fn new() -> Self {
            Self { count: 0 }
        }
    }

    impl Visitor for ExpressionCounter {
        type Error = ParallaxError;

        fn visit_expr(&mut self, expr: &Expr) -> Result<(), Self::Error> {
            self.count += 1;
            walk_expr(self, expr)
        }
    }

    #[test]
    fn test_expression_counter() {
        let expr = Expr::new(
            ExprKind::Binary {
                left: Box::new(Expr::new(
                    ExprKind::Literal(Literal::Int(1)),
                    Span { start: 0, end: 0 },
                )),
                op: crate::ast::expr::BinaryOp::Add,
                right: Box::new(Expr::new(
                    ExprKind::Literal(Literal::Int(2)),
                    Span { start: 0, end: 0 },
                )),
            },
            Span { start: 0, end: 0 },
        );

        let mut counter = ExpressionCounter::new();
        counter.visit_expr(&expr).unwrap();

        assert_eq!(counter.count, 3);
    }
}