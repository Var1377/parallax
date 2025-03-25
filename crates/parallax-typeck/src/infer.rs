use parallax_lang::ast::{self, items, Expr, ExprKind, common::Ident};
use crate::{
    context::{Ty, ConcreteTy, Constraint},
    error::TypeError,
    db::TypeContextOps,
};

/// Generate type constraints for the crate
pub struct ConstraintGenerator;

impl ConstraintGenerator {
    /// Create a new constraint generator
    pub fn new() -> Self {
        Self
    }

    /// Infer constraints for a single top-level item.
    pub fn infer_item<C: TypeContextOps>(&self, item: &ast::Item, ctx: &mut C) {
        match &item.kind {
            ast::ItemKind::Function(func) => {
                self.infer_function(func, ctx);
            }
            // TODO: Add cases for other item kinds (Struct, Enum, Impl, etc.)
            _ => {
                ctx.error(TypeError::Internal(
                    format!("Unsupported item kind during inference: {:?}", item.kind)
                ));
            }
        }
    }

    fn infer_function<C: TypeContextOps>(&self, func: &items::Function, ctx: &mut C) -> Ty {
        // Infer parameter types
        let param_tys = func.params.iter()
            .map(|param| {
                if let Some(ty) = &param.ty {
                    self.resolve_type(ty, ctx)
                } else {
                    ctx.new_ty_var() // No type annotation, use a type variable
                }
            })
            .collect::<Vec<_>>();
            
        // Infer return type
        let ret_ty = if let Some(ret_ty) = &func.return_type {
            self.resolve_type(ret_ty, ctx)
        } else {
            Ty::Concrete(ConcreteTy::Unit) // No return type, default to unit
        };
        
        // Infer the body type
        let body_ty = self.infer_expr(&func.body, ctx);
        
        // Add constraint that body type must be compatible with return type
        ctx.add_constraint(Constraint::Eq(body_ty.clone(), ret_ty.clone()));
        
        // Create and record function type
        let fn_ty = Ty::Function {
            params: param_tys,
            ret: Box::new(ret_ty.clone()),
        };
        
        ctx.record_type(func.span, fn_ty.clone());
        
        // Return the function type
        fn_ty
    }

    pub fn infer_expr<C: TypeContextOps>(&self, expr: &Expr, ctx: &mut C) -> Ty {
        let ty = match &expr.kind {
            ExprKind::Literal(lit) => self.infer_literal(lit),
            ExprKind::Call { func, args } => self.infer_call(func, args, ctx),
            ExprKind::Binary { left, op, right } => self.infer_binary(left, *op, right, ctx),
            ExprKind::Field { object, name } => self.infer_field_access(object, name, ctx),
            ExprKind::Struct { path, fields, base } => self.infer_struct_expr(path, fields, base.as_ref(), ctx),
            ExprKind::Match { scrutinee, arms } => self.infer_match(scrutinee, arms, ctx),
            ExprKind::Let { pattern, type_ann, value } => self.infer_let(pattern, type_ann.as_ref(), value, ctx),
            ExprKind::Lambda { generic_params, params, body } => self.infer_lambda(generic_params, params, body, ctx),
            ExprKind::Array(elements) => self.infer_array(elements, ctx),
            ExprKind::Tuple(elements) => self.infer_tuple(elements, ctx),
            ExprKind::Map(entries) => self.infer_map(entries, ctx),
            ExprKind::HashSet(elements) => self.infer_hashset(elements, ctx),
            ExprKind::If { condition, then_branch, else_branch } => 
                self.infer_if(condition, then_branch, else_branch.as_ref(), ctx),
            ExprKind::Path(path) => self.infer_path(path, ctx),
            ExprKind::Unary { op, expr: inner_expr } => self.infer_unary(op.clone(), inner_expr, ctx),
            ExprKind::Paren(inner) => self.infer_expr(inner, ctx),
            ExprKind::Block(statements) => self.infer_block(statements, ctx),
            #[allow(unreachable_patterns)]
            _ => {
                ctx.error(TypeError::Internal(
                    format!("unsupported expression kind: {:?}", expr.kind),
                ));
                ctx.new_ty_var()
            }
        };

        // Record the type for this expression
        ctx.record_type(expr.span, ty.clone());
        ty
    }

    fn infer_literal(&self, lit: &ast::Literal) -> Ty {
        match lit {
            ast::Literal::Int(_) => Ty::Concrete(ConcreteTy::Int),
            ast::Literal::Float(_) => Ty::Concrete(ConcreteTy::Float),
            ast::Literal::String(_) => Ty::Concrete(ConcreteTy::String),
            ast::Literal::Char(_) => Ty::Concrete(ConcreteTy::Char),
            ast::Literal::Bool(_) => Ty::Concrete(ConcreteTy::Bool),
        }
    }

    fn infer_call<C: TypeContextOps>(&self, func: &Expr, args: &[ast::expr::Argument], ctx: &mut C) -> Ty {
        // First, infer the type of the function
        let func_ty = match &func.kind {
            // If we're directly calling a path (like a function name),
            // look it up in the symbol table instead of inferring it
            ExprKind::Path(path) => {
                if let Some(ty) = ctx.lookup_symbol(path) {
                    println!("Found function symbol {:?} with type {:?}", path, ty);
                    // Get the return type, but don't return early - we need to check arg types
                    if let Ty::Function { ret, params, .. } = &ty {
                        // Create constraints for the parameters
                        let ret_clone = (**ret).clone();
                        let param_clone = params.clone();
                        
                        // Infer types for each argument
                        let arg_tys = args.iter()
                            .map(|arg| self.infer_expr(&arg.value, ctx))
                            .collect::<Vec<_>>();
                        
                        // Check that the number of arguments matches
                        if arg_tys.len() != param_clone.len() {
                            ctx.error(TypeError::ArgumentCountMismatch {
                                expected: param_clone.len(),
                                actual: arg_tys.len(),
                                span: func.span,
                            });
                        } else {
                            // Check each argument type against the parameter type
                            for (arg_ty, param_ty) in arg_tys.iter().zip(param_clone.iter()) {
                                ctx.add_constraint(Constraint::Eq(arg_ty.clone(), param_ty.clone()));
                            }
                        }
                        
                        return ret_clone;
                    }
                    ty
                } else {
                    // If not found, report an error and use a fresh type variable
                    println!("Function symbol not found: {:?}", path);
                    ctx.error(TypeError::UnresolvedType {
                        ty: format!("function {:?}", path),
                        span: func.span,
                    });
                    ctx.new_ty_var()
                }
            }
            // Otherwise, infer the function's type from the expression
            _ => {
                println!("Inferring function type for non-path expression");
                self.infer_expr(func, ctx)
            }
        };

        // Infer types for each argument
        let arg_tys = args.iter()
            .map(|arg| self.infer_expr(&arg.value, ctx))
            .collect::<Vec<_>>();

        let ret_ty = ctx.new_ty_var();

        // Add constraint that func_ty is a function type with matching args
        println!("Adding constraint: {:?} = Function{{ params: {:?}, ret: {:?} }}", func_ty, arg_tys, ret_ty);
        ctx.add_constraint(Constraint::Eq(
            func_ty,
            Ty::Function {
                params: arg_tys,
                ret: Box::new(ret_ty.clone()),
            },
        ));

        ret_ty
    }

    fn infer_binary<C: TypeContextOps>(&self, left: &Expr, op: ast::BinaryOp, right: &Expr, ctx: &mut C) -> Ty {
        let left_ty = self.infer_expr(left, ctx);
        let right_ty = self.infer_expr(right, ctx);

        match op {
            ast::BinaryOp::Add | ast::BinaryOp::Sub | ast::BinaryOp::Mul | ast::BinaryOp::Div => {
                // Numeric operators require numeric types
                ctx.add_constraint(Constraint::Eq(left_ty.clone(), right_ty));
                left_ty
            }
            ast::BinaryOp::Eq | ast::BinaryOp::Lt | ast::BinaryOp::Gt => {
                // Comparison operators require same type and return bool
                ctx.add_constraint(Constraint::Eq(left_ty, right_ty));
                Ty::Concrete(ConcreteTy::Bool)
            }
            ast::BinaryOp::And | ast::BinaryOp::Or => {
                // Logical operators require bool operands and return bool
                ctx.add_constraint(Constraint::Eq(
                    left_ty,
                    Ty::Concrete(ConcreteTy::Bool),
                ));
                ctx.add_constraint(Constraint::Eq(
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

    /// Infer the type of a type AST node
    fn resolve_type<C: TypeContextOps>(&self, ty: &ast::Type, ctx: &mut C) -> Ty {
        // Try to resolve the type using the context's resolver
        match ctx.resolve_type_from_ast(ty) {
            Ok(resolved) => return resolved,
            Err(_) => {
                // Fall back to our own resolution logic if the context can't resolve it
            }
        }
        
        // Simple fallback resolution
        match &ty.kind {
            ast::TypeKind::Path(path) => {
                if path.is_empty() {
                    ctx.error(TypeError::UnresolvedType {
                        ty: "empty path".to_string(),
                        span: ty.span,
                    });
                    return Ty::Error;
                }
                
                // Handle primitive types
                match path[0].0.as_str() {
                    "Int" => Ty::Concrete(ConcreteTy::Int),
                    "Float" => Ty::Concrete(ConcreteTy::Float),
                    "Bool" => Ty::Concrete(ConcreteTy::Bool),
                    "String" => Ty::Concrete(ConcreteTy::String),
                    "Char" => Ty::Concrete(ConcreteTy::Char),
                    "Unit" => Ty::Concrete(ConcreteTy::Unit),
                    _ => {
                        // Try to look up the symbol
                        if let Some(ty) = ctx.lookup_symbol(path) {
                            ty
                        } else {
                            // Create a named type
                            Ty::Concrete(ConcreteTy::Named {
                                name: path.last().unwrap().0.clone(),
                                args: vec![],
                            })
                        }
                    }
                }
            }
            ast::TypeKind::Function(param_boxed, ret_boxed) => {
                // Process parameter type
                let param_ty = self.resolve_type(param_boxed, ctx);
                
                // Process return type
                let ret_ty = self.resolve_type(ret_boxed, ctx);
                
                // Create function type
                Ty::Function {
                    params: vec![param_ty],
                    ret: Box::new(ret_ty),
                }
            }
            ast::TypeKind::Tuple(tuple_elements) => {
                // Process tuple element types
                let element_tys = tuple_elements.iter()
                    .map(|elem_ty| self.resolve_type(elem_ty, ctx))
                    .collect::<Vec<_>>();
                
                Ty::Tuple(element_tys)
            }
            ast::TypeKind::Array(element_boxed, _size) => {
                // Process array element type
                let element_ty = self.resolve_type(element_boxed, ctx);
                
                // Create array type
                create_array_type(element_ty)
            }
            _ => {
                ctx.error(TypeError::UnresolvedType {
                    ty: format!("{:?}", ty.kind),
                    span: ty.span,
                });
                Ty::Error
            }
        }
    }

    // Implement field access type checking
    fn infer_field_access<C: TypeContextOps>(&self, object: &Expr, field_name: &Ident, ctx: &mut C) -> Ty {
        // 1. Infer the type of the object
        let object_ty = self.infer_expr(object, ctx);
        
        // Check for empty field name first - this is an error regardless of object type
        if field_name.0.is_empty() {
            ctx.error(TypeError::FieldNotFound {
                field_name: "<empty>".to_string(), 
                ty: object_ty.clone(),
                span: object.span,
            });
            return ctx.new_ty_var();
        }
        
        // 2. Based on the object type, look up the field
        match &object_ty {
            // For named types (structs, enums), we need to look up the field in the definition
            Ty::Concrete(ConcreteTy::Named { name: struct_name, args: _ }) => {
                // Look up the struct definition to find field types
                
                // In a full implementation, we would:
                // 1. Use the SymbolResolver to get the struct definition
                // 2. Look up the field in the struct's fields
                // 3. Return the field's type
                
                // For now, we'll use a type variable as a placeholder
                // In the future, this would be replaced with actual field type lookup
                
                // Ideally, we would create a special type error if the field doesn't exist
                
                // Log that we're looking for a field on a struct
                println!("Looking for field {} on struct {}", field_name.0, struct_name);
                
                // Return a fresh type variable representing the field type
                // This will be unified later if constraints are added
                let field_ty = ctx.new_ty_var();
                
                // Record the field access relationship for later phases
                ctx.record_type(object.span, object_ty.clone());
                
                field_ty
            }
            
            // For tuples, field access is numeric (tuple.0, tuple.1, etc.)
            Ty::Tuple(elements) => {
                // Check if the field name is a numeric index
                if let Ok(index) = field_name.0.parse::<usize>() {
                    if index < elements.len() {
                        // Valid tuple field access - return the type at that index
                        elements[index].clone()
                    } else {
                        // Index out of bounds
                        ctx.error(TypeError::FieldNotFound {
                            field_name: field_name.0.clone(),
                            ty: object_ty.clone(),
                            span: object.span,
                        });
                        ctx.new_ty_var()
                    }
                } else {
                    // Non-numeric field access on tuple
                    ctx.error(TypeError::InvalidTupleField {
                        field_name: field_name.0.clone(), 
                        span: object.span,
                    });
                    ctx.new_ty_var()
                }
            }
            
            // Field access not supported on other types
            _ => {
                ctx.error(TypeError::FieldAccessOnNonStruct {
                    ty: object_ty.clone(),
                    span: object.span,
                });
                ctx.new_ty_var()
            }
        }
    }
    
    // Implement struct expression type checking
    fn infer_struct_expr<C: TypeContextOps>(
        &self,
        path: &[Ident],
        fields: &[(Ident, Expr)],
        base: Option<&Box<Expr>>,
        ctx: &mut C
    ) -> Ty {
        // 1. Look up the struct definition in the type environment
        if path.is_empty() {
            ctx.error(TypeError::Internal("Empty path in struct expression".to_string()));
            return ctx.new_ty_var();
        }
        
        // Extract the struct name from the path
        let struct_name = path.last().unwrap().0.clone();
        
        // 2. Infer types for fields
        let mut field_types = Vec::new();
        for (field_name, field_expr) in fields {
            let field_ty = self.infer_expr(field_expr, ctx);
            field_types.push((field_name.0.clone(), field_ty));
        }
        
        // 3. Handle base expression if present
        if let Some(base_expr) = base {
            let base_ty = self.infer_expr(base_expr, ctx);
            
            // Verify base is a struct of the same type
            match &base_ty {
                Ty::Concrete(ConcreteTy::Named { name, .. }) if name == &struct_name => {
                    // In a real implementation, we would:
                    // a. Check that the provided fields don't conflict with base
                    // b. Merge field types from base with provided fields
                }
                _ => {
                    ctx.error(TypeError::InvalidBaseStruct {
                        base_ty: base_ty.clone(),
                        struct_name: struct_name.clone(),
                        span: base_expr.span,
                    });
                }
            }
        }
        
        // 4. In a real implementation, we would:
        // a. Look up the struct definition from the resolver
        // b. Verify that all required fields are provided
        // c. Check that field types match the struct definition
        
        // Return a named type for the struct
                Ty::Concrete(ConcreteTy::Named {
            name: struct_name,
            args: Vec::new(), // No generic args for now
        })
    }

    // Implement match expression type checking
    fn infer_match<C: TypeContextOps>(&self, scrutinee: &Expr, arms: &[(ast::Pattern, Expr)], ctx: &mut C) -> Ty {
        // 1. Infer the type of the scrutinee (the value being matched)
        let scrutinee_ty = self.infer_expr(scrutinee, ctx);
        
        // 2. Infer the type of each arm
        let mut arm_tys = Vec::new();
        
        for (pattern, arm_expr) in arms {
            // Check that the pattern is compatible with the scrutinee type
            self.check_pattern(pattern, &scrutinee_ty, ctx);
            
            // Infer the type of the arm expression
            let arm_ty = self.infer_expr(arm_expr, ctx);
            arm_tys.push(arm_ty);
        }
        
        // 3. All arms must have the same type
        if arm_tys.is_empty() {
            // Empty match - should not happen in valid code
            ctx.error(TypeError::Internal("Match expression with no arms".to_string()));
            return ctx.new_ty_var();
        }
        
        let result_ty = arm_tys[0].clone();
        
        // Add constraints that all arms have the same type
        for arm_ty in &arm_tys[1..] {
            ctx.add_constraint(Constraint::Eq(result_ty.clone(), arm_ty.clone()));
        }
        
        result_ty
    }
    
    // Check that a pattern is compatible with the expected type
    fn check_pattern<C: TypeContextOps>(&self, pattern: &ast::Pattern, expected_ty: &Ty, ctx: &mut C) -> Ty {
        match &pattern.kind {
            // Variable binding patterns (like `x` in `let x = 42;`)
            ast::PatternKind::Identifier(_ident) => {
                // In a real implementation, we would register this variable in the environment
                // with the expected type
                expected_ty.clone()
            }
            
            // Literal patterns (like `42` in `match x { 42 => ... }`)
            ast::PatternKind::Literal(lit) => {
                let lit_ty = self.infer_literal(lit);
                ctx.add_constraint(Constraint::Eq(lit_ty.clone(), expected_ty.clone()));
                lit_ty
            }
            
            // Wildcard pattern (like `_` in `match x { _ => ... }`)
            ast::PatternKind::Wildcard => {
                // Wildcard can match any type
                expected_ty.clone()
            }
            
            // Tuple patterns (like `(x, y)` in `let (x, y) = (1, 2);`)
            ast::PatternKind::Tuple(patterns) => {
                match expected_ty {
                    Ty::Tuple(element_tys) => {
                        if patterns.len() != element_tys.len() {
                            ctx.error(TypeError::TuplePatternLengthMismatch {
                                expected: element_tys.len(),
                                actual: patterns.len(),
                                span: pattern.span,
                            });
                        } else {
                            // Check each element pattern against the corresponding element type
                            for (pattern, element_ty) in patterns.iter().zip(element_tys) {
                                self.check_pattern(pattern, element_ty, ctx);
                            }
                        }
                        expected_ty.clone()
                    }
                    _ => {
                        ctx.error(TypeError::PatternTypeMismatch {
                            expected: expected_ty.clone(),
                            pattern_desc: "tuple pattern".to_string(),
                            span: pattern.span,
                        });
                        expected_ty.clone()
                    }
                }
            }
            
            // Struct patterns (like `Point { x, y }` in `let Point { x, y } = p;`)
            ast::PatternKind::Struct { path: _, fields: _ } => {
                // In a real implementation, we would:
                // 1. Look up the struct definition
                // 2. Verify that the pattern's fields are valid for the struct
                // 3. Check that the struct type matches the expected type
                
                // For now, just return the expected type
                expected_ty.clone()
            }
            
            // TODO: Add support for other pattern kinds
            _ => {
                ctx.error(TypeError::Internal(
                    format!("Unsupported pattern kind: {:?}", pattern.kind),
                ));
                expected_ty.clone()
            }
        }
    }
    
    // Implement let expression type checking
    fn infer_let<C: TypeContextOps>(&self, pattern: &ast::Pattern, type_ann: Option<&ast::Type>, value: &Expr, ctx: &mut C) -> Ty {
        // 1. Infer the type of the value
        let value_ty = self.infer_expr(value, ctx);
        
        // 2. If there's a type annotation, add a constraint that the value's type matches it
        if let Some(type_ann) = type_ann {
            let ann_ty = self.resolve_type(type_ann, ctx);
            ctx.add_constraint(Constraint::Eq(value_ty.clone(), ann_ty.clone()));
        }
        
        // 3. Check that the pattern is compatible with the value type
        self.check_pattern(pattern, &value_ty, ctx);
        
        // 4. Return the value's type as the type of the let expression
        value_ty
    }

    // Implement lambda expression type checking
    fn infer_lambda<C: TypeContextOps>(
        &self,
        generic_params: &Option<Vec<ast::expr::GenericParam>>,
        params: &[ast::items::Parameter],
        body: &Expr,
        ctx: &mut C
    ) -> Ty {
        // 1. Process generic parameters (if any)
        if let Some(_generics) = generic_params {
            // In a real implementation, we would:
            // a. Create type variables for each generic parameter
            // b. Register them in the type environment
            // c. Handle any constraints/bounds on the type parameters
            
            // For now, we'll ignore generic parameters since we haven't implemented
            // generic type handling yet
        }
        
        // 2. Process parameters
        let mut param_types = Vec::new();
        
        for param in params {
            let param_ty = if let Some(type_ann) = &param.ty {
                // Use the explicit type annotation if available
                self.resolve_type(type_ann, ctx)
            } else {
                // Otherwise, use a fresh type variable
                ctx.new_ty_var()
            };
            
            // In a real implementation, we would add the parameter to the local environment
            // with its type, so it can be referenced in the body
            
            param_types.push(param_ty);
        }
        
        // 3. Infer the type of the body
        let body_ty = self.infer_expr(body, ctx);
        
        // 4. Create and return the function type
        Ty::Function {
            params: param_types,
            ret: Box::new(body_ty),
        }
    }

    // Implement array type checking
    fn infer_array<C: TypeContextOps>(&self, elements: &[Expr], ctx: &mut C) -> Ty {
        if elements.is_empty() {
            // Empty array - element type is unknown
            // We'd need a type annotation or context to determine it
            let element_ty = ctx.new_ty_var();
            return create_array_type(element_ty);
        }
        
        // Infer type of the first element
        let first_ty = self.infer_expr(&elements[0], ctx);
        
        // All other elements must have the same type
        for element in &elements[1..] {
            let element_ty = self.infer_expr(element, ctx);
            ctx.add_constraint(Constraint::Eq(first_ty.clone(), element_ty));
        }
        
        // Return array type with inferred element type
        create_array_type(first_ty)
    }
    
    // Implement tuple type checking
    fn infer_tuple<C: TypeContextOps>(&self, elements: &[Expr], ctx: &mut C) -> Ty {
        // Infer the type of each element
        let element_tys = elements.iter()
            .map(|e| self.infer_expr(e, ctx))
            .collect::<Vec<_>>();
        
        // Return tuple type with element types
        Ty::Tuple(element_tys)
    }
    
    // Implement map type checking
    fn infer_map<C: TypeContextOps>(&self, entries: &[(Expr, Expr)], ctx: &mut C) -> Ty {
        if entries.is_empty() {
            // Empty map - key and value types are unknown
            let key_ty = ctx.new_ty_var();
            let value_ty = ctx.new_ty_var();
            return create_map_type(key_ty, value_ty);
        }
        
        // Infer key and value types from the first entry
        let (first_key, first_value) = &entries[0];
        let key_ty = self.infer_expr(first_key, ctx);
        let value_ty = self.infer_expr(first_value, ctx);
        
        // All other entries must have compatible key and value types
        for (key, value) in &entries[1..] {
            let entry_key_ty = self.infer_expr(key, ctx);
            let entry_value_ty = self.infer_expr(value, ctx);
            
            ctx.add_constraint(Constraint::Eq(key_ty.clone(), entry_key_ty));
            ctx.add_constraint(Constraint::Eq(value_ty.clone(), entry_value_ty));
        }
        
        // Return map type
        create_map_type(key_ty, value_ty)
    }
    
    // Implement hashset type checking
    fn infer_hashset<C: TypeContextOps>(&self, elements: &[Expr], ctx: &mut C) -> Ty {
        if elements.is_empty() {
            // Empty hashset - element type is unknown
            let element_ty = ctx.new_ty_var();
            return create_hashset_type(element_ty);
        }
        
        // Infer type of the first element
        let first_ty = self.infer_expr(&elements[0], ctx);
        
        // All other elements must have the same type
        for element in &elements[1..] {
            let element_ty = self.infer_expr(element, ctx);
            ctx.add_constraint(Constraint::Eq(first_ty.clone(), element_ty));
        }
        
        // Return hashset type
        create_hashset_type(first_ty)
    }

    // Implement if expression type checking
    fn infer_if<C: TypeContextOps>(
        &self,
        condition: &Expr,
        then_branch: &Expr,
        else_branch: Option<&Box<Expr>>,
        ctx: &mut C
    ) -> Ty {
        // 1. Infer the type of the condition
        let cond_ty = self.infer_expr(condition, ctx);
        
        // 2. Condition must be a boolean
        ctx.add_constraint(Constraint::Eq(
            cond_ty,
            Ty::Concrete(ConcreteTy::Bool),
        ));
        
        // 3. Infer the type of the then branch
        let then_ty = self.infer_expr(then_branch, ctx);
        
        // 4. If there's an else branch, infer its type and make sure it's compatible with the then branch
        if let Some(else_expr) = else_branch {
            let else_ty = self.infer_expr(else_expr, ctx);
            
            // Both branches must have the same type
            ctx.add_constraint(Constraint::Eq(then_ty.clone(), else_ty.clone()));
            
            // The result type is the type of either branch (they're constrained to be the same)
            then_ty
        } else {
            // If there's no else branch, the result type is Unit
            Ty::Concrete(ConcreteTy::Unit)
        }
    }

    // Implement path (identifier) resolution
    fn infer_path<C: TypeContextOps>(&self, path: &[Ident], ctx: &mut C) -> Ty {
        // Use the TypeContextOps trait to look up the symbol
        if let Some(ty) = ctx.lookup_symbol(path) {
            return ty;
        }
        
        // If not found, report an error and return a fresh type variable
        ctx.error(TypeError::UnresolvedType {
            ty: format!("path {:?}", path),
            span: ast::Span { start: 0, end: 0 }, // Use real span in production
        });
        
        ctx.new_ty_var()
    }
    
    // Implement unary operations
    fn infer_unary<C: TypeContextOps>(&self, op: ast::expr::UnaryOp, expr: &Expr, ctx: &mut C) -> Ty {
        // Infer the type of the operand
        let operand_ty = self.infer_expr(expr, ctx);
        
        match op {
            ast::expr::UnaryOp::Neg => {
                // Negation should work on numeric types (Int, Float)
                // For now, we'll just return the same type as the operand
                operand_ty
            }
            ast::expr::UnaryOp::Not => {
                // Logical not should work on Bool
                ctx.add_constraint(Constraint::Eq(
                    operand_ty.clone(),
                    Ty::Concrete(ConcreteTy::Bool),
                ));
                Ty::Concrete(ConcreteTy::Bool)
            }
            ast::expr::UnaryOp::Ref => {
                // Reference operator creates a reference to the operand
                // In a real implementation, we would have a dedicated reference type
                // For now, just return a placeholder
                ctx.new_ty_var()
            }
            ast::expr::UnaryOp::Deref => {
                // Dereference operator should work on reference types
                // In a real implementation, we would extract the referenced type
                // For now, just return a placeholder
                ctx.new_ty_var()
            }
        }
    }

    // Implement block expression type checking
    fn infer_block<C: TypeContextOps>(&self, statements: &[Expr], ctx: &mut C) -> Ty {
        // In a real implementation, we would maintain a block scope for variables defined in the block
        
        if statements.is_empty() {
            // Empty block has Unit type
            return Ty::Concrete(ConcreteTy::Unit);
        }
        
        // Infer the type of each statement
        for stmt in &statements[..statements.len() - 1] {
            self.infer_expr(stmt, ctx);
            // For statements that aren't the last one, we ignore their types
            // (they are evaluated for side effects only)
        }
        
        // The type of the block is the type of the last statement/expression
        self.infer_expr(&statements[statements.len() - 1], ctx)
    }
}

// Helper functions for collection types

// Create an array type with the given element type
fn create_array_type(element_ty: Ty) -> Ty {
    Ty::Concrete(ConcreteTy::Named {
        name: "Array".to_string(),
        args: vec![element_ty],
    })
}

// Create a map type with the given key and value types
fn create_map_type(key_ty: Ty, value_ty: Ty) -> Ty {
    Ty::Concrete(ConcreteTy::Named {
        name: "Map".to_string(),
        args: vec![key_ty, value_ty],
    })
}

// Create a hashset type with the given element type
fn create_hashset_type(element_ty: Ty) -> Ty {
    Ty::Concrete(ConcreteTy::Named {
        name: "HashSet".to_string(),
        args: vec![element_ty],
    })
}

/// Helper function to convert a path (Vec<Ident>) to a string
fn path_to_string(path: &Vec<Ident>) -> String {
    path.iter()
        .map(|ident| ident.0.clone())
        .collect::<Vec<_>>()
        .join("::")
} 