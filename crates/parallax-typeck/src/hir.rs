use parallax_lang::ast;
use crate::context::{TypeContext, Ty, ConcreteTy};
use crate::error::TypeError;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Crate {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Item {
    pub kind: ItemKind,
    pub span: ast::Span,
    pub ty: Ty,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ItemKind {
    Function(Function),
    Struct(Struct),
    Module(Module),
    // Add more item kinds as needed
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub return_ty: Ty,
    pub body: Expr,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Module {
    pub name: String,
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Field {
    pub name: String,
    pub ty: Ty,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: Ty,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Ty,
    pub span: ast::Span,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ExprKind {
    Literal(ast::Literal),
    Call { func: Box<Expr>, args: Vec<Expr> },
    Binary { left: Box<Expr>, op: ast::BinaryOp, right: Box<Expr> },
    Field { object: Box<Expr>, name: String },
    Path(String),
    Block(Vec<Expr>),
    If { condition: Box<Expr>, then_branch: Box<Expr>, else_branch: Option<Box<Expr>> },
    // Add more expression kinds as needed
}

/// Lower the AST to HIR with type information
pub fn lower_crate(ctx: &TypeContext) -> Result<Crate, TypeError> {
    // Get the resolved AST from the TypeContext
    let ast = match ctx.db.resolved_ast() {
        Ok(ast) => ast,
        Err(errors) => {
            // If there are resolution errors, convert to type errors
            return Err(TypeError::Internal(format!("Resolution errors: {:?}", errors)));
        }
    };
    
    // Convert the root AST item to HIR
    match &ast.kind {
        ast::ItemKind::Module(module) => {
            // Lower module items
            let items = lower_module_items(module, ctx)?;
            
            // Create HIR crate
            Ok(Crate { items })
        },
        _ => {
            // Root item should be a module
            Err(TypeError::Internal("Root AST item is not a module".to_string()))
        }
    }
}

/// Lower module items to HIR
fn lower_module_items(module: &ast::items::Module, ctx: &TypeContext) -> Result<Vec<Item>, TypeError> {
    let mut items = Vec::new();
    
    for item in &module.items {
        if let Some(hir_item) = lower_item(item, ctx)? {
            items.push(hir_item);
        }
    }
    
    Ok(items)
}

/// Lower a single AST item to HIR
fn lower_item(item: &ast::Item, ctx: &TypeContext) -> Result<Option<Item>, TypeError> {
    // Look up the type of the item in the TypeContext
    let item_ty = match ctx.type_map.get(&item.span) {
        Some(ty) => ty.clone(),
        None => {
            // No type for this item - means it wasn't processed during type checking
            // For now, just skip it
            return Ok(None);
        }
    };
    
    // Convert the AST item kind to HIR item kind
    let kind = match &item.kind {
        ast::ItemKind::Function(func) => {
            // Convert function
            ItemKind::Function(lower_function(func, ctx)?)
        },
        ast::ItemKind::Struct(struct_def) => {
            // Convert struct
            ItemKind::Struct(lower_struct(struct_def, ctx)?)
        },
        ast::ItemKind::Module(module) => {
            // Convert module
            let items = lower_module_items(module, ctx)?;
            ItemKind::Module(Module {
                name: module.name.0.clone(),
                items,
            })
        },
        // Add more cases as needed
        _ => {
            // Skip unsupported item kinds for now
            return Ok(None);
        }
    };
    
    // Create and return the HIR item
    Ok(Some(Item {
        kind,
        span: item.span,
        ty: item_ty,
    }))
}

/// Lower a function to HIR
fn lower_function(func: &ast::items::Function, ctx: &TypeContext) -> Result<Function, TypeError> {
    // Convert parameters
    let mut params = Vec::new();
    for param in &func.params {
        let param_ty = match ctx.type_map.get(&param.span) {
            Some(ty) => ty.clone(),
            None => {
                // No type for this parameter - use a default type
                ctx.concrete_ty(crate::context::ConcreteTy::Unit)
            }
        };
        
        // Extract name from parameter pattern
        let param_name = match &param.pattern.kind {
            ast::PatternKind::Identifier(ident) => ident.0.clone(),
            _ => {
                // For non-identifier patterns, use a placeholder name
                format!("param_{}", params.len())
            }
        };
        
        params.push(Param {
            name: param_name,
            ty: param_ty,
        });
    }
    
    // Determine return type
    let return_ty = match &func.return_type {
        Some(ty_ast) => {
            // Get type from type annotation
            match ctx.type_map.get(&ty_ast.span) {
                Some(ty) => ty.clone(),
                None => {
                    // No type for this annotation - use a default type
                    ctx.concrete_ty(crate::context::ConcreteTy::Unit)
                }
            }
        },
        None => {
            // No explicit return type - use Unit
            ctx.concrete_ty(crate::context::ConcreteTy::Unit)
        }
    };
    
    // Convert function body
    let body = lower_expr(&func.body, ctx)?;
    
    Ok(Function {
        name: func.name.0.clone(),
        params,
        return_ty,
        body,
    })
}

/// Lower a struct to HIR
fn lower_struct(struct_def: &ast::items::StructDef, ctx: &TypeContext) -> Result<Struct, TypeError> {
    // Convert fields
    let mut fields = Vec::new();
    for field in &struct_def.fields {
        let field_ty = match ctx.type_map.get(&field.span) {
            Some(ty) => ty.clone(),
            None => {
                // If no type found, use a placeholder unit type
                // In a real implementation, we would look up the type from the AST
                println!("Warning: No type information for field {}", field.name.0);
                Ty::Concrete(ConcreteTy::Unit)
            }
        };
        
        fields.push(Field {
            name: field.name.0.clone(),
            ty: field_ty,
        });
    }
    
    Ok(Struct {
        name: struct_def.name.0.clone(),
        fields,
    })
}

/// Lower an expression to HIR
fn lower_expr(expr: &ast::Expr, ctx: &TypeContext) -> Result<Expr, TypeError> {
    // Look up the type of the expression
    let expr_ty = match ctx.type_map.get(&expr.span) {
        Some(ty) => ty.clone(),
        None => {
            // No type for this expression - create a placeholder
            println!("Warning: No type found for expression at {:?}", expr.span);
            Ty::Concrete(ConcreteTy::Unit)
        }
    };
    
    // Convert the expression kind
    let kind = match &expr.kind {
        ast::ExprKind::Literal(lit) => {
            // Literal expressions are straightforward
            ExprKind::Literal(lit.clone())
        },
        ast::ExprKind::Binary { left, op, right } => {
            // Convert binary operation
            ExprKind::Binary {
                left: Box::new(lower_expr(left, ctx)?),
                op: *op,
                right: Box::new(lower_expr(right, ctx)?),
            }
        },
        ast::ExprKind::Call { func, args } => {
            // Convert function call
            let hir_func = Box::new(lower_expr(func, ctx)?);
            
            // Process arguments
            let mut hir_args = Vec::new();
            for arg in args {
                // Extract the value from the Argument struct
                hir_args.push(lower_expr(&arg.value, ctx)?);
            }
            
            ExprKind::Call {
                func: hir_func,
                args: hir_args,
            }
        },
        ast::ExprKind::Field { object, name } => {
            // Convert field access
            ExprKind::Field {
                object: Box::new(lower_expr(object, ctx)?),
                name: name.0.clone(),
            }
        },
        ast::ExprKind::Path(path) => {
            // Convert path to a simple string for now
            // In a real implementation, we would preserve the full path structure
            let path_str = path.iter().map(|ident| ident.0.clone()).collect::<Vec<_>>().join("::");
            ExprKind::Path(path_str)
        },
        ast::ExprKind::Block(statements) => {
            // Convert block
            let mut hir_statements = Vec::new();
            for stmt in statements {
                hir_statements.push(lower_expr(stmt, ctx)?);
            }
            
            ExprKind::Block(hir_statements)
        },
        ast::ExprKind::If { condition, then_branch, else_branch } => {
            // Convert if expression
            ExprKind::If {
                condition: Box::new(lower_expr(condition, ctx)?),
                then_branch: Box::new(lower_expr(then_branch, ctx)?),
                else_branch: match else_branch {
                    Some(expr) => Some(Box::new(lower_expr(expr, ctx)?)),
                    None => None,
                },
            }
        },
        // Add more cases as needed
        _ => {
            // For unsupported expression kinds, create a placeholder
            println!("Warning: Unsupported expression kind {:?}", expr.kind);
            ExprKind::Literal(ast::Literal::Bool(false))
        }
    };
    
    // Create the HIR expression
    Ok(Expr {
        kind,
        ty: expr_ty,
        span: expr.span,
    })
}