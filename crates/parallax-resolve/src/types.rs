//! Type resolution system for Parallax.
//!
//! This module handles resolving and checking types during the name resolution phase.
//! It provides functionality for:
//! - Resolving type references to their definitions
//! - Checking type compatibility
//! - Managing generic type parameters and their constraints
//! - Unification of types during inference

use parallax_lang::ast::{
    common::Span,
    types::{Type, TypeKind},
};
use fxhash::FxHashMap;
use crate::{
    error::ResolveError,
    symbol::{Symbol, SymbolTable},
    Result,
};

/// Represents a resolved type in the Parallax language.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedType {
    /// A primitive type (i32, bool, etc.).
    Primitive(PrimitiveType),
    
    /// A user-defined type (struct, enum, etc.).
    Named {
        /// The name of the type.
        name: String,
        /// The symbol representing the type.
        symbol: Option<Symbol>,
        /// Generic arguments for the type.
        args: Vec<ResolvedType>,
        /// The span of the type reference.
        span: Span,
    },
    
    /// A function type (A -> B).
    Function {
        /// The parameter type.
        param: Box<ResolvedType>,
        /// The return type.
        ret: Box<ResolvedType>,
        /// The span of the type reference.
        span: Span,
    },
    
    /// A tuple type (A, B, C).
    Tuple {
        /// The element types.
        elements: Vec<ResolvedType>,
        /// The span of the type reference.
        span: Span,
    },
    
    /// An array type [A; n].
    Array {
        /// The element type.
        element: Box<ResolvedType>,
        /// The size of the array.
        size: usize,
        /// The span of the type reference.
        span: Span,
    },
    
    /// A type variable (for generics).
    TypeVar {
        /// The name of the type variable.
        name: String,
        /// The span of the type reference.
        span: Span,
    },
    
    /// An error type. Used to avoid cascading errors.
    Error,
}

/// Primitive types in Parallax.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveType {
    /// The 32-bit signed integer type.
    I32,
    /// The 64-bit signed integer type.
    I64,
    /// The 32-bit floating point type.
    F32,
    /// The 64-bit floating point type.
    F64,
    /// The boolean type.
    Bool,
    /// The character type.
    Char,
    /// The string type.
    String,
    /// The unit type. Represents the absence of a value.
    Unit,
}

/// Type environment for type resolution and checking.
pub struct TypeEnv {
    /// Maps variable names to their types.
    vars: FxHashMap<String, ResolvedType>,
    /// Maps type names to their definitions.
    types: FxHashMap<String, ResolvedType>,
    /// Type variables for generics.
    type_vars: FxHashMap<String, ResolvedType>,
    /// Constraint maps for type variables.
    constraints: FxHashMap<String, Vec<TypeConstraint>>,
    /// Type substitutions for instantiating generic types with concrete types.
    substitutions: FxHashMap<String, ResolvedType>,
}

/// A constraint on a type variable.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeConstraint {
    /// The type must be equal to another type.
    Equals(ResolvedType),
    /// The type must be a subtype of another type.
    Subtype(ResolvedType),
    /// The type must implement a trait.
    Implements(String),
}

impl TypeEnv {
    /// Create a new type environment.
    pub fn new() -> Self {
        let mut env = Self {
            vars: FxHashMap::default(),
            types: FxHashMap::default(),
            type_vars: FxHashMap::default(),
            constraints: FxHashMap::default(),
            substitutions: FxHashMap::default(),
        };
        
        // Add primitive types
        env.add_primitive_types();
        
        env
    }
    
    /// Add primitive types to the environment.
    fn add_primitive_types(&mut self) {
        // Add primitive types to the environment
        let primitives = [
            ("i32", PrimitiveType::I32),
            ("i64", PrimitiveType::I64),
            ("f32", PrimitiveType::F32),
            ("f64", PrimitiveType::F64),
            ("bool", PrimitiveType::Bool),
            ("char", PrimitiveType::Char),
            ("string", PrimitiveType::String),
            ("()", PrimitiveType::Unit),
        ];
        
        for (name, prim) in primitives.iter() {
            self.types.insert(name.to_string(), ResolvedType::Primitive(*prim));
        }
    }
    
    /// Resolve a type reference to a resolved type.
    pub fn resolve_type(&mut self, ty: &Type, symbol_table: &SymbolTable) -> Result<ResolvedType> {
        match &ty.kind {
            TypeKind::Path(path) => {
                // Handle primitive types
                if path.len() == 1 {
                    let name = &path[0].0;
                    // Check for primitive types
                    if let Some(resolved) = self.types.get(name) {
                        return Ok(resolved.clone());
                    }
                    
                    // Check if it's a type variable
                    if let Some(resolved) = self.type_vars.get(name) {
                        return Ok(resolved.clone());
                    }
                    
                    // Look for type in symbol table
                    let symbols = symbol_table.lookup(name);
                    let type_symbols = symbols.iter().filter(|s| {
                        matches!(s, Symbol::Struct { .. } | Symbol::Enum { .. } | Symbol::TypeParam { .. } | Symbol::Trait { .. })
                    }).collect::<Vec<_>>();
                    
                    if !type_symbols.is_empty() {
                        let symbol = *type_symbols[0];
                        return Ok(ResolvedType::Named {
                            name: name.clone(),
                            symbol: Some(symbol.clone()),
                            args: Vec::new(),
                            span: ty.span,
                        });
                    }
                }
                
                // Handle qualified paths (e.g., module::Type)
                // This would handle paths like "std::collections::HashMap" in a real impl
                // For now, just create a named type with the last segment as the name
                Ok(ResolvedType::Named {
                    name: path.last().unwrap().0.clone(),
                    symbol: None,
                    args: Vec::new(),
                    span: ty.span,
                })
            }
            TypeKind::Function(param_ty, ret_ty) => {
                let param = self.resolve_type(param_ty, symbol_table)?;
                let ret = self.resolve_type(ret_ty, symbol_table)?;
                
                Ok(ResolvedType::Function {
                    param: Box::new(param),
                    ret: Box::new(ret),
                    span: ty.span,
                })
            }
            TypeKind::Tuple(types) => {
                let elements = types
                    .iter()
                    .map(|ty| self.resolve_type(ty, symbol_table))
                    .collect::<Result<Vec<_>>>()?;
                
                Ok(ResolvedType::Tuple {
                    elements,
                    span: ty.span,
                })
            }
            TypeKind::Array(elem_ty, size) => {
                let element = self.resolve_type(elem_ty, symbol_table)?;
                
                Ok(ResolvedType::Array {
                    element: Box::new(element),
                    size: *size,
                    span: ty.span,
                })
            }
            TypeKind::KindApp(base_ty, args) => {
                let base = self.resolve_type(base_ty, symbol_table)?;
                
                let args = args
                    .iter()
                    .map(|ty| self.resolve_type(ty, symbol_table))
                    .collect::<Result<Vec<_>>>()?;
                
                match base {
                    ResolvedType::Named { name, symbol, args: _, span } => {
                        Ok(ResolvedType::Named {
                            name,
                            symbol,
                            args,
                            span,
                        })
                    }
                    _ => {
                        // Only named types can have type arguments
                        Err(ResolveError::generic_error(
                            "Type application not allowed on this type".to_string(),
                            ty.span,
                            std::path::PathBuf::from("<unknown>"),
                            "".to_string(),
                        ))
                    }
                }
            }
        }
    }
    
    /// Add a type variable to the environment.
    pub fn add_type_var(&mut self, name: String, span: Span) {
        let ty_var = ResolvedType::TypeVar { name: name.clone(), span };
        self.type_vars.insert(name, ty_var);
    }
    
    /// Add a variable to the environment.
    pub fn add_var(&mut self, name: String, ty: ResolvedType) {
        self.vars.insert(name, ty);
    }
    
    /// Get the type of a variable.
    pub fn get_var_type(&self, name: &str) -> Option<&ResolvedType> {
        self.vars.get(name)
    }
    
    /// Add a constraint to a type variable.
    pub fn add_constraint(&mut self, var_name: String, constraint: TypeConstraint) {
        self.constraints.entry(var_name).or_insert_with(Vec::new).push(constraint);
    }
    
    /// Check if two types are compatible.
    pub fn check_compatibility(&self, ty1: &ResolvedType, ty2: &ResolvedType) -> bool {
        match (ty1, ty2) {
            // Identical primitive types are compatible
            (ResolvedType::Primitive(p1), ResolvedType::Primitive(p2)) => p1 == p2,
            
            // Function types are compatible if their parameters and return types are compatible
            (
                ResolvedType::Function { param: p1, ret: r1, .. },
                ResolvedType::Function { param: p2, ret: r2, .. },
            ) => {
                self.check_compatibility(p1, p2) && self.check_compatibility(r1, r2)
            }
            
            // Tuple types are compatible if they have the same length and their elements are compatible
            (
                ResolvedType::Tuple { elements: e1, .. },
                ResolvedType::Tuple { elements: e2, .. },
            ) => {
                if e1.len() != e2.len() {
                    return false;
                }
                
                e1.iter().zip(e2.iter()).all(|(t1, t2)| self.check_compatibility(t1, t2))
            }
            
            // Array types are compatible if their element types are compatible and they have the same size
            (
                ResolvedType::Array { element: e1, size: s1, .. },
                ResolvedType::Array { element: e2, size: s2, .. },
            ) => {
                s1 == s2 && self.check_compatibility(e1, e2)
            }
            
            // Named types are compatible if they have the same name and their arguments are compatible
            (
                ResolvedType::Named { name: n1, args: a1, .. },
                ResolvedType::Named { name: n2, args: a2, .. },
            ) => {
                if n1 != n2 || a1.len() != a2.len() {
                    return false;
                }
                
                a1.iter().zip(a2.iter()).all(|(t1, t2)| self.check_compatibility(t1, t2))
            }
            
            // Type variables are compatible with any type (for now)
            (ResolvedType::TypeVar { .. }, _) => true,
            (_, ResolvedType::TypeVar { .. }) => true,
            
            // Anything is compatible with an error type (to avoid cascading errors)
            (ResolvedType::Error, _) => true,
            (_, ResolvedType::Error) => true,
            
            // Otherwise, the types are not compatible
            _ => false,
        }
    }
    
    /// Add a type substitution from a type variable to a concrete type.
    pub fn add_substitution(&mut self, type_var: String, concrete_type: ResolvedType) {
        self.substitutions.insert(type_var, concrete_type);
    }
    
    /// Remove a type substitution.
    pub fn remove_substitution(&mut self, type_var: &str) {
        self.substitutions.remove(type_var);
    }
    
    /// Apply substitutions to a type, replacing type variables with their concrete types.
    pub fn apply_substitutions(&self, ty: &ResolvedType) -> ResolvedType {
        match ty {
            ResolvedType::TypeVar { name, span } => {
                if let Some(substitution) = self.substitutions.get(name) {
                    // Apply substitutions recursively
                    self.apply_substitutions(substitution)
                } else {
                    // No substitution available
                    ty.clone()
                }
            }
            ResolvedType::Function { param, ret, span } => {
                let param = self.apply_substitutions(param);
                let ret = self.apply_substitutions(ret);
                ResolvedType::Function {
                    param: Box::new(param),
                    ret: Box::new(ret),
                    span: *span,
                }
            }
            ResolvedType::Tuple { elements, span } => {
                let elements = elements.iter()
                    .map(|elem| self.apply_substitutions(elem))
                    .collect();
                ResolvedType::Tuple {
                    elements,
                    span: *span,
                }
            }
            ResolvedType::Array { element, size, span } => {
                let element = self.apply_substitutions(element);
                ResolvedType::Array {
                    element: Box::new(element),
                    size: *size,
                    span: *span,
                }
            }
            ResolvedType::Named { name, symbol, args, span } => {
                let args = args.iter()
                    .map(|arg| self.apply_substitutions(arg))
                    .collect();
                ResolvedType::Named {
                    name: name.clone(),
                    symbol: symbol.clone(),
                    args,
                    span: *span,
                }
            }
            // Primitive types and error type don't need substitution
            _ => ty.clone(),
        }
    }
}

impl Default for TypeEnv {
    fn default() -> Self {
        Self::new()
    }
}

/// Type resolver that integrates with the name resolver.
pub struct TypeResolver {
    /// The type environment.
    pub env: TypeEnv,
}

impl TypeResolver {
    /// Create a new type resolver.
    pub fn new() -> Self {
        Self {
            env: TypeEnv::new(),
        }
    }
    
    /// Resolve a type reference.
    pub fn resolve_type(&mut self, ty: &Type, symbol_table: &SymbolTable) -> Result<ResolvedType> {
        self.env.resolve_type(ty, symbol_table)
    }
    
    /// Resolve the type of an expression.
    pub fn resolve_expr_type(
        &mut self,
        expr: &parallax_lang::ast::expr::Expr,
        symbol_table: &SymbolTable,
    ) -> Result<ResolvedType> {
        match &expr.kind {
            // Literals always have a known type
            parallax_lang::ast::expr::ExprKind::Literal(lit) => {
                match lit {
                    parallax_lang::ast::common::Literal::Int(_) => Ok(ResolvedType::Primitive(PrimitiveType::I32)),
                    parallax_lang::ast::common::Literal::Float(_) => Ok(ResolvedType::Primitive(PrimitiveType::F64)),
                    parallax_lang::ast::common::Literal::Bool(_) => Ok(ResolvedType::Primitive(PrimitiveType::Bool)),
                    parallax_lang::ast::common::Literal::Char(_) => Ok(ResolvedType::Primitive(PrimitiveType::Char)),
                    parallax_lang::ast::common::Literal::String(_) => Ok(ResolvedType::Primitive(PrimitiveType::String)),
                }
            }
            
            // Path expressions refer to variables, functions, or other named entities
            parallax_lang::ast::expr::ExprKind::Path(path) => {
                // Look up the symbol for the path
                if path.is_empty() {
                    return Err(ResolveError::generic_error(
                        "Empty path in expression".to_string(),
                        expr.span,
                        std::path::PathBuf::from("<unknown>"),
                        "".to_string(),
                    ));
                }
                
                // For simple paths (just an identifier), look up the variable type
                if path.len() == 1 {
                    let name = &path[0].0;
                    if let Some(var_type) = self.env.get_var_type(name) {
                        return Ok(var_type.clone());
                    }
                }
                
                // Try to find the symbol in the symbol table
                let name = &path.last().unwrap().0;
                let symbols = symbol_table.lookup(name);
                if symbols.is_empty() {
                    return Err(ResolveError::undefined_name(
                        name.clone(),
                        expr.span,
                        std::path::PathBuf::from("<unknown>"),
                        "".to_string(),
                    ));
                }
                
                // Based on the symbol, determine the type
                for symbol in symbols {
                    match symbol {
                        Symbol::Function {  .. } => {
                            // Create a function type based on the function signature
                            // For simplicity, this is a placeholder; a real implementation would
                            // analyze the parameter and return types from the signature
                            return Ok(ResolvedType::Named {
                                name: name.clone(),
                                symbol: Some(symbol.clone()),
                                args: vec![],
                                span: expr.span,
                            });
                        }
                        Symbol::Variable { ty, .. } => {
                            // If we have the type information, use it
                            if let Some(ty) = ty {
                                // In a real implementation, we would convert from the AST type
                                // to a ResolvedType, but for this placeholder, we'll just create
                                // a "variable" type with the name
                                return Ok(ResolvedType::Named {
                                    name: format!("var:{}", name),
                                    symbol: Some(symbol.clone()),
                                    args: vec![],
                                    span: expr.span,
                                });
                            } else {
                                // Variable without type info, assume an error or unknown type
                                return Ok(ResolvedType::Error);
                            }
                        }
                        _ => {
                            // Other symbols may not be valid in expressions
                            continue;
                        }
                    }
                }
                
                // Could not determine the type
                Ok(ResolvedType::Error)
            }
            
            // Binary operations depend on the operands and the operation
            parallax_lang::ast::expr::ExprKind::Binary { left, op, right } => {
                let left_type = self.resolve_expr_type(left, symbol_table)?;
                let right_type = self.resolve_expr_type(right, symbol_table)?;
                
                match op {
                    // Arithmetic operations on numbers yield numbers
                    parallax_lang::ast::expr::BinaryOp::Add |
                    parallax_lang::ast::expr::BinaryOp::Sub |
                    parallax_lang::ast::expr::BinaryOp::Mul |
                    parallax_lang::ast::expr::BinaryOp::Div => {
                        // For now, just use the left operand's type for arithmetic
                        // In a more complete implementation, we would handle type promotion
                        // and check that both operands are numeric
                        if let ResolvedType::Primitive(PrimitiveType::I32) = left_type {
                            Ok(ResolvedType::Primitive(PrimitiveType::I32))
                        } else if let ResolvedType::Primitive(PrimitiveType::I64) = left_type {
                            Ok(ResolvedType::Primitive(PrimitiveType::I64))
                        } else if let ResolvedType::Primitive(PrimitiveType::F32) = left_type {
                            Ok(ResolvedType::Primitive(PrimitiveType::F32))
                        } else if let ResolvedType::Primitive(PrimitiveType::F64) = left_type {
                            Ok(ResolvedType::Primitive(PrimitiveType::F64))
                        } else {
                            // Error: not a numeric type
                            Err(ResolveError::generic_error(
                                "Arithmetic operations require numeric operands".to_string(),
                                expr.span,
                                std::path::PathBuf::from("<unknown>"),
                                "".to_string(),
                            ))
                        }
                    }
                    
                    // Comparison operations yield booleans
                    parallax_lang::ast::expr::BinaryOp::Eq |
                    parallax_lang::ast::expr::BinaryOp::Lt |
                    parallax_lang::ast::expr::BinaryOp::Gt => {
                        // Any comparison yields a boolean
                        // In a more complete implementation, we would check that the
                        // operands are comparable
                        Ok(ResolvedType::Primitive(PrimitiveType::Bool))
                    }
                    
                    // Logical operations on booleans yield booleans
                    parallax_lang::ast::expr::BinaryOp::And |
                    parallax_lang::ast::expr::BinaryOp::Or => {
                        // Check that both operands are booleans
                        if !matches!(left_type, ResolvedType::Primitive(PrimitiveType::Bool)) ||
                           !matches!(right_type, ResolvedType::Primitive(PrimitiveType::Bool)) {
                            return Err(ResolveError::generic_error(
                                "Logical operations require boolean operands".to_string(),
                                expr.span,
                                std::path::PathBuf::from("<unknown>"),
                                "".to_string(),
                            ));
                        }
                        
                        Ok(ResolvedType::Primitive(PrimitiveType::Bool))
                    }
                    
                    // Function arrow type (not actually a binary operation in expressions)
                    parallax_lang::ast::expr::BinaryOp::Arrow => {
                        // This should not occur in normal expressions
                        Err(ResolveError::generic_error(
                            "Arrow operator not expected in expressions".to_string(),
                            expr.span,
                            std::path::PathBuf::from("<unknown>"),
                            "".to_string(),
                        ))
                    }
                }
            }
            
            // Unary operations depend on the operand and the operation
            parallax_lang::ast::expr::ExprKind::Unary { op, expr: inner_expr } => {
                let inner_type = self.resolve_expr_type(inner_expr, symbol_table)?;
                
                match op {
                    // Negation requires a numeric operand and yields the same type
                    parallax_lang::ast::expr::UnaryOp::Neg => {
                        if let ResolvedType::Primitive(primitive) = inner_type {
                            match primitive {
                                PrimitiveType::I32 |
                                PrimitiveType::I64 |
                                PrimitiveType::F32 |
                                PrimitiveType::F64 => Ok(ResolvedType::Primitive(primitive)),
                                _ => Err(ResolveError::generic_error(
                                    "Unary negation requires a numeric operand".to_string(),
                                    expr.span,
                                    std::path::PathBuf::from("<unknown>"),
                                    "".to_string(),
                                )),
                            }
                        } else {
                            Err(ResolveError::generic_error(
                                "Unary negation requires a numeric operand".to_string(),
                                expr.span,
                                std::path::PathBuf::from("<unknown>"),
                                "".to_string(),
                            ))
                        }
                    }
                    
                    // Logical not requires a boolean operand and yields a boolean
                    parallax_lang::ast::expr::UnaryOp::Not => {
                        if !matches!(inner_type, ResolvedType::Primitive(PrimitiveType::Bool)) {
                            return Err(ResolveError::generic_error(
                                "Logical not requires a boolean operand".to_string(),
                                expr.span,
                                std::path::PathBuf::from("<unknown>"),
                                "".to_string(),
                            ));
                        }
                        
                        Ok(ResolvedType::Primitive(PrimitiveType::Bool))
                    }
                    
                    // Reference operator creates a reference to the inner type
                    parallax_lang::ast::expr::UnaryOp::Ref => {
                        // In a full implementation, we would create a proper reference type
                        // For now, we just return the inner type
                        Ok(inner_type)
                    }
                    
                    // Dereference operator accesses the underlying type
                    parallax_lang::ast::expr::UnaryOp::Deref => {
                        // In a full implementation, we would extract the referenced type
                        // For now, we just return the inner type
                        Ok(inner_type)
                    }
                }
            }
            
            // If expressions yield the type of the then branch (and else branch if present)
            parallax_lang::ast::expr::ExprKind::If { condition, then_branch, else_branch } => {
                // The condition must be a boolean
                let cond_type = self.resolve_expr_type(condition, symbol_table)?;
                if !matches!(cond_type, ResolvedType::Primitive(PrimitiveType::Bool)) {
                    return Err(ResolveError::generic_error(
                        "If condition must be a boolean".to_string(),
                        condition.span,
                        std::path::PathBuf::from("<unknown>"),
                        "".to_string(),
                    ));
                }
                
                // Get the type of the then branch
                let then_type = self.resolve_expr_type(then_branch, symbol_table)?;
                
                // If there's an else branch, its type must be compatible with the then branch
                if let Some(else_branch) = else_branch {
                    let else_type = self.resolve_expr_type(else_branch, symbol_table)?;
                    
                    if !self.env.check_compatibility(&then_type, &else_type) {
                        return Err(ResolveError::generic_error(
                            "Then and else branches must have compatible types".to_string(),
                            expr.span,
                            std::path::PathBuf::from("<unknown>"),
                            "".to_string(),
                        ));
                    }
                    
                    // Return the then type (could choose the more specific of the two)
                    Ok(then_type)
                } else {
                    // Without an else branch, the if expression yields unit
                    Ok(ResolvedType::Primitive(PrimitiveType::Unit))
                }
            }
            
            // Block expressions yield the type of their last expression, or unit if empty
            parallax_lang::ast::expr::ExprKind::Block(exprs) => {
                if exprs.is_empty() {
                    return Ok(ResolvedType::Primitive(PrimitiveType::Unit));
                }
                
                // Process all expressions in the block
                for expr in exprs.iter().take(exprs.len() - 1) {
                    self.resolve_expr_type(expr, symbol_table)?;
                }
                
                // The last expression determines the block's type
                let last_expr = exprs.last().unwrap();
                self.resolve_expr_type(last_expr, symbol_table)
            }
            
            // Let expressions yield the unit type
            parallax_lang::ast::expr::ExprKind::Let { pattern, type_ann, value } => {
                // Resolve the type of the value
                let value_type = self.resolve_expr_type(value, symbol_table)?;
                
                // If there's a type annotation, resolve it and check compatibility
                if let Some(type_ann) = type_ann {
                    let ann_type = self.resolve_type(type_ann, symbol_table)?;
                    
                    if !self.env.check_compatibility(&value_type, &ann_type) {
                        return Err(ResolveError::generic_error(
                            "Value type is incompatible with type annotation".to_string(),
                            expr.span,
                            std::path::PathBuf::from("<unknown>"),
                            "".to_string(),
                        ));
                    }
                    
                    // The annotated type is the definitive type
                    // Store it in the environment for the pattern's variables
                    self.add_binding_to_env(pattern, ann_type.clone());
                } else {
                    // No type annotation, infer from the value
                    self.add_binding_to_env(pattern, value_type.clone());
                }
                
                // Let expressions yield unit
                Ok(ResolvedType::Primitive(PrimitiveType::Unit))
            }
            
            // Function calls require resolving the function and argument types
            parallax_lang::ast::expr::ExprKind::Call { func, args } => {
                // Resolve the function type
                let func_type = self.resolve_expr_type(func, symbol_table)?;
                
                // If the function type is a function, get its return type
                match func_type {
                    ResolvedType::Function { ret, .. } => {
                        // Resolve all argument types
                        for arg in args {
                            let arg_type = self.resolve_expr_type(&arg.value, symbol_table)?;
                            // In a complete implementation, we would check arg types against param types
                        }
                        
                        // The call expression yields the function's return type
                        Ok(*ret)
                    }
                    // If the function type couldn't be resolved as a function,
                    // but is a named type, it might be a constructor
                    ResolvedType::Named { name, symbol, .. } => {
                        // Resolve all argument types
                        for arg in args {
                            self.resolve_expr_type(&arg.value, symbol_table)?;
                        }
                        
                        // For constructors, the call yields an instance of the called type
                        Ok(ResolvedType::Named {
                            name,
                            symbol,
                            args: vec![],
                            span: expr.span,
                        })
                    }
                    _ => {
                        // Not a callable type
                        Err(ResolveError::generic_error(
                            "Called expression is not a function".to_string(),
                            func.span,
                            std::path::PathBuf::from("<unknown>"),
                            "".to_string(),
                        ))
                    }
                }
            }
            
            // Other expression types would be handled here
            _ => {
                // For now, return error for unsupported expression types
                Ok(ResolvedType::Error)
            }
        }
    }
    
    /// Add a binding to the environment based on a pattern
    fn add_binding_to_env(&mut self, pattern: &parallax_lang::ast::pattern::Pattern, ty: ResolvedType) {
        match &pattern.kind {
            parallax_lang::ast::pattern::PatternKind::Identifier(ident) => {
                self.env.add_var(ident.0.clone(), ty);
            }
            // For more complex patterns, we would need to handle nested bindings
            _ => {
                // Not implemented for complex patterns yet
            }
        }
    }

    /// Process generic parameters by adding them to the environment as type variables.
    pub fn process_generic_params(&mut self, 
                                 generic_params: &Option<Vec<parallax_lang::ast::expr::GenericParam>>,
                                 symbol_table: &SymbolTable) {
        if let Some(params) = generic_params {
            for param in params {
                // Get the parent scope span as GenericParam doesn't have a span field
                let span = Span { start: 0, end: 0 }; // Use a default span since GenericParam doesn't have one
                self.env.add_type_var(param.name.0.clone(), span);
                
                // Note: GenericParam doesn't have a direct bounds field
                // Bounds are represented in the where clause
                // We would typically process these in process_where_clause
            }
        }
    }
    
    /// Process a where clause by adding constraints to the type environment.
    pub fn process_where_clause(&mut self, 
                              where_clause: &Option<parallax_lang::ast::items::WhereClause>,
                              symbol_table: &SymbolTable) -> Result<()> {
        if let Some(clause) = where_clause {
            for predicate in &clause.predicates {
                let ty = self.resolve_type(&predicate.ty, symbol_table)?;
                
                // Extract type variable name if it's a type variable
                let type_var_name = match &ty {
                    ResolvedType::TypeVar { name, .. } => Some(name.clone()),
                    ResolvedType::Named { name, .. } => Some(name.clone()),
                    _ => None,
                };
                
                if let Some(name) = type_var_name {
                    // Add each bound as a constraint
                    for bound in &predicate.bounds {
                        let bound_ty = self.resolve_type(bound, symbol_table)?;
                        match &bound_ty {
                            ResolvedType::Named { name: trait_name, .. } => {
                                self.env.add_constraint(
                                    name.clone(),
                                    TypeConstraint::Implements(trait_name.clone())
                                );
                            },
                            _ => {
                                // Only trait bounds are supported for now
                                return Err(ResolveError::generic_error(
                                    "Expected a trait bound".to_string(),
                                    bound.span,
                                    std::path::PathBuf::from("<unknown>"),
                                    "".to_string(),
                                ));
                            }
                        }
                    }
                }
            }
        }
        
        Ok(())
    }
    
    /// Check if a type satisfies all its constraints.
    pub fn check_constraints(&self, type_var: &str, concrete_type: &ResolvedType) -> bool {
        if let Some(constraints) = self.env.constraints.get(type_var) {
            for constraint in constraints {
                match constraint {
                    TypeConstraint::Equals(expected_type) => {
                        if !self.env.check_compatibility(concrete_type, expected_type) {
                            return false;
                        }
                    },
                    TypeConstraint::Subtype(super_type) => {
                        // For now, we just check compatibility as we don't have a proper subtyping system
                        if !self.env.check_compatibility(concrete_type, super_type) {
                            return false;
                        }
                    },
                    TypeConstraint::Implements(trait_name) => {
                        // In a real implementation, we would check if the concrete type
                        // implements the specified trait by looking up trait implementations
                        // For now, we'll just assume it does
                        // TODO: Implement proper trait implementation checking
                    }
                }
            }
        }
        
        true
    }
    
    /// Instantiate a generic type with concrete type arguments.
    pub fn instantiate_generic_type(&mut self, 
                                  generic_type: &ResolvedType, 
                                  type_args: &[ResolvedType]) -> Result<ResolvedType> {
        match generic_type {
            ResolvedType::Named { name, symbol, args, span } => {
                if !args.is_empty() {
                    // This is already an instantiated generic type
                    return Ok(generic_type.clone());
                }
                
                // Get type parameters from the symbol
                let type_params = match symbol {
                    Some(Symbol::Struct { def, .. }) => {
                        def.generic_params.as_ref().map(|params| {
                            params.iter().map(|p| p.name.0.clone()).collect::<Vec<_>>()
                        })
                    },
                    Some(Symbol::Enum { def, .. }) => {
                        def.generic_params.as_ref().map(|params| {
                            params.iter().map(|p| p.name.0.clone()).collect::<Vec<_>>()
                        })
                    },
                    Some(Symbol::Function { sig, .. }) => {
                        sig.generic_params.as_ref().map(|params| {
                            params.iter().map(|p| p.name.0.clone()).collect::<Vec<_>>()
                        })
                    },
                    Some(Symbol::Trait { def, .. }) => {
                        def.generic_params.as_ref().map(|params| {
                            params.iter().map(|p| p.name.0.clone()).collect::<Vec<_>>()
                        })
                    },
                    _ => None,
                };
                
                if let Some(params) = type_params {
                    if params.len() != type_args.len() {
                        return Err(ResolveError::generic_error(
                            format!("Expected {} type arguments but got {}", params.len(), type_args.len()),
                            *span,
                            std::path::PathBuf::from("<unknown>"),
                            "".to_string(),
                        ));
                    }
                    
                    // Add substitutions for type parameters
                    for (param, arg) in params.iter().zip(type_args.iter()) {
                        self.env.add_substitution(param.clone(), arg.clone());
                    }
                    
                    // Return instantiated type
                    let result = ResolvedType::Named {
                        name: name.clone(),
                        symbol: symbol.clone(),
                        args: type_args.to_vec(),
                        span: *span,
                    };
                    
                    // Remove substitutions to avoid affecting other types
                    for param in params {
                        self.env.remove_substitution(&param);
                    }
                    
                    Ok(result)
                } else {
                    // Not a generic type
                    Err(ResolveError::generic_error(
                        format!("Type '{}' is not generic and cannot be instantiated with type arguments", name),
                        *span,
                        std::path::PathBuf::from("<unknown>"),
                        "".to_string(),
                    ))
                }
            },
            _ => {
                // Only named types can be instantiated
                Err(ResolveError::generic_error(
                    "Only named types can be instantiated with type arguments".to_string(),
                    Span { start: 0, end: 0 }, // We don't have a span for ResolvedType
                    std::path::PathBuf::from("<unknown>"),
                    "".to_string(),
                ))
            }
        }
    }
}

impl Default for TypeResolver {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parallax_lang::ast::common::Span;
    
    #[test]
    fn test_primitive_types() {
        let env = TypeEnv::new();
        
        // Check that primitive types exist in the environment
        assert!(matches!(
            env.types.get("i32"),
            Some(ResolvedType::Primitive(PrimitiveType::I32))
        ));
        
        assert!(matches!(
            env.types.get("bool"),
            Some(ResolvedType::Primitive(PrimitiveType::Bool))
        ));
        
        assert!(matches!(
            env.types.get("()"),
            Some(ResolvedType::Primitive(PrimitiveType::Unit))
        ));
    }
    
    #[test]
    fn test_type_compatibility() {
        let env = TypeEnv::new();
        
        // Primitive types
        let i32_type = ResolvedType::Primitive(PrimitiveType::I32);
        let i64_type = ResolvedType::Primitive(PrimitiveType::I64);
        let bool_type = ResolvedType::Primitive(PrimitiveType::Bool);
        
        // Same types are compatible
        assert!(env.check_compatibility(&i32_type, &i32_type));
        
        // Different primitive types are not compatible
        assert!(!env.check_compatibility(&i32_type, &i64_type));
        assert!(!env.check_compatibility(&i32_type, &bool_type));
        
        // Function types
        let fn_i32_bool = ResolvedType::Function {
            param: Box::new(i32_type.clone()),
            ret: Box::new(bool_type.clone()),
            span: Span { start: 0, end: 0 },
        };
        
        let fn_i32_bool2 = ResolvedType::Function {
            param: Box::new(i32_type.clone()),
            ret: Box::new(bool_type.clone()),
            span: Span { start: 10, end: 20 },
        };
        
        let fn_i64_bool = ResolvedType::Function {
            param: Box::new(i64_type.clone()),
            ret: Box::new(bool_type.clone()),
            span: Span { start: 0, end: 0 },
        };
        
        // Same function types are compatible
        assert!(env.check_compatibility(&fn_i32_bool, &fn_i32_bool2));
        
        // Different function types are not compatible
        assert!(!env.check_compatibility(&fn_i32_bool, &fn_i64_bool));
    }
} 