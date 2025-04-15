use crate::definitions::{DefinitionInfo, DefinitionKind};
use crate::error::{ResolutionError, ResolverWarning};
use crate::scopes::ModuleScope;
use crate::types::{
    PrimitiveType, ResolvedArgument, ResolvedDefinitions, ResolvedExpr, ResolvedExprKind,
    ResolvedImpl, ResolvedParameter, ResolvedPattern, ResolvedPatternKind,
    ResolvedType, Symbol,
};
use miette::SourceSpan;
use parallax_syntax::ast::Literal;
use parallax_syntax::ast::{self, expr::Expr, pattern::Pattern};
use parallax_syntax::SyntaxDatabase;
use std::collections::HashMap;

/// Represents information about a local binding (variable).
#[derive(Debug, Clone)]
struct LocalBinding {
    name: String,
    resolved_type: ResolvedType,
    defined_at: SourceSpan, // Span where the variable was defined (e.g., let pattern)
    used: bool,             // Track if the variable has been used
    #[allow(dead_code)]
    is_mutable: bool, // Track whether the binding is mutable
}

/// Stack of scopes for resolving local variables during expression resolution.
#[derive(Debug, Default)]
struct ScopeStack {
    scopes: Vec<HashMap<String, LocalBinding>>,
}

impl ScopeStack {
    pub fn new() -> Self {
        Default::default()
    }
}

impl ScopeStack {
    /// Push a new scope onto the stack (e.g., entering a block).
    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// Pop the current scope from the stack (e.g., leaving a block).
    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    /// Add a new local variable binding to the *current* (innermost) scope.
    fn add_binding(
        &mut self,
        binding: LocalBinding,
        warnings: &mut Vec<ResolverWarning>,
    ) -> Result<(), ResolutionError> {
        if self.scopes.is_empty() {
            // Should not happen if push_scope is called initially
            return Err(ResolutionError::InternalError {
                message: "Attempted to add binding with no active scope".to_string(),
                span: Some(binding.defined_at),
            });
        }

        // Check for shadowing in outer scopes (this is a warning)
        // We need to collect positions first to avoid borrowing issues
        let mut shadowed_span = None;
        for i in (0..self.scopes.len() - 1).rev() {
            if let Some(shadowed) = self.scopes[i].get(&binding.name) {
                shadowed_span = Some(shadowed.defined_at);
                break; // Only report one shadowing warning per variable
            }
        }

        if let Some(span) = shadowed_span {
            warnings.push(ResolverWarning::ShadowedVariable {
                name: binding.name.clone(),
                original_span: span,
                shadow_span: binding.defined_at,
            });
        }

        // Check for same-scope redeclaration (this is an error, not just a warning)
        let last_scope = self.scopes.last_mut().unwrap();
        if let Some(existing) = last_scope.get(&binding.name) {
            return Err(ResolutionError::DuplicateDefinition {
                name: binding.name.clone(),
                span: binding.defined_at,
                previous_span: existing.defined_at,
            });
        }

        last_scope.insert(binding.name.clone(), binding);
        Ok(())
    }

    /// Look up a variable name starting from the innermost scope.
    #[allow(dead_code)]
    fn find_binding(&self, name: &str) -> Option<&LocalBinding> {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.get(name) {
                // Cannot mark used with immutable self. Find needs to be mutable?
                // Or usage tracking happens separately after resolution.
                // Let's assume usage is tracked elsewhere for now.
                return Some(binding);
            }
        }
        None
    }

    /// Look up a variable name starting from the innermost scope and get a mutable reference.
    fn find_binding_mut(&mut self, name: &str) -> Option<&mut LocalBinding> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(binding) = scope.get_mut(name) {
                return Some(binding);
            }
        }
        None
    }
}

/// Main function for Pass 4.
/// Iterates through functions and impl methods defined in Pass 3 and resolves their bodies.
pub fn resolve_bodies<'db>(
    db: &'db dyn SyntaxDatabase,
    definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>,
    module_scopes: &HashMap<Symbol, ModuleScope>,
    prelude_scope: &HashMap<String, Symbol>, // Added prelude scope
    // Note: Takes mutable access to update function bodies
    resolved_defs: &mut ResolvedDefinitions,
    errors: &mut Vec<ResolutionError>,
    warnings: &mut Vec<ResolverWarning>, // Add warnings parameter
) {
    // --- Resolve standalone function bodies ---
    // Clone function symbols first to avoid double mutable borrow issues
    let standalone_function_symbols: Vec<Symbol> = resolved_defs
        .functions
        .iter()
        .filter(|f| {
            // Ensure the function is not part of an impl/trait by checking its DefinitionInfo parent
            definitions_map
                .get(&f.symbol)
                .and_then(|info| info.parent_symbol)
                .and_then(|p_sym| definitions_map.get(&p_sym))
                .map_or(true, |p_info| {
                    !matches!(p_info.kind, DefinitionKind::Trait | DefinitionKind::Impl)
                })
        })
        .map(|f| f.symbol)
        .collect();

    for func_symbol in standalone_function_symbols {
        resolve_single_function_body(
            db,
            definitions_map,
            module_scopes,
            prelude_scope,
            resolved_defs,
            errors,
            warnings,
            func_symbol,
            None,
        );
    }

    // --- Resolve impl method bodies ---
    // Clone impl symbols and their method symbols first to avoid borrow issues
    let impl_method_symbols: Vec<(Symbol, Symbol)> = resolved_defs
        .impls
        .iter()
        .flat_map(|imp| {
            imp.methods
                .iter()
                .map(move |assoc_func| (imp.impl_symbol, assoc_func.func_symbol))
        })
        .collect();

    // Need to find the ResolvedImpl again within the loop, but this is less complex than the double borrow
    for (impl_symbol, method_symbol) in impl_method_symbols {
        // Find the DefinitionInfo for the method to check if it has an AST item.
        let method_def_info = definitions_map.get(&method_symbol);

        // Only proceed if the method has an associated AST item.
        if method_def_info.and_then(|info| info.ast_item).is_some() {
            // Find the impl context (immutable borrow needed just for context)
            // Clone the found impl_def to avoid holding the reference across mutable borrow
            let impl_context = resolved_defs
                .impls
                .iter()
                .find(|imp| imp.impl_symbol == impl_symbol)
                .cloned(); // Clone the Option<ResolvedImpl>

            if impl_context.is_none() {
                errors.push(ResolutionError::InternalError {
                    message: format!(
                        "Impl symbol {} not found during body resolution (pass 2)",
                        impl_symbol.id()
                    ),
                    span: None,
                });
                continue; // Skip this method if impl context not found
            }

            // Resolve the body for this specific method, passing the found context
            resolve_single_function_body(
                db,
                definitions_map,
                module_scopes,
                prelude_scope,
                resolved_defs, // Still mutable for updating the function body
                errors,
                warnings,
                method_symbol,         // Symbol of the function to resolve
                impl_context.as_ref(), // Pass Option<&ResolvedImpl>
            );
        } else {
            // Log or handle methods without AST (e.g., generated methods, or error state)
            println!(
                "DEBUG [resolve_bodies]: Skipping body resolution for method {:?} (no AST item found in DefinitionInfo)",
                method_symbol
            );
        }
    }
}

/// Helper to resolve the body of a single function (standalone or associated).
pub(crate) fn resolve_single_function_body<'db>(
    db: &'db dyn SyntaxDatabase,
    definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>,
    module_scopes: &HashMap<Symbol, ModuleScope>,
    prelude_scope: &HashMap<String, Symbol>, // Added prelude scope
    resolved_defs: &mut ResolvedDefinitions, // Mutable to update body
    errors: &mut Vec<ResolutionError>,
    warnings: &mut Vec<ResolverWarning>,
    func_symbol: Symbol,
    impl_context: Option<&ResolvedImpl>, // Context if this is an impl method
) {
    // Find the function again in the mutable map using its symbol
    if let Some(func_def) = resolved_defs
        .functions
        .iter_mut()
        .find(|f| f.symbol == func_symbol)
    {
        // If the body is already resolved (e.g., due to duplicate processing or error), skip.
        if func_def.body.is_some() {
            return;
        }

        // Find the original AST item (needed for the body AST)
        let func_ast = definitions_map
            .get(&func_symbol)
            .and_then(|def_info| def_info.ast_item)
            .and_then(|item| match &item.kind {
                ast::items::ItemKind::Function(f) => Some(f),
                _ => None,
            })
            .expect("Function AST not found during body resolution");

        // Create the initial scope stack for the function
        let mut scope_stack = ScopeStack::new();
        scope_stack.push_scope(); // Push the top-level scope for parameters

        // Determine the definition context (Trait or Impl) for resolving `Self`
        let definition_context = definitions_map
            .get(&func_symbol)
            .and_then(|def_info| def_info.parent_symbol)
            .and_then(|parent_symbol| definitions_map.get(&parent_symbol))
            .map(|parent_info| (parent_info.kind, parent_info.symbol));

        // Add function parameters to the scope
        println!(
            "DEBUG [resolve_single_function_body]: Adding parameters for func {:?} ({})",
            func_symbol, func_def.name
        );
        for param in &func_def.parameters {
            println!(
                "  -> Param: name='{}', type={:?}, symbol={:?}",
                param.name, param.param_type, param.symbol
            );
            let binding = LocalBinding {
                name: param.name.clone(),
                resolved_type: param.param_type.clone(),
                defined_at: param.span,
                used: false,       // Mark unused initially
                is_mutable: false, // Assume immutable for now (TODO: get mutability from AST pattern)
            };
            // Use checked add_binding which handles shadowing warnings
            if let Err(e) = scope_stack.add_binding(binding, warnings) {
                errors.push(e);
                // Continue adding other params even if one fails?
            }
        }

        // Get the expected return type from the resolved function signature
        let expected_return_type = func_def.return_type.clone();
        let func_module_symbol = func_def.module_symbol; // Module where the function is defined

        // Resolve the body expression using the function's scope stack
        let mut resolved_body_opt: Option<ResolvedExpr> = None; // Store resolved body here
        let mut sub_expr_had_effectful_call = false;
        if let Some(body_ast) = &func_ast.body {
            match resolve_expression(
                db,
                definitions_map,
                module_scopes,
                prelude_scope,      // Pass prelude scope down
                func_module_symbol, // Pass the current function's module symbol
                &mut scope_stack,   // Pass the mutable scope stack
                body_ast,           // Pass the AST expression node (dereferenced Box<Expr>)
                errors,             // Pass down the errors vector
                warnings,           // Pass down the warnings vector
                definition_context, // Pass down definition context
                impl_context,       // Pass down impl context
            ) {
                Ok((resolved_body, encountered_effectful_call_in_sub)) => {
                    // --- Type Check: Function Body vs Return Type ---
                    // Check if the resolved body's type matches the declared return type
                    // Allow compatibility, not just strict equality (e.g., for future subtyping)
                    /* // REMOVED
                    if !types_are_compatible(&expected_return_type, &resolved_body.resolved_type) {
                        errors.push(ResolutionError::TypeMismatch {
                            expected: format!("{:?}", expected_return_type),
                            found: format!("{:?}", resolved_body.resolved_type),
                            span: body_ast.span, // Access span *only* when body_ast exists
                            context: Some(format!("in function '{}' body", func_def.name)),
                        });
                    }
                    */
                    resolved_body_opt = Some(resolved_body); // Store the successfully resolved body
                    sub_expr_had_effectful_call = encountered_effectful_call_in_sub;
                }
                Err(e) => {
                    // Body resolution failed, add the error
                    errors.push(e);
                    // Leave resolved_body_opt as None
                }
            }
        } else {
            // Function has no body (e.g., trait method signature)
            // Handle cases where a body is expected but missing?
            // For now, we assume it's okay (like a trait definition).
            // If expected_return_type is not Unit and there's no body, is that an error?
            // Probably depends on whether it's a trait method decl or not.
            // Pass 3 (resolve_types) should handle trait method signatures correctly.
            // We might need a check here if this function *should* have had a body.
        }

        // --- Check for effectful calls within the resolved body ---
        let mut body_causes_effect = sub_expr_had_effectful_call; // Start with effects from sub-expressions
        if let Some(ref resolved_body) = resolved_body_opt {
            let mut visitor = EffectfulCallVisitor::new(resolved_defs);
            visitor.visit_expr(resolved_body);
            if visitor.found_effectful_call {
                body_causes_effect = true;
                println!(
                    "DEBUG: Body of function {} found to contain effectful call",
                    func_symbol.id()
                );
            }
        }

        // Update the ResolvedFunction with the resolved body and potentially updated effectful flag
        if let Some(func_to_update) = resolved_defs
            .functions
            .iter_mut()
            .find(|f| f.symbol == func_symbol)
        {
            func_to_update.body = resolved_body_opt;
            // Update effectful status: OR existing status with body effect status
            func_to_update.is_effectful |= body_causes_effect;
            if body_causes_effect {
                println!(
                    "DEBUG: Marking function {} as effectful due to body calls.",
                    func_to_update.name
                );
            }
        } else {
            // This indicates an internal inconsistency if the symbol exists but isn't in resolved_defs.functions
            errors.push(ResolutionError::InternalError {
                message: format!(
                    "Failed to find function {} in resolved_defs to update body",
                    func_symbol.id()
                ),
                span: Some(func_ast.span),
            });
        }

        // Check for unused variables in the function's scope
        check_unused_variables(&scope_stack, warnings);

        // Pop the function's main scope
        scope_stack.pop_scope();
    } else {
        errors.push(ResolutionError::InternalError {
            message: format!(
                "Function symbol {} not found in resolved_defs during body resolution",
                func_symbol.id()
            ),
            span: None,
        });
    }
}

/// Check for unused variables in all scopes and generate warnings
fn check_unused_variables(scope_stack: &ScopeStack, warnings: &mut Vec<ResolverWarning>) {
    for scope in &scope_stack.scopes {
        for binding in scope.values() {
            if !binding.used {
                warnings.push(ResolverWarning::UnusedLocalVariable {
                    name: binding.name.clone(),
                    span: binding.defined_at,
                });
            }
        }
    }
}

/// Recursively resolve an AST expression.
// Returns Ok((resolved_expression, encountered_effectful_call_in_sub_expressions))
fn resolve_expression<'db>(
    db: &'db dyn SyntaxDatabase,
    definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>,
    module_scopes: &HashMap<Symbol, ModuleScope>,
    prelude_scope: &HashMap<String, Symbol>,
    current_module_symbol: Symbol,
    scope_stack: &mut ScopeStack,
    ast_expr: &Expr,
    errors: &mut Vec<ResolutionError>,
    warnings: &mut Vec<ResolverWarning>,
    definition_context: Option<(DefinitionKind, Symbol)>,
    impl_context: Option<&ResolvedImpl>,
) -> Result<(ResolvedExpr, bool), ResolutionError> {
    let mut encountered_effectful_call_in_sub = false;
    match &ast_expr.kind {
        ast::expr::ExprKind::Literal(lit) => {
            let resolved_type = match lit {
                // Default integer literals to I32
                ast::common::Literal::Int(_) => ResolvedType::Primitive(PrimitiveType::I32),
                // Default float literals to F64
                ast::common::Literal::Float(_) => ResolvedType::Primitive(PrimitiveType::F64),
                ast::common::Literal::String(_) => ResolvedType::Primitive(PrimitiveType::String),
                ast::common::Literal::Bool(_) => ResolvedType::Primitive(PrimitiveType::Bool),
                ast::common::Literal::Char(_) => ResolvedType::Primitive(PrimitiveType::Char),
            };
            let resolved_expr = ResolvedExpr {
                kind: ResolvedExprKind::Literal(lit.clone()),
                span: ast_expr.span,
                resolved_type,
            };
            Ok((resolved_expr, encountered_effectful_call_in_sub))
        }
        ast::expr::ExprKind::Path(ref path_expr) => {
            if path_expr.is_empty() {
                return Err(ResolutionError::InternalError {
                    message: "Empty path expression encountered".to_string(),
                    span: Some(ast_expr.span),
                });
            }

            // Handle single-segment paths (potential variables or simple paths)
            if path_expr.len() == 1 {
                let ident = &path_expr[0];
                let name = &ident.name;

                // DEBUG LOG: Check local scope first
                // println!(
                //     "DEBUG [resolve_expression Path]: Looking up identifier '{}'",
                //     name
                // );
                if let Some(binding) = scope_stack.find_binding_mut(name) {
                    // Use find_binding_mut to mark used
                    // println!(
                    //     "  -> Found '{}' in local scope stack. Type: {:?}",
                    //     name, binding.resolved_type
                    // );
                    binding.used = true; // Mark as used
                    let resolved_type = binding.resolved_type.clone();
                    // TODO: How to represent a local variable access in ResolvedExprKind?
                    // Maybe just Path(Symbol) where Symbol is the *parameter/binding* symbol?
                    // For now, let's use a placeholder or figure out the correct Symbol.
                    // Using the definition symbol temporarily, but this needs correction.
                    let binding_symbol = Symbol::fresh(); // Placeholder - FIND CORRECT SYMBOL LATER
                    return Ok((
                        ResolvedExpr {
                            kind: ResolvedExprKind::Path(binding_symbol), // Placeholder symbol
                            span: ast_expr.span,
                            resolved_type,
                        },
                        false,
                    )); // Assume local var access is not diverging
                }
                println!("  -> '{}' not found in local scope stack. Trying module/prelude path resolution.", name);
            }

            // If not found locally OR it's a multi-segment path, resolve as module/prelude path
            match crate::resolve_types::resolve_path(
                definitions_map,
                module_scopes,
                prelude_scope,
                current_module_symbol,
                path_expr, // Use path_expr directly
                ast_expr.span,
            ) {
                Ok(resolved_symbol) => {
                    // Path resolved to a definition (enum variant, function, constant etc.)
                    // Determine the type of the resolved symbol
                    let resolved_type = get_symbol_type(definitions_map, resolved_symbol);
                    Ok((
                        ResolvedExpr {
                            kind: ResolvedExprKind::Path(resolved_symbol),
                            span: ast_expr.span,
                            resolved_type,
                        },
                        false,
                    )) // Assume path resolution doesn't diverge
                }
                Err(e) => Err(e), // Propagate path resolution error
            }
        }
        ast::expr::ExprKind::Block(items) => {
            scope_stack.push_scope();
            let mut resolved_exprs = Vec::new();
            let mut _diverges = false; // Prefix with _ // Default unless sub-pattern/expr diverges
            let mut last_type = ResolvedType::Primitive(PrimitiveType::Unit);

            for (i, expr) in items.iter().enumerate() {
                let is_last = i == items.len() - 1;
                let (resolved_inner_expr, inner_diverges) = resolve_expression(
                    db, definitions_map, module_scopes, prelude_scope, current_module_symbol, scope_stack, expr, errors, warnings, definition_context, impl_context
                )?;
                resolved_exprs.push(resolved_inner_expr.clone()); // Clone needed here
                if inner_diverges { _diverges = true; }

                if is_last {
                    last_type = resolved_inner_expr.resolved_type;
                } else {
                    // Checking if non-last statements are Unit belongs in type inference
                    /* // REMOVED
                    if !matches!(resolved_inner_expr.kind, ResolvedExprKind::Let { .. }) {
                        if !types_are_compatible(
                            &resolved_inner_expr.resolved_type,
                            &ResolvedType::Primitive(PrimitiveType::Unit),
                        ) {
                            // warnings.push(ResolverWarning::NonUnitStatement { span: resolved_inner_expr.span });
                        }
                    }
                    */
                }
            }

            scope_stack.pop_scope();
            let resolved_expr = ResolvedExpr {
                kind: ResolvedExprKind::Block(resolved_exprs),
                span: ast_expr.span,
                resolved_type: last_type,
            };
            Ok((resolved_expr, encountered_effectful_call_in_sub))
        }
        ast::expr::ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let (resolved_condition, condition_effectful) = resolve_expression(
                db,
                definitions_map,
                module_scopes,
                prelude_scope,
                current_module_symbol,
                scope_stack,
                condition,
                errors,
                warnings,
                definition_context,
                impl_context,
            )?;
            let (resolved_then, then_effectful) = resolve_expression(
                db,
                definitions_map,
                module_scopes,
                prelude_scope,
                current_module_symbol,
                scope_stack,
                then_branch,
                errors,
                warnings,
                definition_context,
                impl_context,
            )?;
            encountered_effectful_call_in_sub |= condition_effectful | then_effectful;

            let mut resolved_else = None;
            if let Some(else_expr) = else_branch {
                let (resolved_else_inner, else_effectful) = resolve_expression(
                    db,
                    definitions_map,
                    module_scopes,
                    prelude_scope,
                    current_module_symbol,
                    scope_stack,
                    else_expr,
                    errors,
                    warnings,
                    definition_context,
                    impl_context,
                )?;
                resolved_else = Some(Box::new(resolved_else_inner));
                encountered_effectful_call_in_sub |= else_effectful;
            }

            // TODO: Type check if condition is bool, and unify branch types
            let result_type = resolved_then.resolved_type.clone(); // Placeholder

            let resolved_expr = ResolvedExpr {
                kind: ResolvedExprKind::If {
                    condition: Box::new(resolved_condition),
                    then_branch: Box::new(resolved_then),
                    else_branch: resolved_else,
                },
                span: ast_expr.span,
                resolved_type: result_type,
            };
            Ok((resolved_expr, encountered_effectful_call_in_sub))
        }
        ast::expr::ExprKind::Let {
            pattern,
            type_ann,
            value,
        } => {
            // First, resolve the value expression
            let (resolved_value, value_diverges) = resolve_expression(
                db,
                definitions_map,
                module_scopes,
                prelude_scope,
                current_module_symbol,
                scope_stack,
                value,
                errors,
                warnings,
                definition_context,
                impl_context,
            )?;
            let value_type = resolved_value.resolved_type.clone();
            // Initialize diverges based on the value expression
            let mut diverges = value_diverges;

            // Resolve the type annotation if it exists
            let resolved_annotation_opt: Option<ResolvedType> = if let Some(type_ann) = type_ann {
                // Qualify the call to resolve_type
                let resolved_type = crate::resolve_types::resolve_type(
                    db,
                    definitions_map,
                    module_scopes,
                    prelude_scope,
                    current_module_symbol,
                    type_ann,
                    &scope_stack.scopes.last().map_or(Vec::new(), |s| s.keys().cloned().collect()), // Pass current generic scope names
                    definition_context,
                    type_ann.span,
                )?;
                Some(resolved_type)
            } else {
                None
            };

            // Determine the type expected for the pattern
            let expected_pattern_type = resolved_annotation_opt.clone().unwrap_or(value_type);

            // Resolve the pattern and bind variables
            let (resolved_pattern, pattern_diverges) = resolve_pattern(
                db, definitions_map, module_scopes, prelude_scope, current_module_symbol, scope_stack, pattern, &expected_pattern_type, errors, warnings, definition_context, impl_context
            )?;
            // Combine divergence information
            diverges |= pattern_diverges;

            let resolved_expr = ResolvedExpr {
                kind: ResolvedExprKind::Let {
                    pattern: resolved_pattern,
                    value: Box::new(resolved_value),
                    // Use the correctly scoped Option<ResolvedType>
                    type_annotation: resolved_annotation_opt,
                },
                span: ast_expr.span,
                resolved_type: ResolvedType::Primitive(PrimitiveType::Unit), // Let expressions evaluate to Unit
            };
            Ok((resolved_expr, diverges))
        }
        ast::expr::ExprKind::Binary { left, op, right } => {
            let (resolved_left, left_effectful) = resolve_expression(
                db,
                definitions_map,
                module_scopes,
                prelude_scope,
                current_module_symbol,
                scope_stack,
                left,
                errors,
                warnings,
                definition_context,
                impl_context,
            )?;
            let (resolved_right, right_effectful) = resolve_expression(
                db,
                definitions_map,
                module_scopes,
                prelude_scope,
                current_module_symbol,
                scope_stack,
                right,
                errors,
                warnings,
                definition_context,
                impl_context,
            )?;
            encountered_effectful_call_in_sub |= left_effectful | right_effectful;

            // TODO: Type check the binary operation based on op, resolved_left.resolved_type, and resolved_right.resolved_type
            // For now, placeholder type is Unknown or left's type
            let result_type = resolved_left.resolved_type.clone(); // Placeholder

            let resolved_expr = ResolvedExpr {
                kind: ResolvedExprKind::Binary {
                    left: Box::new(resolved_left),
                    op: *op,
                    right: Box::new(resolved_right),
                },
                span: ast_expr.span,
                resolved_type: result_type, // Placeholder
            };
            Ok((resolved_expr, encountered_effectful_call_in_sub))
        }
        ast::expr::ExprKind::Unary { op, expr } => {
            let (resolved_operand, operand_effectful) = resolve_expression(
                db,
                definitions_map,
                module_scopes,
                prelude_scope,
                current_module_symbol,
                scope_stack,
                expr,
                errors,
                warnings,
                definition_context,
                impl_context,
            )?;
            encountered_effectful_call_in_sub |= operand_effectful;

            // TODO: Type check unary operation
            let result_type = resolved_operand.resolved_type.clone(); // Placeholder

            let resolved_expr = ResolvedExpr {
                kind: ResolvedExprKind::Unary {
                    op: op.clone(), // Clone the operator
                    expr: Box::new(resolved_operand),
                },
                span: ast_expr.span,
                resolved_type: result_type, // Placeholder
            };
            Ok((resolved_expr, encountered_effectful_call_in_sub))
        }
        ast::expr::ExprKind::Call { func, args } => {
            // Resolve the function part first
            let (resolved_func_expr, func_expr_effectful) = resolve_expression(
                db,
                definitions_map,
                module_scopes,
                prelude_scope,
                current_module_symbol,
                scope_stack,
                func, // Resolve the 'func' part of the call
                errors,
                warnings,
                definition_context,
                impl_context,
            )?;

            // Resolve the arguments
            let mut resolved_args = Vec::new();
            let mut args_effectful = false;
            for arg in args {
                let (resolved_value, arg_effectful) = resolve_expression(
                    db,
                    definitions_map,
                    module_scopes,
                    prelude_scope,
                    current_module_symbol,
                    scope_stack,
                    &arg.value,
                    errors,
                    warnings,
                    definition_context,
                    impl_context,
                )?;
                args_effectful |= arg_effectful;
                resolved_args.push(ResolvedArgument {
                    name: arg.name.as_ref().map(|ident| ident.name.clone()),
                    value: resolved_value,
                    span: arg.span,
                });
            }

            // Determine the overall effectfulness
            let encountered_effectful_call_in_sub = func_expr_effectful | args_effectful;

            // Determine the type - Placeholder for now
            // TODO: Proper function type resolution/inference needed here
            let return_type = ResolvedType::Unknown;

            // Extract the function symbol if the resolved function expression is a simple path.
            let func_symbol_opt: Option<Symbol> = match &resolved_func_expr.kind {
                ResolvedExprKind::Path(sym) => Some(*sym),
                // TODO: Handle calls on other expressions (e.g., closures stored in variables)
                _ => None,
            };

            // Create a generic Call expression kind.
            // The type checker will later inspect resolved_func_expr.kind
            // to see if it was a Field access (indicating a method call).
            let resolved_expr = ResolvedExpr {
                kind: ResolvedExprKind::Call {
                    // Use the extracted symbol or None
                    func_symbol: func_symbol_opt,
                    args: resolved_args,
                },
                span: ast_expr.span,
                resolved_type: return_type, // Placeholder
            };
            Ok((resolved_expr, encountered_effectful_call_in_sub))
        }
        ast::expr::ExprKind::Field { object, name } => {
            // Resolve the object expression
            let (resolved_object, object_effectful) = resolve_expression(
                db,
                definitions_map,
                module_scopes,
                prelude_scope,
                current_module_symbol,
                scope_stack,
                object, // Resolve the 'object' part
                errors,
                warnings,
                definition_context,
                impl_context,
            )?;
            let encountered_effectful_call_in_sub = object_effectful;

            // Create a Field expression kind. Type is Unknown until type checking.
            let resolved_expr = ResolvedExpr {
                kind: ResolvedExprKind::Field {
                    object: Box::new(resolved_object),
                    field_name: name.name.clone(),
                },
                span: ast_expr.span,
                // The type of the field is unknown until type checking resolves it based on the object's type
                resolved_type: ResolvedType::Unknown,
            };
            Ok((resolved_expr, encountered_effectful_call_in_sub))
        }
        ast::expr::ExprKind::Struct {
            path: ast_path,
            fields,
            base,
        } => {
            match crate::resolve_types::resolve_path(
                definitions_map,
                module_scopes,
                prelude_scope,
                current_module_symbol,
                ast_path,
                ast_expr.span,
            ) {
                Ok(struct_symbol) => {
                    let mut resolved_ast_fields = Vec::new();
                    for (ident, expr) in fields {
                        let (resolved_expr, field_effectful) = resolve_expression(
                            db,
                            definitions_map,
                            module_scopes,
                            prelude_scope,
                            current_module_symbol,
                            scope_stack,
                            expr,
                            errors,
                            warnings,
                            definition_context,
                            impl_context,
                        )?;
                        encountered_effectful_call_in_sub |= field_effectful;
                        resolved_ast_fields.push((ident.name.clone(), resolved_expr));
                    }
                    let mut resolved_base = None;
                    if let Some(base_expr) = base {
                        let (resolved_base_expr, base_effectful) = resolve_expression(
                            db,
                            definitions_map,
                            module_scopes,
                            prelude_scope,
                            current_module_symbol,
                            scope_stack,
                            base_expr,
                            errors,
                            warnings,
                            definition_context,
                            impl_context,
                        )?;
                        resolved_base = Some(Box::new(resolved_base_expr));
                        encountered_effectful_call_in_sub |= base_effectful;
                    }

                    let struct_type = ResolvedType::UserDefined {
                        symbol: struct_symbol,
                        type_args: None,
                    };
                    let resolved_expr = ResolvedExpr {
                        kind: ResolvedExprKind::Struct {
                            struct_symbol,
                            fields: resolved_ast_fields,
                            base: resolved_base,
                        },
                        span: ast_expr.span,
                        resolved_type: struct_type,
                    };
                    Ok((resolved_expr, encountered_effectful_call_in_sub))
                }
                Err(e) => Err(e),
            }
        }
        ast::expr::ExprKind::Array(elements) => {
            let mut resolved_elements = Vec::new();
            let mut _element_type = ResolvedType::Unknown; // Prefix with _
            for (i, elem) in elements.iter().enumerate() {
                let (resolved_elem, elem_effectful) = resolve_expression(
                    db,
                    definitions_map,
                    module_scopes,
                    prelude_scope,
                    current_module_symbol,
                    scope_stack,
                    elem,
                    errors,
                    warnings,
                    definition_context,
                    impl_context,
                )?;
                encountered_effectful_call_in_sub |= elem_effectful;
                if i == 0 {
                    _element_type = resolved_elem.resolved_type.clone(); // Prefix with _
                } else {
                    // TODO: Unify subsequent element types with the first one
                    // if !check_types_equal(&resolved_elem.resolved_type, &element_type) {
                }
                resolved_elements.push(resolved_elem);
            }
            let array_type = ResolvedType::Array {
                element_type: Box::new(_element_type),
                size: Some(elements.len()),
            };
            let resolved_expr = ResolvedExpr {
                kind: ResolvedExprKind::Array(resolved_elements),
                span: ast_expr.span,
                resolved_type: array_type,
            };
            Ok((resolved_expr, encountered_effectful_call_in_sub))
        }
        ast::expr::ExprKind::Tuple(elements) => {
            let mut resolved_elements = Vec::new();
            let mut _element_types = Vec::new(); // Prefix with _
            for elem in elements {
                let (resolved_elem, elem_effectful) = resolve_expression(
                    db,
                    definitions_map,
                    module_scopes,
                    prelude_scope,
                    current_module_symbol,
                    scope_stack,
                    elem,
                    errors,
                    warnings,
                    definition_context,
                    impl_context,
                )?;
                encountered_effectful_call_in_sub |= elem_effectful;
                _element_types.push(resolved_elem.resolved_type.clone()); // Prefix with _
                resolved_elements.push(resolved_elem);
            }
            let tuple_type = ResolvedType::Tuple(_element_types);
            let resolved_expr = ResolvedExpr {
                kind: ResolvedExprKind::Tuple(resolved_elements),
                span: ast_expr.span,
                resolved_type: tuple_type,
            };
            Ok((resolved_expr, encountered_effectful_call_in_sub))
        }
        ast::expr::ExprKind::Map(entries) => {
            let mut resolved_entries = Vec::new();
            let mut _key_type = ResolvedType::Unknown; // Prefix with _
            let mut _value_type = ResolvedType::Unknown; // Prefix with _
            for (i, (key, value)) in entries.iter().enumerate() {
                let (resolved_key, key_effectful) = resolve_expression(
                    db,
                    definitions_map,
                    module_scopes,
                    prelude_scope,
                    current_module_symbol,
                    scope_stack,
                    key,
                    errors,
                    warnings,
                    definition_context,
                    impl_context,
                )?;
                let (resolved_value, value_effectful) = resolve_expression(
                    db,
                    definitions_map,
                    module_scopes,
                    prelude_scope,
                    current_module_symbol,
                    scope_stack,
                    value,
                    errors,
                    warnings,
                    definition_context,
                    impl_context,
                )?;
                encountered_effectful_call_in_sub |= key_effectful | value_effectful;
                if i == 0 {
                    _key_type = resolved_key.resolved_type.clone(); // Prefix with _
                    _value_type = resolved_value.resolved_type.clone(); // Prefix with _
                } else {
                    // TODO: Unify subsequent key/value types with the first one
                    // if !check_types_equal(&resolved_key.resolved_type, &key_type) {
                }
                resolved_entries.push((resolved_key, resolved_value));
            }
            let map_type = ResolvedType::Unknown;
            let resolved_expr = ResolvedExpr {
                kind: ResolvedExprKind::Map(resolved_entries),
                span: ast_expr.span,
                resolved_type: map_type,
            };
            Ok((resolved_expr, encountered_effectful_call_in_sub))
        }
        ast::expr::ExprKind::HashSet(elements) => {
            let mut resolved_elements = Vec::new();
            let mut _element_type = ResolvedType::Unknown; // Prefix with _
            for (i, elem) in elements.iter().enumerate() {
                let (resolved_elem, elem_effectful) = resolve_expression(
                    db,
                    definitions_map,
                    module_scopes,
                    prelude_scope,
                    current_module_symbol,
                    scope_stack,
                    elem,
                    errors,
                    warnings,
                    definition_context,
                    impl_context,
                )?;
                encountered_effectful_call_in_sub |= elem_effectful;
                if i == 0 {
                    _element_type = resolved_elem.resolved_type.clone(); // Prefix with _
                } else {
                    // TODO: Unify subsequent element types with the first one
                    // if !check_types_equal(&resolved_elem.resolved_type, &element_type) {
                }
                resolved_elements.push(resolved_elem);
            }
            let set_type = ResolvedType::Unknown;
            let resolved_expr = ResolvedExpr {
                kind: ResolvedExprKind::HashSet(resolved_elements),
                span: ast_expr.span,
                resolved_type: set_type,
            };
            Ok((resolved_expr, encountered_effectful_call_in_sub))
        }
        ast::expr::ExprKind::Paren(expr) => {
            // --- Parenthesized Expression ---
            // Resolve the inner expression and propagate its type and value.
            let (resolved_inner, inner_effectful) = resolve_expression(
                db,
                definitions_map,
                module_scopes,
                prelude_scope,
                current_module_symbol,
                scope_stack,
                expr,
                errors,
                warnings,
                definition_context,
                impl_context,
            )?;
            encountered_effectful_call_in_sub |= inner_effectful;
            let resolved_expr = ResolvedExpr {
                kind: ResolvedExprKind::Paren(Box::new(resolved_inner.clone())),
                span: ast_expr.span,
                resolved_type: resolved_inner.resolved_type,
            };
            Ok((resolved_expr, encountered_effectful_call_in_sub))
        }
        ast::expr::ExprKind::Match { scrutinee, arms } => {
            // println!("DEBUG [resolve_expression Match]: Resolving match expression...");
            let (resolved_scrutinee, _) = resolve_expression(
                db,
                definitions_map,
                module_scopes,
                prelude_scope,
                current_module_symbol,
                scope_stack,
                scrutinee,
                errors,
                warnings,
                definition_context,
                impl_context,
            )?;
            let scrutinee_type = resolved_scrutinee.resolved_type.clone();
            // println!("  -> Scrutinee type: {:?}", scrutinee_type);

            let mut resolved_arms = Vec::new();
            let mut arm_types = Vec::new();
            let mut combined_divergence = true; // Assume divergence unless an arm doesn't diverge

            for (ast_pattern, ast_arm_expr) in arms {
                println!(
                    "  -> Resolving match arm with pattern: {:?}",
                    ast_pattern.kind
                );
                // 1. Push a new scope for this arm's bindings
                scope_stack.push_scope();
                println!("    -> Pushed scope for arm.");

                // 2. Resolve the pattern for this arm, passing the scrutinee's type as expected
                //    This will add bindings from the pattern to the scope_stack.
                let (resolved_pattern, _) = resolve_pattern(
                    db,
                    definitions_map,
                    module_scopes,
                    prelude_scope,
                    current_module_symbol,
                    scope_stack, // Pass mutable scope_stack
                    ast_pattern,
                    &scrutinee_type,
                    errors,
                    warnings,
                    definition_context,
                    impl_context,
                )?;
                println!("    -> Resolved pattern: {:?}", resolved_pattern.kind);

                // 3. Resolve the expression on the right side of the arm
                //    This resolution happens within the scope created for the arm.
                println!("    -> Resolving arm expression...");
                let (resolved_arm_expr, arm_diverges) = resolve_expression(
                    db,
                    definitions_map,
                    module_scopes,
                    prelude_scope,
                    current_module_symbol,
                    scope_stack, // Pass scope_stack again
                    ast_arm_expr,
                    errors,
                    warnings,
                    definition_context,
                    impl_context,
                )?;
                println!(
                    "    -> Resolved arm expression. Type: {:?}, Diverges: {}",
                    resolved_arm_expr.resolved_type, arm_diverges
                );

                // 4. Pop the scope created for this arm
                scope_stack.pop_scope();
                println!("    -> Popped scope for arm.");

                arm_types.push(resolved_arm_expr.resolved_type.clone());
                resolved_arms.push((resolved_pattern, resolved_arm_expr));
                combined_divergence &= arm_diverges; // If any arm doesn't diverge, the match doesn't
            }

            // TODO: Check exhaustiveness of patterns.
            // TODO: Determine the overall type of the match expression.
            // This usually involves finding the least upper bound (LUB) or common supertype
            // of all arm expression types. For now, use the type of the first arm or Unknown.
            let match_type = arm_types.first().cloned().unwrap_or(ResolvedType::Unknown);
            println!(
                "  -> Determined match expression type: {:?} (Simple: first arm/unknown)",
                match_type
            );

            Ok((
                ResolvedExpr {
                    kind: ResolvedExprKind::Match {
                        scrutinee: Box::new(resolved_scrutinee),
                        arms: resolved_arms,
                    },
                    span: ast_expr.span,
                    resolved_type: match_type, // Placeholder type
                },
                combined_divergence,
            ))
        }
        ast::expr::ExprKind::Lambda {
            generic_params: _,
            params,
            body,
        } => {
            scope_stack.push_scope();
            fn extract_param_name(pattern: &ast::pattern::Pattern) -> String {
                match &pattern.kind {
                    ast::pattern::PatternKind::Identifier(ident) => ident.name.clone(),
                    _ => "_pattern_".to_string(),
                }
            }
            let mut resolved_params = Vec::new();
            let _lambda_generic_params: Vec<crate::types::ResolvedGenericParamDef> = Vec::new();
            for param in params {
                let param_name = extract_param_name(&param.pattern);
                let param_type = match &param.ty {
                    Some(ty) => crate::resolve_types::resolve_type(
                        db,
                        definitions_map,
                        module_scopes,
                        prelude_scope,
                        current_module_symbol,
                        ty,
                        &[],
                        definition_context,
                        ty.span,
                    )?,
                    None => ResolvedType::Unknown,
                };

                // Generate a symbol for the lambda parameter
                let param_symbol = Symbol::fresh();

                let binding = LocalBinding {
                    name: param_name.clone(),
                    resolved_type: param_type.clone(),
                    defined_at: param.span,
                    used: false,
                    is_mutable: false, // Assuming lambda params are immutable for now
                };
                if let Err(e) = scope_stack.add_binding(binding, warnings) {
                    errors.push(e);
                }
                resolved_params.push(ResolvedParameter {
                    symbol: param_symbol, // Use the generated symbol
                    name: param_name,
                    param_type,
                    is_variadic: param.is_variadic,
                    has_default: param.default_value.is_some(),
                    span: param.span,
                });
            }
            let (resolved_body, body_effectful) = resolve_expression(
                db,
                definitions_map,
                module_scopes,
                prelude_scope,
                current_module_symbol,
                scope_stack,
                body,
                errors,
                warnings,
                definition_context,
                impl_context,
            )?;
            scope_stack.pop_scope();
            let lambda_type = ResolvedType::Function {
                param_types: resolved_params
                    .iter()
                    .map(|p| p.param_type.clone())
                    .collect(),
                return_type: Box::new(resolved_body.resolved_type.clone()),
            };
            let resolved_expr = ResolvedExpr {
                kind: ResolvedExprKind::Lambda {
                    params: resolved_params,
                    body: Box::new(resolved_body),
                },
                span: ast_expr.span,
                resolved_type: lambda_type,
            };
            Ok((resolved_expr, body_effectful))
        }
        _ => {
            // Placeholder for unhandled expression kinds
            errors.push(ResolutionError::InternalError {
                message: format!(
                    "Expression resolution not implemented for {:?}",
                    ast_expr.kind
                ),
                span: Some(ast_expr.span),
            });
            Ok((
                ResolvedExpr {
                    kind: ResolvedExprKind::Path(Symbol(0)), // Placeholder for error
                    span: ast_expr.span,
                    resolved_type: ResolvedType::Unknown,
                },
                false, // Assume false if unhandled
            ))
        }
    }
}

/// Helper function to get the type of a symbol
fn get_symbol_type(
    definitions_map: &HashMap<Symbol, DefinitionInfo>,
    symbol: Symbol,
) -> ResolvedType {
    if let Some(def_info) = definitions_map.get(&symbol) {
        match def_info.kind {
            DefinitionKind::Function => {
                // Get function signature information
                // TODO: Look up the actual ResolvedFunction signature
                if let Some(ast::items::ItemKind::Function(_)) =
                    def_info.ast_item.map(|item| &item.kind)
                {
                    // Simple function type for now
                    // Can be enhanced later to include full parameter and return types
                    ResolvedType::Function {
                        param_types: vec![], // Simplified for now
                        return_type: Box::new(ResolvedType::Primitive(PrimitiveType::Unit)),
                    }
                } else {
                    ResolvedType::Unknown
                }
            }
            DefinitionKind::Struct => {
                ResolvedType::UserDefined {
                    symbol,
                    type_args: None, // TODO: Handle generics
                }
            }
            DefinitionKind::Enum => {
                ResolvedType::UserDefined {
                    symbol,
                    type_args: None, // TODO: Handle generics
                }
            }
            DefinitionKind::EnumVariant => {
                // The type of an enum variant constructor depends on the variant kind.
                // Unit -> EnumType
                // Tuple(T1, T2) -> fn(T1, T2) -> EnumType
                // Struct { f1: T1 } -> fn(...) -> EnumType (needs proper function type)
                // TODO: This requires looking up the parent enum's resolved type
                // and the variant structure. Complex, return Unknown for now.
                ResolvedType::Unknown
            }
            DefinitionKind::Module => ResolvedType::Unknown, // Modules are not types
            DefinitionKind::Trait => {
                // Traits themselves don't have a value/type in expressions
                // Although they can be used as bounds, they are not directly usable as a type
                // TODO: Perhaps introduce a specific ResolvedType::Trait(symbol) later?
                ResolvedType::Unknown
            }
            DefinitionKind::Impl => {
                // Impl blocks don't represent a type that can be named in an expression
                ResolvedType::Unknown
            }
        }
    } else {
        ResolvedType::Unknown // Symbol not found
    }
}

/// Recursively resolves an AST pattern, adding bindings to the scope stack.
fn resolve_pattern<'db>(
    db: &'db dyn SyntaxDatabase,
    definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>,
    module_scopes: &HashMap<Symbol, ModuleScope>,
    prelude_scope: &HashMap<String, Symbol>,
    current_module_symbol: Symbol,
    scope_stack: &mut ScopeStack,
    ast_pattern: &Pattern,
    expected_type: &ResolvedType,
    errors: &mut Vec<ResolutionError>,
    warnings: &mut Vec<ResolverWarning>,
    definition_context: Option<(DefinitionKind, Symbol)>,
    impl_context: Option<&ResolvedImpl>,
) -> Result<(ResolvedPattern, bool), ResolutionError> {
    println!(
        "DEBUG [resolve_pattern]: Resolving pattern {:?} against expected type {:?}",
        ast_pattern.kind, expected_type
    );
    let span = ast_pattern.span;
    let mut is_diverging = false; // Default unless sub-pattern/expr diverges

    let resolved_kind = match &ast_pattern.kind {
        ast::pattern::PatternKind::Identifier(ident) => {
            let name = ident.name.clone();
            println!("  -> PatternKind::Identifier: '{}'", name);

            // Create a local binding for this identifier.
            // The type of the binding is the type expected for this pattern position.
            let binding = LocalBinding {
                name: name.clone(),
                resolved_type: expected_type.clone(), // Variable gets the type of the pattern
                defined_at: ident.span,
                used: false,       // Mark unused initially
                is_mutable: false, // TODO: Determine mutability (e.g., `let mut x`)
            };

            println!(
                "    -> Adding binding: name='{}', type={:?}",
                name, binding.resolved_type
            );
            // Add the binding to the *current* scope.
            // `add_binding` handles duplicate checks/shadowing warnings.
            if let Err(e) = scope_stack.add_binding(binding, warnings) {
                errors.push(e);
                // If adding binding fails, should we error out? Let's continue for now.
            }

            ResolvedPatternKind::Identifier(name)
        }

        ast::pattern::PatternKind::Literal(lit) => {
            ResolvedPatternKind::Literal(lit.clone())
        }

        ast::pattern::PatternKind::PathPattern { path } => {
            // e.g., List::Nil
            println!("  -> PatternKind::PathPattern: {:?}", path);
            // 1. Resolve the path
            match crate::resolve_types::resolve_path(
                definitions_map,
                module_scopes,
                prelude_scope,
                current_module_symbol,
                path,
                span,
            ) {
                Ok(resolved_symbol) => {
                    // 2. Verify it's an EnumVariant
                    if let Some(def_info) = definitions_map.get(&resolved_symbol) {
                        if def_info.kind != DefinitionKind::EnumVariant {
                            errors.push(ResolutionError::PatternMismatch {
                                message: format!(
                                    "Path pattern resolved to {:?}, expected an enum variant.",
                                    def_info.kind
                                ),
                                span,
                                help: None,
                            });
                            // Return placeholder on error to allow parsing rest of file
                            return Ok((
                                ResolvedPattern {
                                    kind: ResolvedPatternKind::Wildcard,
                                    span,
                                    resolved_type: expected_type.clone(),
                                },
                                false,
                            ));
                        }

                        // 3. Check if variant belongs to the expected enum type
                        if let ResolvedType::UserDefined {
                            symbol: expected_enum_symbol,
                            ..
                        } = expected_type
                        {
                            if def_info.parent_symbol != Some(*expected_enum_symbol) {
                                errors.push(ResolutionError::TypeMismatch {
                                    expected: format!("Variant of enum {:?}", expected_type),
                                    found: format!("Variant of enum {:?}", def_info.parent_symbol),
                                    span,
                                    context: Some("in path pattern".to_string()),
                                });
                                return Ok((
                                    ResolvedPattern {
                                        kind: ResolvedPatternKind::Wildcard,
                                        span,
                                        resolved_type: expected_type.clone(),
                                    },
                                    false,
                                ));
                            }
                        } else {
                            errors.push(ResolutionError::TypeMismatch {
                                expected: "Enum type".to_string(),
                                found: format!("{:?}", expected_type),
                                span,
                                context: Some("for path pattern matching".to_string()),
                            });
                            return Ok((
                                ResolvedPattern {
                                    kind: ResolvedPatternKind::Wildcard,
                                    span,
                                    resolved_type: expected_type.clone(),
                                },
                                false,
                            ));
                        }

                        // 4. Verify it's a Unit variant (no arguments expected)
                        if !matches!(
                            def_info.variant_kind,
                            Some(crate::definitions::VariantKind::Unit)
                        ) {
                            errors.push(ResolutionError::PatternMismatch {
                                message: format!("Enum variant '{}' expects arguments, but none were provided in path pattern.", def_info.name),
                                span,
                                help: Some("Use `Variant(...)` pattern instead?".to_string()),
                            });
                            return Ok((
                                ResolvedPattern {
                                    kind: ResolvedPatternKind::Wildcard,
                                    span,
                                    resolved_type: expected_type.clone(),
                                },
                                false,
                            ));
                        }

                        // 5. Success: Return constructor pattern with no args
                        println!(
                            "    -> Resolved PathPattern to Unit variant: {:?}",
                            resolved_symbol
                        );
                        ResolvedPatternKind::Constructor {
                            symbol: resolved_symbol,
                            // Represent args of unit variant as empty tuple pattern
                            args: Box::new(ResolvedPattern {
                                kind: ResolvedPatternKind::Tuple(vec![]),
                                span, // Use pattern span for the implicit tuple
                                resolved_type: ResolvedType::Tuple(vec![]),
                            }),
                        }
                    } else {
                        // Symbol resolved but not in map - Internal Error
                        errors.push(ResolutionError::InternalError {
                            message: "Resolved path symbol not found in definitions map"
                                .to_string(),
                            span: Some(span),
                        });
                        ResolvedPatternKind::Wildcard // Placeholder
                    }
                }
                Err(e) => {
                    errors.push(e);
                    ResolvedPatternKind::Wildcard // Placeholder on resolution failure
                }
            }
        }

        ast::pattern::PatternKind::ConstructorWithArgs { path, args } => {
            println!(
                "  -> PatternKind::ConstructorWithArgs: {:?}({:?})",
                path, args
            );
            // 1. Resolve path to variant symbol
            match crate::resolve_types::resolve_path(
                definitions_map,
                module_scopes,
                prelude_scope,
                current_module_symbol,
                path,
                span,
            ) {
                Ok(resolved_symbol) => {
                    if let Some(def_info) = definitions_map.get(&resolved_symbol) {
                        if def_info.kind != DefinitionKind::EnumVariant {
                            errors.push(ResolutionError::PatternMismatch {
                                message: format!("Unit variant '{}' used with arguments in pattern.", def_info.name),
                                span,
                                help: Some("Unit variants cannot be matched with arguments. Use just the variant name.".to_string()),
                            });
                            return Ok((
                                ResolvedPattern {
                                    kind: ResolvedPatternKind::Wildcard,
                                    span,
                                    resolved_type: expected_type.clone(),
                                },
                                false,
                            ));
                        }
                        if let ResolvedType::UserDefined {
                            symbol: expected_enum_symbol,
                            ..
                        } = expected_type
                        {
                            if def_info.parent_symbol != Some(*expected_enum_symbol) {
                                errors.push(ResolutionError::TypeMismatch {
                                    expected: format!("Variant of enum {:?}", expected_type),
                                    found: format!("Variant of enum {:?}", def_info.parent_symbol),
                                    span,
                                    context: Some("in constructor pattern".to_string()),
                                });
                                return Ok((
                                    ResolvedPattern {
                                        kind: ResolvedPatternKind::Wildcard,
                                        span,
                                        resolved_type: expected_type.clone(),
                                    },
                                    false,
                                ));
                            }
                        } else {
                            errors.push(ResolutionError::TypeMismatch {
                                expected: "Enum type".to_string(),
                                found: format!("{:?}", expected_type),
                                span,
                                context: Some("for constructor pattern matching".to_string()),
                            });
                            return Ok((
                                ResolvedPattern {
                                    kind: ResolvedPatternKind::Wildcard,
                                    span,
                                    resolved_type: expected_type.clone(),
                                },
                                false,
                            ));
                        }

                        // 3. Get expected argument types from variant definition
                        let expected_arg_types_ast = match &def_info.variant_kind {
                            Some(crate::definitions::VariantKind::Tuple(types)) => types.clone(),
                            Some(crate::definitions::VariantKind::Struct(_)) => {
                                // TODO: Handle struct variants properly - need named field patterns
                                errors.push(ResolutionError::InternalError {
                                    message: "Struct variant patterns not yet fully supported"
                                        .to_string(),
                                    span: Some(span),
                                });
                                Vec::new() // Return empty for now
                            }
                            Some(crate::definitions::VariantKind::Unit) => {
                                errors.push(ResolutionError::PatternMismatch {
                                    message: format!(
                                        "Unit variant '{}' used with arguments in pattern.",
                                        def_info.name
                                    ),
                                    span,
                                    help: None,
                                });
                                Vec::new()
                            }
                            None => {
                                // Should not happen for variants
                                errors.push(ResolutionError::InternalError {
                                    message: "Variant definition info missing variant kind"
                                        .to_string(),
                                    span: Some(span),
                                });
                                Vec::new()
                            }
                        };

                        // --- Resolve Expected Arg Types ---
                        // We need to resolve the AST types stored in the variant definition
                        // This requires calling resolve_type. We need the context of the enum definition.
                        let enum_symbol = def_info.parent_symbol.unwrap(); // Safe due to checks above
                        let enum_generic_params = definitions_map
                            .get(&enum_symbol)
                            .and_then(|ei| ei.generic_params.as_ref())
                            .map(|names| names.as_slice())
                            .unwrap_or(&[]);

                        let resolved_expected_arg_types: Vec<ResolvedType> = expected_arg_types_ast
                            .iter()
                            .map(|ast_ty| {
                                match crate::resolve_types::resolve_type(
                                    db,
                                    definitions_map,
                                    module_scopes,
                                    prelude_scope,
                                    // Use the module where the *enum* is defined for resolving types in its signature
                                    definitions_map
                                        .get(&enum_symbol)
                                        .map_or(current_module_symbol, |ei| {
                                            ei.parent_symbol.unwrap_or(enum_symbol)
                                        }),
                                    ast_ty,
                                    enum_generic_params,
                                    Some((DefinitionKind::Enum, enum_symbol)), // Context is the enum
                                    ast_ty.span,
                                ) {
                                    Ok(ty) => ty,
                                    Err(e) => {
                                        errors.push(e);
                                        ResolvedType::Unknown
                                    }
                                }
                            })
                            .collect();
                        let expected_args_type = ResolvedType::Tuple(resolved_expected_arg_types);
                        // --- End Resolve Expected Arg Types ---

                        // 4. Resolve the argument sub-pattern(s)
                        let mut resolved_args_pattern_kind = ResolvedPatternKind::Wildcard; // Default
                        if let Ok((resolved_args_pat, _)) = resolve_pattern(
                            db,
                            definitions_map,
                            module_scopes,
                            prelude_scope,
                            current_module_symbol,
                            scope_stack,         // Pass scope_stack!
                            args,                // Pass the nested pattern AST node
                            &expected_args_type, // Pass the *resolved* expected types
                            errors,
                            warnings,
                            definition_context,
                            impl_context,
                        ) {
                            resolved_args_pattern_kind = resolved_args_pat.kind;
                        } else {
                            println!("WARN: Failed to resolve sub-pattern for constructor.");
                            // Keep Wildcard as placeholder
                        }

                        // 5. TODO: Arity check (compare resolved_args_pattern structure with expected_arg_types count)

                        // 6. Return Constructor kind
                        println!(
                            "    -> Resolved ConstructorWithArgs pattern for variant: {:?}",
                            resolved_symbol
                        );
                        ResolvedPatternKind::Constructor {
                            symbol: resolved_symbol,
                            args: Box::new(ResolvedPattern {
                                // Box the resolved pattern
                                kind: resolved_args_pattern_kind,
                                span: args.span, // Use span of the args pattern
                                resolved_type: expected_args_type, // Use the derived expected type
                            }),
                        }
                    } else {
                        /* ... Internal error: symbol not found ... */
                        ResolvedPatternKind::Wildcard
                    }
                }
                Err(e) => {
                    errors.push(e);
                    ResolvedPatternKind::Wildcard
                }
            }
        }

        // --- TODO: Implement other PatternKind variants ---
        ast::pattern::PatternKind::Tuple(patterns) => {
            println!("  -> PatternKind::Tuple");
            // Extract element types from expected_type if it's a Tuple
            let expected_element_types = match expected_type {
                ResolvedType::Tuple(types) => types.clone(),
                _ => {
                    errors.push(ResolutionError::TypeMismatch {
                        expected: format!("Tuple type with {} elements", patterns.len()),
                        found: format!("{:?}", expected_type),
                        span,
                        context: Some("in tuple pattern".to_string()),
                    });
                    vec![ResolvedType::Unknown; patterns.len()]
                }
            };

            if expected_element_types.len() != patterns.len() {
                errors.push(ResolutionError::PatternMismatch {
                    message: format!(
                        "Tuple pattern has {} elements, but expected type has {}",
                        patterns.len(),
                        expected_element_types.len()
                    ),
                    span,
                    help: None,
                });
                // Even on arity mismatch, try resolving elements to gather more errors?
                // Or return early? Let's return early for now.
                return Ok((
                    ResolvedPattern {
                        kind: ResolvedPatternKind::Wildcard,
                        span,
                        resolved_type: expected_type.clone(),
                    },
                    false,
                ));
            }

            let mut resolved_elements = Vec::new();
            for (i, elem_pattern) in patterns.iter().enumerate() {
                let elem_expected_type = expected_element_types
                    .get(i)
                    .cloned()
                    .unwrap_or(ResolvedType::Unknown);

                if let Ok((resolved_elem, _)) = resolve_pattern(
                    db,
                    definitions_map,
                    module_scopes,
                    prelude_scope,
                    current_module_symbol,
                    scope_stack,
                    elem_pattern,
                    &elem_expected_type,
                    errors,
                    warnings,
                    definition_context,
                    impl_context,
                ) {
                    resolved_elements.push(resolved_elem);
                } else {
                    // Error resolving element, add placeholder
                    resolved_elements.push(ResolvedPattern {
                        kind: ResolvedPatternKind::Wildcard,
                        span: elem_pattern.span,
                        resolved_type: ResolvedType::Unknown,
                    });
                }
            }
            ResolvedPatternKind::Tuple(resolved_elements)
        }
        // ast::pattern::PatternKind::Struct { path, fields }
        // ... etc.
        ast::pattern::PatternKind::Wildcard => ResolvedPatternKind::Wildcard,

        _ => {
            errors.push(ResolutionError::InternalError {
                message: format!(
                    "Pattern kind {:?} not yet supported in resolve_pattern",
                    ast_pattern.kind
                ),
                span: Some(span),
            });
            ResolvedPatternKind::Wildcard
        }
    };

    Ok((
        ResolvedPattern {
            kind: resolved_kind,
            span: ast_pattern.span,
            resolved_type: expected_type.clone(),
        },
        is_diverging,
    ))
}

/// Helper function to get the type of a literal
// TODO: Remove if truly unused
/*
fn get_literal_type(lit: &Literal) -> ResolvedType {
    match lit {
        Literal::Int(_) => ResolvedType::Primitive(PrimitiveType::I32),
        Literal::Float(_) => ResolvedType::Primitive(PrimitiveType::F64),
        Literal::String(_) => ResolvedType::Primitive(PrimitiveType::String),
        Literal::Bool(_) => ResolvedType::Primitive(PrimitiveType::Bool),
        Literal::Char(_) => ResolvedType::Primitive(PrimitiveType::Char),
    }
}
*/

// --- Visitor for checking effectful calls in resolved body --- 

struct EffectfulCallVisitor<'a> {
    resolved_defs: &'a ResolvedDefinitions,
    found_effectful_call: bool,
}

impl<'a> EffectfulCallVisitor<'a> {
    fn new(resolved_defs: &'a ResolvedDefinitions) -> Self {
        Self { resolved_defs, found_effectful_call: false }
    }

    fn visit_expr(&mut self, expr: &ResolvedExpr) {
        if self.found_effectful_call { return; } // Short-circuit

        match &expr.kind {
            ResolvedExprKind::Call { func_symbol, args, .. } => {
                if let Some(symbol) = func_symbol {
                    if let Some(callee) = self.resolved_defs.functions.iter().find(|f| f.symbol == *symbol) {
                        if callee.is_effectful {
                            self.found_effectful_call = true;
                            return;
                        }
                    }
                }
                // Also visit args
                for arg in args {
                    self.visit_expr(&arg.value);
                }
            }
            ResolvedExprKind::MethodCall { resolved_method_symbol, args, object, .. } => {
                if let Some(symbol) = resolved_method_symbol {
                     if let Some(callee) = self.resolved_defs.functions.iter().find(|f| f.symbol == *symbol) {
                        if callee.is_effectful {
                            self.found_effectful_call = true;
                            return;
                        }
                    }
                }
                self.visit_expr(object);
                for arg in args {
                    self.visit_expr(&arg.value);
                }
            }
            ResolvedExprKind::Block(exprs) => {
                for sub_expr in exprs {
                    self.visit_expr(sub_expr);
                }
            }
            ResolvedExprKind::If { condition, then_branch, else_branch } => {
                self.visit_expr(condition);
                self.visit_expr(then_branch);
                if let Some(else_b) = else_branch {
                    self.visit_expr(else_b);
                }
            }
            ResolvedExprKind::Match { scrutinee, arms } => {
                self.visit_expr(scrutinee);
                for (_, arm_expr) in arms {
                    self.visit_expr(arm_expr);
                }
            }
             ResolvedExprKind::Binary { left, right, .. } => {
                 self.visit_expr(left);
                 self.visit_expr(right);
             }
             ResolvedExprKind::Unary { expr: inner_expr, .. } => {
                 self.visit_expr(inner_expr);
             }
             ResolvedExprKind::Lambda { body, .. } => {
                 self.visit_expr(body);
             }
             ResolvedExprKind::Field { object, .. } => {
                 self.visit_expr(object);
             }
             ResolvedExprKind::Array(elements) | ResolvedExprKind::Tuple(elements) | ResolvedExprKind::HashSet(elements) => {
                 for elem in elements {
                     self.visit_expr(elem);
                 }
             }
             ResolvedExprKind::Map(entries) => {
                 for (key, value) in entries {
                     self.visit_expr(key);
                     self.visit_expr(value);
                 }
             }
             ResolvedExprKind::Let { value, .. } => {
                 self.visit_expr(value);
             }
             ResolvedExprKind::Struct { fields, base, .. } => {
                 for (_, field_expr) in fields {
                     self.visit_expr(field_expr);
                 }
                 if let Some(base_expr) = base {
                     self.visit_expr(base_expr);
                 }
             }
             ResolvedExprKind::Paren(inner_expr) => {
                 self.visit_expr(inner_expr);
             }
             ResolvedExprKind::VariantConstructor { args, .. } => {
                 for arg in args {
                     self.visit_expr(&arg.value);
                 }
             }
            // Literals and Paths don't contain sub-expressions with calls
            ResolvedExprKind::Literal(_) | ResolvedExprKind::Path(_) => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*; // Import items from parent module
    use crate::error::ResolverWarning;
    use crate::types::{ResolvedType, PrimitiveType};
    use miette::SourceSpan;
    use std::collections::HashMap;

    // Helper to create a dummy span
    fn dummy_span() -> SourceSpan {
        SourceSpan::from((0, 0))
    }

    // Helper to create a simple LocalBinding
    fn create_binding(name: &str, ty: ResolvedType, used: bool) -> LocalBinding {
        LocalBinding {
            name: name.to_string(),
            resolved_type: ty,
            defined_at: dummy_span(),
            used,
            is_mutable: false,
        }
    }

    #[test]
    fn test_scope_stack_basic() {
        let mut stack = ScopeStack::new();
        let mut warnings = Vec::new();
        let type_i32 = ResolvedType::Primitive(PrimitiveType::I32);
        let type_bool = ResolvedType::Primitive(PrimitiveType::Bool);

        stack.push_scope(); // Scope 0
        assert!(stack.add_binding(create_binding("x", type_i32.clone(), false), &mut warnings).is_ok());
        assert_eq!(stack.scopes.len(), 1);
        assert_eq!(stack.scopes[0].len(), 1);

        // Find binding in current scope
        let found_x = stack.find_binding("x");
        assert!(found_x.is_some());
        assert_eq!(found_x.unwrap().resolved_type, type_i32);
        assert!(!found_x.unwrap().used);

        // Add another binding to current scope
        assert!(stack.add_binding(create_binding("y", type_bool.clone(), true), &mut warnings).is_ok());
        assert_eq!(stack.scopes[0].len(), 2);

        // Cannot add duplicate in same scope
        assert!(stack.add_binding(create_binding("x", type_i32.clone(), false), &mut warnings).is_err());
        assert_eq!(stack.scopes[0].len(), 2); // Should not have been added

        // Find non-existent binding
        assert!(stack.find_binding("z").is_none());

        stack.pop_scope(); // Pop outer
    }

    #[test]
    fn test_scope_stack_shadowing_and_duplicates() {
        let mut stack = ScopeStack::new();
        let mut warnings: Vec<ResolverWarning> = Vec::new();

        // Outer scope
        stack.push_scope();
        let binding_a1 = create_binding("a", ResolvedType::Primitive(PrimitiveType::I32), true);
        let span_a1 = binding_a1.defined_at;
        stack.add_binding(binding_a1, &mut warnings).unwrap();
        assert!(warnings.is_empty());

        // Inner scope
        stack.push_scope();
        let binding_a2 = create_binding("a", ResolvedType::Primitive(PrimitiveType::Bool), false);
        let span_a2 = binding_a2.defined_at;
        // Adding 'a' again should shadow and produce a warning
        stack.add_binding(binding_a2, &mut warnings).unwrap();
        assert_eq!(warnings.len(), 1);
        if let Some(ResolverWarning::ShadowedVariable { ref name, original_span, shadow_span }) = warnings.first() {
            assert_eq!(name, "a");
            assert_eq!(*original_span, span_a1);
            assert_eq!(*shadow_span, span_a2);
        } else {
            panic!("Expected ShadowedVariable warning for 'a'");
        }
        warnings.clear(); // Clear warnings for next check

        // Add 'y' to inner scope
        stack.add_binding(create_binding("y", ResolvedType::Primitive(PrimitiveType::String), false), &mut warnings).unwrap();
        // Since 'y' doesn't exist in any outer scope, this should NOT produce a shadowing warning
        assert_eq!(warnings.len(), 0, "Adding 'y' should not produce any shadowing warnings");
        warnings.clear();

        // Try adding duplicate 'y' in the *same* scope (should be an error)
        let duplicate_y_binding = create_binding("y", ResolvedType::Primitive(PrimitiveType::F64), false);
        let result = stack.add_binding(duplicate_y_binding.clone(), &mut warnings);
        assert!(result.is_err());
        if let Err(ResolutionError::DuplicateDefinition { name, .. }) = result {
            assert_eq!(name, "y");
        } else {
            panic!("Expected DuplicateDefinition error for 'y'");
        }
        assert!(warnings.is_empty()); // Duplicate definition is an error, not warning

        stack.pop_scope(); // Pop inner
        stack.pop_scope(); // Pop outer
    }

    #[test]
    fn test_scope_stack_mark_used() {
        let mut stack = ScopeStack::new();
        let mut warnings = Vec::new();
        let type_i32 = ResolvedType::Primitive(PrimitiveType::I32);

        stack.push_scope(); // Scope 0
        assert!(stack.add_binding(create_binding("unused", type_i32.clone(), false), &mut warnings).is_ok());
        stack.push_scope(); // Scope 1
        assert!(stack.add_binding(create_binding("used_inner", type_i32.clone(), false), &mut warnings).is_ok());
        assert!(stack.add_binding(create_binding("unused_inner", type_i32.clone(), false), &mut warnings).is_ok());

        // Mark used_inner as used (it's in the current scope)
        let binding_mut = stack.find_binding_mut("used_inner");
        assert!(binding_mut.is_some());
        binding_mut.unwrap().used = true;

        // Check final unused variables (requires manual call)
        let mut final_warnings = Vec::new();
        check_unused_variables(&stack, &mut final_warnings);

        assert_eq!(final_warnings.len(), 2);
        let unused_names: Vec<_> = final_warnings.iter().map(|w| match w {
            ResolverWarning::UnusedLocalVariable { name, .. } => name.as_str(),
            _ => panic!("Unexpected warning type"),
        }).collect();
        assert!(unused_names.contains(&"unused"));
        assert!(unused_names.contains(&"unused_inner"));
        assert!(!unused_names.contains(&"used_inner"));
    }
} // End of tests module
