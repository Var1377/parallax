use crate::definitions::{DefinitionInfo, DefinitionKind};
use crate::error::{ResolutionError, ResolverWarning};
use crate::scopes::ModuleScope;
use crate::types::{
    PrimitiveType, ResolvedArgument, ResolvedDefinitions, ResolvedExpr, ResolvedExprKind,
    ResolvedImpl, ResolvedParameter, ResolvedPattern, ResolvedPatternKind,
    ResolvedType, Symbol,
};
use miette::SourceSpan;
use parallax_syntax::ast::{self, expr::Expr, pattern::Pattern};
use parallax_syntax::SyntaxDatabase;
use std::collections::HashMap;

/// Represents information about a local binding (variable).
#[derive(Debug, Clone)]
struct LocalBinding {
    symbol: Symbol, // <<< ADD THIS FIELD
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
        _warnings: &mut Vec<ResolverWarning>,
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
        
        // if let Some(span) = shadowed_span {
        //     warnings.push(ResolverWarning::ShadowedVariable {
        //         name: binding.name.clone(),
        //         original_span: span,
        //         shadow_span: binding.defined_at,
        //     });
        // }

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
    prelude_scope: &HashMap<String, Symbol>,
    resolved_defs: &mut ResolvedDefinitions,
    errors: &mut Vec<ResolutionError>,
    warnings: &mut Vec<ResolverWarning>,
) {
    // Create a map to store the results temporarily
    let mut resolved_bodies: HashMap<Symbol, (Option<ResolvedExpr>, bool)> = HashMap::new();

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
        // Call the refactored function, now taking resolved_defs immutably
        match resolve_single_function_body(
            db,
            definitions_map,
            module_scopes,
            prelude_scope,
            resolved_defs, // Pass immutably
            errors,
            warnings,
            func_symbol,
            None, // No impl context for standalone functions
        ) {
            Ok(result) => { resolved_bodies.insert(func_symbol, result); },
            Err(_) => { /* Error already pushed to errors vector inside helper */ }
        }
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
        // Find the DefinitionInfo for the method to check if it has an AST node.
        let method_def_info = definitions_map.get(&method_symbol);

        // Only proceed if the method has an associated Function AST node.
        if method_def_info.and_then(|info| info.ast_function.as_ref()).is_some() {
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
            match resolve_single_function_body(
                db,
                definitions_map,
                module_scopes,
                prelude_scope,
                resolved_defs, // Pass immutably
                errors,
                warnings,
                method_symbol,         // Symbol of the function to resolve
                impl_context.as_ref(), // Pass Option<&ResolvedImpl>
            ) {
                Ok(result) => {
                    println!("[resolve_bodies INSERT] Inserting result for method_symbol: {:?}", method_symbol); // <<< ADDED PRINT
                    resolved_bodies.insert(method_symbol, result);
                },
                Err(_) => { /* Error already pushed to errors vector inside helper */ }
            }
        }
    }

    // --- Resolve trait method default bodies ---
    // Clone trait symbols and their method symbols first
    let trait_method_symbols: Vec<(Symbol, Symbol)> = resolved_defs
        .traits
        .iter()
        .flat_map(|trt| {
            trt.methods
                .iter()
                .map(move |assoc_func| (trt.symbol, assoc_func.func_symbol))
        })
        .collect();

    for (trait_symbol, method_symbol) in trait_method_symbols {
        // Find the DefinitionInfo for the method to check if it has a default body AST node.
        if let Some(method_def_info) = definitions_map.get(&method_symbol) {
            if method_def_info.default_body_ast.is_some() {
                 // Resolve the default body
                 match resolve_single_function_body(
                    db,
                    definitions_map,
                    module_scopes,
                    prelude_scope,
                    resolved_defs, // Pass immutably
                    errors,
                    warnings,
                    method_symbol, // Symbol of the function (trait method) to resolve
                    None, // No impl context for resolving default bodies in traits
                 ) {
                    Ok(result) => {
                        println!("[resolve_bodies INSERT DEFAULT] Inserting result for trait method_symbol: {:?}", method_symbol);
                        // Store the resolved default body separately
                        resolved_bodies.insert(method_symbol, result);
                    },
                    Err(_) => { /* Error already pushed */ }
                 }
            }
        }
    }

    // --- Update ResolvedDefinitions with collected bodies --- 
    for func_def in resolved_defs.functions.iter_mut() {
        println!("[resolve_bodies UPDATE LOOP] Checking func_def.symbol: {:?} ({})", func_def.symbol, func_def.name); // <<< ADDED PRINT
        if let Some((body_opt, is_effectful)) = resolved_bodies.remove(&func_def.symbol) {
            // Determine if this function corresponds to a trait method with a default body
            let is_trait_default_body = definitions_map
                .get(&func_def.symbol)
                .map_or(false, |info| info.default_body_ast.is_some());

            if is_trait_default_body {
                 println!("[resolve_bodies UPDATE DEFAULT] Updating default body for {:?} ({}) - body_opt.is_some(): {}", func_def.symbol, func_def.name, body_opt.is_some());
                 func_def.resolved_default_body = body_opt;
                 // Update effectful status based on default body too
                 // If the signature already marked it effectful, keep it. Otherwise, update based on body analysis.
                 if !func_def.is_effectful {
                    func_def.is_effectful = is_effectful;
                 }
            } else {
                 println!("[resolve_bodies UPDATE] Updating body for {:?} ({}) - body_opt.is_some(): {}", func_def.symbol, func_def.name, body_opt.is_some());
                 func_def.body = body_opt;
                 // Update effectful status based on impl body
                 if !func_def.is_effectful {
                    func_def.is_effectful = is_effectful;
                 }
            }
        }
        // If not found in resolved_bodies map, it means resolution wasn't attempted (e.g., no AST) or failed.
        // In either case, body remains None, and is_effectful remains as determined by signature pass.
    }
}

/// Helper to resolve the body of a single function (standalone or associated).
pub(crate) fn resolve_single_function_body<'db>(
    db: &'db dyn SyntaxDatabase,
    definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>,
    module_scopes: &HashMap<Symbol, ModuleScope>,
    prelude_scope: &HashMap<String, Symbol>,
    resolved_defs: &mut ResolvedDefinitions,
    errors: &mut Vec<ResolutionError>,
    warnings: &mut Vec<ResolverWarning>,
    func_symbol: Symbol,
    impl_context: Option<&ResolvedImpl>,
) -> Result<(Option<ResolvedExpr>, bool), ResolutionError> {
    // Find the function definition info from the initial map
    println!("[resolve_single_function_body START] Processing symbol: {:?}", func_symbol); // <<< DEBUG PRINT
    let func_def_info = match definitions_map.get(&func_symbol) {
        Some(info) => info,
        None => {
            // Push error directly instead of relying on caller finding the symbol
            errors.push(ResolutionError::InternalError {
                message: format!(
                    "DefinitionInfo for function symbol {} not found during body resolution",
                    func_symbol.id()
                ),
                span: None,
            });
            // Return an error state appropriate for the new signature
            return Err(ResolutionError::InternalError {
                 message: format!("DefinitionInfo missing for {}", func_symbol.id()),
                 span: None
            });
        }
    };

    // Check if this is an intrinsic function. Intrinsics have no Parallax body.
    if let Some(crate::definitions::SpecialDefinitionKind::Intrinsic) = func_def_info.special_kind {
        println!("[resolve_single_function_body] Detected intrinsic {}, skipping body resolution.", func_def_info.name); // Optional debug log
        // Return Ok with None for the body, but use the effectful flag determined during definition collection.
        return Ok((None, func_def_info.is_effectful));
    }

    // Get the function's signature details from resolved_defs (read-only access is okay)
    let func_signature = match resolved_defs.functions.iter().find(|f| f.symbol == func_symbol) {
        Some(sig) => sig,
        None => {
            errors.push(ResolutionError::InternalError {
                message: format!(
                    "ResolvedFunction signature for symbol {} not found during body resolution",
                    func_symbol.id()
                ),
                span: Some(func_def_info.span),
            });
             return Err(ResolutionError::InternalError {
                 message: format!("ResolvedFunction signature missing for {}", func_symbol.id()),
                 span: Some(func_def_info.span)
             });
        }
    };

    // If the body is already resolved in the signature struct (shouldn't happen if called correctly, but check anyway)
    if func_signature.body.is_some() {
        println!("[resolve_single_function_body] Body already resolved for {}, skipping.", func_signature.name); // <<< DEBUG PRINT
        return Ok((func_signature.body.clone(), func_signature.is_effectful)); // Return existing body/status
    }

    // --- AST BODY RETRIEVAL (Prioritize default_body_ast if present) ---
    println!("[resolve_single_function_body] DefinitionInfo lookup result for {:?}: true", func_symbol); // <<< DEBUG PRINT

    let body_ast_option: Option<&ast::expr::Expr> = if let Some(default_body) = func_def_info.default_body_ast.as_ref() {
        println!("[resolve_single_function_body] Using default_body_ast for symbol {:?}", func_symbol);
        Some(default_body)
    } else {
        // Fallback to ast_function.body
        let func_ast_option: Option<&ast::items::Function> = func_def_info
            .ast_function.as_ref(); // Borrow the Option<Function>
        println!("[resolve_single_function_body] func_ast_option.is_some(): {}", func_ast_option.is_some()); // <<< DEBUG PRINT

        let body_opt = func_ast_option
            .and_then(|func_ast| {
                println!("[resolve_single_function_body] Found Function AST, checking func_ast.body..."); // <<< DEBUG PRINT
                func_ast.body.as_deref() // Dereference the Box<Expr>
            });
        println!("[resolve_single_function_body] body_ast_option from func_ast.is_some(): {}", body_opt.is_some()); // <<< DEBUG PRINT
        body_opt
    };

    // Ensure we have *some* body AST to work with if this function *should* have one
    // (i.e., it's not just a trait signature declaration without a default)
    if body_ast_option.is_none() && func_def_info.ast_function.is_some() {
        // We expected a body from ast_function but didn't find one (or default was None too)
        let error_span = Some(func_def_info.span);
        errors.push(ResolutionError::InternalError {
            message: format!(
                "Function AST exists for symbol {:?} but body is missing (and no default body provided)",
                func_symbol
            ),
            span: error_span,
        });
        return Err(ResolutionError::InternalError { // Return error
             message: "Missing function body AST".to_string(),
             span: error_span
         });
    }
    // If body_ast_option is still None here, it means it's a trait method *without* a default body,
    // which is fine, and we'll just return Ok((None, effectful_status)) later.

    // --- END AST BODY RETRIEVAL ---

    // Create the initial scope stack for the function
    let mut scope_stack = ScopeStack::new();
    scope_stack.push_scope(); // Push the top-level scope for parameters

    // Determine the definition context (Trait or Impl) for resolving `Self`
    let definition_context = definitions_map
        .get(&func_symbol)
        .and_then(|def_info| def_info.parent_symbol)
        .and_then(|parent_symbol| definitions_map.get(&parent_symbol))
        .map(|parent_info| (parent_info.kind, parent_info.symbol));

    // Add parameters to scope using the resolved signature info
    for param in &func_signature.parameters { // Use func_signature here
        let binding = LocalBinding {
            name: param.name.clone(),
            resolved_type: param.param_type.clone(),
            symbol: param.symbol,
            defined_at: param.span,
            used: false,
            is_mutable: false,
        };
        if let Err(e) = scope_stack.add_binding(binding, warnings) {
            errors.push(e);
        }
    }

    let func_module_symbol = func_signature.module_symbol;

    // Resolve the body expression
    let mut resolved_body_opt: Option<ResolvedExpr> = None;
    let mut sub_expr_had_effectful_call = false;

    if let Some(body_ast) = body_ast_option {
        println!("[resolve_single_function_body] Attempting to resolve expression for body...");
        match resolve_expression(
            db,
            definitions_map,
            module_scopes,
            prelude_scope,
            func_module_symbol,
            &mut scope_stack,
            body_ast,
            errors,
            warnings,
            definition_context,
            impl_context,
        ) {
            Ok((resolved_body, encountered_effectful_call_in_sub)) => {
                println!("[resolve_single_function_body] resolve_expression successful.");
                resolved_body_opt = Some(resolved_body);
                sub_expr_had_effectful_call = encountered_effectful_call_in_sub;
            }
            Err(e) => {
                println!("[resolve_single_function_body] resolve_expression FAILED: {:?}", e);
                errors.push(e.clone()); // Clone error to push
                // Don't return error yet, just leave body as None
                // Return the original error *after* cleanup
                scope_stack.pop_scope(); // Pop scope before returning Err
                return Err(e); // Return the resolution error
            }
        }
    } else {
        println!("[resolve_single_function_body] No body_ast found to resolve.");
        // Function has no body (trait method signature)
        // Leave resolved_body_opt as None
    }

    // Check for effectful calls within the resolved body
    let mut body_causes_effect = sub_expr_had_effectful_call;
    if let Some(ref resolved_body) = resolved_body_opt {
        let mut visitor = EffectfulCallVisitor::new(resolved_defs); // Pass resolved_defs (read-only is fine here)
        visitor.visit_expr(resolved_body);
        if visitor.found_effectful_call {
            body_causes_effect = true;
        }
    }

    // Check for unused variables
    check_unused_variables(&scope_stack, warnings);

    // Pop the function's main scope
    scope_stack.pop_scope();

    // Return the resolved body and effectful status
    // Combine existing effectful status from signature with status derived from body
    let final_effectful_status = func_signature.is_effectful | body_causes_effect;
    Ok((resolved_body_opt, final_effectful_status))
}

/// Check for unused variables in all scopes and generate warnings
fn check_unused_variables(_scope_stack: &ScopeStack, _warnings: &mut Vec<ResolverWarning>) {
    // for scope in &scope_stack.scopes {
    //     for binding in scope.values() {
    //         if !binding.used {
    //             warnings.push(ResolverWarning::UnusedLocalVariable {
    //                 name: binding.name.clone(),
    //                 span: binding.defined_at,
    //             });
    //         }
    //     }
    // }
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
    println!("[resolve_expression START] Processing AST Expr: {:?}", ast_expr); // <<< DEBUG PRINT ADDED
    let mut encountered_effectful_call_in_sub = false;
    let mut had_effectful_sub_expression = false;
    let mut resolved_type = ResolvedType::Unknown;

    let resolved_kind = match &ast_expr.kind {
        ast::expr::ExprKind::Literal(lit) => {
            resolved_type = match lit {
                ast::common::Literal::Int { value: _, suffix } => {
                    match suffix.as_deref() {
                        Some("i8") => ResolvedType::Primitive(PrimitiveType::I8),
                        Some("i16") => ResolvedType::Primitive(PrimitiveType::I16),
                        Some("i32") => ResolvedType::Primitive(PrimitiveType::I32),
                        Some("i64") => ResolvedType::Primitive(PrimitiveType::I64),
                        Some("i128") => ResolvedType::Primitive(PrimitiveType::I128),
                        Some("u8") => ResolvedType::Primitive(PrimitiveType::U8),
                        Some("u16") => ResolvedType::Primitive(PrimitiveType::U16),
                        Some("u32") => ResolvedType::Primitive(PrimitiveType::U32),
                        Some("u64") => ResolvedType::Primitive(PrimitiveType::U64),
                        Some("u128") => ResolvedType::Primitive(PrimitiveType::U128),
                        Some(other) => {
                            warnings.push(ResolverWarning::InvalidLiteralSuffix {
                                suffix: other.to_string(),
                                literal: format!("{:?}", lit),
                                span: ast_expr.span,
                            });
                            ResolvedType::IntegerLiteral // Fallback for invalid suffix
                        }
                        None => ResolvedType::IntegerLiteral, // No suffix, use generic integer
                    }
                }
                ast::common::Literal::Float { value: _, suffix } => {
                    match suffix.as_deref() {
                        Some("f32") => ResolvedType::Primitive(PrimitiveType::F32),
                        Some("f64") => ResolvedType::Primitive(PrimitiveType::F64),
                        Some(other) => {
                            warnings.push(ResolverWarning::InvalidLiteralSuffix {
                                suffix: other.to_string(),
                                literal: format!("{:?}", lit),
                                span: ast_expr.span,
                            });
                            ResolvedType::FloatLiteral // Fallback for invalid suffix
                        }
                        None => ResolvedType::FloatLiteral, // No suffix, use generic float
                    }
                }
                ast::common::Literal::String(_) => ResolvedType::Primitive(PrimitiveType::String),
                ast::common::Literal::Bool(_) => ResolvedType::Primitive(PrimitiveType::Bool),
                ast::common::Literal::Char(_) => ResolvedType::Primitive(PrimitiveType::Char),
            };
            ResolvedExprKind::Literal(lit.clone())
        }
        ast::expr::ExprKind::Path(ref path_expr) => {
            if path_expr.is_empty() {
                return Err(ResolutionError::InternalError {
                    message: "Empty path expression encountered".to_string(),
                    span: Some(ast_expr.span),
                });
            }

            if path_expr.len() == 1 {
                let ident = &path_expr[0];
                let name = &ident.name;

                // --- NEW: Handle 'self' --- 
                if name == "self" {
                    if let Some((kind, _)) = definition_context {
                        if kind == DefinitionKind::Trait || kind == DefinitionKind::Impl {
                            resolved_type = ResolvedType::SelfType;
                            ResolvedExprKind::SelfRef // <<< Use new SelfRef kind
                        } else {
                            errors.push(ResolutionError::NameNotFound { name: "self".to_string(), span: ident.span, help: Some("`self` can only be used within traits or impls.".to_string()) });
                            ResolvedExprKind::Path(Symbol::new(0)) // Error placeholder
                        }
                    } else {
                         errors.push(ResolutionError::NameNotFound { name: "self".to_string(), span: ident.span, help: Some("`self` can only be used within traits or impls.".to_string()) });
                         ResolvedExprKind::Path(Symbol::new(0))
                    }
                // --- END NEW 'self' handling --- 
                } else if let Some(binding) = scope_stack.find_binding_mut(name) {
                    // Found in local scope
                    binding.used = true;
                    resolved_type = binding.resolved_type.clone();
                    ResolvedExprKind::Variable { // <<< Use Variable Kind
                        binding_symbol: binding.symbol,
                        name: binding.name.clone(),
                    }
                } else {
                    // Not found locally, try module/prelude path resolution
                    match crate::resolve_types::resolve_path(
                        definitions_map,
                        module_scopes,
                        prelude_scope,
                        current_module_symbol,
                        path_expr,
                        ast_expr.span,
                    ) {
                        Ok(resolved_symbol) => {
                            resolved_type = get_symbol_type(definitions_map, resolved_symbol);
                            ResolvedExprKind::Path(resolved_symbol)
                        }
                        Err(e) => return Err(e),
                    }
                }
            } else {
                // --- Path with Multiple Segments ---
                match crate::resolve_types::resolve_path(
                    definitions_map,
                    module_scopes,
                    prelude_scope,
                    current_module_symbol,
                    path_expr,
                    ast_expr.span,
                ) {
                    Ok(resolved_symbol) => {
                        resolved_type = get_symbol_type(definitions_map, resolved_symbol);
                        ResolvedExprKind::Path(resolved_symbol)
                    }
                    Err(e) => return Err(e),
                }
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
            resolved_type = last_type; // Assign the block's type
            ResolvedExprKind::Block(resolved_exprs)
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
            resolved_type = resolved_then.resolved_type.clone(); // Placeholder - Use then branch type

            ResolvedExprKind::If {
                condition: Box::new(resolved_condition),
                then_branch: Box::new(resolved_then),
                else_branch: resolved_else,
            }
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

            resolved_type = ResolvedType::Primitive(PrimitiveType::Unit); // Let expressions evaluate to Unit
            ResolvedExprKind::Let {
                pattern: resolved_pattern,
                value: Box::new(resolved_value),
                type_annotation: resolved_annotation_opt,
            }
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
            resolved_type = resolved_left.resolved_type.clone(); // Placeholder

            ResolvedExprKind::Binary {
                left: Box::new(resolved_left),
                op: *op,
                right: Box::new(resolved_right),
            }
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
            resolved_type = resolved_operand.resolved_type.clone(); // Placeholder

            ResolvedExprKind::Unary {
                op: op.clone(), // Clone the operator
                expr: Box::new(resolved_operand),
            }
        }
        ast::expr::ExprKind::Call { func, args } => {
            // <<< DEBUG START >>>
            if let ast::expr::ExprKind::Path(p) = &func.kind {
                 if let Some(ident) = p.last() {
                    if ident.name == "__intrinsic_i32_add__" { // Use ident.name directly
                        println!("[resolve_expression] Encountered call to: {:?}", func);
                    }
                }
            }
            // <<< DEBUG END >>>
           
            // Resolve the function expression first
            let (resolved_func_expr, func_expr_effectful) = resolve_expression(
                db, definitions_map, module_scopes, prelude_scope, current_module_symbol,
                scope_stack, func, errors, warnings, definition_context, impl_context
            )?;

            // Resolve the arguments
            let mut resolved_args = Vec::new();
            let mut args_effectful = false;
            for arg in args {
                let (resolved_value, arg_effectful) = resolve_expression(
                    db, definitions_map, module_scopes, prelude_scope, current_module_symbol,
                    scope_stack, &arg.value, errors, warnings, definition_context, impl_context
                )?;
                args_effectful |= arg_effectful;
                resolved_args.push(ResolvedArgument {
                    name: arg.name.as_ref().map(|ident| ident.name.clone()),
                    value: resolved_value,
                    span: arg.span,
                });
            }

            had_effectful_sub_expression = func_expr_effectful | args_effectful;

            // Determine the type of the call result
            resolved_type = ResolvedType::Unknown; // Placeholder - Needs proper type inference
            let resolved_func_symbol = match &resolved_func_expr.kind {
                ResolvedExprKind::Path(sym) => Some(*sym),
                // TODO: Handle calls on other expression kinds (closures, etc.)
                _ => None,
            };

            // Assign the Call kind
            ResolvedExprKind::Call {
                func_symbol: resolved_func_symbol,
                args: resolved_args,
            }
        }
        ast::expr::ExprKind::Field { object, name } => {
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
            encountered_effectful_call_in_sub = object_effectful;

            // --- NEW: Handle field access on SelfRef --- 
            if matches!(resolved_object.kind, ResolvedExprKind::SelfRef) {
                 let method_name_str = name.name.clone();
                 if let Some((DefinitionKind::Trait, trait_symbol)) = definition_context {
                    // Look up method in trait def_info
                    let method_symbol_opt = definitions_map.values().find(|info| {
                        info.parent_symbol == Some(trait_symbol) &&
                        info.kind == DefinitionKind::Function &&
                        info.name == method_name_str
                    }).map(|info| info.symbol);

                    if let Some(method_symbol) = method_symbol_opt {
                         // Resolve the field access directly to the method's Path
                         resolved_type = get_symbol_type(definitions_map, method_symbol);
                         ResolvedExprKind::Path(method_symbol) // <<< Resolve to Path(method_symbol)
                    } else {
                         errors.push(ResolutionError::UnknownField { field_name: method_name_str, struct_name: "Self".to_string(), span: name.span });
                         ResolvedExprKind::Path(Symbol::new(0)) // Error placeholder
                    }
                } else {
                    // Field access on Self outside of a trait context? Error.
                    errors.push(ResolutionError::InternalError { message: "Field access on SelfType outside trait context".to_string(), span: Some(name.span) });
                    ResolvedExprKind::Path(Symbol::new(0))
                }
            // --- END NEW 'SelfRef' field access handling --- 
            } else {
                // Regular field access logic (remains unchanged)
                resolved_type = ResolvedType::Unknown; // Type determined later
                ResolvedExprKind::Field {
                    object: Box::new(resolved_object),
                    field_name: name.name.clone(),
                }
            }
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

                    resolved_type = ResolvedType::UserDefined {
                        symbol: struct_symbol,
                        type_args: None, // TODO: Handle generics application
                    };
                    ResolvedExprKind::Struct {
                        struct_symbol,
                        fields: resolved_ast_fields,
                        base: resolved_base,
                    }
                }
                Err(e) => return Err(e),
            }
        }
        ast::expr::ExprKind::Array(elements) => {
            let mut resolved_elements = Vec::new();
            let mut element_type = ResolvedType::Unknown;
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
                    element_type = resolved_elem.resolved_type.clone();
                } else {
                    // TODO: Unify subsequent element types with the first one
                    // if !check_types_equal(&resolved_elem.resolved_type, &element_type) {
                }
                resolved_elements.push(resolved_elem);
            }
            resolved_type = ResolvedType::Array {
                element_type: Box::new(element_type),
                size: Some(elements.len()),
            };
            ResolvedExprKind::Array(resolved_elements)
        }
        ast::expr::ExprKind::Tuple(elements) => {
            let mut resolved_elements = Vec::new();
            let mut element_types = Vec::new();
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
                element_types.push(resolved_elem.resolved_type.clone());
                resolved_elements.push(resolved_elem);
            }
            resolved_type = ResolvedType::Tuple(element_types);
            ResolvedExprKind::Tuple(resolved_elements)
        }
        ast::expr::ExprKind::Map(entries) => {
            let mut resolved_entries = Vec::new();
            let mut key_type = ResolvedType::Unknown;
            let mut value_type = ResolvedType::Unknown;
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
                    key_type = resolved_key.resolved_type.clone();
                    value_type = resolved_value.resolved_type.clone();
                } else {
                    // TODO: Unify subsequent key/value types with the first one
                    // if !check_types_equal(&resolved_key.resolved_type, &key_type) {
                }
                resolved_entries.push((resolved_key, resolved_value));
            }
            resolved_type = ResolvedType::Unknown; // TODO: Proper Map type
            ResolvedExprKind::Map(resolved_entries)
        }
        ast::expr::ExprKind::HashSet(elements) => {
            let mut resolved_elements = Vec::new();
            let mut element_type = ResolvedType::Unknown;
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
                    element_type = resolved_elem.resolved_type.clone();
                } else {
                    // TODO: Unify subsequent element types with the first one
                    // if !check_types_equal(&resolved_elem.resolved_type, &element_type) {
                }
                resolved_elements.push(resolved_elem);
            }
            resolved_type = ResolvedType::Unknown; // TODO: Proper Set type
            ResolvedExprKind::HashSet(resolved_elements)
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
            resolved_type = resolved_inner.resolved_type.clone(); // Use inner type
            ResolvedExprKind::Paren(Box::new(resolved_inner))
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
                // 1. Push a new scope for this arm's bindings
                scope_stack.push_scope();

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

                // 3. Resolve the expression on the right side of the arm
                //    This resolution happens within the scope created for the arm.
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


                // 4. Pop the scope created for this arm
                scope_stack.pop_scope();

                arm_types.push(resolved_arm_expr.resolved_type.clone());
                resolved_arms.push((resolved_pattern, resolved_arm_expr));
                combined_divergence &= arm_diverges; // If any arm doesn't diverge, the match doesn't
            }

            // TODO: Check exhaustiveness of patterns.
            // TODO: Determine the overall type of the match expression.
            // This usually involves finding the least upper bound (LUB) or common supertype
            // of all arm expression types. For now, use the type of the first arm or Unknown.
            resolved_type = arm_types.first().cloned().unwrap_or(ResolvedType::Unknown);

            // TODO: Handle divergence
            // diverges = combined_divergence; // Update the divergence status based on arms

            ResolvedExprKind::Match {
                scrutinee: Box::new(resolved_scrutinee),
                arms: resolved_arms,
            }
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
                    symbol: param_symbol,
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
            resolved_type = lambda_type; // Assign the lambda's type
            ResolvedExprKind::Lambda {
                params: resolved_params,
                body: Box::new(resolved_body),
            }
        }
        _ => {
             // Report internal error for unsupported kinds
             errors.push(ResolutionError::InternalError {
                 message: format!("Unsupported expression kind {:?} encountered during resolution.", ast_expr.kind),
                 span: Some(ast_expr.span),
             });
             // Return error directly
             return Err(ResolutionError::InternalError { 
                 message: format!("Unsupported expression kind {:?}", ast_expr.kind),
                 span: Some(ast_expr.span)
             }); 
        }
    };

    // --- Construct final ResolvedExpr --- 
    Ok((
        ResolvedExpr {
            kind: resolved_kind,
            span: ast_expr.span,
            resolved_type,
        },
        encountered_effectful_call_in_sub,
    ))
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
                // Check if the cloned function AST exists
                if def_info.ast_function.is_some() {
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
            DefinitionKind::AssociatedType => ResolvedType::Unknown, // An assoc type *declaration* isn't a type itself in expr context
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
    let span = ast_pattern.span;
    let is_diverging = false; // Default unless sub-pattern/expr diverges

    let resolved_kind = match &ast_pattern.kind {
        ast::pattern::PatternKind::Identifier(ident) => {
            let name = ident.name.clone();

            let binding_symbol = Symbol::fresh();

            // Create a local binding for this identifier.
            // The type of the binding is the type expected for this pattern position.
            let binding = LocalBinding {
                name: name.clone(),
                resolved_type: expected_type.clone(), // Variable gets the type of the pattern
                symbol: binding_symbol,
                defined_at: ident.span,
                used: false,       // Mark unused initially
                is_mutable: false, // TODO: Determine mutability (e.g., `let mut x`)
            };

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
            // Literals, Paths, and Variables don't contain sub-expressions with calls
            ResolvedExprKind::Literal(_) | ResolvedExprKind::Path(_) | ResolvedExprKind::Variable { .. } => {}
            // <<< ADDED SelfRef arm >>>
            ResolvedExprKind::SelfRef => {}
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
            symbol: Symbol::new(u32::MAX), // Placeholder
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
} // End of tests module
