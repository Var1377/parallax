// parallax-hir/src/lower/mod.rs
// Main module for the lowering pass (Typed AST -> ANF HIR)

// Declare sub-modules
pub mod expr;
pub mod items;
pub mod pattern;
pub mod types;

// Use functions from sub-modules
use expr::*;
use items::*;
use pattern::*;
use types::*;

// Keep necessary imports here
use crate::hir::*;
use miette::SourceSpan;
use parallax_resolve::types::Symbol as TypeSymbol;
use parallax_types::types::{TypedModule, TypedArgument, Ty, TyKind, PrimitiveType as ParallaxPrimitiveType, TypedExpr, TypedExprKind, TypedPattern, TypedPatternKind, TypedDefinitions};
use parallax_types::types::{TypedFunction, TypedStruct, TypedEnum, TypedField, TypedVariant};
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::atomic::{AtomicU32, Ordering};
use std::cmp::max;


// --- Lowering Context ---

/// Context for lowering from Typed AST to ANF HIR.
pub(super) struct LoweringContext<'def> {
    /// Counter for generating fresh HirVar IDs.
    next_hir_var_id: AtomicU32,
    /// Counter for generating fresh Symbol IDs (for lambdas, etc.).
    next_symbol_id: AtomicU32,
    /// Access to the global definitions (structs, enums, functions).
    definitions: &'def TypedDefinitions,
    /// Map from TypeSymbol (from Typed AST) to HirVar (for function parameters/locals).
    /// Organized as a stack of scopes to handle nested bindings.
    symbol_to_hir_var: Vec<HashMap<TypeSymbol, HirVar>>,
    /// Map from TypeSymbol to its original Ty, mirroring symbol_to_hir_var scoping.
    symbol_to_type: Vec<HashMap<TypeSymbol, Ty>>,
    /// Collects HIR functions generated for lambdas.
    pub(super) generated_lambda_functions: Vec<HirFunction>,
}

impl<'def> LoweringContext<'def> {
    pub fn new(definitions: &'def TypedDefinitions) -> Self {
        println!("[LoweringContext] Creating new context");
        // Find the maximum existing symbol ID to avoid collisions
        let max_existing_symbol_id = find_max_symbol_id(definitions);
        LoweringContext {
            next_hir_var_id: AtomicU32::new(0),
            // Start fresh symbol IDs *after* the max existing one
            next_symbol_id: AtomicU32::new(max_existing_symbol_id + 1),
            definitions,
            symbol_to_hir_var: vec![HashMap::new()], // Start with one empty scope
            symbol_to_type: vec![HashMap::new()],    // Start with one empty scope
            generated_lambda_functions: Vec::new(), // Initialize as empty
        }
    }

    /// Generates a fresh HirVar ID.
    fn fresh_hir_var(&mut self) -> HirVar {
        let id = self.next_hir_var_id.fetch_add(1, Ordering::SeqCst);
        let var = HirVar(id);
        var
    }

    /// Generates a fresh Symbol ID.
    fn fresh_symbol(&mut self) -> TypeSymbol {
        let id = self.next_symbol_id.fetch_add(1, Ordering::SeqCst);
        let symbol = TypeSymbol::new(id);
        symbol
    }

    /// Get a HirVar for a TypeSymbol (e.g., function parameter) from any active scope.
    /// Returns None if the symbol isn't found in any scope.
    fn get_hir_var(&self, symbol: TypeSymbol) -> Option<HirVar> {
        // Search scopes from innermost to outermost
        for (i, scope) in self.symbol_to_hir_var.iter().rev().enumerate() {
            if let Some(hir_var) = scope.get(&symbol) {
                return Some(*hir_var);
            }
        }
        None
    }

    /// Get the original Ty for a TypeSymbol from any active scope.
    /// Returns None if the symbol isn't found in any scope.
    fn get_type_for_symbol(&self, symbol: TypeSymbol) -> Option<Ty> {
        // Search scopes from innermost to outermost
        for scope in self.symbol_to_type.iter().rev() {
            if let Some(ty) = scope.get(&symbol) {
                return Some(ty.clone()); // Clone the type
            }
        }
        None
    }

    /// Get or create a HirVar for a TypeSymbol (e.g., function parameter).
    /// Creates the binding in the current (innermost) scope.
    ///
    /// **Important**: This only manages the HirVar mapping. Use `add_binding_with_type`
    /// if you need to register both the HirVar and its original Ty.
    fn get_or_create_hir_var(&mut self, symbol: TypeSymbol) -> HirVar {
        // Try to find in any scope first
        if let Some(var) = self.get_hir_var(symbol) {
            return var;
        }
        
        // Not found, create a new one in the current scope
        // Call fresh_hir_var first to avoid simultaneous mutable borrows
        let new_var = self.fresh_hir_var(); 
        let current_scope = self.symbol_to_hir_var.last_mut().unwrap();
        // Note: We are not adding to symbol_to_type here. Caller must use add_binding_with_type
        // if the type association is needed later via get_type_for_symbol.
        current_scope.insert(symbol, new_var);
        new_var
    }

    /// Push a new scope for variables. Used when entering constructs that introduce 
    /// new variable bindings (match arms, lambdas, etc.)
    fn push_scope(&mut self) {
        println!("[LoweringContext] Pushing new scope (level {})", self.symbol_to_hir_var.len());
        self.symbol_to_hir_var.push(HashMap::new());
        self.symbol_to_type.push(HashMap::new());
    }

    /// Pop the innermost scope, removing any variables that were bound within it.
    fn pop_scope(&mut self) {
        println!("[LoweringContext] Popping scope (level {})", self.symbol_to_hir_var.len() - 1);
        if self.symbol_to_hir_var.len() > 1 {
            self.symbol_to_hir_var.pop();
            self.symbol_to_type.pop();
        } else {
            println!("WARN: Attempted to pop the last scope in LoweringContext.");
        }
    }

    /// Add a specific binding (Symbol -> HirVar) and its type (Symbol -> Ty)
    /// to the current scope. This is the preferred way to add bindings.
    fn add_binding_with_type(&mut self, symbol: TypeSymbol, hir_var: HirVar, ty: Ty) {
        println!("[LoweringContext] Adding binding: {:?} -> {:?} (type: {:?}) in scope {}",
                 symbol, hir_var, ty.kind, self.symbol_to_hir_var.len() - 1);
        let current_var_scope = self.symbol_to_hir_var.last_mut().unwrap();
        current_var_scope.insert(symbol, hir_var);
        let current_type_scope = self.symbol_to_type.last_mut().unwrap();
        current_type_scope.insert(symbol, ty);
    }

    /// Get the definitions.
    fn definitions(&self) -> &'def TypedDefinitions {
        self.definitions
    }
}

// --- Helper to find max Symbol ID ---

fn find_max_symbol_id(defs: &TypedDefinitions) -> u32 {
    let mut max_id = 0;

    // Check functions and their parameters/locals (though locals aren't directly here)
    for (fn_sym, func) in &defs.functions {
        max_id = max(max_id, fn_sym.id());
        for param in &func.params {
            max_id = max(max_id, param.symbol.id());
        }
        // We might need a deeper dive into function bodies if TypedExpr contains Symbols directly?
        // For now, assume top-level symbols and params cover most cases.
    }

    // Check structs and their fields
    for (st_sym, st) in &defs.structs {
        max_id = max(max_id, st_sym.id());
        for field in &st.fields {
            max_id = max(max_id, field.symbol.id());
        }
    }

    // Check enums and their variants/fields
    for (en_sym, en) in &defs.enums {
        max_id = max(max_id, en_sym.id());
        for variant in &en.variants {
            match variant {
                TypedVariant::Unit { symbol, .. } => max_id = max(max_id, symbol.id()),
                TypedVariant::Tuple { symbol, .. } => max_id = max(max_id, symbol.id()), // Check symbol, not contained types
                TypedVariant::Struct { symbol, fields, .. } => {
                    max_id = max(max_id, symbol.id());
                    for field in fields {
                        max_id = max(max_id, field.symbol.id());
                    }
                }
            }
        }
    }
    
    println!("[LoweringContext] Max existing symbol ID found: {}", max_id);
    max_id
}


// --- Top-Level Lowering Function ---

/// Lowers a complete typed module (output from type checking) to ANF HIR.
pub fn lower_module_to_anf_hir(typed_module: &TypedModule) -> HirModule {
    let defs = &typed_module.definitions;
    let mut ctx = LoweringContext::new(defs);

    let mut hir_functions = Vec::new();
    let mut hir_structs = Vec::new();
    let mut hir_enums = Vec::new();

    // Lower functions
    for (&symbol, typed_function) in &defs.functions {
        let hir_function = lower_function_to_hir(&mut ctx, symbol, typed_function, defs);
        hir_functions.push(hir_function);
    }

    // Lower structs
    for (&symbol, typed_struct) in &defs.structs {
        let hir_struct = lower_struct_def(&mut ctx, symbol, typed_struct);
        hir_structs.push(hir_struct);
    }

    // Lower enums
    for (&symbol, typed_enum) in &defs.enums {
        let hir_enum = lower_enum_def(&mut ctx, symbol, typed_enum);
        hir_enums.push(hir_enum);
    }

    // *** Add generated lambda functions ***
    println!("[LoweringContext] Adding {} generated lambda functions", ctx.generated_lambda_functions.len());
    // println!("[DEBUG] hir_functions before extend: {:?}", hir_functions.iter().map(|f| f.symbol).collect::<Vec<_>>());
    // We need to take ownership from the context or clone them
    let lambdas = ctx.generated_lambda_functions.into_iter(); // Consume the Vec
    // println!("[DEBUG] Lambdas to add: {:?}", lambdas.clone().map(|f| f.symbol).collect::<Vec<_>>()); // Can't clone iterator easily
    hir_functions.extend(lambdas); 
    // println!("[DEBUG] hir_functions after extend: {:?}", hir_functions.iter().map(|f| f.symbol).collect::<Vec<_>>());

    // Retrieve the final ID counter value after lowering
    let final_var_id = ctx.next_hir_var_id.load(Ordering::SeqCst);

    HirModule {
        name: "main".to_string(), // Placeholder name
        functions: hir_functions,
        structs: hir_structs,
        enums: hir_enums,
        statics: Vec::new(), // Add missing statics field
        entry_point: typed_module.entry_point, // Copy entry point symbol
        intrinsics: typed_module.intrinsics.clone(), // Propagate intrinsics
        next_var_id: final_var_id, // Store the next available ID
    }
}

// Helper function to lower a single function
fn lower_function_to_hir<'def>(
    ctx: &mut LoweringContext<'def>,
    symbol: TypeSymbol,
    typed_function: &'def TypedFunction,
    defs: &'def TypedDefinitions
) -> HirFunction {
    let sig = lower_signature(ctx, typed_function, defs);
    let body = lower_body(ctx, typed_function, defs);

     HirFunction {
         symbol,
         name: typed_function.name.clone(),
         signature: sig,
         body,
         span: typed_function.span,
     }
}

// Placeholder for function body lowering
fn lower_body<'def>(
    ctx: &mut LoweringContext<'def>,
    typed_function: &'def TypedFunction,
    defs: &'def TypedDefinitions
) -> Option<HirExpr> {
    typed_function.body.as_ref().map(|body_expr| {
        // --- Add function parameters to the context's initial scope ---
        // Clear existing bindings in the base scope (should be empty, but safety first)
        // ctx.symbol_to_hir_var[0].clear(); // Removed - context should persist between sig/body
        // ctx.symbol_to_type[0].clear();    // Removed - context should persist between sig/body
        
        // Ensure we are operating on the base scope for parameters
        assert_eq!(ctx.symbol_to_hir_var.len(), 1, "Context should have only base scope before lowering body");
        assert_eq!(ctx.symbol_to_type.len(), 1, "Context should have only base scope before lowering body");
        
        for param in &typed_function.params {
            // Use get_or_create_hir_var to ensure the variable is registered
            // even if the signature lowering somehow didn't put it in the scope yet.
            let hir_var = ctx.get_or_create_hir_var(param.symbol);
            // Add the type mapping explicitly using add_binding_with_type
            // which will handle both maps correctly.
            ctx.add_binding_with_type(param.symbol, hir_var, param.ty.clone());
        }
        // --- End parameter registration ---

        // Prepare the lambda body context if needed (e.g., for nested lambdas within the func)
        // Might need push_scope / pop_scope around lower_expression if func body adds scopes.
        
        lower_expression(ctx, body_expr, defs)
    })
}

// --- Utility Functions ---

/// Alias for binding data tuple (var, type, value)
pub(super) type BindingData = (HirVar, HirType, HirValue);

/// Helper to decompose an HirExpr into bindings and the final tail expression.
/// Note: This consumes the expression.
pub fn flatten_hir_expr(expr: HirExpr) -> (Vec<BindingData>, HirTailExpr) {
    let mut bindings = Vec::new();
    let mut current_expr = expr;

    loop {
        match current_expr.kind {
            HirExprKind::Let { var, var_ty, value, rest } => {
                bindings.push((var, var_ty, *value));
                current_expr = *rest;
            }
            HirExprKind::Tail(tail_expr) => {
                // Reverse bindings to maintain original order for build_nested_lets
                bindings.reverse();
                return (bindings, tail_expr);
            }
        }
    }
}