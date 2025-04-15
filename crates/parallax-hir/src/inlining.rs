// parallax-hir/src/inlining.rs
use crate::hir::*;
use parallax_resolve::types::Symbol;
use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::atomic::{AtomicU32, Ordering}; // For generating fresh IDs

const INLINING_SIZE_THRESHOLD: usize = 25; // Configurable threshold

// --- Call Graph & Recursion Detection ---

struct CallGraph {
    // Maps function symbol to the set of function symbols it calls directly
    graph: HashMap<Symbol, HashSet<Symbol>>,
    // Set of all functions involved in any recursion cycle
    recursive_functions: HashSet<Symbol>,
}

impl CallGraph {
    fn build(module: &HirModule) -> Self {
        let mut graph = HashMap::new();
        let mut recursive_functions = HashSet::new();

        // 1. Build the initial graph
        for func in &module.functions {
            let callees = Self::find_direct_callees(func);
            graph.insert(func.symbol, callees);
        }

        // 2. Detect cycles
        let mut visited = HashSet::new(); // Nodes visited in the current overall traversal
        let mut recursion_stack = HashSet::new(); // Nodes currently in the DFS path

        for func_symbol in graph.keys() {
            if !visited.contains(func_symbol) {
                Self::detect_cycles_dfs(
                    *func_symbol,
                    &graph,
                    &mut visited,
                    &mut recursion_stack,
                    &mut recursive_functions,
                );
            }
        }

        CallGraph { graph, recursive_functions }
    }

    // Finds direct calls to global functions within a function's body
    fn find_direct_callees(func: &HirFunction) -> HashSet<Symbol> {
        let mut callees = HashSet::new();
        if let Some(body) = &func.body {
            Self::collect_callees_expr(body, &mut callees);
        }
        callees
    }

    fn collect_callees_expr(expr: &HirExpr, callees: &mut HashSet<Symbol>) {
        match &expr.kind {
            HirExprKind::Let { value, rest, .. } => {
                Self::collect_callees_value(value, callees);
                Self::collect_callees_expr(rest, callees);
            }
            HirExprKind::Tail(tail_expr) => {
                Self::collect_callees_tail(tail_expr, callees);
            }
        }
    }

    fn collect_callees_value(value: &HirValue, callees: &mut HashSet<Symbol>) {
        match value {
            HirValue::Call { func: Operand::Global(callee_symbol), args, .. } => {
                callees.insert(*callee_symbol);
                for arg in args { Self::collect_callees_operand(arg, callees); }
            }
            HirValue::Call { func, args, .. } => {
                // Call via variable/closure - handle operands
                Self::collect_callees_operand(func, callees);
                 for arg in args { Self::collect_callees_operand(arg, callees); }
            }
            HirValue::Aggregate { fields, .. } => {
                 for field in fields { Self::collect_callees_operand(field, callees); }
            }
            HirValue::Project { base, projection, .. } => {
                Self::collect_callees_operand(base, callees);
                 if let ProjectionKind::ArrayIndex(op) = projection {
                     Self::collect_callees_operand(op, callees);
                 }
            }
             HirValue::Closure { function_symbol, captures, .. } => {
                 // The referenced function_symbol itself might be called later,
                 // but the closure creation itself doesn't call it directly here.
                 // We *could* add function_symbol here if we want to prevent inlining
                 // functions that *create* closures referencing recursive functions,
                 // but let's stick to direct calls for now.
                 for capture in captures { Self::collect_callees_operand(capture, callees); }
             }
             HirValue::Use(op) => Self::collect_callees_operand(op, callees),
        }
    }

     fn collect_callees_tail(tail_expr: &HirTailExpr, callees: &mut HashSet<Symbol>) {
         match tail_expr {
             HirTailExpr::Return(op) => Self::collect_callees_operand(op, callees),
             HirTailExpr::If { condition, then_branch, else_branch } => {
                 Self::collect_callees_operand(condition, callees);
                 Self::collect_callees_expr(then_branch, callees);
                 Self::collect_callees_expr(else_branch, callees);
             }
             HirTailExpr::Match { scrutinee, arms, otherwise } => {
                 Self::collect_callees_operand(scrutinee, callees);
                 for (_pattern, expr) in arms {
                     Self::collect_callees_expr(expr, callees);
                     // Patterns don't contain calls
                 }
                 if let Some(otherwise_expr) = otherwise {
                     Self::collect_callees_expr(otherwise_expr, callees);
                 }
             }
             HirTailExpr::Never => {}
         }
     }

     fn collect_callees_operand(operand: &Operand, _callees: &mut HashSet<Symbol>) {
         // Operands themselves (Var, Const, Global) don't represent calls directly
         match operand {
             Operand::Var(_) | Operand::Const(_) | Operand::Global(_) => {}
         }
     }


    // DFS based cycle detection
    fn detect_cycles_dfs(
        node: Symbol,
        graph: &HashMap<Symbol, HashSet<Symbol>>,
        visited: &mut HashSet<Symbol>,
        recursion_stack: &mut HashSet<Symbol>,
        recursive_functions: &mut HashSet<Symbol>,
    ) {
        visited.insert(node);
        recursion_stack.insert(node);

        if let Some(neighbors) = graph.get(&node) {
            for &neighbor in neighbors {
                if recursion_stack.contains(&neighbor) {
                    // Cycle detected involving 'neighbor' and nodes in recursion_stack
                    // Mark all nodes in the current path as recursive (might over-approximate slightly, but safe)
                    // A more precise approach would trace back the cycle.
                    recursive_functions.extend(recursion_stack.iter());
                    // Mark the neighbor itself too, as it's part of the cycle
                    recursive_functions.insert(neighbor);
                } else if !visited.contains(&neighbor) {
                    Self::detect_cycles_dfs(
                        neighbor,
                        graph,
                        visited,
                        recursion_stack,
                        recursive_functions,
                    );
                }
                // If visited but not in recursion_stack, it's part of a completed path, no cycle here.

                // If the neighbor was found to be recursive in a sub-call, mark the current node too?
                // No, only mark if part of a direct cycle found *in this path*.
                 if recursive_functions.contains(&neighbor) {
                    // If we call a function that is known to be recursive,
                    // should we mark the caller as potentially recursive for inlining purposes?
                    // Maybe not strictly necessary for cycle detection, but useful for the inliner.
                    // For now, let's stick to direct cycle detection.
                }
            }
        }

        recursion_stack.remove(&node); // Remove node from path when backtracking
    }

    fn is_recursive(&self, func_symbol: Symbol) -> bool {
        self.recursive_functions.contains(&func_symbol)
    }
}

// --- Size Calculation ---

fn calculate_hir_size(expr: &HirExpr) -> usize {
    let mut size = 0;
    count_nodes_expr(expr, &mut size);
    size
}

fn count_nodes_expr(expr: &HirExpr, size: &mut usize) {
    *size += 1; // Count this expression node
    match &expr.kind {
        HirExprKind::Let { value, rest, .. } => {
            count_nodes_value(value, size);
            count_nodes_expr(rest, size);
        }
        HirExprKind::Tail(tail_expr) => {
            count_nodes_tail(tail_expr, size);
        }
    }
}

fn count_nodes_value(value: &HirValue, size: &mut usize) {
     *size += 1; // Count this value node
     match value {
         HirValue::Call { args, .. } => {
             // Count operands within args (base func operand is handled below)
             for arg in args { count_nodes_operand(arg, size); }
         }
         HirValue::Aggregate { fields, .. } => {
              for field in fields { count_nodes_operand(field, size); }
         }
         HirValue::Project { base, projection, .. } => {
             count_nodes_operand(base, size);
              if let ProjectionKind::ArrayIndex(op) = projection {
                  count_nodes_operand(op, size);
              }
         }
          HirValue::Closure { captures, .. } => {
              for capture in captures { count_nodes_operand(capture, size); }
          }
          HirValue::Use(op) => count_nodes_operand(op, size),
     }
}

fn count_nodes_tail(tail_expr: &HirTailExpr, size: &mut usize) {
    *size += 1; // Count this tail expression node
    match tail_expr {
        HirTailExpr::Return(op) => count_nodes_operand(op, size),
        HirTailExpr::If { condition, then_branch, else_branch } => {
            count_nodes_operand(condition, size);
            count_nodes_expr(then_branch, size);
            count_nodes_expr(else_branch, size);
        }
        HirTailExpr::Match { scrutinee, arms, otherwise } => {
            count_nodes_operand(scrutinee, size);
            for (_pattern, expr) in arms {
                // Don't count pattern nodes for size heuristic? Or add small cost?
                count_nodes_expr(expr, size);
            }
            if let Some(otherwise_expr) = otherwise {
                count_nodes_expr(otherwise_expr, size);
            }
        }
        HirTailExpr::Never => {}
    }
}

fn count_nodes_operand(operand: &Operand, size: &mut usize) {
    *size += 1; // Count operand node
    match operand {
        Operand::Var(_) | Operand::Const(_) | Operand::Global(_) => {}
    }
}


// --- Inlining Context & Substitution ---
struct InliningContext<'a> {
    param_subst: &'a HashMap<HirVar, Operand>,
    var_map: &'a mut HashMap<HirVar, HirVar>,
    next_hir_var_id: &'a AtomicU32,
}
impl<'a> InliningContext<'a> {
    fn fresh_hir_var(&mut self) -> HirVar {
        HirVar(self.next_hir_var_id.fetch_add(1, Ordering::SeqCst))
    }
    fn get_or_create_renamed_var(&mut self, original_var: HirVar) -> HirVar {
        if let Some(renamed) = self.var_map.get(&original_var) {
            *renamed
        } else {
            let fresh = self.fresh_hir_var();
            self.var_map.insert(original_var, fresh);
            fresh
        }
    }
}
fn substitute_and_rename_operand(operand: &Operand, ctx: &mut InliningContext<'_>) -> Operand {
    match operand {
        Operand::Var(v) => {
            if let Some(subst_operand) = ctx.param_subst.get(v) {
                 subst_operand.clone()
            }
            else {
                 Operand::Var(ctx.get_or_create_renamed_var(*v))
            }
        }
        Operand::Const(_) | Operand::Global(_) => operand.clone(),
    }
}
fn substitute_and_rename_type(ty: &HirType, _ctx: &mut InliningContext<'_>) -> HirType {
    ty.clone()
}
fn substitute_and_rename_pattern(pattern: &HirPattern, ctx: &mut InliningContext<'_>) -> HirPattern {
    match pattern {
        HirPattern::Variant { variant_symbol, bindings } => {
             let new_bindings = bindings.iter().map(|(var, ty)| {
                 (ctx.get_or_create_renamed_var(*var), substitute_and_rename_type(ty, ctx))
             }).collect();
             HirPattern::Variant { variant_symbol: *variant_symbol, bindings: new_bindings }
        }
        HirPattern::Const(lit) => HirPattern::Const(lit.clone()),
        HirPattern::Bind { var, var_ty } => {
             HirPattern::Bind { var: ctx.get_or_create_renamed_var(*var), var_ty: substitute_and_rename_type(var_ty, ctx) }
        }
        HirPattern::Wildcard => HirPattern::Wildcard,
    }
}
fn substitute_and_rename_value(value: &HirValue, ctx: &mut InliningContext<'_>) -> HirValue {
    match value {
        HirValue::Use(op) => HirValue::Use(substitute_and_rename_operand(op, ctx)),
        HirValue::Call { func, args } => {
            HirValue::Call { func: substitute_and_rename_operand(func, ctx), args: args.iter().map(|arg| substitute_and_rename_operand(arg, ctx)).collect() }
        }
        HirValue::Aggregate { kind, fields } => {
            HirValue::Aggregate { kind: kind.clone(), fields: fields.iter().map(|f| substitute_and_rename_operand(f, ctx)).collect() }
        }
        HirValue::Project { base, projection } => {
             let new_projection = match projection {
                 ProjectionKind::ArrayIndex(op) => ProjectionKind::ArrayIndex(substitute_and_rename_operand(op, ctx)),
                 _ => projection.clone(),
             };
             HirValue::Project { base: substitute_and_rename_operand(base, ctx), projection: new_projection }
        }
        HirValue::Closure { function_symbol, captures } => {
             HirValue::Closure { function_symbol: *function_symbol, captures: captures.iter().map(|cap| substitute_and_rename_operand(cap, ctx)).collect() }
        }
    }
}
fn substitute_and_rename_tail(tail_expr: &HirTailExpr, ctx: &mut InliningContext<'_>) -> HirTailExpr {
    match tail_expr {
        HirTailExpr::Return(op) => HirTailExpr::Return(substitute_and_rename_operand(op, ctx)),
        HirTailExpr::If { condition, then_branch, else_branch } => {
            HirTailExpr::If {
                condition: substitute_and_rename_operand(condition, ctx),
                then_branch: Box::new(substitute_and_rename_expr(then_branch, ctx)),
                else_branch: Box::new(substitute_and_rename_expr(else_branch, ctx)),
            }
        }
        HirTailExpr::Match { scrutinee, arms, otherwise } => {
             let new_arms = arms.iter().map(|(pattern, expr)| {
                 (substitute_and_rename_pattern(pattern, ctx), substitute_and_rename_expr(expr, ctx))
             }).collect();
             HirTailExpr::Match {
                 scrutinee: substitute_and_rename_operand(scrutinee, ctx),
                 arms: new_arms,
                 otherwise: otherwise.as_ref().map(|expr| Box::new(substitute_and_rename_expr(expr, ctx))),
             }
        }
        HirTailExpr::Never => HirTailExpr::Never,
    }
}
fn substitute_and_rename_expr(expr: &HirExpr, ctx: &mut InliningContext<'_>) -> HirExpr {
    let new_ty = substitute_and_rename_type(&expr.ty, ctx);
    let new_kind = match &expr.kind {
        HirExprKind::Let { var, var_ty, value, rest } => {
            let new_value = substitute_and_rename_value(value, ctx);
            let fresh_var = ctx.get_or_create_renamed_var(*var);
            let new_var_ty = substitute_and_rename_type(var_ty, ctx);
            let new_rest = substitute_and_rename_expr(rest, ctx);
            HirExprKind::Let {
                var: fresh_var,
                var_ty: new_var_ty,
                value: Box::new(new_value),
                rest: Box::new(new_rest),
            }
        }
        HirExprKind::Tail(tail_expr) => {
            HirExprKind::Tail(substitute_and_rename_tail(tail_expr, ctx))
        }
    };
    HirExpr {
        kind: new_kind,
        ty: new_ty,
        span: expr.span,
    }
}

// --- Max ID Calculation ---

fn find_max_hir_var_id(module: &HirModule) -> u32 {
    let mut max_id: u32 = 0;
    let mut visitor = MaxIdVisitor { max_id: &mut max_id };
    for func in &module.functions {
        visitor.visit_function(func);
    }
    max_id // Return the maximum found ID
}

struct MaxIdVisitor<'a> {
    max_id: &'a mut u32,
}

impl<'a> MaxIdVisitor<'a> {
    fn visit_function(&mut self, func: &HirFunction) {
        for (var, _) in &func.signature.params {
            *self.max_id = (*self.max_id).max(var.0);
        }
        if let Some(body) = &func.body {
            self.visit_expr(body);
        }
    }

    fn visit_expr(&mut self, expr: &HirExpr) {
        match &expr.kind {
            HirExprKind::Let { var, value, rest, .. } => {
                *self.max_id = (*self.max_id).max(var.0);
                self.visit_value(value);
                self.visit_expr(rest);
            }
            HirExprKind::Tail(tail) => self.visit_tail(tail),
        }
    }

    fn visit_value(&mut self, value: &HirValue) {
        match value {
            HirValue::Use(op) |
            HirValue::Call { func: op, .. } |
            HirValue::Project { base: op, .. } => self.visit_operand(op),
            HirValue::Aggregate { fields, .. } => {
                for field in fields { self.visit_operand(field); }
            }
            HirValue::Closure { captures, .. } => {
                for cap in captures { self.visit_operand(cap); }
            }
        }
        if let HirValue::Call { args, .. } = value {
            for arg in args { self.visit_operand(arg); }
        }
        if let HirValue::Project { projection: ProjectionKind::ArrayIndex(op), .. } = value {
             self.visit_operand(op);
        }
    }

    fn visit_tail(&mut self, tail: &HirTailExpr) {
        match tail {
            HirTailExpr::Return(op) => self.visit_operand(op),
            HirTailExpr::If { condition, then_branch, else_branch } => {
                self.visit_operand(condition);
                self.visit_expr(then_branch);
                self.visit_expr(else_branch);
            }
            HirTailExpr::Match { scrutinee, arms, otherwise } => {
                self.visit_operand(scrutinee);
                for (pat, expr) in arms {
                    self.visit_pattern(pat);
                    self.visit_expr(expr);
                }
                if let Some(other) = otherwise { self.visit_expr(other); }
            }
            HirTailExpr::Never => {},
        }
    }

    fn visit_operand(&mut self, operand: &Operand) {
        if let Operand::Var(var) = operand {
            *self.max_id = (*self.max_id).max(var.0);
        }
    }

    fn visit_pattern(&mut self, pattern: &HirPattern) {
        match pattern {
            HirPattern::Variant { bindings, .. } => {
                for (var, _) in bindings { *self.max_id = (*self.max_id).max(var.0); }
            }
            HirPattern::Bind { var, .. } => {
                *self.max_id = (*self.max_id).max(var.0);
            }
            HirPattern::Const(_) | HirPattern::Wildcard => {},
        }
    }
}

// --- Transformation Pass ---
enum TransformedValue {
    Value(HirValue),
    Inlined(HirExpr),
}

fn transform_expr(
    expr: &HirExpr,
    module: &HirModule,
    candidates: &HashSet<Symbol>,
    next_hir_var_id: &AtomicU32,
) -> HirExpr {
    match &expr.kind {
        HirExprKind::Let { var, var_ty, value, rest } => {
            let transformed_rest = transform_expr(rest, module, candidates, next_hir_var_id);
            let transformed_value_result = transform_value(value, module, candidates, next_hir_var_id);

            match transformed_value_result {
                TransformedValue::Value(new_value) => {
                    let rest_ty = transformed_rest.ty.clone();
                    HirExpr {
                        kind: HirExprKind::Let {
                            var: *var,
                            var_ty: substitute_and_rename_type(var_ty, &mut InliningContext { param_subst: &HashMap::new(), var_map: &mut HashMap::new(), next_hir_var_id }),
                            value: Box::new(new_value),
                            rest: Box::new(transformed_rest),
                        },
                        ty: rest_ty,
                        span: expr.span,
                    }
                },
                TransformedValue::Inlined(inlined_expr) => {
                    stitch_inlined_expr(*var, inlined_expr, transformed_rest)
                }
            }
        }
        HirExprKind::Tail(tail_expr) => {
            let transformed_tail = transform_tail_expr(tail_expr, module, candidates, next_hir_var_id);
            let new_type = match &transformed_tail {
                HirTailExpr::Return(op) => {
                    substitute_and_rename_type(&expr.ty, &mut InliningContext { param_subst: &HashMap::new(), var_map: &mut HashMap::new(), next_hir_var_id })
                },
                HirTailExpr::Never => HirType::Never,
                HirTailExpr::If { then_branch, else_branch, .. } => {
                    then_branch.ty.clone()
                },
                HirTailExpr::Match { arms, otherwise, .. } => {
                    if let Some(other_expr) = otherwise {
                        other_expr.ty.clone()
                    } else if let Some((_, first_arm_expr)) = arms.first() {
                        first_arm_expr.ty.clone()
                    } else {
                        expr.ty.clone()
                    }
                }
            };
            HirExpr {
                kind: HirExprKind::Tail(transformed_tail),
                ty: new_type,
                span: expr.span,
            }
        }
    }
}

fn transform_value(
    value: &HirValue,
    module: &HirModule,
    candidates: &HashSet<Symbol>,
    next_hir_var_id: &AtomicU32,
) -> TransformedValue {
    match value {
        HirValue::Call { func: Operand::Global(callee_symbol), args } if candidates.contains(callee_symbol) => {
            println!(" --> Inlining call to: {}", module.functions.iter().find(|f| f.symbol == *callee_symbol).map_or("?", |f| &f.name));
             let owned_args = args.iter().map(transform_operand).collect();
             TransformedValue::Inlined(inline_call_site(*callee_symbol, owned_args, module, next_hir_var_id))
        }
        HirValue::Call { func, args } => {
            TransformedValue::Value(HirValue::Call { func: transform_operand(func), args: args.iter().map(transform_operand).collect() })
        }
         HirValue::Use(op) => TransformedValue::Value(HirValue::Use(transform_operand(op))),
         HirValue::Aggregate { kind, fields } => {
             TransformedValue::Value(HirValue::Aggregate { kind: kind.clone(), fields: fields.iter().map(transform_operand).collect() })
         }
         HirValue::Project { base, projection } => {
              let transformed_projection = match projection {
                  ProjectionKind::ArrayIndex(op) => ProjectionKind::ArrayIndex(transform_operand(op)),
                  _ => projection.clone(),
              };
              TransformedValue::Value(HirValue::Project { base: transform_operand(base), projection: transformed_projection })
         }
         HirValue::Closure { function_symbol, captures } => {
              TransformedValue::Value(HirValue::Closure { function_symbol: *function_symbol, captures: captures.iter().map(transform_operand).collect() })
         }
    }
}

fn transform_tail_expr(
    tail_expr: &HirTailExpr,
    module: &HirModule,
    candidates: &HashSet<Symbol>,
    next_hir_var_id: &AtomicU32,
) -> HirTailExpr {
     match tail_expr {
         HirTailExpr::Return(op) => HirTailExpr::Return(transform_operand(op)),
         HirTailExpr::If { condition, then_branch, else_branch } => {
             HirTailExpr::If {
                 condition: transform_operand(condition),
                 then_branch: Box::new(transform_expr(then_branch, module, candidates, next_hir_var_id)),
                 else_branch: Box::new(transform_expr(else_branch, module, candidates, next_hir_var_id)),
             }
         }
         HirTailExpr::Match { scrutinee, arms, otherwise } => {
              let transformed_arms = arms.iter().map(|(pattern, expr)| {
                  (transform_pattern(pattern), transform_expr(expr, module, candidates, next_hir_var_id))
              }).collect();
              HirTailExpr::Match {
                  scrutinee: transform_operand(scrutinee),
                  arms: transformed_arms,
                  otherwise: otherwise.as_ref().map(|expr| Box::new(transform_expr(expr, module, candidates, next_hir_var_id))),
              }
         }
         HirTailExpr::Never => HirTailExpr::Never,
     }
}

fn transform_operand(operand: &Operand) -> Operand {
    operand.clone()
}

fn transform_pattern(pattern: &HirPattern) -> HirPattern {
    pattern.clone()
}

fn inline_call_site(
    callee_symbol: Symbol,
    call_args: Vec<Operand>,
    module: &HirModule,
    next_hir_var_id: &AtomicU32,
) -> HirExpr {
    let callee_func = module.functions.iter()
        .find(|f| f.symbol == callee_symbol)
        .expect("Inlining candidate function not found");

    let mut param_subst = HashMap::new();
    if callee_func.signature.params.len() != call_args.len() {
         panic!("Argument count mismatch during inlining for {}", callee_func.name);
    }
    for ((param_var, _), arg_operand) in callee_func.signature.params.iter().zip(call_args.iter()) {
        param_subst.insert(*param_var, arg_operand.clone());
    }

    let mut var_map = HashMap::new();
    let mut ctx = InliningContext {
        param_subst: &param_subst,
        var_map: &mut var_map,
        next_hir_var_id,
    };

    let original_body = callee_func.body.as_ref().expect("Inlining candidate has no body");
    substitute_and_rename_expr(original_body, &mut ctx)
}

fn stitch_inlined_expr(
    original_let_var: HirVar,
    inlined_body: HirExpr,
    original_rest: HirExpr,
) -> HirExpr {
    let mut bindings = Vec::new();
    let mut current_expr = inlined_body;
    let mut last_span = current_expr.span;

    let final_tail_expr = loop {
        last_span = current_expr.span;
        match current_expr.kind {
            HirExprKind::Let { var, var_ty, value, rest } => {
                 bindings.push((var, *value, var_ty));
                 current_expr = *rest;
            }
            HirExprKind::Tail(tail_expr) => {
                 break tail_expr;
            }
        }
    };

    let (final_operand, final_operand_ty) = match final_tail_expr {
        HirTailExpr::Return(op) => {
            (op, current_expr.ty)
        }
        HirTailExpr::Never => {
            println!("WARN: Inlined function ends in Never, subsequent code is dead.");
             return build_nested_lets(bindings, HirExpr {
                 kind: HirExprKind::Tail(HirTailExpr::Never),
                 ty: HirType::Never,
                 span: last_span,
             });
        }
        _ => panic!("Unexpected tail expression kind after inlining: {:?}", final_tail_expr),
    };

    let innermost_ty = original_rest.ty.clone();
    let innermost_span = last_span;

    let innermost_expr = HirExpr {
        kind: HirExprKind::Let {
            var: original_let_var,
            var_ty: final_operand_ty,
            value: Box::new(HirValue::Use(final_operand)),
            rest: Box::new(original_rest),
        },
        ty: innermost_ty,
        span: innermost_span,
    };

    build_nested_lets(bindings, innermost_expr)
}

/// Helper function to construct nested Let expressions
fn build_nested_lets(bindings: Vec<(HirVar, HirValue, HirType)>, final_expr: HirExpr) -> HirExpr {
    let mut current_expr = final_expr;
    for (var, value, var_ty) in bindings.into_iter().rev() {
        let outer_span = current_expr.span;
        let outer_ty = current_expr.ty.clone();
        let current_var_ty = substitute_and_rename_type(&var_ty, &mut InliningContext { param_subst: &HashMap::new(), var_map: &mut HashMap::new(), next_hir_var_id: &AtomicU32::new(0) });
        current_expr = HirExpr {
            kind: HirExprKind::Let {
                var,
                var_ty: current_var_ty,
                value: Box::new(value),
                rest: Box::new(current_expr),
            },
            ty: outer_ty,
            span: outer_span,
        };
    }
    current_expr
}

fn transform_function_body(
    func: &HirFunction,
    module: &HirModule,
    candidates: &HashSet<Symbol>,
    next_hir_var_id: &AtomicU32,
) -> Option<HirExpr> {
    func.body.as_ref().map(|body| {
        transform_expr(body, module, candidates, next_hir_var_id)
    })
}

/// Performs function inlining on the HIR module.
/// Takes mutable access to update the next_var_id counter.
pub fn perform_inlining(module: &mut HirModule) {
    println!("Performing HIR Inlining Pass...");

    // Build call graph using immutable reference
    let call_graph = CallGraph::build(module);

    // Identify candidates first
    let mut candidates = HashSet::new();
    for func in &module.functions {
        let is_recursive = call_graph.is_recursive(func.symbol);
        let size = func.body.as_ref().map_or(0, calculate_hir_size);
        let is_small_enough = size > 0 && size < INLINING_SIZE_THRESHOLD;
        if !is_recursive && is_small_enough {
             println!("  -> Candidate for inlining: {} (size: {}, recursive: {})", func.name, size, is_recursive);
             candidates.insert(func.symbol);
        } else {
             println!("  -> Not inlining: {} (size: {}, recursive: {})", func.name, size, is_recursive);
        }
    }

    if candidates.is_empty() {
        println!("No candidates found for inlining.");
        return; // Exit early, no changes needed
    }

    // Find the actual maximum existing HirVar ID
    let max_id = find_max_hir_var_id(module);
    println!("Max existing HirVar ID found: {}", max_id);
    let next_hir_var_id = AtomicU32::new(max_id + 1);

    // Create a temporary owned copy of the original module data needed for transformation
    // This avoids complex borrowing issues with the module reference inside the map closure.
    let original_functions = module.functions.clone();
    let original_module_view = HirModule {
        name: module.name.clone(),
        functions: original_functions, // Use the clone
        structs: module.structs.clone(),
        enums: module.enums.clone(),
        statics: module.statics.clone(), // Add missing field
        entry_point: module.entry_point,
        next_var_id: module.next_var_id, // Copy initial ID
    };


    // Transform functions using the view
    let transformed_functions: Vec<HirFunction> = original_module_view.functions.iter()
        .map(|func| {
            // Pass immutable reference to original_module_view
            let new_body = transform_function_body(func, &original_module_view, &candidates, &next_hir_var_id);
            HirFunction {
                symbol: func.symbol,
                name: func.name.clone(),
                signature: func.signature.clone(),
                body: new_body,
                span: func.span,
            }
        })
        .collect();

    // Update the original module with the transformed functions
    module.functions = transformed_functions;

    // Update the next_var_id in the original module
    module.next_var_id = next_hir_var_id.load(Ordering::SeqCst);

    println!("Inlining pass complete. Next var ID: {}", module.next_var_id);
    // No return value needed as we modify the module in place
} 