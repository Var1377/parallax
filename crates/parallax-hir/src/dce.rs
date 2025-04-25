// parallax-hir/src/dce.rs
// Dead Code Elimination pass for ANF HIR.

use crate::hir::*;
use parallax_resolve::types::Symbol;
use std::collections::{HashSet, VecDeque};

// Internal enum to represent item kinds for DCE processing
enum ItemKind {
    Function,
    Struct,
    Enum,
}

struct DceContext {
    reachable_symbols: HashSet<Symbol>, // Functions, Structs, Enums
    worklist: VecDeque<Symbol>, // Symbols to process
    module: HirModule, // Keep a reference to the original module
}

impl DceContext {
    fn new(module: HirModule) -> Self {
        DceContext {
            reachable_symbols: HashSet::new(),
            worklist: VecDeque::new(),
            module,
        }
    }

    fn add_reachable(&mut self, symbol: Symbol) {
        if self.reachable_symbols.insert(symbol) {
            self.worklist.push_back(symbol);
        }
    }

    fn find_function(&self, symbol: Symbol) -> Option<&HirFunction> {
        self.module.functions.iter().find(|f| f.symbol == symbol)
    }

    fn find_struct(&self, symbol: Symbol) -> Option<&HirStructDef> {
        self.module.structs.iter().find(|s| s.symbol == symbol)
    }

    fn find_enum(&self, symbol: Symbol) -> Option<&HirEnumDef> {
        self.module.enums.iter().find(|e| e.symbol == symbol)
    }

    fn run(&mut self) {
        // Use the entry point symbol directly from the module
        if let Some(main_symbol) = self.module.entry_point {
            self.add_reachable(main_symbol);
        } else {
            println!("WARN: No entry point symbol found in HIR module, DCE might remove everything.");
            // Optionally, keep all public functions as roots? For now, proceed.
        }

        while let Some(symbol) = self.worklist.pop_front() {
            // Process the symbol based on its kind
            // Check existence immutably first, release borrow immediately
            let kind_opt = if self.find_function(symbol).is_some() { Some(ItemKind::Function) } // 0 for Function
                      else if self.find_struct(symbol).is_some() { Some(ItemKind::Struct) } // 1 for Struct
                      else if self.find_enum(symbol).is_some() { Some(ItemKind::Enum) } // 2 for Enum
                      else { None };

            // Now perform the mutable borrow and visit based on the kind
            match kind_opt {
                Some(ItemKind::Function) => self.visit_function_by_symbol(symbol),
                Some(ItemKind::Struct) => self.visit_struct_by_symbol(symbol),
                Some(ItemKind::Enum) => self.visit_enum_by_symbol(symbol),
                None => {
                    // If it's none of these, it might be an enum variant symbol added
                    // during type analysis, which doesn't need further direct processing here
                    // as its type dependencies (the enum itself) would have been handled.
                }
            }
        }
    }

    // Renamed original visit_function, now takes Symbol
    fn visit_function_by_symbol(&mut self, func_symbol: Symbol) {
        // Fetch the function immutably within this scope
        // .clone() is needed because the visit methods below might borrow self mutably again.
        // We clone the necessary parts (signature, body) to avoid holding the borrow on self.module.
        let func_data_opt = self.find_function(func_symbol).map(|f| (f.signature.clone(), f.body.clone()));

        if let Some((signature, body)) = func_data_opt {
            // Visit parameter types and return type
            for (_, param_ty) in &signature.params {
                self.visit_type(param_ty);
            }
            self.visit_type(&signature.return_type);

            // Visit body expression
            if let Some(body_expr) = &body {
                self.visit_expr(body_expr);
            }
        } else {
             // Should not happen if called correctly after check
             println!("WARN: Function symbol {:?} not found during visit.", func_symbol);
        }
    }

     // Renamed original visit_struct, now takes Symbol
     fn visit_struct_by_symbol(&mut self, struct_symbol: Symbol) {
         // Clone necessary data to avoid holding the borrow
         let fields_opt = self.find_struct(struct_symbol).map(|s| s.fields.clone());

         if let Some(fields) = fields_opt {
            // Visit field types
            for (_, _, field_ty) in &fields {
                self.visit_type(field_ty);
            }
         } else {
             println!("WARN: Struct symbol {:?} not found during visit.", struct_symbol);
         }
     }

     // Renamed original visit_enum, now takes Symbol
     fn visit_enum_by_symbol(&mut self, enum_symbol: Symbol) {
         // Clone necessary data to avoid holding the borrow
         let variants_opt = self.find_enum(enum_symbol).map(|e| e.variants.clone());

         if let Some(variants) = variants_opt {
             // Add variant symbols and visit their field types
             for variant in &variants {
                 // Variant symbols themselves are considered reachable if the enum is reachable
                 // because they are needed for construction/matching.
                 self.add_reachable(variant.symbol);
                 for field_ty in &variant.fields {
                     self.visit_type(field_ty);
                 }
             }
         } else {
             println!("WARN: Enum symbol {:?} not found during visit.", enum_symbol);
         }
     }

    fn visit_expr(&mut self, expr: &HirExpr) {
        self.visit_type(&expr.ty); // Visit the expression's type itself
        match &expr.kind {
            HirExprKind::Let { value, rest, var_ty, .. } => {
                self.visit_type(var_ty);
                self.visit_value(value);
                self.visit_expr(rest);
            }
            HirExprKind::Tail(tail_expr) => {
                self.visit_tail_expr(tail_expr);
            }
        }
    }

     fn visit_value(&mut self, value: &HirValue) {
         match value {
             HirValue::Use(op) => self.visit_operand(op),
             HirValue::Call { func, args } => {
                 self.visit_operand(func);
                 for arg in args {
                     self.visit_operand(arg);
                 }
             }
             HirValue::Aggregate { kind, fields } => {
                 match kind {
                     AggregateKind::Struct(s) | AggregateKind::EnumVariant(s) => {
                         self.add_reachable(*s); // Struct def or Enum Variant symbol
                     }
                     AggregateKind::Tuple | AggregateKind::Array => {}
                 }
                 for field in fields {
                     self.visit_operand(field);
                 }
             }
             HirValue::Project { base, projection } => {
                 self.visit_operand(base);
                 match projection {
                      ProjectionKind::Field(s) => { /* Field symbol isn't a top-level definition */ },
                      ProjectionKind::TupleIndex(_) => {},
                      ProjectionKind::ArrayIndex(op) => self.visit_operand(op),
                      ProjectionKind::Downcast(s) => self.add_reachable(*s), // Enum Variant symbol
                 }
             }
             HirValue::Closure { function_symbol, captures } => {
                 self.add_reachable(*function_symbol);
                 for capture in captures {
                     self.visit_operand(capture);
                 }
             }
         }
     }

     fn visit_tail_expr(&mut self, tail_expr: &HirTailExpr) {
         match tail_expr {
             HirTailExpr::Value(op) => self.visit_operand(op),
             HirTailExpr::If { condition, then_branch, else_branch } => {
                 self.visit_operand(condition);
                 self.visit_expr(then_branch);
                 self.visit_expr(else_branch);
             }
             HirTailExpr::Match { scrutinee, arms, otherwise } => {
                 self.visit_operand(scrutinee);
                 for (pattern, expr) in arms {
                     self.visit_pattern(pattern);
                     self.visit_expr(expr);
                 }
                 if let Some(otherwise_expr) = otherwise {
                     self.visit_expr(otherwise_expr);
                 }
             }
             HirTailExpr::Never => {}
         }
     }

     fn visit_operand(&mut self, operand: &Operand) {
         match operand {
             Operand::Var(_) => {} // Local variable
             Operand::Const(lit) => self.visit_literal(lit),
             Operand::Global(s) => self.add_reachable(*s), // Global function/constant symbol
         }
     }

     fn visit_literal(&mut self, _lit: &HirLiteral) {
         // Literals don't introduce dependencies on top-level items
     }

     fn visit_pattern(&mut self, pattern: &HirPattern) {
         match pattern {
             HirPattern::Variant { variant_symbol, bindings } => {
                 self.add_reachable(*variant_symbol);
                 for (_, ty) in bindings {
                     self.visit_type(ty);
                 }
             }
             HirPattern::Const(lit) => self.visit_literal(lit),
             HirPattern::Bind { var_ty, .. } => self.visit_type(var_ty),
             HirPattern::Wildcard => {}
         }
     }

    fn visit_type(&mut self, ty: &HirType) {
        match ty {
            HirType::Primitive(_) => {}
            HirType::Adt(s) => self.add_reachable(*s), // Struct or Enum symbol
            HirType::Tuple(tys) => {
                for t in tys {
                    self.visit_type(t);
                }
            }
            HirType::Array(elem_ty, _) => self.visit_type(elem_ty),
            HirType::FunctionPointer(params, ret) => {
                for p in params {
                    self.visit_type(p);
                }
                self.visit_type(ret);
            }
            HirType::Never => {}
        }
    }
}

/// Performs Dead Code Elimination on the HIR module.
///
/// It identifies reachable items starting from the `main` function
/// and removes any functions, structs, or enums that are not reachable.
/// It preserves the `next_var_id` from the input module.
pub fn perform_dce(module: HirModule) -> HirModule {
    let mut ctx = DceContext::new(module);
    ctx.run();

    // Filter the original module based on reachable symbols
    let reachable_symbols = ctx.reachable_symbols;
    let original_module = ctx.module; // ctx takes ownership

    let functions = original_module
        .functions
        .into_iter()
        .filter(|f| reachable_symbols.contains(&f.symbol))
        .collect();

    let structs = original_module
        .structs
        .into_iter()
        .filter(|s| reachable_symbols.contains(&s.symbol))
        .collect();

    // Keep enums if the enum symbol OR any of its variant symbols are reachable.
    // This is because variant symbols might be added from patterns/projections
    // even if the enum type itself isn't directly referenced.
    let enums = original_module
        .enums
        .into_iter()
        .filter(|e| {
             reachable_symbols.contains(&e.symbol) || e.variants.iter().any(|v| reachable_symbols.contains(&v.symbol))
        })
        .collect();

    HirModule {
        name: original_module.name,
        functions,
        structs,
        enums,
        statics: original_module.statics,
        entry_point: original_module.entry_point,
        next_var_id: original_module.next_var_id, // Propagate the ID counter
        intrinsics: original_module.intrinsics,
    }
} 