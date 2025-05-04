use crate::CodegenError;
use parallax_hir::hir::{self, HirExpr, HirExprKind, HirFunction, HirLiteral, HirModule, HirPattern, HirTailExpr, HirType, HirValue, Operand, PrimitiveType};
use parallax_hir::{Symbol, HirStructDef, HirEnumDef};
use parallax_mir::{lower_module as lower_hir_to_mir, LoweringError as MirLoweringError, MirModule};
use parallax_native::{compile_hir, CompiledArtifact, NativeError};
use parallax_net::{lower_module as lower_mir_to_net, CompiledNet, LoweringError as NetLoweringError};
// Imports for layout computation and GC initialization
use parallax_layout::{LayoutComputer, DescriptorStore, LayoutError};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

/// Represents the output of the code generation process.
#[derive(Debug)]
pub struct CompiledOutput {
    /// The native artifact produced (e.g., machine code, object file).
    pub native_artifact: CompiledArtifact,
    /// The interaction net artifact produced.
    pub inet_artifact: CompiledNet,
    /// The descriptor store containing layout information. Must be kept alive.
    pub descriptor_store: Box<DescriptorStore>,
}

impl CompiledOutput {
    /// Get a function pointer for a specified symbol
    pub fn get_function_ptr(&self, symbol: Symbol) -> Option<*const u8> {
        self.native_artifact.get_function_ptr(symbol)
    }
    
    /// Cast a function pointer to a callable Rust function.
    /// 
    /// # Safety
    ///
    /// This is unsafe because the caller must ensure:
    /// 1. The function signature matches what was compiled
    /// 2. The function is correctly implemented and won't cause undefined behavior
    /// 3. The JIT module is still alive (contained in self)
    pub unsafe fn get_function<F>(&self, symbol: Symbol) -> Option<F> 
    where
        F: Copy, // Function pointers are Copy
    {
        self.native_artifact.get_function::<F>(symbol)
    }
    
    /// Helper to call a nullary (no arguments) function that returns an i64
    /// 
    /// # Safety
    ///
    /// Caller must ensure the function was compiled with a matching signature
    pub unsafe fn call_nullary_i64(&self, symbol: Symbol) -> Option<i64> {
        self.native_artifact.call_nullary_i64(symbol)
    }
}

/// Visitor to collect all unique HIR types from a module.
struct HirTypeCollector {
    types: HashSet<HirType>,
    // We need ADT definitions to resolve types within variants/structs during traversal
    struct_defs: HashMap<Symbol, HirStructDef>,
    enum_defs: HashMap<Symbol, HirEnumDef>,
    // Track visited functions/statics to avoid infinite loops in case of recursion?
    // Might not be necessary if type collection itself doesn't recurse infinitely.
}

impl HirTypeCollector {
    fn new(struct_defs: HashMap<Symbol, HirStructDef>, enum_defs: HashMap<Symbol, HirEnumDef>) -> Self {
        HirTypeCollector {
            types: HashSet::new(),
            struct_defs,
            enum_defs,
        }
    }

    fn collect(&mut self, module: &HirModule) {
        for func in &module.functions {
            self.visit_function(func);
        }
        for static_item in &module.statics {
            self.visit_type(&static_item.ty);
            if let Some(init) = &static_item.initializer {
                self.visit_value(init);
            }
        }
        // Also explicitly add primitive types that might not appear otherwise?
        // E.g., Unit, Never - LayoutComputer handles these implicitly now.
    }

    fn visit_function(&mut self, func: &HirFunction) {
        for (_, param_ty) in &func.signature.params {
            self.visit_type(param_ty);
        }
        self.visit_type(&func.signature.return_type);
        if let Some(body) = &func.body {
            self.visit_expr(body);
        }
    }

    fn visit_expr(&mut self, expr: &HirExpr) {
        self.visit_type(&expr.ty); // Collect the expression's own type
        match &expr.kind {
            HirExprKind::Let { var_ty, value, rest, .. } => {
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
            HirValue::Use(operand) => self.visit_operand(operand),
            HirValue::Call { func, args } => {
                self.visit_operand(func);
                // Determine func type (might need context, difficult here)
                // If func is Operand::Global, look up known function type
                // If func is Operand::Var, type should be FunctionPointer
                // Assume FunctionPointer for now to collect subtypes
                if let Some(HirType::FunctionPointer(param_types, ret_type)) = self.get_operand_hir_type(func) {
                    param_types.iter().for_each(|t| self.visit_type(t));
                    self.visit_type(&ret_type);
                }
                for arg in args {
                    self.visit_operand(arg);
                }
            }
            HirValue::Aggregate { kind, fields } => {
                match kind {
                    hir::AggregateKind::Tuple => {
                        let elem_types: Vec<_> = fields.iter().map(|op| self.get_operand_hir_type(op).unwrap_or(HirType::Never)).collect();
                        self.visit_type(&HirType::Tuple(elem_types));
                    }
                    hir::AggregateKind::Struct(s_sym) => {
                        self.visit_type(&HirType::Adt(*s_sym));
                    }
                    hir::AggregateKind::Array => {
                        if !fields.is_empty() {
                             if let Some(elem_ty) = self.get_operand_hir_type(&fields[0]) {
                                 self.visit_type(&HirType::Array(Arc::new(elem_ty), Some(fields.len())));
                             }
                         } else {
                             self.visit_type(&HirType::Primitive(PrimitiveType::Unit)); // Represent empty array as unit
                         }
                    }
                    hir::AggregateKind::EnumVariant(v_sym) => {
                        if let Some((enum_def, _)) = self.get_enum_and_variant_def(*v_sym) {
                            self.visit_type(&HirType::Adt(enum_def.symbol));
                        }
                    }
                }
                for field_op in fields {
                    self.visit_operand(field_op);
                }
            }
            HirValue::Project { base, projection } => {
                self.visit_operand(base);
                if let hir::ProjectionKind::ArrayIndex(index_op) = projection {
                    self.visit_operand(index_op);
                }
                // The resulting type of projection is also collected when visiting the parent expression/value
            }
            HirValue::Closure { function_symbol: _, captures } => {
                // Collect types of captured operands
                let captured_types: Vec<_> = captures.iter()
                    .map(|op| self.get_operand_hir_type(op).unwrap_or(HirType::Never))
                    .collect();
                // Add the synthesized environment tuple type
                self.visit_type(&HirType::Tuple(captured_types));
                // Visit the captured operands themselves
                for cap in captures {
                    self.visit_operand(cap);
                }
                // Also need the function pointer type of the function_symbol
                 // This requires looking up the function, difficult without more context.
            }
        }
    }

    fn visit_tail_expr(&mut self, tail_expr: &HirTailExpr) {
        match tail_expr {
            HirTailExpr::Value(operand) => self.visit_operand(operand),
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
            HirTailExpr::Never => { /* No types */ }
        }
    }

    fn visit_pattern(&mut self, pattern: &HirPattern) {
        match pattern {
            HirPattern::Const(lit) => self.visit_literal(lit),
            HirPattern::Wildcard => { /* No types */ }
            HirPattern::Bind { var_ty, .. } => self.visit_type(var_ty),
            HirPattern::Variant { variant_symbol, bindings } => {
                // Collect the enum type itself
                 if let Some((enum_def, _)) = self.get_enum_and_variant_def(*variant_symbol) {
                     self.visit_type(&HirType::Adt(enum_def.symbol));
                 }
                // Collect types within bindings (just the types, not the vars)
                for (_, binding_ty) in bindings { // Iterate through (HirVar, HirType)
                    self.visit_type(binding_ty); // Visit the type
                }
            }
        }
    }

    fn visit_operand(&mut self, operand: &Operand) {
        match operand {
            Operand::Var(_) => { /* Type collected from binding site (let/param/pattern) */ }
            Operand::Const(lit) => self.visit_literal(lit),
            Operand::Global(_) => { /* Type collected via get_operand_hir_type or module traversal */ }
        }
    }

    fn visit_literal(&mut self, literal: &HirLiteral) {
        let prim_ty = match literal {
            HirLiteral::IntLiteral { ty, .. } => *ty,
            HirLiteral::FloatLiteral { ty, .. } => *ty,
            HirLiteral::StringLiteral(_) => PrimitiveType::String,
            HirLiteral::BoolLiteral(_) => PrimitiveType::Bool,
            HirLiteral::CharLiteral(_) => PrimitiveType::Char,
            HirLiteral::Unit => PrimitiveType::Unit,
        };
        self.visit_type(&HirType::Primitive(prim_ty));
    }

    fn visit_type(&mut self, hir_type: &HirType) {
        if self.types.insert(hir_type.clone()) {
            // If type was newly inserted, visit its subtypes recursively
            match hir_type {
                HirType::Adt(symbol) => {
                    // --- Fix for borrow checker --- 
                    let mut sub_types = Vec::new();
                    if let Some(struct_def) = self.struct_defs.get(symbol) {
                         for (_, _, field_ty) in &struct_def.fields {
                             sub_types.push(field_ty.clone());
                         }
                    } else if let Some(enum_def) = self.enum_defs.get(symbol) {
                         for variant in &enum_def.variants {
                             for field_ty in &variant.fields {
                                 sub_types.push(field_ty.clone());
                             }
                         }
                    }
                    // Release immutable borrow before mutable borrow
                    for sub_type in sub_types {
                        self.visit_type(&sub_type);
                    }
                    // --- End fix ---
                }
                HirType::Tuple(elements) => {
                    for element_type in elements {
                        self.visit_type(element_type);
                    }
                }
                HirType::Array(element_type, _) => {
                    self.visit_type(element_type);
                }
                HirType::FunctionPointer(param_types, return_type) => {
                    for param_type in param_types {
                        self.visit_type(param_type);
                    }
                    self.visit_type(return_type);
                }
                HirType::Primitive(_) => { /* No subtypes */ }
                HirType::Never => { /* No subtypes */ }
            }
        }
    }

    // --- Helper methods (simplified, may need more context) ---

    fn get_operand_hir_type(&self, operand: &Operand) -> Option<HirType> {
         match operand {
             Operand::Var(_) => None, // Cannot determine var type reliably here
             Operand::Const(lit) => Some(match lit {
                 HirLiteral::IntLiteral { ty, .. } => HirType::Primitive(*ty),
                 HirLiteral::FloatLiteral { ty, .. } => HirType::Primitive(*ty),
                 HirLiteral::StringLiteral(_) => HirType::Primitive(PrimitiveType::String),
                 HirLiteral::BoolLiteral(_) => HirType::Primitive(PrimitiveType::Bool),
                 HirLiteral::CharLiteral(_) => HirType::Primitive(PrimitiveType::Char),
                 HirLiteral::Unit => HirType::Primitive(PrimitiveType::Unit),
             }),
             Operand::Global(_) => None, // Cannot determine global type reliably here
         }
     }

     fn get_enum_and_variant_def(&self, variant_symbol: Symbol) -> Option<(&HirEnumDef, &hir::HirEnumVariant)> {
         for enum_def in self.enum_defs.values() { 
             if let Some(variant_def) = enum_def.variants.iter().find(|v| v.symbol == variant_symbol) {
                 return Some((enum_def, variant_def));
             }
         }
         None
     }

     fn get_struct_def(&self, symbol: Symbol) -> Option<&HirStructDef> {
         self.struct_defs.get(&symbol)
     }
}

/// Orchestrates the code generation process for a given HIR module.
///
/// This function lowers HIR to MIR, then MIR to Interaction Nets, and also
/// compiles HIR to native code.
pub fn generate_module(hir_module: &HirModule) -> Result<CompiledOutput, CodegenError> {
    // --- Layout Computation --- 
    println!("Initializing Layout Computer...");
    let mut layout_computer = LayoutComputer::new(
        hir_module.structs.iter().map(|s| (s.symbol, s.clone())).collect::<HashMap<_, _>>(),
        hir_module.enums.iter().map(|e| (e.symbol, e.clone())).collect::<HashMap<_, _>>()
    );

    println!("Collecting HIR types for layout computation...");
    let mut type_collector = HirTypeCollector::new(
        hir_module.structs.iter().map(|s| (s.symbol, s.clone())).collect::<HashMap<_, _>>(),
        hir_module.enums.iter().map(|e| (e.symbol, e.clone())).collect::<HashMap<_, _>>()
    );
    type_collector.collect(hir_module);
    let collected_types = type_collector.types;

    println!("Pre-computing layouts for {} unique types...", collected_types.len());
    for hir_type in &collected_types {
        // Compute layout for each collected type to populate the computer's caches and descriptor list
        match layout_computer.get_or_create_descriptor_index(hir_type) {
            Ok(_) => { /* Successfully computed/cached */ }
            Err(e) => {
                // Handle layout errors during pre-computation
                eprintln!("Error computing layout for type {:?}: {}", hir_type, e);
                return Err(CodegenError::Layout(e));
            }
        }
    }

    // Finalize returns Vec and maps
    let (descriptors_vec, adt_map, primitive_map, tuple_map, array_map) = layout_computer.finalize();
    // Use the len() of the Vec directly
    println!("Layout computation finalized. Store size: {}", descriptors_vec.len());

    // Create the DescriptorStore struct *after* getting the Vec and Box it
    let descriptor_store = Box::new(DescriptorStore { descriptors: descriptors_vec });

    // --- Lowering and Compilation --- 
    println!("Lowering HIR to MIR...");
    let mir_module = lower_hir_to_mir(
        hir_module,
        descriptor_store.clone(), // Pass the owned store (will be moved into MirModule)
        adt_map.clone(),         // Clone maps for MIR
        primitive_map.clone(),
        tuple_map.clone(),
        array_map.clone(),
    )
    .map_err(CodegenError::MirLowering)?; // Use error variant

    // println!("MIR: {:#?}", MirModule {
    //     intrinsics: Default::default(),
    //     ..mir_module.clone()
    // });
    
    println!("Lowering MIR to Interaction Net...");
    let inet_artifact = lower_mir_to_net(&mir_module)
        .map_err(CodegenError::NetLowering)?; // Use error variant

    println!("Starting native code generation...");
    let native_artifact = compile_hir(
        hir_module,
        &descriptor_store,
        &adt_map,
        &primitive_map,
        &tuple_map,
        &array_map,
        Some(0)
    ).map_err(CodegenError::NativeBackend)?;
    println!("Native code generation finished.");

    Ok(CompiledOutput {
        native_artifact,
        inet_artifact,
        descriptor_store, // Return the store to keep it alive
    })
} 