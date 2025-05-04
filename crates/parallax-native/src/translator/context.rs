use crate::NativeError;
use cranelift_codegen::ir::{Value, Block, Type, InstBuilder};
use cranelift_frontend::FunctionBuilder;
use parallax_layout::{LayoutError, helpers, DescriptorIndex, LayoutDescriptor, LayoutComputer, DescriptorStore};
use parallax_hir::hir::{HirVar, HirType, HirStructDef, HirEnumDef, HirEnumVariant, Operand, HirLiteral, PrimitiveType};
use parallax_hir::Symbol;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::cell::Cell;

/// The `TranslationContext` maintains the state during the translation of a function body.
/// 
/// It's responsible for tracking:
/// - The mapping from HIR variables to Cranelift IR values
/// - Basic blocks for control flow
/// - Functions we've already seen/declared
/// - Layout information via immutable references to pre-computed stores/maps.
pub struct TranslationContext<'ctx> {
    /// Maps HirVars to their current Cranelift IR values
    var_values: HashMap<HirVar, Value>,
    
    /// Maps HirVars to their types
    var_types: HashMap<HirVar, HirType>,
    
    /// Maps HIR control flow constructs (like If/Match branches) to their Cranelift blocks
    blocks: HashMap<String, Block>,
    
    /// Maps function symbols to their declaration info
    function_info: HashMap<Symbol, KnownFunction>,
    
    /// Maps global data symbols (like string literals) to their JIT name and type.
    /// Note: This is primarily for `.L.str` style data.
    global_data_info: HashMap<Symbol, (String, HirType)>,

    /// Maps global static variable symbols to their info (name, type, mutability).
    global_statics: HashMap<Symbol, (String, HirType, bool)>,
    
    /// Reference to the module's struct definitions.
    struct_defs: &'ctx HashMap<Symbol, HirStructDef>,
    
    /// Reference to the module's enum definitions.
    enum_defs: &'ctx HashMap<Symbol, HirEnumDef>,
    
    /// Immutable reference to the pre-computed descriptor store.
    descriptor_store: &'ctx DescriptorStore,

    /// Immutable reference to the pre-computed ADT symbol -> index map.
    adt_index_map: &'ctx HashMap<Symbol, DescriptorIndex>,
    
    /// Immutable reference to the pre-computed primitive type -> index map.
    primitive_index_map: &'ctx HashMap<PrimitiveType, DescriptorIndex>,
    
    /// Immutable reference to the pre-computed tuple type -> index map.
    tuple_index_map: &'ctx HashMap<Vec<HirType>, DescriptorIndex>,
    
    /// Map of (ElementType, Size) -> DescriptorIndex for fixed-size arrays
    array_index_map: &'ctx HashMap<(HirType, usize), DescriptorIndex>,
    
    /// Index of the pre-generated static descriptor for ClosureRef.
    handle_descriptor_index: Option<DescriptorIndex>,
    
    /// Counter for generating unique names for blocks, data, etc.
    name_counter: Cell<usize>,
    
    /// Counter for generating unique names for data symbols (like string literals)
    data_symbol_counter: Cell<usize>,

    /// Number of GC handles pushed onto the shadow stack in the current function.
    shadow_stack_push_count: Cell<usize>,

    /// Set of symbols corresponding to intrinsic functions.
    intrinsic_symbols: &'ctx HashSet<Symbol>,
}

/// Information about a function that we've seen during translation
#[derive(Clone)] // Clone needed for get_function_like_info
pub struct KnownFunction {
    /// The name of the function
    pub name: String,
    /// Its HIR return type, useful for call site translation
    pub return_type: HirType,
    /// Parameter types, useful for call site translation
    pub param_types: Vec<HirType>,
}

/// Enum representing different kinds of global symbols
#[derive(Debug, Clone, PartialEq)]
pub enum GlobalInfo {
    Function(Symbol, HirType), // Symbol and function type
    Struct(Symbol, HirType), // Symbol and struct type
    Enum(Symbol, HirType), // Symbol and enum type
    Data(String, HirType), // Data symbol name and type (e.g., string literals)
    Static(Symbol, HirType, bool), // Static variable: Symbol, Type, is_mutable
}

impl<'ctx> TranslationContext<'ctx> {
    /// Create a new translation context.
    /// Takes immutable references to pre-computed layout info.
    pub fn new(
        struct_defs: &'ctx HashMap<Symbol, HirStructDef>,
        enum_defs: &'ctx HashMap<Symbol, HirEnumDef>,
        descriptor_store: &'ctx DescriptorStore,
        adt_index_map: &'ctx HashMap<Symbol, DescriptorIndex>,
        primitive_index_map: &'ctx HashMap<PrimitiveType, DescriptorIndex>,
        tuple_index_map: &'ctx HashMap<Vec<HirType>, DescriptorIndex>,
        array_index_map: &'ctx HashMap<(HirType, usize), DescriptorIndex>,
        handle_descriptor_index: Option<DescriptorIndex>,
        intrinsic_symbols: &'ctx HashSet<Symbol>,
    ) -> Self {
        println!(
            "[TranslationContext::new] Created context. Intrinsic symbols received: {:?}",
            intrinsic_symbols
        );
        Self {
            var_values: HashMap::new(),
            var_types: HashMap::new(),
            blocks: HashMap::new(),
            function_info: HashMap::new(),
            global_data_info: HashMap::new(),
            global_statics: HashMap::new(),
            struct_defs,
            enum_defs,
            descriptor_store,
            adt_index_map,
            primitive_index_map,
            tuple_index_map,
            array_index_map,
            handle_descriptor_index,
            name_counter: Cell::new(0),
            data_symbol_counter: Cell::new(0),
            shadow_stack_push_count: Cell::new(0),
            intrinsic_symbols,
        }
    }
    
    /// Register a variable with its Cranelift value AND its HirType.
    pub fn add_var_binding(&mut self, var: HirVar, value: Value, ty: HirType) {
        self.var_values.insert(var, value);
        self.var_types.insert(var, ty);
    }
    
    /// Get the current Cranelift value for a HIR variable
    pub fn get_var(&self, var: HirVar) -> Option<Value> {
        self.var_values.get(&var).copied()
    }
    
    /// Get the type of a HIR variable
    pub fn get_var_type(&self, var: HirVar) -> Option<HirType> {
        self.var_types.get(&var).cloned()
    }
    
    /// Add only the type for a variable (used for zero-sized types).
    pub fn add_var_type(&mut self, var: HirVar, ty: HirType) {
        self.var_types.insert(var, ty);
    }
    
    /// Add only the value for a variable (used when restoring scope).
    pub fn add_var_value(&mut self, var: HirVar, value: Value) {
        self.var_values.insert(var, value);
    }
    
    /// Remove the binding (value and type) for a variable.
    pub fn remove_var_binding(&mut self, var: HirVar) {
        self.var_values.remove(&var);
        self.var_types.remove(&var);
    }
    
    /// Remove only the value binding for a variable.
    pub fn remove_var_value(&mut self, var: HirVar) {
        self.var_values.remove(&var);
    }
    
    /// Remove only the type binding for a variable.
    pub fn remove_var_type(&mut self, var: HirVar) {
        self.var_types.remove(&var);
    }

    /// Get the global static info map.
    pub fn get_global_statics(&self) -> &HashMap<Symbol, (String, HirType, bool)> {
        &self.global_statics
    }

    /// Register info about a global static variable.
    pub fn add_global_static_info(&mut self, symbol: Symbol, name: String, ty: HirType, is_mutable: bool) {
        self.global_statics.insert(symbol, (name, ty, is_mutable));
    }

    /// Get information about a global symbol (function, struct, enum, data, static)
    pub fn get_global_info(&self, symbol: Symbol) -> Option<GlobalInfo> {
        if let Some(known_func) = self.function_info.get(&symbol) {
            return Some(GlobalInfo::Function(symbol, known_func.return_type.clone()));
        }
        if let Some((data_symbol_name, data_type)) = self.global_data_info.get(&symbol) {
            return Some(GlobalInfo::Data(data_symbol_name.clone(), data_type.clone()));
        }
        if self.get_struct_def(symbol).is_some() {
            let struct_type = HirType::Adt(symbol);
            return Some(GlobalInfo::Struct(symbol, struct_type));
        }
        if self.get_enum_def(symbol).is_some() {
            let enum_type = HirType::Adt(symbol);
            return Some(GlobalInfo::Enum(symbol, enum_type));
        }
        if let Some((_name, ty, is_mutable)) = self.global_statics.get(&symbol) {
            return Some(GlobalInfo::Static(symbol, ty.clone(), *is_mutable));
        }
        None
    }

    /// Register info about a known function.
    pub fn add_function_info(&mut self, symbol: Symbol, info: KnownFunction) {
        self.function_info.insert(symbol, info);
    }
    
    /// Get function-specific info (params, return type) from global_info if it represents a function.
    /// This is a placeholder/adapter until global_info is better typed.
    pub fn get_function_like_info(&self, symbol: Symbol) -> Option<KnownFunction> {
        self.function_info.get(&symbol).cloned()
    }

    /// Get a reference to a struct definition by its symbol.
    pub fn get_struct_def(&self, symbol: Symbol) -> Option<&'ctx HirStructDef> {
        self.struct_defs.get(&symbol)
    }

    /// Get a reference to an enum definition by its symbol.
    pub fn get_enum_def(&self, symbol: Symbol) -> Option<&'ctx HirEnumDef> {
        self.enum_defs.get(&symbol)
    }

    /// Get references to struct/enum definition maps (needed for descriptor generation).
    pub fn struct_defs_map(&self) -> &'ctx HashMap<Symbol, HirStructDef> {
         self.struct_defs
     }
    pub fn enum_defs_map(&self) -> &'ctx HashMap<Symbol, HirEnumDef> {
         self.enum_defs
     }

    /// Get enum and variant definitions from a variant symbol.
    pub fn get_enum_and_variant_def(&self, variant_symbol: Symbol) -> Option<(&'ctx HirEnumDef, &'ctx HirEnumVariant)> {
        for enum_def in self.enum_defs.values() { // Iterate over values
            if let Some(variant_def) = enum_def.variants.iter().find(|v| v.symbol == variant_symbol) {
                return Some((enum_def, variant_def));
            }
        }
        None
    }

    /// Generate the next unique name ID.
    pub fn next_name_id(&self) -> usize {
        let id = self.name_counter.get();
        self.name_counter.set(id + 1);
        id
    }

    /// Generate the next unique name ID specifically for data symbols.
    pub fn next_data_symbol_id(&self) -> usize {
        let id = self.data_symbol_counter.get();
        self.data_symbol_counter.set(id + 1);
        id
    }

    /// Register a basic block with a given name
    pub fn add_block(&mut self, name: String, block: Block) {
        self.blocks.insert(name, block);
    }
    
    /// Get a basic block by name
    pub fn get_block(&self, name: &str) -> Option<Block> {
        self.blocks.get(name).copied()
    }

    /// Get the HIR type of an operand, if known.
    pub fn get_operand_type(&self, operand: &Operand) -> Option<HirType> {
        match operand {
            Operand::Var(v) => self.get_var_type(*v),
            Operand::Const(lit) => {
                // Determine literal type directly here
                let prim_ty = match lit {
                    HirLiteral::IntLiteral { ty, .. } => *ty,
                    HirLiteral::FloatLiteral { ty, .. } => *ty,
                    HirLiteral::StringLiteral(_) => PrimitiveType::String,
                    HirLiteral::BoolLiteral(_) => PrimitiveType::Bool,
                    HirLiteral::CharLiteral(_) => PrimitiveType::Char,
                    HirLiteral::Unit => PrimitiveType::Unit,
                 };
                 Some(HirType::Primitive(prim_ty))
            },
            Operand::Global(s) => self.get_global_info(*s).map(|info| info.get_type()),
        }
    }

    /// Get the current shadow stack push count.
    pub fn shadow_stack_push_count(&self) -> usize {
        self.shadow_stack_push_count.get()
    }

    /// Increment the shadow stack push count.
    pub fn increment_shadow_stack_push_count(&self) {
        self.shadow_stack_push_count.set(self.shadow_stack_push_count.get() + 1);
    }

    /// Reset the shadow stack push count (e.g., after popping).
    pub fn reset_shadow_stack_push_count(&self) {
        self.shadow_stack_push_count.set(0);
    }

    /// Looks up the pre-computed descriptor index for a given HIR type.
    /// Renamed from get_or_create_descriptor_index.
    pub fn get_descriptor_index(
        &self,
        hir_type: &HirType,
    ) -> Result<DescriptorIndex, NativeError> {
        match hir_type {
            HirType::Primitive(p) => self.primitive_index_map.get(p)
                .copied()
                .ok_or_else(|| NativeError::LayoutError(LayoutError::Other(format!("Missing primitive descriptor index for {:?}", p)))),
            HirType::Adt(s) => self.adt_index_map.get(s)
                .copied()
                .ok_or_else(|| NativeError::LayoutError(LayoutError::UnknownAdt(*s))),
            HirType::Tuple(elems) => self.tuple_index_map.get(elems)
                .copied()
                .ok_or_else(|| NativeError::LayoutError(LayoutError::Other(format!("Missing tuple descriptor index for {:?}", elems)))),
            // Handle fixed-size and dynamic arrays
            HirType::Array(elem_ty, size_opt) => match size_opt {
                Some(size) => { // Fixed-size: lookup in array_index_map
                    let key = (elem_ty.as_ref().clone(), *size);
                     self.array_index_map.get(&key)
                        .copied()
                         .ok_or_else(|| NativeError::LayoutError(LayoutError::Other(format!("Missing array descriptor index for {:?}", key))))
                }
                None => { // Dynamic: use handle index
                    self.handle_descriptor_index
                        .ok_or_else(|| NativeError::LayoutError(LayoutError::Other("Handle descriptor index not set for dynamic Array type".to_string())))
                }
            }
            HirType::FunctionPointer(..) => self.handle_descriptor_index
                .ok_or_else(|| NativeError::LayoutError(LayoutError::Other("Handle descriptor index not set for FunctionPointer".to_string()))),
            HirType::Never => self.primitive_index_map.get(&PrimitiveType::Unit) // Assume Never uses Unit's index
                .copied()
                .ok_or_else(|| NativeError::LayoutError(LayoutError::Other("Unit descriptor index not found for Never type".to_string()))),
        }
    }

    /// Provides immutable access to the underlying TypeDescriptor storage.
    pub fn type_descriptors_slice(&self) -> &[LayoutDescriptor] {
        &self.descriptor_store.descriptors
    }

    /// Gets an immutable reference to a specific LayoutDescriptor by its index.
    pub fn get_descriptor_by_index(&self, index: DescriptorIndex) -> Option<&LayoutDescriptor> {
        self.descriptor_store.descriptors.get(index)
    }

    /// Gets the Cranelift type for an enum discriminant based on its descriptor index.
    pub fn get_discriminant_cl_type(&self, enum_descriptor_index: DescriptorIndex) -> Result<Type, NativeError> {
        let descriptor = self.get_descriptor_by_index(enum_descriptor_index)
            .ok_or_else(|| NativeError::LayoutError(LayoutError::Other("Enum descriptor not found".to_string())))?;
        helpers::get_discriminant_cl_type(descriptor).map_err(|e| NativeError::LayoutError(e))
    }

    /// Gets discriminant layout info (offset, size) for an enum.
    pub fn get_enum_discriminant_info(&self, enum_descriptor_index: DescriptorIndex) -> Result<(usize, usize), NativeError> {
        let descriptor = self.get_descriptor_by_index(enum_descriptor_index)
            .ok_or_else(|| NativeError::LayoutError(LayoutError::Other("Enum descriptor not found".to_string())))?;
        helpers::get_enum_discriminant_info(descriptor).map_err(|e| NativeError::LayoutError(e))
    }

    /// Gets layout info for a specific enum variant (payload offset, payload descriptor index).
    pub fn get_enum_variant_info(&self, enum_descriptor_index: DescriptorIndex, variant_index: usize) -> Result<(usize, DescriptorIndex), NativeError> {
        let descriptor = self.get_descriptor_by_index(enum_descriptor_index)
            .ok_or_else(|| NativeError::LayoutError(LayoutError::Other("Enum descriptor not found".to_string())))?;
        // Use the helper directly, it handles the logic
        helpers::get_enum_variant_info(descriptor, variant_index as u64).map_err(|e| NativeError::LayoutError(e))
    }

    /// Gets the byte offset and descriptor index for a field within an aggregate type
    /// (Struct, Tuple, or Array).
    /// For arrays, field_index must be 0, and it returns the element's info.
    /// Works for Structs and Tuples (which use LayoutDescriptor::Struct).
    pub fn get_aggregate_field_info(&self, aggregate_descriptor_index: DescriptorIndex, field_index: usize) -> Result<(usize, DescriptorIndex), NativeError> {
        let descriptor = self.get_descriptor_by_index(aggregate_descriptor_index)
            .ok_or_else(|| NativeError::LayoutError(LayoutError::Other(format!("Descriptor index {} not found for aggregate field lookup", aggregate_descriptor_index))))?;
        match descriptor {
            LayoutDescriptor::Struct { .. } => {
                let offset = helpers::get_struct_field_offset_bytes(descriptor, field_index)?;
                let desc_idx = helpers::get_struct_field_descriptor_index(descriptor, field_index)?;
                Ok((offset, desc_idx))
            }
            LayoutDescriptor::Array { .. } => {
                 if field_index >= 1 {
                     return Err(NativeError::LayoutError(LayoutError::Other(format!(
                         "Field index {} out of bounds for array descriptor index {}",
                         field_index, aggregate_descriptor_index
                     ))));
                 }
                 let (element_desc_idx, _) = helpers::get_array_info(descriptor)?;
                 Ok((0, element_desc_idx))
            }
            _ => Err(NativeError::LayoutError(LayoutError::Other(format!(
                "Expected Struct, Tuple, or Array descriptor at index {}, found {:?}",
                aggregate_descriptor_index, descriptor
            )))),
        }
    }

    /// Check if a given symbol corresponds to a known intrinsic function.
    pub fn is_intrinsic(&self, symbol: Symbol) -> bool {
        self.intrinsic_symbols.contains(&symbol)
    }

    /// Debug helper: Get IDs of all intrinsic symbols for logging purposes
    pub fn intrinsic_symbols_debug_ids(&self) -> Vec<u32> {
        self.intrinsic_symbols.iter().map(|sym| sym.id()).collect()
    }
    
    /// Special debug function to check intrinsics by ID matching rather than by Symbol equality
    pub fn is_intrinsic_by_id(&self, symbol: Symbol) -> bool {
        let id = symbol.id();
        self.intrinsic_symbols.iter().any(|sym| sym.id() == id)
    }
}

/// Helper function to create a local variable in a Cranelift function
pub fn declare_variable(
    builder: &mut FunctionBuilder,
    ctx: &mut TranslationContext,
    var: HirVar,
    hir_type: HirType,
    cl_type: Type,
) -> Result<Value, NativeError> {
    // Declare the variable in Cranelift's Variable infrastructure (if needed for stack slots later)
    // builder.declare_var(Variable::new(var.0 as usize), cl_type);

    // Create a *placeholder* Cranelift value. Actual assignment happens in translate_value.
    // This simplifies let-binding translation. Initializing might not be ideal.
    // Consider creating stack slots later if values need addresses.
    let value = builder.ins().iconst(cl_type, 0); // Default initialize to 0 for now

    // Register in our translation context
    ctx.add_var_binding(var, value, hir_type);

    Ok(value)
}

/// Generate a unique name for a block based on context
pub fn gen_block_name(prefix: &str, unique_id: usize) -> String {
    format!("{}_{}", prefix, unique_id)
}

impl GlobalInfo {
    pub fn get_type(&self) -> HirType {
        match self {
            GlobalInfo::Function(_, ty) => ty.clone(),
            GlobalInfo::Struct(_, ty) => ty.clone(),
            GlobalInfo::Enum(_, ty) => ty.clone(),
            GlobalInfo::Data(_, ty) => ty.clone(),
            GlobalInfo::Static(_, ty, _) => ty.clone(),
        }
    }
}