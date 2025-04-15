use crate::NativeError;
use cranelift_codegen::ir::{Value, Block, Type, InstBuilder};
use cranelift_frontend::FunctionBuilder;
use parallax_hir::hir::{HirVar, HirType, HirStructDef, HirEnumDef, HirEnumVariant, Operand, HirLiteral, ResolvePrimitiveType};
use parallax_hir::Symbol;
use std::collections::HashMap;

/// The `TranslationContext` maintains the state during the translation of a function body.
/// 
/// It's responsible for tracking:
/// - The mapping from HIR variables to Cranelift IR values
/// - Basic blocks for control flow
/// - Functions we've already seen/declared
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
    
    /// Maps struct symbols to their definitions
    struct_defs: &'ctx [HirStructDef],
    
    /// Maps enum symbols to their definitions
    enum_defs: &'ctx [HirEnumDef],
    
    /// Counter for generating unique names for blocks, data, etc.
    name_counter: usize,
    
    /// Counter for generating unique names for data symbols (like string literals)
    data_symbol_counter: usize,

    /// Number of GC handles pushed onto the shadow stack in the current function.
    shadow_stack_push_count: usize,
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
    /// Create a new empty translation context
    pub fn new(
        struct_defs: &'ctx [HirStructDef],
        enum_defs: &'ctx [HirEnumDef]
    ) -> Self {
        Self {
            var_values: HashMap::new(),
            var_types: HashMap::new(),
            blocks: HashMap::new(),
            function_info: HashMap::new(),
            global_data_info: HashMap::new(),
            global_statics: HashMap::new(),
            struct_defs,
            enum_defs,
            name_counter: 0,
            data_symbol_counter: 0,
            shadow_stack_push_count: 0,
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
        // Check function symbols
        if let Some(known_func) = self.function_info.get(&symbol) {
            return Some(GlobalInfo::Function(symbol, known_func.return_type.clone()));
        }

        // Check global data symbols
        if let Some((data_symbol_name, data_type)) = self.global_data_info.get(&symbol) {
            return Some(GlobalInfo::Data(data_symbol_name.clone(), data_type.clone()));
        }

        // Check struct definitions
        if self.get_struct_def(symbol).is_some() {
            // Structs are represented as Adt(Symbol) in HirType
            let struct_type = HirType::Adt(symbol);
            return Some(GlobalInfo::Struct(symbol, struct_type));
        }

        // Check enum definitions
        if self.get_enum_def(symbol).is_some() {
            // Enums are also represented as Adt(Symbol) in HirType
            let enum_type = HirType::Adt(symbol);
            return Some(GlobalInfo::Enum(symbol, enum_type));
        }

        // Check global static variables
        if let Some((name, ty, is_mutable)) = self.global_statics.get(&symbol) {
            return Some(GlobalInfo::Static(symbol, ty.clone(), *is_mutable));
        }

        None // Symbol not found
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
        self.struct_defs.iter().find(|s| s.symbol == symbol)
    }

    /// Get a reference to an enum definition by its symbol.
    pub fn get_enum_def(&self, symbol: Symbol) -> Option<&'ctx HirEnumDef> {
        self.enum_defs.iter().find(|e| e.symbol == symbol)
    }

    /// Placeholder: Get enum and variant definitions from a variant symbol.
    /// FIXME: This needs a proper implementation, likely requiring changes
    /// to HIR or the resolver/type checker to map variant symbols back to enums.
    pub fn get_enum_and_variant_def(&self, variant_symbol: Symbol) -> Option<(&'ctx HirEnumDef, &'ctx HirEnumVariant)> {
        for enum_def in self.enum_defs {
            if let Some(variant_def) = enum_def.variants.iter().find(|v| v.symbol == variant_symbol) {
                return Some((enum_def, variant_def));
            }
        }
        None
    }

    /// Generate the next unique name ID.
    pub fn next_name_id(&mut self) -> usize {
        let id = self.name_counter;
        self.name_counter += 1;
        id
    }

    /// Generate the next unique name ID specifically for data symbols.
    pub fn next_data_symbol_id(&mut self) -> usize {
        let id = self.data_symbol_counter;
        self.data_symbol_counter += 1;
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
                     HirLiteral::Int(_) => ResolvePrimitiveType::I64, // Assuming i64 default
                     HirLiteral::Float(_) => ResolvePrimitiveType::F64,
                     HirLiteral::String(_) => ResolvePrimitiveType::String,
                     HirLiteral::Bool(_) => ResolvePrimitiveType::Bool,
                     HirLiteral::Char(_) => ResolvePrimitiveType::Char,
                     HirLiteral::Unit => ResolvePrimitiveType::Unit,
                 };
                 Some(HirType::Primitive(prim_ty))
            },
            Operand::Global(s) => self.get_global_info(*s).map(|info| info.get_type()),
        }
    }

    /// Get the current shadow stack push count.
    pub fn shadow_stack_push_count(&self) -> usize {
        self.shadow_stack_push_count
    }

    /// Increment the shadow stack push count.
    pub fn increment_shadow_stack_push_count(&mut self) {
        self.shadow_stack_push_count += 1;
    }

    /// Reset the shadow stack push count (e.g., after popping).
    pub fn reset_shadow_stack_push_count(&mut self) {
        self.shadow_stack_push_count = 0;
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