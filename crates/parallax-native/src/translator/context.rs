use crate::NativeError;
use cranelift_codegen::ir::{Value, Block, Type, InstBuilder};
use cranelift_frontend::FunctionBuilder;
use parallax_gc::layout::{LayoutError, helpers::{get_size_bytes, get_alignment_bytes, get_struct_field_offset_bytes, get_enum_discriminant_info, get_discriminant_cl_type, get_enum_variant_info, layout_contains_handles}};
use parallax_hir::hir::{HirVar, HirType, HirStructDef, HirEnumDef, HirEnumVariant, Operand, HirLiteral, PrimitiveType};
use parallax_hir::Symbol;
use std::collections::{HashMap, HashSet};
use parallax_gc::{LayoutDescriptor, DescriptorIndex};
use std::sync::Arc;

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
    
    /// Reference to the module's struct definitions.
    struct_defs: &'ctx HashMap<Symbol, HirStructDef>,
    
    /// Reference to the module's enum definitions.
    enum_defs: &'ctx HashMap<Symbol, HirEnumDef>,
    
    /// Mutable reference to the central descriptor store's Vec.
    /// Descriptors are added here during layout calculation.
    type_descriptors: &'ctx mut Vec<LayoutDescriptor>,

    /// Cache mapping ADT Symbols to their computed DescriptorIndex.
    /// Used by get_or_create_descriptor_index.
    adt_descriptor_indices: &'ctx mut HashMap<Symbol, DescriptorIndex>,

    /// Cache for primitive types (HirType -> DescriptorIndex)
    primitive_descriptor_indices: HashMap<PrimitiveType, DescriptorIndex>,

    /// Cache for tuple types (Vec<HirType> -> DescriptorIndex)
    tuple_descriptor_indices: HashMap<Vec<HirType>, DescriptorIndex>,

    /// Cache for array types ((HirType, usize) -> DescriptorIndex)
    array_descriptor_indices: HashMap<(HirType, usize), DescriptorIndex>,

    /// Cache for function pointer types (Vec<HirType>, HirType -> DescriptorIndex)
    func_ptr_descriptor_indices: HashMap<(Vec<HirType>, HirType), DescriptorIndex>,

    /// Set of ADT symbols currently being processed for layout (detects recursion).
    processing_layout: HashSet<Symbol>,

    /// Index of the pre-generated static descriptor for ClosureRef.
    static_closure_ref_descriptor_index: Option<DescriptorIndex>,
    
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
    /// Create a new translation context.
    pub fn new(
        struct_defs: &'ctx HashMap<Symbol, HirStructDef>,
        enum_defs: &'ctx HashMap<Symbol, HirEnumDef>,
        type_descriptors: &'ctx mut Vec<LayoutDescriptor>,
        adt_descriptor_indices: &'ctx mut HashMap<Symbol, DescriptorIndex>,
        static_closure_ref_descriptor_index: Option<DescriptorIndex>,
    ) -> Self {
        // Initialize primitive caches, etc.
        let mut primitive_descriptor_indices = HashMap::new();
        // TODO: Pre-populate with known standard indices if they are truly fixed.
        // If Unit descriptor is guaranteed at index 3:
        // primitive_descriptor_indices.insert(ResolvePrimitiveType::Unit, UNIT_DESCRIPTOR_INDEX);

        Self {
            var_values: HashMap::new(),
            var_types: HashMap::new(),
            blocks: HashMap::new(),
            function_info: HashMap::new(),
            global_data_info: HashMap::new(),
            global_statics: HashMap::new(),
            struct_defs,
            enum_defs,
            type_descriptors,
            adt_descriptor_indices,
            primitive_descriptor_indices,
            tuple_descriptor_indices: HashMap::new(),
            array_descriptor_indices: HashMap::new(),
            func_ptr_descriptor_indices: HashMap::new(),
            processing_layout: HashSet::new(),
            static_closure_ref_descriptor_index,
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

    /// Get the index of the static ClosureRef descriptor.
    pub fn get_static_closure_ref_descriptor_index(&self) -> Result<DescriptorIndex, NativeError> {
        self.static_closure_ref_descriptor_index
            .ok_or_else(|| NativeError::LayoutError(LayoutError::Other("Static ClosureRef descriptor index not found in context".to_string())))
    }

    /// Adds a dynamically generated descriptor and returns its index.
    pub fn add_dynamic_descriptor(&mut self, descriptor: LayoutDescriptor) -> DescriptorIndex {
        let index = self.type_descriptors.len();
        self.type_descriptors.push(descriptor);
        index
    }

    /// Gets or creates the layout descriptor index for a given HIR type.
    /// This is the main entry point for layout calculation during translation.
    pub fn get_or_create_descriptor_index(
        &mut self,
        hir_type: &HirType,
    ) -> Result<DescriptorIndex, NativeError> {
        // --- 1. Check Caches --- //
        match hir_type {
            HirType::Primitive(prim) => {
                if let Some(index) = self.primitive_descriptor_indices.get(prim) {
                    return Ok(*index);
                }
            }
            HirType::Adt(symbol) => {
                if let Some(index) = self.adt_descriptor_indices.get(symbol) {
                    return Ok(*index);
                }
                 // Check recursion guard
                 if !self.processing_layout.insert(*symbol) {
                    // Cycle detected! For now, assume recursive types are represented by Handles.
                    // TODO: Verify this assumption or allow recursive struct layout if possible.
                    return Ok(HANDLE_DESCRIPTOR_INDEX);
                }
            }
            HirType::Tuple(elements) => {
                if let Some(index) = self.tuple_descriptor_indices.get(elements) {
                    return Ok(*index);
                }
                // TODO: Add recursion detection/handling for tuples if needed.
            }
            HirType::Array(elem_ty, size) => {
                 let key = ((*elem_ty).as_ref().clone(), *size); // Clone needed for map key
                 if let Some(index) = self.array_descriptor_indices.get(&key) {
                     return Ok(*index);
                 }
                // TODO: Add recursion detection/handling for arrays if needed.
            }
            HirType::FunctionPointer(params, ret) => {
                 let key = (params.clone(), (**ret).clone()); // Clone needed for map key
                 if let Some(index) = self.func_ptr_descriptor_indices.get(&key) {
                     return Ok(*index);
                 }
            }
            HirType::Never => {
                 // Never should likely resolve to something like Unit layout (size 0)
                 // Let's assume Unit is pre-registered or handle it below.
                 // Fall through to primitive handling for Unit.
                 if let Some(index) = self.primitive_descriptor_indices.get(&PrimitiveType::Unit) {
                     return Ok(*index);
                 }
            }
        }

        // --- 2. Compute LayoutDescriptor (Recursive Step) --- //
        let computed_descriptor = match hir_type {
            HirType::Primitive(prim) => self.compute_primitive_descriptor(*prim)?,
            HirType::Adt(symbol) => self.compute_adt_descriptor(*symbol)?,
            HirType::Tuple(elements) => self.compute_tuple_descriptor(elements)?,
            HirType::Array(elem_ty, size) => self.compute_array_descriptor(elem_ty, *size)?,
            HirType::FunctionPointer(_, _) => LayoutDescriptor::Handle, // Represent function pointers as GC Handles
            HirType::Never => self.compute_primitive_descriptor(PrimitiveType::Unit)?, // Treat Never like Unit
        };

        // --- 3. Store Descriptor & Update Cache --- //
        let new_index = self.type_descriptors.len();
        self.type_descriptors.push(computed_descriptor);

        // Update the appropriate cache
        match hir_type {
            HirType::Primitive(prim) => {
                self.primitive_descriptor_indices.insert(*prim, new_index);
            }
            HirType::Adt(symbol) => {
                self.adt_descriptor_indices.insert(*symbol, new_index);
                self.processing_layout.remove(symbol); // Remove from recursion guard
            }
            HirType::Tuple(elements) => {
                self.tuple_descriptor_indices.insert(elements.clone(), new_index);
            }
            HirType::Array(elem_ty, size) => {
                let key = ((*elem_ty).as_ref().clone(), *size);
                self.array_descriptor_indices.insert(key, new_index);
            }
            HirType::FunctionPointer(params, ret) => {
                let key = (params.clone(), (**ret).clone());
                self.func_ptr_descriptor_indices.insert(key, new_index);
            }
             HirType::Never => {
                 self.primitive_descriptor_indices.insert(PrimitiveType::Unit, new_index);
             }
        }

        Ok(new_index)
    }

    /// Checks the caches immutably for a descriptor index.
    pub fn find_descriptor_index_in_cache(&self, hir_type: &HirType) -> Option<DescriptorIndex> {
        match hir_type {
            HirType::Primitive(prim) => self.primitive_descriptor_indices.get(prim).copied(),
            HirType::Adt(symbol) => self.adt_descriptor_indices.get(symbol).copied(),
            HirType::Tuple(elements) => self.tuple_descriptor_indices.get(elements).copied(),
            HirType::Array(elem_ty, size) => {
                let key = ((*elem_ty).as_ref().clone(), *size);
                self.array_descriptor_indices.get(&key).copied()
            }
            HirType::FunctionPointer(params, ret) => {
                let key = (params.clone(), (**ret).clone());
                self.func_ptr_descriptor_indices.get(&key).copied()
            }
            HirType::Never => self.primitive_descriptor_indices.get(&PrimitiveType::Unit).copied(),
        }
    }

    // --- Helper methods for computing specific LayoutDescriptor variants --- //

    fn compute_primitive_descriptor(&mut self, prim: PrimitiveType) -> Result<LayoutDescriptor, NativeError> {
         // TODO: Handle String separately? String needs heap allocation.
         // For layout purpose, it could be Primitive { ptr_size, ptr_align } if represented as Handle.
         // Let's assume Handle descriptor is used for String. String handled in translate_operand.
         if prim == PrimitiveType::String { return Ok(LayoutDescriptor::Handle); }
         if prim == PrimitiveType::Unit { return Ok(LayoutDescriptor::Primitive{ size_bytes: 0, align_bytes: 1}); }

        let (size_bytes, align_bytes) = match prim {
            PrimitiveType::I8 | PrimitiveType::U8 | PrimitiveType::Bool => (1, 1),
            PrimitiveType::I16 | PrimitiveType::U16 => (2, 2),
            PrimitiveType::I32 | PrimitiveType::U32 | PrimitiveType::Char => (4, 4),
            PrimitiveType::I64 | PrimitiveType::U64 | PrimitiveType::F64 => (8, 8),
            PrimitiveType::I128 | PrimitiveType::U128 => (16, 8), // Assuming 8-byte alignment for 128-bit
            PrimitiveType::F32 => (4, 4),
            PrimitiveType::Unit | PrimitiveType::String => unreachable!(), // Handled above
        };
        Ok(LayoutDescriptor::Primitive { size_bytes, align_bytes })
    }

    fn compute_adt_descriptor(&mut self, symbol: Symbol) -> Result<LayoutDescriptor, NativeError> {
        if let Some(struct_def) = self.struct_defs.get(&symbol) {
            self.compute_struct_descriptor(struct_def)
        } else if let Some(enum_def) = self.enum_defs.get(&symbol) {
            self.compute_enum_descriptor(enum_def)
        } else {
            Err(NativeError::LayoutError(LayoutError::Other(format!("ADT Symbol {:?} not found during layout computation", symbol))))
        }
    }

    fn compute_struct_descriptor(&mut self, struct_def: &HirStructDef) -> Result<LayoutDescriptor, NativeError> {
        let mut fields_layout: Vec<(usize, DescriptorIndex)> = Vec::with_capacity(struct_def.fields.len());
        let mut current_offset = 0;
        let mut max_align = 1;
        let mut handle_offsets_vec = Vec::new();

        for (_field_sym, _field_name, field_hir_type) in &struct_def.fields {
            let field_desc_idx = self.get_or_create_descriptor_index(field_hir_type)?;
            // Need the actual descriptor to get size/align
            let field_descriptor = self.type_descriptors.get(field_desc_idx)
                 .ok_or_else(|| NativeError::LayoutError(LayoutError::Other("Failed to get field descriptor during struct layout".to_string())))?;
            let field_size = get_size_bytes(field_descriptor);
            let field_align = get_alignment_bytes(field_descriptor);

            // Align current offset
            current_offset = (current_offset + field_align - 1) & !(field_align - 1);
            fields_layout.push((current_offset, field_desc_idx));

            if layout_contains_handles(field_descriptor, self.type_descriptors) {
                handle_offsets_vec.push(current_offset);
            }

            current_offset += field_size;
            max_align = max_align.max(field_align);
        }

        let size_bytes = (current_offset + max_align - 1) & !(max_align - 1); // Final size
        Ok(LayoutDescriptor::Struct {
            size_bytes,
            align_bytes: max_align,
            fields: fields_layout.into_boxed_slice(),
            handle_offsets: handle_offsets_vec.into_boxed_slice(),
        })
    }

    fn compute_enum_descriptor(&mut self, enum_def: &HirEnumDef) -> Result<LayoutDescriptor, NativeError> {
        if enum_def.variants.is_empty() {
            // Represent empty enum like Unit
            return self.compute_primitive_descriptor(PrimitiveType::Unit);
        }

        // 1. Determine Discriminant Size/Alignment
        let num_variants = enum_def.variants.len();
        let (discriminant_size_bytes, discriminant_align_bytes) = if num_variants <= (u8::MAX as usize + 1) { (1, 1)
        } else if num_variants <= (u16::MAX as usize + 1) { (2, 2)
        } else if num_variants <= (u32::MAX as usize + 1) { (4, 4)
        } else { (8, 8) }; // Assume u64 max
        let discriminant_offset = 0; // Assume discriminant is first

        // 2. Calculate Max Variant Payload Layout & Prepare Variant Info
        let mut max_payload_size = 0;
        let mut max_payload_align = 1;
        let mut variants_layout_info = Vec::with_capacity(enum_def.variants.len());

        for (i, variant) in enum_def.variants.iter().enumerate() {
            let payload_hir_type = HirType::Tuple(variant.fields.clone());
            let payload_desc_idx = self.get_or_create_descriptor_index(&payload_hir_type)?;
            let payload_descriptor = self.type_descriptors.get(payload_desc_idx)
                .ok_or_else(|| NativeError::LayoutError(LayoutError::Other("Failed to get payload descriptor during enum layout".to_string())))?;

            let payload_size = get_size_bytes(payload_descriptor);
            let payload_align = get_alignment_bytes(payload_descriptor);

            // Calculate offset for this payload relative to the start of the enum object
            let payload_offset = (discriminant_size_bytes + payload_align - 1) & !(payload_align - 1);
            variants_layout_info.push((i as u64, payload_offset, payload_desc_idx)); // Use index as discriminant value

            max_payload_size = max_payload_size.max(payload_size);
            max_payload_align = max_payload_align.max(payload_align);

            // Check if payload contains handles, but don't store offset here
            let _payload_contains_handles = layout_contains_handles(payload_descriptor, self.type_descriptors);

        }

        // 3. Calculate Overall Enum Layout
        let overall_align = discriminant_align_bytes.max(max_payload_align);
        let union_payload_offset = (discriminant_size_bytes + max_payload_align - 1) & !(max_payload_align - 1);
        let size_bytes = (union_payload_offset + max_payload_size + overall_align - 1) & !(overall_align - 1);

        Ok(LayoutDescriptor::Enum {
            size_bytes,
            align_bytes: overall_align,
            discriminant_offset,
            discriminant_size_bytes,
            variants: variants_layout_info.into_boxed_slice(),
        })
    }

    fn compute_tuple_descriptor(&mut self, elements: &[HirType]) -> Result<LayoutDescriptor, NativeError> {
         if elements.is_empty() {
             return self.compute_primitive_descriptor(PrimitiveType::Unit);
         }

         // Treat tuple like an unnamed struct
         let mut fields_layout: Vec<(usize, DescriptorIndex)> = Vec::with_capacity(elements.len());
         let mut current_offset = 0;
         let mut max_align = 1;
         let mut handle_offsets_vec = Vec::new();

         for elem_hir_type in elements {
             let elem_desc_idx = self.get_or_create_descriptor_index(elem_hir_type)?;
             let elem_descriptor = self.type_descriptors.get(elem_desc_idx)
                 .ok_or_else(|| NativeError::LayoutError(LayoutError::Other("Failed to get element descriptor during tuple layout".to_string())))?;
             let elem_size = get_size_bytes(elem_descriptor);
             let elem_align = get_alignment_bytes(elem_descriptor);

             current_offset = (current_offset + elem_align - 1) & !(elem_align - 1);
             fields_layout.push((current_offset, elem_desc_idx));

             if layout_contains_handles(elem_descriptor, self.type_descriptors) {
                 handle_offsets_vec.push(current_offset);
             }

             current_offset += elem_size;
             max_align = max_align.max(elem_align);
         }

         let size_bytes = (current_offset + max_align - 1) & !(max_align - 1);
         Ok(LayoutDescriptor::Struct {
             size_bytes,
             align_bytes: max_align,
             fields: fields_layout.into_boxed_slice(),
             handle_offsets: handle_offsets_vec.into_boxed_slice(),
         })
     }

     fn compute_array_descriptor(&mut self, element_ty: &HirType, count: usize) -> Result<LayoutDescriptor, NativeError> {
         let elem_desc_idx = self.get_or_create_descriptor_index(element_ty)?;
         let elem_descriptor = self.type_descriptors.get(elem_desc_idx)
             .ok_or_else(|| NativeError::LayoutError(LayoutError::Other("Failed to get element descriptor during array layout".to_string())))?;
         let elem_size = get_size_bytes(elem_descriptor);
         let elem_align = get_alignment_bytes(elem_descriptor);

         let stride = (elem_size + elem_align - 1) & !(elem_align - 1);
         let size_bytes = if count == 0 {
             0
         } else {
             stride * count
         };
         let align_bytes = elem_align;

         let element_contains_handles = layout_contains_handles(elem_descriptor, self.type_descriptors);

         Ok(LayoutDescriptor::Array {
             size_bytes,
             align_bytes,
             element_descriptor_index: elem_desc_idx,
             count,
             element_stride_bytes: stride,
             element_contains_handles,
         })
     }

    // Provides immutable access to the underlying TypeDescriptor storage.
    /// Needed by translation code to get stable pointers (with caveats).
    pub fn type_descriptors_slice(&self) -> &[LayoutDescriptor] {
        &self.type_descriptors
    }

    /// Gets an immutable reference to a specific LayoutDescriptor by its index.
    pub fn get_descriptor_by_index(&self, index: DescriptorIndex) -> Option<&LayoutDescriptor> {
        self.type_descriptors.get(index)
    }

    /// Gets the Cranelift type for an enum discriminant based on its descriptor index.
    pub fn get_discriminant_cl_type(&self, enum_descriptor_index: DescriptorIndex) -> Result<Type, NativeError> {
        let descriptor = self.get_descriptor_by_index(enum_descriptor_index)
            .ok_or_else(|| NativeError::LayoutError(LayoutError::Other("Enum descriptor not found".to_string())))?;
        if let LayoutDescriptor::Enum { discriminant_size_bytes, .. } = descriptor {
            match discriminant_size_bytes {
                1 => Ok(Type::int(8).unwrap()),
                2 => Ok(Type::int(16).unwrap()),
                4 => Ok(Type::int(32).unwrap()),
                8 => Ok(Type::int(64).unwrap()),
                _ => Err(NativeError::LayoutError(LayoutError::Other("Invalid discriminant size".to_string()))),
            }
        } else {
            Err(NativeError::LayoutError(LayoutError::Other("Descriptor is not an Enum".to_string())))
        }
    }

    /// Gets discriminant layout info (offset, size) for an enum.
    pub fn get_enum_discriminant_info(&self, enum_descriptor_index: DescriptorIndex) -> Result<(usize, usize), NativeError> {
        let descriptor = self.get_descriptor_by_index(enum_descriptor_index)
            .ok_or_else(|| NativeError::LayoutError(LayoutError::Other("Enum descriptor not found".to_string())))?;
        if let LayoutDescriptor::Enum { discriminant_offset, discriminant_size_bytes, .. } = descriptor {
            Ok((*discriminant_offset, *discriminant_size_bytes))
        } else {
            Err(NativeError::LayoutError(LayoutError::Other("Descriptor is not an Enum".to_string())))
        }
    }

    /// Gets layout info for a specific enum variant (discriminant value, payload offset, payload descriptor index).
    pub fn get_enum_variant_info(&self, enum_descriptor_index: DescriptorIndex, variant_index: usize) -> Result<(u64, usize, DescriptorIndex), NativeError> {
        let descriptor = self.get_descriptor_by_index(enum_descriptor_index)
            .ok_or_else(|| NativeError::LayoutError(LayoutError::Other("Enum descriptor not found".to_string())))?;
        if let LayoutDescriptor::Enum { variants, .. } = descriptor {
            variants.get(variant_index)
                .copied()
                .ok_or_else(|| NativeError::LayoutError(LayoutError::Other("Variant index out of bounds".to_string())))
        } else {
            Err(NativeError::LayoutError(LayoutError::Other("Descriptor is not an Enum".to_string())))
        }
    }

    /// Gets the byte offset and descriptor index for a field within an aggregate type
    /// (Struct, Tuple, or Array).
    /// For arrays, field_index must be 0, and it returns the element's info.
    /// Works for Structs and Tuples (which use LayoutDescriptor::Struct).
    pub fn get_aggregate_field_info(&self, aggregate_descriptor_index: DescriptorIndex, field_index: usize) -> Result<(usize, DescriptorIndex), NativeError> {
        let descriptor = self.get_descriptor_by_index(aggregate_descriptor_index)
            .ok_or_else(|| NativeError::LayoutError(LayoutError::Other(format!("Descriptor index {} not found for aggregate field lookup", aggregate_descriptor_index))))?;
        match descriptor {
            LayoutDescriptor::Struct { fields, .. } => {
                if field_index >= fields.len() {
                    return Err(NativeError::LayoutError(LayoutError::Other(format!(
                        "Field index {} out of bounds for struct/tuple descriptor index {}",
                        field_index, aggregate_descriptor_index
                    ))));
                }
                Ok((fields[field_index].0, fields[field_index].1))
            }
            LayoutDescriptor::Array { element_descriptor_index, .. } => {
                if field_index >= 1 {
                    return Err(NativeError::LayoutError(LayoutError::Other(format!(
                        "Field index {} out of bounds for array descriptor index {}",
                        field_index, aggregate_descriptor_index
                    ))));
                }
                Ok((0, *element_descriptor_index))
            }
            _ => Err(NativeError::LayoutError(LayoutError::Other(format!(
                "Expected Struct, Tuple, or Array descriptor at index {}, found {:?}",
                aggregate_descriptor_index, descriptor
            )))),
        }
    }

    /// Attempts to reconstruct the HirType from a DescriptorIndex.
    /// This is inherently limited because LayoutDescriptor discards some
    /// high-level type information (like specific struct/enum names).
    /// It's primarily useful for simple cases or where an approximate type is sufficient.
    pub fn get_type_from_descriptor_index(&self, index: DescriptorIndex) -> Result<HirType, NativeError> {
        // Check cache first? (Maybe add caching later if needed)
        let descriptor = self.get_descriptor_by_index(index)
             .ok_or_else(|| NativeError::LayoutError(LayoutError::Other(format!("Descriptor index {} not found for type reconstruction", index))))?;

        // Match on the *actual* LayoutDescriptor variants from parallax-gc
        match descriptor {
            LayoutDescriptor::Primitive { size_bytes, .. } => {
                // Guess primitive type based on size. This is lossy!
                match size_bytes {
                    0 => Ok(HirType::Primitive(PrimitiveType::Unit)), // Special case for ZST
                    1 => Ok(HirType::Primitive(PrimitiveType::U8)), // Could be I8, Bool
                    2 => Ok(HirType::Primitive(PrimitiveType::U16)), // Could be I16
                    4 => Ok(HirType::Primitive(PrimitiveType::U32)), // Could be I32, F32, Char
                    8 => Ok(HirType::Primitive(PrimitiveType::U64)), // Could be I64, F64
                    16 => Ok(HirType::Primitive(PrimitiveType::U128)), // Could be I128
                    _ => Err(NativeError::LayoutError(LayoutError::Other(format!("Unsupported primitive size {} for descriptor index {}", size_bytes, index))))
                }
            }
            LayoutDescriptor::Handle => {
                // Most ambiguous case. No direct HirType equivalent.
                // Map to U64 as a placeholder pointer-like type.
                // Callers should ideally avoid relying on this reconstruction.
                Ok(HirType::Primitive(PrimitiveType::U64))
            }
            LayoutDescriptor::Struct { fields, .. } => {
                // Cannot reconstruct original ADT symbol.
                // Attempt to reconstruct as HirType::Tuple.
                let element_types = fields
                    .iter()
                    .map(|(_, field_desc_idx)| self.get_type_from_descriptor_index(*field_desc_idx))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(HirType::Tuple(element_types))
            }
            LayoutDescriptor::Enum { .. } => {
                // Cannot reconstruct original ADT symbol or variants meaningfully.
                Err(NativeError::LayoutError(LayoutError::Other(format!(
                    "Cannot reconstruct HirType from Enum descriptor index {}", index
                ))))
            }
            LayoutDescriptor::Array { element_descriptor_index, count, .. } => {
                let element_type = self.get_type_from_descriptor_index(*element_descriptor_index)?;
                // Use the correct tuple variant syntax for HirType::Array
                Ok(HirType::Array(Arc::new(element_type), *count))
            }
        }
    }

    /// Recursively check if a type requires heap allocation.
    /// Uses `visiting` set to prevent infinite loops for recursive types.
    pub fn type_needs_heap_allocation(
        &mut self, // Needs mutable access to potentially compute nested descriptors
        hir_type: &HirType,
        visiting: &mut HashSet<Symbol>
    ) -> bool {
        match hir_type {
            HirType::Primitive(PrimitiveType::String) => true, // String requires heap
            HirType::Primitive(_) => false,
            HirType::Tuple(elements) => {
                 elements.iter().any(|elem_ty| self.type_needs_heap_allocation(elem_ty, visiting))
             }
            HirType::Array(_, _) => true,
            HirType::FunctionPointer(_, _) => true, // Closures (created from FnPtrs) are heap allocated
            HirType::Never => false,
            // ADTs (structs/enums) need deeper inspection
            HirType::Adt(symbol) => {
                // Prevent infinite recursion
                if visiting.contains(symbol) {
                    return false; // Assume non-recursive part determines heap need
                }
                visiting.insert(*symbol);

                // Get the descriptor, potentially creating it
                let desc_idx_result = self.get_or_create_descriptor_index(hir_type);
                
                // Need to clone the descriptor store reference to pass immutably to layout_contains_handles
                // This is a workaround for the borrow checker. It's not ideal performance-wise.
                // A better solution might involve refactoring layout_contains_handles or the context.
                let descriptor_vec_clone = self.type_descriptors.clone(); 
                
                let needs_heap = match desc_idx_result {
                    Ok(desc_idx) => {
                        if let Some(descriptor) = self.type_descriptors.get(desc_idx) { // Immutable borrow here
                             // Use the stateless helper function `layout_contains_handles`
                             // Pass the cloned descriptor store
                             layout_contains_handles(descriptor, &descriptor_vec_clone)
                        } else {
                            false // Descriptor not found, assume false
                        }
                    }
                    Err(_) => false, // Error getting descriptor index, assume false
                };

                visiting.remove(symbol); // Backtrack
                needs_heap
            }
        }
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

// --- Constant Descriptor Indices (Assumed to be initialized in backend.rs) --- //
// TODO: Define these robustly, potentially getting them from backend init.
const PRIMITIVE_POINTER_SIZE_DESCRIPTOR_INDEX: DescriptorIndex = 1; // Placeholder
const HANDLE_DESCRIPTOR_INDEX: DescriptorIndex = 2; // Placeholder for Handle
// Add indices for other common primitives (i64, f64, bool, etc.) if needed frequently.
// Or generate them on demand via get_or_create_descriptor_index.
const UNIT_DESCRIPTOR_INDEX: DescriptorIndex = 3; // Placeholder for Primitive { size=0, align=1 }