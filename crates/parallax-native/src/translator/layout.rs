use crate::translator::context::TranslationContext;
use crate::NativeError;
use parallax_hir::hir::{HirType, HirStructDef, HirEnumDef, ResolvePrimitiveType, HirEnumVariant};
use parallax_hir::Symbol;
use repc::layout::{Type, TypeLayout, TypeVariant, BuiltinType, Record, RecordField, RecordKind, Array, FieldLayout, Layout};
use repc::{compute_layout, HOST_TARGET, Target};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

/// State for handling recursive type layout computation and cycles.
pub struct LayoutComputer {
    layout_cache: HashMap<Symbol, Arc<TypeLayout>>,
    /// Store simplified errors for ADTs.
    error_cache: HashMap<Symbol, String>,
    processing: HashSet<Symbol>,
    /// Cache for detailed enum layout information.
    enum_info_cache: HashMap<Symbol, Arc<EnumLayoutInfo>>,
}

/// Holds detailed layout information calculated manually for enums.
pub struct EnumLayoutInfo {
    /// Overall layout (size, alignment) of the enum.
    pub overall_layout: Arc<TypeLayout>,
    /// The Cranelift integer type used for the discriminant.
    pub discriminant_cl_type: cranelift_codegen::ir::Type,
    /// The size of the discriminant in bytes.
    pub discriminant_size_bytes: u64,
    /// The offset of the discriminant in bytes (usually 0).
    pub discriminant_offset_bytes: u64,
    /// Map from Variant Symbol to the offset of its payload in bytes.
    pub variant_payload_offsets: HashMap<Symbol, u64>,
    /// Map from Variant Symbol to the layout of its payload (as a tuple).
    pub variant_payload_layouts: HashMap<Symbol, Arc<TypeLayout>>,
}

pub fn new_layout_computer() -> LayoutComputer {
    LayoutComputer {
        layout_cache: HashMap::new(),
        error_cache: HashMap::new(),
        processing: HashSet::new(),
        enum_info_cache: HashMap::new(), // Initialize the new cache
    }
}

impl LayoutComputer {
    /// Computes the ABI layout for a given HIR type.
    /// This is the main recursive function called by get_layout.
    fn compute_hir_type_layout<'ctx, 'hir>(
        &mut self, 
        hir_type: &HirType, 
        ctx: &'ctx TranslationContext<'hir>
    ) -> Result<Arc<TypeLayout>, NativeError> {
        // NOTE: Caching for ADTs is handled by the caller (get_layout) to manage recursion.
        // Caching for primitives/tuples/arrays could be added here if needed.

        match hir_type {
            HirType::Adt(symbol) => {
                 // ADT layout is handled by get_layout using hir_adt_to_repc_type_or_layout
                 // This path shouldn't be taken directly for ADTs from outside get_layout.
                 // However, recursive calls for fields *within* ADTs might land here.
                 // We rely on get_layout being called for those fields.
                 // Let's call get_layout here to ensure consistency with caching/recursion handling.
                 get_layout(hir_type, self, ctx)
            }
            HirType::Primitive(_) | HirType::Tuple(_) | HirType::Array(_, _) | HirType::FunctionPointer(_, _) | HirType::Never => {
                 // Handle non-ADT types by converting to repc Type and computing layout
                 let repc_type = self.hir_to_repc_type(hir_type, ctx)?;
                 let target = HOST_TARGET.as_ref().expect("Host target not available");
                 compute_layout(*target, &repc_type)
                     .map(|computed_type_with_layout| Arc::new(computed_type_with_layout.layout))
                     .map_err(|e| NativeError::LayoutError(format!("repc layout computation failed for {:?}: {}", hir_type, e)))
             }
        }
    }
    
    /// Converts an HIR ADT (struct/enum) identified by Symbol to a repc Type
    /// or computes enum layout manually.
    fn hir_adt_to_repc_type_or_layout<'ctx, 'hir>(
        &mut self,
        symbol: Symbol,
        ctx: &'ctx TranslationContext<'hir>,
    ) -> Result<Either<Type<()>, Arc<TypeLayout>>, NativeError> { // Return Either Type or Layout
        if let Some(struct_def) = ctx.get_struct_def(symbol) {
            // Existing struct logic using repc
            self.hir_struct_to_repc_record(struct_def, ctx).map(Either::Left)
        } else if let Some(enum_def) = ctx.get_enum_def(symbol) {
            // New enum logic using manual calculation
            // NOTE: Caching & cycle detection is handled by the caller (get_layout)
            self.compute_manual_enum_layout(enum_def, ctx).map(Either::Right) // Wrap computed layout in Either::Right
        } else {
            Err(NativeError::LayoutError(format!(
                "ADT symbol {:?} not found during layout computation",
                symbol
            )))
        }
    }

    /// Computes the layout for an enum manually.
    fn compute_manual_enum_layout<'ctx, 'hir>(
        &mut self,
        enum_def: &'ctx HirEnumDef,
        ctx: &'ctx TranslationContext<'hir>,
    ) -> Result<Arc<TypeLayout>, NativeError> {
        // Check cache for detailed info first
        if let Some(cached_info) = self.enum_info_cache.get(&enum_def.symbol) {
            return Ok(cached_info.overall_layout.clone());
        }
        
        if enum_def.variants.is_empty() {
            // Represent empty enum like Unit
            let unit_layout = self.compute_hir_type_layout(&HirType::Primitive(ResolvePrimitiveType::Unit), ctx)?;
            // Cache minimal info for empty enum
            let enum_info = EnumLayoutInfo {
                overall_layout: unit_layout.clone(),
                discriminant_cl_type: cranelift_codegen::ir::types::I8, // Dummy type
                discriminant_size_bytes: 0,
                discriminant_offset_bytes: 0,
                variant_payload_offsets: HashMap::new(),
                variant_payload_layouts: HashMap::new(),
            };
            self.enum_info_cache.insert(enum_def.symbol, Arc::new(enum_info));
            return Ok(unit_layout);
        }

        // --- 1. Determine Discriminant Type/Size/Alignment --- 
        let num_variants = enum_def.variants.len();
        let (discriminant_cl_type, discriminant_layout) = if num_variants <= u8::MAX as usize + 1 {
            (cranelift_codegen::ir::types::I8, self.compute_hir_type_layout(&HirType::Primitive(ResolvePrimitiveType::U8), ctx)?)
        } else if num_variants <= u16::MAX as usize + 1 {
             (cranelift_codegen::ir::types::I16, self.compute_hir_type_layout(&HirType::Primitive(ResolvePrimitiveType::U16), ctx)?)
        } else if num_variants <= u32::MAX as usize + 1 {
             (cranelift_codegen::ir::types::I32, self.compute_hir_type_layout(&HirType::Primitive(ResolvePrimitiveType::U32), ctx)?)
        } else {
             (cranelift_codegen::ir::types::I64, self.compute_hir_type_layout(&HirType::Primitive(ResolvePrimitiveType::U64), ctx)?)
        };
        let discriminant_size_bytes = get_size_bytes(&discriminant_layout);
        let discriminant_align_bytes = get_alignment_bytes(&discriminant_layout);
        let discriminant_offset_bytes = 0; // Discriminant is always first for now

        // --- 2. Calculate Max Variant Payload Layout & Store Individual Layouts/Offsets --- 
        let mut max_payload_size_bytes: u64 = 0;
        let mut max_payload_align_bytes: u64 = 1; 
        let mut variant_payload_offsets: HashMap<Symbol, u64> = HashMap::new();
        let mut variant_payload_layouts: HashMap<Symbol, Arc<TypeLayout>> = HashMap::new();

        for variant in &enum_def.variants {
            let payload_layout: Arc<TypeLayout>;
            let payload_size: u64;
            let payload_align: u64;

            if variant.fields.is_empty() {
                // Use layout of Unit for empty payload
                payload_layout = self.compute_hir_type_layout(&HirType::Primitive(ResolvePrimitiveType::Unit), ctx)?;
                payload_size = 0;
                payload_align = 1;
            } else {
                let payload_hir_type = HirType::Tuple(variant.fields.clone());
                payload_layout = self.compute_hir_type_layout(&payload_hir_type, ctx)?;
                payload_size = get_size_bytes(&payload_layout);
                payload_align = get_alignment_bytes(&payload_layout);
            }

            // Store layout for this specific variant
            variant_payload_layouts.insert(variant.symbol, payload_layout.clone());

            // Calculate payload offset for this variant (discriminant size rounded up to *this* payload's alignment)
            let current_payload_offset = round_up_to_alignment(discriminant_size_bytes, payload_align);
            variant_payload_offsets.insert(variant.symbol, current_payload_offset);

            // Update maximums needed for overall layout calculation
            max_payload_size_bytes = max_payload_size_bytes.max(payload_size);
            max_payload_align_bytes = max_payload_align_bytes.max(payload_align);
        }

        // --- 3. Calculate Overall Enum Layout --- 
        let overall_align_bytes = discriminant_align_bytes.max(max_payload_align_bytes);
        // Calculate offset for the start of the *union* part of the payload area
        let union_payload_offset_bytes = round_up_to_alignment(discriminant_size_bytes, max_payload_align_bytes);
        let total_size_bytes = round_up_to_alignment(union_payload_offset_bytes + max_payload_size_bytes, overall_align_bytes);

        // --- 4. Construct Overall TypeLayout --- 
        let overall_align_bits = overall_align_bytes * 8;
        let overall_layout = Arc::new(TypeLayout {
            size_bits: total_size_bytes * 8,
            field_alignment_bits: overall_align_bits, 
            pointer_alignment_bits: overall_align_bits, 
            required_alignment_bits: overall_align_bits,
        });

        // --- 5. Construct and Cache EnumLayoutInfo --- 
        let enum_info = EnumLayoutInfo {
            overall_layout: overall_layout.clone(),
            discriminant_cl_type, // Store the determined Cranelift type
            discriminant_size_bytes,
            discriminant_offset_bytes,
            variant_payload_offsets,
            variant_payload_layouts,
        };
        self.enum_info_cache.insert(enum_def.symbol, Arc::new(enum_info));

        // Return the overall layout
        Ok(overall_layout)
    }

    /// Converts an HIR type to the `repc` representation (`Type<()>`).
    /// Used for primitives, tuples, arrays, function pointers, and structs.
    /// Enums are handled separately for layout calculation.
    fn hir_to_repc_type<'ctx, 'hir>(
        &mut self, 
        hir_type: &HirType, 
        ctx: &'ctx TranslationContext<'hir>
    ) -> Result<Type<()>, NativeError> {
        match hir_type {
            HirType::Primitive(prim) => Self::hir_primitive_to_repc_type(*prim),
            HirType::Adt(symbol) => {
                // This should only be called for structs here, enums handled manually.
                if let Some(struct_def) = ctx.get_struct_def(*symbol) {
                     self.hir_struct_to_repc_record(struct_def, ctx)
                } else if ctx.get_enum_def(*symbol).is_some() {
                     Err(NativeError::LayoutError("Internal error: hir_to_repc_type called for enum".to_string()))
                } else {
                     Err(NativeError::LayoutError(format!("ADT symbol {:?} not found", symbol)))
                }
            }
            HirType::Tuple(elements) => self.hir_tuple_to_repc_record(elements, ctx),
            HirType::Array(element_ty, size) => self.hir_array_to_repc_array(element_ty, *size, ctx),
            HirType::FunctionPointer(..) => {
                // Represent function pointers as usize (assuming u64 for repc)
                Ok(Type { layout: (), annotations: vec![], variant: TypeVariant::Builtin(BuiltinType::U64) })
            }
            HirType::Never => {
                // Represent Never as an empty struct.
                Ok(Type { layout: (), annotations: vec![], variant: TypeVariant::Record(Record { kind: RecordKind::Struct, fields: vec![] }) })
            }
        }
    }

    /// Converts HIR primitive types to repc Type<()>
    fn hir_primitive_to_repc_type(prim: ResolvePrimitiveType) -> Result<Type<()>, NativeError> {
        match prim {
            ResolvePrimitiveType::Unit => {
                // Represent Unit as an empty struct
                Ok(Type { layout: (), annotations: vec![], variant: TypeVariant::Record(Record { kind: RecordKind::Struct, fields: vec![] }) })
            }
            ResolvePrimitiveType::String => {
                // Represent String as { ptr: u64, len: u64 }
                 let ptr_type = Type { layout: (), annotations: vec![], variant: TypeVariant::Builtin(BuiltinType::U64) }; // ptr as u64
                 let len_type = Type { layout: (), annotations: vec![], variant: TypeVariant::Builtin(BuiltinType::U64) }; // len as u64
                 let fields = vec![
                     RecordField { layout: None, annotations: vec![], named: true, bit_width: None, ty: ptr_type },
                     RecordField { layout: None, annotations: vec![], named: true, bit_width: None, ty: len_type },
                 ];
                 Ok(Type { layout: (), annotations: vec![], variant: TypeVariant::Record(Record { kind: RecordKind::Struct, fields }) })
            }
            // ResolvePrimitiveType::Isize and ResolvePrimitiveType::Usize should not appear here.
            // They should be resolved to fixed-size integers earlier.
            _ => {
                let builtin = match prim {
                    ResolvePrimitiveType::I8 => BuiltinType::I8,
                    ResolvePrimitiveType::I16 => BuiltinType::I16,
                    ResolvePrimitiveType::I32 => BuiltinType::I32,
                    ResolvePrimitiveType::I64 => BuiltinType::I64,
                    ResolvePrimitiveType::I128 => BuiltinType::I128,
                    ResolvePrimitiveType::U8 => BuiltinType::U8,
                    ResolvePrimitiveType::U16 => BuiltinType::U16,
                    ResolvePrimitiveType::U32 => BuiltinType::U32,
                    ResolvePrimitiveType::U64 => BuiltinType::U64,
                    ResolvePrimitiveType::U128 => BuiltinType::U128,
                    ResolvePrimitiveType::F32 => BuiltinType::F32,
                    ResolvePrimitiveType::F64 => BuiltinType::F64,
                    ResolvePrimitiveType::Bool => BuiltinType::Bool,
                    ResolvePrimitiveType::Char => BuiltinType::Char,
                    // Unit and String handled above
                    _ => return Err(NativeError::LayoutError(format!(
                        "Unexpected primitive type during layout: {:?}. Should be resolved earlier.",
                         prim
                    ))),
                };
                Ok(Type { layout: (), annotations: vec![], variant: TypeVariant::Builtin(builtin) })
            }
        }
    }

    /// Converts an HIR StructDef to a repc Record type.
    fn hir_struct_to_repc_record<'ctx, 'hir>(
        &mut self, 
        struct_def: &HirStructDef,
        ctx: &'ctx TranslationContext<'hir>
    ) -> Result<Type<()>, NativeError> {
        let mut fields = Vec::with_capacity(struct_def.fields.len());
        for (_field_symbol, _field_name, field_hir_type) in &struct_def.fields {
            // Use hir_to_repc_type for recursive conversion
            let field_repc_type = self.hir_to_repc_type(field_hir_type, ctx)?; 
            fields.push(RecordField {
                layout: None, 
                annotations: Vec::new(),
                named: true, 
                bit_width: None,
                ty: field_repc_type,
            });
        }
        Ok(Type { layout: (), annotations: vec![], variant: TypeVariant::Record(Record { kind: RecordKind::Struct, fields }) })
    }

    /// Converts an HIR Tuple type to a repc Record type (Struct kind).
    fn hir_tuple_to_repc_record<'ctx, 'hir>(
        &mut self, 
        elements: &[HirType],
        ctx: &'ctx TranslationContext<'hir>
    ) -> Result<Type<()>, NativeError> {
         if elements.is_empty() {
              return Self::hir_primitive_to_repc_type(ResolvePrimitiveType::Unit); // Use Unit representation
         }
         let mut fields = Vec::with_capacity(elements.len());
         for element_hir_type in elements.iter() {
             // Use hir_to_repc_type for recursive conversion
             let field_repc_type = self.hir_to_repc_type(element_hir_type, ctx)?;
             fields.push(RecordField {
                 layout: None,
                 annotations: Vec::new(),
                 named: false, // Tuple fields are positional, not named
                 bit_width: None,
                 ty: field_repc_type,
             });
         }
         Ok(Type { layout: (), annotations: vec![], variant: TypeVariant::Record(Record { kind: RecordKind::Struct, fields }) })
    }

     /// Converts an HIR Array type to a repc Array type.
     fn hir_array_to_repc_array<'ctx, 'hir>(
         &mut self, 
         element_ty: &Arc<HirType>,
         size: usize,
         ctx: &'ctx TranslationContext<'hir>
     ) -> Result<Type<()>, NativeError> {
         // Use hir_to_repc_type for recursive conversion
         let element_repc_type = self.hir_to_repc_type(element_ty, ctx)?;
         let array = Array { element_type: Box::new(element_repc_type), num_elements: Some(size as u64) }; 
         Ok(Type { layout: (), annotations: vec![], variant: TypeVariant::Array(array) })
     }
}

/// Helper function for alignment calculations
fn round_up_to_alignment(value: u64, alignment: u64) -> u64 {
    if alignment == 0 { return value; } // Avoid division by zero
    (value + alignment - 1) & !(alignment - 1)
}

/// Helper enum to distinguish between repc::Type and manually computed TypeLayout
#[derive(Debug)]
enum Either<L, R> {
    Left(L),
    Right(R),
}

/// Computes the ABI layout for a given HIR type using `repc` or manual calculation for enums.
/// Handles caching and recursion detection.
pub fn get_layout<'ctx, 'hir>(
    hir_type: &HirType,
    computer: &mut LayoutComputer,
    ctx: &'ctx TranslationContext<'hir>
) -> Result<Arc<TypeLayout>, NativeError> {
     match hir_type {
         HirType::Adt(symbol) => {
             // Check cache first
             if let Some(layout) = computer.layout_cache.get(symbol) {
                 return Ok(layout.clone());
             }
             if let Some(err_msg) = computer.error_cache.get(symbol) {
                  return Err(NativeError::LayoutError(format!("Cached error for {:?}: {}", symbol, err_msg)));
             }
              // Handle recursion detection
             if !computer.processing.insert(*symbol) {
                  return Err(NativeError::LayoutError(format!("Recursive type cycle detected for ADT {:?}", symbol)));
             }

             // Call the function that returns Either
             let result_either = computer.hir_adt_to_repc_type_or_layout(*symbol, ctx);

             // Process the result, cache it, and remove from processing set
             let layout_result = match result_either {
                 Ok(Either::Left(repc_type)) => {
                     // Need to compute layout from repc_type for structs
                     let target = HOST_TARGET.as_ref().expect("Host target not available");
                     compute_layout(*target, &repc_type)
                         .map(|computed_type_with_layout| Arc::new(computed_type_with_layout.layout))
                         .map_err(|e| NativeError::LayoutError(format!("repc layout computation failed for struct {:?}: {}", symbol, e)))
                 }
                 Ok(Either::Right(layout)) => Ok(layout), // Already have the layout for enums
                 Err(e) => Err(e),
             };

             // Update cache based on result
             match &layout_result {
                 Ok(layout) => {
                     computer.layout_cache.insert(*symbol, layout.clone());
                 }
                 Err(err) => {
                     // Cache simplified error message
                     computer.error_cache.insert(*symbol, format!("{}", err)); 
                 }
             }
             computer.processing.remove(symbol); // Remove symbol after processing
             layout_result
         }
         _ => { // Handle non-ADT types (Primitive, Tuple, Array, FuncPtr, Never)
             // These don't require the ADT-specific caching/recursion logic here,
             // rely on compute_hir_type_layout for their computation.
             computer.compute_hir_type_layout(hir_type, ctx)
         }
     }
}

// --- Helper functions using the computed layout ---

pub fn get_size_bytes(layout: &TypeLayout) -> u64 {
    (layout.size_bits + 7) / 8
}

pub fn get_alignment_bytes(layout: &TypeLayout) -> u64 {
    (layout.required_alignment_bits + 7) / 8
}

/// Retrieves the fully computed `repc` type definition with layout information.
/// Required for getting struct field offsets from `repc`.
/// Returns an error for Enums as they use manual layout.
pub fn get_repc_type_with_layout<'ctx, 'hir>(
    hir_type: &HirType,
    computer: &mut LayoutComputer,
    ctx: &'ctx TranslationContext<'hir>
) -> Result<Type<TypeLayout>, NativeError> {
     match hir_type {
         HirType::Adt(symbol) => {
              if ctx.get_enum_def(*symbol).is_some() {
                   // Enums now have manual layout, repc::Type<TypeLayout> is not directly available.
                   return Err(NativeError::LayoutError(format!(
                       "Cannot get repc::Type<TypeLayout> for enum {:?} using this function. Use specific enum layout helpers.",
                       symbol
                   )));
               } else { // Assume struct
                    // Recompute struct layout using repc
                    let repc_type = computer.hir_to_repc_type(hir_type, ctx)?;
                    let target = HOST_TARGET.as_ref().expect("Host target not available");
                    compute_layout(*target, &repc_type)
                         .map_err(|e| NativeError::LayoutError(format!("repc layout computation failed for struct {:?}: {}", symbol, e)))
                }
          }
          _ => { // Handle non-ADT types
                let repc_type = computer.hir_to_repc_type(hir_type, ctx)?;
                let target = HOST_TARGET.as_ref().expect("Host target not available");
                compute_layout(*target, &repc_type)
                    .map_err(|e| NativeError::LayoutError(format!("repc layout computation failed for {:?}: {}", hir_type, e)))
          }
     }
}

/// Get the offset in bytes of a field within a struct layout.
pub fn get_field_offset_bytes(
    computed_struct_type: &Type<TypeLayout>,
    field_index: usize,
) -> Result<u64, NativeError> {
    match &computed_struct_type.variant {
        TypeVariant::Record(record) => {
            if record.kind == RecordKind::Struct {
                if let Some(field) = record.fields.get(field_index) {
                     match field.layout {
                         Some(field_layout) => Ok((field_layout.offset_bits + 7) / 8),
                         None => Err(NativeError::LayoutError("Field layout not computed by repc".to_string())),
                     }
                } else {
                    Err(NativeError::LayoutError(format!("Field index {} out of bounds for struct {:?}", field_index, computed_struct_type)))
                }
            } else {
                Err(NativeError::LayoutError("Attempted to get field offset from non-struct record".to_string()))
            }
        }
        _ => Err(NativeError::LayoutError("Attempted to get field offset from non-record type".to_string())),
    }
}

// --- TODO: Add functions to get enum layout details ---
// pub fn get_enum_layout_info(...) -> Result<Arc<EnumLayoutInfo>, NativeError> { ... }
// pub fn get_enum_discriminant_type(...) -> Result<BuiltinType, NativeError> { ... }
// pub fn get_variant_payload_offset(...) -> Result<u64, NativeError> { ... }
// pub fn get_variant_payload_layout(...) -> Result<Arc<TypeLayout>, NativeError> { ... }

// --- Functions to get enum layout details ---

/// Ensures enum layout is computed and cached, then retrieves the cached info.
fn get_enum_layout_info<'ctx, 'hir>(
    enum_symbol: Symbol,
    computer: &mut LayoutComputer,
    ctx: &'ctx TranslationContext<'hir>,
) -> Result<Arc<EnumLayoutInfo>, NativeError> {
    // Ensure layout is computed and cached by calling get_layout
    // This computes both overall layout and populates enum_info_cache
    let _ = get_layout(&HirType::Adt(enum_symbol), computer, ctx)?;
    // Retrieve from cache
    computer.enum_info_cache.get(&enum_symbol)
        .cloned()
        .ok_or_else(|| NativeError::LayoutError(format!("Enum layout info for {:?} not found in cache after computation", enum_symbol)))
}

/// Gets the Cranelift type used for the discriminant of an enum.
pub fn get_enum_discriminant_type<'ctx, 'hir>(
    enum_symbol: Symbol,
    computer: &mut LayoutComputer,
    ctx: &'ctx TranslationContext<'hir>,
) -> Result<cranelift_codegen::ir::Type, NativeError> {
    let info = get_enum_layout_info(enum_symbol, computer, ctx)?;
    Ok(info.discriminant_cl_type)
}

/// Gets the offset (in bytes) of the discriminant field of an enum.
pub fn get_enum_discriminant_offset_bytes<'ctx, 'hir>(
    enum_symbol: Symbol,
    computer: &mut LayoutComputer,
    ctx: &'ctx TranslationContext<'hir>,
) -> Result<u64, NativeError> {
    let info = get_enum_layout_info(enum_symbol, computer, ctx)?;
    Ok(info.discriminant_offset_bytes) // Usually 0
}

/// Gets the offset (in bytes) of the payload for a specific enum variant.
pub fn get_variant_payload_offset_bytes<'ctx, 'hir>(
    enum_symbol: Symbol, // Need the parent enum symbol
    variant_symbol: Symbol,
    computer: &mut LayoutComputer,
    ctx: &'ctx TranslationContext<'hir>,
) -> Result<u64, NativeError> {
    let info = get_enum_layout_info(enum_symbol, computer, ctx)?;
    info.variant_payload_offsets.get(&variant_symbol)
        .copied()
        .ok_or_else(|| NativeError::LayoutError(format!("Payload offset for variant {:?} not found in enum {:?}", variant_symbol, enum_symbol)))
}

/// Gets the computed layout of the payload for a specific enum variant.
pub fn get_variant_payload_layout<'ctx, 'hir>(
    enum_symbol: Symbol, // Need the parent enum symbol
    variant_symbol: Symbol,
    computer: &mut LayoutComputer,
    ctx: &'ctx TranslationContext<'hir>,
) -> Result<Arc<TypeLayout>, NativeError> {
    let info = get_enum_layout_info(enum_symbol, computer, ctx)?;
    info.variant_payload_layouts.get(&variant_symbol)
        .cloned()
        .ok_or_else(|| NativeError::LayoutError(format!("Payload layout for variant {:?} not found in enum {:?}", variant_symbol, enum_symbol)))
}