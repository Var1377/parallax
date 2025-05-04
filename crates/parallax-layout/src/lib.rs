use parallax_hir::hir::{HirType, HirStructDef, HirEnumDef};
use parallax_hir::{PrimitiveType, Symbol};
use repc::layout::{Type, TypeVariant, BuiltinType, Record, RecordField, RecordKind, Annotation, Array};
use repc::Target;
use std::collections::{HashMap, HashSet};
use thiserror::Error;

// Make descriptor and helpers public within the layout module
mod adt;
mod array;
pub mod helpers;
mod tuple;
mod descriptor;

// Publicly export key types
pub use descriptor::{DescriptorIndex, LayoutDescriptor, DescriptorStore};
pub use helpers::{layout_contains_handles, get_discriminant_cl_type}; // Expose necessary helpers

/// Errors that can occur during layout computation.
#[derive(Error, Debug)]
pub enum LayoutError {
    #[error("Type recursion detected for symbol {0}")]
    TypeRecursion(Symbol),
    
    #[error("Layout computation failed: {0}")]
    RepcError(#[from] repc::Error),
    
    #[error("Unknown ADT symbol: {0}")]
    UnknownAdt(Symbol),
    
    #[error("Missing descriptor for index: {0}")]
    MissingDescriptor(DescriptorIndex),
    
    #[error("Invalid field index: {0}")]
    InvalidFieldIndex(usize),
    
    #[error("Invalid enum variant for enum {0}: {1}")]
    InvalidEnumVariant(Symbol, Symbol),
    
    #[error("Other layout error: {0}")]
    Other(String),
}

/// Handles computation of memory layouts for HIR types.
/// 
/// Uses the repc crate to calculate C-compatible layouts,
/// which are then converted to LayoutDescriptor for GC usage.
pub struct LayoutComputer {
    /// Internal store for computed layouts.
    descriptors: Vec<LayoutDescriptor>,
    
    /// Target platform for layout computation
    target: Target,
    
    /// Cache of primitive type descriptor indices
    primitive_descriptor_indices: HashMap<PrimitiveType, DescriptorIndex>,
    
    /// Cache of ADT symbol to descriptor index
    adt_descriptor_indices: HashMap<Symbol, DescriptorIndex>,
    
    /// Cache of tuple layouts (fields -> descriptor index)
    tuple_descriptor_indices: HashMap<Vec<HirType>, DescriptorIndex>,
    
    /// Cache of array layouts ((elem_type, size) -> descriptor index)
    array_descriptor_indices: HashMap<(HirType, usize), DescriptorIndex>,
    
    /// Descriptor index for Handle type, used for GC handles and function pointers
    handle_descriptor_index: Option<DescriptorIndex>,
    
    /// Used to detect recursion in type definitions
    processing_layout: HashSet<Symbol>,
    
    /// Map of struct definitions by symbol (owned)
    struct_defs: HashMap<Symbol, HirStructDef>,
    
    /// Map of enum definitions by symbol (owned)
    enum_defs: HashMap<Symbol, HirEnumDef>,
}

impl LayoutComputer {
    /// Creates a new LayoutComputer.
    pub fn new(
        struct_defs: HashMap<Symbol, HirStructDef>,
        enum_defs: HashMap<Symbol, HirEnumDef>,
    ) -> Self {
        let mut descriptors = Vec::new();
        // Add Handle descriptor at index 0
        let handle_idx = descriptors.len();
        descriptors.push(LayoutDescriptor::Handle);

        LayoutComputer {
            descriptors,
            target: repc::HOST_TARGET.expect("Host target should be available"),
            handle_descriptor_index: Some(handle_idx),
            primitive_descriptor_indices: HashMap::new(),
            adt_descriptor_indices: HashMap::new(),
            tuple_descriptor_indices: HashMap::new(),
            array_descriptor_indices: HashMap::new(),
            processing_layout: HashSet::new(),
            struct_defs,
            enum_defs,
        }
    }
    
    /// Gets or computes the descriptor index for a HIR type.
    /// 
    /// First checks the caches, then computes the layout if not found.
    /// Returns a Result with the descriptor index or an error.
    pub fn get_or_create_descriptor_index(&mut self, hir_type: &HirType) -> Result<DescriptorIndex, LayoutError> {
        match hir_type {
            HirType::Primitive(prim_type) => {
                if let Some(idx) = self.primitive_descriptor_indices.get(prim_type) {
                    return Ok(*idx);
                }
                // Handle String/Handle case directly
                if *prim_type == PrimitiveType::String {
                    // String uses Handle layout
                    return self.handle_descriptor_index.ok_or_else(|| LayoutError::Other("Handle descriptor index not set for String".to_string()));
                }
                let descriptor = self.compute_primitive_layout(*prim_type)?;
                let idx = self.add_descriptor(descriptor);
                self.primitive_descriptor_indices.insert(*prim_type, idx);
                Ok(idx)
            }
            
            HirType::Adt(symbol) => {
                if let Some(idx) = self.adt_descriptor_indices.get(symbol) {
                    return Ok(*idx);
                }
                if self.processing_layout.contains(symbol) {
                    // Recursion: Return handle index
                    return self.handle_descriptor_index.ok_or_else(|| LayoutError::Other("Handle descriptor index not set for recursive ADT".to_string()));
                }
                self.processing_layout.insert(*symbol);
                let result = if let Some(struct_def) = self.struct_defs.get(symbol).cloned() { 
                    self.compute_struct_layout(&struct_def) 
                } else if let Some(enum_def) = self.enum_defs.get(symbol).cloned() { 
                    self.compute_enum_layout(&enum_def)
                } else {
                    Err(LayoutError::UnknownAdt(*symbol))
                };
                match result {
                    Ok(descriptor) => {
                        let idx = self.add_descriptor(descriptor);
                        self.adt_descriptor_indices.insert(*symbol, idx);
                        self.processing_layout.remove(symbol); 
                        Ok(idx)
                    }
                    Err(e) => {
                        self.processing_layout.remove(symbol);
                        Err(e)
                    }
                }
            }
            
            HirType::Tuple(elements) => {
                if elements.is_empty() {
                    return self.get_or_create_descriptor_index(&HirType::Primitive(PrimitiveType::Unit));
                }
                if let Some(idx) = self.tuple_descriptor_indices.get(elements) {
                    return Ok(*idx);
                }
                let descriptor = self.compute_tuple_layout(elements)?;
                let idx = self.add_descriptor(descriptor);
                self.tuple_descriptor_indices.insert(elements.clone(), idx);
                Ok(idx)
            }
            
            // Handle fixed-size and dynamic arrays
            HirType::Array(elem_ty, size_opt) => match size_opt {
                Some(size) => { // Fixed-size array
                   let key = (elem_ty.as_ref().clone(), *size);
                   if let Some(idx) = self.array_descriptor_indices.get(&key) {
                       return Ok(*idx);
                   }
                   let descriptor = self.compute_array_layout(elem_ty, *size)?;
                   let idx = self.add_descriptor(descriptor);
                   self.array_descriptor_indices.insert(key, idx);
                   Ok(idx)
                }
                None => { // Dynamic array uses Handle layout
                   self.handle_descriptor_index.ok_or_else(|| LayoutError::Other("Handle descriptor index not set for dynamic Array type".to_string()))
                }
            }

            HirType::FunctionPointer(..) => {
                // Function pointers are represented as handles
                self.handle_descriptor_index.ok_or_else(|| LayoutError::Other("Handle descriptor index not set for FunctionPointer".to_string()))
            }
            
            HirType::Never => {
                // Never type is zero-sized, similar to unit
                self.get_or_create_descriptor_index(&HirType::Primitive(PrimitiveType::Unit))
            }
        }
    }
    
    /// Adds a descriptor to the store and returns its index.
    fn add_descriptor(&mut self, descriptor: LayoutDescriptor) -> DescriptorIndex {
        let index = self.descriptors.len();
        self.descriptors.push(descriptor);
        index
    }
    
    /// Computes layout for a primitive type.
    fn compute_primitive_layout(&mut self, prim_type: PrimitiveType) -> Result<LayoutDescriptor, LayoutError> {
        match prim_type {
            PrimitiveType::I8 | PrimitiveType::U8 => {
                Ok(LayoutDescriptor::Primitive { size_bytes: 1, align_bytes: 1 })
            }
            PrimitiveType::I16 | PrimitiveType::U16 => {
                Ok(LayoutDescriptor::Primitive { size_bytes: 2, align_bytes: 2 })
            }
            PrimitiveType::I32 | PrimitiveType::U32 | PrimitiveType::F32 => {
                Ok(LayoutDescriptor::Primitive { size_bytes: 4, align_bytes: 4 })
            }
            PrimitiveType::I64 | PrimitiveType::U64 | PrimitiveType::F64 => {
                Ok(LayoutDescriptor::Primitive { size_bytes: 8, align_bytes: 8 })
            }
            PrimitiveType::I128 | PrimitiveType::U128 => {
                 // Use repc to get reliable size/alignment for i128
                 let repc_type = self.hir_type_to_repc_type(&HirType::Primitive(prim_type))?;
                 let layout = repc::compute_layout(self.target, &repc_type).map_err(LayoutError::RepcError)?;
                 Ok(LayoutDescriptor::Primitive {
                     size_bytes: (layout.layout.size_bits / 8) as usize,
                     align_bytes: (layout.layout.required_alignment_bits / 8) as usize,
                 })
            }
            PrimitiveType::Bool => {
                Ok(LayoutDescriptor::Primitive { size_bytes: 1, align_bytes: 1 })
            }
            PrimitiveType::Char => {
                // Rust char is 4 bytes
                Ok(LayoutDescriptor::Primitive { size_bytes: 4, align_bytes: 4 })
            }
            PrimitiveType::Unit => {
                // Unit is zero-sized
                Ok(LayoutDescriptor::Primitive { size_bytes: 0, align_bytes: 1 })
            }
            PrimitiveType::String => {
                 Err(LayoutError::Other("String primitive should be handled as Handle directly".to_string()))
            }
        }
    }
    
    // compute_struct_layout, compute_enum_layout, compute_tuple_layout, compute_array_layout 
    // are defined in their respective modules (adt.rs, array.rs) and made pub(crate)

    // Define hir_type_to_repc_type here, consolidating the logic from adt.rs/array.rs
    pub(crate) fn hir_type_to_repc_type(&mut self, hir_type: &HirType) -> Result<Type<()>, LayoutError> { 
        match hir_type {
            HirType::Adt(_) => {
                // ADTs are represented as handles/pointers in the simplified repc type system.
                Ok(Type {
                    layout: (),
                    annotations: vec![],
                    variant: TypeVariant::Builtin(BuiltinType::Pointer),
                })
            }
            HirType::Primitive(prim_type) => {
                let builtin_type = match prim_type {
                    PrimitiveType::I8 => BuiltinType::Char,
                    PrimitiveType::U8 => BuiltinType::UnsignedChar,
                    PrimitiveType::I16 => BuiltinType::Short,
                    PrimitiveType::U16 => BuiltinType::UnsignedShort,
                    PrimitiveType::I32 => BuiltinType::Int,
                    PrimitiveType::U32 => BuiltinType::UnsignedInt,
                    PrimitiveType::I64 => BuiltinType::LongLong,
                    PrimitiveType::U64 => BuiltinType::UnsignedLongLong,
                    PrimitiveType::I128 => {
                        // Handle 128-bit integers as structs with two 64-bit parts
                        return Ok(Type {
                            layout: (),
                            annotations: vec![],
                            variant: TypeVariant::Record(Record {
                                kind: RecordKind::Struct,
                                fields: vec![
                                    RecordField {
                                        layout: None,
                                        annotations: vec![],
                                        named: true,
                                        bit_width: None,
                                        ty: Type {
                                            layout: (),
                                            annotations: vec![],
                                            variant: TypeVariant::Builtin(BuiltinType::UnsignedLongLong),
                                        },
                                    },
                                    RecordField {
                                        layout: None,
                                        annotations: vec![],
                                        named: true,
                                        bit_width: None,
                                        ty: Type {
                                            layout: (),
                                            annotations: vec![],
                                            variant: TypeVariant::Builtin(BuiltinType::UnsignedLongLong),
                                        },
                                    },
                                ],
                            }),
                        });
                    }
                    PrimitiveType::U128 => {
                        // Handle 128-bit integers as structs with two 64-bit parts
                        return Ok(Type {
                            layout: (),
                            annotations: vec![],
                            variant: TypeVariant::Record(Record {
                                kind: RecordKind::Struct,
                                fields: vec![
                                    RecordField {
                                        layout: None,
                                        annotations: vec![],
                                        named: true,
                                        bit_width: None,
                                        ty: Type {
                                            layout: (),
                                            annotations: vec![],
                                            variant: TypeVariant::Builtin(BuiltinType::UnsignedLongLong),
                                        },
                                    },
                                    RecordField {
                                        layout: None,
                                        annotations: vec![],
                                        named: true,
                                        bit_width: None,
                                        ty: Type {
                                            layout: (),
                                            annotations: vec![],
                                            variant: TypeVariant::Builtin(BuiltinType::UnsignedLongLong),
                                        },
                                    },
                                ],
                            }),
                        });
                    }
                    PrimitiveType::F32 => BuiltinType::Float,
                    PrimitiveType::F64 => BuiltinType::Double,
                    PrimitiveType::Bool => BuiltinType::UnsignedChar,
                    PrimitiveType::Char => BuiltinType::UnsignedInt,
                    PrimitiveType::Unit => {
                        // Unit is zero-sized, represent as empty struct
                        return Ok(Type {
                            layout: (),
                            annotations: vec![],
                            variant: TypeVariant::Record(Record {
                                kind: RecordKind::Struct,
                                fields: vec![],
                            }),
                        });
                    }
                    PrimitiveType::String => {
                        // String is a handle, represent as pointer
                        return Ok(Type {
                            layout: (),
                            annotations: vec![],
                            variant: TypeVariant::Builtin(BuiltinType::Pointer),
                        });
                    }
                };
                
                Ok(Type {
                    layout: (),
                    annotations: vec![],
                    variant: TypeVariant::Builtin(builtin_type),
                })
            }
             HirType::Tuple(elements) => {
                if elements.is_empty() {
                    // Empty tuple is like unit - empty struct
                    return Ok(Type {
                        layout: (),
                        annotations: vec![],
                        variant: TypeVariant::Record(Record {
                            kind: RecordKind::Struct,
                            fields: vec![],
                        }),
                    });
                }
                
                // Create a struct with fields for each tuple element
                let mut fields = Vec::with_capacity(elements.len());
                
                for element_type in elements {
                    // Use recursive call to handle nested types
                    let repc_type = self.hir_type_to_repc_type(element_type)?;
                    
                    fields.push(RecordField {
                        layout: None,
                        annotations: vec![],
                        named: true,
                        bit_width: None,
                        ty: repc_type,
                    });
                }
                
                Ok(Type {
                    layout: (),
                    annotations: vec![],
                    variant: TypeVariant::Record(Record {
                        kind: RecordKind::Struct,
                        fields,
                    }),
                })
            }
            
            HirType::Array(elem_ty, size_opt) => match size_opt {
                Some(size) => { // Fixed-size array
                    let repc_elem_ty = self.hir_type_to_repc_type(elem_ty)?;
                    Ok(Type {
                        layout: (),
                        annotations: vec![],
                        variant: TypeVariant::Array(Array {
                            element_type: Box::new(repc_elem_ty),
                            num_elements: Some(*size as u64),
                        }),
                    })
                }
                None => { // Dynamic array uses Handle layout
                    Ok(Type {
                        layout: (),
                        annotations: vec![],
                        variant: TypeVariant::Builtin(BuiltinType::Pointer),
                    })
                }
            }
            
            HirType::FunctionPointer(..) => {
                // Function pointers are represented as void* (like handles)
                Ok(Type {
                    layout: (),
                    annotations: vec![],
                    variant: TypeVariant::Builtin(BuiltinType::Pointer),
                })
            }
            HirType::Never => {
                 // Never type is zero-sized, like unit
                 Ok(Type {
                     layout: (),
                     annotations: vec![],
                     variant: TypeVariant::Record(Record {
                         kind: RecordKind::Struct,
                         fields: vec![],
                     }),
                 })
             }
        }
    }

    /// Finalizes the layout computation and returns the descriptor store and ADT map.
    pub fn finalize(self) -> (
        Vec<LayoutDescriptor>, 
        HashMap<Symbol, DescriptorIndex>, 
        HashMap<PrimitiveType, DescriptorIndex>,
        HashMap<Vec<HirType>, DescriptorIndex>,
        HashMap<(HirType, usize), DescriptorIndex> // Restore array_indices map
    ) {
        (
            self.descriptors,
            self.adt_descriptor_indices,
            self.primitive_descriptor_indices,
            self.tuple_descriptor_indices,
            self.array_descriptor_indices, // Restore
        )
    }
}

/* // TODO: Uncomment and fix this function once RepCType is available/imported
/// Helper to convert HIR type to RepCType for code generation backend.
/// Requires a layout computer to resolve ADT/Tuple/Array layouts.
// Assuming RepCType and hir_primitive_to_repc_type are defined elsewhere and imported
// use crate::repc::{RepCType, hir_primitive_to_repc_type}; // Example import
pub fn hir_type_to_repc_type(
    hir_type: &HirType,
    layout_computer: &mut LayoutComputer,
) -> Result<RepCType, LayoutError> { // Assuming RepCType is the correct return type
    match hir_type {
        HirType::Primitive(prim) => Ok(hir_primitive_to_repc_type(prim)), // Assuming this function exists
        HirType::Adt(symbol) => {
            let _desc_idx = layout_computer.get_or_create_descriptor_index(hir_type)?;
            Ok(RepCType::Struct(*symbol)) // Assuming RepCType::Struct exists
        },
        HirType::Tuple(elements) => {
            let field_types = elements
                .iter()
                .map(|t| hir_type_to_repc_type(t, layout_computer))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(RepCType::Tuple(field_types)) // Assuming RepCType::Tuple exists
        }
        HirType::Array(elem_ty, size_opt) => match size_opt {
            Some(size) => { 
                let repc_elem_ty = hir_type_to_repc_type(elem_ty, layout_computer)?;
                Ok(RepCType::Array { element_type: Box::new(repc_elem_ty), size: *size }) // Assuming RepCType::Array exists
            }
            None => { 
                Ok(RepCType::Handle) // Assuming RepCType::Handle exists
            }
        }
        HirType::FunctionPointer(..) => Ok(RepCType::Handle), 
        HirType::Never => Ok(RepCType::Void), // Assuming RepCType::Void exists
    }
}
*/

// If hir_primitive_to_repc_type was part of this file originally,
// ensure it's handled correctly (e.g., also commented or moved elsewhere).
// pub fn hir_primitive_to_repc_type(prim: &PrimitiveType) -> RepCType { ... }