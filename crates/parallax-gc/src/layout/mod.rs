use crate::layout::descriptor::{DescriptorIndex, LayoutDescriptor};
use parallax_hir::hir::{HirType, HirStructDef, HirEnumDef};
use parallax_hir::{PrimitiveType, Symbol};
use repc::layout::{Type, TypeVariant, BuiltinType, Record, RecordField, RecordKind, Annotation, Array};
use repc::Target;
use std::collections::{HashMap, HashSet};
use thiserror::Error;

// Make descriptor and helpers public within the layout module
pub mod adt;
pub mod array;
pub mod helpers;
pub mod tuple;
pub mod descriptor;

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
pub struct LayoutComputer<'a> {
    /// Reference to the descriptor store, used to store computed layouts
    descriptors: &'a mut Vec<LayoutDescriptor>,
    
    /// Target platform for layout computation
    target: Target,
    
    /// Descriptor index for Handle type, used for GC handles
    handle_descriptor_index: DescriptorIndex,
    
    /// Cache of primitive type descriptor indices
    primitive_descriptor_indices: HashMap<PrimitiveType, DescriptorIndex>,
    
    /// Cache of ADT symbol to descriptor index
    adt_descriptor_indices: HashMap<Symbol, DescriptorIndex>,
    
    /// Cache of tuple layouts (fields -> descriptor index)
    tuple_descriptor_indices: HashMap<Vec<HirType>, DescriptorIndex>,
    
    /// Cache of array layouts ((element_type, count) -> descriptor index)
    array_descriptor_indices: HashMap<(HirType, usize), DescriptorIndex>,
    
    /// Used to detect recursion in type definitions
    processing_layout: HashSet<Symbol>,
    
    /// Map of struct definitions by symbol
    struct_defs: HashMap<Symbol, HirStructDef>,
    
    /// Map of enum definitions by symbol
    enum_defs: HashMap<Symbol, HirEnumDef>,
}

impl<'a> LayoutComputer<'a> {
    /// Creates a new LayoutComputer with the given descriptor store and handle index.
    pub fn new(
        descriptors: &'a mut Vec<LayoutDescriptor>, 
        handle_descriptor_index: DescriptorIndex,
        struct_defs: HashMap<Symbol, HirStructDef>,
        enum_defs: HashMap<Symbol, HirEnumDef>,
    ) -> Self {
        LayoutComputer {
            descriptors,
            target: repc::HOST_TARGET.expect("Host target should be available"),
            handle_descriptor_index,
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
        // Check the appropriate cache based on type variant
        match hir_type {
            HirType::Primitive(prim_type) => {
                if let Some(idx) = self.primitive_descriptor_indices.get(prim_type) {
                    return Ok(*idx);
                }
                let descriptor = self.compute_primitive_layout(*prim_type)?;
                let idx = self.add_descriptor(descriptor);
                self.primitive_descriptor_indices.insert(*prim_type, idx);
                Ok(idx)
            }
            
            HirType::Adt(symbol) => {
                // Check cache first
                if let Some(idx) = self.adt_descriptor_indices.get(symbol) {
                    return Ok(*idx);
                }
                
                // Check for recursion
                if self.processing_layout.contains(symbol) {
                    return Err(LayoutError::TypeRecursion(*symbol));
                }
                
                // Mark as processing to detect cycles
                self.processing_layout.insert(*symbol);
                
                // Look up definition and compute layout
                // We clone the definition to avoid borrow issues if compute_struct/enum_layout needs mutable self later.
                // Requires HirStructDef/HirEnumDef to be Clone.
                let result = if let Some(struct_def) = self.struct_defs.get(symbol).cloned() { 
                    self.compute_struct_layout(&struct_def) 
                } else if let Some(enum_def) = self.enum_defs.get(symbol).cloned() { 
                    self.compute_enum_layout(&enum_def)
                } else {
                    Err(LayoutError::UnknownAdt(*symbol))
                };

                // Handle the result *after* computation finishes and recursive calls return
                match result {
                    Ok(descriptor) => {
                let idx = self.add_descriptor(descriptor);
                self.adt_descriptor_indices.insert(*symbol, idx);
                        self.processing_layout.remove(symbol); // Remove *after* success
                        Ok(idx)
                    }
                    Err(e) => {
                        // Important: Remove from processing set even on error to allow future attempts
                self.processing_layout.remove(symbol);
                        Err(e)
                    }
                }
            }
            
            HirType::Tuple(elements) => {
                if let Some(idx) = self.tuple_descriptor_indices.get(elements) {
                    return Ok(*idx);
                }
                let descriptor = self.compute_tuple_layout(elements)?;
                let idx = self.add_descriptor(descriptor);
                self.tuple_descriptor_indices.insert(elements.clone(), idx);
                Ok(idx)
            }
            
            HirType::Array(element_type, count) => {
                let key = ((**element_type).clone(), *count);
                if let Some(idx) = self.array_descriptor_indices.get(&key) {
                    return Ok(*idx);
                }
                let descriptor = self.compute_array_layout(element_type, *count)?;
                let idx = self.add_descriptor(descriptor);
                self.array_descriptor_indices.insert(key, idx);
                Ok(idx)
            }
            
            HirType::FunctionPointer(..) => {
                // Function pointers are represented as handles
                Ok(self.handle_descriptor_index)
            }
            
            HirType::Never => {
                // Never type is zero-sized, similar to unit
                let descriptor = LayoutDescriptor::Primitive { 
                    size_bytes: 0, 
                    align_bytes: 1 
                };
                let idx = self.add_descriptor(descriptor);
                Ok(idx)
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
                Ok(LayoutDescriptor::Primitive { size_bytes: 16, align_bytes: 16 })
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
                // Strings are GC handles
                Ok(LayoutDescriptor::Handle)
            }
        }
    }
    
    // compute_struct_layout, compute_enum_layout, compute_tuple_layout, compute_array_layout 
    // are defined in their respective modules (adt.rs, array.rs) and made pub(crate)
} 