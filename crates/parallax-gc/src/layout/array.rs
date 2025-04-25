use crate::layout::descriptor::{DescriptorIndex, LayoutDescriptor};
use crate::layout::{LayoutComputer, LayoutError};
use parallax_hir::hir::HirType;
use parallax_hir::Symbol;
use repc::layout::{Type, TypeVariant, Record, RecordField, RecordKind, Array, BuiltinType};
use crate::layout::helpers::layout_contains_handles;

impl<'a> LayoutComputer<'a> {
    /// Computes layout for a tuple type.
    pub(crate) fn compute_tuple_layout(&mut self, elements: &[HirType]) -> Result<LayoutDescriptor, LayoutError> {
        if elements.is_empty() {
            // Empty tuple is like unit - zero-sized
            return Ok(LayoutDescriptor::Primitive { 
                size_bytes: 0, 
                align_bytes: 1 
            });
        }
        
        // Get descriptor indices for all elements
        let mut element_indices = Vec::with_capacity(elements.len());
        for element_type in elements {
            let descriptor_index = self.get_or_create_descriptor_index(element_type)?;
            element_indices.push(descriptor_index);
        }
        
        // Create the repc struct type for the tuple
        let mut fields = Vec::with_capacity(elements.len());
        
        for element_type in elements {
            // Convert HirType to repc type
            let repc_type = self.hir_type_to_repc_type(element_type)?;
            
            fields.push(RecordField {
                layout: None, // Layout will be computed by repc
                annotations: vec![],
                named: true,
                bit_width: None,
                ty: repc_type,
            });
        }
        
        let tuple_type = Type {
            layout: (),
            annotations: vec![],
            variant: TypeVariant::Record(Record {
                kind: RecordKind::Struct,
                fields,
            }),
        };
        
        // Compute layout using repc
        let computed_layout = repc::compute_layout(self.target, &tuple_type)
            .map_err(LayoutError::RepcError)?;
        
        // Extract the field offsets from the computed layout
        let repc_record = match computed_layout.variant {
            TypeVariant::Record(record) => record,
            _ => return Err(LayoutError::Other("Expected record type".to_string())),
        };
        
        // Create field offset -> descriptor index mappings
        let mut fields_with_offsets = Vec::with_capacity(repc_record.fields.len());
        
        for (i, field) in repc_record.fields.iter().enumerate() {
            let field_layout = field.layout.unwrap(); // Safe after compute_layout
            let offset = field_layout.offset_bits / 8; // Convert bits to bytes
            fields_with_offsets.push((offset as usize, element_indices[i])); // Cast offset to usize
        }
        
        // Calculate handle_offsets for the tuple (represented as struct)
        let mut handle_offsets = Vec::new();
        for (offset, field_desc_idx) in &fields_with_offsets {
            let field_descriptor = self.descriptors.get(*field_desc_idx)
                .ok_or(LayoutError::MissingDescriptor(*field_desc_idx))?;
            if layout_contains_handles(field_descriptor, &self.descriptors) {
                handle_offsets.push(*offset);
            }
        }

        // Create the LayoutDescriptor::Struct (tuples are represented as structs)
        Ok(LayoutDescriptor::Struct {
            size_bytes: (computed_layout.layout.size_bits / 8) as usize, // Cast size to usize
            align_bytes: (computed_layout.layout.required_alignment_bits / 8) as usize, // Cast align to usize
            fields: fields_with_offsets.into_boxed_slice(),
            handle_offsets: handle_offsets.into_boxed_slice(), // Store handle offsets for tuple
        })
    }
    
    /// Computes layout for an array type.
    pub(crate) fn compute_array_layout(&mut self, element_type: &HirType, count: usize) -> Result<LayoutDescriptor, LayoutError> {
        // Get the descriptor index for the element type
        let element_descriptor_index = self.get_or_create_descriptor_index(element_type)?;
        
        // Convert HirType to repc type for the element
        let element_repc_type = self.hir_type_to_repc_type(element_type)?;

        // --- Compute layout for the *element* type to get its size and alignment --- 
        let element_computed_layout = repc::compute_layout(self.target, &element_repc_type)
            .map_err(|e| LayoutError::Other(format!("Failed to compute layout for array element {:?}: {}", element_type, e)))?;
        let element_size_bytes = (element_computed_layout.layout.size_bits / 8) as usize;
        let element_align_bytes = (element_computed_layout.layout.required_alignment_bits / 8) as usize;
        
        // Calculate stride (size rounded up to alignment)
        let element_stride_bytes = if element_align_bytes > 0 {
            (element_size_bytes + element_align_bytes - 1) & !(element_align_bytes - 1)
        } else {
            element_size_bytes // Should not happen for non-ZST, but handle defensively
        };
        // --- End Element Layout Computation ---
        
        // Create the repc array type
        let array_type = Type {
            layout: (),
            annotations: vec![],
            variant: TypeVariant::Array(Array {
                element_type: Box::new(element_repc_type), // Use computed element type
                num_elements: Some(count as u64), 
            }),
        };
        
        // Compute layout for the whole array type using repc
        let computed_array_layout = repc::compute_layout(self.target, &array_type)
            .map_err(LayoutError::RepcError)?;
            
        // Check if element contains handles
        let element_descriptor = self.descriptors.get(element_descriptor_index)
            .ok_or(LayoutError::MissingDescriptor(element_descriptor_index))?;
        let element_contains_handles = layout_contains_handles(element_descriptor, &self.descriptors);
        
        // Create the LayoutDescriptor::Array
        Ok(LayoutDescriptor::Array {
            size_bytes: (computed_array_layout.layout.size_bits / 8) as usize, 
            align_bytes: (computed_array_layout.layout.required_alignment_bits / 8) as usize, 
            element_descriptor_index,
            count,
            element_stride_bytes, // Store calculated stride
            element_contains_handles, // Store handle check result
        })
    }
    
    /// Extends hir_type_to_repc_type to handle Tuple, Array, and Adt types.
    pub(crate) fn hir_type_to_repc_type_extended(&mut self, hir_type: &HirType) -> Result<Type<()>, LayoutError> {
        match hir_type {
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
            
            HirType::Array(element_type, count) => {
                let element_repc_type = self.hir_type_to_repc_type(element_type)?;
                
                Ok(Type {
                    layout: (),
                    annotations: vec![],
                    variant: TypeVariant::Array(Array {
                        element_type: Box::new(element_repc_type), // Changed field name
                        num_elements: Some(*count as u64), // Changed field name and type
                    }),
                })
            }
            
            HirType::Adt(symbol) => {
                // For ADTs, we need to look up the struct or enum
                // But this gets complex due to potential recursion
                // For now, represent as an opaque pointer
                Ok(Type {
                    layout: (),
                    annotations: vec![],
                    variant: TypeVariant::Builtin(BuiltinType::Pointer), // Changed
                })
            }
            
            HirType::FunctionPointer(..) | HirType::Never => {
                // Function pointers are represented as void*
                // Never type is represented as void (empty struct for zero size)
                Ok(Type {
                    layout: (),
                    annotations: vec![],
                    variant: TypeVariant::Builtin(BuiltinType::Pointer), // Changed
                })
            }
            
            // Primitive types are handled in the base implementation
            _ => self.hir_type_to_repc_type(hir_type),
        }
    }
} 