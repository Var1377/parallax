use crate::descriptor::{DescriptorIndex, LayoutDescriptor};
use crate::{LayoutComputer, LayoutError};
use parallax_hir::hir::HirType;
use repc::layout::{Type, TypeVariant, Record, RecordField, RecordKind};
use crate::helpers::layout_contains_handles;

impl LayoutComputer {
    /// Computes layout for a tuple type.
    pub(crate) fn compute_tuple_layout(&mut self, elements: &[HirType]) -> Result<LayoutDescriptor, LayoutError> {
        if elements.is_empty() {
            // Empty tuple is like unit - zero-sized
            // Delegate to get_or_create to handle caching for Unit
            return self.get_or_create_descriptor_index(&HirType::Primitive(parallax_hir::PrimitiveType::Unit))
                       .and_then(|idx| self.descriptors.get(idx).cloned().ok_or(LayoutError::MissingDescriptor(idx)));
            // return Ok(LayoutDescriptor::Primitive { 
            //     size_bytes: 0, 
            //     align_bytes: 1 
            // });
        }
        
        // Get descriptor indices for all elements
        let mut element_indices = Vec::with_capacity(elements.len());
        for element_type in elements {
            // Use self.get_or_create_descriptor_index
            let descriptor_index = self.get_or_create_descriptor_index(element_type)?;
            element_indices.push(descriptor_index);
        }
        
        // Create the repc struct type for the tuple
        let mut fields = Vec::with_capacity(elements.len());
        
        for element_type in elements {
            // Convert HirType to repc type
            // Pass &mut self
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
            // Use immutable access to self.descriptors
            let field_descriptor = self.descriptors.get(*field_desc_idx)
                .ok_or(LayoutError::MissingDescriptor(*field_desc_idx))?;
            // Pass self.descriptors immutably
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
}
