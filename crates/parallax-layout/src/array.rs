use crate::descriptor::{DescriptorIndex, LayoutDescriptor};
use crate::{LayoutComputer, LayoutError};
use parallax_hir::hir::HirType;
use parallax_hir::Symbol;
use repc::layout::{Type, TypeVariant, Record, RecordField, RecordKind, Array, BuiltinType};
use crate::helpers::layout_contains_handles;

impl LayoutComputer {
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
} 