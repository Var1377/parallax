use crate::descriptor::{LayoutDescriptor, DescriptorIndex};
use crate::LayoutError;
use parallax_hir::hir::HirType;
use parallax_hir::Symbol;
use cranelift_codegen::ir::Type as ClifType;
use cranelift_codegen::ir::types;

/// Gets the size in bytes for a descriptor.
pub fn get_size_bytes(descriptor: &LayoutDescriptor) -> usize {
    match descriptor {
        LayoutDescriptor::Primitive { size_bytes, .. } => *size_bytes,
        LayoutDescriptor::Handle => std::mem::size_of::<*mut u8>(), // Handle is a pointer
        LayoutDescriptor::Struct { size_bytes, .. } => *size_bytes,
        LayoutDescriptor::Enum { size_bytes, .. } => *size_bytes,
        LayoutDescriptor::Array { size_bytes, .. } => *size_bytes,
    }
}

/// Gets the alignment in bytes for a descriptor.
pub fn get_alignment_bytes(descriptor: &LayoutDescriptor) -> usize {
    match descriptor {
        LayoutDescriptor::Primitive { align_bytes, .. } => *align_bytes,
        LayoutDescriptor::Handle => std::mem::align_of::<*mut u8>(), // Handle alignment
        LayoutDescriptor::Struct { align_bytes, .. } => *align_bytes,
        LayoutDescriptor::Enum { align_bytes, .. } => *align_bytes,
        LayoutDescriptor::Array { align_bytes, .. } => *align_bytes,
    }
}

/// Gets the byte offset and descriptor index for a struct field.
pub fn get_struct_field_offset_bytes(
    struct_descriptor: &LayoutDescriptor,
    field_index: usize,
) -> Result<usize, LayoutError> {
    match struct_descriptor {
        LayoutDescriptor::Struct { fields, .. } => {
            if field_index < fields.len() {
                Ok(fields[field_index].0)
            } else {
                Err(LayoutError::InvalidFieldIndex(field_index))
            }
        }
        _ => Err(LayoutError::Other(format!(
            "Expected Struct descriptor, got {:?}",
            struct_descriptor
        ))),
    }
}

/// Gets the descriptor index for a struct field.
pub fn get_struct_field_descriptor_index(
    struct_descriptor: &LayoutDescriptor,
    field_index: usize,
) -> Result<DescriptorIndex, LayoutError> {
    match struct_descriptor {
        LayoutDescriptor::Struct { fields, .. } => {
            if field_index < fields.len() {
                Ok(fields[field_index].1)
            } else {
                Err(LayoutError::InvalidFieldIndex(field_index))
            }
        }
        _ => Err(LayoutError::Other(format!(
            "Expected Struct descriptor, got {:?}",
            struct_descriptor
        ))),
    }
}

/// Gets the discriminant offset and size for an enum.
pub fn get_enum_discriminant_info(
    enum_descriptor: &LayoutDescriptor,
) -> Result<(usize, usize), LayoutError> {
    match enum_descriptor {
        LayoutDescriptor::Enum {
            discriminant_offset,
            discriminant_size_bytes,
            ..
        } => Ok((*discriminant_offset, *discriminant_size_bytes)),
        _ => Err(LayoutError::Other(format!(
            "Expected Enum descriptor, got {:?}",
            enum_descriptor
        ))),
    }
}

/// Gets the Cranelift type for an enum discriminant.
pub fn get_discriminant_cl_type(
    enum_descriptor: &LayoutDescriptor
) -> Result<ClifType, LayoutError> {
    match enum_descriptor {
        LayoutDescriptor::Enum { discriminant_size_bytes, .. } => {
            let cl_type = match discriminant_size_bytes {
                1 => types::I8,
                2 => types::I16,
                4 => types::I32,
                8 => types::I64,
                _ => return Err(LayoutError::Other(format!(
                    "Unsupported discriminant size: {} bytes",
                    discriminant_size_bytes
                ))),
            };
            Ok(cl_type)
        },
        _ => Err(LayoutError::Other(format!(
            "Expected Enum descriptor, got {:?}",
            enum_descriptor
        ))),
    }
}

/// Gets the payload offset and descriptor index for an enum variant.
pub fn get_enum_variant_info(
    enum_descriptor: &LayoutDescriptor,
    discriminant_value: u64,
) -> Result<(usize, DescriptorIndex), LayoutError> {
    match enum_descriptor {
        LayoutDescriptor::Enum { variants, .. } => {
            for &(discr, payload_offset, desc_idx) in variants.iter() {
                if discr == discriminant_value {
                    return Ok((payload_offset, desc_idx));
                }
            }
            Err(LayoutError::Other(format!(
                "Invalid discriminant value: {}",
                discriminant_value
            )))
        }
        _ => Err(LayoutError::Other(format!(
            "Expected Enum descriptor, got {:?}",
            enum_descriptor
        ))),
    }
}

/// Gets the element descriptor index and count for an array.
pub fn get_array_info(
    array_descriptor: &LayoutDescriptor,
) -> Result<(DescriptorIndex, usize), LayoutError> {
    match array_descriptor {
        LayoutDescriptor::Array {
            element_descriptor_index,
            count,
            ..
        } => Ok((*element_descriptor_index, *count)),
        _ => Err(LayoutError::Other(format!(
            "Expected Array descriptor, got {:?}",
            array_descriptor
        ))),
    }
}

/// Determines if a type needs heap allocation.
/// This is used to decide whether to allocate on stack or heap.
pub fn type_needs_heap_allocation(
    hir_type: &HirType,
    descriptors: &[LayoutDescriptor],
    descriptor_indices: &mut dyn FnMut(&HirType) -> Result<DescriptorIndex, LayoutError>,
    visiting: &mut std::collections::HashSet<Symbol>
) -> bool {
    match hir_type {
        // Primitive types don't need heap allocation unless they're strings
        HirType::Primitive(prim) => {
            use parallax_hir::PrimitiveType;
            matches!(prim, PrimitiveType::String)
        }
        
        // ADTs may need heap allocation if they contain handles or are large
        HirType::Adt(symbol) => {
            // Prevent infinite recursion through mutually recursive types
            if !visiting.insert(*symbol) {
                // Already being visited, conservatively say it needs heap allocation
                return true;
            }
            
            // Get the descriptor index for this ADT
            let desc_idx = match descriptor_indices(hir_type) {
                Ok(idx) => idx,
                Err(_) => {
                    visiting.remove(symbol);
                    return true; // Conservative: assume needs heap allocation if error
                }
            };
            
            // Get the descriptor
            let descriptor = match descriptors.get(desc_idx) {
                Some(desc) => desc,
                None => {
                    visiting.remove(symbol);
                    return true; // Conservative
                }
            };
            
            // Check if this descriptor contains handles using the new stateless helper
            let result = layout_contains_handles(descriptor, descriptors);
            
            // Clean up
            visiting.remove(symbol);
            result
        }
        
        // Tuples need heap allocation if they contain handles or are large
        HirType::Tuple(elements) => {
            // Empty tuple is just unit, doesn't need heap allocation
            if elements.is_empty() {
                return false;
            }
            
            // Check if any element needs heap allocation
            for element in elements {
                if type_needs_heap_allocation(element, descriptors, descriptor_indices, visiting) {
                    return true;
                }
            }
            
            // Large tuples may need heap allocation regardless
            elements.len() > 3 // Arbitrary threshold
        }
        
        HirType::Array(element_type, size_opt) => {
            match size_opt {
                None => true, // Dynamic arrays always need heap allocation
                Some(count) => { // Fixed-size array
                    // Check if element type needs heap allocation
                    if type_needs_heap_allocation(element_type, descriptors, descriptor_indices, visiting) {
                        return true;
                    }
                    // Large arrays may need heap allocation regardless
                    *count > 10 // Arbitrary threshold
                }
            }
        }
        
        // Function pointers typically need heap allocation for closures
        HirType::FunctionPointer(_, _) => true,
        
        // Never type doesn't need heap allocation (it doesn't exist at runtime)
        HirType::Never => false,
    }
}

/// Helper function to check if a descriptor contains handles.
/// This is stateless and works purely on the descriptor structure.
pub fn layout_contains_handles(descriptor: &LayoutDescriptor, all_descriptors: &[LayoutDescriptor]) -> bool {
    // Use a stack for iterative traversal to avoid Rust recursion limits on deep types
    let mut stack = vec![descriptor];
    // Use a set to track visited descriptor indices to prevent infinite loops in recursive descriptors
    let mut visited_indices = std::collections::HashSet::new();

    while let Some(current_descriptor) = stack.pop() {
        match current_descriptor {
            LayoutDescriptor::Primitive { .. } => { /* No handles */ }
            LayoutDescriptor::Handle => return true, // Found a handle!
            LayoutDescriptor::Struct { fields, .. } => {
                for &(_, field_desc_idx) in fields.iter() {
                    if visited_indices.insert(field_desc_idx) { // Only process if not already visited
                        if let Some(field_desc) = all_descriptors.get(field_desc_idx) {
                            stack.push(field_desc);
                        }
                        // else: Missing descriptor, cannot contain handles, ignore.
                    }
                }
            }
            LayoutDescriptor::Enum { variants, .. } => {
                for &(_, _, variant_desc_idx) in variants.iter() {
                    if visited_indices.insert(variant_desc_idx) { // Only process if not already visited
                        if let Some(variant_desc) = all_descriptors.get(variant_desc_idx) {
                            stack.push(variant_desc);
                        }
                        // else: Missing descriptor, ignore.
                    }
                }
            }
            LayoutDescriptor::Array { element_descriptor_index, count, .. } => {
                if *count > 0 { // Only check non-empty arrays
                    if visited_indices.insert(*element_descriptor_index) { // Only process if not already visited
                        if let Some(element_desc) = all_descriptors.get(*element_descriptor_index) {
                            stack.push(element_desc);
                        }
                        // else: Missing descriptor, ignore.
                    }
                }
            }
        }
    }

    false // No handles found after checking all reachable descriptors
} 