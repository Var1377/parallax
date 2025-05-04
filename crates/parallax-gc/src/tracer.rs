use rsgc::prelude::*;
use std::ptr;
use std::mem;
use parallax_layout::{LayoutDescriptor, DescriptorStore};

/// Recursive tracing helper.
/// # Safety
/// - `object_data_ptr` must point to the start of the data for the object being traced.
/// - `descriptor` must be the correct descriptor for the data at `object_data_ptr`.
/// - `descriptor_store` must be valid and contain the descriptor itself and any nested descriptors referenced by index.
/// - `visitor` must be valid.
pub(crate) unsafe fn trace_recursive(
    object_data_ptr: *const u8,
    descriptor: &LayoutDescriptor,
    descriptor_store: &DescriptorStore, // Accept store reference
    visitor: &mut dyn Visitor,
) {
    match descriptor {
        LayoutDescriptor::Primitive { .. } => {
            // No handles to trace
        }
        LayoutDescriptor::Handle => {
            // The object_data_ptr directly points to the handle bits.
            // Read the Handle bits (raw pointer).
            let handle_value = ptr::read(object_data_ptr as *const *mut u8);
            if !handle_value.is_null() {
                visitor.visit(handle_value);
            }
        }
        LayoutDescriptor::Struct { fields, .. } => {
            for (field_offset, field_desc_index) in fields.iter() {
                // Use the passed descriptor_store
                let field_descriptor = descriptor_store.descriptors.get(*field_desc_index)
                    .expect("Invalid descriptor index during struct trace"); // TODO: Handle error more gracefully?

                // Calculate pointer to the field's data.
                let field_data_ptr = object_data_ptr.add(*field_offset);

                // Pass the store down recursively
                trace_recursive(field_data_ptr, field_descriptor, descriptor_store, visitor);
            }
        }
        LayoutDescriptor::Enum { size_bytes: _, align_bytes: _, discriminant_offset, discriminant_size_bytes, variants } => {
            // 1. Read the discriminant value.
            let discriminant_ptr = object_data_ptr.add(*discriminant_offset);
            let discriminant_value: u64 = match *discriminant_size_bytes {
                1 => ptr::read(discriminant_ptr as *const u8) as u64,
                2 => ptr::read(discriminant_ptr as *const u16) as u64,
                4 => ptr::read(discriminant_ptr as *const u32) as u64,
                8 => ptr::read(discriminant_ptr as *const u64),
                _ => panic!("Invalid discriminant size during enum trace"), // TODO: Handle error
            };

            // 2. Find the matching variant descriptor index.
            if let Some((_val, payload_offset, variant_desc_index)) = variants.iter().find(|(v, _, _)| *v == discriminant_value) {
                // Use the passed descriptor_store
                let variant_descriptor = descriptor_store.descriptors.get(*variant_desc_index)
                    .expect("Invalid descriptor index during enum trace"); // TODO: Handle error

                // 4. Calculate pointer to the variant's payload data.
                let payload_data_ptr = object_data_ptr.add(*payload_offset);

                // Pass the store down recursively
                trace_recursive(payload_data_ptr, variant_descriptor, descriptor_store, visitor);
            } else {
                // Discriminant value doesn't match any known variant.
                // This could happen with uninitialized data or corruption. Log? Panic?
                println!("Warning: Encountered unknown discriminant value {} during enum trace.", discriminant_value);
            }
        }
        LayoutDescriptor::Array { 
            size_bytes: _, 
            align_bytes: _, 
            element_descriptor_index, 
            count, 
            element_stride_bytes: _, 
            element_contains_handles: _ 
        } => {
            // Use the passed descriptor_store
            let element_descriptor = descriptor_store.descriptors.get(*element_descriptor_index)
                .expect("Invalid descriptor index during array trace"); // TODO: Handle error

            // Determine element size from its descriptor (more robust than assuming fixed size)
            let element_size = match element_descriptor {
                 LayoutDescriptor::Primitive { size_bytes, .. } => *size_bytes,
                 LayoutDescriptor::Handle => mem::size_of::<*mut u8>(),
                 LayoutDescriptor::Struct { size_bytes, .. } => *size_bytes,
                 LayoutDescriptor::Enum { size_bytes, .. } => *size_bytes,
                 LayoutDescriptor::Array { size_bytes, .. } => *size_bytes,
                 // Cannot have an array of unknown/variable size elements directly inline?
            };
            let element_align = match element_descriptor {
                 LayoutDescriptor::Primitive { align_bytes, .. } => *align_bytes,
                 LayoutDescriptor::Handle => mem::align_of::<*mut u8>(),
                 LayoutDescriptor::Struct { align_bytes, .. } => *align_bytes,
                 LayoutDescriptor::Enum { align_bytes, .. } => *align_bytes,
                 LayoutDescriptor::Array { align_bytes, .. } => *align_bytes,
            };

            if element_size == 0 { return; } // Skip tracing ZST elements

            let mut current_offset = 0;
            for _ in 0..*count {
                 // Ensure alignment for the current element before calculating pointer
                 current_offset = (current_offset + element_align - 1) & !(element_align - 1);
                 let element_data_ptr = object_data_ptr.add(current_offset);
                 // Pass the store down recursively
                 trace_recursive(element_data_ptr, element_descriptor, descriptor_store, visitor);
                 current_offset += element_size;
            }
        }
    }
} 