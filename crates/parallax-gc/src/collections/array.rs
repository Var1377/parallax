use rsgc::prelude::*;
use crate::layout::descriptor::{DescriptorIndex, LayoutDescriptor};
use crate::{GLOBAL_DESCRIPTOR_STORE, tracer};
use memoffset::offset_of;
use std::mem;
use std::ptr;
use rsgc::system::object::VarSize; // Import VarSize

#[repr(C)]
#[derive(Debug, Copy, Clone)] // Header can be copied
pub struct GcRawArrayHeader {
    /// Number of elements currently stored.
    pub len: u32,
    /// Number of elements the allocated buffer can hold.
    pub capacity: u32,
    /// Index into the DescriptorStore for the layout of each element.
    pub element_descriptor_index: DescriptorIndex,
}

/// A variable-sized array managed by the GC, holding raw byte data
/// interpreted according to the element descriptor index in the header.
#[repr(C)]
pub struct GcRawArray {
    /// Header containing metadata (length, capacity, element type).
    pub header: GcRawArrayHeader,
    /// Raw byte data for the elements. Actual layout depends on the element descriptor.
    pub data: [u8; 0],
}

impl GcRawArray {
    /// Calculates the required byte size for the data part of the array
    /// given the element layout and capacity.
    /// # Safety
    /// `element_descriptor` must be valid.
    pub(crate) unsafe fn calculate_data_byte_size(element_descriptor: &LayoutDescriptor, capacity: usize) -> usize {
        let element_size = match element_descriptor {
            LayoutDescriptor::Primitive { size_bytes, .. } => *size_bytes,
            LayoutDescriptor::Handle => mem::size_of::<*mut u8>(),
            LayoutDescriptor::Struct { size_bytes, .. } => *size_bytes,
            LayoutDescriptor::Enum { size_bytes, .. } => *size_bytes,
            LayoutDescriptor::Array { size_bytes, .. } => *size_bytes,
        };
        element_size * capacity
    }

    /// Calculates the alignment required for the data elements.
    /// # Safety
    /// `element_descriptor` must be valid.
     pub(crate) unsafe fn calculate_element_alignment(element_descriptor: &LayoutDescriptor) -> usize {
         match element_descriptor {
            LayoutDescriptor::Primitive { align_bytes, .. } => *align_bytes,
            LayoutDescriptor::Handle => mem::align_of::<*mut u8>(),
            LayoutDescriptor::Struct { align_bytes, .. } => *align_bytes,
            LayoutDescriptor::Enum { align_bytes, .. } => *align_bytes,
            LayoutDescriptor::Array { align_bytes, .. } => *align_bytes,
        }
    }
}

// SAFETY: Object implementation traces elements based on the header's descriptor index.
unsafe impl Object for GcRawArray {
    fn trace(&self, visitor: &mut dyn Visitor) {
        // SAFETY: Accessing static mut requires unsafe block.
        // Assumes GC synchronization ensures safe access during tracing.
        let descriptor_store_ptr = unsafe { GLOBAL_DESCRIPTOR_STORE };
        if descriptor_store_ptr.is_null() {
            println!("FATAL ERROR: GLOBAL_DESCRIPTOR_STORE is null during GcRawArray trace!");
            return;
        }
        // SAFETY: Assumes GLOBAL_DESCRIPTOR_STORE points to a valid, initialized store.
        let descriptor_store = unsafe { &*descriptor_store_ptr };

        // Get element layout
        let element_descriptor = match descriptor_store.descriptors.get(self.header.element_descriptor_index) {
            Some(desc) => desc,
            None => {
                println!(
                    "FATAL ERROR: Invalid element descriptor index {} found in GcRawArray header during trace!",
                    self.header.element_descriptor_index
                );
                return;
            }
        };

        let element_size = match element_descriptor {
            LayoutDescriptor::Primitive { size_bytes, .. } => *size_bytes,
            LayoutDescriptor::Handle => mem::size_of::<*mut u8>(),
            LayoutDescriptor::Struct { size_bytes, .. } => *size_bytes,
            LayoutDescriptor::Enum { size_bytes, .. } => *size_bytes,
            LayoutDescriptor::Array { size_bytes, .. } => *size_bytes,
        };

        if element_size == 0 { return; } // Nothing to trace for ZSTs

        let data_base_ptr = self.data.as_ptr(); // Pointer to start of data
        let len = self.header.len as usize; // Current length of array

        // Iterate through the valid elements [0, len)
        for i in 0..len {
            let element_offset = i * element_size; // Assuming packed elements
            // SAFETY: Calculation assumes i is within bounds [0, len) and elements are packed.
            let element_data_ptr = unsafe { data_base_ptr.add(element_offset) };

            // Trace the element using its descriptor
            // SAFETY: Relies on tracer::trace_recursive safety contract.
            unsafe { tracer::trace_recursive(element_data_ptr, element_descriptor, descriptor_store, visitor) };
        }
    }
}

// SAFETY: Allocation implementation describes the layout for rsgc.
unsafe impl Allocation for GcRawArray {
    // --- Using rsgc v1.1.0 consts --- 
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = 1; // We allocate raw bytes
    const SIZE: usize = mem::size_of::<GcRawArrayHeader>();
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(GcRawArray, header) + offset_of!(GcRawArrayHeader, len);
    const VARSIZE_OFFSETOF_CAPACITY: usize = offset_of!(GcRawArray, header) + offset_of!(GcRawArrayHeader, capacity);
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(GcRawArray, data);
    const NO_HEAP_PTRS: bool = true; // Header itself doesn't have pointers
    const VARSIZE_NO_HEAP_PTRS: bool = false; // Data part *might* have pointers
}