#![feature(unsafe_cell_access)]
// Top-level crate comment if desired

mod tracer;
mod roots;
pub mod collections;
mod ffi;
pub mod readback;

// Core GC and internal types
// Remove unused Heap import
// use rsgc::heap::heap::Heap;
use rsgc::prelude::*;
// Remove unused HeapArguments import
use rsgc::heap::thread::Thread;
use memoffset::offset_of;
use std::ptr;
use std::cell::Cell;

// Imports from internal modules
// use crate::descriptor::{DescriptorIndex, DescriptorStore}; // Removed: Re-exported below
use crate::tracer::trace_recursive;

// Re-export collection types
pub use crate::collections::string::{StringRef, GcByteArray};
pub use crate::collections::array::GcRawArray;
pub use crate::collections::closure::{ClosureRef, CLOSURE_REF_DESCRIPTOR_INDEX};

// Re-export FFI functions
pub use crate::ffi::{
    parallax_gc_alloc, 
    parallax_alloc_object, 
    parallax_alloc_array,
    parallax_alloc_closure,
    parallax_closure_get_function,
    parallax_closure_get_environment
};
pub use crate::roots::{push_shadow_stack, pop_shadow_stack, register_global_root, unregister_global_root};
pub use crate::collections::string::{parallax_alloc_string_ref, parallax_alloc_string_from_rust_buffer};
use parallax_layout::{DescriptorStore, DescriptorIndex};

// Introduce thread-local storage for the descriptor store pointer
thread_local! {
    // Use Cell because *const is Copy
    pub static CURRENT_DESCRIPTOR_STORE: Cell<*const DescriptorStore> = Cell::new(ptr::null());
}

// Function to set the thread-local pointer (unsafe because it deals with raw pointers)
pub unsafe fn set_current_descriptor_store(store_ptr: *const DescriptorStore) {
    CURRENT_DESCRIPTOR_STORE.with(|cell| {
        cell.set(store_ptr);
    });
}

// Function to clear the thread-local pointer
pub fn clear_current_descriptor_store() {
    CURRENT_DESCRIPTOR_STORE.with(|cell| {
        cell.set(ptr::null());
    });
}

/// A generic, variable-sized object managed by the GC.
/// It holds an *index* to its layout information (`LayoutDescriptor`) in a stable
/// `DescriptorStore` and the raw data.
#[repr(C)]
pub struct GcObject {
    /// Index into the `DescriptorStore` containing the `LayoutDescriptor` for `data`.
    /// This index refers to a stable, JIT-owned store.
    // Making this pub(crate) so ffi.rs can initialize it.
    pub(crate) descriptor_index: DescriptorIndex,
    /// The raw byte data of the object. Its actual size is determined by the
    /// allocation size, which should match the size indicated by the LayoutDescriptor.
    // Making this pub(crate) so ffi.rs can initialize it.
    pub(crate) data: [u8; 0],
}

// SAFETY: Allocation implementation describes the layout for rsgc.
unsafe impl Allocation for GcObject {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = 1; // Data is bytes
    // Size of the fixed part (just the DescriptorIndex)
    const SIZE: usize = std::mem::size_of::<DescriptorIndex>();
    // Offsets for length/capacity are not used by GcObject itself.
    const VARSIZE_OFFSETOF_LENGTH: usize = usize::MAX;
    const VARSIZE_OFFSETOF_CAPACITY: usize = usize::MAX;
    // Offset of the variable data part
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(GcObject, data);
    // The fixed part (descriptor index) doesn't contain GC pointers.
    const NO_HEAP_PTRS: bool = true;
    // The variable data part *can* contain GC pointers (Handles).
    const VARSIZE_NO_HEAP_PTRS: bool = false;
}

// SAFETY: Object implementation traces Handle pointers within the data buffer
// based on the LayoutDescriptor found via the index.
unsafe impl Object for GcObject {
    fn trace(&self, visitor: &mut dyn Visitor) {
        // Get descriptor store pointer from thread-local
        let descriptor_store_ptr = CURRENT_DESCRIPTOR_STORE.with(|cell| cell.get());
        if descriptor_store_ptr.is_null() {
            // This might happen if GC tracing occurs on a thread where the store wasn't set.
            // Log an error and skip tracing this object.
            // TODO: Investigate if GC worker threads need the store pointer propagated.
            println!("FATAL ERROR: CURRENT_DESCRIPTOR_STORE is null during GcObject trace! Cannot trace object.");
            return;
        }
        // SAFETY: Pointer is checked non-null.
        let descriptor_store = unsafe { &*descriptor_store_ptr };

        // Get the LayoutDescriptor using the index stored in the GcObject header.
        let descriptor = match descriptor_store.descriptors.get(self.descriptor_index) {
            Some(desc) => desc,
            None => {
                println!(
                    "FATAL ERROR: Invalid descriptor index {} found in GcObject header during trace!",
                    self.descriptor_index
                );
                return;
            }
        };

        let data_ptr = self.data.as_ptr() as *const u8;

        // Pass the store reference to trace_recursive
        // SAFETY: Relies on the safety contract of trace_recursive.
        unsafe { trace_recursive(data_ptr, descriptor, descriptor_store, visitor) };
    }
}

/// Gets the rsgc::Thread handle for the current OS thread.
/// Assumes GC is initialized and thread is attached via main_thread or similar.
pub fn current_thread() -> &'static mut Thread {
    // SAFETY: Assumes GC is initialized and thread is attached or will be attached.
    Thread::current()
}


// --- Helper Functions ---

/// Checks if a HIR type might contain GC references. Used by the native backend.
/// Needs access to struct/enum definitions.
pub fn hir_type_contains_gc_handle(
    hir_type: &parallax_hir::hir::HirType,
    struct_defs: &std::collections::HashMap<parallax_hir::Symbol, parallax_hir::hir::HirStructDef>,
    enum_defs: &std::collections::HashMap<parallax_hir::Symbol, parallax_hir::hir::HirEnumDef>,
    visiting: &mut std::collections::HashSet<parallax_hir::Symbol>,
) -> bool {
    use parallax_hir::hir::{HirType, PrimitiveType};

    match hir_type {
        HirType::Adt(symbol) => {
            if visiting.contains(symbol) {
                // Found cycle involving a Handle - assume handle presence for safety
                return true;
            }
            visiting.insert(*symbol);

            let result = if let Some(struct_def) = struct_defs.get(symbol) {
                struct_def.fields.iter().any(|(_, _, field_ty)| {
                    hir_type_contains_gc_handle(field_ty, struct_defs, enum_defs, visiting)
                })
            } else if let Some(enum_def) = enum_defs.get(symbol) {
                enum_def.variants.iter().any(|variant| {
                    variant.fields.iter().any(|field_ty| {
                        hir_type_contains_gc_handle(field_ty, struct_defs, enum_defs, visiting)
                    })
                })
            } else {
                false // Unknown ADT, assume no handles for safety? Or error?
            };

            visiting.remove(symbol);
            result
        }
        HirType::Tuple(elements) => elements
            .iter()
            .any(|elem_ty| hir_type_contains_gc_handle(elem_ty, struct_defs, enum_defs, visiting)),
        HirType::Array(element_ty, size) => {
            match size {
                Some(_) => { // Fixed-size array: check element type
                     hir_type_contains_gc_handle(element_ty, struct_defs, enum_defs, visiting)
                }
                None => true, // Dynamic array (size None) is always a handle
            }
        }
        // Closures are represented by Handle<GcObject> after allocation
        HirType::FunctionPointer(..) => true,
        // StringRefs point to GcByteArray, represented by Handle<StringRef> -> Handle<GcObject> conceptually
        HirType::Primitive(prim) => matches!(prim, PrimitiveType::String),
        HirType::Never => false,
    }
} 