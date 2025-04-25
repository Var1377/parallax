#![feature(unsafe_cell_access)]
// Top-level crate comment if desired

// Module declarations
pub mod layout;
mod tracer;
mod roots;
mod collections;
mod ffi;
pub mod readback;

use parallax_hir::PrimitiveType;
// Core GC and internal types
use rsgc::heap::heap::Heap;
use rsgc::prelude::*;
use rsgc::heap::{thread::Thread, region::HeapArguments};
use memoffset::offset_of;
use std::ptr;

// Imports from internal modules
// use crate::descriptor::{DescriptorIndex, DescriptorStore}; // Removed: Re-exported below
use crate::tracer::trace_recursive;

// Re-export collection types
pub use crate::collections::string::{StringRef, GcByteArray};
pub use crate::collections::array::GcRawArray;
pub use crate::collections::closure::{ClosureRef, CLOSURE_REF_DESCRIPTOR_INDEX};
// Re-export layout types publicly
pub use crate::layout::descriptor::{DescriptorStore, DescriptorIndex, LayoutDescriptor};

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

// Global pointer to the descriptor store, managed by the JIT/runtime.
// Needs to be accessible by ffi.rs and tracer.rs
// FIXME: This is inherently unsafe without proper synchronization and lifetime management!
pub(crate) static mut GLOBAL_DESCRIPTOR_STORE: *const DescriptorStore = ptr::null();

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


/// Sets the global descriptor store pointer. Called by the runtime during initialization.
/// # Safety
/// `store_ptr` must point to a valid `DescriptorStore` that lives longer than any GC cycle.
/// This function is not thread-safe if called after GC threads have started.
pub unsafe fn set_descriptor_store(store_ptr: *const DescriptorStore) {
    // SAFETY: Function contract requires this is safe to call during init.
    unsafe { GLOBAL_DESCRIPTOR_STORE = store_ptr };
}

// SAFETY: Object implementation traces Handle pointers within the data buffer
// based on the LayoutDescriptor found via the index.
unsafe impl Object for GcObject {
    fn trace(&self, visitor: &mut dyn Visitor) {
        // SAFETY: Accessing static mut requires unsafe block.
        // Assumes GC synchronization ensures safe access during tracing.
        let descriptor_store_ptr = unsafe { GLOBAL_DESCRIPTOR_STORE };
        if descriptor_store_ptr.is_null() {
            println!("FATAL ERROR: GLOBAL_DESCRIPTOR_STORE is null during GcObject trace!");
            // Cannot proceed without descriptors.
            return;
        }
        // SAFETY: Assumes GLOBAL_DESCRIPTOR_STORE points to a valid, initialized store.
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

        // Get pointer to the start of the object's variable data part.
        let data_ptr = self.data.as_ptr() as *const u8;

        // Start the recursive trace using the object's descriptor and data pointer.
        // SAFETY: Relies on the safety contract of trace_recursive.
        unsafe { trace_recursive(data_ptr, descriptor, descriptor_store, visitor) };
    }
}


// --- Global Heap Initialization ---

/// Initialize the GC system.
/// Must be called before any other GC operations.
pub fn init_gc() {
    // Initialize the heap globally. We don't store the Heap instance itself.
    let args = HeapArguments::default();
    let _heap = Heap::new(args);
    
    // Ensure the current thread is attached
    let _current_thread = current_thread();

    // Create initial descriptor store
    let mut descriptor_store = DescriptorStore {
        descriptors: Vec::new(),
    };

    // Initialize the closure descriptor and save its index
    collections::closure::initialize_closure_ref_descriptor(&mut descriptor_store);

    // Set global descriptor store pointer for tracer to use
    unsafe {
        GLOBAL_DESCRIPTOR_STORE = Box::into_raw(Box::new(descriptor_store));
    }

    // Note: Root registration happens through the shadow stack and global roots APIs
    // directly called from JIT-generated code

    log::info!("Parallax GC initialized with closure descriptor");
}

/// Attempts to gracefully shut down the GC background threads.
///
/// NOTE: The public API for triggering GC shutdown in rsgc outside of the
/// `main_thread` callback pattern is currently unclear from documentation.
/// This function is a placeholder.
pub fn shutdown_gc() {
    log::info!("Parallax GC shutdown requested...");
    // TODO: Implement actual GC shutdown logic if possible.
    // This might involve finding the ControlThread instance(s) and calling stop().n    // For now, we assume cleanup happens on process exit.
    log::warn!("Actual GC thread shutdown mechanism not implemented.");
}

/// Gets the rsgc::Thread handle for the current OS thread.
/// Assumes GC is initialized.
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
        HirType::Array(element_ty, _size) => {
            hir_type_contains_gc_handle(element_ty, struct_defs, enum_defs, visiting)
        }
        // Closures are represented by Handle<GcObject> after allocation
        HirType::FunctionPointer(..) => true,
        // StringRefs point to GcByteArray, represented by Handle<StringRef> -> Handle<GcObject> conceptually
        HirType::Primitive(prim) => matches!(prim, PrimitiveType::String),
        HirType::Never => false,
    }
} 