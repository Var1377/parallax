use rsgc::prelude::*;
use std::mem;
use memoffset::offset_of;
use parallax_layout::{DescriptorIndex, LayoutDescriptor, DescriptorStore};
use crate::current_thread;
use log;
use once_cell::sync::OnceCell;

/// Represents a closure reference at runtime.
/// Contains a function pointer and an environment handle.
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct ClosureRef {
    /// Pointer to the function's compiled code
    pub func_ptr: *const u8,
    /// Handle to the closure's environment (GcObject)
    pub env_handle: *mut u8,
}

// SAFETY: ClosureRef has fixed size
unsafe impl Allocation for ClosureRef {}

// SAFETY: ClosureRef contains a GC handle that needs to be traced
unsafe impl Object for ClosureRef {
    fn trace(&self, visitor: &mut dyn Visitor) {
        // SAFETY: We're calling visit on a valid GC handle pointer
        unsafe {
            if !self.env_handle.is_null() {
                visitor.visit(self.env_handle);
            }
        }
    }
}

/// Use OnceCell for thread-safe one-time initialization
pub static CLOSURE_REF_DESCRIPTOR_INDEX: OnceCell<DescriptorIndex> = OnceCell::new();

/// Initializes the descriptor for ClosureRef and stores its index via OnceCell.
/// Can be called multiple times; initialization happens only once.
/// Returns the index (existing or newly created).
pub fn initialize_closure_ref_descriptor(store: &mut DescriptorStore) -> DescriptorIndex {
    *CLOSURE_REF_DESCRIPTOR_INDEX.get_or_init(|| {
        log::debug!("Initializing ClosureRef descriptor...");
        let func_ptr_offset = offset_of!(ClosureRef, func_ptr);
        let env_handle_offset = offset_of!(ClosureRef, env_handle);
        const PLACEHOLDER_HANDLE_IDX: DescriptorIndex = usize::MAX;
        const PLACEHOLDER_PRIMITIVE_IDX: DescriptorIndex = 0;

        let closure_descriptor = LayoutDescriptor::Struct {
            size_bytes: mem::size_of::<ClosureRef>(),
            align_bytes: mem::align_of::<ClosureRef>(),
            fields: vec![
                (func_ptr_offset, PLACEHOLDER_PRIMITIVE_IDX),
                (env_handle_offset, PLACEHOLDER_HANDLE_IDX),
            ].into_boxed_slice(),
            handle_offsets: vec![env_handle_offset].into_boxed_slice(),
        };

        let closure_desc_index = store.descriptors.len();
        store.descriptors.push(closure_descriptor);
        log::info!("Initialized ClosureRef descriptor at index: {}", closure_desc_index);
        closure_desc_index
    })
}

/// FFI: Allocates a closure with function pointer and environment handle.
/// 
/// # Parameters
/// * `func_ptr`: Pointer to the lambda's function body
/// * `env_handle`: Handle to the environment object
/// 
/// # Returns
/// A tagged Handle<ClosureRef> (LSB is set to 1 to indicate closure)
/// The raw (untagged) pointer is pushed to the shadow stack automatically.
#[no_mangle]
pub extern "C" fn parallax_alloc_closure(
    func_ptr: *const u8, 
    env_handle: *mut u8
) -> *mut u8 {
    let thread = current_thread();
    thread.safepoint();

    // Get the descriptor index (panics if not initialized, which shouldn't happen)
    let _descriptor_index = CLOSURE_REF_DESCRIPTOR_INDEX.get()
        .expect("CLOSURE_REF_DESCRIPTOR_INDEX not initialized before allocation");

    // Create the ClosureRef object
    let closure_ref = ClosureRef {
        func_ptr,
        env_handle,
    };
    
    // Allocate on GC heap
    let handle = thread.allocate(closure_ref);
    
    // Push the untagged handle to shadow stack
    let raw_ptr = handle.as_ptr() as *mut u8;
    crate::roots::push_shadow_stack(raw_ptr);
    
    // Return tagged pointer (set LSB to 1)
    (raw_ptr as usize | 1) as *mut u8
}

/// FFI: Gets the function pointer from a closure reference.
/// The handle should be tagged (LSB set to 1).
#[no_mangle]
pub extern "C" fn parallax_closure_get_function(closure_handle: *mut u8) -> *const u8 {
    let thread = current_thread();
    thread.safepoint();
    
    // Untag the handle
    let untagged_handle = (closure_handle as usize & !1) as *mut u8;
    
    // Get the function pointer
    unsafe {
        let handle = Handle::<ClosureRef>::from_raw(untagged_handle);
        (*handle).func_ptr
    }
}

/// FFI: Gets the environment handle from a closure reference.
/// The handle should be tagged (LSB set to 1).
#[no_mangle]
pub extern "C" fn parallax_closure_get_environment(closure_handle: *mut u8) -> *mut u8 {
    let thread = current_thread();
    thread.safepoint();
    
    // Untag the handle
    let untagged_handle = (closure_handle as usize & !1) as *mut u8;
    
    // Get the environment handle
    unsafe {
        let handle = Handle::<ClosureRef>::from_raw(untagged_handle);
        (*handle).env_handle
    }
} 