use rsgc::prelude::*;
use std::mem;
use memoffset::offset_of;
use crate::layout::descriptor::{DescriptorIndex, LayoutDescriptor, DescriptorStore};
use crate::{current_thread, GcObject};

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

/// Descriptor index for the static ClosureRef type.
/// This is initialized during GC initialization and used by the JIT.
pub static mut CLOSURE_REF_DESCRIPTOR_INDEX: Option<DescriptorIndex> = None;

/// Creates and initializes the static descriptor for ClosureRef.
/// Called during GC initialization.
pub(crate) fn initialize_closure_ref_descriptor(store: &mut DescriptorStore) -> DescriptorIndex {
    let descriptor_index = store.descriptors.len();
    
    // Handle descriptor for env_handle
    let handle_descriptor_index = descriptor_index + 1;
    
    store.descriptors.push(LayoutDescriptor::Struct {
        size_bytes: mem::size_of::<ClosureRef>(),
        align_bytes: mem::align_of::<ClosureRef>(),
        fields: Box::new([
            // env_handle: GC handle, needs descriptor
            (offset_of!(ClosureRef, env_handle), handle_descriptor_index),
        ]),
        // Calculate the offset of the handle field
        handle_offsets: Box::new([offset_of!(ClosureRef, env_handle)]),
    });
    
    // Add Handle descriptor
    store.descriptors.push(LayoutDescriptor::Handle);
    
    // Store the index in the global static
    unsafe {
        CLOSURE_REF_DESCRIPTOR_INDEX = Some(descriptor_index);
    }
    
    descriptor_index
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
    
    // Create the ClosureRef object
    let closure_ref = ClosureRef {
        func_ptr,
        env_handle,
    };
    
    // Allocate on GC heap
    let handle = thread.allocate(closure_ref);
    
    // Push the untagged handle to shadow stack
    let raw_ptr = unsafe { handle.as_ptr() as *mut u8 };
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