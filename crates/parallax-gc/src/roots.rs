use std::cell::UnsafeCell;
use std::ptr;
use rsgc::prelude::*;

thread_local! {
    static SHADOW_STACK: UnsafeCell<Vec<*mut u8>> = UnsafeCell::new(Vec::new());
    static GLOBAL_ROOTS: UnsafeCell<Vec<*mut u8>> = UnsafeCell::new(Vec::new());
}

/// FFI: Pushes a root onto the shadow stack.
#[no_mangle]
pub extern "C" fn push_shadow_stack(ptr: *mut u8) {
    if !ptr.is_null() {
        SHADOW_STACK.with(|stack| unsafe {
            stack.as_mut_unchecked().push(ptr);
        });
    }
}

/// FFI: Pops roots from the shadow stack.
#[no_mangle]
pub extern "C" fn pop_shadow_stack(count: usize) {
    SHADOW_STACK.with(|stack| unsafe {
        let stack = stack.as_mut_unchecked();
        stack.truncate(stack.len().saturating_sub(count));
    });
}

/// FFI: Registers a global root. ptr_to_handle points to the memory holding the Handle bits.
#[no_mangle]
pub extern "C" fn register_global_root(ptr_to_handle: *mut u8) {
    if !ptr_to_handle.is_null() {
        GLOBAL_ROOTS.with(|roots| unsafe {
            let roots = roots.as_mut_unchecked();
            if !roots.contains(&ptr_to_handle) {
                roots.push(ptr_to_handle);
            }
        });
    }
}

/// FFI: Unregisters a global root.
#[no_mangle]
pub extern "C" fn unregister_global_root(ptr_to_handle: *mut u8) {
    if !ptr_to_handle.is_null() {
        GLOBAL_ROOTS.with(|roots| unsafe {
            let roots = roots.as_mut_unchecked();
            roots.retain(|&p| p != ptr_to_handle);
        });
    }
}

/// Visits roots on the shadow stack (called by GC).
/// # Safety - Visitor must be valid.
unsafe fn visit_shadow_stack_roots(visitor: &mut dyn Visitor) {
    SHADOW_STACK.with(|stack| unsafe {
        let stack = stack.as_ref_unchecked();
        for &ptr in stack.iter() {
             visitor.visit(ptr);
        }
    });
}

/// Visits global roots (called by GC).
/// # Safety - Visitor must be valid. Assumes stored pointers are valid Handle bits.
unsafe fn visit_global_roots(visitor: &mut dyn Visitor) {
    GLOBAL_ROOTS.with(|roots| unsafe {
        let roots = roots.as_ref_unchecked();
        for &root_ptr in roots.iter() {
            // Read the Handle bits from the global variable's location
            let handle_value: *mut u8 = ptr::read_volatile(root_ptr as *const *mut u8);
            if !handle_value.is_null() {
                 visitor.visit(handle_value);
            }
        }
    });
}

/// Main root visiting function called by the GC.
/// Needs to be registered with rsgc, perhaps via HeapArguments or a custom Root type.
/// # Safety - Visitor must be valid.
pub unsafe fn visit_roots(visitor: &mut dyn Visitor) {
    visit_shadow_stack_roots(visitor);
    visit_global_roots(visitor);
} 