use rsgc::prelude::*;
use std::mem::{self, MaybeUninit};
use std::ptr::{self, addr_of_mut};
use memoffset::offset_of;
use crate::current_thread; // Assuming current_thread() is accessible from crate root

/// Enum to distinguish captured value types for GC tracing.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaptureType {
    Handle = 0, // Represents a GC Handle (stored as usize/pointer)
    U64 = 1,    // Represents a u64 value
    F64 = 2,    // Represents an f64 value (stored as u64 bits)
}

/// FFI-safe representation of a captured item's data.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct CaptureItem {
    pub ty: CaptureType,
    pub data: usize, // Raw data/pointer bits
}

/// Represents the captured environment for a closure (Variable-sized).
#[repr(C)]
#[derive(Debug)]
pub struct ClosureEnv {
    len: u32,
    capacity: u32,
    data: [(CaptureType, usize); 0],
}

// SAFETY: Allocation impl correctly describes layout.
unsafe impl Allocation for ClosureEnv {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = mem::size_of::<(CaptureType, usize)>();
    const SIZE: usize = mem::size_of::<u32>() * 2; // len + capacity
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(ClosureEnv, len);
    const VARSIZE_OFFSETOF_CAPACITY: usize = offset_of!(ClosureEnv, capacity);
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(ClosureEnv, data);
    // Contains Handles in the variable part
    const NO_HEAP_PTRS: bool = false;
    const VARSIZE_NO_HEAP_PTRS: bool = false;
}

// SAFETY: Object impl correctly traces Handles in the data array.
unsafe impl Object for ClosureEnv {
    fn trace(&self, visitor: &mut dyn Visitor) {
        let data_ptr = self.data.as_ptr();
        for i in 0..(self.len as usize) {
            let item_ptr = unsafe { data_ptr.add(i) };
            let (capture_type, data_val) = unsafe { *item_ptr };
            if capture_type == CaptureType::Handle {
                let handle_ptr = data_val as *mut u8;
                unsafe { visitor.visit(handle_ptr) };
            }
        }
    }
}

/// Represents a closure reference: function pointer + environment handle.
/// This struct itself is allocated on the heap.
#[repr(C)] // Ensure predictable layout for potential FFI use
#[derive(Debug)]
pub struct ClosureRef {
    pub func_ptr: *const u8, // Untagged function pointer
    pub env_ptr: Handle<ClosureEnv>, // Handle to the variable-sized env
}

// SAFETY: Allocation impl for fixed-size ClosureRef.
unsafe impl Allocation for ClosureRef {}

// SAFETY: Object impl traces the env_ptr Handle.
unsafe impl Object for ClosureRef {
    fn trace(&self, visitor: &mut dyn Visitor) {
        unsafe { visitor.visit(self.env_ptr.as_ptr() as *mut u8) };
    }
}

/// FFI: Allocates a `ClosureRef` and its environment.
/// Returns a *TAGGED* Handle (LSB=1) suitable for use in JIT code.
#[no_mangle]
pub extern "C" fn parallax_alloc_closure(
    func_ptr: *const u8,
    captures_ptr: *const CaptureItem,
    captures_len: usize,
) -> Handle<ClosureRef> { // Return Handle, not raw ptr
    let thread = current_thread();
    // Check safepoint first 
    thread.safepoint();
    
    // 1. Allocate ClosureEnv
    let mut env_handle_uninit: Handle<MaybeUninit<ClosureEnv>> = thread.allocate_varsize(captures_len);
    let env_ptr: *mut ClosureEnv = env_handle_uninit.as_mut_ptr();
    // 2. Initialize ClosureEnv
    unsafe {
        ptr::write(addr_of_mut!((*env_ptr).len), captures_len as u32);
        ptr::write(addr_of_mut!((*env_ptr).capacity), captures_len as u32);
        let data_start_ptr = addr_of_mut!((*env_ptr).data) as *mut CaptureItem;
        ptr::copy_nonoverlapping(captures_ptr, data_start_ptr, captures_len);
    }
    let initialized_env_handle: Handle<ClosureEnv> = unsafe { mem::transmute(env_handle_uninit) };
    // 3. Allocate ClosureRef
    let closure_ref = ClosureRef { func_ptr, env_ptr: initialized_env_handle };
    let closure_ref_handle: Handle<ClosureRef> = thread.allocate(closure_ref);
    // 4. Tag Handle
    let tagged_raw_ptr_mut = (closure_ref_handle.as_ptr() as usize | 1) as *mut u8;
    unsafe { Handle::<ClosureRef>::from_raw(tagged_raw_ptr_mut) }
} 