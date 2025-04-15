use rsgc::prelude::*;
use std::mem::{self, MaybeUninit};
use std::ptr::{self, addr_of_mut};
use memoffset::offset_of;
use crate::current_thread; // Assuming current_thread() is accessible from crate root

/// Represents a reference to string data at runtime.
#[repr(C)] // Ensure predictable layout
#[derive(Debug, Copy, Clone)]
pub struct StringRef {
    pub ptr: *const u8, // Pointer to UTF-8 byte data
    pub len: usize,     // Length in bytes
}

// SAFETY: StringRef has fixed size.
unsafe impl Allocation for StringRef {}

// SAFETY: StringRef contains no GC Handles *itself*.
unsafe impl Object for StringRef {
    fn trace(&self, _visitor: &mut dyn Visitor) {}
}

/// A variable-sized byte array managed by the GC.
#[repr(C)]
#[derive(Debug)]
pub struct GcByteArray {
    len: u32,
    capacity: u32,
    data: [u8; 0],
}

// SAFETY: Allocation impl correctly describes layout.
unsafe impl Allocation for GcByteArray {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = mem::size_of::<u8>();
    const SIZE: usize = mem::size_of::<u32>() * 2;
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(GcByteArray, len);
    const VARSIZE_OFFSETOF_CAPACITY: usize = offset_of!(GcByteArray, capacity);
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(GcByteArray, data);
    const NO_HEAP_PTRS: bool = true;
    const VARSIZE_NO_HEAP_PTRS: bool = true;
}

// SAFETY: GcByteArray contains no GC pointers.
unsafe impl Object for GcByteArray {
    fn trace(&self, _visitor: &mut dyn Visitor) {}
}

/// FFI: Allocates a `StringRef` pointing to existing static data.
#[no_mangle]
pub extern "C" fn parallax_alloc_string_ref(ptr: *const u8, len: usize) -> Handle<StringRef> {
    let thread = current_thread();
    thread.safepoint();
    thread.allocate(StringRef { ptr, len })
}

/// FFI: Allocates a `StringRef` and copies string data into a new `GcByteArray`.
#[no_mangle]
pub extern "C" fn parallax_alloc_string_from_rust_buffer(
    buffer_ptr: *const u8,
    buffer_len: usize,
) -> Handle<StringRef> {
    let thread = current_thread();
    thread.safepoint();
    
    // 1. Allocate GcByteArray
    let mut byte_array_handle_uninit: Handle<MaybeUninit<GcByteArray>> = thread.allocate_varsize(buffer_len);
    let byte_array_ptr: *mut GcByteArray = byte_array_handle_uninit.as_mut_ptr();
    // 2. Initialize GcByteArray
    unsafe {
        ptr::write(addr_of_mut!((*byte_array_ptr).len), buffer_len as u32);
        ptr::write(addr_of_mut!((*byte_array_ptr).capacity), buffer_len as u32);
        let data_start_ptr = addr_of_mut!((*byte_array_ptr).data) as *mut u8;
        ptr::copy_nonoverlapping(buffer_ptr, data_start_ptr, buffer_len);
    }
    let initialized_byte_array_handle: Handle<GcByteArray> = unsafe { mem::transmute(byte_array_handle_uninit) };
    // 3. Create StringRef pointing into GcByteArray
    let string_data_ptr = unsafe {
        (initialized_byte_array_handle.as_ptr() as *const u8).add(offset_of!(GcByteArray, data))
    };
    let string_ref_obj = StringRef { ptr: string_data_ptr, len: buffer_len };
    // 4. Allocate StringRef
    thread.allocate(string_ref_obj)
} 