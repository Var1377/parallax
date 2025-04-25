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

/// Variable-sized byte array for GC-managed strings.
/// Contains the length followed immediately by the byte data.
#[repr(C)] // Ensure layout: len first, then data
pub struct GcByteArray {
    pub(crate) len: u32,
    // Use a flexible array member pattern (conceptually)
    // The actual data follows this header in memory.
    pub(crate) data: [u8; 0],
}

// Safety: Allocation implementation describes the layout for rsgc.
unsafe impl Allocation for GcByteArray {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = 1; // Data is bytes
    // Size of the fixed part (just the length)
    const SIZE: usize = std::mem::size_of::<u32>();
    // Offset of the length field itself
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(GcByteArray, len);
    const VARSIZE_OFFSETOF_CAPACITY: usize = usize::MAX; // No separate capacity field
    // Offset of the variable data part
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(GcByteArray, data);
    // Neither fixed nor variable parts contain GC pointers
    const NO_HEAP_PTRS: bool = true;
    const VARSIZE_NO_HEAP_PTRS: bool = true;
}

// Safety: GcByteArray contains no GC pointers, so trace is a no-op.
unsafe impl Object for GcByteArray {
    fn trace(&self, _visitor: &mut dyn Visitor) {
        // No handles within GcByteArray itself
    }
}

impl GcByteArray {
    /// Calculates the offset of the data part relative to the start of the struct.
    /// Useful for FFI and manual pointer manipulation.
    pub fn data_offset() -> usize {
        Self::VARSIZE_OFFSETOF_VARPART
    }

    /// Returns the length of the byte array (number of bytes).
    pub fn len(&self) -> u32 {
        self.len
    }

    /// Returns a slice containing the bytes of the array.
    /// # Safety
    /// The caller must ensure the GcByteArray handle is valid and points to initialized data.
    pub unsafe fn data_slice(&self) -> &[u8] {
        let data_ptr = (self as *const Self as *const u8).add(Self::data_offset());
        std::slice::from_raw_parts(data_ptr, self.len as usize)
    }
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