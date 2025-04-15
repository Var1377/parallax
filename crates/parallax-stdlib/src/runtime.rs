#![allow(dead_code)] // Allow unused for now

// Import necessary items from native backend
use parallax_native::StringRef;
use parallax_native::gc::Handle; // Need Handle

use std::io::{self, Write}; // For stdin/stdout

// Declare the FFI functions we need from the native backend
extern "C" {
    // Old function (still used for empty string on EOF/Error for now)
    // fn parallax_alloc_string_ref(ptr: *const u8, len: usize) -> Handle<StringRef>; // Keep if needed for empty string

    // New function to allocate string and its buffer together
    fn parallax_alloc_string_from_rust_buffer(ptr: *const u8, len: usize) -> Handle<StringRef>;
}

/// Runtime implementation for `println(s: string) -> ()`
/// Takes a Handle<StringRef> which is essentially a pointer.
#[no_mangle]
pub extern "C" fn println(s_handle: Handle<StringRef>) {
    if s_handle.is_null() {
        // Handle null pointer case, maybe print "<null>" or panic?
        eprintln!("Error: println called with null string reference.");
        return;
    }
    // SAFETY: Assumes s_handle points to a valid, initialized StringRef object.
    // The lifetime of the pointed-to bytes (ptr) must be valid during this call.
    // We also assume the StringRef struct itself is live (managed by GC).
    let s_ref = unsafe { *s_handle.as_ptr() };

    // SAFETY: Assumes s_ref.ptr is valid for s_ref.len bytes and contains UTF-8.
    let slice = unsafe { std::slice::from_raw_parts(s_ref.ptr, s_ref.len) };
    match std::str::from_utf8(slice) {
        Ok(string) => {
            println!("{}", string);
        }
        Err(_) => {
            // Handle invalid UTF-8, maybe print bytes?
            eprintln!("Error: println received invalid UTF-8 sequence.");
        }
    }
}

/// Runtime implementation for `readln() -> string`
/// Returns a Handle<StringRef> to a GC-allocated StringRef.
#[no_mangle]
pub extern "C" fn readln() -> Handle<StringRef> {
    let mut buffer = String::new();
    let empty_bytes: &[u8] = b""; // Reusable empty slice
    match io::stdin().read_line(&mut buffer) {
        Ok(0) => { // EOF reached
            // Return an empty string representation using the new allocator
            // SAFETY: Allocating StringRef+Buffer for empty bytes.
            unsafe { parallax_alloc_string_from_rust_buffer(empty_bytes.as_ptr(), 0) }
        }
        Ok(_) => {
            // Remove trailing newline characters
            if buffer.ends_with('\n') {
                buffer.pop();
                if buffer.ends_with('\r') {
                    buffer.pop();
                }
            }

            // Allocate the StringRef and copy the buffer content onto the GC heap
            // SAFETY: Allocating StringRef pointing GC-allocated bytes copied from buffer.
            unsafe { parallax_alloc_string_from_rust_buffer(buffer.as_ptr(), buffer.len()) }
        }
        Err(e) => {
            // Handle read error, maybe panic or return a specific error string?
            eprintln!("Error reading line: {}", e);
            // Return an empty string representation on error using the new allocator
            // SAFETY: Allocating StringRef+Buffer for empty bytes.
            unsafe { parallax_alloc_string_from_rust_buffer(empty_bytes.as_ptr(), 0) }
        }
    }
} 