use parallax_layout::{DescriptorIndex, LayoutDescriptor, DescriptorStore};
use crate::{current_thread, CURRENT_DESCRIPTOR_STORE}; // Need access to current_thread and store
use rsgc::prelude::*;
use std::mem::{self, MaybeUninit};
use std::ptr::{self, addr_of_mut};
use std::alloc;
use crate::collections::array::{GcRawArray, GcRawArrayHeader};
use rsgc::heap::thread::Thread; // Import Thread

/// FFI: Allocates raw memory using the standard Rust allocator.
///
/// # WARNING - DEVELOPMENT ONLY - MEMORY LEAK
/// This function allocates memory but DOES NOT register it with the `rsgc` garbage collector.
/// Any memory allocated via this function WILL BE LEAKED and WILL NOT BE TRACED.
/// This is a temporary workaround to allow JIT development of aggregate construction logic.
/// A proper solution requires integrating the JIT's dynamic type information with
/// `rsgc`'s typed allocation and tracing mechanisms.
///
/// # Safety
/// Caller must ensure size and align are valid. The returned pointer points to
/// uninitialized memory.
#[no_mangle]
pub unsafe extern "C" fn parallax_gc_alloc(size: usize, align: usize) -> *mut u8 {
    if size == 0 {
        // Return a dangling but aligned pointer for zero-sized allocations
        // Matches behavior of Rust's allocator for ZSTs.
        return align as *mut u8;
    }
    match alloc::Layout::from_size_align(size, align) {
        Ok(layout) => {
            // Use the global allocator directly.
            // This memory is NOT managed by rsgc.
            let ptr = alloc::alloc(layout);
            if ptr.is_null() {
                // Allocation failed (out of memory)
                // TODO: How should the JIT handle OOM?
                // For now, just return null. A robust solution might trap.
                return std::ptr::null_mut();
            }
            // Optional: Zero the memory if desired, though initialization happens in JIT.
            // ptr::write_bytes(ptr, 0, size);
            ptr
        }
        Err(_) => {
            // Invalid layout requested
            // TODO: How should the JIT handle invalid layout requests?
            // Return null for now.
            std::ptr::null_mut()
        }
    }
}

/// FFI: Allocates a generic GcObject based on a descriptor *index* and initializes
/// its data from a provided buffer.
/// Requires the descriptor store pointer to be set in thread-local storage.
///
/// # Arguments
/// * `descriptor_index`: Index into the stable `DescriptorStore` owned by the JIT/runtime.
/// * `init_data_ptr`: Pointer to a buffer containing the initial byte values for the object's data part.
///                   The size of this buffer must match the size indicated by the descriptor at the index.
///
/// # Returns
/// A `Handle<GcObject>` to the newly allocated and initialized object.
/// Returns `Handle::null()` if allocation fails, the index is invalid, or the global descriptor store is not set.
///
/// # Safety
/// * `GLOBAL_DESCRIPTOR_STORE` must be set and point to a valid, live `DescriptorStore`.
/// * `descriptor_index` must be a valid index within that store.
/// * `init_data_ptr` must be a valid pointer to a buffer of the correct size according to the descriptor.
/// * The data at `init_data_ptr` must represent the object's fields correctly, especially Handle bits.
#[no_mangle]
pub extern "C" fn parallax_alloc_object(
    descriptor_index: DescriptorIndex,
    init_data_ptr: *const u8, // Pointer to stack buffer with initial field values
) -> Handle<crate::GcObject> { // Return Handle<GcObject>
    let mut thread = current_thread(); // Make thread mutable
    thread.safepoint();

    // Get descriptor store pointer from thread-local
    let descriptor_store_ptr = CURRENT_DESCRIPTOR_STORE.with(|cell| cell.get());
    if descriptor_store_ptr.is_null() {
        // This should not happen if the runtime correctly sets the thread-local
        println!("Error: parallax_alloc_object called but CURRENT_DESCRIPTOR_STORE was not set.");
        return unsafe { Handle::from_raw(ptr::null_mut()) };
    }

    // SAFETY: descriptor_store_ptr is checked non-null.
    let descriptor_store = unsafe { &*descriptor_store_ptr };

    // Get the descriptor using the non-null store reference.
    let descriptor = match descriptor_store.descriptors.get(descriptor_index) {
         Some(desc) => desc,
         None => {
             println!("Error: parallax_alloc_object called with invalid descriptor_index: {}", descriptor_index);
             return unsafe { Handle::from_raw(ptr::null_mut()) };
         }
    };

    // Determine data size from the descriptor.
    let data_size = match descriptor {
        LayoutDescriptor::Primitive { size_bytes, .. } => *size_bytes,
        LayoutDescriptor::Handle => mem::size_of::<*mut u8>(),
        // Use size_bytes directly from the descriptor, which now includes handle_offsets etc.
        LayoutDescriptor::Struct { size_bytes, .. } => *size_bytes,
        LayoutDescriptor::Enum { size_bytes, .. } => *size_bytes,
        LayoutDescriptor::Array { size_bytes, .. } => *size_bytes,
    };

    let mut handle_uninit: Handle<MaybeUninit<crate::GcObject>> = thread.allocate_varsize(data_size);

    if handle_uninit.as_ptr().is_null() {
        println!("Error: GC allocation failed in parallax_alloc_object");
        return unsafe { Handle::from_raw(ptr::null_mut()) }; // Allocation failed
    }

    let gc_ptr: *mut crate::GcObject = handle_uninit.as_mut_ptr();

    unsafe {
        // 1. Write the descriptor *index* into the fixed part of GcObject.
        ptr::write(addr_of_mut!((*gc_ptr).descriptor_index), descriptor_index);

        // 2. Copy the initial data from the provided buffer into the variable data part.
        if data_size > 0 { // Avoid copy if ZST
            let data_dest_ptr = addr_of_mut!((*gc_ptr).data) as *mut u8;
            ptr::copy_nonoverlapping(init_data_ptr, data_dest_ptr, data_size);
        }

        // 3. Cast the handle to the initialized type *before* the barrier.
        let initialized_handle: Handle<crate::GcObject> = mem::transmute(handle_uninit);

        // 4. Apply Write Barriers using the store reference
        apply_write_barriers_for_descriptor(
            &mut thread,
            initialized_handle,
            addr_of_mut!((*gc_ptr).data) as *const u8,
            descriptor,
            descriptor_store // Pass the store reference
        );
    }

    unsafe { mem::transmute(handle_uninit) }
}

/// Helper to apply write barriers recursively based on a descriptor.
/// # Safety
/// * `thread` must be the current rsgc thread.
/// * `object_handle` must be the handle to the GcObject being initialized.
/// * `data_ptr` must point to the start of the data corresponding to `descriptor` within the object.
/// * `descriptor` must correctly describe the layout of the data at `data_ptr`.
/// * `descriptor_store` must be a valid pointer to the global store.
unsafe fn apply_write_barriers_for_descriptor(
    thread: &mut Thread,
    object_handle: Handle<crate::GcObject>,
    data_ptr: *const u8,
    descriptor: &LayoutDescriptor,
    descriptor_store: &DescriptorStore,
) {
    match descriptor {
        LayoutDescriptor::Primitive { .. } => { /* No handles */ }
        LayoutDescriptor::Handle => {
            // Read the handle value directly from data_ptr.
            let handle_value = ptr::read(data_ptr as *const *mut u8);
            if !handle_value.is_null() {
                // Apply barrier for the containing object.
                thread.write_barrier(object_handle);
            }
        }
        LayoutDescriptor::Struct { handle_offsets, .. } => {
            // Iterate through the pre-calculated handle offsets.
            for &offset in handle_offsets.iter() {
                let field_ptr = data_ptr.add(offset);
                let handle_value = ptr::read(field_ptr as *const *mut u8);
                if !handle_value.is_null() {
                    thread.write_barrier(object_handle);
                    // Optimization: If one field needs a barrier, the whole object does.
                    // We could potentially break early, but iterating all handles
                    // might be necessary if rsgc becomes more granular later.
                }
            }
        }
        LayoutDescriptor::Enum { discriminant_offset, discriminant_size_bytes, variants, .. } => {
            // 1. Read the discriminant value.
            let discriminant_ptr = data_ptr.add(*discriminant_offset);
            let discriminant_value: u64 = match *discriminant_size_bytes {
                1 => ptr::read(discriminant_ptr as *const u8) as u64,
                2 => ptr::read(discriminant_ptr as *const u16) as u64,
                4 => ptr::read(discriminant_ptr as *const u32) as u64,
                8 => ptr::read(discriminant_ptr as *const u64),
                _ => {
                    println!("Error: Invalid discriminant size {} during write barrier application.", discriminant_size_bytes);
                    return; // Cannot proceed
                }
            };

            // 2. Find the matching variant descriptor index.
            if let Some((_discr, payload_offset, variant_desc_index)) = variants.iter().find(|(v, _, _)| *v == discriminant_value) {
                // 3. Get the variant's payload descriptor.
                let variant_descriptor = match descriptor_store.descriptors.get(*variant_desc_index) {
                    Some(desc) => desc,
                    None => {
                         println!("Error: Invalid variant descriptor index {} during write barrier application.", variant_desc_index);
                         return;
                    }
                };

                // 4. Calculate pointer to the variant's payload data.
                let payload_data_ptr = data_ptr.add(*payload_offset);

                // 5. Recursively apply barriers for the payload.
                apply_write_barriers_for_descriptor(
                    thread,
                    object_handle, // Barrier applies to the outer object handle
                    payload_data_ptr,
                    variant_descriptor,
                    descriptor_store
                );
            } else {
                 println!("Warning: Unknown discriminant value {} during write barrier application.", discriminant_value);
            }
        }
        LayoutDescriptor::Array { element_descriptor_index, count, element_stride_bytes, element_contains_handles, .. } => {
            // Only need to apply barriers if elements might contain handles.
            if *element_contains_handles {
                let element_descriptor = match descriptor_store.descriptors.get(*element_descriptor_index) {
                    Some(desc) => desc,
                    None => {
                         println!("Error: Invalid element descriptor index {} during array write barrier application.", element_descriptor_index);
                         return;
                    }
                };

                let stride = *element_stride_bytes;
                if stride == 0 && *count > 0 { // Avoid issues with ZST elements
                    return;
                }

                for i in 0..*count {
                    let element_offset = i * stride;
                    let element_data_ptr = data_ptr.add(element_offset);
                    // Recursively apply barriers for each element.
                    apply_write_barriers_for_descriptor(
                        thread,
                        object_handle, // Barrier applies to the outer object handle
                        element_data_ptr,
                        element_descriptor,
                        descriptor_store
                    );
                }
            }
        }
    }
}

/// FFI: Allocates a GcRawArray with the specified element layout and capacity.
/// Requires the descriptor store pointer to be set in thread-local storage.
/// The array is initially empty (length 0).
///
/// # Arguments
/// * `element_descriptor_index`: Index into the stable `DescriptorStore` for the element type.
/// * `capacity`: The desired initial capacity (number of elements).
///
/// # Returns
/// A `Handle<GcRawArray>` to the newly allocated array.
/// Returns `Handle::null()` if allocation fails, the index is invalid,
/// or the global descriptor store is not set.
///
/// # Safety
/// * `GLOBAL_DESCRIPTOR_STORE` must be set and point to a valid, live `DescriptorStore`.
/// * `element_descriptor_index` must be a valid index within that store.
#[no_mangle]
pub unsafe extern "C" fn parallax_alloc_array(
    element_descriptor_index: DescriptorIndex,
    capacity: usize,
) -> Handle<GcRawArray> {
    let thread = current_thread();
    thread.safepoint();

    // Get descriptor store pointer from thread-local
    let descriptor_store_ptr = CURRENT_DESCRIPTOR_STORE.with(|cell| cell.get());
    if descriptor_store_ptr.is_null() {
        println!("Error: parallax_alloc_array called but CURRENT_DESCRIPTOR_STORE was not set.");
        return unsafe { Handle::from_raw(ptr::null_mut()) };
    }
    let descriptor_store = unsafe { &*descriptor_store_ptr };

    // Get the element descriptor from the store reference.
    let element_descriptor = match descriptor_store.descriptors.get(element_descriptor_index) {
        Some(desc) => desc,
        None => {
            println!("Error: parallax_alloc_array called with invalid element_descriptor_index: {}", element_descriptor_index);
            return unsafe { Handle::from_raw(ptr::null_mut()) };
        }
    };

    // Calculate the total byte size needed for the data part.
    // SAFETY: descriptor is valid per check above.
    let data_byte_size = GcRawArray::calculate_data_byte_size(element_descriptor, capacity);
    // Determine alignment for allocation (use element alignment)
    // TODO: Does allocate_varsize need alignment info? rsgc docs aren't explicit.
    // Let's assume default alignment works for now.

    // Allocate memory for the GcRawArray (header + variable data part).
    let mut handle_uninit: Handle<MaybeUninit<GcRawArray>> = thread.allocate_varsize(data_byte_size);

    if handle_uninit.as_ptr().is_null() {
        println!("Error: GC allocation failed in parallax_alloc_array");
        return unsafe { Handle::from_raw(ptr::null_mut()) }; // Allocation failed
    }

    // Get a mutable pointer to the allocated memory.
    let gc_ptr: *mut GcRawArray = handle_uninit.as_mut_ptr();

    // Initialize the GcRawArrayHeader.
    // SAFETY: gc_ptr is valid and points to the allocated memory.
    unsafe {
        let header_ptr = addr_of_mut!((*gc_ptr).header);
        // Write len, capacity, and element descriptor index.
        ptr::write(header_ptr, GcRawArrayHeader {
            len: 0, // Start with length 0
            capacity: capacity as u32, // Store requested capacity
            element_descriptor_index,
        });
        
        // NOTE: The data part remains uninitialized. Elements should be
        // written individually using appropriate accessors later, which
        // would apply write barriers if necessary.
    }

    // SAFETY: We have initialized the header. Data is uninitialized but allocated.
    unsafe { mem::transmute(handle_uninit) }
}

// Re-export the closure allocation function
pub use crate::collections::closure::{parallax_alloc_closure, parallax_closure_get_function, parallax_closure_get_environment}; 