use rsgc::heap::heap::Heap;
use rsgc::prelude::*;
use rsgc::heap::{thread::Thread, region::HeapArguments};
use memoffset::offset_of;
use std::mem::{self, MaybeUninit};
use std::ptr::{self, addr_of_mut};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

// Declare modules
mod closure;
mod string;

// Re-export public items
pub use closure::*; 
pub use string::*; 

// --- Global Heap Initialization ---

/// Initializes the global GC heap and attaches the current thread.
/// Must be called by the main thread before any GC operations.
pub fn init_gc() {
    // Initialize the heap globally. We don't store the Heap instance itself.
    let args = HeapArguments::default();
    // TODO: Figure out how to set ParallaxGlobalRoots on HeapArguments or register it separately.

    // Call Heap::new directly, discarding the result, per signature `-> &'static mut Self`
    let _heap_ref = Heap::new(args);

    // Ensure the current thread is attached (calling current() might do this)
    let _ = current_thread();
    log::info!("Parallax GC Initialized and main thread attached.");
}

/// Gets the rsgc::Thread handle for the current OS thread.
pub(crate) fn current_thread() -> &'static mut Thread {
    // SAFETY: Assumes GC is initialized and thread is attached.
    Thread::current()
}

// --- Global Roots Implementation ---

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

// --- Public FFI Allocation Functions ---



// --- Public Rooting Functions (FFI and Internal) ---

thread_local! {
    static SHADOW_STACK: RefCell<Vec<*mut u8>> = RefCell::new(Vec::new());
    static GLOBAL_ROOTS: RefCell<Vec<*mut u8>> = RefCell::new(Vec::new());
}

/// FFI: Pushes a root onto the shadow stack.
#[no_mangle]
pub extern "C" fn push_shadow_stack(ptr: *mut u8) {
    if !ptr.is_null() {
        SHADOW_STACK.with(|stack| stack.borrow_mut().push(ptr));
    }
}

/// FFI: Pops roots from the shadow stack.
#[no_mangle]
pub extern "C" fn pop_shadow_stack(count: usize) {
    SHADOW_STACK.with(|stack| {
        let mut stack = stack.borrow_mut();
        let len = stack.len();
        let new_len = len.saturating_sub(count);
        stack.truncate(new_len);
    });
}

/// FFI: Registers a global root. ptr_to_handle points to the memory holding the Handle bits.
#[no_mangle]
pub extern "C" fn register_global_root(ptr_to_handle: *mut u8) {
    if !ptr_to_handle.is_null() {
        GLOBAL_ROOTS.with(|roots| {
            let mut roots = roots.borrow_mut();
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
        GLOBAL_ROOTS.with(|roots| {
            roots.borrow_mut().retain(|&p| p != ptr_to_handle);
        });
    }
}

/// Visits roots on the shadow stack (called by GC).
/// # Safety - Visitor must be valid.
unsafe fn visit_shadow_stack_roots(visitor: &mut dyn Visitor) {
    SHADOW_STACK.with(|stack| {
        let stack = stack.borrow();
        for &ptr in stack.iter() {
             visitor.visit(ptr);
        }
    });
}

/// Visits global roots (called by GC).
/// # Safety - Visitor must be valid. Assumes stored pointers are valid Handle bits.
unsafe fn visit_global_roots(visitor: &mut dyn Visitor) {
    GLOBAL_ROOTS.with(|roots| {
        let roots = roots.borrow();
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
/// # Safety - Visitor must be valid.
pub unsafe fn visit_roots(visitor: &mut dyn Visitor) {
    visit_shadow_stack_roots(visitor);
    visit_global_roots(visitor);
}

// --- Helper Functions ---

/// Checks if a HIR type might contain GC references. Used by the native backend.
/// Needs access to struct/enum definitions.
pub fn hir_type_contains_gc_handle(
    hir_type: &parallax_hir::hir::HirType,
    struct_defs: &HashMap<parallax_hir::Symbol, parallax_hir::hir::HirStructDef>, 
    enum_defs: &HashMap<parallax_hir::Symbol, parallax_hir::hir::HirEnumDef>, 
    visiting: &mut HashSet<parallax_hir::Symbol>,
) -> bool {
    use parallax_hir::hir::{HirType, ResolvePrimitiveType};

    match hir_type {
        HirType::Adt(symbol) => {
            if visiting.contains(symbol) {
                return false; // Already visiting, break cycle
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
                false // Unknown ADT, assume no handles for safety?
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
        HirType::FunctionPointer(..) => true, // Function closures capture environment handles
        HirType::Primitive(prim) => matches!(prim, ResolvePrimitiveType::String), // StringRef is a handle
        HirType::Never => false,
    }
}

// --- Old/Placeholder functions (remove or adapt) ---

/*
// This raw allocator is problematic because rsgc needs type info (Allocation trait).
// Keeping it commented out as a reminder of the need for typed allocation.
#[no_mangle]
pub extern "C" fn parallax_gc_alloc_raw(size: usize, _type_id: u64) -> *mut u8 {
    let thread = current_thread();
    let layout = match Layout::from_size_align(size, mem::align_of::<usize>()) {
        Ok(l) => l,
        Err(_) => return ptr::null_mut(),
    };
    // This is likely incorrect/unsafe for rsgc tracing.
    // let ptr = unsafe { thread.allocate_raw(layout) }; // allocate_raw might expect layout?
    // ptr
    ptr::null_mut() // Return null until a safe raw allocation strategy exists
}
*/

// REMOVED HirType Checker

// --- HirType Checker (Moved from native/backend, now internal/private if needed) ---
// Keeping this internal for now, as its public use case is unclear after refactor.
// It might be useful for assertions or internal checks. Requires HirType dependency.
/*
use parallax_hir::{Symbol, hir::{HirType, StructDefinition, EnumDefinition, ResolvePrimitiveType}};
use std::collections::{HashMap, HashSet}; // Requires parallax-hir dep

fn hir_type_contains_gc_handle(
    hir_type: &HirType,
    struct_defs: &HashMap<Symbol, StructDefinition>,
    enum_defs: &HashMap<Symbol, EnumDefinition>,
    visiting: &mut HashSet<Symbol>,
) -> bool {
    // ... implementation from previous steps ...
    // Needs adjustment if struct/enum defs aren't readily available here.
    true // Placeholder
}
*/ 