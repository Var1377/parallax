use crate::layout::descriptor::{DescriptorIndex, LayoutDescriptor, DescriptorStore};
use crate::{GLOBAL_DESCRIPTOR_STORE, GcObject};
use parallax_hir::hir::{HirType, HirStructDef, HirEnumDef, PrimitiveType};
use parallax_hir::Symbol;
use thiserror::Error;
use std::collections::{HashMap, HashSet};
use std::ptr;
use rsgc::prelude::*; // Import prelude for Handle
use std::mem;
use std::fmt;
use crate::collections::string::GcByteArray;
use crate::layout::LayoutError;
use crate::CLOSURE_REF_DESCRIPTOR_INDEX;
use crate::collections::array::{GcRawArray, GcRawArrayHeader};

/// Errors that can occur during GC heap readback.
#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum ReadbackError {
    #[error("Attempted to readback from a null handle.")]
    NullHandle,
    #[error("Cycle detected during readback at handle address {0:#x}")]
    CycleDetected(usize),
    #[error("Invalid descriptor index {0} encountered during readback.")]
    InvalidDescriptorIndex(DescriptorIndex),
    #[error("Global descriptor store pointer is not set.")]
    DescriptorStoreNotSet,
    #[error("Failed to read object header or data from heap.")]
    NodeReadError, // TODO: Add more context?
    #[error("Readback for layout type is not supported: {0}")]
    UnsupportedLayout(String),
    #[error("Type mismatch during readback: {0}")]
    TypeError(String),
    #[error("Pointer read from offset {0} was null when not expected.")]
    UnexpectedNullPointer(usize),
}

/// Represents the structured data read back from the GC heap.
#[derive(Clone, PartialEq)] // Remove Eq due to f64
pub enum ReadbackTerm {
    Int(i128),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
    Struct {
        name: Option<String>, // Optional: HIR struct name
        fields: Vec<(Option<String>, ReadbackTerm)>, // Optional: HIR field name
    },
    Enum {
        name: Option<String>, // Optional: HIR enum name
        variant_name: Option<String>, // Optional: HIR variant name
        discriminant: u64, // Store the actual discriminant value
        payload: Option<Box<ReadbackTerm>>,
    },
    Array(Vec<ReadbackTerm>),
    Closure { func_ptr: usize, env: Box<ReadbackTerm> },
    OpaqueHandle(usize), // For handles we can't or don't fully interpret
    CycleMarker(usize), // Placeholder inserted when a cycle is hit
    Unit,
    // Maybe remove this variant and let the caller handle the Result?
    // Error(ReadbackError),
}

// Manual Debug implementation for better formatting, especially for structs/enums/floats
impl fmt::Debug for ReadbackTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ReadbackTerm::Int(v) => write!(f, "{}", v),
            ReadbackTerm::Float(v) => write!(f, "{}", v),
            ReadbackTerm::Bool(v) => write!(f, "{}", v),
            ReadbackTerm::Char(v) => write!(f, "'{}'", v.escape_default()),
            ReadbackTerm::String(v) => write!(f, "\"{}\"", v.escape_default()),
            ReadbackTerm::Struct { name, fields } => {
                let struct_name = name.as_deref().unwrap_or("_");
                if fields.is_empty() {
                    write!(f, "{}", struct_name)
                } else {
                    write!(f, "{} {{ ", struct_name)?;
                    for (i, (field_name, field_value)) in fields.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        if let Some(n) = field_name {
                            write!(f, "{}: {:?}", n, field_value)?;
                        } else {
                            write!(f, "{:?}", field_value)?;
                        }
                    }
                    write!(f, " }}")
                }
            }
            ReadbackTerm::Enum { name, variant_name, discriminant, payload } => {
                let enum_name = name.as_deref().unwrap_or("Enum");
                let default_variant_name = format!("Variant{}", discriminant);
                let variant_name = variant_name.as_deref().unwrap_or(&default_variant_name);
                write!(f, "{}::{}", enum_name, variant_name)?;
                if let Some(p) = payload {
                    write!(f, "({:?})", p)?;
                }
                Ok(())
            }
            ReadbackTerm::Array(elements) => f.debug_list().entries(elements).finish(),
            ReadbackTerm::Closure { func_ptr, env } => {
                f.debug_struct("Closure")
                    .field("func_ptr", &format_args!("{:#x}", func_ptr))
                    .field("env", env)
                    .finish()
            }
            ReadbackTerm::OpaqueHandle(addr) => write!(f, "Handle({:#x})", addr),
            ReadbackTerm::CycleMarker(addr) => write!(f, "<Cycle @ {:#x}>", addr),
            ReadbackTerm::Unit => write!(f, "()"),
        }
    }
}

/// Public entry point for reading back a GC object.
pub fn readback_gc(
    handle_raw: usize, // Raw pointer/usize from JIT
    expected_hir_type: &HirType,
    struct_defs: &HashMap<Symbol, HirStructDef>,
    enum_defs: &HashMap<Symbol, HirEnumDef>,
) -> Result<ReadbackTerm, ReadbackError> {
    if handle_raw == 0 {
        return Err(ReadbackError::NullHandle);
    }
    let mut visited = HashSet::new();
    // Safety: readback_recursive is unsafe due to pointer deref and static access
    unsafe {
        readback_recursive(
            handle_raw,
            expected_hir_type,
            struct_defs,
            enum_defs,
            &mut visited,
        )
    }
}

/// Recursive helper function for readback.
unsafe fn readback_recursive(
    handle_raw: usize,
    expected_hir_type: &HirType,
    struct_defs: &HashMap<Symbol, HirStructDef>,
    enum_defs: &HashMap<Symbol, HirEnumDef>,
    visited: &mut HashSet<usize>,
) -> Result<ReadbackTerm, ReadbackError> {
    // --- String Check ---
    if let HirType::Primitive(PrimitiveType::String) = expected_hir_type {
        // Assume handle_raw points to a GcByteArray
        let byte_array_ptr = handle_raw as *const GcByteArray;
        if byte_array_ptr.is_null() {
            return Err(ReadbackError::NullHandle);
        }
        // Use the public accessors
        // SAFETY: byte_array_ptr is valid (checked non-null), accessors rely on valid ptr.
        let byte_array_ref: &GcByteArray = &*byte_array_ptr;
        let byte_slice = byte_array_ref.data_slice();

        // Convert to String (handle potential UTF-8 error)
        match String::from_utf8(byte_slice.to_vec()) {
            Ok(s) => {
                return Ok(ReadbackTerm::String(s));
            }
            Err(e) => {
                return Err(ReadbackError::TypeError(format!("Invalid UTF-8 sequence in GcByteArray: {}", e)));
            }
        }
    }
    // --- End String Check ---

    // --- Cycle Detection ---
    if !visited.insert(handle_raw) {
        return Ok(ReadbackTerm::CycleMarker(handle_raw));
    }

    // --- Core Steps --- 
    let obj_ptr = handle_raw as *const GcObject;
    if obj_ptr.is_null() {
        visited.remove(&handle_raw);
        return Err(ReadbackError::NullHandle); // Should have been caught earlier, but check again
    }

    // Read descriptor index
    // SAFETY: Assumes obj_ptr is valid and aligned.
    let descriptor_index = ptr::read(ptr::addr_of!((*obj_ptr).descriptor_index));

    // Access descriptor store
    // SAFETY: Accessing static mut.
    let descriptor_store_ptr = GLOBAL_DESCRIPTOR_STORE;
    // Check if the *pointer* is null
    if descriptor_store_ptr.is_null() {
        visited.remove(&handle_raw);
        return Err(ReadbackError::DescriptorStoreNotSet);
    }
    // SAFETY: Assumes store_ptr is valid (checked above).
    let descriptor_store = &*descriptor_store_ptr;

    // Get descriptor
    let descriptor = descriptor_store.descriptors.get(descriptor_index).ok_or(ReadbackError::InvalidDescriptorIndex(descriptor_index))?;

    // Get data pointer using offset (since obj_ref approach didn't work)
    // SAFETY: Assumes obj_ptr is valid and points to a GcObject.
    let data_start_ptr = (obj_ptr as *const u8).add(GcObject::VARSIZE_OFFSETOF_VARPART);

    // --- Special case: ClosureRef ---
    if descriptor_index == CLOSURE_REF_DESCRIPTOR_INDEX.expect("CLOSURE_REF_DESCRIPTOR_INDEX not initialized") {
        let func_ptr = ptr::read(data_start_ptr as *const usize);
        let env_handle_ptr_offset = mem::size_of::<usize>();
        let env_handle_raw = ptr::read(data_start_ptr.add(env_handle_ptr_offset) as *const usize);

        // Read back environment as Opaque handle for now
        let env_term = if env_handle_raw == 0 {
            ReadbackTerm::Unit // Env can be unit for non-capturing closures
        } else {
            // We don't know the env type, return Opaque
            ReadbackTerm::OpaqueHandle(env_handle_raw)
            // TODO: Later, get env type and call recursively:
            // readback_recursive(env_handle_raw, &env_hir_type, struct_defs, enum_defs, visited)?
        };

        visited.remove(&handle_raw);
        return Ok(ReadbackTerm::Closure { func_ptr, env: Box::new(env_term) });
    }
    // --- End ClosureRef --- 

    let result = match descriptor {
        LayoutDescriptor::Primitive { .. } => {
            // We need the *specific* primitive type from expected_hir_type
            if let HirType::Primitive(prim_ty) = expected_hir_type {
                 read_primitive_from_bytes(data_start_ptr, prim_ty)
            } else if let HirType::Tuple(elements) = expected_hir_type { // Handle Unit encoded as Tuple([])
                 if elements.is_empty() {
                      Ok(ReadbackTerm::Unit)
                 } else {
                      Err(ReadbackError::TypeError(format!("Expected Primitive or Unit, got {:?}", expected_hir_type)))
                 }
            } else {
                 Err(ReadbackError::TypeError(format!("Layout is Primitive, but expected HIR type is {:?}", expected_hir_type)))
            }
        }
        LayoutDescriptor::Handle => {
            // This describes the layout *of the GcObject itself*.
            // If a GcObject's layout is just "Handle", it means the object's data
            // *is* a raw handle (usize). Read it and return as Opaque.
            let nested_handle_raw = ptr::read(data_start_ptr as *const usize);
            Ok(ReadbackTerm::OpaqueHandle(nested_handle_raw))
        }
        LayoutDescriptor::Struct { fields, .. } => {
            let struct_symbol = if let HirType::Adt(sym) = expected_hir_type {
                *sym
            } else {
                // Cannot determine struct definition without the symbol
                return Err(ReadbackError::TypeError(format!("Expected ADT type for Struct layout, got {:?}", expected_hir_type)));
            };

            let struct_def = struct_defs.get(&struct_symbol)
                .ok_or_else(|| ReadbackError::TypeError(format!("Struct definition {:?} not found", struct_symbol)))?;

            let mut read_fields = Vec::with_capacity(fields.len());

            // Assuming descriptor `fields` order matches `struct_def.fields` order
            if fields.len() != struct_def.fields.len() {
                 return Err(ReadbackError::TypeError(format!("Layout descriptor field count ({}) mismatch with HIR struct def field count ({}) for {:?}", fields.len(), struct_def.fields.len(), struct_symbol)));
            }

            for (field_idx, (field_offset, field_desc_index)) in fields.iter().enumerate() {
                let (_field_symbol, field_name, field_hir_type) = &struct_def.fields[field_idx];

                let field_data_ptr = data_start_ptr.add(*field_offset);
                let field_descriptor = descriptor_store.descriptors.get(*field_desc_index)
                    .ok_or_else(|| ReadbackError::InvalidDescriptorIndex(*field_desc_index))?;

                // Recursively read the field based on its descriptor and expected HIR type
                let field_term = match field_descriptor {
                     LayoutDescriptor::Primitive { .. } |
                     LayoutDescriptor::Struct { .. } |
                     LayoutDescriptor::Enum { .. } |
                     LayoutDescriptor::Array { .. } => {
                         // These need a handle. Read the handle from the field's data ptr.
                         let field_handle_raw = ptr::read(field_data_ptr as *const usize);
                         if field_handle_raw == 0 {
                             // How to represent null handle field? Unit? Error?
                             ReadbackTerm::OpaqueHandle(0) // Or Unit?
                         } else {
                            readback_recursive(field_handle_raw, field_hir_type, struct_defs, enum_defs, visited)?
                         }
                     }
                    LayoutDescriptor::Handle => {
                         // Similar to above, read the handle stored at field_data_ptr
                         let field_handle_raw = ptr::read(field_data_ptr as *const usize);
                         if field_handle_raw == 0 {
                             ReadbackTerm::OpaqueHandle(0)
                         } else {
                             // We need the *target type* of the handle (field_hir_type)
                            readback_recursive(field_handle_raw, field_hir_type, struct_defs, enum_defs, visited)?
                         }
                     }
                };

                read_fields.push((Some(field_name.clone()), field_term));
            }

            Ok(ReadbackTerm::Struct { name: Some(struct_def.name.clone()), fields: read_fields })
        }
        LayoutDescriptor::Enum { discriminant_offset, discriminant_size_bytes, variants, .. } => {
            let enum_symbol = if let HirType::Adt(sym) = expected_hir_type {
                *sym
            } else {
                return Err(ReadbackError::TypeError(format!("Expected ADT type for Enum layout, got {:?}", expected_hir_type)));
            };
            let enum_def = enum_defs.get(&enum_symbol)
                .ok_or_else(|| ReadbackError::TypeError(format!("Enum definition {:?} not found", enum_symbol)))?;

            // 1. Read discriminant
            let discriminant_ptr = data_start_ptr.add(*discriminant_offset);
            let discriminant_value: u64 = match *discriminant_size_bytes {
                1 => ptr::read(discriminant_ptr as *const u8) as u64,
                2 => ptr::read(discriminant_ptr as *const u16) as u64,
                4 => ptr::read(discriminant_ptr as *const u32) as u64,
                8 => ptr::read(discriminant_ptr as *const u64),
                _ => return Err(ReadbackError::TypeError(format!("Invalid discriminant size {} for enum {:?}", discriminant_size_bytes, enum_symbol))),
            };

            // 2. Find matching variant in descriptor
            let variant_info = variants.iter().find(|(v, _, _)| *v == discriminant_value)
                 .ok_or_else(|| ReadbackError::TypeError(format!("Discriminant value {} not found in layout descriptor variants for enum {:?}", discriminant_value, enum_symbol)))?;
            let (_val, payload_offset, payload_desc_idx) = variant_info;

            // 3. Find matching variant in HIR definition (for names and payload type)
            // Assuming order matches layout descriptor or using discriminant value as index? Using discriminant as index might be safer.
            let variant_def = enum_def.variants.get(discriminant_value as usize)
                 .ok_or_else(|| ReadbackError::TypeError(format!("Discriminant value {} out of bounds for HIR enum def variants for enum {:?}", discriminant_value, enum_symbol)))?;

            // 4. Read payload if it exists
            let payload_term = {
                let payload_descriptor = descriptor_store.descriptors.get(*payload_desc_idx)
                    .ok_or_else(|| ReadbackError::InvalidDescriptorIndex(*payload_desc_idx))?;

                // Check if payload is effectively Unit (e.g., Primitive layout with size 0)
                let payload_is_unit = match payload_descriptor {
                    LayoutDescriptor::Primitive { size_bytes, .. } => *size_bytes == 0,
                    // Handle empty tuple represented as struct
                    LayoutDescriptor::Struct { fields, .. } => fields.is_empty(), 
                    _ => false,
                };

                if payload_is_unit || variant_def.fields.is_empty() {
                    None // No payload or Unit payload
                } else {
                    let payload_data_ptr = data_start_ptr.add(*payload_offset);
                    let expected_payload_hir_type = variant_def.fields.get(0) // Assume single payload field for now
                         .ok_or_else(|| ReadbackError::TypeError(format!("Enum variant {}::{} has payload descriptor but no HIR field", enum_def.name, variant_def.name)))?;

                    // Read handle for the payload
                    let payload_handle_raw = ptr::read(payload_data_ptr as *const usize);
                    if payload_handle_raw == 0 {
                         // Represent null payload handle? Error?
                         return Err(ReadbackError::UnexpectedNullPointer(*payload_offset));
                    }
                    
                    let read_payload = readback_recursive(payload_handle_raw, expected_payload_hir_type, struct_defs, enum_defs, visited)?;
                    Some(Box::new(read_payload))
                }
            };

            Ok(ReadbackTerm::Enum {
                 name: Some(enum_def.name.clone()),
                 variant_name: Some(variant_def.name.clone()),
                 discriminant: discriminant_value,
                 payload: payload_term,
             })
        }
        LayoutDescriptor::Array { element_descriptor_index, element_stride_bytes, .. } => {
            let array_ptr = handle_raw as *const GcRawArray;
            let header_ptr = ptr::addr_of!((*array_ptr).header);
            let header = ptr::read(header_ptr);
            let len = header.len as usize;

            let element_descriptor = descriptor_store.descriptors.get(*element_descriptor_index)
                 .ok_or_else(|| ReadbackError::InvalidDescriptorIndex(*element_descriptor_index))?;

            let expected_element_hir_type = if let HirType::Array(elem_ty, _) = expected_hir_type {
                &**elem_ty
            } else {
                return Err(ReadbackError::TypeError(format!("Expected Array type for Array layout, got {:?}", expected_hir_type)));
            };

            let mut elements = Vec::with_capacity(len);
            let stride = *element_stride_bytes;
            let elements_data_start_ptr = (array_ptr as *const u8).add(mem::size_of::<GcRawArrayHeader>());

            for i in 0..len {
                let element_data_ptr = elements_data_start_ptr.add(i * stride);

                let element_term = match element_descriptor {
                     LayoutDescriptor::Primitive { .. } => {
                         if let HirType::Primitive(prim_ty) = expected_element_hir_type {
                            read_primitive_from_bytes(element_data_ptr, prim_ty)
                         } else {
                            Err(ReadbackError::TypeError("Expected primitive element type for Primitive layout".to_string()))
                         }
                     }
                     LayoutDescriptor::Handle |
                     LayoutDescriptor::Struct { .. } |
                     LayoutDescriptor::Enum { .. } |
                     LayoutDescriptor::Array { .. } => {
                         let element_handle_raw = ptr::read(element_data_ptr as *const usize);
                         if element_handle_raw == 0 {
                             Ok(ReadbackTerm::OpaqueHandle(0)) // Or Unit?
                         } else {
                             readback_recursive(element_handle_raw, expected_element_hir_type, struct_defs, enum_defs, visited)
                         }
                     }
                }?; // Propagate errors

                elements.push(element_term);
            }
            Ok(ReadbackTerm::Array(elements))
        }

        // TODO: Handle other layouts (ClosureRef, etc.)
    };

    // Cleanup visited set
    visited.remove(&handle_raw);

    result
}

/// Reads a primitive value from raw bytes based on the expected PrimitiveType.
unsafe fn read_primitive_from_bytes(
    ptr: *const u8,
    prim_ty: &PrimitiveType
) -> Result<ReadbackTerm, ReadbackError> {
    // SAFETY: Caller ensures ptr is valid and aligned for the type.
    match prim_ty {
        PrimitiveType::I8   => Ok(ReadbackTerm::Int(ptr::read(ptr as *const i8) as i128)),
        PrimitiveType::I16  => Ok(ReadbackTerm::Int(ptr::read(ptr as *const i16) as i128)),
        PrimitiveType::I32  => Ok(ReadbackTerm::Int(ptr::read(ptr as *const i32) as i128)),
        PrimitiveType::I64  => Ok(ReadbackTerm::Int(ptr::read(ptr as *const i64) as i128)),
        PrimitiveType::I128 => Ok(ReadbackTerm::Int(ptr::read(ptr as *const i128))),
        PrimitiveType::U8   => Ok(ReadbackTerm::Int(ptr::read(ptr as *const u8) as i128)),
        PrimitiveType::U16  => Ok(ReadbackTerm::Int(ptr::read(ptr as *const u16) as i128)),
        PrimitiveType::U32  => Ok(ReadbackTerm::Int(ptr::read(ptr as *const u32) as i128)),
        PrimitiveType::U64  => Ok(ReadbackTerm::Int(ptr::read(ptr as *const u64) as i128)),
        PrimitiveType::U128 => Ok(ReadbackTerm::Int(ptr::read(ptr as *const u128) as i128)), // Read as u128, store as i128
        PrimitiveType::F32  => Ok(ReadbackTerm::Float(ptr::read(ptr as *const f32) as f64)),
        PrimitiveType::F64  => Ok(ReadbackTerm::Float(ptr::read(ptr as *const f64))),
        PrimitiveType::Bool => Ok(ReadbackTerm::Bool(ptr::read(ptr as *const u8) != 0)),
        PrimitiveType::Char => {
            let val = ptr::read(ptr as *const u32);
            match std::char::from_u32(val) {
                Some(c) => Ok(ReadbackTerm::Char(c)),
                None => Err(ReadbackError::TypeError(format!("Invalid char value read: {}", val))),
            }
        }
        PrimitiveType::Unit => Ok(ReadbackTerm::Unit),
        PrimitiveType::String => {
            Err(ReadbackError::TypeError("Attempted to read String as raw primitive bytes".to_string()))
        }
    }
} 