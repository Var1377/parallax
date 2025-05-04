use crate::descriptor::{DescriptorIndex, LayoutDescriptor};
use crate::{LayoutComputer, LayoutError};
use parallax_hir::hir::{HirStructDef, HirEnumDef, HirType, PrimitiveType};
use repc::layout::{Type, TypeVariant, Record, RecordField, RecordKind, BuiltinType, Annotation};
use std::collections::HashMap;
use crate::helpers::layout_contains_handles;

impl LayoutComputer {
    /// Computes layout for a struct type.
    pub(crate) fn compute_struct_layout(&mut self, struct_def: &HirStructDef) -> Result<LayoutDescriptor, LayoutError> {
        // First get descriptor indices for all fields
        let mut field_indices = Vec::with_capacity(struct_def.fields.len());
        let mut field_types = Vec::with_capacity(struct_def.fields.len());
        
        for (_, _, field_type) in &struct_def.fields {
            let descriptor_index = self.get_or_create_descriptor_index(field_type)?;
            field_indices.push(descriptor_index);
            field_types.push(field_type.clone());
        }
        
        // Create the repc struct type
        let mut fields = Vec::with_capacity(struct_def.fields.len());
        
        for (i, field_type) in field_types.iter().enumerate() {
            // Convert HirType to repc type
            let repc_type = self.hir_type_to_repc_type(field_type)?;
            
            fields.push(RecordField {
                layout: None, // Layout will be computed by repc
                annotations: vec![],
                named: true,
                bit_width: None,
                ty: repc_type,
            });
        }
        
        let struct_type = Type {
            layout: (),
            annotations: vec![],
            variant: TypeVariant::Record(Record {
                kind: RecordKind::Struct,
                fields,
            }),
        };
        
        // Compute layout using repc
        let computed_layout = repc::compute_layout(self.target, &struct_type)
            .map_err(LayoutError::RepcError)?;
        
        // Extract the field offsets from the computed layout
        let repc_record = match computed_layout.variant {
            TypeVariant::Record(record) => record,
            _ => return Err(LayoutError::Other("Expected record type".to_string())),
        };
        
        // Create field offset -> descriptor index mappings
        let mut fields_with_offsets = Vec::with_capacity(repc_record.fields.len());
        
        for (i, field) in repc_record.fields.iter().enumerate() {
            let field_layout = field.layout.unwrap(); // Safe after compute_layout
            let offset = field_layout.offset_bits / 8; // Convert bits to bytes
            fields_with_offsets.push((offset as usize, field_indices[i])); // Cast offset to usize
        }
        
        // Sort by offset (should already be in order for structs but just to be safe)
        fields_with_offsets.sort_by_key(|(offset, _)| *offset);
        
        // Calculate handle_offsets
        let mut handle_offsets = Vec::new();
        for (offset, field_desc_idx) in &fields_with_offsets {
            // Get the descriptor for the field using immutable access
            let field_descriptor = self.descriptors.get(*field_desc_idx)
                .ok_or(LayoutError::MissingDescriptor(*field_desc_idx))?;
            
            // Check if the field descriptor contains handles using immutable access
            // Pass self.descriptors immutably
            if layout_contains_handles(field_descriptor, &self.descriptors) {
                handle_offsets.push(*offset);
            }
        }

        // Create the LayoutDescriptor::Struct
        Ok(LayoutDescriptor::Struct {
            size_bytes: (computed_layout.layout.size_bits / 8) as usize, // Cast size to usize
            align_bytes: (computed_layout.layout.required_alignment_bits / 8) as usize, // Cast align to usize
            fields: fields_with_offsets.into_boxed_slice(),
            handle_offsets: handle_offsets.into_boxed_slice(), // Store calculated offsets
        })
    }
    
    /// Computes layout for an enum type.
    pub(crate) fn compute_enum_layout(&mut self, enum_def: &HirEnumDef) -> Result<LayoutDescriptor, LayoutError> {
        // Determine discriminant size based on variant count
        let variant_count = enum_def.variants.len();
        let discriminant_size_bytes = if variant_count <= 256 {
            1 // u8
        } else if variant_count <= 65536 {
            2 // u16
        } else {
            4 // u32
        };
        
        // Create a repc struct type for the enum:
        // struct Enum {
        //   discriminant: u8/u16/u32,
        //   union { variant1, variant2, ... }
        // }
        
        // First, build the union of variant payloads
        let mut variant_fields = Vec::with_capacity(variant_count);
        let mut variant_descriptor_indices: HashMap<u64, DescriptorIndex> = HashMap::new();
        
        for (i, variant) in enum_def.variants.iter().enumerate() {
            // Convert the variant's fields to a tuple type (simpler than handling named fields)
            let tuple_type = HirType::Tuple(variant.fields.clone());
            let variant_descriptor_index = self.get_or_create_descriptor_index(&tuple_type)?;
            
            // Store descriptor index mapped to discriminant value
            variant_descriptor_indices.insert(i as u64, variant_descriptor_index);
            
            // Create a repc type for this variant's payload
            let repc_type = self.hir_type_to_repc_type(&tuple_type)?;
            
            variant_fields.push(RecordField {
                layout: None,
                annotations: vec![],
                named: true,
                bit_width: None,
                ty: repc_type,
            });
        }
        
        // Create the union type
        let union_type = Type {
            layout: (),
            annotations: vec![],
            variant: TypeVariant::Record(Record {
                kind: RecordKind::Union,
                fields: variant_fields,
            }),
        };
        
        // Create the discriminant field
        let discriminant_type = match discriminant_size_bytes {
            1 => BuiltinType::UnsignedChar,
            2 => BuiltinType::UnsignedShort,
            4 => BuiltinType::UnsignedInt,
            _ => BuiltinType::UnsignedInt,
        };
        
        let discriminant_field = RecordField {
            layout: None,
            annotations: vec![],
            named: true,
            bit_width: None,
            ty: Type {
                layout: (),
                annotations: vec![],
                variant: TypeVariant::Builtin(discriminant_type),
            },
        };
        
        // Create the union field
        let union_field = RecordField {
            layout: None,
            annotations: vec![],
            named: true,
            bit_width: None,
            ty: union_type,
        };
        
        // Create the enum struct type
        let enum_type = Type {
            layout: (),
            annotations: vec![],
            variant: TypeVariant::Record(Record {
                kind: RecordKind::Struct,
                fields: vec![discriminant_field, union_field],
            }),
        };
        
        // Compute layout using repc
        let computed_layout = repc::compute_layout(self.target, &enum_type)
            .map_err(LayoutError::RepcError)?;
        
        // Extract the fields from the computed layout
        let repc_record = match computed_layout.variant {
            TypeVariant::Record(record) => record,
            _ => return Err(LayoutError::Other("Expected record type".to_string())),
        };
        
        // Get the discriminant offset
        let discriminant_field = &repc_record.fields[0];
        let discriminant_layout = discriminant_field.layout.unwrap();
        let discriminant_offset = discriminant_layout.offset_bits / 8;
        
        // Get the payload offset
        let payload_field = &repc_record.fields[1];
        let payload_layout = payload_field.layout.unwrap();
        let payload_offset = payload_layout.offset_bits / 8;
        
        // Convert variant indices to (discriminant, payload_offset, descriptor_index) tuples
        let variants: Box<[(u64, usize, DescriptorIndex)]> = variant_descriptor_indices
            .into_iter()
            .map(|(discr, desc_idx)| (discr, payload_offset as usize, desc_idx))
            .collect();
        
        // Create the LayoutDescriptor::Enum
        Ok(LayoutDescriptor::Enum {
            size_bytes: (computed_layout.layout.size_bits / 8) as usize,
            align_bytes: (computed_layout.layout.required_alignment_bits / 8) as usize,
            discriminant_offset: discriminant_offset as usize,
            discriminant_size_bytes: discriminant_size_bytes as usize,
            variants,
        })
    }
} 