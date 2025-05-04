//!
//! Defines the context structure used during the lowering of a single function,
//! along with helper methods for accessing HIR/MIR information.

use super::*;
use parallax_hir::hir::PrimitiveType as HirPrimitiveType;
use parallax_hir::hir::{ HirType, PrimitiveType };
use parallax_layout::{DescriptorStore, DescriptorIndex};
use std::collections::HashMap;
use parallax_resolve::types::Symbol;

/// Context for lowering a single function.
///
/// Manages the state needed to build the `MirGraph` for a function, including:
/// - Access to the overall HIR module ([`HirModule`]).
/// - Access to the pre-computed descriptor store ([`DescriptorStore`]).
/// - Access to the pre-computed layout index maps.
/// - The [`MirGraph`] being constructed.
/// - Mapping from HIR variables ([`HirVar`]) to the MIR node and port index
///   that produces their value (`NodeId`, `PortIndex`).
/// - The symbol ([`Symbol`]) of the function currently being lowered.
/// - Mutable access to the map tracking closure specializations ([`ClosureSpecialization`]).
/// - If lowering a specialized closure, a map from captured HIR operands ([`Operand`])
///   to the index of the corresponding parameter added to the specialized function.
pub(super) struct FunctionLoweringContext<'ctx> {
    pub(super) hir_module: &'ctx HirModule,
    pub(super) descriptor_store: &'ctx DescriptorStore,
    pub(super) adt_index_map: &'ctx HashMap<Symbol, DescriptorIndex>,
    pub(super) primitive_index_map: &'ctx HashMap<PrimitiveType, DescriptorIndex>,
    pub(super) tuple_index_map: &'ctx HashMap<Vec<HirType>, DescriptorIndex>,
    pub(super) array_index_map: &'ctx HashMap<(HirType, usize), DescriptorIndex>,
    pub(super) mir_graph: MirGraph,
    pub(super) var_map: HashMap<HirVar, (NodeId, PortIndex)>,
    pub(super) current_func_symbol: Symbol,
    /// Map from original lambda Symbol to its specialization details.
    pub(super) closure_spec_map: &'ctx mut HashMap<Symbol, ClosureSpecialization>,
    /// If lowering a specialized function, maps captured operands to their parameter index.
    pub(super) captured_operand_map: HashMap<Operand, u32>,
}

impl<'ctx> FunctionLoweringContext<'ctx> {
    /// Creates a new lowering context for a specific function.
    ///
    /// Initializes the `MirGraph` with the given `func_symbol` and sets up
    /// the necessary references and maps for the lowering process.
    ///
    /// # Arguments
    /// * `hir_module` - Reference to the containing HIR module.
    /// * `func_symbol` - The symbol of the function/closure being lowered.
    /// * `descriptor_store` - Reference to the pre-computed descriptor store.
    /// * `adt_index_map`, `primitive_index_map`, etc. - References to the pre-computed layout index maps.
    /// * `closure_spec_map` - Mutable reference to the closure specialization map.
    /// * `captured_operand_map` - Map of captured HIR operands to their parameter indices
    ///   (empty for non-specialized functions).
    #[allow(clippy::too_many_arguments)] // Temporarily allow while refactoring
    pub(super) fn new(
        hir_module: &'ctx HirModule,
        func_symbol: Symbol,
        descriptor_store: &'ctx DescriptorStore,
        adt_index_map: &'ctx HashMap<Symbol, DescriptorIndex>,
        primitive_index_map: &'ctx HashMap<PrimitiveType, DescriptorIndex>,
        tuple_index_map: &'ctx HashMap<Vec<HirType>, DescriptorIndex>,
        array_index_map: &'ctx HashMap<(HirType, usize), DescriptorIndex>,
        closure_spec_map: &'ctx mut HashMap<Symbol, ClosureSpecialization>,
        captured_operand_map: HashMap<Operand, u32>,
    ) -> Self {
        Self {
            hir_module,
            descriptor_store,
            adt_index_map,
            primitive_index_map,
            tuple_index_map,
            array_index_map,
            mir_graph: MirGraph::new(func_symbol),
            var_map: HashMap::new(),
            current_func_symbol: func_symbol,
            closure_spec_map,
            captured_operand_map,
        }
    }

    /// Lowers an HIR type ([`HirType`]) to its corresponding MIR type ([`MirType`]).
    ///
    /// Delegates to the `lower_hir_type_to_mir_type` function in the `types` module.
    pub(super) fn lower_type(&self, hir_type: &HirType) -> MirType {
        // Call the function from its new location
        types::lower_hir_type_to_mir_type(hir_type)
    }

    /// Determines the HIR type ([`HirType`]) of a given HIR literal ([`HirLiteral`]).
    ///
    /// Returns the corresponding primitive or tuple type.
    pub(super) fn get_literal_type(&self, literal: &HirLiteral) -> HirType {
        match literal {
            HirLiteral::IntLiteral { ty, .. } => HirType::Primitive(*ty),
            HirLiteral::FloatLiteral { ty, .. } => HirType::Primitive(*ty),
            HirLiteral::StringLiteral(_) => HirType::Primitive(HirPrimitiveType::String),
            HirLiteral::BoolLiteral(_) => HirType::Primitive(HirPrimitiveType::Bool),
            HirLiteral::CharLiteral(_) => HirType::Primitive(HirPrimitiveType::Char),
            HirLiteral::Unit => HirType::Tuple(vec![]),
        }
    }

    /// Gets the MIR type ([`MirType`]) produced by a specific output port of a node.
    ///
    /// Retrieves the specified node from the `mir_graph` and determines the type
    /// of the value produced at the given `port_index` based on the node type.
    ///
    /// # Errors
    /// Returns `LoweringError::Internal` if the `node_id` is invalid or if the `port_index`
    /// is invalid for the node type (e.g., accessing port 2 of a `Closure` node).
    /// Returns `LoweringError::TypeMismatch` if the node is a `FunctionCall` but its
    /// associated `func_ty` is not a `MirType::FunctionPointer`.
    pub(super) fn get_port_type(&self, node_id: NodeId, port_index: PortIndex) -> Result<MirType, LoweringError> {
        let node = self.mir_graph.nodes.get(&node_id)
             // Propagate error if node doesn't exist (shouldn't happen in valid graph)
            .ok_or_else(|| LoweringError::Internal(format!("Node {:?} not found during type lookup", node_id)))?; 
        match node {
            MirNode::Parameter { ty, .. } => Ok(ty.clone()),
            MirNode::Constant { ty, .. } => Ok(ty.clone()),
            MirNode::StaticAddr { ty, .. } => Ok(ty.clone()),
            MirNode::Constructor { ty, .. } => Ok(ty.clone()),
            MirNode::Project { field_ty, .. } => Ok(field_ty.clone()),
            MirNode::FunctionCall { func_ty, .. } => {
                match func_ty {
                    MirType::FunctionPointer(_, ret_ty) => Ok(ret_ty.as_ref().clone()),
                     // Use specific error type
                    _ => Err(LoweringError::TypeMismatch("FunctionCall node must have FunctionPointer type".to_string())),
                }
            }
            MirNode::IfValue { ty, .. } => Ok(ty.clone()),
            // Unreachable nodes don't produce a value, so requesting their type is an error.
             MirNode::Unreachable => Err(LoweringError::Internal("Attempted to get type of Unreachable node".to_string())), 
            // Array nodes produce values of specific types.
            MirNode::ArrayConstruct { element_ty, size } => Ok(MirType::Array(Arc::new(element_ty.clone()), Some(*size))),
            MirNode::ArrayProject { element_ty, .. } => Ok(element_ty.clone()),
            MirNode::Closure { env_ty, func_ptr_ty, .. } => {
                match port_index {
                    PortIndex(0) => Ok(env_ty.clone()),       // Port 0 outputs the environment tuple
                    PortIndex(1) => Ok(func_ptr_ty.clone()), // Port 1 outputs the function pointer
                    _ => Err(LoweringError::Internal(format!(
                        "Invalid port index {} requested for Closure node {:?}",
                        port_index.0,
                        node_id
                    ))),
                }
            }
        }
    }

    /// Gets the MIR return type ([`MirType`]) of a function identified by its symbol.
    ///
    /// Looks up the function in the `hir_module`, lowers its signature, and extracts
    /// the return type from the resulting `MirType::FunctionPointer`.
    ///
    /// # Errors
    /// Returns `LoweringError::Internal` if the function symbol is not found.
    /// Returns `LoweringError::TypeMismatch` if the symbol corresponds to something
    /// that is not a function (should not happen if HIR is well-formed).
    pub(super) fn get_function_return_type(&self, func_symbol: Symbol) -> Result<MirType, LoweringError> {
        self.get_function_type(func_symbol)? // Use ? to propagate None error from get_function_type
            .map(|ft| match ft {
                MirType::FunctionPointer(_, ret) => Ok(ret.as_ref().clone()),
                 _ => Err(LoweringError::TypeMismatch(format!("Expected function symbol {:?} to have FunctionPointer type", func_symbol))),
            })
             // If get_function_type returned Ok(None)
            .ok_or_else(|| LoweringError::Internal(format!("Function {:?} not found for return type lookup", func_symbol)))?
    }

    /// Gets the MIR function pointer type ([`MirType::FunctionPointer`]) for a function symbol.
    ///
    /// Searches the `hir_module` for the function definition, lowers its parameter
    /// and return types, and constructs the corresponding `MirType::FunctionPointer`.
    ///
    /// Returns `Ok(None)` if the function symbol is not found in the module.
    ///
    /// # Errors
    /// Propagates errors from `lower_type` if type lowering fails (unlikely for well-formed HIR).
    pub(super) fn get_function_type(&self, func_symbol: Symbol) -> Result<Option<MirType>, LoweringError> {
        Ok(self.hir_module
            .functions
            .iter()
            .find(|f| f.symbol == func_symbol)
            .map(|f| {
                let params = f.signature.params.iter()
                    .map(|(_, ty)| self.lower_type(ty))
                    .collect();
                let ret = Arc::new(self.lower_type(&f.signature.return_type));
                MirType::FunctionPointer(params, ret)
            }))
    }

    /// Gets the MIR type ([`MirType`]) of a global item (function or static) by its symbol.
    ///
    /// First attempts to find a function with the symbol using `get_function_type`.
    /// If not found, searches the `hir_module`'s statics for a matching symbol and lowers its type.
    ///
    /// # Errors
    /// Returns `LoweringError::Internal` if neither a function nor a static with the
    /// given symbol is found in the module, nor an intrinsic.
    pub(super) fn get_global_type(&self, symbol: Symbol) -> Result<MirType, LoweringError> {
        // 1. Check regular functions
        if let Ok(Some(func_type)) = self.get_function_type(symbol) {
            return Ok(func_type);
        }

        // 2. Check statics
        if let Some(static_def) = self.hir_module.statics.iter().find(|s| s.symbol == symbol) {
            return Ok(self.lower_type(&static_def.ty));
        }

        // 3. Check intrinsics
        if let Some((intrinsic_path, _)) = self.hir_module.intrinsics.iter().find(|(_, s)| *s == symbol) {
            // Attempt to infer intrinsic type from path (basic version for tests)
            return infer_intrinsic_type_from_path(intrinsic_path);
        }

        // Not found anywhere
        Err(LoweringError::Internal(format!(
            "Failed to find global function, static, or intrinsic {:?} for type lookup",
            symbol
        )))
    }

    /// Gets the 0-based index of a field within a struct definition by its symbol.
    ///
    /// # Arguments
    /// * `aggregate_ty` - The [`MirType::Adt`] of the struct.
    /// * `field_symbol` - The [`Symbol`] of the field to find.
    ///
    /// # Errors
    /// Returns `LoweringError::TypeMismatch` if `aggregate_ty` is not `MirType::Adt`.
    /// Returns `LoweringError::Unsupported` if the ADT symbol refers to an enum (direct field access on enums is not supported).
    /// Returns `LoweringError::Internal` if the ADT symbol is not found in the module or if the `field_symbol` is not found within the struct.
    pub(super) fn get_field_index(&self, aggregate_ty: &MirType, field_symbol: Symbol) -> Result<u32, LoweringError> {
        let adt_symbol = match aggregate_ty {
            MirType::Adt(s) => *s,
             _ => return Err(LoweringError::TypeMismatch("Cannot get field index from non-ADT type".to_string())),
        };
        if let Some(struct_def) = self.hir_module.structs.iter().find(|s| s.symbol == adt_symbol) {
            struct_def.fields.iter().position(|(sym, _, _)| *sym == field_symbol)
                 .map(|pos| Ok(pos as u32))
                 // Use specific error
                .ok_or_else(|| LoweringError::Internal(format!("Field symbol {:?} not found in struct definition {:?}", field_symbol, adt_symbol)))?
        } else if self.hir_module.enums.iter().any(|e| e.symbol == adt_symbol) {
             // Field access on enums requires downcast first
             Err(LoweringError::Unsupported("Direct field access on enum type (use Downcast first)".to_string()))
        } else {
             Err(LoweringError::Internal(format!("ADT symbol {:?} not found for field index lookup", adt_symbol)))
        }
    }

    /// Gets the MIR type ([`MirType`]) of a field within an aggregate type (struct or tuple) by its 0-based index.
    ///
    /// # Arguments
    /// * `aggregate_ty` - The [`MirType`] of the struct (`MirType::Adt`) or tuple (`MirType::Tuple`).
    /// * `field_index` - The 0-based index of the field.
    ///
    /// # Errors
    /// Returns `LoweringError::TypeMismatch` if `aggregate_ty` is not an ADT or Tuple.
    /// Returns `LoweringError::Unsupported` if `aggregate_ty` is an enum ADT.
    /// Returns `LoweringError::Internal` if the ADT symbol is not found, or if the `field_index` is out of bounds for the struct/tuple.
    pub(super) fn get_field_type(&self, aggregate_ty: &MirType, field_index: u32) -> Result<MirType, LoweringError> {
         match aggregate_ty {
            MirType::Adt(s) => {
                if let Some(struct_def) = self.hir_module.structs.iter().find(|sd| sd.symbol == *s) {
                    if let Some((_, _, ty)) = struct_def.fields.get(field_index as usize) {
                        Ok(self.lower_type(ty))
                    } else {
                         Err(LoweringError::Internal(format!("Field index {} out of bounds for struct {:?}", field_index, s)))
                    }
                } else if self.hir_module.enums.iter().any(|ed| ed.symbol == *s) {
                    // Field access on enums requires downcast first
                    Err(LoweringError::Unsupported("Direct field type lookup on enum type (use Downcast first)".to_string()))
                } else {
                    Err(LoweringError::Internal(format!("ADT symbol {:?} not found for field type lookup", s)))
                }
            }
            MirType::Tuple(types) => {
                 if let Some(ty) = types.get(field_index as usize) {
                     Ok(ty.clone())
                 } else {
                      Err(LoweringError::Internal(format!("Tuple index {} out of bounds for tuple type", field_index)))
                 }
            }
             _ => Err(LoweringError::TypeMismatch("Cannot get field type from non-aggregate type".to_string())),
        }
    }

    /// Finds the parent enum symbol for a given variant symbol.
    ///
    /// Iterates through the enum definitions in the `hir_module` to find which one contains
    /// the `variant_symbol`.
    ///
    /// # Errors
    /// Returns `LoweringError::Internal` if no enum definition containing the `variant_symbol` is found.
    pub(super) fn get_enum_symbol_for_variant(&self, variant_symbol: Symbol) -> Result<Symbol, LoweringError> {
        for enum_def in &self.hir_module.enums {
            if enum_def.variants.iter().any(|v| v.symbol == variant_symbol) {
                 return Ok(enum_def.symbol);
            }
        }
          Err(LoweringError::Internal(format!("Enum symbol not found for variant symbol {:?}", variant_symbol)))
    }
}

// --- Helper function for intrinsic type inference ---
// NOTE: This is a simplified version for testing purposes. A real implementation
// would likely involve a more robust parsing mechanism shared with type checking/resolve.
fn infer_intrinsic_type_from_path(path: &str) -> Result<MirType, LoweringError> {
    // Example: "std::num::__intrinsic_i32_add__" -> fn(i32, i32) -> i32
    // Example: "std::num::__intrinsic_i32_lt__" -> fn(i32, i32) -> bool
    if path.contains("i32_add") {
        let i32_ty = MirType::Primitive(ResolvePrimitiveType::I32);
        Ok(MirType::FunctionPointer(vec![i32_ty.clone(), i32_ty.clone()], Arc::new(i32_ty)))
    } else if path.contains("i32_lt") {
        let i32_ty = MirType::Primitive(ResolvePrimitiveType::I32);
        let bool_ty = MirType::Primitive(ResolvePrimitiveType::Bool);
        Ok(MirType::FunctionPointer(vec![i32_ty.clone(), i32_ty], Arc::new(bool_ty)))
    }
    // Add case for i64_add
    else if path.contains("i64_add") {
        let i64_ty = MirType::Primitive(ResolvePrimitiveType::I64);
        Ok(MirType::FunctionPointer(vec![i64_ty.clone(), i64_ty.clone()], Arc::new(i64_ty)))
    }
    // Add more cases as needed for other intrinsics used in tests
    else {
         Err(LoweringError::Unsupported(format!("Cannot infer MIR type for intrinsic path: {}", path)))
    }
} 