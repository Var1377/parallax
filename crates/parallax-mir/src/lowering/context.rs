//!
//! Defines the context structure used during the lowering of a single function,
//! along with helper methods for accessing HIR/MIR information.

use super::*;

/// Context for lowering a single function.
///
/// Manages the state needed to build the `MirGraph` for a function, including:
/// - Access to the overall HIR module.
/// - Layout computation utilities.
/// - The `MirGraph` being constructed.
/// - Mapping from `HirVar` to the producing `(NodeId, PortIndex)` in the MIR graph.
/// - Information about closure specializations.
/// - Mapping for captured variables in specialized closures.
pub(super) struct FunctionLoweringContext<'a> {
    pub(super) hir_module: &'a HirModule,
    pub(super) layout_computer: &'a mut LayoutComputer,
    pub(super) mir_graph: MirGraph,
    pub(super) var_map: HashMap<HirVar, (NodeId, PortIndex)>,
    pub(super) current_func_symbol: Symbol,
    /// Map from original lambda Symbol to its specialization details.
    pub(super) closure_spec_map: &'a mut HashMap<Symbol, ClosureSpecialization>,
    /// If lowering a specialized function, maps captured operands to their parameter index.
    pub(super) captured_operand_map: HashMap<Operand, u32>,
}

impl<'a> FunctionLoweringContext<'a> {
    /// Creates a new lowering context for a specific function.
    pub(super) fn new(
        hir_module: &'a HirModule,
        func_symbol: Symbol,
        layout_computer: &'a mut LayoutComputer,
        closure_spec_map: &'a mut HashMap<Symbol, ClosureSpecialization>,
        captured_operand_map: HashMap<Operand, u32>,
    ) -> Self {
        Self {
            hir_module,
            layout_computer,
            mir_graph: MirGraph::new(func_symbol),
            var_map: HashMap::new(),
            current_func_symbol: func_symbol,
            closure_spec_map,
            captured_operand_map,
        }
    }

    /// Lowers an HIR type to a MIR type.
    pub(super) fn lower_type(&self, hir_type: &HirType) -> MirType {
        // Call the function from its new location
        types::lower_hir_type_to_mir_type(hir_type)
    }

    /// Determines the HIR type of a literal.
    pub(super) fn get_literal_type(&self, literal: &HirLiteral) -> HirType {
        match literal {
            HirLiteral::Int(_) => HirType::Primitive(ResolvePrimitiveType::I64),
            HirLiteral::Float(_) => HirType::Primitive(ResolvePrimitiveType::F64),
            HirLiteral::String(_) => HirType::Primitive(ResolvePrimitiveType::String),
            HirLiteral::Bool(_) => HirType::Primitive(ResolvePrimitiveType::Bool),
            HirLiteral::Char(_) => HirType::Primitive(ResolvePrimitiveType::Char),
            HirLiteral::Unit => HirType::Tuple(vec![]),
        }
    }

    /// Gets the MIR type produced by a specific output port of a node.
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
            MirNode::Downcast { payload_ty, .. } => Ok(payload_ty.clone()),
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
            MirNode::ArrayConstruct { element_ty, size } => Ok(MirType::Array(Arc::new(element_ty.clone()), *size)),
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
            // BinaryOp and IsVariant nodes produce results of specific types.
            MirNode::BinaryOp { result_ty, .. } => Ok(result_ty.clone()),
            MirNode::IsVariant { .. } => Ok(MirType::Primitive(ResolvePrimitiveType::Bool)),
        }
    }

    /// Gets the MIR return type of a function.
    pub(super) fn get_function_return_type(&self, func_symbol: Symbol) -> Result<MirType, LoweringError> {
        self.get_function_type(func_symbol)? // Use ? to propagate None error from get_function_type
            .map(|ft| match ft {
                MirType::FunctionPointer(_, ret) => Ok(ret.as_ref().clone()),
                 _ => Err(LoweringError::TypeMismatch(format!("Expected function symbol {:?} to have FunctionPointer type", func_symbol))),
            })
             // If get_function_type returned Ok(None)
            .ok_or_else(|| LoweringError::Internal(format!("Function {:?} not found for return type lookup", func_symbol)))?
    }

    /// Gets the MIR function pointer type for a function symbol.
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

    /// Gets the MIR type of a global (function or static).
    pub(super) fn get_global_type(&self, symbol: Symbol) -> Result<MirType, LoweringError> {
        if let Ok(Some(func_type)) = self.get_function_type(symbol) {
            return Ok(func_type);
        }
        self.hir_module
            .statics
            .iter()
            .find(|s| s.symbol == symbol)
            .map(|s| Ok(self.lower_type(&s.ty)))
             // Use proper error type if not found
            .ok_or_else(|| LoweringError::Internal(format!("Failed to find global function or static {:?} for type lookup", symbol)))?
    }

    /// Gets the 0-based index of a struct field.
    pub(super) fn get_field_index(&mut self, aggregate_ty: &MirType, field_symbol: Symbol) -> Result<u32, LoweringError> {
        let adt_symbol = match aggregate_ty {
            MirType::Adt(s) => *s,
             _ => return Err(LoweringError::TypeMismatch("Cannot get field index from non-ADT type".to_string())),
        };
        if let Some(struct_def) = self.hir_module.structs.iter().find(|s| s.symbol == adt_symbol) {
            struct_def.fields.iter().position(|(sym, _, _)| *sym == field_symbol)
                 .map(|pos| Ok(pos as u32))
                 // Use specific error
                .ok_or_else(|| LoweringError::Internal(format!("Field symbol {:?} not found in struct definition {:?}", field_symbol, adt_symbol)))?
        } else if let Some(_enum_def) = self.hir_module.enums.iter().find(|e| e.variants.iter().any(|v| v.symbol == field_symbol)) {
             // Still needs implementation for direct enum field access if ever allowed
             Err(LoweringError::Unsupported("Direct field access on enum type (use Downcast first)".to_string()))
        } else {
             Err(LoweringError::Internal(format!("ADT symbol {:?} not found for field index lookup", adt_symbol)))
        }
    }

    /// Gets the MIR type of a field within an aggregate type (struct or tuple) by index.
    pub(super) fn get_field_type(&mut self, aggregate_ty: &MirType, field_index: u32) -> Result<MirType, LoweringError> {
         match aggregate_ty {
            MirType::Adt(s) => {
                if let Some(struct_def) = self.hir_module.structs.iter().find(|sd| sd.symbol == *s) {
                    if let Some((_, _, ty)) = struct_def.fields.get(field_index as usize) {
                        Ok(self.lower_type(ty))
                    } else {
                         Err(LoweringError::Internal(format!("Field index {} out of bounds for struct {:?}", field_index, s)))
                    }
                } else if let Some(_enum_def) = self.hir_module.enums.iter().find(|ed| ed.symbol == *s) {
                    // Still needs implementation
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
    pub(super) fn get_enum_symbol_for_variant(&self, variant_symbol: Symbol) -> Result<Symbol, LoweringError> {
        for enum_def in &self.hir_module.enums {
            if enum_def.variants.iter().any(|v| v.symbol == variant_symbol) {
                 return Ok(enum_def.symbol);
            }
        }
          Err(LoweringError::Internal(format!("Enum symbol not found for variant symbol {:?}", variant_symbol)))
    }
} 