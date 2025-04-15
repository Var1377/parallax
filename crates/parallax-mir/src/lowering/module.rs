//!
//! Contains the main entry points for lowering (`lower_module`, `lower_function`).

use super::*;

// --- Module Lowering ---

/// Lowers an entire HIR module to its MIR representation.
///
/// This involves:
/// 1. A pre-pass to identify closures and prepare for specialization.
/// 2. Lowering struct and enum definitions, computing layout.
/// 3. Lowering regular function bodies.
/// 4. Lowering specialized closure function bodies.
/// 5. Lowering global static variables.
///
/// Returns the complete `MirModule` or a `LoweringError`.
pub fn lower_module(hir_module: &HirModule) -> Result<MirModule, LoweringError> {
    let mut layout_computer = layout::new_layout_computer();
    let mut mir_functions = HashMap::new();
    let mut mir_structs = Vec::new();
    let mut mir_enums = Vec::new();
    let mut closure_spec_map: HashMap<Symbol, ClosureSpecialization> = HashMap::new();

    // --- Closure Pre-computation Pass ---
    // Call the top-level pre-pass function, handling the Result
    for func in &hir_module.functions {
        if let Some(body) = &func.body {
            // Pass the map mutably to allow find_closures_in_value to populate capture types
            prepass::find_closures_in_expr(body, hir_module, &mut closure_spec_map)?;
        }
    }
    // --- End Closure Pre-computation ---

    // Create the translation context needed for layout computations
    let ctx = TranslationContext::new(&hir_module.structs, &hir_module.enums);

    // Lower Structs
    for struct_def in &hir_module.structs {
        let hir_type = HirType::Adt(struct_def.symbol);
        let layout = get_layout(&hir_type, &mut layout_computer, &ctx)
            .map_err(LoweringError::Layout)?; // Use map_err for specific error type

        let mir_fields = struct_def
            .fields
            .iter()
            .map(|(sym, name, ty)| (*sym, name.clone(), types::lower_hir_type_to_mir_type(ty)))
            .collect();

        mir_structs.push(MirStructDef {
            symbol: struct_def.symbol,
            name: struct_def.name.clone(),
            fields: mir_fields,
            layout,
        });
    }

    // Lower Enums
    for enum_def in &hir_module.enums {
        let hir_type = HirType::Adt(enum_def.symbol);
        let overall_layout = get_layout(&hir_type, &mut layout_computer, &ctx)
             .map_err(LoweringError::Layout)?;

        let discriminant_cl_type = get_enum_discriminant_type(enum_def.symbol, &mut layout_computer, &ctx)
             .map_err(LoweringError::Layout)?;

        let mut variant_payload_offsets = HashMap::new();
        let mut variant_payload_layouts = HashMap::new();

        // Collect results to handle potential errors within the map
        let mir_variants_results: Result<Vec<_>, LoweringError> = enum_def
            .variants
            .iter()
            .enumerate()
            .map(|(index, v)| {
                let payload_offset = get_variant_payload_offset_bytes(
                    enum_def.symbol,
                    v.symbol,
                    &mut layout_computer,
                    &ctx,
                )
                .map_err(LoweringError::Layout)?;
                let payload_layout = get_variant_payload_layout(
                    enum_def.symbol,
                    v.symbol,
                    &mut layout_computer,
                    &ctx,
                )
                 .map_err(LoweringError::Layout)?;

                variant_payload_offsets.insert(v.symbol, payload_offset);
                variant_payload_layouts.insert(v.symbol, payload_layout);

                let mir_fields = v.fields.iter().map(types::lower_hir_type_to_mir_type).collect();
                Ok(MirEnumVariant {
                    symbol: v.symbol,
                    name: v.name.clone(),
                    fields: mir_fields,
                    discriminant: index as u64,
                })
            })
            .collect();

        let mir_variants = mir_variants_results?;

        mir_enums.push(MirEnumDef {
            symbol: enum_def.symbol,
            name: enum_def.name.clone(),
            variants: mir_variants,
            overall_layout: overall_layout.clone(),
            discriminant_cl_type,
            variant_payload_offsets,
            variant_payload_layouts,
        });
    }

    // Lower regular function bodies (pass the mutable map)
    for func in &hir_module.functions {
        // Skip original lambda bodies - they will be lowered during the specialization pass
        if closure_spec_map.contains_key(&func.symbol) {
            continue;
        }

        if func.body.is_some() {
            // Lower as a regular function, graph symbol is the function's own symbol
            let graph = lower_function(
                hir_module,
                func,
                func.symbol, // Target symbol is the function's own symbol
                &mut layout_computer,
                &mut closure_spec_map,
            )?;
            mir_functions.insert(func.symbol, graph);
        } else {
            // Extern functions have no body, create empty graph
            mir_functions.insert(func.symbol, MirGraph::new(func.symbol));
        }
    }

    // Lower the specialized closure functions
    for (original_symbol, spec) in &closure_spec_map {
        let original_func = hir_module
            .functions
            .iter()
            .find(|f| f.symbol == *original_symbol)
            .ok_or_else(|| {
                LoweringError::Internal(format!(
                    "Original function definition {:?} not found for closure specialization",
                    original_symbol
                ))
            })?;

        // Lower the original function body, but create a graph associated with the specialized symbol
        let graph = lower_function(
            hir_module,
            original_func,
            spec.specialized_symbol, // Target symbol is the specialized one
            &mut layout_computer,
            &mut closure_spec_map, // Pass map mutably again (needed for lower_value inside)
        )?;
        mir_functions.insert(spec.specialized_symbol, graph);
    }

    // Lower statics
    let mut mir_statics = Vec::with_capacity(hir_module.statics.len());
    for static_def in &hir_module.statics {
        let mir_ty = types::lower_hir_type_to_mir_type(&static_def.ty);

        let initializer = match &static_def.initializer {
            Some(HirValue::Use(Operand::Const(lit))) => Some(lit.clone()),
            Some(_) => {
                // Warn or error about complex static initializers?
                eprintln!(
                    "Warning: Complex static initializer for {:?} lowered as None.",
                    static_def.symbol
                );
                None
            }
            None => None,
        };

        mir_statics.push(MirGlobalStatic {
            symbol: static_def.symbol,
            name: static_def.name.clone(),
            ty: mir_ty,
            initializer,
            is_mutable: static_def.is_mutable,
        });
    }

    Ok(MirModule {
        name: hir_module.name.clone(),
        functions: mir_functions,
        structs: mir_structs,
        enums: mir_enums,
        statics: mir_statics,
        entry_point: hir_module.entry_point,
    })
}


// --- Function Lowering ---

/// Lowers a single HIR function body to a MIR graph.
///
/// This function handles both regular functions and specialized closure functions.
///
/// # Arguments
/// * `hir_module` - A reference to the containing HIR module.
/// * `func_def` - The HIR definition of the function *body* to lower (this is always the original lambda body for specialized closures).
/// * `target_graph_symbol` - The `Symbol` to assign to the resulting `MirGraph`. This will be the function's own symbol for regular functions, or the specialized symbol for closures.
/// * `layout_computer` - Mutable access to the layout computer.
/// * `closure_spec_map` - Mutable access to the map containing closure specialization details. This is used to retrieve capture information for specialized functions and populated by `lower_value` when lowering `HirValue::Closure`.
///
/// # Returns
/// A `Result` containing the lowered `MirGraph` or a `LoweringError`.
fn lower_function(
    hir_module: &HirModule,
    func_def: &HirFunction,
    target_graph_symbol: Symbol,
    layout_computer: &mut LayoutComputer,
    closure_spec_map: &mut HashMap<Symbol, ClosureSpecialization>,
) -> Result<MirGraph, LoweringError> {
    // Ensure body exists before proceeding
    let body_to_lower = match func_def.body.as_ref() {
        Some(b) => b,
        None => {
            return Err(LoweringError::Internal(format!(
                "lower_function called on function definition {:?} with no body",
                func_def.symbol
            )))
        }
    };

    let original_func_symbol = func_def.symbol;
    let is_specialized = target_graph_symbol != original_func_symbol;

    let mut capture_params_to_add: Vec<MirType> = Vec::new();
    let mut original_params_offset = 0;
    let mut captured_operand_map: HashMap<Operand, u32> = HashMap::new();

    if is_specialized {
        // Look up the spec using the *original* function symbol
        let spec = closure_spec_map.get(&original_func_symbol).ok_or_else(|| {
            LoweringError::Internal(format!(
                "Cannot find closure spec for original symbol {:?} when lowering specialized function {:?}",
                original_func_symbol, target_graph_symbol
            ))
        })?;

        // Check consistency
        if spec.specialized_symbol != target_graph_symbol {
            return Err(LoweringError::Internal(format!(
                "Mismatch between target symbol {:?} and spec specialized symbol {:?} for original {:?}",
                target_graph_symbol, spec.specialized_symbol, original_func_symbol
            )));
        }

        // Use the capture types populated previously by lower_value
        capture_params_to_add = spec.capture_types.clone();
        original_params_offset = capture_params_to_add.len() as u32;

        // Build the map needed by lower_operand
        for (idx, operand) in spec.captured_operands.iter().enumerate() {
            captured_operand_map.insert(operand.clone(), idx as u32);
        }
        // Validate that capture_types were populated
        if capture_params_to_add.is_empty() && !spec.captured_operands.is_empty() {
            // This error check is crucial because lower_value populates this.
            // If it's still empty here, something went wrong (e.g., the closure value was never lowered).
            return Err(LoweringError::Internal(format!(
                "Capture types not populated in spec for original function {:?} (specialized {:?})",
                original_func_symbol, target_graph_symbol
            )));
        }
    }

    // Create the context for the graph we are building
    let mut ctx = FunctionLoweringContext::new(
        hir_module,
        target_graph_symbol, // Use the target symbol for the context's graph
        layout_computer,
        closure_spec_map,
        captured_operand_map, // Pass the map (empty if not specialized)
    );

    // --- Lower parameters ---
    // 1. Add parameters for captured variables (if specialized)
    for (i, cap_ty) in capture_params_to_add.iter().enumerate() {
        let param_node_id = ctx
            .mir_graph
            .add_node(MirNode::Parameter { index: i as u32, ty: cap_ty.clone() });
        ctx.mir_graph.parameter_nodes.push(param_node_id);
    }

    // 2. Add parameters for the original function signature
    // Use func_def (which is the original function definition)
    for (i, (param_var, param_ty)) in func_def.signature.params.iter().enumerate() {
        let mir_ty = ctx.lower_type(param_ty); // Changed: Call method on ctx
        let param_index = i as u32 + original_params_offset;
        let param_node_id = ctx
            .mir_graph
            .add_node(MirNode::Parameter { index: param_index, ty: mir_ty });
        ctx.mir_graph.parameter_nodes.push(param_node_id);
        // Bind the original HirVar to this parameter node
        ctx.var_map.insert(*param_var, (param_node_id, PortIndex(0)));
    }

    // --- Lower the body ---
    // Body comes from func_def (original definition)
    // lower_operand will use ctx.captured_operand_map correctly
    let final_port = ctx.lower_expr(body_to_lower)?; // Changed: Call method on ctx
    ctx.mir_graph.return_port = Some(final_port);

    Ok(ctx.mir_graph)
}