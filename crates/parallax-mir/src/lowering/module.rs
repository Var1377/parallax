//! # Module Lowering Logic (`lowering::module`)
//!
//! Contains the primary functions responsible for orchestrating the lowering of an entire
//! HIR module ([`lower_module`]) and individual HIR functions ([`lower_function`]) into
//! their MIR ([`MirGraph`]) representations.

// Remove old context import if present
// use parallax_native::translator::context::TranslationContext;

use super::*; use parallax_gc::layout::LayoutComputer;
// Import necessary items from parent `lowering` module
use parallax_gc::LayoutDescriptor; // Import LayoutDescriptor for Handle initialization
use parallax_gc::DescriptorStore; // Import DescriptorStore for passing to lower_function

// --- Module Lowering ---

/// Lowers an entire [`HirModule`] to its [`MirModule`] representation.
///
/// This function serves as the main entry point for the HIR-to-MIR lowering phase.
/// It coordinates the translation of all top-level items (functions, closures,
/// structs, enums, statics) within the HIR module into their corresponding MIR components.
/// See the `lowering` module documentation for a detailed breakdown of the process.
///
/// # Arguments
///
/// * `hir_module`: A reference to the complete High-Level Intermediate Representation module.
///
/// # Returns
///
/// * `Ok(MirModule)`: The successfully lowered Mid-Level Intermediate Representation module.
/// * `Err(LoweringError)`: An error occurred during any step of the lowering process
///   (e.g., layout computation failure, type mismatch, undefined symbol, unsupported feature).
pub fn lower_module(hir_module: &HirModule) -> Result<MirModule, LoweringError> {
    // Initialize shared resources: layout computer and MIR data structures.
    // Create the DescriptorStore and initialize LayoutComputer
    let mut descriptors = Vec::new();
    // Add the Handle descriptor initially, assuming index 0
    let handle_desc_index = descriptors.len();
    descriptors.push(LayoutDescriptor::Handle);

    // Convert Vec<HirStructDef> and Vec<HirEnumDef> to HashMap for LayoutComputer
    let struct_defs_map = hir_module
        .structs
        .iter()
        .map(|s| (s.symbol, s.clone()))
        .collect::<HashMap<_, _>>();
    let enum_defs_map = hir_module
        .enums
        .iter()
        .map(|e| (e.symbol, e.clone()))
        .collect::<HashMap<_, _>>();

    let mut layout_computer = LayoutComputer::new(
        &mut descriptors,
        handle_desc_index,
        struct_defs_map,
        enum_defs_map,
    );

    // --- Pre-compute ALL struct and enum layouts ---
    for struct_def in &hir_module.structs {
        let hir_type = HirType::Adt(struct_def.symbol);
        layout_computer.get_or_create_descriptor_index(&hir_type)?;
    }
    for enum_def in &hir_module.enums {
        let hir_type = HirType::Adt(enum_def.symbol);
        layout_computer.get_or_create_descriptor_index(&hir_type)?;
    }
    // --- End Pre-computation ---

    // Old layout computer initialization
    // let mut layout_computer = LayoutComputer::new();
    let mut mir_functions: HashMap<Symbol, MirGraph> = HashMap::new();
    let mut mir_structs: Vec<MirStructDef> = Vec::new();
    let mut mir_enums: Vec<MirEnumDef> = Vec::new();
    // Map to store information needed for closure specialization.
    let mut closure_spec_map: HashMap<Symbol, ClosureSpecialization> = HashMap::new();

    // --- Step 1: Closure Pre-computation Pass ---
    // Traverse the HIR to identify all closure definitions (`HirValue::Closure`)
    // and the operands they capture. This populates `closure_spec_map` with
    // `ClosureSpecialization` entries, assigning a unique `specialized_symbol`
    // for each original closure definition encountered.
    for func in &hir_module.functions {
        if let Some(body) = &func.body {
            // Recursively search for closures within the function body.
            prepass::find_closures_in_expr(body, hir_module, &mut closure_spec_map)?;
        }
    }
    // --- End Closure Pre-computation ---

    // --- Step 2: Struct & Enum Lowering (Minor Change) ---
    // This step now just retrieves the already computed index and MIR types.

    // Lower Struct Definitions
    for struct_def in &hir_module.structs {
        let hir_type = HirType::Adt(struct_def.symbol);
        // Get the pre-computed descriptor index.
        let descriptor_index = layout_computer
            .get_or_create_descriptor_index(&hir_type)
            .map_err(LoweringError::Layout)?;

        // Lower field types from HIR to MIR.
        let mir_fields: Vec<(Symbol, String, MirType)> = struct_def
            .fields
            .iter()
            .map(|(sym, name, ty)| (*sym, name.clone(), types::lower_hir_type_to_mir_type(ty)))
            .collect();

        mir_structs.push(MirStructDef {
            symbol: struct_def.symbol,
            name: struct_def.name.clone(),
            fields: mir_fields,
            // Store the descriptor index instead of the layout itself.
            descriptor_index,
        });
    }

    // Lower Enum Definitions
    for enum_def in &hir_module.enums {
        let hir_type = HirType::Adt(enum_def.symbol);
        // Get the pre-computed descriptor index.
        let descriptor_index = layout_computer
            .get_or_create_descriptor_index(&hir_type)
            .map_err(LoweringError::Layout)?;

        // Lower variants - no need to compute offsets/layouts here, they are in the descriptor.
        let mir_variants: Vec<MirEnumVariant> = enum_def
            .variants
            .iter()
            .enumerate()
            .map(|(index, v)| {
                // Create MIR variant definition.
                MirEnumVariant {
                    symbol: v.symbol,
                    name: v.name.clone(),
                    fields: v.fields.iter()
                        .map(|ty| types::lower_hir_type_to_mir_type(ty))
                        .collect(),
                    discriminant: index as u64, // Assign discriminant based on definition order.
                }
            })
            .collect();

        // Create and store the MIR enum definition.
        mir_enums.push(MirEnumDef {
            symbol: enum_def.symbol,
            name: enum_def.name.clone(),
            variants: mir_variants,
            // Store the descriptor index.
            descriptor_index,
        });
    }
    // --- End Struct & Enum Lowering ---

    // --- Create final DescriptorStore BEFORE function lowering ---
    // The descriptors vec is no longer mutably borrowed by layout_computer after this.
    let descriptor_store = DescriptorStore { descriptors };

    // --- Step 3: Regular Function Lowering ---
    // Lower functions that are *not* original closure definitions.
    for func in &hir_module.functions {
        // Skip original lambda bodies identified in the pre-pass.
        // They will be lowered during the specialization step using their specialized symbol.
        if closure_spec_map.contains_key(&func.symbol) {
            continue;
        }

        if func.body.is_some() {
            // Lower this function's body. The target graph symbol is the function's own symbol.
            let graph = lower_function(
                hir_module,
                func, // The HIR definition to lower
                func.symbol, // The symbol for the resulting MirGraph
                &descriptor_store, // Pass immutable store
                &mut closure_spec_map,
            )?;
            mir_functions.insert(func.symbol, graph);
        } else {
            // External functions (no body) are represented by an empty MirGraph (declaration).
            mir_functions.insert(func.symbol, MirGraph::new(func.symbol));
        }
    }
    // --- End Regular Function Lowering ---

    // --- Step 4: Closure Specialization Lowering ---
    // Iterate through the specialization info gathered in the pre-pass.
    // Note: We iterate over a borrow `&closure_spec_map` because `lower_function` needs
    // mutable access to potentially update capture_types within the map's values.
    let specialization_keys: Vec<Symbol> = closure_spec_map.keys().cloned().collect();
    for original_symbol in specialization_keys {
        // Retrieve the required ClosureSpecialization details using the original symbol.
        // We expect it to exist based on the pre-pass.
        let spec = closure_spec_map.get(&original_symbol)
            .ok_or_else(|| LoweringError::Internal(format!(
                "Closure spec unexpectedly missing for original symbol {:?} during specialization lowering",
                original_symbol
            )))?;
        let specialized_symbol = spec.specialized_symbol; // Get the unique symbol for the specialized graph.

        // Find the original HIR function definition for the closure's body.
        let original_func = hir_module
            .functions
            .iter()
            .find(|f| f.symbol == original_symbol)
            .ok_or_else(|| {
                LoweringError::Internal(format!(
                    "Original function definition {:?} not found for closure specialization",
                    original_symbol
                ))
            })?;

        // Lower the original function body, but create a MirGraph associated with the specialized symbol.
        // The `lower_function` logic will handle adding captures as parameters based on the spec.
        // Crucially, `lower_value` within this call will populate `spec.capture_types` in the map.
        let graph = lower_function(
            hir_module,
            original_func, // Lower the *original* closure body
            specialized_symbol, // But create a graph identified by the *specialized* symbol
            &descriptor_store, // Pass immutable store
            &mut closure_spec_map, // Pass map mutably - lower_value needs it to store capture_types
        )?;
        mir_functions.insert(specialized_symbol, graph);
    }
    // --- End Closure Specialization Lowering ---

    // --- Step 5: Static Variable Lowering ---
    let mut mir_statics = Vec::with_capacity(hir_module.statics.len());
    for static_def in &hir_module.statics {
        let mir_ty = types::lower_hir_type_to_mir_type(&static_def.ty);

        // Handle initializer: Currently only supports constant literals.
        let initializer = match &static_def.initializer {
            Some(HirValue::Use(Operand::Const(lit))) => Some(lit.clone()),
            Some(other) => {
                // Warn or error for complex static initializers?
                // For now, warn and treat as None.
                // Future: Could generate an initializer function.
                println!(
                    "Warning: Complex static initializer ({:?}) for {:?} lowered as None.",
                    other, static_def.symbol
                );
                None
            }
            None => None,
        };

        // Create the MIR static definition.
        mir_statics.push(MirGlobalStatic {
            symbol: static_def.symbol,
            name: static_def.name.clone(),
            ty: mir_ty,
            initializer,
            is_mutable: static_def.is_mutable,
        });
    }
    // --- End Static Variable Lowering ---

    // --- Step 6: Module Assembly ---
    // DescriptorStore was created earlier
    // let descriptor_store = DescriptorStore { descriptors };

    // Collect all lowered components into the final MirModule.
    Ok(MirModule {
        name: hir_module.name.clone(),
        functions: mir_functions,
        structs: mir_structs,
        enums: mir_enums,
        statics: mir_statics,
        entry_point: hir_module.entry_point,
        intrinsics: hir_module.intrinsics.clone(), // Propagate intrinsics
        // Add the descriptor store
        descriptor_store,
    })
}


// --- Function Lowering ---

/// Lowers a single HIR function body ([`HirFunction`]) to a [`MirGraph`].
///
/// This function handles the detailed translation of HIR expressions within a function
/// into the nodes and edges of a MIR dataflow graph. It is used internally by
/// [`lower_module`] for both regular functions and specialized closure bodies.
///
/// # Specialization Handling
///
/// The behavior changes slightly based on whether `target_graph_symbol` (the symbol for the
/// [`MirGraph`] being created) matches `func_def.symbol` (the symbol of the original
/// HIR function definition being lowered):
///
/// *   **Regular Function:** `target_graph_symbol == func_def.symbol`. Parameters are handled
///     directly from `func_def.signature`.
/// *   **Specialized Closure:** `target_graph_symbol != func_def.symbol`. The function retrieves
///     [`ClosureSpecialization`] details using `func_def.symbol`. Captured variables
///     (identified in `spec.captured_operands`) are added as *leading* parameters to the
///     [`MirGraph`], before the original parameters from `func_def.signature`.
///     A `captured_operand_map` is created to link the original captured [`Operand`]s to the
///     indices of these new capture parameters.
///
/// # Parameter Lowering Strategy
///
/// Currently, all function parameters (including captures for specialized closures) are aggregated
/// into a single [`MirType::Tuple`]. A single `MirNode::Parameter` is added to the graph representing
/// this aggregate tuple. [`MirNode::Project`] nodes are then immediately added, connected to the
/// aggregate parameter node, to extract each individual original parameter or captured variable.
/// The results of these projections are then mapped to the corresponding [`HirVar`] (for original
/// parameters) or used via the `captured_operand_map` (for captures) when lowering the function body.
///
/// # Body Lowering
///
/// The core translation happens by calling [`expr::lower_expr`] on the function's body
/// (`body_to_lower`). This recursively descends the HIR expression tree, using a
/// [`FunctionLoweringContext`] to build the MIR graph nodes and edges, track variable bindings,
/// and manage types.
///
/// # Arguments
///
/// * `hir_module`: A reference to the containing [`HirModule`] (needed for context, e.g., resolving types, finding definitions).
/// * `func_def`: The HIR definition of the function *body* to lower. For specialized closures,
///   this is always the *original* HIR definition of the closure lambda.
/// * `target_graph_symbol`: The [`Symbol`] to assign to the resulting [`MirGraph`]. This will be
///   the function's own symbol for regular functions, or the unique specialized symbol for closures.
/// * `descriptor_store`: Immutable access to the [`DescriptorStore`] for type layout information.
/// * `closure_spec_map`: Mutable access to the map containing [`ClosureSpecialization`] details.
///   This is primarily used to retrieve capture information for specialized functions and is crucially
///   *updated* by [`expr::lower_value`] when it processes the `HirValue::Closure` corresponding to
///   `func_def.symbol` to store the determined `capture_types`.
///
/// # Returns
///
/// * `Ok(MirGraph)`: The successfully lowered MIR dataflow graph for the function body.
/// * `Err(LoweringError)`: An error occurred during the lowering of this specific function body.
///
/// # Panics
/// Can panic if internal invariants are violated (e.g., attempting to lower a function definition
/// with no body, inconsistencies in the `closure_spec_map`).
pub(super) fn lower_function<'a>(
    hir_module: &'a HirModule,
    func_def: &'a HirFunction,
    target_graph_symbol: Symbol,
    descriptor_store: &'a DescriptorStore,
    closure_spec_map: &'a mut HashMap<Symbol, ClosureSpecialization>,
) -> Result<MirGraph, LoweringError> {
    // Ensure the function definition has a body to lower.
    let body_to_lower = func_def.body.as_ref().ok_or_else(|| {
        LoweringError::Internal(format!(
            "lower_function called on function definition {:?} with no body (should be extern or already handled)",
            func_def.symbol
        ))
    })?;

    let original_func_symbol = func_def.symbol;
    let is_specialized = target_graph_symbol != original_func_symbol;

    // --- Prepare for Specialization (if applicable) ---
    let mut capture_mir_types: Vec<MirType> = Vec::new();
    let mut captured_operand_map: HashMap<Operand, u32> = HashMap::new(); // Maps captured Operand -> param index

    if is_specialized {
        // Retrieve the specialization info using the *original* function symbol.
        let spec = closure_spec_map.get(&original_func_symbol).ok_or_else(|| {
            // This indicates a logic error - the pre-pass should have populated this.
            LoweringError::Internal(format!(
                "Cannot find closure spec for original symbol {:?} when lowering specialized function {:?}",
                original_func_symbol, target_graph_symbol
            ))
        })?;

        // Sanity check: ensure the target symbol matches the one assigned in the pre-pass.
        if spec.specialized_symbol != target_graph_symbol {
            return Err(LoweringError::Internal(format!(
                "Mismatch between target symbol {:?} and spec specialized symbol {:?} for original {:?}",
                target_graph_symbol, spec.specialized_symbol, original_func_symbol
            )));
        }

        // **Crucially**, retrieve the capture types. These should have been populated
        // when `lower_value` processed the `HirValue::Closure` that created this instance.
        capture_mir_types = spec.capture_types.clone();

        // Build the map needed by `lower_operand` to redirect captured variables
        // to the correct input parameter index of the specialized function.
        for (idx, operand) in spec.captured_operands.iter().enumerate() {
            // Requires `Operand` to derive Hash, Eq, PartialEq in parallax_hir
            captured_operand_map.insert(operand.clone(), idx as u32);
        }

        // Validate that capture types were indeed populated if captures exist.
        if capture_mir_types.is_empty() && !spec.captured_operands.is_empty() {
            // This signifies that the `HirValue::Closure` for this specialization was likely
            // never encountered and lowered by `lower_value`, which is an internal error.
            return Err(LoweringError::Internal(format!(
                "Capture types not populated in spec for original function {:?} (specialized {:?}) before lowering body. Did lower_value run?",
                original_func_symbol, target_graph_symbol
            )));
        }
    }
    // --- End Specialization Prep ---

    // Create the lowering context for this specific function graph.
    let mut ctx = FunctionLoweringContext::new(
        hir_module,
        target_graph_symbol, // The symbol for the MirGraph being built.
        descriptor_store, // Pass the descriptor store
        closure_spec_map, // Pass map mutably (needed by lower_value within lower_expr).
        captured_operand_map, // Pass the map (empty if not specialized).
    );

    // --- Lower Parameters (Aggregate Tuple Strategy) ---
    // 1. Collect all parameter types: captures first, then original parameters.
    let mut aggregate_param_constituent_types = Vec::new();
    aggregate_param_constituent_types.extend(capture_mir_types.iter().cloned());

    // Lower original parameter types.
    let original_param_mir_types: Vec<MirType> = func_def
        .signature
        .params
        .iter()
        .map(|(_, ty)| ctx.lower_type(ty))
        .collect();
    aggregate_param_constituent_types.extend(original_param_mir_types.iter().cloned());

    // 2. Create the aggregate tuple type.
    let aggregate_param_mir_type = MirType::Tuple(aggregate_param_constituent_types.clone());

    // 3. Add the single aggregate parameter node to the graph.
    let aggregate_param_node_id = ctx.mir_graph.add_parameter_node(aggregate_param_mir_type.clone());

    // 4. Add projection nodes to extract individual parameters from the aggregate.
    let mut current_tuple_index = 0;

    // Projections for captured variables (if specialized).
    // These don't map directly to HirVars in the original sense but provide the
    // source value for captured Operands via the captured_operand_map.
    for cap_ty in &capture_mir_types {
        let field_index = current_tuple_index;
        let project_node = MirNode::Project {
            field_index,
            aggregate_ty: aggregate_param_mir_type.clone(),
            field_ty: cap_ty.clone(),
        };
        let project_node_id = ctx.mir_graph.add_node(project_node);
        // Connect projection input to the aggregate parameter output.
        ctx.mir_graph.add_edge(
            aggregate_param_node_id, PortIndex(0),
            project_node_id, PortIndex(0),
        );
        // Note: No direct mapping to HirVar needed here; handled by captured_operand_map in lower_operand.
        current_tuple_index += 1;
    }

    // Projections for original function parameters.
    for (i, (param_var, _ /* original hir_ty */)) in func_def.signature.params.iter().enumerate() {
        let field_index = current_tuple_index;
        let field_mir_ty = original_param_mir_types[i].clone();

        let project_node = MirNode::Project {
            field_index,
            aggregate_ty: aggregate_param_mir_type.clone(),
            field_ty: field_mir_ty.clone(),
        };
        let project_node_id = ctx.mir_graph.add_node(project_node);
        // Connect projection input to the aggregate parameter output.
        ctx.mir_graph.add_edge(
            aggregate_param_node_id, PortIndex(0),
            project_node_id, PortIndex(0),
        );

        // Bind the original HirVar to the *output* of its corresponding projection node.
        // This makes the parameter available in the function body's scope.
        let prev = ctx.var_map.insert(*param_var, (project_node_id, PortIndex(0)));
        if prev.is_some() {
            // This shouldn't happen with unique HirVars for parameters.
            return Err(LoweringError::Internal(format!(
                "Duplicate HirVar {:?} encountered during parameter lowering for function {:?}",
                param_var, target_graph_symbol
            )));
        }

        current_tuple_index += 1;
    }
    // --- End Parameter Lowering ---

    // --- Lower the Function Body ---
    // Recursively lower the HIR expression tree (body_to_lower) using the context.
    // This builds the main part of the MIR graph.
    // `lower_operand` inside this call will use `ctx.captured_operand_map` correctly
    // to find the projection nodes for captured variables when needed.
    let final_port = expr::lower_expr(&mut ctx, body_to_lower)?;

    // Set the graph's return port to the final result of the body expression.
    ctx.mir_graph.set_return_port(final_port.0, final_port.1); // Use helper method
    // --- End Body Lowering ---

    // Return the completed graph.
    Ok(ctx.mir_graph)
}