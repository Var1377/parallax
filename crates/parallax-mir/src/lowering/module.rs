//! # Module Lowering Logic (`lowering::module`)
//!
//! Contains the primary functions responsible for orchestrating the lowering of an entire
//! HIR module ([`lower_module`]) and individual HIR functions ([`lower_function`]) into
//! their MIR ([`MirGraph`]) representations.

// Remove old context import if present
// use parallax_native::translator::context::TranslationContext;

use super::*; use parallax_layout::LayoutComputer;
// Import necessary items from parent `lowering` module
// use parallax_gc::LayoutDescriptor; // Remove old GC import
// use parallax_gc::DescriptorStore; // Remove old GC import
use parallax_layout::{DescriptorStore, LayoutDescriptor, LayoutError, DescriptorIndex}; // Use parallax-layout imports
use parallax_hir::{ HirType, PrimitiveType }; // Add imports
use std::collections::HashMap;
use parallax_resolve::types::Symbol;

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
/// * `descriptor_store_box`: The pre-computed layout information for all types.
/// * `adt_index_map`: A map from ADT symbols to their corresponding descriptor indices.
/// * `primitive_index_map`: A map from primitive type symbols to their corresponding descriptor indices.
/// * `tuple_index_map`: A map from tuple type signatures to their corresponding descriptor indices.
/// * `array_index_map`: A map from array type signatures and their sizes to their corresponding descriptor indices.
///
/// # Returns
///
/// * `Ok(MirModule)`: The successfully lowered Mid-Level Intermediate Representation module.
/// * `Err(LoweringError)`: An error occurred during any step of the lowering process
///   (e.g., type mismatch, undefined symbol, unsupported feature).
pub fn lower_module(
    hir_module: &HirModule,
    // Accept Box<DescriptorStore> and maps by value
    descriptor_store_box: Box<DescriptorStore>,
    adt_index_map: HashMap<Symbol, DescriptorIndex>,
    primitive_index_map: HashMap<PrimitiveType, DescriptorIndex>,
    tuple_index_map: HashMap<Vec<HirType>, DescriptorIndex>,
    array_index_map: HashMap<(HirType, usize), DescriptorIndex>,
) -> Result<MirModule, LoweringError> {
    // --- Remove old layout computation setup ---
    // let mut descriptors = Vec::new();
    // let handle_desc_index = descriptors.len();
    // descriptors.push(LayoutDescriptor::Handle); // Assuming Handle is LayoutDescriptor::Handle
    // let struct_defs_map = hir_module ... collect();
    // let enum_defs_map = hir_module ... collect();
    // let mut layout_computer = LayoutComputer::new(...);
    // ... pre-computation loops ...
    // let descriptor_store = DescriptorStore { descriptors }; // Store is now passed in

    // Pass references to maps needed during lowering
    let descriptor_store_ref = &*descriptor_store_box;
    let adt_index_map_ref = &adt_index_map;
    let primitive_index_map_ref = &primitive_index_map;
    let tuple_index_map_ref = &tuple_index_map;
    let array_index_map_ref = &array_index_map;

    let mut mir_functions: HashMap<Symbol, MirGraph> = HashMap::new();
    let mut mir_structs: Vec<MirStructDef> = Vec::new();
    let mut mir_enums: Vec<MirEnumDef> = Vec::new();
    let mut closure_spec_map: HashMap<Symbol, ClosureSpecialization> = HashMap::new();

    // --- Step 1: Closure Pre-computation Pass (remains the same) ---
    for func in &hir_module.functions {
        if let Some(body) = &func.body {
            prepass::find_closures_in_expr(body, hir_module, &mut closure_spec_map)?;
        }
    }
    // --- End Closure Pre-computation ---

    // --- Step 2: Struct & Enum Lowering (Uses adt_index_map_ref) ---
    for struct_def in &hir_module.structs {
        let mir_fields: Vec<(Symbol, String, MirType)> = struct_def
            .fields
            .iter()
            .map(|(sym, name, ty)| (*sym, name.clone(), types::lower_hir_type_to_mir_type(ty)))
            .collect();

        // Use the map ref to find the DescriptorIndex
        let descriptor_index = *adt_index_map_ref.get(&struct_def.symbol)
            .ok_or_else(|| LoweringError::Internal(format!(
                "Descriptor index not found for struct {:?}", struct_def.symbol
            )))?;
            
        mir_structs.push(MirStructDef {
            symbol: struct_def.symbol,
            name: struct_def.name.clone(),
            fields: mir_fields,
            descriptor_index, // Use the index from the map
        });
    }

    for enum_def in &hir_module.enums {
        let mir_variants: Vec<MirEnumVariant> = enum_def
            .variants
            .iter()
            .enumerate()
            .map(|(index, v)| {
                MirEnumVariant {
                    symbol: v.symbol,
                    name: v.name.clone(),
                    fields: v.fields.iter()
                        .map(|ty| types::lower_hir_type_to_mir_type(ty))
                        .collect(),
                    discriminant: index as u64,
                }
            })
            .collect();
            
        // Use the map ref to find the DescriptorIndex
        let descriptor_index = *adt_index_map_ref.get(&enum_def.symbol)
            .ok_or_else(|| LoweringError::Internal(format!(
                "Descriptor index not found for enum {:?}", enum_def.symbol
            )))?;
            
        mir_enums.push(MirEnumDef {
            symbol: enum_def.symbol,
            name: enum_def.name.clone(),
            variants: mir_variants,
            descriptor_index, // Use the index from the map
        });
    }
    // --- End Struct & Enum Lowering ---

    // --- Step 3 & 4: Function and Closure Lowering (Pass store and map refs) ---
    // Process regular functions first
    for func in &hir_module.functions {
        if closure_spec_map.contains_key(&func.symbol) {
            continue;
        }
        if func.body.is_some() {
            let graph = lower_function(
                hir_module,
                func,
                func.symbol,
                descriptor_store_ref, // Pass ref
                adt_index_map_ref,    // Pass ref
                primitive_index_map_ref, // Pass ref
                tuple_index_map_ref,     // Pass ref
                array_index_map_ref,     // Pass ref
                &mut closure_spec_map,
            )?;
            mir_functions.insert(func.symbol, graph);
        } else {
            mir_functions.insert(func.symbol, MirGraph::new(func.symbol));
        }
    }
    // Process specialized closures
    let specialization_keys: Vec<Symbol> = closure_spec_map.keys().cloned().collect();
    for original_symbol in specialization_keys {
        let spec = closure_spec_map.get(&original_symbol)
             .ok_or_else(|| LoweringError::Internal(format!(
                 "Closure spec missing for {:?}", original_symbol
             )))?;
        let specialized_symbol = spec.specialized_symbol;
        let original_func = hir_module
            .functions
            .iter()
            .find(|f| f.symbol == original_symbol)
            .ok_or_else(|| LoweringError::Internal(format!(
                "Original function {:?} not found", original_symbol
            )))?;
        let graph = lower_function(
            hir_module,
            original_func,
            specialized_symbol,
            descriptor_store_ref, // Pass ref
            adt_index_map_ref,    // Pass ref
            primitive_index_map_ref, // Pass ref
            tuple_index_map_ref,     // Pass ref
            array_index_map_ref,     // Pass ref
            &mut closure_spec_map,
        )?;
        mir_functions.insert(specialized_symbol, graph);
    }
    // --- End Function Lowering ---

    // --- Step 5: Static Variable Lowering (remains mostly the same) ---
    let mut mir_statics = Vec::with_capacity(hir_module.statics.len());
    for static_def in &hir_module.statics {
        let mir_ty = types::lower_hir_type_to_mir_type(&static_def.ty);
        let initializer = match &static_def.initializer {
            Some(HirValue::Use(Operand::Const(lit))) => Some(lit.clone()),
            Some(other) => {
                println!(
                    "Warning: Complex static initializer ({:?}) for {:?} lowered as None.",
                    other, static_def.symbol
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
    // --- End Static Variable Lowering ---

    // --- Step 6: Module Assembly ---
    Ok(MirModule {
        name: hir_module.name.clone(),
        functions: mir_functions,
        structs: mir_structs,
        enums: mir_enums,
        statics: mir_statics,
        entry_point: hir_module.entry_point,
        intrinsics: hir_module.intrinsics.clone(),
        descriptor_store: descriptor_store_box, // Store the owned Box
        // Store owned maps
        adt_index_map,
        primitive_index_map,
        tuple_index_map,
        array_index_map,
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
    descriptor_store: &'a DescriptorStore, // Correctly accepts ref
    adt_index_map: &'a HashMap<Symbol, DescriptorIndex>,
    primitive_index_map: &'a HashMap<PrimitiveType, DescriptorIndex>,
    tuple_index_map: &'a HashMap<Vec<HirType>, DescriptorIndex>,
    array_index_map: &'a HashMap<(HirType, usize), DescriptorIndex>,
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
        descriptor_store, // Pass the descriptor store reference
        adt_index_map,        // Pass map refs
        primitive_index_map,
        tuple_index_map,
        array_index_map,
        closure_spec_map, // Pass map mutably (needed by lower_value within lower_expr).
        captured_operand_map, // Pass the map (empty if not specialized).
    );

    // --- Lower Parameters (Aggregate Tuple Strategy) ---
    // 1. Collect all parameter types for the aggregate tuple.
    let mut aggregate_param_constituent_types = Vec::new();

    // If specialized, the first element is the environment tuple itself.
    let mut captured_env_ty = None;
    if is_specialized {
        let env_ty = MirType::Tuple(capture_mir_types.clone());
        aggregate_param_constituent_types.push(env_ty.clone());
        captured_env_ty = Some(env_ty);
    }

    // Lower and add original parameter types.
    let original_param_mir_types: Vec<MirType> = func_def
        .signature
        .params
        .iter()
        .map(|(_, ty)| ctx.lower_type(ty))
        .collect();
    aggregate_param_constituent_types.extend(original_param_mir_types.iter().cloned());

    // 2. Create the aggregate tuple type for the parameter node.
    let aggregate_param_mir_type = MirType::Tuple(aggregate_param_constituent_types);

    // 3. Add the single aggregate parameter node to the graph.
    let aggregate_param_node_id = ctx.mir_graph.add_parameter_node(aggregate_param_mir_type.clone());

    // 4. Add projection nodes to extract individual parameters from the aggregate.
    let mut current_tuple_index = 0;

    // If specialized, project the environment tuple first.
    let mut projected_env_node_id_opt = None;
    if let Some(env_ty) = captured_env_ty {
        let env_project_node = MirNode::Project {
            field_index: current_tuple_index, // Env is always index 0
            aggregate_ty: aggregate_param_mir_type.clone(),
            field_ty: env_ty,
        };
        let env_project_node_id = ctx.mir_graph.add_node(env_project_node);
        ctx.mir_graph.add_edge(aggregate_param_node_id, PortIndex(0), env_project_node_id, PortIndex(0));
        projected_env_node_id_opt = Some(env_project_node_id);
        current_tuple_index += 1;

        // Now, project *captured variables* from the *projected environment tuple*.
        if let Some(projected_env_node_id) = projected_env_node_id_opt {
             let env_tuple_ty = MirType::Tuple(capture_mir_types.clone()); // Reconstruct env tuple type
             for (capture_index, cap_ty) in capture_mir_types.iter().enumerate() {
                 let capture_project_node = MirNode::Project {
                      field_index: capture_index as u32,
                      aggregate_ty: env_tuple_ty.clone(), // Project from the env tuple
                      field_ty: cap_ty.clone(),
                 };
                 let capture_project_node_id = ctx.mir_graph.add_node(capture_project_node);
                 // Connect projection input to the *projected environment* output.
                 ctx.mir_graph.add_edge(
                      projected_env_node_id, PortIndex(0),
                      capture_project_node_id, PortIndex(0),
                 );
                 // Note: The mapping from Operand to these capture_project_node_id
                 // is handled implicitly by captured_operand_map using the original Operand
                 // and the capture_index used here during lower_operand.
             }
         }
    }

    // Projections for original function parameters (indices continue after env tuple, if any).
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