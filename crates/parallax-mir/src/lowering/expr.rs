//!
//! # HIR Expression Lowering (`lowering::expr`)
//!
//! This module contains the core recursive logic for translating HIR expressions
//! (`HirExpr`, `HirValue`, `HirTailExpr`, `Operand`) into MIR graph nodes
//! ([`MirNode`]) and edges ([`MirEdge`]) within a [`FunctionLoweringContext`].
//! Each function typically takes the context and the HIR construct, adds the necessary
//! MIR nodes and edges to the context's graph, and returns the final output port
//! ([`NodeId`], [`PortIndex`]) representing the value produced by the HIR construct.

use super::*; // Import necessary items from parent `lowering` module
use parallax_hir::hir::{HirType, PrimitiveType as HirPrimitiveType};
use parallax_hir::hir::{AggregateKind, HirPattern}; // Added import
use std::collections::{HashMap, HashSet}; // Ensure HashMap and HashSet are imported

/// Lowers an HIR operand ([`Operand`]) to the MIR node and port that produces its value.
///
/// This handles:
/// *   **Variables (`Operand::Var`):** Looks up the variable in the `ctx.var_map`.
///     If the context is for a specialized closure (`ctx.captured_operand_map` is populated),
///     it first checks if the variable is a captured one. If so, it finds the corresponding
///     parameter projection node in the MIR graph instead of using `var_map`.
/// *   **Constants (`Operand::Const`):** Creates a new [`MirNode::Constant`] in the graph.
/// *   **Globals (`Operand::Global`):** Creates a new [`MirNode::StaticAddr`] in the graph.
///
/// # Arguments
/// * `ctx`: The mutable lowering context for the current function.
/// * `operand`: The HIR operand to lower.
///
/// # Returns
/// * `Ok((NodeId, PortIndex))`: The node and output port producing the operand's value in the MIR graph.
/// * `Err(LoweringError::UndefinedVariable)`: If an `Operand::Var` is not found in `var_map` or `captured_operand_map`.
/// * `Err(LoweringError::Internal)`: If looking up a captured variable's projection node fails (graph inconsistency).
/// * `Err(...)`: Propagates errors from `ctx.get_global_type`.
pub(super) fn lower_operand<'ctx>(
    ctx: &mut FunctionLoweringContext<'ctx>,
    operand: &Operand,
) -> Result<(NodeId, PortIndex), LoweringError> {
    // --- Handle Captured Operands in Specialized Functions ---
    // Check if this operand corresponds to a known captured variable/value for the current specialized function.
    // The `captured_operand_map` links the original Operand (e.g., `Operand::Var(x)` from the closure definition scope)
    // to the index of the capture within the *environment tuple* (which itself is the first projected parameter).
    if let Some(capture_env_index) = ctx.captured_operand_map.get(operand).copied() {
        // This operand *is* a capture for the specialized function being lowered.
        // We need the node that provides the value for this capture index *from the environment tuple*.

        // 1. Get the single aggregate parameter node for the function graph.
        let aggregate_param_node_id = ctx.mir_graph.parameter_node.ok_or_else(|| {
            LoweringError::Internal(format!(
                "MIR graph for specialized function {:?} is missing aggregate param node for capture {:?}",
                ctx.current_func_symbol, operand
            ))
        })?;

        // 2. Find the Project node that extracts the *environment tuple* (always field index 0)
        //    from the aggregate parameter node.
        let env_tuple_proj_node_id = ctx.mir_graph.edges.iter()
            .filter(|edge| edge.from_node == aggregate_param_node_id && edge.from_port == PortIndex(0))
            .find_map(|edge| {
                ctx.mir_graph.nodes.get(&edge.to_node).and_then(|node| {
                    if let MirNode::Project { field_index: 0, .. } = node {
                        Some(edge.to_node) // Found the env tuple projection
                    } else { None }
                })
            })
            .ok_or_else(|| {
                LoweringError::Internal(format!(
                    "Could not find projection node for environment tuple (index 0) in function {:?}",
                    ctx.current_func_symbol
                ))
            })?;

        // 3. Find the Project node that extracts the specific captured variable (using `capture_env_index`)
        //    *from the projected environment tuple*.
        let capture_proj_node_id = ctx.mir_graph.edges.iter()
            // Find edges coming *from* the environment tuple projection node.
            .filter(|edge| edge.from_node == env_tuple_proj_node_id && edge.from_port == PortIndex(0))
            // Check if the destination node is a Project node with the correct field index (`capture_env_index`).
            .find_map(|edge| {
                ctx.mir_graph.nodes.get(&edge.to_node).and_then(|node| {
                    if let MirNode::Project { field_index, .. } = node {
                        if *field_index == capture_env_index {
                            Some(edge.to_node) // Found the correct capture projection node
                        } else { None }
                    } else { None }
                })
            })
            .ok_or_else(|| {
                // This indicates an inconsistency between lower_function (where these projections are created)
                // and lower_operand (where we are looking for them).
                LoweringError::Internal(format!(
                    "Could not find projection node for captured variable index {} from env tuple {:?} (operand {:?}) in function {:?}",
                    capture_env_index, env_tuple_proj_node_id, operand, ctx.current_func_symbol
                ))
            })?;

        // The value of the captured operand is the output (port 0) of this final capture projection node.
        return Ok((capture_proj_node_id, PortIndex(0)));
    }
    // --- End Captured Operand Handling ---

    // If the operand is not a capture (or we are not in a specialized function),
    // lower it normally based on its kind.
    match operand {
        Operand::Var(var) => {
            // Look up the variable in the current scope's variable map.
            match ctx.var_map.get(var).copied() {
                Some((node_id, port_index)) => {
                    // Check if this variable was bound to a HirValue::Closure
                    if let Some(MirNode::Closure { .. }) = ctx.mir_graph.nodes.get(&node_id) {
                        // If it's a closure variable, return the function pointer port (Port 1)
                        Ok((node_id, PortIndex(1)))
                    } else {
                        // Otherwise, return the original port (usually Port 0 for other values)
                        Ok((node_id, port_index))
                    }
                }
                None => Err(LoweringError::UndefinedVariable(*var)) // Error if variable not found
            }
        }
        Operand::Const(literal) => {
            // Create a new Constant node in the MIR graph.
            let mir_ty = ctx.lower_type(&ctx.get_literal_type(literal));
            let node_id = ctx.mir_graph.add_node(MirNode::Constant { value: literal.clone(), ty: mir_ty });
            // Constants have a single output port (index 0).
            Ok((node_id, PortIndex(0)))
        }
        Operand::Global(symbol) => {
            // Create a new StaticAddr node representing the global function or static.
            let mir_ty = ctx.get_global_type(*symbol)?;
            let node_id = ctx.mir_graph.add_node(MirNode::StaticAddr { symbol: *symbol, ty: mir_ty });
            // StaticAddr nodes have a single output port (index 0).
            Ok((node_id, PortIndex(0)))
        }
    }
}

/// Lowers an HIR value computation ([`HirValue`]) into MIR nodes and edges.
///
/// Returns the MIR node and output port that produces the result of the value computation.
///
/// This handles:
/// *   `HirValue::Use`: Delegates to `lower_operand`.
/// *   `HirValue::Call`: Lowers the function operand and arguments, then creates a [`MirNode::FunctionCall`].
///     Distinguishes between regular calls and closure calls based on whether the function operand
///     originates directly from a [`MirNode::Closure`].
/// *   `HirValue::Aggregate`: Lowers field operands, determines the aggregate type (Struct, Enum Variant,
///     Tuple, Array), and creates a [`MirNode::Constructor`] or [`MirNode::ArrayConstruct`].
/// *   `HirValue::Project`: Lowers the base operand, determines the projection kind (Field, TupleIndex,
///     ArrayIndex, Downcast), and creates a [`MirNode::Project`], [`MirNode::ArrayProject`], or [`MirNode::Downcast`].
/// *   `HirValue::Closure`: **Crucially**, this is where `capture_types` are determined and stored
///     in the `closure_spec_map`. It lowers captured operands, retrieves the pre-assigned specialized symbol,
///     constructs the closure environment type and specialized function pointer type, and creates a
///     [`MirNode::Closure`].
///
/// # Arguments
/// * `ctx`: The mutable lowering context for the current function.
/// * `value`: The HIR value computation to lower.
///
/// # Returns
/// * `Ok((NodeId, PortIndex))`: The node and output port producing the value in the MIR graph.
/// * `Err(LoweringError::Internal)`: If looking up a captured variable's projection node fails (graph inconsistency).
/// * `Err(...)`: Propagates errors from `ctx.get_global_type`.
pub(super) fn lower_value<'ctx>(
    ctx: &mut FunctionLoweringContext<'ctx>,
    value: &HirValue,
) -> Result<(NodeId, PortIndex), LoweringError> {
     match value {
        // Simple use of an existing variable, constant, or global.
        HirValue::Use(operand) => lower_operand(ctx, operand),

        // Function call.
        HirValue::Call { func, args } => {
            // Lower the operand providing the function value (could be a global func symbol or a closure).
            let (func_operand_node, func_operand_port) = lower_operand(ctx, func)?;

            // Determine the *kind* of node producing the function value. This helps distinguish
            // between calling a regular function pointer and calling a closure.
            let func_source_node_kind = ctx.mir_graph.nodes.get(&func_operand_node)
                .ok_or_else(|| LoweringError::Internal(format!(
                    "MIR node {:?} providing function operand not found during call lowering of {:?}",
                    func_operand_node, func
                )))?.clone(); // Clone the MirNode enum instance

            // Lower all argument operands.
            let mut arg_ports: Vec<(NodeId, PortIndex)> = Vec::with_capacity(args.len());
            for arg in args {
                arg_ports.push(lower_operand(ctx, arg)?);
            }

            // Check if the function value comes *directly* from the function pointer output (Port 1)
            // of a Closure node. This indicates a closure call.
            if matches!(func_source_node_kind, MirNode::Closure { .. }) && func_operand_port == PortIndex(1) {
                // --- Closure Call --- 
                // The function pointer itself is output 1 of the func_operand_node.
                // The environment is output 0 of the func_operand_node.
                let closure_func_ptr_ty = ctx.get_port_type(func_operand_node, PortIndex(1))?;
                // Optional: Get env type for validation? let env_ty = ctx.get_port_type(func_operand_node, PortIndex(0))?;

                // Create the FunctionCall node, providing the specialized function pointer type.
                let call_node_id = ctx.mir_graph.add_node(MirNode::FunctionCall {
                    func_ty: closure_func_ptr_ty,
                });

                // Connect inputs to the FunctionCall node:
                // Input 0: Specialized function pointer (from Closure node Port 1)
                ctx.mir_graph.add_edge(func_operand_node, PortIndex(1), call_node_id, PortIndex(0));
                // Input 1: Environment tuple (from Closure node Port 0)
                ctx.mir_graph.add_edge(func_operand_node, PortIndex(0), call_node_id, PortIndex(1));
                // Inputs 2..N+1: Original arguments provided to the call expression.
                for (i, (arg_node, arg_port)) in arg_ports.iter().enumerate() {
                    ctx.mir_graph.add_edge(*arg_node, *arg_port, call_node_id, PortIndex(i as u32 + 2));
                }
                // The result of the call is output port 0.
                Ok((call_node_id, PortIndex(0)))

            } else {
                // --- Regular Function Call --- 
                // Assume the func_operand provides a regular function pointer.
                let func_ty = ctx.get_port_type(func_operand_node, func_operand_port)?;
                // Validate that it actually *is* a function pointer type.
                if !matches!(func_ty, MirType::FunctionPointer(..)) {
                    return Err(LoweringError::TypeMismatch(format!(
                        "Attempted to call a non-function type: {:?} (produced by node {:?}, port {}) for HIR value {:?}",
                        func_ty, func_operand_node, func_operand_port.0, value
                    )));
                }

                // Create the FunctionCall node.
                let call_node_id = ctx.mir_graph.add_node(MirNode::FunctionCall { func_ty: func_ty.clone() });

                // Connect inputs:
                // Input 0: Function pointer value (from the func_operand).
                ctx.mir_graph.add_edge(func_operand_node, func_operand_port, call_node_id, PortIndex(0));
                // Inputs 1..N: Original arguments.
                for (i, (arg_node, arg_port)) in arg_ports.iter().enumerate() {
                    ctx.mir_graph.add_edge(*arg_node, *arg_port, call_node_id, PortIndex(i as u32 + 1));
                }
                // Result is output port 0.
                Ok((call_node_id, PortIndex(0)))
            }
        }

        // Aggregate construction (Struct, Enum Variant, Tuple, Array).
        HirValue::Aggregate { kind, fields } => {
            // Lower all field operands first.
            let field_ports: Vec<(NodeId, PortIndex)> = fields.iter()
                .map(|op| lower_operand(ctx, op))
                .collect::<Result<_, _>>()?;

            // Get the MIR types of the lowered field values.
            let field_mir_types: Vec<MirType> = field_ports.iter()
                .map(|(id, port)| ctx.get_port_type(*id, *port))
                .collect::<Result<_, _>>()?;

            // Determine the kind of aggregate and create the appropriate MIR node.
            match kind {
                AggregateKind::Struct(struct_symbol) => {
                    let aggregate_ty = MirType::Adt(*struct_symbol);
                    let constructor_node_id = ctx.mir_graph.add_node(MirNode::Constructor {
                        tag: *struct_symbol,
                        field_types: field_mir_types,
                        ty: aggregate_ty,
                    });
                    // Connect field values to constructor inputs.
                    for (i, (field_node, field_port)) in field_ports.iter().enumerate() {
                         ctx.mir_graph.add_edge(*field_node, *field_port, constructor_node_id, PortIndex(i as u32));
                    }
                    Ok((constructor_node_id, PortIndex(0)))
                }
                AggregateKind::EnumVariant(variant_symbol) => {
                    // Need the parent enum's symbol for the Adt type.
                    let enum_symbol = ctx.get_enum_symbol_for_variant(*variant_symbol)?;
                    let aggregate_ty = MirType::Adt(enum_symbol);
                    let constructor_node_id = ctx.mir_graph.add_node(MirNode::Constructor {
                        tag: *variant_symbol, // Tag is the variant symbol itself.
                        field_types: field_mir_types,
                        ty: aggregate_ty,
                    });
                    // Connect field values to constructor inputs.
                    for (i, (field_node, field_port)) in field_ports.iter().enumerate() {
                         ctx.mir_graph.add_edge(*field_node, *field_port, constructor_node_id, PortIndex(i as u32));
                    }
                    Ok((constructor_node_id, PortIndex(0)))
                }
                AggregateKind::Tuple => {
                    let aggregate_ty = MirType::Tuple(field_mir_types.clone());
                    let constructor_node_id = ctx.mir_graph.add_node(MirNode::Constructor {
                        tag: Symbol(0), // Use a dummy tag for tuples.
                        field_types: field_mir_types,
                        ty: aggregate_ty,
                    });
                    // Connect field values to constructor inputs.
                    for (i, (field_node, field_port)) in field_ports.iter().enumerate() {
                         ctx.mir_graph.add_edge(*field_node, *field_port, constructor_node_id, PortIndex(i as u32));
                    }
                    Ok((constructor_node_id, PortIndex(0)))
                }
                AggregateKind::Array => {
                    // Handle empty array construction (if needed/allowed).
                    if field_mir_types.is_empty() {
                         // Decide on representation or error.
                         return Err(LoweringError::Unsupported("Lowering empty array construction is not yet supported".to_string()));
                    }
                    // Validate all elements have the same type.
                    let element_ty = field_mir_types[0].clone();
                    let size = field_mir_types.len();
                    if !field_mir_types.iter().all(|ty| *ty == element_ty) {
                         return Err(LoweringError::TypeMismatch("Array elements must all have the same type".to_string()));
                    }
                    // Create ArrayConstruct node.
                    let array_node_id = ctx.mir_graph.add_node(MirNode::ArrayConstruct {
                        element_ty: element_ty.clone(),
                        size,
                    });
                    // Connect element values to ArrayConstruct inputs.
                    for (i, (field_node, field_port)) in field_ports.iter().enumerate() {
                        ctx.mir_graph.add_edge(*field_node, *field_port, array_node_id, PortIndex(i as u32));
                    }
                    Ok((array_node_id, PortIndex(0)))
                }
            }
        }

        // Field/Index/Variant Projection.
        HirValue::Project { base, projection } => {
            // Lower the base operand being projected from.
            let (base_node, base_port) = lower_operand(ctx, base)?;
            // Get the type of the base value.
            let aggregate_ty = ctx.get_port_type(base_node, base_port)?;

            match projection {
                ProjectionKind::Field(field_symbol) => {
                    // Projecting a named struct field.
                    let field_index = ctx.get_field_index(&aggregate_ty, *field_symbol)?;
                    let field_ty = ctx.get_field_type(&aggregate_ty, field_index)?;
                    let project_node_id = ctx.mir_graph.add_node(MirNode::Project {
                        field_index,
                        aggregate_ty: aggregate_ty.clone(), // Store base type info
                        field_ty,
                    });
                    // Connect base value to projection input.
                    ctx.mir_graph.add_edge(base_node, base_port, project_node_id, PortIndex(0));
                    Ok((project_node_id, PortIndex(0)))
                }
                ProjectionKind::TupleIndex(idx) => {
                    // Projecting a tuple element by index.
                    let field_index = *idx;
                    let field_ty = ctx.get_field_type(&aggregate_ty, field_index)?;
                    let project_node_id = ctx.mir_graph.add_node(MirNode::Project {
                        field_index,
                        aggregate_ty: aggregate_ty.clone(),
                        field_ty,
                    });
                    // Connect base value to projection input.
                    ctx.mir_graph.add_edge(base_node, base_port, project_node_id, PortIndex(0));
                    Ok((project_node_id, PortIndex(0)))
                }
                ProjectionKind::ArrayIndex(index_operand) => {
                    // Projecting an array element by dynamic index.
                    // Lower the index operand.
                    let (index_node, index_port) = lower_operand(ctx, index_operand)?;
                    let index_ty = ctx.get_port_type(index_node, index_port)?;
                    // Validate index type.
                    match index_ty {
                        MirType::Primitive(ResolvePrimitiveType::I8 | ResolvePrimitiveType::I16 | ResolvePrimitiveType::I32 | ResolvePrimitiveType::I64 | ResolvePrimitiveType::I128 |
                        ResolvePrimitiveType::U8 | ResolvePrimitiveType::U16 | ResolvePrimitiveType::U32 | ResolvePrimitiveType::U64 | ResolvePrimitiveType::U128) => {},
                        _ => return Err(LoweringError::TypeMismatch(format!("Array index must be an integer type, found {:?}", index_ty))),
                    }

                    // Extract element type from the base array type.
                    let element_ty = match &aggregate_ty {
                         MirType::Array(elem_ty, _) => elem_ty.as_ref().clone(),
                         _ => return Err(LoweringError::TypeMismatch(format!("ArrayIndex projection requires an Array type, found {:?}", aggregate_ty))),
                    };

                    // Create ArrayProject node.
                    let project_node_id = ctx.mir_graph.add_node(MirNode::ArrayProject {
                         array_ty: aggregate_ty.clone(), // Store full array type
                         index_ty,
                         element_ty,
                    });
                    // Connect base array value to input 0.
                    ctx.mir_graph.add_edge(base_node, base_port, project_node_id, PortIndex(0));
                    // Connect index value to input 1.
                    ctx.mir_graph.add_edge(index_node, index_port, project_node_id, PortIndex(1));
                    Ok((project_node_id, PortIndex(0)))
                }
                // Add arm to handle Downcast explicitly, returning an error
                ProjectionKind::Downcast(variant_symbol) => {
                    Err(LoweringError::Unsupported(format!(
                        "Standalone Downcast projection for variant {:?} is unsupported. Use a match expression.",
                        variant_symbol
                    )))
                }
            }
        }

        // Closure creation.
        HirValue::Closure { function_symbol, captures } => {
            // Lower the operands that are captured by the closure.
            let capture_ports: Vec<(NodeId, PortIndex)> = captures.iter()
                .map(|op| lower_operand(ctx, op))
                .collect::<Result<_, _>>()?;
            // Determine the MIR types of the captured values.
            let capture_mir_types: Vec<MirType> = capture_ports.iter()
                .map(|(id, port)| ctx.get_port_type(*id, *port))
                .collect::<Result<_, _>>()?;

            // --- Update Closure Specialization Map --- 
            // Retrieve the mutable spec entry for this original lambda symbol.
            // This spec should have been created during the pre-pass.
            let spec = ctx.closure_spec_map.get_mut(function_symbol)
                .ok_or_else(|| LoweringError::Internal(format!(
                    "Closure specialization info not found in map for original lambda symbol {:?} during HirValue::Closure lowering",
                    function_symbol
                )))?;

            // **Store the determined capture types in the spec.**
            // This is crucial because the types might depend on the context where the closure
            // is defined, which is only known now during `lower_value`.
            if spec.capture_types.is_empty() {
                // First time encountering this closure spec during value lowering.
                spec.capture_types = capture_mir_types.clone();
            } else if spec.capture_types != capture_mir_types {
                // If we encounter the same original lambda later with *different* capture types,
                // this indicates an inconsistency, likely requiring different specializations
                // (which the current model doesn't support - it assumes one specialization per original lambda).
                // TODO: Revisit closure specialization strategy if needed for different capture type sets.
                return Err(LoweringError::Internal(format!(
                    "Inconsistent capture types detected for closure {:?}. Expected {:?}, found {:?}. Re-specialization might be needed.",
                    function_symbol, spec.capture_types, capture_mir_types
                )));
            }
            // --- End Update Closure Specialization Map ---

            // Clone signature *before* storing types back into spec
            let original_signature = spec.original_signature.clone();
            // Get specialized symbol
            let specialized_symbol = spec.specialized_symbol;

            // Construct the type of the environment tuple (captures).
            let env_ty = MirType::Tuple(capture_mir_types.clone());

            // Construct the type of the specialized function pointer.
            // Parameters = environment tuple followed by original parameters.
            let mut specialized_param_types = vec![env_ty.clone()]; // Start with env type
            specialized_param_types.extend(
                original_signature.params.iter().map(|(_, ty)| ctx.lower_type(ty))
            );
            let specialized_ret_type = Arc::new(ctx.lower_type(&original_signature.return_type));
            let specialized_func_ptr_ty = MirType::FunctionPointer(specialized_param_types, specialized_ret_type);

            // Create the Closure node in the MIR graph.
            let closure_node_id = ctx.mir_graph.add_node(MirNode::Closure {
                original_lambda_symbol: *function_symbol,
                specialized_function_symbol: specialized_symbol,
                capture_types: capture_mir_types.clone(), // Store capture types in the node
                env_ty,
                func_ptr_ty: specialized_func_ptr_ty,
            });

            // Connect the captured values to the Closure node inputs.
            for (i, (capture_node, capture_port)) in capture_ports.iter().enumerate() {
                ctx.mir_graph.add_edge(*capture_node, *capture_port, closure_node_id, PortIndex(i as u32));
            }

            // The `HirValue::Closure` itself evaluates to the environment tuple (output port 0).
            // The function pointer (output port 1) is used implicitly when this closure is called.
            Ok((closure_node_id, PortIndex(0)))
        }
    }
}

/// Lowers an HIR expression ([`HirExpr`]) into MIR nodes and edges.
///
/// An `HirExpr` is either a `let` binding followed by another expression, or a terminal
/// `TailExpr`. This function handles the recursive structure and variable scoping.
///
/// # Arguments
/// * `ctx`: The mutable lowering context.
/// * `expr`: The HIR expression to lower.
///
/// # Returns
/// * `Ok((NodeId, PortIndex))`: The node and output port producing the final value of the expression sequence.
/// * `Err(...)`: Propagates errors from `lower_value` or `lower_tail_expr`.
pub(super) fn lower_expr<'ctx>(
    ctx: &mut FunctionLoweringContext<'ctx>,
    expr: &HirExpr,
) -> Result<(NodeId, PortIndex), LoweringError> {
    match &expr.kind {
        HirExprKind::Let { var, var_ty: _, value, rest } => {
            // 1. Lower the value being bound.
            let value_port @ (value_node_id, value_port_index) = lower_value(ctx, value)?;

            // 2. Introduce the variable binding into the context for the `rest` expression.
            //    Handle potential shadowing by saving the old binding.
            //    TODO: Consider a more robust scoping mechanism if complex shadowing rules arise.
            let old_binding = ctx.var_map.insert(*var, value_port);

            // 3. Recursively lower the rest of the expression sequence (`rest`).
            let result_port = lower_expr(ctx, rest)?;

            // 4. Restore the previous variable binding (if any) after `rest` is lowered.
            //    This correctly handles shadowing: the variable reverts to its previous
            //    meaning (or becomes undefined) outside the scope of the `let`.
            if let Some(old_shadowed_port) = old_binding {
                // There was a previous binding for this var; restore it.
                ctx.var_map.insert(*var, old_shadowed_port);
            } else {
                // This var was newly introduced; remove it from the map.
                ctx.var_map.remove(var);
            }

            // The result of the `let` expression is the result of the `rest` part.
            Ok(result_port)
        }
        // If the expression is just a terminal tail expression.
        HirExprKind::Tail(tail_expr) => lower_tail_expr(ctx, tail_expr),
    }
}

/// Lowers an HIR tail expression ([`HirTailExpr`]) into MIR nodes and edges.
///
/// Tail expressions represent the final computation in an expression sequence (like `return`,
/// `if`, `match`, or diverging `never`). They typically determine the final value of the
/// containing `HirExpr` sequence or block.
///
/// # Arguments
/// * `ctx`: The mutable lowering context.
/// * `tail_expr`: The HIR tail expression to lower.
///
/// # Returns
/// * `Ok((NodeId, PortIndex))`: The node and output port producing the final value of the tail expression.
///   For `Never`, this might be a dummy port from an `Unreachable` node.
/// * `Err(...)`: Propagates errors from `lower_operand`, `lower_expr`, or returns specific errors
///   for type mismatches (e.g., in `if` branches) or unsupported constructs (e.g., non-exhaustive `match`).
pub(super) fn lower_tail_expr<'ctx>(
    ctx: &mut FunctionLoweringContext<'ctx>,
    tail_expr: &HirTailExpr,
) -> Result<(NodeId, PortIndex), LoweringError> {
    match tail_expr {
        // Return statement: The result is simply the value of the returned operand.
        HirTailExpr::Value(operand) => lower_operand(ctx, operand),

        // If expression.
        HirTailExpr::If { condition, then_branch, else_branch } => {
            // Lower the condition operand.
            let (cond_node, cond_port) = lower_operand(ctx, condition)?;
            let cond_ty = ctx.get_port_type(cond_node, cond_port)?;
            // TODO: Validate cond_ty is Bool?

            // Recursively lower the `then` and `else` branches (which are `HirExpr`s).
            let (then_node, then_port) = lower_expr(ctx, then_branch)?;
            let (else_node, else_port) = lower_expr(ctx, else_branch)?;

            // Get the result types of the branches.
            let result_ty_then = ctx.get_port_type(then_node, then_port)?;
            let result_ty_else = ctx.get_port_type(else_node, else_port)?;

            // Check if branch types match (or if one is Never).
            let final_if_ty = if result_ty_then == result_ty_else {
                result_ty_then // Types match, use either.
            } else if result_ty_then.is_never() {
                result_ty_else // If `then` is Never, result type is `else` type.
            } else if result_ty_else.is_never() {
                result_ty_then // If `else` is Never, result type is `then` type.
            } else {
                // Types mismatch and neither is Never.
                return Err(LoweringError::TypeMismatch(format!(
                    "If branches have incompatible types: then={:?}, else={:?}",
                    result_ty_then, result_ty_else
                )));
            };

            // Create the IfValue node.
            let if_node_id = ctx.mir_graph.add_node(MirNode::IfValue {
                condition_ty: cond_ty, // Store condition type for reference
                ty: final_if_ty,      // Result type of the node
            });

            // Connect condition, then, and else values to the IfValue inputs.
            ctx.mir_graph.add_edge(cond_node, cond_port, if_node_id, PortIndex(0)); // Condition
            ctx.mir_graph.add_edge(then_node, then_port, if_node_id, PortIndex(1)); // Then value
            ctx.mir_graph.add_edge(else_node, else_port, if_node_id, PortIndex(2)); // Else value

            // Result is the output port 0 of the IfValue node.
            Ok((if_node_id, PortIndex(0)))
        }

         // Match expression.
         HirTailExpr::Match { scrutinee, arms, otherwise } => {
            // --- Match Lowering ---
            let (scrutinee_node, scrutinee_port) = lower_operand(ctx, scrutinee)?;
            let scrutinee_ty = ctx.get_port_type(scrutinee_node, scrutinee_port)?;

            // TODO: Need robust scrutinee duplication for each check.

            let mut current_else_result_port: Option<(NodeId, PortIndex)> = None;

            if let Some(otherwise_expr) = otherwise {
                 current_else_result_port = Some(lower_expr(ctx, otherwise_expr)?);
            }

            for (pattern, arm_expr) in arms.iter().rev() {
                let arm_result_port = lower_expr(ctx, arm_expr)?;

                let (cond_node, cond_port) = match pattern {
                    HirPattern::Variant { variant_symbol, bindings } => {
                         // Placeholder: Create a dummy 'true' constant for now.
                         // TODO: Implement IsVariant intrinsic call
                         let true_const_node = ctx.mir_graph.add_node(MirNode::Constant {
                             value: parallax_hir::hir::HirLiteral::BoolLiteral(true),
                             ty: ctx.lower_type(&HirType::Primitive(HirPrimitiveType::Bool)),
                         });
                         Ok((true_const_node, PortIndex(0)))
                    }
                    HirPattern::Const(literal) => {
                         // Placeholder: Create a dummy 'true' constant for now.
                         // TODO: Implement Eq intrinsic call
                         let true_const_node = ctx.mir_graph.add_node(MirNode::Constant {
                             value: parallax_hir::hir::HirLiteral::BoolLiteral(true),
                             ty: ctx.lower_type(&HirType::Primitive(HirPrimitiveType::Bool)),
                         });
                         Ok((true_const_node, PortIndex(0)))
                    }
                    HirPattern::Wildcard => {
                         if current_else_result_port.is_none() {
                             current_else_result_port = Some(arm_result_port);
                              continue;
                         } else {
                              return Err(LoweringError::Unsupported("Multiple Wildcard/Bind patterns or explicit otherwise in match".to_string()));
                         }
                    }
                    HirPattern::Bind { var, var_ty } => {
                         // TODO: Handle binding *var = scrutinee
                         if current_else_result_port.is_none() {
                              current_else_result_port = Some(arm_result_port);
                              continue;
                         } else {
                              return Err(LoweringError::Unsupported("Multiple Wildcard/Bind patterns or explicit otherwise in match".to_string()));
                         }
                    }
                }?;

                let else_result_port = current_else_result_port.ok_or_else(||
                    LoweringError::Internal("Match lowering failed: missing else branch during reverse iteration, but match is exhaustive.".to_string())
                )?;

                let cond_ty = ctx.get_port_type(cond_node, cond_port)?;
                let then_ty = ctx.get_port_type(arm_result_port.0, arm_result_port.1)?;
                let else_ty = ctx.get_port_type(else_result_port.0, else_result_port.1)?;

                let final_ty = if then_ty == else_ty {
                    then_ty
                } else if then_ty.is_never() {
                    else_ty
                } else if else_ty.is_never() {
                    then_ty
                } else {
                    return Err(LoweringError::TypeMismatch(format!(
                        "Match arm types incompatible: then={:?}, else={:?}",
                        then_ty, else_ty
                    )));
                };

                let if_node_id = ctx.mir_graph.add_node(MirNode::IfValue {
                    condition_ty: cond_ty,
                    ty: final_ty,
                });

                ctx.mir_graph.add_edge(cond_node, cond_port, if_node_id, PortIndex(0));
                ctx.mir_graph.add_edge(arm_result_port.0, arm_result_port.1, if_node_id, PortIndex(1));
                ctx.mir_graph.add_edge(else_result_port.0, else_result_port.1, if_node_id, PortIndex(2));

                current_else_result_port = Some((if_node_id, PortIndex(0)));
            }

            current_else_result_port.ok_or_else(|| LoweringError::Internal("Match lowering failed: No final result port obtained.".to_string()))
        }

        // Never expression (diverges).
        HirTailExpr::Never => {
             // Create an Unreachable node.
             let unreachable_node_id = ctx.mir_graph.add_node(MirNode::Unreachable);
             // Unreachable nodes don't produce a value via an output *port* in the dataflow sense.
             // However, the containing `HirExpr` needs *some* result port.
             // We return the NodeId and PortIndex(0) as a convention, but callers should
             // ideally check the type (`get_port_type` will err for Unreachable).
             // Alternatively, handle `Never` types specially in calling code (like `lower_match`).
             // For now, using (node_id, 0) allows the basic structure to work.
             Ok((unreachable_node_id, PortIndex(0)))
        }
    }
}


