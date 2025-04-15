//!
//! Contains the core logic for lowering HIR expressions (`HirExpr`, `HirValue`, `HirTailExpr`, `Operand`)
//! into MIR nodes and edges within a `FunctionLoweringContext`.

use super::*;
use crate::mir::{MirNode, MirOp}; // Import MIR specifics needed here
use parallax_common::hir::{AggregateKind, HirExpr, HirExprKind, HirLiteral, HirPattern, HirTailExpr, HirType, HirValue, HirVar, Operand, ProjectionKind, ResolvePrimitiveType}; // Import HIR specifics needed

/// Lowers an HIR operand (variable, constant, global) to the MIR port that produces its value.
pub(super) fn lower_operand<'a>(
    ctx: &mut FunctionLoweringContext<'a>,
    operand: &Operand,
) -> Result<(NodeId, PortIndex), LoweringError> {
    // Check if this operand corresponds to a captured variable in a specialized function
    if let Some(param_index) = ctx.captured_operand_map.get(operand) {
        // It's a capture! Find the corresponding Parameter node.
        // Parameter nodes are added in order: captures first, then original params.
        if let Some(param_node_id) = ctx.mir_graph.parameter_nodes.get(*param_index as usize) {
            return Ok((*param_node_id, PortIndex(0)));
        } else {
            return Err(LoweringError::Internal(format!(
                "Captured operand {:?} mapped to invalid parameter index {}",
                operand, param_index
            )));
        }
    }
    
    // If not a capture, proceed as before
    match operand {
        Operand::Var(var) => {
            // Look up in the regular variable map
            ctx.var_map.get(var).copied()
                .ok_or_else(|| LoweringError::UndefinedVariable(*var))
        }
        Operand::Const(literal) => {
            let ty = ctx.lower_type(&ctx.get_literal_type(literal));
            let node_id = ctx.mir_graph.add_node(MirNode::Constant { value: literal.clone(), ty });
            Ok((node_id, PortIndex(0)))
        }
        Operand::Global(symbol) => {
            let ty = ctx.get_global_type(*symbol)?;
            let node_id = ctx.mir_graph.add_node(MirNode::StaticAddr { symbol: *symbol, ty });
            Ok((node_id, PortIndex(0)))
        }
    }
}

/// Lowers an HIR value computation into MIR nodes and returns the output port.
pub(super) fn lower_value<'a>(
    ctx: &mut FunctionLoweringContext<'a>,
    value: &HirValue,
) -> Result<(NodeId, PortIndex), LoweringError> {
     match value {
        HirValue::Use(operand) => lower_operand(ctx, operand),
        HirValue::Call { func, args } => {
            let (func_operand_node, func_operand_port) = lower_operand(ctx, func)?;

            // Check the node producing the function operand
            let func_node_def = ctx.mir_graph.nodes.get(&func_operand_node)
                .ok_or_else(|| LoweringError::Internal(format!(
                    "Node {:?} providing function operand not found", func_operand_node
                )))?;
            
            // Lower arguments regardless of call type
            let arg_ports_results: Result<Vec<_>, _> = args.iter().map(|arg| lower_operand(ctx, arg)).collect();
            let arg_ports = arg_ports_results?;

            match func_node_def {
                // If the function operand comes directly from a Closure node, it's a closure call.
                MirNode::Closure { .. } => {
                    // Get the types from the Closure node itself
                    let _closure_env_ty = ctx.get_port_type(func_operand_node, PortIndex(0))?; // Env type needed for validation?
                    let closure_func_ptr_ty = ctx.get_port_type(func_operand_node, PortIndex(1))?;

                    // --- Closure Call --- 
                    let call_node_id = ctx.mir_graph.add_node(MirNode::FunctionCall { func_ty: closure_func_ptr_ty });

                    // Input 0: Specialized function pointer (from Closure node Port 1)
                    ctx.mir_graph.add_edge(func_operand_node, PortIndex(1), call_node_id, PortIndex(0));
                    
                    // Input 1: Environment tuple (from Closure node Port 0)
                    ctx.mir_graph.add_edge(func_operand_node, PortIndex(0), call_node_id, PortIndex(1));
                    
                    // Inputs 2..N+1: Original arguments
                    for (i, (arg_node, arg_port)) in arg_ports.iter().enumerate() {
                        ctx.mir_graph.add_edge(*arg_node, *arg_port, call_node_id, PortIndex(i as u32 + 2));
                    }
                    Ok((call_node_id, PortIndex(0)))
                }
                // Otherwise, it's a regular function call (or an error)
                _ => {
                    // --- Regular Function Call --- 
                    let func_ty = ctx.get_port_type(func_operand_node, func_operand_port)?;
                    if !matches!(func_ty, MirType::FunctionPointer(..)) {
                        return Err(LoweringError::TypeMismatch(format!(
                            "Attempted to call a non-function type: {:?} produced by node {:?}", 
                            func_ty, func_operand_node
                        )));
                    }

                    let call_node_id = ctx.mir_graph.add_node(MirNode::FunctionCall { func_ty: func_ty.clone() });
                    
                    // Input 0: Function value (from the original operand port)
                    ctx.mir_graph.add_edge(func_operand_node, func_operand_port, call_node_id, PortIndex(0));
                    
                    // Inputs 1..N: Original arguments
                    for (i, (arg_node, arg_port)) in arg_ports.iter().enumerate() {
                        ctx.mir_graph.add_edge(*arg_node, *arg_port, call_node_id, PortIndex(i as u32 + 1));
                    }
                    Ok((call_node_id, PortIndex(0)))
                }
            }
        }
        HirValue::Aggregate { kind, fields } => {
            // Collect results from lower_operand for fields
            let field_ports_results: Result<Vec<_>, _> = fields.iter().map(|op| lower_operand(ctx, op)).collect();
            let field_ports = field_ports_results?; // Propagate error

            // Collect results from get_port_type for field types
            let field_types_results: Result<Vec<_>, _> = field_ports.iter().map(|(id, port)| ctx.get_port_type(*id, *port)).collect();
            let field_types = field_types_results?; // Propagate error

            let (tag, aggregate_ty) = match kind {
                AggregateKind::Struct(s) => (*s, MirType::Adt(*s)),
                AggregateKind::EnumVariant(variant_symbol) => {
                    let enum_symbol = ctx.get_enum_symbol_for_variant(*variant_symbol)?;
                    (*variant_symbol, MirType::Adt(enum_symbol))
                }
                AggregateKind::Tuple => {
                    (Symbol(0), MirType::Tuple(field_types.clone()))
                }
                AggregateKind::Array => {
                    if field_types.is_empty() {
                         return Err(LoweringError::Unsupported("Lowering empty array construction".to_string()));
                    }
                    let element_ty = field_types[0].clone();
                    let size = field_types.len();
                    if !field_types.iter().all(|ty| *ty == element_ty) {
                         return Err(LoweringError::TypeMismatch("Array elements must all have the same type".to_string()));
                    }
                    let array_node_id = ctx.mir_graph.add_node(MirNode::ArrayConstruct {
                        element_ty: element_ty.clone(),
                        size,
                    });
                    for (i, (field_node, field_port)) in field_ports.iter().enumerate() {
                        ctx.mir_graph.add_edge(*field_node, *field_port, array_node_id, PortIndex(i as u32));
                    }
                    return Ok((array_node_id, PortIndex(0)));
                }
            };
            let constructor_node_id = ctx.mir_graph.add_node(MirNode::Constructor {
                tag,
                field_types,
                ty: aggregate_ty,
            });
            for (i, (field_node, field_port)) in field_ports.iter().enumerate() {
                 ctx.mir_graph.add_edge(*field_node, *field_port, constructor_node_id, PortIndex(i as u32));
            }
            Ok((constructor_node_id, PortIndex(0)))
        }
        HirValue::Project { base, projection } => {
            let (base_node, base_port) = lower_operand(ctx, base)?;
            let aggregate_ty = ctx.get_port_type(base_node, base_port)?;

            match projection {
                ProjectionKind::Field(field_symbol) => {
                    let field_index = ctx.get_field_index(&aggregate_ty, *field_symbol)?;
                    let field_ty = ctx.get_field_type(&aggregate_ty, field_index)?;
                    let project_node_id = ctx.mir_graph.add_node(MirNode::Project {
                        field_index,
                        aggregate_ty,
                        field_ty,
                    });
                    ctx.mir_graph.add_edge(base_node, base_port, project_node_id, PortIndex(0));
                    Ok((project_node_id, PortIndex(0)))
                }
                ProjectionKind::TupleIndex(idx) => {
                    let field_index = *idx;
                    let field_ty = ctx.get_field_type(&aggregate_ty, field_index)?;
                    let project_node_id = ctx.mir_graph.add_node(MirNode::Project {
                        field_index,
                        aggregate_ty,
                        field_ty,
                    });
                    ctx.mir_graph.add_edge(base_node, base_port, project_node_id, PortIndex(0));
                    Ok((project_node_id, PortIndex(0)))
                }
                ProjectionKind::Downcast(variant_symbol) => {
                    let enum_symbol = match aggregate_ty {
                         MirType::Adt(s) => s,
                         _ => return Err(LoweringError::TypeMismatch("Downcast projection requires an ADT type".to_string()))
                    };
                    let enum_def = ctx.hir_module.enums.iter().find(|e| e.symbol == enum_symbol)
                         .ok_or_else(|| LoweringError::Internal(format!("Enum definition not found for symbol {:?}", enum_symbol)))?;
                    let variant_def = enum_def.variants.iter().find(|v| v.symbol == *variant_symbol)
                         .ok_or_else(|| LoweringError::Internal(format!("Variant symbol {:?} not found in enum {:?}", variant_symbol, enum_symbol)))?;
                    
                    let payload_hir_type = HirType::Tuple(variant_def.fields.clone());
                    let payload_ty = ctx.lower_type(&payload_hir_type);

                    let downcast_node_id = ctx.mir_graph.add_node(MirNode::Downcast {
                        enum_symbol,
                        variant_symbol: *variant_symbol,
                        payload_ty,
                    });
                    ctx.mir_graph.add_edge(base_node, base_port, downcast_node_id, PortIndex(0));
                    Ok((downcast_node_id, PortIndex(0)))
                }
                ProjectionKind::ArrayIndex(index_operand) => {
                    let (index_node, index_port) = lower_operand(ctx, index_operand)?;
                    let index_ty = ctx.get_port_type(index_node, index_port)?;
                    match index_ty {
                        MirType::Primitive(ResolvePrimitiveType::I8 | ResolvePrimitiveType::I16 | ResolvePrimitiveType::I32 | ResolvePrimitiveType::I64 | ResolvePrimitiveType::I128 | ResolvePrimitiveType::U8 | ResolvePrimitiveType::U16 | ResolvePrimitiveType::U32 | ResolvePrimitiveType::U64 | ResolvePrimitiveType::U128) => {},
                        _ => return Err(LoweringError::TypeMismatch(format!("Array index must be an integer, found {:?}", index_ty))),
                    }

                    let element_ty = match &aggregate_ty {
                         MirType::Array(elem_ty, _) => elem_ty.as_ref().clone(),
                         _ => return Err(LoweringError::TypeMismatch("ArrayIndex projection requires an Array type".to_string())),
                    };

                    let project_node_id = ctx.mir_graph.add_node(MirNode::ArrayProject {
                         array_ty: aggregate_ty.clone(),
                         index_ty,
                         element_ty,
                    });
                    ctx.mir_graph.add_edge(base_node, base_port, project_node_id, PortIndex(0));
                    ctx.mir_graph.add_edge(index_node, index_port, project_node_id, PortIndex(1));
                    Ok((project_node_id, PortIndex(0)))
                }
            }
        }
        HirValue::Closure { function_symbol, captures } => {
            let capture_ports_results: Result<Vec<_>, _> = captures.iter().map(|op| lower_operand(ctx, op)).collect();
            let capture_ports = capture_ports_results?;
            let capture_types_results: Result<Vec<_>, _> = capture_ports.iter().map(|(id, port)| ctx.get_port_type(*id, *port)).collect();
            let capture_types = capture_types_results?;

            let spec = ctx.closure_spec_map.get_mut(function_symbol)
                .ok_or_else(|| LoweringError::Internal(format!(
                    "Closure specialization info not found for original symbol {:?}", 
                    function_symbol
                )))?;
            
            if spec.capture_types.is_empty() {
                spec.capture_types = capture_types.clone();
            } else if spec.capture_types != capture_types {
                return Err(LoweringError::Internal(format!(
                    "Inconsistent capture types detected for closure {:?}", function_symbol
                )));
            }

            let specialized_symbol = spec.specialized_symbol;
            let original_signature = &spec.original_signature;
            let env_ty = MirType::Tuple(capture_types.clone());
            let mut specialized_param_types = capture_types.clone();
            specialized_param_types.extend(
                original_signature.params.iter().map(|(_, ty)| ctx.lower_type(ty))
            );
            let specialized_ret_type = Arc::new(ctx.lower_type(&original_signature.return_type));
            let func_ptr_ty = MirType::FunctionPointer(specialized_param_types, specialized_ret_type);
            
            let closure_node_id = ctx.mir_graph.add_node(MirNode::Closure {
                original_lambda_symbol: *function_symbol,
                specialized_function_symbol: specialized_symbol,
                capture_types: capture_types.clone(), 
                env_ty,
                func_ptr_ty,
            });

            for (i, (capture_node, capture_port)) in capture_ports.iter().enumerate() {
                ctx.mir_graph.add_edge(*capture_node, *capture_port, closure_node_id, PortIndex(i as u32));
            }

            Ok((closure_node_id, PortIndex(0)))
        }
    }
}

/// Lowers an HIR expression (let binding or tail expression) into MIR nodes.
pub(super) fn lower_expr<'a>(
    ctx: &mut FunctionLoweringContext<'a>,
    expr: &HirExpr,
) -> Result<(NodeId, PortIndex), LoweringError> {
    match &expr.kind {
        HirExprKind::Let { var, var_ty: _, value, rest } => {
            let value_port = lower_value(ctx, value)?; // Use ?
            // Temporarily store the old binding if shadowing
            let old_binding = ctx.var_map.insert(*var, value_port);
            let result_port = lower_expr(ctx, rest)?; // Use ?
            // Restore old binding if it existed, otherwise remove the current one
            if let Some(old_shadowed_port) = old_binding {
                ctx.var_map.insert(*var, old_shadowed_port);
            } else {
                ctx.var_map.remove(var);
            }
            Ok(result_port)
        }
        HirExprKind::Tail(tail_expr) => lower_tail_expr(ctx, tail_expr),
    }
}

/// Lowers an HIR tail expression (return, if, match, never) into MIR nodes.
pub(super) fn lower_tail_expr<'a>(
    ctx: &mut FunctionLoweringContext<'a>,
    tail_expr: &HirTailExpr,
) -> Result<(NodeId, PortIndex), LoweringError> {
    match tail_expr {
        HirTailExpr::Return(operand) => lower_operand(ctx, operand),
        HirTailExpr::If { condition, then_branch, else_branch } => {
            let (cond_node, cond_port) = lower_operand(ctx, condition)?;
            let cond_ty = ctx.get_port_type(cond_node, cond_port)?;
            let (then_node, then_port) = lower_expr(ctx, then_branch)?; 
            let (else_node, else_port) = lower_expr(ctx, else_branch)?; 
            let result_ty_then = ctx.get_port_type(then_node, then_port)?;
            let result_ty_else = ctx.get_port_type(else_node, else_port)?;

            // Use proper error propagation for type mismatch
            if result_ty_then != result_ty_else {
                 return Err(LoweringError::TypeMismatch(format!(
                     "If branches have different types: {:?} vs {:?}",
                     result_ty_then, result_ty_else
                 )));
            }
            
            let if_node_id = ctx.mir_graph.add_node(MirNode::IfValue {
                condition_ty: cond_ty,
                ty: result_ty_then, // Use the (now verified) type
            });
            ctx.mir_graph.add_edge(cond_node, cond_port, if_node_id, PortIndex(0));
            ctx.mir_graph.add_edge(then_node, then_port, if_node_id, PortIndex(1)); 
            ctx.mir_graph.add_edge(else_node, else_port, if_node_id, PortIndex(2));
            Ok((if_node_id, PortIndex(0)))
        }
         // Still panic for Match, assuming it's lowered earlier or needs dedicated node
         HirTailExpr::Match { scrutinee, arms, otherwise } => {
             lower_match(ctx, scrutinee, arms, otherwise)
         },
        HirTailExpr::Never => {
             let unreachable_node_id = ctx.mir_graph.add_node(MirNode::Unreachable);
             // For an Unreachable node, there isn't really a meaningful output port.
             // Returning a dummy port (NodeId, PortIndex(0)) might be okay if the caller
             // handles the type correctly (e.g., knows it's Never). 
             // However, it's safer to perhaps indicate this differently if possible.
             // For now, returning the node ID and port 0, assuming consumers check type.
             Ok((unreachable_node_id, PortIndex(0))) 
        }
    }
}

/// Lowers an HIR match expression into nested MIR `IfValue` and `BinaryOp`/`IsVariant` nodes.
pub(super) fn lower_match<'a>(
    ctx: &mut FunctionLoweringContext<'a>,
    scrutinee_operand: &Operand,
    arms: &[(HirPattern, HirExpr)],
    otherwise: &Option<Box<HirExpr>>,
) -> Result<(NodeId, PortIndex), LoweringError> {
    let (scrutinee_node, scrutinee_port) = lower_operand(ctx, scrutinee_operand)?;
    let scrutinee_ty = ctx.get_port_type(scrutinee_node, scrutinee_port)?;

    // Lower the 'otherwise' branch first, if it exists.
    // This will be the final 'else' if no arms match.
    let final_else_port = if let Some(otherwise_expr) = otherwise {
        lower_expr(ctx, otherwise_expr)?
    } else {
        // Previously returned Unreachable. Now return error if not exhaustive.
        return Err(LoweringError::Unsupported(
            "Non-exhaustive match expressions without an `otherwise` clause are not supported.".to_string()
        ));
        // let unreachable_node = ctx.mir_graph.add_node(MirNode::Unreachable);
        // (unreachable_node, PortIndex(0)) // Dummy port
    };

    // Lower arms iteratively, building nested Ifs/Downcasts
    let mut current_else_port = final_else_port;

    for (pattern, arm_expr) in arms.iter().rev() { // Iterate backwards to build nested structure
        match pattern {
            HirPattern::Variant { variant_symbol, bindings } => {
                // 1. Get Enum Info
                let enum_symbol = match &scrutinee_ty {
                     MirType::Adt(s) => *s,
                      _ => return Err(LoweringError::TypeMismatch("Match on variant requires ADT scrutinee".to_string()))
                 };

                // 2. Check if scrutinee is the correct variant
                let is_variant_node = ctx.mir_graph.add_node(MirNode::IsVariant {
                     enum_symbol,
                     variant_symbol: *variant_symbol,
                 });
                ctx.mir_graph.add_edge(scrutinee_node, scrutinee_port, is_variant_node, PortIndex(0));
                let condition_port = (is_variant_node, PortIndex(0));

                // 3. If it matches (Then branch), lower the arm expression with bindings.
                let then_port = {
                     // Scope for bindings
                     let mut bindings_restoration = Vec::new();

                     // Downcast *inside* the conditional branch to get the payload
                     let variant_def = ctx.hir_module.enums.iter()
                         .find(|e| e.symbol == enum_symbol)
                         .ok_or_else(|| LoweringError::Internal(format!(
                             "Enum definition {:?} not found during match lowering", enum_symbol
                         )))?
                         .variants.iter().find(|v| v.symbol == *variant_symbol)
                         .ok_or_else(|| LoweringError::Internal(format!(
                             "Variant definition {:?} not found in enum {:?} during match lowering",
                             variant_symbol, enum_symbol
                         )))?;
                     let payload_hir_type = HirType::Tuple(variant_def.fields.clone());
                     let payload_ty = ctx.lower_type(&payload_hir_type);

                     let downcast_node = ctx.mir_graph.add_node(MirNode::Downcast {
                         enum_symbol,
                         variant_symbol: *variant_symbol,
                         payload_ty: payload_ty.clone(),
                     });
                     // IMPORTANT: Downcast input comes from the *original scrutinee*, not the boolean result
                     ctx.mir_graph.add_edge(scrutinee_node, scrutinee_port, downcast_node, PortIndex(0));
                     let downcast_payload_port = (downcast_node, PortIndex(0));

                     // Bind variables from payload fields
                     if bindings.len() != variant_def.fields.len() {
                         return Err(LoweringError::Internal(format!("Pattern binding count mismatch for variant {:?}", variant_symbol)));
                     }
                     for (i, (binding_var, _binding_ty)) in bindings.iter().enumerate() {
                         let field_ty = ctx.get_field_type(&payload_ty, i as u32)?;
                         let project_node = ctx.mir_graph.add_node(MirNode::Project {
                             field_index: i as u32,
                             aggregate_ty: payload_ty.clone(),
                             field_ty,
                         });
                         ctx.mir_graph.add_edge(downcast_payload_port.0, downcast_payload_port.1, project_node, PortIndex(0));
                         let binding_port = (project_node, PortIndex(0));
                         // Shadow previous binding if necessary, save old value
                         let old_binding = ctx.var_map.insert(*binding_var, binding_port);
                         bindings_restoration.push((*binding_var, old_binding));
                     }

                     // Lower the arm expression itself
                     let arm_result_port = lower_expr(ctx, arm_expr)?;

                     // Restore var_map state after lowering arm expression
                     for (var, old_binding) in bindings_restoration {
                        if let Some(port) = old_binding {
                            ctx.var_map.insert(var, port);
                        } else {
                            ctx.var_map.remove(&var);
                        }
                     }
                     arm_result_port // Return the result of the arm
                };

                // 4. Create the IfValue node
                // The type of the IfValue node must match the type of the 'else' branch (result of previous arms/otherwise)
                let result_ty = ctx.get_port_type(current_else_port.0, current_else_port.1)?;
                let then_ty = ctx.get_port_type(then_port.0, then_port.1)?;
                // Handle case where one branch is Never
                if result_ty != then_ty && !result_ty.is_never() && !then_ty.is_never() {
                    return Err(LoweringError::TypeMismatch(format!(
                        "Match arm types mismatch: {:?} vs {:?}",
                        then_ty, result_ty
                    )));
                }
                // If one branch is Never, the resulting type is the type of the other branch.
                let final_if_ty = if result_ty.is_never() { then_ty } else { result_ty };

                let if_node = ctx.mir_graph.add_node(MirNode::IfValue {
                     condition_ty: MirType::Primitive(ResolvePrimitiveType::Bool),
                     ty: final_if_ty, 
                });
                ctx.mir_graph.add_edge(condition_port.0, condition_port.1, if_node, PortIndex(0)); // Condition
                ctx.mir_graph.add_edge(then_port.0, then_port.1, if_node, PortIndex(1)); // Then branch
                ctx.mir_graph.add_edge(current_else_port.0, current_else_port.1, if_node, PortIndex(2)); // Else branch

                // The output of this If becomes the input for the next iteration's Else
                current_else_port = (if_node, PortIndex(0));
            }
            HirPattern::Const(literal) => {
                 // 1. Lower the constant literal
                 let (const_node, const_port) = lower_operand(ctx, &Operand::Const(literal.clone()))?;
                 let const_ty = ctx.get_port_type(const_node, const_port)?;

                 // Check if scrutinee and const types match
                 if scrutinee_ty != const_ty {
                      // This might indicate an earlier type error, but we catch it here too.
                      return Err(LoweringError::TypeMismatch(format!(
                          "Scrutinee type {:?} does not match constant pattern type {:?}",
                          scrutinee_ty, const_ty
                      )));
                 }

                 // 2. Create BinaryOp for equality check
                 let eq_node = ctx.mir_graph.add_node(MirNode::BinaryOp {
                      op: MirOp::Eq,
                      lhs_ty: scrutinee_ty.clone(),
                      rhs_ty: const_ty,
                      result_ty: MirType::Primitive(ResolvePrimitiveType::Bool),
                 });
                 ctx.mir_graph.add_edge(scrutinee_node, scrutinee_port, eq_node, PortIndex(0)); // LHS
                 ctx.mir_graph.add_edge(const_node, const_port, eq_node, PortIndex(1)); // RHS
                 let condition_port = (eq_node, PortIndex(0));

                 // 3. Lower the arm expression (Then branch)
                 let then_port = lower_expr(ctx, arm_expr)?;
                 let then_ty = ctx.get_port_type(then_port.0, then_port.1)?;

                 // 4. Create the IfValue node
                 let result_ty = ctx.get_port_type(current_else_port.0, current_else_port.1)?;
                 if result_ty != then_ty && !result_ty.is_never() && !then_ty.is_never() {
                     return Err(LoweringError::TypeMismatch(format!(
                         "Match arm types mismatch: {:?} vs {:?}",
                         then_ty, result_ty
                     )));
                 }
                 let final_if_ty = if result_ty.is_never() { then_ty } else { result_ty };

                 let if_node = ctx.mir_graph.add_node(MirNode::IfValue {
                      condition_ty: MirType::Primitive(ResolvePrimitiveType::Bool),
                      ty: final_if_ty,
                 });
                 ctx.mir_graph.add_edge(condition_port.0, condition_port.1, if_node, PortIndex(0));
                 ctx.mir_graph.add_edge(then_port.0, then_port.1, if_node, PortIndex(1));
                 ctx.mir_graph.add_edge(current_else_port.0, current_else_port.1, if_node, PortIndex(2));

                 // Output of If becomes the new Else
                 current_else_port = (if_node, PortIndex(0));
             }
            HirPattern::Bind { var, .. } => {
                // Bind the scrutinee itself to the variable
                let old_binding = ctx.var_map.insert(*var, (scrutinee_node, scrutinee_port));
                let arm_then_port = lower_expr(ctx, arm_expr)?;
                // Restore binding
                if let Some(port) = old_binding {
                     ctx.var_map.insert(*var, port);
                 } else {
                     ctx.var_map.remove(var);
                 }
                current_else_port = arm_then_port; // This arm becomes the current result
            }
            HirPattern::Wildcard => {
                // Wildcard matches anything, becomes the 'then' branch
                current_else_port = lower_expr(ctx, arm_expr)?;
            }
        }
    }
    // The final result is the port determined by the nested structure
    Ok(current_else_port)
}


// Rest of the lowering functions will be moved here... 