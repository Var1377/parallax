use super::builder::NetBuilder;
use super::error::LoweringError;
use crate::port::Port;
use crate::node::{Async, Constructor, Duplicator, Eraser, NodeType, Number, Static, Switch, Wire, Pointer};
use crate::encoding::*; // Import the new encoding module
use parallax_hir::Symbol;
// use parallax_mir::mir::LiteralValue; // Commented out again
// use parallax_mir::mir::Symbol; // Keep commented out, rely on mod.rs import (still might be private)
use parallax_mir::mir::{MirNode, NodeId, PortIndex, MirType};
use std::sync::atomic::{AtomicU64, Ordering};
use std::collections::HashMap;

/// Holds the mapping from MIR port indices to specific Net ports
/// for a single lowered MIR node.
#[derive(Debug, Default, Clone)]
pub(super) struct LoweredPorts {
    /// `inputs[i]` stores the net `Port` corresponding to MIR input `PortIndex(i)`.
    /// Uses Option because not all MIR nodes conceptually have inputs mapped this way (e.g., Constant).
    /// The vector size should match the expected number of MIR inputs.
    inputs: Vec<Option<Port>>,
    /// `outputs[i]` stores the net `Port` corresponding to MIR output `PortIndex(i)`.
    /// Uses Option because not all MIR nodes produce distinct outputs for every index (e.g., Unreachable).
    /// The vector size should match the expected number of MIR outputs.
    outputs: Vec<Option<Port>>,
}

impl LoweredPorts {
    /// Helper to create LoweredPorts with pre-sized vectors.
    fn with_capacity(input_cap: usize, output_cap: usize) -> Self {
        LoweredPorts {
            inputs: Vec::with_capacity(input_cap),
            outputs: Vec::with_capacity(output_cap),
        }
    }

    /// Helper to get an input port, returning an error if out of bounds or None.
    fn get_input(&self, index: u32) -> Result<Port, LoweringError> {
        self.inputs
            .get(index as usize)
            .and_then(|opt_port| *opt_port)
            .ok_or_else(|| LoweringError::Internal(format!("Input port index {} not found or not mapped", index)))
    }

    /// Helper to get an output port, returning an error if out of bounds or None.
    fn get_output(&self, index: u32) -> Result<Port, LoweringError> {
        self.outputs
            .get(index as usize)
            .and_then(|opt_port| *opt_port)
            .ok_or_else(|| LoweringError::Internal(format!("Output port index {} not found or not mapped", index)))
    }
}

/// Lowers a single MIR node into its interaction net representation.
///
/// Converts the MIR node into the appropriate interaction net node(s) based on the node type.
/// Maps the node's output ports to the corresponding interaction net ports.
/// Also maps an "internal" port for nodes that are conceptually roots of subgraphs (like Call, Op, If) used for edge connection.
///
/// # Arguments
/// * `builder` - The net builder that tracks mappings and allocates nodes
/// * `node_id` - The ID of the MIR node to lower
/// * `mir_node` - The MIR node definition
///
/// # Returns
/// `Ok(())` if the node was successfully lowered, or an error if the node type is unsupported.
pub fn lower_mir_node(
    builder: &mut NetBuilder,
    node_id: NodeId,
    mir_node: &MirNode,
) -> Result<LoweredPorts, LoweringError> {
    match mir_node {
        MirNode::Parameter { index, ty } => {
            // Parameters are handled entirely by the RootCON connections based on usage.
            // No net node needs to be allocated here. We just need to provide an entry
            // in the port map, which will be potentially overwritten by the DUP/ERA
            // created in lower_function.
            builder.map_output_port(node_id, PortIndex(0), Port::NULL); // Placeholder mapping
            Ok(LoweredPorts::with_capacity(0, 1))
        }

        MirNode::Constant { value, ty } => {
            // Lower constants to Number nodes.
            let value_u128 = crate::encoding::encode_literal_to_u128(value)?;

            let (idx, principal_port) = builder.alloc_number(Number {
                principle: Port::NULL,
                data: value_u128, // Assign the u128 value
            });
            builder.config.numbers[idx].principle = principal_port;
            builder.map_output_port(node_id, PortIndex(0), principal_port);
            // No MIR inputs for Constant.
            Ok(LoweredPorts::with_capacity(0, 1))
        }
        
        MirNode::StaticAddr { symbol, ty } => {
            // Static addresses (global functions, constants, ops) are represented by Static nodes.
            // The Static's data holds the symbol identifier or operation encoding.
            
            // Check if this symbol corresponds to a known intrinsic
            let encoded_data = if let Some(encoded_intrinsic) = builder.intrinsic_op_map.get(symbol) {
                 // It's an intrinsic, use the pre-encoded value from the map
                 println!("Lowering intrinsic: {:?} -> Encoded {}", symbol, encoded_intrinsic);
                 *encoded_intrinsic // Use the u64 value directly
            } else {
                 // Not an intrinsic, assume it's a regular function pointer
                 // TODO: Handle non-function pointers (e.g., global statics) based on ty.
                 // Runtime needs support for TAG_GLOBAL_VAR etc.
                 println!("Lowering function pointer: {:?}", symbol);
                 encode_static_function(*symbol) // Use encoding function from encoding module
            };

            let (idx, principal_port) = builder.alloc_static(Static {
                principle: Port::NULL,
                data: AtomicU64::new(encoded_data),
            });
            builder.config.statics[idx].principle = principal_port;
            builder.map_output_port(node_id, PortIndex(0), principal_port);
            // No MIR inputs for StaticAddr.
            Ok(LoweredPorts::with_capacity(0, 1))
        }
        
        MirNode::Constructor { tag, field_types, ty } => {
            // Lower to a flat tree: Cons(f0, Cons(f1, ... Cons(fN-1, TAG)))
            // TAG is Static(Tag) or Static(NIL) for tuples.
            // MIR Input i maps to Left Aux of Cons_i.
            // MIR Output 0 maps to Principal of Cons_0.
            let arity = field_types.len();
            let partition_id = builder.partition_id; // Cache partition id

            // Allocate the Static TAG node (or NIL for tuples)
            let tag_port = {
                 let encoded_data = if *tag != Symbol::new(0) {
                     // Assume non-zero tag is a variant/function - use encode_static_variant or function?
                     // For now, assume TAG_IS_VARIANT for non-zero constructor tags.
                     encode_static_variant(*tag)
                 } else {
                     encode_static_nil() // Use TAG_NIL for tuples (tag 0)
                 };
                 let (idx, port) = builder.alloc_static(Static {
                     principle: Port::NULL,
                     data: AtomicU64::new(encoded_data),
                 });
                 builder.config.statics[idx].principle = port;
                 port
            };

            // Recursive helper to build the flat tree structure
            // Returns the principal port of the constructor created at this level.
            fn build_flat_cons_tree(
                builder: &mut NetBuilder,
                node_id: NodeId,
                field_idx: usize,
                arity: usize,
                tag_port: Port, // The pre-allocated TAG/NIL node
                partition_id: u16, // Expect u16 here
            ) -> Result<Port, LoweringError> {
                if field_idx == arity {
                    // Base case: Reached the end, return the pre-allocated TAG/NIL port
                    Ok(tag_port)
                } else {
                    // Recursive step: Create Cons(field_i, next)
                    let next_port = build_flat_cons_tree(builder, node_id, field_idx + 1, arity, tag_port, partition_id)?;

                    // Allocate the constructor for this field
                    let (cons_idx, cons_principal_port) = builder.alloc_constructor(Constructor {
                        principle: Port::NULL, // Will be set below
                        left: Port::NULL,      // Placeholder for field input (connected via edge lowering)
                        right: next_port,      // Connect to the rest of the tree
                    });
                    builder.config.constructors[cons_idx].principle = cons_principal_port;

                    // Map the MIR input port for this field to the LEFT AUXILIARY port of this constructor.
                    // Also add wire for the connection to the next element (right port).
                    builder.config.initial_wires.push(Wire(
                        Port::right(NodeType::Constructor, partition_id, cons_idx as u64),
                        next_port
                    ));

                    let field_input_port = Port::left(NodeType::Constructor, partition_id, cons_idx as u64); // Use u16 partition_id
                    builder.map_input_port(node_id, PortIndex(field_idx as u32), field_input_port);

                    // Return the PRINCIPAL port of the constructor created at this level
                    Ok(cons_principal_port)
                }
            }

            // Build the tree starting from field 0
            let root_cons_port = build_flat_cons_tree(builder, node_id, 0, arity, tag_port, partition_id as u16)?;

            // Map the MIR Constructor's output port to the PRINCIPAL port of the *root* constructor (Cons_0)
            builder.map_output_port(node_id, PortIndex(0), root_cons_port);
            // Constructor node conceptually takes inputs PortIndex(0..N-1)
            Ok(LoweredPorts::with_capacity(arity, 1))
        }
        
        MirNode::ArrayConstruct { element_ty, size } => {
             Err(LoweringError::NotImplemented("ArrayConstruct"))
        }
        
        MirNode::ArrayProject { .. } => {
            Err(LoweringError::NotImplemented("ArrayProject"))
        }
        
        MirNode::Project { field_index, aggregate_ty, field_ty } => {
            // Lower Project to a DUP/ERA gadget.
            let gadget = build_projection_gadget(builder, *field_index)?;
            builder.map_output_port(node_id, PortIndex(0), gadget.output); // Map MIR output to gadget output
            // Project has one input (PortIndex 0), map it to the gadget's input port.
            builder.map_input_port(node_id, PortIndex(0), gadget.principal_in);
            Ok(LoweredPorts::with_capacity(1, 1))
        }
        
        MirNode::Closure { original_lambda_symbol, specialized_function_symbol, capture_types, env_ty, func_ptr_ty } => {
            // Lowered to: Constructor (captures) + Static (function ptr)
            let arity = capture_types.len();

            // Allocate the Static NIL node for the capture list terminator
            let nil_port = {
                let (idx, port) = builder.alloc_static(Static {
                    principle: Port::NULL,
                    data: AtomicU64::new(encode_static_nil()), // Use encoding function
                });
                builder.config.statics[idx].principle = port;
                port
            };
            
            // Recursive helper to build the nested structure for captures
            fn build_capture_list(builder: &mut NetBuilder, closure_node_id: NodeId, capture_idx: usize, arity: usize, nil_port: Port) -> Result<Port, LoweringError> {
                if capture_idx == arity {
                    Ok(nil_port) // Base case: return NIL
                } else {
                    let tail_port = build_capture_list(builder, closure_node_id, capture_idx + 1, arity, nil_port)?;
                    let (cons_idx, cons_port) = builder.alloc_constructor(Constructor {
                        principle: Port::NULL,
                        left: Port::NULL, // Placeholder for capture input
                        right: tail_port,
                    });
                    builder.config.constructors[cons_idx].principle = cons_port;
                    
                    // Add wire for the connection to the tail (right port).
                    builder.config.initial_wires.push(Wire(
                        Port::right(NodeType::Constructor, builder.partition_id, cons_idx as u64),
                        tail_port
                    ));

                    // Map MIR input port for this capture -> Left Aux of this Cons node
                    let capture_input_port = Port::left(NodeType::Constructor, builder.partition_id, cons_idx as u64);
                    builder.map_input_port(closure_node_id, PortIndex(capture_idx as u32), capture_input_port);
                    
                    Ok(cons_port)
                }
            }

            // Build the capture list (environment)
            let capture_list_head_port = build_capture_list(builder, node_id, 0, arity, nil_port)?;
             
             // Function Pointer part (Static node)
             let (static_idx, static_port) = builder.alloc_static(Static {
                 principle: Port::NULL,
                 data: AtomicU64::new(encode_static_function(*specialized_function_symbol)), // Use encoding function
             });
             builder.config.statics[static_idx].principle = static_port;
             
             // Map outputs
             builder.map_output_port(node_id, PortIndex(0), capture_list_head_port); // Env output (Port 0)
             builder.map_output_port(node_id, PortIndex(1), static_port);            // Func ptr output (Port 1)
             
             // Closure takes inputs PortIndex(0..N-1) for captures.
             Ok(LoweredPorts::with_capacity(arity, 2))
        }
        
        MirNode::FunctionCall { func_ty } => {
            // Lowered to nested AppCONs: App(App(...App(FuncRef, Arg0)..., ArgN-2), ArgN-1)
            // Input 0 -> FuncRef -> Right Aux of App0
            // Input i+1 -> Arg_i -> Left Aux of App_i
            // Output 0 <- Principal of AppN-1
            // Connection: App_i.Principle -> Right Aux of App_{i+1}

            let arity = match func_ty {
                MirType::FunctionPointer(params, _) => params.len(),
                _ => return Err(LoweringError::InvalidMir(format!(
                    "FunctionCall node {:?} has non-function type: {:?}", node_id, func_ty
                ))),
            };

            if arity == 0 {
                // Arity 0: App(FuncRef, NIL?)
                // Create a single AppCON.
                // Input 0 (FuncRef) connects to its Right Aux.
                // Output 0 comes from its Principal.
                // We need a NIL node for the argument list.
                let nil_port = {
                     let (idx, port) = builder.alloc_static(Static {
                         principle: Port::NULL,
                         data: AtomicU64::new(encode_static_nil()), // Use encoding function
                     });
                     builder.config.statics[idx].principle = port;
                     port
                };

                let (app_idx, app_port) = builder.alloc_constructor(Constructor {
                     principle: Port::NULL, 
                     left: nil_port, // Left connects to NIL arg
                     right: Port::NULL // Right connects to FuncRef (via edge)
                });
                builder.config.constructors[app_idx].principle = app_port;
                
                builder.map_output_port(node_id, PortIndex(0), app_port);
                // Map internal port for function connection (Input 0)
                let internal_node_id_base = node_id.0;
                builder.map_input_port(node_id, PortIndex(0), app_port);
                Ok(LoweredPorts::with_capacity(1, 1))
            } else {
                // Arity N > 0: Create N AppCON nodes
                let mut app_cons_data = Vec::with_capacity(arity);

                // 1. Allocate all AppCON nodes first
                for _ in 0..arity {
                    let (app_idx, app_port) = builder.alloc_constructor(Constructor::new_null());
                    builder.config.constructors[app_idx].principle = app_port;
                    app_cons_data.push((app_idx, app_port));
                }

                // 2. Connect the AppCONs internally: App_i.P -> App_{i+1}.R
                for i in 0..(arity - 1) {
                     let (idx_i, port_i) = app_cons_data[i]; // App_i's principal port
                     let (idx_i_plus_1, _port_i_plus_1) = app_cons_data[i+1];
                     // Set App_{i+1}'s right port to App_i's principal
                     builder.config.constructors[idx_i_plus_1].right = port_i;
                     // Add wire for this internal connection
                     builder.config.initial_wires.push(Wire(
                         port_i, // App_i.P is source
                         Port::right(NodeType::Constructor, builder.partition_id, idx_i_plus_1 as u64) // App_{i+1}.R is dest
                     ));
                }
                
                // 3. Map MIR Inputs to correct AppCON ports
                // - MIR Input 0 (FuncRef) -> Right Aux of App_0
                let func_input_target_port = Port::right(NodeType::Constructor, builder.partition_id, app_cons_data[0].0 as u64);
                builder.map_input_port(node_id, PortIndex(0), func_input_target_port);
                
                // - MIR Input i+1 (Arg_i) -> Left Aux of App_i
                for i in 0..arity {
                    let (app_idx, _app_port) = app_cons_data[i];
                    let arg_input_target_port = Port::left(NodeType::Constructor, builder.partition_id, app_idx as u64);
                    builder.map_input_port(node_id, PortIndex((i + 1) as u32), arg_input_target_port);
                }

                // 4. Map MIR Output 0 -> Principal Port of App_{N-1} (outermost AppCON)
                builder.map_output_port(node_id, PortIndex(0), app_cons_data[arity - 1].1);

                Ok(LoweredPorts::with_capacity(arity + 1, 1)) // Arity args + 1 function input
            }
        }
        
        MirNode::IfValue { condition_ty, ty } => {
            // Lowered to Switch: Switch(Cond, TrueVal, FalseVal)
            let (idx, principal_port) = builder.alloc_switch(Switch::new_null());
            builder.config.switches[idx].principle = principal_port;
            builder.map_output_port(node_id, PortIndex(0), principal_port); // Result comes from Switch interaction
            
            // Map inputs:
            // Input 0 (Cond) -> Principal Port
            // Input 1 (True Val) -> Left Aux
            // Input 2 (False Val) -> Right Aux
            builder.map_input_port(node_id, PortIndex(0), principal_port);
            builder.map_input_port(node_id, PortIndex(1), Port::left(NodeType::Switch, builder.partition_id, idx as u64));
            builder.map_input_port(node_id, PortIndex(2), Port::right(NodeType::Switch, builder.partition_id, idx as u64));

            Ok(LoweredPorts::with_capacity(3, 1))
        }
        
        MirNode::Unreachable => {
            // No inputs or outputs to map for Unreachable.
            let (idx, principal_port) = builder.alloc_eraser(Eraser::new_null());
            builder.config.erasers[idx].principle = principal_port;
            // Create a wire linking the eraser to itself? Or handle in runtime?
            // For now, just create the node.
            Ok(LoweredPorts::with_capacity(0, 0))
        }
        // MirNode::MatchDispatch { enum_symbol, arms, otherwise } => {
        //     // Lower MatchDispatch MIR node to the corresponding net node.
        //     // The net node itself doesn't store the full map; the runtime uses its identity
        //     // or encoded data to find the dispatch logic.

        //     // Encode the function ID containing this match dispatch for runtime identification.
        //     let function_id = builder.current_function_id;
        //     let encoded_data = encode_static_data(TAG_MATCH_DISPATCH, function_id as u64);

        //     // Allocate the MatchDispatch node
        //     let (idx, principal_port) = builder.alloc_match_dispatch(MatchDispatch {
        //         principle: Port::NULL, // Input (scrutinee) connects here
        //         output: Port::NULL,    // Output (result) comes from here
        //         data: AtomicU64::new(encoded_data), // Store encoded function_id
        //     });

        //     // Get the concrete output port for the MatchDispatch node
        //     let output_port = Port::new(
        //         crate::port::PortType::Right, // Convention: Use Right Aux for output?
        //         NodeType::MatchDispatch as u8, // Cast to u8
        //         builder.partition_id,
        //         idx as u64
        //     );

        //     // Update the allocated node with its ports
        //     builder.config.match_dispatches[idx].principle = principal_port;
        //     builder.config.match_dispatches[idx].output = output_port;

        //     // Map MIR input port 0 (scrutinee) -> Net node principal port
        //     // Edge lowering needs to know how to connect to this principal port.
        //     // We map the MIR node's internal ID to the principal port.
        //     builder.map_input_port(node_id, PortIndex(0), principal_port);

        //     // Map MIR output port 0 (result) -> Net node output port
        //     builder.map_output_port(node_id, PortIndex(0), output_port);

        //     // --- BEGIN: Store arm data --- 
        //     let mut net_arms = HashMap::with_capacity(arms.len());
        //     for (variant_symbol, (arm_node_id, arm_port_index)) in arms {
        //         let arm_net_port = builder.get_output_net_port(*arm_node_id, *arm_port_index)?;
        //         net_arms.insert(variant_symbol.id() as u64, arm_net_port);
        //     }

        //     let otherwise_net_port = match otherwise {
        //         Some((other_node_id, other_port_index)) => {
        //             Some(builder.get_output_net_port(*other_node_id, *other_port_index)?)
        //         }
        //         None => None,
        //     };
            
        //     // Ensure the builder's vec is large enough
        //     if idx >= builder.match_arm_data.len() {
        //          builder.match_arm_data.resize_with(idx + 1, || None);
        //     }
        //     builder.match_arm_data[idx] = Some((net_arms, otherwise_net_port));
        //     // --- END: Store arm data ---

        //     // IMPORTANT: The connection of arms/otherwise results is handled by the runtime rule,
        //     // not by creating wires here during lowering. The runtime connects the caller
        //     // (connected to `output_port`) to the appropriate arm's root port.
        //     Ok(())
        // }

        // Add a catch-all for remaining node types
        _ => {
             // This case should ideally not be hit if all variants in mir.rs are handled
             println!("Warning: Unhandled MIR node encountered during lowering: {:?}", mir_node);
              Err(LoweringError::UnsupportedNode(mir_node.clone()))
         }
    }
} 

// --- Gadget Building Logic (Remains here as it's specific to Project node lowering) ---

/// Represents the input and output ports of a generated gadget.
struct GadgetPorts {
    principal_in: Port, // Port on the gadget that interacts with the Aggregate
    output: Port,       // Port where the final projected field emerges
}

/// Recursively builds the projection gadget P_k.
/// P_0 = { DUP_0, ERA_0 } where DUP_0.left=output, DUP_0.right=ERA_0.p
/// P_k = { DUP_k, ERA_k, P_{k-1} } where DUP_k.left=ERA_k.p, DUP_k.right=P_{k-1}.principal_in, output=P_{k-1}.output
fn build_projection_gadget(builder: &mut NetBuilder, index: u32) -> Result<GadgetPorts, LoweringError> {
    let partition_id = builder.partition_id; // Cache partition_id
    if index == 0 {
        // Base Case: Proj(0) = { DUP_0, ERA_0 }
        let (era0_idx, era0_p) = builder.alloc_eraser(Eraser::new_null());
        let dup0_right_port = era0_p;
        let (dup0_idx, dup0_p) = builder.alloc_duplicator(Duplicator {
            principle: Port::NULL, // Input port of the gadget
            left: Port::NULL,      // Will be the output port
            right: dup0_right_port, // Connect right aux to eraser's principal port
        });
        // Define the port on the Duplicator that connects to the Eraser
        let dup0_connecting_port = Port::right(NodeType::Duplicator, partition_id as u16, dup0_idx as u64);
        // Wire eraser principal back to dup right aux port value
        builder.config.erasers[era0_idx].principle = dup0_connecting_port; // Eraser connects to DUP's right
        // Set principal port on the duplicator itself
        builder.config.duplicators[dup0_idx].principle = dup0_p;
        // Define the output port (left aux of the duplicator)
        let output_port = Port::left(NodeType::Duplicator, partition_id as u16, dup0_idx as u64); // Ensure partition_id is u16

        // *** Add internal wire ***
        builder.config.initial_wires.push(Wire(dup0_connecting_port, era0_p));

        Ok(GadgetPorts { principal_in: dup0_p, output: output_port })
    } else {
        // Recursive Step: Proj(k) = { DUP_k, ERA_k, P_{k-1} }
        let (erak_idx, erak_p) = builder.alloc_eraser(Eraser::new_null());
        let dupk_left_port = erak_p; // DUP_k's left connects to ERA_k's principal
        // Build P_{k-1} recursively
        let sub_gadget = build_projection_gadget(builder, index - 1)?;
        let (dupk_idx, dupk_p) = builder.alloc_duplicator(Duplicator {
            principle: Port::NULL,            // Input port of this gadget level
            left: dupk_left_port,            // Connect left aux to the eraser's principal port
            right: sub_gadget.principal_in,   // Connect right aux to sub-gadget's input port
        });
        // Define the port on DUP_k that connects to ERA_k
        let dupk_connecting_port = Port::left(NodeType::Duplicator, partition_id as u16, dupk_idx as u64);
        // Wire eraser principal back to dup left aux port value
        builder.config.erasers[erak_idx].principle = dupk_connecting_port; // Eraser connects to DUP's left
        // Set principal port on the duplicator itself
        builder.config.duplicators[dupk_idx].principle = dupk_p;

        // *** Add internal wire ***
        builder.config.initial_wires.push(Wire(dupk_connecting_port, erak_p));

        // The final output port comes from the base case (P_0) of the sub-gadget
        Ok(GadgetPorts { principal_in: dupk_p, output: sub_gadget.output })
    }
} 