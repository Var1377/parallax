use super::builder::NetBuilder;
use super::error::LoweringError;
use crate::port::Port;
use crate::node::{Async, Constructor, Duplicator, Eraser, NodeType, Number, Static, Switch, Redex, Pointer};
use parallax_hir::Symbol;
// use parallax_mir::mir::LiteralValue; // Commented out again
// use parallax_mir::mir::Symbol; // Keep commented out, rely on mod.rs import (still might be private)
use parallax_mir::mir::{MirNode, NodeId, PortIndex, MirType};
use std::sync::atomic::{AtomicU64, Ordering};
use std::collections::HashMap;

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
) -> Result<(), LoweringError> {
    match mir_node {
        MirNode::Parameter { index, ty } => {
            // Parameters are represented by a Duplicator node at the function boundary.
            // Their "output" port is created and mapped here, but the actual
            // Duplicator node might be implicitly handled by the runtime setup
            // or connected via the RootCON. Let's create a placeholder Duplicator
            // for mapping purposes, assuming its principal port represents the param value.
            let (idx, principal_port) = builder.alloc_duplicator(Duplicator::new_null());
            builder.config.duplicators[idx].principle = principal_port;
            builder.map_output_port(node_id, PortIndex(0), principal_port);
            builder.map_internal_node_port(node_id, principal_port); // Map internal for consistency
            Ok(())
        }

        MirNode::Constant { value, ty } => {
            // Lower constants to Number nodes.
            use parallax_hir::hir::HirLiteral;
            let value_u128: u128 = match value {
                // Use the struct variants
                HirLiteral::IntLiteral { value: i, .. } => *i as u128, // Cast integer value
                HirLiteral::FloatLiteral { value: f, .. } => f.to_bits() as u128, // Store f64 bits
                HirLiteral::BoolLiteral(b) => *b as u128, // 0 or 1
                HirLiteral::CharLiteral(c) => *c as u128, // Store character code
                HirLiteral::Unit => 0, // Represent Unit as 0
                HirLiteral::StringLiteral(_) => {
                    return Err(LoweringError::NotImplemented("String Constant Lowering"))
                }
            };

            let (idx, principal_port) = builder.alloc_number(Number {
                principle: Port::NULL,
                data: value_u128, // Assign the u128 value
            });
            builder.config.numbers[idx].principle = principal_port;
            builder.map_output_port(node_id, PortIndex(0), principal_port);
            builder.map_internal_node_port(node_id, principal_port);
            Ok(())
        }
        
        MirNode::StaticAddr { symbol, ty } => {
            // Static addresses (global functions, constants, ops) are represented by Static nodes.
            // The Static's data holds the symbol identifier or operation encoding.
            
            // Check if this symbol corresponds to a known intrinsic
            let encoded_data = if let Some(op_code) = builder.intrinsic_op_map.get(symbol) {
                 // It's an intrinsic, encode with intrinsic tag and op code
                 println!("Lowering intrinsic: {:?} -> OpCode {}", symbol, op_code);
                 encode_static_data(TAG_INTRINSIC_OP, *op_code)
            } else {
                 // Not an intrinsic, assume it's a regular function pointer
                 // TODO: Handle non-function pointers (e.g., global statics) based on ty.
                 // Runtime needs support for TAG_GLOBAL_VAR etc.
                 println!("Lowering function pointer: {:?}", symbol);
                 encode_static_symbol(TAG_FUNCTION, *symbol)
            };

            let (idx, principal_port) = builder.alloc_static(Static {
                principle: Port::NULL,
                data: AtomicU64::new(encoded_data),
            });
            builder.config.statics[idx].principle = principal_port;
            builder.map_output_port(node_id, PortIndex(0), principal_port);
            builder.map_internal_node_port(node_id, principal_port); // Map internal for consistency
            Ok(())
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
                let (tag_val, tag_node_tag) = if *tag != Symbol::new(0) {
                    (tag.id() as u64, TAG_FUNCTION) // Use `tag` instead of `symbol`
                } else {
                    (0, TAG_NIL) // Use TAG_NIL for tuples
                };
                let encoded_data = encode_static_data(tag_node_tag, tag_val);
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
                    // Edge lowering will use this mapping to connect the field's value.
                    let field_input_port = Port::left(NodeType::Constructor, partition_id, cons_idx as u64); // Use u16 partition_id
                    // Use map_internal_node_port with a derived ID for the input connection point
                    let input_target_node_id = NodeId(node_id.0 * 1000 + field_idx as u32);
                    builder.map_internal_node_port(input_target_node_id, field_input_port);

                    // Return the PRINCIPAL port of the constructor created at this level
                    Ok(cons_principal_port)
                }
            }

            // Build the tree starting from field 0
            let root_cons_port = build_flat_cons_tree(builder, node_id, 0, arity, tag_port, partition_id as u16)?;

            // Map the MIR Constructor's output port to the PRINCIPAL port of the *root* constructor (Cons_0)
            builder.map_output_port(node_id, PortIndex(0), root_cons_port);
            // Map the MIR Constructor's internal node ID also to the root principal port.
            // This is used for edges *originating from* the constructor result.
            builder.map_internal_node_port(node_id, root_cons_port);
            Ok(())
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
            builder.map_internal_node_port(node_id, gadget.principal_in); // Map MIR internal (for edge connection) to gadget input
            Ok(())
        }
        
        MirNode::Closure { original_lambda_symbol, specialized_function_symbol, capture_types, env_ty, func_ptr_ty } => {
            // Lowered to: Constructor (captures) + Static (function ptr)
            let arity = capture_types.len();

            // Allocate the Static NIL node for the capture list terminator
            let nil_port = {
                let (idx, port) = builder.alloc_static(Static {
                    principle: Port::NULL,
                    data: AtomicU64::new(encode_static_data(TAG_NIL, 0)),
                });
                builder.config.statics[idx].principle = port;
                port
            };
            
            // Recursive helper to build the nested structure for captures
            // This is almost identical to the one in MirNode::Constructor
            fn build_capture_list(builder: &mut NetBuilder, node_id: NodeId, capture_idx: usize, arity: usize, nil_port: Port) -> Result<Port, LoweringError> {
                if capture_idx == arity {
                    Ok(nil_port) // Base case: return NIL
                } else {
                    let tail_port = build_capture_list(builder, node_id, capture_idx + 1, arity, nil_port)?;
                    let (cons_idx, cons_port) = builder.alloc_constructor(Constructor {
                        principle: Port::NULL,
                        left: Port::NULL, // Placeholder for capture input
                        right: tail_port,
                    });
                    builder.config.constructors[cons_idx].principle = cons_port;
                    
                    // Map internal node ID for this constructor step (used by edge lowering)
                    // Use a slightly different scheme to avoid clashes with other nodes? Base * 1000 + 100 + index?
                    let internal_node_sub_id = node_id.0 * 1000 + 100 + capture_idx as u32;
                    builder.map_internal_node_port(NodeId(internal_node_sub_id), cons_port);
                    
                    Ok(cons_port)
                }
            }

            // Build the capture list (environment)
            let capture_list_head_port = build_capture_list(builder, node_id, 0, arity, nil_port)?;
             
             // Function Pointer part (Static node)
             let (static_idx, static_port) = builder.alloc_static(Static {
                 principle: Port::NULL,
                 data: AtomicU64::new(encode_static_symbol(TAG_FUNCTION, *specialized_function_symbol)), // Store encoded FunctionID
             });
             builder.config.statics[static_idx].principle = static_port;
             
             // Map outputs
             builder.map_output_port(node_id, PortIndex(0), capture_list_head_port); // Env output (Port 0)
             builder.map_output_port(node_id, PortIndex(1), static_port);            // Func ptr output (Port 1)
             
             // Map the internal port for the *original* Closure MIR node to the capture list head.
             // Edge lowering logic for captures will use the sub-IDs mapped in build_capture_list.
             builder.map_internal_node_port(node_id, capture_list_head_port);
             
            Ok(())
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
                         data: AtomicU64::new(encode_static_data(TAG_NIL, 0)),
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
                builder.map_internal_node_port(NodeId(internal_node_id_base * 1000 + 0), app_port);
            } else {
                // Arity N > 0: Create N AppCON nodes
                let mut app_cons_data = Vec::with_capacity(arity);
                let internal_node_id_base = node_id.0;

                for i in 0..arity {
                    let (app_idx, app_port) = builder.alloc_constructor(Constructor::new_null());
                    builder.config.constructors[app_idx].principle = app_port;
                    app_cons_data.push((app_idx, app_port));

                    // Map internal port for this AppCON node (used by edge lowering for args/func).
                    // NodeId(base * 1000 + i) maps to the principal port of App_i.
                    let internal_node_id = NodeId(internal_node_id_base * 1000 + i as u32);
                    builder.map_internal_node_port(internal_node_id, app_port);
                }

                // Connect the AppCONs: App_i.principle -> App_{i+1}.right
                for i in 0..(arity - 1) {
                     let (idx_i, port_i) = app_cons_data[i];
                     let (idx_i_plus_1, _port_i_plus_1) = app_cons_data[i+1];
                     // Get mutable ref to App_{i+1} and set its right port
                     if let Some(app_i_plus_1_node) = builder.config.constructors.get_mut(idx_i_plus_1) {
                          app_i_plus_1_node.right = port_i; // Connect App_i's principal
                     } else {
                          return Err(LoweringError::Internal(format!("AppCON node {} disappeared during linking", idx_i_plus_1)));
                     }
                }
                
                // If arity > 0, the right port of App0 is left NULL (to be connected to FuncRef via edge)
                // If arity > 0, the left ports of all AppCONs are left NULL (to be connected to Args via edges)

                // Map the MIR node's output port 0 to the principal port of the *outermost* AppCON (AppN-1).
                builder.map_output_port(node_id, PortIndex(0), app_cons_data[arity - 1].1);
            }
            Ok(())
        }
        
        MirNode::IfValue { condition_ty, ty } => {
            // Lowered to Switch: Switch(Cond, TrueVal, FalseVal)
            let (idx, principal_port) = builder.alloc_switch(Switch::new_null());
            builder.config.switches[idx].principle = principal_port;
            builder.map_output_port(node_id, PortIndex(0), principal_port); // Result comes from Switch interaction
             builder.map_internal_node_port(node_id, principal_port); // Map internal for Cond connection
             // Edges connect Cond input (port 0) to principal, TrueVal input (port 1) to left aux, FalseVal input (port 2) to right aux.
            Ok(())
        }
        
        MirNode::Unreachable => {
            // Lower to an Eraser node loop?
            let (idx, principal_port) = builder.alloc_eraser(Eraser::new_null());
            builder.config.erasers[idx].principle = principal_port;
            // Create a redex linking the eraser to itself? Or handle in runtime?
            // For now, just create the node.
            // No output port to map. Map internal? 
             builder.map_internal_node_port(node_id, principal_port);
            Ok(())
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
        //     builder.map_internal_node_port(node_id, principal_port);

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
        //     // not by creating edges here during lowering. The runtime connects the caller
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

// --- Static Node Data Encoding ---
const TAG_SHIFT: u32 = 60;
const DATA_MASK: u64 = (1u64 << TAG_SHIFT) - 1;

const TAG_NIL: u64 = 0;         // Represents the end of a list or Unit/Tuple tag
const TAG_FUNCTION: u64 = 1;    // Data = Function Symbol ID
const TAG_GLOBAL_VAR: u64 = 2;  // Data = Global Static Symbol ID (Unused for now)
const TAG_INTRINSIC_OP: u64 = 6; // Data = Encoded Intrinsic Op Code
const TAG_IS_VARIANT: u64 = 8;   // Data = Variant Symbol ID (Used by MIR lowering for match)

// --- Intrinsic Operation Codes (fit within DATA_MASK) ---
// Grouped by type. Ensure these values don't clash and fit in 60 bits.
// Using ranges: 0-127 (i64), 128-255 (u64), 256-383 (i32), ..., 1024+ (misc)
pub const OP_TYPE_SHIFT: u32 = 8; // Shift amount to get type category
pub const OP_CODE_MASK: u64 = (1 << OP_TYPE_SHIFT) - 1;

// i64: 0-127
pub const OP_I64_ADD: u64 = 0;
pub const OP_I64_SUB: u64 = 1;
pub const OP_I64_MUL: u64 = 2;
pub const OP_I64_DIV: u64 = 3;
pub const OP_I64_REM: u64 = 4;
pub const OP_I64_AND: u64 = 5;
pub const OP_I64_OR:  u64 = 6;
pub const OP_I64_XOR: u64 = 7;
pub const OP_I64_SHL: u64 = 8;
pub const OP_I64_SHR: u64 = 9; // Arithmetic right shift
pub const OP_I64_NEG: u64 = 10;
pub const OP_I64_NOT: u64 = 11; // Bitwise NOT
pub const OP_I64_ABS: u64 = 12;
pub const OP_I64_POW: u64 = 13;
pub const OP_I64_EQ:  u64 = 14;
pub const OP_I64_NE:  u64 = 15;
pub const OP_I64_LT:  u64 = 16;
pub const OP_I64_LE:  u64 = 17;
pub const OP_I64_GT:  u64 = 18;
pub const OP_I64_GE:  u64 = 19;

// u64: 128-255
pub const OP_U64_ADD: u64 = 128 + 0;
pub const OP_U64_SUB: u64 = 128 + 1;
pub const OP_U64_MUL: u64 = 128 + 2;
pub const OP_U64_DIV: u64 = 128 + 3;
pub const OP_U64_REM: u64 = 128 + 4;
pub const OP_U64_AND: u64 = 128 + 5;
pub const OP_U64_OR:  u64 = 128 + 6;
pub const OP_U64_XOR: u64 = 128 + 7;
pub const OP_U64_SHL: u64 = 128 + 8;
pub const OP_U64_SHR: u64 = 128 + 9; // Logical right shift
pub const OP_U64_NOT: u64 = 128 + 11;
// Abs/Neg not applicable
pub const OP_U64_POW: u64 = 128 + 13;
pub const OP_U64_EQ:  u64 = 128 + 14;
pub const OP_U64_NE:  u64 = 128 + 15;
pub const OP_U64_LT:  u64 = 128 + 16;
pub const OP_U64_LE:  u64 = 128 + 17;
pub const OP_U64_GT:  u64 = 128 + 18;
pub const OP_U64_GE:  u64 = 128 + 19;

// f64: 256-383
pub const OP_F64_ADD: u64 = 256 + 0;
pub const OP_F64_SUB: u64 = 256 + 1;
pub const OP_F64_MUL: u64 = 256 + 2;
pub const OP_F64_DIV: u64 = 256 + 3;
pub const OP_F64_REM: u64 = 256 + 4;
pub const OP_F64_NEG: u64 = 256 + 10;
pub const OP_F64_ABS: u64 = 256 + 12;
pub const OP_F64_POW: u64 = 256 + 13;
pub const OP_F64_EQ:  u64 = 256 + 14;
pub const OP_F64_NE:  u64 = 256 + 15;
pub const OP_F64_LT:  u64 = 256 + 16;
pub const OP_F64_LE:  u64 = 256 + 17;
pub const OP_F64_GT:  u64 = 256 + 18;
pub const OP_F64_GE:  u64 = 256 + 19;

// i32: 384-511
pub const OP_I32_ADD: u64 = 384 + 0;
pub const OP_I32_SUB: u64 = 384 + 1;
pub const OP_I32_MUL: u64 = 384 + 2;
pub const OP_I32_DIV: u64 = 384 + 3;
pub const OP_I32_REM: u64 = 384 + 4;
pub const OP_I32_AND: u64 = 384 + 5;
pub const OP_I32_OR:  u64 = 384 + 6;
pub const OP_I32_XOR: u64 = 384 + 7;
pub const OP_I32_SHL: u64 = 384 + 8;
pub const OP_I32_SHR: u64 = 384 + 9;
pub const OP_I32_NEG: u64 = 384 + 10;
pub const OP_I32_NOT: u64 = 384 + 11;
pub const OP_I32_ABS: u64 = 384 + 12;
pub const OP_I32_POW: u64 = 384 + 13;
pub const OP_I32_EQ:  u64 = 384 + 14;
pub const OP_I32_NE:  u64 = 384 + 15;
pub const OP_I32_LT:  u64 = 384 + 16;
pub const OP_I32_LE:  u64 = 384 + 17;
pub const OP_I32_GT:  u64 = 384 + 18;
pub const OP_I32_GE:  u64 = 384 + 19;

// u32: 512-639
pub const OP_U32_ADD: u64 = 512 + 0;
pub const OP_U32_SUB: u64 = 512 + 1;
pub const OP_U32_MUL: u64 = 512 + 2;
pub const OP_U32_DIV: u64 = 512 + 3;
pub const OP_U32_REM: u64 = 512 + 4;
pub const OP_U32_AND: u64 = 512 + 5;
pub const OP_U32_OR:  u64 = 512 + 6;
pub const OP_U32_XOR: u64 = 512 + 7;
pub const OP_U32_SHL: u64 = 512 + 8;
pub const OP_U32_SHR: u64 = 512 + 9;
pub const OP_U32_NOT: u64 = 512 + 11;
pub const OP_U32_POW: u64 = 512 + 13;
pub const OP_U32_EQ:  u64 = 512 + 14;
pub const OP_U32_NE:  u64 = 512 + 15;
pub const OP_U32_LT:  u64 = 512 + 16;
pub const OP_U32_LE:  u64 = 512 + 17;
pub const OP_U32_GT:  u64 = 512 + 18;
pub const OP_U32_GE:  u64 = 512 + 19;

// f32: 640-767
pub const OP_F32_ADD: u64 = 640 + 0;
pub const OP_F32_SUB: u64 = 640 + 1;
pub const OP_F32_MUL: u64 = 640 + 2;
pub const OP_F32_DIV: u64 = 640 + 3;
pub const OP_F32_REM: u64 = 640 + 4;
pub const OP_F32_NEG: u64 = 640 + 10;
pub const OP_F32_ABS: u64 = 640 + 12;
pub const OP_F32_POW: u64 = 640 + 13;
pub const OP_F32_EQ:  u64 = 640 + 14;
pub const OP_F32_NE:  u64 = 640 + 15;
pub const OP_F32_LT:  u64 = 640 + 16;
pub const OP_F32_LE:  u64 = 640 + 17;
pub const OP_F32_GT:  u64 = 640 + 18;
pub const OP_F32_GE:  u64 = 640 + 19;

// bool: 768-895
pub const OP_BOOL_AND: u64 = 768 + 5;
pub const OP_BOOL_OR:  u64 = 768 + 6;
pub const OP_BOOL_XOR: u64 = 768 + 7;
pub const OP_BOOL_NOT: u64 = 768 + 11;
pub const OP_BOOL_EQ:  u64 = 768 + 14;
pub const OP_BOOL_NE:  u64 = 768 + 15;

// i16: 896-1023
pub const OP_I16_ADD: u64 = 896 + 0;
pub const OP_I16_SUB: u64 = 896 + 1;
pub const OP_I16_MUL: u64 = 896 + 2;
pub const OP_I16_DIV: u64 = 896 + 3;
pub const OP_I16_REM: u64 = 896 + 4;
pub const OP_I16_AND: u64 = 896 + 5;
pub const OP_I16_OR:  u64 = 896 + 6;
pub const OP_I16_XOR: u64 = 896 + 7;
pub const OP_I16_SHL: u64 = 896 + 8;
pub const OP_I16_SHR: u64 = 896 + 9;
pub const OP_I16_NEG: u64 = 896 + 10;
pub const OP_I16_NOT: u64 = 896 + 11;
pub const OP_I16_ABS: u64 = 896 + 12;
pub const OP_I16_POW: u64 = 896 + 13;
pub const OP_I16_EQ:  u64 = 896 + 14;
pub const OP_I16_NE:  u64 = 896 + 15;
pub const OP_I16_LT:  u64 = 896 + 16;
pub const OP_I16_LE:  u64 = 896 + 17;
pub const OP_I16_GT:  u64 = 896 + 18;
pub const OP_I16_GE:  u64 = 896 + 19;

// u16: 1152-1279 (Skipped 1024-1151 to leave space after OP_PANIC)
pub const OP_U16_ADD: u64 = 1152 + 0;
pub const OP_U16_SUB: u64 = 1152 + 1;
pub const OP_U16_MUL: u64 = 1152 + 2;
pub const OP_U16_DIV: u64 = 1152 + 3;
pub const OP_U16_REM: u64 = 1152 + 4;
pub const OP_U16_AND: u64 = 1152 + 5;
pub const OP_U16_OR:  u64 = 1152 + 6;
pub const OP_U16_XOR: u64 = 1152 + 7;
pub const OP_U16_SHL: u64 = 1152 + 8;
pub const OP_U16_SHR: u64 = 1152 + 9;
pub const OP_U16_NOT: u64 = 1152 + 11;
pub const OP_U16_POW: u64 = 1152 + 13;
pub const OP_U16_EQ:  u64 = 1152 + 14;
pub const OP_U16_NE:  u64 = 1152 + 15;
pub const OP_U16_LT:  u64 = 1152 + 16;
pub const OP_U16_LE:  u64 = 1152 + 17;
pub const OP_U16_GT:  u64 = 1152 + 18;
pub const OP_U16_GE:  u64 = 1152 + 19;

// i8: 1280-1407
pub const OP_I8_ADD: u64 = 1280 + 0;
pub const OP_I8_SUB: u64 = 1280 + 1;
pub const OP_I8_MUL: u64 = 1280 + 2;
pub const OP_I8_DIV: u64 = 1280 + 3;
pub const OP_I8_REM: u64 = 1280 + 4;
pub const OP_I8_AND: u64 = 1280 + 5;
pub const OP_I8_OR:  u64 = 1280 + 6;
pub const OP_I8_XOR: u64 = 1280 + 7;
pub const OP_I8_SHL: u64 = 1280 + 8;
pub const OP_I8_SHR: u64 = 1280 + 9;
pub const OP_I8_NEG: u64 = 1280 + 10;
pub const OP_I8_NOT: u64 = 1280 + 11;
pub const OP_I8_ABS: u64 = 1280 + 12;
pub const OP_I8_POW: u64 = 1280 + 13;
pub const OP_I8_EQ:  u64 = 1280 + 14;
pub const OP_I8_NE:  u64 = 1280 + 15;
pub const OP_I8_LT:  u64 = 1280 + 16;
pub const OP_I8_LE:  u64 = 1280 + 17;
pub const OP_I8_GT:  u64 = 1280 + 18;
pub const OP_I8_GE:  u64 = 1280 + 19;

// u8: 1408-1535
pub const OP_U8_ADD: u64 = 1408 + 0;
pub const OP_U8_SUB: u64 = 1408 + 1;
pub const OP_U8_MUL: u64 = 1408 + 2;
pub const OP_U8_DIV: u64 = 1408 + 3;
pub const OP_U8_REM: u64 = 1408 + 4;
pub const OP_U8_AND: u64 = 1408 + 5;
pub const OP_U8_OR:  u64 = 1408 + 6;
pub const OP_U8_XOR: u64 = 1408 + 7;
pub const OP_U8_SHL: u64 = 1408 + 8;
pub const OP_U8_SHR: u64 = 1408 + 9;
pub const OP_U8_NOT: u64 = 1408 + 11;
pub const OP_U8_POW: u64 = 1408 + 13;
pub const OP_U8_EQ:  u64 = 1408 + 14;
pub const OP_U8_NE:  u64 = 1408 + 15;
pub const OP_U8_LT:  u64 = 1408 + 16;
pub const OP_U8_LE:  u64 = 1408 + 17;
pub const OP_U8_GT:  u64 = 1408 + 18;
pub const OP_U8_GE:  u64 = 1408 + 19;

// i128: 1536-1663
pub const OP_I128_ADD: u64 = 1536 + 0;
pub const OP_I128_SUB: u64 = 1536 + 1;
pub const OP_I128_MUL: u64 = 1536 + 2;
pub const OP_I128_DIV: u64 = 1536 + 3;
pub const OP_I128_REM: u64 = 1536 + 4;
pub const OP_I128_AND: u64 = 1536 + 5;
pub const OP_I128_OR:  u64 = 1536 + 6;
pub const OP_I128_XOR: u64 = 1536 + 7;
pub const OP_I128_SHL: u64 = 1536 + 8;
pub const OP_I128_SHR: u64 = 1536 + 9;
pub const OP_I128_NEG: u64 = 1536 + 10;
pub const OP_I128_NOT: u64 = 1536 + 11;
pub const OP_I128_ABS: u64 = 1536 + 12;
pub const OP_I128_POW: u64 = 1536 + 13;
pub const OP_I128_EQ:  u64 = 1536 + 14;
pub const OP_I128_NE:  u64 = 1536 + 15;
pub const OP_I128_LT:  u64 = 1536 + 16;
pub const OP_I128_LE:  u64 = 1536 + 17;
pub const OP_I128_GT:  u64 = 1536 + 18;
pub const OP_I128_GE:  u64 = 1536 + 19;

// u128: 1664-1791
pub const OP_U128_ADD: u64 = 1664 + 0;
pub const OP_U128_SUB: u64 = 1664 + 1;
pub const OP_U128_MUL: u64 = 1664 + 2;
pub const OP_U128_DIV: u64 = 1664 + 3;
pub const OP_U128_REM: u64 = 1664 + 4;
pub const OP_U128_AND: u64 = 1664 + 5;
pub const OP_U128_OR:  u64 = 1664 + 6;
pub const OP_U128_XOR: u64 = 1664 + 7;
pub const OP_U128_SHL: u64 = 1664 + 8;
pub const OP_U128_SHR: u64 = 1664 + 9;
pub const OP_U128_NOT: u64 = 1664 + 11;
pub const OP_U128_POW: u64 = 1664 + 13;
pub const OP_U128_EQ:  u64 = 1664 + 14;
pub const OP_U128_NE:  u64 = 1664 + 15;
pub const OP_U128_LT:  u64 = 1664 + 16;
pub const OP_U128_LE:  u64 = 1664 + 17;
pub const OP_U128_GT:  u64 = 1664 + 18;
pub const OP_U128_GE:  u64 = 1664 + 19;

// Misc Ops: Use range starting after the last numeric type (e.g., 2048+)
pub const OP_PANIC: u64 = 2048;

// We assume Function IDs, Symbol IDs, indices, and op codes fit within 60 bits

// Helper functions (could be moved to a shared module)

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
        let (dup0_idx, dup0_p) = builder.alloc_duplicator(Duplicator {
            principle: Port::NULL, // Input port of the gadget
            left: Port::NULL,      // Will be the output port
            right: era0_p,         // Connect right aux to eraser's principal port
        });
        // Wire eraser principal back to dup right aux port value
        builder.config.erasers[era0_idx].principle = Port::right(NodeType::Duplicator, partition_id, dup0_idx as u64);
        // Set principal port on the duplicator itself
        builder.config.duplicators[dup0_idx].principle = dup0_p;
        // Define the output port (left aux of the duplicator)
        let output_port = Port::left(NodeType::Duplicator, partition_id, dup0_idx as u64);

        Ok(GadgetPorts { principal_in: dup0_p, output: output_port })
    } else {
        // Recursive Step: Proj(k) = { DUP_k, ERA_k, P_{k-1} }
        let (erak_idx, erak_p) = builder.alloc_eraser(Eraser::new_null());
        // Build P_{k-1} recursively
        let sub_gadget = build_projection_gadget(builder, index - 1)?;
        let (dupk_idx, dupk_p) = builder.alloc_duplicator(Duplicator {
            principle: Port::NULL,            // Input port of this gadget level
            left: erak_p,                     // Connect left aux to the eraser's principal port
            right: sub_gadget.principal_in,   // Connect right aux to sub-gadget's input port
        });
        // Wire eraser principal back to dup left aux port value
        builder.config.erasers[erak_idx].principle = Port::left(NodeType::Duplicator, partition_id, dupk_idx as u64);
        // Set principal port on the duplicator itself
        builder.config.duplicators[dupk_idx].principle = dupk_p;

        // The final output port comes from the base case (P_0) of the sub-gadget
        Ok(GadgetPorts { principal_in: dupk_p, output: sub_gadget.output })
    }
}

#[inline(always)]
fn encode_static_data(tag: u64, data: u64) -> u64 {
    // Basic assertion, could be more robust
    debug_assert!(data <= DATA_MASK, "Static data exceeds 60 bits");
    (tag << TAG_SHIFT) | (data & DATA_MASK)
}

#[inline(always)]
fn encode_static_symbol(tag: u64, symbol: Symbol) -> u64 {
    encode_static_data(tag, symbol.id() as u64) // Assumes Symbol::id() exists and returns u32/usize
} 