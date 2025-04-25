use super::builder::NetBuilder;
use super::error::LoweringError;
// use parallax_mir::mir::{MirEdge, NodeId, SsaVarId}; // Commented out unresolved SsaVarId
use parallax_mir::mir::{MirEdge, NodeId, PortIndex};
use std::sync::atomic::Ordering;
use crate::port::Port as NetPort;
use crate::node::{NodeType, Redex}; // Added NodeType
use parallax_mir::mir::{MirNode, MirType};
use crate::node::{Constructor, Duplicator, Eraser, Static, Switch}; // Added Switch
use std::sync::atomic::AtomicU64;

/// Creates the initial Redex representing a connection described by an MIR edge.
/// Adds comments explaining the logic for different target node types.
pub(super) fn create_redex_for_edge(
    builder: &mut NetBuilder,
    edge: &MirEdge,
) -> Result<(), LoweringError> {

    let target_mir_node = builder.get_mir_node(edge.to_node)?;

    // Removed special handling for edges FROM Project nodes.
    // Project nodes are now lowered to Static nodes, handled below.

    // --- Normal Edge Handling --- 
    
    // 1. Get the source port from the port map.
    //    This now correctly handles Project/IsVariant/Downcast outputs as they
    //    are mapped to the principal port of their corresponding Static node.
    let source_net_port = builder.get_output_net_port(edge.from_node, edge.from_port)?;

    // 2. Determine the *actual* target port on the target net node.
    //    This might not be the principal port mapped in `lower_mir_node`.
    //    It depends on the target MIR node type and the input port index.
    let initial_target_port = builder.get_internal_node_port(edge.to_node)?;
    let target_net_port = determine_target_port(
        builder,
        edge.to_node,
        edge.to_port,
        target_mir_node,
        initial_target_port,
    )?;

    // 3. Add the redex connecting the source and target net ports.
    builder.config.initial_redexes.push(Redex(source_net_port, target_net_port));

    Ok(())
}

/// Determines the specific NetPort (Principal, Left Aux, Right Aux) on a target net node
/// that corresponds to a given MIR input port index.
/// Takes the initially mapped port (usually principal or internal placeholder) as a starting point.
pub(super) fn determine_target_port(
    builder: &NetBuilder, // Changed to immutable borrow
    target_mir_node_id: NodeId,
    target_mir_port_index: PortIndex,
    target_mir_node: &MirNode,
    initial_target_port: NetPort // The port initially mapped (e.g., by map_internal_node_port)
) -> Result<NetPort, LoweringError>
{
    // Use node_index() method from NetPort
    let target_node_type_u8 = initial_target_port.node_type();
    let target_kind = NodeType::from_u8(target_node_type_u8).ok_or_else(|| 
        LoweringError::Internal(format!("Invalid node type {} in port {:?}", target_node_type_u8, initial_target_port))
    )?;
    let target_partition = initial_target_port.partition_id();
    let target_index = initial_target_port.node_index(); // Use correct method

    match target_mir_node {
        MirNode::Parameter { .. } | MirNode::Constant { .. } | MirNode::StaticAddr { .. } |
        MirNode::Unreachable => {
            // These nodes don't have *inputs* in the net representation handled by edges.
            // Parameter output is handled specially in lower_function.
            // Constant/StaticAddr are sources only.
            // Terminators don't map to consuming net nodes.
            Err(LoweringError::Internal(format!(
                "Attempted to create edge TO node {:?} ({:?}), which should not receive input via edge",
                target_mir_node_id, target_mir_node
            )))
        }

        MirNode::Constructor { .. } => {
            // Phase 2: Flat tree Cons(f0, Cons(f1,... Cons(fN-1, TAG)))
            // MIR Input i maps to the LEFT aux port of the i-th constructor node.
            // We find this port using the derived internal node ID.
            let input_index = target_mir_port_index.0;
            let internal_node_id_base = target_mir_node_id.0;
            let internal_input_node_id = NodeId(internal_node_id_base * 1000 + input_index);
            builder.get_internal_node_port(internal_input_node_id) // This should return the Left Aux Port directly
        }

        MirNode::ArrayConstruct { .. } => {
            // Marked as unimplemented in lowering/nodes.rs
            Err(LoweringError::NotImplemented("Edge to ArrayConstruct"))
         }
         
         MirNode::ArrayProject { .. } => {
            // Marked as unimplemented in lowering/nodes.rs
            Err(LoweringError::NotImplemented("Edge to ArrayProject"))
         }

         MirNode::Project { .. } | MirNode::Parameter { .. } | MirNode::MatchDispatch { .. } => {
             // These map MIR input 0 to the internal node port (Gadget Input, Duplicator Principal, MatchDispatch Principal).
             let input_index = target_mir_port_index.0;
             if input_index == 0 {
                 // The internal map holds the principal port of the Static/Dup/Gadget Input
                 builder.get_internal_node_port(target_mir_node_id)
             } else {
                 Err(LoweringError::Internal(format!(
                     "Edge targets port {} on {:?} {:?}, expected 0",
                     input_index, target_mir_node, target_mir_node_id
                 )))
             }
         }

        MirNode::Closure { capture_types, .. } => {
            // Closure lowered to nested constructors for captures + Static for func ptr.
            // Inputs 0..N-1 are captures, connecting to the *Left* Aux port of the
            // i-th nested constructor in the capture list.
            let num_captures = capture_types.len();
            let capture_input_index = target_mir_port_index.0;
            if capture_input_index >= num_captures as u32 { // Cast usize to u32
                return Err(LoweringError::Internal(format!(
                    "Edge targets capture input {} on Closure {:?}, but only {} captures",
                    capture_input_index, target_mir_node_id, num_captures
                )));
            }

            // Find the specific nested constructor node mapped for this capture input index.
            // Use the internal node mapping scheme from lowering/nodes.rs (base * 1000 + 100 + index)
            let internal_node_id_base = target_mir_node_id.0;
            let internal_node_sub_id = internal_node_id_base * 1000 + 100 + capture_input_index as u32;
            let nested_cons_port = builder.get_internal_node_port(NodeId(internal_node_sub_id))?;
            
            // Capture inputs always connect to the Left Aux port of their corresponding Cons node.
            // The internal node map holds this Left Aux port directly.
            builder.get_internal_node_port(NodeId(internal_node_sub_id))
        }

        // Renamed Call to FunctionCall
        MirNode::FunctionCall { func_ty, .. } => {
            // Lowered to App(App(...App(FuncRef, Arg0)..., ArgN-2), ArgN-1)
            let arity = match func_ty {
                MirType::FunctionPointer(params, _) => params.len(),
                _ => return Err(LoweringError::InvalidMir(format!(
                    "FunctionCall node {:?} has non-function type: {:?}", target_mir_node_id, func_ty
                ))),
            };
            
            let input_index = target_mir_port_index.0; // 0 for FuncRef, 1..N for Args 0..N-1
            let internal_node_id_base = target_mir_node_id.0;
            let internal_app_node_id = NodeId(internal_node_id_base * 1000 + input_index as u32);

            // The internal mapping holds the Principal port of the relevant AppCON (App_i)
            let app_i_principal_port = builder.get_internal_node_port(internal_app_node_id)?;
            let app_i_kind = NodeType::from_u8(app_i_principal_port.node_type()).ok_or_else(|| 
                     LoweringError::Internal(format!("Invalid node type {} in call app{} port {:?}", app_i_principal_port.node_type(), input_index, app_i_principal_port))
            )?;
            if app_i_kind != NodeType::Constructor {
                 return Err(LoweringError::Internal(format!("Expected Constructor for App{} internal mapping, found {:?}", input_index, app_i_kind)));
            }

            // Determine Left or Right Aux based on index
            if input_index == 0 { // Function Ref
                Ok(NetPort::right(app_i_kind, app_i_principal_port.partition_id(), app_i_principal_port.node_index())) // Connect to App0 Right Aux
            } else { // Argument i
                 Ok(NetPort::left(app_i_kind, app_i_principal_port.partition_id(), app_i_principal_port.node_index())) // Connect to App_i Left Aux
            }
        }

        MirNode::IfValue { .. } => {
            // Input 0 (Cond) -> Principal
            // Input 1 (True) -> Left Aux
            // Input 2 (False) -> Right Aux
            let input_index = target_mir_port_index.0;
            // initial_target_port should be the principal port of the Switch
            if target_kind != NodeType::Switch {
                return Err(LoweringError::Internal(format!(
                    "IfValue node {:?} did not map to a Switch port, got {:?} ({:?})",
                    target_mir_node_id, initial_target_port, target_kind
                )));
            }
            let port = match input_index {
                0 => initial_target_port,                                                  // Cond -> Principal
                1 => NetPort::left(target_kind, target_partition, target_index), // True Val -> Left Aux
                2 => NetPort::right(target_kind, target_partition, target_index),// False Val -> Right Aux
                _ => return Err(LoweringError::Internal(format!(
                    "Edge targets port {} on IfValue {:?}, expected 0, 1, or 2",
                    input_index, target_mir_node_id
                ))),
            };
            Ok(port)
        }
    }
}

/*
fn determine_source_port(
    builder: &NetBuilder,
    source_mir_node_id: NodeId,
    source_mir_port_index: PortIndex,
    source_mir_node: &MirNode,
    initial_source_port: Port // The port initially mapped (usually principal)
) -> Result<Port, LoweringError> {
    // For most nodes, just use the mapped port directly
    // Special handling is needed for nodes whose interaction behavior might
    // require using a specific auxiliary port instead of the principal
    match source_mir_node {
        // Examples where mapping a source port might need special handling:
        // In interaction nets, a node's principal port is typically the one that
        // interacts with other nodes' principal ports. When connecting to 
        // an auxiliary port, we use the node's auxiliary ports.

        // For most nodes, just use the port as mapped
        _ => Ok(initial_source_port),
    }
} 
*/ 