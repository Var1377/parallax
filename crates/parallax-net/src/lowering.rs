use crate::node::{Constructor, Duplicator, Ref, Number, Switch, Async, Eraser, Redex, NodeType};
use crate::port::Port;
use crate::partition::{Partition, PartitionState, NodeStorage};
use parallax_mir::mir::{MirModule, MirGraph, MirNode, NodeId, PortIndex, MirEdge, MirType, Symbol, MirOp};
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::atomic::AtomicU64;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum LoweringError {
    #[error("Unsupported MIR node type: {0:?}")]
    UnsupportedNode(MirNode),
    #[error("Unsupported MIR type: {0:?}")]
    UnsupportedType(MirType),
    #[error("Unsupported MIR binary operation: {0:?}")]
    UnsupportedOp(MirOp),
    #[error("Invalid MIR structure: {0}")]
    InvalidMir(String),
    #[error("Failed to find source/target port for MIR edge: {0:?}")]
    PortNotFound(MirEdge),
    #[error("Missing node definition during lowering for node ID: {0:?}")]
    NodeNotFound(NodeId),
    #[error("Lowering for {0} not implemented yet")]
    NotImplemented(&'static str),
    #[error("Internal error during lowering: {0}")]
    Internal(String),
}

/// Represents the initial state of an interaction net for a single function.
/// This contains the allocated nodes and the initial redexes to kickstart reduction.
/// Note: This holds copies of NodeStorage conceptually; the actual runtime
/// will merge these into live Partitions.
#[derive(Debug, Default)]
pub struct InitialNetConfig {
    pub constructors: NodeStorage<Constructor>,
    pub duplicators: NodeStorage<Duplicator>,
    pub refs: NodeStorage<Ref>,
    pub numbers: NodeStorage<Number>,
    pub switches: NodeStorage<Switch>,
    pub asyncs: NodeStorage<Async>,
    pub erasers: NodeStorage<Eraser>,
    pub initial_redexes: Vec<Redex>,
    /// Mapping from MIR parameter index to the principal Port of its entry Duplicator.
    pub parameter_ports: Vec<Port>,
    /// The single exit Port providing the function's return value.
    pub return_port: Option<Port>,
    // We might need a way to map back from net node index/Port to original MIR node for debugging
    // pub debug_info: HashMap<Port, (NodeId, PortIndex)>,
}

/// Helper struct to manage the state during the lowering of a single MirGraph.
struct NetBuilder<'a> {
    /// Reference to the MIR graph being lowered.
    graph: &'a MirGraph,
    /// The interaction net configuration being built.
    config: InitialNetConfig,
    /// Mapping from (MirNodeId, MirPortIndex) to the corresponding interaction net Port.
    port_map: HashMap<(NodeId, PortIndex), Port>,
    /// Mapping for nodes that don't have a direct output port mapped (e.g., OpApply constructor)
    internal_node_map: HashMap<NodeId, Port>,
    partition_id: u16,
}

impl<'a> NetBuilder<'a> {
    fn new(graph: &'a MirGraph, partition_id: u16) -> Self {
        NetBuilder {
            graph,
            config: InitialNetConfig::default(),
            port_map: HashMap::new(),
            internal_node_map: HashMap::new(),
            partition_id,
        }
    }

    /// Allocates a new node in the appropriate slab and returns its index and principal port.
    fn alloc_constructor(&mut self, node: Constructor) -> (usize, Port) {
        let index = self.config.constructors.insert(node);
        let port = Port::principal(NodeType::Constructor, self.partition_id, index as u64);
        (index, port)
    }

    fn alloc_duplicator(&mut self, node: Duplicator) -> (usize, Port) {
        let index = self.config.duplicators.insert(node);
        let port = Port::principal(NodeType::Duplicator, self.partition_id, index as u64);
        (index, port)
    }

    fn alloc_ref(&mut self, node: Ref) -> (usize, Port) {
        let index = self.config.refs.insert(node);
        let port = Port::principal(NodeType::Ref, self.partition_id, index as u64);
        (index, port)
    }

    fn alloc_number(&mut self, node: Number) -> (usize, Port) {
        let index = self.config.numbers.insert(node);
        let port = Port::principal(NodeType::Number, self.partition_id, index as u64);
        (index, port)
    }

    fn alloc_switch(&mut self, node: Switch) -> (usize, Port) {
        let index = self.config.switches.insert(node);
        let port = Port::principal(NodeType::Switch, self.partition_id, index as u64);
        (index, port)
    }

    fn alloc_async(&mut self, node: Async) -> (usize, Port) {
        let index = self.config.asyncs.insert(node);
        let port = Port::principal(NodeType::Async, self.partition_id, index as u64);
        (index, port)
    }

    fn alloc_eraser(&mut self, node: Eraser) -> (usize, Port) {
        let index = self.config.erasers.insert(node);
        let port = Port::principal(NodeType::Eraser, self.partition_id, index as u64);
        (index, port)
    }

    /// Registers the mapping from an MIR port to a net port.
    fn map_output_port(&mut self, mir_node_id: NodeId, mir_port_index: PortIndex, net_port: Port) {
        self.port_map.insert((mir_node_id, mir_port_index), net_port);
    }

    fn map_internal_node_port(&mut self, mir_node_id: NodeId, net_port: Port) {
        self.internal_node_map.insert(mir_node_id, net_port);
    }

    /// Retrieves the net port corresponding to an MIR port.
    fn get_output_net_port(&self, mir_node_id: NodeId, mir_port_index: PortIndex) -> Result<Port, LoweringError> {
        self.port_map.get(&(mir_node_id, mir_port_index)).cloned().ok_or_else(|| {
            let edge_context = self.graph.edges.iter().find(|e| (e.from_node == mir_node_id && e.from_port == mir_port_index) || (e.to_node == mir_node_id));
            LoweringError::Internal(format!(
                "Could not find mapped output net port for MIR port ({:?}, {:?}). Originating node: {:?}. Edge context: {:?}",
                mir_node_id, mir_port_index, self.graph.nodes.get(&mir_node_id), edge_context
            ))
        })
    }

    fn get_internal_node_port(&self, mir_node_id: NodeId) -> Result<Port, LoweringError> {
        self.internal_node_map.get(&mir_node_id).cloned().ok_or_else(|| {
            LoweringError::Internal(format!("Could not find internal mapped port for MIR node {:?}", mir_node_id))
        })
    }

    /// Retrieves the MIR node definition.
    fn get_mir_node(&self, node_id: NodeId) -> Result<&MirNode, LoweringError> {
        self.graph.nodes.get(&node_id).ok_or(LoweringError::NodeNotFound(node_id))
    }

    /// Helper to lower nested structures (like tuples, arrays, >2-arity constructors)
    /// Returns the principal port of the head constructor.
    fn lower_nested_constructor(&mut self, element_ports: &[Port]) -> Result<Port, LoweringError> {
        if element_ports.is_empty() {
            // How to represent empty tuple/nil? Maybe a specific Ref/Number tag?
            // For now, create a constructor with NULL ports - needs reduction rule.
            let (idx, principal_port) = self.alloc_constructor(Constructor {
                principle: Port::NULL, left: Port::NULL, right: Port::NULL
            });
            self.config.constructors[idx].principle = principal_port;
            // TODO: Need a way to tag this as NIL or use a specific encoding.
            Ok(principal_port)
        } else if element_ports.len() == 1 {
            // Single element, just return its port (no constructor needed?)
            // Or wrap in a unary constructor? Let's wrap for consistency.
            let (idx, principal_port) = self.alloc_constructor(Constructor {
                principle: Port::NULL, left: element_ports[0], right: Port::NULL // Use NULL for second element
            });
            self.config.constructors[idx].principle = principal_port;
            Ok(principal_port)
        } else {
            // Recursively build nested pairs: Cons(a, Cons(b, Cons(c, ...)))
            let tail_port = self.lower_nested_constructor(&element_ports[1..])?;
            let (idx, principal_port) = self.alloc_constructor(Constructor {
                principle: Port::NULL, left: element_ports[0], right: tail_port
            });
            self.config.constructors[idx].principle = principal_port;
            Ok(principal_port)
        }
    }
}

/// Lowers a full MIR module into a collection of initial interaction net configurations.
pub fn lower_module(
    mir_module: &MirModule,
) -> Result<HashMap<Symbol, InitialNetConfig>, LoweringError> {
    let mut lowered_functions = HashMap::new();
    let mut partition_counter: u16 = 0;
    for (symbol, graph) in &mir_module.functions {
        println!("Lowering function: {:?}", symbol);
        let current_partition_id = partition_counter;
        partition_counter = partition_counter.checked_add(1).ok_or_else(|| LoweringError::Internal("Too many partitions".to_string()))?;
        let config = lower_function(graph, current_partition_id)?;
        lowered_functions.insert(*symbol, config);
    }
    Ok(lowered_functions)
}

/// Lowers a single MIR function graph into its initial interaction net configuration.
fn lower_function(
    graph: &MirGraph,
    partition_id: u16,
) -> Result<InitialNetConfig, LoweringError> {
    let mut builder = NetBuilder::new(graph, partition_id);
    // Order matters for nodes like BinOp that create internal nodes referenced later.
    // A simple topological sort might be better, but for now, iterate nodes then edges.
    for (node_id, mir_node) in &graph.nodes {
         lower_mir_node(&mut builder, *node_id, mir_node)?;
    }
     for edge in &graph.edges {
        // Skip edges originating from Project nodes, as they are handled specially
        // when processing edges *to* the Project node's consumers.
        if let MirNode::Project{..} = builder.get_mir_node(edge.from_node)? {
            continue;
        }
        create_redex_for_edge(&mut builder, edge)?;
    }
    builder.config.parameter_ports = graph.parameter_nodes
        .iter()
        .map(|param_node_id| builder.get_output_net_port(*param_node_id, PortIndex(0)))
        .collect::<Result<Vec<_>, _>>()?;
    builder.config.return_port = match graph.return_port {
         Some((node_id, port_index)) => Some(builder.get_output_net_port(node_id, port_index)?),
         None => None,
    };
    Ok(builder.config)
}

/// Encode MIR Op into u64 for Number node data field, reserving high bits for type/tag.
/// This is a placeholder encoding. Needs coordination with runtime reduction rules.
fn encode_mir_op(op: MirOp) -> u64 {
    match op {
        MirOp::Add => 1, MirOp::Sub => 2, MirOp::Mul => 3, MirOp::Div => 4, MirOp::Mod => 5,
        MirOp::BitAnd => 6, MirOp::BitOr => 7, MirOp::BitXor => 8, MirOp::Shl => 9, MirOp::Shr => 10,
        MirOp::Eq => 11, MirOp::Ne => 12, MirOp::Lt => 13, MirOp::Le => 14, MirOp::Gt => 15, MirOp::Ge => 16,
    }
}

/// Lowers a single MIR node, creating corresponding net nodes and mapping output ports.
/// Adds comments explaining the mapping strategy for each node type.
fn lower_mir_node(
    builder: &mut NetBuilder,
    node_id: NodeId,
    mir_node: &MirNode,
) -> Result<(), LoweringError> {
    match mir_node {
        // --- Function Interface --- 
        MirNode::Parameter { .. } => {
            // Lowering Strategy:
            // Represent a function parameter entry point with a Duplicator.
            // The parameter value "enters" through the principal port during a function call interaction.
            // The Duplicator's auxiliary ports (left/right) distribute the value to consumers within the function graph.
            let (idx, principal_port) = builder.alloc_duplicator(Duplicator {
                 principle: Port::NULL, // Placeholder, value comes from caller interaction
                 left: Port::NULL,      // Connected to consumers via reduction
                 right: Port::NULL,     // Connected to consumers via reduction
             });
             // Map the MIR parameter's output port (index 0) to the Duplicator's principal port.
             // Edges originating from this MIR port will connect to this principal port.
             builder.map_output_port(node_id, PortIndex(0), principal_port);
             // Store the principal port in the Duplicator struct for completeness/debugging (optional).
             builder.config.duplicators[idx].principle = principal_port;
             Ok(())
        }

        // --- Constants and Globals --- 
        MirNode::Constant { value, ty } => {
            // Lowering Strategy:
            // Represent primitive constants (Int, Bool, Float, Char) using Number nodes.
            // The numeric value (or bit representation) is stored in the Number node's data field.
            // Strings are lowered into linked lists of Constructor nodes holding character codes (Numbers).
            match value {
                parallax_hir::hir::HirLiteral::Unit => {
                    // Lower Unit to the NIL representation.
                    let nil_port = builder.lower_nested_constructor(&[])?;
                    builder.map_output_port(node_id, PortIndex(0), nil_port);
                }
                parallax_hir::hir::HirLiteral::Int(val) => {
                    let truncated_val = *val as u64; // Potential truncation from u128
                     if *val != truncated_val as u128 {
                        println!("Warning: Integer literal {} truncated during lowering to u64", val);
                     }
                    let (idx, principal_port) = builder.alloc_number(Number {
                         principle: Port::NULL, // Placeholder
                         data: AtomicU64::new(truncated_val),
                     });
                     builder.config.numbers[idx].principle = principal_port;
                     builder.map_output_port(node_id, PortIndex(0), principal_port);
                }
                parallax_hir::hir::HirLiteral::Bool(val) => {
                    let num_val = if *val { 1 } else { 0 }; // Encode bool as 1 or 0
                     let (idx, principal_port) = builder.alloc_number(Number {
                         principle: Port::NULL, data: AtomicU64::new(num_val),
                     });
                     builder.config.numbers[idx].principle = principal_port;
                     builder.map_output_port(node_id, PortIndex(0), principal_port);
                }
                parallax_hir::hir::HirLiteral::Float(val) => {
                    let bits = val.to_bits(); // Store f64 bits directly in u64
                    let (idx, principal_port) = builder.alloc_number(Number {
                         principle: Port::NULL, data: AtomicU64::new(bits),
                     });
                      builder.config.numbers[idx].principle = principal_port;
                      builder.map_output_port(node_id, PortIndex(0), principal_port);
                 }
                parallax_hir::hir::HirLiteral::Char(c) => {
                    let num_val = *c as u64; // Store char as its Unicode value
                    let (idx, principal_port) = builder.alloc_number(Number {
                        principle: Port::NULL, data: AtomicU64::new(num_val),
                    });
                    builder.config.numbers[idx].principle = principal_port;
                    builder.map_output_port(node_id, PortIndex(0), principal_port);
                }
                parallax_hir::hir::HirLiteral::String(s) => {
                    // Lower string to list: Cons(char_num_0, Cons(char_num_1, ... Nil))
                    let char_ports: Vec<Port> = s.chars().map(|c| {
                        let num_val = c as u64;
                        let (idx, pport) = builder.alloc_number(Number {
                            principle: Port::NULL, data: AtomicU64::new(num_val),
                        });
                        builder.config.numbers[idx].principle = pport;
                        pport
                    }).collect();
                    // Use helper to create nested constructors
                    let list_head_port = builder.lower_nested_constructor(&char_ports)?;
                    builder.map_output_port(node_id, PortIndex(0), list_head_port);
                }
            }
            Ok(())
        }
         MirNode::StaticAddr { symbol, ty } => {
             // Lowering Strategy:
             // Represent the address of a function or static variable using a Ref node.
             // The Ref node's data field stores the unique Symbol ID (symbol.0).
             // Runtime interaction rules (e.g., Ref~Consumer) handle loading/calling.
             let id = symbol.0;
             let (idx, principal_port) = builder.alloc_ref(Ref {
                 principle: Port::NULL, data: AtomicU64::new(id),
             });
             builder.config.refs[idx].principle = principal_port;
             builder.map_output_port(node_id, PortIndex(0), principal_port);
             Ok(())
         }

         // --- Data Structures --- 
         MirNode::Constructor { tag, field_types, ty } => {
              // Lowering Strategy:
              // Represent structs, enum variants, tuples using Constructor nodes.
              // For arity <= 2, use one Constructor node. Inputs connect to left/right aux ports.
              // For arity > 2, use nested Constructors: Cons(f0, Cons(f1, Cons(f2, ...))).
              // Inputs connect to the left aux port of the corresponding nested Constructor.
              // The `tag` (Symbol ID) might be needed by runtime rules (e.g., for enum matching).
              // Currently, the tag is not explicitly stored; rules might infer it from wiring or expect a specific pattern.
              
               let arity = field_types.len();
               let output_port: Port;

               if arity <= 2 {
                  // Allocate a single Constructor node.
                  // Ports are initialized to NULL; edges will create redexes to connect inputs later.
                  let (idx, principal_port) = builder.alloc_constructor(Constructor {
                      principle: Port::NULL, left: Port::NULL, right: Port::NULL
                  });
                  builder.config.constructors[idx].principle = principal_port;
                  output_port = principal_port;
                  // Map internal node port with derived ID for edge connection to aux ports
                  // This helps `create_redex_for_edge` find the correct node index.
                  builder.map_internal_node_port(node_id, principal_port);
               } else {
                   // Arity > 2: requires nesting: Cons(f0, Cons(f1, ... Cons(fN-2, fN-1))) 
                   // We need N-1 constructor nodes.
                   let num_cons = arity - 1;
                   let mut cons_indices = Vec::with_capacity(num_cons);
                   let mut cons_principals = Vec::with_capacity(num_cons);
                   
                   // Allocate all N-1 constructors first.
                   for i in 0..num_cons {
                       let (idx, pport) = builder.alloc_constructor(Constructor {
                           principle: Port::NULL, left: Port::NULL, right: Port::NULL
                       });
                       builder.config.constructors[idx].principle = pport;
                       // Map *internal* nodes with derived IDs based on field index `i`.
                       // Input `i` will connect to the left aux port of the constructor at `cons_principals[i]`.
                       // Input `N-1` connects to the right aux port of the constructor at `cons_principals[N-2]`.
                       builder.map_internal_node_port(NodeId(node_id.0 * 1000 + i as u32), pport); 
                       cons_indices.push(idx);
                       cons_principals.push(pport);
                   }
                   
                   // Link the `right` ports to form the chain, except for the last one.
                   for i in 0..(num_cons - 1) {
                       builder.config.constructors[cons_indices[i]].right = cons_principals[i+1];
                   }
                   // The last constructor's right port remains NULL for now, it will be connected
                   // by the edge providing the last field (input N-1).
                   
                   // The principal port of the whole structure is the principal of the first constructor.
                   output_port = cons_principals[0];
               }
               
              // Map the single output port (index 0) of the MIR Constructor node 
              // to the principal port of the (potentially nested) net Constructor structure.
              builder.map_output_port(node_id, PortIndex(0), output_port);
              Ok(())
         }
         MirNode::Project { .. } => {
             // Lowering Strategy:
             // Projection (`a.field`) is not represented by a static node.
             // It's handled dynamically during reduction when a consumer interacts
             // with the aggregate (Constructor) it wants to project from.
             // The edge originating from the Project node's output is handled specially
             // in `create_redex_for_edge` to set up the correct interaction.
             println!("Skipping node creation for Project {:?}", node_id);
             Ok(())
         }
         MirNode::ArrayConstruct { size, .. } => {
             // Lowering Strategy:
             // Lower arrays to nested constructors: Cons(e0, Cons(e1, ... Nil)).
             // Similar to multi-arity Constructors, but ends with a NIL representation.
             // NIL representation: A Constructor node with a special tag or specific NULL pattern? (TODO)
             
             let num_elems = *size;
             let output_port: Port;

             // Create NIL node (placeholder)
             // TODO: Define a proper NIL representation (e.g., specific Ref ID or Constructor pattern).
             let (nil_idx, nil_port) = builder.alloc_constructor(Constructor { principle: Port::NULL, left: Port::NULL, right: Port::NULL });
             builder.config.constructors[nil_idx].principle = nil_port;

             if num_elems == 0 {
                 output_port = nil_port;
             } else {
                 // Allocate N constructors for N elements.
                 let mut cons_nodes = Vec::with_capacity(num_elems);
                  for i in 0..num_elems {
                      let (idx, pport) = builder.alloc_constructor(Constructor { principle: Port::NULL, left: Port::NULL, right: Port::NULL });
                      builder.config.constructors[idx].principle = pport;
                      // Map internal nodes for edge connection (Input `i` connects to Left Aux of `i`-th cons)
                      builder.map_internal_node_port(NodeId(node_id.0 * 1000 + i as u32), pport);
                      cons_nodes.push((idx, pport));
                  }
                  // Link the right ports, ending with NIL.
                 for i in 0..(num_elems - 1) {
                     builder.config.constructors[cons_nodes[i].0].right = cons_nodes[i+1].1;
                 }
                 // Link the last constructor to NIL.
                 builder.config.constructors[cons_nodes[num_elems-1].0].right = nil_port;
                 
                 // Output is the head of the list.
                 output_port = cons_nodes[0].1;
             }
             // Map the MIR node's output port to the head of the list.
             builder.map_output_port(node_id, PortIndex(0), output_port);
             Ok(())
        }
        MirNode::ArrayProject { .. } => {
             // Lowering Strategy:
             // Array projection (`arr[idx]`) requires dynamic interaction.
             // Create a Duplicator node to coordinate the interaction.
             // Input 0 (Array - Cons list head) connects to Left Aux.
             // Input 1 (Index - Number) connects to Right Aux.
             // Output 0 (Element) comes from the Principal port after reduction.
             // Assumes a runtime interaction rule: Duplicator(Array, Index) -> Element.
             // This rule likely involves interactions between the Duplicator, Index, and Array Cons nodes.
             let (project_dup_idx, project_dup_port) = builder.alloc_duplicator(Duplicator {
                 principle: Port::NULL, // Element output comes here
                 left: Port::NULL,      // Array input connects here
                 right: Port::NULL,     // Index input connects here
             });
             builder.config.duplicators[project_dup_idx].principle = project_dup_port;
             // Map the MIR node output (the element) to the Duplicator's principal port.
             builder.map_output_port(node_id, PortIndex(0), project_dup_port);
             // Map internal node for edge connection to aux ports.
             builder.map_internal_node_port(node_id, project_dup_port);
             Ok(())
        }
        MirNode::Closure { specialized_function_symbol, capture_types, .. } => {
            // Lowering Strategy (New Definition):
            // Closure node produces two outputs:
            // 1. Environment Tuple (Output 0): A nested Constructor list holding captured variables (Inputs 0..N-1).
            // 2. Function Pointer (Output 1): A Ref node holding the Symbol ID of the specialized function.

            // 1. Create Function Pointer Ref node.
            let func_id = specialized_function_symbol.0;
            let (ref_idx, func_ref_port) = builder.alloc_ref(Ref {
                principle: Port::NULL, data: AtomicU64::new(func_id)
            });
            builder.config.refs[ref_idx].principle = func_ref_port;
            // Map MIR Output 1 to the function pointer Ref node.
            builder.map_output_port(node_id, PortIndex(1), func_ref_port);

            // 2. Create Environment Tuple (Nested Constructors for captures).
             let num_captures = capture_types.len();
             let env_head_port: Port;
             let nil_port = { // Define NIL representation (e.g., constructor with specific ports/tag) - TODO refine this
                 let (idx, pport) = builder.alloc_constructor(Constructor { principle: Port::NULL, left: Port::NULL, right: Port::NULL });
                 builder.config.constructors[idx].principle = pport;
                 pport
             };

             if num_captures == 0 {
                 env_head_port = nil_port; // Environment is NIL for no captures.
             } else {
                 // Allocate N constructors for N captures.
                 let mut capture_cons_nodes = Vec::with_capacity(num_captures);
                  for i in 0..num_captures {
                      let (idx, pport) = builder.alloc_constructor(Constructor { principle: Port::NULL, left: Port::NULL, right: Port::NULL });
                      builder.config.constructors[idx].principle = pport;
                      // Map internal nodes for edge connection (Capture Input `i` connects to Left Aux of `i`-th cons).
                      builder.map_internal_node_port(NodeId(node_id.0 * 1000 + i as u32), pport);
                      capture_cons_nodes.push((idx, pport));
                  }
                  // Link the right ports, ending with NIL.
                 for i in 0..(num_captures - 1) {
                     builder.config.constructors[capture_cons_nodes[i].0].right = capture_cons_nodes[i+1].1;
                 }
                 builder.config.constructors[capture_cons_nodes[num_captures-1].0].right = nil_port;
                 env_head_port = capture_cons_nodes[0].1; // Head of the capture list.
             }
            // Map MIR Output 0 to the environment tuple's head constructor.
            builder.map_output_port(node_id, PortIndex(0), env_head_port);
            Ok(())
        }

        // --- Control Flow & Operations ---
        MirNode::FunctionCall { .. } => {
             // Lowering Strategy:
             // Function calls are complex and rely heavily on runtime interaction rules.
             // We assume a rule like `Ref ~ Duplicator` triggers the call.
             // The Ref node (Function Address, from Input 0) interacts with a Duplicator node.
             // This Duplicator acts as the continuation point, receiving the result.
             // Input arguments (Inputs 1..N) must be made available to the Ref~Duplicator interaction.
             // This lowering step only creates the Duplicator continuation node.
             // Edges connecting the function Ref and arguments target this Duplicator.
             let (continuation_dup_idx, continuation_port) = builder.alloc_duplicator(Duplicator {
                 principle: Port::NULL, // Result consumer connects here
                 left: Port::NULL,      // Function result arrives here
                 right: Port::NULL,     // Used by runtime rule? Or erased?
             });
             builder.config.duplicators[continuation_dup_idx].principle = continuation_port;
             // Map the MIR node output (the function's return value) to the continuation Duplicator's principal port.
             builder.map_output_port(node_id, PortIndex(0), continuation_port);
             // Map internal node for edge connection to the Duplicator (where Ref and Args connect).
             builder.map_internal_node_port(node_id, continuation_port);
             Ok(())
        }
        MirNode::IfValue { .. } => {
            // Lowering Strategy:
            // Lower `if cond then true_val else false_val` to a Switch node.
            // Input 0 (Condition - Bool Number) connects to Principal port.
            // Input 1 (True Value) connects to Left Aux port.
            // Input 2 (False Value) connects to Right Aux port.
            // Output 0 (Result) comes from interaction with the Principal port.
            // Assumes runtime rule `Switch(cond) ~ Consumer` checks cond and routes interaction to Left/Right Aux.
            let (idx, principal_port) = builder.alloc_switch(Switch {
                principle: Port::NULL, // Condition input connects here
                left: Port::NULL,      // True value input connects here
                right: Port::NULL      // False value input connects here
            });
            builder.config.switches[idx].principle = principal_port;
            // Map the MIR output port (the result) to the Switch's principal port (conceptually).
            // The actual value is obtained via interaction through the aux ports.
            builder.map_output_port(node_id, PortIndex(0), principal_port);
            // Map internal node for edge connection to aux/principal ports.
            builder.map_internal_node_port(node_id, principal_port);
             Ok(())
        }
        MirNode::BinaryOp { op, .. } => {
             // Lowering Strategy:
             // Lower binary operations using an intermediate Constructor node (`OpApply`)
             // and a Number node holding the operation code (`OpCode`).
             // Pattern: OpApply(OpCode, Lhs) ~ Rhs -> Result
             // 1. Create OpCode Number node. Output 0 (Result) maps to its principal port.
             // 2. Create OpApply Constructor node. Left connects to OpCode, Right will connect to Lhs.
             // Assumes runtime rule `Constructor(OpCode, Lhs) ~ Rhs(Number)` performs the operation.
             
             // 1. Create OpCode Number node.
            let op_code = encode_mir_op(*op);
            let (op_node_idx, op_node_port) = builder.alloc_number(Number {
                 principle: Port::NULL, // Result consumer connects here
                 data: AtomicU64::new(op_code),
             });
             builder.config.numbers[op_node_idx].principle = op_node_port;
             // Map MIR Output 0 (Result) to the OpCode Number's principal port.
             builder.map_output_port(node_id, PortIndex(0), op_node_port);

             // 2. Create intermediate OpApply Constructor node.
             let (apply_node_idx, apply_node_port) = builder.alloc_constructor(Constructor {
                 principle: Port::NULL, // RHS connects here
                 left: op_node_port,    // Link to the OpCode node
                 right: Port::NULL,     // LHS connects here
             });
             builder.config.constructors[apply_node_idx].principle = apply_node_port;
             // Map the MIR BinaryOp node internally to the OpApply node's principal port 
             // so that edges for LHS and RHS can find the correct target ports.
             builder.map_internal_node_port(node_id, apply_node_port);
             Ok(())
        }
         MirNode::IsVariant { variant_symbol, .. } => {
             // Lowering Strategy:
             // Check if an enum value (Constructor) matches a specific variant tag.
             // Use a Ref node holding the target variant tag (`TargetTagRef`).
             // Input 0 (Enum value) connects to the Principal port of TargetTagRef.
             // Output 0 (Bool result) is produced by the interaction and comes from TargetTagRef's Principal port.
             // Assumes runtime rule `Ref(TargetTag) ~ EnumConstructor -> Bool(Number)`. 
             let target_tag = variant_symbol.0; // Assuming Symbol wraps u64 tag
             let (tag_ref_idx, tag_ref_port) = builder.alloc_ref(Ref {
                principle: Port::NULL, // Boolean result consumer connects here
                data: AtomicU64::new(target_tag),
             });
             builder.config.refs[tag_ref_idx].principle = tag_ref_port;
             // Map MIR Output 0 (Bool result) to the TargetTagRef's principal port.
             builder.map_output_port(node_id, PortIndex(0), tag_ref_port);
             // Map internal node for the edge connecting the enum value.
             builder.map_internal_node_port(node_id, tag_ref_port);
             Ok(())
         }
         
        // --- Termination --- 
        MirNode::Unreachable => {
            // Lowering Strategy:
            // Represent unreachable code by an infinite loop: two Eraser nodes
            // whose principal ports point to each other.
             let (e1_idx, e1_port) = builder.alloc_eraser(Eraser { principle: Port::NULL });
             builder.config.erasers[e1_idx].principle = e1_port;
             let (e2_idx, e2_port) = builder.alloc_eraser(Eraser { principle: Port::NULL });
             builder.config.erasers[e2_idx].principle = e2_port;
             // Create the redex linking them. This starts the infinite reduction.
             builder.config.initial_redexes.push(Redex(e1_port, e2_port));
             // No output ports to map.
             Ok(())
        }
    }
}

/// Creates the initial Redex representing a connection described by an MIR edge.
/// Adds comments explaining the logic for different target node types.
fn create_redex_for_edge(
    builder: &mut NetBuilder,
    edge: &MirEdge,
) -> Result<(), LoweringError> {

    let source_mir_node = builder.get_mir_node(edge.from_node)?;
    let target_mir_node = builder.get_mir_node(edge.to_node)?;

    // --- Special Handling for Edges *FROM* Project --- 
    // Project nodes themselves aren't created; their logic is embedded here.
    if let MirNode::Project { field_index, .. } = source_mir_node {
        // An edge FROM Project Output TO Consumer Input means:
        // The Consumer should interact with the Aggregate provider (Project Input 0).
        // We create a temporary "Projector" Ref node encoding the field index.
        // Redex 1: ProjectorRef ~ AggregateProvider 
        // Redex 2: Consumer ~ ProjectorRef 
        // Runtime rule `Ref(proj_code) ~ Constructor` extracts the field and links to ProjectorRef aux.
        // Runtime rule `Consumer ~ Ref(proj_code)` links Consumer to ProjectorRef aux (getting the field).
        
        // Find the edge feeding Project input 0 (the aggregate)
        let input_edge = builder.graph.edges.iter().find(|e| e.to_node == edge.from_node && e.to_port == PortIndex(0))
            .ok_or_else(|| LoweringError::InvalidMir(format!("Project node {:?} has no input edge", edge.from_node)))?;
        // Get the port where the aggregate (e.g., a Constructor) is produced.
        let aggregate_provider_port = builder.get_output_net_port(input_edge.from_node, input_edge.from_port)?;

        // Get the port where the projected field is consumed.
        // This is the *target* port of the current MIR edge `edge`.
        let consumer_port = builder.get_output_net_port(edge.to_node, edge.to_port)?;
        // Note: We need to get the *actual* target aux/principal port on the consumer node,
        // so we determine `final_dst_port` for the consumer first.
        let final_consumer_dst_port = determine_target_port(builder, edge.to_node, edge.to_port, target_mir_node, consumer_port)?;

        // Create the intermediate "Projector" Ref node encoding the field index.
        // Use a high bit to distinguish projection Refs from function/static Refs.
        let proj_code = (*field_index as u64) | (1 << 60); 
        let (proj_ref_idx, proj_ref_port) = builder.alloc_ref(Ref {
            principle: Port::NULL, // Consumer will connect here
            data: AtomicU64::new(proj_code),
        });
        builder.config.refs[proj_ref_idx].principle = proj_ref_port;

        // Create Redex 1: Projector Ref ~ Aggregate Provider
        builder.config.initial_redexes.push(Redex(proj_ref_port, aggregate_provider_port));
        
        // Create Redex 2: Consumer ~ Projector Ref (Principal port)
        builder.config.initial_redexes.push(Redex(final_consumer_dst_port, proj_ref_port));

        println!("Project Edge {:?}: Created Projector Ref {:?}. Redexes: ({:?} ~ {:?}), ({:?} ~ {:?})", 
                 edge, proj_ref_port, proj_ref_port, aggregate_provider_port, final_consumer_dst_port, proj_ref_port);

        return Ok(()); // Handled Project source edge
    }

    // --- General Edge Processing --- 

    // Get the source net port (output port of the source MIR node)
    let final_src_port = builder.get_output_net_port(edge.from_node, edge.from_port)?;

    // Determine the final target net port (could be principal or auxiliary)
    // Use a helper function for clarity.
    // Get the initially mapped port for the target node/port index. For aux ports, this is often the principal port.
    let initial_target_port = builder.get_output_net_port(edge.to_node, edge.to_port)
         .or_else(|_| builder.get_internal_node_port(edge.to_node))?; // Try internal map if output fails

    let final_dst_port = determine_target_port(builder, edge.to_node, edge.to_port, target_mir_node, initial_target_port)?;
    
    // Create the redex connecting the source value producer to the target value consumer/port.
    // println!("Creating Redex: {:?} ~ {:?} (From MIR Edge: {:?})", final_src_port, final_dst_port, edge); // Debug
    builder.config.initial_redexes.push(Redex(final_src_port, final_dst_port));

    Ok(())
}

/// Helper function to determine the actual target net port (principal or auxiliary)
/// based on the MIR target node type and input port index.
fn determine_target_port(
    builder: &NetBuilder, 
    target_mir_node_id: NodeId, 
    target_mir_port_index: PortIndex, 
    target_mir_node: &MirNode,
    initial_target_port: Port // The port initially mapped (usually principal)
) -> Result<Port, LoweringError> 
{
    let node_idx = initial_target_port.node_index(); // Index in the corresponding slab
    let partition_id = builder.partition_id;

    match target_mir_node {
        // Target is a Constructor: Inputs map to aux ports.
        MirNode::Constructor { field_types, .. } => {
            let arity = field_types.len();
            if arity <= 2 {
                match target_mir_port_index.0 {
                    0 => Ok(Port::left(NodeType::Constructor, partition_id, node_idx)),
                    1 => Ok(Port::right(NodeType::Constructor, partition_id, node_idx)),
                    _ => Err(LoweringError::InvalidMir(format!("Invalid input port index {:?} for Constructor (arity {})", target_mir_port_index, arity))),
                }
            } else {
                 // Nested Constructors: Cons(f0, Cons(f1, ... Cons(fN-2, fN-1)))
                 // Input `i` connects to the Left Aux port of the i-th internal constructor node.
                 // Input `N-1` connects to the Right Aux port of the (N-2)-th internal constructor node.
                 let target_input_idx = target_mir_port_index.0 as usize;
                 if target_input_idx < arity - 1 { // Inputs 0 to N-2 connect to Left Aux of cons i
                     let internal_cons_port = builder.get_internal_node_port(NodeId(target_mir_node_id.0 * 1000 + target_input_idx as u32))?;
                     Ok(Port::left(NodeType::Constructor, partition_id, internal_cons_port.node_index()))
                 } else if target_input_idx == arity - 1 { // Input N-1 connects to Right Aux of cons N-2
                     let internal_cons_port = builder.get_internal_node_port(NodeId(target_mir_node_id.0 * 1000 + (target_input_idx - 1) as u32))?;
                     Ok(Port::right(NodeType::Constructor, partition_id, internal_cons_port.node_index()))
                 } else {
                     Err(LoweringError::InvalidMir(format!("Invalid input port index {:?} for nested Constructor (arity {})", target_mir_port_index, arity)))
                 }
            }
        }
        // Target is IfValue (Switch): Inputs map to specific ports.
        MirNode::IfValue { .. } => {
             match target_mir_port_index.0 {
                 0 => Ok(Port::principal(NodeType::Switch, partition_id, node_idx)), // Condition -> Principal
                 1 => Ok(Port::left(NodeType::Switch, partition_id, node_idx)),      // True Val -> Left Aux
                 2 => Ok(Port::right(NodeType::Switch, partition_id, node_idx)),     // False Val -> Right Aux
                 _ => Err(LoweringError::InvalidMir(format!("Invalid input port index {:?} for IfValue", target_mir_port_index))),
             }
        }
        // Target is FunctionCall (Duplicator): All inputs connect to principal.
         MirNode::FunctionCall { .. } => {
             // The initial_target_port should be the continuation duplicator's principal port.
             Ok(initial_target_port)
         }
         // Target is BinaryOp (Internal OpApply Constructor): Inputs map to specific ports.
         MirNode::BinaryOp { .. } => {
             // Get the internal OpApply constructor node's principal port (which was stored in internal_node_map)
             let apply_node_port = builder.get_internal_node_port(target_mir_node_id)?;
             let apply_node_idx = apply_node_port.node_index();
             match target_mir_port_index.0 {
                  0 => Ok(Port::right(NodeType::Constructor, partition_id, apply_node_idx)), // LHS -> Right Aux
                  1 => Ok(Port::principal(NodeType::Constructor, partition_id, apply_node_idx)), // RHS -> Principal
                 _ => Err(LoweringError::InvalidMir(format!("Invalid input port index {:?} for BinaryOp", target_mir_port_index))),
             }
         }
         // Target is IsVariant (Internal TargetTag Ref): Input maps to principal.
         MirNode::IsVariant { .. } => {
             // Get the internal TargetTag Ref node's principal port
             let tag_ref_port = builder.get_internal_node_port(target_mir_node_id)?;
             if target_mir_port_index.0 == 0 {
                  Ok(tag_ref_port) // Enum input connects to principal
             } else {
                  Err(LoweringError::InvalidMir(format!("Invalid input port index {:?} for IsVariant", target_mir_port_index)))
             }
         }
         // MirNode::Downcast removed
        // Target is ArrayConstruct (Nested Constructors): Input `i` maps to Left Aux of i-th cons.
        MirNode::ArrayConstruct { size, .. } => {
             if *size > 0 {
                let target_cons_input_idx = target_mir_port_index.0 as usize;
                if target_cons_input_idx < *size { 
                    let internal_cons_port = builder.get_internal_node_port(NodeId(target_mir_node_id.0 * 1000 + target_cons_input_idx as u32))?;
                    Ok(Port::left(NodeType::Constructor, partition_id, internal_cons_port.node_index()))
                } else {
                    Err(LoweringError::InvalidMir(format!("Invalid input port index {:?} for ArrayConstruct (size {})", target_mir_port_index, size)))
                }
             } else {
                 Err(LoweringError::InvalidMir("Edge targets ArrayConstruct with size 0?".into()))
             }
        }
        // Target is ArrayProject (Duplicator): Inputs map to aux ports.
        MirNode::ArrayProject { .. } => {
            // Get the internal Duplicator node's principal port
            let dup_port = builder.get_internal_node_port(target_mir_node_id)?;
            let dup_node_idx = dup_port.node_index();
            match target_mir_port_index.0 {
                0 => Ok(Port::left(NodeType::Duplicator, partition_id, dup_node_idx)), // Array -> Left Aux
                1 => Ok(Port::right(NodeType::Duplicator, partition_id, dup_node_idx)),// Index -> Right Aux
                _ => Err(LoweringError::InvalidMir(format!("Invalid input port index {:?} for ArrayProject", target_mir_port_index))),
            }
        }
        // Target is Closure: Capture inputs map to Left Aux of internal constructors.
        MirNode::Closure { capture_types, .. } => {
             // Input `i` (capture `i`) connects to the Left Aux port of the i-th constructor in capture list.
             let capture_index = target_mir_port_index.0 as usize;
             if capture_index < capture_types.len() { 
                // Retrieve the principal port of the i-th internal constructor node for captures
                let internal_cons_port = builder.get_internal_node_port(NodeId(target_mir_node_id.0 * 1000 + capture_index as u32))?;
                Ok(Port::left(NodeType::Constructor, partition_id, internal_cons_port.node_index()))
             } else {
                 Err(LoweringError::InvalidMir(format!("Invalid input port index {:?} for Closure ({} captures)", target_mir_port_index, capture_types.len())))
             }
        }

        // Default case: Target port is the principal port mapped from the node's output.
        // This applies to Parameter, Constant, StaticAddr, Unreachable (no inputs),
        // and cases where the MIR output port correctly maps to the net interaction port
        // (e.g., consumer of IfValue result interacts with Switch principal).
        _ => Ok(initial_target_port),
    }
} 