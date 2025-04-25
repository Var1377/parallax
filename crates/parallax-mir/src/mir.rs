//! # Mid-level Intermediate Representation (MIR) - Graph Based
//!
//! This module defines the Mid-level Intermediate Representation (MIR) used in the Parallax compiler.
//! MIR serves as a bridge between the High-Level Intermediate Representation (HIR) and lower-level
//! representations closer to the target machine or execution model (like interaction nets).
//!
//! ## Core Design
//!
//! The MIR represents functions as **directed dataflow graphs**. Nodes in the graph represent
//! operations or values (like constants, parameters), and edges represent the flow of data
//! between these operations. This graph structure is designed to be suitable for analysis,
//! optimization, and eventual lowering to execution models that emphasize data dependencies,
//! such as interaction nets.
//!
//! ## Key Components
//!
//! *   [`MirModule`]: Represents an entire compiled crate or module, containing function graphs,
//!     type definitions (structs, enums), and static variable definitions.
//! *   [`MirGraph`]: Represents a single function's body as a dataflow graph. Contains nodes
//!     ([`MirNode`]), edges ([`MirEdge`]), and references to the function's parameters and
//!     return value source.
//! *   [`MirNode`]: An enum defining the different types of operations or values that can exist
//!     in the graph (e.g., `Constant`, `FunctionCall`, `BinaryOp`, `IfValue`). Each node variant
//!     has specific input and output ports.
//! *   [`MirEdge`]: Defines a connection between an output port of one node and an input port
//!     of another node.
//! *   [`MirType`]: Represents types within the MIR, mirroring HIR types but potentially
//!     simplified or canonicalized.
//! *   [`MirStructDef`], [`MirEnumDef`], [`MirGlobalStatic`]: Definitions for user-defined types
//!     and global variables, including layout information computed during lowering.
//! *   [`NodeId`], [`PortIndex`]: Identifiers for nodes and their ports within a graph.

// Allow dead code during development
#![allow(dead_code)]

use cranelift_codegen::ir::Type as CraneliftType;
use repc::layout::TypeLayout; // Use translator path suggested by linter

use parallax_hir::hir::HirLiteral;
use parallax_resolve::types::{PrimitiveType as ResolvePrimitiveType, Symbol};
use std::collections::HashMap;
use std::sync::Arc;
// Add imports for GC layout types
use parallax_gc::{DescriptorIndex, DescriptorStore};

// --- Core Graph Structures ---

/// Unique identifier for a node within a specific [`MirGraph`].
/// NodeIds are local to the graph they belong to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NodeId(pub u32);

/// Index of an input or output port on a [`MirNode`].
/// Ports are indexed starting from 0. The meaning of each index depends on the `MirNode` variant.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct PortIndex(pub u32);

/// Represents a directed edge connecting an output port of one node to an input port of another node.
/// This defines the flow of data within the [`MirGraph`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MirEdge {
    /// The node providing the data.
    pub from_node: NodeId,
    /// The specific output port on the `from_node`.
    pub from_port: PortIndex,
    /// The node receiving the data.
    pub to_node: NodeId,
    /// The specific input port on the `to_node`.
    pub to_port: PortIndex,
}

/// Represents a complete Parallax module/crate translated into MIR.
///
/// Contains all the necessary information generated during the lowering phase,
/// including function graphs, type definitions with layout, and statics.
#[derive(Debug, Clone)]
pub struct MirModule {
    /// The name of the original module.
    pub name: String,
    /// A map from function [`Symbol`]s to their corresponding [`MirGraph`] representations.
    /// Includes both regular functions and specialized closure functions.
    pub functions: HashMap<Symbol, MirGraph>,
    /// Definitions of all structs in the module.
    pub structs: Vec<MirStructDef>,
    /// Definitions of all enums in the module.
    pub enums: Vec<MirEnumDef>,
    /// Definitions of all global static variables.
    pub statics: Vec<MirGlobalStatic>,
    /// The [`Symbol`] of the function designated as the entry point, if any.
    pub entry_point: Option<Symbol>,
    /// List of (full_path, symbol) pairs for intrinsic functions found in stdlib.
    pub intrinsics: Vec<(String, Symbol)>,
    /// The collection of layout descriptors for all types in the module.
    /// Owned by the MirModule and used by later stages (e.g., codegen).
    pub descriptor_store: DescriptorStore,
    // Note: NodeId/PortIndex uniqueness is managed within each MirGraph.
    // There's no global ID space across graphs in the MirModule itself.
}

/// Represents a single function (or specialized closure) as a dataflow graph.
///
/// The graph consists of nodes ([`MirNode`]) connected by edges ([`MirEdge`]).
/// It explicitly identifies the node providing the function's parameters and the
/// port providing the return value.
#[derive(Debug, Clone, PartialEq)]
pub struct MirGraph {
    /// The unique [`Symbol`] identifying this function or specialized closure.
    pub symbol: Symbol,
    /// The collection of all nodes within this function's graph, indexed by their [`NodeId`].
    pub nodes: HashMap<NodeId, MirNode>,
    /// The list of all edges connecting nodes within this graph.
    pub edges: Vec<MirEdge>,
    /// The [`NodeId`] of the single `MirNode::Parameter` that represents the function's
    /// input parameters (potentially aggregated into a tuple). This is expected to be `Some`
    /// after lowering is complete for functions with bodies.
    pub parameter_node: Option<NodeId>,
    /// The specific node and output port (`NodeId`, `PortIndex`) that produces the
    /// function's return value. This is expected to be `Some` after lowering is complete
    /// for functions that return a value.
    pub return_port: Option<(NodeId, PortIndex)>,
    /// A counter used to allocate unique [`NodeId`]s within this graph instance.
    next_node_id: u32,
}

// --- Node Types ---

/// Defines the different kinds of operations or values represented by nodes in a [`MirGraph`].
///
/// Each variant specifies its expected input and output ports.
///
/// # Port Conventions:
/// *   **Inputs:** Data flows *into* the node through input ports.
/// *   **Outputs:** Data flows *out of* the node through output ports.
/// *   **Indexing:** Ports are typically indexed starting from 0.
#[derive(Debug, Clone, PartialEq)] // Eq may not be possible due to HirLiteral floats
pub enum MirNode {
    /// Represents the source of an input parameter to the function graph.
    /// In the current lowering strategy, there's typically one aggregate `Parameter` node
    /// representing all inputs (including captures) packed into a tuple.
    /// Projections (`MirNode::Project`) are then used to extract individual parameters.
    ///
    /// *   **Outputs:**
    ///     *   Port 0: The parameter value (potentially an aggregate type).
    /// *   **Inputs:** None.
    Parameter { index: u32, ty: MirType },

    /// Represents a literal constant value (integer, float, string, bool, char, unit).
    ///
    /// *   **Outputs:**
    ///     *   Port 0: The constant value.
    /// *   **Inputs:** None.
    Constant { value: HirLiteral, ty: MirType },

    /// Represents the identity or address of a global item (function or static variable).
    /// This node provides the "value" that can be called (for functions) or loaded from/stored to.
    ///
    /// *   **Outputs:**
    ///     *   Port 0: The global item's identity (often a `FunctionPointer` or the static's type).
    /// *   **Inputs:** None.
    StaticAddr { symbol: Symbol, ty: MirType },

    /// Constructs an aggregate value (struct, enum variant payload, or tuple).
    /// Takes individual fields as inputs and produces the combined aggregate value.
    ///
    /// *   **Outputs:**
    ///     *   Port 0: The constructed aggregate value (`ty`).
    /// *   **Inputs:**
    ///     *   Port `i` (0 <= `i` < N): The value for the `i`-th field (`field_types[i]`).
    Constructor {
        /// For structs/variants: the [`Symbol`] of the struct or variant.
        /// For tuples: a special tag (often Symbol(0)).
        tag: Symbol,
        /// The types of the input fields, determining the number of input ports.
        field_types: Vec<MirType>,
        /// The resulting aggregate type ([`MirType::Adt`] or [`MirType::Tuple`]).
        ty: MirType,
    },

    /// Extracts a single field from an aggregate value (struct or tuple).
    ///
    /// *   **Outputs:**
    ///     *   Port 0: The extracted field value (`field_ty`).
    /// *   **Inputs:**
    ///     *   Port 0: The aggregate value (`aggregate_ty`).
    Project {
        /// 0-based index of the field to extract within the aggregate.
        field_index: u32,
        /// The type of the input aggregate value.
        aggregate_ty: MirType,
        /// The type of the extracted field (and the output).
        field_ty: MirType,
    },

    /// Calls a function value. The function to be called is provided dynamically as an input.
    /// Handles both regular function calls and closure calls (where the environment is passed).
    ///
    /// *   **Outputs:**
    ///     *   Port 0: The return value of the function.
    /// *   **Inputs:**
    ///     *   Port 0: The function value to call (must have type `MirType::FunctionPointer`).
    ///     *   Port 1..N: The arguments to pass to the function. The number and types
    ///         of arguments depend on `func_ty`. For closure calls, Input 1 is typically
    ///         the closure environment, and Inputs 2..N are the original arguments.
    FunctionCall {
        /// The expected type signature of the function being called. Used for validation
        /// and determining input/output port types.
        func_ty: MirType, // Must be MirType::FunctionPointer
    },

    /// A conditional value selector (multiplexer). Selects one of two input values
    /// based on a boolean condition.
    ///
    /// *   **Outputs:**
    ///     *   Port 0: The selected value (`ty`).
    /// *   **Inputs:**
    ///     *   Port 0: The boolean condition value (`condition_ty`, expected `Bool`).
    ///     *   Port 1: The value to select if the condition is true (`ty`).
    ///     *   Port 2: The value to select if the condition is false (`ty`).
    IfValue {
        /// The type of the condition input (expected `MirType::Primitive(Bool)`).
        condition_ty: MirType,
        /// The type of the two value inputs (Port 1, Port 2) and the output port (Port 0).
        /// Inputs 1 and 2 must have the same type.
        ty: MirType,
    },

    /// Constructs an array value from a fixed number of element values.
    ///
    /// *   **Outputs:**
    ///     *   Port 0: The constructed array value (`MirType::Array(element_ty, size)`).
    /// *   **Inputs:**
    ///     *   Port `i` (0 <= `i` < `size`): The value for the `i`-th element (`element_ty`).
    ArrayConstruct {
        /// The type of each element in the array. All inputs must have this type.
        element_ty: MirType,
        /// The fixed size of the array, determining the number of input ports.
        size: usize,
    },

    /// Extracts a single element from an array using a dynamically computed index.
    ///
    /// *   **Outputs:**
    ///     *   Port 0: The extracted element value (`element_ty`).
    /// *   **Inputs:**
    ///     *   Port 0: The array value (`array_ty`).
    ///     *   Port 1: The index value (`index_ty`, expected integer).
    ArrayProject {
        /// The type of the input array, including its size.
        array_ty: MirType, // e.g., MirType::Array(MirType::Primitive(I32), 10)
        /// The type of the index input (must be an integer primitive type).
        index_ty: MirType,
        /// The type of the elements within the array (and the output type).
        element_ty: MirType,
    },

    /// Represents the creation site of a closure.
    /// It packages captured variables into an environment tuple and provides a
    /// function pointer to the specialized MIR graph generated for the closure's body.
    ///
    /// *   **Outputs:**
    ///     *   Port 0: The environment tuple containing captured values (`env_ty`).
    ///     *   Port 1: A function pointer to the specialized closure function (`func_ptr_ty`).
    /// *   **Inputs:**
    ///     *   Port `i` (0 <= `i` < N): The value of the `i`-th captured variable (`capture_types[i]`).
    Closure {
        /// [`Symbol`] of the original HIR function definition for the lambda's body.
        original_lambda_symbol: Symbol,
        /// [`Symbol`] of the *new* [`MirGraph`] generated specifically for this closure,
        /// which takes captures as extra leading arguments.
        specialized_function_symbol: Symbol,
        /// The types of the captured variables, defining inputs 0..N-1.
        capture_types: Vec<MirType>,
        /// The type of the environment tuple produced at Output 0 (should be `MirType::Tuple(capture_types)`).
        env_ty: MirType,
        /// The type of the function pointer produced at Output 1. This pointer refers to the
        /// `specialized_function_symbol`'s graph. Its signature includes the capture types
        /// followed by the original lambda parameters.
        func_ptr_ty: MirType, // e.g., FunctionPointer([cap1_ty, cap2_ty, orig_param1_ty], ret_ty)
    },

    /// Represents a point in the dataflow graph from which execution cannot proceed
    /// (e.g., corresponds to the `!` "never" type, or dead code).
    /// It has no defined dataflow inputs or outputs, though it might have control-flow implications
    /// in other MIR forms.
    Unreachable,

    /// Dispatches on the tag of an enum value, routing to the correct arm subgraph.
    ///
    /// *   **Inputs:**
    ///     *   Port 0: The enum value to match (must be `MirType::Adt(enum_symbol)`).
    /// *   **Outputs:**
    ///     *   Port 0: The result of the selected arm.
    ///
    /// The `arms` map associates variant symbols to the (NodeId, PortIndex) of the root of each arm's subgraph.
    /// The `otherwise` port is used for wildcard, const, or default arms.
    MatchDispatch {
        /// The enum type being matched on.
        enum_symbol: Symbol,
        /// Mapping from variant symbol to (NodeId, PortIndex) for each arm.
        arms: std::collections::HashMap<Symbol, (NodeId, PortIndex)>,
        /// The (NodeId, PortIndex) for the wildcard/default/otherwise arm, if any.
        otherwise: Option<(NodeId, PortIndex)>,
    },
}

// --- Types, Ops, Defs (Adapted from previous MIR/HIR) ---

/// Represents types within the MIR system. Mirrors `HirType` closely.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MirType {
    /// Basic built-in types (e.g., `i32`, `f64`, `bool`).
    Primitive(ResolvePrimitiveType),
    /// User-defined aggregate types (structs or enums), identified by their [`Symbol`].
    Adt(Symbol),
    /// Anonymous product types.
    Tuple(Vec<MirType>),
    /// Fixed-size arrays. Uses `Arc` for potentially shared element types.
    Array(Arc<MirType>, usize),
    /// Types representing functions. Includes parameter types and return type.
    FunctionPointer(Vec<MirType>, Arc<MirType>),
    /// The bottom type (`!`), indicating diverging computations.
    Never,
}

// --- Helper for Never Type ---
impl MirType {
    /// Checks if this type is the `Never` type.
    pub fn is_never(&self) -> bool {
        matches!(self, MirType::Never)
    }
}


// --- MIR Definitions with Layout ---

/// MIR definition of a struct, referencing its layout via `DescriptorIndex`.
#[derive(Debug, Clone, PartialEq)]
pub struct MirStructDef {
    pub symbol: Symbol,
    pub name: String,
    /// Fields represented as (Symbol, Name, Type).
    pub fields: Vec<(Symbol, String, MirType)>,
    /// Index into the `MirModule`'s `descriptor_store` for this struct's layout.
    pub descriptor_index: DescriptorIndex,
}

/// MIR definition of an enum, referencing its layout via `DescriptorIndex`.
#[derive(Debug, Clone, PartialEq)]
pub struct MirEnumDef {
    pub symbol: Symbol,
    pub name: String,
    /// Definitions of each variant within the enum.
    pub variants: Vec<MirEnumVariant>,
    /// Index into the `MirModule`'s `descriptor_store` for this enum's layout.
    pub descriptor_index: DescriptorIndex,
}

/// MIR definition of a single enum variant.
#[derive(Debug, Clone, PartialEq)]
pub struct MirEnumVariant {
    pub symbol: Symbol,
    pub name: String,
    /// The types of the fields contained within this variant's payload.
    pub fields: Vec<MirType>,
    /// The unique numerical discriminant value assigned to this variant.
    pub discriminant: u64,
}

/// MIR definition of a global static variable.
#[derive(Debug, Clone, PartialEq)]
pub struct MirGlobalStatic {
    pub symbol: Symbol,
    pub name: String,
    pub ty: MirType,
    /// The initial value, if it's a simple constant representable by [`HirLiteral`].
    /// More complex initializers might require dedicated initializer functions.
    pub initializer: Option<HirLiteral>, // Simplified
    /// Indicates if the static variable can be modified after initialization.
    /// Handling mutability might require specific node types or analysis passes later.
    pub is_mutable: bool,
}

// --- Helper Methods ---

impl MirGraph {
    /// Creates a new, empty `MirGraph` associated with the given function [`Symbol`].
    pub fn new(symbol: Symbol) -> Self {
        MirGraph {
            symbol,
            nodes: HashMap::new(),
            edges: Vec::new(),
            parameter_node: None,
            return_port: None,
            next_node_id: 0,
        }
    }

    /// Adds a [`MirNode`] to the graph, assigning it a new unique [`NodeId`].
    /// Returns the [`NodeId`] assigned to the added node.
    pub fn add_node(&mut self, node: MirNode) -> NodeId {
        let id = NodeId(self.next_node_id);
        self.next_node_id += 1;
        // Check for potential overflow if functions become extremely large
        assert!(self.next_node_id < u32::MAX, "MirGraph NodeId overflow for {:?}", self.symbol);
        self.nodes.insert(id, node);
        id
    }

    /// Adds a [`MirEdge`] connecting two nodes within the graph.
    /// It's the caller's responsibility to ensure nodes and ports exist and types match.
    pub fn add_edge(&mut self, from_node: NodeId, from_port: PortIndex, to_node: NodeId, to_port: PortIndex) {
        // Potential validation: Check if nodes exist? Check type compatibility?
        // For now, assume valid edges are added during lowering.
        self.edges.push(MirEdge { from_node, from_port, to_node, to_port });
    }

    /// Adds the single aggregate parameter node to the graph.
    ///
    /// This is typically called once during the initial setup of function lowering.
    /// It creates a `MirNode::Parameter` representing the source of all function inputs.
    ///
    /// # Panics
    /// Panics if `parameter_node` is already `Some`, preventing multiple parameter nodes.
    pub fn add_parameter_node(&mut self, ty: MirType) -> NodeId {
        assert!(self.parameter_node.is_none(), "Attempted to add multiple parameter nodes to MIR graph for {:?}", self.symbol);
        // In the current design, the single parameter node always has index 0
        let node = MirNode::Parameter { index: 0, ty };
        let node_id = self.add_node(node);
        self.parameter_node = Some(node_id);
        node_id
    }

    /// Sets the return port for the graph.
    ///
    /// This specifies which node and output port provide the final return value of the function.
    /// Should be called once when the lowering of the function body is complete.
    ///
    /// # Panics
    /// Panics if `return_port` is already `Some`.
    pub fn set_return_port(&mut self, node_id: NodeId, port_index: PortIndex) {
         assert!(self.return_port.is_none(), "Attempted to set multiple return ports for MIR graph for {:?}", self.symbol);
         // Potential validation: Check if node_id exists and has the specified port?
         self.return_port = Some((node_id, port_index));
    }
}



