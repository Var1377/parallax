//! Mid-level Intermediate Representation (MIR) - Graph Based
//! 
//! This MIR represents functions as directed graphs, suitable for lowering
//! towards interaction net models.

// Allow dead code during development
#![allow(dead_code)]

use cranelift_codegen::ir::Type as CraneliftType;
use repc::layout::TypeLayout;

use parallax_hir::hir::{HirLiteral, ResolvePrimitiveType};
use parallax_resolve::types::Symbol;
use std::collections::HashMap;
use std::sync::Arc;

// --- Core Graph Structures ---

/// Unique identifier for a node within a `MirGraph`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NodeId(pub u32);

/// Index of an input or output port on a `MirNode`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct PortIndex(pub u32);

/// Represents a connection between two node ports.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MirEdge {
    pub from_node: NodeId,
    pub from_port: PortIndex,
    pub to_node: NodeId,
    pub to_port: PortIndex,
}

/// Represents a whole module/crate in MIR.
#[derive(Debug, Clone, PartialEq)]
pub struct MirModule {
    pub name: String,
    /// Functions represented as graphs.
    pub functions: HashMap<Symbol, MirGraph>,
    /// Definitions are needed to interpret constructor tags etc.
    pub structs: Vec<MirStructDef>,
    pub enums: Vec<MirEnumDef>,
    /// Statics might be represented as special nodes or handled separately.
    pub statics: Vec<MirGlobalStatic>,
    pub entry_point: Option<Symbol>,
    // Need a way to manage NodeId uniqueness across graphs?
    // Maybe each graph manages its own IDs starting from 0.
}

/// Represents a single function as a dataflow graph.
#[derive(Debug, Clone, PartialEq)]
pub struct MirGraph {
    pub symbol: Symbol, // Symbol of the function this graph represents
    /// The nodes composing the function body.
    pub nodes: HashMap<NodeId, MirNode>,
    /// Edges defining the data flow between nodes.
    pub edges: Vec<MirEdge>,
    /// Node IDs corresponding to the function parameters (in order).
    pub parameter_nodes: Vec<NodeId>,
    /// The single output port providing the function's return value.
    pub return_port: Option<(NodeId, PortIndex)>,
    /// Counter for allocating new NodeIds within this graph.
    pub next_node_id: u32,
}

// --- Node Types ---

/// Represents a node in the MIR dataflow graph.
/// Each node has a defined set of input and output ports.
#[derive(Debug, Clone, PartialEq)] // Eq may not be possible due to HirLiteral floats
pub enum MirNode {
    /// Represents an input parameter to the function.
    /// Has 1 output port (index 0) of the specified type.
    Parameter { index: u32, ty: MirType },

    /// Represents a literal constant value.
    /// Has 1 output port (index 0) of the specified type.
    Constant { value: HirLiteral, ty: MirType },

    /// Represents the address/identity of a global static variable or function.
    /// Has 1 output port (index 0) of the specified type.
    StaticAddr { symbol: Symbol, ty: MirType },

    /// Constructs an aggregate value (struct, enum variant, tuple).
    /// - `tag`: Symbol identifying the struct, variant, or a special tuple tag.
    /// - `field_types`: Types of the inputs (defines input port count).
    /// - `ty`: The resulting aggregate type (`Adt` or `Tuple`).
    /// Has N input ports (0..N-1) and 1 output port (index 0).
    Constructor {
        tag: Symbol,
        field_types: Vec<MirType>,
        ty: MirType, // Resulting aggregate type
    },

    /// Projects a single field from an aggregate.
    /// - `field_index`: 0-based index of the field to extract.
    /// - `aggregate_ty`: Type of the input aggregate.
    /// - `field_ty`: Type of the extracted field (output).
    /// Has 1 input port (index 0) and 1 output port (index 0).
    Project {
        field_index: u32,
        aggregate_ty: MirType,
        field_ty: MirType,
    },

    /// Calls a function value provided on port 0.
    /// - Input Port 0: Function value (Type: `FunctionPointer(arg_types, ret_type)`).
    /// - Input Port 1..N: Function arguments.
    /// - Output Port 0: Return value.
    /// Stores the expected function signature type for validation/lowering.
    FunctionCall {
        func_ty: MirType, // Must be MirType::FunctionPointer
    },

    /// Conditional switch/multiplexer.
    /// - Input 0: Condition (Boolean)
    /// - Input 1: Value if true
    /// - Input 2: Value if false
    /// Has 3 input ports and 1 output port (0), type `ty`.
    /// Requires inputs 1 and 2 to have the same type `ty`.
    IfValue {
        condition_ty: MirType, // Should be Bool
        ty: MirType,           // Type of inputs 1, 2 and output 0
    },

    /// Constructs an array from its elements.
    /// - Inputs 0..N-1: Element values (must all have `element_ty`).
    /// - Output 0: Array value (Type: `Array(element_ty, N)`).
    ArrayConstruct {
        element_ty: MirType,
        size: usize,
    },

    /// Projects an element from an array using a dynamic index.
    /// - Input 0: Array value (Type: `Array(element_ty, size)`).
    /// - Input 1: Index value (Type: integer, e.g., `Primitive(U64)`).
    /// - Output 0: Element value (Type: `element_ty`).
    ArrayProject {
        array_ty: MirType, // Full array type including size
        index_ty: MirType, // Type of the index operand
        element_ty: MirType,
    },

    /// Constructs a closure environment and provides a pointer to the specialized function.
    /// - Inputs 0..N-1: Captured variable values.
    /// - Output 0: The environment tuple object (Type: `env_ty`).
    /// - Output 1: A function pointer to the specialized closure body (Type: `func_ptr_ty`).
    Closure {
        /// Symbol of the original HIR function for the lambda body.
        original_lambda_symbol: Symbol,
        /// Symbol of the generated specialized MIR function that takes captures as arguments.
        specialized_function_symbol: Symbol,
        /// Types of the captured variables (defines inputs 0..N-1).
        capture_types: Vec<MirType>,
        /// The type of the environment tuple output (Output 0).
        env_ty: MirType, // Should be MirType::Tuple(capture_types)
        /// The type of the specialized function pointer output (Output 1).
        func_ptr_ty: MirType, // Should be MirType::FunctionPointer(env_types + original_param_types, ret_type)
    },

    /// Represents a point where execution cannot continue (e.g., from `!` type).
    /// Has no inputs or outputs in the dataflow sense.
    Unreachable,

    /// Performs a binary operation (e.g., +, -, ==, <).
    /// - Input 0: Left-hand side operand.
    /// - Input 1: Right-hand side operand.
    /// - Output 0: Result of the operation.
    BinaryOp {
        op: MirOp,    // The specific operation to perform.
        lhs_ty: MirType, // Type of the left operand.
        rhs_ty: MirType, // Type of the right operand.
        result_ty: MirType, // Type of the result (e.g., Bool for comparisons).
    },

    /// Checks if an enum value matches a specific variant.
    /// - Input 0: Enum value (Type: Adt(enum_symbol)).
    /// - Output 0: Boolean result (true if variant matches).
    IsVariant {
        enum_symbol: Symbol,
        variant_symbol: Symbol,
    },
}

// --- Binary Operations ---
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MirOp {
    // Arithmetic
    Add, Sub, Mul, Div, Mod,
    // Bitwise
    BitAnd, BitOr, BitXor, Shl, Shr,
    // Comparison
    Eq, Ne, Lt, Le, Gt, Ge,
    // Logical (short-circuiting handled by IfValue)
    // And, Or, // Not typically needed as separate nodes
}

// --- Types, Ops, Defs (Adapted from previous MIR/HIR) ---

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MirType {
    Primitive(ResolvePrimitiveType),
    Adt(Symbol), // Struct or Enum
    Tuple(Vec<MirType>),
    Array(Arc<MirType>, usize), // How to represent in graph?
    FunctionPointer(Vec<MirType>, Arc<MirType>),
    Never,
}

// Definitions now include layout information.
#[derive(Debug, Clone, PartialEq)] // TypeLayout might not be Eq
pub struct MirStructDef {
    pub symbol: Symbol,
    pub name: String,
    pub fields: Vec<(Symbol, String, MirType)>,
    /// Computed layout for the struct.
    pub layout: Arc<TypeLayout>,
}

#[derive(Debug, Clone, PartialEq)] // TypeLayout might not be Eq
pub struct MirEnumDef {
    pub symbol: Symbol,
    pub name: String,
    pub variants: Vec<MirEnumVariant>,
    /// Overall computed layout for the enum.
    pub overall_layout: Arc<TypeLayout>,
    /// The Cranelift integer type used for the discriminant.
    pub discriminant_cl_type: CraneliftType,
    /// Map from Variant Symbol to the offset of its payload in bytes.
    pub variant_payload_offsets: HashMap<Symbol, u64>,
    /// Map from Variant Symbol to the layout of its payload (as a tuple).
    pub variant_payload_layouts: HashMap<Symbol, Arc<TypeLayout>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MirEnumVariant {
    pub symbol: Symbol,
    pub name: String,
    pub fields: Vec<MirType>,
    pub discriminant: u64,
}

// Statics need representation, maybe as Constant nodes?
#[derive(Debug, Clone, PartialEq)]
pub struct MirGlobalStatic {
    pub symbol: Symbol,
    pub name: String,
    pub ty: MirType,
    // Initializer needs to be a constant representable by MirNode::Constant
    pub initializer: Option<HirLiteral>, // Simplified
    pub is_mutable: bool, // How to handle mutability in graph?
}

// --- Helper Methods ---

impl MirGraph {
    pub fn new(symbol: Symbol) -> Self {
        MirGraph {
            symbol,
            nodes: HashMap::new(),
            edges: Vec::new(),
            parameter_nodes: Vec::new(),
            return_port: None,
            next_node_id: 0,
        }
    }

    pub fn add_node(&mut self, node: MirNode) -> NodeId {
        let id = NodeId(self.next_node_id);
        self.next_node_id += 1;
        self.nodes.insert(id, node);
        id
    }

    pub fn add_edge(&mut self, from_node: NodeId, from_port: PortIndex, to_node: NodeId, to_port: PortIndex) {
        self.edges.push(MirEdge { from_node, from_port, to_node, to_port });
    }
}



