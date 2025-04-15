use parallax_resolve::types::Symbol;
pub use parallax_resolve::types::PrimitiveType as ResolvePrimitiveType;
use miette::SourceSpan;
use std::sync::Arc;

// --- Core HIR Structures (ANF-based) ---

/// Represents a whole module/crate in HIR.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirModule {
    pub name: String,
    pub functions: Vec<HirFunction>,
    pub structs: Vec<HirStructDef>,
    pub enums: Vec<HirEnumDef>,
    /// Global static variables defined in the module.
    pub statics: Vec<HirGlobalStatic>,
    /// The symbol for the main entry point function, if found.
    pub entry_point: Option<Symbol>,
    /// The next available HirVar ID after initial lowering.
    /// Optimization passes should start generating IDs from this value.
    pub next_var_id: u32,
}

/// Unique identifier for a bound variable in the ANF HIR.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HirVar(pub u32);

/// Represents a function in HIR.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirFunction {
    pub symbol: Symbol,
    pub name: String,
    pub signature: HirFunctionSignature,
    /// Body is now a single ANF expression. None for extern functions.
    pub body: Option<HirExpr>,
    pub span: SourceSpan,
}

/// Represents the signature of a function in HIR.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirFunctionSignature {
    /// Parameters are now named variables with types.
    pub params: Vec<(HirVar, HirType)>,
    pub return_type: HirType,
    pub is_effectful: bool, // Tracking effects might still be relevant
}

/// Represents a struct definition in HIR (mostly unchanged).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirStructDef {
    pub symbol: Symbol,
    pub name: String,
    pub fields: Vec<(Symbol, String, HirType)>,
    pub span: SourceSpan,
    // Add generic params if needed later
}

/// Represents an enum definition in HIR (mostly unchanged).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirEnumDef {
    pub symbol: Symbol,
    pub name: String,
    pub variants: Vec<HirEnumVariant>,
    pub span: SourceSpan,
    // Add generic params if needed later
}

/// Represents a variant within an enum definition in HIR (mostly unchanged).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirEnumVariant {
    pub symbol: Symbol, // Symbol for the variant constructor/tag
    pub name: String,
    pub fields: Vec<HirType>, // Types of fields it contains
    pub span: SourceSpan,
}

/// Represents a global static variable definition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirGlobalStatic {
    pub symbol: Symbol,
    pub name: String,
    pub ty: HirType,
    /// Optional initializer. Translating complex initializers is non-trivial.
    /// Start with None or support only simple literals initially.
    pub initializer: Option<HirValue>, // Using HirValue for simplicity, adjust as needed
    pub is_mutable: bool,
    pub span: SourceSpan,
}

// --- ANF Expression Representation ---

/// An operand: a simple variable or a literal constant, or a global symbol.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operand {
    Var(HirVar),
    Const(HirLiteral),
    Global(Symbol), // For referring to top-level functions/constants
}

/// Represents an expression in A-Normal Form (ANF).
/// Expressions have a type and a source span.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirExpr {
    pub kind: HirExprKind,
    pub ty: HirType,
    pub span: SourceSpan,
}

/// Kinds of expressions in ANF HIR.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirExprKind {
    /// Bind the result of a computation to a variable: `let var = value in rest`
    Let {
        var: HirVar,
        var_ty: HirType, // Type of the variable being bound
        value: Box<HirValue>, // The computation being bound
        rest: Box<HirExpr>,   // The expression where 'var' is in scope
    },
    /// An expression whose value is directly used (must be the tail).
    /// This represents the final result or a control flow construct.
    Tail(HirTailExpr),
}

/// Represents computations that can be bound in a `Let`. These computations
/// take simple operands and produce a single result value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirValue {
    /// Use an existing variable or constant.
    Use(Operand),
    /// Call a function (which must be an Operand resolving to a function).
    Call {
        func: Operand,
        args: Vec<Operand>,
    },
    /// Construct an aggregate value (tuple, struct, enum variant).
    Aggregate {
        kind: AggregateKind,
        fields: Vec<Operand>,
    },
    /// Project a field/element from an aggregate.
    Project {
        base: Operand, // The aggregate operand
        projection: ProjectionKind, // What kind of projection
    },
    /// Create a closure object.
    Closure {
        /// Symbol of the function implementing the lambda's body.
        function_symbol: Symbol,
        /// List of operands representing the captured variables' values.
        captures: Vec<Operand>,
    },
}

/// Represents expressions that can appear in "tail position" (end of a function or block).
/// These often involve control flow or are the final value computation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirTailExpr {
    /// Return a simple value (variable or constant).
    Return(Operand),
    /// Conditional branch. Both branches must eventually yield a value (often via Return).
    If {
        condition: Operand, // Must be boolean
        then_branch: Box<HirExpr>,
        else_branch: Box<HirExpr>,
    },
    /// Match/Switch on a value.
    Match {
        scrutinee: Operand,
        arms: Vec<(HirPattern, HirExpr)>,
        /// Fallback branch if no pattern matches. Lowering should ensure exhaustiveness
        /// or explicitly handle non-exhaustive cases (e.g., panic or return default).
        otherwise: Option<Box<HirExpr>>,
    },
    /// A divergent expression (e.g., a call to a function that never returns).
    Never,
}

// --- Supporting Enums and Structs ---

/// Represents the kind of projection from an aggregate.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProjectionKind {
    Field(Symbol),      // Project struct field (using field's Symbol)
    TupleIndex(u32),    // Project tuple element index
    ArrayIndex(Operand), // Project array element (index must be operand)
    /// Cast/check enum variant tag *before* projecting fields.
    Downcast(Symbol), // Check if value matches variant `Symbol`
}

/// Pattern for Match arms.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirPattern {
    /// Match a specific enum variant and bind its fields.
    Variant {
        variant_symbol: Symbol,
        bindings: Vec<(HirVar, HirType)>, // Variables to bind the variant's fields to, with types
    },
    /// Match a literal constant.
    Const(HirLiteral),
    /// Bind the scrutinee to a variable (useful for `x @ Pattern` or default cases).
    Bind {
        var: HirVar,
        var_ty: HirType,
    },
    /// Wildcard, matches anything without binding.
    Wildcard,
    // TODO: Add other patterns like Tuple, Struct if needed for match destructuring
}

/// Represents an aggregate type kind for HirValue::Aggregate.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AggregateKind {
    Tuple,
    Array,
    Struct(Symbol), // Symbol of the struct definition
    EnumVariant(Symbol), // Symbol of the specific enum variant constructor
}

/// Represents a literal value in HIR (mostly unchanged).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirLiteral {
    Int(i64),
    Float(u64), // Store as bits representation of f64
    String(String),
    Bool(bool),
    Char(char),
    Unit,
}

/// Represents a type in HIR (mostly unchanged).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HirType {
    Primitive(ResolvePrimitiveType),
    Adt(Symbol), // Abstract Data Type (struct or enum), identified by its definition Symbol
    Tuple(Vec<HirType>),
    Array(Arc<HirType>, usize),
    FunctionPointer(Vec<HirType>, Arc<HirType>), // Type of a function itself
    Never, // The ! type
}

impl HirType {
    /// Check if the type is the never type `!`.
    pub fn is_never(&self) -> bool {
        matches!(self, HirType::Never)
    }
}