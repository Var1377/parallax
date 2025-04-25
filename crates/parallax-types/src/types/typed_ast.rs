use std::collections::HashMap;
use miette::SourceSpan;
use parallax_resolve::types::Symbol;

// Removed outdated TODO comment
use crate::types::core::Ty; // Removed TyKind, TypeId
use crate::types::definitions::{TypedStruct, TypedEnum, TypedFunction};
use crate::context::trait_repo::TraitRepository; // Updated path

// --- Typed Expressions and Patterns ---

/// Represents a fully typed expression
#[derive(Debug, Clone)]
pub struct TypedExpr {
    pub kind: TypedExprKind,
    pub ty: Ty,
    pub span: SourceSpan,
}

/// Kind of a typed expression
#[derive(Debug, Clone)]
pub enum TypedExprKind {
    Block(Vec<TypedExpr>),
    If {
        condition: Box<TypedExpr>,
        then_branch: Box<TypedExpr>,
        else_branch: Option<Box<TypedExpr>>,
    },
    // TODO: Add Match arm later
    Call {
        // TODO: Represent the called function/method info
        func: Box<TypedExpr>, // Expression that evaluates to a function
        args: Vec<TypedArgument>,
    },
    // TODO: Add Lambda later
    Lambda {
        params: Vec<(Symbol, Ty)>,
        body: Box<TypedExpr>,
    },
    // Updated Literal variant
    IntLiteral {
        value: i128,
        suffix: Option<String>,
    },
    FloatLiteral {
        value: f64,
        suffix: Option<String>,
    },
    StringLiteral(String),
    CharLiteral(char),
    BoolLiteral(bool),
    Variable {
        /// The resolved symbol for the variable binding (parameter or let).
        symbol: Symbol,
        /// Original name (for debugging/errors).
        name: String,
    },
    Field {
        object: Box<TypedExpr>,
        /// The resolved symbol for the field
        field_symbol: Symbol,
        /// Original field name (for debugging/errors)
        field_name: String, 
    },
    Array(Vec<TypedExpr>),
    Tuple(Vec<TypedExpr>),
    // TODO: Add Map, HashSet later
    Let {
        pattern: TypedPattern,
        value: Box<TypedExpr>,
    },
    Struct {
        name: String,
        fields: Vec<(String, TypedExpr)>,
        base: Option<Box<TypedExpr>>,
    },
    Paren(Box<TypedExpr>),
    // TODO: Add VariantConstructor later
    VariantConstructor {
        enum_name: String,
        variant_name: String,
        args: Vec<TypedArgument>,
    },
    Match {
        scrutinee: Box<TypedExpr>,
        arms: Vec<TypedMatchArm>,
    },
    Error,
    // Logical operators (short-circuiting)
    LogicalAnd {
        left: Box<TypedExpr>,
        right: Box<TypedExpr>,
    },
    LogicalOr {
        left: Box<TypedExpr>,
        right: Box<TypedExpr>,
    },
    /// Map literal expression
    Map(Vec<(TypedExpr, TypedExpr)>),
    /// HashSet literal expression
    HashSet(Vec<TypedExpr>),
}

/// Represents a typed argument in a function call
#[derive(Debug, Clone)]
pub struct TypedArgument {
    pub name: Option<String>,
    pub value: TypedExpr,
    pub span: SourceSpan,
}

// TODO: Define TypedPattern later 

// --- Typed Pattern Definitions ---

/// Represents a fully typed pattern
#[derive(Debug, Clone)]
pub struct TypedPattern {
    pub kind: TypedPatternKind,
    pub span: SourceSpan,
    pub ty: Ty, // The type this pattern matches
}

/// Kind of a typed pattern. Mirrors ResolvedPatternKind.
#[derive(Debug, Clone)]
pub enum TypedPatternKind {
    Identifier {
        /// The resolved symbol for the identifier
        symbol: Symbol,
        /// Original name (for debugging/errors)
        name: String,
    },
    Literal(parallax_syntax::ast::common::Literal),
    Constructor { 
        // Use resolved names from type checking 
        enum_name: String, 
        variant_name: String,
        // Store resolved symbol if available?
        // variant_symbol: Symbol, 
        args: Box<TypedPattern> 
    },
    Array(Vec<TypedPattern>),
    Tuple(Vec<TypedPattern>),
    Struct { 
        // Use resolved name from type checking
        struct_name: String, 
        // Store resolved symbol if available?
        // struct_symbol: Symbol,
        fields: Vec<TypedPatternField> 
    },
    Rest,
    Or(Box<TypedPattern>, Box<TypedPattern>),
    Wildcard,
}

/// Represents a field within a typed struct pattern
#[derive(Debug, Clone)]
pub struct TypedPatternField {
    pub name: String,
    pub pattern: Option<TypedPattern>,
    pub span: SourceSpan,
}

// --- Typed Statements --- 

/// Represents a fully typed statement
#[derive(Debug, Clone)]
pub struct TypedStatement {
    pub kind: TypedStatementKind,
    pub span: SourceSpan,
}

/// Kind of a typed statement
#[derive(Debug, Clone)]
pub enum TypedStatementKind {
    /// An expression used as a statement (result is discarded).
    Expression(TypedExpr),
    // /// A return statement.
    // Return(Option<TypedExpr>), // Option for `return;` vs `return value;`
    // TODO: Add Let statement if needed later (currently handled in Block Expr)
    // Let { name: String, value: TypedExpr }, 
}

/// Represents a fully typed match arm
#[derive(Debug, Clone)]
pub struct TypedMatchArm {
    pub pattern: TypedPattern,
    pub body: TypedExpr,
}

/// A collection of all typed definitions in a module
#[derive(Debug, Clone, Default)]
pub struct TypedDefinitions {
    /// All typed structs (Using Symbol as key)
    pub structs: HashMap<Symbol, TypedStruct>,
    /// All typed enums (Using Symbol as key)
    pub enums: HashMap<Symbol, TypedEnum>,
    /// All typed functions (Using Symbol as key)
    pub functions: HashMap<Symbol, TypedFunction>,
}

impl TypedDefinitions {
    // Add method to add a struct
    pub fn add_struct(&mut self, symbol: Symbol, def: TypedStruct) {
        self.structs.insert(symbol, def);
    }

    // Add method to add an enum
    pub fn add_enum(&mut self, symbol: Symbol, def: TypedEnum) {
        self.enums.insert(symbol, def);
    }

    // Add method to add a function
    pub fn add_function(&mut self, symbol: Symbol, def: TypedFunction) {
        self.functions.insert(symbol, def);
    }
}

/// A complete typed module containing all definitions and errors
#[derive(Debug, Clone)]
pub struct TypedModule {
    /// All typed definitions in the module
    pub definitions: TypedDefinitions,
    /// Repository of traits and implementations
    pub trait_repo: TraitRepository,
    /// The symbol for the main entry point function (e.g., `main`), if found.
    pub entry_point: Option<Symbol>,
    /// List of (full_path, symbol) pairs for intrinsic functions found in stdlib.
    pub intrinsics: Vec<(String, Symbol)>,
    /// Any type errors that occurred during checking
    pub errors: Vec<String>,
} 