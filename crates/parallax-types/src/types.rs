use std::collections::{HashMap, HashSet};
use std::fmt;
use std::sync::Arc;

use miette::SourceSpan;
use serde::{Deserialize, Serialize};
use parallax_resolve::types::Symbol;
use crate::traits::{TraitId, ImplId};

/// A unique identifier for a type variable
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TypeId(pub u32);

impl fmt::Display for TypeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "t{}", self.0)
    }
}

/// A type in the Parallax language
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ty {
    /// The kind of type
    pub kind: TyKind,
    /// Source span of the type in the original code
    pub span: Option<SourceSpan>,
}

impl Ty {
    /// Create a new type with no source span
    pub fn new(kind: TyKind) -> Self {
        Self { kind, span: None }
    }

    /// Create a new type with the given source span
    pub fn with_span(kind: TyKind, span: SourceSpan) -> Self {
        Self { kind, span: Some(span) }
    }
    
    /// Returns true if this type is a type variable
    pub fn is_var(&self) -> bool {
        matches!(self.kind, TyKind::Var(_))
    }
    
    /// Returns true if this type contains no type variables
    pub fn is_concrete(&self) -> bool {
        self.free_vars().is_empty()
    }

    /// Apply a substitution to this type, replacing type variables
    pub fn apply_subst(&self, subst: &crate::inference::Substitution) -> Self {
        match &self.kind {
            TyKind::Var(id) => {
                if let Some(ty) = subst.get(id) {
                    let mut res = ty.apply_subst(subst);
                    if res.span.is_none() {
                        res.span = self.span;
                    }
                    res
                } else {
                    self.clone()
                }
            }
            TyKind::Primitive(_) => self.clone(),
            TyKind::Named { name, args } => {
                let new_args = args.iter().map(|arg| arg.apply_subst(subst)).collect();
                Ty {
                    kind: TyKind::Named { name: name.clone(), args: new_args },
                    span: self.span,
                }
            }
            TyKind::Array(elem_ty, size) => {
                let new_elem = elem_ty.apply_subst(subst);
                Ty {
                    kind: TyKind::Array(Arc::new(new_elem), *size),
                    span: self.span,
                }
            }
            TyKind::Tuple(tys) => {
                let new_tys = tys.iter().map(|ty| ty.apply_subst(subst)).collect();
                Ty {
                    kind: TyKind::Tuple(new_tys),
                    span: self.span,
                }
            }
            TyKind::Function(params, ret) => {
                let new_params = params.iter().map(|param| param.apply_subst(subst)).collect();
                let new_ret = ret.apply_subst(subst);
                Ty {
                    kind: TyKind::Function(new_params, Arc::new(new_ret)),
                    span: self.span,
                }
            }
            TyKind::Error => self.clone(),
            TyKind::Never => self.clone(),
            TyKind::SelfType => self.clone(),
            TyKind::Map(key_ty, value_ty) => {
                let new_key_ty = key_ty.apply_subst(subst);
                let new_value_ty = value_ty.apply_subst(subst);
                Ty {
                    kind: TyKind::Map(Arc::new(new_key_ty), Arc::new(new_value_ty)),
                    span: self.span,
                }
            }
            TyKind::Set(elem_ty) => {
                let new_elem_ty = elem_ty.apply_subst(subst);
                Ty {
                    kind: TyKind::Set(Arc::new(new_elem_ty)),
                    span: self.span,
                }
            }
        }
    }

    /// Get the free type variables in this type
    pub fn free_vars(&self) -> HashSet<TypeId> {
        let mut vars = HashSet::new();
        self.collect_free_vars(&mut vars);
        vars
    }

    // Helper for free_vars to avoid redundant allocations
    fn collect_free_vars(&self, vars: &mut HashSet<TypeId>) {
        match &self.kind {
            TyKind::Var(id) => { vars.insert(*id); }
            TyKind::Primitive(_) | TyKind::Error | TyKind::Never | TyKind::SelfType => {} // Skip SelfType
            TyKind::Named { args, .. } => {
                for arg in args {
                    arg.collect_free_vars(vars);
                }
            }
            TyKind::Array(elem_ty, _) => elem_ty.collect_free_vars(vars),
            TyKind::Tuple(tys) => {
                for ty in tys {
                    ty.collect_free_vars(vars);
                }
            }
            TyKind::Function(params, ret) => {
                for param in params {
                    param.collect_free_vars(vars);
                }
                ret.collect_free_vars(vars);
            }
            TyKind::Map(key_ty, value_ty) => {
                key_ty.collect_free_vars(vars);
                value_ty.collect_free_vars(vars);
            }
            TyKind::Set(elem_ty) => elem_ty.collect_free_vars(vars),
        }
    }
}

/// The kind of a type
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind {
    /// A type variable used during type inference
    Var(TypeId),
    
    /// A primitive type (int, float, etc.)
    Primitive(PrimitiveType),
    
    /// A named type (struct, enum, type alias, etc.)
    Named {
        /// The name of the type
        name: String,
        /// Type arguments for generic types
        args: Vec<Ty>,
    },
    
    /// An array type with a known size
    Array(Arc<Ty>, usize),
    
    /// A tuple type
    Tuple(Vec<Ty>),
    
    /// A function type
    Function(Vec<Ty>, Arc<Ty>),
    
    /// An error type, used when type checking fails
    Error,

    /// The never type `!`, indicating divergence
    Never,

    /// Placeholder for the 'Self' type keyword within traits and impls.
    SelfType,

    /// A map type (key, value)
    Map(Arc<Ty>, Arc<Ty>),

    /// A set type
    Set(Arc<Ty>),
}

/// Primitive types in the Parallax language
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PrimitiveType {
    /// Signed 8-bit integer
    I8,
    /// Signed 16-bit integer
    I16,
    /// Signed 32-bit integer
    I32, 
    /// Signed 64-bit integer
    I64,
    /// Signed 128-bit integer
    I128,
    /// Unsigned 8-bit integer
    U8,
    /// Unsigned 16-bit integer
    U16,
    /// Unsigned 32-bit integer
    U32,
    /// Unsigned 64-bit integer
    U64,
    /// Unsigned 128-bit integer
    U128,
    /// 32-bit floating point
    F32,
    /// 64-bit floating point
    F64,
    /// Boolean type
    Bool,
    /// Character type
    Char,
    /// String type
    String,
    /// The unit type `()`, representing the absence of a value.
    Unit,
    /// Integer literal type (can be narrowed to any integer type)
    IntegerLiteral,
    /// Float literal type (can be narrowed to f32 or f64)
    FloatLiteral,
}

impl fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrimitiveType::I8 => write!(f, "i8"),
            PrimitiveType::I16 => write!(f, "i16"),
            PrimitiveType::I32 => write!(f, "i32"),
            PrimitiveType::I64 => write!(f, "i64"),
            PrimitiveType::I128 => write!(f, "i128"),
            PrimitiveType::U8 => write!(f, "u8"),
            PrimitiveType::U16 => write!(f, "u16"),
            PrimitiveType::U32 => write!(f, "u32"),
            PrimitiveType::U64 => write!(f, "u64"),
            PrimitiveType::U128 => write!(f, "u128"),
            PrimitiveType::F32 => write!(f, "f32"),
            PrimitiveType::F64 => write!(f, "f64"),
            PrimitiveType::Bool => write!(f, "bool"),
            PrimitiveType::Char => write!(f, "char"),
            PrimitiveType::String => write!(f, "string"),
            PrimitiveType::Unit => write!(f, "()"),
            PrimitiveType::IntegerLiteral => write!(f, "{{integer}}"),
            PrimitiveType::FloatLiteral => write!(f, "{{float}}"),
        }
    }
}

/// Information about a typed function
#[derive(Debug, Clone)]
pub struct TypedFunction {
    /// The name of the function
    pub name: String,
    /// Parameters with their types
    pub params: Vec<TypedParameter>,
    /// Return type
    pub return_type: Ty,
    /// Generic parameters on the function
    pub generic_params: Vec<String>,
    /// Original span of the function declaration
    pub span: SourceSpan,
    /// Optional typed body of the function
    pub body: Option<TypedExpr>,
    /// Whether the function has side effects
    pub is_effectful: bool,
}

/// Information about a typed parameter
#[derive(Debug, Clone)]
pub struct TypedParameter {
    /// The name of the parameter
    pub name: String,
    /// The resolved symbol for the parameter
    pub symbol: Symbol,
    /// The type of the parameter
    pub ty: Ty,
    /// Whether the parameter is variadic
    pub is_variadic: bool,
    /// Whether the parameter has a default value
    pub has_default: bool,
    /// Original span of the parameter declaration
    pub span: SourceSpan,
}

/// Definition of a generic parameter including its bounds.
/// This version is used internally by the type checker.
#[derive(Debug, Clone)]
pub struct GenericParamDef {
    pub name: String,      // Original name from source
    pub symbol: Symbol,    // Symbol from resolver
    pub id: TypeId,        // TypeId assigned by typechecker (usually a TyKind::Var)
    pub bounds: Vec<TraitRef>, // Use the checker's TraitRef
    pub span: SourceSpan,
}

/// Information about a typed struct
#[derive(Debug, Clone)]
pub struct TypedStruct {
    /// The name of the struct
    pub name: String,
    /// The symbol associated with the struct definition
    pub symbol: Symbol,
    /// Fields with their types
    pub fields: Vec<TypedField>,
    /// Generic parameters on the struct
    pub generic_params: Vec<GenericParamDef>,
    /// Original span of the struct declaration
    pub span: SourceSpan,
}

/// Information about a typed field
#[derive(Debug, Clone)]
pub struct TypedField {
    /// The name of the field
    pub name: String,
    /// The resolved symbol for the field (added for HIR lowering)
    pub symbol: Symbol,
    /// The type of the field
    pub ty: Ty,
    /// Whether the field is public
    pub is_public: bool,
    /// Original span of the field declaration
    pub span: SourceSpan,
}

/// Information about a typed enum
#[derive(Debug, Clone)]
pub struct TypedEnum {
    /// The name of the enum
    pub name: String,
    /// The symbol associated with the enum definition
    pub symbol: Symbol,
    /// Variants of the enum
    pub variants: Vec<TypedVariant>,
    /// Generic parameters on the enum
    pub generic_params: Vec<GenericParamDef>,
    /// Original span of the enum declaration
    pub span: SourceSpan,
}

/// Information about a typed enum variant
#[derive(Debug, Clone)]
pub enum TypedVariant {
    /// A simple variant with no data
    Unit {
        /// The name of the variant
        name: String,
        /// The symbol associated with the variant definition
        symbol: Symbol,
        /// Original span of the variant declaration
        span: SourceSpan,
    },
    
    /// A tuple variant with ordered fields
    Tuple {
        /// The name of the variant
        name: String,
        /// The symbol associated with the variant definition
        symbol: Symbol,
        /// Types of the tuple fields
        types: Vec<Ty>,
        /// Original span of the variant declaration
        span: SourceSpan,
    },
    
    /// A struct variant with named fields
    Struct {
        /// The name of the variant
        name: String,
        /// The symbol associated with the variant definition
        symbol: Symbol,
        /// Fields of the struct variant
        fields: Vec<TypedField>,
        /// Original span of the variant declaration
        span: SourceSpan,
    },
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
    pub trait_repo: crate::traits::TraitRepository,
    /// The symbol for the main entry point function (e.g., `main`), if found.
    pub entry_point: Option<Symbol>,
    /// List of (full_path, symbol) pairs for intrinsic functions found in stdlib.
    pub intrinsics: Vec<(String, Symbol)>,
    /// Any type errors that occurred during checking
    pub errors: Vec<String>,
}

/// A context for type checking and type information
#[derive(Debug, Clone, Default)]
pub struct TypeContext {
    /// Map from type names to type definitions
    types: HashMap<String, TypeDef>,
    /// Map from symbol to original name (temporary solution)
    symbol_names: HashMap<Symbol, String>,
    /// Map from type Symbol -> method Symbol -> method signature for inherent impls
    inherent_methods: HashMap<Symbol, HashMap<Symbol, FunctionSignature>>,
    /// Map from Trait Symbol to TraitId
    trait_ids: HashMap<Symbol, TraitId>,
    /// Map from Type Symbol to TypeDef
    type_defs_by_symbol: HashMap<Symbol, TypeDef>,
}

impl TypeContext {
    /// Create a new empty type context
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
            symbol_names: HashMap::new(), 
            inherent_methods: HashMap::new(), 
            trait_ids: HashMap::new(), // Initialize
            type_defs_by_symbol: HashMap::new(), // Initialize
        }
    }
    
    /// Add a type definition and its symbol-name mapping
    pub fn add_type(&mut self, symbol: Symbol, name: String, def: TypeDef) {
        self.symbol_names.insert(symbol, name.clone());
        self.type_defs_by_symbol.insert(symbol, def.clone()); // Store by symbol too
        self.types.insert(name, def);
    }

    /// Get the original name for a symbol (temporary solution)
    pub fn get_name_for_symbol(&self, symbol: &Symbol) -> Option<&String> {
        self.symbol_names.get(symbol)
    }

    /// Get the symbol for a type name
    pub fn get_symbol_for_name(&self, name: &str) -> Option<Symbol> {
        // Inefficient reverse lookup - consider adding name->symbol map if needed often
        self.symbol_names.iter()
            .find(|(_, n)| *n == name)
            .map(|(s, _)| *s)
    }
    
    /// Get a type definition by name
    pub fn get_type(&self, name: &str) -> Option<&TypeDef> {
        self.types.get(name)
    }

    /// Get a type definition by symbol
    pub fn get_type_by_symbol(&self, symbol: &Symbol) -> Option<&TypeDef> {
        self.type_defs_by_symbol.get(symbol)
    }
    
    /// Check if a type exists
    pub fn has_type(&self, name: &str) -> bool {
        self.types.contains_key(name)
    }
    
    /// Get all type definitions
    pub fn all_types(&self) -> &HashMap<String, TypeDef> {
        &self.types
    }

    /// Add a mapping from a trait symbol to its TraitId
    pub fn add_trait_symbol(&mut self, symbol: Symbol, _name: String, id: TraitId) {
        self.trait_ids.insert(symbol, id);
    }

    /// Add an inherent method signature associated with a type symbol.
    pub(crate) fn add_inherent_method(&mut self, type_symbol: Symbol, method_symbol: Symbol, signature: FunctionSignature) {
        self.inherent_methods
            .entry(type_symbol)
            .or_default()
            .insert(method_symbol, signature);
    }

    /// Get the inherent methods for a given type symbol.
    pub(crate) fn get_inherent_methods(&self, type_symbol: &Symbol) -> Option<&HashMap<Symbol, FunctionSignature>> {
        self.inherent_methods.get(type_symbol)
    }
}

/// A type definition stored in the TypeContext
#[derive(Debug, Clone)]
pub enum TypeDef {
    /// A struct definition
    Struct(StructDef),
    /// An enum definition
    Enum(EnumDef),
    /// A function signature
    Function(FunctionSignature),
    // Note: Trait definitions are primarily stored in TraitRepository,
    // but TypeContext might hold a mapping from Symbol -> TraitId/Name.
}

/// A field in a struct type
#[derive(Debug, Clone)]
pub struct Field {
    /// Field name
    pub name: String,
    /// The resolved symbol for the field
    pub symbol: Symbol,
    /// Field type
    pub ty: Ty,
    /// Source span
    pub span: SourceSpan,
}

/// A struct type definition
#[derive(Debug, Clone)]
pub struct StructDef {
    /// Struct name
    pub name: String,
    /// The symbol associated with the struct definition
    pub symbol: Symbol,
    /// Generic parameters
    pub generic_params: Vec<GenericParamDef>,
    /// Fields
    pub fields: Vec<Field>,
    /// Source span
    pub span: SourceSpan,
}

/// A variant in an enum
#[derive(Debug, Clone)]
pub struct EnumVariant {
    /// Variant name
    pub name: String,
    /// Symbol for the variant itself
    pub symbol: Symbol,
    /// Variant fields (empty for unit variants)
    pub fields: Vec<Field>,
    /// Source span
    pub span: SourceSpan,
}

/// An enum type definition
#[derive(Debug, Clone)]
pub struct EnumDef {
    /// Enum name
    pub name: String,
    /// The symbol associated with the enum definition
    pub symbol: Symbol,
    /// Generic parameters
    pub generic_params: Vec<GenericParamDef>,
    /// Variants
    pub variants: Vec<EnumVariant>,
    /// Source span
    pub span: SourceSpan,
}

/// A type annotation for a parameter
#[derive(Debug, Clone)]
pub struct ParamType {
    /// Parameter name
    pub name: String,
    /// Parameter type
    pub ty: Ty,
    /// Source span
    pub span: SourceSpan,
}

/// Defines different kinds of self parameter for a function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SelfParamKind {
    /// Self parameter is passed by value. 
    /// In Rust, this would be `self`.
    Value,
    
    /// No self parameter is used.
    None,
}

/// A function signature
#[derive(Debug, Clone)]
pub struct FunctionSignature {
    /// Function name
    pub name: String,
    /// Optional 'self' parameter kind (None for free functions)
    pub self_param: Option<SelfParamKind>,
    /// Generic parameters
    pub generic_params: Vec<GenericParamDef>,
    /// Parameters (excluding self)
    pub params: Vec<ParamType>,
    /// Return type
    pub return_type: Ty,
    /// Source span
    pub span: SourceSpan,
}

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
    Literal(parallax_syntax::ast::common::Literal),
    Variable {
        /// The resolved symbol for the variable
        symbol: Symbol,
        /// Original name (for debugging/errors)
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

/// Represents a resolved trait reference (e.g., a bound `MyTrait<String>`).
/// This version is used internally by the type checker.
#[derive(Debug, Clone)]
pub struct TraitRef {
    pub trait_id: TraitId, // The ID of the referenced trait
    pub type_arguments: Vec<Ty>, // Concrete types for the trait's generic parameters
    pub span: SourceSpan,
}

/// Definition of an associated type within a trait.
#[derive(Debug, Clone)]
pub struct AssociatedTypeDef {
    pub name: String,
    pub symbol: Symbol, // Symbol for the associated type itself
    // pub bounds: Vec<TraitRef>, // TODO: Add bounds later if needed
    pub default: Option<Ty>, // TODO: Add default type later if needed
    pub span: SourceSpan,
}

/// Information about a trait definition stored by the typechecker.
#[derive(Debug, Clone)]
pub struct TraitDef {
    pub id: TraitId, // ID assigned by the TraitRepository
    pub trait_symbol: Symbol, // Original symbol from resolver
    pub name: String,
    pub generic_params: Vec<GenericParamDef>,
    // pub supertraits: Vec<TraitRef>, // Placeholder for resolved supertrait references
    pub methods: HashMap<Symbol, TraitMethod>, // Map method Symbol -> TraitMethod signature
    pub associated_types: HashMap<Symbol, AssociatedTypeDef>, // Added associated types
    pub span: SourceSpan,
}

/// Information about a method signature within a trait definition.
#[derive(Debug, Clone)]
pub struct TraitMethod {
    pub name: String,
    pub method_symbol: Symbol, // Original symbol from resolver
    pub signature: FunctionSignature, // Use the existing FunctionSignature
}

/// Information about a trait implementation stored by the typechecker.
#[derive(Debug, Clone)]
pub struct ImplDef {
    pub id: ImplId, // ID assigned by the TraitRepository
    pub impl_symbol: Symbol, // Original symbol from resolver
    pub trait_ref: Option<TraitRef>, // Use the checker's TraitRef
    pub implementing_type: Ty, // The concrete type implementing the trait
    pub generic_params: Vec<GenericParamDef>, // Generic parameters defined *on the impl block itself*.
    pub methods: HashMap<Symbol, Symbol>, // Map: Trait Method Symbol -> Impl Method Symbol
    pub associated_type_bindings: HashMap<Symbol, Ty>, // Added associated type bindings
    pub span: SourceSpan,
}