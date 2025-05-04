// src/types/hir.rs
//! Defines the structures for the High-level Intermediate Representation (HIR)
//! produced by the type checker.

use std::collections::BTreeMap;
use miette::SourceSpan;
use parallax_resolve::types::Symbol;
// Use crate path
use crate::types::core::Ty; // Removed PrimitiveType
// Removed unused ParamType, GenericParamDef, SelfParamKind, TraitRepository
use parallax_syntax::ast::common::Literal as AstLiteral; // Keep AstLiteral for HIR literals

// --- Typed Expressions and Patterns ---

/// Represents a fully typed expression node in the HIR.
#[derive(Debug, Clone, PartialEq)]
pub struct TypedExpr {
    pub kind: TypedExprKind,
    pub ty: Ty,
    pub span: SourceSpan,
}

/// Kind of a typed expression in the HIR.
#[derive(Debug, Clone, PartialEq)]
pub enum TypedExprKind {
    Block(Vec<TypedExpr>),
    If {
        condition: Box<TypedExpr>,
        then_branch: Box<TypedExpr>,
        else_branch: Option<Box<TypedExpr>>,
    },
    Match {
        scrutinee: Box<TypedExpr>,
        arms: Vec<TypedMatchArm>,
    },
    /// Represents various kinds of calls after resolution:
    /// - Direct function call (`func_symbol` is the function's symbol)
    /// - Method call (`func_symbol` is the method's symbol, `self_arg` is Some)
    /// - Closure call (`func_symbol` is None, `func_expr` holds the closure expr)
    Call {
        /// The expression evaluating to the callable (function name, method receiver, closure variable).
        func_expr: Box<TypedExpr>,
        /// The specific symbol of the function/method being called, if resolved directly.
        /// This is crucial for linking to the correct signature and definition.
        func_symbol: Option<Symbol>,
        /// Concrete type arguments used for this specific call, if the function/method is generic.
        type_args: Option<Vec<Ty>>, // Added type args
        /// Resolved arguments passed to the function/method.
        args: Vec<TypedArgument>,
    },
    Lambda {
        params: Vec<TypedParameter>, // Use TypedParameter for consistency
        body: Box<TypedExpr>,
        // TODO: Add return type annotation if supported?
    },
    // Specific literal kinds for clarity
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
    /// Accessing a variable, parameter, or function/variant name by its symbol.
    Variable {
        /// The resolved symbol for the variable binding (parameter or let) or definition (function, variant).
        symbol: Symbol,
        /// Original name (for debugging/errors).
        name: String,
    },
    /// Accessing a struct field.
    Field {
        /// The typed expression for the object being accessed.
        object: Box<TypedExpr>,
        /// The resolved symbol for the specific field definition.
        field_symbol: Symbol,
        /// Original field name (for debugging/errors).
        field_name: String,
    },
    /// Accessing a tuple element by index.
    TupleField {
        /// The typed expression for the tuple being accessed.
        tuple: Box<TypedExpr>,
        /// The index of the element being accessed.
        index: usize,
    },
    Array(Vec<TypedExpr>),
    Tuple(Vec<TypedExpr>),
    Map(Vec<(TypedExpr, TypedExpr)>),
    HashSet(Vec<TypedExpr>),
    Let {
        pattern: TypedPattern,
        value: Box<TypedExpr>,
        // Type annotation is implicitly handled by unifying pattern.ty with value.ty
    },
    /// Instantiating a struct.
    Struct {
        /// Name of the struct (primarily for HIR readability).
        name: String,
        /// Symbol of the struct definition being instantiated.
        struct_symbol: Symbol,
        fields: Vec<(String, TypedExpr)>, // Field name -> TypedExpr value
        base: Option<Box<TypedExpr>>, // For struct update syntax
    },
    Paren(Box<TypedExpr>),
    /// Constructing an enum variant.
    VariantConstructor {
        enum_name: String,
        variant_name: String,
        enum_symbol: Symbol,
        variant_symbol: Symbol,
        /// Arguments passed to the constructor (ordered for tuple variants, named for struct variants).
        args: Vec<TypedArgument>,
    },
    /// Placeholder for expressions that failed during type checking.
    /// The `ty` field will likely be `TyKind::Error` or `TyKind::Never`.
    Error,
    // Specific kinds for logical operators to handle short-circuiting.
    LogicalAnd {
        left: Box<TypedExpr>,
        right: Box<TypedExpr>,
    },
    LogicalOr {
        left: Box<TypedExpr>,
        right: Box<TypedExpr>,
    },
}

/// Represents a typed argument in a function or variant constructor call.
#[derive(Debug, Clone, PartialEq)]
pub struct TypedArgument {
    /// Optional name (for named arguments or struct variant fields).
    pub name: Option<String>,
    /// The typed expression providing the argument's value.
    pub value: TypedExpr,
    /// Source span of the argument.
    pub span: SourceSpan,
}

// --- Typed Pattern Definitions ---

/// Represents a fully typed pattern node in the HIR.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedPattern {
    pub kind: TypedPatternKind,
    pub span: SourceSpan,
    /// The type this pattern matches against.
    pub ty: Ty,
}

/// Kind of a typed pattern in the HIR.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedPatternKind {
    /// Binding a variable: `x`, `mut y`.
    Identifier {
        /// The unique symbol assigned to this binding.
        symbol: Symbol,
        /// Original name from source.
        name: String,
    },
    /// Matching a literal value: `1`, `"hello"`, `true`.
    Literal(AstLiteral),
    /// Matching an enum variant: `Option::Some(x)`, `Color::Red`,
    /// `MyEnum::StructVariant { field1: pat1, .. }`.
    Constructor {
        enum_name: String,
        variant_name: String,
        enum_symbol: Symbol,
        variant_symbol: Symbol,
        /// Sub-patterns matching the variant's arguments/fields.
        /// For unit variants: Empty Vec.
        /// For tuple variants: Vec containing patterns for tuple elements.
        /// For struct variants: Vec containing TypedPatternField patterns.
        args: Vec<TypedPatternArgument>,
    },
    /// Matching an array: `[a, b, ..]`.
    Array(Vec<TypedPattern>),
    /// Matching a tuple: `(x, _, y)`.
    Tuple(Vec<TypedPattern>),
    /// Matching a struct: `Point { x, y: 0 }`.
    Struct {
        struct_name: String,
        struct_symbol: Symbol,
        /// Field patterns within the struct match.
        fields: Vec<TypedPatternField>,
    },
    /// The rest pattern `..` in array/tuple patterns.
    Rest,
    /// An OR-pattern: `pat1 | pat2`.
    Or(Box<TypedPattern>, Box<TypedPattern>),
    /// The wildcard pattern `_`.
    Wildcard,
}

/// Represents an argument/field within a constructor pattern in the HIR.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedPatternArgument {
    /// A positional argument pattern (for tuple variants).
    Positional(TypedPattern),
    /// A named field pattern (for struct variants).
    Named(TypedPatternField),
    /// The rest pattern `..` within a tuple or struct variant pattern.
    Rest(SourceSpan),
}

/// Represents a field within a typed struct pattern (e.g., `x: pattern` or just `x`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedPatternField {
    /// Name of the field being matched.
    pub name: String,
    /// The sub-pattern for the field's value.
    /// This should always be `Some` after desugaring shorthand patterns.
    pub pattern: TypedPattern, // Changed from Option<TypedPattern>
    /// Source span of this field pattern element.
    pub span: SourceSpan,
}

// --- Typed Statements ---

/// Represents a fully typed statement node in the HIR.
#[derive(Debug, Clone, PartialEq)]
pub struct TypedStatement {
    pub kind: TypedStatementKind,
    pub span: SourceSpan,
}

/// Kind of a typed statement in the HIR.
#[derive(Debug, Clone, PartialEq)]
pub enum TypedStatementKind {
    /// An expression used as a statement (result is discarded, must have Unit type or be Never).
    Expression(TypedExpr),
    // Note: Let bindings are currently handled within TypedExprKind::Block or TypedExprKind::Let
}

/// Represents a fully typed match arm in the HIR.
#[derive(Debug, Clone, PartialEq)]
pub struct TypedMatchArm {
    pub pattern: TypedPattern,
    pub body: TypedExpr,
    // Optional: Add span for the whole arm?
}

// --- Top-Level Typed Structures ---

/// Represents a fully type-checked function, including its body.
/// Stored in `TypedDefinitions`.
#[derive(Debug, Clone, PartialEq)]
pub struct TypedFunction {
    /// The name of the function.
    pub name: String,
    /// Resolved typed parameters (including potentially `self`).
    /// Note: `self` parameter info might be implicitly handled via `implementing_type` in `ImplDef` context.
    pub params: Vec<TypedParameter>,
    /// Resolved return type.
    pub return_type: Ty,
    /// Names of generic parameters declared on this function (for HIR representation).
    /// The actual `TypeId`s and bounds are handled during checking via `GenericParamDef`.
    pub generic_params: Vec<String>,
    /// Original source span of the function definition.
    pub span: SourceSpan,
    /// The fully typed body expression, if the function has one and it checked successfully.
    pub body: Option<TypedExpr>,
    /// Whether the function is known to have side effects.
    pub is_effectful: bool,
}

/// Represents a fully typed function parameter (used in `TypedFunction`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedParameter {
    /// The name of the parameter.
    pub name: String,
    /// The resolved symbol for the parameter binding.
    pub symbol: Symbol,
    /// The fully resolved type of the parameter.
    pub ty: Ty,
    /// Whether the parameter is variadic (not currently supported).
    pub is_variadic: bool,
    /// Whether the parameter has a default value (not currently supported).
    pub has_default: bool,
    /// Original span of the parameter declaration.
    pub span: SourceSpan,
}

/// Represents a fully type-checked struct definition.
/// Stored in `TypedDefinitions`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedStruct {
    /// The name of the struct.
    pub name: String,
    /// The symbol associated with the struct definition.
    pub symbol: Symbol,
    /// Typed fields of the struct.
    pub fields: Vec<TypedField>,
    /// Names of generic parameters declared on this struct (for HIR representation).
    pub generic_params: Vec<String>,
    /// Original span of the struct declaration.
    pub span: SourceSpan,
}

/// Represents a fully type-checked enum definition.
/// Stored in `TypedDefinitions`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedEnum {
    /// The name of the enum.
    pub name: String,
    /// The symbol associated with the enum definition.
    pub symbol: Symbol,
    /// Typed variants of the enum.
    pub variants: Vec<TypedVariant>,
    /// Names of generic parameters declared on this enum (for HIR representation).
    pub generic_params: Vec<String>,
    /// Original span of the enum declaration.
    pub span: SourceSpan,
}

/// Represents a fully type-checked enum variant (used in `TypedEnum`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedVariant {
    /// The name of the variant.
    pub name: String,
    /// The symbol associated with the variant definition.
    pub symbol: Symbol,
    /// Typed fields of the variant (empty for unit, indexed for tuple, named for struct).
    pub fields: Vec<TypedField>,
    /// Original span of the variant declaration.
    pub span: SourceSpan,
}

/// Represents a fully type-checked struct field (used in `TypedStruct` and `TypedVariant`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedField {
    /// The name of the field.
    pub name: String,
    /// The resolved symbol for the field definition.
    pub symbol: Symbol,
    /// The fully resolved type of the field.
    pub ty: Ty,
    /// Whether the field is public (relevant for access control).
    pub is_public: bool,
    /// Original span of the field declaration.
    pub span: SourceSpan,
}

/// A collection of all typed definitions in a module or crate.
/// This forms the main output of the type checking process for definitions.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct TypedDefinitions {
    /// Map from struct Symbol -> TypedStruct definition.
    pub structs: BTreeMap<Symbol, TypedStruct>,
    /// Map from enum Symbol -> TypedEnum definition.
    pub enums: BTreeMap<Symbol, TypedEnum>,
    /// Map from function/method Symbol -> TypedFunction definition (including body).
    pub functions: BTreeMap<Symbol, TypedFunction>,
    // Note: TraitDef and ImplDef are stored in TraitRepository
}

impl TypedDefinitions {
    /// Adds a typed struct definition.
    /// Precondition: `symbol` should uniquely identify `def`.
    /// Postcondition: `structs` map contains the mapping `symbol` -> `def`.
    pub fn add_struct(&mut self, symbol: Symbol, def: TypedStruct) {
        // Assertion: Ensure we aren't overwriting an existing definition with the same symbol.
        // assert!(!self.structs.contains_key(&symbol), "Duplicate struct symbol {:?}", symbol);
        self.structs.insert(symbol, def);
    }

    /// Adds a typed enum definition.
    /// Precondition: `symbol` should uniquely identify `def`.
    /// Postcondition: `enums` map contains the mapping `symbol` -> `def`.
    pub fn add_enum(&mut self, symbol: Symbol, def: TypedEnum) {
        // assert!(!self.enums.contains_key(&symbol), "Duplicate enum symbol {:?}", symbol);
        self.enums.insert(symbol, def);
    }

    /// Adds a typed function definition (including potential body).
    /// Precondition: `symbol` should uniquely identify `def`.
    /// Postcondition: `functions` map contains the mapping `symbol` -> `def`.
    pub fn add_function(&mut self, symbol: Symbol, def: TypedFunction) {
        // assert!(!self.functions.contains_key(&symbol), "Duplicate function symbol {:?}", symbol);
        self.functions.insert(symbol, def);
    }
}

/// Represents a fully type-checked module (or potentially the entire crate).
/// This is the final output structure of the `parallax-types` crate.
#[derive(Debug, Clone, PartialEq)]
pub struct TypedModule {
    /// All typed definitions (structs, enums, functions with bodies).
    pub definitions: TypedDefinitions,
    /// The symbol for the main entry point function (e.g., `main`), if found.
    pub entry_point: Option<Symbol>,
    /// List of (full_path, symbol) pairs for intrinsic functions found in stdlib.
    /// Propagated from `ResolvedModuleStructure` for use by later compiler stages.
    pub intrinsics: Vec<(String, Symbol)>,
    /// Any fatal type errors encountered during checking.
    /// If not empty, the definitions might be incomplete or incorrect.
    pub errors: Vec<crate::error::TypeError>,
}

// --- Apply Substitution to Typed Structures ---

impl TypedExpr {
    /// Applies a substitution recursively to the types within this expression.
    pub fn apply_subst_mut(&mut self, subst: &crate::context::Substitution) {
        // Apply to the outer type first by replacing it with the substituted version
        // self.ty.apply_subst_mut(subst); // <<< Commented out
        self.ty = self.ty.apply_subst(subst); // <<< Use immutable apply_subst and assign back

        // Apply recursively to inner expressions/patterns
        match &mut self.kind {
            TypedExprKind::Block(exprs) => {
                for expr in exprs { expr.apply_subst_mut(subst); }
            }
            TypedExprKind::If { condition, then_branch, else_branch } => {
                condition.apply_subst_mut(subst);
                then_branch.apply_subst_mut(subst);
                if let Some(else_b) = else_branch { else_b.apply_subst_mut(subst); }
            }
            TypedExprKind::Match { scrutinee, arms } => {
                scrutinee.apply_subst_mut(subst);
                for arm in arms {
                    arm.pattern.apply_subst_mut(subst); // Apply to pattern types
                    arm.body.apply_subst_mut(subst);
                }
            }
            TypedExprKind::Call { func_expr, type_args, args, .. } => {
                func_expr.apply_subst_mut(subst);
                if let Some(t_args) = type_args {
                    for t_arg in t_args { t_arg.apply_subst_mut(subst); }
                }
                for arg in args { arg.apply_subst_mut(subst); }
            }
            TypedExprKind::Lambda { params, body } => {
                for param in params { param.ty.apply_subst_mut(subst); }
                body.apply_subst_mut(subst);
            }
            TypedExprKind::Variable { .. } => { /* No inner types */ }
            TypedExprKind::IntLiteral { .. } => { /* No inner types, outer self.ty already handled */ }
            TypedExprKind::FloatLiteral { .. } => { /* No inner types, outer self.ty already handled */ }
            TypedExprKind::StringLiteral(_) => { /* No inner types */ }
            TypedExprKind::CharLiteral(_) => { /* No inner types */ }
            TypedExprKind::BoolLiteral(_) => { /* No inner types */ }
            TypedExprKind::Error => { /* No inner types */ }
            TypedExprKind::Field { object, .. } => {
                object.apply_subst_mut(subst);
            }
             TypedExprKind::TupleField { tuple, .. } => {
                tuple.apply_subst_mut(subst);
            }
            TypedExprKind::Array(exprs) | TypedExprKind::Tuple(exprs) | TypedExprKind::HashSet(exprs) => {
                for expr in exprs { expr.apply_subst_mut(subst); }
            }
            TypedExprKind::Map(pairs) => {
                for (k, v) in pairs { k.apply_subst_mut(subst); v.apply_subst_mut(subst); }
            }
            TypedExprKind::Let { pattern, value } => {
                pattern.apply_subst_mut(subst);
                value.apply_subst_mut(subst);
            }
            TypedExprKind::Struct { fields, base, .. } => {
                for (_, expr) in fields { expr.apply_subst_mut(subst); }
                if let Some(b) = base { b.apply_subst_mut(subst); }
            }
            TypedExprKind::Paren(expr) => {
                expr.apply_subst_mut(subst);
            }
            TypedExprKind::VariantConstructor { args, .. } => {
                for arg in args { arg.apply_subst_mut(subst); }
            }
            TypedExprKind::LogicalAnd { left, right } | TypedExprKind::LogicalOr { left, right } => {
                left.apply_subst_mut(subst);
                right.apply_subst_mut(subst);
            }
        }
    }
}

impl TypedArgument {
    pub fn apply_subst_mut(&mut self, subst: &crate::context::Substitution) {
        self.value.apply_subst_mut(subst);
    }
}

impl TypedPattern {
    pub fn apply_subst_mut(&mut self, subst: &crate::context::Substitution) {
        self.ty.apply_subst_mut(subst);
        match &mut self.kind {
            TypedPatternKind::Identifier { .. } |
            TypedPatternKind::Literal(_) |
            TypedPatternKind::Rest |
            TypedPatternKind::Wildcard => { /* No inner types */ }
            TypedPatternKind::Constructor { args, .. } => {
                for arg in args { arg.apply_subst_mut(subst); }
            }
            TypedPatternKind::Array(pats) | TypedPatternKind::Tuple(pats) => {
                for pat in pats { pat.apply_subst_mut(subst); }
            }
            TypedPatternKind::Struct { fields, .. } => {
                for field in fields { field.apply_subst_mut(subst); }
            }
            TypedPatternKind::Or(lhs, rhs) => {
                lhs.apply_subst_mut(subst);
                rhs.apply_subst_mut(subst);
            }
        }
    }
}

impl TypedPatternArgument {
    pub fn apply_subst_mut(&mut self, subst: &crate::context::Substitution) {
        match self {
            TypedPatternArgument::Positional(pat) => pat.apply_subst_mut(subst),
            TypedPatternArgument::Named(field) => field.apply_subst_mut(subst),
            TypedPatternArgument::Rest(_) => { /* No type */ }
        }
    }
}

impl TypedPatternField {
    pub fn apply_subst_mut(&mut self, subst: &crate::context::Substitution) {
        self.pattern.apply_subst_mut(subst);
    }
}

// --- End Apply Substitution --- 