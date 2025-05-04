use miette::{Diagnostic, SourceSpan};
use thiserror::Error;
use crate::types::{Ty, TyKind, PrimitiveType};
use crate::context::inference::TypeId;
use parallax_resolve::types::{Symbol, ResolvedType};

/// Result type for type operations
pub type TypeResult<T> = Result<T, TypeError>;

/// Errors that can occur during type checking
#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    /// Type mismatch error
    #[error("Type mismatch: expected {expected}, found {found}")]
    TypeMismatch {
        /// Expected type
        expected: String,
        /// Found type
        found: String,
        /// Source span
        span: SourceSpan,
    },

    /// Undefined variable error
    #[error("Undefined variable: {name}")]
    UndefinedVariable {
        /// Variable name
        name: String,
        /// Source span
        span: SourceSpan,
    },

    /// Unknown field error
    #[error("Unknown field: {field} on type {ty}")]
    UnknownField {
        /// Field name
        field: String,
        /// Type name
        ty: String,
        /// Source span
        span: SourceSpan,
    },

    /// Invalid array index error
    #[error("Invalid array index: expected integer, found {found}")]
    InvalidArrayIndex {
        /// Found type
        found: String,
        /// Source span
        span: SourceSpan,
    },

    /// Not a function error
    #[error("Not a function: {found}")]
    NotAFunction {
        /// Found type
        found: String,
        /// Source span
        span: SourceSpan,
    },

    /// Wrong number of arguments error
    #[error("Wrong number of arguments: expected {expected}, found {found}")]
    WrongNumberOfArguments {
        /// Expected number of arguments
        expected: usize,
        /// Found number of arguments
        found: usize,
        /// Source span
        span: SourceSpan,
    },

    /// Invalid operand types error
    #[error("Invalid operand types for {op}: expected {expected}")]
    InvalidOperandTypes {
        /// Operator
        op: String,
        /// Expected type
        expected: String,
        /// Source span
        span: SourceSpan,
    },

    /// Not a struct error
    #[error("Not a struct: {found}")]
    NotAStruct {
        /// Found type
        found: String,
        /// Source span
        span: SourceSpan,
    },

    /// Unknown struct field error
    #[error("Unknown field: {field} on struct {struct_name}")]
    UnknownStructField {
        /// Field name
        field: String,
        /// Struct name
        struct_name: String,
        /// Source span
        span: SourceSpan,
    },

    /// Missing field error
    #[error("Missing field: {field} in struct {struct_name}")]
    MissingField {
        /// Field name
        field: String,
        /// Struct name
        struct_name: String,
        /// Source span
        span: SourceSpan,
    },

    /// Unknown enum error
    #[error("Unknown enum: {enum_name}")]
    UnknownEnum {
        /// Enum name
        enum_name: String,
        /// Source span
        span: SourceSpan,
    },

    /// Unknown enum variant error
    #[error("Unknown variant: {variant} on enum {enum_name}")]
    UnknownVariant {
        /// Variant name
        variant: String,
        /// Enum name
        enum_name: String,
        /// Source span
        span: SourceSpan,
    },

    /// Not an enum error
    #[error("Not an enum: {found}")]
    NotAnEnum {
        /// Found type
        found: String,
        /// Source span
        span: SourceSpan,
    },

    /// Not a method error
    #[error("Not a method: {method} on type {ty}")]
    NotAMethod {
        /// Method name
        method: String,
        /// Type name
        ty: String,
        /// Source span
        span: SourceSpan,
    },

    /// No matching method error
    #[error("No matching method: {method} on type {ty}")]
    NoMatchingMethod {
        /// Method name
        method: String,
        /// Type name
        ty: String,
        /// Potential candidates
        candidates: Option<String>,
        /// Source span
        span: SourceSpan,
    },

    /// Not a trait error
    #[error("Not a trait: {found}")]
    NotATrait {
        /// Found name
        found: String,
        /// Source span
        span: SourceSpan,
    },

    /// Type does not implement trait error
    #[error("Type {ty} does not implement trait {trait_name}")]
    TypeDoesNotImplementTrait {
        /// Type name
        ty: String,
        /// Trait name
        trait_name: String,
        /// Source span
        span: SourceSpan,
    },

    /// Multiple valid trait implementations found for a type.
    #[error("Ambiguous trait implementation for trait '{trait_name}' on type {ty}")]
    AmbiguousTraitImpl {
        ty: String,
        trait_name: String,
        span: SourceSpan, // Span of the code requiring the trait bound
    },

    /// Required trait not implemented error
    #[error("Required trait {trait_name} not implemented for type {ty}")]
    RequiredTraitNotImplemented {
        /// Type name
        ty: String,
        /// Trait name
        trait_name: String,
        /// Source span
        span: SourceSpan,
    },

    /// Wrong pattern type error
    #[error("Wrong pattern type: expected {expected}, found {found}")]
    WrongPatternType {
        /// Expected type
        expected: String,
        /// Found type
        found: String,
        /// Source span
        span: SourceSpan,
    },

    /// Or-pattern branches bind different sets of variables.
    #[error("Or-pattern branches bind different variables")]
    OrPatternBindingMismatch {
        // TODO: List differing variables?
        span: SourceSpan,
    },

    /// Non-exhaustive patterns error
    #[error("Non-exhaustive patterns")]
    NonExhaustivePatterns {
        /// Source span
        span: SourceSpan,
    },

    /// Inference recursion limit error
    #[error("Type inference recursion limit reached")]
    InferenceRecursionLimit {
        /// Source span
        span: SourceSpan,
    },

    /// Internal error
    #[error("Internal error: {message}")]
    InternalError {
        /// Error message
        message: String,
        /// Source span
        span: Option<SourceSpan>,
    },

    /// Unsupported operator error
    #[error("Unsupported operator: {op}")]
    UnsupportedOperator {
        /// Operator string
        op: String,
        /// Source span
        span: SourceSpan,
    },

    /// Multiple methods found matching the call.
    #[error("Ambiguous method call: '{method}' on type {ty}")]
    AmbiguousMethodCall {
        method: String,
        ty: String,
        // TODO: List candidate traits/impls?
        span: SourceSpan, // Span of the method call
    },

    #[error("Generic argument count mismatch: expected {expected}, found {found} for {kind} '{name}'")]
    GenericArgCountMismatch {
        kind: String, // e.g., "Function", "Struct", "Enum", "Method"
        name: String,
        expected: usize,
        found: usize,
        span: SourceSpan,
    },

    #[error("Type mismatch: expected {expected}, found {found}")]
    ReturnTypeMismatch {
        expected: String,
        found: String,
        span: SourceSpan,
        error: Box<TypeError>, // Nested error for details
    },

    #[error("'Self' type used outside of trait or impl context")]
    SelfOutsideImplOrTrait { span: SourceSpan },

    #[error("Resolver error occurred: see resolver diagnostics")]
    ResolverError { span: SourceSpan },

    #[error("Unknown trait: {name}")]
    UnknownTrait { name: String, span: SourceSpan },

    #[error("Generic parameter count mismatch for {kind} '{name}': expected {expected}, found {found}")]
    GenericParamCountMismatch {
        kind: String, // e.g., "Method"
        name: String,
        expected: usize,
        found: usize,
        span: SourceSpan,
    },

    #[error("Parameter count mismatch for function/method '{name}': expected {expected}, found {found}")]
    ParamCountMismatch {
        name: String,
        expected: usize,
        found: usize,
        span: SourceSpan,
    },

    #[error("Parameter type mismatch for method '{method_name}' in trait '{trait_name}' (parameter {param_index}): expected {expected}, found {found}")]
    MethodParamMismatch {
        trait_name: String,
        method_name: String,
        param_index: usize,
        expected: String,
        found: String,
        span: SourceSpan,
        error: Box<TypeError>, // Nested unification error
    },

    #[error("Return type mismatch for method '{method_name}' in trait '{trait_name}': expected {expected}, found {found}")]
    MethodReturnTypeMismatch {
        trait_name: String,
        method_name: String,
        expected: String,
        found: String,
        span: SourceSpan,
        error: Box<TypeError>, // Nested unification error
    },

    #[error("Unsupported expression")]
    UnsupportedExpression {
        /// Source span
        span: SourceSpan,
    },

    #[error("Unsupported literal")]
    UnsupportedLiteral {
        /// Source span
        span: SourceSpan,
    },

    #[error("Not a value: {name}")]
    NotAValue {
        /// Name
        name: String,
        /// Source span
        span: SourceSpan,
    },

    #[error("Unknown identifier: {name}")]
    UnknownIdentifier {
        /// Name
        name: String,
        /// Source span
        span: SourceSpan,
    },

    /// A method required by a trait was not found in the impl block.
    #[error("Missing implementation for method '{method_name}' in trait '{trait_name}'")]
    MissingMethodImpl {
        trait_name: String,
        method_name: String,
        span: SourceSpan,
    },

    /// Missing associated type impl
    #[error("Missing implementation for associated type '{type_name}' in trait '{trait_name}'")]
    MissingAssocTypeImpl {
        trait_name: String,
        type_name: String,
        span: SourceSpan,
    },

    /// Error indicating a trait was used where a concrete type was expected.
    #[error("Trait \"{trait_name}\" used as a type")]
    TraitUsedAsType {
        trait_name: String,
        span: SourceSpan,
    },

    #[error("Generic bound mismatch for {kind} '{param_name}' in {trait_name}::{method_name}: {message}")]
    GenericBoundMismatch {
        kind: String, // e.g., "Method generic parameter"
        param_name: String,
        trait_name: String,
        method_name: String,
        span: SourceSpan,
        message: String,
    },

    /// Error resolving a generic parameter on an impl block.
    #[error("Failed to resolve generic parameter '{param_name}' on impl: {source_error}")]
    ImplGenericParamResolutionError {
        param_name: String,
        span: SourceSpan,
        source_error: Box<TypeError>, // Box the nested error
    },

    /// Attempted to unify two variables that require different literal defaults (e.g., i32 vs f64).
    #[error("Conflicting literal defaults for types {ty1} and {ty2}")]
    LiteralDefaultConflict {
        ty1: String,
        ty2: String,
        span: SourceSpan,
    },
}

impl TypeError {
    /// Returns the primary source span associated with the error, if available.
    pub fn span(&self) -> Option<SourceSpan> {
        match self {
            TypeError::TypeMismatch { span, .. } => Some(*span),
            TypeError::UndefinedVariable { span, .. } => Some(*span),
            TypeError::UnknownField { span, .. } => Some(*span),
            TypeError::InvalidArrayIndex { span, .. } => Some(*span),
            TypeError::NotAFunction { span, .. } => Some(*span),
            TypeError::WrongNumberOfArguments { span, .. } => Some(*span),
            TypeError::InvalidOperandTypes { span, .. } => Some(*span),
            TypeError::NotAStruct { span, .. } => Some(*span),
            TypeError::UnknownStructField { span, .. } => Some(*span),
            TypeError::MissingField { span, .. } => Some(*span),
            TypeError::UnknownEnum { span, .. } => Some(*span),
            TypeError::UnknownVariant { span, .. } => Some(*span),
            TypeError::NotAnEnum { span, .. } => Some(*span),
            TypeError::NotAMethod { span, .. } => Some(*span),
            TypeError::NoMatchingMethod { span, .. } => Some(*span),
            TypeError::NotATrait { span, .. } => Some(*span),
            TypeError::TypeDoesNotImplementTrait { span, .. } => Some(*span),
            TypeError::AmbiguousTraitImpl { span, .. } => Some(*span),
            TypeError::RequiredTraitNotImplemented { span, .. } => Some(*span),
            TypeError::WrongPatternType { span, .. } => Some(*span),
            TypeError::OrPatternBindingMismatch { span, .. } => Some(*span),
            TypeError::NonExhaustivePatterns { span, .. } => Some(*span),
            TypeError::InferenceRecursionLimit { span, .. } => Some(*span),
            TypeError::InternalError { span, .. } => *span,
            TypeError::UnsupportedOperator { span, .. } => Some(*span),
            TypeError::AmbiguousMethodCall { span, .. } => Some(*span),
            TypeError::GenericArgCountMismatch { span, .. } => Some(*span),
            TypeError::ReturnTypeMismatch { span, .. } => Some(*span),
            TypeError::SelfOutsideImplOrTrait { span, .. } => Some(*span),
            TypeError::ResolverError { span, .. } => Some(*span),
            TypeError::UnknownTrait { span, .. } => Some(*span),
            TypeError::GenericParamCountMismatch { span, .. } => Some(*span),
            TypeError::ParamCountMismatch { span, .. } => Some(*span),
            TypeError::MethodParamMismatch { span, .. } => Some(*span),
            TypeError::MethodReturnTypeMismatch { span, .. } => Some(*span),
            TypeError::UnsupportedExpression { span, .. } => Some(*span),
            TypeError::UnsupportedLiteral { span, .. } => Some(*span),
            TypeError::NotAValue { span, .. } => Some(*span),
            TypeError::UnknownIdentifier { span, .. } => Some(*span),
            TypeError::MissingMethodImpl { span, .. } => Some(*span),
            TypeError::MissingAssocTypeImpl { span, .. } => Some(*span),
            TypeError::TraitUsedAsType { span, .. } => Some(*span),
            TypeError::GenericBoundMismatch { span, .. } => Some(*span),
            TypeError::ImplGenericParamResolutionError { span, .. } => Some(*span),
            TypeError::LiteralDefaultConflict { span, .. } => Some(*span),
        }
    }
}

/// Helper function to format a type for display in error messages.
pub fn display_type(ty: &Ty) -> String {
    match &ty.kind {
        TyKind::Primitive(p) => format!("{}", p),
        TyKind::InferInt(_) => "{integer}".to_string(),
        TyKind::InferFloat(_) => "{float}".to_string(),
        TyKind::Named { name, args, .. } => {
            if args.is_empty() {
                name.clone()
            } else {
                format!("{}<{}>", name, args.iter().map(display_type).collect::<Vec<_>>().join(", "))
            }
        }
        TyKind::Function(params, ret) => {
            format!("fn({}) -> {}", params.iter().map(display_type).collect::<Vec<_>>().join(", "), display_type(ret))
        }
        TyKind::Tuple(elements) => {
            format!("({})", elements.iter().map(display_type).collect::<Vec<_>>().join(", "))
        }
        TyKind::Array(elem_ty, size_opt) => {
            format!("[{}; {:?}]", display_type(elem_ty), size_opt)
        }
        TyKind::Map(key_ty, val_ty) => format!("Map<{}, {}>", display_type(key_ty), display_type(val_ty)),
        TyKind::Set(elem_ty) => format!("Set<{}>", display_type(elem_ty)),
        TyKind::Pointer(inner, ptr_type) => {
            let prefix = match ptr_type {
                crate::types::core::PointerType::Const => "*const",
                crate::types::core::PointerType::Mutable => "*mut",
            };
            format!("{} {}", prefix, display_type(inner))
        }
        TyKind::GenericParam(name) => name.clone(),
        TyKind::Var(TypeId(id)) => format!("$t{}", id),
        TyKind::SelfType => "Self".to_string(),
        TyKind::Never => "!".to_string(),
        TyKind::Error => "<error>".to_string(),
    }
} 