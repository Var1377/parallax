use miette::SourceSpan;

// --- Path Definition ---

/// Represents a unique identifier for a definition (e.g., function, struct, module).
///
/// Symbols are generated uniquely for each definition during the resolution process.
/// They provide a way to refer to specific definitions unambiguously, even if multiple
/// definitions share the same name in different scopes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(pub u32);

impl Symbol {
    /// Creates a new symbol from an integer
    pub const fn new(id: u32) -> Self {
        Self(id)
    }
    
    /// Gets the underlying integer value
    pub const fn id(&self) -> u32 {
        self.0
    }
}

// --- Static counter for generating unique symbols ---
use std::sync::atomic::{AtomicU32, Ordering};

static NEXT_SYMBOL: AtomicU32 = AtomicU32::new(1); // Start at 1, reserve 0 for error/invalid

impl Symbol {
    /// Creates a new globally unique symbol.
    ///
    /// This should be the primary way to generate new symbols during resolution.
    pub fn fresh() -> Self {
        let id = NEXT_SYMBOL.fetch_add(1, Ordering::SeqCst);
        Symbol(id)
    }
}

// --- Type Resolution ---

/// A reference to a resolved type after semantic analysis.
///
/// This enum represents the various kinds of types that can exist in the Parallax
/// language once they have been fully resolved from the AST representation.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedType {
    /// A built-in primitive type.
    Primitive(PrimitiveType),
    /// A user-defined type (struct, enum, or trait).
    UserDefined {
        /// The unique symbol identifying the type definition (struct, enum, trait).
        symbol: Symbol,
        /// Resolved type arguments, if the type is generic (e.g., `List<i32>`). `None` if not generic or arguments aren't specified/resolved yet.
        type_args: Option<Vec<ResolvedType>>,
    },
    /// A function type (e.g., `fn(i32) -> bool`).
    Function {
        /// The types of the function's parameters.
        param_types: Vec<ResolvedType>,
        /// The return type of the function.
        return_type: Box<ResolvedType>,
    },
    /// A tuple type (e.g., `(i32, String)`).
    Tuple(Vec<ResolvedType>),
    /// An array type (e.g., `[i32; 5]`).
    Array {
        /// The type of the elements in the array.
        element_type: Box<ResolvedType>,
        /// The size of the array, if known at compile time.
        size: Option<usize>,
    },
    /// A generic type parameter (e.g., `T` in `struct List<T> { ... }`).
    /// The `String` holds the name of the parameter.
    GenericParam(String),
    /// Represents an unresolved type due to errors during resolution or a type
    /// that could not be inferred.
    Unknown,
    /// Represents the never type `!` which indicates divergence (e.g., a function that never returns).
    Never,
    /// Represents the `Self` type used within a trait definition or an impl block.
    SelfType,
}

/// Primitive types supported by the language.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    /// Integer type (size may depend on target architecture or be specified later).
    Int,
    /// Floating-point type (size may depend on target architecture or be specified later).
    Float,
    /// Boolean type (`true` or `false`).
    Bool,
    /// Character type.
    Char,
    /// String type.
    String,
    /// The unit type `()`, representing the absence of a value.
    Unit,
}

// --- Resolved Generic Parameter Definition ---

/// Represents a resolved generic parameter definition (e.g., `<T: Display>`).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedGenericParamDef {
    /// The name of the generic parameter (e.g., "T").
    pub name: String,
    /// List of trait symbols that bound this parameter (e.g., the `Symbol` for `Display`).
    pub bounds: Vec<Symbol>,
    /// The source span where this generic parameter was defined.
    pub span: SourceSpan,
}

// --- Resolved Definitions (Top Level) ---

/// Represents a resolved parameter in a function definition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedParameter {
    /// The unique symbol identifying this parameter.
    pub symbol: Symbol,
    /// The name of the parameter. Might be derived from a pattern.
    pub name: String,
    /// The resolved type of the parameter.
    pub param_type: ResolvedType,
    /// Whether this parameter is variadic (e.g., `...rest`).
    pub is_variadic: bool,
    /// Whether this parameter has a default value.
    pub has_default: bool,
    /// The source span of the parameter definition.
    pub span: SourceSpan,
}

/// Represents a resolved struct field.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedField {
    /// The name of the field.
    pub name: String,
    /// The resolved type of the field.
    pub field_type: ResolvedType,
    /// Whether the field is public.
    pub is_public: bool,
    /// The source span of the field definition.
    pub span: SourceSpan,
}

/// Represents a resolved enum variant.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedEnumVariant {
    /// A unit variant (e.g., `Option::None`).
    Unit {
        /// The name of the variant.
        name: String,
        /// The unique symbol for this variant.
        symbol: Symbol,
        /// The source span of the variant definition.
        span: SourceSpan,
    },
    /// A tuple variant (e.g., `Result::Ok(T)`).
    Tuple {
        /// The name of the variant.
        name: String,
        /// The unique symbol for this variant.
        symbol: Symbol,
        /// The resolved types of the fields within the tuple.
        fields: Vec<ResolvedType>,
        /// The source span of the variant definition.
        span: SourceSpan,
    },
    /// A struct variant (e.g., `Message::Data { id: i32, payload: String }`).
    Struct {
        /// The name of the variant.
        name: String,
        /// The unique symbol for this variant.
        symbol: Symbol,
        /// The resolved fields within the variant's struct.
        fields: Vec<ResolvedField>,
        /// The source span of the variant definition.
        span: SourceSpan,
    },
}

/// A fully resolved struct definition after semantic analysis.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedStruct {
    /// The unique symbol identifying this struct definition.
    pub symbol: Symbol,
    /// The name of the struct.
    pub name: String,
    /// The symbol of the module where this struct is defined.
    pub module_symbol: Symbol,
    /// The resolved fields of the struct.
    pub fields: Vec<ResolvedField>,
    /// The resolved generic parameters defined by the struct (e.g., `<T>`).
    pub generic_params: Vec<ResolvedGenericParamDef>,
    /// Whether the struct definition is public.
    pub is_public: bool,
    /// The source span of the struct definition.
    pub span: SourceSpan,
}

/// A fully resolved enum definition after semantic analysis.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedEnum {
    /// The unique symbol identifying this enum definition.
    pub symbol: Symbol,
    /// The name of the enum.
    pub name: String,
    /// The symbol of the module where this enum is defined.
    pub module_symbol: Symbol,
    /// The resolved variants of the enum.
    pub variants: Vec<ResolvedEnumVariant>,
    /// The resolved generic parameters defined by the enum (e.g., `<T, E>`).
    pub generic_params: Vec<ResolvedGenericParamDef>,
    /// Whether the enum definition is public.
    pub is_public: bool,
    /// The source span of the enum definition.
    pub span: SourceSpan,
}

/// A fully resolved function definition (can be standalone or associated) after semantic analysis.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedFunction {
    /// The unique symbol identifying this function definition.
    pub symbol: Symbol,
    /// The name of the function.
    pub name: String,
    /// The symbol of the module where this function's source code is defined.
    /// For associated functions (methods), this is the module containing the trait or impl block.
    pub module_symbol: Symbol,
    /// The resolved parameters of the function.
    pub parameters: Vec<ResolvedParameter>,
    /// The resolved return type of the function.
    pub return_type: ResolvedType,
    /// The resolved body of the function, represented as a `ResolvedExpr`.
    /// This is `None` if the function is defined in a trait signature or if
    /// body resolution hasn't occurred or failed.
    pub body: Option<ResolvedExpr>,
    /// The resolved generic parameters defined directly on the function (e.g., `fn foo<A>()`).
    /// Generic parameters from parent contexts (like impls or traits) are handled separately during resolution.
    pub generic_params: Vec<ResolvedGenericParamDef>,
    /// Whether the function definition is public.
    pub is_public: bool,
    /// The source span of the function definition.
    pub span: SourceSpan,
    /// Whether the function has side effects (determined during resolution).
    pub is_effectful: bool,
}

// --- Resolved Traits and Implementations ---

/// Represents a resolved associated function signature within a trait or impl.
///
/// This primarily links to the actual `ResolvedFunction` definition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedAssociatedFunction {
    /// The `Symbol` of the `ResolvedFunction` corresponding to this associated function.
    pub func_symbol: Symbol,
    /// If this associated function is part of an `impl Trait`, this optionally holds the `Symbol`
    /// of the corresponding method definition in the trait itself. This helps link impl methods
    /// back to the trait methods they implement. `None` for inherent impl methods or trait methods.
    pub trait_method_symbol: Option<Symbol>,
}

/// A fully resolved trait definition after semantic analysis.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedTrait {
    /// The unique symbol identifying this trait definition.
    pub symbol: Symbol,
    /// The name of the trait.
    pub name: String,
    /// The symbol of the module where this trait is defined.
    pub module_symbol: Symbol,
    /// The resolved generic parameters defined by the trait (e.g., `<T>`).
    pub generic_params: Vec<ResolvedGenericParamDef>,
    /// The associated functions (methods) defined in this trait's signature.
    pub methods: Vec<ResolvedAssociatedFunction>,
    // TODO: Add associated types: pub associated_types: Vec<ResolvedAssociatedType>,
    /// Whether the trait definition is public.
    pub is_public: bool,
    /// The source span of the trait definition.
    pub span: SourceSpan,
}

/// A fully resolved trait implementation block (`impl Trait for Type` or `impl Type`) after semantic analysis.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedImpl {
    /// A unique symbol identifying this specific impl block.
    pub impl_symbol: Symbol,
    /// The resolved generic parameters defined on the impl block itself (e.g., `impl<T>`).
    pub generic_params: Vec<ResolvedGenericParamDef>,
    /// The symbol of the trait being implemented, if any. `None` for inherent impls (`impl Type`).
    pub trait_symbol: Option<Symbol>,
    /// The type for which the trait is implemented (e.g., `MyStruct<T>`). This is the `Self` type within the impl block.
    pub implementing_type: ResolvedType,
    /// The methods defined in this impl block.
    pub methods: Vec<ResolvedAssociatedFunction>,
    // TODO: Add associated type bindings: pub associated_type_bindings: Vec<(String, ResolvedType)>,
    /// The source span of the impl block definition.
    pub span: SourceSpan,
}

// --- Resolved Expressions and Patterns ---

/// Represents a resolved expression node in the AST, augmented with type information.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedExpr {
    /// The kind of resolved expression.
    pub kind: ResolvedExprKind,
    /// The source span of the original expression.
    pub span: SourceSpan,
    /// The resolved type of this expression.
    pub resolved_type: ResolvedType,
}

/// The different kinds of resolved expressions.
/// These mirror the AST `ExprKind` but typically contain resolved symbols or types.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedExprKind {
    /// A block expression `{ ... }`. Contains a sequence of resolved expressions.
    Block(Vec<ResolvedExpr>),
    /// An `if` expression.
    If {
        /// The resolved condition expression.
        condition: Box<ResolvedExpr>,
        /// The resolved `then` branch expression.
        then_branch: Box<ResolvedExpr>,
        /// The optional resolved `else` branch expression.
        else_branch: Option<Box<ResolvedExpr>>,
    },
    /// A `match` expression.
    Match {
        /// The resolved expression being matched upon (the scrutinee).
        scrutinee: Box<ResolvedExpr>,
        /// The arms of the match, each containing a resolved pattern and a resolved expression.
        arms: Vec<(ResolvedPattern, ResolvedExpr)>,
    },
    /// A binary operation (e.g., `a + b`).
    Binary {
        /// The resolved left-hand side expression.
        left: Box<ResolvedExpr>,
        /// The binary operator.
        op: parallax_syntax::ast::expr::BinaryOp,
        /// The resolved right-hand side expression.
        right: Box<ResolvedExpr>,
    },
    /// A unary operation (e.g., `-x`, `!flag`).
    Unary {
        /// The unary operator.
        op: parallax_syntax::ast::expr::UnaryOp,
        /// The resolved expression being operated upon.
        expr: Box<ResolvedExpr>,
    },
    /// A function call expression.
    Call {
        // TODO: This needs to resolve to the function expression/path correctly.
        // Currently it's just a symbol, which might not be enough for complex cases
        // (e.g., calling a closure stored in a variable).
        /// The symbol of the function being called, if resolved directly to a named function. `None` otherwise (e.g., calling a closure).
        func_symbol: Option<Symbol>,
        /// The resolved arguments passed to the function.
        args: Vec<ResolvedArgument>,
    },
    /// A method call expression (e.g., `obj.method(arg)`).
    MethodCall {
        /// The resolved object expression upon which the method is called.
        object: Box<ResolvedExpr>,
        /// The name of the method being called.
        method_name: String,
        /// The resolved symbol of the specific method implementation being called.
        /// This could be from an inherent impl or a trait impl. `None` if resolution failed.
        resolved_method_symbol: Option<Symbol>,
        /// The resolved arguments passed to the method (excluding the receiver `object`).
        args: Vec<ResolvedArgument>,
    },
    /// A lambda or closure expression (e.g., `|x| => x + 1`).
    Lambda {
        // TODO: Resolve generic params if lambdas can have them.
        /// The resolved parameters of the lambda.
        params: Vec<ResolvedParameter>,
        /// The resolved body expression of the lambda.
        body: Box<ResolvedExpr>,
    },
    /// A literal value (e.g., `10`, `"hello"`, `true`).
    Literal(parallax_syntax::ast::common::Literal),
    /// A path expression resolving to a definition (e.g., a variable, constant, function, enum variant).
    Path(Symbol),
    /// A field access expression (e.g., `my_struct.field`).
    Field {
        /// The resolved object expression whose field is being accessed.
        object: Box<ResolvedExpr>,
        /// The name of the field being accessed.
        field_name: String,
    },
    /// An array literal expression (e.g., `[1, 2, 3]`).
    Array(Vec<ResolvedExpr>),
    /// A tuple literal expression (e.g., `(1, "a")`).
    Tuple(Vec<ResolvedExpr>),
    /// A map literal expression (e.g., `{"key": value}`).
    Map(Vec<(ResolvedExpr, ResolvedExpr)>),
    /// A hash set literal expression (e.g., `#{item1, item2}`).
    HashSet(Vec<ResolvedExpr>),
    /// A `let` binding expression. Note: In many languages, `let` is a statement,
    /// but here it might be represented as an expression that evaluates to unit or the bound value.
    Let {
        /// The resolved pattern used for binding.
        pattern: ResolvedPattern,
        /// The resolved value being assigned.
        value: Box<ResolvedExpr>,
        /// The optional resolved type annotation provided in the `let` statement.
        type_annotation: Option<ResolvedType>,
    },
    /// A struct instantiation expression (e.g., `MyStruct { field: value }`).
    Struct {
        /// The resolved symbol of the struct being instantiated.
        struct_symbol: Symbol,
        /// The resolved fields provided in the instantiation.
        fields: Vec<(String, ResolvedExpr)>,
        /// An optional base struct expression for functional update (e.g., `MyStruct { field: value, ..base }`).
        base: Option<Box<ResolvedExpr>>,
    },
    /// A parenthesized expression `(expr)`.
    Paren(Box<ResolvedExpr>),
    /// An enum variant constructor call (e.g., `Option::Some(value)`).
    VariantConstructor {
        /// The resolved symbol of the specific enum variant being constructed.
        variant_symbol: Symbol,
        /// The resolved arguments passed to the variant constructor.
        args: Vec<ResolvedArgument>,
    },
}

/// Represents a resolved argument in a function or method call.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedArgument {
    /// The optional name if it's a named argument (e.g., `name: value`).
    pub name: Option<String>,
    /// The resolved expression for the argument's value.
    pub value: ResolvedExpr,
    /// The source span of the original argument.
    pub span: SourceSpan,
}

/// Represents a resolved pattern node in the AST, augmented with type information.
/// Used in `let` bindings, `match` arms, and function parameters.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedPattern {
    /// The kind of resolved pattern.
    pub kind: ResolvedPatternKind,
    /// The source span of the original pattern.
    pub span: SourceSpan,
    /// The resolved type that this pattern matches against.
    pub resolved_type: ResolvedType,
}

/// The different kinds of resolved patterns.
/// These mirror the AST `PatternKind` but contain resolved symbols and sub-patterns.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedPatternKind {
    /// An identifier pattern, binding a variable (e.g., `let x = ...`).
    Identifier(String),
    /// A literal pattern (e.g., `match x { 1 => ... }`).
    Literal(parallax_syntax::ast::common::Literal),
    /// An enum variant or struct constructor pattern (e.g., `Some(y)`, `Point { x, .. }`).
    Constructor {
        /// The resolved symbol of the enum variant or struct being matched.
        symbol: Symbol,
        /// The resolved sub-patterns for the arguments or fields.
        args: Box<ResolvedPattern>, // Box<ResolvedPattern> ? Or Vec<ResolvedPattern>? Needs clarification based on AST.
    },
    /// An array pattern (e.g., `[a, b, ..rest]`).
    Array(Vec<ResolvedPattern>),
    /// A tuple pattern (e.g., `(x, _, z)`).
    Tuple(Vec<ResolvedPattern>),
    /// A struct pattern (e.g., `Point { x: 0, y }`).
    Struct {
        /// The resolved symbol of the struct being matched.
        struct_symbol: Symbol,
        /// The resolved field patterns within the struct pattern.
        fields: Vec<ResolvedPatternField>,
        // TODO: Add rest pattern indication `..` if needed.
    },
    /// A rest pattern `..` used in array or tuple patterns.
    Rest,
    /// An OR-pattern `pat1 | pat2`.
    Or(Box<ResolvedPattern>, Box<ResolvedPattern>),
    /// A wildcard pattern `_`.
    Wildcard,
}

/// Represents a field within a resolved struct pattern (e.g., `x: pattern` or just `x`).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedPatternField {
    /// The name of the field being matched.
    pub name: String,
    /// The optional resolved sub-pattern for the field's value.
    /// If `None`, it implies shorthand (field name is used as the binding identifier).
    pub pattern: Option<ResolvedPattern>,
    /// The source span of this field pattern element.
    pub span: SourceSpan,
}

// --- Final Resolution Output ---

/// Collection of all resolved definitions resulting from the resolution passes.
/// This structure holds the core semantic information about the user's code.
#[derive(Debug, Clone, Default, Hash, PartialEq, Eq)]
pub struct ResolvedDefinitions {
    /// All resolved struct definitions.
    pub structs: Vec<ResolvedStruct>,
    /// All resolved enum definitions.
    pub enums: Vec<ResolvedEnum>,
    /// All resolved function definitions (including associated functions/methods).
    pub functions: Vec<ResolvedFunction>,
    /// All resolved trait definitions.
    pub traits: Vec<ResolvedTrait>,
    /// All resolved impl blocks (trait implementations and inherent implementations).
    pub impls: Vec<ResolvedImpl>,
}

/// Represents the final output of the name resolution and type checking process
/// for a given module structure (typically a crate).
///
/// This is the main artifact produced by the `parallax-resolve` crate.
#[salsa::tracked]
pub struct ResolvedModuleStructure<'db> {
    /// The collection of fully resolved definitions (structs, enums, functions, etc.).
    #[return_ref]
    pub definitions: ResolvedDefinitions,
    /// Any fatal errors encountered during resolution. If this is non-empty,
    /// the `definitions` might be incomplete or inconsistent.
    #[return_ref]
    pub errors: Vec<crate::error::ResolutionError>,
    /// Any non-fatal warnings detected during resolution.
    #[return_ref]
    pub warnings: Vec<crate::error::ResolverWarning>,
} 