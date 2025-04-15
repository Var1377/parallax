use thiserror::Error;
use miette::{Diagnostic, SourceSpan};

/// Errors that can occur during name and type resolution.
#[derive(Debug, Error, Diagnostic, Clone, Hash, PartialEq, Eq)]
pub enum ResolutionError {
    /// Indicates that a name referenced in the source code could not be found
    /// in the current scope or any accessible parent scopes/imports.
    #[error("Name not found: Could not find `{name}` in the current scope")]
    #[diagnostic(code(parallax_resolve::name_not_found))]
    NameNotFound {
        /// The name that could not be found.
        name: String,
        #[label("referenced here")]
        span: SourceSpan,
        #[help("Is `{name}` defined or imported correctly?")]
        /// Optional help message, potentially suggesting scopes searched or possible typos.
        help: Option<String>,
    },

    /// Indicates that the same name has been defined multiple times within the same scope
    /// (e.g., two functions with the same name in one module).
    #[error("Duplicate definition: `{name}` is defined multiple times")]
    #[diagnostic(code(parallax_resolve::duplicate_definition))]
    DuplicateDefinition {
        /// The name that was defined multiple times.
        name: String,
        #[label("current definition here")]
        span: SourceSpan,
        #[label("previously defined here")]
        previous_span: SourceSpan,
    },

    /// Indicates an error occurred while trying to resolve a `use` import statement.
    #[error("Import error: Failed to resolve import path `{path}`")]
    #[diagnostic(code(parallax_resolve::import_error))]
    ImportError {
        /// The import path string that failed to resolve.
        path: String,
        #[label("import referenced here")]
        span: SourceSpan,
        #[help("Check if the module and item exist and are accessible")]
        /// Optional reason for the failure (e.g., item not found, path ambiguous, not a module).
        reason: Option<String>,
    },
    
    /// Indicates that a name resolves to multiple possible items in the current scope,
    /// making the reference ambiguous (e.g., importing two different items with the same alias).
    #[error("Ambiguous reference: `{name}` could refer to multiple items")]
    #[diagnostic(code(parallax_resolve::ambiguous_reference))]
    AmbiguousReference {
        /// The ambiguous name.
        name: String,
        #[label("referenced here")]
        span: SourceSpan,
        /// A list of the fully qualified paths of the candidate items.
        candidates: Vec<String>, // List of possible paths as strings
    },
    
    /// Indicates a mismatch between the expected type and the actual type found
    /// for an expression, pattern, or definition.
    #[error("Type mismatch: Expected `{expected}`, found `{found}`")]
    #[diagnostic(code(parallax_resolve::type_mismatch))]
    TypeMismatch {
        /// String representation of the expected `ResolvedType`.
        expected: String,
        /// String representation of the found `ResolvedType`.
        found: String,
        #[label("this expression has type `{found}`")]
        span: SourceSpan,
        #[help("Expected type `{expected}` based on context")]
        /// Optional description of the context where the mismatch occurred (e.g., "in function argument", "in variable assignment").
        context: Option<String>,
    },

    /// Indicates that a segment within a multi-part path (e.g., `a::b::c`)
    /// does not refer to a valid or accessible module or definition type
    /// through which the path can continue.
    #[error("Invalid path segment: `{segment}` in path `{path}` is not a valid module or definition")]
    #[diagnostic(code(parallax_resolve::invalid_path_segment))]
    InvalidPathSegment {
        /// The specific segment within the path that is invalid.
        segment: String,
        /// The full path string being resolved.
        path: String,
        #[label("`{segment}` is not recognized here")]
        span: SourceSpan,
    },
    
    /// Indicates an attempt to access an item (module, function, type, etc.) that is not public
    /// from a scope where it is not visible.
    #[error("Private item access: Cannot access private item `{path}` from the current scope")]
    #[diagnostic(code(parallax_resolve::private_item_access))]
    PrivateItemAccess {
        /// The path to the private item being accessed.
        path: String, // Path to the private item
        #[label("attempted access here")]
        span: SourceSpan,
    },

    /// Indicates a circular dependency detected during the resolution process,
    /// such as mutually recursive type definitions or imports without clear base cases.
    #[error("Resolution cycle detected involving `{path}`")]
    #[diagnostic(code(parallax_resolve::resolution_cycle))]
    ResolutionCycle {
        /// A representative path involved in the cycle.
        path: String, // Path involved in the cycle
        #[label("cycle detected here")]
        span: SourceSpan,
        /// A list of paths representing the detected cycle.
        cycle_path: Vec<String>, // Paths involved in the cycle
    },
    
    /// A catch-all for internal errors within the resolver, indicating a potential bug
    /// or unexpected state.
    #[error("Internal resolver error: {message}")]
    #[diagnostic(code(parallax_resolve::internal_error))]
    InternalError {
        /// The detailed error message.
        message: String,
        /// Optional source span for context, if available.
        span: Option<SourceSpan>, // Optional span for context
    },

    /// Indicates an error related to pattern matching, such as incorrect structure,
    /// type mismatches within patterns, or non-exhaustive match arms (if checked here).
    #[error("Pattern mismatch: {message}")]
    #[diagnostic(code(parallax_resolve::pattern_mismatch))]
    PatternMismatch {
        /// A message describing the specific pattern error.
        message: String,
        #[label("invalid pattern")]
        span: SourceSpan,
        #[help("{help}")]
        /// Optional help message.
        help: Option<String>,
    },
    
    /// Indicates accessing or matching on a field that does not exist in a struct or struct variant.
    #[error("Unknown field: `{field_name}` is not a field of struct `{struct_name}`")]
    #[diagnostic(code(parallax_resolve::unknown_field))]
    UnknownField {
        /// The name of the field that was not found.
        field_name: String,
        /// The name of the struct being accessed.
        struct_name: String,
        #[label("unknown field")]
        span: SourceSpan,
    },
    
    /// Indicates that a required field is missing in a struct literal or struct pattern.
    #[error("Missing field: `{field_name}` is required in struct `{struct_name}`")]
    #[diagnostic(code(parallax_resolve::missing_field))]
    MissingField {
        /// The name of the missing field.
        field_name: String,
        /// The name of the struct definition.
        struct_name: String,
        #[label("struct is missing required field")]
        span: SourceSpan,
    },
    
    /// Indicates a mismatch in the number of elements in an array pattern
    /// compared to the expected size of the array type.
    #[error("Array size mismatch: Expected {expected} elements, found {found}")]
    #[diagnostic(code(parallax_resolve::array_size_mismatch))]
    ArraySizeMismatch {
        /// The expected number of elements.
        expected: usize,
        /// The number of elements found in the pattern.
        found: usize,
        #[label("array pattern has wrong size")]
        span: SourceSpan,
    },
    
    /// Indicates a mismatch in the structure of a constructor pattern (enum variant or struct)
    /// compared to the definition (e.g., trying to match a tuple variant with a struct pattern).
    #[error("Constructor mismatch: {message}")]
    #[diagnostic(code(parallax_resolve::constructor_mismatch))]
    ConstructorMismatch {
        /// A message describing the mismatch.
        message: String,
        #[label("invalid constructor pattern")]
        span: SourceSpan,
    },
    
    /// General error related to constructors, potentially covering arity or type issues not
    /// covered by `ConstructorMismatch` or argument errors.
    #[error("Constructor error: {message}")]
    #[diagnostic(code(parallax_resolve::constructor_error))]
    ConstructorError {
        /// A message describing the constructor error.
        message: String,
        #[label("constructor error")]
        span: SourceSpan,
    },
    
    /// Indicates that too many arguments were provided in a function/method call or constructor pattern.
    #[error("Too many arguments: expected {expected}, found {found}")]
    #[diagnostic(code(parallax_resolve::too_many_arguments))]
    TooManyArguments {
        /// The expected number of arguments.
        expected: usize,
        /// The number of arguments actually found.
        found: usize,
        #[label("too many arguments")]
        span: SourceSpan,
    },
    
    /// Indicates that too few arguments were provided in a function/method call or constructor pattern.
    #[error("Too few arguments: expected {expected}, found {found}")]
    #[diagnostic(code(parallax_resolve::too_few_arguments))]
    TooFewArguments {
        /// The expected number of arguments.
        expected: usize,
        /// The number of arguments actually found.
        found: usize,
        #[label("too few arguments")]
        span: SourceSpan,
    },

    /// Indicates a syntax error occurred during parsing of a source file.
    /// This often happens before full resolution can proceed for that file.
    #[error("Syntax error in {}: {message}", location.as_deref().unwrap_or("<unknown location>"))]
    #[diagnostic(code(parallax_resolve::syntax_error))]
    SyntaxError {
        /// A message describing the syntax error.
        message: String,
        /// The source span where the error occurred.
        span: Option<SourceSpan>,
        /// The file/location where the error happened.
        location: Option<String>,
    },
}

/// Non-fatal warnings detected during name resolution and type checking.
#[derive(Debug, Error, Diagnostic, Clone, Hash, PartialEq, Eq)]
pub enum ResolverWarning {
    /// Indicates that a local variable was defined (e.g., with `let`) but never read.
    #[error("Unused variable: `{name}` is defined but never used")]
    #[diagnostic(code(parallax_resolve::unused_variable))]
    UnusedLocalVariable {
        /// The name of the unused variable.
        name: String,
        #[label("unused variable")]
        span: SourceSpan,
    },
    
    /// Indicates that a variable definition shadows a previous definition with the same name
    /// in an outer scope.
    #[error("Shadowed variable: `{name}` shadows a previous definition")]
    #[diagnostic(code(parallax_resolve::shadowed_variable))]
    ShadowedVariable {
        /// The name of the variable involved in shadowing.
        name: String,
        #[label("original definition")]
        original_span: SourceSpan,
        #[label("shadowing definition")]
        shadow_span: SourceSpan,
    },
    
    /// Indicates code that will never be executed because it follows a
    /// diverging expression (like `return`, `panic`, or a loop without a break).
    #[error("Unreachable code")]
    #[diagnostic(code(parallax_resolve::unreachable_code))]
    UnreachableCode {
        #[label("this code will never be executed")]
        span: SourceSpan,
    },
    
    /// Indicates that an item was imported using `use` but was never referenced
    /// within the module.
    #[error("Unused import: `{path}` is imported but never used")]
    #[diagnostic(code(parallax_resolve::unused_import))]
    UnusedImport {
        /// The path string of the unused import.
        path: String,
        #[label("unused import")]
        span: SourceSpan,
    },
    
    /// Indicates a type mismatch that, while potentially allowed through coercion or subtyping
    /// in some languages, might be suspicious or indicative of a logic error.
    /// (Note: Exact applicability depends on the language's type system rules).
    #[error("Suspicious type mismatch: expected `{expected}`, found `{found}`")]
    #[diagnostic(code(parallax_resolve::suspicious_type_mismatch))]
    SuspiciousTypeMismatch {
        /// String representation of the expected type.
        expected: String,
        /// String representation of the found type.
        found: String,
        #[label("types don't match")]
        span: SourceSpan,
    },
} 