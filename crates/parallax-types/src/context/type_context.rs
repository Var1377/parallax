// src/context/type_context.rs
//! Manages definitions of types (structs, enums) and standalone functions.

use std::collections::HashMap;
use miette::SourceSpan;
use parallax_resolve::{types::Symbol, definitions::DefinitionKind};

// Use crate paths
use crate::types::{TypeDef, StructDef, EnumDef, FunctionSignature};
use crate::error::{TypeError, TypeResult};
use crate::context::TraitRepository;

/// Stores known type and function definitions accessible by their symbol.
/// Does not handle traits or impls (see `TraitRepository`).
#[derive(Debug, Clone, Default)]
pub struct TypeContext {
    /// Mapping from a definition's Symbol to its `TypeDef`.
    definitions: HashMap<Symbol, TypeDef>,
    /// Mapping from a definition's Symbol to its original `DefinitionKind` from the resolver.
    /// This helps distinguish between, e.g., a Struct symbol and an Enum symbol even
    /// if they somehow had the same ID (which shouldn't happen with Salsa symbols).
    definition_kinds: HashMap<Symbol, DefinitionKind>,
}

impl TypeContext {
    /// Creates a new, empty type context.
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a type or function definition to the context.
    ///
    /// Preconditions: `symbol` is unique for this definition. `kind` matches the kind of `def`.
    /// Postconditions: The context contains the mapping `symbol` -> `def` and `symbol` -> `kind`.
    /// Assertions: Checks for symbol collisions and kind mismatches.
    pub fn add_definition(&mut self, symbol: Symbol, kind: DefinitionKind, def: TypeDef) -> TypeResult<()> {
        // Assertion: Ensure the symbol doesn't already exist with a *different* kind.
        if let Some(existing_kind) = self.definition_kinds.get(&symbol) {
            if *existing_kind != kind {
                // This indicates a resolver bug or misuse of symbols.
                return Err(TypeError::InternalError {
                    message: format!("Symbol {:?} reused with different kinds ({:?} vs {:?})", symbol, existing_kind, kind),
                    span: None, // No specific span for this internal error
                });
            }
            // If it exists with the *same* kind, maybe issue a warning or specific error?
            // For now, allow overwrite for simplicity, assuming resolver guarantees uniqueness.
        }

        // Assertion: Verify the TypeDef variant matches the DefinitionKind.
        match (&def, kind) {
            (TypeDef::Struct(_), DefinitionKind::Struct) => {},
            (TypeDef::Enum(_), DefinitionKind::Enum) => {},
            (TypeDef::Function(_), DefinitionKind::Function) => {}, // Functions map to Function kind
            // Mismatches
            (TypeDef::Struct(_), _) => return Err(TypeError::InternalError { message: format!("Kind mismatch for Struct {:?}: got {:?}", symbol, kind), span: None }),
            (TypeDef::Enum(_), _) => return Err(TypeError::InternalError { message: format!("Kind mismatch for Enum {:?}: got {:?}", symbol, kind), span: None }),
            (TypeDef::Function(_), _) => return Err(TypeError::InternalError { message: format!("Kind mismatch for Function {:?}: got {:?}", symbol, kind), span: None }),
        }

        self.definitions.insert(symbol, def);
        self.definition_kinds.insert(symbol, kind);
        Ok(())
    }

    /// Looks up a definition by its symbol.
    ///
    /// Preconditions: `symbol` is the symbol to look up.
    /// Postconditions: Returns `Some(&TypeDef)` if found, `None` otherwise.
    pub fn get_definition(&self, symbol: &Symbol) -> Option<&TypeDef> {
        self.definitions.get(symbol)
    }

    /// Gets the definition kind for a symbol.
    ///
    /// Preconditions: `symbol` is the symbol to look up.
    /// Postconditions: Returns `Some(&DefinitionKind)` if found, `None` otherwise.
    pub fn get_definition_kind(&self, symbol: &Symbol) -> Option<&DefinitionKind> {
        self.definition_kinds.get(symbol)
    }

    /// Looks up a struct definition specifically.
    /// Returns an error if the symbol exists but isn't a struct.
    ///
    /// Preconditions: `symbol` is the symbol to look up. `error_span` for reporting.
    /// Postconditions: Returns `Ok(&StructDef)` if found and is a struct.
    ///                 Returns `Err(TypeError::InternalError)` if not found or wrong kind.
    pub fn get_struct_def(&self, symbol: &Symbol, error_span: SourceSpan) -> TypeResult<&StructDef> {
        match self.definitions.get(symbol) {
            Some(TypeDef::Struct(sd)) => Ok(sd),
            Some(_) => Err(TypeError::InternalError { message: format!("Expected struct, found different definition kind for symbol {:?}", symbol), span: Some(error_span)}),
            None => Err(TypeError::InternalError { message: format!("Struct definition not found for symbol {:?}", symbol), span: Some(error_span) }),
        }
    }

    /// Looks up an enum definition specifically.
    /// Returns an error if the symbol exists but isn't an enum.
    ///
    /// Preconditions: `symbol` is the symbol to look up. `error_span` for reporting.
    /// Postconditions: Returns `Ok(&EnumDef)` if found and is an enum.
    ///                 Returns `Err(TypeError::InternalError)` if not found or wrong kind.
    pub fn get_enum_def(&self, symbol: &Symbol, error_span: SourceSpan) -> TypeResult<&EnumDef> {
        match self.definitions.get(symbol) {
            Some(TypeDef::Enum(ed)) => Ok(ed),
            Some(_) => Err(TypeError::InternalError { message: format!("Expected enum, found different definition kind for symbol {:?}", symbol), span: Some(error_span)}),
            None => Err(TypeError::InternalError { message: format!("Enum definition not found for symbol {:?}", symbol), span: Some(error_span) }),
        }
    }

    /// Looks up a function signature specifically.
    /// Returns an error if the symbol exists but isn't a function/method.
    ///
    /// Preconditions: `symbol` is the symbol to look up. `error_span` for reporting.
    /// Postconditions: Returns `Ok(&FunctionSignature)` if found and is a function.
    ///                 Returns `Err(TypeError::InternalError)` if not found or wrong kind.
    pub fn get_function_sig(&self, symbol: &Symbol, error_span: SourceSpan) -> TypeResult<&FunctionSignature> {
        match self.definitions.get(symbol) {
            Some(TypeDef::Function(sig)) => Ok(sig),
            _ => Err(TypeError::InternalError {
                 message: format!("Function definition not found for symbol {:?}", symbol),
                 span: Some(error_span),
            }),
        }
    }

    /// Gets the signature for a function/method symbol, searching both the TypeContext
    /// and the method signatures stored within ImplDefs in the TraitRepository.
    /// This is necessary because impl method signatures are not stored directly in TypeContext.
    pub fn get_any_function_sig<'a>(
        &'a self,
        trait_repo: &'a TraitRepository, // Need access to the trait repo
        symbol: &Symbol,
        error_span: SourceSpan,
    ) -> TypeResult<&'a FunctionSignature> {
        // 1. Try TypeContext first (standalone functions + trait methods added directly?)
        if let Some(TypeDef::Function(sig)) = self.definitions.get(symbol) {
            return Ok(sig);
        }

        // 2. Try TraitRepository (impl methods)
        if let Some(sig) = trait_repo.find_impl_method_sig(symbol) {
            return Ok(sig);
        }

        // 3. Try TraitRepository (trait definition methods)
        if let Some(sig) = trait_repo.find_trait_method_sig(symbol) {
            return Ok(sig);
        }

        // 4. NOT FOUND
        Err(TypeError::InternalError {
            message: format!(
                "Function definition, impl method, or trait method signature not found for symbol {:?}",
                symbol
            ),
            span: Some(error_span),
        })
    }

    /// Iterate over all enum definitions.
    pub(crate) fn iter_enums(&self) -> impl Iterator<Item = &EnumDef> {
        self.definitions.values().filter_map(|def| match def {
            TypeDef::Enum(ed) => Some(ed),
            _ => None,
        })
    }
} 