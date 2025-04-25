#![doc = include_str!("../README.md")]
#![allow(dead_code, unused_imports, unused_mut, unused_variables)]

pub mod error;
pub mod types;
pub mod definitions;
pub mod scopes;
pub mod resolve_types;
pub mod resolve_expr;
pub mod core;

pub use types::{ 
    ResolvedType, PrimitiveType, ResolvedParameter, 
    ResolvedField, ResolvedEnumVariant, ResolvedStruct, ResolvedEnum, 
    ResolvedFunction, ResolvedExpr, ResolvedExprKind, ResolvedArgument, 
    ResolvedPattern, ResolvedPatternKind, ResolvedPatternField, 
    ResolvedDefinitions, ResolvedModuleStructure, Symbol
};
pub use error::{ResolutionError, ResolverWarning};
pub use core::Resolver;

use parallax_syntax::{ModuleUnit, SyntaxDatabase};
use parallax_stdlib::load_stdlib_frame;

/// The main database trait for the resolution process.
///
/// This trait extends the `SyntaxDatabase` (which provides access to the parsed AST)
/// and adds the primary query method `resolve_definitions` for performing name
/// resolution and type checking.
#[salsa::db]
pub trait ResolveDatabase: SyntaxDatabase {
    /// Performs name resolution and type checking on the provided `root_module`.
    ///
    /// This is the main entry point query for the `parallax-resolve` crate.
    /// It orchestrates the different resolution passes (definition collection,
    /// scope building, signature resolution, body resolution) and returns
    /// the fully resolved module structure, including any errors or warnings.
    ///
    /// This method is memoized by the salsa database via the underlying tracked query.
    ///
    /// # Arguments
    /// * `root_module`: The `ModuleUnit` representing the root of the code to resolve.
    ///
    /// # Returns
    /// A `ResolvedModuleStructure` containing the resolved definitions, errors, and warnings.
    fn resolve_definitions<'db>(&'db self, root_module: ModuleUnit<'db>) -> ResolvedModuleStructure<'db> where Self: Sized {
        resolve_definitions_query(self, root_module)
    }
}

// The actual tracked query function that performs the resolution.
#[salsa::tracked]
fn resolve_definitions_query<'db>(
    db: &'db dyn ResolveDatabase,
    root_module: ModuleUnit<'db>
) -> ResolvedModuleStructure<'db> {
    // Load the standard library frame.
    let stdlib_frame = load_stdlib_frame(db);

    // Create a vector containing the dependencies (just stdlib for now).
    let dependencies = vec![stdlib_frame];

    // Create the resolver, passing the dependencies vector.
    let resolver = Resolver::new(db, root_module, dependencies);
    resolver.resolve()
}