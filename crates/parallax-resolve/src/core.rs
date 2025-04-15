// Core resolver logic orchestrating the different passes.

use crate::definitions::{DefinitionKind, DefinitionInfo, traverse_dir, traverse_module_unit, SpecialDefinitionKind};
use crate::error::{ResolutionError, ResolverWarning};
use crate::scopes::{ModuleScope, build_module_scopes, build_directory_scopes};
use crate::types::{ResolvedDefinitions, ResolvedModuleStructure, Symbol};
use parallax_syntax::{ModuleUnit, SyntaxDatabase};
use parallax_source::Frame;
use std::collections::HashMap;

/// The main resolver struct.
///
/// This struct holds immutable references to the database, the root module
/// AST node, and loaded dependency frames. It provides the entry point (`resolve`)
/// to start the multi-pass resolution process.
pub struct Resolver<'db> {
    db: &'db dyn SyntaxDatabase,
    root_module: ModuleUnit<'db>,
    dependencies: Vec<Frame<'db>>,
}

/// Internal context struct holding the mutable state during resolution passes.
///
/// This struct aggregates the data collected and modified across the different
/// resolver passes (definitions, scopes, resolved types, errors, warnings).
struct ResolveRunContext<'db> {
    definitions_map: HashMap<Symbol, DefinitionInfo<'db>>,
    module_scopes: HashMap<Symbol, ModuleScope>,
    prelude_scope: HashMap<String, Symbol>,
    entry_point_module_symbol: Option<Symbol>,
    errors: Vec<ResolutionError>,
    warnings: Vec<ResolverWarning>,
    resolved_definitions: ResolvedDefinitions,
    core_traits: Vec<(String, Symbol)>,
    intrinsics: Vec<(String, Symbol)>,
}

impl<'db> Resolver<'db> {
    /// Creates a new `Resolver` instance.
    ///
    /// # Arguments
    ///
    /// * `db`: A reference to the database providing access to syntax trees.
    /// * `root_module`: The root `ModuleUnit` AST node to start resolution from.
    /// * `dependencies`: A vector of loaded `Frame`s representing dependencies (including stdlib).
    pub fn new(
        db: &'db dyn SyntaxDatabase,
        root_module: ModuleUnit<'db>,
        dependencies: Vec<Frame<'db>>,
    ) -> Self {
        Self { db, root_module, dependencies }
    }

    /// Runs the complete multi-pass resolution process.
    ///
    /// This method consumes the `Resolver` and executes the following passes sequentially:
    /// 1. **Collect Definitions:** Scans the AST to find all top-level definitions
    ///    (structs, enums, functions, traits, impls, modules) and stores basic information
    ///    about them (`definitions::collect_definitions`).
    /// 2. **Build Module Scopes:** Resolves `use` declarations (imports) and determines the
    ///    set of names visible within each module's scope (`scopes::build_module_scopes`).
    /// 3. **Resolve Signatures:** Resolves type references within the signatures of all
    ///    definitions (struct fields, enum variants, function parameters/return types, trait/impl types)
    ///    (`resolve_types::resolve_signatures`).
    /// 4. **Resolve Function Bodies:** Resolves expressions, performs type checking within function
    ///    bodies, and resolves local variable references (`resolve_expr::resolve_bodies`).
    ///
    /// # Returns
    ///
    /// A `ResolvedModuleStructure` containing the fully resolved definitions,
    /// along with any errors and warnings encountered during the process.
    pub fn resolve(self) -> ResolvedModuleStructure<'db> {
        // Consumes self

        // Initialize the context with empty state
        let mut context = ResolveRunContext {
            definitions_map: HashMap::new(),
            module_scopes: HashMap::new(),
            prelude_scope: HashMap::new(),
            entry_point_module_symbol: None,
            errors: Vec::new(),
            warnings: Vec::new(),
            resolved_definitions: ResolvedDefinitions::default(),
            core_traits: Vec::new(),
            intrinsics: Vec::new(),
        };

        // --- Pass 1: Collect Definitions ---
        println!("DEBUG [Resolver::resolve]: Collecting definitions for root module...");
        traverse_module_unit(
            self.db,
            self.root_module,
            &mut context.definitions_map,
            &mut context.errors,
            None,
            "",
        );

        // --- Determine Root Module Symbol (AFTER collecting its definitions) ---
        // Find the DefinitionInfo corresponding to the root_module unit.
        // The root module has no parent and an empty path prefix.
        let root_module_symbol_opt = context.definitions_map.iter()
            .find(|(_, info)| info.parent_symbol.is_none() && info.full_path.is_empty() && info.kind == DefinitionKind::Module)
            .map(|(symbol, _)| *symbol);

        if root_module_symbol_opt.is_none() {
            // This warning should ideally not happen now, but kept for safety.
            println!("WARN [Resolver::resolve]: Could not determine the root module symbol AFTER definition collection.");
        }
        // Store the correctly found root module symbol
        context.entry_point_module_symbol = root_module_symbol_opt;

        // 1b. Traverse all dependency frames
        println!("DEBUG [Resolver::resolve]: Collecting definitions for {} dependencies...", self.dependencies.len());
        for dep_frame in &self.dependencies {
            let dep_root_dir = dep_frame.root(self.db);
            // Access name via config -> inner -> package -> name
            let dep_name = dep_frame.config(self.db).inner(self.db).package.name;
            println!("  -> Traversing dependency frame: {}", dep_name);
            traverse_dir(
                self.db,
                dep_root_dir,
                &mut context.definitions_map,
                &mut context.errors,
                None,
                "",
            );
        }
        println!("DEBUG [Resolver::resolve]: Definition collection complete. Map size: {}", context.definitions_map.len());

        // --- Pass 1.5: Collect Core Traits and Intrinsics ---
        println!("DEBUG [Resolver::resolve]: Collecting core traits and intrinsics...");
        for (symbol, def_info) in &context.definitions_map {
            match def_info.special_kind {
                Some(SpecialDefinitionKind::CoreTrait) => {
                    context.core_traits.push((def_info.full_path.clone(), *symbol));
                }
                Some(SpecialDefinitionKind::Intrinsic) => {
                    context.intrinsics.push((def_info.full_path.clone(), *symbol));
                }
                None => {}
            }
        }
        println!("DEBUG [Resolver::resolve]: Found {} core traits, {} intrinsics.", context.core_traits.len(), context.intrinsics.len());

        // --- Pass 2: Build Module Scopes (Imports) ---
        println!("DEBUG [Resolver::resolve]: Building scopes for root module...");
        build_module_scopes(
            self.db,
            self.root_module,
            &context.definitions_map,
            &mut context.module_scopes,
            &mut context.errors,
            &mut context.warnings,
        );

        // <<< CHECK 1 >>>
        println!("DEBUG [Resolver::resolve]: After root scope build:");
        println!("  -> module_scopes size: {}", context.module_scopes.len());
        println!("  -> module_scopes contains root (Symbol(1)) key: {}", context.module_scopes.contains_key(&Symbol::new(1)));
        println!("  -> module_scopes keys: {:?}", context.module_scopes.keys().collect::<Vec<_>>());

        // 2b. Build scopes for all dependency frames
        // <<< CHECK 2 >>>
        println!("DEBUG [Resolver::resolve]: Before dependency scope build loop:");
        println!("  -> module_scopes contains root (Symbol(1)) key: {}", context.module_scopes.contains_key(&Symbol::new(1)));
        println!("DEBUG [Resolver::resolve]: Building scopes for {} dependencies...", self.dependencies.len());
        for dep_frame in &self.dependencies {
             let dep_root_dir = dep_frame.root(self.db);
             // Access name via config -> inner -> package -> name
             let dep_name = dep_frame.config(self.db).inner(self.db).package.name;
             println!("  -> Building scopes for dependency frame: {}", dep_name);
             build_directory_scopes(
                 self.db,
                 dep_root_dir,
                 &context.definitions_map,
                 &mut context.module_scopes,
                 &mut context.errors,
                 &mut context.warnings,
                 "",
             );
        }
        // <<< CHECK 3 >>>
        println!("DEBUG [Resolver::resolve]: After dependency scope build loop:");
        println!("  -> module_scopes size: {}", context.module_scopes.len());
        println!("  -> module_scopes contains root (Symbol(1)) key: {}", context.module_scopes.contains_key(&Symbol::new(1)));
        println!("  -> module_scopes keys: {:?}", context.module_scopes.keys().collect::<Vec<_>>());
        println!("DEBUG [Resolver::resolve]: Scope building complete. Map size: {}", context.module_scopes.len());

        // --- Build Prelude Scope (after ALL scopes are built) ---
        println!("DEBUG [Resolver::resolve]: Building prelude scope...");
        context.prelude_scope = crate::resolve_types::build_prelude_scope(
            &context.definitions_map,
            &context.module_scopes,
        );
        println!("DEBUG [Resolver::resolve]: Prelude scope size: {}", context.prelude_scope.len());

        // <<< ADD DEBUG HERE >>>
        println!("DEBUG [Resolver::resolve]: After prelude scope build:");
        println!("  -> module_scopes size: {}", context.module_scopes.len());
        println!("  -> module_scopes contains root (Symbol(1)) key: {}", context.module_scopes.contains_key(&Symbol::new(1)));
        // Print all keys:
        println!("  -> module_scopes keys: {:?}", context.module_scopes.keys().collect::<Vec<_>>());

        // --- Pass 3: Resolve Signatures ---
        println!("DEBUG [Resolver::resolve]: Resolving signatures...");
        crate::resolve_types::resolve_signatures(
            self.db,
            &context.definitions_map,
            &context.module_scopes,
            &context.prelude_scope,
            &mut context.resolved_definitions,
            &mut context.errors,
        );
        println!("DEBUG [Resolver::resolve]: Signature resolution complete.");

        // <<< ADD DEBUG HERE >>>
        println!("DEBUG [Resolver::resolve]: After signature resolution:");
        println!("  -> module_scopes size: {}", context.module_scopes.len());
        println!("  -> module_scopes contains root (Symbol(1)) key: {}", context.module_scopes.contains_key(&Symbol::new(1)));
        // Print all keys:
        println!("  -> module_scopes keys: {:?}", context.module_scopes.keys().collect::<Vec<_>>());

        // --- Determine Entry Point Function Symbol (searching root::main and root::main::main) ---
        let entry_point_function_symbol = if let Some(root_module_sym) = context.entry_point_module_symbol {
            println!("  -> [Entry Point] Found root module symbol: {:?}", root_module_sym);
            let mut main_symbol: Option<Symbol> = None;

            // Check for 'main' directly in the root module scope
            if let Some(root_scope) = context.module_scopes.get(&root_module_sym) {
                 println!("  -> [Entry Point] Found root scope.");
                if let Some(entry) = root_scope.items.get("main") {
                    println!("  -> [Entry Point] Found 'main' item in root scope: Symbol {:?}", entry.symbol);
                    // Found something named 'main' in the root scope. Is it a function?
                    if let Some(def_info) = context.definitions_map.get(&entry.symbol) {
                         println!("  -> [Entry Point] Root 'main' item kind: {:?}", def_info.kind);
                        if def_info.kind == DefinitionKind::Function {
                            main_symbol = Some(entry.symbol); // Found root::main function
                             println!("  -> [Entry Point] Case 1: Found main function directly in root scope: {:?}", main_symbol);
                        } else if def_info.kind == DefinitionKind::Module {
                            // Found root::main module. Look inside it for the main function.
                            println!("  -> [Entry Point] Root 'main' item is a module. Looking inside module {:?}", entry.symbol);
                            if let Some(main_sub_scope) = context.module_scopes.get(&entry.symbol) {
                                println!("  -> [Entry Point] Found scope for main submodule {:?}.", entry.symbol);
                                if let Some(func_entry) = main_sub_scope.items.get("main") {
                                    println!("  -> [Entry Point] Found 'main' item in submodule scope: Symbol {:?}", func_entry.symbol);
                                    if let Some(func_def_info) = context.definitions_map.get(&func_entry.symbol) {
                                         println!("  -> [Entry Point] Submodule 'main' item kind: {:?}", func_def_info.kind);
                                        if func_def_info.kind == DefinitionKind::Function {
                                            main_symbol = Some(func_entry.symbol); // Found root::main::main function
                                             println!("  -> [Entry Point] Case 2: Found main function in submodule scope: {:?}", main_symbol);
                                        }
                                    } else {
                                        println!("  -> [Entry Point] !! Failed to get definition for submodule 'main' item: Symbol {:?}", func_entry.symbol);
                                    }
                                } else {
                                    println!("  -> [Entry Point] !! Did not find 'main' item within submodule {:?} scope.", entry.symbol);
                                }
                            } else {
                                println!("  -> [Entry Point] !! Failed to get scope for main submodule: Symbol {:?}", entry.symbol);
                            }
                        }
                    } else {
                         println!("  -> [Entry Point] !! Failed to get definition for root 'main' item: Symbol {:?}", entry.symbol);
                    }
                } else {
                    println!("  -> [Entry Point] !! Did not find 'main' item in root scope.");
                }
            } else {
                println!("  -> [Entry Point] !! Failed to get root scope for symbol: {:?}", root_module_sym);
            }

            // If not found via direct lookup or nested module lookup, explicitly search for root::main::main
            // This handles cases where the root scope might not directly contain the 'main' module entry
            // (e.g., if scope building logic changes).
            if main_symbol.is_none() {
                 println!("  -> [Entry Point] Main not found yet. Trying explicit submodule search...");
                let main_submodule_symbol = context.definitions_map.iter()
                    .find(|(_, info)| {
                        info.kind == DefinitionKind::Module &&
                        info.name == "main" &&
                        info.parent_symbol == Some(root_module_sym)
                    })
                    .map(|(symbol, _)| *symbol);
                
                if let Some(main_sub_sym) = main_submodule_symbol {
                     if let Some(main_sub_scope) = context.module_scopes.get(&main_sub_sym) {
                        if let Some(func_entry) = main_sub_scope.items.get("main") {
                            if let Some(func_def_info) = context.definitions_map.get(&func_entry.symbol) {
                                println!("  -> [Entry Point] Submodule 'main' item kind (explicit search): {:?}", func_def_info.kind);
                                if func_def_info.kind == DefinitionKind::Function {
                                    main_symbol = Some(func_entry.symbol); // Found root::main::main via explicit search
                                     println!("  -> [Entry Point] Case 3: Found main function via explicit submodule search: {:?}", main_symbol);
                                }
                            } else {
                                println!("  -> [Entry Point] !! Failed to get definition for submodule 'main' item (explicit search): Symbol {:?}", func_entry.symbol);
                            }
                        } else {
                             println!("  -> [Entry Point] !! Did not find 'main' item within submodule {:?} scope (explicit search).", main_sub_sym);
                        }
                     } else {
                         println!("  -> [Entry Point] Explicit search: Did not find a 'main' submodule under root {:?}", root_module_sym);
                     }
                } else {
                    println!("  -> [Entry Point] Explicit search: Did not find a 'main' submodule under root {:?}", root_module_sym);
                }
            }

            println!("  -> [Entry Point] Final value before returning: {:?}", main_symbol);
            main_symbol // Return whatever was found
        } else {
             println!("  -> [Entry Point] !! Did not find root module symbol initially.");
            None // No root module symbol found
        };

        // <<< ADD DEBUG HERE >>>
        println!("DEBUG [Resolver::resolve]: Final entry point symbol lookup result: {:?}", entry_point_function_symbol);

        if entry_point_function_symbol.is_none() {
             // This warning is now more meaningful if it appears.
             println!("WARN [Resolver::resolve]: Entry point function 'main' not found in the root module's scope.");
        }

        // --- Pass 4: Resolve Function Bodies ---
        println!("DEBUG [Resolver::resolve]: Resolving function bodies...");
        crate::resolve_expr::resolve_bodies(
            self.db,
            &context.definitions_map,
            &context.module_scopes,
            &context.prelude_scope,
            &mut context.resolved_definitions,
            &mut context.errors,
            &mut context.warnings,
        );
        println!("DEBUG [Resolver::resolve]: Function body resolution complete.");

        // Return the final state from the context
        // Salsa requires the db as the first argument to the generated `new` method.
        ResolvedModuleStructure::new(
            self.db,
            context.resolved_definitions,
            entry_point_function_symbol,
            context.core_traits,
            context.intrinsics,
            context.errors,
            context.warnings,
        )
    }
}

// --- Unit Tests ---

#[cfg(test)]
mod tests {
    // Unit testing `Resolver::new` is trivial and doesn't provide much value, as it just stores references.
    // Unit testing `Resolver::resolve` requires significant setup of a mock database and AST,
    // which is better handled in integration tests that use the actual parser and database.
    // Therefore, we will rely on integration tests for validating the resolver's core logic.

    // We can keep a simple test function to ensure the module compiles.
    #[test]
    fn core_module_compiles() {
        assert!(true);
    }
}
