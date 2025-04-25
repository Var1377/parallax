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

        // Store the correctly found root module symbol
        context.entry_point_module_symbol = root_module_symbol_opt;

        // 1b. Traverse all dependency frames
        for dep_frame in &self.dependencies {
            let dep_root_dir = dep_frame.root(self.db);
            // Access name via config -> inner -> package -> name
            let dep_name = dep_frame.config(self.db).inner(self.db).package.name;
            traverse_dir(
                self.db,
                dep_root_dir,
                &mut context.definitions_map,
                &mut context.errors,
                None,
                "",
            );
        }

        // --- Pass 1.5: Collect Core Traits and Intrinsics ---
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

        // --- Pass 2: Build Module Scopes (Imports) ---
        build_module_scopes(
            self.db,
            self.root_module,
            &context.definitions_map,
            &mut context.module_scopes,
            &mut context.errors,
            &mut context.warnings,
        );

        
        for dep_frame in &self.dependencies {
             let dep_root_dir = dep_frame.root(self.db);
             // Access name via config -> inner -> package -> name
             let dep_name = dep_frame.config(self.db).inner(self.db).package.name;
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
        
        context.prelude_scope = crate::resolve_types::build_prelude_scope(
            &context.definitions_map,
            &context.module_scopes,
        );
        
        crate::resolve_types::resolve_signatures(
            self.db,
            &context.definitions_map,
            &context.module_scopes,
            &context.prelude_scope,
            &mut context.resolved_definitions,
            &mut context.errors,
        );
        
        // --- Determine Entry Point Function Symbol (searching root::main and root::main::main) ---
        let entry_point_function_symbol = if let Some(root_module_sym) = context.entry_point_module_symbol {
            let mut main_symbol: Option<Symbol> = None;

            // Check for 'main' directly in the root module scope
            if let Some(root_scope) = context.module_scopes.get(&root_module_sym) {
                if let Some(entry) = root_scope.items.get("main") {
                    // Found something named 'main' in the root scope. Is it a function?
                    if let Some(def_info) = context.definitions_map.get(&entry.symbol) {
                        if def_info.kind == DefinitionKind::Function {
                            main_symbol = Some(entry.symbol); // Found root::main function
                        } else if def_info.kind == DefinitionKind::Module {
                            // Found root::main module. Look inside it for the main function.
                            if let Some(main_sub_scope) = context.module_scopes.get(&entry.symbol) {
                                if let Some(func_entry) = main_sub_scope.items.get("main") {
                                    if let Some(func_def_info) = context.definitions_map.get(&func_entry.symbol) {
                                        if func_def_info.kind == DefinitionKind::Function {
                                            main_symbol = Some(func_entry.symbol); // Found root::main::main function
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // If not found via direct lookup or nested module lookup, explicitly search for root::main::main
            // This handles cases where the root scope might not directly contain the 'main' module entry
            // (e.g., if scope building logic changes).
            if main_symbol.is_none() {
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
                                if func_def_info.kind == DefinitionKind::Function {
                                    main_symbol = Some(func_entry.symbol); // Found root::main::main via explicit search
                                }
                            }
                        }
                     }
                }
            }

            main_symbol // Return whatever was found
        } else {
            None // No root module symbol found
        };

        // --- Pass 4: Resolve Function Bodies ---
        crate::resolve_expr::resolve_bodies(
            self.db,
            &context.definitions_map,
            &context.module_scopes,
            &context.prelude_scope,
            &mut context.resolved_definitions,
            &mut context.errors,
            &mut context.warnings,
        );

        // Return the final state from the context, including ALL resolved definitions (root + dependencies)
        ResolvedModuleStructure::new(
            self.db,
            context.resolved_definitions, // Use the FULL list
            entry_point_function_symbol,
            context.core_traits,
            context.intrinsics,
            context.errors,
            context.warnings,
        )
    }
}

/// Helper function to filter ResolvedDefinitions to only include items
/// belonging to the root module or its descendants.
fn filter_definitions_by_root<'db>(
    all_resolved_defs: &ResolvedDefinitions,
    all_def_infos: &HashMap<Symbol, DefinitionInfo<'db>>,
    root_module_symbol: Option<Symbol>,
) -> ResolvedDefinitions {
    let root_sym = match root_module_symbol {
        Some(sym) => sym,
        None => return ResolvedDefinitions::default(), // No root, return empty
    };

    let mut filtered = ResolvedDefinitions::default();

    // Helper closure to check if a symbol belongs to the root hierarchy
    let is_in_root_hierarchy = |symbol: Symbol| -> bool {
        let mut current = Some(symbol);
        while let Some(curr_sym) = current {
            if curr_sym == root_sym {
                return true;
            }
            // Check the definition info map for the parent
            current = all_def_infos.get(&curr_sym).and_then(|info| info.parent_symbol);
        }
        false
    };

    // Filter Structs
    filtered.structs = all_resolved_defs.structs.iter()
        .filter(|s| is_in_root_hierarchy(s.module_symbol))
        .cloned()
        .collect();

    // Filter Enums
    filtered.enums = all_resolved_defs.enums.iter()
        .filter(|e| is_in_root_hierarchy(e.module_symbol))
        .cloned()
        .collect();

    // Filter Functions
    filtered.functions = all_resolved_defs.functions.iter()
        .filter(|f| is_in_root_hierarchy(f.module_symbol))
        .cloned()
        .collect();

    // Filter Traits
    filtered.traits = all_resolved_defs.traits.iter()
        .filter(|t| is_in_root_hierarchy(t.module_symbol))
        .cloned()
        .collect();

    // Filter Impls
    // For impls, we need to check the module where the *impl block* itself is defined.
    // We can get this from the `all_def_infos` map using the `impl_symbol`.
    filtered.impls = all_resolved_defs.impls.iter()
        .filter(|imp| {
            all_def_infos.get(&imp.impl_symbol)
                         .and_then(|info| info.parent_symbol) // Impl block's parent module
                         .map_or(false, |module_sym| is_in_root_hierarchy(module_sym))
        })
        .cloned()
        .collect();

    // Note: Intrinsics are global and likely don't need filtering here,
    // but they are handled separately in ResolvedModuleStructure anyway.
    // filtered.intrinsics = all_resolved_defs.intrinsics.clone();

    filtered
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
