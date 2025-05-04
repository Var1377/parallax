// Pass 2: Handle scopes, imports (use declarations).
use std::collections::HashMap;
use parallax_syntax::{ast, ModuleUnit, SyntaxDatabase};
use parallax_syntax::ast::items::{ItemKind, UseTree, UseTreeKind};
use crate::definitions::{DefinitionInfo, DefinitionKind}; 
use crate::types::Symbol;
use crate::error::{ResolutionError, ResolverWarning};
use miette::SourceSpan;
use parallax_source::{Dir, SourceFile};

/// Represents a resolved symbol entry within a module's scope.
///
/// This indicates that a name (e.g., "MyStruct") is visible in the scope
/// and points to the actual definition (`symbol`). It also tracks usage
/// for imports.
#[derive(Debug, Clone)]
pub struct ScopeEntry {
    /// The unique `Symbol` identifying the actual definition (struct, fn, etc.).
    pub symbol: Symbol,
    /// If this entry was brought into scope via a `use` declaration,
    /// this holds the `Symbol` of the module containing the `use` statement.
    /// `None` if the item was defined directly within the scope's module.
    pub imported_from: Option<Symbol>,
    /// If imported, this is the source span of the specific `use` tree item
    /// that imported this symbol.
    pub import_span: Option<SourceSpan>,
    /// Flag used to track whether an imported item has been referenced.
    /// Used for generating "unused import" warnings.
    pub is_used: bool,
}

/// Represents the names visible within a single module scope.
///
/// Contains items defined directly within the module and items imported
/// via `use` declarations.
#[derive(Debug, Clone, Default)]
pub struct ModuleScope {
    /// Maps simple names (e.g., "MyStruct") visible in this scope to their `ScopeEntry`,
    /// which contains the resolved `Symbol` and import information.
    pub items: HashMap<String, ScopeEntry>,
    // Potential future enhancement: Reference to parent scope for hierarchical lookups.
    // pub parent_scope: Option<Symbol>,
}

/// Pass 2: Build scopes for all modules, resolving `use` declarations.
///
/// This function recursively traverses the module tree, starting from `root_module`.
/// For each module, it:
/// 1. Adds all direct child definitions (functions, structs, enums, etc., found in Pass 1)
///    and enum variants to the module's scope.
/// 2. Processes all `use` declarations within the module, resolving the imported paths
///    and adding the corresponding symbols to the scope, handling aliases and glob imports.
/// 3. Stores the resulting `ModuleScope` in the `module_scopes` map, keyed by the module's `Symbol`.
/// 4. Recursively calls itself for all child modules.
/// 5. After processing all modules, it checks for unused imports across all scopes.
///
/// # Arguments
///
/// * `db`: Database access for querying AST and module info.
/// * `module`: The current `ModuleUnit` being processed.
/// * `definitions_map`: The map of all definitions collected in Pass 1.
/// * `module_scopes`: The map being populated with `ModuleScope` for each module symbol.
/// * `errors`: Vector to collect resolution errors (e.g., import errors, ambiguous names).
/// * `warnings`: Vector to collect resolver warnings (e.g., unused imports).
pub fn build_module_scopes<'db>(
    db: &dyn SyntaxDatabase,
    module: ModuleUnit<'db>,
    definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>,
    module_scopes: &mut HashMap<Symbol, ModuleScope>,
    errors: &mut Vec<ResolutionError>,
    warnings: &mut Vec<ResolverWarning>,
) {
    // Start recursion with None as the parent symbol for the root module
    build_module_scopes_recursive(db, module, None, definitions_map, module_scopes, errors, warnings);

    // Post-process: Check for unused imports after all scopes are built.
    for scope in module_scopes.values() {
        for (name, entry) in scope.get_unused_imports() {
            if let Some(span) = entry.import_span {
                warnings.push(ResolverWarning::UnusedImport {
                    path: name,
                    span,
                });
            }
        }
    }
}

/// Recursive helper for building module scopes.
fn build_module_scopes_recursive<'db>(
    db: &dyn SyntaxDatabase,
    module: ModuleUnit<'db>,
    parent_module_symbol: Option<Symbol>,
    definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>,
    module_scopes: &mut HashMap<Symbol, ModuleScope>,
    errors: &mut Vec<ResolutionError>,
    warnings: &mut Vec<ResolverWarning>,
) {
    let module_name = module.name(db);
    let module_path_str = module.path(db); // Keep for logging

    // Find the Symbol for the current module using name and parent symbol
    let module_symbol_opt = definitions_map.iter()
        .find(|(_, info)| {
            info.kind == DefinitionKind::Module &&
            // Compare name as slices
            info.name.as_str() == module_name.as_str() &&
            info.parent_symbol == parent_module_symbol
        })
        .map(|(s, _)| *s);

    let module_symbol = match module_symbol_opt {
        Some(symbol) => symbol,
        None => {
            // This should not happen if Pass 1 correctly registered the module
            errors.push(ResolutionError::InternalError {
                message: format!(
                    "Internal: Could not find definition for module '{}' with parent {:?} during scope building.",
                    module_name,
                    parent_module_symbol
                ),
                span: None, // No specific span available here
            });
            return; // Cannot proceed without the correct symbol
        }
    };

    // Check if scope already processed (safety measure against potential infinite loops if module structure is cyclic)
    if module_scopes.contains_key(&module_symbol) {
        return;
    }

    let mut current_scope = ModuleScope::default();

    // 1. Add items defined directly in this module to its scope
    for (def_symbol, def_info) in definitions_map {
        // Check if the definition is a direct child of the current module
        if def_info.parent_symbol == Some(module_symbol) {
            // Add the simple name mapping to the symbol
            let scope_entry = ScopeEntry {
                symbol: *def_symbol,
                imported_from: None, // Defined locally
                import_span: None,
                is_used: false, // Usage tracking mainly relevant for imports
            };
            if let Some(existing_entry) = current_scope.items.insert(def_info.name.clone(), scope_entry) {
                // Pass 1 should catch duplicates defined in the *same* source file.
                // This check handles potential name collisions between an item defined here
                // and one potentially brought in by a glob import processed earlier (if order changed),
                // or theoretically, if Pass 1 allowed duplicates.
                if existing_entry.symbol != *def_symbol {
                    errors.push(ResolutionError::AmbiguousReference {
                        name: def_info.name.clone(),
                        span: def_info.span, // Span of the current item causing conflict
                        candidates: vec![
                            definitions_map.get(&existing_entry.symbol).map_or_else(|| "?".to_string(), |i| i.full_path.clone()),
                            def_info.full_path.clone(),
                        ],
                    });
                }
            }

            // If this is an enum, add its variants to the scope too
            // Variants are treated as if they are defined directly in the enum's parent scope.
            if def_info.kind == DefinitionKind::Enum {
                for (variant_symbol, variant_info) in definitions_map {
                    if variant_info.kind == DefinitionKind::EnumVariant && variant_info.parent_symbol == Some(*def_symbol) {
                        let variant_scope_entry = ScopeEntry {
                            symbol: *variant_symbol,
                            imported_from: None, // Defined locally (as part of enum)
                            import_span: None,
                            is_used: false,
                        };
                        if let Some(existing_variant_entry) = current_scope.items.insert(variant_info.name.clone(), variant_scope_entry) {
                            // Check for collision between variant name and other items/variants in scope
                            if existing_variant_entry.symbol != *variant_symbol {
                                errors.push(ResolutionError::AmbiguousReference {
                                    name: variant_info.name.clone(),
                                    span: variant_info.span, // Span of the variant definition
                                    candidates: vec![
                                        definitions_map.get(&existing_variant_entry.symbol).map_or_else(|| "?".to_string(), |i| i.full_path.clone()),
                                        variant_info.full_path.clone(),
                                    ],
                                });
                            }
                        }
                    }
                }
            }
        }
    }

    // 2. Process `use` declarations within this module
    if let Some(parsed_file) = module.ast(db) {
        let items = parsed_file.ast(db);
        for item in items {
            if let ItemKind::Use(use_decl) = &item.kind {
                resolve_use_tree(
                    db,
                    module_symbol, // Symbol of the module where the `use` is declared
                    &use_decl.tree,
                    &mut current_scope,
                    definitions_map,
                    module_scopes, // Pass the map of scopes built so far (needed for path resolution)
                    errors,
                    warnings, // Pass warnings for potential future use inside resolve_use_tree
                );
            }
        }
    }

    // 3. Store the completed scope for this module before recursing
    // Note: Inserting here means child modules resolve imports based on the parent's scope *before* the parent processes its own imports.
    // This matches Rust's behavior (imports are not available until after the `use` statement).
    // However, path resolution (`resolve_path`) *can* look up through parent scopes.
    module_scopes.insert(module_symbol, current_scope);

    // 4. Recurse into child modules
    let children = module.children(db);
    for child_module in children {
        // Pass the *current* module's symbol as the parent for the recursive call
        build_module_scopes_recursive(db, *child_module, Some(module_symbol), definitions_map, module_scopes, errors, warnings);
    }
}

/// Resolves a single `UseTree` recursively, adding imported names to the `current_scope`.
///
/// # Arguments
///
/// * `db_access`: Database access.
/// * `current_module_symbol`: The symbol of the module containing the `use` declaration.
/// * `use_tree`: The `UseTree` AST node to resolve.
/// * `current_scope`: The `ModuleScope` to add the imported items to.
/// * `definitions_map`: Map of all known definitions.
/// * `module_scopes`: Map of scopes built so far (needed for path resolution).
/// * `errors`: Vector for reporting errors.
/// * `warnings`: Vector for reporting warnings.
fn resolve_use_tree<'db>(
    db_access: &dyn SyntaxDatabase,
    current_module_symbol: Symbol, // Symbol of the module containing the `use`
    use_tree: &UseTree,
    current_scope: &mut ModuleScope,
    definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>,
    module_scopes: &HashMap<Symbol, ModuleScope>, // Read-only access needed for resolve_path
    errors: &mut Vec<ResolutionError>,
    warnings: &mut Vec<ResolverWarning>,
) {
    resolve_use_tree_recursive(db_access, current_module_symbol, Vec::new(), use_tree, current_scope, definitions_map, module_scopes, errors, warnings);
}

/// Recursive helper for resolving `UseTree` structures.
///
/// # Arguments
/// * `current_import_prefix`: Path segments accumulated before this part of the tree (e.g., `std::collections`).
/// * other args: Same as `resolve_use_tree`.
fn resolve_use_tree_recursive<'db>(
    db: &dyn SyntaxDatabase,
    current_module_symbol: Symbol,
    current_import_prefix: Vec<ast::common::Ident>,
    use_tree: &UseTree,
    current_scope: &mut ModuleScope,
    definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>,
    module_scopes: &HashMap<Symbol, ModuleScope>,
    errors: &mut Vec<ResolutionError>,
    _warnings: &mut Vec<ResolverWarning>, // Mark as unused
) {
    match &use_tree.kind {
        UseTreeKind::Path { segment, alias, sub_tree } => {
            let mut path_segments = current_import_prefix.clone();
            path_segments.push(segment.clone());

            // Try to resolve the full path accumulated so far relative to the current module.
            // NOTE: Pass a dummy prelude scope here, as use paths shouldn't rely on the prelude directly.
            // The prelude is only checked as a fallback in resolve_path.
            let dummy_prelude_scope = HashMap::new();
            match crate::resolve_types::resolve_path(definitions_map, module_scopes, &dummy_prelude_scope, current_module_symbol, &path_segments, use_tree.span) {
                Ok(resolved_symbol) => {
                    // Check if the resolved item is accessible from the current module.
                    // Pass the module containing the *use* statement (`current_module_symbol`)
                    // and the module where the resolved item is defined.
                    let resolved_def_info = definitions_map.get(&resolved_symbol);
                    let _defining_module_symbol = resolved_def_info.and_then(|info| info.parent_symbol).unwrap_or(resolved_symbol);

                    // Check accessibility: Is the item public, or are we accessing from within the defining module or its descendants?
                    if !is_accessible(definitions_map, current_module_symbol, resolved_symbol) {
                        errors.push(ResolutionError::PrivateItemAccess {
                            path: path_segments.iter().map(|s| s.name.as_str()).collect::<Vec<_>>().join("::"),
                            span: segment.span, // Pinpoint the segment causing the access issue
                        });
                        return; // Don't proceed with this path
                    }

                    if let Some(sub) = sub_tree {
                        // Path segment resolved successfully, continue resolving the sub-tree.
                        resolve_use_tree_recursive(
                            db, current_module_symbol,
                            path_segments, // Pass the updated segment list
                            sub, current_scope, definitions_map, module_scopes, 
                            errors, _warnings // Pass unused _warnings
                        );
                    } else {
                        // Leaf node: Add the resolved item to the current scope.
                        let name_in_scope = alias.as_ref().unwrap_or(segment).name.clone();
                        let scope_entry = ScopeEntry {
                            symbol: resolved_symbol,
                            imported_from: Some(current_module_symbol),
                            import_span: Some(use_tree.span),
                            is_used: false, // Mark as unused initially
                        };

                        if let Some(existing_entry) = current_scope.items.insert(name_in_scope.clone(), scope_entry) {
                            // Check for name collision in the scope
                            if existing_entry.symbol != resolved_symbol {
                                errors.push(ResolutionError::AmbiguousReference {
                                    name: name_in_scope,
                                    span: use_tree.span, // Span of the use item causing conflict
                                    candidates: vec![
                                        definitions_map.get(&existing_entry.symbol).map_or_else(|| "?".to_string(), |i| i.full_path.clone()),
                                        resolved_def_info.map_or_else(|| "?".to_string(), |i| i.full_path.clone()),
                                    ],
                                });
                            }
                        }
                    }
                },
                Err(resolve_err) => {
                    // Path resolution failed. Report the error.
                    // Use the specific error from resolve_path if it's helpful, otherwise a generic ImportError.
                     match resolve_err {
                         ResolutionError::NameNotFound { name, span: err_span, .. } => errors.push(ResolutionError::ImportError {
                             path: path_segments.iter().map(|s| s.name.as_str()).collect::<Vec<_>>().join("::"),
                             span: err_span, // Use span from NameNotFound if possible
                             reason: Some(format!("cannot find '{}' in scope", name)),
                         }),
                         ResolutionError::InvalidPathSegment { segment: seg_name, path: p, span: err_span } => errors.push(ResolutionError::ImportError {
                             path: p,
                             span: err_span,
                             reason: Some(format!("segment '{}' is not a valid module or item for path continuation", seg_name)),
                         }),
                         // Convert other resolution errors into a generic import error
                         _ => errors.push(ResolutionError::ImportError {
                             path: path_segments.iter().map(|s| s.name.as_str()).collect::<Vec<_>>().join("::"),
                             span: use_tree.span,
                             reason: Some("Could not resolve import path".to_string()),
                         }),
                     }
                }
            }
        },
        UseTreeKind::Group(trees) => {
            // Process each tree in the group using the same current prefix.
            for tree in trees {
                 resolve_use_tree_recursive(
                    db, current_module_symbol,
                    current_import_prefix.clone(), // Pass the prefix down
                    tree, current_scope, definitions_map, module_scopes, 
                    errors, _warnings // Pass unused _warnings
                );
            }
        },
        UseTreeKind::Glob => {
            // Resolve the prefix path first.
             let dummy_prelude_scope = HashMap::new(); // Prelude doesn't affect glob resolution directly
            match crate::resolve_types::resolve_path(definitions_map, module_scopes, &dummy_prelude_scope, current_module_symbol, &current_import_prefix, use_tree.span) {
                Ok(module_symbol) => {
                    // Check if the prefix resolves to a module or an enum.
                    if let Some(prefix_def_info) = definitions_map.get(&module_symbol) {
                        let import_target_module = match prefix_def_info.kind {
                            DefinitionKind::Module => Some(module_symbol),
                            DefinitionKind::Enum => Some(module_symbol), // Allow importing variants via glob `use MyEnum::*`
                            _ => None,
                        };

                        if let Some(target_symbol) = import_target_module {
                            // Iterate through all definitions and find direct children of the target module/enum
                            // that are accessible from the current module.
                            for (child_symbol, child_def_info) in definitions_map {
                                if child_def_info.parent_symbol == Some(target_symbol) && // Check parent is the glob target
                                   child_def_info.kind != DefinitionKind::Module && // Don't import submodules via glob (usually)
                                   is_accessible(definitions_map, current_module_symbol, *child_symbol)
                                {
                                    let scope_entry = ScopeEntry {
                                        symbol: *child_symbol,
                                        imported_from: Some(current_module_symbol),
                                        import_span: Some(use_tree.span), // Span of the glob `*`
                                        is_used: false,
                                    };
                                    // Add the child to the current scope
                                    if let Some(existing_entry) = current_scope.items.insert(child_def_info.name.clone(), scope_entry) {
                                        // Check for collision
                                        if existing_entry.symbol != *child_symbol {
                                            errors.push(ResolutionError::AmbiguousReference {
                                                name: child_def_info.name.clone(),
                                                // Span points to the glob import, as that's what introduced the ambiguity here
                                                span: use_tree.span,
                                                candidates: vec![
                                                    definitions_map.get(&existing_entry.symbol).map_or_else(|| "?".to_string(), |i| i.full_path.clone()),
                                                    child_def_info.full_path.clone(),
                                                ],
                                            });
                                        }
                                        // If collision, the new one replaces the old one in the map here.
                                    }
                                }
                            }
                        } else {
                             // Prefix resolved, but not to a module or enum
                             errors.push(ResolutionError::ImportError {
                                path: current_import_prefix.iter().map(|s| s.name.as_str()).collect::<Vec<_>>().join("::"),
                                span: use_tree.span, // Span of the path part before `::*`
                                reason: Some("Glob import must point to a module or enum".to_string()),
                            });
                        }
                    } else {
                         // Symbol resolved by resolve_path, but not found in definitions_map (internal error)
                         errors.push(ResolutionError::InternalError {
                            message: format!("Resolved glob prefix symbol {:?} not found in definitions map", module_symbol),
                            span: Some(use_tree.span),
                        });
                    }
                }
                 Err(resolve_err) => {
                     // Failed to resolve the prefix path itself.
                     errors.push(ResolutionError::ImportError {
                        path: current_import_prefix.iter().map(|s| s.name.as_str()).collect::<Vec<_>>().join("::"),
                        span: use_tree.span, // Span of the path part before `::*`
                        reason: Some(format!("Could not resolve path prefix for glob import: {:?}", resolve_err)),
                    });
                 }
            }
        },
    }
}

/// Checks if a target item is accessible from a given module (`from_module`).
///
/// Accessibility Rules:
/// 1. Public items (`pub`) are always accessible.
/// 2. Private items are accessible only if `from_module` is the *same module*
///    where the `target_item` is defined, OR if `from_module` is a *descendant*
///    of the module where `target_item` is defined.
///
/// # Arguments
///
/// * `definitions_map`: The map containing info for all definitions.
/// * `from_module`: The `Symbol` of the module attempting the access.
/// * `target_item`: The `Symbol` of the item (function, struct, module, etc.) being accessed.
///
/// # Returns
///
/// `true` if the item is accessible, `false` otherwise.
pub(crate) fn is_accessible(
    definitions_map: &HashMap<Symbol, DefinitionInfo>,
    from_module: Symbol, // Module trying to access
    target_item: Symbol, // Item being accessed
) -> bool {
    // Attempt to get definition info for the target item
    if let Some(item_def_info) = definitions_map.get(&target_item) {
        // For private modules, we need special handling
        if item_def_info.kind == DefinitionKind::Module && !item_def_info.is_public {
            // Private modules are only accessible from:
            // 1. The module itself
            if from_module == target_item {
                return true;
            }
            
            // 2. Direct parent of the module
            if item_def_info.parent_symbol == Some(from_module) {
                return true;
            }
            
            // 3. Child modules of the private module
            if is_within_module_hierarchy(definitions_map, from_module, target_item) {
                return true;
            }
            
            // Otherwise, private modules are not accessible
            return false;
        }
        
        // For public items (including modules), we still need to check path accessibility
        if item_def_info.is_public {
            // For modules, we need to check if the entire path is accessible
            if item_def_info.kind == DefinitionKind::Module {
                // For a module to be accessible, all parent modules in the path must also be accessible
                let mut current = item_def_info.parent_symbol;
                let mut from_current = from_module;
                
                // If they're in the same module hierarchy, check if the target_item module is accessible
                // from the from_module along every step of the path
                while let Some(parent_symbol) = current {
                    // Skip this check if from_module is a descendant of the current item
                    let mut is_descendant = false;
                    let mut temp_from = from_current;
                    while let Some(from_parent) = definitions_map.get(&temp_from).and_then(|info| info.parent_symbol) {
                        if from_parent == parent_symbol {
                            is_descendant = true;
                            break;
                        }
                        temp_from = from_parent;
                    }
                    
                    if is_descendant {
                        break; // Descendant can access parent modules
                    }
                    
                    // Check if the current parent module is accessible
                    if let Some(parent_info) = definitions_map.get(&parent_symbol) {
                        if !parent_info.is_public {
                            // If any parent module is private, the path is not accessible
                            // unless from_module is within that private module's hierarchy
                            if !is_within_module_hierarchy(definitions_map, from_current, parent_symbol) {
                                return false;
                            }
                        }
                    }
                    
                    current = definitions_map.get(&parent_symbol).and_then(|info| info.parent_symbol);
                    
                    // Update from_current to its parent for the next iteration
                    from_current = definitions_map.get(&from_current)
                        .and_then(|info| info.parent_symbol)
                        .unwrap_or(from_current);
                }
            }
            
            return true;
        }

        // Rule 2: Private items require checking the module hierarchy.
        // Find the module where the target item is *actually* defined.
        // For items like functions or structs, it's their parent_symbol.
        // For modules themselves, it's their own symbol (or parent for visibility relative to parent).
        let defining_module_symbol = match item_def_info.kind {
            // For modules, we need to check if the module itself is accessible
            DefinitionKind::Module => {
                if item_def_info.parent_symbol.is_none() {
                    // Root module - always accessible
                    return true;
                }
                item_def_info.parent_symbol
            },
            // For non-module items, the defining scope is their direct parent module.
            DefinitionKind::EnumVariant => item_def_info.parent_symbol.and_then(|enum_sym| definitions_map.get(&enum_sym)?.parent_symbol),
            _ => item_def_info.parent_symbol,
        };

        // If we can't determine the defining module (e.g., root module's parent is None),
        // something is wrong, or it's a root-level private item (which shouldn't be possible to access).
        let defining_module_symbol = match defining_module_symbol {
            Some(symbol) => symbol,
            None => {
                // If parent_symbol is None, it means target_item *is* the root module itself.
                // Private items at the root shouldn't exist or be accessible from anywhere else.
                // Or it's an item directly in root - it's accessible only from root.
                return from_module == target_item; // Only accessible if accessing itself (which is weird)
            }
        };

        // Private item accessibility check: the item is accessible if from_module is within
        // the hierarchy of the defining module (either the module itself or a descendant)
        is_within_module_hierarchy(definitions_map, from_module, defining_module_symbol)
    } else {
        // Target item doesn't exist in the definitions map (shouldn't happen if symbol is valid).
        false
    }
}

// Helper function to check if a module is within another module's hierarchy
fn is_within_module_hierarchy(
    definitions_map: &HashMap<Symbol, DefinitionInfo>,
    from_module: Symbol,  // Module trying to access
    defining_module: Symbol,  // Module containing the definition
) -> bool {
    // If accessing from the defining module itself, access is granted
    if from_module == defining_module {
        return true;
    }
    
    // Walk up from from_module to see if defining_module is an ancestor
    let mut current = from_module;
    while let Some(parent) = definitions_map.get(&current).and_then(|info| info.parent_symbol) {
        if parent == defining_module {
            return true;
        }
        current = parent;
    }
    
    // If we got here, from_module is not within the hierarchy of defining_module
    false
}

impl ModuleScope {
    /// Marks an item in the scope as used.
    ///
    /// Primarily used to track the usage of imported items to detect unused imports.
    /// Does nothing if the name is not found in the scope.
    pub fn mark_used(&mut self, name: &str) {
        if let Some(entry) = self.items.get_mut(name) {
            entry.is_used = true;
        }
    }

    /// Returns a list of all imported items in this scope that were never marked as used.
    ///
    /// Used to generate "unused import" warnings.
    ///
    /// # Returns
    ///
    /// A `Vec` containing tuples of `(String, &ScopeEntry)` for each unused import,
    /// where the `String` is the name used in the scope.
    pub fn get_unused_imports(&self) -> Vec<(String, &ScopeEntry)> {
        self.items.iter()
            .filter(|(_, entry)| entry.imported_from.is_some() && !entry.is_used)
            .map(|(name, entry)| (name.clone(), entry))
            .collect()
    }
}

/// Builds scopes for a module structure represented by a `Dir`.
///
/// This function is analogous to `build_module_scopes` but works on the `Dir`
/// structure used for the standard library or potentially other directory-based frames.
/// It recursively traverses the directory tree.
pub fn build_directory_scopes<'db>(
    db: &'db dyn SyntaxDatabase,
    dir: Dir<'db>,
    definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>,
    module_scopes: &mut HashMap<Symbol, ModuleScope>,
    errors: &mut Vec<ResolutionError>,
    warnings: &mut Vec<ResolverWarning>,
    // Keep track of the full path prefix to find the correct module symbol for the dir
    current_path_prefix: &str,
) {
    let dir_name = dir.name(db);
    // Use helper from definitions module, which should be accessible via crate path
    let dir_path_str = crate::definitions::make_full_path(current_path_prefix, &dir_name);

    // 1. Find the Symbol for the current directory module from definitions_map.
    let dir_module_symbol = match definitions_map.iter()
        .find(|(_, info)| info.kind == DefinitionKind::Module && info.full_path == dir_path_str)
        .map(|(s, _)| *s)
    {
        Some(symbol) => symbol,
        None => {
            // This directory wasn't found during definition collection, which is unexpected.
            errors.push(ResolutionError::InternalError {
                message: format!("Module definition not found for directory path '{}' during scope building.", dir_path_str),
                span: None, // No specific span here
            });
            return; // Cannot proceed without the module symbol
        }
    };

    let mut current_scope = ModuleScope::default();

    // 2. Add file-modules defined within this directory to the directory's scope.
    for (def_symbol, def_info) in definitions_map {
        // Check if the definition is a file-module directly parented by this dir-module
        if def_info.parent_symbol == Some(dir_module_symbol) && def_info.kind == DefinitionKind::Module {
             let scope_entry = ScopeEntry {
                symbol: *def_symbol,
                imported_from: None, // Defined locally within this dir
                import_span: None,
                is_used: false,
            };
             // Add the file-module itself to the directory's scope
             if let Some(existing_entry) = current_scope.items.insert(def_info.name.clone(), scope_entry) {
                  if existing_entry.symbol != *def_symbol {
                     errors.push(ResolutionError::DuplicateDefinition {
                         name: def_info.name.clone(),
                         span: def_info.span, // Span of the module definition
                         previous_span: existing_entry.import_span.unwrap_or(def_info.span), // Use import span if available
                     });
                  }
             }
        }
    }


    // 3. Insert the scope for the *directory* itself.
    // This scope now contains references to the file-modules within it.
    // println!(
    //     "DEBUG [build_directory_scopes]: Inserting scope for DIR module: {:?} (name: '{}', path: '{}') with {} items",
    //     dir_module_symbol,
    //     dir_name,
    //     dir_path_str,
    //     current_scope.items.len()
    // );
    module_scopes.insert(dir_module_symbol, current_scope);


    // 4. Build scopes for each file-module within this directory
    for file in dir.files(db) {
        let file_location = file.location(db);
        let file_name_full = std::path::Path::new(&file_location).file_name().and_then(|s| s.to_str()).unwrap_or("");
        let file_module_name = std::path::Path::new(file_name_full).file_stem().and_then(|s| s.to_str()).unwrap_or("");
        if file_module_name.is_empty() { continue; }

        let file_module_path = crate::definitions::make_full_path(&dir_path_str, file_module_name);

        // Find the symbol for this file-module (which should have been created in Pass 1)
         let file_module_symbol = match definitions_map.iter()
            .find(|(_, info)| info.kind == DefinitionKind::Module && info.full_path == file_module_path)
            .map(|(s, _)| *s)
         {
            Some(symbol) => symbol,
            None => {
                 errors.push(ResolutionError::InternalError {
                    message: format!("Module definition not found for file path '{}' during scope building.", file_module_path),
                    span: None,
                 });
                 continue;
            }
         };

         // Build scope for this file-module using the helper
         build_file_module_scope(db, file, file_module_symbol, &file_module_path, definitions_map, module_scopes, errors, warnings);
    }


    // 5. Recurse into child directories
    let sub_dirs = dir.dirs(db);
    for sub_dir in sub_dirs {
        // Parent symbol is the current directory's symbol
        build_directory_scopes(db, *sub_dir, definitions_map, module_scopes, errors, warnings, &dir_path_str);
    }
}


/// Helper function to build the scope for a single file treated as a module.
/// This processes items defined within the file and its `use` declarations.
fn build_file_module_scope<'db>(
    db: &'db dyn SyntaxDatabase,
    file: SourceFile<'db>,
    file_module_symbol: Symbol,
    _file_module_path: &str, // Pass the calculated path -> Mark unused
    definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>,
    module_scopes: &mut HashMap<Symbol, ModuleScope>,
    errors: &mut Vec<ResolutionError>,
    warnings: &mut Vec<ResolverWarning>,
) {
     let mut current_scope = ModuleScope::default();

    // 1. Add items defined directly in this file-module to its scope
    for (def_symbol, def_info) in definitions_map {
        // Check if the definition's parent is the current file-module symbol
        if def_info.parent_symbol == Some(file_module_symbol) {
            let scope_entry = ScopeEntry {
                symbol: *def_symbol,
                imported_from: None, // Defined locally
                import_span: None,
                is_used: false,
            };
            if let Some(existing_entry) = current_scope.items.insert(def_info.name.clone(), scope_entry) {
                  if existing_entry.symbol != *def_symbol {
                      errors.push(ResolutionError::DuplicateDefinition {
                          name: def_info.name.clone(),
                          span: def_info.span,
                          previous_span: existing_entry.import_span.unwrap_or(def_info.span),
                      });
                  }
            }

             // Add enum variants if this definition is an enum
             if def_info.kind == DefinitionKind::Enum {
                 for (variant_symbol, variant_info) in definitions_map {
                     if variant_info.kind == DefinitionKind::EnumVariant && variant_info.parent_symbol == Some(*def_symbol) {
                         let variant_scope_entry = ScopeEntry {
                             symbol: *variant_symbol,
                             imported_from: None,
                             import_span: None,
                             is_used: false,
                         };
                          if let Some(existing_variant_entry) = current_scope.items.insert(variant_info.name.clone(), variant_scope_entry) {
                              if existing_variant_entry.symbol != *variant_symbol {
                                  errors.push(ResolutionError::DuplicateDefinition {
                                      name: variant_info.name.clone(),
                                      span: variant_info.span,
                                      previous_span: existing_variant_entry.import_span.unwrap_or(variant_info.span),
                                  });
                              }
                          }
                     }
                 }
             }
        }
    }

     // 2. Process `use` declarations within this file
     // Ensure parse_file_query is accessible
     use parallax_syntax::parse_file_query;
     let parsed_file = parse_file_query(db, file);
     let items = parsed_file.ast(db);
     for item in items {
         if let ast::items::ItemKind::Use(use_decl) = &item.kind {
             resolve_use_tree(
                 db,
                 file_module_symbol, // Resolve relative to the file-module
                 &use_decl.tree,
                 &mut current_scope, // Add imports to this file's scope
                 definitions_map,
                 module_scopes, // Pass existing scopes for resolving paths in `use`
                 errors,
                 warnings,
             );
         }
     }

     // 3. Store the completed scope for this file-module
    //  println!(
    //      "DEBUG [build_file_module_scope]: Inserting scope for FILE module: {:?} (name: '{}', path: '{}') with {} items",
    //      file_module_symbol,
    //      definitions_map.get(&file_module_symbol).map_or("?", |i| &i.name), // Get name from def map
    //      file_module_path,
    //      current_scope.items.len()
    //  );
     module_scopes.insert(file_module_symbol, current_scope);
}

// Add helper function `make_full_path` if it's not already visible here
// Re-declare or import if necessary. For now, assume it's accessible via crate::definitions.

// --- Unit Tests ---

#[cfg(test)]
mod tests {
    use super::*; // Import items from parent module
    use crate::definitions::{DefinitionInfo, DefinitionKind};
    use crate::types::Symbol;
    use miette::SourceSpan;
    use std::collections::HashMap;

    // Helper to create a dummy span
    fn dummy_span() -> SourceSpan {
        SourceSpan::from((0, 0))
    }

    // --- Tests for ModuleScope --- 

    #[test]
    fn test_module_scope_mark_used_and_get_unused() {
        let mut scope = ModuleScope::default();
        let symbol1 = Symbol::fresh();
        let symbol2 = Symbol::fresh();
        let symbol3 = Symbol::fresh();
        let module_sym = Symbol::fresh();

        scope.items.insert("ItemA_UnusedImport".to_string(), ScopeEntry {
            symbol: symbol1,
            imported_from: Some(module_sym), // Imported
            import_span: Some(dummy_span()),
            is_used: false,
        });
        scope.items.insert("ItemB_UsedImport".to_string(), ScopeEntry {
            symbol: symbol2,
            imported_from: Some(module_sym), // Imported
            import_span: Some(dummy_span()),
            is_used: false, // Initially unused
        });
        scope.items.insert("ItemC_Local".to_string(), ScopeEntry {
            symbol: symbol3,
            imported_from: None, // Defined locally
            import_span: None,
            is_used: false,
        });

        // Initial check
        let unused1 = scope.get_unused_imports();
        assert_eq!(unused1.len(), 2);
        assert!(unused1.iter().any(|(name, _)| name == "ItemA_UnusedImport"));
        assert!(unused1.iter().any(|(name, _)| name == "ItemB_UsedImport"));

        // Mark ItemB used
        scope.mark_used("ItemB_UsedImport");

        // Second check
        let unused2 = scope.get_unused_imports();
        assert_eq!(unused2.len(), 1);
        assert_eq!(unused2[0].0, "ItemA_UnusedImport");
        assert_eq!(unused2[0].1.symbol, symbol1);

        // Mark ItemA used
        scope.mark_used("ItemA_UnusedImport");
        let unused3 = scope.get_unused_imports();
        assert!(unused3.is_empty());

        // Try marking a non-existent item (should not panic)
        scope.mark_used("NonExistent");
        let unused4 = scope.get_unused_imports();
        assert!(unused4.is_empty());

        // Try marking local item (should have no effect on unused *imports*)
        scope.mark_used("ItemC_Local");
        let unused5 = scope.get_unused_imports();
        assert!(unused5.is_empty());
        assert!(scope.items.get("ItemC_Local").unwrap().is_used); // Check local marked
    }

    // --- Tests for is_accessible --- 

    // Helper to create DefinitionInfo for accessibility tests
    fn create_def(defs: &mut HashMap<Symbol, DefinitionInfo>, symbol: Symbol, parent: Option<Symbol>, name: &str, kind: DefinitionKind, is_public: bool) {
        let info = DefinitionInfo {
            symbol,
            parent_symbol: parent,
            kind,
            name: name.to_string(),
            full_path: format!("test::{}", name), // Simple path for testing
            is_public,
            span: dummy_span(),
            ast_item: None,
            source_file: None,
            generic_params: None,
            variant_kind: None,
            impl_trait_ast: None,
            impl_type_ast: None,
            supertrait_asts: None,
            is_effectful: false,
            special_kind: None,
            ast_function: None,
            default_body_ast: None,
            assoc_type_bound_asts: None,
        };
        defs.insert(symbol, info);
    }

    // Setup for accessibility tests: crate::mod_a::mod_b, crate::mod_c
    fn setup_accessibility_defs() -> (HashMap<Symbol, DefinitionInfo<'static>>, Symbol, Symbol, Symbol, Symbol) {
        let mut defs = HashMap::new();
        let crate_root = Symbol::fresh();
        let mod_a = Symbol::fresh();
        let mod_b = Symbol::fresh();
        let mod_c = Symbol::fresh();

        create_def(&mut defs, crate_root, None, "crate", DefinitionKind::Module, true);
        create_def(&mut defs, mod_a, Some(crate_root), "mod_a", DefinitionKind::Module, true);
        create_def(&mut defs, mod_b, Some(mod_a), "mod_b", DefinitionKind::Module, true);
        create_def(&mut defs, mod_c, Some(crate_root), "mod_c", DefinitionKind::Module, false); // Private module

        (defs, crate_root, mod_a, mod_b, mod_c)
    }

    #[test]
    fn test_is_accessible_public_items() {
        let (mut defs, crate_root, mod_a, mod_b, mod_c) = setup_accessibility_defs();

        let pub_item_in_root = Symbol::fresh();
        let pub_item_in_a = Symbol::fresh();
        let pub_item_in_b = Symbol::fresh();
        let pub_item_in_c = Symbol::fresh(); // Public item inside private module

        create_def(&mut defs, pub_item_in_root, Some(crate_root), "PubRootItem", DefinitionKind::Function, true);
        create_def(&mut defs, pub_item_in_a, Some(mod_a), "PubAItem", DefinitionKind::Struct, true);
        create_def(&mut defs, pub_item_in_b, Some(mod_b), "PubBItem", DefinitionKind::Enum, true);
        create_def(&mut defs, pub_item_in_c, Some(mod_c), "PubCItem", DefinitionKind::Function, true);

        // Public items should be accessible from anywhere
        assert!(is_accessible(&defs, crate_root, pub_item_in_root), "root -> PubRootItem");
        assert!(is_accessible(&defs, mod_a, pub_item_in_root), "a -> PubRootItem");
        assert!(is_accessible(&defs, mod_b, pub_item_in_root), "b -> PubRootItem");
        assert!(is_accessible(&defs, mod_c, pub_item_in_root), "c -> PubRootItem");

        assert!(is_accessible(&defs, crate_root, pub_item_in_a), "root -> PubAItem");
        assert!(is_accessible(&defs, mod_a, pub_item_in_a), "a -> PubAItem");
        assert!(is_accessible(&defs, mod_b, pub_item_in_a), "b -> PubAItem");
        assert!(is_accessible(&defs, mod_c, pub_item_in_a), "c -> PubAItem");

        assert!(is_accessible(&defs, crate_root, pub_item_in_b), "root -> PubBItem");
        assert!(is_accessible(&defs, mod_a, pub_item_in_b), "a -> PubBItem");
        assert!(is_accessible(&defs, mod_b, pub_item_in_b), "b -> PubBItem");
        assert!(is_accessible(&defs, mod_c, pub_item_in_b), "c -> PubBItem");

        // Accessing public item inside private module (`mod_c`)
        assert!(is_accessible(&defs, crate_root, pub_item_in_c), "root -> PubCItem (pub in priv mod)"); // Can access the item itself
        assert!(is_accessible(&defs, mod_a, pub_item_in_c), "a -> PubCItem (pub in priv mod)");
        assert!(is_accessible(&defs, mod_b, pub_item_in_c), "b -> PubCItem (pub in priv mod)");
        assert!(is_accessible(&defs, mod_c, pub_item_in_c), "c -> PubCItem (pub in priv mod)");
        // Note: Accessing `mod_c` itself would fail from outside `crate` due to `mod_c` being private.
    }

    #[test]
    fn test_is_accessible_private_items() {
        let (mut defs, crate_root, mod_a, mod_b, mod_c) = setup_accessibility_defs();

        let priv_item_in_root = Symbol::fresh();
        let priv_item_in_a = Symbol::fresh();
        let priv_item_in_b = Symbol::fresh();
        let priv_item_in_c = Symbol::fresh(); // Private item inside private module

        create_def(&mut defs, priv_item_in_root, Some(crate_root), "PrivRootItem", DefinitionKind::Function, false);
        create_def(&mut defs, priv_item_in_a, Some(mod_a), "PrivAItem", DefinitionKind::Struct, false);
        create_def(&mut defs, priv_item_in_b, Some(mod_b), "PrivBItem", DefinitionKind::Enum, false);
        create_def(&mut defs, priv_item_in_c, Some(mod_c), "PrivCItem", DefinitionKind::Function, false);

        // 1. Accessing PrivRootItem (defined in `crate_root`)
        assert!(is_accessible(&defs, crate_root, priv_item_in_root), "root -> PrivRootItem (self)");
        assert!(is_accessible(&defs, mod_a, priv_item_in_root), "a -> PrivRootItem (descendant)");
        assert!(is_accessible(&defs, mod_b, priv_item_in_root), "b -> PrivRootItem (descendant)");
        assert!(is_accessible(&defs, mod_c, priv_item_in_root), "c -> PrivRootItem (descendant)");

        // 2. Accessing PrivAItem (defined in `mod_a`)
        assert!(!is_accessible(&defs, crate_root, priv_item_in_a), "root -> PrivAItem (ancestor DENIED)");
        assert!(is_accessible(&defs, mod_a, priv_item_in_a), "a -> PrivAItem (self)");
        assert!(is_accessible(&defs, mod_b, priv_item_in_a), "b -> PrivAItem (descendant)");
        assert!(!is_accessible(&defs, mod_c, priv_item_in_a), "c -> PrivAItem (sibling DENIED)");

        // 3. Accessing PrivBItem (defined in `mod_b`)
        assert!(!is_accessible(&defs, crate_root, priv_item_in_b), "root -> PrivBItem (ancestor DENIED)");
        assert!(!is_accessible(&defs, mod_a, priv_item_in_b), "a -> PrivBItem (ancestor DENIED)");
        assert!(is_accessible(&defs, mod_b, priv_item_in_b), "b -> PrivBItem (self)");
        assert!(!is_accessible(&defs, mod_c, priv_item_in_b), "c -> PrivBItem (cousin DENIED)");

        // 4. Accessing PrivCItem (defined in `mod_c` which is private to `crate_root`)
        assert!(!is_accessible(&defs, crate_root, priv_item_in_c), "root -> PrivCItem (ancestor DENIED)");
        assert!(!is_accessible(&defs, mod_a, priv_item_in_c), "a -> PrivCItem (cousin DENIED)");
        assert!(!is_accessible(&defs, mod_b, priv_item_in_c), "b -> PrivCItem (cousin DENIED)");
        assert!(is_accessible(&defs, mod_c, priv_item_in_c), "c -> PrivCItem (self)");
    }

     #[test]
    fn test_is_accessible_modules() {
        let (defs, crate_root, mod_a, mod_b, mod_c) = setup_accessibility_defs();

        // Accessing public modules
        assert!(is_accessible(&defs, crate_root, mod_a), "root -> mod_a (public child)");
        assert!(is_accessible(&defs, mod_a, mod_b), "a -> mod_b (public child)");
        assert!(is_accessible(&defs, mod_b, mod_a), "b -> mod_a (public parent)"); // Can name parent
        assert!(is_accessible(&defs, mod_c, mod_a), "c -> mod_a (public sibling)"); // Can name sibling
        assert!(is_accessible(&defs, mod_b, crate_root), "b -> crate_root (public ancestor)"); // Can name root

        // Accessing private module mod_c (defined in crate_root)
        assert!(is_accessible(&defs, crate_root, mod_c), "root -> mod_c (private child)"); // Allowed from parent
        assert!(!is_accessible(&defs, mod_a, mod_c), "a -> mod_c (private sibling DENIED)");
        assert!(!is_accessible(&defs, mod_b, mod_c), "b -> mod_c (private cousin DENIED)");
        assert!(is_accessible(&defs, mod_c, mod_c), "c -> mod_c (self)");
    }

    // Note: Testing `build_module_scopes` and `resolve_use_tree` requires
    // a mock `SyntaxDatabase` and AST for modules with `use` statements,
    // making them suitable for integration tests.
} 