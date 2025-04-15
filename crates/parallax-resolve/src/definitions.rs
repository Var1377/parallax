// Pass 1: Collect all top-level definitions (structs, enums, functions)
// from the module structure.

use std::collections::HashMap;
use parallax_syntax::{ModuleUnit, SyntaxDatabase};
use parallax_syntax::parse_file_query;
use parallax_syntax::ast::{self, items::{Item, ItemKind, TraitBound}};
use parallax_source::Dir;
use crate::types::Symbol; // Updated to use Symbol instead of DefinitionPath
use crate::error::ResolutionError;
use miette::SourceSpan;

/// Describes the kind of an enum variant's structure.
#[derive(Debug, Clone, PartialEq, Eq, Hash)] // Added PartialEq, Eq, Hash
pub enum VariantKind {
    /// A variant with no associated data (e.g., `Option::None`).
    Unit,
    /// A variant with unnamed, ordered fields (e.g., `Result::Ok(T)`).
    /// Stores the original AST types, to be resolved later.
    Tuple(Vec<ast::types::Type>), // Store original AST types to resolve later
    /// A variant with named fields (e.g., `Message::Data { id: i32 }`).
    /// Stores field names and their original AST types.
    Struct(Vec<(String, ast::types::Type)>), // Field name and AST type
}

/// Intermediate representation for a discovered top-level definition during Pass 1.
///
/// This struct holds basic information extracted directly from the AST before
/// full name resolution or type checking occurs. It serves as input for subsequent passes.
#[derive(Debug, Clone)]
pub struct DefinitionInfo<'db> {
    /// The unique symbol assigned to this definition.
    pub symbol: Symbol,
    /// The symbol of the parent module or definition (e.g., the enum for an enum variant).
    /// `None` only for the root module.
    pub parent_symbol: Option<Symbol>,
    /// The kind of definition (Struct, Enum, Function, etc.).
    pub kind: DefinitionKind,
    /// The simple name of the definition (e.g., "MyStruct", "my_function").
    /// For impls, this might be a generated descriptive name like `<impl Trait for Type>`.
    pub name: String,
    /// Cached fully qualified path string (e.g., "my_crate::my_mod::MyStruct").
    /// For impls, the path might be less canonical.
    pub full_path: String,
    /// Whether the definition is declared as public (`pub`).
    pub is_public: bool,
    /// The source span covering the definition in the original source code.
    pub span: SourceSpan,
    /// A reference to the original AST item node.
    /// This is used by later passes to access details like function bodies or field types.
    /// `None` for definitions not directly represented by a single `Item` (like Module).
    pub ast_item: Option<&'db Item>,
    /// Stores the names of generic parameters declared on the item (e.g., `["T", "U"]`).
    /// `None` if the item is not generic.
    pub generic_params: Option<Vec<String>>, // Store generic param names if applicable
    /// Describes the structure of an enum variant. `None` for non-EnumVariant kinds.
    pub variant_kind: Option<VariantKind>, // Only populated for EnumVariant
    // Fields specific to Impl definitions
    /// Reference to the AST node of the trait being implemented (e.g., `Some(Display)`).
    /// `None` for inherent impls.
    pub impl_trait_ast: Option<&'db ast::types::Type>, // AST node for the implemented trait
    /// Reference to the AST node of the type implementing the trait (the `Self` type).
    pub impl_type_ast: Option<&'db ast::types::Type>, // AST node for the implementing type
    // Field specific to Trait definitions
    /// References to the AST nodes of the supertraits declared for this trait.
    pub supertrait_asts: Option<Vec<&'db TraitBound>>, // Store TraitBound AST nodes
    /// Metadata: Indicates if the function has side effects and needs capability passing.
    pub is_effectful: bool, // Added field
    /// Indicates if this definition has special compiler support (core trait, intrinsic).
    pub special_kind: Option<SpecialDefinitionKind>, // New field
}

/// Discriminant for the different kinds of top-level definitions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefinitionKind {
    Struct,
    Enum,
    EnumVariant,
    Function,
    Module,
    Trait,
    Impl,
}

/// Indicates definitions with special compiler support.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SpecialDefinitionKind {
    CoreTrait,
    Intrinsic,
    // Potentially others later
}

/// Helper function to create a fully qualified path string.
///
/// Joins a module path and an item name with `::`.
/// If the module path is empty (indicating the root), only the name is returned.
pub(crate) fn make_full_path(module_path: &str, name: &str) -> String {
    if module_path.is_empty() {
        name.to_string()
    } else {
        format!("{}::{}", module_path, name)
    }
}

/// Generates a descriptive (but not necessarily unique) name for an impl block based on its AST.
/// Used for the `name` field in `DefinitionInfo` for `Impl` kinds.
/// Example outputs: `<impl MyTrait for MyType>`, `<impl MyType>`
fn make_impl_name(impl_def: &ast::items::ImplDef) -> String {
    let trait_part = impl_def.trait_type.as_ref().map_or_else(
        || "".to_string(), // No space needed for inherent impls
        |t| format!("{:?} for ", t) // Add trait name and space for trait impls
    );
    // Format consistently: <impl Trait for Type> or <impl Type>
    format!("<impl {}{}>", trait_part, format!("{:?}", impl_def.self_type))
}

/// Helper function to process items within a parsed file/module.
fn process_items_in_module<'db>(
    _db: &'db dyn SyntaxDatabase,
    items: &'db [Item], // Takes slice of Item
    module_path_str: &str,
    module_symbol: Symbol,
    definitions_map: &mut HashMap<Symbol, DefinitionInfo<'db>>,
    errors: &mut Vec<ResolutionError>,
) {
    // DEBUG LOG
    println!("DEBUG [process_items_in_module]: Processing items for module path: '{}', module_symbol: {:?}", module_path_str, module_symbol);
    for item in items { // item is &'db Item here
        let (kind, name_ident_opt, generics_opt, trait_ast_opt_ref, type_ast_opt_ref, supertrait_asts_opt_ref) = match &item.kind {
            ItemKind::Struct(s) => (DefinitionKind::Struct, Some(&s.name), s.generic_params.as_ref(), None, None, None),
            ItemKind::Enum(e) => (DefinitionKind::Enum, Some(&e.name), e.generic_params.as_ref(), None, None, None),
            ItemKind::Function(f) => {
                // --- ADDED DEBUG LOG ---
                println!("  -> Found Function item: name='{}', span={:?}", f.name.name, f.span);
                // --- END ADDED DEBUG LOG ---
                (DefinitionKind::Function, Some(&f.name), f.generic_params.as_ref(), None, None, None)
            },
            ItemKind::Trait(t) => (DefinitionKind::Trait, Some(&t.name), t.generic_params.as_ref(), None, None, Some(t.supertraits.iter().collect::<Vec<_>>())),
            ItemKind::Impl(i) => (DefinitionKind::Impl, None, i.generic_params.as_ref(), i.trait_type.as_ref(), Some(&i.self_type), None),
            _ => continue,
        };
        
        let item_symbol = Symbol::fresh();
        let generic_names = generics_opt.map(|gp| gp.iter().map(|p| p.name.name.clone()).collect());

        let (name, full_path) = if let Some(ident) = name_ident_opt {
            let n = ident.name.clone();
            let fp = make_full_path(module_path_str, &n);
            (n, fp)
        } else if let ItemKind::Impl(i) = &item.kind {
            let n = make_impl_name(i);
            let fp = make_full_path(module_path_str, &format!("impl-{}", item_symbol.id()));
            (n, fp)
        } else {
            errors.push(ResolutionError::InternalError { message: "Failed to determine name/path for item".to_string(), span: Some(item.span) });
            continue;
        };


        // --- Effectful Function Check ---
        let mut is_effectful = false;
        if kind == DefinitionKind::Function {
            if full_path == "std::io::println" || full_path == "std::io::readln" || full_path == "std::panic::panic" {
                is_effectful = true;
            }
        }
        // --- End Effectful Check ---

        // --- Duplicate Definition Check ---
        let mut duplicate_found = false;
        if name_ident_opt.is_some() {
            for existing_def in definitions_map.values() {
                if existing_def.parent_symbol == Some(module_symbol) && existing_def.name == name {
                    println!("  -> !! Duplicate check: Found existing='{}' ({:?}) with parent {:?} matching current='{}' ({:?})",
                             existing_def.name, existing_def.symbol, existing_def.parent_symbol, name, item_symbol);
                    errors.push(ResolutionError::DuplicateDefinition {
                        name: name.clone(),
                        span: item.span,
                        previous_span: existing_def.span,
                    });
                    duplicate_found = true;
                    break;
                }
            }
        }
        if duplicate_found && name_ident_opt.is_some() {
            println!("  -> !! Duplicate found for '{}' ({:?}), skipping insertion.", name, item_symbol);
            continue;
        }
        // --- End Duplicate Check ---


        // Now we can store the reference because `item` comes from the Salsa-managed slice
        let def_info = DefinitionInfo {
            symbol: item_symbol,
            parent_symbol: Some(module_symbol),
            kind,
            name: name.clone(),
            full_path: full_path.clone(),
            is_public: item.visibility,
            span: item.span,
            ast_item: Some(item), // Store the reference &'db Item
            generic_params: generic_names.clone(),
            variant_kind: None,
            impl_trait_ast: trait_ast_opt_ref,
            impl_type_ast: type_ast_opt_ref,
            supertrait_asts: supertrait_asts_opt_ref,
            is_effectful,
            special_kind: None,
        };

        println!("  -> Inserting definition: name='{}', symbol={:?}, parent={:?}, kind={:?}",
                 def_info.name, def_info.symbol, def_info.parent_symbol, def_info.kind);
        definitions_map.insert(item_symbol, def_info.clone());

        // Process Enum Variants
        if let ItemKind::Enum(enum_def) = &item.kind {
             for variant in &enum_def.variants {
                let variant_name = variant.name.name.clone();
                let variant_path_str = make_full_path(&full_path, &variant_name);
                let variant_symbol = Symbol::fresh();

                let variant_kind_enum = match &variant.kind {
                    ast::items::EnumVariantKind::Unit => VariantKind::Unit,
                    ast::items::EnumVariantKind::Tuple(types) => VariantKind::Tuple(types.clone()),
                    ast::items::EnumVariantKind::Struct(fields) => {
                        let field_infos = fields.iter().map(|f| (f.name.name.clone(), f.ty.clone())).collect();
                        VariantKind::Struct(field_infos)
                    }
                };

                let variant_def_info = DefinitionInfo {
                    symbol: variant_symbol,
                    parent_symbol: Some(item_symbol),
                    kind: DefinitionKind::EnumVariant,
                    name: variant_name.clone(),
                    full_path: variant_path_str.clone(),
                    is_public: item.visibility,
                    span: variant.span,
                    ast_item: Some(item), // Point to the enum's AST item
                    generic_params: generic_names.clone(),
                    variant_kind: Some(variant_kind_enum),
                    impl_trait_ast: None,
                    impl_type_ast: None,
                    supertrait_asts: None,
                    is_effectful: false,
                    special_kind: None,
                };
                
                // ... (variant duplicate check) ...
                let mut variant_duplicate = false;
                for existing_def in definitions_map.values() {
                    if existing_def.kind == DefinitionKind::EnumVariant &&
                       existing_def.parent_symbol == Some(item_symbol) &&
                       existing_def.name == variant_name
                    {
                        errors.push(ResolutionError::DuplicateDefinition {
                            name: variant_name.clone(),
                            span: variant.span,
                            previous_span: existing_def.span,
                        });
                        variant_duplicate = true;
                        break;
                    }
                }
                if !variant_duplicate {
                    definitions_map.insert(variant_symbol, variant_def_info.clone());
                    // DEBUG LOG (moved after insert)
                    // Log the info that was potentially inserted
                    println!("    -> Adding variant: name='{}', symbol={:?}, full_path='{}'", variant_def_info.name, variant_def_info.symbol, variant_def_info.full_path);
                }
            }
        }

        // <<< ADDED: Process items inside Trait definitions >>>
        if let ItemKind::Trait(trait_def) = &item.kind {
            let trait_symbol = item_symbol; // Symbol of the trait itself
            let trait_base_path = &full_path; // Path like "my_mod::MyTrait"

            for trait_item in &trait_def.items {
                match trait_item {
                    // Use the correct field name 'function'
                    ast::items::TraitItem::Method { function: func_ast, .. } => {
                        // Access fields from the Function struct bound to func_ast
                        let func_name = func_ast.name.name.clone();
                        let func_symbol = Symbol::fresh();
                        let func_full_path = format!("{}::{}", trait_base_path, func_name);
                        let func_generic_names = func_ast.generic_params.as_ref().map(|gp| gp.iter().map(|p| p.name.name.clone()).collect());

                        // Check for duplicate *methods within this trait*
                        let mut duplicate_method = false;
                        for existing_def in definitions_map.values() {
                            // Check parent is this trait AND name matches
                            if existing_def.parent_symbol == Some(trait_symbol) && existing_def.name == func_name {
                                errors.push(ResolutionError::DuplicateDefinition {
                                    name: func_name.clone(),
                                    span: func_ast.span, // Use span from the Function struct
                                    previous_span: existing_def.span,
                                });
                                duplicate_method = true;
                                break;
                            }
                        }
                        if duplicate_method { continue; }

                        let func_def_info = DefinitionInfo {
                            symbol: func_symbol,
                            parent_symbol: Some(trait_symbol),
                            kind: DefinitionKind::Function, // Treat trait method sigs like function defs for now
                            name: func_name.clone(),
                            full_path: func_full_path.clone(),
                            is_public: true, // Trait methods are implicitly public interface
                            span: func_ast.span, // Use span from the Function struct
                            // Keep ast_item as None, it still points to the parent Trait item.
                            // The resolver needs the Function AST node from func_ast later, 
                            // but DefinitionInfo doesn't store it directly for sub-items yet.
                            ast_item: None, 
                            generic_params: func_generic_names,
                            variant_kind: None,
                            impl_trait_ast: None,
                            impl_type_ast: None,
                            supertrait_asts: None,
                            is_effectful: false, // Assume trait sigs are not effectful by default
                            special_kind: None,
                        };
                        println!("    -> Inserting Trait Method definition: name='{}', symbol={:?}, parent={:?}",
                                func_def_info.name, func_def_info.symbol, func_def_info.parent_symbol);
                        definitions_map.insert(func_symbol, func_def_info.clone());
                    }
                     ast::items::TraitItem::AssociatedType { .. } => {
                         // TODO: Handle associated types if needed
                         println!("    -> Found associated type in trait {}, skipping definition collection for now.", trait_base_path);
                     }
                }
            }
        }
        // <<< END ADDED SECTION >>>

        // Process items inside Impl blocks
        if let ItemKind::Impl(impl_def) = &item.kind {
            // Use the impl block's symbol (item_symbol) as the parent
            // Use the impl block's full_path (full_path) as a base for the method path
            let impl_symbol = item_symbol;
            let impl_base_path = &full_path; // Path like "my_mod::impl-123"

            for impl_item in &impl_def.items {
                // Match directly on the ImplItem enum
                match impl_item {
                    ast::items::ImplItem::Method(func_ast) => {
                        let func_name = func_ast.name.name.clone();
                        let func_symbol = Symbol::fresh();
                        // Construct path based on impl's path and func name
                        let func_full_path = format!("{}::{}", impl_base_path, func_name); 
                        let func_generic_names = func_ast.generic_params.as_ref().map(|gp| gp.iter().map(|p| p.name.name.clone()).collect());
                        
                        // Check for effects (can reuse logic)
                        let mut func_is_effectful = false;
                        if func_full_path.contains("std::io::println") || func_full_path.contains("std::panic::panic") {
                            func_is_effectful = true;
                        }
                        // TODO: Add check for intrinsics based on full_path?

                        // Check for duplicates *within this impl block*
                        let mut duplicate_method = false;
                        for existing_def in definitions_map.values() {
                            if existing_def.parent_symbol == Some(impl_symbol) && existing_def.name == func_name {
                                errors.push(ResolutionError::DuplicateDefinition {
                                    name: func_name.clone(),
                                    span: func_ast.span,
                                    previous_span: existing_def.span,
                                });
                                duplicate_method = true;
                                break;
                            }
                        }
                        if duplicate_method { continue; }

                        let func_def_info = DefinitionInfo {
                            symbol: func_symbol,
                            parent_symbol: Some(impl_symbol),
                            kind: DefinitionKind::Function,
                            name: func_name.clone(),
                            full_path: func_full_path.clone(),
                            is_public: true,
                            span: func_ast.span,
                            ast_item: None, // Cannot store reference to Function within ImplItem easily
                            generic_params: func_generic_names,
                            variant_kind: None,
                            impl_trait_ast: None,
                            impl_type_ast: None,
                            supertrait_asts: None,
                            is_effectful: func_is_effectful,
                            special_kind: None, // TODO: Check for intrinsics
                        };

                        definitions_map.insert(func_symbol, func_def_info.clone());
                    }
                    ast::items::ImplItem::AssociatedType { name, ty: _, span: _ } => {
                        // TODO: Optionally handle associated types here if they need DefinitionInfo entries.
                        // For now, just print a debug message.
                        println!("    -> Found associated type '{}' in impl {}, skipping definition collection for now.", name.name, impl_base_path);
                    }
                }
            }
        }
    }
}

/// Recursive helper for traversing ModuleUnit structure.
pub(crate) fn traverse_module_unit<'db>(
    db: &'db dyn SyntaxDatabase,
    module: ModuleUnit<'db>,
    definitions_map: &mut HashMap<Symbol, DefinitionInfo<'db>>,
    errors: &mut Vec<ResolutionError>,
    parent_module_symbol: Option<Symbol>,
    current_path_prefix: &str,
) {
    let module_name = module.name(db);
    // Handle root module path explicitly
    let module_path_str = if parent_module_symbol.is_none() {
        String::new() // Root module has an empty path (String)
    } else {
        make_full_path(current_path_prefix, &module_name)
    };
    let module_symbol = Symbol::fresh();

    let module_span = module.ast(db)
        .and_then(|parsed_file| {
            parsed_file.ast(db).first().map(|item_ref| item_ref.span)
        })
        .unwrap_or_else(|| miette::SourceSpan::from((0, 0)));

    let module_info = DefinitionInfo {
        symbol: module_symbol,
        parent_symbol: parent_module_symbol,
        kind: DefinitionKind::Module,
        name: module_name.clone(),
        full_path: module_path_str.clone(),
        is_public: true,
        span: module_span,
        ast_item: None, // ModuleUnit doesn't map to a single item
        generic_params: None,
        variant_kind: None,
        impl_trait_ast: None,
        impl_type_ast: None,
        supertrait_asts: None,
        is_effectful: false,
        special_kind: None,
    };
    definitions_map.insert(module_symbol, module_info.clone());

    // DEBUG LOG: Confirm module insertion
    println!(
        "DEBUG [traverse_module_unit]: Inserted module: name='{}', symbol={:?}, parent={:?}",
        module_info.name, module_info.symbol, module_info.parent_symbol
    );

    // Process items using the helper function
    if let Some(parsed_file) = module.ast(db) {
        let items = parsed_file.ast(db);
        // Pass module_path_str as a slice for process_items_in_module
        process_items_in_module(db, &items, &module_path_str, module_symbol, definitions_map, errors);
    }

    // Recursively process child ModuleUnits
    let children = module.children(db);
    for child in children {
        // Pass module_path_str as a slice for recursive calls
        traverse_module_unit(db, *child, definitions_map, errors, Some(module_symbol), &module_path_str);
    }
}

/// Recursive helper for traversing Dir structure.
pub(crate) fn traverse_dir<'db>(
    db: &'db dyn SyntaxDatabase,
    dir: Dir<'db>,
    definitions_map: &mut HashMap<Symbol, DefinitionInfo<'db>>,
    errors: &mut Vec<ResolutionError>,
    parent_module_symbol: Option<Symbol>,
    current_path_prefix: &str,
) {
    let dir_name = dir.name(db); // Renamed from module_name for clarity
    let dir_path_str = make_full_path(current_path_prefix, &dir_name);
    let dir_symbol = Symbol::fresh(); // Symbol for the directory itself

    // DEBUG LOG
    println!(
        "DEBUG [traverse_dir]: Entering dir: '{}', path_prefix: '{}', parent: {:?}, assigned_symbol: {:?}",
        dir_name, current_path_prefix, parent_module_symbol, dir_symbol
    );

    // --- Create DefinitionInfo for the Directory Module ---
    let mut dir_span = miette::SourceSpan::from((0, 0)); // Default span
    // Try to get a span from the first file within the directory for the dir module
    if let Some(first_file) = dir.files(db).first() {
        let parsed_file = parse_file_query(db, *first_file);
        if let Some(first_item) = parsed_file.ast(db).first() {
            dir_span = first_item.span;
        }
    }

    let dir_module_info = DefinitionInfo {
        symbol: dir_symbol,
        parent_symbol: parent_module_symbol,
        kind: DefinitionKind::Module,
        name: dir_name.clone(),
        full_path: dir_path_str.clone(),
        is_public: true, // Assume stdlib dirs/files map to public modules for now
        span: dir_span,
        ast_item: None,
        generic_params: None,
        variant_kind: None,
        impl_trait_ast: None,
        impl_type_ast: None,
        supertrait_asts: None,
        is_effectful: false,
        special_kind: None,
    };
    definitions_map.insert(dir_symbol, dir_module_info);

    // --- Process Files as Submodules ---
    for file in dir.files(db) {
        // Extract file name without extension to use as module name
        let file_location = file.location(db); // e.g., "std/result.plx"
        let file_name_full = std::path::Path::new(&file_location)
            .file_name()
            .and_then(|os_str| os_str.to_str())
            .unwrap_or(""); // e.g., "result.plx"
        let file_module_name = std::path::Path::new(file_name_full)
            .file_stem()
            .and_then(|os_str| os_str.to_str())
            .unwrap_or(""); // e.g., "result"

        // Skip if name is empty (shouldn't happen with valid files)
        if file_module_name.is_empty() {
            continue;
        }

        let file_module_path = make_full_path(&dir_path_str, file_module_name);
        let file_module_symbol = Symbol::fresh();

        // DEBUG LOG
        println!(
            "  -> Processing file as module: name='{}', path='{}', parent={:?}, assigned_symbol={:?}",
            file_module_name, file_module_path, Some(dir_symbol), file_module_symbol
        );


        let parsed_file = parse_file_query(db, file);
        let items_slice = parsed_file.ast(db);

        // Determine span for the file module (use first item's span)
        let file_module_span = items_slice.first().map_or(dir_span, |item| item.span); // Fallback to dir span

        // Create DefinitionInfo for the file-module
        let file_module_info = DefinitionInfo {
            symbol: file_module_symbol,
            parent_symbol: Some(dir_symbol), // Parent is the directory module
            kind: DefinitionKind::Module,
            name: file_module_name.to_string(),
            full_path: file_module_path.clone(),
            is_public: true, // Assume public
            span: file_module_span,
            ast_item: None, // No single AST item represents the file-module
            generic_params: None,
            variant_kind: None,
            impl_trait_ast: None,
            impl_type_ast: None,
            supertrait_asts: None,
            is_effectful: false,
            special_kind: None,
        };
         definitions_map.insert(file_module_symbol, file_module_info);


        // Report parsing errors from this file
        for parse_error in parsed_file.errors(db) {
            use parallax_syntax::SyntaxError; // Add local import
            let error_span: Option<SourceSpan> = match parse_error {
                 SyntaxError::ParseError { span, .. } => *span,
                 SyntaxError::NodeError { span, .. } => *span,
                 SyntaxError::AstError { span, .. } => *span,
                 SyntaxError::SyntaxError { span, .. } => *span,
                 _ => None,
            };
            errors.push(ResolutionError::SyntaxError {
                message: format!("{}", parse_error),
                span: error_span,
                location: Some(file_location.clone()), // Clone moved value
            });
        }

        // Process items *within* this file, associating them with the file-module symbol
        process_items_in_module(
            db,
            items_slice,
            &file_module_path,    // Use file module's path
            file_module_symbol, // Use file module's symbol
            definitions_map,
            errors
        );
    }

    // --- Recursively process child Dirs ---
    let sub_dirs = dir.dirs(db);
    for sub_dir in sub_dirs {
        // Parent symbol for subdirectories is the current directory's symbol
        traverse_dir(db, *sub_dir, definitions_map, errors, Some(dir_symbol), &dir_path_str);
    }
}

#[cfg(test)]
mod tests {
    use super::*; // Import items from parent module
    use parallax_syntax::ast::items::{ImplDef, StructDef};
    use parallax_syntax::ast::types::{Type, TypeKind};
    use parallax_syntax::ast::common::Ident;
    use miette::SourceSpan;

    // Helper to create a dummy span
    fn dummy_span() -> SourceSpan {
        SourceSpan::from((0, 0))
    }

    #[test]
    fn test_make_full_path() {
        assert_eq!(make_full_path("", "MyItem"), "MyItem");
        assert_eq!(make_full_path("my_mod", "MyItem"), "my_mod::MyItem");
        assert_eq!(make_full_path("crate::utils", "Helper"), "crate::utils::Helper");
    }

    // Test make_impl_name
    #[test]
    fn test_make_impl_name() {
        // Mock AST nodes needed for the test
        let type_name = || Ident { name: "MyType".to_string(), span: dummy_span() };
        let trait_name = || Ident { name: "MyTrait".to_string(), span: dummy_span() };
        let dummy_type = || Type { kind: TypeKind::Path(vec![type_name()]), span: dummy_span() };
        let dummy_trait_type = || Type { kind: TypeKind::Path(vec![trait_name()]), span: dummy_span() };

        // Inherent impl: `impl MyType { ... }`
        let inherent_impl = ImplDef {
            generic_params: None,
            trait_type: None,
            self_type: dummy_type(),
            where_clause: None,
            items: vec![],
            span: dummy_span(),
        };
        // Note: Debug format includes struct names and quotes
        expect_test::expect!["<impl Type { kind: Path([Ident { name: \"MyType\", span: SourceSpan { offset: SourceOffset(0), length: 0 } }]), span: SourceSpan { offset: SourceOffset(0), length: 0 } }>"]
            .assert_eq(&make_impl_name(&inherent_impl));

        // Trait impl: `impl MyTrait for MyType { ... }`
        let trait_impl = ImplDef {
            generic_params: None,
            trait_type: Some(dummy_trait_type()),
            self_type: dummy_type(),
            where_clause: None,
            items: vec![],
            span: dummy_span(),
        };
        // Note: Debug format includes struct names and quotes
        expect_test::expect!["<impl Type { kind: Path([Ident { name: \"MyTrait\", span: SourceSpan { offset: SourceOffset(0), length: 0 } }]), span: SourceSpan { offset: SourceOffset(0), length: 0 } } for Type { kind: Path([Ident { name: \"MyType\", span: SourceSpan { offset: SourceOffset(0), length: 0 } }]), span: SourceSpan { offset: SourceOffset(0), length: 0 } }>"]
            .assert_eq(&make_impl_name(&trait_impl));
            
        // Test with more complex types in Debug format
         let complex_type = || Type {
             kind: TypeKind::KindApp(
                 Box::new(Type { kind: TypeKind::Path(vec![Ident { name: "Vec".to_string(), span: dummy_span() }]), span: dummy_span() }),
                 vec![Type { kind: TypeKind::Path(vec![Ident { name: "i32".to_string(), span: dummy_span() }]), span: dummy_span() }]
             ),
             span: dummy_span()
         };
         let complex_trait_impl = ImplDef {
            generic_params: None,
            trait_type: Some(dummy_trait_type()),
            self_type: complex_type(),
            where_clause: None,
            items: vec![],
            span: dummy_span(),
        };
         expect_test::expect!["<impl Type { kind: Path([Ident { name: \"MyTrait\", span: SourceSpan { offset: SourceOffset(0), length: 0 } }]), span: SourceSpan { offset: SourceOffset(0), length: 0 } } for Type { kind: KindApp(Type { kind: Path([Ident { name: \"Vec\", span: SourceSpan { offset: SourceOffset(0), length: 0 } }]), span: SourceSpan { offset: SourceOffset(0), length: 0 } }, [Type { kind: Path([Ident { name: \"i32\", span: SourceSpan { offset: SourceOffset(0), length: 0 } }]), span: SourceSpan { offset: SourceOffset(0), length: 0 } }]), span: SourceSpan { offset: SourceOffset(0), length: 0 } }>"]
            .assert_eq(&make_impl_name(&complex_trait_impl));
    }

    // Note: Testing `collect_definitions` and `traverse_module_unit` requires
    // a mock `SyntaxDatabase` and representative AST structures, making them suitable
    // for integration tests rather than simple unit tests here.
} 