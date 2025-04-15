# parallax-syntax

## Overview

This crate is responsible for parsing source code into an Abstract Syntax Tree (AST) representation for the Parallax programming language. It defines the AST structures, handles parsing of source files, error recovery, and diagnostic reporting.

The syntax crate transforms raw source text into a structured representation that can be used by later compilation phases, and is a crucial link in the compiler pipeline.

## Salsa Integration and Frame Processing

The core responsibility of this crate is to:

1.  Take Frame structures from `parallax-source`
2.  Parse all source files within the Frame
3.  Construct a Module Structure representing the program structure, including inline modules
4.  Provide efficient incremental parsing through Salsa

### Database Definition

```rust
#[salsa::db]
pub trait SyntaxDatabase: parallax_source::SourceDatabase + salsa::Database {
    /// Parse a source file into an Abstract Syntax Tree
    ///
    /// This query function takes a source file and parses it into a syntax tree,
    /// capturing any parse errors that occur.
    fn parse_file<'db>(&'db self, file: SourceFile<'db>) -> ParsedFile<'db>
    where Self: Sized;

    /// Parse an entire frame, producing a ModuleUnit
    ///
    /// This query function analyzes a frame and builds a module tree representing
    /// the structure of modules and their contents within the frame.
    fn parse_frame<'db>(&'db self, frame: Frame<'db>) -> ModuleUnit<'db>
    where Self: Sized;

    /// Resolve module structure for a frame
    ///
    /// This query function analyzes a frame and builds a forest structure of modules
    /// representing the organization of source files in the frame. Each module may
    /// contain child modules, forming a hierarchical forest.
    fn frame_module_structure<'db>(&'db self, frame: Frame<'db>) -> ModuleStructure<'db>
    where Self: Sized;
}
```

### Core Data Structures

```rust
/// Represents a parsed source file with its AST
#[salsa::tracked]
pub struct ParsedFile<'db> {
    /// Root AST node
    #[tracked]
    #[return_ref]
    pub ast: Vec<Item>,

    /// Parsing errors
    #[tracked]
    #[return_ref]
    pub errors: Vec<SyntaxError>,
}

/// Describes how a module was defined
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ModuleOriginKind {
    /// A module defined by a directory in the filesystem
    Directory,
    /// A module defined by a source file
    File,
    /// A module defined inline with mod {} declaration
    Inline,
    /// The crate root module
    CrateRoot,
}

/// Represents a unified module structure that can represent
/// file-based, directory-based, and inline modules
#[salsa::tracked]
pub struct ModuleUnit<'db> {
    /// Module name
    #[id]
    #[return_ref]
    pub name: String,

    /// Full module path
    #[return_ref]
    pub path: String,

    /// Source file (if this is a file-based module)
    #[return_ref]
    pub source: Option<SourceFile<'db>>,

    /// Parsed content (if available)
    #[return_ref]
    pub ast: Option<ParsedFile<'db>>,

    /// Module origin type
    pub origin: ModuleOriginKind,

    /// Child modules
    #[tracked]
    #[return_ref]
    pub children: Vec<ModuleUnit<'db>>,
}

/// Represents the module structure of a frame
#[salsa::tracked]
pub struct ModuleStructure<'db> {
    /// Root modules in the frame (forest structure)
    #[tracked]
    #[return_ref]
    pub roots: Vec<ModuleUnit<'db>>,
}
```

## Structure

The crate is organized as follows:

- `src/lib.rs` - Main entry point, database trait definition, and core query implementations
- `src/ast/` - AST node definitions
    - `src/ast/items.rs` - Module, function, type declarations
    - `src/ast/expr.rs` - Expression nodes
    - `src/ast/pattern.rs` - Pattern matching constructs
    - `src/ast/types.rs` - Type-related AST nodes
    - `src/ast/common.rs` - Common AST elements (Ident, Literal)
- `src/parser/` - Parser implementation
    - `src/parser/mod.rs` - Main parser coordination (`ParallaxParser`, `parse_source_file`)
    - `src/parser/common.rs` - Common parsing utilities (spans, paths, node helpers)
    - `src/parser/items.rs` - Parse declarations and items (functions, structs, enums, traits, impls, uses, modules)
    - `src/parser/expr.rs` - Parse expressions
    - `src/parser/types.rs` - Parse type declarations
    - `src/parser/pattern.rs` - Parse pattern matching
    - `src/parser/literals.rs` - Parse literal values (strings, numbers, bools, chars)
    - `src/parser/calls.rs` - Parse function call expressions and arguments
    - `src/parser/structs.rs` - Parse struct definitions and bodies
    - `src/parser/scanner.rs` - Utility for scanning parser code for feature coverage
    - `src/parser/validate.rs` - Utility for validating parser coverage against EBNF
- `src/error.rs` - Error types (`SyntaxError`) and diagnostic reporting
- `src/location.rs` - Source location management (`Position`, `Range`, `Location`)
- `src/visitor.rs` - AST visitor pattern implementation

## Implementation Details

### Parsing Implementation

The parser uses the `tree-sitter-parallax` grammar to generate an initial concrete syntax tree (CST) and then converts relevant parts into our AST structures. Error handling combines tree-sitter's error nodes with our custom AST validation.

```rust
/// Parse a source file to produce a ParsedFile
#[salsa::tracked]
pub fn parse_file_query<'db>(db: &'db dyn SyntaxDatabase, file: SourceFile<'db>) -> ParsedFile<'db> {
    let mut parser = ParallaxParser::new().unwrap();
    let content = file.contents(db);

    // Parse source into AST
    let (ast, errors) = parser.parse_ast(&content);

    // Create ParsedFile directly from the tuple components
    ParsedFile::new(db, ast, errors)
}
```

### Module Tree Construction

The `parse_frame_query` builds a `ModuleUnit` tree representing the structure defined by directories and files within the frame. It also handles inline modules defined within files.

```rust
/// Parse an entire frame to produce a ModuleUnit
#[salsa::tracked]
pub fn parse_frame_query<'db>(db: &'db dyn SyntaxDatabase, frame: Frame<'db>) -> ModuleUnit<'db> {
    // Get root directory from frame
    let root_dir = frame.root(db);
    let package_name = frame.config(db).inner(db).package.name.clone();
    build_module_tree(db, frame, &root_dir, &package_name, &package_name)
}

/// Build a module tree from a directory
fn build_module_tree<'db>(
    db: &'db dyn SyntaxDatabase,
    frame: Frame<'db>,
    dir: &Dir<'db>,
    name: &str,
    path: &str,
) -> ModuleUnit<'db> {
    let mut children = Vec::new();

    // Process subdirectories as modules
    for subdir in dir.dirs(db) {
        let dir_name = subdir.name(db);
        let sub_path = format!("{}::{}", path, dir_name);
        let child_module = build_module_tree(db, frame, subdir, &dir_name, &sub_path);
        children.push(child_module);
    }

    // Process source files
    for file in dir.files(db) {
        // Only process .plx files
        if !file.location(db).ends_with(".plx") {
            continue;
        }

        // Parse the file using the query function directly
        let parsed = parse_file_query(db, file);

        let file_name = file.location(db)
            .split(|c| c == '/' || c == '\')
            .last()
            .unwrap_or("unnamed")
            .replace(".plx", "");

        let file_path = format!("{}::{}", path, file_name);

        // Extract module declarations from the parsed file
        let module_declarations = extract_module_declarations(&parsed.ast(db));

        // Create child modules for each module declaration
        let mut file_children = Vec::new();
        for (mod_name, mod_items) in module_declarations {
            let inline_parsed = ParsedFile::new(db, mod_items, Vec::new());
            let inline_path = format!("{}::{}", file_path, mod_name);
            let inline_module = ModuleUnit::new(
                db, mod_name, inline_path, None, Some(inline_parsed), ModuleOriginKind::Inline, Vec::new()
            );
            file_children.push(inline_module);
        }

        // Create module for the file with its inline module children
        let leaf_module = ModuleUnit::new(
            db, file_name, file_path, Some(file), Some(parsed), ModuleOriginKind::File, file_children
        );
        children.push(leaf_module);
    }

    let origin = if name == path { ModuleOriginKind::CrateRoot } else { ModuleOriginKind::Directory };
    ModuleUnit::new(db, name.to_string(), path.to_string(), None, None, origin, children)
}

/// Build a module structure mapping from a frame
#[salsa::tracked]
fn frame_module_structure_query<'db>(db: &'db dyn SyntaxDatabase, frame: Frame<'db>) -> ModuleStructure<'db> {
    // Simply wrap the result of parse_frame_query in a ModuleStructure
    let root_module = parse_frame_query(db, frame);
    ModuleStructure::new(db, vec![root_module])
}
```

### Error Handling

Parsing errors (`SyntaxError`) are collected during the `parse_file_query`. These errors include both low-level tree-sitter errors and higher-level errors found during the AST construction phase. The `ParsedFile` struct stores these errors alongside the generated AST (which might be partial in case of errors).

## Usage

This crate is used in the compiler pipeline after source file loading and frame resolution (`parallax-source`). It takes frames and produces module structures (`ModuleStructure`) containing parsed ASTs (`ParsedFile`) for subsequent name resolution and type checking phases.

## Dependencies

- `parallax-source` - For source file management (`SourceFile`), frame structures (`Frame`), and directory/file access (`Dir`).
- `tree-sitter` - Core library for the tree-sitter parsing framework.
- `tree-sitter-parallax` - The specific tree-sitter grammar for the Parallax language.
- `salsa` - For incremental computation and caching of parsing results.
- `miette` - For rich diagnostic error reporting.
- `thiserror` - For defining custom error types (`SyntaxError`).
- `serde` - For potential serialization needs (though not directly used in core parsing logic shown).
- `regex` - Used internally by the parser scanner/validator utilities.
- `parallax-utils` - Common utilities potentially shared across crates.

## Contributing

See the repository's main README for general contribution guidelines. 