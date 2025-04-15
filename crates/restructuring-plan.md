# Parallax Compiler Restructuring Plan with Salsa 0.19.0

This document outlines the step-by-step plan for restructuring each crate in the Parallax compiler to use the Salsa 0.19.0 framework for incremental compilation. Each crate has been analyzed to ensure it has a single, clear responsibility, and some crates may be split to maintain this principle.

## Salsa 0.19.0 Implementation Strategy

In alignment with our Parallax codebase's implementation pattern, we'll use the following Salsa constructs:

1. **Database Traits with #[salsa::db]**: Each crate will define a trait using the `#[salsa::db]` attribute. This trait will contain the query functions for that specific module.

   ```rust
   #[salsa::db]
   pub trait SourceDatabase: salsa::Database {
       /// Resolves a frame by its configuration path
       fn resolve_frame<'db>(&'db self, config_path: &str) -> Frame<'db> 
       where
           Self: Sized
       {
           let path = Path::new(self, std::path::PathBuf::from(config_path));
           frame::resolve_frame(self, path)
       }
   }
   ```

2. **Input Structs with #[salsa::input]**: For base inputs to the compilation pipeline (like source files), we'll use `#[salsa::input]`. These are structs whose field values are stored in the database and can be modified between compilations.

   ```rust
   #[salsa::input]
   pub struct SourceFile {
       pub path: String,
       #[return_ref]
       pub contents: String,
   }
   ```

   - Input structs are created with `SourceFile::new(db, path, contents)`
   - Fields can be read with getters like `file.path(db)` 
   - Fields can be updated with setters like `file.set_contents(db).to(new_contents)`
   - The `#[return_ref]` attribute returns a reference to avoid cloning

3. **Tracked Structs with #[salsa::tracked]**: For intermediate values in the compilation pipeline (like parsed ASTs), we'll use `#[salsa::tracked]`. These structs represent derived values whose fields are immutable once created.

   ```rust
   #[salsa::tracked]
   pub struct Frame<'db> {
       #[tracked]
       pub config: FrameConfig,
       #[tracked]
       pub config_source: Option<SourceFile<'db>>,
       #[tracked]
       pub root: Dir<'db>,
       #[tracked]
       pub dependencies: FxHashMap<String, Frame<'db>>,
   }
   ```

   - Tracked structs carry a `'db` lifetime ensuring the database remains immutable while they're in use
   - They're created with `Frame::new(db, config, config_source, root, dependencies)`
   - Fields are accessed via getters like `frame.root(db)`
   - Fields marked with `#[tracked]` create dependencies when accessed
   - Cannot be modified after creation

4. **Tracked Functions with #[salsa::tracked]**: For core algorithm functions that need memoization, we'll use `#[salsa::tracked]` on functions:

   ```rust
   #[salsa::tracked]
   pub fn resolve_frame<'db>(db: &'db dyn SourceDatabase, input_path: Path<'db>) -> Frame<'db> {
       // Implementation that reads from the database
       // Result is cached based on the inputs
   }
   ```

   - Tracked functions are the unit of reuse in Salsa
   - First parameter is always the database reference
   - Results are automatically cached
   - Recomputed only when dependencies change
   - The `'db` lifetime ensures the database remains valid

5. **Interned Values with #[salsa::interned]**: For small values that are frequently compared like identifiers, we'll use `#[salsa::interned]`. These ensure that identical values get the same internal ID.

   ```rust
   #[salsa::interned]
   pub struct Symbol {
       #[return_ref]
       name: String,
       kind: SymbolKind,
   }
   ```

   - If you intern the same data twice, you get back the same integer ID
   - Equality comparisons become fast integer comparisons
   - Like tracked values, they carry a `'db` lifetime
   - Perfect for symbols, identifiers, and other small frequently compared values

6. **Return References with #[return_ref]**: For large data structures, we'll use the `#[return_ref]` attribute to avoid unnecessary cloning:

   ```rust
   #[salsa::tracked]
   pub struct Dir<'db> {
       #[tracked]
       pub files: Vec<SourceFile<'db>>,
       #[tracked]
       #[return_ref]
       pub dirs: Vec<Dir<'db>>,
   }
   ```

   - The `#[return_ref]` attribute on a field causes the getter to return a reference
   - For functions, use `#[salsa::tracked(return_ref)]` to return a reference
   - Ideal for collections and large data structures to avoid cloning

7. **Choosing the Right Granularity**: When designing tracked functions, we'll carefully choose the granularity of reuse:

   - For cheap operations like parsing, track at a coarser granularity (e.g., whole file)
   - For expensive operations like type checking, track at a finer granularity (e.g., per function)
   - Consider the cost-benefit of tracking vs. the overhead of tracking itself

8. **Implementation Strategies**:
   - Database traits will be implemented on the central database
   - Standalone functions will be defined for reusability
   - Helper functions for reporting errors through accumulators
   
9. **Central Database Structure**: The central database will implement all database traits:

   ```rust
   // In the central database crate
   #[salsa::db]
   #[derive(Default, Clone)]
   pub struct Compiler {
       storage: salsa::Storage<Self>,
       root_config: PathBuf,
   }
   
   // Storage implementation
   #[salsa::db]
   impl salsa::Database for Compiler {
       fn salsa_event(&self, event: &dyn Fn() -> salsa::Event) {
           let event = event();
           eprintln!("Event: {event:?}");
       }
   }
   
   // Using a macro to implement multiple traits
   macro_rules! impl_source_database {
       ($($db:ident),*) => {
           $(
               #[salsa::db]
               impl $db for Compiler {}
           )*
       };
   }
   
   impl_source_database!(SourceDatabase, SyntaxDatabase);
   ```

10. **Error Handling with Accumulators**: For error reporting, we'll use helper functions to report errors through accumulators:

    ```rust
    // Report an error
    pub fn report_error(db: &dyn SourceDatabase, location: String, error: FrameError) {
        // Report via diagnostic system
        Diagnostics::push(db, ParallaxDiagnostic::new(location, error));
    }
    ```

Here's an example combining these patterns:

```rust
// In a crate that defines database traits
#[salsa::db]
pub trait SourceDatabase: salsa::Database {
    fn read_source_file(&self, path: PathBuf) -> io::Result<SourceFile>;
}

// Helper function implementation
pub fn read_source_file(db: &dyn SourceDatabase, path: PathBuf) -> io::Result<SourceFile> {
    let contents = fs::read_to_string(&path)?;
    Ok(source_file(db, path, contents))
}

// In the central database crate
#[salsa::db]
impl SourceDatabase for ParallaxDatabase {
    fn read_source_file(&self, path: PathBuf) -> io::Result<SourceFile> {
        read_source_file(self, path)
    }
}
```

## Central Database Structure

We'll implement a central database in `parallax-db` that integrates all the database traits:

```rust
// parallax-db/src/database.rs
use std::path::PathBuf;
use salsa::prelude::*;
use std::io;
use parallax_source::{SourceDatabase, SourceFile, read_source_file};

/// The central database for the Parallax compiler that integrates all query groups.
#[salsa::db]
#[derive(Default, Clone)]
pub struct ParallaxDatabase {
    storage: salsa::Storage<Self>,
}

#[salsa::db]
impl salsa::Database for ParallaxDatabase {
    fn salsa_event(&self, event: &dyn Fn() -> salsa::Event) {
        let event = event();
        eprintln!("Event: {event:?}");
    }
}

#[salsa::db]
impl SourceDatabase for ParallaxDatabase {
    fn read_source_file(&self, path: PathBuf) -> io::Result<SourceFile> {
        read_source_file(self, path)
    }
}
```

As we develop additional crates, we'll add their database trait implementations to this central database structure.

Each crate below will define its part of this integrated database structure.

The crates are ordered according to the compilation pipeline flow, with each crate building upon the outputs of earlier crates.

## 0. parallax-stdlib (New Crate)

**Purpose:** Define the standard library types, traits, functions, and intrinsics that are built into the language.

### Implementation Checklist
- [ ] Define Salsa jar for standard library components
- [ ] Define core primitive types (integers, floats, booleans, etc.)
- [ ] Create essential traits (Copy, Clone, Eq, Ord, Display, etc.)
- [ ] Implement standard functions (memory management, I/O, etc.)
- [ ] Define language intrinsics (operations that need special compiler support)
- [ ] Create serializable interface for stdlib components
- [ ] Implement target-specific optimizations for intrinsics
- [ ] Define prelude module with common imports
- [ ] Create module structure for standard library
- [ ] Add comprehensive documentation for all stdlib items
- [ ] Implement test suite for standard library functionality
- [ ] Create serialized representation for fast loading

### Inputs and Outputs
**Inputs:**
- None (self-contained)

**Outputs:**
- Standard library types, traits, and functions
- Intrinsic definitions
- Prelude components
- Serialized standard library

## 1. parallax-source (New Crate)

**Purpose:** Manage source files, file system interactions, source text handling, and diagnostic reporting.

### Implementation Checklist
- [ ] Define database trait for source file management:
  ```rust
  #[salsa::db]
  pub trait SourceDatabase: salsa::Database {
      // Source file input struct queries
      fn read_source_file(&self, path: PathBuf) -> io::Result<SourceFile>;
  }
  ```
- [ ] Define `SourceFile` as a Salsa input struct:
  ```rust
  #[salsa::input]
  pub struct SourceFile {
      #[return_ref]
      pub path: PathBuf,
      #[return_ref]
      pub contents: String,
  }
  ```
- [ ] Implement file loading, reading, and caching
  ```rust
  // Helper function implementation
  pub fn read_source_file(db: &dyn SourceDatabase, path: PathBuf) -> io::Result<SourceFile> {
      let contents = fs::read_to_string(&path)?;
      Ok(source_file(db, path, contents))
  }
  
  // Factory function for creating source files
  pub fn source_file(db: &dyn SourceDatabase, path: PathBuf, contents: String) -> SourceFile {
      SourceFile::new(db, path, contents)
  }
  ```
- [ ] Add support for path resolution and workspace management
- [ ] Create a virtual file system for testing
- [ ] Implement source file dependencies tracking
- [ ] Add `SourceText` change detection for incremental updates:
  ```rust
  // Example showing how to update a source file with durability
  fn update_source_file(db: &mut dyn SourceDatabase, path: PathBuf, new_text: String) {
      let file = db.source_file(path.clone());
      file.set_text(db)
          .with_durability(salsa::Durability::MEDIUM)
          .to(new_text);
  }
  ```
- [ ] Add support for loading standard library source files
- [ ] Implement special handling for builtin/intrinsic source locations
- [ ] Use miette's `SourceSpan` for all source locations
- [ ] Implement miette's `SourceCode` trait for source text representation
- [ ] Create utilities for mapping positions to line/column information using miette types
- [ ] Implement error handling using thiserror and miette
- [ ] Implement diagnostic accumulator system for error reporting
- [ ] Add diagnostic display, filtering, and collection utilities
- [ ] Create ParallaxError trait for rich diagnostic representation
- [ ] Support diagnostic reporting from any compiler stage

### Additional Functionality for Frame Support
- Support for loading source files from Frames
- Frame-relative path resolution
- Source file caching per Frame

### Additional Implementation for Frame Support
- [ ] Add `FrameSourceRoot` for managing Frame source files:
  ```rust
  #[salsa::tracked]
  pub struct FrameSourceRoot {
      frame_id: FrameId,
      #[return_ref]
      source_files: Vec<SourceFile>,
  }
  ```
- [ ] Implement source file loading from Frame directories
- [ ] Add source file change detection per Frame

### Inputs and Outputs
**Inputs:**
- File paths
- Source text content
- Frame directories (when using Frames)

**Outputs:**
- `SourceFile` input struct (with path, text content, metadata)
- `SourceRoot` for managing collections of source files
- `FrameSourceRoot` for Frame-specific source files
- `ParallaxDiagnostic` accumulator for error reporting
- `Report` struct for rendering diagnostics with source context

## 2. parallax-utils (New Crate)

**Purpose:** Provide common utilities and data structures used across the compiler.

### Implementation Checklist
- [ ] Define common data structures (maps, sets, etc. with custom hashing)
- [ ] Create utility functions for common operations
- [ ] Implement string interning and caching
- [ ] Provide timing and profiling tools
- [ ] Create test utilities and helpers

### Inputs and Outputs
**Inputs:**
- N/A (utility crate)

**Outputs:**
- Utility functions and data structures
- String interning
- Profiling tools

## 3. parallax-syntax (Renamed from parallax-lang)

**Purpose:** Parse source code into an abstract syntax tree (AST) representation.

### Implementation Checklist
- [ ] Define database trait for syntax parsing:
  ```rust
  #[salsa::db]
  pub trait SyntaxDatabase: SourceDatabase + salsa::Database {
      /// Parses a file into an Abstract Syntax Tree (AST).
      fn parse_file(&self, file: SourceFile) -> Arc<Ast>;
      
      /// Returns the set of parse errors for a file.
      fn parse_errors(&self, file: SourceFile) -> Vec<ParseError>;
      
      /// Returns the module structure discovered during parsing.
      fn file_module_info(&self, file: SourceFile) -> Arc<ModuleInfo>;
  }
  ```
- [ ] Define `Ast` as a tracked structure:
  ```rust
  #[salsa::input]
  pub struct Ast {
      source_file: SourceFile,
      #[return_ref]
      root: AstNode,
      file_id: u32,
  }
  ```
- [ ] Implement the parsing function:
  ```rust
  pub fn parse_file(db: &dyn SyntaxDatabase, file: SourceFile) -> Arc<Ast> {
      let contents = file.contents(db);
      let (root, errors) = parse_text(&contents);
      
      // Store errors using accumulator pattern
      for error in errors {
          parallax_diagnostics::Diagnostics::push(db, error);
      }
      
      Arc::new(Ast::new(db, file, root, file.path(db).as_os_str().to_string_lossy().to_string()))
  }
  ```
- [ ] Extract parser from current implementation
- [ ] Integrate tree-sitter parser
- [ ] Implement error recovery and diagnostic reporting using accumulator pattern
- [ ] Add span information for source mapping
- [ ] Create module structure identification
- [ ] Support incremental reparsing of changed source regions
- [ ] Add parsing support for intrinsic attributes and annotations
- [ ] Implement special syntax for standard library core features
- [ ] Implement comprehensive parse error types with miette::Diagnostic
- [ ] Ensure all AST nodes preserve source location information
- [ ] Add detailed error recovery with helpful suggestions

### Inputs and Outputs
**Inputs:**
- `SourceFile` from parallax-source

**Outputs:**
- `Ast` tracked structure
- Parse errors via diagnostics accumulator
- Module structure information

## 4. parallax-resolve

**Purpose:** Resolve names, imports, and create the symbol table.

### Implementation Checklist
- [ ] Define database trait for name resolution:
  ```rust
  #[salsa::db]
  pub trait NameResolutionDatabase: SyntaxDatabase + salsa::Database {
      /// Resolves all names in a crate and builds the symbol table.
      fn resolve_crate(&self, crate_id: CrateId) -> Arc<CrateSymbols>;
      
      /// Resolves names in a single module.
      fn resolve_module(&self, module_id: ModuleId) -> Arc<ModuleSymbols>;
      
      /// Resolves an import path to its target symbol.
      fn resolve_path(&self, module_id: ModuleId, path: &str) -> Option<Symbol>;
      
      /// Gets the complete symbol table for the crate.
      fn symbol_table(&self, crate_id: CrateId) -> Arc<SymbolTable>;
  }
  ```
- [ ] Define `ResolvedAst` as an input structure:
  ```rust
  #[salsa::input]
  pub struct ResolvedAst {
      ast: Ast,
      #[return_ref]
      symbol_map: HashMap<AstId, Symbol>,
      file_id: u32,
  }
  ```
- [ ] Define `Symbol` as an input structure:
  ```rust
  #[salsa::input]
  pub struct Symbol {
      #[return_ref]
      name: String,
      kind: SymbolKind,
      defining_module: ModuleId,
      visibility: Visibility,
  }
  ```
- [ ] Extract name resolution logic from current implementation
- [ ] Implement incremental symbol table
- [ ] Create scoped symbol resolution
- [ ] Integrate with module structure
- [ ] Implement path-based name resolution
- [ ] Add visibility checking
- [ ] Build name resolution dependency graph
- [ ] Implement automatic prelude importing mechanism
- [ ] Make standard library symbols available without explicit imports
- [ ] Handle special-case resolution for built-in types and traits
- [ ] Create scope chain that includes standard library items
- [ ] Use diagnostics accumulator for name resolution errors:
  ```rust
  fn resolve_path(db: &dyn NameResolutionDatabase, module_id: ModuleId, path: &str) -> Option<Symbol> {
      // Resolution logic...
      
      if let Err(error) = result {
          parallax_diagnostics::Diagnostics::push(db, error);
          return None;
      }
      
      // Return resolved symbol if successful
      Some(symbol)
  }
  ```

### Additional Functionality for Frame Support
- Cross-Frame name resolution
- Import resolution across Frame boundaries
- Frame-prefixed symbol paths

### Additional Implementation for Frame Support
- [ ] Extend symbol table to include Frame origin
- [ ] Create query function `resolve_external_import` for cross-frame imports:
  ```rust
  #[salsa::tracked]
  fn resolve_external_import(db: &dyn NameResolutionDatabase, frame: FrameId, path: &str) -> Option<Symbol>;
  ```
- [ ] Implement visibility checking for Frame-exported items
- [ ] Add support for Frame-qualified paths
- [ ] Create Frame import resolution mechanism

### Inputs and Outputs
**Inputs:**
- `Ast` from parallax-syntax
- `Frame` interfaces when using Frames

**Outputs:**
- `ResolvedAst` tracked structure
- `SymbolTable` tracked structure
- Name resolution errors via diagnostics accumulator

## 5. parallax-types

**Purpose:** Define the type system, type representation, and trait system.

### Implementation Checklist
- [ ] Define Salsa jar for types
- [ ] Define core type structures as tracked
- [ ] Create query functions for type operations
- [ ] Extract type system from current implementation
- [ ] Implement trait system
- [ ] Create type equality and subtyping rules
- [ ] Add generic type handling
- [ ] Implement trait bounds checking
- [ ] Build type representation system
- [ ] Create type ID system for efficient equality checks
- [ ] Handle special built-in types with compiler-specific behavior
- [ ] Implement trait relationships for standard library traits
- [ ] Support intrinsic type properties (e.g., numeric traits)
- [ ] Create special type compatibility rules for standard types

### Additional Functionality for Frame Support
- Cross-Frame type compatibility
- Type equivalence across Frame boundaries
- Frame-specific trait implementations

### Additional Implementation for Frame Support
- [ ] Add Frame context to type definitions
- [ ] Create query function `resolve_external_type(db: &dyn Db, frame: Frame, path: &str) -> Type`
- [ ] Implement type compatibility checking across Frames
- [ ] Support trait implementations across Frame boundaries
- [ ] Add Frame-aware type ID system

### Inputs and Outputs
**Inputs:**
- Type definitions from resolved AST
- Frame type information when using Frames

**Outputs:**
- Type system structures
- Trait definitions and implementations
- Type relationships
- Subtyping rules

## 6. parallax-type-inference (Split from parallax-typeck)

**Purpose:** Perform type inference on resolved AST.

### Implementation Checklist
- [ ] Define Salsa jar for type inference
- [ ] Define `InferredAst` as a tracked structure
- [ ] Create query function `infer_types(db: &dyn Db, resolved_ast: ResolvedAst) -> InferredAst`
- [ ] Extract type inference from current implementation
- [ ] Implement constraint-based type inference
- [ ] Create type unification algorithm
- [ ] Implement generic type instantiation
- [ ] Support incremental type inference
- [ ] Add fine-grained inference dependencies
- [ ] Add special handling for standard library generic types
- [ ] Implement type inference for intrinsic functions
- [ ] Support built-in operator overloading via standard traits

### Inputs and Outputs
**Inputs:**
- `ResolvedAst` from parallax-name-resolution
- Type definitions from parallax-types

**Outputs:**
- `InferredAst` tracked structure
- Type inference errors
- Type constraints
- Inferred type information

## 7. parallax-trait-solver (Split from parallax-typeck)

**Purpose:** Handle trait resolution, implementation selection, and coherence checking.

### Implementation Checklist
- [ ] Define Salsa jar for trait solving
- [ ] Define `TraitSolution` as a tracked structure
- [ ] Create query function `solve_traits(db: &dyn Db, inferred_ast: InferredAst) -> TraitSolution`
- [ ] Extract trait resolution from current implementation
- [ ] Implement impl selection algorithm
- [ ] Create coherence checking system
- [ ] Support associated type resolution
- [ ] Implement trait obligation fulfillment
- [ ] Add incremental trait solving
- [ ] Add automatic trait implementations for primitive types
- [ ] Support compiler-enforced trait implementations
- [ ] Handle special trait bounds for intrinsics

### Additional Functionality for Frame Support
- Cross-Frame trait implementations
- Trait resolution across Frame boundaries

### Additional Implementation for Frame Support
- [ ] Add Frame context to trait implementations
- [ ] Create query function `resolve_external_impl(db: &dyn Db, frame: Frame, trait_ref: TraitRef) -> Option<Impl>`
- [ ] Implement coherence checking across Frames
- [ ] Support trait implementation visibility across Frame boundaries

### Inputs and Outputs
**Inputs:**
- `InferredAst` from parallax-type-inference
- Trait definitions from parallax-types
- Frame trait information when using Frames

**Outputs:**
- `TraitSolution` tracked structure
- Resolved trait implementations
- Trait resolution errors
- Associated type bindings

## 8. parallax-typeck (Refactored)

**Purpose:** Perform final type checking and validation.

### Implementation Checklist
- [ ] Define Salsa jar for type checking
- [ ] Define `TypecheckedAst` as a tracked structure
- [ ] Create query function `typecheck(db: &dyn Db, inferred_ast: InferredAst, trait_solution: TraitSolution) -> TypecheckedAst`
- [ ] Extract type checking from current implementation
- [ ] Implement pattern matching type checking
- [ ] Create final validation passes
- [ ] Support comprehensive error reporting
- [ ] Add coercion insertion
- [ ] Create fine-grained type checking dependencies
- [ ] Add special type checking for intrinsics
- [ ] Implement special checking for standard library invariants
- [ ] Support compiler-enforced standard library guarantees

### Inputs and Outputs
**Inputs:**
- `InferredAst` from parallax-type-inference
- `TraitSolution` from parallax-trait-solver

**Outputs:**
- `TypecheckedAst` tracked structure
- Type errors and diagnostics
- Final type information

## 9. parallax-hir (Renamed from parallax-ir)

**Purpose:** Define and generate the High-level Intermediate Representation from typechecked AST.

### Implementation Checklist
- [ ] Define Salsa jar for HIR
- [ ] Define core HIR structures as tracked
- [ ] Define `Hir` as a tracked structure
- [ ] Create query function `lower_to_hir(db: &dyn Db, typechecked_ast: TypecheckedAst) -> Hir`
- [ ] Extract HIR data structures from current implementation
- [ ] Implement HIR serialization for efficient storage
- [ ] Create HIR visitor pattern
- [ ] Support type annotations in HIR
- [ ] Implement stable HIR IDs for incremental compilation
- [ ] Extract HIR lowering from current implementation
- [ ] Implement language feature desugaring
- [ ] Create per-function lowering for parallelization
- [ ] Add incremental HIR lowering
- [ ] Support type preservation during lowering
- [ ] Implement special lowering rules for standard library constructs
- [ ] Handle desugaring of syntax that maps to stdlib functions
- [ ] Support special handling of intrinsic declarations

### Inputs and Outputs
**Inputs:**
- `TypecheckedAst` from parallax-typeck

**Outputs:**
- HIR data structures
- Serialization/deserialization functions
- Visitor traits
- `Hir` tracked structure
- Lowering errors

## 10. parallax-hir-opt

**Purpose:** Perform optimization and analysis passes on HIR.

### Implementation Checklist
- [ ] Define Salsa jar for HIR optimization
- [ ] Define `OptimizedHir` as a tracked structure
- [ ] Create query function `optimize_hir(db: &dyn Db, hir: Hir) -> OptimizedHir`
- [ ] Implement HIR validation
- [ ] Create HIR optimization passes
- [ ] Support constant evaluation
- [ ] Implement function inlining at HIR level
- [ ] Add incremental optimization

### Inputs and Outputs
**Inputs:**
- `Hir` from parallax-hir

**Outputs:**
- `OptimizedHir` tracked structure
- Validation errors
- Optimization statistics

## 11. parallax-mono

**Purpose:** Perform monomorphization of generic functions.

### Implementation Checklist
- [ ] Define Salsa jar for monomorphization
- [ ] Define `MonomorphizedHir` as a tracked structure
- [ ] Create query function `monomorphize(db: &dyn Db, hir: OptimizedHir) -> MonomorphizedHir`
- [ ] Extract monomorphization from current implementation
- [ ] Implement type instantiation
- [ ] Create instance collection and caching
- [ ] Support function specialization
- [ ] Add incremental monomorphization

### Inputs and Outputs
**Inputs:**
- `OptimizedHir` from parallax-hir-opt

**Outputs:**
- `MonomorphizedHir` tracked structure
- Monomorphized functions
- Monomorphization errors

## 12. parallax-mir

**Purpose:** Define and generate MIR schema from monomorphized HIR.

### Implementation Checklist
- [ ] Define Salsa jar for MIR
- [ ] Define core MIR structures as tracked
- [ ] Define `Mir` as a tracked structure
- [ ] Create query functions for MIR operations
- [ ] Create query function `generate_mir(db: &dyn Db, mono_hir: MonomorphizedHir) -> Mir`
- [ ] Extract MIR data structures from current implementation
- [ ] Implement MIR serialization for efficient storage
- [ ] Create MIR visitor pattern
- [ ] Implement stable MIR IDs for incremental compilation
- [ ] Extract MIR generation from current implementation
- [ ] Implement control flow graph generation
- [ ] Create per-function generation for parallelization
- [ ] Add incremental MIR generation
- [ ] Implement intrinsic function calls as special MIR operations
- [ ] Support standard library special case operations
- [ ] Create MIR representations for core runtime operations

### Inputs and Outputs
**Inputs:**
- `MonomorphizedHir` from parallax-mono

**Outputs:**
- MIR data structures
- Serialization/deserialization functions
- Visitor traits
- `Mir` tracked structure
- Generation errors
- Control flow graphs

## 13. parallax-mir-opt

**Purpose:** Perform MIR-level optimizations.

### Implementation Checklist
- [ ] Define Salsa jar for MIR optimization
- [ ] Define `OptimizedMir` as a tracked structure
- [ ] Create query function `optimize_mir(db: &dyn Db, mir: Mir) -> OptimizedMir`
- [ ] Extract optimization passes from current implementation
- [ ] Implement pass manager for configurable optimizations
- [ ] Create analysis framework for MIR
- [ ] Support inlining across function boundaries
- [ ] Add incremental optimization
- [ ] Implement target-specific optimizations
- [ ] Create optimization passes for standard library function calls
- [ ] Add special optimizations for intrinsics
- [ ] Implement constant evaluation for standard library pure functions

### Inputs and Outputs
**Inputs:**
- `Mir` from parallax-mir

**Outputs:**
- `OptimizedMir` tracked structure
- Optimization statistics
- Optimization errors

## 14. parallax-effects (New Crate)

**Purpose:** Track and validate side effects in the Parallax language.

### Implementation Checklist
- [ ] Define Salsa jar for effect tracking
- [ ] Define `EffectAnnotatedMir` as a tracked structure
- [ ] Create query function `track_effects(db: &dyn Db, optimized_mir: OptimizedMir) -> EffectAnnotatedMir`
- [ ] Implement effect inference algorithm
- [ ] Create effect sequencing validation
- [ ] Support arrow operator for explicit sequencing
- [ ] Add automatic resource cleanup tracking
- [ ] Implement affine type checking for resources
- [ ] Create parallel safety validation

### Inputs and Outputs
**Inputs:**
- `OptimizedMir` from parallax-mir-opt

**Outputs:**
- `EffectAnnotatedMir` tracked structure
- Effect annotations
- Effect validation errors
- Resource usage tracking

## 15. parallax-inet (Expanded from part of parallax-codegen)

**Purpose:** Generate and optimize interaction networks from MIR for parallel execution.


### Implementation Checklist
- [ ] Define Salsa jar for interaction networks
- [ ] Define `InteractionNetwork` as a tracked structure
- [ ] Create query function `generate_inet(db: &dyn Db, effect_mir: EffectAnnotatedMir) -> InteractionNetwork`
- [ ] Implement node and port representation
- [ ] Create MIR to interaction network transformation
- [ ] Support automatic parallelism extraction
- [ ] Implement interaction network optimization
- [ ] Add network partitioning for efficient execution
- [ ] Create validation for correctness
- [ ] Implement runtime support for interaction networks
- [ ] Create execution model for interaction networks

### Inputs and Outputs
**Inputs:**
- `EffectAnnotatedMir` from parallax-effects

**Outputs:**
- `InteractionNetwork` tracked structure
- Network statistics
- Parallelism information
- Transformation errors
- Runtime integration code
- Interaction network bytecode

## 16. parallax-llvm (Split from parallax-codegen)

**Purpose:** Generate LLVM IR and native code from MIR for sequential execution.



### Implementation Checklist
- [ ] Define Salsa jar for LLVM code generation
- [ ] Define `LlvmModule` as a tracked structure
- [ ] Create query function `generate_llvm(db: &dyn Db, effect_mir: EffectAnnotatedMir, target: Target) -> LlvmModule`
- [ ] Implement LLVM context and module management
- [ ] Create type translation for Parallax to LLVM types
- [ ] Support function generation
- [ ] Implement expression codegen
- [ ] Create target-specific optimizations
- [ ] Support runtime library integration
- [ ] Add debug information generation
- [ ] Implement interface with interaction network code
- [ ] Map intrinsics to optimized LLVM instructions
- [ ] Implement target-specific versions of standard library functions
- [ ] Add special handling for core runtime operations

### Inputs and Outputs
**Inputs:**
- `EffectAnnotatedMir` from parallax-effects
- Target configuration

**Outputs:**
- LLVM IR for sequential code
- Optimization metadata
- Generated object code
- Code generation errors

## 17. parallax-codegen (Coordinator for code generation)

**Purpose:** Coordinate code generation between LLVM and interaction networks, producing the final executable.

### Implementation Checklist
- [ ] Define Salsa jar for code generation coordination
- [ ] Define `GeneratedExecutable` as a tracked structure
- [ ] Create query function `generate_executable(db: &dyn Db, llvm_module: LlvmModule, inet: InteractionNetwork, target: Target) -> GeneratedExecutable`
- [ ] Implement integration between LLVM and interaction network code
- [ ] Create runtime initialization code
- [ ] Support mixed execution model bootstrapping
- [ ] Add platform-specific linking
- [ ] Implement library dependency resolution
- [ ] Create debug information coordination
- [ ] Support both executable and library output formats
- [ ] Include standard library runtime components in the executable
- [ ] Link with target-specific standard library implementations
- [ ] Handle platform-dependent initialization of standard library

### Inputs and Outputs
**Inputs:**
- `LlvmModule` from parallax-llvm
- `InteractionNetwork` from parallax-inet
- Target configuration

**Outputs:**
- Runtime initialization code
- Linked executable or library
- Build artifacts
- Generation errors

## Error Reporting Infrastructure

Error reporting is integrated directly into the parallax-source crate, using miette and thiserror for comprehensive, user-friendly error reporting.

### Core Error Handling Components in parallax-source

The following components are provided in the parallax-source crate:

1. **ParallaxDiagnostic Accumulator**: A Salsa accumulator for collecting diagnostics
   ```rust
   #[salsa::accumulator]
   pub struct ParallaxDiagnostic {
       pub location: String,
       pub report: Arc<dyn ParallaxError>,
   }
   ```

2. **ParallaxError Trait**: A trait for errors that can be reported with source context
   ```rust
   pub trait ParallaxError: Diagnostic + Send + Sync + RefUnwindSafe + Display + Error + 'static {
       fn report(self, db: &dyn SourceDatabase, file: SourceFile) -> Report;
   }
   ```

3. **Report Struct**: For rendering diagnostics with source code context
   ```rust
   pub struct Report {
       pub source_code: String,
       pub report: Arc<dyn ParallaxError>,
   }
   ```

4. **Helper Functions**: Utilities to report errors via the accumulator
   ```rust
   pub fn report_error<R: ParallaxError>(
       db: &dyn SourceDatabase, 
       location: String, 
       report: R
   ) {
       ParallaxDiagnostic {
           location,
           report: Arc::new(report),
       }.accumulate(db);
   }
   ```

### Core Requirements for All Crates

Every crate in the compiler pipeline must implement the following:

1. **Error Types with thiserror**
   - [ ] Define detailed error enums with thiserror
   - [ ] Ensure all error variants include helpful messages
   - [ ] Add error codes for documentation references

2. **Diagnostic Integration with miette**
   - [ ] Implement miette::Diagnostic for all error types
   - [ ] Use miette's `SourceSpan` for all source code locations
   - [ ] Include help messages and suggestions where appropriate

3. **Source Span Preservation**
   - [ ] Use miette's `SourceSpan` types throughout the compiler pipeline
   - [ ] Never define custom span types that duplicate miette functionality
   - [ ] Ensure source locations are preserved through transformations
   - [ ] Never discard span information when converting between representations
   - [ ] Include spans in all data structures that represent source elements

4. **Error Propagation**
   - [ ] Report errors via the ParallaxDiagnostic accumulator
   - [ ] Include source context when reporting errors
   - [ ] Support reporting multiple errors before giving up
   - [ ] Prioritize errors by severity and relevance

### Source Location Types

**IMPORTANT**: To maintain consistency, use miette's types rather than custom implementations:

```rust
use miette::{SourceSpan, SourceCode};

// AST node with source location
pub struct AstNode {
    pub kind: NodeKind,
    pub span: SourceSpan, // Use miette's SourceSpan directly
}

// Source file implementing SourceCode
#[salsa::tracked]
pub struct SourceFile {
    pub path: PathBuf,
    #[return_ref]
    pub content: String,
}

// Implement miette's SourceCode for our SourceFile
impl SourceCode for SourceFile {
    fn read_span<'a>(
        &'a self, 
        span: &SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize
    ) -> Result<Box<dyn miette::SpanContents<'a> + 'a>, miette::Error> {
        // Implementation...
    }
}
```

### Error Type Examples

```rust
// In parallax-syntax/src/error.rs
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Debug, Error, Diagnostic)]
pub enum SyntaxError {
    #[error("Unexpected token '{found}', expected {expected}")]
    #[diagnostic(
        code(syntax::unexpected_token),
        help("Try adding a '{expected}' here")
    )]
    UnexpectedToken {
        #[source_code]
        src: SourceFile, // Implement SourceCode for SourceFile
        #[label("unexpected token")]
        span: SourceSpan, // Use miette's SourceSpan
        found: String,
        expected: String,
    },
    
    // Additional errors...
}
```

## Updated Compilation Pipeline Overview

The revised compilation pipeline now follows this order with refined steps:

1. **Standard Library Loading** → `parallax-stdlib`
   - Load serialized standard library definitions
   - Make prelude available to subsequent phases

2. **Source Management & Diagnostics** → `parallax-source`
   - Load source files from Frames and standard library
   - Provide diagnostic infrastructure for error reporting

3. **Parsing** → `parallax-syntax`
   - Parse source files to AST with knowledge of language constructs

4. **Name Resolution** → `parallax-name-resolution`
   - Resolve symbols across modules, Frames, and standard library
   - Import prelude automatically

5. **Type System** → `parallax-types`
   - Define type representations and trait system

6. **Type Inference** → `parallax-type-inference`
   - Infer types for expressions and variables

7. **Trait Solving** → `parallax-trait-solver`
   - Resolve trait implementations and coherence

8. **Type Checking** → `parallax-typeck`
   - Final type checking and validation

9. **High-level Intermediate Representation** → `parallax-hir`
    - Define HIR data structures
    - Lower typechecked AST to HIR

10. **HIR Optimization** → `parallax-hir-opt`
    - Optimize and validate HIR

11. **Monomorphization** → `parallax-mono`
    - Create monomorphized instances of generic functions

12. **Mid-level Intermediate Representation** → `parallax-mir`
    - Define MIR data structures
    - Generate MIR from monomorphized HIR

13. **MIR Optimization** → `parallax-mir-opt`
    - Perform MIR-level optimizations

14. **Effect Tracking** → `parallax-effects`
    - Track and validate side effects
    - Ensure proper effect sequencing
    - Handle resource management

15. **Interaction Net Generation** → `parallax-inet`
    - Transform MIR to interaction networks
    - Extract parallelism
    - Optimize networks
    - Generate interaction net bytecode

16. **LLVM Code Generation** → `parallax-llvm`
    - Generate LLVM IR from MIR
    - Perform LLVM optimizations
    - Generate native code

17. **Code Generation Coordination** → `parallax-codegen`
    - Coordinate LLVM and interaction net code
    - Link components into final executable
    - Create runtime initialization
    - Generate debuggable binaries

Each stage produces data that is consumed by subsequent stages, with Salsa tracking fine-grained dependencies for efficient incremental compilation.

## Updated Migration Strategy

1. **Phase 1: Setup Infrastructure**
   - Create parallax-stdlib crate with core language definitions
   - Create parallax-source crate with source management and diagnostic capabilities
   - Create parallax-utils crate with common utilities
   - Implement parallax-db for the central database

2. **Phase 2: Frontend**
   - Convert parallax-syntax
   - Implement parallax-name-resolution
   - Create parallax-types

3. **Phase 3: Type System**
   - Implement parallax-type-inference
   - Create parallax-trait-solver
   - Refactor parallax-typeck

4. **Phase 4: HIR and Monomorphization**
   - Implement parallax-hir
   - Add parallax-hir-opt
   - Implement parallax-mono

5. **Phase 5: MIR**
   - Implement parallax-mir
   - Implement parallax-mir-opt

6. **Phase 6: Parallax-Specific Features**
   - Implement parallax-effects for effect tracking
   - Create parallax-inet for interaction networks
   - Implement parallax-llvm for LLVM code generation
   - Create parallax-codegen for final code coordination

7. **Phase 7: Backend and Testing**
   - Implement parallax-testing
   - Update parallax-cli
   - Finalize error reporting throughout the pipeline

8. **Phase 8: Package Management**
   - Implement parallax-package-manager

## Conclusion

This restructuring plan divides the Parallax compiler into clearly defined crates, each with a single responsibility. By using Salsa for incremental compilation, the compiler will gain significant performance improvements while maintaining modularity and clean architecture. 

## Central Database Implementation (parallax-db)

The `parallax-db` crate will serve as the central integration point for all the query groups defined across the various compiler crates. This structure follows the modern Salsa 0.19.0 approach as seen in projects like rust-analyzer.

### Implementation Checklist

- [ ] Define the central database struct that integrates all storage types:
  ```rust
  #[salsa::db(
      SourceDatabaseStorage,
      SyntaxDatabaseStorage,
      NameResolutionDatabaseStorage,
      TypesDatabaseStorage,
      TypeInferenceDatabaseStorage,
      TraitSolverDatabaseStorage,
      TypeckDatabaseStorage,
      HirDatabaseStorage,
      // ... other storage types ...
      DiagnosticsDatabaseStorage
  )]
  #[derive(Default)]
  pub struct ParallaxDatabase {
      storage: salsa::Storage<Self>,
      // Optional non-Salsa fields if needed
  }
  ```

- [ ] Implement the required Salsa database traits:
  ```rust
  impl salsa::Database for ParallaxDatabase {
      fn salsa_event(&self, event: salsa::Event) {
          // Optional event handling, useful for debugging
          match event.kind() {
              salsa::EventKind::WillExecute { database_key } => {
                  tracing::trace!("Executing query: {:?}", database_key.debug(self));
              }
              _ => {}
          }
      }
  }
  
  // Support for parallel query execution
  impl salsa::ParallelDatabase for ParallaxDatabase {
      fn snapshot(&self) -> salsa::Snapshot<Self> {
          salsa::Snapshot::new(Self {
              storage: self.storage.snapshot(),
              // Clone any other non-Salsa fields
          })
      }
  }
  ```

- [ ] Create helper methods for database operations:
  ```rust
  impl ParallaxDatabase {
      // Create a new database instance
      pub fn new() -> Self {
          Self::default()
      }
      
      // Apply changes to the database
      pub fn apply_changes(&mut self, changes: Vec<CompilerChange>) {
          for change in changes {
              match change {
                  CompilerChange::UpdateSourceFile { path, text } => {
                      let file = self.source_file(path);
                      file.set_text(self).to(text);
                  }
                  // Handle other change types
              }
          }
      }
      
      // Get all diagnostics for a given file
      pub fn diagnostics_for_file(&self, file: SourceFile) -> Vec<ParallaxDiagnostic> {
          // Collect diagnostics from various phases
          let parse_diags = parse_file::accumulated::<Diagnostics>(self, file)
              .into_iter()
              .map(|d| d.0)
              .collect::<Vec<_>>();
              
          let resolve_diags = resolve_module::accumulated::<Diagnostics>(self, ModuleId::from_file(self, file))
              .into_iter()
              .map(|d| d.0)
              .collect::<Vec<_>>();
              
          // Combine and return all diagnostics
          [parse_diags, resolve_diags].concat()
      }
      
      // Check if compilation has errors
      pub fn has_errors(&self, crate_id: CrateId) -> bool {
          self.diagnostics_for_crate(crate_id)
              .iter()
              .any(|d| d.severity() == Severity::Error)
      }
  }
  ```

- [ ] Define the database trait composition:
  ```rust
  // Trait combining all database traits for convenient usage
  pub trait CompilerDatabase: 
      SourceDatabase +
      SyntaxDatabase +
      NameResolutionDatabase +
      TypesDatabase +
      TypeInferenceDatabase +
      TraitSolverDatabase +
      TypeckDatabase +
      HirDatabase +
      // ... other database traits ...
      DiagnosticsDatabase +
      salsa::Database
  {}
  
  // Implement the combined trait for the database
  impl CompilerDatabase for ParallaxDatabase {}
  ```

- [ ] Add testing utilities:
  ```rust
  #[cfg(test)]
  pub mod testing {
      use super::*;
      
      pub fn test_db() -> ParallaxDatabase {
          ParallaxDatabase::default()
      }
      
      pub fn db_with_file(text: &str, path: &str) -> (ParallaxDatabase, SourceFile) {
          let mut db = test_db();
          let path = PathBuf::from(path);
          let file = SourceFile::new(&mut db, text.to_string(), path);
          (db, file)
      }
  }
  ```

### Integration with Other Crates

The `parallax-db` crate will:

1. Have dependencies on all other crates that define query groups
2. Export a unified `CompilerDatabase` trait for convenient usage across the codebase
3. Provide the concrete `ParallaxDatabase` implementation used by the compiler
4. Include testing utilities for creating database instances in tests

### Database Usage in CLI and LSP

The database will be the central point for compiler operations:

```rust
// Example usage in CLI
fn compile_crate(path: &Path) -> Result<(), CompileError> {
    // Create and initialize database
    let mut db = ParallaxDatabase::new();
    
    // Load crate
    let crate_id = load_crate(&mut db, path)?;
    
    // Run the compilation pipeline via queries
    let _parsed = db.parse_crate(crate_id);
    let _resolved = db.resolve_crate(crate_id);
    let _typechecked = db.typecheck_crate(crate_id);
    let mir = db.generate_mir(crate_id);
    
    // Check for errors
    if db.has_errors(crate_id) {
        let diagnostics = db.diagnostics_for_crate(crate_id);
        report_diagnostics(&diagnostics);
        return Err(CompileError::CompilationFailed);
    }
    
    // Generate code if no errors
    let _executable = db.generate_executable(mir);
    
    Ok(())
}
```

This central database approach ensures all compiler components can efficiently share data while maintaining the benefits of Salsa's incremental computation. 