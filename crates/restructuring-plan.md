# Parallax Compiler Restructuring Plan

This document outlines the step-by-step plan for restructuring each crate in the Parallax compiler to use the Salsa framework for incremental compilation. Each crate has been analyzed to ensure it has a single, clear responsibility, and some crates may be split to maintain this principle.

The crates are ordered according to the compilation pipeline flow, with each crate building upon the outputs of earlier crates.

## 1. parallax-source (New Crate)

**Purpose:** Manage source files, file system interactions, and source text handling.

### Implementation Checklist
- [ ] Create Salsa jar with inputs for source files
- [ ] Define `SourceFile` as a tracked structure
- [ ] Implement file loading, reading, and caching
- [ ] Add support for path resolution and workspace management
- [ ] Create a virtual file system for testing
- [ ] Implement source file dependencies tracking
- [ ] Add `SourceText` change detection for incremental updates

### Additional Functionality for Frame Support
- Support for loading source files from Frames
- Frame-relative path resolution
- Source file caching per Frame

### Additional Implementation for Frame Support
- [ ] Add `FrameSourceRoot` for managing Frame source files
- [ ] Create query function `frame_source_files(db: &dyn Db, frame: Frame) -> FrameSourceRoot`
- [ ] Implement source file loading from Frame directories
- [ ] Add source file change detection per Frame

### Inputs and Outputs
**Inputs:**
- File paths
- Source text content
- Frame directories (when using Frames)

**Outputs:**
- `SourceFile` tracked structure (with path, text content, metadata)
- `SourceRoot` for managing collections of source files
- `FrameSourceRoot` for Frame-specific source files

## 2. parallax-frame (New Crate)

**Purpose:** Manage library (Frame) definitions, dependencies, and interfaces.
```

### Implementation Checklist
- [ ] Define Salsa jar for frame management
- [ ] Define `Frame` as a tracked structure
- [ ] Create query function `load_frame(db: &dyn Db, path: &Path) -> Frame`
- [ ] Implement frame manifest parser (frame.toml)
- [ ] Create dependency resolution and version checking
- [ ] Implement frame loading and caching
- [ ] Design frame interface exposure mechanism
- [ ] Create dependency graph builder for proper compilation order
- [ ] Add interface file generation and parsing

### Inputs and Outputs
**Inputs:**
- Frame manifest files
- Dependency specifications
- Frame paths

**Outputs:**
- `Frame` tracked structure
- `FrameInterface` for exposed APIs
- `FrameGraph` for dependency relationships
- Frame loading errors

## 3. parallax-syntax (Renamed from parallax-lang)

**Purpose:** Parse source code into an abstract syntax tree (AST) representation.

### Implementation Checklist
- [ ] Define Salsa jar for syntax parsing
- [ ] Define `Ast` as a tracked structure
- [ ] Create query function `parse_file(db: &dyn Db, file: SourceFile) -> Ast`
- [ ] Extract parser from current implementation
- [ ] Integrate tree-sitter parser
- [ ] Implement error recovery and diagnostic reporting
- [ ] Add span information for source mapping
- [ ] Create module structure identification
- [ ] Support incremental reparsing of changed source regions

### Inputs and Outputs
**Inputs:**
- `SourceFile` from parallax-source

**Outputs:**
- `Ast` tracked structure
- Parse errors
- Module structure information

## 4. parallax-name-resolution (Split from parallax-resolve)

**Purpose:** Resolve names, imports, and create the symbol table.

### Implementation Checklist
- [ ] Define Salsa jar for name resolution
- [ ] Define `ResolvedAst` as a tracked structure
- [ ] Create query function `resolve_names(db: &dyn Db, ast: Ast) -> ResolvedAst`
- [ ] Extract name resolution logic from current implementation
- [ ] Implement incremental symbol table
- [ ] Create scoped symbol resolution
- [ ] Integrate with module structure
- [ ] Implement path-based name resolution
- [ ] Add visibility checking
- [ ] Build name resolution dependency graph

### Additional Functionality for Frame Support
- Cross-Frame name resolution
- Import resolution across Frame boundaries
- Frame-prefixed symbol paths

### Additional Implementation for Frame Support
- [ ] Extend symbol table to include Frame origin
- [ ] Create query function `resolve_external_import(db: &dyn Db, frame: Frame, path: &str) -> Symbol`
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
- Name resolution errors

## 5. parallax-types (Renamed from parallax-type-system)

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

## Updated Compilation Pipeline Overview

The revised compilation pipeline now follows this order with refined steps:

1. **Frame Resolution** → `parallax-frame`
   - Parse Frame manifests and resolve dependencies

2. **Source Loading** → `parallax-source`
   - Load source files from Frames

3. **Parsing** → `parallax-syntax`
   - Parse source files to AST

4. **Name Resolution** → `parallax-name-resolution`
   - Resolve symbols across modules and Frames

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
   - Create parallax-source and parallax-db crates
   - Implement parallax-frame for Frame management

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

8. **Phase 8: Package Management**
   - Implement parallax-package-manager

## Conclusion

This restructuring plan divides the Parallax compiler into clearly defined crates, each with a single responsibility. By using Salsa for incremental compilation, the compiler will gain significant performance improvements while maintaining modularity and clean architecture. 

## Modified Build Pipeline for Frame Support

With Frame support added, the build pipeline now includes these additional steps:

1. **Frame Resolution**: Resolve all dependencies of the main Frame
   - Parse the main Frame's manifest
   - Recursively resolve all dependencies
   - Build a dependency graph
   - Check for version conflicts

2. **Frame Loading**: Load all required Frames into the compilation context
   - Load each Frame's source files
   - Parse Frame interfaces
   - Make external symbols available for import

3. **Cross-Frame Resolution**: During name resolution and type checking
   - Resolve imports across Frame boundaries
   - Check visibility of imported symbols
   - Verify type compatibility between Frames

4. **Compound Compilation**: When building a project with dependencies
   - Determine which Frames need compilation
   - Compile Frames in dependency order
   - Link Frame artifacts together

5. **Interface Generation**: When building a Frame as a library
   - Extract public API from the compilation
   - Generate interface files for consumers
   - Create metadata for efficient importing

## Frame Manifest Format

Each Frame will be defined by a `frame.toml` file with a structure similar to:

```toml
[frame]
name = "example_frame"
version = "0.1.0"
authors = ["Your Name <your.email@example.com>"]
description = "An example Parallax Frame"

[dependencies]
math = "1.0.0"
collections = { version = "0.5.0", path = "../collections" }
graphics = { git = "https://github.com/example/graphics-frame.git", tag = "v2.0.0" }
```

## Frame Interface Files

To optimize compilation of Frame dependencies, each compiled Frame will generate an interface file containing:

1. **Exported Symbols**: Public items available for import
2. **Type Definitions**: Public type information
3. **Trait Implementations**: Available implementations
4. **Metadata**: Compilation information for incremental builds

These interface files allow dependent Frames to compile without accessing the source code of dependencies. 