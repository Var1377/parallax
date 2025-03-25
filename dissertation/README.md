# The Parallel Reduction of Interaction Networks

## Dissertation Structure (Cambridge CS Guidelines - Detailed Breakdown)

This dissertation strictly adheres to the structure mandated by the Cambridge Computer Science Part II Project guidelines, ensuring all required elements are addressed in their designated sections.

- **Chapter 1: Introduction**
  - **1.1 Principal Motivation:**
    - 1.1.1 The Growing Challenge of Parallel Programming Complexity.
    - 1.1.2 The Need for Effective Automatic Parallelization.
    - 1.1.3 Introducing Parallax: An Overview of its Aims.
  - **1.2 Context within Computer Science:**
    - 1.2.1 Relation to Multi-Core Architecture Trends.
    - 1.2.2 Relevance to Programming Language Design Principles (Implicit Parallelism).
    - 1.2.3 Connection to Compiler Construction Techniques for Parallel Targets.
  - **1.3 Brief Survey of Related Work:**
    - 1.3.1 Overview of Manual and Semi-Automatic Parallelism Techniques.
    - 1.3.2 Functional Programming Approaches (e.g., Haskell, Futhark) and their Trade-offs.
    - 1.3.3 Prior Interaction Network Implementations (e.g., Bend) and Identified Gaps (Memory Efficiency).
    - *Note: Bibliographic references are used; lengthy quotations are confined to appendices as per guidelines.*
  - **1.4 Dissertation Outline:**
    - A brief guide to the structure and content of the subsequent chapters.

- **Chapter 2: Preparation**
  *(This chapter details the foundational work preceding implementation, emphasizing a professional approach and aligning with the initial project proposal.)*
  - **2.1 Refinement of the Project Proposal:**
    - 2.1.1 Initial Goal: Compiler and Parallel Runtime for Interaction Networks.
    - 2.1.2 Clarification: Focus on Memory Performance as Key Evaluation Metric.
    - 2.1.3 Refinement: Defining the Scope of the `Parallax` Language (Minimalist Functional Language).
    - 2.1.4 Approach Decision: Development from scratch in Rust to control memory layout and interaction system.

  - **2.2 Requirements Analysis:** *(Mandatory Section)*
    - **2.2.1 Functional Requirements:**
      - 2.2.1.1 Compiler: Transform `Parallax` source to an Interaction Network representation.
      - 2.2.1.2 Runtime: Reduce the generated Interaction Network in parallel across multiple CPU cores.
      - 2.2.1.3 `Parallax` Language Features (Minimum): Integer support (values, arithmetic, comparison), recursion, data aggregation (tuples/structs/ADTs), pattern matching.
    - **2.2.2 Performance Requirements:**
      - 2.2.2.1 Primary Goal: Demonstrate improved memory performance (footprint, churn) compared to existing systems (e.g., Bend).
      - 2.2.2.2 Secondary Goal: Achieve competitive parallel scaling, although absolute speed not the primary focus.
    - **2.2.3 Non-Functional Requirements:**
      - 2.2.3.1 Safety: Leverage Rust for memory and concurrency safety.
      - 2.2.3.2 Tooling: Provide basic means to compile and run `Parallax` code.
      - 2.2.3.3 Benchmarkability: Enable evaluation through profiling or instrumented allocation.

  - **2.3 Design Decisions:**
    - **2.3.1 `Parallax` Language (PLX) Design:**
      - 2.3.1.1 Syntax: Minimalist functional syntax (details TBD at this stage).
      - 2.3.1.2 Semantics: Based on L2 (Part 1B Semantics), focusing on core functional features.
      - 2.3.1.3 Type System: Initially optional, basic type checking considered an extension (as per proposal success criteria).
      - 2.3.1.4 Data Structures: Defining tuple/struct/ADT representation for memory evaluation.
    - **2.3.2 Interaction Network System:**
      - 2.3.2.1 Base Choice: Initial consideration of Mazza's symmetric combinators {ε, δ, ζ} \cite{mazza}.
      - 2.3.2.2 Extension Strategy: Planning for extended combinators (e.g., number agents, operators) to improve efficiency and reduce network size (as described in proposal).
      - 2.3.2.3 Reduction Strategy: Design considerations for parallel reduction based on literature review.
    - **2.3.3 High-Level System Architecture:**
      - 2.3.3.1 Compiler Pipeline: Defining initial stages (Parsing -> IN Generation).
      - 2.3.3.2 Runtime Components: Core elements (Network Representation, Scheduler, Reducer, Memory Manager).
      - 2.3.3.3 Implementation Language: Rust (chosen for safety, performance, memory control).

  - **2.4 Theoretical Foundations and Learning:**
    - **2.4.1 Literature Review:** *(Addresses proposal item 1.a)*
      - 2.4.1.1 Study of practical Interaction Network reduction techniques.
      - 2.4.1.2 Investigation of Lafont's original work \cite{lafont90} and subsequent developments.
      - 2.4.1.3 Understanding Symmetric Interaction Combinators \cite{mazza}.
    - **2.4.2 Research on Existing Runtimes:** *(Addresses proposal item 1.b)*
      - 2.4.2.1 Analysis of HigherOrderCO/Bend implementation \cite{bend}.
      - 2.4.2.2 Investigation of other graph reduction systems and their memory management strategies.
    - **2.4.3 Language and System Learning:**
      - 2.4.3.1 Leveraging Part 1B Semantics and Compilers knowledge.
      - 2.4.3.2 Deepening Rust expertise, particularly in concurrency and low-level memory APIs.
      - 2.4.3.3 Initial investigation into potential LLVM usage for backend generation (although not core requirement).

  - **2.5 Software Engineering Methodology:**
    - 2.5.1 Version Control: Git usage with university GitLab repository.
    - 2.5.2 Benchmarking Framework Design: *(Addresses proposal item 1.d)*
      - 2.5.2.1 Selection of benchmark algorithms (initially sorting algorithms).
      - 2.5.2.2 Defining memory profiling methodology (instrumentation vs. external profilers).
      - 2.5.2.3 Establishing baseline measurements using existing implementations (e.g., Bend).
    - 2.5.3 Development Environment: Setup on primary and backup machines (as per resource declaration).
    - 2.5.4 Contingency Planning: Strategy for hardware/software failure (local backups, version control).

  - **2.6 Starting Point Declaration:** *(Mandatory Section, based on proposal)*
    - 2.6.1 Prior Work: Research completed on implicit parallelism and Interaction Networks.
    - 2.6.2 Codebase Status: Project not started at proposal time; development commenced post-proposal.
    - 2.6.3 Primary Comparison: HigherOrderCO/Bend \cite{bend} identified as the main inspiration and comparison point.
    - 2.6.4 Justification for New Implementation: Desire to innovate on core memory layout and interaction system, preventing direct use of existing runtimes.
    - 2.6.5 Relevant Experience: Limited practical IN/compiler experience, reasonable Rust concurrency experience.
    - 2.6.6 Acknowledged Learning Curve: Dependence on Part 1B/Part II course material noted.

- **Chapter 3: Implementation**
  *(This chapter describes the tangible artifacts produced and the process, highlighting professional practices.)*
  - **3.1 Overview of Implemented System Components:**
    - 3.1.1 The Parallax Compiler Pipeline.
    - 3.1.2 The `parallax-net` Runtime.
    - 3.1.3 The `parallax-cli` Tooling.
  
  - **3.2 Compiler Frontend Implementation (`parallax-lang`):**
    - **3.2.1 Abstract Syntax Tree (AST) Design:**
      - 3.2.1.1 Expression Hierarchy (`expr.rs`): Literals, operations, closures, and control flow.
      - 3.2.1.2 Statement Representation (`items.rs`): Function and type definitions, modules.
      - 3.2.1.3 Pattern Matching System (`pattern.rs`): Destructuring and binding patterns.
      - 3.2.1.4 Type System Representation (`types.rs`): Primitive types, ADTs, traits, generics.
      - 3.2.1.5 Source Location Tracking (`location.rs`): Tracking for error reporting.
    
    - **3.2.2 Parser Implementation:**
      - 3.2.2.1 Recursive Descent Parsing Strategy.
      - 3.2.2.2 Lexical Analysis (`scanner.rs`): Token recognition and categorization.
      - 3.2.2.3 Expression Parsing (`expr.rs`): Operator precedence, function calls.
      - 3.2.2.4 Item-Level Parsing (`items.rs`): Modules, functions, structs, traits.
      - 3.2.2.5 Pattern Parsing (`pattern.rs`): Destructuring in various contexts.
      - 3.2.2.6 Type Expression Parsing (`types.rs`): Type annotations and generic parameters.
      - 3.2.2.7 Literal Parsing (`literals.rs`): Numbers, strings, and special values.
      - 3.2.2.8 Validation Mechanisms (`validate.rs`): Early semantic verification.

    - **3.2.3 Error Handling and Diagnostics:**
      - 3.2.3.1 Error Representation (`error.rs`): Error types and categorization.
      - 3.2.3.2 Error Recovery Strategies: Continuing after syntax errors.
      - 3.2.3.3 Diagnostic Message Generation: User-friendly error messages.
  
  - **3.3 Name Resolution and Binding (`parallax-resolve`):**
    - **3.3.1 Symbol Table Implementation (`symbol.rs`):**
      - 3.3.1.1 Symbol Representation and Management.
      - 3.3.1.2 Symbol Resolution Mechanisms.
      - 3.3.1.3 Handling of Forward References.
    
    - **3.3.2 Namespace Management (`namespace.rs`):**
      - 3.3.2.1 Global Namespace Organization.
      - 3.3.2.2 Local Scope Handling.
      - 3.3.2.3 Resolution of Ambiguities.
    
    - **3.3.3 Import Resolution (`imports.rs`):**
      - 3.3.3.1 Module Import Processing.
      - 3.3.3.2 Handling of Path-Based References.
      - 3.3.3.3 Visibility and Privacy Enforcement.
    
    - **3.3.4 Type Resolution (`types.rs`):**
      - 3.3.4.1 Type Name Resolution.
      - 3.3.4.2 Generic Parameter Binding.
      - 3.3.4.3 Trait Reference Resolution.
  
  - **3.4 Type Checking and Inference (`parallax-typeck`):**
    - **3.4.1 Type Checking Context (`context.rs`):**
      - 3.4.1.1 Environment Representation.
      - 3.4.1.2 Type Environment Management.
      - 3.4.1.3 Integration with Resolver Output.
    
    - **3.4.2 Type Inference Engine (`infer.rs`):**
      - 3.4.2.1 Inference Algorithm Implementation.
      - 3.4.2.2 Constraint Generation.
      - 3.4.2.3 Type Variable Management.
    
    - **3.4.3 Type Unification (`unify.rs`):**
      - 3.4.3.1 Unification Algorithm.
      - 3.4.3.2 Handling of Generic Types.
      - 3.4.3.3 Subtyping Relationships.
    
    - **3.4.4 Trait System (`traits/`):**
      - 3.4.4.1 Trait Implementation Verification.
      - 3.4.4.2 Trait Resolution for Generic Contexts.
      - 3.4.4.3 Default Implementation Handling.
    
    - **3.4.5 Diagnostic Generation (`diagnostics.rs`, `error.rs`):**
      - 3.4.5.1 Type Error Classification.
      - 3.4.5.2 Error Message Generation.
      - 3.4.5.3 Suggestions for Type Errors.
  
  - **3.5 High-Level IR Generation (`parallax-hir`):**
    - **3.5.1 HIR Structure (`hir.rs`):**
      - 3.5.1.1 Type-Annotated Expression Representation.
      - 3.5.1.2 Simplified Control Flow.
      - 3.5.1.3 Explicit Representation of Implicit Operations.
    
    - **3.5.2 AST to HIR Lowering (`lower.rs`):**
      - 3.5.2.1 Transformation Process.
      - 3.5.2.2 Type Information Integration.
      - 3.5.2.3 Desugaring Complex Expressions.
    
    - **3.5.3 HIR Database Interface (`db.rs`):**
      - 3.5.3.1 Incremental Computation Model.
      - 3.5.3.2 Query-based Architecture.
      - 3.5.3.3 Caching Mechanisms.
  
  - **3.6 Mid-Level IR and Optimization (`parallax-mir`):**
    - **3.6.1 MIR Structure and Core Components:**
      - 3.6.1.1 Basic Block Representation: Control flow structuring using basic blocks with terminators.
      - 3.6.1.2 Function Representation (`function.rs`): Function bodies, signature handling, and scope management.
      - 3.6.1.3 Statement Design (`statement.rs`): Representing assignments, operations, and declarations.
      - 3.6.1.4 Terminator Logic (`terminator.rs`): Modeling control flow transitions (branching, returns, calls).
      - 3.6.1.5 Memory Places (`place.rs`): Abstraction of memory locations (local variables, fields, etc.).
      - 3.6.1.6 MIR Visitor Pattern (`visit.rs`): Generic traversal and transformation framework for MIR components.

    - **3.6.2 HIR to MIR Lowering (`lower/`):**
      - 3.6.2.1 Expression Flattening: Converting nested expressions to sequences of simpler statements.
      - 3.6.2.2 Control Flow Structuring: Transformation of high-level control structures into basic blocks.
      - 3.6.2.3 SSA Form Generation: Ensuring each variable is assigned exactly once.
      - 3.6.2.4 Pattern Matching Lowering: Converting pattern matches to explicit conditionals and bindings.
      - 3.6.2.5 Iterator and Closure Translation: Implementation of functional constructs.

    - **3.6.3 Monomorphization (`mono/`):**
      - 3.6.3.1 Generic Function Specialization: Creating concrete instances of generic functions.
      - 3.6.3.2 Trait Implementation Resolution: Selecting specific trait implementations.
      - 3.6.3.3 Collection of Specialized Functions: Gathering all required function variants.
      - 3.6.3.4 Type Layout Computation: Determining memory layout for concrete types.

    - **3.6.4 Optimization Passes (`opt/`):**
      - 3.6.4.1 Dead Code Elimination: Removing unreachable or unused code.
      - 3.6.4.2 Constant Folding and Propagation: Evaluating compile-time constants and propagating values.
      - 3.6.4.3 Function Inlining: Replacing function calls with function bodies for performance.
      - 3.6.4.4 Common Subexpression Elimination: Avoiding redundant computations.
      - 3.6.4.5 Loop Optimization: Improving loop performance (unrolling, invariant code motion).
      - 3.6.4.6 Automatic Parallelization Analysis: Identifying and annotating parallel execution opportunities.

    - **3.6.5 Utility Infrastructure (`util/`):**
      - 3.6.5.1 MIR Manipulation Helpers: Common transformations and queries.
      - 3.6.5.2 Type Utilities: Type handling and manipulation functions.
      - 3.6.5.3 Validation Logic: MIR correctness verification.
      - 3.6.5.4 Debugging Aids: MIR visualization and inspection tools.

    - **3.6.6 Incremental Computation (`db.rs`):**
      - 3.6.6.1 Query System Integration: Salsa-based incremental computation.
      - 3.6.6.2 MIR Caching Strategy: Efficient storage and retrieval of MIR artifacts.
      - 3.6.6.3 Dependency Tracking: Managing relationships between MIR components.

  - **3.7 Code Generation (`parallax-codegen`):**
    - **3.7.1 Interaction Network Generation (`inet.rs`):**
      - 3.7.1.1 MIR to Interaction Network Translation: Mapping MIR constructs to interaction agents.
      - 3.7.1.2 Agent Creation and Typing: Generating typed interaction net agents from MIR entities.
      - 3.7.1.3 Connection Management: Encoding data flow as agent connections.
      - 3.7.1.4 Control Flow Translation: Mapping basic blocks and terminators to interaction patterns.
      - 3.7.1.5 Function Boundary Interfaces: Entry and exit point conventions for function calls.
      - 3.7.1.6 Memory Operations: Translation of load/store operations to interaction primitives.

    - **3.7.2 LLVM IR Generation (`llvm.rs`):**
      - 3.7.2.1 LLVM Context Management: Setting up and configuring the LLVM compilation environment.
      - 3.7.2.2 Type System Mapping: Converting Parallax types to LLVM representations.
      - 3.7.2.3 Function Generation: Translating MIR functions to LLVM functions.
      - 3.7.2.4 Instruction Selection: Choosing appropriate LLVM instructions for MIR operations.
      - 3.7.2.5 Runtime Integration: Interface points between compiled code and the Parallax runtime.
      - 3.7.2.6 Native Code Generation: Final compilation to target machine code.
      - 3.7.2.7 Debug Information: Source-level debugging metadata generation.
      - 3.7.2.8 Leveraging Existing Optimizations: Utilizing LLVM's optimization passes for generated native code.

    - **3.7.3 Integration and Orchestration (`lib.rs`):**
      - 3.7.3.1 Compilation Strategy Selection: Choosing between Interaction Net and direct LLVM paths (potentially hybrid approaches).
      - 3.7.3.2 Parallel Compilation: Managing compilation of multiple translation units.
      - 3.7.3.3 Code Generation Phases: Sequencing the generation process.
      - 3.7.3.4 Error Handling and Reporting: Managing and reporting code generation errors.

    - **3.7.4 Optimization at Code Generation:**
      - 3.7.4.1 Network Structure Optimization: Simplifying interaction networks before execution.
      - 3.7.4.2 LLVM Pass Configuration: Setting up and sequencing LLVM optimization passes.
      - 3.7.4.3 Target-Specific Optimizations: Architecture-specific code improvements.
      - 3.7.4.4 Inlining and Specialization Decisions: Final inlining choices at codegen time.
      - 3.7.4.5 Vectorization: Utilizing SIMD instructions for data-parallel operations.

  - **3.8 Runtime Implementation (`parallax-net`):**
    - **3.8.1 Core Runtime Components:**
      - 3.8.1.1 Node Representation (`node.rs`): Memory-efficient encoding of interaction network nodes.
      - 3.8.1.2 Port Management (`port.rs`): Connection points between nodes and their efficient representation.
      - 3.8.1.3 Runtime State Management (`runtime.rs`): Overall runtime system coordination.
      - 3.8.1.4 Worker Thread Implementation (`worker.rs`): Thread pool and execution logic.
      - 3.8.1.5 Network Partitioning (`partition.rs`): Dividing the interaction network for parallel execution.
      - 3.8.1.6 Reduction Rules (`reduce.rs`): Implementation of the interaction combinators.

    - **3.8.2 Memory Management:**
      - 3.8.2.1 Node Memory Layout: Compact representation using multi-word encoding.
      - 3.8.2.2 Connection Encoding: Efficient port-to-port references.
      - 3.8.2.3 Memory Allocation Strategy: Custom allocator optimized for network operations.
      - 3.8.2.4 Garbage Collection Implementation: Reference counting and cycle detection.
      - 3.8.2.5 Memory Pooling: Thread-local pools for rapid allocation.
      - 3.8.2.6 Cache-Conscious Design: Memory layout optimized for cache locality.

    - **3.8.3 Parallel Reduction Engine:**
      - 3.8.3.1 Worker Thread Management: Creation, scheduling, and coordination of worker threads.
      - 3.8.3.2 Work Stealing Scheduler: Load balancing through work stealing queues.
      - 3.8.3.3 Parallel Reduction Algorithm: Concurrent application of interaction rules.
      - 3.8.3.4 Thread Coordination: Synchronization mechanisms for worker interaction.
      - 3.8.3.5 Work Item Representation: Encoding of pending interactions.
      - 3.8.3.6 Termination Detection: Detecting when reduction is complete.

    - **3.8.4 Interaction Rule Implementation:**
      - 3.8.4.1 Core Combinators: Implementing symmetric combinators (ε, δ, ζ).
      - 3.8.4.2 Arithmetic Operations: Rules for numeric computation nodes.
      - 3.8.4.3 Control Flow Combinators: Handling conditionals and iteration.
      - 3.8.4.4 Data Structure Operations: List, array, and record manipulation.
      - 3.8.4.5 Dynamic Rule Selection: Efficient dispatch to appropriate rule implementations.
      - 3.8.4.6 Rule Application Strategy: Optimizing the order of rule application.

    - **3.8.5 Performance Optimizations:**
      - 3.8.5.1 Lock-Free Algorithms: Minimizing synchronization overhead in critical paths.
      - 3.8.5.2 NUMA-Aware Memory Management: Optimizing for non-uniform memory architectures.
      - 3.8.5.3 Memory Prefetching: Anticipatory loading of network regions.
      - 3.8.5.4 Vectorized Rule Application: Using SIMD instructions for multiple reductions.
      - 3.8.5.5 Thread Affinity: Processor core assignment strategies.
      - 3.8.5.6 Optimization Barriers: Preventing compiler optimizations that harm parallelism.

    - **3.8.6 Integration with Host Environment:**
      - 3.8.6.1 Foreign Function Interface: Integration with external C/C++ code.
      - 3.8.6.2 Memory Management Coordination: Handling shared memory between runtime and host.
      - 3.8.6.3 I/O Operations: Implementation of input/output capabilities.
      - 3.8.6.4 Exception Handling: Managing and reporting runtime errors.
      - 3.8.6.5 Resource Management: Controlling system resource usage.

  - **3.9 CLI and Tooling (`parallax-cli`):**
    - **3.9.1 Command-Line Interface:**
      - 3.9.1.1 Build Command Implementation.
      - 3.9.1.2 Run Command Implementation.
      - 3.9.1.3 Test Framework Integration.
    
    - **3.9.2 Compiler Driver:**
      - 3.9.2.1 Compilation Pipeline Orchestration.
      - 3.9.2.2 Error Reporting and Formatting.
      - 3.9.2.3 Output Generation Options.
    
    - **3.9.3 Development Tools:**
      - 3.9.3.1 Debug Information Generation.
      - 3.9.3.2 Performance Profiling Support.
      - 3.9.3.3 Integration with External Tools.
  
  - **3.10 Testing and Validation Approaches:**
    - **3.10.1 Unit Testing Framework:**
      - 3.10.1.1 Parser Test Suite.
      - 3.10.1.2 Type Checker Test Suite.
      - 3.10.1.3 Runtime Component Tests.
    
    - **3.10.2 Integration Testing:**
      - 3.10.2.1 End-to-End Compilation Tests.
      - 3.10.2.2 Runtime Behavior Validation.
      - 3.10.2.3 Performance Regression Tests.
    
    - **3.10.3 Validation and Verification:**
      - 3.10.3.1 Type System Soundness Tests.
      - 3.10.3.2 Parallel Execution Correctness Tests.
      - 3.10.3.3 Memory Management Validation.
  
  - **3.11 Repository Overview:** *(Mandatory Section, approx. 1 page)*
    - **3.11.1 Repository Structure:**
      - 3.11.1.1 `crates/` Directory Organization.
      - 3.11.1.2 Compiler Frontend Crates: `parallax-lang`, `parallax-resolve`, `parallax-typeck`, `parallax-hir`.
      - 3.11.1.3 Compiler Backend Crates: `parallax-mir`, `parallax-codegen`.
      - 3.11.1.4 Runtime Crates: `parallax-net`, `parallax-hvm`.
      - 3.11.1.5 Tooling Crates: `parallax-cli`, `tree-sitter-parallax`.
    
    - **3.11.2 Build System and Dependencies:**
      - 3.11.2.1 Cargo Workspace Configuration.
      - 3.11.2.2 External Dependencies and Versions.
      - 3.11.2.3 Development Dependencies.
    
    - **3.11.3 Attribution of External Code:**
      - 3.11.3.1 LLVM Integration Components.
      - 3.11.3.2 Parser Generator Usage (if applicable).
      - 3.11.3.3 Runtime Libraries and Frameworks.
    
    - **3.11.4 Development Workflow:**
      - 3.11.4.1 Contribution Process.
      - 3.11.4.2 Testing Framework.
      - 3.11.4.3 Documentation Generation.

- **Chapter 4: Evaluation**
  *(This chapter presents evidence of system performance and goal achievement, maintaining a rigorous, professional standard.)*
  - **4.1 Evaluation Methodology:**
    - 4.1.1 Design of the Benchmark Suite (Rationale for selection).
    - 4.1.2 Metrics: Execution Time, Memory Usage (Peak, Allocation Rate), Scalability, Correctness.
    - 4.1.3 Experimental Environment (Hardware Specifications, Software Versions).
    - 4.1.4 Comparison Points (Sequential C, other relevant systems like Bend).
  - **4.2 Quantitative Performance Results:**
    - 4.2.1 Memory Efficiency Analysis (Tabulated data, graphs).
    - 4.2.2 Parallel Scaling Performance (Speedup graphs, efficiency analysis).
    - 4.2.3 Presentation of Confidence Intervals/Statistical Significance where appropriate.
  - **4.3 Qualitative Assessment:**
    - 4.3.1 Demonstration of Correctness on Simple and Complex Cases.
    - 4.3.2 Case Studies Illustrating Runtime Behaviour on Specific Benchmarks.
    - 4.3.3 Usability Considerations from a Programmer's Perspective.
  - **4.4 Analysis Against Goals:**
    - 4.4.1 Assessment of How Many Original Goals Were Met.
    - 4.4.2 Discussion of Supporting Evidence for Achieved Goals.
    - 4.4.3 Honest Appraisal of Limitations, Unmet Goals, or Residual Issues.
  - *(Voluminous outputs/raw data deferred to appendices.)*

- **Chapter 5: Conclusions**
  *(A concise summary and reflection.)*
  - **5.1 Summary of Achievements:**
    - Recapitulation of the Parallax system and its key features.
    - Brief restatement of the main findings from the Evaluation chapter.
  - **5.2 Reflection on Original Motivations:**
    - Connecting the project outcomes back to the challenges outlined in the Introduction.
  - **5.3 Lessons Learned:**
    - 5.3.1 Insights Gained Regarding Interaction Network Implementation.
    - 5.3.2 Reflections on Compiler/Runtime Design for Parallelism.
    - 5.3.3 Experiences with the Development Process and Tools.
  - **5.4 Hindsight and Potential Improvements:**
    - How the project might be approached differently if started again.
  - **5.5 Future Work:**
    - Potential enhancements to the language, compiler, or runtime.
    - Suggestions for further research directions.

- **Bibliography**
  - List of all cited references, formatted correctly and consistently.

- **Appendices**
  *(Optional, within page limits)*
  - Potential Content: Code samples (PLX, Rust), detailed benchmark data, formal grammar, circuit diagrams (if applicable), extended user guide examples.
  - *Note: Contains supplementary material supporting the main text.*

- **Index**
  - *(Optional)*

- **Project Proposal**
  - The original, unmodified project proposal document, attached as required.

## File Organization

- `main.tex`: The main LaTeX file that includes all other files
- `chapters/`: Directory containing the chapter files
  - `introduction.tex`: Chapter 1
  - `preparation.tex`: Chapter 2
  - `implementation.tex`: Chapter 3
  - `evaluation.tex`: Chapter 4
  - `conclusions.tex`: Chapter 5
  - `appendix.tex`: Appendices
- `figures/`: Directory for figures and diagrams
- `references.bib`: Bibliography file in BibTeX format

## Guidelines

The dissertation follows the Cambridge Computer Science Part II Project guidelines:

- The main body should not exceed 40 pages or 12,000 words
- Use 12-point font with 2cm margins
- The structure follows the required format:
  1. Cover page
  2. Declaration of originality
  3. Proforma
  4. Table of contents
  5. Chapters 1-5
  6. Bibliography
  7. Appendices
  8. Index
  9. Project Proposal