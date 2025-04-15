# Parallax HIR (High-level Intermediate Representation)

This crate defines the High-level Intermediate Representation (HIR) used within the Parallax compiler. It serves as a bridge between the type-checked Abstract Syntax Tree (AST) and lower-level representations like bytecode or target machine code.

## Design

*   **A-Normal Form (ANF):** The HIR is based on A-Normal Form. In ANF, complex expressions are broken down into a sequence of bindings (`let var = value in ...`), where `value` is a simple computation (like a function call, primitive operation, or aggregate creation) operating on atomic operands (variables or constants). This structured form simplifies subsequent analysis, optimization, and code generation passes.
*   **Explicit Types:** All HIR nodes, including variable bindings and expressions, carry explicit type information (`HirType`), derived from the type-checking phase.
*   **Symbol-Based:** Definitions (functions, structs, enums, statics) are identified using `Symbol`s provided by the `parallax-resolve` crate, ensuring unique identification across the compilation process.
*   **Control Flow:** Control flow constructs like `if` and `match` are represented explicitly within the `HirTailExpr` enum, which appears only in tail positions within an `HirExpr`.

## Core Structures

*   **`HirModule`**: Represents an entire compiled crate/module, containing collections of functions, structs, enums, and global statics (`statics`). It also tracks the entry point and the next available variable ID (`next_var_id`).
*   **`HirFunction`**: Represents a function, including its signature (`HirFunctionSignature`) and body (`HirExpr`). External functions have no body.
*   **`HirVar`**: A unique identifier for a local variable within the ANF structure.
*   **`HirExpr`**: The main expression type, either a `Let` binding or a `Tail` expression.
*   **`HirValue`**: Represents simple computations that can be bound in a `Let` statement (e.g., `Call`, `Aggregate`, `Project`, `Use`).
*   **`HirTailExpr`**: Represents expressions that appear in tail position, often related to control flow (`If`, `Match`) or the final result (`Return`).
*   **`Operand`**: Represents the inputs to `HirValue` computations, restricted to variables (`Var`), constants (`Const`), or global symbols (`Global`).
*   **`HirType`**: Represents types within the HIR, closely related to the types defined in `parallax-types`.
*   **`HirStructDef`, `HirEnumDef`, `HirGlobalStatic`**: Definitions for user-defined types and global variables.

## Usage

This crate is primarily used internally by the Parallax compiler.

1.  **Lowering:** The `parallax-types` crate's typed AST is lowered into `HirModule` using functions within the `lower` module (e.g., `lower::lower_to_hir`).
2.  **Optimization:** The HIR can be transformed by optimization passes provided in this crate:
    *   `perform_dce(HirModule) -> HirModule`: Performs Dead Code Elimination, removing unused functions, types, and statics. Note: Takes ownership and returns a new module.
    *   `perform_inlining(&mut HirModule)`: Performs function inlining based on heuristics like function size and recursion. Note: Modifies the module in place.
3.  **Code Generation:** Subsequent compiler passes (e.g., code generation for a VM or native target) consume the (potentially optimized) `HirModule`.

## Implementation Details

*   **Variable Renaming:** Passes like inlining require careful variable renaming to avoid conflicts. Unique `HirVar` IDs are generated using an atomic counter (`next_hir_var_id` in `HirModule`).
*   **Recursion Detection:** The inlining pass uses a call graph analysis to detect and avoid inlining recursive functions.
*   **Pass Ordering:** Typically, lowering is performed first, followed by optimization passes (like DCE and inlining, potentially run iteratively or in a specific order), and finally code generation.

## Contributing

(Contribution guidelines TBD)

## License

(License information TBD - likely Apache 2.0 or MIT) 