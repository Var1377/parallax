# Parallax Code Generation Orchestrator

This crate acts as the central orchestrator for the code generation phase of the Parallax compiler. It takes the High-level Intermediate Representation (HIR) from `parallax-hir` and delegates the actual code generation work to specific backend crates.

## Design Goals

*   **Backend Agnostic:** Provide a single entry point (`generate_module`) for code generation, allowing different backends (e.g., native machine code, interaction nets, WebAssembly) to be plugged in.
*   **Coordination:** Manage the process of invoking one or more code generation backends based on compiler configuration or targets.
*   **Unified Output:** Define a common output structure (`CompiledOutput`) that encapsulates the results from the active backend(s).

## Current Implementation

Currently, this crate primarily acts as a thin wrapper around the `parallax-native` crate, which performs Just-In-Time (JIT) compilation of the HIR to native machine code using Cranelift.

*   **Input:** Takes an `&HirModule` (defined in `parallax-hir`).
*   **Process:** Calls `parallax_native::compile_hir` to generate native code.
*   **Output:** Returns a `CompiledOutput` struct, which currently contains the `CompiledArtifact` from `parallax-native`.

## Core Structures

*   **`generate_module(hir_module: &HirModule) -> Result<CompiledOutput, CodegenError>`**: The main function that drives code generation.
*   **`CompiledOutput`**: A struct holding the results of the code generation. It currently wraps the native artifact and provides methods to access function pointers from the JIT-compiled code (e.g., `get_function_ptr`, `get_function`, `call_nullary_i64`).
    *   **Safety:** Methods like `get_function` and `call_nullary_i64` are `unsafe` as they require the caller to ensure type safety and lifetime management of the JIT-compiled code.
*   **`CodegenError`**: An enum representing errors that can occur during code generation orchestration, including errors propagated from the backends.

## Usage

This crate is used internally by the compiler driver after HIR generation and optimization phases.

1.  The optimized `HirModule` is passed to `generate_module`.
2.  `generate_module` invokes the configured backend(s) (currently just `parallax-native`).
3.  The resulting `CompiledOutput` is returned, containing the executable artifact(s). This can then be used, for example, to execute the compiled code directly (in a JIT scenario).

## Future Work

*   **Interaction Net Backend:** Integrate an interaction net compiler backend (placeholders like `inet.rs` exist).
*   **Backend Selection Logic:** Implement logic to choose and configure different backends based on command-line flags or configuration files.
*   **Static Compilation:** Support generating persistent artifacts like object files or executables, not just JIT code.

## Contributing

(Contribution guidelines TBD)

## License

(License information TBD - likely Apache 2.0 or MIT) 