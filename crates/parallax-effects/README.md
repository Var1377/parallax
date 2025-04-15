# parallax-effects

## Overview

This crate tracks and validates side effects in the Parallax programming language. It provides effect inference, effect sequencing validation, automatic resource cleanup tracking, and parallel safety validation. 

Effect tracking is a core feature of Parallax that ensures safe, predictable handling of side effects and enables both automatic parallelization and proper resource management. It annotates MIR with effect information that guides subsequent compilation phases.

## Structure

The crate is organized as follows:

- `src/lib.rs` - Main entry point and database trait definition
- `src/inference.rs` - Effect inference algorithm
- `src/validation.rs` - Effect sequencing validation
- `src/sequencing.rs` - Arrow operator for explicit sequencing
- `src/resource.rs` - Automatic resource cleanup tracking
- `src/affine.rs` - Affine type checking for resources
- `src/parallel.rs` - Parallel safety validation
- `src/annotation.rs` - MIR effect annotation utilities
- `src/error.rs` - Effect validation error types

## Usage

This crate is used in the compiler pipeline after MIR optimization. It takes optimized MIR and adds effect annotations and validations, returning effect-annotated MIR ready for code generation.

## Dependencies

- `parallax-mir-opt` - For the optimized MIR input
- `parallax-types` - For type information
- `salsa` - For incremental compilation

## Contributing

See the repository's main README for general contribution guidelines. 