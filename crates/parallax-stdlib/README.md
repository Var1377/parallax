# parallax-stdlib

## Overview

This crate defines the standard library types, traits, functions, and intrinsics that are built into the Parallax programming language. It provides the core functionality that all Parallax programs can use without explicit imports.

The standard library is a critical component of the language, defining everything from primitive types and operations to advanced features like memory management, I/O, and concurrency primitives.

## Structure

The crate is organized as follows:

- `src/lib.rs` - Main entry point and database trait definition
- `src/types.rs` - Standard library types
- `src/traits.rs` - Core traits (Copy, Clone, Eq, Ord, Display, etc.)
- `src/functions.rs` - Standard functions
- `src/intrinsics.rs` - Language intrinsics (operations with special compiler support)
- `src/prelude.rs` - Prelude module with common imports
- `src/collections/` - Collection types and algorithms
- `src/io/` - Input/output functionality
- `src/concurrency/` - Concurrency primitives
- `src/memory/` - Memory management utilities
- `src/serialization.rs` - Interface for stdlib component serialization

## Usage

This crate is typically used as part of the compiler pipeline. Standard library components are automatically available to all Parallax programs, either explicitly imported or through the prelude.

## Dependencies

- `parallax-diagnostics` - For error reporting
- `salsa` - For incremental compilation

## Contributing

See the repository's main README for general contribution guidelines. 