# parallax-db

## Overview

This crate provides the core database infrastructure for the Parallax compiler, built upon the Salsa incremental compilation framework. It defines the central `Compiler` database which integrates query groups from various compiler phases (source management, syntax parsing, name resolution, type checking, etc.).

The goal is to enable efficient incremental compilation by tracking dependencies between compiler queries and caching results, allowing for precise invalidation when source code changes.

## Structure

The crate currently contains:

- `src/lib.rs` - Main entry point, re-exporting core types.
- `src/database.rs` - The central `Compiler` database implementation, integrating `SourceDatabase`, `SyntaxDatabase`, `ResolveDatabase`, `TypeDatabase` traits.
- `src/error.rs` - Database-related error types (`DatabaseError`, `DatabaseResult`).

*(Note: Other planned modules like `query.rs`, `storage.rs`, `cache.rs`, etc., are placeholders for future development.)*

## Usage

This crate provides the `Compiler` struct, which acts as the central Salsa database. Other compiler crates (like `parallax-syntax`, `parallax-resolve`, `parallax-types`) define their specific logic as queries implemented on traits (e.g., `SyntaxDatabase`). The `Compiler` database integrates these traits, allowing the overall compilation process to be driven through queries on the `Compiler` instance.

Example snippet from `database.rs` showing database integration:

```rust
use parallax_source::SourceDatabase;
use parallax_syntax::SyntaxDatabase;
use parallax_resolve::ResolveDatabase;
use parallax_types::TypeDatabase;
use salsa::Database;

#[salsa::db]
#[derive(Default, Clone)]
pub struct Compiler {
    storage: salsa::Storage<Self>,
    root_config: std::path::PathBuf,
}

#[salsa::db]
impl salsa::Database for Compiler {}

// Implements the database traits from other crates
#[salsa::db]
impl SourceDatabase for Compiler {}
#[salsa::db]
impl SyntaxDatabase for Compiler {}
#[salsa::db]
impl ResolveDatabase for Compiler {}
#[salsa::db]
impl TypeDatabase for Compiler {}
```

## Dependencies

- `salsa` - Core incremental compilation framework.
- `miette` - For diagnostic error reporting.
- `thiserror` - For defining custom error types.
- `serde` - For serialization capabilities.
- `log` - Logging facade.
- `triomphe` - Atomic reference counting.
- `parking_lot` - For efficient concurrency primitives.
- `dashmap` - Concurrent hash map implementation.
- `indexmap` - For ordered map/set implementations.
- Compiler Crates:
    - `parallax-source`
    - `parallax-syntax`
    - `parallax-resolve`
    - `parallax-types`
    - *(Other Parallax crates like `parallax-hir`, `parallax-codegen` will be integrated here)*

## Contributing

See the repository's main README for general contribution guidelines. 