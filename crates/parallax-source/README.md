# parallax-source

Source file and frame management for the Parallax compiler with Salsa-based incremental compilation support.

## Overview

The `parallax-source` crate is responsible for:

- Loading and managing "Frames" of source code, representing compilation units (like crates or packages).
- Handling frame configuration (`frame.toml`), including package metadata and dependencies.
- Representing the directory structure and source files (`.plx`) within a frame.
- Tracking source file changes to enable incremental compilation via Salsa.
- Providing a diagnostic system for reporting errors during frame loading and processing.

This crate provides the foundational input data (source code, configuration, dependencies) for the Parallax compiler pipeline.

## Key Components

- **`Frame` (`frame.rs`)**: A Salsa-tracked struct representing a compilation unit. It contains:
    - `FrameConfig`: The parsed configuration.
    - Root `Dir`: The top-level directory structure.
    - Resolved Dependencies: A map of dependency names to their loaded `Frame` structs.
- **`Dir` (`frame.rs`)**: A Salsa-tracked struct representing a directory within a frame. Contains:
    - Name: The directory's name.
    - Files: A `Vec<SourceFile>` for `.plx` files in the directory.
    - Dirs: A `Vec<Dir>` for subdirectories.
- **`SourceFile` (`file.rs`)**: A Salsa-tracked struct representing a single source file. Contains:
    - Location: The canonical path string.
    - Contents: The string content of the file.
- **`FrameConfig` (`config.rs`)**: A Salsa input struct holding the parsed content of `frame.toml` via `FrameConfigInner`. Includes package info (`PackageInfo`) and dependencies (`Dependency`).
- **`Path` (`path.rs`)**: A Salsa-interned struct efficiently representing filesystem paths (`PathBuf`).
- **Diagnostics System (`diagnostic.rs`)**: Defines:
    - `ParallaxDiagnostic`: A Salsa accumulator for collecting diagnostics.
    - `ParallaxError`: A trait for errors that can be reported with source context via `miette`.
    - `FrameError` (`error.rs`): Specific errors related to frame loading and configuration.

## Loading Frames

The primary way to load source code is via the `load_frame` tracked query function defined on the `SourceDatabase` trait. Given an input path:

1.  It canonicalizes the path.
2.  It attempts to find and read `frame.toml` in the target directory or use the input path if it's a file.
3.  It parses the TOML content into a `FrameConfig`.
4.  It resolves path dependencies listed in the config by recursively calling `load_frame`.
5.  It builds the `Dir` structure representing the source files (`.plx`) and subdirectories, starting from the configured entry point or the frame directory.
6.  Errors during any step (path not found, invalid config, unreadable files, dependency resolution failure) are reported using the `ParallaxDiagnostic` accumulator.
7.  It returns a `Frame` struct containing the configuration, root directory, and resolved dependency frames.

```rust
use parallax_source::{SourceDatabase, Frame, Path};
use std::path::PathBuf;

fn load_my_crate(db: &dyn SourceDatabase, path: PathBuf) -> Frame {
    // Get an interned Path
    let frame_path = Path::new(db, path);
    // Load the frame
    frame::load_frame(db, frame_path)
    // Errors are accumulated in the database
}
```

## Integration with Salsa

This crate leverages Salsa for incremental computation:

- `SourceFile`, `Frame`, and `Dir` are `#[salsa::tracked]` structs.
- `FrameConfig` is a `#[salsa::input]` struct.
- `Path` is a `#[salsa::interned]` struct.
- `SourceDatabase` is a `#[salsa::db]` trait defining queries like `load_frame`.
- `ParallaxDiagnostic` is a `#[salsa::accumulator]` for collecting errors.

Changes to `FrameConfig` inputs or the content of `SourceFile`s will trigger re-computation only for the necessary downstream queries (parsing, resolution, compilation).

## Dependencies

- `salsa`: Core incremental computation framework.
- `miette`: For diagnostic error reporting framework.
- `thiserror`: For deriving standard error traits.
- `serde`: For serializing/deserializing `FrameConfigInner`.
- `toml`: For parsing `frame.toml` files.
- `fxhash`: Faster hash map implementation used for dependencies.
- `log`: For logging (optional).

## License

This crate is part of the Parallax compiler and is licensed under the same terms as the main project. 