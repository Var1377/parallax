# Parallax CLI (plx)

The `plx` command-line interface is the primary tool for managing and interacting with Parallax projects (frames). It provides commands for creating, building, running, checking, and managing Parallax frames and their dependencies.

## Design Goals

*   **User-Friendly:** Provide clear commands and helpful error messages using `clap` for argument parsing and `miette` for rich diagnostics.
*   **Modular:** Organize functionality into distinct command modules (`src/commands/`).
*   **Integrated:** Leverage the core compiler logic from the `parallax-db` crate for tasks like building and checking.
*   **Standardized:** Follow common Rust project conventions and integrate with the Parallax project structure defined by `frame.toml` (handled by `parallax-source`).

## Installation

Build and install the CLI locally using Cargo:

```bash
cargo install --path .
```

Ensure the installation directory (usually `~/.cargo/bin`) is in your system's `PATH`.

## Usage

The basic structure of a `plx` command is:

```bash
plx [OPTIONS] <COMMAND> [SUBCOMMAND_OPTIONS] [ARGS]
```

### Available Commands

*   **`plx new <PATH> [--lib]`**: Creates a new Parallax frame (project) at the specified path. Use `--lib` to create a library frame instead of a binary.
    ```bash
    plx new my_awesome_frame
    plx new my_utility_lib --lib
    ```
*   **`plx build [PATH] [-r|--release] [-p|--profile]`**: Compiles the frame found in the current directory or the specified `PATH`.
    *   `-r`, `--release`: Build optimized artifacts.
    *   `-p`, `--profile`: Build with profiling information (Note: Currently unused in build logic).
    ```bash
    plx build
    plx build path/to/my_frame -r
    ```
*   **`plx run [PATH] [-r|--release] [-p|--profile] [-- <ARGS>]`**: Builds and runs the frame's binary executable. Any arguments after `--` are passed directly to the executable.
    *   `-r`, `--release`: Build and run optimized artifacts.
    *   `-p`, `--profile`: Build and run with profiling information (Note: Currently unused in build logic).
    ```bash
    plx run
    plx run -r -- --port 8080 --verbose
    ```
*   **`plx check`**: Analyzes the current frame and reports errors without building artifacts.
    ```bash
    plx check
    ```
*   **`plx clean`**: Removes build artifacts (the `target/` directory) from the current frame.
    ```bash
    plx clean
    ```

### Planned Commands (Not Yet Implemented)

*   `init`: Initialize a frame in an existing directory.
*   `test`: Compile and run tests.
*   `add`: Add dependencies.
*   `remove`: Remove dependencies.
*   `update`: Update dependencies.
*   `fmt`: Format Parallax source code.
*   `show`: Inspect compiler artifacts and stages (e.g., `show dependencies`, `show ast`).

## Implementation Details

*   **Entry Point:** `src/main.rs` parses command-line arguments using `clap` and dispatches to the appropriate command handler.
*   **Command Handlers:** Logic for each command (e.g., `build`, `new`) resides in its own module within `src/commands/`.
*   **Error Handling:** Custom error types defined in `src/error.rs` (`CliError`) are used with `miette` to generate user-friendly diagnostic reports.
*   **Core Logic:** Compilation, type checking, and other core compiler tasks are delegated to the `Compiler` struct from the `parallax-db` crate.
*   **Project Discovery:** The `src/utils.rs` module contains helpers like `find_frame_root` to locate the `frame.toml` file, signifying the project root.
*   **Configuration:** Frame configuration (`frame.toml`) is loaded and parsed using types from the `parallax-source` crate.

## Contributing

Contributions are welcome! Please follow standard Rust development practices. (Further contribution guidelines TBD).

## License

(License information TBD - likely Apache 2.0 or MIT). 