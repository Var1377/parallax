use std::{collections::HashMap, fs, path::PathBuf};
use tempfile::TempDir;
use parallax_db::Compiler;
use parallax_source::SourceDatabase; // Needed for db.load_frame

/// Represents a minimal Parallax project structure in memory.
/// Keys are relative file paths (e.g., "frame.toml", "src/main.plx"),
/// values are file contents.
type TestProject = HashMap<String, String>;

/// Sets up a temporary directory with the given project structure
/// and returns a Compiler instance rooted at that directory.
///
/// # Panics
/// Panics if the temporary directory or file creation fails.
fn setup_test_compiler(project: TestProject) -> (Compiler, TempDir) {
    let temp_dir = TempDir::new().expect("Failed to create temporary directory");
    let root_path = temp_dir.path();

    for (relative_path, content) in project {
        let full_path = root_path.join(&relative_path);
        if let Some(parent) = full_path.parent() {
            fs::create_dir_all(parent)
                .expect(&format!("Failed to create directory: {:?}", parent));
        }
        fs::write(&full_path, content)
            .expect(&format!("Failed to write file: {:?}", full_path));
    }

    // The Compiler expects the path to the *crate root*, which contains frame.toml
    let compiler = Compiler::new(root_path.to_path_buf());

    (compiler, temp_dir)
}

#[test]
fn test_simple_addition() {
    let mut project = TestProject::new();
    project.insert(
        "frame.toml".to_string(),
        r#"
[package]
name = "simple_test"
version = "0.1.0"
entry_point = "src/main.plx"
"#
        .to_string(),
    );
    project.insert(
        "src/main.plx".to_string(),
        r#"
fn main() -> i64 = {
    let x = 10;
    let y = 32;
    x + y // Should result in 42
};
"#
        .to_string(),
    );

    let (db, _temp_dir) = setup_test_compiler(project);

    // Attempt to compile the project to HIR
    // In a real test, you'd assert specific outcomes or errors.
    // For now, we just check if it compiles without panicking.
    let hir_result = db.compile_to_hir();

    println!("HIR result: {:?}", hir_result);

    // Example assertion (more detailed checks would go here)
    assert!(hir_result.is_ok(), "Compilation to HIR failed: {:?}", hir_result.err());

    // Optionally, compile for running
    let run_result = db.compile_for_run();
    assert!(run_result.is_ok(), "Compilation for run failed: {:?}", run_result.err());

    // TODO: Add checks for diagnostics accumulation
    // let diagnostics = db.load_frame::accumulated::<ParallaxDiagnostic>(&db, "path/to/frame.toml");
    // assert!(diagnostics.is_empty());

    // TODO: Add checks on the resulting HIR or compiled artifact

    // _temp_dir is kept alive until the end of the test, automatically cleaning up
}

// Add more tests below... 