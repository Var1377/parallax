use crate::commands::build::handle_build; // Reuse build logic
use crate::error::CliError;
use crate::utils::find_frame_root;
use std::path::PathBuf;
use std::process::Command;
use std::env;
// --- New Imports ---
use parallax_db::Compiler; // Needed to interact with the compiler database
use parallax_rt::{init_runtime, run_artifact, RuntimeError}; // From our new runtime crate
use parallax_native::CompiledArtifact; // The result of compilation
use parallax_hir::Symbol; // To represent the entry point function
use std::collections::HashMap; // For dummy artifact
use parallax_hir::hir::{HirType, ResolvePrimitiveType}; // Import HirType

// Add a new error variant to CliError for runtime issues
impl From<RuntimeError> for CliError {
    fn from(rt_err: RuntimeError) -> Self {
        // TODO: Improve this mapping, maybe add specific variants to CliError
        CliError::RuntimeError(rt_err.to_string())
    }
}

pub fn handle_run(path: Option<PathBuf>, _release: bool, _profile: bool, args: Vec<String>) -> Result<(), CliError> {
    // 1. Find the frame root (as before)
    let start_path = match path {
        Some(p) => p,
        None => env::current_dir().map_err(|e| CliError::IoError {
            path: PathBuf::from("."),
            operation: "getting current directory for run".to_string(),
            source: e,
        })?,
    };
    let frame_root = find_frame_root(&start_path)?;
    println!("Running frame at: {}", frame_root.display());

    // 2. Compile the project using the database
    println!("Compiling project...");
    let compiler = Compiler::new(frame_root.clone());
    // Destructure the new tuple including the entry type
    let (artifact, entry_symbol, entry_type) = compiler.compile_for_run().map_err(CliError::from)?;
    println!("Compilation successful. Entry point symbol: {:?}, Type: {:?}", entry_symbol, entry_type);

    // 3. Initialize the Parallax Runtime (mainly GC)
    println!("Initializing Parallax Runtime...");
    init_runtime().map_err(CliError::from)?;

    // 4. Run the compiled artifact using the runtime, passing the entry type
    println!("Executing artifact...");
    let execution_result = run_artifact(artifact, entry_symbol, &entry_type, args);

    // 5. Handle execution result
    match execution_result {
        Ok(exit_code) => {
            // Check if the return type is i64 to provide more specific output
            match &entry_type {
                HirType::Primitive(ResolvePrimitiveType::I64) => {
                    println!("Program returned: {}", exit_code);
                },
                _ => {
                    println!("Program exited with code: {}", exit_code);
                }
            }
            
            // TODO: Convert i64 exit code to actual process exit code?
            if exit_code == 0 {
                Ok(())
            } else {
                Err(CliError::RunFailed) // Use existing RunFailed for non-zero exit
            }
        }
        Err(runtime_error) => {
            eprintln!("Runtime Error: {}", runtime_error);
            Err(CliError::from(runtime_error)) // Convert runtime error to CliError
        }
    }

    // --- Old code removed (handle_build call, Command execution) ---
} 