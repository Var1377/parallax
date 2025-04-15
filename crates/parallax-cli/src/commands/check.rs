use crate::error::CliError;
use crate::utils::find_frame_root;
use parallax_db::Compiler;
use std::env;
use std::path::PathBuf;

pub fn handle_check(_diagnostics_flag: bool) -> Result<(), CliError> {
    let current_dir = env::current_dir().map_err(|e| CliError::IoError {
        path: PathBuf::from("."), // Placeholder path
        operation: "getting current directory".to_string(),
        source: e,
    })?;
    let frame_root = find_frame_root(&current_dir)?;

    println!("Checking frame at: {}", frame_root.display());

    // Initialize the compiler database from parallax-db2
    let db = Compiler::new(frame_root.clone());

    // Compile up to HIR. This includes parsing, resolution, and type checking.
    // Errors during these phases will be propagated as DatabaseError -> CliError.
    let _hir_module = db.compile_to_hir().map_err(CliError::from)?;

    // TODO: Add more detailed diagnostic reporting here if needed.

    // If db.compile_to_hir() succeeded, we report success for the check command.
    // Assumption: The database/compile process handles reporting/logging of any
    // non-critical diagnostics internally or through other means if it returns Ok.

    println!("✅ Frame checked successfully (compiled to HIR).");
    Ok(())
} 