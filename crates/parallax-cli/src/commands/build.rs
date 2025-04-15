use crate::error::CliError;
use parallax_db::Compiler;
use std::path::PathBuf;

pub fn handle_build(frame_root: PathBuf, release: bool, _profile: bool) -> Result<(), CliError> {
    let mode = if release { "release" } else { "debug" };
    println!("Building frame at: {} ({} mode)", frame_root.display(), mode);

    // TODO: Add progress indication (e.g., indicatif)
    // TODO: Make use of profile

    let db = Compiler::new(frame_root);

    // Call the main compile function from the database.
    // This assumes `compile` handles all steps: load, parse, resolve, typecheck, IR gen, codegen.
    // And reports errors via returning Err(DatabaseError).
    match db.compile_for_run() {
        Ok(_) => {
            println!("✅ Frame built successfully.");
            // TODO: Eventually get artifact path from compile result?
            Ok(())
        }
        Err(db_err) => {
            // The From<DatabaseError> for CliError handles the conversion.
            Err(CliError::from(db_err))
            // We might want a specific BuildFailed error here, but DbError likely contains the details.
            // Err(CliError::BuildFailed)
        }
    }
} 