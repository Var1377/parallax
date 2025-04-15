use crate::error::CliError;
use crate::utils::find_frame_root;
use std::env;
use std::fs;
use std::path::PathBuf;

pub fn handle_clean() -> Result<(), CliError> {
    let current_dir = env::current_dir().map_err(|e| CliError::IoError {
        path: PathBuf::from("."),
        operation: "getting current directory".to_string(),
        source: e,
    })?;
    let frame_root = find_frame_root(&current_dir)?;

    let target_dir = frame_root.join("target");

    if target_dir.exists() {
        println!("Cleaning frame at: {} (removing target/)", frame_root.display());
        fs::remove_dir_all(&target_dir).map_err(|e| CliError::IoError {
            path: target_dir.clone(),
            operation: format!("removing directory '{}'", target_dir.display()),
            source: e,
        })?;
        println!("✅ Successfully cleaned the frame.");
    } else {
        println!("Frame already clean (target/ directory not found).");
    }

    Ok(())
} 