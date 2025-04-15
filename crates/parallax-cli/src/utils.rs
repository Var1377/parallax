use std::path::{Path, PathBuf};
use crate::error::CliError;

/// Finds the root directory of a Parallax frame by searching upwards for `frame.toml`.
pub fn find_frame_root(start_path: &Path) -> Result<PathBuf, CliError> {
    let mut current = if start_path.is_dir() {
        start_path.to_path_buf()
    } else {
        start_path.parent().map_or_else(
            || start_path.to_path_buf(), // Use start_path if no parent
            |p| p.to_path_buf()
        )
    };

    loop {
        let config_path = current.join("frame.toml");
        if config_path.is_file() {
            // Attempt to canonicalize the path for consistency
            return current.canonicalize().map_err(|e| CliError::IoError {
                 path: current.clone(), // Report the path we tried to canonicalize
                 operation: format!("canonicalizing frame root '{}'", current.display()),
                 source: e,
             });
        }

        if !current.pop() {
            // Reached filesystem root without finding frame.toml
            return Err(CliError::FrameNotFound {
                searched_path: start_path.to_path_buf(),
            });
        }
    }
} 