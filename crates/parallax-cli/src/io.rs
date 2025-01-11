use std::path::PathBuf;

use crate::error::{CliError, convert_io_error};

pub fn read_file(path: PathBuf) -> Result<String, CliError> {
    let contents = std::fs::read_to_string(&path).map_err(|e| convert_io_error(e, path))?;
    Ok(contents)
}