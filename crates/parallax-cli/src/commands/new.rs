use crate::error::CliError;
use std::fs;
use std::path::PathBuf;
use parallax_source::{FrameConfigInner, PackageInfo}; // Use FrameConfigInner for defaults

pub fn handle_new(path: PathBuf, lib: bool) -> Result<(), CliError> {
    println!("Creating {} `{}`", if lib { "library" } else { "binary" }, path.display());

    // 1. Create root directory. It may already exist.
    if !path.exists() {
        fs::create_dir(&path).map_err(|e| CliError::IoError {
            path: path.clone(),
            operation: "creating directory".to_string(),
            source: e,
        })?;
    }

    // 2. Create frame.toml
    let frame_name = path.file_name().map_or_else(|| "new_frame".to_string(), |n| n.to_string_lossy().into_owned());
    let frame_config = FrameConfigInner {
        package: PackageInfo {
            name: frame_name.clone(),
            version: "0.1.0".to_string(),
            entry_point: if lib { "src/lib.plx".to_string() } else { "src/main.plx".to_string() },
            ..Default::default()
        },
        ..Default::default()
    };
    let toml_content = toml::to_string_pretty(&frame_config).map_err(|e| CliError::InternalError(format!("Failed to serialize frame.toml: {}", e)))?;
    let toml_path = path.join("frame.toml");
    fs::write(&toml_path, toml_content).map_err(|e| CliError::IoError {
        path: toml_path,
        operation: "writing frame.toml".to_string(),
        source: e,
    })?;

    // 3. Create src directory
    let src_path = path.join("src");

    if !src_path.exists() {
        fs::create_dir(&src_path).map_err(|e| CliError::IoError {
            path: src_path.clone(),
            operation: "creating src directory".to_string(),
            source: e,
        })?;
    }

    // 4. Create entry point file
    let entry_file_name = if lib { "lib.plx" } else { "main.plx" };
    let entry_path = src_path.join(entry_file_name);
    let entry_content = if lib {
        "pub fn hello() -> string = \"Hello from library!\";\n"
    } else {
        "fn main() = {\n    println(\"Hello, world!\")\n};\n"
    };

    if !entry_path.exists() {
        fs::write(&entry_path, entry_content).map_err(|e| CliError::IoError {
            path: entry_path,
            operation: format!("writing {}", entry_file_name),
            source: e,
        })?;
    }

    // 5. Create .gitignore
    let gitignore_path = path.join(".gitignore");
    let gitignore_content = "\ntarget\n";
    fs::write(&gitignore_path, gitignore_content).map_err(|e| CliError::IoError {
        path: gitignore_path,
        operation: "writing .gitignore".to_string(),
        source: e,
    })?;

    println!("Successfully created frame `{}`", frame_name);
    Ok(())
} 