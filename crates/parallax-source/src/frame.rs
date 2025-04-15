use crate::diagnostic::report_error;
use crate::{Dependency, FrameConfig, FrameConfigInner, PackageInfo, FrameError, SourceDatabase, SourceFile};
use crate::path::Path;
use std::{fs, path::PathBuf};
use fxhash::FxHashMap;

#[salsa::tracked]
pub struct Frame<'db> {
    pub config: FrameConfig,

    #[tracked]
    pub config_source: Option<SourceFile<'db>>,
    #[tracked]
    pub root: Dir<'db>,
    
    /// Resolved dependencies of this frame
    #[tracked]
    pub dependencies: FxHashMap<String, Frame<'db>>,
}

#[salsa::tracked]
pub struct Dir<'db> {
    /// The name of this directory (useful for module naming)
    #[id]
    pub name: String,
    
    #[tracked]
    pub files: Vec<SourceFile<'db>>,

    #[tracked]
    #[return_ref]
    pub dirs: Vec<Dir<'db>>,
}

/// Resolve a frame from a configuration path with improved error reporting
///
/// This function resolves a frame from a configuration path, reading and parsing
/// the frame configuration, and building a directory structure. It reports errors
/// through the diagnostic accumulator system.
///
/// # Parameters
///
/// * `db` - The source database
/// * `input_path` - Path to the frame's configuration file or directory
///
/// # Returns
///
/// The resolved Frame structure, or a default frame with errors reported via accumulator
#[salsa::tracked]
pub fn load_frame<'db>(db: &'db dyn SourceDatabase, input_path: Path<'db>) -> Frame<'db> {
    // Create a default config to use in error cases
    let default_config = FrameConfigInner::default();
    let empty_dir = Dir::new(db, "root".to_string(), vec![], vec![]);
    
    // Variables for tracking config, config source, and directory structure
    let mut frame_config = FrameConfig::new(db, default_config.clone());
    let mut config_source: Option<SourceFile<'db>> = None;
    let mut frame_dir_path = input_path.inner(db).clone();
    let mut dependencies = FxHashMap::default();
    let default_config_name = "frame.toml"; // Default configuration filename
    
    // Resolve the frame configuration
    match input_path.inner(db).canonicalize() {
        Ok(frame_path) => {
            // Path exists, try to find and parse the configuration file
            
            // Determine file path and parent directory
            let (_parent_dir, config_file_path, content) = if frame_path.is_file() {
                // If the path is directly to a file, assume it's a config file
                let parent = frame_path.parent().unwrap_or_else(|| std::path::Path::new("."));
                frame_dir_path = parent.to_path_buf();
                    
                // Read file content
                match fs::read_to_string(&frame_path) {
                    Ok(content) => (parent.to_path_buf(), frame_path.clone(), content),
                    Err(err) => {
                        let error = FrameError::ConfigReadError(err.to_string());
                        report_error(db, frame_path.to_string_lossy().into_owned(), error);
                        
                        // Continue with default config but still scan directory
                        (parent.to_path_buf(), frame_path.clone(), "".to_string())
                    }
                }
            } else {
                // It's a directory, look for default config file inside it
                frame_dir_path = frame_path.clone();
                let config_path_file = frame_path.join(default_config_name);
                
                if config_path_file.exists() {
                    // Read the file content
                    match fs::read_to_string(&config_path_file) {
                        Ok(content) => (frame_path.clone(), config_path_file, content),
                        Err(err) => {
                            let error = FrameError::ConfigReadError(err.to_string());
                            report_error(db, config_path_file.to_string_lossy().into_owned(), error);
                            
                            // Continue with default config but still scan directory
                            (frame_path.clone(), config_path_file, "".to_string())
                        }
                    }
                } else {
                    // Look for any .toml file in the directory that might be a config
                    let mut found_config = None;
                    if let Ok(entries) = fs::read_dir(&frame_path) {
                        for entry in entries.filter_map(Result::ok) {
                            let entry_path = entry.path();
                            if entry_path.is_file() && 
                               entry_path.extension().map_or(false, |ext| ext == "toml") {
                                // Found a .toml file, try to use it as config
                                if let Ok(content) = fs::read_to_string(&entry_path) {
                                    found_config = Some((frame_path.clone(), entry_path, content));
                                    break;
                                }
                            }
                        }
                    }
                    
                    found_config.unwrap_or_else(|| {
                        // Report config file not found
                        let path_display = frame_path.to_string_lossy().into_owned();
                        let error = FrameError::ConfigNotFound(frame_path);
                        report_error(db, path_display, error);
                        
                        // Continue with default config
                        (frame_dir_path.clone(), frame_dir_path.join(default_config_name), "".to_string())
                    })
                }
            };
            
            // Create a source file for the config content
            if !content.is_empty() {
                config_source = Some(SourceFile::new(
                    db, 
                    config_file_path.to_string_lossy().into_owned(), 
                    content.clone()
                ));
            }
            
            // Parse the configuration if we have content
            if !content.is_empty() {
                match toml::from_str::<FrameConfigInner>(&content) {
                    Ok(config_inner) => {
                        frame_config = FrameConfig::new(db, config_inner.clone());
                        
                        // Process dependencies
                        for (name, details) in &config_inner.dependencies {
                            match details {
                                Dependency::Simple(path_str) => {
                                    // Report simple dependencies not supported yet
                                    let error = FrameError::UnsupportedDependencyType(format!("Simple dependency: {}", path_str));
                                    report_error(db, input_path.inner(db).to_string_lossy().into_owned(), error);
                                }
                                Dependency::Detailed(details) => {
                                    if let Some(path) = &details.path {
                                        let dep_path = Path::new(db, frame_dir_path.join(path));
                                        let dep_frame = load_frame(db, dep_path);
                                        dependencies.insert(name.clone(), dep_frame);
                                    } else {
                                        // Report non-path dependency not supported yet
                                        let error = FrameError::UnsupportedDependencyType(format!("Non-path dependency: {}", name));
                                        report_error(db, input_path.inner(db).to_string_lossy().into_owned(), error);
                                    }
                                }
                            }
                        }
                    },
                    Err(parse_err) => {
                        let error = FrameError::ConfigParseError(parse_err.to_string());
                        report_error(db, config_file_path.to_string_lossy().into_owned(), error);
                    }
                }
            }
        },
        Err(err) => {
            // Report path not found
            let error = FrameError::CanonicalizeError(err.to_string());
            report_error(db, input_path.inner(db).to_string_lossy().into_owned(), error);
        }
    };
    
    // Build directory structure regardless of config parse success
    let root = match build_frame_structure(db, &frame_dir_path, &frame_config.inner(db)) {
        Ok(root) => root,
        Err(err) => {
            report_error(db, frame_dir_path.to_string_lossy().into_owned(), err);
            empty_dir
        }
    };
    
    // Create and return the frame
    Frame::new(db, frame_config, config_source, root, dependencies)
}

/// Build a frame structure from a directory path and configuration
///
/// This function builds a directory structure for a frame based on the configuration.
/// It handles finding the entry point and processing the relevant directory structure.
///
/// # Parameters
///
/// * `db` - The source database
/// * `frame_dir` - The directory containing the frame
/// * `config` - The frame configuration
///
/// # Returns
///
/// A result containing the built Dir if successful, or a FrameError if an error occurred
fn build_frame_structure<'db>(
    db: &'db dyn SourceDatabase, 
    frame_dir: &std::path::Path,
    config: &FrameConfigInner
) -> Result<Dir<'db>, FrameError> {
    // Get the entry point path from the configuration
    let entry_path = frame_dir.join(&config.package.entry_point);
    
    if entry_path.is_file() {
        // If the entry point is a file, create a directory structure with just that file
        if let Some(extension) = entry_path.extension() {
            if extension == "plx" {
                // Read the file content and create a SourceFile
                if let Ok(content) = fs::read_to_string(&entry_path) {
                    // Get the absolute path for location
                    let absolute_path = match entry_path.canonicalize() {
                        Ok(abs_path) => abs_path.to_string_lossy().to_string(),
                        Err(_) => {
                            return Err(FrameError::EntryPointError(
                                format!("Failed to get absolute path for {}", entry_path.display())
                            ));
                        }
                    };
                    
                    // Extract directory name from the entry point
                    let dir_name = entry_path
                        .parent()
                        .and_then(|p| p.file_name())
                        .map(|n| n.to_string_lossy().to_string())
                        .unwrap_or_else(|| "root".to_string());
                    
                    let files = vec![SourceFile::new(db, absolute_path, content)];
                    Ok(Dir::new(db, dir_name, files, vec![]))
                } else {
                    Err(FrameError::EntryPointError(
                        format!("Failed to read entry point file: {}", entry_path.display())
                    ))
                }
            } else {
                Err(FrameError::EntryPointError(
                    format!("Entry point file has invalid extension: {}", entry_path.display())
                ))
            }
        } else {
            Err(FrameError::EntryPointError(
                format!("Entry point file has no extension: {}", entry_path.display())
            ))
        }
    } else if entry_path.is_dir() {
        // If the entry point is a directory, build the directory tree from it
        build_directory_tree(db, &entry_path).ok_or_else(|| 
            FrameError::DirectoryStructureError(
                format!("Failed to build directory structure for {}", entry_path.display())
            )
        )
    } else {
        // If the entry point doesn't exist, try using the frame directory itself
        if frame_dir.is_dir() {
            build_directory_tree(db, frame_dir).ok_or_else(|| 
                FrameError::DirectoryStructureError(
                    format!("Failed to build directory structure for {}", frame_dir.display())
                )
            )
        } else {
            Err(FrameError::EntryPointError(
                format!("Entry point does not exist: {}", entry_path.display())
            ))
        }
    }
}

/// Build a directory tree from a file path
///
/// Recursively builds a directory tree, collecting all source files with .plx extension
/// and subdirectories into a Dir structure.
///
/// # Parameters
///
/// * `db` - The source database
/// * `path` - The path to the directory to build the tree from
///
/// # Returns
///
/// An option containing the built Dir if successful, or None if the directory
/// couldn't be read or processed.
fn build_directory_tree<'db>(db: &'db dyn SourceDatabase, path: &std::path::Path) -> Option<Dir<'db>> {
    // Check if the path exists and is a directory
    if !path.is_dir() {
        return None;
    }
    
    // Extract the directory name
    let dir_name = path
        .file_name()
        .map(|n| n.to_string_lossy().to_string())
        .unwrap_or_else(|| "unnamed".to_string());
    
    let mut files = Vec::new();
    let mut dirs = Vec::new();
    
    // Try to read the directory contents
    match fs::read_dir(path) {
        Ok(entries) => {
            for entry in entries.flatten() {
                let entry_path = entry.path();
                
                if entry_path.is_file() {
                    // Only consider relevant source files
                    if let Some(extension) = entry_path.extension() {
                        if extension == "plx" {
                            // Read the file content and create a SourceFile
                            if let Ok(content) = fs::read_to_string(&entry_path) {
                                // Get the absolute path for location
                                let absolute_path = match entry_path.canonicalize() {
                                    Ok(abs_path) => abs_path.to_string_lossy().to_string(),
                                    Err(_) => continue, // Skip files where we can't get absolute path
                                };
                                
                                files.push(SourceFile::new(db, absolute_path, content));
                            }
                        }
                    }
                } else if entry_path.is_dir() {
                    // Recursively process subdirectories
                    if let Some(subdir) = build_directory_tree(db, &entry_path) {
                        dirs.push(subdir);
                    }
                }
            }
            
            Some(Dir::new(db, dir_name, files, dirs))
        },
        Err(_) => None,
    }
}
