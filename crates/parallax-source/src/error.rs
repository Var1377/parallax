use std::path::PathBuf;
use thiserror::Error;
use miette::{Diagnostic, SourceSpan};

/// A wrapper around std::io::Error that is RefUnwindSafe
#[derive(Debug, Error)]
#[error("{0}")]
pub struct IoError(#[from] std::io::Error);

/// Errors that can occur when resolving a frame
#[derive(Debug, Error, Diagnostic, Clone)]
pub enum FrameError {
    /// The frame directory or file path does not exist
    #[error("Path does not exist: {0}")]
    #[diagnostic(
        code("FRAME-001"),
        help("Make sure the path exists and has proper permissions"),
        url("https://parallaxlang.org/docs/frames/path-resolution")
    )]
    PathNotFound(PathBuf),
    
    /// The frame configuration file does not exist
    #[error("Frame configuration file not found in {0}")]
    #[diagnostic(
        code("FRAME-002"),
        help("Every frame must have a frame.toml file in its root directory"),
        url("https://parallaxlang.org/docs/frames/configuration")
    )]
    ConfigNotFound(PathBuf),
    
    /// Error reading the frame configuration file
    #[error("Error reading frame configuration: {0}")]
    #[diagnostic(
        code("FRAME-003"),
        help("Check file permissions and ensure the file is not corrupted"),
        url("https://parallaxlang.org/docs/frames/troubleshooting")
    )]
    ConfigReadError(String),
    
    /// Error parsing the frame configuration
    #[error("Invalid frame configuration: {0}")]
    #[diagnostic(
        code("FRAME-004"),
        help("Check your frame.toml syntax and ensure it follows the format specification"),
        url("https://parallaxlang.org/docs/frames/toml-format")
    )]
    ConfigParseError(String),
    
    /// Error building the directory structure
    #[error("Error building frame directory structure: {0}")]
    #[diagnostic(
        code("FRAME-005"),
        help("Ensure all directories are accessible and contain valid source files"),
        url("https://parallaxlang.org/docs/frames/directory-structure")
    )]
    DirectoryStructureError(String),

    /// Error with specific location information in configuration
    #[error("Frame configuration error at {position}: {message}")]
    #[diagnostic(
        code("FRAME-006"),
        help("Review the frame configuration section highlighted above"),
        url("https://parallaxlang.org/docs/frames/configuration-validation")
    )]
    ConfigurationLocationError {
        position: String,
        message: String,
        #[source_code]
        src: String,
        #[label("error occurs here")]
        span: SourceSpan,
    },
    
    /// Error resolving a dependency
    #[error("Error resolving dependency '{name}': {reason}")]
    #[diagnostic(
        code("FRAME-007"),
        help("Check that the dependency path is correct and accessible"),
        url("https://parallaxlang.org/docs/frames/dependencies")
    )]
    DependencyResolutionError {
        name: String,
        reason: String,
    },
    
    /// Error with unsupported dependency type
    #[error("Unsupported dependency type: {0}")]
    #[diagnostic(
        code("FRAME-008"),
        help("Currently only path-based dependencies are supported"),
        url("https://parallaxlang.org/docs/frames/dependency-types")
    )]
    UnsupportedDependencyType(String),
    
    /// Error with entry point resolution
    #[error("Entry point error: {0}")]
    #[diagnostic(
        code("FRAME-009"),
        help("Check that the entry point exists and is a valid .plx file"),
        url("https://parallaxlang.org/docs/frames/entry-points")
    )]
    EntryPointError(String),
    
    /// Error with frame canonicalization
    #[error("Could not canonicalize path: {0}")]
    #[diagnostic(
        code("FRAME-010"),
        help("The path may contain invalid characters or refer to a non-existent location"),
        url("https://parallaxlang.org/docs/frames/path-resolution")
    )]
    CanonicalizeError(String),
    
    /// Generic frame error
    #[error("Frame error: {0}")]
    #[diagnostic(
        code("FRAME-999"),
        help("Unexpected error while processing frame"),
        url("https://parallaxlang.org/docs/frames/troubleshooting")
    )]
    GenericError(String),
}

// Conversion from io::Error to FrameError
impl From<std::io::Error> for FrameError {
    fn from(err: std::io::Error) -> Self {
        FrameError::ConfigReadError(err.to_string())
    }
}

// Conversion from toml::de::Error to FrameError
impl From<toml::de::Error> for FrameError {
    fn from(err: toml::de::Error) -> Self {
        FrameError::ConfigParseError(err.to_string())
    }
}