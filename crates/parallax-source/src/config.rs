use serde::{Serialize, Deserialize};
use fxhash::FxHashMap;

#[salsa::input]
pub struct FrameConfig {
    #[salsa::return_ref]
    pub inner: FrameConfigInner,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct FrameConfigInner {
    /// Package information
    pub package: PackageInfo,
    
    /// Dependencies of this frame
    #[serde(default)]
    pub dependencies: FxHashMap<String, Dependency>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageInfo {
    /// Name of the package
    pub name: String,
    
    /// Version of the package (semver)
    pub version: String,

    /// Entry point of the package
    #[serde(default = "default_entry_point")]
    pub entry_point: String,
    
    /// Authors of the package
    #[serde(default)]
    pub authors: Vec<String>,
    
    /// Package description
    #[serde(default)]
    pub description: Option<String>,
    
    /// License information
    #[serde(default)]
    pub license: Option<String>,
    
    /// Repository URL
    #[serde(default)]
    pub repository: Option<String>,
    
    /// Documentation URL
    #[serde(default)]
    pub documentation: Option<String>,
    
    /// Homepage URL
    #[serde(default)]
    pub homepage: Option<String>,
    
    /// Keywords for package search
    #[serde(default)]
    pub keywords: Vec<String>,
    
    /// Categories this package belongs to
    #[serde(default)]
    pub categories: Vec<String>,
}

impl Default for PackageInfo {
    fn default() -> Self {
        Self {
            name: "".to_string(),
            version: "".to_string(),
            entry_point: "main.plx".to_string(),
            authors: vec![],
            description: None,
            license: None,
            repository: None,
            documentation: None,
            homepage: None,
            keywords: vec![],
            categories: vec![],
        }
    }
}

pub fn default_entry_point() -> String {
    "src/main.plx".to_string()
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Dependency {
    /// Simple version string dependency
    Simple(String),
    
    /// Detailed dependency specification
    Detailed(DependencyDetails),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DependencyDetails {
    /// Version requirement
    #[serde(default)]
    pub version: Option<String>,
    
    /// Git repository URL
    #[serde(default)]
    pub git: Option<String>,
    
    /// Branch name for git dependency
    #[serde(default)]
    pub branch: Option<String>,
    
    /// Tag for git dependency
    #[serde(default)]
    pub tag: Option<String>,
    
    /// Revision/commit hash for git dependency
    #[serde(default)]
    pub rev: Option<String>,
    
    /// Path to local dependency
    #[serde(default)]
    pub path: Option<String>,
    
    /// Whether the dependency is optional
    #[serde(default)]
    pub optional: bool,
}