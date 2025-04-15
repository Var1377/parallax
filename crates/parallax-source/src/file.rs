/// A source file in the Parallax compiler.
///
/// This is a Salsa input struct, which means it can be modified
/// and Salsa will track its changes for incremental compilation.
///
/// # Fields
///
/// * `path` - The filesystem path to the source file
/// * `contents` - The raw text content of the source file
///
/// # Salsa Attributes
///
/// * `#[salsa::input]` - Marks this as an input to the compiler pipeline
/// * `#[return_ref]` - Indicates fields return references rather than clones
///
/// # Example
///
/// ```rust,ignore
/// let file = SourceFile::new(db, PathBuf::from("main.px"), "fn main() {}".to_string());
/// 
/// // Reading content via the salsa database
/// let content = file.contents(db);
/// 
/// // Updating content, which will trigger recompilation of dependent queries
/// file.set_contents(db).to("fn main() { println!(\"Hello\"); }".to_string());
/// ```
#[salsa::tracked]
pub struct SourceFile<'db> {
    /// Path to the source file (without the plx extension)
    #[tracked]
    pub location: String,
    
    /// Contents of the source file
    #[return_ref]
    pub contents: String,
}