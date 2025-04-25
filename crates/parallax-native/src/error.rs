use thiserror::Error;
use cranelift_codegen::settings::SetError;

/// Errors that can occur during native code generation.
#[derive(Error, Debug)]
pub enum NativeError {
    #[error("Failed during Cranelift code generation: {0}")]
    CraneliftGen(#[from] cranelift_codegen::CodegenError),

    #[error("Failed during module processing: {0}")]
    CraneliftModule(#[from] cranelift_module::ModuleError),

    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),
    // TODO: Add more specific error types as needed

    #[error("Unsupported host architecture or features")]
    UnsupportedHost,

    #[error("Failed to configure Cranelift settings: {0}")]
    SettingsError(#[from] SetError),

    #[error("ISA setup failed: {0}")]
    IsaSetupError(String),

    #[error("Function declaration failed in module")]
    FunctionDeclarationError,

    #[error("Function definition failed in module: {0}")]
    CompilationError(String),

    #[error("Feature not yet implemented: {0}")]
    Unimplemented(String),

    #[error("Layout computation error: {0}")]
    LayoutError(#[from] parallax_gc::layout::LayoutError),

    #[error("Type error during translation: {0}")]
    TypeError(String),
} 