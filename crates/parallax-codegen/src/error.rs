use thiserror::Error;
use parallax_native::NativeError;
use parallax_mir::LoweringError as MirLoweringError;
use parallax_net::LoweringError as NetLoweringError;
use parallax_layout::LayoutError;

/// Errors that can occur during the code generation orchestration phase.
#[derive(Error, Debug)]
pub enum CodegenError {
    #[error("Error during layout computation: {0}")]
    Layout(#[from] LayoutError),

    #[error("Error lowering HIR to MIR: {0}")]
    MirLowering(#[from] MirLoweringError),

    #[error("Error lowering MIR to Net: {0}")]
    NetLowering(#[from] NetLoweringError),

    #[error("Error from native backend: {0}")]
    NativeBackend(#[from] NativeError),

    // TODO: Add other error types specific to the orchestration logic
    #[error("Orchestration feature not yet implemented: {0}")]
    Unimplemented(String),
} 