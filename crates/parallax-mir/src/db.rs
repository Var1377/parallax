//! Query database for MIR.
//!
//! Defines queries related to MIR generation, monomorphization and optimization.

use std::sync::Arc;

use parallax_hir::db::HirDatabase;
use salsa::Database;

use crate::error::MirError;
use crate::ir::function::MirFunction;
use crate::ir::visit::MirCounter;

/// Database trait for MIR queries
#[salsa::query_group(MirDatabaseStorage)]
pub trait MirDatabase: HirDatabase + Database {
    /// Lower HIR functions to MIR
    #[salsa::invoke(crate::lower::lower_function)]
    fn mir_function(&self, hir_function_id: u32) -> Result<Arc<MirFunction>, MirError>;

    /// Count MIR items in a function
    #[salsa::invoke(crate::lower::count_mir)]
    fn mir_count(&self, hir_function_id: u32) -> Result<MirCounter, MirError>;

    /// Monomorphize a function with concrete type arguments
    #[salsa::invoke(crate::mono::monomorphize_function)]
    fn monomorphized_function(
        &self,
        mir_function: Arc<MirFunction>,
        type_args: Vec<Arc<crate::ir::function::MirType>>,
    ) -> Result<Arc<MirFunction>, MirError>;

    /// Optimize a function
    #[salsa::invoke(crate::opt::optimize_function)]
    fn optimized_function(
        &self,
        mir_function: Arc<MirFunction>,
        opt_level: OptimizationLevel,
    ) -> Result<Arc<MirFunction>, MirError>;

    /// Get the standard library functions
    #[salsa::invoke(crate::lower::load_stdlib)]
    fn stdlib_functions(&self) -> Arc<Vec<Arc<MirFunction>>>;
}

/// Optimization level for MIR
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OptimizationLevel {
    /// No optimizations
    None,
    /// Basic optimizations (constant folding, dead code elimination)
    Basic,
    /// Full optimizations
    Full,
}

/// Helper functions that may be useful for other modules
impl dyn MirDatabase {
    /// Lower a HIR function to MIR, and then optimize it
    pub fn lower_and_optimize(
        &self,
        hir_function_id: u32,
        opt_level: OptimizationLevel,
    ) -> Result<Arc<MirFunction>, MirError> {
        let mir_function = self.mir_function(hir_function_id)?;
        self.optimized_function(mir_function, opt_level)
    }

    /// Lower a HIR function to MIR, monomorphize it with concrete type arguments, and then optimize it
    pub fn lower_monomorphize_and_optimize(
        &self,
        hir_function_id: u32,
        type_args: Vec<Arc<crate::ir::function::MirType>>,
        opt_level: OptimizationLevel,
    ) -> Result<Arc<MirFunction>, MirError> {
        let mir_function = self.mir_function(hir_function_id)?;
        let monomorphized = self.monomorphized_function(mir_function, type_args)?;
        self.optimized_function(monomorphized, opt_level)
    }
} 