//! Query database for MIR.
//!
//! Defines queries related to MIR generation, monomorphization and optimization.

use std::sync::Arc;

use parallax_hir::HirDatabase;
use salsa::Database;

use crate::error::MirError;
use crate::ir::function::MirFunction;
use crate::ir::visit::MirCounter;

/// Database trait for MIR queries
#[salsa::db]
pub trait MirDatabase: HirDatabase + Database {
    /// Lower HIR functions to MIR
    fn mir_function(&self, hir_function_id: u32) -> Result<Arc<MirFunction>, MirError> where Self: Sized {
        crate::lower::lower_function(self, hir_function_id)
    }
    
    /// Monomorphize a function with concrete type arguments
    fn monomorphized_function(
        &self,
        mir_function: Arc<MirFunction>,
        type_args: Vec<Arc<crate::ir::function::MirType>>,
    ) -> Result<Arc<MirFunction>, MirError> where Self: Sized {
        crate::mono::monomorphize_function(self, mir_function, type_args)
    }

    /// Optimize a function
    fn optimized_function(
        &self,
        mir_function: Arc<MirFunction>,
        opt_level: OptimizationLevel,
    ) -> Result<Arc<MirFunction>, MirError> where Self: Sized {
        crate::opt::optimize_function(self, mir_function, opt_level)
    }

    /// Get the standard library functions
    fn stdlib_functions(&self) -> Arc<Vec<Arc<MirFunction>>> where Self: Sized {
        crate::lower::load_stdlib(self)
    }
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