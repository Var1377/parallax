#![allow(dead_code, unused_imports, unused_mut, unused_variables)]

//! Parallax-Net: A Parallel Interaction Net Runtime
//! 
//! This crate provides a high-performance runtime for executing interaction nets in parallel.
//! It implements a work-stealing scheduler that efficiently distributes reduction work across
//! multiple threads while maintaining thread safety and performance.
//! 
//! # Architecture
//! 
//! The runtime is built around several key components:
//! 
//! - [`Runtime`]: The central coordinator that manages workers and partitions
//! - [`Partition`]: A collection of nodes that can be processed by a single worker
//! - [`Worker`]: A thread that processes partitions and their redexes
//! - [`Port`]: A unique identifier for node ports in the interaction net
//! 
//! # Thread Safety
//! 
//! The system uses several mechanisms to ensure thread safety:
//! - `RwLock` for partition access
//! - `UnsafeCell` for worker state (with careful access management)
//! - Atomic operations for coordination
//! - Concurrent queues for communication between threads
//! 
//! # Memory Management
//! 
//! - Uses `Slab` for efficient node storage
//! - Implements garbage collection through erase queues
//! - Maintains good cache locality through partition ownership
//! 
//! # Usage
//! 
//! ```rust
//! use parallax_net::{Runtime, Partition};
//! use std::sync::Arc;
//! 
//! // Create and start the runtime
//! let runtime = Runtime::new();
//! Runtime::start(runtime);
//! 
//! // Create a partition and add it to the runtime
//! let partition = Arc::new(Partition::new());
//! let partition_id = runtime.insert_partition(partition);
//! 
//! // The runtime will automatically distribute work across workers
//! // and manage the reduction process
//! ```
//! 
//! # Performance Considerations
//! 
//! The runtime is optimized for:
//! - Efficient work distribution across threads
//! - Cache-friendly memory access patterns
//! - Lock-free algorithms where possible
//! - Minimized contention through partition isolation
//! 
//! # Safety
//! 
//! The system uses `unsafe` blocks carefully with explicit safety documentation.
//! Access to shared state is controlled through locks and atomic operations.
//! Partition ownership ensures exclusive access to node storage.

// mod node; // Remove private declaration
// mod partition; // Keep private for now, Partition struct is re-exported
// mod port; // Remove private declaration
mod lowering;
pub mod encoding;

use std::collections::HashMap;

// pub use node::{NodeType, Redex}; // Now re-exported below
// pub use port::Port; // Now re-exported below
pub use lowering::{InitialNetConfig, LoweringError, lower_module};

// Make node and port modules public
pub mod node;
pub mod port;

// Re-export key types for convenience
pub use node::{NodeType, Wire, Constructor, Duplicator, Eraser, Static, Number, Switch, Async, Pointer};
pub use parallax_hir::Symbol;
pub use port::Port;

#[derive(Debug)]
pub struct CompiledNet {
    /// The lowered interaction net configuration for each function.
    pub networks: HashMap<Symbol, InitialNetConfig>,
}