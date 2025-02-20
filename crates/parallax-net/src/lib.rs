//! Parallax-net: A high-performance parallel graph reduction engine
//! 
//! This crate implements a parallel graph reduction system optimized for NUMA architectures.
//! It provides efficient node allocation, work stealing, and SIMD-accelerated pattern matching
//! for interaction nets.
//!
//! # Key Features
//! 
//! - NUMA-aware memory allocation and work distribution
//! - Lock-free concurrent data structures
//! - SIMD-accelerated pattern matching
//! - Asynchronous computation support
//!
//! # Architecture
//!
//! The system is built around several key components:
//!
//! - Node storage and allocation (`NodeData`, `NodeAllocator`)
//! - Pattern matching and reduction (`Reducer`)
//! - Work stealing scheduler (`Scheduler`)
//! - Async runtime support (`AsyncRuntime`)

#![feature(portable_simd)]
#![feature(integer_atomics)]
#![feature(stdarch_x86_rtm)]

mod allocator;
mod async_node;
mod cache;  // Add cache module
mod node;
pub mod numa;  // Make the module public
mod reduce;
mod runtime;
mod simd;
mod utils;  // Add utils module
mod work_stealing;

use std::sync::atomic::{AtomicU64, AtomicU32, AtomicU8, Ordering};
use std::sync::Arc;
use parking_lot::Mutex;

pub use allocator::NodeAllocator;
pub use async_node::{AsyncRuntime, AsyncState};
pub use cache::{CacheInfo, CachePressureMonitor, CACHE_LINE_SIZE, prefetch_data};  // Export cache utilities
pub use node::{Node, NodeStorage, NodeState};
pub use reduce::{Reducer, ReductionResult};
pub use runtime::{Config as RuntimeConfig, Runtime, WorkItem};
pub use simd::{NodeTypeMatcher, MatchResult};
pub use utils::{EnhancedBackoff, atomic_update};  // Update utils exports
pub use work_stealing::{Scheduler, WorkQueue};

/// Size of memory chunks used for cache-line optimization
const CHUNK_SIZE: usize = 64 * 1024; // 64KB chunks for L1 cache optimization

/// Represents the different types of nodes in the interaction system.
/// Each node type has specific interaction rules with other types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum NodeType {
    /// δ (Delta) - Control flow routing node
    /// Used for directing computation flow in the graph
    Delta = 0,
    
    /// λ (Lambda) - Function abstraction node
    /// Represents function definitions and closures
    Lambda = 1,
    
    /// ρ (Rho) - Function application node
    /// Handles function calls and parameter passing
    Rho = 2,
    
    /// ε (Epsilon) - Garbage collection node
    /// Marks nodes for cleanup and memory reclamation
    Epsilon = 3,
    
    /// ζ (Zeta) - Duplication node
    /// Handles copying and sharing of subgraphs
    Zeta = 4,
    
    /// Async - Node representing pending asynchronous computation
    /// Used for handling non-blocking operations
    Async = 5,
    
    /// Num - Immutable numeric value node
    /// Represents constant numeric values in the graph
    Num = 6,
}

impl NodeType {
    /// Converts a raw u8 value into a NodeType.
    /// Returns Delta as default for invalid values.
    pub fn from_u8(value: u8) -> Self {
        match value {
            0 => NodeType::Delta,
            1 => NodeType::Lambda,
            2 => NodeType::Rho,
            3 => NodeType::Epsilon,
            4 => NodeType::Zeta,
            5 => NodeType::Async,
            6 => NodeType::Num,
            _ => NodeType::Delta, // Default to Delta for invalid values
        }
    }

    /// Converts the NodeType to its raw u8 representation
    pub fn as_u8(&self) -> u8 {
        *self as u8
    }
}

/// Cache-aligned node storage using a struct of arrays (SoA) layout.
/// This layout optimizes for SIMD operations and cache efficiency.
#[repr(C, align(64))]
pub struct NodeData {
    /// Array of atomic node types
    types: Box<[AtomicU8]>,
    /// Array of port connections (3 ports per node)
    ports: Box<[Vec<AtomicU32>]>,
    /// Array of node values
    values: Box<[AtomicU64]>,
    /// Array of node states
    states: Box<[AtomicU64]>,
    /// Total capacity of the storage
    capacity: usize,
}

impl NodeData {
    /// Creates a new NodeData instance with the specified capacity.
    /// All nodes are initialized with zero values.
    pub fn new(capacity: usize) -> Self {
        Self {
            types: (0..capacity).map(|_| AtomicU8::new(0)).collect(),
            ports: (0..capacity)
                .map(|_| (0..3).map(|_| AtomicU32::new(0)).collect())
                .collect(),
            values: (0..capacity).map(|_| AtomicU64::new(0)).collect(),
            states: (0..capacity).map(|_| AtomicU64::new(0)).collect(),
            capacity,
        }
    }

    /// Gets the type of node at the specified index
    pub fn get_type(&self, idx: usize) -> NodeType {
        NodeType::from_u8(self.types[idx].load(Ordering::Acquire))
    }

    /// Sets the type of node at the specified index
    pub fn set_type(&self, idx: usize, value: NodeType) {
        self.types[idx].store(value.as_u8(), Ordering::Release);
    }

    /// Gets the port connection at the specified node and port index
    pub fn get_port(&self, idx: usize, port: usize) -> u32 {
        self.ports[idx][port].load(Ordering::Acquire)
    }

    /// Sets the port connection at the specified node and port index
    pub fn set_port(&self, idx: usize, port: usize, value: u32) {
        self.ports[idx][port].store(value, Ordering::Release);
    }

    /// Gets the value stored in the node at the specified index
    pub fn get_value(&self, idx: usize) -> u64 {
        self.values[idx].load(Ordering::Acquire)
    }

    /// Sets the value stored in the node at the specified index
    pub fn set_value(&self, idx: usize, value: u64) {
        self.values[idx].store(value, Ordering::Release);
    }

    /// Gets the state of the node at the specified index
    pub fn get_state(&self, idx: usize) -> u64 {
        self.states[idx].load(Ordering::Acquire)
    }

    /// Sets the state of the node at the specified index
    pub fn set_state(&self, idx: usize, value: u64) {
        self.states[idx].store(value, Ordering::Release);
    }

    /// Returns the total capacity of the storage
    pub fn capacity(&self) -> usize {
        self.capacity
    }
}

// Implement Send + Sync for NodeData as it's safe to share across threads
unsafe impl Send for NodeData {}
unsafe impl Sync for NodeData {}

/// Index into the node storage.
/// This is a newtype wrapper around usize for type safety.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NodeIndex(pub(crate) usize);

/// Main entry point for the parallel graph reduction runtime.
/// Coordinates node storage, allocation, async operations, and reduction.
pub struct Net {
    /// Storage for all nodes in the graph
    node_storage: Arc<NodeStorage>,
    /// Allocator for creating new nodes
    node_allocator: Arc<NodeAllocator>,
    /// Runtime for handling async operations
    async_runtime: Arc<AsyncRuntime>,
    /// Core reduction runtime
    runtime: Arc<Mutex<Runtime>>,
}

impl Net {
    /// Creates a new Net instance with the specified configuration
    pub fn new(config: NetConfig) -> Self {
        let runtime_config = RuntimeConfig {
            num_threads: config.num_threads,
            chunk_size: config.chunk_size,
        };

        let node_storage = Arc::new(NodeStorage::new(config.capacity));
        let node_allocator = Arc::new(NodeAllocator::new(
            config.num_threads,
        ));

        Self {
            node_storage,
            node_allocator,
            async_runtime: Arc::new(AsyncRuntime::new()),
            runtime: Arc::new(Mutex::new(Runtime::new(runtime_config))),
        }
    }

    /// Spawns an async computation associated with a node
    pub fn spawn_async<F>(&self, future: F, node: NodeIndex)
    where
        F: std::future::Future<Output = u64> + Send + 'static,
    {
        self.async_runtime.spawn(future, node);
    }

    /// Submits a pair of nodes for reduction
    pub fn submit_reduction(&self, left: NodeIndex, right: NodeIndex) {
        self.runtime.lock().submit(WorkItem { left, right });
    }

    /// Starts the reduction runtime
    pub fn run(&self) {
        self.runtime.lock().run();
    }
}

/// Configuration for the Net runtime
#[derive(Debug, Clone)]
pub struct NetConfig {
    /// Number of worker threads to spawn
    pub num_threads: usize,
    /// Size of memory chunks for allocation
    pub chunk_size: usize,
    /// Total capacity for node storage
    pub capacity: usize,
}

impl Default for NetConfig {
    fn default() -> Self {
        Self {
            num_threads: std::thread::available_parallelism()
                .map(|n| n.get())
                .unwrap_or(1),
            chunk_size: CHUNK_SIZE,
            capacity: 1024 * 1024, // Default to 1M nodes
        }
    }
}
