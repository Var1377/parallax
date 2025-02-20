//! Graph reduction engine implementing interaction net semantics.
//!
//! This module implements the core reduction rules for interaction nets.
//! It provides:
//!
//! - SIMD-accelerated pattern matching
//! - Atomic node transitions
//! - Garbage collection integration
//! - Async node handling
//!
//! The reduction rules follow standard interaction net semantics,
//! with extensions for asynchronous computation and garbage collection.

use crate::{NodeStorage, NodeAllocator, NodeType, NodeIndex, NodeTypeMatcher};
use std::sync::Arc;

/// Result of attempting to reduce a pair of nodes.
/// Represents the three possible outcomes of a reduction step.
#[derive(Debug, Clone)]
pub enum ReductionResult {
    /// Reduction completed successfully, returning the new node
    Complete(NodeIndex),
    /// Node requires async computation before reduction can continue
    NeedsAsync(NodeIndex),
    /// No reduction possible between these nodes
    NoReduction,
}

/// Implements the core reduction rules of the interaction net system.
/// Handles pattern matching, node creation, and garbage collection.
pub struct Reducer {
    /// Storage for all nodes in the system
    storage: Arc<NodeStorage>,
    /// Allocator for creating new nodes
    allocator: Arc<NodeAllocator>,
    /// SIMD pattern matcher for efficient rule matching
    matcher: NodeTypeMatcher,
    /// ID of the CPU core this reducer runs on
    core_id: usize,
}

impl Reducer {
    /// Creates a new reducer instance.
    ///
    /// # Arguments
    /// * `storage` - Shared node storage
    /// * `allocator` - Node allocator
    /// * `core_id` - CPU core ID for NUMA-aware allocation
    pub fn new(storage: Arc<NodeStorage>, allocator: Arc<NodeAllocator>, core_id: usize) -> Self {
        Self {
            storage,
            allocator,
            matcher: NodeTypeMatcher::new(),
            core_id,
        }
    }

    /// Validates that port indices are within bounds.
    /// Port indices must be less than 2^30 to allow for metadata bits.
    fn validate_ports(ports: &[u32; 3]) {
        debug_assert!(ports.iter().all(|&p| p < (1 << 30)), "Port index too large: {:?}", ports);
    }

    /// Processes multiple node pairs in parallel using SIMD.
    ///
    /// # Arguments
    /// * `nodes` - Slice of node pairs to reduce
    ///
    /// # Returns
    /// Vector of reduction results, one for each input pair
    ///
    /// This function uses SIMD instructions to efficiently match reduction
    /// patterns across multiple node pairs simultaneously.
    pub fn reduce_batch(&self, nodes: &[(NodeIndex, NodeIndex)]) -> Vec<ReductionResult> {
        // Pre-allocate result vector
        let mut results = Vec::with_capacity(nodes.len());
        
        // Pre-allocate type buffer to avoid repeated allocations
        let mut type_buffer = Vec::with_capacity(nodes.len() * 2);

        // Process in chunks for better cache utilization
        for chunk in nodes.chunks(32) {
            // Clear buffer for this chunk
            type_buffer.clear();
            
            // Prefetch node data for the next chunk if available
            if let Some(next_chunk) = nodes.chunks(32).nth(1) {
                for &(left, right) in next_chunk {
                    unsafe {
                        // Prefetch left and right nodes
                        self.matcher.prefetch_data(
                            self.storage.get(left.0).map_or(std::ptr::null(), |n| n as *const _ as *const u8),
                            std::mem::size_of::<NodeType>()
                        );
                        self.matcher.prefetch_data(
                            self.storage.get(right.0).map_or(std::ptr::null(), |n| n as *const _ as *const u8),
                            std::mem::size_of::<NodeType>()
                        );
                    }
                }
            }
            
            // Extract types into contiguous array for SIMD processing
            for &(left, right) in chunk {
                // Load types using atomic operations
                if let (Some(left_node), Some(right_node)) = (
                    self.storage.get(left.0),
                    self.storage.get(right.0)
                ) {
                    type_buffer.push(left_node.get_type().as_u8());
                    type_buffer.push(right_node.get_type().as_u8());
                }
            }

            // Use SIMD to find patterns
            let match_result = self.matcher.match_batch(&type_buffer);
            
            // Process results
            for (&(left, right), type_pair) in chunk.iter().zip(type_buffer.chunks_exact(2)) {
                let result = match type_pair {
                    // δ-λ reduction
                    [0, 1] if match_result.delta_lambda_count > 0 => {
                        self.delta_lambda(left, right)
                    }
                    // δ-ρ reduction
                    [0, 2] if match_result.delta_rho_count > 0 => {
                        self.delta_rho(left, right)
                    }
                    // λ-ρ reduction
                    [1, 2] if match_result.lambda_rho_count > 0 => {
                        self.lambda_rho(left, right)
                    }
                    // ζ-ζ reduction
                    [4, 4] if match_result.zeta_zeta_count > 0 => {
                        self.zeta_zeta(left, right)
                    }
                    _ => ReductionResult::NoReduction,
                };
                results.push(result);
            }
        }

        results
    }

    /// Attempts to reduce a single pair of nodes.
    ///
    /// # Arguments
    /// * `left` - Left node of the pair
    /// * `right` - Right node of the pair
    ///
    /// # Returns
    /// Result of the reduction attempt
    pub fn reduce(&self, left: NodeIndex, right: NodeIndex) -> ReductionResult {
        let (left_node, right_node) = match (self.storage.get(left.0), self.storage.get(right.0)) {
            (Some(l), Some(r)) => (l, r),
            _ => return ReductionResult::NoReduction,
        };

        match (left_node.get_type(), right_node.get_type()) {
            // δ-λ interaction (Delta = 0, Lambda = 1)
            (NodeType::Delta, NodeType::Lambda) => self.delta_lambda(left, right),

            // δ-ρ interaction (Delta = 0, Rho = 2)
            (NodeType::Delta, NodeType::Rho) => self.delta_rho(left, right),

            // λ-ρ interaction (Lambda = 1, Rho = 2)
            (NodeType::Lambda, NodeType::Rho) => self.lambda_rho(left, right),

            // ζ-ζ interaction (Zeta = 4)
            (NodeType::Zeta, NodeType::Zeta) => self.zeta_zeta(left, right),

            // Async node interactions (Async = 5)
            (NodeType::Async, _) => self.handle_async(left, right),
            (_, NodeType::Async) => self.handle_async(right, left),

            // No reduction possible
            _ => ReductionResult::NoReduction,
        }
    }

    /// Implements the δ-λ reduction rule.
    /// Creates a new ρ node connected to the auxiliary ports.
    fn delta_lambda(&self, delta: NodeIndex, lambda: NodeIndex) -> ReductionResult {
        // Create new ρ node
        let Some(rho_idx) = self.allocator.alloc(self.core_id, NodeType::Rho) else {
            return ReductionResult::NoReduction;
        };
        let Some(rho) = self.storage.get(rho_idx.0) else {
            return ReductionResult::NoReduction;
        };

        // Get source nodes
        let (Some(delta_node), Some(lambda_node)) = (
            self.storage.get(delta.0),
            self.storage.get(lambda.0)
        ) else {
            return ReductionResult::NoReduction;
        };

        // Get current ports
        let delta_ports = delta_node.get_ports();
        let lambda_ports = lambda_node.get_ports();

        // Validate port indices
        Self::validate_ports(&delta_ports);
        Self::validate_ports(&lambda_ports);

        // Connect aux1 to delta's aux1, aux2 to lambda's aux1
        let new_ports = [delta_ports[1], lambda_ports[1], 0];
        Self::validate_ports(&new_ports);

        // Attempt atomic transition
        if !rho.atomic_transition(
            NodeType::Rho,
            NodeType::Rho,
            [0, 0, 0],
            new_ports
        ) {
            return ReductionResult::NoReduction;
        }

        // Create epsilon nodes for garbage collection
        let Some(eps1) = self.allocator.alloc(self.core_id, NodeType::Epsilon) else {
            return ReductionResult::NoReduction;
        };
        let Some(eps2) = self.allocator.alloc(self.core_id, NodeType::Epsilon) else {
            return ReductionResult::NoReduction;
        };

        // Connect epsilon nodes to the original nodes
        if let (Some(eps1_node), Some(eps2_node)) = (
            self.storage.get(eps1.0),
            self.storage.get(eps2.0)
        ) {
            eps1_node.atomic_transition(
                NodeType::Epsilon,
                NodeType::Epsilon,
                [0, 0, 0],
                [delta.0 as u32, 0, 0]
            );
            eps2_node.atomic_transition(
                NodeType::Epsilon,
                NodeType::Epsilon,
                [0, 0, 0],
                [lambda.0 as u32, 0, 0]
            );
        }

        ReductionResult::Complete(rho_idx)
    }

    /// Implements the δ-ρ reduction rule.
    /// Creates a new λ node connected to the auxiliary ports.
    fn delta_rho(&self, delta: NodeIndex, rho: NodeIndex) -> ReductionResult {
        // Create new λ node
        let Some(lambda_idx) = self.allocator.alloc(self.core_id, NodeType::Lambda) else {
            return ReductionResult::NoReduction;
        };
        let Some(lambda) = self.storage.get(lambda_idx.0) else {
            return ReductionResult::NoReduction;
        };

        // Get source nodes
        let (Some(delta_node), Some(rho_node)) = (
            self.storage.get(delta.0),
            self.storage.get(rho.0)
        ) else {
            return ReductionResult::NoReduction;
        };

        // Get current ports
        let delta_ports = delta_node.get_ports();
        let rho_ports = rho_node.get_ports();

        // Validate port indices
        Self::validate_ports(&delta_ports);
        Self::validate_ports(&rho_ports);

        // Connect aux1 to delta's aux1, aux2 to rho's aux1
        let new_ports = [delta_ports[1], rho_ports[1], 0];
        Self::validate_ports(&new_ports);

        // Attempt atomic transition
        if !lambda.atomic_transition(
            NodeType::Lambda,
            NodeType::Lambda,
            [0, 0, 0],
            new_ports
        ) {
            return ReductionResult::NoReduction;
        }

        // Create epsilon nodes for garbage collection
        let Some(eps1) = self.allocator.alloc(self.core_id, NodeType::Epsilon) else {
            return ReductionResult::NoReduction;
        };
        let Some(eps2) = self.allocator.alloc(self.core_id, NodeType::Epsilon) else {
            return ReductionResult::NoReduction;
        };

        // Connect epsilon nodes to the original nodes
        if let (Some(eps1_node), Some(eps2_node)) = (
            self.storage.get(eps1.0),
            self.storage.get(eps2.0)
        ) {
            eps1_node.atomic_transition(
                NodeType::Epsilon,
                NodeType::Epsilon,
                [0, 0, 0],
                [delta.0 as u32, 0, 0]
            );
            eps2_node.atomic_transition(
                NodeType::Epsilon,
                NodeType::Epsilon,
                [0, 0, 0],
                [rho.0 as u32, 0, 0]
            );
        }

        ReductionResult::Complete(lambda_idx)
    }

    /// Implements the λ-ρ reduction rule.
    /// Creates a new δ node connected to the auxiliary ports.
    fn lambda_rho(&self, lambda: NodeIndex, rho: NodeIndex) -> ReductionResult {
        // Create new δ node
        let Some(delta_idx) = self.allocator.alloc(self.core_id, NodeType::Delta) else {
            return ReductionResult::NoReduction;
        };
        let Some(delta) = self.storage.get(delta_idx.0) else {
            return ReductionResult::NoReduction;
        };

        // Get source nodes
        let (Some(lambda_node), Some(rho_node)) = (
            self.storage.get(lambda.0),
            self.storage.get(rho.0)
        ) else {
            return ReductionResult::NoReduction;
        };

        // Get current ports
        let lambda_ports = lambda_node.get_ports();
        let rho_ports = rho_node.get_ports();

        // Validate port indices
        Self::validate_ports(&lambda_ports);
        Self::validate_ports(&rho_ports);

        // Connect aux1 to lambda's aux1, aux2 to rho's aux1
        let new_ports = [lambda_ports[1], rho_ports[1], 0];
        Self::validate_ports(&new_ports);

        // Attempt atomic transition
        if !delta.atomic_transition(
            NodeType::Delta,
            NodeType::Delta,
            [0, 0, 0],
            new_ports
        ) {
            return ReductionResult::NoReduction;
        }

        // Create epsilon nodes for garbage collection
        let Some(eps1) = self.allocator.alloc(self.core_id, NodeType::Epsilon) else {
            return ReductionResult::NoReduction;
        };
        let Some(eps2) = self.allocator.alloc(self.core_id, NodeType::Epsilon) else {
            return ReductionResult::NoReduction;
        };

        // Connect epsilon nodes to the original nodes
        if let (Some(eps1_node), Some(eps2_node)) = (
            self.storage.get(eps1.0),
            self.storage.get(eps2.0)
        ) {
            eps1_node.atomic_transition(
                NodeType::Epsilon,
                NodeType::Epsilon,
                [0, 0, 0],
                [lambda.0 as u32, 0, 0]
            );
            eps2_node.atomic_transition(
                NodeType::Epsilon,
                NodeType::Epsilon,
                [0, 0, 0],
                [rho.0 as u32, 0, 0]
            );
        }

        ReductionResult::Complete(delta_idx)
    }

    /// Implements the ζ-ζ reduction rule.
    /// Creates two new ζ nodes for duplication.
    ///
    /// The ζ (zeta) node is used for duplicating subgraphs. When two
    /// zeta nodes interact, they create two new zeta nodes that will
    /// duplicate the connected subgraphs.
    fn zeta_zeta(&self, zeta1: NodeIndex, zeta2: NodeIndex) -> ReductionResult {
        // Create two new ζ nodes
        let (Some(new_zeta1_idx), Some(new_zeta2_idx)) = (
            self.allocator.alloc(self.core_id, NodeType::Zeta),
            self.allocator.alloc(self.core_id, NodeType::Zeta)
        ) else {
            return ReductionResult::NoReduction;
        };

        let (Some(new_zeta1), Some(new_zeta2)) = (
            self.storage.get(new_zeta1_idx.0),
            self.storage.get(new_zeta2_idx.0)
        ) else {
            return ReductionResult::NoReduction;
        };

        // Get source nodes
        let (Some(zeta1_node), Some(zeta2_node)) = (
            self.storage.get(zeta1.0),
            self.storage.get(zeta2.0)
        ) else {
            return ReductionResult::NoReduction;
        };

        // Get current ports
        let zeta1_ports = zeta1_node.get_ports();
        let zeta2_ports = zeta2_node.get_ports();

        // Attempt atomic transitions
        if !new_zeta1.atomic_transition(
            NodeType::Zeta,
            NodeType::Zeta,
            [0, 0, 0],
            [zeta1_ports[1], 0, 0]
        ) || !new_zeta2.atomic_transition(
            NodeType::Zeta,
            NodeType::Zeta,
            [0, 0, 0],
            [zeta2_ports[1], 0, 0]
        ) {
            return ReductionResult::NoReduction;
        }

        // Create epsilon nodes for garbage collection
        let Some(eps1) = self.allocator.alloc(self.core_id, NodeType::Epsilon) else {
            return ReductionResult::NoReduction;
        };
        let Some(eps2) = self.allocator.alloc(self.core_id, NodeType::Epsilon) else {
            return ReductionResult::NoReduction;
        };

        // Connect epsilon nodes to the original nodes
        if let (Some(eps1_node), Some(eps2_node)) = (
            self.storage.get(eps1.0),
            self.storage.get(eps2.0)
        ) {
            eps1_node.atomic_transition(
                NodeType::Epsilon,
                NodeType::Epsilon,
                [0, 0, 0],
                [zeta1.0 as u32, 0, 0]
            );
            eps2_node.atomic_transition(
                NodeType::Epsilon,
                NodeType::Epsilon,
                [0, 0, 0],
                [zeta2.0 as u32, 0, 0]
            );
        }

        ReductionResult::Complete(new_zeta1_idx)
    }

    /// Handles interaction with async nodes.
    ///
    /// When an async node interacts with another node:
    /// 1. If computation is not ready, store the interaction for later
    /// 2. If computation is ready, reduce to an interaction net representing the result
    ///
    /// # Arguments
    /// * `async_node` - The async node
    /// * `other` - The node interacting with the async node
    fn handle_async(&self, async_node: NodeIndex, other: NodeIndex) -> ReductionResult {
        let Some(node) = self.storage.get(async_node.0) else {
            return ReductionResult::NoReduction;
        };

        if node.is_async() && node.is_ready() {
            // Get the result value
            let value = node.get_value();
            
            // Create the root node of the result interaction net
            let Some(root_idx) = self.allocator.alloc(self.core_id, NodeType::Delta) else {
                return ReductionResult::NoReduction;
            };
            
            // Get the waiting node's ports
            let Some(other_node) = self.storage.get(other.0) else {
                return ReductionResult::NoReduction;
            };
            let other_ports = other_node.get_ports();

            // Connect the result net to the waiting node
            let Some(root_node) = self.storage.get(root_idx.0) else {
                return ReductionResult::NoReduction;
            };

            // Set up ports for the new root node
            // Port[0]: Principal port (will connect to waiting node)
            // Port[1]: First auxiliary port (will connect to result subgraph)
            // Port[2]: Second auxiliary port (available for further connections)
            if !root_node.atomic_transition(
                NodeType::Delta,
                NodeType::Delta,
                [0, 0, 0],
                [other_ports[0], 0, 0]
            ) {
                return ReductionResult::NoReduction;
            }

            // The async node is consumed by this reduction
            // No need for garbage collection - it's part of the reduction semantics
            
            ReductionResult::Complete(root_idx)
        } else {
            // Store the waiting node's reference in the async node's ports
            // This follows interaction net semantics where nodes can be connected
            // but not yet reduced
            let mut async_ports = node.get_ports();
            async_ports[1] = other.0 as u32; // Store waiting node in auxiliary port
            Self::validate_ports(&async_ports);
            if !node.set_ports(async_ports) {
                return ReductionResult::NoReduction;
            }

            ReductionResult::NeedsAsync(async_node)
        }
    }

    /// Implements the ε-node reduction rule.
    /// When an epsilon node interacts with any other node, it creates new
    /// epsilon nodes that connect to all auxiliary ports of the other node.
    fn handle_epsilon(&self, epsilon: NodeIndex, other: NodeIndex) -> ReductionResult {
        let (Some(other_node), Some(eps_node)) = (
            self.storage.get(other.0),
            self.storage.get(epsilon.0)
        ) else {
            return ReductionResult::NoReduction;
        };

        // Get ports of the other node
        let other_ports = other_node.get_ports();
        
        // Create new epsilon nodes for each auxiliary port
        let mut new_epsilons = Vec::new();
        for &port in &other_ports[1..] {  // Skip principal port
            if port != 0 {  // Only create epsilon for connected ports
                if let Some(eps_idx) = self.allocator.alloc(self.core_id, NodeType::Epsilon) {
                    if let Some(new_eps) = self.storage.get(eps_idx.0) {
                        // Connect epsilon to the auxiliary port
                        let mut eps_ports = [0; 3];
                        eps_ports[0] = port;  // Connect to auxiliary port
                        if new_eps.atomic_transition(
                            NodeType::Epsilon,
                            NodeType::Epsilon,
                            [0, 0, 0],
                            eps_ports
                        ) {
                            new_epsilons.push(eps_idx);
                        }
                    }
                }
            }
        }

        // The original nodes are erased by the reduction
        ReductionResult::Complete(NodeIndex(0))  // Return null node to indicate erasure
    }
}