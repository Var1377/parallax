//! Core NUMA (Non-Uniform Memory Access) functionality.
//!
//! This module provides fundamental NUMA operations used across the codebase:
//! - Basic topology information and mapping
//! - NUMA node distance calculations
//! - Thread and memory binding
//! - CPU set management
//!
//! More specialized NUMA-aware functionality lives in the modules that use it:
//! - Cache optimizations in allocator.rs
//! - Work stealing strategies in work_stealing.rs
//! - Thread scheduling in runtime.rs

use std::sync::Arc;
use hwlocality::{
    cpu::{binding::CpuBindingFlags, cpuset::CpuSet}, 
    memory::binding::{MemoryBindingPolicy, MemoryBindingFlags}, 
    object::{attributes::ObjectAttributes, distance::DistancesKind, types::ObjectType, TopologyObject}, 
    Topology
};
use std::collections::HashMap;
use lazy_static::lazy_static;
use std::slice;

lazy_static! {
    /// Global hardware topology information
    pub(crate) static ref TOPOLOGY: Arc<Topology> = Arc::new(Topology::new().unwrap());
    /// Cache of distances between NUMA nodes
    pub(crate) static ref NUMA_DISTANCES: Arc<Vec<Vec<u64>>> = Arc::new(initialize_distances());
}

/// Initialize the NUMA distance matrix
pub(crate) fn initialize_distances() -> Vec<Vec<u64>> {
    let topology = Arc::clone(&TOPOLOGY);
    
    // Get all NUMA nodes
    let nodes: Vec<_> = topology.objects_with_type(ObjectType::NUMANode).collect();
    let num_nodes = nodes.len();
    
    // Initialize distance matrix with default distances
    let mut distances = vec![vec![40; num_nodes]; num_nodes];
    
    // Try to get distances from OS
    if let Ok(distance_matrix) = (*topology).distances(Some(DistancesKind::FROM_OS)) {
        if let Some(matrix) = distance_matrix.first() {
            // Store all distances in the matrix
            for ((from, to), distance) in matrix.enumerate_distances() {
                distances[from][to] = distance;
            }
        }
    } else {
        // Fallback: Calculate distances based on topology
        for (i, from) in nodes.iter().enumerate() {
            for (j, to) in nodes.iter().enumerate() {
                distances[i][j] = if i == j {
                    10 // Same node
                } else if let Some(common_ancestor) = find_common_ancestor(from, to) {
                    match common_ancestor.object_type() {
                        ObjectType::Package => 20, // Same package
                        ObjectType::Group => 30,   // Same group
                        ObjectType::Machine => 40, // Different NUMA domains
                        _ => 40,
                    }
                } else {
                    40 // Default distance
                };
            }
        }
    }
    
    distances
}

/// Finds the closest common ancestor between two topology objects
fn find_common_ancestor<'a>(obj1: &'a TopologyObject, obj2: &'a TopologyObject) -> Option<&'a TopologyObject> {
    obj1.ancestors()
        .find(|ancestor| {
            let ancestor_ptr = *ancestor as *const TopologyObject;
            obj2.ancestors().any(|other| std::ptr::eq(other as *const TopologyObject, ancestor_ptr))
        })
}

/// Returns the total number of NUMA nodes in the system.
pub fn get_num_nodes() -> usize {
    let topology = Arc::clone(&TOPOLOGY);
    topology.objects_with_type(ObjectType::NUMANode).count()
}

/// Determines which NUMA node a given CPU core belongs to.
///
/// # Arguments
/// * `core_id` - The ID of the CPU core to check
///
/// # Returns
/// The NUMA node ID that contains this core, or 0 if not found
///
/// This function traverses the hardware topology to find the NUMA node
/// that contains the specified CPU core.
pub fn get_node_for_core(core_id: usize) -> usize {
    let topology = Arc::clone(&TOPOLOGY);
    
    let cores: Vec<_> = topology.objects_with_type(ObjectType::Core).collect();
    if let Some(core) = cores.get(core_id) {
        if let Some(node) = core.ancestors().find(|obj| obj.object_type() == ObjectType::NUMANode) {
            return node.os_index().unwrap_or(0);
        }
    }
    0
}

/// Creates a mapping from CPU cores to their containing NUMA nodes.
///
/// # Arguments
/// * `num_cores` - Number of cores to map
///
/// # Returns
/// HashMap mapping core IDs to their NUMA node IDs
pub fn create_core_to_node_mapping(num_cores: usize) -> HashMap<usize, usize> {
    let mut numa_map = HashMap::new();
    let topology = Arc::clone(&TOPOLOGY);

    let cores: Vec<_> = topology.objects_with_type(ObjectType::Core).collect();
    for (i, core) in cores.iter().enumerate().take(num_cores) {
        if let Some(node) = core.ancestors()
            .find(|obj| obj.object_type() == ObjectType::NUMANode) {
            if let Some(node_id) = node.os_index() {
                numa_map.insert(i, node_id);
            }
        }
    }
    numa_map
}

/// Gets the CPU set for a NUMA node with cache
pub fn get_node_cpuset(numa_node: usize) -> CpuSet {
    let topology = Arc::clone(&TOPOLOGY);
    
    let nodes: Vec<_> = topology.objects_with_type(ObjectType::NUMANode).collect();
    if let Some(node) = nodes.get(numa_node) {
        if let Some(cpuset) = node.cpuset() {
            let mut result = CpuSet::new();
            result.copy_from(cpuset);
            return result;
        }
    }
    CpuSet::new()
}

/// Binds memory to a specific NUMA node with optimal flags
pub fn bind_memory(ptr: *const u8, size: usize, numa_node: usize) {
    let topology = Arc::clone(&TOPOLOGY);
    
    let nodes: Vec<_> = topology.objects_with_type(ObjectType::NUMANode).collect();
    if let Some(node) = nodes.get(numa_node) {
        if let Some(nodeset) = node.nodeset() {
            unsafe {
                let memory_slice = slice::from_raw_parts(ptr, size);
                let _ = (*topology).bind_memory_area(
                    memory_slice,
                    &nodeset,
                    MemoryBindingPolicy::Bind,
                    MemoryBindingFlags::MIGRATE | MemoryBindingFlags::STRICT
                );
            }
        }
    }
}

/// Binds the current thread to a specific CPU core with optimal flags
pub fn bind_thread_to_core(core_id: usize) -> bool {
    let topology = Arc::clone(&TOPOLOGY);
    
    let cores: Vec<_> = topology.objects_with_type(ObjectType::Core).collect();
    if let Some(core) = cores.get(core_id) {
        if let Some(cpuset) = core.cpuset() {
            let owned_cpuset = cpuset.to_owned();
            return (*topology).bind_cpu(
                owned_cpuset,
                CpuBindingFlags::STRICT
            ).is_ok();
        }
    }
    false
}

/// Calculates the distance between two NUMA nodes
pub fn calculate_distance(from_node: usize, to_node: usize) -> u64 {
    NUMA_DISTANCES[from_node][to_node]
}

/// Calculate distance between nodes based on topology when no direct distance is available
fn calculate_topology_distance(from: &TopologyObject, to: &TopologyObject) -> u64 {
    // If same node
    if from.os_index() == to.os_index() {
        return 10;
    }

    // If same package
    if let Some(from_package) = from.ancestors().find(|obj| obj.object_type() == ObjectType::Package) {
        if let Some(to_package) = to.ancestors().find(|obj| obj.object_type() == ObjectType::Package) {
            if from_package.os_index() == to_package.os_index() {
                return 20;
            }
        }
    }

    // Different packages
    40
}

/// Gets the optimal batch size based on cache configuration
pub fn get_optimal_batch_size() -> usize {
    let topology = Arc::clone(&TOPOLOGY);
    
    // Try to get L3 cache size first
    if let Some(l3) = topology.objects_with_type(ObjectType::L3Cache).next() {
        if let Some(ObjectAttributes::Cache(cache_attr)) = l3.attributes() {
            if let Some(line_size) = cache_attr.line_size() {
                return line_size.get() * 8;
            }
        }
    }
    
    // Fallback to L2 if L3 not available
    if let Some(l2) = topology.objects_with_type(ObjectType::L2Cache).next() {
        if let Some(ObjectAttributes::Cache(cache_attr)) = l2.attributes() {
            if let Some(line_size) = cache_attr.line_size() {
                return line_size.get() * 4;
            }
        }
    }
    
    64 // Default fallback
}

#[derive(Debug, Default)]
pub struct NumaTopologyInfo {
    pub num_nodes: usize,
    pub node_memory: Vec<u64>,
    pub l3_cache_size: Option<u64>,
    pub cache_line_size: Option<usize>,
}

/// Gets detailed information about the NUMA topology
pub fn get_numa_info() -> NumaTopologyInfo {
    let topology = Arc::clone(&TOPOLOGY);
    let mut info = NumaTopologyInfo::default();
    
    // Get all NUMA nodes
    let nodes: Vec<_> = topology.objects_with_type(ObjectType::NUMANode).collect();
    info.num_nodes = nodes.len();
    
    // Get memory information for each node
    for node in nodes {
        let mem = node.total_memory();
        info.node_memory.push(mem);
    }
    
    // Get cache information
    if let Some(l3) = topology.objects_with_type(ObjectType::L3Cache).next() {
        if let Some(ObjectAttributes::Cache(cache_attr)) = l3.attributes() {
            info.l3_cache_size = cache_attr.size().map(|s| s.get());
            info.cache_line_size = cache_attr.line_size().map(|s| s.get());
        }
    }
    
    info
}

/// Gets the memory size of a NUMA node in bytes
pub fn get_node_memory(node_idx: usize) -> Option<u64> {
    let topology = Arc::clone(&TOPOLOGY);
    let nodes: Vec<_> = topology.objects_with_type(ObjectType::NUMANode).collect();
    
    nodes.get(node_idx).map(|node| node.total_memory())
}