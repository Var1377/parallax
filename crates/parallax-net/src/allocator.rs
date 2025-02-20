//! NUMA-aware node allocation system for the parallel graph reduction engine.
//!
//! This module provides a high-performance, NUMA-aware memory allocation system
//! optimized for graph nodes. The allocator is designed to maximize performance
//! in NUMA architectures through several key mechanisms:
//!
//! # Core Features
//!
//! - **NUMA-Aware Memory Management**:
//!   - Per-core memory regions bound to local NUMA nodes
//!   - Intelligent memory binding with migration support
//!   - Distance-based work stealing for memory allocation
//!
//! - **Cache Optimization**:
//!   - Cache-line aligned allocations (64 bytes)
//!   - Smart prefetching based on memory pressure
//!   - Struct-of-Arrays layout for better cache utilization
//!
//! - **Concurrency Support**:
//!   - Lock-free operations using atomic primitives
//!   - Per-core allocation regions to minimize contention
//!   - Lock-free free lists via SegQueue
//!
//! - **Adaptive Behavior**:
//!   - Dynamic batch sizing based on system load
//!   - Pressure-aware memory operations
//!   - Automatic threshold adjustment
//!
//! # Memory Layout
//!
//! The allocator uses a hierarchical memory organization:
//! ```text
//! System
//! ├── NUMA Node 0
//! │   ├── Core 0 Region
//! │   │   ├── Memory Block (64KB aligned)
//! │   │   └── Free List
//! │   └── Core 1 Region
//! └── NUMA Node 1
//!     └── Core 2 Region
//! ```
//!
//! # Performance Considerations
//!
//! - **Memory Binding**: 
//!   - Allocations are bound to the NUMA node of the requesting core
//!   - Memory migration is supported for load balancing
//!   - Uses hardware topology information for optimal placement
//!
//! - **Pressure Management**:
//!   - Monitors allocation pressure per region
//!   - Implements work stealing based on pressure metrics
//!   - Adaptive thresholds for different system states

use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::alloc::{Layout, alloc, dealloc};
use std::ptr::NonNull;
use hwlocality::cpu::cpuset::CpuSet;
use crate::{NodeType, NodeIndex};
use std::cell::UnsafeCell;
use crossbeam_queue::SegQueue;
use crossbeam::deque::{Stealer, Steal};
use dashmap::DashMap;
use crate::numa;
use std::collections::HashMap;
use crate::cache::{CACHE_LINE_SIZE, prefetch_data};
use crate::utils::{EnhancedBackoff, atomic_update};
use crate::utils::calculate_batch_size;
use crate::utils::BatchSizeParams;

/// Number of nodes allocated per CPU core
const NODES_PER_CORE: usize = 64 * 1024; // 64K nodes per core
/// Threshold at which garbage collection is triggered
const GC_PRESSURE_THRESHOLD: f64 = 0.8;

/// Prefetch distances for different pressure levels (in cache lines)
const PREFETCH_LOW_PRESSURE: usize = 4;    // Default prefetch distance
const PREFETCH_MED_PRESSURE: usize = 8;    // Medium pressure prefetch
const PREFETCH_HIGH_PRESSURE: usize = 16;  // High pressure prefetch

/// Pressure thresholds for different system states
const PRESSURE_LOW_LOAD: f64 = 0.7;      // Threshold when system load is low
const PRESSURE_MED_LOAD: f64 = 0.8;      // Threshold when system load is medium
const PRESSURE_HIGH_LOAD: f64 = 0.9;     // Threshold when system load is high
const LOAD_UPDATE_INTERVAL: u64 = 1000;   // Update interval in milliseconds

/// A cache-line aligned block of memory with NUMA node binding.
/// Provides the fundamental memory management unit for the allocator.
#[repr(C, align(64))]
struct MemoryBlock {
    /// Raw pointer to the allocated memory
    /// Always aligned to cache line boundaries
    ptr: NonNull<u8>,
    /// Memory layout information including size and alignment
    layout: Layout,
    /// NUMA node this block is bound to
    /// Used for topology-aware operations
    numa_node: usize,
}

impl MemoryBlock {
    /// Creates a new memory block bound to the specified NUMA node.
    ///
    /// # Arguments
    /// * `size` - Requested size in bytes
    /// * `numa_node` - NUMA node to bind the memory to
    ///
    /// # Panics
    /// Panics if memory allocation fails or if the layout is invalid
    fn new(size: usize, numa_node: usize) -> Self {
        // Round up to cache line size
        let aligned_size = (size + CACHE_LINE_SIZE - 1) & !(CACHE_LINE_SIZE - 1);
        let layout = Layout::from_size_align(aligned_size, CACHE_LINE_SIZE)
            .expect("Invalid memory layout");

        let ptr = unsafe {
            let ptr = alloc(layout);
            NonNull::new(ptr).expect("Memory allocation failed")
        };

        // Zero the memory
        unsafe {
            std::ptr::write_bytes(ptr.as_ptr(), 0, aligned_size);
        }

        // Bind to NUMA node
        numa::bind_memory(ptr.as_ptr(), layout.size(), numa_node);

        Self { ptr, layout, numa_node }
    }

    /// Gets a pointer to the underlying memory
    pub fn as_ptr(&self) -> *const u8 {
        self.ptr.as_ptr()
    }

    /// Gets a mutable pointer to the underlying memory
    pub fn as_mut_ptr(&mut self) -> *mut u8 {
        self.ptr.as_ptr()
    }

    /// Gets a slice of the underlying memory
    pub fn as_slice(&self) -> &[u8] {
        unsafe {
            std::slice::from_raw_parts(self.ptr.as_ptr(), self.layout.size())
        }
    }

    /// Gets a mutable slice of the underlying memory
    pub fn as_slice_mut(&mut self) -> &mut [u8] {
        unsafe {
            std::slice::from_raw_parts_mut(self.ptr.as_ptr(), self.layout.size())
        }
    }
}

impl Drop for MemoryBlock {
    fn drop(&mut self) {
        unsafe {
            // Unbind memory from NUMA node if needed
            #[cfg(target_os = "linux")]
            if let Ok(nodes) = TOPOLOGY.objects_with_type(ObjectType::NUMANode) {
                if let Some(node) = nodes.get(self.numa_node) {
                    if let Some(nodeset) = node.nodeset() {
                        let _ = TOPOLOGY.set_area_membind(
                            self.ptr.as_ptr() as *mut _,
                            self.layout.size(),
                            nodeset,
                            MemoryBindingPolicy::Default,
                            MemoryBindingFlags::MIGRATE
                        );
                    }
                }
            }

            // Deallocate memory
            dealloc(self.ptr.as_ptr(), self.layout);
        }
    }
}

impl AsRef<[u8]> for MemoryBlock {
    fn as_ref(&self) -> &[u8] {
        self.as_slice()
    }
}

impl AsMut<[u8]> for MemoryBlock {
    fn as_mut(&mut self) -> &mut [u8] {
        self.as_slice_mut()
    }
}

/// Tracks system load and memory pressure thresholds.
/// Adapts allocation behavior based on system state.
#[derive(Debug)]
struct LoadTracker {
    /// Moving average of allocation success rate (scaled by 1000)
    success_rate: AtomicUsize,
    /// Timestamp of last update for decay calculations
    last_update: AtomicUsize,
    /// Current pressure threshold (scaled by 1000)
    pressure_threshold: AtomicUsize,
    /// Number of samples in current window
    sample_count: AtomicUsize,
}

impl LoadTracker {
    fn new() -> Self {
        Self {
            success_rate: AtomicUsize::new(1000), // Start at 100% (scaled by 1000)
            last_update: AtomicUsize::new(0),
            pressure_threshold: AtomicUsize::new((PRESSURE_MED_LOAD * 1000.0) as usize),
            sample_count: AtomicUsize::new(0),
        }
    }

    fn update(&self, success: bool) {
        self.sample_count.fetch_add(1, Ordering::Relaxed);
        let current_time = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_millis() as usize;
        
        // Update success rate with exponential moving average using atomic_update
        atomic_update::<u128, _>(
            unsafe { std::mem::transmute(&self.success_rate) },
            |old_rate| {
                let old_rate = old_rate as usize;
                let new_rate = if success {
                    (old_rate * 9 + 1000) / 10
                } else {
                    (old_rate * 9) / 10
                };
                Some(new_rate as u128)
            },
        );

        // Update pressure threshold periodically
        if current_time - self.last_update.load(Ordering::Relaxed) > LOAD_UPDATE_INTERVAL as usize {
            self.adjust_threshold();
            self.last_update.store(current_time, Ordering::Relaxed);
            self.sample_count.store(0, Ordering::Relaxed);
        }
    }

    fn adjust_threshold(&self) {
        let success_rate = self.success_rate.load(Ordering::Relaxed) as f64 / 1000.0;
        
        let new_threshold = if success_rate > 0.9 {
            // High success rate - can be more aggressive
            (PRESSURE_HIGH_LOAD * 1000.0) as usize
        } else if success_rate < 0.6 {
            // Low success rate - be more conservative
            (PRESSURE_LOW_LOAD * 1000.0) as usize
        } else {
            // Moderate success rate
            (PRESSURE_MED_LOAD * 1000.0) as usize
        };

        atomic_update::<u128, _>(
            unsafe { std::mem::transmute(&self.pressure_threshold) },
            |_| Some(new_threshold as u128),
        );
    }

    fn get_threshold(&self) -> f64 {
        self.pressure_threshold.load(Ordering::Relaxed) as f64 / 1000.0
    }
}

/// A per-thread allocation region with NUMA awareness.
/// Manages a block of memory and maintains a free list for recycling.
#[repr(C, align(64))]
struct Region {
    /// Base index for this region's nodes
    base: usize,
    /// Current allocation watermark
    watermark: AtomicUsize,
    /// Lock-free queue of freed indices
    free_list: SegQueue<usize>,
    /// NUMA node this region is bound to
    numa_node: usize,
    /// Underlying memory block
    memory: MemoryBlock,
    /// Exponential backoff for contention
    backoff: UnsafeCell<EnhancedBackoff>,
    /// CPU set for this NUMA node
    cpuset: Arc<CpuSet>,
    /// Current batch size for allocations
    batch_size: AtomicUsize,
    /// Success rate tracking for batch sizes
    batch_stats: DashMap<usize, (usize, usize)>, // (successes, attempts)
    /// Load tracker for adaptive thresholds
    load_tracker: LoadTracker,
}

impl Region {
    /// Creates a new region for the specified CPU core
    fn new(core_id: usize) -> Self {
        let numa_node = numa::get_node_for_core(core_id);
        let cpuset = numa::get_node_cpuset(numa_node);
        
        let block_size = NODES_PER_CORE * CACHE_LINE_SIZE;
        let mut memory = MemoryBlock::new(block_size, numa_node);
        memory.as_slice_mut().fill(0);

        // Initialize with default batch size
        let batch_size = AtomicUsize::new(CACHE_LINE_SIZE);
        let batch_stats = DashMap::new();

        Self {
            base: core_id * NODES_PER_CORE,
            watermark: AtomicUsize::new(core_id * NODES_PER_CORE),
            free_list: SegQueue::new(),
            numa_node,
            memory,
            backoff: UnsafeCell::new(EnhancedBackoff::for_atomic_ops()),
            cpuset: Arc::new(cpuset),
            batch_size,
            batch_stats,
            load_tracker: LoadTracker::new(),
        }
    }

    /// Updates batch size based on allocation success rate
    fn update_batch_size(&self, success: bool, current_batch: usize) {
        let mut entry = self.batch_stats
            .entry(current_batch)
            .or_insert((0, 0));
        
        if success {
            entry.0 += 1; // Increment successes
        }
        entry.1 += 1; // Increment attempts

        // Only adjust after sufficient samples
        if entry.1 >= 100 {
            let success_rate = entry.0 as f64 / entry.1 as f64;
            
            let params = BatchSizeParams {
                current_size: current_batch,
                success_rate,
                numa_distance: None, // Not relevant for allocator
                memory_pressure: Some(self.get_pressure()),
                min_size: CACHE_LINE_SIZE,
                max_size: CACHE_LINE_SIZE * 8,
            };

            let new_batch = calculate_batch_size(params);

            // Update batch size using atomic_update if changed
            if new_batch != current_batch {
                atomic_update::<u128, _>(
                    unsafe { std::mem::transmute(&self.batch_size) },
                    |_| Some(new_batch as u128),
                );
                // Reset statistics for new batch size
                self.batch_stats.insert(new_batch, (0, 0));
            }

            // Reset current batch statistics
            self.batch_stats.insert(current_batch, (0, 0));
        }
    }

    /// Tries to pop an index from the free list
    fn try_pop_free(&self) -> Option<usize> {
        self.free_list.pop()
    }

    /// Prefetches the next cache line of memory
    fn prefetch_next(&self, current_idx: usize) {
        let base_ptr = self.memory.as_ptr();
        let offset = current_idx * CACHE_LINE_SIZE;
        prefetch_data(unsafe { base_ptr.add(offset) }, CACHE_LINE_SIZE);
    }

    /// Prefetches a range of memory ahead
    fn prefetch_range(&self, start_idx: usize, count: usize) {
        let base_ptr = self.memory.as_ptr();
        let offset = start_idx * CACHE_LINE_SIZE;
        prefetch_data(unsafe { base_ptr.add(offset) }, count * CACHE_LINE_SIZE);
    }

    /// Enhanced prefetching based on current memory pressure
    fn enhanced_prefetch(&self, start_idx: usize, count: usize) {
        // Determine prefetch distance based on memory pressure
        let pressure = self.get_pressure();
        let prefetch_distance = if pressure < 0.3 {
            PREFETCH_LOW_PRESSURE  // Low pressure - aggressive prefetch
        } else if pressure < 0.7 {
            PREFETCH_MED_PRESSURE  // Medium pressure
        } else {
            PREFETCH_HIGH_PRESSURE  // High pressure - conservative prefetch
        };

        // Prefetch the range with the determined distance
        self.prefetch_range(start_idx, count.min(prefetch_distance));
    }

    /// Allocates a new node index with dynamic batch sizing and enhanced prefetching
    fn alloc(&self) -> Option<usize> {
        let mut backoff = EnhancedBackoff::new();
        
        loop {
            // Try to get from free list first
            if let Some(idx) = self.try_pop_free() {
                self.enhanced_prefetch(idx, PREFETCH_LOW_PRESSURE);
                return Some(idx);
            }

            // Try to allocate from watermark using atomic_update
            let result = atomic_update::<u128, _>(
                unsafe { std::mem::transmute(&self.watermark) },
                |current| {
                    let current = current as usize;
                    let next = current + 1;
                    if next * CACHE_LINE_SIZE > self.memory.layout.size() {
                        None // Region is full
                    } else {
                        Some(next as u128)
                    }
                },
            );

            if result {
                let current = self.watermark.load(Ordering::Acquire) - 1;
                self.enhanced_prefetch(current, PREFETCH_LOW_PRESSURE);
                return Some(current);
            }

            if !backoff.snooze() {
                return None;
            }
        }
    }

    /// Frees a node index back to this region
    fn free(&self, idx: usize) {
        if idx >= self.base && idx < self.base + NODES_PER_CORE {
            self.free_list.push(idx);
        }
    }

    /// Calculates NUMA distance to another node
    fn numa_distance(&self, other_node: usize) -> u64 {
        numa::calculate_distance(self.numa_node, other_node)
    }

    /// Returns the current memory pressure (0.0 - 1.0)
    fn get_pressure(&self) -> f64 {
        let free_count = self.free_list.len();
        1.0 - (free_count as f64 / NODES_PER_CORE as f64)
    }

    fn get_adaptive_threshold(&self) -> f64 {
        self.load_tracker.get_threshold()
    }
}

// Implement Send + Sync for Region as it's safe to share
unsafe impl Send for Region {}
unsafe impl Sync for Region {}

/// A NUMA-aware node allocator that manages per-core regions
/// and implements work stealing based on NUMA topology.
pub struct NodeAllocator {
    /// Per-core allocation regions
    regions: Vec<Arc<Region>>,
    /// Global counter for fallback allocation
    global_counter: AtomicUsize,
    /// Number of NUMA nodes in the system
    numa_nodes: usize,
}

impl NodeAllocator {
    /// Creates a new allocator with the specified number of cores and capacity
    pub fn new(num_cores: usize) -> Self {
        let numa_nodes = numa::get_num_nodes();
        numa::initialize_distances();

        let regions = (0..num_cores)
            .map(|i| Arc::new(Region::new(i)))
            .collect();

        Self {
            regions,
            global_counter: AtomicUsize::new(num_cores * NODES_PER_CORE),
            numa_nodes,
        }
    }

    /// Finds the best region to steal from based on NUMA distance
    /// and memory pressure.
    fn find_steal_target(&self, core_id: usize) -> Option<usize> {
        let local_numa = self.regions[core_id].numa_node;
        
        // Get all regions sorted by NUMA distance and pressure
        let mut candidates: Vec<_> = self.regions.iter()
            .enumerate()
            .filter(|&(i, _)| i != core_id)
            .map(|(i, r)| {
                let distance = r.numa_distance(local_numa);
                let pressure = r.get_pressure();
                (distance, pressure, i)
            })
            .collect();

        // Sort by pressure first within each NUMA distance group
        candidates.sort_by(|&(d1, p1, _), &(d2, p2, _)| {
            d1.cmp(&d2).then(p1.partial_cmp(&p2).unwrap())
        });

        // Group by NUMA distance
        let mut distance_groups: HashMap<u64, Vec<(u64, f64, usize)>> = HashMap::new();
        for candidate in candidates.clone() {
            distance_groups.entry(candidate.0)
                .or_default()
                .push(candidate);
        }

        // Try to find a target with acceptable pressure in closest NUMA node first
        let mut distances: Vec<_> = distance_groups.keys().copied().collect();
        distances.sort_unstable();
        
        for distance in distances {
            if let Some(group) = distance_groups.get(&distance) {
                // Find region with lowest pressure in this NUMA distance group
                if let Some(&(_, pressure, idx)) = group.iter()
                    .filter(|&&(_, p, _)| p < GC_PRESSURE_THRESHOLD)
                    .min_by(|a, b| a.1.partial_cmp(&b.1).unwrap()) {
                    return Some(idx);
                }
            }
        }

        // If no region found with acceptable pressure, pick the one with lowest pressure overall
        candidates.into_iter()
            .min_by(|a, b| a.1.partial_cmp(&b.1).unwrap())
            .map(|(_, _, idx)| idx)
    }

    /// Allocates a new node with pressure-based distribution
    pub fn alloc(&self, core_id: usize, node_type: NodeType) -> Option<NodeIndex> {
        // Try local region first
        if let Some(idx) = self.regions[core_id].alloc() {
            return Some(NodeIndex(idx));
        }
        
        let local_region = &self.regions[core_id];
        let local_pressure = local_region.get_pressure();
        let pressure_threshold = local_region.get_adaptive_threshold();
        
        // Under high pressure, try to find a better region
        if local_pressure > pressure_threshold {
            if let Some(steal_target) = self.find_steal_target(core_id) {
                if let Some(idx) = self.regions[steal_target].alloc() {
                    #[cfg(debug_assertions)]
                    eprintln!(
                        "Core {} allocated from region {} under pressure (distance: {}, pressure: {:.2}, threshold: {:.2})",
                        core_id,
                        steal_target,
                        self.regions[steal_target].numa_distance(local_region.memory.numa_node),
                        self.regions[steal_target].get_pressure(),
                        pressure_threshold
                    );
                    return Some(NodeIndex(idx));
                }
            }
        }

        // If still no allocation, try global fallback with pressure awareness
        let mut backoff = EnhancedBackoff::for_atomic_ops();
        loop {
            let mut idx = self.global_counter.fetch_add(CACHE_LINE_SIZE, Ordering::AcqRel);

            // Ensure cache line alignment
            if idx % CACHE_LINE_SIZE != 0 {
                let aligned = (idx + CACHE_LINE_SIZE - 1) & !(CACHE_LINE_SIZE - 1);
                self.global_counter.store(aligned, Ordering::Release);
                idx = aligned;
            }

            // Check pressure at the target region
            let target_region = idx / NODES_PER_CORE;
            if target_region < self.regions.len() {
                let target_pressure = self.regions[target_region].get_pressure();
                if target_pressure > GC_PRESSURE_THRESHOLD {
                    // Back off and try again if target region is under pressure
                    if !backoff.snooze() {
                        break;
                    }
                    continue;
                }
            }

            return Some(NodeIndex(idx));
        }

        None
    }

    /// Frees a node back to its owning region
    ///
    /// # Arguments
    /// * `idx` - Index of the node to free
    pub fn free(&self, idx: NodeIndex) {
        let core_id = idx.0 / NODES_PER_CORE;
        if core_id < self.regions.len() {
            self.regions[core_id].free(idx.0);
        }
    }

    /// Returns the number of NUMA nodes in the system
    #[inline]
    pub fn numa_nodes(&self) -> usize {
        self.numa_nodes
    }

    /// Returns the total capacity of all regions
    #[inline]
    pub fn total_capacity(&self) -> usize {
        usize::MAX // Now effectively unbounded
    }

    /// Gets statistics about memory usage and NUMA distribution
    pub fn get_stats(&self) -> AllocatorStats {
        let mut stats = AllocatorStats {
            total_allocated: 0,
            free_list_sizes: Vec::with_capacity(self.regions.len()),
            numa_distribution: vec![0; self.numa_nodes],
        };

        for region in &self.regions {
            let free_count = region.free_list.len();
            
            let allocated = NODES_PER_CORE - free_count;
            stats.total_allocated += allocated;
            stats.free_list_sizes.push(free_count);
            stats.numa_distribution[region.memory.numa_node] += allocated;
        }

        stats
    }
}

/// Statistics about memory allocation and NUMA distribution
#[derive(Debug, Clone)]
pub struct AllocatorStats {
    /// Total number of allocated nodes
    pub total_allocated: usize,
    /// Size of free lists per region
    pub free_list_sizes: Vec<usize>,
    /// Distribution of allocated nodes across NUMA nodes
    pub numa_distribution: Vec<usize>,
}

fn try_steal_batch(victim: &Stealer<usize>, batch_size: usize) -> Vec<usize> {
    let mut stolen = Vec::with_capacity(batch_size);
    let mut backoff = EnhancedBackoff::for_work_stealing();
    
    while stolen.len() < batch_size && !backoff.is_completed() {
        match victim.steal() {
            Steal::Success(idx) => {
                stolen.push(idx);
            }
            Steal::Empty => break,
            Steal::Retry => {
                backoff.snooze();
                continue;
            }
        }
    }
    stolen
}