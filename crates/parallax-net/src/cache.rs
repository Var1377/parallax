//! Cache optimization utilities.
//!
//! This module provides centralized cache-related functionality:
//! - Cache line size and alignment constants
//! - Prefetching strategies
//! - Cache-aware data structures
//! - Cache pressure monitoring

use std::sync::atomic::AtomicU32;
use hwlocality::object::types::ObjectType;
use std::sync::Arc;
use crate::numa::TOPOLOGY;
/// Cache line size in bytes - aligned with hardware architecture
pub const CACHE_LINE_SIZE: usize = 64;

/// Prefetch distances for different cache levels (in cache lines)
pub const PREFETCH_L1_DISTANCE: usize = 2;
pub const PREFETCH_L2_DISTANCE: usize = 8;
pub const PREFETCH_L3_DISTANCE: usize = 16;

/// Cache pressure thresholds
pub const PRESSURE_LOW: f64 = 0.7;
pub const PRESSURE_MEDIUM: f64 = 0.8;
pub const PRESSURE_HIGH: f64 = 0.9;

/// Cache-aware data structure marker trait
pub trait CacheAligned {
    /// Returns the cache line alignment requirement
    fn cache_alignment() -> usize {
        CACHE_LINE_SIZE
    }
}

/// Cache pressure monitoring for adaptive strategies
#[derive(Debug)]
pub struct CachePressureMonitor {
    /// Access counter for pressure calculation
    accesses: AtomicU32,
    /// Miss counter for pressure calculation
    misses: AtomicU32,
}

impl CachePressureMonitor {
    pub fn new() -> Self {
        Self {
            accesses: AtomicU32::new(0),
            misses: AtomicU32::new(0),
        }
    }

    pub fn record_access(&self, is_miss: bool) {
        self.accesses.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        if is_miss {
            self.misses.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        }
    }

    pub fn get_pressure(&self) -> f64 {
        let accesses = self.accesses.load(std::sync::atomic::Ordering::Relaxed);
        let misses = self.misses.load(std::sync::atomic::Ordering::Relaxed);
        if accesses == 0 {
            return 0.0;
        }
        misses as f64 / accesses as f64
    }

    pub fn reset(&self) {
        self.accesses.store(0, std::sync::atomic::Ordering::Relaxed);
        self.misses.store(0, std::sync::atomic::Ordering::Relaxed);
    }
}

/// Prefetch data into cache with architecture-specific optimizations
#[cfg(target_arch = "x86_64")]
pub fn prefetch_data(ptr: *const u8, size: usize) {
    use std::arch::x86_64::*;
    let base = ptr as usize;
    
    unsafe {
        // L1 cache prefetch
        for i in 0..PREFETCH_L1_DISTANCE {
            let addr = base + i * CACHE_LINE_SIZE;
            if addr < base + size {
                _mm_prefetch(addr as *const i8, _MM_HINT_T0);
            }
        }

        // L2 cache prefetch
        for i in PREFETCH_L1_DISTANCE..PREFETCH_L2_DISTANCE {
            let addr = base + i * CACHE_LINE_SIZE;
            if addr < base + size {
                _mm_prefetch(addr as *const i8, _MM_HINT_T1);
            }
        }

        // L3 cache prefetch
        for i in PREFETCH_L2_DISTANCE..PREFETCH_L3_DISTANCE {
            let addr = base + i * CACHE_LINE_SIZE;
            if addr < base + size {
                _mm_prefetch(addr as *const i8, _MM_HINT_T2);
            }
        }
    }
}

#[cfg(target_arch = "aarch64")]
pub fn prefetch_data(ptr: *const u8, size: usize) {
    use std::arch::aarch64::*;
    let base = ptr as usize;
    
    unsafe {
        for i in 0..PREFETCH_L3_DISTANCE {
            let addr = base + i * CACHE_LINE_SIZE;
            if addr < base + size {
                __prefetch(addr as *const i8);
            }
        }
    }
}

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub fn prefetch_data(_ptr: *const u8, _size: usize) {
    // No-op for unsupported architectures
}

/// Gets cache information for the current system
pub fn get_cache_info() -> CacheInfo {
    let topology = Arc::clone(&TOPOLOGY);
    let mut info = CacheInfo::default();

    // Get L3 cache information
    if let Some(l3) = topology.objects_with_type(ObjectType::L3Cache).next() {
        if let Some(hwlocality::object::attributes::ObjectAttributes::Cache(cache_attr)) = l3.attributes() {
            info.l3_size = cache_attr.size().map(|s| s.get());
            info.l3_line_size = cache_attr.line_size().map(|s| s.get());
        }
    }

    // Get L2 cache information
    if let Some(l2) = topology.objects_with_type(ObjectType::L2Cache).next() {
        if let Some(hwlocality::object::attributes::ObjectAttributes::Cache(cache_attr)) = l2.attributes() {
            info.l2_size = cache_attr.size().map(|s| s.get());
            info.l2_line_size = cache_attr.line_size().map(|s| s.get());
        }
    }

    // Get L1 cache information
    if let Some(l1) = topology.objects_with_type(ObjectType::L1Cache).next() {
        if let Some(hwlocality::object::attributes::ObjectAttributes::Cache(cache_attr)) = l1.attributes() {
            info.l1_size = cache_attr.size().map(|s| s.get());
            info.l1_line_size = cache_attr.line_size().map(|s| s.get());
        }
    }

    info
}

/// Cache information for the current system
#[derive(Debug, Default)]
pub struct CacheInfo {
    pub l1_size: Option<u64>,
    pub l2_size: Option<u64>,
    pub l3_size: Option<u64>,
    pub l1_line_size: Option<usize>,
    pub l2_line_size: Option<usize>,
    pub l3_line_size: Option<usize>,
}

/// Calculates optimal batch size based on cache characteristics
pub fn calculate_cache_batch_size(data_size: usize) -> usize {
    let cache_info = get_cache_info();
    
    // Try to fit in L1 cache first
    if let Some(l1_size) = cache_info.l1_size {
        if data_size * PREFETCH_L1_DISTANCE <= l1_size as usize {
            return PREFETCH_L1_DISTANCE;
        }
    }

    // Try L2 cache next
    if let Some(l2_size) = cache_info.l2_size {
        if data_size * PREFETCH_L2_DISTANCE <= l2_size as usize {
            return PREFETCH_L2_DISTANCE;
        }
    }

    // Default to L3 distance
    PREFETCH_L3_DISTANCE
}

/// Helper macro for cache line alignment
#[macro_export]
macro_rules! cache_align {
    ($ty:ty) => {
        #[repr(C, align(64))]
        $ty
    };
}

/// Helper macro for double cache line alignment
#[macro_export]
macro_rules! double_cache_align {
    ($ty:ty) => {
        #[repr(C, align(128))]
        $ty
    };
} 