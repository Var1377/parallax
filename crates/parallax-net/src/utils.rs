use crossbeam::utils::Backoff;
use std::sync::atomic::{AtomicU128, Ordering};
use dashmap::DashMap;
use crate::cache::CACHE_LINE_SIZE;

/// Prefetch distances for different cache levels
pub const PREFETCH_L1_DISTANCE: usize = 2;  // L1 cache prefetch (in cache lines)
pub const PREFETCH_L2_DISTANCE: usize = 8;  // L2 cache prefetch (in cache lines)
pub const PREFETCH_L3_DISTANCE: usize = 16; // L3 cache prefetch (in cache lines)

/// Helper for atomic operations with exponential backoff
pub fn atomic_update<T, F>(
    atomic: &AtomicU128,
    mut update_fn: F,
) -> bool 
where
    F: FnMut(u128) -> Option<u128>,
{
    let mut backoff = EnhancedBackoff::for_atomic_ops();
    let mut current = atomic.load(Ordering::Acquire);

    while !backoff.is_completed() {
        if let Some(new_value) = update_fn(current) {
            match atomic.compare_exchange_weak(
                current,
                new_value,
                Ordering::AcqRel,
                Ordering::Acquire,
            ) {
                Ok(_) => {
                    std::sync::atomic::fence(Ordering::Release);
                    return true;
                }
                Err(e) => {
                    current = e;
                    if !backoff.snooze() {
                        return false;
                    }
                }
            }
        } else {
            return false;
        }
    }
    false
}

/// Prefetch data for different cache levels
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

/// Enhanced backoff strategy with configurable parameters for different scenarios
#[derive(Debug)]
pub struct EnhancedBackoff {
    /// Inner crossbeam backoff for base functionality
    backoff: Backoff,
    /// Maximum number of retries before giving up
    max_retries: u32,
    /// Current retry count
    current_retries: u32,
    /// Base delay for exponential backoff (in microseconds)
    base_delay: u32,
    /// Maximum delay (in microseconds)
    max_delay: u32,
    /// Whether to use randomized jitter
    use_jitter: bool,
}

impl EnhancedBackoff {
    /// Creates a new backoff with default parameters
    pub fn new() -> Self {
        Self::with_params(10, 1, 1000, true)
    }

    /// Creates a new backoff with custom parameters
    pub fn with_params(max_retries: u32, base_delay_us: u32, max_delay_us: u32, use_jitter: bool) -> Self {
        Self {
            backoff: Backoff::new(),
            max_retries,
            current_retries: 0,
            base_delay: base_delay_us,
            max_delay: max_delay_us,
            use_jitter,
        }
    }

    /// Creates a backoff optimized for TSX retries
    pub fn for_tsx() -> Self {
        Self::with_params(3, 1, 100, false)
    }

    /// Creates a backoff optimized for work stealing
    pub fn for_work_stealing() -> Self {
        Self::with_params(5, 10, 1000, true)
    }

    /// Creates a backoff optimized for atomic operations
    pub fn for_atomic_ops() -> Self {
        Self::with_params(10, 1, 500, true)
    }

    /// Snoozes for the calculated duration
    pub fn snooze(&mut self) -> bool {
        self.current_retries += 1;
        if self.current_retries >= self.max_retries {
            return false;
        }

        // Calculate exponential delay
        let mut delay = self.base_delay * (1 << self.current_retries.min(10));
        delay = delay.min(self.max_delay);

        // Add jitter if enabled
        if self.use_jitter {
            use rand::Rng;
            let mut rng = rand::thread_rng();
            delay = delay / 2 + rng.gen_range(0..=delay / 2);
        }

        // Use crossbeam's backoff for short delays
        if delay < 50 {
            self.backoff.snooze();
        } else {
            std::thread::sleep(std::time::Duration::from_micros(delay as u64));
        }

        true
    }

    /// Resets the backoff state
    pub fn reset(&mut self) {
        self.current_retries = 0;
        self.backoff = Backoff::new();
    }

    /// Returns whether maximum retries have been reached
    pub fn is_completed(&self) -> bool {
        self.current_retries >= self.max_retries
    }

    /// Gets the current retry count
    pub fn retries(&self) -> u32 {
        self.current_retries
    }
}

// Deprecate the old AdaptiveBackoff in favor of EnhancedBackoff
#[deprecated(since = "0.2.0", note = "please use `EnhancedBackoff` instead")]
pub struct AdaptiveBackoff {
    backoff: Backoff,
    max_retries: u32,
    current_retries: u32,
}

/// Batch size calculation parameters
pub struct BatchSizeParams {
    /// Current batch size
    pub current_size: usize,
    /// Success rate (0.0 - 1.0)
    pub success_rate: f64,
    /// NUMA distance (if applicable)
    pub numa_distance: Option<u64>,
    /// Current memory pressure (0.0 - 1.0)
    pub memory_pressure: Option<f64>,
    /// Minimum batch size (usually CACHE_LINE_SIZE)
    pub min_size: usize,
    /// Maximum batch size
    pub max_size: usize,
}

/// Calculates optimal batch size based on various metrics
pub fn calculate_batch_size(params: BatchSizeParams) -> usize {
    let base_adjustment = if params.success_rate > 0.8 {
        // High success rate - try larger batch
        2.0
    } else if params.success_rate < 0.4 {
        // Low success rate - reduce batch size
        0.5
    } else {
        1.0
    };

    // Adjust for NUMA distance if provided
    let numa_factor = params.numa_distance.map(|distance| {
        1.0 / (1.0 + distance as f64 / 100.0)
    }).unwrap_or(1.0);

    // Adjust for memory pressure if provided
    let pressure_factor = params.memory_pressure.map(|pressure| {
        1.0 - (pressure * 0.5) // Reduce batch size under high pressure
    }).unwrap_or(1.0);

    // Calculate new size with all factors
    let new_size = (params.current_size as f64 * base_adjustment * numa_factor * pressure_factor) as usize;

    // Align to SIMD width for optimal processing
    #[cfg(target_feature = "avx512f")]
    let alignment = crate::simd::AVX512_WIDTH;
    #[cfg(all(target_feature = "avx2", not(target_feature = "avx512f")))]
    let alignment = crate::simd::AVX2_WIDTH;
    #[cfg(not(any(target_feature = "avx512f", target_feature = "avx2")))]
    let alignment = crate::simd::SSE_WIDTH;

    // Round up to nearest SIMD width
    let aligned_size = ((new_size + alignment - 1) / alignment) * alignment;
    
    // Clamp to min/max bounds
    aligned_size.clamp(params.min_size, params.max_size)
} 