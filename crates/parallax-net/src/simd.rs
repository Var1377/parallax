//! SIMD-accelerated pattern matching for graph reduction.
//!
//! This module provides optimized SIMD implementations for:
//! - Node type pattern matching
//! - Batch reduction processing
//! - Conflict detection
//!
//! Supports multiple SIMD architectures:
//! - x86_64: AVX-512, AVX2, SSE
//! - AArch64: SVE, NEON
//! - Fallback scalar implementation

#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::*;
#[cfg(target_arch = "aarch64")]
use std::arch::aarch64::*;
use crate::NodeType;
use aligned::{Aligned, A64};
use crate::utils::{PREFETCH_L1_DISTANCE, PREFETCH_L2_DISTANCE, PREFETCH_L3_DISTANCE};

pub const AVX512_WIDTH: usize = 64;  // 512-bit AVX-512
pub const AVX2_WIDTH: usize = 32;    // 256-bit AVX2
pub const SSE_WIDTH: usize = 16;     // 128-bit SSE/NEON
const SIMD_WIDTH: usize = AVX2_WIDTH; // Default to AVX2 width for pattern arrays

/// SIMD-accelerated node type pattern matching
#[repr(C, align(64))]
pub struct NodeTypeMatcher {
    // Pre-computed SIMD vectors for common patterns
    masks: [Aligned<A64, [u8; SIMD_WIDTH]>; 4], // Masks for each pattern type (removed async)
    #[cfg(target_feature = "avx512f")]
    avx512_masks: Aligned<A64, [__m512i; 4]>, // AVX-512 pattern masks
    #[cfg(target_feature = "avx2")]
    avx2_masks: Aligned<A64, [__m256i; 4]>, // AVX2 pattern masks
    #[cfg(target_feature = "avx512f")]
    avx512_conflict_masks: Aligned<A64, [__m512i; 4]>, // Enhanced conflict detection masks
    #[cfg(target_feature = "avx2")]
    avx2_conflict_masks: Aligned<A64, [__m256i; 4]>, // AVX2 conflict detection masks
    #[cfg(target_arch = "aarch64")]
    sve_masks: [Aligned<A64, [u8; SIMD_WIDTH]>; 4], // SVE pattern masks (removed async)
}

impl NodeTypeMatcher {
    pub fn new() -> Self {
        let delta = NodeType::Delta as u8;
        let lambda = NodeType::Lambda as u8;
        let rho = NodeType::Rho as u8;
        let zeta = NodeType::Zeta as u8;

        // Create masks for each pattern type
        let mut delta_lambda_mask = Aligned([0u8; SIMD_WIDTH]);
        let mut delta_rho_mask = Aligned([0u8; SIMD_WIDTH]);
        let mut lambda_rho_mask = Aligned([0u8; SIMD_WIDTH]);
        let mut zeta_zeta_mask = Aligned([0u8; SIMD_WIDTH]);

        // Initialize masks using SIMD_WIDTH
        for i in 0..(SIMD_WIDTH/2) {
            // δλ pattern
            delta_lambda_mask[i*2] = delta;
            delta_lambda_mask[i*2 + 1] = lambda;

            // δρ pattern
            delta_rho_mask[i*2] = delta;
            delta_rho_mask[i*2 + 1] = rho;

            // λρ pattern
            lambda_rho_mask[i*2] = lambda;
            lambda_rho_mask[i*2 + 1] = rho;

            // ζζ pattern
            zeta_zeta_mask[i*2] = zeta;
            zeta_zeta_mask[i*2 + 1] = zeta;
        }

        #[cfg(target_feature = "avx512f")]
        let avx512_masks = unsafe {
            Aligned([
                _mm512_set1_epi8(delta_lambda_mask[0] as i8),
                _mm512_set1_epi8(delta_rho_mask[0] as i8),
                _mm512_set1_epi8(lambda_rho_mask[0] as i8),
                _mm512_set1_epi8(zeta_zeta_mask[0] as i8),
            ])
        };

        #[cfg(target_feature = "avx512f")]
        let avx512_conflict_masks = unsafe {
            Aligned([
                _mm512_set1_epi32(0xFFFFFFFF), // Full mask for redex conflicts
                _mm512_set1_epi32(0x0000FFFF), // Port conflicts mask
                _mm512_set1_epi32(0xFFFF0000), // Node type conflicts mask
                _mm512_set1_epi32(0x0F0F0F0F), // Fine-grained pattern mask
            ])
        };

        #[cfg(target_feature = "avx2")]
        let avx2_masks = unsafe {
            Aligned([
                _mm256_set1_epi8(delta_lambda_mask[0] as i8),
                _mm256_set1_epi8(delta_rho_mask[0] as i8),
                _mm256_set1_epi8(lambda_rho_mask[0] as i8),
                _mm256_set1_epi8(zeta_zeta_mask[0] as i8),
            ])
        };

        #[cfg(target_feature = "avx2")]
        let avx2_conflict_masks = unsafe {
            Aligned([
                _mm256_set1_epi32(0xFFFFFFFF), // Full mask
                _mm256_set1_epi32(0x0000FFFF), // Port conflicts
                _mm256_set1_epi32(0xFFFF0000), // Node type conflicts
                _mm256_set1_epi32(0x0F0F0F0F), // Fine-grained pattern
            ])
        };

        #[cfg(target_arch = "aarch64")]
        let sve_masks = [
            delta_lambda_mask.clone(),
            delta_rho_mask.clone(),
            lambda_rho_mask.clone(),
            zeta_zeta_mask.clone(),
        ];

        Self {
            masks: [
                delta_lambda_mask,
                delta_rho_mask,
                lambda_rho_mask,
                zeta_zeta_mask,
            ],
            #[cfg(target_feature = "avx512f")]
            avx512_masks,
            #[cfg(target_feature = "avx2")]
            avx2_masks,
            #[cfg(target_feature = "avx512f")]
            avx512_conflict_masks,
            #[cfg(target_feature = "avx2")]
            avx2_conflict_masks,
            #[cfg(target_arch = "aarch64")]
            sve_masks,
        }
    }

    /// Prefetches data for upcoming pattern matching
    #[inline(always)]
    pub unsafe fn prefetch_data(&self, ptr: *const u8, size: usize) {
        let base = ptr as usize;
        
        // Calculate optimal prefetch strides based on SIMD width
        #[cfg(target_feature = "avx512f")]
        let stride = AVX512_WIDTH;
        #[cfg(all(target_feature = "avx2", not(target_feature = "avx512f")))]
        let stride = AVX2_WIDTH;
        #[cfg(not(any(target_feature = "avx512f", target_feature = "avx2")))]
        let stride = SSE_WIDTH;

        // Prefetch for L1 cache with SIMD-aligned stride
        for i in 0..PREFETCH_L1_DISTANCE {
            let addr = base + i * stride;
            if addr < base + size {
                #[cfg(target_arch = "x86_64")]
                _mm_prefetch(addr as *const i8, _MM_HINT_T0);
                #[cfg(target_arch = "aarch64")]
                __prefetch(addr as *const i8);
            }
        }

        // Prefetch for L2 cache
        for i in PREFETCH_L1_DISTANCE..PREFETCH_L2_DISTANCE {
            let addr = base + i * stride;
            if addr < base + size {
                #[cfg(target_arch = "x86_64")]
                _mm_prefetch(addr as *const i8, _MM_HINT_T1);
                #[cfg(target_arch = "aarch64")]
                __prefetch(addr as *const i8);
            }
        }

        // Prefetch for L3 cache
        for i in PREFETCH_L2_DISTANCE..PREFETCH_L3_DISTANCE {
            let addr = base + i * stride;
            if addr < base + size {
                #[cfg(target_arch = "x86_64")]
                _mm_prefetch(addr as *const i8, _MM_HINT_T2);
                #[cfg(target_arch = "aarch64")]
                __prefetch(addr as *const i8);
            }
        }
    }

    /// Matches a batch of node types using SIMD instructions.
    /// Returns counts of each pattern type found.
    pub fn match_batch(&self, types: &[u8]) -> MatchResult {
        let mut result = MatchResult::default();

        #[cfg(target_feature = "avx512f")]
        unsafe {
            // Process AVX512_WIDTH bytes at a time with AVX-512
            for chunk in types.chunks_exact(AVX512_WIDTH) {
                let data = _mm512_loadu_si512(chunk.as_ptr() as *const __m512i);
                
                // Match each pattern using AVX-512 instructions
                for (i, mask) in self.avx512_masks.iter().enumerate() {
                    let matches = _mm512_cmpeq_epi8_mask(data, *mask);
                    let count = matches.count_ones();
                    
                    match i {
                        0 => result.delta_lambda_count += count,
                        1 => result.delta_rho_count += count,
                        2 => result.lambda_rho_count += count,
                        3 => result.zeta_zeta_count += count,
                        _ => unreachable!(),
                    }
                }

                // Enhanced conflict detection using AVX-512CD
                let conflicts = _mm512_conflict_epi32(data);
                result.has_conflicts |= !_mm512_testz_si512(conflicts, conflicts);
            }
        }

        #[cfg(all(target_feature = "avx2", not(target_feature = "avx512f")))]
        unsafe {
            // Process AVX2_WIDTH bytes at a time with AVX2
            for chunk in types.chunks_exact(AVX2_WIDTH) {
                let data = _mm256_loadu_si256(chunk.as_ptr() as *const __m256i);
                
                // Match each pattern using AVX2 instructions
                for (i, mask) in self.avx2_masks.iter().enumerate() {
                    let matches = _mm256_cmpeq_epi8(data, *mask);
                    let mask = _mm256_movemask_epi8(matches);
                    let count = mask.count_ones();
                    
                    match i {
                        0 => result.delta_lambda_count += count,
                        1 => result.delta_rho_count += count,
                        2 => result.lambda_rho_count += count,
                        3 => result.zeta_zeta_count += count,
                        _ => unreachable!(),
                    }
                }

                // Conflict detection for AVX2
                let conflicts = _mm256_movemask_epi8(_mm256_cmpeq_epi32(data, _mm256_permute2x128_si256(data, data, 1)));
                result.has_conflicts |= conflicts != 0;
            }
        }

        #[cfg(target_arch = "aarch64")]
        unsafe {
            #[cfg(target_feature = "sve")]
            {
                let vl = core::arch::aarch64::__arm_streaming_sve_vector_length();
                // Dynamically resize masks if needed
                let mut sve_masks = self.sve_masks.clone();
                if vl > SIMD_WIDTH {
                    for mask in &mut sve_masks {
                        // Extend pattern to fill larger SVE vector
                        for i in SIMD_WIDTH..vl {
                            mask[i] = mask[i % SIMD_WIDTH];
                        }
                    }
                }
                
                for chunk in types.chunks_exact(vl) {
                    let data = svld1_u8(chunk.as_ptr());
                    
                    // Match patterns using SVE instructions
                    for (i, mask) in sve_masks.iter().enumerate() {
                        let matches = svcmpeq_u8(svptrue_b8(), data, svdup_n_u8(mask[0]));
                        let count = svptest_any(svptrue_b8(), matches).count_ones();
                        
                        match i {
                            0 => result.delta_lambda_count += count,
                            1 => result.delta_rho_count += count,
                            2 => result.lambda_rho_count += count,
                            3 => result.zeta_zeta_count += count,
                            _ => unreachable!(),
                        }
                    }

                    // SVE conflict detection
                    let conflicts = svdup_n_u8(0);
                    for i in 0..vl/4 {
                        let slice = svld1_u32(chunk.as_ptr().add(i*4) as *const u32);
                        let rotated = svext_u32(slice, slice, 1);
                        let eq = svcmpeq_u32(svptrue_b32(), slice, rotated);
                        if svptest_any(svptrue_b32(), eq) {
                            result.has_conflicts = true;
                            break;
                        }
                    }
                }
            }

            #[cfg(all(target_feature = "neon", not(target_feature = "sve")))]
            {
                // Process SSE_WIDTH bytes at a time with NEON (same width as SSE)
                for chunk in types.chunks_exact(SSE_WIDTH) {
                    let data = vld1q_u8(chunk.as_ptr());
                    
                    // Match patterns using NEON instructions
                    for (i, mask) in self.masks.iter().enumerate() {
                        let matches = vceqq_u8(data, vld1q_u8(mask.as_ptr()));
                        let count = vaddvq_u8(matches);
                        
                        match i {
                            0 => result.delta_lambda_count += count as u32,
                            1 => result.delta_rho_count += count as u32,
                            2 => result.lambda_rho_count += count as u32,
                            3 => result.zeta_zeta_count += count as u32,
                            _ => unreachable!(),
                        }
                    }

                    // NEON conflict detection
                    let rotated = vextq_u32(
                        vreinterpretq_u32_u8(data),
                        vreinterpretq_u32_u8(data),
                        1
                    );
                    let eq = vceqq_u32(vreinterpretq_u32_u8(data), rotated);
                    result.has_conflicts |= vaddvq_u32(eq) != 0;
                }
            }
        }

        // Process remaining elements with scalar code
        for chunk in types.chunks_exact(2) {
            match chunk {
                [0, 1] => result.delta_lambda_count += 1,
                [0, 2] => result.delta_rho_count += 1,
                [1, 2] => result.lambda_rho_count += 1,
                [4, 4] => result.zeta_zeta_count += 1,
                _ => {}
            }
        }

        result
    }
}

/// Result of SIMD pattern matching
#[derive(Debug, Default, Clone, Copy)]
pub struct MatchResult {
    pub delta_lambda_count: u32,
    pub delta_rho_count: u32,
    pub lambda_rho_count: u32,
    pub zeta_zeta_count: u32,
    pub has_conflicts: bool,
}

impl std::ops::AddAssign for MatchResult {
    fn add_assign(&mut self, other: Self) {
        self.delta_lambda_count += other.delta_lambda_count;
        self.delta_rho_count += other.delta_rho_count;
        self.lambda_rho_count += other.lambda_rho_count;
        self.zeta_zeta_count += other.zeta_zeta_count;
        self.has_conflicts |= other.has_conflicts;
    }
}