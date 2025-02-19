use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use super::types::{Agent, AgentType};

const SHARD_BITS: usize = 10;  // Same as sharded-slab default
const SHARD_SIZE: usize = 1 << SHARD_BITS;
const SHARD_MASK: usize = SHARD_SIZE - 1;

// Each shard manages a chunk of both arrays
struct Shard {
    types: Box<[AgentType]>,
    agents: Box<[Agent]>,
    bitmap: AtomicUsize,  // Tracks which slots are occupied
}

pub struct DualSlab {
    shards: Box<[Option<Arc<Shard>>]>,
    len: AtomicUsize,
}

impl Shard {
    fn new() -> Self {
        Shard {
            types: vec![AgentType::Eraser; SHARD_SIZE].into_boxed_slice(),
            agents: vec![Agent { reference: 0 }; SHARD_SIZE].into_boxed_slice(),
            bitmap: AtomicUsize::new(0),
        }
    }

    fn allocate(&self) -> Option<usize> {
        let mut bits = self.bitmap.load(Ordering::Relaxed);
        loop {
            // Find first free slot
            if bits == !0 {
                return None;  // Shard is full
            }
            let slot = bits.trailing_ones() as usize;
            let new_bits = bits | (1 << slot);
            
            match self.bitmap.compare_exchange_weak(
                bits,
                new_bits,
                Ordering::Release,
                Ordering::Relaxed,
            ) {
                Ok(_) => return Some(slot),
                Err(actual) => bits = actual,
            }
        }
    }
}

impl DualSlab {
    pub fn new() -> Self {
        DualSlab {
            shards: vec![None; 32].into_boxed_slice(), // Start with 32 possible shards
            len: AtomicUsize::new(0),
        }
    }

    pub fn insert(&self, agent_type: AgentType, agent: Agent) -> Option<usize> {
        let mut shard_idx = 0;
        loop {
            // Get or create shard
            let shard = match &self.shards[shard_idx] {
                Some(shard) => Arc::clone(shard),
                None => {
                    // Try to atomically insert new shard
                    let new_shard = Arc::new(Shard::new());
                    match self.shards[shard_idx].get_or_insert_with(|| Arc::clone(&new_shard)) {
                        existing => Arc::clone(existing),
                    }
                }
            };

            // Try to allocate in this shard
            if let Some(slot) = shard.allocate() {
                // Store the data
                unsafe {
                    // SAFETY: We own this slot via the bitmap
                    let types = &*shard.types.as_ptr();
                    let agents = &*shard.agents.as_ptr();
                    std::ptr::write(types.as_ptr().add(slot) as *mut _, agent_type);
                    std::ptr::write(agents.as_ptr().add(slot) as *mut _, agent);
                }
                
                self.len.fetch_add(1, Ordering::Relaxed);
                return Some((shard_idx << SHARD_BITS) | slot);
            }
2
            // Try next shard
            shard_idx += 1;
            if shard_idx >= self.shards.len() {
                return None;  // All shards full
            }
        }
    }

    pub fn get(&self, idx: usize) -> Option<(&AgentType, &Agent)> {
        let shard_idx = idx >> SHARD_BITS;
        let slot = idx & SHARD_MASK;

        // Get shard
        let shard = self.shards[shard_idx].as_ref()?;
        
        // Check if slot is allocated
        let bits = shard.bitmap.load(Ordering::Acquire);
        if (bits & (1 << slot)) == 0 {
            return None;
        }

        // SAFETY: Slot is allocated and we've synchronized via Acquire load
        unsafe {
            Some((
                &*shard.types.as_ptr().add(slot),
                &*shard.agents.as_ptr().add(slot)
            ))
        }
    }

    pub fn remove(&self, idx: usize) -> bool {
        let shard_idx = idx >> SHARD_BITS;
        let slot = idx & SHARD_MASK;

        let Some(shard) = &self.shards[shard_idx] else {
            return false;
        };

        let bits = shard.bitmap.load(Ordering::Relaxed);
        if (bits & (1 << slot)) == 0 {
            return false;
        }

        // Clear the bit atomically
        shard.bitmap.fetch_and(!(1 << slot), Ordering::Release);
        self.len.fetch_sub(1, Ordering::Relaxed);
        true
    }

    pub fn len(&self) -> usize {
        self.len.load(Ordering::Relaxed)
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
} 