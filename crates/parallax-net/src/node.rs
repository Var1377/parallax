use std::sync::atomic::{AtomicU128, AtomicU32, AtomicU8, Ordering};
#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::*;
use crate::NodeType;
use std::sync::atomic::AtomicBool;
use std::sync::Once;
use crate::utils::{atomic_update, EnhancedBackoff};
use crate::cache::prefetch_data;

// TSX constants
#[cfg(target_arch = "x86_64")]
const _XBEGIN_STARTED: u32 = !0u32; // -1 as unsigned
#[cfg(target_arch = "x86_64")]
const _XABORT_EXPLICIT: u32 = 1 << 0;
#[cfg(target_arch = "x86_64")]
const _XABORT_RETRY: u32 = 1 << 1;
#[cfg(target_arch = "x86_64")]
const _XABORT_CONFLICT: u32 = 1 << 2;
#[cfg(target_arch = "x86_64")]
const _XABORT_CAPACITY: u32 = 1 << 3;
#[cfg(target_arch = "x86_64")]
const _XABORT_DEBUG: u32 = 1 << 4;
#[cfg(target_arch = "x86_64")]
const _XABORT_NESTED: u32 = 1 << 5;

// TSX support detection
#[cfg(target_arch = "x86_64")]
static TSX_SUPPORTED: AtomicBool = AtomicBool::new(false);
#[cfg(target_arch = "x86_64")]
static TSX_INIT: Once = Once::new();

#[cfg(target_arch = "x86_64")]
fn check_tsx_support() -> bool {
    TSX_INIT.call_once(|| {
        unsafe {
            // Try to execute a TSX transaction
            let status = _xbegin();
            if status == _XBEGIN_STARTED {
                _xend();
                TSX_SUPPORTED.store(true, Ordering::Relaxed);
            }
        }
    });
    TSX_SUPPORTED.load(Ordering::Relaxed)
}

// Bit field offsets and masks
const TYPE_SHIFT: u32 = 0;
const TYPE_MASK: u128 = 0xFF;

const PORT1_SHIFT: u32 = 8;
const PORT2_SHIFT: u32 = 40;
const PORT3_SHIFT: u32 = 72;
const PORT_MASK: u128 = 0xFFFFFFFF;

const VALUE_SHIFT: u32 = 104;
const VALUE_MASK: u128 = 0xFFFFFFFF << VALUE_SHIFT;

const READY_BIT: u128 = 1 << 119;
const ASYNC_BIT: u128 = 1 << 120;
const GC_BIT: u128 = 1 << 121;
const VERSION_SHIFT: u32 = 122;
const VERSION_MASK: u128 = 0x3F << VERSION_SHIFT;

/// Represents a batch of updates to be applied to a node atomically
#[derive(Debug, Clone)]
pub struct NodeUpdate {
    pub new_type: Option<NodeType>,
    pub new_ports: Option<[u32; 3]>,
    pub new_value: Option<u32>,
    pub new_flags: Option<u8>,
}

impl NodeUpdate {
    pub fn apply(&self, mut state: NodeState) -> NodeState {
        if let Some(t) = self.new_type {
            state.type_bits = t.as_u8();
        }
        if let Some(p) = self.new_ports {
            state.ports = p;
        }
        if let Some(v) = self.new_value {
            state.value = v;
        }
        if let Some(f) = self.new_flags {
            state.flags = f;
        }
        state.version = (state.version.wrapping_add(1)) & 0x3F;
        state
    }
}

#[derive(Debug, Clone, Copy)]
pub struct NodeState {
    type_bits: u8,
    ports: [u32; 3],
    value: u32,
    flags: u8,
    version: u8,
}

/// Cache-line aligned node with atomic 128-bit state
#[repr(C, align(64))]
pub struct Node {
    state: AtomicU128,
}

/// Cache-line optimized group of nodes
#[repr(C, align(128))]
pub struct SuperNode {
    nodes: [Node; 2],
    prefetch_hint: AtomicU8,
    next_prefetch: AtomicU8,
}

impl SuperNode {
    pub fn new() -> Self {
        Self {
            nodes: [
                Node::new(NodeType::Delta),
                Node::new(NodeType::Delta),
            ],
            prefetch_hint: AtomicU8::new(0),
            next_prefetch: AtomicU8::new(0),
        }
    }

    #[cfg(target_arch = "x86_64")]
    pub fn prefetch(&self) {
        prefetch_data(self as *const _ as *const u8, std::mem::size_of::<SuperNode>());
        
        // Prefetch next supernode if hint suggests
        let next = self.next_prefetch.load(Ordering::Relaxed);
        if next > 0 {
            unsafe {
                let next_ptr = (self as *const _ as *const u8).add(std::mem::size_of::<SuperNode>());
                prefetch_data(next_ptr, std::mem::size_of::<SuperNode>());
            }
        }
    }

    pub fn prefetch_next(&self, likely_next: bool) {
        self.next_prefetch.store(if likely_next { 1 } else { 0 }, Ordering::Relaxed);
    }

    pub fn get_node(&self, index: usize) -> &Node {
        &self.nodes[index]
    }
}

impl Node {
    pub fn new(node_type: NodeType) -> Self {
        Self {
            state: AtomicU128::new(Self::encode_state(NodeState {
                type_bits: node_type.as_u8(),
                ports: [0; 3],
                value: 0,
                flags: 0,
                version: 0,
            })),
        }
    }

    fn encode_state(state: NodeState) -> u128 {
        let mut encoded: u128 = 0;
        encoded |= (state.type_bits as u128) << TYPE_SHIFT;
        encoded |= (state.ports[0] as u128) << PORT1_SHIFT;
        encoded |= (state.ports[1] as u128) << PORT2_SHIFT;
        encoded |= (state.ports[2] as u128) << PORT3_SHIFT;
        encoded |= (state.value as u128) << VALUE_SHIFT;
        if state.flags & 1 != 0 { encoded |= READY_BIT; }
        if state.flags & 2 != 0 { encoded |= ASYNC_BIT; }
        if state.flags & 4 != 0 { encoded |= GC_BIT; }
        encoded |= ((state.version as u128) & 0x3F) << VERSION_SHIFT;
        encoded
    }

    fn decode_state(encoded: u128) -> NodeState {
        NodeState {
            type_bits: ((encoded >> TYPE_SHIFT) & TYPE_MASK) as u8,
            ports: [
                ((encoded >> PORT1_SHIFT) & PORT_MASK) as u32,
                ((encoded >> PORT2_SHIFT) & PORT_MASK) as u32,
                ((encoded >> PORT3_SHIFT) & PORT_MASK) as u32,
            ],
            value: ((encoded >> VALUE_SHIFT) & VALUE_MASK) as u32,
            flags: (((encoded & READY_BIT != 0) as u8) |
                   (((encoded & ASYNC_BIT != 0) as u8) << 1) |
                   (((encoded & GC_BIT != 0) as u8) << 2)),
            version: ((encoded & VERSION_MASK) >> VERSION_SHIFT) as u8,
        }
    }

    pub fn atomic_transition(
        &self,
        old_type: NodeType,
        new_type: NodeType,
        old_ports: [u32; 3],
        new_ports: [u32; 3],
    ) -> bool {
        atomic_update::<u128, _>(
            &self.state,
            |current| {
                let current_state = Self::decode_state(current);
                if current_state.type_bits == old_type.as_u8() &&
                   current_state.ports == old_ports {
                    let mut new_state = current_state;
                    new_state.type_bits = new_type.as_u8();
                    new_state.ports = new_ports;
                    new_state.version = new_state.version.wrapping_add(1);
                    Some(Self::encode_state(new_state))
                } else {
                    None
                }
            },
        )
    }

    pub fn get_type(&self) -> NodeType {
        let state = Self::decode_state(self.state.load(Ordering::Acquire));
        NodeType::from_u8(state.type_bits)
    }

    pub fn get_ports(&self) -> [u32; 3] {
        Self::decode_state(self.state.load(Ordering::Acquire)).ports
    }

    pub fn set_ports(&self, ports: [u32; 3]) -> bool {
        atomic_update::<u128, _>(
            &self.state,
            |current| {
                let mut state = Self::decode_state(current);
                state.ports = ports;
                state.version = state.version.wrapping_add(1);
                Some(Self::encode_state(state))
            },
        )
    }

    pub fn get_value(&self) -> u32 {
        Self::decode_state(self.state.load(Ordering::Acquire)).value
    }

    pub fn set_value(&self, value: u32) -> bool {
        atomic_update::<u128, _>(
            &self.state,
            |current| {
                let mut state = Self::decode_state(current);
                state.value = value;
                state.version = state.version.wrapping_add(1);
                Some(Self::encode_state(state))
            },
        )
    }

    pub fn is_ready(&self) -> bool {
        let state = self.state.load(Ordering::Acquire);
        state & READY_BIT != 0
    }

    pub fn set_ready(&self, ready: bool) -> bool {
        atomic_update::<u128, _>(
            &self.state,
            |current| {
                let mut state = Self::decode_state(current);
                if ready {
                    state.flags |= 1;
                } else {
                    state.flags &= !1;
                }
                state.version = state.version.wrapping_add(1);
                Some(Self::encode_state(state))
            },
        )
    }

    pub fn is_async(&self) -> bool {
        let state = self.state.load(Ordering::Acquire);
        state & ASYNC_BIT != 0
    }

    pub fn set_async(&self, is_async: bool) -> bool {
        atomic_update::<u128, _>(
            &self.state,
            |current| {
                let mut state = Self::decode_state(current);
                if is_async {
                    state.flags |= 2;
                } else {
                    state.flags &= !2;
                }
                state.version = state.version.wrapping_add(1);
                Some(Self::encode_state(state))
            },
        )
    }

    pub fn is_garbage(&self) -> bool {
        let state = self.state.load(Ordering::Acquire);
        state & GC_BIT != 0
    }

    pub fn mark_garbage(&self) -> bool {
        atomic_update::<u128, _>(
            &self.state,
            |current| {
                let mut state = Self::decode_state(current);
                state.flags |= 4;
                state.version = state.version.wrapping_add(1);
                Some(Self::encode_state(state))
            },
        )
    }

    #[cfg(target_arch = "x86_64")]
    #[inline]
    pub fn tsx_atomic_transition(
        &self,
        old_type: NodeType,
        new_type: NodeType,
        old_ports: [u32; 3],
        new_ports: [u32; 3],
    ) -> bool {
        // Check if TSX is supported, fall back to CAS if not
        if !check_tsx_support() {
            return self.atomic_transition(old_type, new_type, old_ports, new_ports);
        }

        let mut backoff = EnhancedBackoff::for_tsx();

        // Try TSX first
        while !backoff.is_completed() {
            unsafe {
                let status: u32 = _xbegin();
                if status == _XBEGIN_STARTED {
                    // Read current state inside transaction
                    let current_state = Self::decode_state(self.state.load(Ordering::Relaxed));
                    
                    // Validate current state
                    if current_state.type_bits != old_type.as_u8() || 
                       current_state.ports != old_ports {
                        _xend();
                        return false;
                    }

                    // Prepare new state with version increment
                    let new_version = (current_state.version.wrapping_add(1)) & 0x3F;
                    let new_state = NodeState {
                        type_bits: new_type.as_u8(),
                        ports: new_ports,
                        value: current_state.value,
                        flags: current_state.flags,
                        version: new_version,
                    };

                    // Update state inside transaction
                    self.state.store(Self::encode_state(new_state), Ordering::Relaxed);
                    _xend();
                    return true;
                }
            }
            backoff.snooze();
        }

        // Fall back to CAS-based transition
        self.atomic_transition(old_type, new_type, old_ports, new_ports)
    }

    #[cfg(target_arch = "x86_64")]
    #[inline]
    pub fn tsx_set_value(&self, value: u32) -> bool {
        if !check_tsx_support() {
            return self.set_value(value);
        }

        let mut backoff = EnhancedBackoff::for_tsx();

        while !backoff.is_completed() {
            unsafe {
                let status = _xbegin();
                if status == _XBEGIN_STARTED {
                    let mut current_state = Self::decode_state(self.state.load(Ordering::Relaxed));
                    current_state.value = value;
                    current_state.version = (current_state.version.wrapping_add(1)) & 0x3F;
                    self.state.store(Self::encode_state(current_state), Ordering::Relaxed);
                    _xend();
                    return true;
                }
            }
            backoff.snooze();
        }

        self.set_value(value)
    }

    #[cfg(target_arch = "x86_64")]
    #[inline]
    pub fn tsx_set_ports(&self, ports: [u32; 3]) -> bool {
        if !check_tsx_support() {
            return self.set_ports(ports);
        }

        let mut backoff = EnhancedBackoff::for_tsx();

        while !backoff.is_completed() {
            unsafe {
                let status = _xbegin();
                if status == _XBEGIN_STARTED {
                    let mut current_state = Self::decode_state(self.state.load(Ordering::Relaxed));
                    current_state.ports = ports;
                    current_state.version = (current_state.version.wrapping_add(1)) & 0x3F;
                    self.state.store(Self::encode_state(current_state), Ordering::Relaxed);
                    _xend();
                    return true;
                }
            }
            backoff.snooze();
        }

        self.set_ports(ports)
    }

    #[cfg(target_arch = "x86_64")]
    #[inline]
    pub fn tsx_batch_update(
        &self,
        new_type: Option<NodeType>,
        new_ports: Option<[u32; 3]>,
        new_value: Option<u32>,
        new_flags: Option<u8>,
    ) -> bool {
        let update = NodeUpdate {
            new_type,
            new_ports,
            new_value,
            new_flags,
        };

        if !check_tsx_support() {
            return self.fallback_batch_update(&update);
        }

        let mut backoff = EnhancedBackoff::for_tsx();

        while !backoff.is_completed() {
            unsafe {
                let status = _xbegin();
                if status == _XBEGIN_STARTED {
                    // Read current state inside transaction
                    let current_state = Self::decode_state(self.state.load(Ordering::Relaxed));
                    
                    // Check for version conflicts
                    if self.has_conflicts(current_state.version) {
                        _xabort(0xFF);
                    }
                    
                    // Apply updates atomically
                    let new_state = update.apply(current_state);
                    
                    // Update state inside transaction
                    self.state.store(Self::encode_state(new_state), Ordering::Relaxed);
                    _xend();
                    return true;
                }
                
                // Handle transaction failures
                if (status & _XABORT_CONFLICT) != 0 {
                    // Contention detected, back off and retry
                    backoff.snooze();
                } else if (status & _XABORT_CAPACITY) != 0 {
                    // Transaction too large, fall back immediately
                    break;
                } else if (status & _XABORT_DEBUG) != 0 {
                    // Debug breakpoint hit
                    continue;
                } else if (status & _XABORT_NESTED) != 0 {
                    // Nested transaction not supported
                    break;
                } else {
                    // Other failure, increment retry counter
                    backoff.snooze();
                }
            }
        }

        // Fall back to CAS-based updates
        self.fallback_batch_update(&update)
    }

    #[cfg(target_arch = "x86_64")]
    #[inline]
    fn fallback_batch_update(&self, update: &NodeUpdate) -> bool {
        let mut success = true;
        let mut backoff = EnhancedBackoff::new();

        // Try CAS-based update with backoff
        loop {
            let current = self.state.load(Ordering::Acquire);
            let current_state = Self::decode_state(current);
            let new_state = update.apply(current_state);

            match self.state.compare_exchange_weak(
                current,
                Self::encode_state(new_state),
                Ordering::AcqRel,
                Ordering::Acquire,
            ) {
                Ok(_) => {
                    std::sync::atomic::fence(Ordering::Release);
                    break;
                }
                Err(_) => {
                    backoff.snooze();
                    if backoff.is_completed() {
                        success = false;
                        break;
                    }
                }
            }
        }

        success
    }

    #[cfg(target_arch = "x86_64")]
    #[inline]
    fn has_conflicts(&self, expected_version: u8) -> bool {
        let current = self.state.load(Ordering::Relaxed);
        let current_version = ((current & VERSION_MASK) >> VERSION_SHIFT) as u8;
        current_version != expected_version
    }
}

/// Node storage using array of supernodes
#[repr(C, align(128))]
pub struct NodeStorage {
    nodes: Box<[SuperNode]>,
    last_accessed: AtomicU32, // Track last accessed node for prefetch hints
}

impl NodeStorage {
    pub fn new(capacity: usize) -> Self {
        let num_supernodes = (capacity + 1) / 2;
        let nodes = (0..num_supernodes)
            .map(|_| SuperNode::new())
            .collect();
        Self { 
            nodes,
            last_accessed: AtomicU32::new(0),
        }
    }

    pub fn get(&self, idx: usize) -> Option<&Node> {
        let supernode_idx = idx / 2;
        let node_idx = idx % 2;

        // Update access pattern tracking
        let last = self.last_accessed.load(Ordering::Relaxed);
        let sequential = last + 1 == idx as u32 || last + 2 == idx as u32;
        self.last_accessed.store(idx as u32, Ordering::Relaxed);

        self.nodes.get(supernode_idx).map(|sn| {
            #[cfg(target_arch = "x86_64")]
            {
                sn.prefetch();
                // If we detect sequential access, prefetch next supernode
                if sequential && supernode_idx + 1 < self.nodes.len() {
                    sn.prefetch_next(true);
                    if let Some(next) = self.nodes.get(supernode_idx + 1) {
                        next.prefetch();
                    }
                } else {
                    sn.prefetch_next(false);
                }
            }
            sn.get_node(node_idx)
        })
    }

    pub fn capacity(&self) -> usize {
        self.nodes.len() * 2
    }
}