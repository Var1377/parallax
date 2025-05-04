use crate::node::NodeType;

/// A Port identifier that refers to a specific node port across all Partitions.
/// 
/// A Port is a 64-bit value that uniquely identifies a node port in the interaction net.
/// It encodes several pieces of information in a compact format for efficient lookup and comparison.
/// 
/// Structure (64 bits):
/// - bits 0-2: Port type (3 bits)
/// - bits 3-7: Node type (5 bits)
/// - bits 8-23: Partition ID (16 bits)
/// - bits 24-63: Node index within Partition (40 bits)
/// 
/// # Port Types
/// - 0: Principal port
/// - 1: Left auxiliary port
/// - 2: Right auxiliary port
/// 
/// # Node Types
/// - 0: Eraser
/// - 1: Constructor
/// - 2: Duplicator
/// - 3: Ref
/// - 4: Number
/// - 5: Switch
/// - 6: Async
/// 
/// # Performance
/// 
/// The Port structure is optimized for:
/// - Fast comparison and hashing
/// - Efficient storage and transmission
/// - Quick access to port and node information
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Port(u64);

/// Represents a connection from one port to another in the interaction net.
/// 
/// This is used during the generation and manipulation of the interaction net
/// before it is converted into the final format for execution.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Connection {
    /// The index of the destination node
    pub destination: usize,
    /// The port index on the destination node
    pub port: usize,
}

impl Port {
    /// Creates a new port with specified fields
    /// 
    /// # Arguments
    /// * `port_type` - The type of port (principal, left, right)
    /// * `node_type` - The type of node this port belongs to
    /// * `Partition_id` - The ID of the Partition containing this port
    /// * `node_index` - The index of the node within its Partition
    #[inline(always)]
    pub fn new(port_idx: PortType, node_type: u8, partition_id: u16, node_index: u64) -> Self {
        let value = ((node_index & 0xFFFFFFFFFF) << 24) | 
                   ((partition_id as u64) << 8) | 
                   ((node_type as u64 & 0x1F) << 3) | 
                   (port_idx as u64 & 0x7);
        Self(value)
    }

    /// Gets the port type (principal, left, right, etc.)
    #[inline(always)]
    pub fn port_type(&self) -> PortType {
        // SAFETY: We know that 0-3 are valid port types
        unsafe { std::mem::transmute((self.0 & 0x7) as u8) }
    }

    /// Gets the node type (constructor, duplicator, etc.)
    #[inline(always)]
    pub fn node_type(&self) -> u8 {
        ((self.0 >> 3) & 0x1F) as u8
    }

    /// Gets the Partition ID that owns this port
    #[inline(always)]
    pub fn partition_id(&self) -> u16 {
        ((self.0 >> 8) & 0xFFFF) as u16
    }

    /// Gets the node index within its Partition
    #[inline(always)]
    pub fn node_index(&self) -> u64 {
        self.0 >> 24
    }

    /// Returns the raw 64-bit value
    #[inline(always)]
    pub fn as_u64(&self) -> u64 {
        self.0
    }

    /// Creates a port from a raw 64-bit value
    #[inline(always)]
    pub fn from_u64(value: u64) -> Self {
        Self(value)
    }

    /// Returns whether this port belongs to the specified Partition
    #[inline(always)]
    pub fn belongs_to_partition(&self, partition_id: u16) -> bool {
        self.partition_id() == partition_id
    }
    
    /// Creates a principal port reference
    #[inline(always)]
    pub fn principal(node_type: NodeType, partition_id: u16, node_index: u64) -> Self {
        Self::new(PortType::Principal, node_type as u8, partition_id, node_index)
    }
    
    /// Creates a left auxiliary port reference
    #[inline(always)]
    pub fn left(node_type: NodeType, partition_id: u16, node_index: u64) -> Self {
        Self::new(PortType::Left, node_type as u8, partition_id, node_index)
    }
    
    /// Creates a right auxiliary port reference
    #[inline(always)]
    pub fn right(node_type: NodeType, partition_id: u16, node_index: u64) -> Self {
        Self::new(PortType::Right, node_type as u8, partition_id, node_index)
    }
    
    /// Special value for null port
    pub const NULL: Port = Port(u64::MAX);
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum PortType {
    Null = 0,
    Principal = 1,
    Left = 2,
    Right = 3,
}