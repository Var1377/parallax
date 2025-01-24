
/// Represents a connection from one node to another.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Connection {
    /// The unique identifier of the destination node.
    pub destination: usize,
    
    /// The port number on the destination node.
    pub port: usize,
}