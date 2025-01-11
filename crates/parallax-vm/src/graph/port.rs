/// A port on a node that can connect to other ports
#[derive(Debug, Clone)]
pub struct Port {
    /// The ID of the node this port is connected to, if any
    connected_to: Option<u32>,
}

impl Port {
    /// Creates a new unconnected port
    pub fn new() -> Self {
        Self { connected_to: None }
    }

    /// Gets the ID of the node this port is connected to, if any
    pub fn connected_to(&self) -> Option<u32> {
        self.connected_to
    }

    /// Connects this port to another node
    pub fn connect(&mut self, node_id: u32) {
        self.connected_to = Some(node_id);
    }

    /// Disconnects this port
    pub fn disconnect(&mut self) {
        self.connected_to = None;
    }
}

impl Default for Port {
    fn default() -> Self {
        Self::new()
    }
}