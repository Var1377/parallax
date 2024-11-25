// Before memory optimization and before parallelization


pub type Port = usize;

#[derive(Debug, Clone)]
pub enum NodeInner {
    // Variable { ... },
    // Reference,
    Eraser,
    Constructor {
        first: Port,
        second: Port,
    },
    Duplicator{
        first: Port,
        second: Port,
    },
    // Number,
    // Operator,
    // Switch,
}

#[derive(Debug, Clone)]
pub struct Node {
    principal: Port,
    inner: NodeInner,
}