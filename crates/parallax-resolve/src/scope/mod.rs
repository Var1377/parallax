mod global;
mod local;

pub use global::GlobalScope;
pub use local::LocalScope;

use crate::symbol::Symbol;

#[derive(Debug)]
pub struct Scope {
    parent: Option<Box<Scope>>,
    symbols: Vec<Symbol>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            parent: None,
            symbols: Vec::new(),
        }
    }

    pub fn with_parent(parent: Scope) -> Self {
        Self {
            parent: Some(Box::new(parent)),
            symbols: Vec::new(),
        }
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        todo!("Implement symbol resolution in scope")
    }
} 