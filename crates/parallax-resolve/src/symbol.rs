use std::collections::HashMap;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub id: usize,
}

#[derive(Debug, Default)]
pub struct SymbolTable {
    symbols: HashMap<String, Symbol>,
    next_id: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            next_id: 0,
        }
    }

    pub fn insert(&mut self, name: String) -> Symbol {
        todo!("Implement symbol insertion")
    }

    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        todo!("Implement symbol lookup")
    }
} 