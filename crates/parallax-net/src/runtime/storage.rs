use slab::Slab;
use crate::runtime::types::*;
use crate::runtime::port::current_thread_id;

use super::Port;

pub struct ThreadLocalStorage {
    pub nodes: Slab<Reference>,
    pub nums: Slab<Number>,
    pub constructors: Slab<Constructor>,
    pub duplicators: Slab<Duplicator>,
    pub erasers: Slab<Eraser>,
    pub switches: Slab<Switch>,
}

impl ThreadLocalStorage {
    pub fn new() -> Self {
        Self {
            nodes: Slab::new(),
            nums: Slab::new(),
            constructors: Slab::new(),
            duplicators: Slab::new(),
            erasers: Slab::new(),
            switches: Slab::new(),
        }
    }

    pub fn create_constructor(&mut self, con: Constructor) -> Option<usize> {
        Some(self.constructors.insert(con))
    }

    pub fn clear_constructor(&mut self, idx: usize) -> bool {
        if self.constructors.contains(idx) {
            self.constructors.remove(idx);
            true
        } else {
            false
        }
    }

    pub fn get_constructor(&self, idx: usize) -> Option<Constructor> {
        self.constructors.get(idx).cloned()
    }

    pub fn create_number(&mut self, num: Number) -> Option<usize> {
        Some(self.nums.insert(num))
    }

    pub fn clear_number(&mut self, idx: usize) -> bool {
        if self.nums.contains(idx) {
            self.nums.remove(idx);
            true
        } else {
            false
        }
    }

    pub fn get_number(&self, idx: usize) -> Option<Number> {
        self.nums.get(idx).cloned()
    }

    pub fn create_reference(&mut self, reference: Reference) -> Option<usize> {
        Some(self.nodes.insert(reference))
    }

    pub fn clear_reference(&mut self, idx: usize) -> bool {
        if self.nodes.contains(idx) {
            self.nodes.remove(idx);
            true
        } else {
            false
        }
    }

    pub fn get_reference(&self, idx: usize) -> Option<Reference> {
        self.nodes.get(idx).cloned()
    }

    pub fn create_duplicator(&mut self, dup: Duplicator) -> Option<usize> {
        Some(self.duplicators.insert(dup))
    }

    pub fn clear_duplicator(&mut self, idx: usize) -> bool {
        if self.duplicators.contains(idx) {
            self.duplicators.remove(idx);
            true
        } else {
            false
        }
    }

    pub fn get_duplicator(&self, idx: usize) -> Option<Duplicator> {
        self.duplicators.get(idx).cloned()
    }

    pub fn create_eraser(&mut self, eraser: Eraser) -> Option<usize> {
        Some(self.erasers.insert(eraser))
    }

    pub fn clear_eraser(&mut self, idx: usize) -> bool {
        if self.erasers.contains(idx) {
            self.erasers.remove(idx);
            true
        } else {
            false
        }
    }

    pub fn get_eraser(&self, idx: usize) -> Option<Eraser> {
        self.erasers.get(idx).cloned()
    }

    pub fn create_switch(&mut self, switch: Switch) -> Option<usize> {
        Some(self.switches.insert(switch))
    }

    pub fn clear_switch(&mut self, idx: usize) -> bool {
        if self.switches.contains(idx) {
            self.switches.remove(idx);
            true
        } else {
            false
        }
    }

    pub fn get_switch(&self, idx: usize) -> Option<Switch> {
        self.switches.get(idx).cloned()
    }

    pub fn get_agent(&self, port: Port) -> Option<AgentType> {
        if port.thread_id() as u32 != current_thread_id() {
            return None;
        }
        
        Some(match port.destination_agent_type() {
            x if x == AgentType::Reference as u8 => AgentType::Reference,
            x if x == AgentType::Number as u8 => AgentType::Number,
            x if x == AgentType::Constructor as u8 => AgentType::Constructor,
            x if x == AgentType::Duplicator as u8 => AgentType::Duplicator,
            x if x == AgentType::Eraser as u8 => AgentType::Eraser,
            x if x == AgentType::Switch as u8 => AgentType::Switch,
            _ => return None,
        })
    }

    pub fn connect(&mut self, a: Port, b: Port) {
        let a_idx = a.local_index() as usize;
        let b_idx = b.local_index() as usize;

        match (a.destination_agent_type(), b.destination_agent_type()) {
            (x, y) if x == AgentType::Constructor as u8 && y == AgentType::Constructor as u8 => {
                let port_a = a.destination_port() as usize;
                let port_b = b.destination_port() as usize;
                
                let con_a = Constructor([b, Port(0)]);
                let con_b = Constructor([a, Port(0)]);
                
                let _ = self.create_constructor(con_a);
                let _ = self.create_constructor(con_b);
            }
            _ => {}
        }
    }
} 