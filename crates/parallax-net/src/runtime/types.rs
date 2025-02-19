use super::port::Port;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum AgentType {
    /// data (64 bit)
    Reference, 
    /// data (64 bit)
    Number,
    /// 2 aux ports
    Constructor, 
    /// 2 aux ports
    Duplicator, 
    /// nothing
    Eraser, 
    /// 2 aux ports
    Switch, 
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Reference(pub u32);

impl Reference {
    pub fn id(&self) -> u32 {
        self.0 & 0x7FFFFFFF
    }

    pub fn is_native(&self) -> bool {
        (self.0 & 0x80000000) != 0
    }
}

#[derive(Clone)]
pub struct Number(pub u64);

#[derive(Clone)]
pub struct Constructor(pub [Port; 2]);

#[derive(Clone)]
pub struct Duplicator(pub [Port; 2]);

#[derive(Clone)]
pub struct Eraser;

#[derive(Clone)]
pub struct Switch(pub [Port; 2]);

// A redex represents a pair of ports that can interact
#[derive(Debug, Clone, Copy)]
pub struct Redex {
    pub left: Port,
    pub right: Port,
}

// Reduction rules
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ReductionRule {
    Call,   // Function call
    Erase,  // Node erasure
    
    // Computation rules  
    Annihilate, // Constructor-constructor annihilation
    Commute,    // Constructor-duplicator commutation
    Compute,    // Number computation
    
    // Control rules
    Switch,     // Conditional branching
}
