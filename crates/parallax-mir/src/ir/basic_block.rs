use super::instruction::Instruction;

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: usize,
    pub instructions: Vec<Instruction>,
    pub terminator: Option<Terminator>,
}

#[derive(Debug, Clone)]
pub enum Terminator {
    Jump {
        target: usize,
    },
    Branch {
        condition: Value,
        true_target: usize,
        false_target: usize,
    },
    Return {
        value: Option<Value>,
    },
}

#[derive(Debug, Clone)]
pub struct Value {
    pub id: usize,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Float,
    Bool,
    Unit,
    // Add more types as needed
}

impl BasicBlock {
    pub fn new(id: usize) -> Self {
        Self {
            id,
            instructions: Vec::new(),
            terminator: None,
        }
    }

    pub fn add_instruction(&mut self, instruction: Instruction) -> Value {
        todo!("Implement instruction addition")
    }

    pub fn set_terminator(&mut self, terminator: Terminator) {
        todo!("Implement terminator setting")
    }
} 