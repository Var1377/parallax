use super::basic_block::Value;

#[derive(Debug, Clone)]
pub enum Instruction {
    Binary {
        op: BinaryOp,
        left: Value,
        right: Value,
    },
    Unary {
        op: UnaryOp,
        operand: Value,
    },
    Call {
        function: Value,
        arguments: Vec<Value>,
    },
    Load {
        address: Value,
    },
    Store {
        value: Value,
        address: Value,
    },
    Alloca {
        ty: Type,
    },
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    // Add more binary operators
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Neg,
    Not,
    // Add more unary operators
}

#[derive(Debug, Clone)]
pub enum Type {
    Int(usize),    // bit width
    Float(usize),  // bit width
    Pointer(Box<Type>),
    Array(Box<Type>, usize),
    // Add more types
}

impl Instruction {
    pub fn type_of(&self) -> Type {
        todo!("Implement type inference for instructions")
    }
} 