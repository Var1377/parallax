pub trait Add {
    fn add(self, other: Self) -> Self;
}

pub trait Sub {
    fn sub(self, other: Self) -> Self;
}

pub trait Mul {
    fn mul(self, other: Self) -> Self;
}

pub trait Div {
    fn div(self, other: Self) -> Self;
}

pub trait Rem {
    fn rem(self, other: Self) -> Self;
}

pub trait Neg {
    fn neg(self) -> Self;
}

pub trait And {
    fn and(self, other: Self) -> Self;
}

pub trait Or {
    fn or(self, other: Self) -> Self;
}

pub trait Xor {
    fn xor(self, other: Self) -> Self;
}

pub trait Not {
    fn not(self) -> Self;
}

pub trait ShiftLeft {
    fn shl(self, other: Self) -> Self;
}

pub trait ShiftRight {
    fn shr(self, other: Self) -> Self;
}