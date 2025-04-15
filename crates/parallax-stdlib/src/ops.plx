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

pub trait BitAnd {
    fn bitand(self, other: Self) -> Self;
}

pub trait BitOr {
    fn bitor(self, other: Self) -> Self;
}

pub trait BitXor {
    fn bitxor(self, other: Self) -> Self;
}

pub trait BitNot {
    fn bitnot(self) -> Self;
}

pub trait BitShiftLeft {
    fn shl(self, other: Self) -> Self;
}

pub trait BitShiftRight {
    fn shr(self, other: Self) -> Self;
}