use crate::num::*;

pub trait Eq {
    fn eq(self, other: Self) -> bool;
    fn ne(self, other: Self) -> bool = !self.eq(other);
}

pub trait Ord {
    fn lt(self, other: Self) -> bool;
    fn le(self, other: Self) -> bool;
    fn gt(self, other: Self) -> bool;
    fn ge(self, other: Self) -> bool;
}

impl Ord for i32 {
    fn lt(self, other: Self) -> bool = __intrinsic_i32_lt__(self, other);
    fn le(self, other: Self) -> bool = __intrinsic_i32_le__(self, other);
    fn gt(self, other: Self) -> bool = __intrinsic_i32_gt__(self, other);
    fn ge(self, other: Self) -> bool = __intrinsic_i32_ge__(self, other);
}

impl Eq for i32 {
    fn eq(self, other: Self) -> bool = __intrinsic_i32_eq__(self, other);
}
