use crate::option::Option;

pub enum Ordering {
    Less,
    Equal,
    Greater,
}

pub trait Ord {
    fn cmp(self, other: Self) -> Ordering;
}

pub trait PartialOrd {
    fn partial_cmp(self, other: Self) -> Option<Ordering>;

    fn lt(self, other: Self) -> bool = match self.partial_cmp(other) {
        Some(Ordering::Less) => true,
        _ => false,
    };

    fn le(self, other: Self) -> bool = match self.partial_cmp(other) {
        Some(Ordering::Less) => true,
        Some(Ordering::Equal) => true,
        _ => false,
    };

    fn gt(self, other: Self) -> bool = match self.partial_cmp(other) {
        Some(Ordering::Greater) => true,
        _ => false,
    };

    fn ge(self, other: Self) -> bool = match self.partial_cmp(other) {
        Some(Ordering::Less) => false,
        Some(Ordering::Equal) => true,
        Some(Ordering::Greater) => true,
    };
}

impl<T> PartialOrd for T where T: Ord {
    fn partial_cmp(self, other: Self) -> Option<Ordering> = match self.cmp(other) {
        Ordering::Less => Some(Ordering::Less),
        Ordering::Equal => Some(Ordering::Equal),
        Ordering::Greater => Some(Ordering::Greater),
    };
}