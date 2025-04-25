pub enum Option<T> {
    Some(T),
    None,
}


impl<T> Option<T> {
    fn map<U>(self, f: fn(T) -> U) -> Option<U> = match self {
        Some(value) => Some(f(value)),
        None => None,
    };

    fn and_then<U>(self, f: fn(T) -> Option<U>) -> Option<U> = match self {
        Some(value) => f(value),
        None => None,
    };

    fn or_else(self, f: fn() -> Option<T>) -> Option<T> = match self {
        Some(value) => Some(value),
        None => f(),
    };

    fn is_some(self) -> bool = match self {
        Some(_) => true,
        None => false,
    };

    fn is_none(self) -> bool = match self {
        Some(_) => false,
        None => true,
    };

    fn unwrap(self) -> T = match self {
        Some(value) => value,
        None => panic("Unwrap called on None"),
    };

    fn unwrap_or(self, default: T) -> T = match self {
        Some(value) => value,
        None => default,
    };

    fn unwrap_or_else(self, f: fn() -> T) -> T = match self {
        Some(value) => value,
        None => f(),
    };
}
