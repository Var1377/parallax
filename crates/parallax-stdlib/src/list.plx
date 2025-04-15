pub enum List<T> {
    Nil,
    Cons(T, List<T>),
}

impl<T> List<T> {
    fn map<U>(self, f: fn(T) -> U) -> List<U> = match self {
        Nil => Nil,
        Cons(head, tail) => Cons(f(head), tail.map(f)),
    };

    fn rev(self) -> List<T> = match self {
        Nil => Nil,
        Cons(head, tail) => tail.rev().append(Cons(head, Nil)),
    };

    fn append(self, other: List<T>) -> List<T> = match self {
        Nil => other,
        Cons(head, tail) => Cons(head, tail.append(other)),
    };

    fn foldr<U>(self, init: U, f: fn(T, U) -> U) -> U = match self {
        Nil => init,
        Cons(head, tail) => f(head, tail.foldr(init, f)),
    };

    fn foldl<U>(self, init: U, f: fn(U, T) -> U) -> U = match self {
        Nil => init,
        Cons(head, tail) => f(tail.foldl(init, f), head),
    };

    // fn filter(self, f: fn(T) -> bool) -> List<T> = match self {
    //     Nil => Nil,
    //     Cons(head, tail) => if f(head) then Cons(head, tail.filter(f)) else tail.filter(f),
    // };  

    fn zip<U>(self, other: List<U>) -> List<(T, U)> = match (self, other) {
        (Nil, _) => Nil,
        (_, Nil) => Nil,
        (Cons(head1, tail1), Cons(head2, tail2)) => Cons((head1, head2), tail1.zip(tail2)),
    };
}
