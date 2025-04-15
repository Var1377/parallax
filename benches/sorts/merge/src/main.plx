fn merge_sort(lst: List<i32>) -> List<i32> = match lst {
    Nil => Nil,
    Cons(head, tail) => {
        let sorted_tail = merge_sort(tail);
        let merged = merge(head, sorted_tail);
        Cons(merged.head, merge_sort(merged.tail))
    }
};

fn merge(lst1: List<i32>, lst2: List<i32>) -> List<i32> = match (lst1, lst2) {
    (Nil, lst2) => lst2,
    (lst1, Nil) => lst1,
    (Cons(h1, t1), Cons(h2, t2)) => if h1 <= h2 then Cons(h1, merge(t1, lst2)) else Cons(h2, merge(lst1, t2))
};

fn main() -> List<i32> = merge_sort([1,3,2,4,5]);