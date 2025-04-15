fn bubble_sort(lst: List<i32>) -> List<i32> = match lst {
    Nil => Nil,
    Cons(head, tail) => {
        let sorted_tail = bubble_sort(tail);
        if head <= sorted_tail.head then
            Cons(head, sorted_tail)
        else
            Cons(sorted_tail.head, bubble_sort(Cons(head, sorted_tail.tail)))
    }
};

fn main() -> List<i32> = bubble_sort([1,3,2,4,5]);
