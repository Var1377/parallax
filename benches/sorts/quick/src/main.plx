fn quick_sort(lst: List<i32>) -> List<i32> = match lst {
    List::Nil => List::Nil,
    List::Cons(head, tail) => {
        let pivot = head;
        let less = quick_sort(tail.filter(|x| => x < pivot));
        let greater = quick_sort(tail.filter(|x| => x >= pivot));
        less.append(List::Cons(pivot, greater))
    }
};

fn main() -> List<i32> = quick_sort([1,3,2,4,5]);
