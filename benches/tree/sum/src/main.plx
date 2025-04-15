pub enum Tree<T> {
    Leaf(T),
    Node(Tree<T>, Tree<T>),
}

fn sum(tree: Tree<i32>) -> i32 = match tree {
    Leaf(value) => value,
    Node(left, right) => sum(left) + sum(right),
};

fn main() -> i32 = sum(Node(Node(Leaf(1), Leaf(2)), Node(Leaf(3), Leaf(4))));
