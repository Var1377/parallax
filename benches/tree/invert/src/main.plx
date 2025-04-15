pub enum Tree<T> {
    Leaf(T),
    Node(Tree<T>, Tree<T>),
}

fn invert(tree: Tree<i32>) -> Tree<i32> = match tree {
    Leaf(value) => Leaf(value),
    Node(left, right) => Node(invert(right), invert(left)),
};

fn main() -> Tree<i32> = invert(Node(Node(Leaf(1), Leaf(2)), Node(Leaf(3), Leaf(4))));
