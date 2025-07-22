# Functions

Functions are the primary abstraction mechanism in Parallax. They are first-class values that can be passed around, stored in data structures, and composed together. The language's automatic parallelization works by analyzing function calls to identify independent computations.

## Function Syntax

Parallax uses a consistent arrow syntax for all function definitions:

### Basic Functions

```parallax
// Simple function with explicit types
const add = (x: i32, y: i32) -> i32 => x + y;

// Function with block body
const calculate = (x: i32, y: i32) -> i32 => {
    let sum = x + y;
    let product = x * y;
    sum + product  // Last expression is return value
};

// No return type annotation needed when inferrable
const double = x => x * 2;

// Multiple parameters with mixed inference
const greet = (name: String, times) => {
    (0..times).foreach(_ => println!("Hello, {name}!"))
};
```

The `const` keyword indicates that functions are compile-time constants. This enables optimizations and ensures functions can be used in type-level contexts.

### Generic Functions

Generic functions abstract over types using type parameters:

```parallax
// Single type parameter
const identity = [T](x: T) -> T => x;

// Multiple type parameters
const pair = [A, B](a: A, b: B) -> (A, B) => (a, b);

// Generic with constraints
const clone_it = [T is Clone](x: &T) -> T => x.clone();

// Inferred generics from usage
const first = [T](vec: Vec[T]) -> Option[T] => vec.first();
```

Type parameters are introduced with square brackets before the parameter list. This syntax clearly separates type parameters from value parameters.

### Where Clauses

Complex type constraints use where clauses for readability:

```parallax
// Simple constraint in signature
const simple = [T is Display](x: T) -> String => x.to_string();

// Complex constraints in where clause
const process = [T, U](items: Vec[T], f: T -> U) -> Vec[U]
where 
    T is Clone + Debug,
    U is Default + Send
=> {
    items.iter()
        .map(|item| {
            debug!("Processing: {:?}", item);
            f(item.clone())
        })
        .collect()
};

// Multiple bounds on same type
const serialize = [T](value: T) -> String
where T is Serialize + Debug + Clone
=> {
    // Implementation
};
```

Where clauses improve readability when constraints become complex or when multiple types have constraints.

## Anonymous Functions (Closures)

Anonymous functions capture their environment and can be used inline:

```parallax
// Simple closure
let double = x => x * 2;

// With type annotations
let add: (i32, i32) -> i32 = (x, y) => x + y;

// Multi-line closure
let process = data => {
    let validated = validate(data);
    let transformed = transform(validated);
    serialize(transformed)
};

// Capturing environment
let factor = 10;
let scale = x => x * factor;  // Captures 'factor'

// Explicit parameter types when needed
numbers.map((x: i32) => x.to_string());
```

Closures automatically capture variables from their environment. By default, captures are by reference for Copy types and by value for others.

## Higher-Order Functions

Functions can accept and return other functions:

```parallax
// Function taking function parameter
const apply_twice = [T](f: T -> T, x: T) -> T => 
    f(f(x));

// Function returning function
const make_adder = (n: i32) -> (i32 -> i32) => 
    x => x + n;

// Currying
const curry = [A, B, C](f: (A, B) -> C) -> A -> B -> C =>
    a => b => f(a, b);

// Function composition
const compose = [A, B, C](f: B -> C, g: A -> B) -> A -> C =>
    x => f(g(x));

// Usage
let add5 = make_adder(5);
let result = add5(10);  // 15

let double_then_add1 = compose(x => x + 1, x => x * 2);
let result = double_then_add1(5);  // 11
```

Higher-order functions enable powerful abstraction patterns and are fundamental to functional programming in Parallax.

## Function Types

Function types describe the signature of functions:

```parallax
// Function type annotations
type Predicate[T] = T -> bool;
type Transformer[A, B] = A -> B;
type BinaryOp[T] = (T, T) -> T;

// Using function types
const filter = [T](items: Vec[T], pred: Predicate[T]) -> Vec[T] =>
    items.filter(pred);

// Functions in data structures
let operations: Vec[BinaryOp[i32]] = @[
    (x, y) => x + y,
    (x, y) => x - y,
    (x, y) => x * y,
];

// Function fields in structs
type Calculator = struct {
    add: BinaryOp[f64],
    multiply: BinaryOp[f64],
    name: String,
};
```

## Recursion

Recursive functions call themselves and are the primary looping mechanism:

```parallax
// Simple recursion
const factorial = n => 
    if n <= 1 then 1 else n * factorial(n - 1);

// Tail recursion (optimized to loop)
const sum_tail = (list: Vec[i32], acc: i32 = 0) -> i32 => {
    match list with
        [] => acc
        | [head, ...tail] => sum_tail(tail, acc + head)
};

// Mutual recursion
const is_even = n => if n == 0 then true else is_odd(n - 1);
const is_odd = n => if n == 0 then false else is_even(n - 1);

// Recursive data structure processing
const tree_sum = tree => match tree with
    Leaf(value) => value
    | Node(left, right) => tree_sum(left) + tree_sum(right);
```

The compiler optimizes tail-recursive functions into loops, ensuring stack safety.

## Automatic Parallelization

Functions that perform independent computations can be automatically parallelized:

```parallax
// Parallel opportunity in divide-and-conquer
const merge_sort = [T is Ord](list: Vec[T]) -> Vec[T] => {
    if list.len() <= 1 then return list;
    
    let (left, right) = list.split_at(list.len() / 2);
    // These recursive calls can execute in parallel
    let sorted_left = merge_sort(left);
    let sorted_right = merge_sort(right);
    
    merge(sorted_left, sorted_right)
};

// Parallel map operations
const process_all = items => 
    items.map(expensive_computation);  // May parallelize

// Explicit parallel annotation
const parallel_sum = list => match list with
    [] => 0
    | [x] => x
    | _ => {
        let (l, r) = list.split_at(list.len() / 2);
        parallel_sum(l) + parallel_sum(r)  // Parallel execution
    };
```

The compiler and runtime work together to identify and exploit parallelization opportunities without explicit annotations.

## Function Attributes

Attributes provide metadata about functions:

```parallax
// Inline hint for performance
#[inline]
const small_function = x => x + 1;

// Always inline
#[inline(always)]
const critical_path = x => x * 2;

// Never inline
#[inline(never)]
const large_function = x => {
    // Complex implementation
};

// Pure function annotation (no side effects)
#[pure]
const calculate = (x, y) => x * y + 10;

// Test function
#[test]
const test_addition = () => {
    assert_eq!(add(2, 2), 4);
};
```

## Method Syntax

While Parallax doesn't have traditional methods, the `.with()` pattern and trait implementations provide method-like syntax:

```parallax
// Type with enhanced behavior
type Vector2D = struct { x: f64, y: f64 }
    .with(VectorOps);

const VectorOps = impl for Vector2D {
    const magnitude = (&self) -> f64 => 
        (self.x * self.x + self.y * self.y).sqrt();
    
    const normalize = (&self) -> Vector2D => {
        let mag = self.magnitude();
        Vector2D { x: self.x / mag, y: self.y / mag }
    };
};

// Usage looks like methods
let v = Vector2D { x: 3.0, y: 4.0 };
let mag = v.magnitude();  // 5.0
let unit = v.normalize();
```

This provides familiar object-oriented syntax while maintaining functional principles.

## Best Practices

1. **Prefer small, focused functions** that do one thing well
2. **Use descriptive names** that indicate what the function returns
3. **Leverage type inference** but add annotations for public APIs
4. **Design for composition** - functions that combine easily
5. **Avoid side effects** in most functions to enable parallelization
6. **Use where clauses** for complex type constraints
7. **Document public functions** with examples

Functions are the building blocks of Parallax programs. Master them to write elegant, efficient, and automatically parallel code.