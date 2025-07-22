# Collections

Parallax provides built-in syntax for three fundamental collection types: vectors, maps, and sets. These collections are designed to be efficient, composable, and work seamlessly with the language's automatic parallelization features.

## Vectors

Vectors are dynamically-sized arrays that can grow or shrink as needed. They maintain elements in insertion order and provide O(1) indexed access.

### Vector Literals

The `@[...]` syntax creates vector literals:

```parallax
// Basic vector creation
let numbers = @[1, 2, 3, 4, 5];
let strings = @["hello", "world"];
let mixed = @[1, "two", 3.0];  // Error: heterogeneous types

// Empty vector - type must be inferrable
let empty = @[];                    // Type inferred from usage
let typed_empty = @[] : Vec[i32];   // Explicit type annotation

// Nested vectors
let matrix = @[
    @[1, 2, 3],
    @[4, 5, 6],
    @[7, 8, 9]
];
```

### Vector Operations

Vectors support a rich set of functional operations:

```parallax
let vec = @[1, 2, 3, 4, 5];

// Transformation
let doubled = vec.map(x => x * 2);           // @[2, 4, 6, 8, 10]
let strings = vec.map(x => x.to_string());   // @["1", "2", "3", "4", "5"]

// Filtering
let evens = vec.filter(x => x % 2 == 0);     // @[2, 4]
let odds = vec.filter(x => x % 2 != 0);      // @[1, 3, 5]

// Reduction
let sum = vec.fold(0, +);                    // 15
let product = vec.fold(1, *);                // 120
let max = vec.reduce(max).unwrap_or(0);      // 5

// Access
let first = vec.first();                      // Some(1)
let last = vec.last();                        // Some(5)
let third = vec.get(2);                       // Some(3)
let tenth = vec.get(9);                       // None

// Slicing
let slice = vec[1..4];                        // @[2, 3, 4]
let tail = vec[1..];                          // @[2, 3, 4, 5]
let init = vec[..4];                          // @[1, 2, 3, 4]
```

### Vector Comprehensions

While Parallax doesn't have special comprehension syntax, iterator chains provide equivalent functionality:

```parallax
// Traditional comprehension: [x * 2 for x in range(10) if x % 2 == 0]
let evens_doubled = (0..10)
    .filter(x => x % 2 == 0)
    .map(x => x * 2)
    .collect[Vec[_]]();

// Cartesian product
let pairs = (0..3).flat_map(x => 
    (0..3).map(y => (x, y))
).collect[Vec[_]]();
// @[(0,0), (0,1), (0,2), (1,0), (1,1), (1,2), (2,0), (2,1), (2,2)]
```

## Maps

Maps (hash maps) store key-value pairs with O(1) average-case lookup. The `%{...}` syntax creates map literals:

### Map Literals

```parallax
// Basic map creation
let ages = %{
    "Alice" => 30,
    "Bob" => 25,
    "Carol" => 35,
};

// Empty map
let empty = %{} : Map[String, i32];

// Maps can have any hashable key type
let id_map = %{
    1 => "User One",
    2 => "User Two",
    3 => "User Three",
};

// Computed keys
let key = "dynamic";
let dynamic_map = %{
    key => "value",              // Uses variable key
    f"prefix_{key}" => "other",  // Computed key
};
```

### Map Operations

```parallax
let mut scores = %{
    "Alice" => 95,
    "Bob" => 87,
};

// Access
let alice_score = scores.get("Alice");       // Some(95)
let carol_score = scores.get("Carol");       // None
let bob_score = scores["Bob"];               // 87 (panics if missing)

// Modification (requires mut)
scores.insert("Carol", 92);
scores.remove("Bob");
scores.update("Alice", score => score + 5);

// Safe access with defaults
let score = scores.get("David").unwrap_or(0);
let score = scores.get_or_insert("Eve", 85);

// Iteration
scores.iter().foreach(|(name, score)| {
    println!("{}: {}", name, score);
});

// Transformation
let percentages = scores.map_values(score => score as f64 / 100.0);
let passed = scores.filter(|(_, score)| score >= 70);

// Functional updates (creates new map)
let updated = scores.with("Frank", 90);
let removed = scores.without("Alice");
```

### Map Patterns

Maps are commonly used for configuration and lookup tables:

```parallax
// Configuration
let config = %{
    "host" => "localhost",
    "port" => "8080",
    "debug" => "true",
};

// Command dispatch
let commands = %{
    "help" => show_help,
    "quit" => exit_program,
    "save" => save_file,
};

// Memoization
let mut cache = %{} : Map[i32, i32];
const fib = n => cache.get(n).unwrap_or_else(|| {
    let result = if n <= 1 then n else fib(n-1) + fib(n-2);
    cache.insert(n, result);
    result
});
```

## Sets

Sets store unique values with O(1) average-case membership testing. The `#{...}` syntax creates set literals:

### Set Literals

```parallax
// Basic set creation
let numbers = #{1, 2, 3, 4, 5};
let unique = #{1, 1, 2, 2, 3};  // #{1, 2, 3} - duplicates removed

// Empty set
let empty = #{} : Set[i32];

// From iterator
let squares = (1..=5).map(x => x * x).collect[Set[_]]();
// #{1, 4, 9, 16, 25}
```

### Set Operations

Sets support standard mathematical set operations:

```parallax
let a = #{1, 2, 3, 4};
let b = #{3, 4, 5, 6};

// Set operations
let union = a | b;          // #{1, 2, 3, 4, 5, 6}
let intersection = a & b;   // #{3, 4}
let difference = a - b;     // #{1, 2}
let symmetric_diff = a ^ b; // #{1, 2, 5, 6}

// Membership testing
let contains_3 = 3 in a;    // true
let contains_7 = 7 in a;    // false

// Subset/superset testing
let subset = #{1, 2} <= a;  // true
let superset = a >= #{1};   // true
let disjoint = a.is_disjoint(#{7, 8, 9}); // true

// Iteration
a.iter().foreach(x => println!("{}", x));

// Filtering and mapping (produces new set)
let evens = a.filter(x => x % 2 == 0);  // #{2, 4}
let doubled = a.map(x => x * 2);        // #{2, 4, 6, 8}
```

### Set Patterns

Sets are ideal for membership testing and deduplication:

```parallax
// Allowed values
let valid_commands = #{"start", "stop", "restart", "status"};
if command in valid_commands then
    execute_command(command)
else
    error("Unknown command");

// Deduplication
let unique_items = items.iter().collect[Set[_]]();
let unique_count = unique_items.len();

// Set-based algorithms
const find_common_elements = [T](lists: Vec[Vec[T]]) -> Set[T] => {
    lists.map(list => list.iter().collect[Set[_]]())
         .reduce((a, b) => a & b)
         .unwrap_or(#{})
};
```

## Collection Conversions

Collections can be converted between types:

```parallax
// Vector to Set (removes duplicates)
let vec = @[1, 2, 2, 3, 3, 3];
let set = vec.iter().collect[Set[_]]();  // #{1, 2, 3}

// Set to Vector (arbitrary order)
let vec = set.iter().collect[Vec[_]]();

// Map to Vector of pairs
let pairs = map.iter().collect[Vec[_]]();

// Vector of pairs to Map
let vec = @[("a", 1), ("b", 2)];
let map = vec.iter().collect[Map[_, _]]();
```

## Performance Characteristics

Understanding collection performance helps choose the right type:

| Operation | Vector | Map | Set |
|-----------|--------|-----|-----|
| Insert at end | O(1)* | O(1)* | O(1)* |
| Insert at start | O(n) | O(1)* | O(1)* |
| Remove | O(n) | O(1)* | O(1)* |
| Contains | O(n) | O(1)* | O(1)* |
| Index access | O(1) | N/A | N/A |
| Iteration | O(n) | O(n) | O(n) |

\* Amortized average case

## Parallel Operations

Collection operations automatically parallelize when beneficial:

```parallax
// These operations may run in parallel
let results = large_vec
    .filter(expensive_predicate)
    .map(expensive_transform)
    .collect[Vec[_]]();

// Parallel fold with associative operation
let sum = huge_vec.parallel_fold(0, +);
```

The runtime decides when to parallelize based on collection size and operation cost.