# Getting Started with Rayon Parallel Iterators

## Introduction

Rayon's parallel iterators are the easiest way to add parallelism to your Rust programs. They provide a familiar iterator-like interface that automatically distributes work across available CPU cores.

## Basic Concepts

### Sequential vs Parallel Iterators

The key insight of Rayon is that most iterator chains can be parallelized by simply changing the initial iterator:

```rust
use rayon::prelude::*;

// Sequential
let sum: i32 = (0..1_000_000)
    .iter()
    .map(|x| x * x)
    .filter(|&x| x % 2 == 0)
    .sum();

// Parallel - just change .iter() to .par_iter()!
let par_sum: i32 = (0..1_000_000)
    .into_par_iter()
    .map(|x| x * x)
    .filter(|&x| x % 2 == 0)
    .sum();
```

### The Rayon Prelude

Always start by importing the Rayon prelude:

```rust
use rayon::prelude::*;
```

This brings in all the essential traits and methods you need for parallel iteration.

## Creating Parallel Iterators

### From Ranges

```rust
use rayon::prelude::*;

// Parallel range iteration
(0..1000).into_par_iter().for_each(|i| {
    println!("Processing {}", i);
});

// Inclusive ranges
(0..=999).into_par_iter().for_each(|i| {
    // Process items 0 through 999
});
```

### From Collections

```rust
use rayon::prelude::*;

let vec = vec![1, 2, 3, 4, 5];

// Parallel iteration over references
vec.par_iter().for_each(|&x| {
    println!("Value: {}", x);
});

// Parallel iteration with mutable references
let mut vec = vec![1, 2, 3, 4, 5];
vec.par_iter_mut().for_each(|x| {
    *x *= 2;
});

// Parallel iteration consuming the collection
vec.into_par_iter().for_each(|x| {
    println!("Owned value: {}", x);
});
```

### From Arrays and Slices

```rust
use rayon::prelude::*;

let arr = [1, 2, 3, 4, 5];

// Arrays implement IntoParallelIterator
arr.par_iter().for_each(|&x| {
    println!("Array element: {}", x);
});

// Slices work the same way
let slice = &arr[1..4];
slice.par_iter().for_each(|&x| {
    println!("Slice element: {}", x);
});
```

## Core Parallel Iterator Methods

### Transformation Methods

#### `map` - Transform Each Element

```rust
use rayon::prelude::*;

let numbers: Vec<i32> = (0..1000).collect();
let squares: Vec<i32> = numbers
    .par_iter()
    .map(|&x| x * x)
    .collect();
```

#### `filter` - Select Elements

```rust
use rayon::prelude::*;

let numbers: Vec<i32> = (0..1000).collect();
let evens: Vec<i32> = numbers
    .par_iter()
    .filter(|&&x| x % 2 == 0)
    .cloned()
    .collect();
```

#### `filter_map` - Transform and Filter

```rust
use rayon::prelude::*;

let strings = vec!["1", "2", "not_a_number", "4"];
let numbers: Vec<i32> = strings
    .par_iter()
    .filter_map(|s| s.parse().ok())
    .collect();
// Result: [1, 2, 4]
```

### Reduction Methods

#### `reduce` - Combine All Elements

```rust
use rayon::prelude::*;

let numbers: Vec<i32> = (1..=100).collect();

// Sum using reduce
let sum = numbers
    .par_iter()
    .reduce(
        || 0,           // Identity function
        |a, &b| a + b   // Combining function
    );

// Find maximum
let max = numbers
    .par_iter()
    .reduce(
        || &0,
        |a, b| if a > b { a } else { b }
    );
```

#### `sum` and `product` - Built-in Reductions

```rust
use rayon::prelude::*;

let numbers: Vec<i32> = (1..=100).collect();

let sum: i32 = numbers.par_iter().sum();
let product: i32 = numbers.par_iter().product();
```

#### `fold` - Parallel Folding

```rust
use rayon::prelude::*;

let numbers: Vec<i32> = (1..=1000).collect();

// Fold into strings, then combine
let result: String = numbers
    .par_iter()
    .fold(
        || String::new(),           // Initial value for each thread
        |mut s, &x| {              // Fold function
            s.push_str(&x.to_string());
            s.push(',');
            s
        }
    )
    .reduce(
        || String::new(),           // Identity for final reduction
        |mut a, b| {               // Combine partial results
            a.push_str(&b);
            a
        }
    );
```

### Search Methods

#### `find_any` - Find Any Matching Element

```rust
use rayon::prelude::*;

let numbers: Vec<i32> = (0..1000).collect();

// Find any number divisible by 137
let found = numbers
    .par_iter()
    .find_any(|&&x| x % 137 == 0);

match found {
    Some(&x) => println!("Found: {}", x),
    None => println!("Not found"),
}
```

#### `find_first` and `find_last` - Ordered Search

```rust
use rayon::prelude::*;

let numbers: Vec<i32> = (0..1000).collect();

// Find first even number (maintains order)
let first_even = numbers
    .par_iter()
    .find_first(|&&x| x % 2 == 0);

// Find last odd number
let last_odd = numbers
    .par_iter()
    .find_last(|&&x| x % 2 == 1);
```

### Testing Methods

#### `any` and `all` - Boolean Tests

```rust
use rayon::prelude::*;

let numbers: Vec<i32> = (0..1000).collect();

// Check if any number is greater than 500
let has_large = numbers.par_iter().any(|&&x| x > 500);

// Check if all numbers are positive
let all_positive = numbers.par_iter().all(|&&x| x >= 0);
```

## Working with Different Data Types

### Strings

```rust
use rayon::prelude::*;

let text = "Hello, world! This is a test.";

// Parallel character processing
let char_count: usize = text
    .par_chars()
    .filter(|c| c.is_alphabetic())
    .count();

// Parallel line processing
let lines = vec![
    "line 1",
    "line 2", 
    "line 3"
];

let word_counts: Vec<usize> = lines
    .par_iter()
    .map(|line| line.split_whitespace().count())
    .collect();
```

### Options and Results

```rust
use rayon::prelude::*;

// Working with Options
let maybe_numbers: Vec<Option<i32>> = vec![
    Some(1), None, Some(3), Some(4), None
];

let sum: i32 = maybe_numbers
    .par_iter()
    .filter_map(|&opt| opt)
    .sum();

// Working with Results
let results: Vec<Result<i32, &str>> = vec![
    Ok(1), Err("error"), Ok(3), Ok(4)
];

let successful: Vec<i32> = results
    .par_iter()
    .filter_map(|r| r.as_ref().ok())
    .cloned()
    .collect();
```

## Advanced Patterns

### Chaining Operations

```rust
use rayon::prelude::*;

let data: Vec<i32> = (0..1000).collect();

let result: Vec<String> = data
    .par_iter()
    .filter(|&&x| x % 2 == 0)      // Keep even numbers
    .map(|&x| x * x)               // Square them
    .filter(|&x| x > 100)          // Keep large squares
    .map(|x| format!("Value: {}", x)) // Convert to strings
    .collect();
```

### Collecting into Different Types

```rust
use rayon::prelude::*;
use std::collections::{HashMap, HashSet};

let numbers: Vec<i32> = (0..100).collect();

// Collect into Vec
let squares: Vec<i32> = numbers
    .par_iter()
    .map(|&x| x * x)
    .collect();

// Collect into HashSet
let unique_remainders: HashSet<i32> = numbers
    .par_iter()
    .map(|&x| x % 7)
    .collect();

// Collect into HashMap (using enumerate)
let index_map: HashMap<usize, i32> = numbers
    .par_iter()
    .enumerate()
    .filter(|(_, &x)| x % 2 == 0)
    .collect();
```

### Unzipping

```rust
use rayon::prelude::*;

let pairs: Vec<(i32, String)> = (0..100)
    .into_par_iter()
    .map(|x| (x, format!("item_{}", x)))
    .collect();

// Unzip into separate collections
let (numbers, strings): (Vec<i32>, Vec<String>) = pairs
    .into_par_iter()
    .unzip();
```

## Error Handling

### Using `try_for_each`

```rust
use rayon::prelude::*;
use std::fs;

let files = vec!["file1.txt", "file2.txt", "file3.txt"];

// Process files, stopping on first error
let result: Result<(), std::io::Error> = files
    .par_iter()
    .try_for_each(|&filename| {
        let content = fs::read_to_string(filename)?;
        println!("File {} has {} bytes", filename, content.len());
        Ok(())
    });

match result {
    Ok(()) => println!("All files processed successfully"),
    Err(e) => println!("Error processing files: {}", e),
}
```

### Using `try_fold` and `try_reduce`

```rust
use rayon::prelude::*;

let numbers: Vec<i32> = (1..=100).collect();

// Safe division with error handling
let result: Result<i32, &str> = numbers
    .par_iter()
    .try_fold(
        || 1000,
        |acc, &x| {
            if x == 0 {
                Err("Division by zero")
            } else {
                Ok(acc / x)
            }
        }
    )
    .try_reduce(
        || 1,
        |a, b| Ok(a * b)
    );
```

## Performance Considerations

### When Parallelism Helps

```rust
use rayon::prelude::*;
use std::time::Instant;

fn expensive_computation(x: i32) -> i32 {
    // Simulate expensive work
    (0..1000).fold(x, |acc, i| acc.wrapping_mul(i + 1))
}

let data: Vec<i32> = (0..10000).collect();

// Sequential
let start = Instant::now();
let seq_result: Vec<i32> = data
    .iter()
    .map(|&x| expensive_computation(x))
    .collect();
let seq_time = start.elapsed();

// Parallel
let start = Instant::now();
let par_result: Vec<i32> = data
    .par_iter()
    .map(|&x| expensive_computation(x))
    .collect();
let par_time = start.elapsed();

println!("Sequential: {:?}", seq_time);
println!("Parallel: {:?}", par_time);
println!("Speedup: {:.2}x", seq_time.as_secs_f64() / par_time.as_secs_f64());
```

### Avoiding Overhead for Small Tasks

```rust
use rayon::prelude::*;

let small_data: Vec<i32> = (0..100).collect();
let large_data: Vec<i32> = (0..1_000_000).collect();

// For small datasets, sequential might be faster
let small_sum: i32 = small_data.iter().sum();

// For large datasets, parallel is usually better
let large_sum: i32 = large_data.par_iter().sum();
```

## Common Patterns

### Map-Reduce Pattern

```rust
use rayon::prelude::*;

let text = "the quick brown fox jumps over the lazy dog";

// Count word frequencies
let word_counts: std::collections::HashMap<&str, usize> = text
    .par_split_whitespace()
    .map(|word| (word, 1))
    .fold(
        || std::collections::HashMap::new(),
        |mut map, (word, count)| {
            *map.entry(word).or_insert(0) += count;
            map
        }
    )
    .reduce(
        || std::collections::HashMap::new(),
        |mut map1, map2| {
            for (word, count) in map2 {
                *map1.entry(word).or_insert(0) += count;
            }
            map1
        }
    );
```

### Parallel Processing with Side Effects

```rust
use rayon::prelude::*;
use std::sync::atomic::{AtomicUsize, Ordering};

let counter = AtomicUsize::new(0);
let data: Vec<i32> = (0..1000).collect();

data.par_iter().for_each(|&x| {
    if x % 2 == 0 {
        counter.fetch_add(1, Ordering::Relaxed);
    }
    // Process x...
});

println!("Found {} even numbers", counter.load(Ordering::Relaxed));
```

## Best Practices

1. **Start Simple**: Begin with `.par_iter()` and see if it helps
2. **Measure Performance**: Always benchmark to ensure parallelism helps
3. **Avoid Shared Mutable State**: Use atomic operations or message passing
4. **Use Appropriate Granularity**: Ensure tasks are large enough to benefit from parallelism
5. **Consider Memory Access Patterns**: Cache-friendly access patterns perform better
6. **Handle Errors Gracefully**: Use `try_*` methods for operations that can fail

## Next Steps

Now that you understand the basics of parallel iterators, explore:

- [Work-Stealing Algorithm](02-work-stealing.md) - How Rayon distributes work
- [Thread Pool Configuration](03-thread-pools.md) - Customizing execution
- [Parallel Algorithms](04-algorithms.md) - Advanced patterns and techniques