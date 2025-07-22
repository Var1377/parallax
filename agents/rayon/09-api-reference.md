# Rayon 1.10 API Reference

## Overview

This comprehensive API reference covers all major types, traits, and functions in Rayon 1.10 with practical examples.

## Core Traits

### ParallelIterator

The fundamental trait for parallel iteration.

```rust
use rayon::prelude::*;

pub trait ParallelIterator: Sized + Send {
    type Item: Send;
    
    // Core methods
    fn for_each<OP>(self, op: OP) where OP: Fn(Self::Item) + Sync + Send;
    fn for_each_with<OP, T>(self, init: T, op: OP) 
    where OP: Fn(&mut T, Self::Item) + Sync + Send, T: Send + Clone;
    
    fn map<F, R>(self, map_op: F) -> Map<Self, F>
    where F: Fn(Self::Item) -> R + Sync + Send, R: Send;
    
    fn filter<P>(self, filter_op: P) -> Filter<Self, P>
    where P: Fn(&Self::Item) -> bool + Sync + Send;
    
    fn reduce<OP, ID>(self, identity: ID, op: OP) -> Self::Item
    where OP: Fn(Self::Item, Self::Item) -> Self::Item + Sync + Send,
          ID: Fn() -> Self::Item + Sync + Send;
    
    fn fold<T, ID, F>(self, identity: ID, fold_op: F) -> Fold<Self, ID, F>
    where F: Fn(T, Self::Item) -> T + Sync + Send,
          ID: Fn() -> T + Sync + Send, T: Send;
    
    fn collect<C>(self) -> C where C: FromParallelIterator<Self::Item>;
    
    // And many more...
}

// Examples
fn parallel_iterator_examples() {
    let data: Vec<i32> = (0..1000).collect();
    
    // for_each - side effects
    data.par_iter().for_each(|&x| {
        println!("Processing {}", x);
    });
    
    // map - transformation
    let squares: Vec<i32> = data.par_iter().map(|&x| x * x).collect();
    
    // filter - selection
    let evens: Vec<i32> = data.par_iter().filter(|&&x| x % 2 == 0).cloned().collect();
    
    // reduce - aggregation
    let sum = data.par_iter().reduce(|| 0, |a, &b| a + b);
    
    // fold - accumulation
    let sum_fold: i32 = data.par_iter()
        .fold(|| 0, |acc, &x| acc + x)
        .sum();
    
    // Complex chain
    let result: Vec<String> = data.par_iter()
        .filter(|&&x| x % 2 == 0)
        .map(|&x| x * x)
        .filter(|&x| x > 100)
        .map(|x| format!("value_{}", x))
        .collect();
}
```

### IndexedParallelIterator

For iterators with known length and random access.

```rust
use rayon::prelude::*;

pub trait IndexedParallelIterator: ParallelIterator {
    fn len(&self) -> usize;
    
    fn enumerate(self) -> Enumerate<Self>;
    fn skip(self, n: usize) -> Skip<Self>;
    fn take(self, n: usize) -> Take<Self>;
    fn zip<Z>(self, zip_op: Z) -> Zip<Self, Z::Iter>
    where Z: IntoParallelIterator, Z::Iter: IndexedParallelIterator;
    
    fn collect_into_vec(self, target: &mut Vec<Self::Item>);
    fn unzip<A, B>(self) -> (Vec<A>, Vec<B>)
    where Self: ParallelIterator<Item = (A, B)>, A: Send, B: Send;
    
    // Chunking methods
    fn chunks(self, chunk_size: usize) -> Chunks<Self>;
    fn fold_chunks<T, ID, F>(self, chunk_size: usize, identity: ID, fold_op: F) -> FoldChunks<Self, ID, F>
    where F: Fn(T, Self::Item) -> T + Sync + Send,
          ID: Fn() -> T + Sync + Send, T: Send;
}

// Examples
fn indexed_iterator_examples() {
    let data: Vec<i32> = (0..1000).collect();
    
    // enumerate - add indices
    let indexed: Vec<(usize, i32)> = data.par_iter()
        .enumerate()
        .map(|(i, &x)| (i, x * x))
        .collect();
    
    // skip and take
    let middle: Vec<i32> = data.par_iter()
        .skip(100)
        .take(200)
        .cloned()
        .collect();
    
    // zip two iterators
    let other: Vec<i32> = (1000..2000).collect();
    let zipped: Vec<(i32, i32)> = data.par_iter()
        .zip(&other)
        .map(|(&a, &b)| (a, b))
        .collect();
    
    // chunks - process in groups
    let chunk_sums: Vec<i32> = data.par_iter()
        .chunks(10)
        .map(|chunk| chunk.iter().sum())
        .collect();
    
    // collect_into_vec - efficient collection
    let mut result = Vec::with_capacity(data.len());
    data.par_iter().map(|&x| x * 2).collect_into_vec(&mut result);
    
    // unzip pairs
    let pairs: Vec<(i32, String)> = data.par_iter()
        .map(|&x| (x, format!("item_{}", x)))
        .collect();
    let (numbers, strings): (Vec<i32>, Vec<String>) = pairs.into_par_iter().unzip();
}
```

### IntoParallelIterator

Conversion trait for creating parallel iterators.

```rust
use rayon::prelude::*;

pub trait IntoParallelIterator {
    type Iter: ParallelIterator<Item = Self::Item>;
    type Item: Send;
    
    fn into_par_iter(self) -> Self::Iter;
}

// Implementations for common types
fn into_parallel_examples() {
    // Vec<T>
    let vec = vec![1, 2, 3, 4, 5];
    let sum: i32 = vec.into_par_iter().sum();
    
    // &[T]
    let slice = &[1, 2, 3, 4, 5];
    let sum: i32 = slice.par_iter().sum();
    
    // &mut [T]
    let mut slice = [1, 2, 3, 4, 5];
    slice.par_iter_mut().for_each(|x| *x *= 2);
    
    // Range
    let sum: i32 = (0..1000).into_par_iter().sum();
    
    // String
    let text = "hello world";
    let char_count: usize = text.par_chars().count();
    
    // HashMap
    use std::collections::HashMap;
    let mut map = HashMap::new();
    map.insert("a", 1);
    map.insert("b", 2);
    let sum: i32 = map.into_par_iter().map(|(_, v)| v).sum();
}
```

## Parallel Iterator Adaptors

### Map Operations

```rust
use rayon::prelude::*;

fn map_operations() {
    let data: Vec<i32> = (0..1000).collect();
    
    // Basic map
    let squares: Vec<i32> = data.par_iter().map(|&x| x * x).collect();
    
    // map_with - with initialization value
    use std::sync::mpsc::channel;
    let (sender, receiver) = channel();
    data.par_iter().map_with(sender, |s, &x| {
        s.send(x).unwrap();
        x * x
    }).collect::<Vec<_>>();
    
    // map_init - with initialization function
    let results: Vec<i32> = data.par_iter()
        .map_init(
            || rand::thread_rng(), // Initialize RNG per thread
            |rng, &x| {
                use rand::Rng;
                x + rng.gen_range(0..10)
            }
        ).collect();
    
    // filter_map - map and filter combined
    let parsed: Vec<i32> = vec!["1", "2", "not_a_number", "4"]
        .par_iter()
        .filter_map(|s| s.parse().ok())
        .collect();
    
    // flat_map - flatten nested iterators
    let nested = vec![vec![1, 2], vec![3, 4], vec![5, 6]];
    let flattened: Vec<i32> = nested.par_iter()
        .flat_map(|inner| inner.par_iter().cloned())
        .collect();
}
```

### Filter Operations

```rust
use rayon::prelude::*;

fn filter_operations() {
    let data: Vec<i32> = (0..1000).collect();
    
    // Basic filter
    let evens: Vec<i32> = data.par_iter()
        .filter(|&&x| x % 2 == 0)
        .cloned()
        .collect();
    
    // filter_map
    let positive_squares: Vec<i32> = data.par_iter()
        .filter_map(|&x| {
            if x > 0 {
                Some(x * x)
            } else {
                None
            }
        })
        .collect();
    
    // Complex filtering
    let complex_filter: Vec<i32> = data.par_iter()
        .filter(|&&x| x % 2 == 0)  // Even numbers
        .map(|&x| x * x)           // Square them
        .filter(|&x| x > 100)      // Keep large squares
        .collect();
}
```

### Reduction Operations

```rust
use rayon::prelude::*;

fn reduction_operations() {
    let data: Vec<i32> = (1..=1000).collect();
    
    // Built-in reductions
    let sum: i32 = data.par_iter().sum();
    let product: i32 = data.par_iter().take(10).product();
    let min: Option<&i32> = data.par_iter().min();
    let max: Option<&i32> = data.par_iter().max();
    let count: usize = data.par_iter().count();
    
    // Custom reduce
    let sum_custom: i32 = data.par_iter()
        .reduce(|| 0, |a, &b| a + b);
    
    // reduce_with - no identity needed
    let max_custom: Option<i32> = data.par_iter()
        .cloned()
        .reduce_with(|a, b| std::cmp::max(a, b));
    
    // min_by and max_by
    let min_by_abs: Option<&i32> = data.par_iter()
        .min_by(|a, b| a.abs().cmp(&b.abs()));
    
    // min_by_key and max_by_key
    let max_by_square: Option<&i32> = data.par_iter()
        .max_by_key(|&&x| x * x);
    
    // any and all
    let has_even: bool = data.par_iter().any(|&x| x % 2 == 0);
    let all_positive: bool = data.par_iter().all(|&x| x > 0);
}
```

### Fold Operations

```rust
use rayon::prelude::*;
use std::collections::HashMap;

fn fold_operations() {
    let data: Vec<i32> = (0..1000).collect();
    
    // Basic fold
    let sum: i32 = data.par_iter()
        .fold(|| 0, |acc, &x| acc + x)
        .sum(); // Combine partial results
    
    // fold_with - with initial value
    let sum_with: i32 = data.par_iter()
        .fold_with(0, |acc, &x| acc + x)
        .sum();
    
    // Complex fold - histogram
    let histogram: HashMap<i32, usize> = data.par_iter()
        .fold(
            || HashMap::new(),
            |mut map, &x| {
                let bucket = x / 100; // Group by hundreds
                *map.entry(bucket).or_insert(0) += 1;
                map
            }
        )
        .reduce(
            || HashMap::new(),
            |mut map1, map2| {
                for (key, value) in map2 {
                    *map1.entry(key).or_insert(0) += value;
                }
                map1
            }
        );
    
    // try_fold - with error handling
    let safe_sum: Result<i32, &str> = data.par_iter()
        .try_fold(
            || 0i32,
            |acc, &x| {
                acc.checked_add(x).ok_or("overflow")
            }
        )
        .try_reduce(|| 0, |a, b| a.checked_add(b).ok_or("overflow"));
}
```

### Search Operations

```rust
use rayon::prelude::*;

fn search_operations() {
    let data: Vec<i32> = (0..1000).collect();
    
    // find_any - find any matching element
    let found_any: Option<&i32> = data.par_iter()
        .find_any(|&&x| x > 500 && x % 7 == 0);
    
    // find_first - find first matching element (maintains order)
    let found_first: Option<&i32> = data.par_iter()
        .find_first(|&&x| x > 500);
    
    // find_last - find last matching element
    let found_last: Option<&i32> = data.par_iter()
        .find_last(|&&x| x < 500);
    
    // find_map variants
    let strings = vec!["1", "2", "not_a_number", "4"];
    
    let found_map_any: Option<i32> = strings.par_iter()
        .find_map_any(|s| s.parse().ok());
    
    let found_map_first: Option<i32> = strings.par_iter()
        .find_map_first(|s| s.parse().ok());
    
    let found_map_last: Option<i32> = strings.par_iter()
        .find_map_last(|s| s.parse().ok());
    
    // position - find index of element
    let position: Option<usize> = data.par_iter()
        .position_any(|&x| x == 500);
    
    let position_first: Option<usize> = data.par_iter()
        .position_first(|&x| x > 500);
    
    let position_last: Option<usize> = data.par_iter()
        .position_last(|&x| x < 500);
}
```

## Collection Types

### Parallel Slice Operations

```rust
use rayon::prelude::*;

fn slice_operations() {
    let mut data = vec![3, 1, 4, 1, 5, 9, 2, 6, 5, 3];
    
    // Parallel sorting
    data.par_sort();
    data.par_sort_by(|a, b| b.cmp(a)); // Reverse order
    data.par_sort_by_key(|&x| x % 3);  // Sort by remainder
    
    // Parallel sorting (unstable - faster)
    data.par_sort_unstable();
    data.par_sort_unstable_by(|a, b| a.cmp(b));
    data.par_sort_unstable_by_key(|&x| x);
    
    // Chunking operations
    let chunks: Vec<Vec<i32>> = data.par_chunks(3)
        .map(|chunk| chunk.to_vec())
        .collect();
    
    let mut_chunks: Vec<()> = data.par_chunks_mut(3)
        .map(|chunk| {
            for item in chunk {
                *item *= 2;
            }
        })
        .collect();
    
    // Windowing
    let windows: Vec<Vec<i32>> = data.par_windows(3)
        .map(|window| window.to_vec())
        .collect();
    
    // Splitting
    let split_results: Vec<Vec<i32>> = data.par_split(|&x| x == 5)
        .map(|part| part.to_vec())
        .collect();
}
```

### Parallel String Operations

```rust
use rayon::prelude::*;

fn string_operations() {
    let text = "The quick brown fox jumps over the lazy dog";
    
    // Character iteration
    let char_count: usize = text.par_chars().count();
    let uppercase_count: usize = text.par_chars()
        .filter(|c| c.is_uppercase())
        .count();
    
    // Byte iteration
    let byte_sum: u32 = text.par_bytes().map(|b| b as u32).sum();
    
    // Line iteration
    let multiline = "line1\nline2\nline3\nline4";
    let line_lengths: Vec<usize> = multiline.par_lines()
        .map(|line| line.len())
        .collect();
    
    // Word splitting
    let word_count: usize = text.par_split_whitespace().count();
    let words: Vec<String> = text.par_split_whitespace()
        .map(|word| word.to_uppercase())
        .collect();
    
    // Custom splitting
    let parts: Vec<&str> = text.par_split(' ').collect();
    
    // Pattern matching
    let matches: Vec<usize> = text.par_match_indices("o")
        .map(|(index, _)| index)
        .collect();
}
```

### Parallel HashMap and BTreeMap

```rust
use rayon::prelude::*;
use std::collections::{HashMap, BTreeMap};

fn map_operations() {
    let mut map = HashMap::new();
    for i in 0..1000 {
        map.insert(i, format!("value_{}", i));
    }
    
    // Parallel iteration over HashMap
    let key_sum: i32 = map.par_iter()
        .map(|(&key, _)| key)
        .sum();
    
    let value_lengths: Vec<usize> = map.par_iter()
        .map(|(_, value)| value.len())
        .collect();
    
    // Parallel operations on keys and values
    let keys: Vec<i32> = map.par_keys().cloned().collect();
    let values: Vec<String> = map.par_values().cloned().collect();
    
    // BTreeMap operations (similar interface)
    let btree: BTreeMap<i32, String> = map.into_iter().collect();
    let btree_sum: i32 = btree.par_iter()
        .map(|(&key, _)| key)
        .sum();
}
```

## Thread Pool Management

### ThreadPoolBuilder

```rust
use rayon::{ThreadPool, ThreadPoolBuilder};

fn thread_pool_examples() -> Result<(), rayon::ThreadPoolBuildError> {
    // Basic thread pool
    let pool = ThreadPoolBuilder::new()
        .num_threads(4)
        .build()?;
    
    // Advanced configuration
    let advanced_pool = ThreadPoolBuilder::new()
        .num_threads(8)
        .thread_name(|index| format!("worker-{}", index))
        .stack_size(8 * 1024 * 1024) // 8MB stack
        .panic_handler(|panic_info| {
            eprintln!("Worker panic: {:?}", panic_info);
        })
        .start_handler(|index| {
            println!("Thread {} starting", index);
        })
        .exit_handler(|index| {
            println!("Thread {} exiting", index);
        })
        .breadth_first() // Use breadth-first scheduling
        .build()?;
    
    // Using the thread pool
    let result = pool.install(|| {
        (0..1000).into_par_iter().sum::<i32>()
    });
    
    // Scoped thread pool
    ThreadPoolBuilder::new()
        .build_scoped(
            |thread| {
                // Thread initialization
                println!("Initializing thread {}", thread.index());
                thread.run();
            },
            |pool| {
                // Use the pool
                pool.install(|| {
                    (0..1000).into_par_iter().for_each(|i| {
                        println!("Processing {}", i);
                    });
                });
            }
        )?;
    
    Ok(())
}
```

### Global Thread Pool Configuration

```rust
use rayon::ThreadPoolBuilder;

fn configure_global_pool() -> Result<(), rayon::ThreadPoolBuildError> {
    // Configure global pool (must be done before any parallel operations)
    ThreadPoolBuilder::new()
        .num_threads(4)
        .thread_name(|index| format!("global-{}", index))
        .build_global()?;
    
    // Now all parallel operations use the configured global pool
    let sum: i32 = (0..1000).into_par_iter().sum();
    
    Ok(())
}
```

## Low-Level Parallel Operations

### Join Operations

```rust
use rayon::join;

fn join_operations() {
    // Basic join - run two tasks in parallel
    let (left_result, right_result) = join(
        || expensive_computation(1000),
        || expensive_computation(2000)
    );
    
    // Nested joins for divide-and-conquer
    fn parallel_sum(data: &[i32]) -> i32 {
        if data.len() <= 1000 {
            // Base case: sequential sum
            data.iter().sum()
        } else {
            // Recursive case: split and join
            let mid = data.len() / 2;
            let (left, right) = data.split_at(mid);
            
            let (left_sum, right_sum) = join(
                || parallel_sum(left),
                || parallel_sum(right)
            );
            
            left_sum + right_sum
        }
    }
    
    let data: Vec<i32> = (0..100000).collect();
    let sum = parallel_sum(&data);
}

fn expensive_computation(n: usize) -> usize {
    (0..n).fold(0, |acc, i| acc.wrapping_add(i))
}
```

### Scope Operations

```rust
use rayon::scope;

fn scope_operations() {
    let mut data = vec![0; 1000];
    
    // Parallel scope - can spawn multiple tasks
    scope(|s| {
        for chunk in data.chunks_mut(100) {
            s.spawn(|_| {
                for item in chunk {
                    *item = expensive_computation(1000);
                }
            });
        }
        // All spawned tasks complete before scope ends
    });
    
    // FIFO scope - tasks execute in FIFO order
    use rayon::scope_fifo;
    scope_fifo(|s| {
        for i in 0..10 {
            s.spawn_fifo(|_| {
                println!("Task {} executing", i);
            });
        }
    });
}
```

### Spawn Operations

```rust
use rayon::{spawn, spawn_fifo};

fn spawn_operations() {
    // Spawn task in global scope
    spawn(|| {
        println!("Background task executing");
        expensive_computation(1000);
    });
    
    // FIFO spawn
    spawn_fifo(|| {
        println!("FIFO task executing");
    });
    
    // Note: spawned tasks may not complete before main thread exits
    // Use scope for guaranteed completion
}
```

## Error Handling

### Try Operations

```rust
use rayon::prelude::*;

fn error_handling_examples() {
    let data: Vec<i32> = (-10..10).collect();
    
    // try_for_each - stop on first error
    let result: Result<(), &str> = data.par_iter()
        .try_for_each(|&x| {
            if x < 0 {
                Err("negative number")
            } else {
                println!("Processing {}", x);
                Ok(())
            }
        });
    
    // try_fold - fold with error handling
    let safe_sum: Result<i32, &str> = data.par_iter()
        .try_fold(
            || 0i32,
            |acc, &x| {
                if x < 0 {
                    Err("negative number")
                } else {
                    Ok(acc + x)
                }
            }
        )
        .try_reduce(|| 0, |a, b| Ok(a + b));
    
    // try_reduce_with
    let safe_max: Option<Result<i32, &str>> = data.par_iter()
        .map(|&x| {
            if x < 0 {
                Err("negative number")
            } else {
                Ok(x)
            }
        })
        .try_reduce_with(|a, b| {
            match (a, b) {
                (Ok(a), Ok(b)) => Ok(std::cmp::max(a, b)),
                (Err(e), _) | (_, Err(e)) => Err(e),
            }
        });
    
    // Collect errors instead of failing fast
    let results: Vec<Result<i32, &str>> = data.par_iter()
        .map(|&x| {
            if x < 0 {
                Err("negative number")
            } else {
                Ok(x * x)
            }
        })
        .collect();
    
    let (successes, errors): (Vec<_>, Vec<_>) = results
        .into_iter()
        .partition(Result::is_ok);
}
```

## Utility Functions

### Thread Information

```rust
use rayon::{current_num_threads, current_thread_index, max_num_threads};

fn thread_info() {
    println!("Max threads supported: {}", max_num_threads());
    println!("Current pool size: {}", current_num_threads());
    
    (0..10).into_par_iter().for_each(|i| {
        if let Some(thread_index) = current_thread_index() {
            println!("Item {} processed by thread {}", i, thread_index);
        } else {
            println!("Item {} processed outside thread pool", i);
        }
    });
}
```

### Yielding Control

```rust
use rayon::{yield_now, yield_local, Yield};

fn yielding_examples() {
    (0..1000).into_par_iter().for_each(|i| {
        // Do some work
        expensive_computation(100);
        
        // Periodically yield to allow other tasks to run
        if i % 100 == 0 {
            yield_now(); // Yield to any waiting task
        }
        
        // Or yield only to local tasks
        if i % 50 == 0 {
            yield_local(); // Yield to tasks in local queue only
        }
    });
    
    // Check if yielding is beneficial
    let should_yield = yield_now();
    match should_yield {
        Yield::Executed => println!("Yielded and executed other work"),
        Yield::Idle => println!("No other work to yield to"),
    }
}
```

## Performance Monitoring

### Built-in Diagnostics

```rust
use rayon::prelude::*;

fn performance_monitoring() {
    // Check if current thread has pending tasks
    let has_pending = rayon::current_thread_has_pending_tasks();
    if has_pending {
        println!("Current thread has pending work");
    }
    
    // Monitor thread utilization
    (0..1000).into_par_iter().for_each(|i| {
        if i % 100 == 0 {
            println!("Thread {} processing item {}", 
                     rayon::current_thread_index().unwrap_or(999), i);
        }
        expensive_computation(10);
    });
}
```

## Advanced Patterns

### Custom Parallel Extensions

```rust
use rayon::prelude::*;

// Extension trait for custom parallel operations
trait ParallelVecExt<T> {
    fn par_dedup(&mut self) where T: PartialEq + Send;
    fn par_reverse(&mut self) where T: Send;
}

impl<T> ParallelVecExt<T> for Vec<T> {
    fn par_dedup(&mut self) where T: PartialEq + Send {
        // Simplified parallel deduplication
        self.par_sort_by(|a, b| std::ptr::eq(a, b).cmp(&true));
        self.dedup();
    }
    
    fn par_reverse(&mut self) where T: Send {
        let len = self.len();
        self.par_chunks_mut(2).for_each(|chunk| {
            if chunk.len() == 2 {
                chunk.swap(0, 1);
            }
        });
        // Note: This is a simplified example, not a complete implementation
    }
}

// Parallel processing with custom logic
fn custom_parallel_patterns() {
    let mut data = vec![1, 2, 2, 3, 3, 3, 4, 4, 4, 4];
    
    // Use custom extension
    data.par_dedup();
    
    // Parallel batch processing
    let batches: Vec<Vec<i32>> = (0..1000).collect::<Vec<_>>()
        .chunks(100)
        .map(|chunk| chunk.to_vec())
        .collect();
    
    let results: Vec<i32> = batches.into_par_iter()
        .map(|batch| {
            // Process each batch in parallel
            batch.into_iter().map(|x| x * x).sum()
        })
        .collect();
}
```

## Complete Example: Parallel Image Processing

```rust
use rayon::prelude::*;

#[derive(Clone, Debug)]
struct Pixel {
    r: u8,
    g: u8,
    b: u8,
}

struct Image {
    width: usize,
    height: usize,
    pixels: Vec<Pixel>,
}

impl Image {
    fn new(width: usize, height: usize) -> Self {
        Image {
            width,
            height,
            pixels: vec![Pixel { r: 0, g: 0, b: 0 }; width * height],
        }
    }
    
    fn par_apply_filter<F>(&mut self, filter: F)
    where
        F: Fn(&Pixel) -> Pixel + Sync + Send,
    {
        self.pixels.par_iter_mut().for_each(|pixel| {
            *pixel = filter(pixel);
        });
    }
    
    fn par_blur(&mut self, radius: usize) {
        let original = self.pixels.clone();
        
        self.pixels.par_iter_mut().enumerate().for_each(|(i, pixel)| {
            let x = i % self.width;
            let y = i / self.width;
            
            let mut r_sum = 0u32;
            let mut g_sum = 0u32;
            let mut b_sum = 0u32;
            let mut count = 0u32;
            
            for dy in -(radius as i32)..=(radius as i32) {
                for dx in -(radius as i32)..=(radius as i32) {
                    let nx = x as i32 + dx;
                    let ny = y as i32 + dy;
                    
                    if nx >= 0 && nx < self.width as i32 && 
                       ny >= 0 && ny < self.height as i32 {
                        let idx = (ny as usize * self.width + nx as usize);
                        let p = &original[idx];
                        r_sum += p.r as u32;
                        g_sum += p.g as u32;
                        b_sum += p.b as u32;
                        count += 1;
                    }
                }
            }
            
            if count > 0 {
                pixel.r = (r_sum / count) as u8;
                pixel.g = (g_sum / count) as u8;
                pixel.b = (b_sum / count) as u8;
            }
        });
    }
    
    fn par_histogram(&self) -> Vec<usize> {
        let histograms: Vec<Vec<usize>> = self.pixels
            .par_chunks(1000)
            .map(|chunk| {
                let mut hist = vec![0; 256];
                for pixel in chunk {
                    let brightness = ((pixel.r as u32 + pixel.g as u32 + pixel.b as u32) / 3) as usize;
                    hist[brightness] += 1;
                }
                hist
            })
            .collect();
        
        // Combine histograms
        let mut final_hist = vec![0; 256];
        for hist in histograms {
            for (i, count) in hist.into_iter().enumerate() {
                final_hist[i] += count;
            }
        }
        
        final_hist
    }
}

fn image_processing_example() {
    let mut image = Image::new(1920, 1080);
    
    // Initialize with some pattern
    image.pixels.par_iter_mut().enumerate().for_each(|(i, pixel)| {
        let x = i % image.width;
        let y = i / image.width;
        pixel.r = (x % 256) as u8;
        pixel.g = (y % 256) as u8;
        pixel.b = ((x + y) % 256) as u8;
    });
    
    // Apply filters in parallel
    image.par_apply_filter(|pixel| Pixel {
        r: pixel.r.saturating_add(10),
        g: pixel.g,
        b: pixel.b.saturating_sub(10),
    });
    
    // Blur the image
    image.par_blur(2);
    
    // Generate histogram
    let histogram = image.par_histogram();
    println!("Histogram computed: {} buckets", histogram.len());
}
```

## Summary

This API reference covers the complete Rayon 1.10 interface with practical examples. Key takeaways:

- **ParallelIterator** is the core trait with rich functionality
- **IndexedParallelIterator** adds length-aware operations
- **Thread pools** provide fine-grained control over execution
- **Error handling** is built into the parallel operations
- **Low-level operations** like `join` and `scope` enable custom parallel algorithms
- **Performance monitoring** helps optimize parallel code

For the most up-to-date API documentation, always refer to [docs.rs/rayon](https://docs.rs/rayon/).

---

This completes the comprehensive Rayon 1.10 documentation covering all major aspects of the library with practical examples and best practices.