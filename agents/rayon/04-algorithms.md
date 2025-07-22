# Parallel Algorithms and Patterns in Rayon

## Introduction

This guide covers common parallel algorithms and design patterns using Rayon. Understanding these patterns will help you solve complex problems efficiently and write maintainable parallel code.

## Fundamental Parallel Patterns

### 1. Map Pattern

Transform each element independently.

```rust
use rayon::prelude::*;

// Basic map
let numbers: Vec<i32> = (0..1000).collect();
let squares: Vec<i32> = numbers
    .par_iter()
    .map(|&x| x * x)
    .collect();

// Complex transformation
#[derive(Debug)]
struct Person {
    name: String,
    age: u32,
}

#[derive(Debug)]
struct PersonInfo {
    name: String,
    category: String,
    birth_year: u32,
}

let people = vec![
    Person { name: "Alice".to_string(), age: 30 },
    Person { name: "Bob".to_string(), age: 25 },
    Person { name: "Charlie".to_string(), age: 35 },
];

let info: Vec<PersonInfo> = people
    .par_iter()
    .map(|person| PersonInfo {
        name: person.name.clone(),
        category: if person.age >= 30 { "Senior".to_string() } else { "Junior".to_string() },
        birth_year: 2024 - person.age,
    })
    .collect();
```

### 2. Filter Pattern

Select elements based on a predicate.

```rust
use rayon::prelude::*;

// Basic filter
let numbers: Vec<i32> = (0..1000).collect();
let evens: Vec<i32> = numbers
    .par_iter()
    .filter(|&&x| x % 2 == 0)
    .cloned()
    .collect();

// Complex filtering with multiple conditions
let large_even_squares: Vec<i32> = numbers
    .par_iter()
    .filter(|&&x| x % 2 == 0)           // Even numbers
    .map(|&x| x * x)                    // Square them
    .filter(|&x| x > 100)               // Keep large squares
    .collect();

// Filter with expensive predicate
fn is_prime(n: u32) -> bool {
    if n < 2 { return false; }
    for i in 2..=(n as f64).sqrt() as u32 {
        if n % i == 0 { return false; }
    }
    true
}

let primes: Vec<u32> = (2..10000)
    .into_par_iter()
    .filter(|&n| is_prime(n))
    .collect();
```

### 3. Reduce Pattern

Combine all elements into a single result.

```rust
use rayon::prelude::*;

// Sum reduction
let numbers: Vec<i32> = (1..=1000).collect();
let sum: i32 = numbers.par_iter().sum();

// Custom reduction
let product: i32 = numbers
    .par_iter()
    .reduce(|| 1, |acc, &x| acc * x);

// Find maximum with custom comparison
#[derive(Debug, Clone)]
struct Point {
    x: f64,
    y: f64,
}

impl Point {
    fn distance_from_origin(&self) -> f64 {
        (self.x * self.x + self.y * self.y).sqrt()
    }
}

let points = vec![
    Point { x: 1.0, y: 2.0 },
    Point { x: 3.0, y: 4.0 },
    Point { x: 0.5, y: 1.5 },
];

let farthest = points
    .par_iter()
    .reduce_with(|a, b| {
        if a.distance_from_origin() > b.distance_from_origin() { a } else { b }
    });
```

### 4. Fold Pattern

Accumulate values with different types.

```rust
use rayon::prelude::*;
use std::collections::HashMap;

// Fold into different type
let numbers: Vec<i32> = (1..=100).collect();

// Create histogram
let histogram: HashMap<i32, usize> = numbers
    .par_iter()
    .fold(
        || HashMap::new(),
        |mut map, &x| {
            let bucket = x / 10; // Group by tens
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

// Fold with complex accumulator
#[derive(Default)]
struct Statistics {
    count: usize,
    sum: f64,
    sum_squares: f64,
}

impl Statistics {
    fn add(&mut self, value: f64) {
        self.count += 1;
        self.sum += value;
        self.sum_squares += value * value;
    }
    
    fn mean(&self) -> f64 {
        self.sum / self.count as f64
    }
    
    fn variance(&self) -> f64 {
        let mean = self.mean();
        (self.sum_squares / self.count as f64) - (mean * mean)
    }
}

let data: Vec<f64> = (0..10000).map(|x| x as f64 * 0.1).collect();

let stats: Statistics = data
    .par_iter()
    .fold(
        || Statistics::default(),
        |mut stats, &value| {
            stats.add(value);
            stats
        }
    )
    .reduce(
        || Statistics::default(),
        |mut stats1, stats2| {
            stats1.count += stats2.count;
            stats1.sum += stats2.sum;
            stats1.sum_squares += stats2.sum_squares;
            stats1
        }
    );

println!("Mean: {:.2}, Variance: {:.2}", stats.mean(), stats.variance());
```

## Advanced Parallel Patterns

### 1. Divide and Conquer

Break problems into smaller subproblems.

```rust
use rayon::prelude::*;

// Parallel merge sort
fn parallel_merge_sort<T: Ord + Send + Clone>(mut vec: Vec<T>) -> Vec<T> {
    let len = vec.len();
    if len <= 1 {
        return vec;
    }
    
    if len < 1000 {
        // Use sequential sort for small arrays
        vec.sort();
        return vec;
    }
    
    let mid = len / 2;
    let right = vec.split_off(mid);
    
    // Parallel divide
    let (left, right) = rayon::join(
        || parallel_merge_sort(vec),
        || parallel_merge_sort(right)
    );
    
    // Merge results
    merge(left, right)
}

fn merge<T: Ord + Clone>(left: Vec<T>, right: Vec<T>) -> Vec<T> {
    let mut result = Vec::with_capacity(left.len() + right.len());
    let mut left_iter = left.into_iter();
    let mut right_iter = right.into_iter();
    let mut left_val = left_iter.next();
    let mut right_val = right_iter.next();
    
    loop {
        match (left_val.take(), right_val.take()) {
            (Some(l), Some(r)) => {
                if l <= r {
                    result.push(l);
                    left_val = left_iter.next();
                    right_val = Some(r);
                } else {
                    result.push(r);
                    right_val = right_iter.next();
                    left_val = Some(l);
                }
            }
            (Some(l), None) => {
                result.push(l);
                result.extend(left_iter);
                break;
            }
            (None, Some(r)) => {
                result.push(r);
                result.extend(right_iter);
                break;
            }
            (None, None) => break,
        }
    }
    
    result
}

// Usage
let data: Vec<i32> = (0..100000).rev().collect(); // Reverse sorted
let sorted = parallel_merge_sort(data);
```

### 2. Parallel Tree Traversal

Process tree structures in parallel.

```rust
use rayon::prelude::*;

#[derive(Debug)]
struct TreeNode {
    value: i32,
    children: Vec<TreeNode>,
}

impl TreeNode {
    fn new(value: i32) -> Self {
        TreeNode { value, children: Vec::new() }
    }
    
    fn add_child(&mut self, child: TreeNode) {
        self.children.push(child);
    }
    
    // Parallel tree sum
    fn parallel_sum(&self) -> i32 {
        let children_sum: i32 = self.children
            .par_iter()
            .map(|child| child.parallel_sum())
            .sum();
        
        self.value + children_sum
    }
    
    // Parallel tree search
    fn parallel_find(&self, target: i32) -> bool {
        if self.value == target {
            return true;
        }
        
        self.children
            .par_iter()
            .any(|child| child.parallel_find(target))
    }
    
    // Parallel tree transformation
    fn parallel_map<F>(&self, f: &F) -> TreeNode 
    where
        F: Fn(i32) -> i32 + Sync,
    {
        let new_children: Vec<TreeNode> = self.children
            .par_iter()
            .map(|child| child.parallel_map(f))
            .collect();
        
        TreeNode {
            value: f(self.value),
            children: new_children,
        }
    }
}

// Build a sample tree
fn build_tree() -> TreeNode {
    let mut root = TreeNode::new(1);
    
    for i in 2..=5 {
        let mut child = TreeNode::new(i);
        for j in 1..=3 {
            child.add_child(TreeNode::new(i * 10 + j));
        }
        root.add_child(child);
    }
    
    root
}

// Usage
let tree = build_tree();
let sum = tree.parallel_sum();
let found = tree.parallel_find(23);
let doubled_tree = tree.parallel_map(&|x| x * 2);
```

### 3. Producer-Consumer Pattern

Generate and process data in parallel.

```rust
use rayon::prelude::*;
use std::sync::mpsc;
use std::thread;
use std::time::Duration;

// Parallel data processing pipeline
fn parallel_pipeline() {
    let (tx, rx) = mpsc::channel();
    
    // Producer thread
    thread::spawn(move || {
        for i in 0..1000 {
            tx.send(i).unwrap();
            thread::sleep(Duration::from_millis(1)); // Simulate work
        }
    });
    
    // Collect data from producer
    let data: Vec<i32> = rx.iter().collect();
    
    // Parallel processing
    let results: Vec<String> = data
        .par_iter()
        .map(|&x| expensive_processing(x))
        .collect();
    
    println!("Processed {} items", results.len());
}

fn expensive_processing(x: i32) -> String {
    // Simulate expensive computation
    thread::sleep(Duration::from_millis(10));
    format!("processed_{}", x)
}

// Parallel batch processing
fn parallel_batch_processing<T, R, F>(
    data: Vec<T>,
    batch_size: usize,
    processor: F,
) -> Vec<R>
where
    T: Send,
    R: Send,
    F: Fn(&[T]) -> Vec<R> + Sync,
{
    data.par_chunks(batch_size)
        .flat_map(|chunk| processor(chunk))
        .collect()
}

// Usage
let data: Vec<i32> = (0..10000).collect();
let results = parallel_batch_processing(data, 100, |batch| {
    batch.iter().map(|&x| x * x).collect()
});
```

### 4. Parallel Graph Algorithms

Process graph structures efficiently.

```rust
use rayon::prelude::*;
use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone)]
struct Graph {
    adjacency_list: HashMap<usize, Vec<usize>>,
}

impl Graph {
    fn new() -> Self {
        Graph {
            adjacency_list: HashMap::new(),
        }
    }
    
    fn add_edge(&mut self, from: usize, to: usize) {
        self.adjacency_list.entry(from).or_insert_with(Vec::new).push(to);
        self.adjacency_list.entry(to).or_insert_with(Vec::new); // Ensure node exists
    }
    
    // Parallel breadth-first search
    fn parallel_bfs(&self, start: usize) -> Vec<usize> {
        let visited = Arc::new(Mutex::new(HashSet::new()));
        let mut current_level = vec![start];
        let mut result = Vec::new();
        
        visited.lock().unwrap().insert(start);
        
        while !current_level.is_empty() {
            result.extend(current_level.iter());
            
            let next_level: Vec<usize> = current_level
                .par_iter()
                .flat_map(|&node| {
                    self.adjacency_list.get(&node).unwrap_or(&Vec::new())
                        .par_iter()
                        .filter_map(|&neighbor| {
                            let mut visited_guard = visited.lock().unwrap();
                            if visited_guard.insert(neighbor) {
                                Some(neighbor)
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>()
                })
                .collect();
            
            current_level = next_level;
        }
        
        result
    }
    
    // Parallel shortest path (simplified)
    fn parallel_shortest_paths(&self, start: usize) -> HashMap<usize, usize> {
        let distances = Arc::new(Mutex::new(HashMap::new()));
        distances.lock().unwrap().insert(start, 0);
        
        let mut current_distance = 0;
        let mut current_nodes = vec![start];
        
        while !current_nodes.is_empty() {
            let next_nodes: Vec<usize> = current_nodes
                .par_iter()
                .flat_map(|&node| {
                    self.adjacency_list.get(&node).unwrap_or(&Vec::new())
                        .par_iter()
                        .filter_map(|&neighbor| {
                            let mut distances_guard = distances.lock().unwrap();
                            if !distances_guard.contains_key(&neighbor) {
                                distances_guard.insert(neighbor, current_distance + 1);
                                Some(neighbor)
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>()
                })
                .collect();
            
            current_nodes = next_nodes;
            current_distance += 1;
        }
        
        Arc::try_unwrap(distances).unwrap().into_inner().unwrap()
    }
}

// Build sample graph
fn build_sample_graph() -> Graph {
    let mut graph = Graph::new();
    
    // Create a simple graph
    for i in 0..100 {
        for j in 0..3 {
            let neighbor = (i + j + 1) % 100;
            graph.add_edge(i, neighbor);
        }
    }
    
    graph
}
```

## Specialized Algorithms

### 1. Parallel Matrix Operations

```rust
use rayon::prelude::*;

#[derive(Debug, Clone)]
struct Matrix {
    data: Vec<Vec<f64>>,
    rows: usize,
    cols: usize,
}

impl Matrix {
    fn new(rows: usize, cols: usize) -> Self {
        Matrix {
            data: vec![vec![0.0; cols]; rows],
            rows,
            cols,
        }
    }
    
    fn from_data(data: Vec<Vec<f64>>) -> Self {
        let rows = data.len();
        let cols = data[0].len();
        Matrix { data, rows, cols }
    }
    
    // Parallel matrix multiplication
    fn parallel_multiply(&self, other: &Matrix) -> Option<Matrix> {
        if self.cols != other.rows {
            return None;
        }
        
        let mut result = Matrix::new(self.rows, other.cols);
        
        result.data.par_iter_mut().enumerate().for_each(|(i, row)| {
            for j in 0..other.cols {
                row[j] = (0..self.cols)
                    .map(|k| self.data[i][k] * other.data[k][j])
                    .sum();
            }
        });
        
        Some(result)
    }
    
    // Parallel element-wise operations
    fn parallel_add(&self, other: &Matrix) -> Option<Matrix> {
        if self.rows != other.rows || self.cols != other.cols {
            return None;
        }
        
        let result_data: Vec<Vec<f64>> = self.data
            .par_iter()
            .zip(&other.data)
            .map(|(row1, row2)| {
                row1.par_iter()
                    .zip(row2)
                    .map(|(&a, &b)| a + b)
                    .collect()
            })
            .collect();
        
        Some(Matrix::from_data(result_data))
    }
    
    // Parallel transpose
    fn parallel_transpose(&self) -> Matrix {
        let mut result = Matrix::new(self.cols, self.rows);
        
        result.data.par_iter_mut().enumerate().for_each(|(i, row)| {
            for j in 0..self.rows {
                row[j] = self.data[j][i];
            }
        });
        
        result
    }
    
    // Parallel row reduction
    fn parallel_row_sums(&self) -> Vec<f64> {
        self.data
            .par_iter()
            .map(|row| row.iter().sum())
            .collect()
    }
}

// Usage
fn matrix_example() {
    let a = Matrix::from_data(vec![
        vec![1.0, 2.0, 3.0],
        vec![4.0, 5.0, 6.0],
    ]);
    
    let b = Matrix::from_data(vec![
        vec![7.0, 8.0],
        vec![9.0, 10.0],
        vec![11.0, 12.0],
    ]);
    
    if let Some(result) = a.parallel_multiply(&b) {
        println!("Matrix multiplication result: {:?}", result);
    }
    
    let row_sums = a.parallel_row_sums();
    println!("Row sums: {:?}", row_sums);
}
```

### 2. Parallel String Processing

```rust
use rayon::prelude::*;
use std::collections::HashMap;

// Parallel text analysis
fn parallel_text_analysis(text: &str) -> TextStats {
    let words: Vec<&str> = text.par_split_whitespace().collect();
    
    // Parallel word frequency counting
    let word_freq: HashMap<String, usize> = words
        .par_iter()
        .map(|word| word.to_lowercase())
        .fold(
            || HashMap::new(),
            |mut map, word| {
                *map.entry(word).or_insert(0) += 1;
                map
            }
        )
        .reduce(
            || HashMap::new(),
            |mut map1, map2| {
                for (word, count) in map2 {
                    *map1.entry(word).or_insert(0) += count;
                }
                map1
            }
        );
    
    // Parallel character analysis
    let char_stats: CharStats = text
        .par_chars()
        .fold(
            || CharStats::default(),
            |mut stats, ch| {
                stats.total += 1;
                if ch.is_alphabetic() { stats.letters += 1; }
                if ch.is_numeric() { stats.digits += 1; }
                if ch.is_whitespace() { stats.whitespace += 1; }
                stats
            }
        )
        .reduce(
            || CharStats::default(),
            |mut stats1, stats2| {
                stats1.total += stats2.total;
                stats1.letters += stats2.letters;
                stats1.digits += stats2.digits;
                stats1.whitespace += stats2.whitespace;
                stats1
            }
        );
    
    TextStats {
        word_count: words.len(),
        unique_words: word_freq.len(),
        word_frequencies: word_freq,
        char_stats,
    }
}

#[derive(Debug, Default)]
struct CharStats {
    total: usize,
    letters: usize,
    digits: usize,
    whitespace: usize,
}

#[derive(Debug)]
struct TextStats {
    word_count: usize,
    unique_words: usize,
    word_frequencies: HashMap<String, usize>,
    char_stats: CharStats,
}

// Parallel string matching
fn parallel_string_search(text: &str, patterns: &[&str]) -> Vec<(String, Vec<usize>)> {
    patterns
        .par_iter()
        .map(|&pattern| {
            let positions: Vec<usize> = text
                .par_char_indices()
                .filter_map(|(i, _)| {
                    if text[i..].starts_with(pattern) {
                        Some(i)
                    } else {
                        None
                    }
                })
                .collect();
            
            (pattern.to_string(), positions)
        })
        .collect()
}

// Parallel text transformation
fn parallel_text_transform(text: &str) -> String {
    text.par_lines()
        .map(|line| {
            line.par_split_whitespace()
                .map(|word| {
                    if word.len() > 3 {
                        format!("{}***", &word[..3])
                    } else {
                        word.to_string()
                    }
                })
                .collect::<Vec<_>>()
                .join(" ")
        })
        .collect::<Vec<_>>()
        .join("\n")
}
```

### 3. Parallel Numerical Algorithms

```rust
use rayon::prelude::*;

// Parallel Monte Carlo estimation
fn parallel_monte_carlo_pi(samples: usize) -> f64 {
    let inside_circle: usize = (0..samples)
        .into_par_iter()
        .map(|_| {
            let x: f64 = rand::random::<f64>() * 2.0 - 1.0;
            let y: f64 = rand::random::<f64>() * 2.0 - 1.0;
            if x * x + y * y <= 1.0 { 1 } else { 0 }
        })
        .sum();
    
    4.0 * inside_circle as f64 / samples as f64
}

// Parallel numerical integration
fn parallel_integrate<F>(f: F, a: f64, b: f64, n: usize) -> f64
where
    F: Fn(f64) -> f64 + Sync,
{
    let h = (b - a) / n as f64;
    
    (0..n)
        .into_par_iter()
        .map(|i| {
            let x = a + i as f64 * h;
            f(x) * h
        })
        .sum()
}

// Parallel root finding (bisection method)
fn parallel_find_roots<F>(f: F, intervals: Vec<(f64, f64)>, tolerance: f64) -> Vec<f64>
where
    F: Fn(f64) -> f64 + Sync,
{
    intervals
        .into_par_iter()
        .filter_map(|(mut a, mut b)| {
            if f(a) * f(b) > 0.0 {
                return None; // No root in interval
            }
            
            while (b - a).abs() > tolerance {
                let c = (a + b) / 2.0;
                if f(a) * f(c) < 0.0 {
                    b = c;
                } else {
                    a = c;
                }
            }
            
            Some((a + b) / 2.0)
        })
        .collect()
}

// Usage examples
fn numerical_examples() {
    // Monte Carlo Pi estimation
    let pi_estimate = parallel_monte_carlo_pi(1_000_000);
    println!("Pi estimate: {:.6}", pi_estimate);
    
    // Numerical integration of x^2 from 0 to 1
    let integral = parallel_integrate(|x| x * x, 0.0, 1.0, 1_000_000);
    println!("Integral of x^2 from 0 to 1: {:.6}", integral);
    
    // Find roots of x^2 - 2 in multiple intervals
    let intervals = vec![(-2.0, 0.0), (0.0, 2.0), (1.0, 2.0)];
    let roots = parallel_find_roots(|x| x * x - 2.0, intervals, 1e-10);
    println!("Roots of x^2 - 2: {:?}", roots);
}
```

## Performance Optimization Patterns

### 1. Chunking for Better Cache Locality

```rust
use rayon::prelude::*;

// Process data in chunks for better cache performance
fn optimized_processing(data: &[f64]) -> Vec<f64> {
    let chunk_size = 1000; // Tune based on cache size
    
    data.par_chunks(chunk_size)
        .flat_map(|chunk| {
            // Process entire chunk sequentially for cache efficiency
            chunk.iter().map(|&x| expensive_function(x)).collect::<Vec<_>>()
        })
        .collect()
}

fn expensive_function(x: f64) -> f64 {
    // Simulate expensive computation
    (0..100).fold(x, |acc, i| acc + (i as f64).sin())
}

// Adaptive chunking based on work complexity
fn adaptive_chunking<T, R, F>(data: Vec<T>, min_chunk_size: usize, processor: F) -> Vec<R>
where
    T: Send,
    R: Send,
    F: Fn(&T) -> R + Sync,
{
    let num_threads = rayon::current_num_threads();
    let chunk_size = std::cmp::max(min_chunk_size, data.len() / (num_threads * 4));
    
    data.par_chunks(chunk_size)
        .flat_map(|chunk| {
            chunk.iter().map(&processor).collect::<Vec<_>>()
        })
        .collect()
}
```

### 2. Memory-Efficient Patterns

```rust
use rayon::prelude::*;

// Avoid collecting intermediate results
fn memory_efficient_pipeline(data: &[i32]) -> i32 {
    data.par_iter()
        .filter(|&&x| x % 2 == 0)
        .map(|&x| x * x)
        .filter(|&x| x > 100)
        .sum() // Direct reduction without collecting
}

// Use iterators instead of collecting vectors
fn streaming_processing<T, R>(
    data: impl ParallelIterator<Item = T>,
    processor: impl Fn(T) -> R + Sync,
) -> impl ParallelIterator<Item = R>
where
    T: Send,
    R: Send,
{
    data.map(processor) // Return iterator, don't collect
}

// Process data in place when possible
fn in_place_transformation(data: &mut [f64]) {
    data.par_iter_mut().for_each(|x| {
        *x = x.sqrt(); // Modify in place
    });
}
```

### 3. Load Balancing Patterns

```rust
use rayon::prelude::*;

// Dynamic work distribution for uneven workloads
fn dynamic_load_balancing(tasks: Vec<Task>) -> Vec<Result> {
    // Sort by estimated complexity (largest first)
    let mut sorted_tasks = tasks;
    sorted_tasks.par_sort_by_key(|task| std::cmp::Reverse(task.estimated_complexity()));
    
    sorted_tasks
        .into_par_iter()
        .map(|task| process_task(task))
        .collect()
}

#[derive(Debug)]
struct Task {
    id: usize,
    complexity: usize,
}

impl Task {
    fn estimated_complexity(&self) -> usize {
        self.complexity
    }
}

#[derive(Debug)]
struct Result {
    task_id: usize,
    output: String,
}

fn process_task(task: Task) -> Result {
    // Simulate variable work
    std::thread::sleep(std::time::Duration::from_millis(task.complexity as u64));
    Result {
        task_id: task.id,
        output: format!("processed_{}", task.id),
    }
}

// Work stealing with manual job distribution
fn manual_work_distribution<T, R>(
    data: Vec<T>,
    processor: impl Fn(T) -> R + Sync,
) -> Vec<R>
where
    T: Send,
    R: Send,
{
    use rayon::iter::plumbing::*;
    
    // Use Rayon's internal work-stealing more explicitly
    data.into_par_iter()
        .map(processor)
        .collect()
}
```

## Error Handling in Parallel Algorithms

### 1. Graceful Error Propagation

```rust
use rayon::prelude::*;

// Collect all errors instead of stopping at first
fn collect_all_errors(data: Vec<i32>) -> (Vec<String>, Vec<String>) {
    let results: Vec<Result<String, String>> = data
        .into_par_iter()
        .map(|x| {
            if x % 7 == 0 {
                Err(format!("Error processing {}", x))
            } else {
                Ok(format!("Success: {}", x))
            }
        })
        .collect();
    
    let (successes, errors): (Vec<_>, Vec<_>) = results
        .into_iter()
        .partition(Result::is_ok);
    
    let successes: Vec<String> = successes.into_iter().map(Result::unwrap).collect();
    let errors: Vec<String> = errors.into_iter().map(Result::unwrap_err).collect();
    
    (successes, errors)
}

// Early termination on first error
fn fail_fast_processing(data: Vec<i32>) -> Result<Vec<String>, String> {
    data.into_par_iter()
        .map(|x| {
            if x % 7 == 0 {
                Err(format!("Error processing {}", x))
            } else {
                Ok(format!("Success: {}", x))
            }
        })
        .collect() // This will stop on first error
}

// Partial success with error reporting
fn partial_success_processing(data: Vec<i32>) -> (Vec<String>, usize) {
    let results: Vec<Option<String>> = data
        .into_par_iter()
        .map(|x| {
            if x % 7 == 0 {
                None // Error case
            } else {
                Some(format!("Success: {}", x))
            }
        })
        .collect();
    
    let error_count = results.iter().filter(|r| r.is_none()).count();
    let successes: Vec<String> = results.into_iter().flatten().collect();
    
    (successes, error_count)
}
```

## Best Practices Summary

### 1. Choose the Right Pattern

- **Map**: Independent transformations
- **Filter**: Selection based on predicates  
- **Reduce**: Combining all elements
- **Fold**: Accumulating into different types
- **Divide and Conquer**: Recursive problem decomposition

### 2. Optimize for Your Data

- Use chunking for cache efficiency
- Consider memory layout and access patterns
- Balance granularity vs overhead
- Profile to identify bottlenecks

### 3. Handle Errors Appropriately

- Decide between fail-fast vs collect-all-errors
- Use appropriate error types
- Consider partial success scenarios
- Provide meaningful error messages

### 4. Consider Load Balancing

- Sort by complexity for uneven workloads
- Use dynamic work distribution
- Monitor thread utilization
- Adjust chunk sizes based on profiling

## Next Steps

Now that you understand parallel algorithms and patterns:

- [Performance Tuning](05-performance.md) - Optimize your parallel code
- [Debugging](06-debugging.md) - Troubleshoot parallel execution
- [Custom Iterators](08-custom-iterators.md) - Build your own parallel iterators