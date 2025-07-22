# Creating Custom Parallel Iterators

## Introduction

This guide covers how to build your own parallel iterators using Rayon's plumbing infrastructure. Understanding these concepts allows you to create efficient parallel abstractions for domain-specific problems.

## Understanding Parallel Iterator Traits

### Core Traits

```rust
use rayon::prelude::*;
use rayon::iter::plumbing::*;

// The main trait for parallel iterators
pub trait ParallelIterator: Sized + Send {
    type Item: Send;
    
    fn drive_unindexed<C>(self, consumer: C) -> C::Result
    where
        C: UnindexedConsumer<Self::Item>;
}

// For iterators with known length
pub trait IndexedParallelIterator: ParallelIterator {
    fn len(&self) -> usize;
    
    fn drive<C>(self, consumer: C) -> C::Result
    where
        C: Consumer<Self::Item>;
}
```

### Consumer Traits

```rust
// Consumers process items from parallel iterators
pub trait Consumer<Item>: Send + Sized {
    type Folder: Folder<Item, Result = Self::Result>;
    type Reducer: Reducer<Self::Result>;
    type Result: Send;
    
    fn split_at(self, index: usize) -> (Self, Self, Self::Reducer);
    fn into_folder(self) -> Self::Folder;
    fn full(&self) -> bool;
}

// Folders accumulate items sequentially
pub trait Folder<Item>: Sized {
    type Result;
    
    fn consume(self, item: Item) -> Self;
    fn consume_iter<I>(mut self, iter: I) -> Self
    where
        I: IntoIterator<Item = Item>,
    {
        for item in iter {
            self = self.consume(item);
        }
        self
    }
    fn complete(self) -> Self::Result;
    fn full(&self) -> bool;
}

// Reducers combine results from different folders
pub trait Reducer<Result> {
    fn reduce(self, left: Result, right: Result) -> Result;
}
```

## Simple Custom Iterator

### Basic Range Iterator

```rust
use rayon::prelude::*;
use rayon::iter::plumbing::*;

#[derive(Debug, Clone)]
pub struct CustomRange {
    start: usize,
    end: usize,
}

impl CustomRange {
    pub fn new(start: usize, end: usize) -> Self {
        CustomRange { start, end }
    }
}

impl ParallelIterator for CustomRange {
    type Item = usize;
    
    fn drive_unindexed<C>(self, consumer: C) -> C::Result
    where
        C: UnindexedConsumer<Self::Item>,
    {
        bridge(self, consumer)
    }
}

impl IndexedParallelIterator for CustomRange {
    fn len(&self) -> usize {
        self.end.saturating_sub(self.start)
    }
    
    fn drive<C>(self, consumer: C) -> C::Result
    where
        C: Consumer<Self::Item>,
    {
        bridge(self, consumer)
    }
}

impl Producer for CustomRange {
    type Item = usize;
    type IntoIter = std::ops::Range<usize>;
    
    fn into_iter(self) -> Self::IntoIter {
        self.start..self.end
    }
    
    fn split_at(self, index: usize) -> (Self, Self) {
        let mid = self.start + index;
        (
            CustomRange::new(self.start, mid),
            CustomRange::new(mid, self.end),
        )
    }
}

// Usage
fn test_custom_range() {
    let sum: usize = CustomRange::new(0, 1000)
        .map(|x| x * x)
        .sum();
    
    println!("Sum of squares: {}", sum);
}
```

### Custom Map Iterator

```rust
use rayon::prelude::*;
use rayon::iter::plumbing::*;

pub struct CustomMap<I, F> {
    base: I,
    map_fn: F,
}

impl<I, F> CustomMap<I, F> {
    pub fn new(base: I, map_fn: F) -> Self {
        CustomMap { base, map_fn }
    }
}

impl<I, F, R> ParallelIterator for CustomMap<I, F>
where
    I: ParallelIterator,
    F: Fn(I::Item) -> R + Sync + Send,
    R: Send,
{
    type Item = R;
    
    fn drive_unindexed<C>(self, consumer: C) -> C::Result
    where
        C: UnindexedConsumer<Self::Item>,
    {
        let consumer = MapConsumer::new(consumer, &self.map_fn);
        self.base.drive_unindexed(consumer)
    }
}

impl<I, F, R> IndexedParallelIterator for CustomMap<I, F>
where
    I: IndexedParallelIterator,
    F: Fn(I::Item) -> R + Sync + Send,
    R: Send,
{
    fn len(&self) -> usize {
        self.base.len()
    }
    
    fn drive<C>(self, consumer: C) -> C::Result
    where
        C: Consumer<Self::Item>,
    {
        let consumer = MapConsumer::new(consumer, &self.map_fn);
        self.base.drive(consumer)
    }
}

// Consumer for map operation
struct MapConsumer<C, F> {
    base: C,
    map_fn: F,
}

impl<C, F> MapConsumer<C, F> {
    fn new(base: C, map_fn: F) -> Self {
        MapConsumer { base, map_fn }
    }
}

impl<T, R, C, F> Consumer<T> for MapConsumer<C, F>
where
    C: Consumer<R>,
    F: Fn(T) -> R + Sync,
{
    type Folder = MapFolder<C::Folder, F>;
    type Reducer = C::Reducer;
    type Result = C::Result;
    
    fn split_at(self, index: usize) -> (Self, Self, Self::Reducer) {
        let (left, right, reducer) = self.base.split_at(index);
        (
            MapConsumer::new(left, self.map_fn),
            MapConsumer::new(right, self.map_fn),
            reducer,
        )
    }
    
    fn into_folder(self) -> Self::Folder {
        MapFolder {
            base: self.base.into_folder(),
            map_fn: self.map_fn,
        }
    }
    
    fn full(&self) -> bool {
        self.base.full()
    }
}

impl<T, R, C, F> UnindexedConsumer<T> for MapConsumer<C, F>
where
    C: UnindexedConsumer<R>,
    F: Fn(T) -> R + Sync,
{
    fn split_off_left(&self) -> Self {
        MapConsumer::new(self.base.split_off_left(), self.map_fn)
    }
    
    fn to_reducer(&self) -> Self::Reducer {
        self.base.to_reducer()
    }
}

// Folder for map operation
struct MapFolder<F, M> {
    base: F,
    map_fn: M,
}

impl<T, R, F, M> Folder<T> for MapFolder<F, M>
where
    F: Folder<R>,
    M: Fn(T) -> R,
{
    type Result = F::Result;
    
    fn consume(self, item: T) -> Self {
        MapFolder {
            base: self.base.consume((self.map_fn)(item)),
            map_fn: self.map_fn,
        }
    }
    
    fn complete(self) -> Self::Result {
        self.base.complete()
    }
    
    fn full(&self) -> bool {
        self.base.full()
    }
}
```

## Advanced Custom Iterators

### Tree Traversal Iterator

```rust
use rayon::prelude::*;
use rayon::iter::plumbing::*;
use std::collections::VecDeque;

#[derive(Debug, Clone)]
pub struct TreeNode<T> {
    pub value: T,
    pub children: Vec<TreeNode<T>>,
}

impl<T> TreeNode<T> {
    pub fn new(value: T) -> Self {
        TreeNode {
            value,
            children: Vec::new(),
        }
    }
    
    pub fn add_child(&mut self, child: TreeNode<T>) {
        self.children.push(child);
    }
}

pub struct ParallelTreeIterator<T> {
    nodes: Vec<TreeNode<T>>,
}

impl<T> ParallelTreeIterator<T> {
    pub fn new(root: TreeNode<T>) -> Self {
        ParallelTreeIterator {
            nodes: vec![root],
        }
    }
}

impl<T: Send> ParallelIterator for ParallelTreeIterator<T> {
    type Item = T;
    
    fn drive_unindexed<C>(self, consumer: C) -> C::Result
    where
        C: UnindexedConsumer<Self::Item>,
    {
        // Flatten tree into a vector for parallel processing
        let items = flatten_tree(self.nodes);
        items.into_par_iter().drive_unindexed(consumer)
    }
}

fn flatten_tree<T>(nodes: Vec<TreeNode<T>>) -> Vec<T> {
    let mut result = Vec::new();
    let mut queue = VecDeque::from(nodes);
    
    while let Some(node) = queue.pop_front() {
        result.push(node.value);
        queue.extend(node.children);
    }
    
    result
}

// Usage
fn test_tree_iterator() {
    let mut root = TreeNode::new(1);
    root.add_child(TreeNode::new(2));
    root.add_child(TreeNode::new(3));
    
    let sum: i32 = ParallelTreeIterator::new(root)
        .map(|x| x * x)
        .sum();
    
    println!("Sum of squares: {}", sum);
}
```

### Chunked Iterator

```rust
use rayon::prelude::*;
use rayon::iter::plumbing::*;

pub struct ChunkedIterator<I> {
    base: I,
    chunk_size: usize,
}

impl<I> ChunkedIterator<I> {
    pub fn new(base: I, chunk_size: usize) -> Self {
        ChunkedIterator { base, chunk_size }
    }
}

impl<I> ParallelIterator for ChunkedIterator<I>
where
    I: IndexedParallelIterator,
{
    type Item = Vec<I::Item>;
    
    fn drive_unindexed<C>(self, consumer: C) -> C::Result
    where
        C: UnindexedConsumer<Self::Item>,
    {
        bridge(self, consumer)
    }
}

impl<I> IndexedParallelIterator for ChunkedIterator<I>
where
    I: IndexedParallelIterator,
{
    fn len(&self) -> usize {
        (self.base.len() + self.chunk_size - 1) / self.chunk_size
    }
    
    fn drive<C>(self, consumer: C) -> C::Result
    where
        C: Consumer<Self::Item>,
    {
        bridge(self, consumer)
    }
}

impl<I> Producer for ChunkedIterator<I>
where
    I: IndexedParallelIterator,
{
    type Item = Vec<I::Item>;
    type IntoIter = ChunkedSeqIterator<I::IntoIter>;
    
    fn into_iter(self) -> Self::IntoIter {
        ChunkedSeqIterator {
            base: self.base.into_iter(),
            chunk_size: self.chunk_size,
        }
    }
    
    fn split_at(self, index: usize) -> (Self, Self) {
        let split_point = index * self.chunk_size;
        let (left, right) = self.base.split_at(split_point);
        
        (
            ChunkedIterator::new(left, self.chunk_size),
            ChunkedIterator::new(right, self.chunk_size),
        )
    }
}

pub struct ChunkedSeqIterator<I> {
    base: I,
    chunk_size: usize,
}

impl<I> Iterator for ChunkedSeqIterator<I>
where
    I: Iterator,
{
    type Item = Vec<I::Item>;
    
    fn next(&mut self) -> Option<Self::Item> {
        let mut chunk = Vec::with_capacity(self.chunk_size);
        
        for _ in 0..self.chunk_size {
            if let Some(item) = self.base.next() {
                chunk.push(item);
            } else {
                break;
            }
        }
        
        if chunk.is_empty() {
            None
        } else {
            Some(chunk)
        }
    }
}

// Extension trait for easy use
pub trait ParallelIteratorExt: ParallelIterator + Sized {
    fn chunked(self, chunk_size: usize) -> ChunkedIterator<Self>
    where
        Self: IndexedParallelIterator,
    {
        ChunkedIterator::new(self, chunk_size)
    }
}

impl<I: ParallelIterator> ParallelIteratorExt for I {}

// Usage
fn test_chunked_iterator() {
    let data: Vec<i32> = (0..1000).collect();
    
    let chunk_sums: Vec<i32> = data
        .into_par_iter()
        .chunked(100)
        .map(|chunk| chunk.into_iter().sum())
        .collect();
    
    println!("Chunk sums: {:?}", &chunk_sums[..5]);
}
```

## Custom Consumers and Folders

### Custom Collector

```rust
use rayon::prelude::*;
use rayon::iter::plumbing::*;
use std::collections::HashMap;

// Custom consumer for collecting into a HashMap
pub struct HashMapConsumer<K, V> {
    _phantom: std::marker::PhantomData<(K, V)>,
}

impl<K, V> HashMapConsumer<K, V> {
    pub fn new() -> Self {
        HashMapConsumer {
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<K, V> Consumer<(K, V)> for HashMapConsumer<K, V>
where
    K: Eq + std::hash::Hash + Send,
    V: Send,
{
    type Folder = HashMapFolder<K, V>;
    type Reducer = HashMapReducer<K, V>;
    type Result = HashMap<K, V>;
    
    fn split_at(self, _index: usize) -> (Self, Self, Self::Reducer) {
        (
            HashMapConsumer::new(),
            HashMapConsumer::new(),
            HashMapReducer::new(),
        )
    }
    
    fn into_folder(self) -> Self::Folder {
        HashMapFolder::new()
    }
    
    fn full(&self) -> bool {
        false
    }
}

impl<K, V> UnindexedConsumer<(K, V)> for HashMapConsumer<K, V>
where
    K: Eq + std::hash::Hash + Send,
    V: Send,
{
    fn split_off_left(&self) -> Self {
        HashMapConsumer::new()
    }
    
    fn to_reducer(&self) -> Self::Reducer {
        HashMapReducer::new()
    }
}

pub struct HashMapFolder<K, V> {
    map: HashMap<K, V>,
}

impl<K, V> HashMapFolder<K, V> {
    fn new() -> Self {
        HashMapFolder {
            map: HashMap::new(),
        }
    }
}

impl<K, V> Folder<(K, V)> for HashMapFolder<K, V>
where
    K: Eq + std::hash::Hash,
{
    type Result = HashMap<K, V>;
    
    fn consume(mut self, (key, value): (K, V)) -> Self {
        self.map.insert(key, value);
        self
    }
    
    fn complete(self) -> Self::Result {
        self.map
    }
    
    fn full(&self) -> bool {
        false
    }
}

pub struct HashMapReducer<K, V> {
    _phantom: std::marker::PhantomData<(K, V)>,
}

impl<K, V> HashMapReducer<K, V> {
    fn new() -> Self {
        HashMapReducer {
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<K, V> Reducer<HashMap<K, V>> for HashMapReducer<K, V>
where
    K: Eq + std::hash::Hash,
{
    fn reduce(self, mut left: HashMap<K, V>, right: HashMap<K, V>) -> HashMap<K, V> {
        left.extend(right);
        left
    }
}

// Extension trait for collecting into HashMap
pub trait CollectHashMap<K, V>: ParallelIterator<Item = (K, V)>
where
    K: Eq + std::hash::Hash + Send,
    V: Send,
{
    fn collect_hashmap(self) -> HashMap<K, V> {
        self.drive_unindexed(HashMapConsumer::new())
    }
}

impl<I, K, V> CollectHashMap<K, V> for I
where
    I: ParallelIterator<Item = (K, V)>,
    K: Eq + std::hash::Hash + Send,
    V: Send,
{
}

// Usage
fn test_hashmap_collector() {
    let data: Vec<i32> = (0..1000).collect();
    
    let map: HashMap<i32, String> = data
        .into_par_iter()
        .map(|x| (x, format!("value_{}", x)))
        .collect_hashmap();
    
    println!("Map size: {}", map.len());
    println!("Sample entries: {:?}", map.get(&42));
}
```

### Early Termination Consumer

```rust
use rayon::prelude::*;
use rayon::iter::plumbing::*;

// Consumer that stops early when a condition is met
pub struct FindConsumer<P> {
    predicate: P,
}

impl<P> FindConsumer<P> {
    pub fn new(predicate: P) -> Self {
        FindConsumer { predicate }
    }
}

impl<T, P> Consumer<T> for FindConsumer<P>
where
    P: Fn(&T) -> bool + Sync,
{
    type Folder = FindFolder<T, P>;
    type Reducer = FindReducer<T>;
    type Result = Option<T>;
    
    fn split_at(self, _index: usize) -> (Self, Self, Self::Reducer) {
        (
            FindConsumer::new(self.predicate),
            FindConsumer::new(self.predicate),
            FindReducer::new(),
        )
    }
    
    fn into_folder(self) -> Self::Folder {
        FindFolder::new(self.predicate)
    }
    
    fn full(&self) -> bool {
        false
    }
}

impl<T, P> UnindexedConsumer<T> for FindConsumer<P>
where
    P: Fn(&T) -> bool + Sync,
{
    fn split_off_left(&self) -> Self {
        FindConsumer::new(self.predicate)
    }
    
    fn to_reducer(&self) -> Self::Reducer {
        FindReducer::new()
    }
}

pub struct FindFolder<T, P> {
    predicate: P,
    found: Option<T>,
}

impl<T, P> FindFolder<T, P> {
    fn new(predicate: P) -> Self {
        FindFolder {
            predicate,
            found: None,
        }
    }
}

impl<T, P> Folder<T> for FindFolder<T, P>
where
    P: Fn(&T) -> bool,
{
    type Result = Option<T>;
    
    fn consume(mut self, item: T) -> Self {
        if self.found.is_none() && (self.predicate)(&item) {
            self.found = Some(item);
        }
        self
    }
    
    fn complete(self) -> Self::Result {
        self.found
    }
    
    fn full(&self) -> bool {
        self.found.is_some()
    }
}

pub struct FindReducer<T> {
    _phantom: std::marker::PhantomData<T>,
}

impl<T> FindReducer<T> {
    fn new() -> Self {
        FindReducer {
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<T> Reducer<Option<T>> for FindReducer<T> {
    fn reduce(self, left: Option<T>, right: Option<T>) -> Option<T> {
        left.or(right)
    }
}

// Extension trait
pub trait FindAnyExt<T>: ParallelIterator<Item = T> {
    fn find_any_custom<P>(self, predicate: P) -> Option<T>
    where
        P: Fn(&T) -> bool + Sync,
    {
        self.drive_unindexed(FindConsumer::new(predicate))
    }
}

impl<I: ParallelIterator> FindAnyExt<I::Item> for I {}

// Usage
fn test_find_consumer() {
    let data: Vec<i32> = (0..1000000).collect();
    
    let found = data
        .into_par_iter()
        .find_any_custom(|&x| x > 500000 && x % 7 == 0);
    
    println!("Found: {:?}", found);
}
```

## Performance Optimization Techniques

### Adaptive Splitting

```rust
use rayon::prelude::*;
use rayon::iter::plumbing::*;

pub struct AdaptiveRange {
    start: usize,
    end: usize,
    min_chunk_size: usize,
}

impl AdaptiveRange {
    pub fn new(start: usize, end: usize, min_chunk_size: usize) -> Self {
        AdaptiveRange {
            start,
            end,
            min_chunk_size,
        }
    }
}

impl ParallelIterator for AdaptiveRange {
    type Item = usize;
    
    fn drive_unindexed<C>(self, consumer: C) -> C::Result
    where
        C: UnindexedConsumer<Self::Item>,
    {
        bridge(self, consumer)
    }
}

impl IndexedParallelIterator for AdaptiveRange {
    fn len(&self) -> usize {
        self.end.saturating_sub(self.start)
    }
    
    fn drive<C>(self, consumer: C) -> C::Result
    where
        C: Consumer<Self::Item>,
    {
        bridge(self, consumer)
    }
}

impl Producer for AdaptiveRange {
    type Item = usize;
    type IntoIter = std::ops::Range<usize>;
    
    fn into_iter(self) -> Self::IntoIter {
        self.start..self.end
    }
    
    fn split_at(self, index: usize) -> (Self, Self) {
        let mid = self.start + index;
        (
            AdaptiveRange::new(self.start, mid, self.min_chunk_size),
            AdaptiveRange::new(mid, self.end, self.min_chunk_size),
        )
    }
    
    fn min_len(&self) -> usize {
        self.min_chunk_size
    }
    
    fn max_len(&self) -> usize {
        // Limit maximum chunk size for better load balancing
        std::cmp::min(self.len(), self.min_chunk_size * 4)
    }
}

// Usage with adaptive splitting
fn test_adaptive_range() {
    let sum: usize = AdaptiveRange::new(0, 1000000, 1000)
        .map(|x| expensive_computation(x))
        .sum();
    
    println!("Sum: {}", sum);
}

fn expensive_computation(x: usize) -> usize {
    (0..100).fold(x, |acc, i| acc.wrapping_mul(i + 1))
}
```

### Memory-Efficient Iterator

```rust
use rayon::prelude::*;
use rayon::iter::plumbing::*;

// Iterator that processes data in-place to minimize memory usage
pub struct InPlaceMap<I, F> {
    base: I,
    map_fn: F,
}

impl<I, F> InPlaceMap<I, F> {
    pub fn new(base: I, map_fn: F) -> Self {
        InPlaceMap { base, map_fn }
    }
}

impl<I, F> ParallelIterator for InPlaceMap<I, F>
where
    I: ParallelIterator,
    F: Fn(I::Item) -> I::Item + Sync + Send,
{
    type Item = I::Item;
    
    fn drive_unindexed<C>(self, consumer: C) -> C::Result
    where
        C: UnindexedConsumer<Self::Item>,
    {
        let consumer = InPlaceMapConsumer::new(consumer, self.map_fn);
        self.base.drive_unindexed(consumer)
    }
}

struct InPlaceMapConsumer<C, F> {
    base: C,
    map_fn: F,
}

impl<C, F> InPlaceMapConsumer<C, F> {
    fn new(base: C, map_fn: F) -> Self {
        InPlaceMapConsumer { base, map_fn }
    }
}

impl<T, C, F> Consumer<T> for InPlaceMapConsumer<C, F>
where
    C: Consumer<T>,
    F: Fn(T) -> T + Sync,
{
    type Folder = InPlaceMapFolder<C::Folder, F>;
    type Reducer = C::Reducer;
    type Result = C::Result;
    
    fn split_at(self, index: usize) -> (Self, Self, Self::Reducer) {
        let (left, right, reducer) = self.base.split_at(index);
        (
            InPlaceMapConsumer::new(left, self.map_fn),
            InPlaceMapConsumer::new(right, self.map_fn),
            reducer,
        )
    }
    
    fn into_folder(self) -> Self::Folder {
        InPlaceMapFolder {
            base: self.base.into_folder(),
            map_fn: self.map_fn,
        }
    }
    
    fn full(&self) -> bool {
        self.base.full()
    }
}

impl<T, C, F> UnindexedConsumer<T> for InPlaceMapConsumer<C, F>
where
    C: UnindexedConsumer<T>,
    F: Fn(T) -> T + Sync,
{
    fn split_off_left(&self) -> Self {
        InPlaceMapConsumer::new(self.base.split_off_left(), self.map_fn)
    }
    
    fn to_reducer(&self) -> Self::Reducer {
        self.base.to_reducer()
    }
}

struct InPlaceMapFolder<F, M> {
    base: F,
    map_fn: M,
}

impl<T, F, M> Folder<T> for InPlaceMapFolder<F, M>
where
    F: Folder<T>,
    M: Fn(T) -> T,
{
    type Result = F::Result;
    
    fn consume(self, item: T) -> Self {
        let mapped_item = (self.map_fn)(item);
        InPlaceMapFolder {
            base: self.base.consume(mapped_item),
            map_fn: self.map_fn,
        }
    }
    
    fn complete(self) -> Self::Result {
        self.base.complete()
    }
    
    fn full(&self) -> bool {
        self.base.full()
    }
}

// Extension trait
pub trait InPlaceMapExt: ParallelIterator + Sized {
    fn in_place_map<F>(self, f: F) -> InPlaceMap<Self, F>
    where
        F: Fn(Self::Item) -> Self::Item + Sync + Send,
    {
        InPlaceMap::new(self, f)
    }
}

impl<I: ParallelIterator> InPlaceMapExt for I {}
```

## Testing Custom Iterators

### Property-Based Testing

```rust
use rayon::prelude::*;

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_custom_range_equivalence() {
        let range = 0..1000;
        let custom_range = CustomRange::new(0, 1000);
        
        let std_sum: usize = range.sum();
        let custom_sum: usize = custom_range.sum();
        
        assert_eq!(std_sum, custom_sum);
    }
    
    #[test]
    fn test_chunked_iterator_preserves_elements() {
        let data: Vec<i32> = (0..1000).collect();
        let original_sum: i32 = data.iter().sum();
        
        let chunked_sum: i32 = data
            .into_par_iter()
            .chunked(100)
            .flat_map(|chunk| chunk.into_par_iter())
            .sum();
        
        assert_eq!(original_sum, chunked_sum);
    }
    
    #[test]
    fn test_hashmap_collector_correctness() {
        let data: Vec<i32> = (0..1000).collect();
        
        let map: std::collections::HashMap<i32, i32> = data
            .clone()
            .into_par_iter()
            .map(|x| (x, x * x))
            .collect_hashmap();
        
        for &x in &data {
            assert_eq!(map.get(&x), Some(&(x * x)));
        }
    }
    
    #[test]
    fn test_find_consumer_early_termination() {
        use std::sync::atomic::{AtomicUsize, Ordering};
        
        let counter = AtomicUsize::new(0);
        let data: Vec<i32> = (0..1000000).collect();
        
        let found = data
            .into_par_iter()
            .map(|x| {
                counter.fetch_add(1, Ordering::Relaxed);
                x
            })
            .find_any_custom(|&x| x == 100);
        
        assert_eq!(found, Some(100));
        
        // Should have processed fewer items than the total
        let processed = counter.load(Ordering::Relaxed);
        assert!(processed < 1000000);
        println!("Processed {} items to find 100", processed);
    }
}
```

### Benchmarking Custom Iterators

```rust
use rayon::prelude::*;
use std::time::Instant;

fn benchmark_custom_vs_builtin() {
    let data: Vec<usize> = (0..1000000).collect();
    
    // Benchmark built-in iterator
    let start = Instant::now();
    let builtin_sum: usize = data.par_iter().map(|&x| x * x).sum();
    let builtin_time = start.elapsed();
    
    // Benchmark custom iterator
    let start = Instant::now();
    let custom_sum: usize = CustomRange::new(0, 1000000)
        .map(|x| x * x)
        .sum();
    let custom_time = start.elapsed();
    
    assert_eq!(builtin_sum, custom_sum);
    
    println!("Built-in: {:?}", builtin_time);
    println!("Custom: {:?}", custom_time);
    println!("Ratio: {:.2}", 
             custom_time.as_nanos() as f64 / builtin_time.as_nanos() as f64);
}
```

## Best Practices

1. **Implement Both Traits**: Implement both `ParallelIterator` and `IndexedParallelIterator` when possible
2. **Efficient Splitting**: Ensure your `Producer::split_at` method creates balanced splits
3. **Minimize Overhead**: Keep consumer and folder operations lightweight
4. **Test Thoroughly**: Verify correctness and performance against built-in iterators
5. **Handle Edge Cases**: Consider empty iterators and single-element cases
6. **Memory Efficiency**: Avoid unnecessary allocations in hot paths
7. **Early Termination**: Implement `full()` correctly for consumers that can terminate early

## Summary

Creating custom parallel iterators allows you to:
- Optimize for specific data structures and access patterns
- Implement domain-specific parallel algorithms
- Control memory usage and allocation patterns
- Add specialized functionality not available in built-in iterators

The key is understanding the producer-consumer model and implementing the traits correctly for your specific use case.

## Next Steps

- [API Reference](09-api-reference.md) - Complete API documentation with examples
- Study Rayon's source code for more advanced patterns
- Experiment with different splitting strategies for your data structures