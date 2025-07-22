# Debugging and Profiling

This guide covers debugging and profiling techniques for Salsa 0.23, based on analysis of the actual implementation and debugging features.

## Event System

Salsa provides a comprehensive event system for monitoring query execution and performance.

### Event Types
```rust
// From src/event.rs
#[derive(Debug)]
pub enum EventKind {
    /// About to execute a query
    WillExecute {
        database_key: DatabaseKeyIndex,
    },
    
    /// Reused a memoized value instead of executing
    DidValidateMemoizedValue {
        database_key: DatabaseKeyIndex,
    },
    
    /// Will block waiting for another thread
    WillBlockOn {
        other_thread_id: ThreadId,
        database_key: DatabaseKeyIndex,
    },
    
    /// About to discard a stale output
    WillDiscardStaleOutput {
        execute_key: DatabaseKeyIndex,
        output_key: DatabaseKeyIndex,
    },
    
    /// Discarded a stale value
    DidDiscard {
        key: DatabaseKeyIndex,
    },
    
    /// Will iterate a cycle
    WillIterateCycle {
        database_key: DatabaseKeyIndex,
        iteration: u32,
    },
}
```

### Setting Up Event Logging
```rust
use salsa::{Database, Storage, Event, EventKind};
use std::sync::{Arc, Mutex};

#[derive(Clone)]
pub struct LoggingDatabase {
    storage: Storage<Self>,
    events: Arc<Mutex<Vec<Event>>>,
}

impl Default for LoggingDatabase {
    fn default() -> Self {
        let events = Arc::new(Mutex::new(Vec::new()));
        
        Self {
            storage: Storage::new(Some(Box::new({
                let events = events.clone();
                move |event| {
                    events.lock().unwrap().push(event);
                }
            }))),
            events,
        }
    }
}

impl LoggingDatabase {
    pub fn take_events(&self) -> Vec<Event> {
        std::mem::take(&mut *self.events.lock().unwrap())
    }
    
    pub fn print_events(&self) {
        let events = self.take_events();
        for event in events {
            println!("{:?}", event);
        }
    }
}

#[salsa::db]
impl Database for LoggingDatabase {}
```

### Selective Event Logging
```rust
impl Default for SelectiveLoggingDatabase {
    fn default() -> Self {
        let events = Arc::new(Mutex::new(Vec::new()));
        
        Self {
            storage: Storage::new(Some(Box::new({
                let events = events.clone();
                move |event| {
                    match event.kind {
                        // Only log execution and validation events
                        EventKind::WillExecute { .. } |
                        EventKind::DidValidateMemoizedValue { .. } => {
                            events.lock().unwrap().push(event);
                        }
                        _ => {}
                    }
                }
            }))),
            events,
        }
    }
}
```

## Query Debugging

### Debug Attributes
```rust
// Enable debug output for specific queries
#[salsa::input(debug)]
pub struct SourceFile {
    #[returns(ref)]
    pub text: String,
}

#[salsa::tracked(debug)]
fn parse_file(db: &dyn Database, file: SourceFile) -> Ast {
    // Debug info will be printed when this executes
    let text = file.text(db);
    parse_text(text)
}

#[salsa::tracked(debug)]
pub struct ParsedFile<'db> {
    #[tracked]
    pub ast: Ast,
}
```

### Manual Debug Logging
```rust
#[salsa::tracked]
fn complex_analysis(db: &dyn Database, file: SourceFile) -> Analysis {
    eprintln!("Starting analysis of {:?}", file);
    
    let ast = parse_file(db, file);
    eprintln!("Parsed AST with {} nodes", ast.node_count());
    
    let symbols = collect_symbols(db, ast);
    eprintln!("Found {} symbols", symbols.len());
    
    let result = perform_analysis(ast, symbols);
    eprintln!("Analysis complete: {:?}", result.summary());
    
    result
}
```

### Dependency Inspection
```rust
// Custom database trait for debugging
#[salsa::db]
pub trait DebugDatabase: Database {
    fn debug_dependencies(&self, key: DatabaseKeyIndex) {
        // This would require access to internal Salsa structures
        // In practice, use event logging to track dependencies
    }
}
```

## Performance Profiling

### Execution Time Tracking
```rust
use std::time::{Duration, Instant};
use std::collections::HashMap;

#[derive(Clone)]
pub struct ProfilingDatabase {
    storage: Storage<Self>,
    timings: Arc<Mutex<HashMap<String, Vec<Duration>>>>,
}

impl Default for ProfilingDatabase {
    fn default() -> Self {
        let timings = Arc::new(Mutex::new(HashMap::new()));
        
        Self {
            storage: Storage::new(Some(Box::new({
                let timings = timings.clone();
                move |event| {
                    match event.kind {
                        EventKind::WillExecute { database_key } => {
                            // Start timing
                            let start = Instant::now();
                            // Store start time (would need thread-local storage)
                        }
                        EventKind::DidValidateMemoizedValue { database_key } => {
                            // Record cache hit
                        }
                        _ => {}
                    }
                }
            }))),
            timings,
        }
    }
}

impl ProfilingDatabase {
    pub fn print_profile(&self) {
        let timings = self.timings.lock().unwrap();
        for (query, durations) in timings.iter() {
            let total: Duration = durations.iter().sum();
            let avg = total / durations.len() as u32;
            println!("{}: {} calls, total {:?}, avg {:?}", 
                     query, durations.len(), total, avg);
        }
    }
}
```

### Memory Usage Analysis
```rust
#[cfg(feature = "salsa_unstable")]
fn analyze_memory_usage(db: &dyn Database) {
    println!("=== Struct Memory Usage ===");
    let struct_info = db.structs_info();
    let mut total_structs = 0;
    let mut total_struct_memory = 0;
    
    for info in struct_info {
        let total_size = info.size_of_fields() + info.size_of_metadata();
        total_structs += info.count();
        total_struct_memory += total_size;
        
        println!("{}: {} instances, {} bytes total ({} fields + {} metadata)",
                 info.debug_name(),
                 info.count(),
                 total_size,
                 info.size_of_fields(),
                 info.size_of_metadata());
    }
    
    println!("Total structs: {} instances, {} bytes", total_structs, total_struct_memory);
    
    println!("\n=== Query Memory Usage ===");
    let query_info = db.queries_info();
    let mut total_queries = 0;
    let mut total_query_memory = 0;
    
    for (name, info) in query_info {
        let total_size = info.size_of_fields() + info.size_of_metadata();
        total_queries += info.count();
        total_query_memory += total_size;
        
        println!("{}: {} memos, {} bytes total ({} fields + {} metadata)",
                 name,
                 info.count(),
                 total_size,
                 info.size_of_fields(),
                 info.size_of_metadata());
    }
    
    println!("Total queries: {} memos, {} bytes", total_queries, total_query_memory);
    println!("Grand total: {} bytes", total_struct_memory + total_query_memory);
}
```

### Cache Hit Rate Analysis
```rust
#[derive(Default)]
pub struct CacheStats {
    executions: HashMap<String, u64>,
    cache_hits: HashMap<String, u64>,
}

impl CacheStats {
    pub fn record_execution(&mut self, query_name: String) {
        *self.executions.entry(query_name).or_insert(0) += 1;
    }
    
    pub fn record_cache_hit(&mut self, query_name: String) {
        *self.cache_hits.entry(query_name).or_insert(0) += 1;
    }
    
    pub fn print_stats(&self) {
        println!("=== Cache Hit Rates ===");
        for (query, executions) in &self.executions {
            let hits = self.cache_hits.get(query).unwrap_or(&0);
            let total = executions + hits;
            let hit_rate = if total > 0 { 
                (*hits as f64 / total as f64) * 100.0 
            } else { 
                0.0 
            };
            
            println!("{}: {:.1}% hit rate ({} hits, {} executions, {} total)",
                     query, hit_rate, hits, executions, total);
        }
    }
}
```

## Debugging Common Issues

### Infinite Loops and Cycles
```rust
#[salsa::tracked(cycle = recover)]
fn recursive_function(db: &dyn Database, node: Node) -> usize {
    let mut size = 1;
    for child in node.children(db) {
        size += recursive_function(db, child);  // Potential cycle
    }
    size
}

fn recover_recursive_function(
    db: &dyn Database,
    cycle: &salsa::Cycle,
    node: Node,
) -> usize {
    eprintln!("Cycle detected in recursive_function for node {:?}", node);
    eprintln!("Cycle participants: {:?}", cycle.participant_keys());
    
    // Return a reasonable default
    1
}
```

### Unexpected Recomputation
```rust
#[salsa::tracked]
fn debug_recomputation(db: &dyn Database, input: MyInput) -> MyOutput {
    eprintln!("Recomputing debug_recomputation for input {:?}", input);
    
    // Add debug logging to see what dependencies are accessed
    let dep1 = input.field1(db);
    eprintln!("Accessed field1: {:?}", dep1);
    
    let dep2 = expensive_computation(db, input);
    eprintln!("Computed expensive_computation: {:?}", dep2);
    
    combine(dep1, dep2)
}
```

### Memory Leaks
```rust
fn debug_memory_growth(db: &mut dyn Database) {
    // Force garbage collection
    db.trigger_lru_eviction();
    
    #[cfg(feature = "salsa_unstable")]
    {
        let before = get_memory_usage(db);
        
        // Perform operations
        do_work(db);
        
        // Force cleanup again
        db.trigger_lru_eviction();
        
        let after = get_memory_usage(db);
        
        if after > before * 2 {
            eprintln!("Warning: Memory usage doubled from {} to {} bytes", before, after);
            analyze_memory_usage(db);
        }
    }
}

#[cfg(feature = "salsa_unstable")]
fn get_memory_usage(db: &dyn Database) -> usize {
    let struct_info = db.structs_info();
    let query_info = db.queries_info();
    
    let struct_memory: usize = struct_info.iter()
        .map(|info| info.size_of_fields() + info.size_of_metadata())
        .sum();
    
    let query_memory: usize = query_info.values()
        .map(|info| info.size_of_fields() + info.size_of_metadata())
        .sum();
    
    struct_memory + query_memory
}
```

## Testing and Validation

### Determinism Testing
```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_deterministic_execution() {
        let mut db1 = MyDatabase::default();
        let mut db2 = MyDatabase::default();
        
        // Set up identical inputs
        let input1 = MyInput::new(&db1, "test data".to_string());
        let input2 = MyInput::new(&db2, "test data".to_string());
        
        // Run same computation
        let result1 = my_computation(&db1, input1);
        let result2 = my_computation(&db2, input2);
        
        // Results should be identical
        assert_eq!(result1, result2);
        
        // Event logs should be identical too
        let events1 = db1.take_events();
        let events2 = db2.take_events();
        assert_eq!(events1.len(), events2.len());
    }
}
```

### Incremental Behavior Testing
```rust
#[cfg(test)]
mod tests {
    #[test]
    fn test_incremental_behavior() {
        let mut db = LoggingDatabase::default();
        
        let input = MyInput::new(&db, "initial".to_string());
        
        // First computation
        let result1 = expensive_computation(&db, input);
        let events1 = db.take_events();
        let executions1 = count_executions(&events1);
        
        // Second computation (should reuse cache)
        let result2 = expensive_computation(&db, input);
        let events2 = db.take_events();
        let executions2 = count_executions(&events2);
        
        assert_eq!(result1, result2);
        assert_eq!(executions2, 0, "Should have reused cached result");
        
        // Modify input
        input.set_data(&mut db).to("modified".to_string());
        
        // Third computation (should recompute)
        let result3 = expensive_computation(&db, input);
        let events3 = db.take_events();
        let executions3 = count_executions(&events3);
        
        assert_ne!(result1, result3);
        assert!(executions3 > 0, "Should have recomputed after input change");
    }
    
    fn count_executions(events: &[Event]) -> usize {
        events.iter()
            .filter(|event| matches!(event.kind, EventKind::WillExecute { .. }))
            .count()
    }
}
```

## Debugging Tools

### Query Dependency Visualizer
```rust
pub fn visualize_dependencies(db: &dyn Database, root_query: DatabaseKeyIndex) {
    // This would require access to internal dependency structures
    // In practice, use event logging to build dependency graph
    println!("Dependency visualization would go here");
    
    // Example of what this might look like:
    // root_query
    // ├── dependency1
    // │   ├── sub_dependency1
    // │   └── sub_dependency2
    // └── dependency2
    //     └── sub_dependency3
}
```

### Performance Bottleneck Detector
```rust
pub fn detect_bottlenecks(events: &[Event]) {
    let mut query_counts = HashMap::new();
    
    for event in events {
        if let EventKind::WillExecute { database_key } = &event.kind {
            *query_counts.entry(database_key).or_insert(0) += 1;
        }
    }
    
    // Find queries that execute frequently
    let mut frequent_queries: Vec<_> = query_counts.iter().collect();
    frequent_queries.sort_by_key(|(_, count)| std::cmp::Reverse(**count));
    
    println!("=== Most Frequently Executed Queries ===");
    for (key, count) in frequent_queries.iter().take(10) {
        println!("{:?}: {} executions", key, count);
    }
    
    // Suggest optimizations
    for (key, count) in frequent_queries.iter().take(5) {
        if **count > 100 {
            println!("Consider optimizing {:?} - executed {} times", key, count);
        }
    }
}
```

### Regression Detection
```rust
#[cfg(test)]
mod regression_tests {
    use super::*;
    
    #[test]
    fn test_performance_regression() {
        let mut db = ProfilingDatabase::default();
        
        // Baseline measurement
        let start = std::time::Instant::now();
        let _result = expensive_computation(&db, test_input());
        let baseline_duration = start.elapsed();
        
        // Allow 20% performance regression
        let max_allowed = baseline_duration * 120 / 100;
        
        // Test current performance
        let start = std::time::Instant::now();
        let _result = expensive_computation(&db, test_input());
        let current_duration = start.elapsed();
        
        assert!(
            current_duration <= max_allowed,
            "Performance regression detected: {:?} > {:?}",
            current_duration,
            max_allowed
        );
    }
}
```

## Best Practices

### Debugging Strategy
1. **Start with event logging** to understand execution patterns
2. **Use debug attributes** on suspected problematic queries
3. **Add manual logging** for complex logic
4. **Test incrementality** to ensure correct behavior
5. **Monitor memory usage** to detect leaks

### Performance Optimization
1. **Profile before optimizing** to identify real bottlenecks
2. **Measure cache hit rates** to validate memoization effectiveness
3. **Monitor memory growth** to prevent excessive usage
4. **Test with realistic data** to get accurate measurements
5. **Use durability levels** appropriately to minimize recomputation

### Production Monitoring
1. **Log performance metrics** for key queries
2. **Set up alerts** for unusual memory growth
3. **Track cache hit rates** over time
4. **Monitor query execution frequency** to identify hotspots
5. **Use sampling** to reduce monitoring overhead

This comprehensive debugging and profiling approach helps ensure your Salsa-based applications perform well and behave correctly in all scenarios.