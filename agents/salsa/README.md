# Salsa 0.23 - Incremental Computation Framework

Salsa is a generic framework for on-demand, incrementalized computation. It enables you to structure your program as a set of queries (functions) that can be memoized and incrementally recomputed when their inputs change.

## Key Features

- **Incremental Recomputation**: Only recomputes what has actually changed
- **Memoization**: Automatically caches function results
- **Dependency Tracking**: Tracks which queries depend on which inputs
- **Cycle Detection**: Handles recursive dependencies gracefully
- **Parallel Execution**: Supports parallel query execution with work-stealing
- **Memory Management**: Automatic cleanup of stale memoized values

## Core Concepts

### Queries
Functions that can be memoized and incrementally recomputed. Salsa provides several types:

- **Input Queries**: Store base data that can be modified
- **Tracked Functions**: Derived computations that depend on inputs or other queries
- **Interned Values**: Deduplicated values with stable identities
- **Tracked Structs**: Structured data with tracked fields

### Database
The central coordinator that stores all data and manages query execution. Every Salsa program needs a database implementation.

### Revisions
Salsa tracks changes through revisions. When inputs change, a new revision is created, and dependent queries are marked for recomputation.

## Quick Start

Add Salsa to your `Cargo.toml`:

```toml
[dependencies]
salsa = "0.23"
```

## Documentation Structure

- [Getting Started](getting-started.md) - Basic setup and first examples
- [Core Concepts](core-concepts.md) - Deep dive into Salsa's architecture
- [Database Setup](database-setup.md) - Creating and configuring databases
- [Query System](query-system.md) - Understanding different query types
- [Incremental Recomputation](incremental-recomputation.md) - How Salsa optimizes computation
- [Debugging and Profiling](debugging-profiling.md) - Tools for understanding performance
- [Advanced Patterns](advanced-patterns.md) - Complex usage scenarios
- [Migration Guide](migration-guide.md) - Upgrading from previous versions
- [API Reference](api-reference.md) - Complete API documentation

## Version 0.23 Highlights

- Improved tracked struct performance with better memory layout
- Enhanced accumulator system for collecting values during computation
- Better cycle detection and recovery mechanisms
- Optimized dependency tracking with reduced overhead
- Improved parallel execution with work-stealing scheduler
- Better memory management with LRU eviction

## License

Salsa is dual-licensed under Apache 2.0 and MIT licenses.