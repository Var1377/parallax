# Work-Stealing Algorithm in Rayon

## Introduction

Rayon's work-stealing scheduler is the core mechanism that enables efficient parallel execution. It automatically distributes work across available CPU cores and provides excellent load balancing without manual intervention.

## What is Work-Stealing?

Work-stealing is a scheduling algorithm where:

1. **Each thread has its own work queue** (deque - double-ended queue)
2. **Threads work on their own tasks first** (from the bottom of their deque)
3. **Idle threads "steal" work from other threads** (from the top of other deques)
4. **Load balancing happens automatically** without central coordination

This approach minimizes contention and maximizes CPU utilization.

## Rayon's Work-Stealing Implementation

### Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    Thread Pool Registry                     │
├─────────────────────────────────────────────────────────────┤
│  Global Injector Queue  │  Thread 0  │  Thread 1  │ ... │  │
│  (External work)        │   Deque    │   Deque    │     │  │
└─────────────────────────────────────────────────────────────┘
                              │           │
                              ▼           ▼
                         ┌─────────┐ ┌─────────┐
                         │ Worker  │ │ Worker  │
                         │ Thread  │ │ Thread  │
                         └─────────┘ └─────────┘
```

### Key Components

#### 1. Worker Threads
Each worker thread has:
- **Local deque**: LIFO for own work, FIFO for stealing
- **Stealer**: Interface for other threads to steal work
- **Registry reference**: Access to global state

#### 2. Job System
```rust
// Simplified job representation
pub trait Job {
    unsafe fn execute(this: *const ());
}

pub struct JobRef {
    pointer: *const (),
    execute_fn: unsafe fn(*const ()),
}
```

#### 3. Deque Structure
Rayon uses `crossbeam-deque` which provides:
- **Lock-free operations** for high performance
- **LIFO access** for the owning thread (better cache locality)
- **FIFO access** for stealing threads (reduces contention)

## Work-Stealing Process

### 1. Work Distribution

When a parallel iterator starts:

```rust
use rayon::prelude::*;

(0..1000).into_par_iter().for_each(|i| {
    // This work gets distributed across threads
    expensive_computation(i);
});
```

The work distribution follows this process:

1. **Initial Split**: Work is recursively divided into chunks
2. **Job Creation**: Each chunk becomes a job in the work-stealing system
3. **Queue Assignment**: Jobs are placed in worker deques or the global injector

### 2. Work Execution Loop

Each worker thread follows this pattern:

```rust
// Simplified worker loop
loop {
    if let Some(job) = pop_from_local_deque() {
        // Execute own work (LIFO - good cache locality)
        job.execute();
    } else if let Some(job) = steal_from_global_injector() {
        // Take work from global queue
        job.execute();
    } else if let Some(job) = steal_from_other_threads() {
        // Steal work from other threads (FIFO)
        job.execute();
    } else {
        // No work available, go to sleep
        sleep_until_work_available();
    }
}
```

### 3. Stealing Strategy

Rayon implements sophisticated stealing strategies:

#### Random Victim Selection
```rust
// Threads randomly select victims to steal from
let victim_index = random() % num_threads;
if let Some(job) = steal_from_thread(victim_index) {
    job.execute();
}
```

#### Exponential Backoff
When no work is found, threads use exponential backoff:
1. Spin briefly (low latency)
2. Yield to OS scheduler
3. Sleep for increasing durations
4. Wake up when new work arrives

## Parallel Iterator Work-Stealing

### Recursive Splitting

Parallel iterators use recursive splitting to create work:

```rust
// Example: parallel map operation
impl<I, F> ParallelIterator for Map<I, F>
where
    I: ParallelIterator,
    F: Fn(I::Item) -> R + Sync + Send,
{
    fn drive_unindexed<C>(self, consumer: C) -> C::Result
    where
        C: UnindexedConsumer<Self::Item>,
    {
        // Split the work recursively
        let (left, right) = self.base.split();
        
        // Create jobs for each half
        let left_job = || left.drive_unindexed(consumer.split_off_left());
        let right_job = || right.drive_unindexed(consumer);
        
        // Execute in parallel using join
        join(left_job, right_job)
    }
}
```

### Join Operation

The `join` function is the primitive for parallel execution:

```rust
use rayon::join;

let (left_result, right_result) = join(
    || expensive_computation_left(),
    || expensive_computation_right()
);
```

Internally, `join` works as follows:

1. **Push right task** to local deque
2. **Execute left task** immediately
3. **Try to pop right task** when left completes
4. **If stolen**, wait for completion
5. **Return both results**

## Performance Characteristics

### Advantages

1. **Excellent Load Balancing**
   - Idle threads automatically find work
   - No manual load distribution needed

2. **Low Overhead**
   - Lock-free data structures
   - Minimal synchronization

3. **Cache Efficiency**
   - LIFO execution improves locality
   - Recursive splitting maintains data relationships

4. **Scalability**
   - Linear scaling with CPU cores
   - Minimal contention between threads

### Work-Stealing vs Other Approaches

| Approach | Load Balancing | Overhead | Complexity |
|----------|----------------|----------|------------|
| Work-Stealing | Automatic | Low | Low |
| Static Partitioning | Poor | Very Low | Very Low |
| Central Queue | Good | High | Medium |
| Work-Sharing | Manual | Medium | High |

## Tuning Work-Stealing Behavior

### Thread Pool Configuration

```rust
use rayon::ThreadPoolBuilder;

// Configure thread pool
let pool = ThreadPoolBuilder::new()
    .num_threads(8)
    .thread_name(|index| format!("rayon-{}", index))
    .build()
    .unwrap();

pool.install(|| {
    // Work executes in custom thread pool
    (0..1000).into_par_iter().sum::<i32>()
});
```

### Breadth-First vs Depth-First

```rust
use rayon::ThreadPoolBuilder;

// Breadth-first scheduling (better for many small tasks)
let pool = ThreadPoolBuilder::new()
    .breadth_first()
    .build()
    .unwrap();

// Default is depth-first (better cache locality)
```

### Granularity Control

Control work granularity to optimize stealing:

```rust
use rayon::prelude::*;

let data: Vec<i32> = (0..1_000_000).collect();

// Fine-grained (more stealing opportunities)
let sum1: i32 = data.par_iter().sum();

// Coarse-grained (less overhead)
let sum2: i32 = data
    .par_chunks(1000)
    .map(|chunk| chunk.iter().sum::<i32>())
    .sum();
```

## Advanced Work-Stealing Patterns

### Custom Job Types

```rust
use rayon::spawn;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

let counter = Arc::new(AtomicUsize::new(0));

// Spawn individual jobs
for i in 0..100 {
    let counter = Arc::clone(&counter);
    spawn(move || {
        // This work can be stolen by any thread
        let result = expensive_computation(i);
        counter.fetch_add(result, Ordering::Relaxed);
    });
}
```

### Scope for Structured Parallelism

```rust
use rayon::scope;

let mut data = vec![0; 1000];

scope(|s| {
    for chunk in data.chunks_mut(100) {
        s.spawn(|_| {
            // Process chunk in parallel
            for item in chunk {
                *item = expensive_computation(*item);
            }
        });
    }
    // All spawned tasks complete before scope ends
});
```

## Work-Stealing Debugging

### Monitoring Thread Activity

```rust
use rayon::prelude::*;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

let work_counter = Arc::new(AtomicUsize::new(0));
let steal_counter = Arc::new(AtomicUsize::new(0));

(0..1000).into_par_iter().for_each(|i| {
    work_counter.fetch_add(1, Ordering::Relaxed);
    
    // Simulate work that might be stolen
    if rayon::current_thread_index().unwrap() != i % rayon::current_num_threads() {
        steal_counter.fetch_add(1, Ordering::Relaxed);
    }
    
    expensive_computation(i);
});

println!("Total work: {}", work_counter.load(Ordering::Relaxed));
println!("Stolen work: {}", steal_counter.load(Ordering::Relaxed));
```

### Visualizing Work Distribution

```rust
use rayon::prelude::*;
use std::collections::HashMap;
use std::sync::Mutex;

let thread_work = Mutex::new(HashMap::new());

(0..1000).into_par_iter().for_each(|i| {
    let thread_id = rayon::current_thread_index().unwrap();
    
    {
        let mut map = thread_work.lock().unwrap();
        *map.entry(thread_id).or_insert(0) += 1;
    }
    
    expensive_computation(i);
});

let map = thread_work.into_inner().unwrap();
for (thread_id, count) in map {
    println!("Thread {}: {} tasks", thread_id, count);
}
```

## Common Work-Stealing Pitfalls

### 1. Too Fine-Grained Tasks

```rust
// BAD: Too much overhead
(0..1000).into_par_iter().for_each(|i| {
    simple_operation(i); // Very fast operation
});

// GOOD: Batch small operations
(0..1000).into_par_iter()
    .chunks(100)
    .for_each(|chunk| {
        for i in chunk {
            simple_operation(i);
        }
    });
```

### 2. Unbalanced Work

```rust
// BAD: Uneven work distribution
let sizes = vec![1, 1, 1, 1000]; // Last task much larger
sizes.into_par_iter().for_each(|size| {
    expensive_computation_with_size(size);
});

// GOOD: Pre-sort or use dynamic scheduling
let mut sizes = vec![1, 1, 1, 1000];
sizes.sort_by(|a, b| b.cmp(a)); // Largest first
sizes.into_par_iter().for_each(|size| {
    expensive_computation_with_size(size);
});
```

### 3. False Sharing

```rust
use std::sync::atomic::{AtomicUsize, Ordering};

// BAD: False sharing between threads
let counters = vec![AtomicUsize::new(0); 8];
(0..1000).into_par_iter().for_each(|i| {
    let thread_id = rayon::current_thread_index().unwrap();
    counters[thread_id % 8].fetch_add(1, Ordering::Relaxed);
});

// GOOD: Thread-local accumulation
use rayon::prelude::*;
let sum: usize = (0..1000)
    .into_par_iter()
    .fold(|| 0, |acc, _| acc + 1)
    .sum();
```

## Performance Tips

1. **Right-size your tasks**: Aim for 1-10ms per task
2. **Minimize shared state**: Use reduction patterns instead
3. **Consider data layout**: Ensure good cache locality
4. **Profile your code**: Measure actual performance gains
5. **Use appropriate granularity**: Balance overhead vs parallelism

## Integration with Other Systems

### Custom Thread Pools

```rust
use rayon::ThreadPoolBuilder;

// Create isolated thread pool for specific work
let compute_pool = ThreadPoolBuilder::new()
    .num_threads(4)
    .thread_name(|i| format!("compute-{}", i))
    .build()
    .unwrap();

let io_pool = ThreadPoolBuilder::new()
    .num_threads(2)
    .thread_name(|i| format!("io-{}", i))
    .build()
    .unwrap();

// Use different pools for different work types
let compute_result = compute_pool.install(|| {
    (0..1000).into_par_iter().map(|x| x * x).sum::<i32>()
});
```

### Nested Parallelism

```rust
use rayon::prelude::*;

// Outer parallel loop
let results: Vec<Vec<i32>> = (0..10)
    .into_par_iter()
    .map(|i| {
        // Inner parallel computation
        (0..100)
            .into_par_iter()
            .map(|j| expensive_computation(i * 100 + j))
            .collect()
    })
    .collect();
```

Work-stealing handles nested parallelism efficiently by:
- Flattening nested tasks into the same work-stealing pool
- Maintaining good load balancing across all levels
- Avoiding thread pool exhaustion

## Next Steps

Understanding work-stealing helps you:
- Write more efficient parallel code
- Debug performance issues
- Choose appropriate granularity
- Design better parallel algorithms

Continue with:
- [Thread Pool Configuration](03-thread-pools.md) - Customizing the execution environment
- [Performance Tuning](05-performance.md) - Optimizing parallel code
- [Debugging](06-debugging.md) - Troubleshooting parallel execution