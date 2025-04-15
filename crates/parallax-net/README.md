# Parallax-Net

A high-performance runtime for executing interaction nets in parallel. This crate is part of the Parallax project, providing the core runtime system for parallel interaction net reduction.

## Overview

Parallax-Net implements a work-stealing scheduler that efficiently distributes reduction work across multiple threads while maintaining thread safety and performance. It's designed to be:

- **Efficient**: Optimized for high-throughput parallel reduction
- **Safe**: Thread-safe with careful memory management
- **Scalable**: Automatically scales to available CPU cores
- **Flexible**: Supports various node types and reduction rules

## Architecture

The runtime is built around several key components:

### Runtime
The central coordinator that manages:
- A pool of worker threads
- Work distribution across workers
- Access to partitions
- Thread coordination

### Partition
A collection of nodes that can be processed by a single worker:
- Storage for different node types
- Queue of redexes to process
- Ownership transfer mechanism4

### Worker
A thread that processes partitions and their redexes:
- Owns a set of partitions
- Processes redexes from owned partitions
- Can request and transfer partition ownership
- Coordinates with other workers

### Port System
A unique identifier for each node port that encodes:
- Port type (principal, left, right)
- Node type (constructor, duplicator, etc.)
- Packet ID
- Node index

## Node Types

The system supports several node types:

- **Constructor**: Creates compound data structures
- **Duplicator**: Copies data
- **Ref**: Provides references
- **Number**: Stores numeric values
- **Switch**: Implements control flow
- **Async**: Handles asynchronous operations

## Memory Management

The runtime is optimized for efficient memory usage:
- Uses `Slab` for efficient node storage
- Implements garbage collection through erase queues
- Maintains good cache locality through partition ownership
- Minimizes memory allocation during reduction

## Performance Considerations

The runtime is optimized for:
- Efficient work distribution across threads
- Cache-friendly memory access patterns
- Minimized contention through partition isolation

## Dependencies

- `crossbeam-queue`: For concurrent queues
- `parking_lot`: For efficient locks
- `slab`: For efficient node storage
- `fxhash`: For fast hashing
- `num_cpus`: For CPU core detection

## Safety

The system uses `unsafe` blocks carefully with explicit safety documentation. Access to shared state is controlled through locks and atomic operations. Partition ownership ensures exclusive access to node storage.