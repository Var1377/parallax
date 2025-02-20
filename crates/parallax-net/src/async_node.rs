//! Asynchronous computation support for the graph reduction engine.
//!
//! This module provides the infrastructure for handling asynchronous computations
//! within the graph reduction system. It includes:
//!
//! - State management for async nodes
//! - Lock-free completion queues
//! - Batched async event processing
//! - Timeout-based event handling
//! - Integration with tokio runtime

use futures::Future;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::mpsc;
use crate::NodeIndex;
use concurrent_queue::{ConcurrentQueue, PushError};
use event_listener::{Event, Listener};

/// Represents the possible states of an asynchronous node.
/// States transition in the following order:
/// Pending -> Ready -> Resolved -> Propagated
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AsyncState {
    /// Initial state, computation not yet started
    Pending,
    /// Computation is ready to begin
    Ready,
    /// Computation has completed with a result
    Resolved,
    /// Result has been propagated through the graph
    Propagated,
}

/// Represents the completion of an asynchronous computation.
/// Contains both the node that completed and its result value.
#[derive(Debug, Clone)]
pub struct AsyncEvent {
    /// The node that completed computation
    pub node: NodeIndex,
    /// The result value of the computation
    pub value: u64,
}

/// A lock-free queue for handling async computation completions.
/// Cache-aligned for optimal performance in concurrent access.
#[repr(C, align(64))]
pub struct AsyncQueue {
    /// Underlying concurrent queue for events
    queue: ConcurrentQueue<AsyncEvent>,
    /// Event notification for blocking operations
    event: Event,
    /// Maximum capacity of the queue
    capacity: usize,
}

impl AsyncQueue {
    /// Creates a new async queue with the specified capacity
    pub fn new(capacity: usize) -> Self {
        Self {
            queue: ConcurrentQueue::bounded(capacity),
            event: Event::new(),
            capacity,
        }
    }

    /// Pushes a single event to the queue.
    /// Notifies one waiting consumer if successful.
    ///
    /// # Returns
    /// Ok(()) if successful, Err with the event if queue is full
    pub fn push(&self, event: AsyncEvent) -> Result<(), PushError<AsyncEvent>> {
        match self.queue.push(event) {
            Ok(()) => {
                // Notify one waiting consumer
                self.event.notify(1);
                Ok(())
            }
            Err(e) => Err(e),
        }
    }

    /// Pushes multiple events to the queue.
    /// Notifies consumers equal to the number of successful pushes.
    ///
    /// # Returns
    /// Number of events successfully pushed
    pub fn push_batch(&self, events: &[AsyncEvent]) -> usize {
        let mut count = 0;
        for event in events {
            if self.queue.push(event.clone()).is_ok() {
                count += 1;
            }
        }
        // Notify exactly the number of events pushed
        if count > 0 {
            self.event.notify(count);
        }
        count
    }

    /// Attempts to pop an event from the queue.
    /// Returns immediately if queue is empty.
    pub fn pop(&self) -> Option<AsyncEvent> {
        self.queue.pop().ok()
    }

    /// Attempts to pop multiple events from the queue.
    /// Returns as many events as available, up to max_items.
    pub fn pop_batch(&self, max_items: usize) -> Vec<AsyncEvent> {
        let mut result = Vec::with_capacity(max_items);
        
        // Try to fill the batch in one go
        for _ in 0..max_items {
            match self.queue.pop() {
                Ok(event) => result.push(event),
                Err(_) => break,
            }
        }
        
        result
    }

    /// Attempts to pop an event with a timeout.
    /// If no event is available immediately, waits up to timeout_ms milliseconds.
    pub fn pop_with_timeout(&self, timeout_ms: u64) -> Option<AsyncEvent> {
        // Try immediate pop first
        if let Ok(event) = self.queue.pop() {
            return Some(event);
        }

        // Start listening before checking again to avoid race condition
        let listener = self.event.listen();
        
        // Double-check after getting listener
        if let Ok(event) = self.queue.pop() {
            return Some(event);
        }

        // Wait with timeout
        if listener.wait_timeout(Duration::from_millis(timeout_ms)).is_some() {
            self.queue.pop().ok()
        } else {
            None
        }
    }

    /// Attempts to pop multiple events with a timeout.
    /// If no events are available immediately, waits up to timeout_ms milliseconds.
    pub fn pop_batch_with_timeout(&self, max_items: usize, timeout_ms: u64) -> Vec<AsyncEvent> {
        let mut result = Vec::with_capacity(max_items);
        
        // Try immediate batch pop first
        while result.len() < max_items {
            match self.queue.pop() {
                Ok(event) => result.push(event),
                Err(_) => break,
            }
        }

        // If we got any items, return them
        if !result.is_empty() {
            return result;
        }

        // Start listening before checking again
        let listener = self.event.listen();
        
        // Double-check after getting listener
        while result.len() < max_items {
            match self.queue.pop() {
                Ok(event) => result.push(event),
                Err(_) => break,
            }
        }

        // If still empty, wait with timeout
        if result.is_empty() && listener.wait_timeout(Duration::from_millis(timeout_ms)).is_some() {
            while result.len() < max_items {
                match self.queue.pop() {
                    Ok(event) => result.push(event),
                    Err(_) => break,
                }
            }
        }

        result
    }

    /// Returns the current number of events in the queue
    pub fn len(&self) -> usize {
        self.queue.len()
    }

    /// Returns true if the queue is empty
    pub fn is_empty(&self) -> bool {
        self.queue.is_empty()
    }

    /// Returns the maximum capacity of the queue
    pub fn capacity(&self) -> usize {
        self.capacity
    }
}

/// Runtime for managing asynchronous computations.
/// Handles spawning of futures and processing of completions.
pub struct AsyncRuntime {
    /// Queue for completed async events
    queue: Arc<AsyncQueue>,
    /// Channel sender for async events
    sender: mpsc::UnboundedSender<AsyncEvent>,
    /// Size of batches for processing
    batch_size: usize,
}

impl AsyncRuntime {
    /// Creates a new runtime with default capacity (4096)
    pub fn new() -> Self {
        Self::with_capacity(4096)
    }

    /// Creates a new runtime with specified capacity
    pub fn with_capacity(capacity: usize) -> Self {
        let (sender, mut receiver) = mpsc::unbounded_channel();
        let queue = Arc::new(AsyncQueue::new(capacity));
        
        // Set up background task to process channel messages
        let queue_clone = Arc::clone(&queue);
        tokio::spawn(async move {
            let mut batch = Vec::with_capacity(32); // Pre-allocate batch buffer
            
            while let Some(event) = receiver.recv().await {
                batch.push(event);
                
                // Process accumulated events if we have a full batch
                // or if there are no more immediate messages
                if batch.len() >= 32 || receiver.try_recv().is_err() {
                    queue_clone.push_batch(&batch);
                    batch.clear();
                }
            }
        });

        Self {
            queue,
            sender,
            batch_size: 32,
        }
    }

    /// Spawns a single async computation.
    ///
    /// # Arguments
    /// * `future` - Future to execute
    /// * `node` - Node to associate with the computation
    pub fn spawn<F>(&self, future: F, node: NodeIndex) 
    where
        F: Future<Output = u64> + Send + 'static,
    {
        let sender = self.sender.clone();
        tokio::spawn(async move {
            let value = future.await;
            let _ = sender.send(AsyncEvent { node, value });
        });
    }

    /// Spawns multiple async computations in batches.
    ///
    /// # Arguments
    /// * `futures` - Iterator of (future, node) pairs to execute
    pub fn spawn_batch<I, F>(&self, futures: I)
    where
        I: IntoIterator<Item = (F, NodeIndex)>,
        F: Future<Output = u64> + Send + 'static,
    {
        let sender = self.sender.clone();
        let mut batch = Vec::with_capacity(self.batch_size);

        for (future, node) in futures {
            batch.push((future, node));
            
            if batch.len() >= self.batch_size {
                let sender_clone = sender.clone();
                tokio::spawn(async move {
                    let mut events = Vec::with_capacity(batch.len());
                    for (future, node) in batch {
                        let value = future.await;
                        events.push(AsyncEvent { node, value });
                    }
                    // Send all events at once
                    for event in events {
                        let _ = sender_clone.send(event);
                    }
                });
                batch = Vec::with_capacity(self.batch_size);
            }
        }

        // Handle remaining futures
        if !batch.is_empty() {
            tokio::spawn(async move {
                let mut events = Vec::with_capacity(batch.len());
                for (future, node) in batch {
                    let value = future.await;
                    events.push(AsyncEvent { node, value });
                }
                // Send remaining events
                for event in events {
                    let _ = sender.send(event);
                }
            });
        }
    }

    pub fn set_batch_size(&mut self, size: usize) {
        self.batch_size = size;
    }

    pub fn try_complete(&self) -> Option<AsyncEvent> {
        self.queue.pop()
    }

    pub fn try_complete_batch(&self, max_items: usize) -> Vec<AsyncEvent> {
        self.queue.pop_batch(max_items)
    }

    pub fn try_complete_with_timeout(&self, timeout_ms: u64) -> Option<AsyncEvent> {
        self.queue.pop_with_timeout(timeout_ms)
    }

    pub fn try_complete_batch_with_timeout(&self, max_items: usize, timeout_ms: u64) -> Vec<AsyncEvent> {
        self.queue.pop_batch_with_timeout(max_items, timeout_ms)
    }
}

// Implement Send + Sync for AsyncQueue
unsafe impl Send for AsyncQueue {}
unsafe impl Sync for AsyncQueue {}