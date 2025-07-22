# Database Setup and Configuration

This guide covers how to set up and configure Salsa databases based on the actual implementation in version 0.23.

## Basic Database Setup

### Minimal Database
```rust
use salsa::{Database, Storage};

#[salsa::db]
#[derive(Default)]
pub struct MyDatabase {
    storage: Storage<Self>,
}

#[salsa::db]
impl Database for MyDatabase {}
```

The `Storage<Self>` field is required and contains all the internal Salsa state.

### Database with Custom Data
```rust
#[salsa::db]
#[derive(Default)]
pub struct CompilerDatabase {
    storage: Storage<Self>,
    
    // Custom fields for your application
    pub config: CompilerConfig,
    pub file_system: FileSystem,
}

#[salsa::db]
impl Database for CompilerDatabase {}

impl CompilerDatabase {
    pub fn new(config: CompilerConfig) -> Self {
        Self {
            storage: Storage::default(),
            config,
            file_system: FileSystem::new(),
        }
    }
}
```

## Database Traits

### Creating Database Traits
Database traits allow you to group related functionality:

```rust
// Define a trait for parsing operations
#[salsa::db]
pub trait ParserDatabase: Database {
    // This trait can be implemented by any database
    // that wants to support parsing operations
}

// Define a trait for type checking operations  
#[salsa::db]
pub trait TypeCheckerDatabase: ParserDatabase {
    // This trait extends ParserDatabase
    // Type checker needs parsing functionality
}

// Implement the traits for your concrete database
#[salsa::db]
impl ParserDatabase for CompilerDatabase {}

#[salsa::db] 
impl TypeCheckerDatabase for CompilerDatabase {}
```

### Using Database Traits in Functions
```rust
// Functions can require specific database traits
#[salsa::tracked]
fn parse_file(db: &dyn ParserDatabase, file: SourceFile) -> Ast {
    // This function only needs ParserDatabase capabilities
    // Can be called with any database implementing ParserDatabase
}

#[salsa::tracked]
fn type_check(db: &dyn TypeCheckerDatabase, ast: Ast) -> TypedAst {
    // This function needs both parsing and type checking capabilities
    let parsed = parse_file(db, ast.source_file(db));
    check_types(parsed)
}
```

## Storage Configuration

### Event Handling
```rust
use salsa::{Event, EventKind};

impl Default for MyDatabase {
    fn default() -> Self {
        Self {
            storage: Storage::new(Some(Box::new(|event: Event| {
                match event.kind {
                    EventKind::WillExecute { database_key } => {
                        eprintln!("Executing query: {:?}", database_key);
                    }
                    EventKind::DidValidateMemoizedValue { database_key } => {
                        eprintln!("Reused memoized value: {:?}", database_key);
                    }
                    EventKind::WillBlockOn { other_thread_id, database_key } => {
                        eprintln!("Blocking on thread {:?} for {:?}", other_thread_id, database_key);
                    }
                    EventKind::DidDiscard { key } => {
                        eprintln!("Discarded stale value: {:?}", key);
                    }
                    _ => {}
                }
            }))),
        }
    }
}
```

### No Event Handling
```rust
impl Default for MyDatabase {
    fn default() -> Self {
        Self {
            storage: Storage::new(None), // No event handler
        }
    }
}
```

## Database Views and Upcasting

### Database Views
Views allow you to present different interfaces to the same database:

```rust
// Define view traits
#[salsa::db]
pub trait ReadOnlyDatabase: Database {
    // Only read operations allowed
}

#[salsa::db]
pub trait FullDatabase: ReadOnlyDatabase {
    // Both read and write operations
}

// Implement views
#[salsa::db]
impl ReadOnlyDatabase for MyDatabase {}

#[salsa::db]
impl FullDatabase for MyDatabase {}

// Use views to restrict access
fn read_only_operation(db: &dyn ReadOnlyDatabase) {
    // This function can't modify the database
}

fn full_operation(db: &dyn FullDatabase) {
    // This function can modify the database
}
```

### Upcasting Between Views
```rust
fn use_database(db: &dyn Database) {
    // Upcast to specific view
    let read_only: &dyn ReadOnlyDatabase = db.as_view();
    let full: &dyn FullDatabase = db.as_view();
    
    read_only_operation(read_only);
    full_operation(full);
}
```

## Memory Management Configuration

### LRU Configuration
```rust
// Configure LRU limits for specific query types
impl MyDatabase {
    pub fn configure_lru(&mut self) {
        // This would be done through ingredient-specific configuration
        // The exact API depends on your specific ingredients
    }
    
    pub fn trigger_cleanup(&mut self) {
        // Force LRU eviction
        self.trigger_lru_eviction();
    }
}
```

### Manual Memory Management
```rust
impl MyDatabase {
    pub fn clear_caches(&mut self) {
        // Trigger a synthetic write to invalidate all caches
        self.synthetic_write(salsa::Durability::MIN);
    }
    
    pub fn memory_usage(&self) -> Vec<salsa::IngredientInfo> {
        // Get memory usage information (requires salsa_unstable feature)
        #[cfg(feature = "salsa_unstable")]
        {
            self.structs_info()
        }
        #[cfg(not(feature = "salsa_unstable"))]
        {
            vec![]
        }
    }
}
```

## Thread Safety and Snapshots

### Creating Snapshots
```rust
use std::sync::Arc;
use std::thread;

fn parallel_processing(db: &MyDatabase) {
    // Create a snapshot for parallel access
    let db_snapshot = db.snapshot();
    
    // Spawn threads with the snapshot
    let handles: Vec<_> = (0..4).map(|i| {
        let db = db_snapshot.clone();
        thread::spawn(move || {
            // Each thread gets its own database reference
            process_chunk(&*db, i);
        })
    }).collect();
    
    // Wait for all threads to complete
    for handle in handles {
        handle.join().unwrap();
    }
}

fn process_chunk(db: &dyn Database, chunk_id: usize) {
    // Process data using the database snapshot
    // This is read-only access
}
```

### Cancellation Handling
```rust
#[salsa::tracked]
fn long_running_computation(db: &dyn Database, input: LargeInput) -> Result {
    let mut result = Vec::new();
    
    for item in input.items(db) {
        // Check for cancellation periodically
        db.unwind_if_revision_cancelled();
        
        // Process item
        result.push(process_item(db, item));
    }
    
    Result::new(result)
}
```

## Database Lifecycle

### Initialization
```rust
impl MyDatabase {
    pub fn new() -> Self {
        let mut db = Self::default();
        
        // Initialize with default data
        db.setup_defaults();
        
        db
    }
    
    fn setup_defaults(&mut self) {
        // Create initial inputs
        let config = Config::new(self, ConfigData::default());
        // Store reference to config if needed
    }
}
```

### Shutdown
```rust
impl Drop for MyDatabase {
    fn drop(&mut self) {
        // Cleanup is automatic, but you can add custom cleanup here
        eprintln!("Database shutting down");
    }
}
```

## Advanced Configuration

### Custom Durability Management
```rust
#[salsa::input]
pub struct ConfigFile {
    #[returns(ref)]
    pub content: String,
}

impl MyDatabase {
    pub fn load_config(&mut self, path: &str) -> ConfigFile {
        let content = std::fs::read_to_string(path).unwrap();
        
        // Create config with high durability (changes rarely)
        let config = ConfigFile::new(self, content);
        
        // Set high durability for this input
        // This is typically done through the input's durability field
        config
    }
    
    pub fn load_source_file(&mut self, path: &str) -> SourceFile {
        let content = std::fs::read_to_string(path).unwrap();
        
        // Source files have low durability (change frequently)
        SourceFile::new(self, content)
    }
}
```

### Database Composition
```rust
// Compose multiple database traits
#[salsa::db]
pub trait CompilerDatabase: 
    ParserDatabase + 
    TypeCheckerDatabase + 
    CodeGenDatabase 
{
    // Combined interface for full compiler
}

#[salsa::db]
impl CompilerDatabase for MyDatabase {}

// Use the composed interface
fn compile_program(db: &dyn CompilerDatabase, source: SourceFile) -> Binary {
    let ast = parse_file(db, source);
    let typed_ast = type_check(db, ast);
    generate_code(db, typed_ast)
}
```

### Testing Configuration
```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    fn test_database() -> MyDatabase {
        let mut db = MyDatabase::default();
        
        // Configure for testing
        db.enable_logging();
        
        db
    }
    
    #[test]
    fn test_incremental_computation() {
        let mut db = test_database();
        
        let input = SourceFile::new(&db, "test content".to_string());
        let result1 = process_file(&db, input);
        
        // Modify input
        input.set_content(&mut db).to("modified content".to_string());
        let result2 = process_file(&db, input);
        
        // Verify incremental behavior
        assert_ne!(result1, result2);
    }
}
```

## Best Practices

### Database Design
1. **Single Database Instance**: Use one database per application instance
2. **Trait Composition**: Use database traits to organize functionality
3. **View Restrictions**: Use views to limit access where appropriate
4. **Event Monitoring**: Use event handlers for debugging and profiling

### Memory Management
1. **Configure LRU**: Set appropriate LRU limits for your workload
2. **Monitor Usage**: Use memory usage APIs to track consumption
3. **Periodic Cleanup**: Trigger cleanup during idle periods
4. **Durability Levels**: Use appropriate durability for different input types

### Thread Safety
1. **Snapshots for Parallelism**: Use snapshots for read-only parallel access
2. **Cancellation Checks**: Check for cancellation in long-running computations
3. **Avoid Leaks**: Don't leak database references across thread boundaries
4. **Proper Synchronization**: Use proper synchronization for mutable access

This configuration approach ensures optimal performance and correct behavior in your Salsa-based applications.